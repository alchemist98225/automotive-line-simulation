# Portfolio demonstration; all defaults are illustrative, not measured plant data.
#
# Engine from the staffing / reliability model (operators, MTBF/MTTR, support
# work, activity timing), plus all-replication series and CSV/zip exports.
# Public helpers: default_params(), run_line_sim(), write_sim_exports(),
# params_to_tibble(), run_scenarios(), variability_scenarios().
suppressPackageStartupMessages({library(simmer); library(tidyverse)})
sim_now <- function(env) simmer::now(env)
default_params <- function() {
  list(sim_length_min=780, time_mode="shifts", shift_length_min=480, n_shifts=1L,
       release_policy="space", entry_buffer=3L, wip_limit=20L, order_spacing="fixed", n_replications=5L, base_seed=20261027L,
       warmup_min=300, arrival_mean_min=4, grid_dt_min=2, repair_technicians=1L,
       operators=tibble(pool=c("upstream", "downstream"), count=c(2L,2L)),
       stations=tribble(
         ~name, ~capacity, ~dist, ~mean_min, ~sd_min, ~operator_pool, ~load_min, ~unload_min, ~mtbf_min, ~mttr_min, ~task_interval_min, ~task_duration_min,
         "body_shop",2L,"exponential",6,NA_real_,"upstream",1,0.5,240,15,60,4,
         "paint",1L,"normal",8,1.5,"upstream",0.5,0.5,360,20,90,5,
         "final_assembly",2L,"normal",7,1,"downstream",2,1,300,10,45,4,
         "inspection",1L,"exponential",3,NA_real_,"downstream",1,0.5,Inf,0,60,3) %>% mutate(buffer_before=c(0,3,3,3)))
}
# A timing specification is data; each compiled sampler draws on every invocation.
# Normal samples are floored at zero, not redrawn. This changes their expected mean.
timing_spec <- function(method="fixed", mean_min=0, sd_min=0,
                        distribution="normal", observations=numeric()) {
  list(method=method,mean_min=mean_min,sd_min=sd_min,
       distribution=distribution,observations=observations)
}
validate_timing <- function(spec) {
  fail <- function(message) stop("Invalid timing: ",message,call.=FALSE)
  if (!is.list(spec) || length(spec$method)!=1L || is.na(spec$method) ||
      !spec$method %in% c("fixed","distribution","empirical")) fail("unknown method")
  if (spec$method=="empirical") {
    x <- spec$observations
    if (!is.numeric(x) || !length(x) || any(!is.finite(x) | x<0))
      fail("observations must be a nonempty vector of finite, nonnegative minutes")
  } else {
    if (length(spec$mean_min)!=1L || !is.numeric(spec$mean_min) ||
        !is.finite(spec$mean_min) || spec$mean_min<0) fail("mean/duration must be nonnegative")
    if (spec$method=="distribution") {
      if (length(spec$distribution)!=1L || is.na(spec$distribution) ||
          !spec$distribution %in% c("normal","lognormal","exponential")) fail("unknown distribution")
      if (spec$mean_min<=0) fail("distribution mean must be positive")
      if (spec$distribution!="exponential" &&
          (length(spec$sd_min)!=1L || !is.numeric(spec$sd_min) || !is.finite(spec$sd_min) || spec$sd_min<0))
        fail("SD must be finite and nonnegative")
    }
  }
  spec
}
make_duration_fn <- function(spec) {
  spec <- validate_timing(spec)
  if (spec$method=="fixed") return(function() spec$mean_min)
  if (spec$method=="empirical") {
    # sample(x,1) treats a singleton numeric x as 1:x; index sampling avoids that.
    return(function() spec$observations[[sample.int(length(spec$observations),1L)]])
  }
  if (spec$distribution=="exponential") return(function() rexp(1,1/spec$mean_min))
  if (spec$distribution=="normal") return(function() max(0,rnorm(1,spec$mean_min,spec$sd_min)))
  sigma <- sqrt(log1p((spec$sd_min/spec$mean_min)^2))
  mu <- log(spec$mean_min)-sigma^2/2
  function() rlnorm(1,mu,sigma)
}
timing_mean <- function(spec) {
  spec <- validate_timing(spec)
  if (spec$method=="empirical") return(mean(spec$observations))
  if (spec$method=="distribution" && spec$distribution=="normal" && spec$sd_min>0)
    return(spec$mean_min*pnorm(spec$mean_min/spec$sd_min)+spec$sd_min*dnorm(spec$mean_min/spec$sd_min))
  spec$mean_min
}
make_service_fn <- function(dist,mean_min,sd_min) {
  make_duration_fn(timing_spec(if(dist=="deterministic") "fixed" else "distribution",
    mean_min,sd_min,distribution=dist))
}
activity_timing <- function(st,activity) {
  column <- paste0(activity,"_timing")
  if (column %in% names(st)) return(st[[column]][[1]])
  switch(activity,
    auto=timing_spec(if(st$dist=="deterministic") "fixed" else "distribution",st$mean_min,st$sd_min,st$dist),
    load=timing_spec(mean_min=st$load_min),
    unload=timing_spec(mean_min=st$unload_min),
    task=timing_spec(mean_min=st$task_duration_min))
}
# Missing timing list-columns are derived from legacy scalar fields.
# Old app/Quarto parameter lists remain usable, with labor/reliability disabled.
normalize_params <- function(p) {
  defaults <- list(buffer_before=Inf,operator_pool=NA_character_,load_min=0,unload_min=0,
                   mtbf_min=Inf,mttr_min=0,task_interval_min=Inf,task_duration_min=0,task_every_n=0L)
  walk(names(defaults),function(n) {if (!n %in% names(p$stations)) p$stations[[n]] <<- defaults[[n]]})
  if (is.null(p$operators)) p$operators <- tibble(pool=character(),count=integer())
  if (is.null(p$repair_technicians)) p$repair_technicians <- 1L
  walk(c("load","auto","unload","task"),function(activity) {
    column <- paste0(activity,"_timing")
    if (!column %in% names(p$stations))
      p$stations[[column]] <<- map(seq_len(nrow(p$stations)),~activity_timing(p$stations[.x,],activity))
  })
  p$time_mode <- p$time_mode %||% "legacy"
  if (p$time_mode=="shifts") p$sim_length_min <- p$warmup_min+p$shift_length_min*p$n_shifts
  else {p$shift_length_min <- p$sim_length_min-p$warmup_min; p$n_shifts <- 1L}
  p$release_policy <- p$release_policy %||% "scheduled"
  p$entry_buffer <- p$entry_buffer %||% 3L
  p$wip_limit <- p$wip_limit %||% 20L
  p$order_spacing <- p$order_spacing %||% "exponential"
  p
}
validate_params <- function(p) {
  positive <- function(x) all(is.finite(x) & x>0)
  integer_positive <- function(x) positive(x) && all(x==floor(x))
  stopifnot(positive(p$sim_length_min),positive(p$arrival_mean_min),
    integer_positive(p$n_replications),is.finite(p$warmup_min),p$warmup_min>=0,
    p$warmup_min<p$sim_length_min,is.finite(p$base_seed),p$base_seed>=0,
    p$base_seed+p$n_replications-1<=.Machine$integer.max,
    nrow(p$stations)>0,!anyDuplicated(p$stations$name),
    all(grepl("^[a-zA-Z][a-zA-Z0-9_]*$",p$stations$name)),
    integer_positive(p$stations$capacity),positive(p$stations$mean_min),
    all(p$stations$dist %in% c("normal","exponential","deterministic")),
    all(is.finite(p$stations$sd_min[p$stations$dist=="normal"]) & p$stations$sd_min[p$stations$dist=="normal"]>=0),
    all(is.finite(p$stations$load_min) & p$stations$load_min>=0),
    all(is.finite(p$stations$unload_min) & p$stations$unload_min>=0),
    all(!is.na(p$stations$mtbf_min) & p$stations$mtbf_min>0),
    all(is.finite(p$stations$mttr_min) & p$stations$mttr_min>=0),
    all(!is.na(p$stations$task_interval_min) & p$stations$task_interval_min>0),
    all(is.finite(p$stations$task_duration_min) & p$stations$task_duration_min>=0),
    all(is.finite(p$stations$task_every_n) & p$stations$task_every_n>=0 & p$stations$task_every_n==floor(p$stations$task_every_n)),
    !anyDuplicated(p$operators$pool),integer_positive(p$repair_technicians))
  if (nrow(p$operators)) stopifnot(integer_positive(p$operators$count))
  stopifnot(all(na.omit(p$stations$operator_pool) %in% p$operators$pool))
  walk(c("load","auto","unload","task"),function(activity) {
    specs <- p$stations[[paste0(activity,"_timing")]]
    stopifnot(is.list(specs),length(specs)==nrow(p$stations))
    walk(specs,validate_timing)
  })
  stopifnot(p$time_mode %in% c("shifts","legacy"),positive(p$shift_length_min),integer_positive(p$n_shifts),
    p$release_policy %in% c("scheduled","space","conwip"),p$order_spacing %in% c("fixed","exponential"),
    length(p$entry_buffer)==1,is.finite(p$entry_buffer),p$entry_buffer>=0,p$entry_buffer==floor(p$entry_buffer),
    length(p$wip_limit)==1,integer_positive(p$wip_limit),
    all(!is.na(p$stations$buffer_before) & p$stations$buffer_before>=0 &
      (is.infinite(p$stations$buffer_before) | p$stations$buffer_before==floor(p$stations$buffer_before))))
  p
}
# Integrate piecewise constant states, including the state at the warmup boundary.
resource_stats <- function(res,warmup,horizon) {
  res %>% group_by(resource) %>% group_modify(~{
    .x %>% arrange(time) %>% mutate(dt=pmax(0,pmin(lead(time,default=horizon),horizon)-pmax(time,warmup))) %>%
      summarise(avg_util=sum(if_else(capacity>0,server/capacity,0)*dt)/(horizon-warmup),
                avg_queue=sum(queue*dt)/(horizon-warmup),avg_server=sum(server*dt)/(horizon-warmup))
  }) %>% ungroup()
}
run_one_replication <- function(rep_id,seed,params) {
  set.seed(seed)
  env <- simmer(paste0("line",rep_id)); s <- params$stations
  events <- new.env(); events$failures <- list(); events$repairs <- list()
  events$admissions <- list(); events$waits <- list(); events$wip <- 0L; events$entrance <- 0L
  gated <- params$release_policy!="scheduled"
  can_admit <- function() events$entrance < s$capacity[1]+params$entry_buffer &&
    (params$release_policy!="conwip" || events$wip<params$wip_limit)
  update_gate <- function(t) if(gated) t %>% set_capacity("release_gate",function() as.integer(can_admit())) else t
  if(gated) add_resource(env,"release_gate",capacity=1)
  # One slot represents a downstream bay or a waiting position. Held until transfer.
  walk(seq_len(nrow(s))[-1],function(i) {
    if(is.finite(s$buffer_before[i])) add_resource(env,paste0("slots_",s$name[i]),s$capacity[i]+s$buffer_before[i])
  })
  admit <- function() {
    events$wip<-events$wip+1L; events$entrance<-events$entrance+1L
    events$admissions[[length(events$admissions)+1L]]<-tibble(name=get_name(env),release_time=sim_now(env));0
  }
  leave_first <- function() {events$entrance<-events$entrance-1L;0}
  depart <- function() {events$wip<-events$wip-1L;0}
  begin_wait <- function(st,type) {
    id<-length(events$waits)+1L;events$waits[[id]]<-tibble(station=st,type=type,start=sim_now(env),end=NA_real_);id
  }
  end_wait <- function() {id<-get_attribute(env,"wait_id");events$waits[[id]]$end<-sim_now(env);0}
  wait_for <- function(t,resource,st,type) t %>%
    set_attribute("wait_id",function() begin_wait(st,type)) %>% seize(resource) %>% timeout(end_wait)
  record_failure <- function(n) {id<-length(events$failures)+1L;events$failures[[id]]<-tibble(station=n,time=sim_now(env),failure_id=id);id}
  record_repair <- function(n,d) {events$repairs[[length(events$repairs)+1L]] <- tibble(station=n,time=sim_now(env),duration=d,failure_id=get_attribute(env,"failure_id")); d}
  walk2(params$operators$pool,params$operators$count,~add_resource(env,paste0("operator_",.x),.y))
  add_resource(env,"maintenance",params$repair_technicians)
  walk2(s$name,s$capacity, function(n,c) {
    add_resource(env,n,c); add_resource(env,paste0("auto_",n),c,preemptive=TRUE)
  })
  # Distinct automatic capacity permits resume after failure without retaining labor.
  trajectory_for_station <- function(i) {
    st <- s[i,]
    n <- st$name; op <- paste0("operator_",st$operator_pool)
    t <- trajectory(n) %>% seize(n)
    manual <- function(t,duration) {
      if (is.na(st$operator_pool)) return(t %>% timeout(duration))
      wait_for(t,op,n,"waiting_for_operator") %>% timeout(duration) %>% release(op)
    }
    t <- manual(t,make_duration_fn(activity_timing(st,"load")))
    t <- wait_for(t,paste0("auto_",n),n,"waiting_for_automatic_capacity") %>% timeout(make_duration_fn(activity_timing(st,"auto"))) %>% release(paste0("auto_",n))
    t <- manual(t,make_duration_fn(activity_timing(st,"unload")))
    if(i<nrow(s) && is.finite(s$buffer_before[i+1]))
      t <- wait_for(t,paste0("slots_",s$name[i+1]),n,"blocked")
    t <- t %>% release(n)
    if(i>1 && is.finite(st$buffer_before)) t<-t %>% release(paste0("slots_",n))
    if(i==1) t<-update_gate(t %>% timeout(leave_first))
    if (!is.na(st$operator_pool) && st$task_every_n>0 && timing_mean(activity_timing(st,"task"))>0) {
      t <- t %>% set_global(paste0("units_",n),1,mod="+") %>%
        branch(function() as.integer(get_global(env,paste0("units_",n)) %% st$task_every_n==0),continue=TRUE,
          trajectory() %>% seize(op) %>% timeout(make_duration_fn(activity_timing(st,"task"))) %>% release(op))
    }
    t
  }
  admission <- trajectory("vehicle")
  if(gated) admission<-admission %>% seize("release_gate")
  admission<-update_gate(admission %>% timeout(admit))
  if(gated) admission<-admission %>% release("release_gate")
  vehicle <- reduce(seq_len(nrow(s)),function(t,i) join(t,trajectory_for_station(i)),.init=admission)
  vehicle <- update_gate(vehicle %>% timeout(depart))
  # One independent alternating up/repair process per interchangeable machine.
  walk(seq_len(nrow(s)),function(i) {
    st <- s[i,]; n <- st$name
    if (is.finite(st$mtbf_min) && st$mttr_min>0) {
      failure <- trajectory("failure") %>% timeout(function() rexp(1,1/st$mtbf_min)) %>%
        set_attribute("failure_id",function() record_failure(n)) %>%
        seize(paste0("auto_",n)) %>% seize("maintenance") %>%
        timeout(function() record_repair(n,rexp(1,1/st$mttr_min))) %>% release("maintenance") %>%
        release(paste0("auto_",n)) %>% simmer::rollback(7,Inf)
      walk(seq_len(st$capacity),function(j) add_generator(env,paste0("failure_",n,"_",j,"_"),failure,at(0),priority=100,preemptible=100,restart=FALSE))
    }
    if (!is.na(st$operator_pool) && st$task_every_n==0 && is.finite(st$task_interval_min) && timing_mean(activity_timing(st,"task"))>0) {
      task <- trajectory("noncyclic") %>% seize(paste0("operator_",st$operator_pool)) %>%
        timeout(make_duration_fn(activity_timing(st,"task"))) %>% release(paste0("operator_",st$operator_pool))
      add_generator(env,paste0("task_",n,"_"),task,function() rexp(1,1/st$task_interval_min))
    }
  })
  first_order <- TRUE
  order_clock <- function() {
    if(params$order_spacing=="exponential") return(rexp(1,1/params$arrival_mean_min))
    if(first_order) {first_order<<-FALSE;return(0)}
    params$arrival_mean_min
  }
  add_generator(env,"veh",vehicle,order_clock,mon=2,restart=FALSE)
  run(env,until=params$sim_length_min)
  orders <- get_mon_arrivals(env,ongoing=TRUE) %>% as_tibble() %>% filter(startsWith(name,"veh"),start_time>=0,start_time<params$sim_length_min) %>%
    rename(order_time=start_time) %>% left_join(bind_rows(tibble(name=character(),release_time=double()),events$admissions),by="name")
  arrivals <- orders %>% filter(!is.na(release_time)) %>% mutate(start_time=release_time)
  completed <- arrivals %>% filter(finished,end_time>=params$warmup_min)
  # Lead times use departures in the observation window, including pre-warmup entrants.
  ct <- completed$end_time-completed$start_time
  initial <- bind_rows(tibble(resource=s$name,capacity=s$capacity),
    tibble(resource=paste0("auto_",s$name),capacity=s$capacity),
    params$operators %>% transmute(resource=paste0("operator_",pool),capacity=count),
    tibble(resource="maintenance",capacity=params$repair_technicians)) %>%
    mutate(time=0,server=0L,queue=0L,queue_size=Inf,system=0L,limit=Inf)
  res <- bind_rows(initial,get_mon_resources(env) %>% as_tibble())
  stats <- resource_stats(res,params$warmup_min,params$sim_length_min)
  # Arrival-based WIP counts each vehicle once, even when holding several resources.
  wip <- sum(pmax(0,pmin(if_else(arrivals$finished,arrivals$end_time,params$sim_length_min),params$sim_length_min)-pmax(arrivals$start_time,params$warmup_min)))/(params$sim_length_min-params$warmup_min)
  q <- function(prob) if(length(ct)) unname(quantile(ct,prob)) else NA_real_
  list(summary=tibble(replication=rep_id,seed=seed,n_completed=nrow(completed),
         throughput_per_hr=nrow(completed)/((params$sim_length_min-params$warmup_min)/60),
         cycle_time_mean=if(length(ct)) mean(ct) else NA_real_,cycle_time_p50=q(.5),cycle_time_p90=q(.9),cycle_time_p95=q(.95),avg_wip=wip,
         avg_unreleased_backlog=sum(pmax(0,pmin(coalesce(orders$release_time,params$sim_length_min),params$sim_length_min)-pmax(orders$order_time,params$warmup_min)))/(params$sim_length_min-params$warmup_min),
         end_unreleased_backlog=sum(is.na(orders$release_time)),end_wip=sum(!arrivals$finished),
         order_lead_time_mean=if(nrow(completed)) mean(completed$end_time-completed$order_time) else NA_real_,
         throughput_per_labor_hr=if(sum(params$operators$count)>0) nrow(completed)/((params$sim_length_min-params$warmup_min)/60)/sum(params$operators$count) else NA_real_,
         # HPU = labor-hours on the clock per completed unit. Lower is better.
         hpu=if(sum(params$operators$count)>0 && nrow(completed)>0) sum(params$operators$count)*((params$sim_length_min-params$warmup_min)/60)/nrow(completed) else Inf),
       stations=stats %>% filter(resource %in% s$name) %>% mutate(replication=rep_id,seed=seed),
       labor=stats %>% filter(startsWith(resource,"operator_")) %>% mutate(replication=rep_id,seed=seed),
       cycle_times=tibble(replication=rep(rep_id,length(ct)),seed=rep(seed,length(ct)),cycle_time=ct),
       resources_ts=res %>% filter(resource %in% s$name) %>% mutate(replication=rep_id,seed=seed),
       arrivals=arrivals %>% transmute(replication=rep_id,seed=seed,name,start_time,end_time,finished),
       all_resources=stats %>% mutate(replication=rep_id),
       orders=orders %>% transmute(replication=rep_id,seed=seed,name,order_time,release_time,end_time,finished),
       waits=bind_rows(tibble(station=character(),type=character(),start=double(),end=double()),events$waits) %>% mutate(end=coalesce(end,params$sim_length_min),replication=rep_id,seed=seed),
       failure_events=bind_rows(tibble(station=character(),time=double(),failure_id=integer()),events$failures) %>% mutate(replication=rep_id),
       repair_events=bind_rows(tibble(station=character(),time=double(),duration=double(),failure_id=integer()),events$repairs) %>% mutate(replication=rep_id))
}
run_line_sim <- function(params=default_params()) {
  if (is.null(params$grid_dt_min) || !is.finite(params$grid_dt_min) || params$grid_dt_min<=0)
    params$grid_dt_min <- 2
  params <- params %>% normalize_params() %>% validate_params()
  results <- tibble(replication=seq_len(params$n_replications)) %>%
    mutate(seed=params$base_seed+replication-1L,result=map2(replication,seed,~run_one_replication(.x,.y,params)))
  summaries <- map_dfr(results$result,"summary"); stations <- map_dfr(results$result,"stations")
  cycles <- map_dfr(results$result,"cycle_times")
  resources_ts <- map_dfr(results$result,"resources_ts")
  labor <- map_dfr(results$result,"labor")
  arrivals <- map_dfr(results$result,"arrivals")
  failure_events <- map_dfr(results$result,"failure_events")
  repair_events <- map_dfr(results$result,"repair_events")
  reps <- results %>% dplyr::select(replication,seed)
  orders <- map_dfr(results$result,"orders");waits<-map_dfr(results$result,"waits")
  shift_summaries <- production_shift_metrics(orders,reps,params)
  release_timeseries <- build_release_timeseries(orders,reps,params)
  station_waits <- crossing(reps,station=params$stations$name,type=c("blocked","waiting_for_operator","waiting_for_automatic_capacity")) %>%
    left_join(waits %>% mutate(minutes=pmax(0,pmin(end,params$sim_length_min)-pmax(start,params$warmup_min))) %>%
      group_by(replication,station,type) %>% summarise(minutes=sum(minutes),.groups="drop"),by=c("replication","station","type")) %>%
    mutate(minutes=replace_na(minutes,0))
  station_headline <- stations %>% group_by(resource) %>% summarise(avg_util_mean=mean(avg_util),avg_util_sd=sd(avg_util),avg_queue_mean=mean(avg_queue),avg_queue_sd=sd(avg_queue),.groups="drop")
  queue_timeseries <- build_queue_timeseries(resources_ts,params,reps)
  wip_timeseries <- build_wip_timeseries(arrivals,reps,params)
  downtime_by_rep <- build_downtime(repair_events,reps,params,failure_events)
  down_sum <- downtime_by_rep %>% group_by(station) %>% summarise(
    downtime_fraction_mean=mean(downtime_fraction),downtime_fraction_sd=sd(downtime_fraction),
    repairs_per_rep_mean=mean(n_repairs),.groups="drop")
  utilization_by_rep <- stations %>% rename(station=resource)
  station_metrics <- station_headline %>% rename(station=resource) %>% left_join(down_sum,by="station")
  # Full upstream bays may be blocked rather than constraining production.
  constraint_scores <- station_waits %>% filter(type=="blocked") %>% group_by(station) %>%
    summarise(blocked_minutes=mean(minutes),.groups="drop") %>%
    left_join(params$stations %>% transmute(station=name,capacity),by="station") %>%
    left_join(station_metrics,by="station") %>% mutate(
      blocked_fraction=blocked_minutes/(capacity*(params$sim_length_min-params$warmup_min)),
      unblocked_occupancy=pmax(0,avg_util_mean-blocked_fraction))
  list(summaries=summaries,shift_summaries=shift_summaries,orders=orders,release_timeseries=release_timeseries,station_waits=station_waits,waits=waits,stations=stations,cycles=cycles,cycle_times=cycles,
       resources_ts=resources_ts,labor=labor,operator_by_rep=labor,
       all_resources=map_dfr(results$result,"all_resources"),
       failure_events=failure_events,repair_events=repair_events,arrivals=arrivals,
       queue_timeseries=queue_timeseries,wip_timeseries=wip_timeseries,
       downtime_by_rep=downtime_by_rep,utilization_by_rep=utilization_by_rep,
       headline=summaries %>% summarise(across(-c(replication,seed),list(mean=~mean(.x,na.rm=TRUE),sd=~sd(.x,na.rm=TRUE)),.names="{.col}_{.fn}")),
       station_headline=station_headline,station_metrics=station_metrics,constraint_scores=constraint_scores,
       bottleneck=constraint_scores %>% slice_max(unblocked_occupancy,n=1,with_ties=FALSE) %>% pull(station),
       params=params,run_parameters=params_to_tibble(params),seeds=results$seed)
}
# Scenarios are stored with their parameters and full results in list-columns.
run_scenarios <- function(scenarios) {
  stopifnot(all(c("scenario","params") %in% names(scenarios)),!anyDuplicated(scenarios$scenario))
  scenarios %>% mutate(result=map(params,run_line_sim),
    comparison=map(result,~.x$summaries %>% summarise(n=n(),throughput=mean(throughput_per_hr),
      se=sd(throughput_per_hr)/sqrt(n()),
      half_width=if(n()>1) qt(.975,n()-1)*se else NA_real_,
      lower=throughput-half_width,upper=throughput+half_width,
      labor_productivity=mean(throughput_per_labor_hr),wip=mean(avg_wip),lead_time=mean(cycle_time_mean,na.rm=TRUE))))
}

# Fixed/reference and high-variation presets preserve each activity's expected time.
variability_scenarios <- function(params=default_params()) {
  current <- params %>% normalize_params() %>% validate_params()
  fixed <- current; variable <- current
  walk(c("load","auto","unload","task"),function(activity) {
    column <- paste0(activity,"_timing")
    fixed$stations[[column]] <<- map(current$stations[[column]],~timing_spec(mean_min=timing_mean(.x)))
    variable$stations[[column]] <<- map(current$stations[[column]],function(spec) {
      m <- timing_mean(spec)
      if (m==0) timing_spec() else timing_spec("distribution",m,m*.5,"lognormal")
    })
  })
  tibble(scenario=c("Current timing","Fixed at same expected times","Lognormal, CV 0.5, same expected times"),
         params=list(current,fixed,variable))
}

# Carry a piecewise-constant monitor forward onto a regular grid.
resample_step <- function(time,value,grid) {
  if (length(time)==0L || all(is.na(time))) return(rep(NA_real_,length(grid)))
  ord <- order(time); time <- time[ord]; value <- value[ord]
  keep <- !duplicated(time,fromLast=TRUE); time <- time[keep]; value <- value[keep]
  if (length(time)==1L) return(ifelse(grid<time,0,as.numeric(value)))
  ifelse(grid<min(time),0,as.numeric(approx(time,value,xout=grid,method="constant",f=0,rule=2)$y))
}
time_grid <- function(params) {
  dt <- params$grid_dt_min %||% 2
  if (!is.finite(dt) || dt<=0) dt <- 2
  grid <- seq(from=params$warmup_min,to=params$sim_length_min,by=dt)
  if (length(grid)==0L || tail(grid,1)<params$sim_length_min-1e-9) grid <- c(grid,params$sim_length_min)
  unique(grid)
}
build_queue_timeseries <- function(resources_ts,params,reps=NULL) {
  grid <- time_grid(params)
  hours <- (grid - params$warmup_min) / 60
  if(is.null(reps)) reps <- resources_ts %>% distinct(replication,seed)
  rows <- vector("list",nrow(reps)*nrow(params$stations))
  k <- 0L
  for (i in seq_len(nrow(reps))) {
    res_r <- resources_ts %>% filter(replication==reps$replication[[i]])
    for (st in params$stations$name) {
      d <- res_r %>% filter(resource==st) %>% arrange(time)
      k <- k+1L
      rows[[k]] <- tibble(replication=reps$replication[[i]],seed=reps$seed[[i]],resource=st,time=grid,
        hours=hours,
        queue=coalesce(resample_step(d$time,d$queue,grid),0),
        server=coalesce(resample_step(d$time,d$server,grid),0))
    }
  }
  bind_rows(rows)
}
# Vehicle-in-system WIP on the same grid. A vehicle counts once, matching avg_wip.
build_wip_timeseries <- function(arrivals,reps,params) {
  grid <- time_grid(params)
  hours <- (grid - params$warmup_min) / 60
  bind_rows(lapply(seq_len(nrow(reps)),function(i) {
    a <- arrivals %>% filter(replication==reps$replication[[i]])
    if (nrow(a)==0) wip <- rep(0,length(grid)) else {
      end_eff <- ifelse(!is.na(a$finished)&a$finished,a$end_time,Inf)
      end_eff[!is.finite(end_eff)] <- Inf
      # count(start<=t) - count(end<=t) == vehicles with start<=t and end>t
      wip <- findInterval(grid,sort(a$start_time)) - findInterval(grid,sort(end_eff))
    }
    tibble(replication=reps$replication[[i]],seed=reps$seed[[i]],time=grid,hours=hours,wip=wip)
  }))
}
# Machine-time down inside the observation window, from sampled repair durations.
# Minutes that straddle the settling boundary are clipped to the window.
# n_repairs counts failures that start inside the window (not warm-up starts).
build_downtime <- function(repairs,reps,params,failures=NULL) {
  warmup <- params$warmup_min; horizon <- params$sim_length_min; window <- horizon-warmup
  caps <- params$stations %>% transmute(station=name,capacity=capacity)
  grid <- crossing(reps,caps)
  if (nrow(repairs)==0) {
    if(is.null(failures) || nrow(failures)==0) return(grid %>% mutate(downtime_min=0,downtime_fraction=0,n_repairs=0L,total_downtime_min=0,waiting_repair_min=0,total_downtime_fraction=0))
  }
  ov <- repairs %>%
    mutate(overlap=pmax(0,pmin(time+duration,horizon)-pmax(time,warmup))) %>%
    group_by(replication,station) %>%
    summarise(downtime_min=sum(overlap),.groups="drop")
  # Observed failures / repairs: onset inside [warmup, horizon).
  if (!is.null(failures) && nrow(failures) > 0) {
    nf <- failures %>%
      filter(is.finite(time), time >= warmup, time < horizon) %>%
      group_by(replication, station) %>%
      summarise(n_repairs = dplyr::n(), .groups = "drop")
  } else if (nrow(repairs) > 0) {
    nf <- repairs %>%
      filter(is.finite(time), time >= warmup, time < horizon) %>%
      group_by(replication, station) %>%
      summarise(n_repairs = dplyr::n(), .groups = "drop")
  } else {
    nf <- tibble(replication = integer(), station = character(), n_repairs = integer())
  }
  if(!is.null(failures)) {
    down<-failures %>% left_join(repairs %>% transmute(replication,failure_id,repair_end=time+duration),by=c("replication","failure_id")) %>%
      mutate(overlap=pmax(0,pmin(coalesce(repair_end,horizon),horizon)-pmax(time,warmup))) %>%
      group_by(replication,station) %>% summarise(total_downtime_min=sum(overlap),.groups="drop")
    grid<-grid %>% left_join(down,by=c("replication","station"))
  } else grid$total_downtime_min<-NA_real_
  grid %>% left_join(ov,by=c("replication","station")) %>%
    left_join(nf, by = c("replication", "station")) %>%
    mutate(downtime_min=replace_na(downtime_min,0),
           n_repairs=as.integer(replace_na(n_repairs,0)),
           downtime_fraction=downtime_min/(capacity*window),
           total_downtime_min=coalesce(total_downtime_min,downtime_min),
           waiting_repair_min=pmax(0,total_downtime_min-downtime_min),
           total_downtime_fraction=total_downtime_min/(capacity*window))
}
timing_label <- function(spec) {
  if (is.null(spec) || !is.list(spec)) return(NA_character_)
  if (identical(spec$method,"empirical"))
    return(sprintf("empirical:n=%d,mean=%.4g",length(spec$observations),mean(spec$observations)))
  if (identical(spec$method,"distribution"))
    return(sprintf("%s:mean=%.4g,sd=%.4g",spec$distribution,spec$mean_min,spec$sd_min %||% NA_real_))
  sprintf("fixed:%.4g",spec$mean_min)
}
# Flat parameter table safe for CSV (timing specs become text, not list-columns).
params_to_tibble <- function(params) {
  params <- params %>% normalize_params()
  note <- "Example parameters"
  run_cols <- tibble(sim_length_min=params$sim_length_min,n_replications=as.integer(params$n_replications),
    base_seed=as.integer(params$base_seed),warmup_min=params$warmup_min,arrival_mean_min=params$arrival_mean_min,
    time_mode=params$time_mode,shift_length_min=params$shift_length_min,n_shifts=params$n_shifts,
    release_policy=params$release_policy,entry_buffer=params$entry_buffer,wip_limit=params$wip_limit,order_spacing=params$order_spacing,
    grid_dt_min=params$grid_dt_min %||% 2,repair_technicians=as.integer(params$repair_technicians %||% 1L))
  st <- params$stations
  chr <- function(col) vapply(st[[col]],timing_label,character(1))
  stations <- tibble(param_group="station",station=st$name,capacity=st$capacity,buffer_before=st$buffer_before,dist=st$dist,
    mean_min=st$mean_min,sd_min=st$sd_min,operator_pool=st$operator_pool,load_min=st$load_min,
    unload_min=st$unload_min,mtbf_min=st$mtbf_min,mttr_min=st$mttr_min,
    task_interval_min=st$task_interval_min,task_duration_min=st$task_duration_min,task_every_n=st$task_every_n,
    load_timing=chr("load_timing"),auto_timing=chr("auto_timing"),
    unload_timing=chr("unload_timing"),task_timing=chr("task_timing"),note=note)
  stations <- bind_cols(stations,run_cols[rep(1,nrow(stations)),])
  ops <- if (nrow(params$operators)) {
    tibble(param_group="operator",station=params$operators$pool,capacity=params$operators$count,note=note)
  } else tibble(param_group=character(),station=character(),capacity=integer(),note=character())
  run_row <- bind_cols(tibble(param_group="run",station=NA_character_,note=note),run_cols)
  bind_rows(run_row,stations,ops)
}
sim_export_tables <- function(res) {
  list(queue_timeseries=res$queue_timeseries,wip_timeseries=res$wip_timeseries,
       utilization_by_rep=res$utilization_by_rep,cycle_times=res$cycle_times,
       station_metrics=res$station_metrics,run_parameters=res$run_parameters,
       summaries=res$summaries,failure_events=res$failure_events,
       repair_events=res$repair_events,downtime_by_rep=res$downtime_by_rep,
       operator_by_rep=res$operator_by_rep,shift_summaries=res$shift_summaries,
       orders=res$orders,release_timeseries=res$release_timeseries,station_waits=res$station_waits)
}
#' Write every export table as CSV. Optionally bundle them into a zip.
write_sim_exports <- function(res,dir,zip_path=NULL) {
  dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  tables <- sim_export_tables(res)
  paths <- character(0)
  for (nm in names(tables)) {
    p <- file.path(dir,paste0(nm,".csv"))
    readr::write_csv(tables[[nm]],p)
    paths <- c(paths,p)
  }
  if (!is.null(zip_path)) {
    zip_dir <- dirname(zip_path)
    if (!dir.exists(zip_dir) && nzchar(zip_dir) && zip_dir!=".") dir.create(zip_dir,recursive=TRUE,showWarnings=FALSE)
    if (file.exists(zip_path)) file.remove(zip_path)
    csv_basenames <- paste0(names(tables),".csv")
    if (requireNamespace("zip",quietly=TRUE)) {
      zip::zipr(zipfile=zip_path,files=csv_basenames,root=dir)
    } else {
      owd <- setwd(dir); on.exit(setwd(owd),add=TRUE)
      utils::zip(zipfile=zip_path,files=csv_basenames,flags="-jq")
    }
  }
  invisible(paths)
}

# Shifts are consecutive production periods, not a wall-clock night/weekend calendar.
production_shift_metrics <- function(orders,reps,params) {
  crossing(reps,shift=seq_len(params$n_shifts)) %>% mutate(
    start=params$warmup_min+(shift-1)*params$shift_length_min,end=start+params$shift_length_min) %>%
    pmap_dfr(function(replication,seed,shift,start,end) {
      # Explicit local argument avoids ambiguity with dplyr's data-mask column.
      a<-orders[orders$replication==replication,]
      released<-a$release_time; completed<-ifelse(a$finished,a$end_time,Inf)
      stock<-function(t) sum(!is.na(released)&released<t&completed>=t)
      backlog<-function(t) sum(a$order_time<t&(is.na(released)|released>=t))
      done<-a$finished & a$end_time>=start & a$end_time<end
      tibble(replication=replication,seed=seed,shift=shift,start=start,end=end,
        orders_due=sum(a$order_time>=start&a$order_time<end),
        n_released=sum(!is.na(released)&released>=start&released<end),n_completed=sum(done),
        throughput_per_hr=sum(done)/((end-start)/60),start_wip=stock(start),end_wip=stock(end),
        start_backlog=backlog(start),end_backlog=backlog(end),
        avg_wip=sum(pmax(0,pmin(completed,end)-pmax(coalesce(released,end),start)))/(end-start),
        release_lead_time=if(any(done)) mean(a$end_time[done]-released[done]) else NA_real_,
        order_lead_time=if(any(done)) mean(a$end_time[done]-a$order_time[done]) else NA_real_)
    })
}
build_release_timeseries <- function(orders,reps,params) {
  grid<-time_grid(params)
  hours <- (grid - params$warmup_min) / 60
  map2_dfr(reps$replication,reps$seed,function(id,seed) {
    a<-orders[orders$replication==id,]
    due<-findInterval(grid,sort(a$order_time));released<-findInterval(grid,sort(na.omit(a$release_time)))
    finished<-findInterval(grid,sort(a$end_time[a$finished]))
    tibble(replication=id,seed=seed,time=grid,hours=hours,unreleased_backlog=due-released,wip=released-finished,
      cumulative_orders=due,cumulative_releases=released,cumulative_completions=finished)
  })
}
