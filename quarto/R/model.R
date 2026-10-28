# Portfolio demonstration; all defaults are illustrative, not measured plant data.
suppressPackageStartupMessages({library(simmer); library(tidyverse)})
sim_now <- function(env) simmer::now(env)
default_params <- function() {
  list(sim_length_min=480, n_replications=5L, base_seed=20261027L,
       warmup_min=60, arrival_mean_min=4, repair_technicians=1L,
       operators=tibble(pool=c("upstream", "downstream"), count=c(2L,2L)),
       stations=tribble(
         ~name, ~capacity, ~dist, ~mean_min, ~sd_min, ~operator_pool, ~load_min, ~unload_min, ~mtbf_min, ~mttr_min, ~task_interval_min, ~task_duration_min,
         "body_shop",2L,"exponential",6,NA_real_,"upstream",1,0.5,240,15,60,4,
         "paint",1L,"normal",8,1.5,"upstream",0.5,0.5,360,20,90,5,
         "final_assembly",2L,"normal",7,1,"downstream",2,1,300,10,45,4,
         "inspection",1L,"exponential",3,NA_real_,"downstream",1,0.5,Inf,0,60,3))
}
make_service_fn <- function(dist, mean_min, sd_min) {
  force(dist); force(mean_min); force(sd_min)
  switch(dist, exponential=function() rexp(1,1/mean_min),
         normal=function() max(0.1,rnorm(1,mean_min,sd_min)),
         deterministic=function() mean_min, stop("Unknown distribution: ",dist))
}
# Old app/Quarto parameter lists remain usable, with labor/reliability disabled.
normalize_params <- function(p) {
  defaults <- list(operator_pool=NA_character_,load_min=0,unload_min=0,
                   mtbf_min=Inf,mttr_min=0,task_interval_min=Inf,task_duration_min=0,task_every_n=0L)
  walk(names(defaults),function(n) {if (!n %in% names(p$stations)) p$stations[[n]] <<- defaults[[n]]})
  if (is.null(p$operators)) p$operators <- tibble(pool=character(),count=integer())
  if (is.null(p$repair_technicians)) p$repair_technicians <- 1L
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
  record_failure <- function(n) {events$failures[[length(events$failures)+1L]] <- tibble(station=n,time=sim_now(env)); 0}
  record_repair <- function(n,d) {events$repairs[[length(events$repairs)+1L]] <- tibble(station=n,time=sim_now(env),duration=d); d}
  walk2(params$operators$pool,params$operators$count,~add_resource(env,paste0("operator_",.x),.y))
  add_resource(env,"maintenance",params$repair_technicians)
  walk2(s$name,s$capacity, function(n,c) {
    add_resource(env,n,c); add_resource(env,paste0("auto_",n),c,preemptive=TRUE)
  })
  # Distinct automatic capacity permits resume after failure without retaining labor.
  trajectory_for_station <- function(st) {
    n <- st$name; op <- paste0("operator_",st$operator_pool)
    t <- trajectory(n) %>% seize(n)
    manual <- function(t,duration) {
      if (is.na(st$operator_pool)) return(t %>% timeout(duration))
      t %>% seize(op) %>% timeout(duration) %>% release(op)
    }
    t <- manual(t,st$load_min)
    t <- t %>% seize(paste0("auto_",n)) %>% timeout(make_service_fn(st$dist,st$mean_min,st$sd_min)) %>% release(paste0("auto_",n))
    t <- manual(t,st$unload_min) %>% release(n)
    if (!is.na(st$operator_pool) && st$task_every_n>0 && st$task_duration_min>0) {
      t <- t %>% set_global(paste0("units_",n),1,mod="+") %>%
        branch(function() as.integer(get_global(env,paste0("units_",n)) %% st$task_every_n==0),continue=TRUE,
          trajectory() %>% seize(op) %>% timeout(st$task_duration_min) %>% release(op))
    }
    t
  }
  vehicle <- reduce(seq_len(nrow(s)),function(t,i) join(t,trajectory_for_station(s[i,])),.init=trajectory("vehicle"))
  # One independent alternating up/repair process per interchangeable machine.
  walk(seq_len(nrow(s)),function(i) {
    st <- s[i,]; n <- st$name
    if (is.finite(st$mtbf_min) && st$mttr_min>0) {
      failure <- trajectory("failure") %>% timeout(function() rexp(1,1/st$mtbf_min)) %>%
        timeout(function() record_failure(n)) %>%
        seize(paste0("auto_",n)) %>% seize("maintenance") %>%
        timeout(function() record_repair(n,rexp(1,1/st$mttr_min))) %>% release("maintenance") %>%
        release(paste0("auto_",n)) %>% simmer::rollback(7,Inf)
      walk(seq_len(st$capacity),function(j) add_generator(env,paste0("failure_",n,"_",j,"_"),failure,at(0),priority=100,preemptible=100,restart=FALSE))
    }
    if (!is.na(st$operator_pool) && st$task_every_n==0 && is.finite(st$task_interval_min) && st$task_duration_min>0) {
      task <- trajectory("noncyclic") %>% seize(paste0("operator_",st$operator_pool)) %>%
        timeout(st$task_duration_min) %>% release(paste0("operator_",st$operator_pool))
      add_generator(env,paste0("task_",n,"_"),task,function() rexp(1,1/st$task_interval_min))
    }
  })
  add_generator(env,"veh",vehicle,function() rexp(1,1/params$arrival_mean_min),mon=2,restart=FALSE)
  run(env,until=params$sim_length_min)
  arrivals <- get_mon_arrivals(env,ongoing=TRUE) %>% as_tibble() %>% filter(startsWith(name,"veh"))
  completed <- arrivals %>% filter(finished,end_time>=params$warmup_min)
  # Lead times use departures in the observation window, including pre-warmup entrants.
  ct <- completed$end_time-completed$start_time
  res <- get_mon_resources(env) %>% as_tibble()
  stats <- resource_stats(res,params$warmup_min,params$sim_length_min)
  # Arrival-based WIP counts each vehicle once, even when holding several resources.
  wip <- sum(pmax(0,pmin(if_else(arrivals$finished,arrivals$end_time,params$sim_length_min),params$sim_length_min)-pmax(arrivals$start_time,params$warmup_min)))/(params$sim_length_min-params$warmup_min)
  q <- function(prob) if(length(ct)) unname(quantile(ct,prob)) else NA_real_
  list(summary=tibble(replication=rep_id,seed=seed,n_completed=nrow(completed),
         throughput_per_hr=nrow(completed)/((params$sim_length_min-params$warmup_min)/60),
         cycle_time_mean=if(length(ct)) mean(ct) else NA_real_,cycle_time_p50=q(.5),cycle_time_p90=q(.9),cycle_time_p95=q(.95),avg_wip=wip,
         throughput_per_labor_hr=if(sum(params$operators$count)>0) nrow(completed)/((params$sim_length_min-params$warmup_min)/60)/sum(params$operators$count) else NA_real_),
       stations=stats %>% filter(resource %in% s$name) %>% mutate(replication=rep_id,seed=seed),
       labor=stats %>% filter(startsWith(resource,"operator_")) %>% mutate(replication=rep_id),
       cycle_times=tibble(replication=rep(rep_id,length(ct)),cycle_time=ct),
       resources_ts=res %>% filter(resource %in% s$name) %>% mutate(replication=rep_id),
       all_resources=stats %>% mutate(replication=rep_id),
       failure_events=bind_rows(tibble(station=character(),time=double()),events$failures) %>% mutate(replication=rep_id),
       repair_events=bind_rows(tibble(station=character(),time=double(),duration=double()),events$repairs) %>% mutate(replication=rep_id))
}
run_line_sim <- function(params=default_params()) {
  params <- params %>% normalize_params() %>% validate_params()
  results <- tibble(replication=seq_len(params$n_replications)) %>%
    mutate(seed=params$base_seed+replication-1L,result=map2(replication,seed,~run_one_replication(.x,.y,params)))
  summaries <- map_dfr(results$result,"summary"); stations <- map_dfr(results$result,"stations")
  station_headline <- stations %>% group_by(resource) %>% summarise(avg_util_mean=mean(avg_util),avg_util_sd=sd(avg_util),avg_queue_mean=mean(avg_queue),avg_queue_sd=sd(avg_queue),.groups="drop")
  list(summaries=summaries,stations=stations,cycles=map_dfr(results$result,"cycle_times"),
       resources_ts=results$result[[1]]$resources_ts,labor=map_dfr(results$result,"labor"),
       all_resources=map_dfr(results$result,"all_resources"),
       failure_events=map_dfr(results$result,"failure_events"),repair_events=map_dfr(results$result,"repair_events"),
       headline=summaries %>% summarise(across(-c(replication,seed),list(mean=~mean(.x,na.rm=TRUE),sd=~sd(.x,na.rm=TRUE)),.names="{.col}_{.fn}")),
       station_headline=station_headline,
       bottleneck=station_headline %>% slice_max(avg_util_mean,n=1,with_ties=FALSE) %>% pull(resource),params=params)
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
