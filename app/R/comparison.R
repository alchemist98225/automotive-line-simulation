# Two editable parameter snapshots; no changes to the simulation engine.
comparison_colors <- c("Set 1"="#2474A5", "Set 2"="#D46B25")
comparison_activities <- c(load="Loading",auto="Automatic processing",unload="Unloading",task="Support work")
comparison_globals <- c(sim_length_min="Run length (min)",warmup_min="Warmup (min)",
  n_replications="Replications",base_seed="Base seed",arrival_mean_min="Mean interarrival (min)",
  grid_dt_min="Time-series interval (min)",repair_technicians="Repair technicians")
comparison_pools <- function(p) unique(c(p$operators$pool,"upstream","downstream","shared",p$stations$name))
# Lossless representations are also used to detect stale results, including empirical vectors.
comparison_flatten <- function(p) {
  p <- normalize_params(p)
  scalar <- function(x) paste(format(x,digits=17,trim=TRUE),collapse=", ")
  timing <- function(x) switch(x$method,
    fixed=paste("fixed",scalar(x$mean_min)),
    empirical=paste("empirical",scalar(x$observations)),
    distribution=paste(x$distribution,scalar(x$mean_min),if(x$distribution!="exponential") scalar(x$sd_min) else ""))
  run <- tibble(parameter=paste0("Run / ",unname(comparison_globals)),value=map_chr(names(comparison_globals),~scalar(p[[.x]] %||% 2)))
  ops <- p$operators %>% transmute(parameter=paste0("Operators / ",pool),value=map_chr(count,scalar))
  stations <- map_dfr(seq_len(nrow(p$stations)),function(i) {
    st <- p$stations[i,]
    cols <- c("capacity","operator_pool","mtbf_min","mttr_min","task_interval_min","task_every_n")
    bind_rows(tibble(parameter=paste(label_station(st$name),c("Parallel bays","Operator pool","Mean up-time (min)","Mean repair (min)","Task interval (min)","Task every N units"),sep=" / "),value=map_chr(cols,~scalar(st[[.x]]))),
      tibble(parameter=paste(label_station(st$name),unname(comparison_activities),sep=" / "),
        value=map_chr(names(comparison_activities),~timing(activity_timing(st,.x)))))
  })
  bind_rows(run,ops,stations) %>% arrange(parameter)
}
comparison_same <- function(a,b) identical(comparison_flatten(a),comparison_flatten(b))
comparison_diff <- function(a,b) {
  full_join(comparison_flatten(a) %>% rename(`Set 1`=value),
    comparison_flatten(b) %>% rename(`Set 2`=value),by="parameter") %>%
    mutate(across(c(`Set 1`,`Set 2`),~replace_na(.x,"Not used"))) %>%
    filter(`Set 1`!=`Set 2`)
}

parameter_editor_ui <- function(id) uiOutput(NS(id,"controls"))
parameter_editor_server <- function(id) moduleServer(id,function(input,output,session) {
  state <- reactiveVal(list(revision=0L,params=normalize_params(default_params())))
  set_params <- function(p) {
    p <- normalize_params(p) %>% validate_params()
    state(list(revision=isolate(state()$revision)+1L,params=p))
  }
  output$controls <- renderUI({
    p <- state()$params; revision <- state()$revision
    key <- function(x) paste0("v",revision,"_",x)
    ns <- function(x) session$ns(key(x))
    pools <- comparison_pools(p)
    activity_ui <- function(st,a) {
      spec <- activity_timing(st,a); prefix <- paste(st$name,a,sep="_")
      method_id <- key(paste0(prefix,"_method"))
      mean_value <- if(spec$method=="empirical") mean(spec$observations) else spec$mean_min
      tagList(
        selectInput(ns(paste0(prefix,"_method")),"Timing method",
          c("Fixed"="fixed","Distribution"="distribution","Measured observations"="empirical"),selected=spec$method),
        conditionalPanel(sprintf("input['%s'] !== 'empirical'",method_id),ns=session$ns,
          numericInput(ns(paste0(prefix,"_mean")),"Duration / mean (min)",mean_value,min=0,step=.1)),
        conditionalPanel(sprintf("input['%s'] === 'distribution'",method_id),ns=session$ns,
          selectInput(ns(paste0(prefix,"_distribution")),"Distribution",c("Normal, floored at zero"="normal","Lognormal"="lognormal","Exponential"="exponential"),
            selected=if(spec$method=="distribution") spec$distribution else "normal"),
          numericInput(ns(paste0(prefix,"_sd")),"SD (min; ignored for exponential)",if(is.finite(spec$sd_min)) spec$sd_min else 0,min=0,step=.1)),
        conditionalPanel(sprintf("input['%s'] === 'empirical'",method_id),ns=session$ns,
          textAreaInput(ns(paste0(prefix,"_observations")),"Observed minutes (commas, spaces or newlines)",
            value=if(spec$method=="empirical") paste(format(spec$observations,digits=17),collapse=", ") else "",rows=3)))
    }
    accordion(id=session$ns("editor_panels"),open=FALSE,multiple=TRUE,
      accordion_panel("Run settings",tagList(map(names(comparison_globals),function(n) {
        numericInput(ns(n),comparison_globals[[n]],p[[n]] %||% 2,
          min=if(n %in% c("warmup_min","base_seed")) 0 else if(n %in% c("sim_length_min","arrival_mean_min","grid_dt_min")) .001 else 1,
          step=if(n %in% c("arrival_mean_min","grid_dt_min")) .1 else 1)
      }))),
      accordion_panel("Operators & assignments",
        p("A pool can serve several stations. Count 0 removes an unused pool. Every assigned pool needs at least one operator."),
        tagList(map(pools,function(pool) numericInput(ns(paste0("pool_",pool)),paste(pool,"operators"),
          if(pool %in% p$operators$pool) p$operators$count[match(pool,p$operators$pool)] else 0,min=0,step=1))),
        tagList(map(seq_len(nrow(p$stations)),function(i) selectInput(ns(paste0("assignment_",p$stations$name[i])),
          paste(label_station(p$stations$name[i]),"operator pool"),choices=c("No labor constraint"="",setNames(pools,pools)),selected=if(is.na(p$stations$operator_pool[i])) "" else p$stations$operator_pool[i])))),
      accordion_panel("Stations & activity timing",tagList(map(seq_len(nrow(p$stations)),function(i) {
        st <- p$stations[i,]
        accordion(open=FALSE,multiple=TRUE,accordion_panel(label_station(st$name),
          numericInput(ns(paste0("capacity_",st$name)),"Parallel bays",st$capacity,min=1,step=1),
          accordion(open=FALSE,multiple=TRUE,!!!map(names(comparison_activities),function(a)
            accordion_panel(comparison_activities[[a]],activity_ui(st,a))))))
      }))),
      accordion_panel("Reliability & support triggers",tagList(map(seq_len(nrow(p$stations)),function(i) {
        st <- p$stations[i,]; n <- st$name
        tagList(h6(label_station(n)),
          checkboxInput(ns(paste0("failures_",n)),"Equipment failures",is.finite(st$mtbf_min) && st$mttr_min>0),
          numericInput(ns(paste0("mtbf_",n)),"Mean up-time (min)",if(is.finite(st$mtbf_min)) st$mtbf_min else 10000,min=.001),
          numericInput(ns(paste0("mttr_",n)),"Mean active repair (min; 0 disables)",st$mttr_min,min=0),
          checkboxInput(ns(paste0("support_",n)),"Support work enabled",timing_mean(activity_timing(st,"task"))>0 && (st$task_every_n>0 || is.finite(st$task_interval_min))),
          numericInput(ns(paste0("interval_",n)),"Random task mean interval (min)",if(is.finite(st$task_interval_min)) st$task_interval_min else 60,min=.001),
          numericInput(ns(paste0("every_",n)),"Task every N completions (0 = random)",st$task_every_n,min=0,step=1))
      }))))
  })
  outputOptions(output,"controls",suspendWhenHidden=FALSE)
  params <- reactive({
    p <- state()$params; revision <- state()$revision
    val <- function(id,fallback) input[[paste0("v",revision,"_",id)]] %||% fallback
    pools <- comparison_pools(p)
    walk(names(comparison_globals),function(n) p[[n]] <<- val(n,p[[n]] %||% 2))
    p$operators <- tibble(pool=pools,count=map_dbl(pools,function(pool)
      val(paste0("pool_",pool),if(pool %in% p$operators$pool) p$operators$count[match(pool,p$operators$pool)] else 0)))
    stopifnot(all(is.finite(p$operators$count) & p$operators$count>=0 & p$operators$count==floor(p$operators$count)))
    p$operators <- p$operators %>% filter(count>0)
    st <- p$stations
    read_spec <- function(i,a) {
      spec <- activity_timing(st[i,],a); prefix <- paste(st$name[i],a,sep="_")
      method <- val(paste0(prefix,"_method"),spec$method)
      if(method=="empirical") return(timing_spec("empirical",observations=parse_observations(
        val(paste0(prefix,"_observations"),if(spec$method=="empirical") paste(spec$observations,collapse=",") else ""))))
      timing_spec(method,mean_min=val(paste0(prefix,"_mean"),if(spec$method=="empirical") mean(spec$observations) else spec$mean_min),
        sd_min=val(paste0(prefix,"_sd"),if(is.finite(spec$sd_min)) spec$sd_min else 0),
        distribution=val(paste0(prefix,"_distribution"),if(spec$method=="distribution") spec$distribution else "normal"))
    }
    walk(seq_len(nrow(st)),function(i) {
      n <- st$name[i]
      p$stations$capacity[i] <<- val(paste0("capacity_",n),st$capacity[i])
      pool <- val(paste0("assignment_",n),if(is.na(st$operator_pool[i])) "" else st$operator_pool[i])
      p$stations$operator_pool[i] <<- if(pool=="") NA_character_ else pool
      enabled <- val(paste0("failures_",n),is.finite(st$mtbf_min[i]) && st$mttr_min[i]>0)
      p$stations$mtbf_min[i] <<- if(enabled) val(paste0("mtbf_",n),st$mtbf_min[i]) else Inf
      p$stations$mttr_min[i] <<- val(paste0("mttr_",n),st$mttr_min[i])
      support <- val(paste0("support_",n),timing_mean(activity_timing(st[i,],"task"))>0 && (st$task_every_n[i]>0 || is.finite(st$task_interval_min[i])))
      p$stations$task_interval_min[i] <<- if(support) val(paste0("interval_",n),st$task_interval_min[i]) else Inf
      p$stations$task_every_n[i] <<- val(paste0("every_",n),st$task_every_n[i])
      walk(names(comparison_activities),function(a) p$stations[[paste0(a,"_timing")]][[i]] <<- if(a=="task" && !support) timing_spec() else read_spec(i,a))
    })
    # Import/read roundtrips preserve normal input means, not their floored expectations.
    validate_params(p)
  })
  list(params=params,set_params=set_params,state=state)
})

comparison_page_ui <- function(id) {
  ns <- NS(id)
  nav_panel("Compare two sets",value="compare_sets",
    div(class="p-3",
      h3("Compare two parameter sets"),
      p("Start from the main page, change one parameter at a time, and compare output and flow. Set 1 and Set 2 remain independent of the main page after import."),
      div(class="d-flex flex-wrap gap-2 mb-3",
        actionButton(ns("import"),"Import main settings into both sets"),
        actionButton(ns("clone"),"Copy Set 1 to Set 2"),
        actionButton(ns("match_reps"),"Match Set 2 replications to Set 1"),
        actionButton(ns("run"),"Run comparison",class="btn-primary")),
      uiOutput(ns("status")),
      layout_columns(col_widths=c(6,6),fill=FALSE,
        card(fill=FALSE,card_header(tags$span(style="color:#2474A5","Set 1 — baseline")),parameter_editor_ui(ns("one"))),
        card(fill=FALSE,card_header(tags$span(style="color:#D46B25","Set 2 — study")),parameter_editor_ui(ns("two")))),
      accordion(open=FALSE,accordion_panel("Parameter differences",DTOutput(ns("differences")))),
      p(class="text-muted small","Each run freezes both parameter sets. Reused seeds do not synchronize random draws by event. Differences are descriptive; intervals describe each set’s mean, not the uncertainty of the difference. Default values are illustrative."),
      uiOutput(ns("kpis")),
      navset_card_tab(
        nav_panel("Output distributions",selectInput(ns("metric"),"Metric",c("Throughput (vehicles/hour)"="throughput_per_hr","Lead time (min)"="cycle_time_mean","Average WIP (vehicles)"="avg_wip","Output per operator-hour"="throughput_per_labor_hr")),plotOutput(ns("distribution"),height=440)),
        nav_panel("Lead-time curves",plotOutput(ns("cycles"),height=440)),
        nav_panel("Bay occupancy",plotOutput(ns("occupancy"),height=550)),
        nav_panel("Operators",plotOutput(ns("labor"),height=550)),
        nav_panel("WIP & queues",plotOutput(ns("wip"),height=360),plotOutput(ns("queues"),height=550)),
        nav_panel("Repair time",p("Active repair fraction only; technician waiting is not included in this existing engine metric."),plotOutput(ns("downtime"),height=550)),
        nav_panel("Tables & download",DTOutput(ns("summary")),h5("Station metrics"),DTOutput(ns("stations")),
          downloadButton(ns("download"),"Download both runs and exact inputs (.rds)")))))
}

comparison_page_server <- function(id,main_params,active_page) moduleServer(id,function(input,output,session) {
  one <- parameter_editor_server("one"); two <- parameter_editor_server("two")
  initialized <- reactiveVal(FALSE); results <- reactiveVal(NULL); error <- reactiveVal(NULL)
  import_main <- function() {
    p <- isolate(main_params()) %>% normalize_params() %>% validate_params()
    one$set_params(p); two$set_params(p); initialized(TRUE); error(NULL)
  }
  observeEvent(active_page(), {if(identical(active_page(),"compare_sets") && !initialized()) import_main()},ignoreNULL=TRUE)
  observeEvent(input$import, {import_main()})
  observeEvent(input$clone, {two$set_params(one$params());error(NULL)})
  observeEvent(input$match_reps, {p<-two$params();p$n_replications<-one$params()$n_replications;two$set_params(p);error(NULL)})
  draft <- reactive({tryCatch(list(one=one$params(),two=two$params()),error=function(e) e)})
  dirty <- reactive({r<-results(); d<-draft(); !is.null(r) && (inherits(d,"error") || !comparison_same(d$one,r$params[[1]]) || !comparison_same(d$two,r$params[[2]]))})
  observeEvent(input$run, {
    d <- draft()
    if(inherits(d,"error")) {error(conditionMessage(d));return()}
    error(NULL)
    tryCatch({
      r <- withProgress(message="Running both parameter sets…",value=0,{
        a<-run_line_sim(d$one);incProgress(.5,detail="Set 1 complete")
        b<-run_line_sim(d$two);incProgress(.5,detail="Set 2 complete")
        list(params=list(a$params,b$params),runs=list(a,b),at=format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"))
      })
      results(r)
    },error=function(e) error(conditionMessage(e)))
  })
  output$status <- renderUI({
    d<-draft(); r<-results()
    if(!is.null(error())) return(div(class="alert alert-danger",paste("Comparison not updated:",error())))
    if(inherits(d,"error")) return(div(class="alert alert-warning",paste("Check the parameter inputs:",conditionMessage(d))))
    design <- c("sim_length_min","warmup_min","n_replications","arrival_mean_min")
    mismatches <- design[map_lgl(design,~!identical(as.numeric(d$one[[.x]]),as.numeric(d$two[[.x]])))]
    tagList(
      if(dirty()) div(class="alert alert-warning",strong("Settings changed. Rerun the comparison.")," Charts below still show the previous saved run."),
      if(is.null(r)) div(class="alert alert-info","Both sets are ready to edit. Press Run comparison to generate results."),
      if(!is.null(r)) p(class="text-muted",paste("Results captured:",r$at)),
      if(length(mismatches)) div(class="alert alert-info",paste("Different run conditions:",paste(unname(comparison_globals[mismatches]),collapse=", "),"— interpret differences in that context.")))
  })
  output$differences <- renderDT({d<-draft();req(!inherits(d,"error"));datatable(comparison_diff(d$one,d$two),rownames=FALSE,options=list(dom="t",scrollX=TRUE))})
  table_for <- function(field) {
    r<-results();req(!is.null(r)); map2_dfr(r$runs,names(comparison_colors),~mutate(.x[[field]],set=.y))
  }
  output$kpis <- renderUI({
    r<-results();req(!is.null(r))
    metrics<-c(throughput_per_hr="Throughput / hour",cycle_time_mean="Mean lead time / min",avg_wip="Average WIP",throughput_per_labor_hr="Output / operator-hour")
    layout_columns(fill=FALSE,col_widths=c(3,3,3,3),!!!map(names(metrics),function(m) {
      values<-map_dbl(r$runs,~mean(.x$summaries[[m]],na.rm=TRUE));delta<-values[2]-values[1]
      show<-function(x) if(is.finite(x)) sprintf("%.2f",x) else "Unavailable"
      value_box(title=metrics[[m]],value=tags$div(style="font-size:1.1rem",tags$div(style="color:#2474A5",paste("Set 1:",show(values[1]))),tags$div(style="color:#D46B25",paste("Set 2:",show(values[2])))),
        p(paste("Set 2 − Set 1:",show(delta))),
        p(if(is.finite(values[1]) && values[1]!=0 && is.finite(delta)) sprintf("Change: %+.1f%%",100*delta/values[1]) else "Percent change unavailable"))
    }))
  })
  colored <- function(p,fill=TRUE) {
    if(fill) p<-p+scale_fill_manual(values=comparison_colors,drop=FALSE)
    p+scale_color_manual(values=comparison_colors,drop=FALSE)+theme_minimal(base_size=13)+theme(legend.position="bottom")
  }
  rain <- function(df,value,facet=NULL,label=value) {
    df<-df %>% mutate(value=.data[[value]],set=factor(set,levels=names(comparison_colors))) %>% filter(is.finite(value))
    validate(need(nrow(df)>0,"No completed observations for this metric."))
    p<-ggplot(df,aes(x=set,y=value,fill=set,color=set))
    groups<-df %>% group_by(across(any_of(c("set",facet)))) %>% summarise(n=n(),unique_values=n_distinct(value),.groups="drop")
    if(all(groups$n>=3 & groups$unique_values>=2)) p<-p+ggrain::geom_rain(alpha=.35,seed=20261028)
    else p<-p+geom_boxplot(width=.35,outlier.shape=NA)+geom_point(position=position_jitter(width=.05,height=0,seed=20261028),alpha=.6)
    p<-p+coord_flip()+labs(x=NULL,y=label,fill=NULL,color=NULL,subtitle="Each observation is one replication; identical sets should agree.")
    if(!is.null(facet)) p<-p+facet_wrap(vars(!!sym(facet)),ncol=2,labeller=labeller(.default=label_station))
    colored(p)
  }
  output$distribution <- renderPlot({
    labels<-c(throughput_per_hr="Throughput (vehicles/hour)",cycle_time_mean="Mean lead time (min)",avg_wip="Average WIP (vehicles)",throughput_per_labor_hr="Output per operator-hour")
    metric<-input$metric %||% "throughput_per_hr"
    rain(table_for("summaries"),metric,label=labels[[metric]])
  })
  output$occupancy <- renderPlot({rain(table_for("utilization_by_rep"),"avg_util","station","Bay occupancy fraction")})
  output$labor <- renderPlot({rain(table_for("operator_by_rep"),"avg_util","resource","Operator utilization fraction")})
  output$downtime <- renderPlot({rain(table_for("downtime_by_rep"),"downtime_fraction","station","Active repair fraction")})
  output$cycles <- renderPlot({
    df<-table_for("cycle_times") %>% filter(is.finite(cycle_time))
    validate(need(nrow(df)>0,"No completed vehicles in either set."))
    p<-ggplot(df,aes(cycle_time,fill=set,color=set))
    if(all((df %>% count(set))$n>=2) && n_distinct(df$set)==2) p<-p+geom_density(alpha=.18,linewidth=1)
    else p<-p+geom_histogram(position="identity",alpha=.3,bins=20)
    colored(p+labs(x="Vehicle lead time (min)",y="Density / count",fill=NULL,color=NULL,
      subtitle="Pooled departure cohorts; unfinished vehicles are excluded."))
  })
  flow_plot <- function(field,column,facet=NULL) {
    df<-table_for(field)
    validate(need(nrow(df)>0,"No time-series observations."))
    by<-c("set","time",facet)
    means<-df %>% group_by(across(all_of(by))) %>% summarise(value=mean(.data[[column]]),.groups="drop")
    p<-ggplot(means,aes(time,value,color=set))+geom_step(linewidth=.9)+labs(x="Simulation time (min)",y=if(column=="wip") "WIP (vehicles)" else "Queue (vehicles)",color=NULL,
      subtitle="Mean across replications at each time; each set uses its own observation window.")
    if(!is.null(facet)) p<-p+facet_wrap(vars(!!sym(facet)),ncol=2)
    colored(p,fill=FALSE)
  }
  output$wip <- renderPlot({flow_plot("wip_timeseries","wip")})
  output$queues <- renderPlot({flow_plot("queue_timeseries","queue","resource")})
  summary_data <- reactive({table_for("summaries") %>% group_by(set) %>% summarise(
    replications=n(),throughput=mean(throughput_per_hr),throughput_se=sd(throughput_per_hr)/sqrt(n()),
    ci_half=if(n()>1) qt(.975,n()-1)*throughput_se else NA_real_,
    throughput_lower=throughput-ci_half,throughput_upper=throughput+ci_half,
    mean_lead_time=mean(cycle_time_mean,na.rm=TRUE),mean_wip=mean(avg_wip),
    output_per_operator_hour=mean(throughput_per_labor_hr,na.rm=TRUE),.groups="drop")})
  output$summary <- renderDT({
    df<-summary_data();names(df)<-c("Parameter set","Replications","Throughput / hour","Standard error","95% CI half-width","95% CI lower","95% CI upper","Mean lead time / min","Average WIP","Output / operator-hour")
    datatable(df,rownames=FALSE,options=list(dom="t",scrollX=TRUE)) %>% formatRound(columns=3:10,digits=2)
  })
  output$stations <- renderDT({
    df<-table_for("station_metrics") %>% mutate(station=label_station(station)) %>%
      rename(Station=station,`Parameter set`=set,`Mean occupancy`=avg_util_mean,`Occupancy SD`=avg_util_sd,
        `Mean queue`=avg_queue_mean,`Queue SD`=avg_queue_sd,`Mean active repair fraction`=downtime_fraction_mean,
        `Repair fraction SD`=downtime_fraction_sd,`Mean repairs`=repairs_per_rep_mean)
    datatable(df,rownames=FALSE,options=list(dom="t",scrollX=TRUE)) %>% formatRound(columns=names(df)[vapply(df,is.numeric,logical(1))],digits=3)
  })
  output$download <- downloadHandler(filename=function() "two_parameter_sets.rds",content=function(file) {req(!is.null(results()));saveRDS(results(),file)})
  list(one=one,two=two,results=results,draft=draft,dirty=dirty,summary=summary_data)
})
