e <- new.env()
source("app/app.R",local=e,chdir=TRUE)
# First paint: no input registration and no Run click. Dynamic renderUI
# values are NULL; the session must still produce the placeholder run.
shiny::testServer(e$server, {
  stopifnot(is.null(input$run), is.null(input$op_a), is.null(input$load_body_shop), is.null(input$mtbf_paint))
  res <- sim_result()
  d <- e$default_params()
  stopifnot(nrow(res$summaries)==d$n_replications)
  stopifnot(nrow(res$labor)==nrow(d$operators)*d$n_replications)
  stopifnot(res$bottleneck %in% d$stations$name)
  stopifnot(identical(res$params$operators$pool, c("upstream","downstream")))
  stopifnot(all(res$params$operators$count==2))
  stopifnot(res$params$stations$mttr_min[res$params$stations$name=="inspection"]==0)
  stopifnot(all(res$params$stations$mttr_min[res$params$stations$name!="inspection"]>0))
  stopifnot(nrow(res$queue_timeseries)>0, nrow(res$wip_timeseries)>0, nrow(res$downtime_by_rep)>0)
  stopifnot(is.character(output$kpi_throughput), nzchar(output$kpi_throughput))
  invisible(output$plot_util)
  invisible(output$plot_queue)
  invisible(output$plot_wip)
  invisible(output$plot_down)
  invisible(output$plot_labor)
  invisible(output$plot_cycle)
  invisible(output$table_metrics)
  invisible(output$reliability_table)
  invisible(output$labor_table)
})
ui_html <- as.character(e$ui)
page_has <- function(label) {
  grepl(label, ui_html, fixed = TRUE) ||
    grepl(gsub("&", "&amp;", label, fixed = TRUE), ui_html, fixed = TRUE)
}
for (label in c(
  "Bottleneck & capacity", "Flow over time", "Lead time & data",
  "Station occupancy", "Operators", "Downtime", "Staffing comparison",
  "WIP", "Queues", "Cycle time", "Station metrics", "Timing comparison", "Downloads",
  "520px", "clamp(450px, 52vh, 550px)"
)) stopifnot(page_has(label))
# Downloads moved out of the sidebar accordion into the Lead time tab.
stopifnot(!grepl("Download data", ui_html, fixed = TRUE))
cat("Results tab layout check passed\n")
cat("First-load check passed\n")
shiny::testServer(e$server, {
  session$setInputs(staffing="zones",op_a=2,op_b=2,repair_staff=1,reliability=TRUE,noncyclic=TRUE,
    sim_length=480,warmup=60,arrival_mean=4,n_reps=2,seed=20261027,
    body_cap=2,body_mean=6,paint_cap=1,paint_mean=8,fa_cap=2,fa_mean=7,insp_cap=1,insp_mean=3)
  ids <- c(load="load_min",unload="unload_min",mtbf="mtbf_min",mttr="mttr_min",interval="task_interval_min",task="task_duration_min")
  for(i in seq_len(nrow(e$default_params()$stations))) {
    st <- e$default_params()$stations[i,]
    for(id in names(ids)) do.call(session$setInputs,setNames(list(st[[ids[[id]]]]),paste0(id,"_",st$name)))
    do.call(session$setInputs,setNames(list(0L),paste0("every_",st$name)))
  }
  session$setInputs(run=1)
  stopifnot(nrow(sim_result()$summaries)==2,nrow(sim_result()$labor)==4)
  session$setInputs(queue_layout="facet", queue_station="all")
  invisible(output$plot_queue)
  session$setInputs(queue_layout="overlay", queue_station="paint")
  invisible(output$plot_queue)
  session$setInputs(queue_station="all")
  invisible(output$plot_queue)
  session$setInputs(compare=1)
  stopifnot(nrow(comparison())==4)
  invisible(output$scenario_table)
  invisible(output$reliability_table)
  session$setInputs(load_body_shop_method="empirical",load_body_shop_observations="0.5, 1.0\n1.5",
    auto_paint_method="distribution",auto_paint_distribution="lognormal",auto_paint_sd=2,
    unload_paint_method="fixed",task_paint_method="empirical",task_paint_observations="3 4 5",run=10)
  stopifnot(identical(sim_result()$params$stations$load_timing[[1]]$observations,c(.5,1,1.5)))
  session$setInputs(compare_timing=1)
  stopifnot(nrow(timing_comparison())==3)
  invisible(output$timing_table)
  session$setInputs(noncyclic=FALSE,run=11)
  stopifnot(all(purrr::map_dbl(sim_result()$params$stations$task_timing,e$timing_mean)==0))
  session$setInputs(staffing="shared",op_all=3,run=2)
  stopifnot(nrow(sim_result()$params$operators)==1)
  session$setInputs(staffing="dedicated",op_body_shop=1,op_paint=1,op_final_assembly=2,op_inspection=1,run=3)
  stopifnot(sum(sim_result()$params$operators$count)==5)
})
stopifnot(identical(e$parse_observations("1, 2; 3\n4"),c(1,2,3,4)))
stopifnot(inherits(try(e$parse_observations("1, bad, 2"),silent=TRUE),"try-error"))
cat("Shiny server checks passed\n")
