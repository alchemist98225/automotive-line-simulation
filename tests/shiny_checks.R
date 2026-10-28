e <- new.env()
source("app/app.R",local=e,chdir=TRUE)
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
  session$setInputs(compare=1)
  stopifnot(nrow(comparison())==4)
  invisible(output$scenario_table)
  invisible(output$reliability_table)
  session$setInputs(staffing="shared",op_all=3,run=2)
  stopifnot(nrow(sim_result()$params$operators)==1)
  session$setInputs(staffing="dedicated",op_body_shop=1,op_paint=1,op_final_assembly=2,op_inspection=1,run=3)
  stopifnot(sum(sim_result()$params$operators$count)==5)
})
cat("Shiny server checks passed\n")
