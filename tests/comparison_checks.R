e<-new.env();source("app/app.R",local=e,chdir=TRUE)
p<-e$default_params();p$n_replications<-3L
shiny::testServer(e$parameter_editor_server,{
  set_params(p)
  stopifnot(e$comparison_same(params(),e$normalize_params(p)))
  invisible(output$controls) # includes collapsed nested activity controls
  session$setInputs(v1_capacity_paint=2,v1_n_replications=4)
  stopifnot(params()$stations$capacity[2]==2,params()$n_replications==4)
  session$setInputs(v1_paint_auto_method="empirical",v1_paint_auto_observations="4, 5, 6")
  stopifnot(identical(params()$stations$auto_timing[[2]]$observations,c(4,5,6)))
  # Import after edits must ignore all controls from the previous revision.
  set_params(p)
  stopifnot(params()$stations$capacity[2]==1,params()$n_replications==3)
  stopifnot(e$comparison_same(params(),e$normalize_params(p)))
  session$setInputs(v2_paint_auto_method="distribution",v2_paint_auto_distribution="normal",v2_paint_auto_mean=1,v2_paint_auto_sd=3)
  stopifnot(params()$stations$auto_timing[[2]]$mean_min==1)
  session$setInputs(v2_pool_upstream=0)
  stopifnot(inherits(try(params(),silent=TRUE),"try-error"))
})
shiny::testServer(e$comparison_page_server,args=list(main_params=shiny::reactive(p),active_page=shiny::reactive("compare_sets")),{
  session$flushReact()
  stopifnot(initialized(),e$comparison_same(one$params(),two$params()),nrow(e$comparison_diff(one$params(),two$params()))==0)
  session$setInputs(run=1)
  r<-results(); stopifnot(!is.null(r),!dirty())
  stopifnot(isTRUE(all.equal(r$runs[[1]]$summaries,r$runs[[2]]$summaries,tolerance=1e-12)))
  invisible(output$kpis);invisible(output$summary);invisible(output$stations)
  invisible(output$distribution);invisible(output$cycles);invisible(output$occupancy)
  invisible(output$labor);invisible(output$downtime);invisible(output$wip);invisible(output$queues)
  b<-two$params();b$stations$capacity[2]<-2L;two$set_params(b)
  stopifnot(dirty(),results()$params[[2]]$stations$capacity[2]==1,one$params()$stations$capacity[2]==1)
  invisible(output$status)
  session$setInputs(run=2)
  stopifnot(!dirty(),results()$params[[2]]$stations$capacity[2]==2)
  # Match counts without overwriting other study changes.
  b<-two$params();b$n_replications<-1L;two$set_params(b)
  session$setInputs(match_reps=1)
  stopifnot(two$params()$n_replications==3,two$params()$stations$capacity[2]==2)
  session$setInputs(clone=1)
  stopifnot(e$comparison_same(one$params(),two$params()))
  session$setInputs(import=1)
  stopifnot(e$comparison_same(one$params(),e$normalize_params(p)))
  # An invalid empirical edit must retain old results and flag the draft.
  rev<-two$state()$revision
  do.call(session$setInputs,setNames(list("empirical"),paste0("two-v",rev,"_paint_auto_method")))
  do.call(session$setInputs,setNames(list("invalid"),paste0("two-v",rev,"_paint_auto_observations")))
  before<-results();session$setInputs(run=3)
  stopifnot(!is.null(error()),identical(results(),before),dirty())
})
# Same empirical mean/count is insufficient for treating two sets as identical.
a<-e$normalize_params(p);b<-a
 a$stations$auto_timing[[1]]<-e$timing_spec("empirical",observations=c(1,3))
 b$stations$auto_timing[[1]]<-e$timing_spec("empirical",observations=c(2,2))
stopifnot(!e$comparison_same(a,b))
cat("Two-set comparison checks passed\n")
