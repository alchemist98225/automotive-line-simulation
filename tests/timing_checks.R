source("app/R/model.R")
fixed <- make_duration_fn(timing_spec(mean_min=2.5))
stopifnot(all(replicate(20,fixed())==2.5))
single <- make_duration_fn(timing_spec("empirical",observations=5))
stopifnot(all(replicate(50,single())==5))
empirical <- make_duration_fn(timing_spec("empirical",observations=c(.5,1,3)))
set.seed(21); draws <- replicate(1000,empirical())
stopifnot(all(draws %in% c(.5,1,3)),length(unique(draws))==3)
# Check lognormal mean/SD conversion against the parameters specified in minutes.
lognormal <- make_duration_fn(timing_spec("distribution",4,2,"lognormal"))
set.seed(22); draws <- replicate(50000,lognormal())
stopifnot(abs(mean(draws)-4)<.06,abs(sd(draws)-2)<.08)
normal <- make_duration_fn(timing_spec("distribution",1,3,"normal"))
set.seed(23); draws <- replicate(50000,normal())
stopifnot(all(draws>=0),abs(mean(draws)-timing_mean(timing_spec("distribution",1,3,"normal")))<.06)
walk(list(numeric(),c(1,NA),c(-1,2),Inf),function(x) {
  stopifnot(inherits(try(make_duration_fn(timing_spec("empirical",observations=x)),silent=TRUE),"try-error"))
})
p <- default_params() %>% normalize_params(); p$n_replications <- 2L
p$stations <- p$stations %>% mutate(
  load_timing=map(load_min,~timing_spec("empirical",observations=c(.x/2,.x*1.5))),
  auto_timing=map(mean_min,~timing_spec("distribution",.x,.x/4,"lognormal")),
  unload_timing=map(unload_min,~timing_spec(mean_min=.x)),
  task_timing=map(task_duration_min,~timing_spec("empirical",observations=c(.x,.x+1))))
stopifnot(nrow(run_line_sim(p)$summaries)==2)
# Each preset must preserve expected duration for every activity and station.
presets <- variability_scenarios(p)
walk(c("load","auto","unload","task"),function(activity) {
  means <- map(presets$params,~map_dbl(.x$stations[[paste0(activity,"_timing")]],timing_mean))
  stopifnot(isTRUE(all.equal(means[[1]],means[[2]])),isTRUE(all.equal(means[[1]],means[[3]])))
})
stopifnot(nrow(run_scenarios(presets))==3)
# Counts and random support work both accept empirical duration specifications.
p$stations$task_every_n <- 2L
stopifnot(nrow(run_line_sim(p)$summaries)==2)
cat("Timing checks passed\n")
