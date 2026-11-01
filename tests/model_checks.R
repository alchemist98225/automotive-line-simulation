source("app/R/model.R")
p <- default_params(); p$time_mode <- "legacy"; p$n_replications <- 2L
# Warmup boundary: a machine busy continuously must retain 100% utilization.
synthetic <- tibble(resource="machine",time=c(0,10),server=c(0,1),queue=0,capacity=1)
stopifnot(resource_stats(synthetic,20,100)$avg_util==1)
# Reproducibility, coherent WIP, and operators released during automatic processing.
a <- run_line_sim(p); b <- run_line_sim(p)
stopifnot(isTRUE(all.equal(a$summaries,b$summaries,tolerance=1e-12)),all(a$summaries$avg_wip>=0),all(a$labor$avg_util>=0 & a$labor$avg_util<=1))
# Analytic single-bay limit: two minutes load + six automatic + two unload = 6/hour.
one <- p; one$sim_length_min <- 10000; one$warmup_min <- 1000; one$arrival_mean_min <- .1
one$stations <- p$stations[1,] %>% mutate(capacity=1L,dist="deterministic",mean_min=6,load_min=2,unload_min=2,mtbf_min=Inf,task_interval_min=Inf)
one$operators <- tibble(pool="upstream",count=1L)
c <- run_line_sim(one)
stopifnot(all(abs(c$summaries$throughput_per_hr-6)<.01),all(abs(c$labor$avg_util-.4)<.001))
# More operators cannot exceed the same single-machine deterministic limit.
more <- one; more$operators$count <- 5L
stopifnot(identical(c$summaries$throughput_per_hr,run_line_sim(more)$summaries$throughput_per_hr))
# Frequent long repairs must lower the deterministic single-machine throughput.
failed <- one; failed$stations$mtbf_min <- 20; failed$stations$mttr_min <- 20
stopifnot(mean(run_line_sim(failed)$summaries$throughput_per_hr)<5)
# Severe non-cyclic labor demand reduces output.
tasks <- one; tasks$stations$task_interval_min <- 5; tasks$stations$task_duration_min <- 4
stopifnot(mean(run_line_sim(tasks)$summaries$throughput_per_hr)<5)
# No-completion runs and invalid inputs should be handled deliberately.
empty <- p; empty$sim_length_min <- .001; empty$warmup_min <- 0
stopifnot(all(run_line_sim(empty)$summaries$n_completed==0))
invalid <- p; invalid$operators$count[1] <- 0
stopifnot(inherits(try(run_line_sim(invalid),silent=TRUE),"try-error"))
# Legacy parameter structures still run without enabling added features.
legacy <- p; legacy$operators <- NULL
legacy$stations <- legacy$stations %>% dplyr::select(name,capacity,dist,mean_min,sd_min)
stopifnot(nrow(run_line_sim(legacy)$summaries)==2)
stopifnot(identical(readLines("app/R/model.R"),readLines("quarto/R/model.R")))

# Post-warm-up accounting: warm-up failure contributes 0 minutes; straddling clips.
reps <- tibble::tibble(replication = 1L, seed = 1L)
s <- default_params(); s$n_replications <- 1L; s$warmup_min <- 100; s$shift_length_min <- 100; s$n_shifts <- 1L
s <- normalize_params(s); s$stations <- s$stations[1, ]; s$stations$capacity <- 1L
# Failure fully inside warm-up (ends before 100): 0 contribution, not counted.
# Failure starting at 90 with repair lasting 30: clips to 20 minutes inside window.
# Failure starting at 120 inside window lasting 10: counts fully, n_repairs includes it.
failures <- tibble::tibble(
  replication = 1L, station = "body_shop",
  time = c(10, 90, 120), failure_id = 1:3
)
repairs <- tibble::tibble(
  replication = 1L, station = "body_shop",
  time = c(20, 95, 120), duration = c(5, 25, 10), failure_id = 1:3
)
d <- build_downtime(repairs, reps, s, failures)
stopifnot(abs(d$downtime_min - 30) < 1e-9)           # 20 clipped + 10 full
stopifnot(identical(as.integer(d$n_repairs), 1L))    # only onset at 120
stopifnot(abs(d$total_downtime_min - 30) < 1e-9)
# Hours column on flow series starts at 0 after settling.
p2 <- default_params(); p2$n_replications <- 1L
r2 <- run_line_sim(p2)
stopifnot("hours" %in% names(r2$queue_timeseries), "hours" %in% names(r2$wip_timeseries))
stopifnot(isTRUE(all.equal(min(r2$wip_timeseries$hours), 0)))
stopifnot(isTRUE(all.equal(max(r2$wip_timeseries$hours), 8)))

cat("Model checks passed\n")
