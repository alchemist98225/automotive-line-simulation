source("app/R/model.R")
p <- default_params(); p$n_replications <- 2L
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
cat("Model checks passed\n")
