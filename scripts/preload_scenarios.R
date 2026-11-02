#!/usr/bin/env Rscript
# preload_scenarios.R
# Rebuild app/data/preload_scenarios.rds: 20-rep dedicated (1 paint) vs shared
# (2 paint) pair with common seeds. Every other setting stays at default_params().

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- {
  cand <- c(".", "..")
  hit <- cand[file.exists(file.path(cand, "app", "R", "model.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
source("app/R/model.R", local = TRUE)
source("app/R/analysis_data.R", local = TRUE)

dir.create(file.path("app", "data"), showWarnings = FALSE, recursive = TRUE)

s1p <- scenario_dedicated_one_paint(20L)
s2p <- scenario_shared_two_paint(20L)
stopifnot(identical(s1p$base_seed, s2p$base_seed))

message("Running Scenario 1 (dedicated, 1 paint), 20 reps …")
r1 <- run_line_sim(s1p)
message("Running Scenario 2 (shared, 2 paint), 20 reps …")
r2 <- run_line_sim(s2p)

summarize_run <- function(lab, r) {
  thr <- mean(r$summaries$throughput_per_hr)
  lt <- mean(r$summaries$cycle_time_mean, na.rm = TRUE)
  hpu <- mean(r$summaries$hpu[is.finite(r$summaries$hpu)])
  bn <- r$bottleneck
  message(sprintf("%s: throughput=%.2f lead=%.1f HPU=%.3f bottleneck=%s", lab, thr, lt, hpu, bn))
}
summarize_run(preload_scenario_labels[[1]], r1)
summarize_run(preload_scenario_labels[[2]], r2)

obj <- list(
  scenario1 = list(label = preload_scenario_labels[[1]], params = s1p, result = r1),
  scenario2 = list(label = preload_scenario_labels[[2]], params = s2p, result = r2),
  meta = list(
    n_replications = 20L,
    base_seed = s1p$base_seed,
    generated_at = format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
  )
)
out <- file.path("app", "data", "preload_scenarios.rds")
saveRDS(obj, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")
