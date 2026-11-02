#!/usr/bin/env Rscript
# analysis_precompute.R
# Rebuild app/data/analysis_precomputed.rds from the overnight CLI libraries.
# By default reuses existing CSVs under results/ or the handoff folders.
# Pass --rerun to execute the CLI runners first (slow; needs furrr workers).
#
# Usage:
#   Rscript scripts/analysis_precompute.R
#   Rscript scripts/analysis_precompute.R --rerun --cores 7

suppressPackageStartupMessages({
  library(tidyverse)
})

args <- commandArgs(trailingOnly = TRUE)
rerun <- "--rerun" %in% args
cores <- {
  i <- match("--cores", args)
  if (!is.na(i) && length(args) >= i + 1) as.integer(args[[i + 1]]) else max(1L, parallel::detectCores() - 1L)
}

root <- {
  # Prefer repo root when launched from scripts/ or app/
  cand <- c(".", "..", file.path("..", ".."))
  hit <- cand[file.exists(file.path(cand, "app", "R", "model.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
dir.create(file.path("app", "data"), showWarnings = FALSE, recursive = TRUE)

if (isTRUE(rerun)) {
  message("Re-running CLI analyses (cores=", cores, ") …")
  system2("Rscript", c("scripts/break_sweep.R", "--reps", "10", "--out", "results/break_sweep", "--cores", cores))
  system2("Rscript", c("scripts/validate_run.R", "--out", "results/validation", "--cores", cores))
  system2("Rscript", c("scripts/warmup_run.R", "--out", "results/warmup", "--reps", "30", "--cores", cores))
  system2("Rscript", c("scripts/tornado_run.R", "--out", "results/tornado", "--reps", "10", "--cores", cores))
}

pick_file <- function(primary, fallback) {
  if (file.exists(primary)) return(primary)
  if (file.exists(fallback)) return(fallback)
  stop("Missing input: ", primary, " (also tried ", fallback, ")", call. = FALSE)
}

break_dir <- if (dir.exists("results/break_sweep")) "results/break_sweep" else "/workspace/sim/break_handoff"
val_dir <- if (dir.exists("results/validation")) "results/validation" else "/workspace/sim/validation_handoff"
wu_dir <- if (dir.exists("results/warmup")) "results/warmup" else "/workspace/sim/warmup_handoff"
tor_dir <- if (dir.exists("results/tornado")) "results/tornado" else "/workspace/sim/tornado_handoff"

analysis <- list(
  break_sweep = list(
    summary = readr::read_csv(pick_file(file.path(break_dir, "summary.csv.gz"), file.path(break_dir, "summary.csv")), show_col_types = FALSE),
    breakpoints = readr::read_csv(file.path(break_dir, "breakpoints.csv"), show_col_types = FALSE),
    fill_threshold = 0.95
  ),
  tornado = list(
    effects = readr::read_csv(file.path(tor_dir, "effects.csv"), show_col_types = FALSE)
  ),
  validation = list(
    mmc_summary = readr::read_csv(file.path(val_dir, "mmc_summary.csv"), show_col_types = FALSE),
    littles_summary = readr::read_csv(file.path(val_dir, "littles_summary.csv"), show_col_types = FALSE),
    littles_reps = readr::read_csv(pick_file(file.path(val_dir, "littles_reps.csv.gz"), file.path(val_dir, "littles_reps.csv")), show_col_types = FALSE)
  ),
  warmup = list(
    smooth = readr::read_csv(pick_file(file.path(wu_dir, "welch_smooth.csv.gz"), file.path(wu_dir, "welch_smooth.csv")), show_col_types = FALSE),
    recommend = readr::read_csv(file.path(wu_dir, "warmup_recommendation.csv"), show_col_types = FALSE),
    current_warmup = 300
  ),
  meta = list(
    source = if (isTRUE(rerun)) "CLI re-run via analysis_precompute.R" else "packaged from existing CLI/handoff CSVs",
    generated_at = format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
  )
)

out <- file.path("app", "data", "analysis_precomputed.rds")
saveRDS(analysis, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")
