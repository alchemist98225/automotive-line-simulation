#!/usr/bin/env Rscript
# CLI integer-GA optimizer for queues, zone operators, and paint capacity.
# Objective: minimize mean HPU subject to mean throughput >= baseline.
# Reuses app/R/model.R via the DOE helpers — does not fork the simulation.
#
# Example:
#   Rscript scripts/opt_run.R --pop 24 --gen 15 --reps 3 --elite-reps 15 \
#     --out results/opt_example --seed 42 --cores 4

suppressPackageStartupMessages({
  library(tidyverse)
  library(parallel)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else normalizePath("scripts")
source(file.path(script_dir, "opt_lib.R"))
repo_root <- opt_repo_root(script_dir)

parse_args <- function(argv) {
  opts <- list(
    pop = 24L, gen = 15L, reps = 3L, elite_reps = 15L, elites = 5L,
    out = "results/opt_run", seed = 42L,
    cores = max(1L, parallel::detectCores() - 1L),
    penalty = 10, p_mut = 0.2, help = FALSE
  )
  if (!length(argv) || "--help" %in% argv || "-h" %in% argv) {
    opts$help <- TRUE
    return(opts)
  }
  i <- 1L
  while (i <= length(argv)) {
    key <- argv[[i]]
    take <- function() {
      if (i == length(argv)) stop(key, " needs a value", call. = FALSE)
      i <<- i + 1L
      argv[[i]]
    }
    if (key == "--pop") opts$pop <- as.integer(take())
    else if (key == "--gen") opts$gen <- as.integer(take())
    else if (key == "--reps") opts$reps <- as.integer(take())
    else if (key == "--elite-reps") opts$elite_reps <- as.integer(take())
    else if (key == "--elites") opts$elites <- as.integer(take())
    else if (key == "--out") opts$out <- take()
    else if (key == "--seed") opts$seed <- as.integer(take())
    else if (key == "--cores") opts$cores <- as.integer(take())
    else if (key == "--penalty") opts$penalty <- as.numeric(take())
    else if (key == "--p-mut") opts$p_mut <- as.numeric(take())
    else stop("Unknown argument: ", key, " (try --help)", call. = FALSE)
    i <- i + 1L
  }
  opts
}

usage <- function() {
  cat("Usage:\n",
      "  Rscript scripts/opt_run.R --pop N --gen N --reps N --out DIR",
      " [--seed N] [--cores N] [--elite-reps N] [--elites N]",
      " [--penalty X] [--p-mut X]\n\n",
      "  Minimizes mean HPU with mean throughput >= baseline mean.\n",
      "  Knobs: queues (paint/FA/inspection), zone operators, paint capacity.\n",
      sep = "")
}

opts <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help)) {
  usage()
  quit(status = 0)
}

out_dir <- opts$out
if (!grepl("^/", out_dir)) out_dir <- file.path(repo_root, out_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "chunks"), recursive = TRUE, showWarnings = FALSE)

cat(sprintf(
  "Optimize: pop=%d gen=%d reps=%d elite_reps=%d seed=%d cores=%d penalty=%.3g\n",
  opts$pop, opts$gen, opts$reps, opts$elite_reps, opts$seed, opts$cores, opts$penalty
), file = stderr())
cat(sprintf("Output: %s\n", out_dir), file = stderr())

t_wall0 <- proc.time()[["elapsed"]]
res <- opt_run_ga(
  pop_size = opts$pop,
  n_gen = opts$gen,
  n_reps = opts$reps,
  elite_reps = opts$elite_reps,
  n_elites = opts$elites,
  base_seed = opts$seed,
  penalty = opts$penalty,
  p_mut = opts$p_mut,
  cores = opts$cores,
  verbose = TRUE
)
wall <- proc.time()[["elapsed"]] - t_wall0

# Writes
readr::write_csv(res$history, file.path(out_dir, "convergence.csv"))
readr::write_csv(res$evaluations, file.path(out_dir, "evaluations.csv.gz"))
readr::write_csv(res$elites, file.path(out_dir, "elites.csv"))
readr::write_csv(res$best_summary, file.path(out_dir, "best_vs_baseline.csv"))
readr::write_csv(res$param_diff, file.path(out_dir, "param_diff.csv"))

# Best design row (DOE columns) — feed doe_params_from_row / future Compare import
best_row <- opt_genome_to_row(res$best$genome, design_id = "optimized")
readr::write_csv(best_row, file.path(out_dir, "best_params_design.csv"))

# App-style run_parameters table (same shape as Simulate download)
readr::write_csv(res$best$run_parameters, file.path(out_dir, "best_run_parameters.csv"))
readr::write_csv(res$baseline$run_parameters, file.path(out_dir, "baseline_run_parameters.csv"))

# Checkpoint-style metrics for best (elite reps)
doe_write_chunk(res$best$metrics, out_dir, "optimized")
doe_write_chunk(res$baseline$metrics, out_dir, "baseline")

info <- list(
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  pop = opts$pop, gen = opts$gen, reps = opts$reps,
  elite_reps = opts$elite_reps, elites = opts$elites,
  seed = opts$seed, cores = opts$cores, penalty = opts$penalty,
  p_mut = opts$p_mut, wall_sec = wall,
  thr_floor = res$thr_floor,
  best_fitness = res$best$fitness,
  best_hpu = res$best$hpu_mean,
  best_thr = res$best$thr_mean,
  baseline_hpu = res$baseline$hpu_mean,
  baseline_thr = res$baseline$thr_mean,
  objective = "minimize mean HPU subject to mean throughput >= baseline",
  knobs = paste(res$defs$name, collapse = ", "),
  note = "Illustrative only — placeholder parameters, not plant predictions."
)
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(info, file.path(out_dir, "run_info.json"), auto_unbox = TRUE, pretty = TRUE)
} else {
  writeLines(paste0(names(info), ": ", unlist(info)), file.path(out_dir, "run_info.txt"))
}

cat(sprintf("Wrote results under %s (wall %s)\n", out_dir, doe_format_duration(wall)), file = stderr())
cat(sprintf(
  "Best HPU=%.4f (baseline %.4f); thr=%.3f (floor %.3f); feasible=%s\n",
  res$best$hpu_mean, res$baseline$hpu_mean, res$best$thr_mean, res$thr_floor,
  res$best$feasible
), file = stderr())
