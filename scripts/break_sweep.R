#!/usr/bin/env Rscript
# Demand break-sweep CLI: where does the line stop keeping up with arrivals?
# Reuses app/R/model.R via the DOE helpers. Parallel replications with furrr.
#
# Example:
#   Rscript scripts/break_sweep.R \
#     --reps 10 --out /workspace/sim/break_handoff --seed 42 --cores 7
#
# Writes:
#   summary.csv.gz, rep_metrics.csv.gz, breakpoints.csv, where_the_line_breaks.png
#
# Results are illustrative only — placeholder parameters, not plant predictions.

suppressPackageStartupMessages({
  library(tidyverse)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else normalizePath("scripts")
source(file.path(script_dir, "break_lib.R"))
source(file.path(script_dir, "break_plot.R"))
repo_root <- doe_repo_root(script_dir)
doe_source_model(repo_root)

parse_args <- function(argv) {
  opts <- list(
    reps = 10L,
    out = file.path(repo_root, "results", "break_sweep"),
    seed = 42L,
    cores = max(1L, parallel::detectCores() - 1L),
    demand_min = 3,
    demand_max = 18,
    demand_step = 0.5,
    fill_threshold = 0.95,
    help = FALSE
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
    if (key == "--reps") opts$reps <- as.integer(take())
    else if (key == "--out") opts$out <- take()
    else if (key == "--seed") opts$seed <- as.integer(take())
    else if (key == "--cores") opts$cores <- as.integer(take())
    else if (key == "--demand-min") opts$demand_min <- as.numeric(take())
    else if (key == "--demand-max") opts$demand_max <- as.numeric(take())
    else if (key == "--demand-step") opts$demand_step <- as.numeric(take())
    else if (key == "--fill-threshold") opts$fill_threshold <- as.numeric(take())
    else stop("Unknown argument: ", key, " (try --help)", call. = FALSE)
    i <- i + 1L
  }
  opts
}

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript scripts/break_sweep.R [--reps N] [--out DIR] [--seed N] [--cores N]\n",
    "    [--demand-min X] [--demand-max X] [--demand-step X] [--fill-threshold P]\n\n",
    "  Sweeps offered demand for Default line and 2-bay balanced setups.\n",
    "  Break point: first demand where mean throughput < fill-threshold × demand.\n",
    sep = ""
  )
}

opts <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help)) {
  usage()
  quit(status = 0)
}
if (is.na(opts$reps) || opts$reps < 1) stop("--reps must be a positive integer", call. = FALSE)
if (is.na(opts$cores) || opts$cores < 1) stop("--cores must be a positive integer", call. = FALSE)
if (!(opts$fill_threshold > 0 && opts$fill_threshold < 1)) {
  stop("--fill-threshold must be in (0, 1)", call. = FALSE)
}

out_dir <- opts$out
if (!grepl("^/", out_dir)) out_dir <- file.path(repo_root, out_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

demand_grid <- break_default_demand_grid(opts$demand_min, opts$demand_max, opts$demand_step)
grid <- break_make_grid(demand_grid)

cat(sprintf(
  "Break sweep: %d setups × %d demand points = %d runs, %d reps, seed=%d, workers=%d\n",
  n_distinct(grid$setup_id), length(demand_grid), nrow(grid),
  opts$reps, opts$seed, opts$cores
), file = stderr())
cat(sprintf("Demand grid: %.2f … %.2f by %.2f veh/hr\n",
            min(demand_grid), max(demand_grid), opts$demand_step), file = stderr())
cat(sprintf("Output: %s\n", out_dir), file = stderr())

run <- break_run_sweep(
  grid,
  n_reps = opts$reps,
  base_seed = opts$seed,
  workers = opts$cores,
  progress = TRUE
)

summary_tbl <- break_summarise(run$rep_metrics)
breakpoints <- break_find_breakpoints(summary_tbl, fill_threshold = opts$fill_threshold)

readr::write_csv(summary_tbl, file.path(out_dir, "summary.csv.gz"))
readr::write_csv(run$rep_metrics, file.path(out_dir, "rep_metrics.csv.gz"))
readr::write_csv(breakpoints, file.path(out_dir, "breakpoints.csv"))

meta <- tibble::tibble(
  elapsed_sec = run$elapsed_sec,
  workers = run$workers,
  n_reps = run$n_reps,
  base_seed = run$base_seed,
  n_points = run$n_points,
  fill_threshold = opts$fill_threshold,
  demand_min = opts$demand_min,
  demand_max = opts$demand_max,
  demand_step = opts$demand_step
)
readr::write_csv(meta, file.path(out_dir, "run_meta.csv"))

plot_obj <- break_plot_sweep(summary_tbl, breakpoints, fill_threshold = opts$fill_threshold)
png_path <- file.path(out_dir, "where_the_line_breaks.png")
break_save_plot(plot_obj, png_path)

cat(sprintf("Done in %s on %d workers.\n",
            doe_format_duration(run$elapsed_sec), run$workers), file = stderr())
cat("Break points:\n", file = stderr())
for (i in seq_len(nrow(breakpoints))) {
  r <- breakpoints[i, ]
  if (isTRUE(r$found)) {
    cat(sprintf("  %s: %.1f veh/hr (thr=%.2f, fill=%.1f%%, lead=%.1f min)\n",
                r$setup_label, r$break_demand_per_hr, r$break_throughput_mean,
                100 * r$break_fill_rate, r$break_lead_time_mean), file = stderr())
  } else {
    cat(sprintf("  %s: no break on grid\n", r$setup_label), file = stderr())
  }
}
cat(sprintf("Chart: %s\n", png_path), file = stderr())
