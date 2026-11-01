#!/usr/bin/env Rscript
# CLI design-of-experiments runner for the simmer automotive line.
# Reuses app/R/model.R — does not fork the simulation logic.
#
# Example:
#   Rscript scripts/doe_run.R \
#     --design designs/example_paint_ops.csv \
#     --reps 5 --out results/doe_example --seed 42 --cores 4
#
# Checkpoint / resume: each design_id is written under out/chunks/ as soon as
# it finishes. Re-running the same --out skips design points that already exist.
#
# HPU = (n_operators × observation_hours) / n_completed
#     = inverse of the model's throughput_per_labor_hr
# Results are illustrative only — placeholder parameters, not plant predictions.

suppressPackageStartupMessages({
  library(tidyverse)
  library(parallel)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else normalizePath("scripts")
source(file.path(script_dir, "doe_lib.R"))
repo_root <- doe_repo_root(script_dir)
doe_source_model(repo_root)

# --- argument parsing (base R; no optparse dependency) ---------------------
parse_args <- function(argv) {
  opts <- list(
    design = NULL, reps = 5L, out = "results/doe_run",
    seed = 42L, cores = max(1L, parallel::detectCores() - 1L),
    detail = FALSE, help = FALSE
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
    if (key == "--design") opts$design <- take()
    else if (key == "--reps") opts$reps <- as.integer(take())
    else if (key == "--out") opts$out <- take()
    else if (key == "--seed") opts$seed <- as.integer(take())
    else if (key == "--cores") opts$cores <- as.integer(take())
    else if (key == "--detail") opts$detail <- TRUE
    else stop("Unknown argument: ", key, " (try --help)", call. = FALSE)
    i <- i + 1L
  }
  opts
}

usage <- function() {
  cat("Usage:\n",
      "  Rscript scripts/doe_run.R --design FILE --reps N --out DIR",
      " [--seed N] [--cores N] [--detail]\n\n",
      "  --design   CSV from doe_make_design.R (required)\n",
      "  --reps     Replications per design point (default 5)\n",
      "  --out      Output directory (default results/doe_run)\n",
      "  --seed     Base seed; design×rep seeds are derived (default 42)\n",
      "  --cores    parallel::mclapply workers (default detectCores()-1)\n",
      "  --detail   Also save per-design RDS with raw engine tables\n",
      sep = "")
}

opts <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help) || is.null(opts$design)) {
  usage()
  if (is.null(opts$design) && !isTRUE(opts$help)) {
    quit(status = 2)
  }
  quit(status = 0)
}
if (!file.exists(opts$design)) stop("Design file not found: ", opts$design, call. = FALSE)
if (is.na(opts$reps) || opts$reps < 1) stop("--reps must be a positive integer", call. = FALSE)
if (is.na(opts$cores) || opts$cores < 1) stop("--cores must be a positive integer", call. = FALSE)

design <- readr::read_csv(opts$design, show_col_types = FALSE) %>% doe_validate_design()
design <- design %>% mutate(.row_index = row_number())

out_dir <- opts$out
if (!grepl("^/", out_dir)) out_dir <- file.path(repo_root, out_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "chunks"), recursive = TRUE, showWarnings = FALSE)
if (opts$detail) dir.create(file.path(out_dir, "detail"), recursive = TRUE, showWarnings = FALSE)

# Resume: skip design_ids that already have a chunk
pending <- design %>%
  mutate(chunk = vapply(design_id, function(id) doe_chunk_path(out_dir, id), character(1)),
         done = file.exists(chunk)) 
n_done <- sum(pending$done)
n_total <- nrow(pending)
to_run <- pending %>% filter(!done)

cat(sprintf(
  "DOE run: %d design points, %d reps, seed=%d, cores=%d\n",
  n_total, opts$reps, opts$seed, opts$cores
), file = stderr())
cat(sprintf("Output: %s\n", out_dir), file = stderr())
if (n_done) {
  cat(sprintf("Resume: %d/%d design points already checkpointed; %d remaining\n",
              n_done, n_total, nrow(to_run)), file = stderr())
}
if (!nrow(to_run)) {
  cat("Nothing to run — assembling summaries from existing chunks.\n", file = stderr())
}

# One design point: all reps via run_line_sim (seeded base_seed for rep 1).
run_design_point <- function(row_df) {
  tryCatch({
    row <- row_df[1, , drop = FALSE]
    id <- as.character(row$design_id[[1]])
    idx <- as.integer(row$.row_index[[1]])
    base <- doe_rep_seed(opts$seed, idx, 1L, opts$reps)
    params <- doe_params_from_row(row, n_replications = opts$reps, base_seed = base)
    row_clean <- row %>% select(-any_of(".row_index"))
    t0 <- proc.time()[["elapsed"]]
    result <- run_line_sim(params)
    elapsed <- proc.time()[["elapsed"]] - t0
    metrics <- doe_extract_rep_metrics(result, row_clean, result$seeds) %>%
      mutate(elapsed_sec = elapsed / opts$reps)
    doe_write_chunk(metrics, out_dir, id)
    if (opts$detail) {
      saveRDS(
        list(design_id = id, params = result$params, result = result, metrics = metrics),
        file.path(out_dir, "detail", paste0(gsub("[^A-Za-z0-9._-]+", "_", id), ".rds"))
      )
    }
    list(design_id = id, elapsed = elapsed, n_reps = opts$reps)
  }, error = function(e) {
    structure(conditionMessage(e), class = "try-error", design_id = as.character(row_df$design_id[[1]]))
  })
}

# Run pending points in waves so we can print ETA between waves.
run_rows <- split(to_run, to_run$design_id)
# Preserve design order
run_ids <- to_run$design_id
run_rows <- run_rows[run_ids]

wave_size <- max(1L, as.integer(opts$cores))
n_pending <- length(run_ids)
finished <- 0L
t_start <- proc.time()[["elapsed"]]
elapsed_sum <- 0

if (n_pending > 0) {
  waves <- split(run_ids, ceiling(seq_along(run_ids) / wave_size))
  for (w in seq_along(waves)) {
    ids <- waves[[w]]
    batch <- run_rows[ids]
    if (opts$cores == 1L || length(ids) == 1L) {
      results <- lapply(batch, run_design_point)
    } else {
      results <- parallel::mclapply(
        batch, run_design_point,
        mc.cores = min(opts$cores, length(ids)),
        mc.preschedule = FALSE
      )
    }
    # Surface worker errors
    for (r in results) {
      if (inherits(r, "try-error") || is.null(r$design_id)) {
        stop("A design point failed: ", paste(as.character(r), collapse = " "), call. = FALSE)
      }
      elapsed_sum <- elapsed_sum + r$elapsed
    }
    finished <- finished + length(ids)
    wall <- proc.time()[["elapsed"]] - t_start
    rate <- finished / max(wall, 1e-6)
    remain <- n_pending - finished
    eta <- if (rate > 0) remain / rate else NA_real_
    cat(sprintf(
      "[%d/%d pending] wave %d/%d done | wall %s | ETA %s | last ids: %s\n",
      finished, n_pending, w, length(waves),
      doe_format_duration(wall),
      if (is.na(eta)) "?" else doe_format_duration(eta),
      paste(ids, collapse = ", ")
    ), file = stderr())
  }
}

# Assemble all chunks → rep_metrics + summary
chunk_files <- list.files(file.path(out_dir, "chunks"), pattern = "\\.csv\\.gz$", full.names = TRUE)
if (!length(chunk_files)) stop("No chunk files found under ", out_dir, call. = FALSE)

rep_metrics <- map_dfr(chunk_files, doe_read_chunk) %>%
  arrange(design_id, replication)

# Keep only design_ids from this design file (ignore stale chunks from older runs)
rep_metrics <- rep_metrics %>% filter(design_id %in% design$design_id)

summary_tbl <- doe_summarise_reps(rep_metrics)

rep_path <- file.path(out_dir, "rep_metrics.csv.gz")
sum_path <- file.path(out_dir, "summary.csv")
# readr compresses automatically when the path ends in .gz
readr::write_csv(rep_metrics, rep_path)
readr::write_csv(summary_tbl, sum_path)

# run_info
git_sha <- tryCatch(
  system("git rev-parse HEAD", intern = TRUE, ignore.stderr = TRUE)[1],
  error = function(e) NA_character_
)
info <- list(
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  design_file = normalizePath(opts$design),
  n_design_points = n_total,
  n_reps = opts$reps,
  seed = opts$seed,
  cores = opts$cores,
  detail = opts$detail,
  git_sha = git_sha,
  r_version = R.version.string,
  simmer_version = as.character(utils::packageVersion("simmer")),
  hpu_definition = paste(
    "HPU = (n_operators * observation_hours) / n_completed;",
    "observation_hours = (sim_length_min - warmup_min) / 60;",
    "inverse of throughput_per_labor_hr. Lower is more productive."
  ),
  note = "PLACEHOLDER EXAMPLE PARAMETERS — results are illustrative only, not plant predictions.",
  seed_rule = "seed(design_row, rep) = base_seed + (row_index-1)*n_reps + (rep-1)"
)
info_path <- file.path(out_dir, "run_info.json")
jsonlite_ok <- requireNamespace("jsonlite", quietly = TRUE)
if (jsonlite_ok) {
  jsonlite::write_json(info, info_path, auto_unbox = TRUE, pretty = TRUE)
} else {
  info_path <- file.path(out_dir, "run_info.txt")
  writeLines(paste0(names(info), ": ", unlist(info)), info_path)
}

cat(sprintf(
  "Wrote %s (%d rows) and %s (%d rows)\n",
  rep_path, nrow(rep_metrics), sum_path, nrow(summary_tbl)
), file = stderr())
cat(sprintf("Run info: %s\n", info_path), file = stderr())
if (n_pending > 0) {
  per_point <- (proc.time()[["elapsed"]] - t_start) / n_pending
  cat(sprintf(
    "Timing: %.2fs wall per design point (%d reps) on %d cores\n",
    per_point, opts$reps, opts$cores
  ), file = stderr())
}
