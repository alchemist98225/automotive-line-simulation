# Tiny optimizer smoke: small pop/gen. Run from repo root: Rscript tests/opt_checks.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/opt_lib.R")

stopifnot(nrow(opt_knob_defs()) == 6L)

res <- opt_run_ga(
  pop_size = 4L,
  n_gen = 2L,
  n_reps = 2L,
  elite_reps = 3L,
  n_elites = 2L,
  base_seed = 7L,
  penalty = 10,
  p_mut = 0.25,
  cores = 1L,
  verbose = FALSE
)

stopifnot(nrow(res$history) == 2L)
stopifnot(all(c("best_fitness", "mean_fitness") %in% names(res$history)))
stopifnot(nrow(res$elites) >= 1L)
stopifnot(is.finite(res$best$fitness), is.finite(res$baseline$hpu_mean))
stopifnot(nrow(res$param_diff) == 6L)
stopifnot(nrow(res$best_summary) == 2L)

g <- opt_default_genome()
row <- opt_genome_to_row(g, design_id = "roundtrip")
stopifnot(identical(opt_row_to_genome(row), g))

# Fitness penalizes throughput shortfall
bad <- opt_evaluate_genome(
  genome = opt_default_genome(),
  n_reps = 2L,
  base_seed = 7L,
  cand_index = 1L,
  thr_floor = 1e9,
  penalty = 10,
  cache = NULL
)
stopifnot(bad$fitness > bad$hpu_mean, bad$feasible == FALSE)

message("opt_checks OK")
