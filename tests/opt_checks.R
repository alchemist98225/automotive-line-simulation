# Tiny optimizer smoke: small pop/gen. Run from repo root: Rscript tests/opt_checks.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/opt_lib.R")

stopifnot(nrow(opt_knob_defs()) == 6L)

# App defaults via pristine path ≈ 6 vehicles/hr (1 paint capacity), not the
# guided 2-paint-bay experiment (~11/hr).
base <- opt_evaluate_default_params(n_reps = 5L, base_seed = 20261027L)
stopifnot(base$thr_mean > 4, base$thr_mean < 8)
stopifnot(is.finite(base$hpu_mean), base$hpu_mean > 0.4, base$hpu_mean < 1.0)

# Genome default overlay matches default_params knobs (same thr ballpark, same seed)
g_base <- opt_evaluate_genome(
  genome = opt_default_genome(),
  n_reps = 5L,
  base_seed = 20261027L,
  cand_index = 1L,
  thr_floor = 0,
  penalty = 0,
  shared_seed = TRUE
)
stopifnot(abs(g_base$thr_mean - base$thr_mean) < 1e-9)
stopifnot(abs(g_base$hpu_mean - base$hpu_mean) < 1e-9)

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
stopifnot(min(res$history$generation) == 0L)
stopifnot(nrow(res$generation0) == 4L)
stopifnot(all(c("n_unique", "mean_pairwise_l1", "pct_feasible") %in% names(res$history)))
stopifnot(all(c("best_fitness", "mean_fitness") %in% names(res$history)))
stopifnot(nrow(res$elites) >= 1L)
stopifnot(is.finite(res$best$fitness), is.finite(res$baseline$hpu_mean))
stopifnot(identical(as.integer(res$baseline$n_reps), 3L)) # elite reps
stopifnot(identical(as.integer(res$best$n_reps), 3L))
stopifnot(nrow(res$param_diff) == 6L)
stopifnot(nrow(res$best_summary) == 2L)
stopifnot(all(res$best_summary$n_reps == 3L))
stopifnot(nrow(res$operator_util) >= 2L)

g <- opt_default_genome()
row <- opt_genome_to_row(g, design_id = "roundtrip")
stopifnot(identical(opt_row_to_genome(row), g))

bad <- opt_evaluate_genome(
  genome = opt_default_genome(),
  n_reps = 2L,
  base_seed = 7L,
  cand_index = 1L,
  thr_floor = 1e9,
  penalty = 10,
  cache = NULL,
  shared_seed = TRUE
)
stopifnot(bad$fitness > bad$hpu_mean, bad$feasible == FALSE)

message("opt_checks OK")
