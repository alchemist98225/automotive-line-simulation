# Tiny optimizer smoke: small pop/gen. Run from repo root: Rscript tests/opt_checks.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/opt_lib.R")

stopifnot(nrow(opt_knob_defs()) == 6L)
stopifnot(identical(as.integer(opt_knob_defs()$default[opt_knob_defs()$name == "cap_paint"]), 2L))
stopifnot(identical(as.integer(opt_knob_defs()$default[opt_knob_defs()$name == "op_upstream"]), 1L))
stopifnot(identical(as.integer(opt_knob_defs()$default[opt_knob_defs()$name == "op_downstream"]), 2L))

# Scenario 2 baseline ≈ 11 vehicles/hr (shared 1+2 ops, paint 2).
base <- opt_evaluate_baseline(n_reps = 5L, base_seed = 20261027L)
stopifnot(base$thr_mean > 8, base$thr_mean < 14)
stopifnot(is.finite(base$hpu_mean), base$hpu_mean > 0.2, base$hpu_mean < 0.6)
stopifnot(abs(base$cost_hpu) < 1e-9)  # vs self

# Default genome via doe overlay matches guided baseline under shared CRN seeds
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
stopifnot(abs(g_base$cost_hpu) < 1e-9)

# Extra paint capacity raises cost_hpu; reducing queues credits
g_more <- opt_evaluate_genome(
  genome = opt_clip_genome(c(3L, 3L, 3L, 2L, 2L, 3L)),  # +1 paint
  n_reps = 3L,
  base_seed = 20261027L,
  cand_index = 99L,
  thr_floor = 0,
  penalty = 0,
  shared_seed = TRUE,
  capacity_cost_h = 1,
  queue_cost_h = 0.1
)
stopifnot(g_more$extra_paint_cap == 1L, g_more$cost_rate_h > 0, g_more$cost_hpu > 0)

g_less_q <- opt_evaluate_genome(
  genome = opt_clip_genome(c(1L, 1L, 1L, 2L, 2L, 2L)),  # −6 queue spots
  n_reps = 3L,
  base_seed = 20261027L,
  cand_index = 100L,
  thr_floor = 0,
  penalty = 0,
  shared_seed = TRUE,
  capacity_cost_h = 1,
  queue_cost_h = 0.1
)
stopifnot(g_less_q$extra_queue_spots == -6L, g_less_q$cost_rate_h < 0, g_less_q$cost_hpu < 0)

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
stopifnot(all(c("n_unique", "mean_pairwise_l1", "pct_feasible",
                "mean_fitness_feasible", "best_cost_hpu") %in% names(res$history)))
stopifnot(all(c("best_fitness", "mean_fitness", "cost_hpu") %in% c(names(res$history), names(res$evaluations))))
stopifnot(nrow(res$elites) >= 1L)
stopifnot(is.finite(res$best$fitness), is.finite(res$baseline$hpu_mean))
stopifnot(identical(as.integer(res$baseline$n_reps), 3L))
stopifnot(identical(as.integer(res$best$n_reps), 3L))
stopifnot(nrow(res$param_diff) == 6L)
stopifnot(nrow(res$best_summary) == 2L)
stopifnot(all(c("cost_hpu", "fitness") %in% names(res$best_summary)))
stopifnot(all(res$best_summary$n_reps == 3L))
stopifnot(nrow(res$operator_util) >= 2L)
# Baseline thr should be guided-scale (~11), not 1-paint (~6)
stopifnot(res$thr_floor_elite > 8)

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
