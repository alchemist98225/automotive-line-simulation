# Tiny break-sweep smoke: coarse grid, 2 reps, sequential. Run from repo root:
#   Rscript tests/break_checks.R
suppressPackageStartupMessages(library(tidyverse))

script_dir <- normalizePath("scripts")
source(file.path(script_dir, "break_lib.R"))
source(file.path(script_dir, "break_plot.R"))
repo_root <- doe_repo_root(script_dir)
doe_source_model(repo_root)

defs <- break_setup_defs()
stopifnot(nrow(defs) == 2L, all(c("default", "guided_2") %in% defs$setup_id))
stopifnot(identical(as.integer(defs$cap_paint[defs$setup_id == "guided_2"]), 2L))

# Coarse demand grid straddling default-line capacity (~6–7/hr).
grid <- break_make_grid(c(4, 7, 12))
stopifnot(nrow(grid) == 6L)
stopifnot(all(c("design_id", "setup_id", "offered_demand_per_hr", "arrival_mean_min") %in% names(grid)))

run <- break_run_sweep(grid, n_reps = 2L, base_seed = 11L, workers = 1L, progress = FALSE)
stopifnot(nrow(run$rep_metrics) == 6L * 2L)
stopifnot(all(c("throughput_per_hr", "cycle_time_mean", "avg_wip", "hpu",
                "offered_demand_per_hr", "setup_id") %in% names(run$rep_metrics)))

# CRN: same demand_index + rep → same seed across setups
seed_check <- run$rep_metrics %>%
  distinct(setup_id, demand_index, replication, seed) %>%
  group_by(demand_index, replication) %>%
  summarise(n_seed = n_distinct(seed), .groups = "drop")
stopifnot(all(seed_check$n_seed == 1L))

summary_tbl <- break_summarise(run$rep_metrics)
stopifnot(nrow(summary_tbl) == 6L)
stopifnot(all(summary_tbl$throughput_mean > 0))

bp <- break_find_breakpoints(summary_tbl, fill_threshold = 0.95)
stopifnot(nrow(bp) == 2L)
# Default line (paint=1) should break by demand 12 on this coarse grid
def_bp <- bp %>% filter(setup_id == "default")
stopifnot(isTRUE(def_bp$found), def_bp$break_demand_per_hr <= 12)

# Plot builds without error
p <- break_plot_sweep(summary_tbl, bp)
stopifnot(inherits(p, "patchwork") || inherits(p, "ggplot"))

# Guided Experiment paint capacity matches opt helper when available
if (exists("opt_guided_experiment_params", mode = "function") || TRUE) {
  source(file.path(repo_root, "app", "R", "optimize_space.R"), local = FALSE)
  g <- opt_guided_experiment_params()
  stopifnot(identical(as.integer(g$stations$capacity[g$stations$name == "paint"]), 2L))
}

cat("break_checks: OK\n")
