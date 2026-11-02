# Warm-up smoke. Run from repo root: Rscript tests/warmup_checks.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/warmup_lib.R")
doe_source_model(doe_repo_root(normalizePath("scripts")))
wu_ensure_simmer_run()
# Tiny ensemble
ens <- wu_run_ensemble(n_reps = 2L, base_seed = 3L, workers = 1L, sim_length_min = 200, dt = 5)
stopifnot(nrow(ens) > 10, all(c("time_min", "wip", "setup_id") %in% names(ens)))
sm <- wu_welch_smooth(ens, window = 5L)
stopifnot(all(c("wip_smooth", "wip_mean") %in% names(sm)))
rec <- wu_recommend(sm, current_warmup = 100, end_window_min = 40)
stopifnot(nrow(rec) == 2L)
cat("warmup_checks: OK\n")
