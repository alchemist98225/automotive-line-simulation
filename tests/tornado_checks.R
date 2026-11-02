# Tornado smoke. Run from repo root: Rscript tests/tornado_checks.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/tornado_lib.R")
doe_source_model(doe_repo_root(normalizePath("scripts")))
stopifnot(nrow(tor_factor_defs()) >= 8L)
# Subset factors for speed
# Monkey-patch by running only baseline + one factor via internal pieces
base <- tor_baseline_row()
f <- tor_factor_defs() %>% dplyr::filter(factor == "cap_paint")
row_lo <- tor_apply_factor(base, f[1,], "low")
row_hi <- tor_apply_factor(base, f[1,], "high")
tor_ensure_simmer_run()
lo <- tor_eval_row(row_lo, n_reps = 2L, base_seed = 5L, factor_index = 1L)
hi <- tor_eval_row(row_hi, n_reps = 2L, base_seed = 5L, factor_index = 1L)
stopifnot(nrow(lo) == 2L, nrow(hi) == 2L)
# CRN: same seeds
stopifnot(identical(sort(lo$seed), sort(hi$seed)))
cat("tornado_checks: OK\n")
