#!/usr/bin/env Rscript
# Welch warm-up analysis for Default and 2-bay setups.
#   Rscript scripts/warmup_run.R --out /workspace/sim/warmup_handoff --reps 30 --cores 7

suppressPackageStartupMessages(library(tidyverse))
args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else normalizePath("scripts")
source(file.path(script_dir, "warmup_lib.R"))
source(file.path(script_dir, "warmup_plot.R"))
repo_root <- doe_repo_root(script_dir)

opts <- list(out = "/workspace/sim/warmup_handoff", reps = 30L, cores = 7L, seed = 42L, window = 31L)
argv <- commandArgs(trailingOnly = TRUE)
i <- 1L
while (i <= length(argv)) {
  key <- argv[[i]]; take <- function() { i <<- i + 1L; argv[[i]] }
  if (key == "--out") opts$out <- take()
  else if (key == "--reps") opts$reps <- as.integer(take())
  else if (key == "--cores") opts$cores <- as.integer(take())
  else if (key == "--seed") opts$seed <- as.integer(take())
  else if (key == "--window") opts$window <- as.integer(take())
  i <- i + 1L
}
dir.create(opts$out, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("Welch ensemble: %d reps × 2 setups, workers=%d\n", opts$reps, opts$cores), file = stderr())
ens <- wu_run_ensemble(n_reps = opts$reps, base_seed = opts$seed, workers = opts$cores,
                            arrival_mean_min = 10)
readr::write_csv(ens, file.path(opts$out, "ensemble_wip.csv.gz"))
smooth <- wu_welch_smooth(ens, window = opts$window)
readr::write_csv(smooth, file.path(opts$out, "welch_smooth.csv.gz"))
rec <- wu_recommend(smooth, current_warmup = 300)
readr::write_csv(rec, file.path(opts$out, "warmup_recommendation.csv"))
ggsave(file.path(opts$out, "welch_warmup.png"),
       plot_welch_warmup(smooth, rec, 300),
       width = 11, height = 7.5, dpi = 150, bg = "white")
print(rec)
cat("Chart:", file.path(opts$out, "welch_warmup.png"), "\n")
