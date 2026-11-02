#!/usr/bin/env Rscript
# OAT tornado around 2-bay balanced @ ~10 veh/hr.
#   Rscript scripts/tornado_run.R --out /workspace/sim/tornado_handoff --reps 10 --cores 7

suppressPackageStartupMessages(library(tidyverse))
args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else normalizePath("scripts")
source(file.path(script_dir, "tornado_lib.R"))
source(file.path(script_dir, "tornado_plot.R"))

opts <- list(out = "/workspace/sim/tornado_handoff", reps = 10L, cores = 7L, seed = 42L)
argv <- commandArgs(trailingOnly = TRUE)
i <- 1L
while (i <= length(argv)) {
  key <- argv[[i]]; take <- function() { i <<- i + 1L; argv[[i]] }
  if (key == "--out") opts$out <- take()
  else if (key == "--reps") opts$reps <- as.integer(take())
  else if (key == "--cores") opts$cores <- as.integer(take())
  else if (key == "--seed") opts$seed <- as.integer(take())
  i <- i + 1L
}
dir.create(opts$out, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("Tornado: reps=%d workers=%d\n", opts$reps, opts$cores), file = stderr())
run <- tor_run(n_reps = opts$reps, base_seed = opts$seed, workers = opts$cores)
sum <- tor_summarise(run$rep_metrics)
eff <- tor_effects(sum)
readr::write_csv(run$rep_metrics, file.path(opts$out, "rep_metrics.csv.gz"))
readr::write_csv(sum, file.path(opts$out, "summary.csv"))
readr::write_csv(eff, file.path(opts$out, "effects.csv"))
ggsave(file.path(opts$out, "tornado.png"), plot_tornado_pair(eff),
       width = 11, height = 12, dpi = 150, bg = "white")
cat("Baseline HPU/thr:\n"); print(sum %>% filter(factor == "baseline"))
cat("Top HPU drivers:\n"); print(eff %>% select(label, hpu_low, hpu_high, hpu_abs_span) %>% head(8))
cat("Chart:", file.path(opts$out, "tornado.png"), "\n")
