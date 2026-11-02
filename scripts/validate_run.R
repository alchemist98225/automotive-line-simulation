#!/usr/bin/env Rscript
# Run M/M/c and Little's Law validation; write tables + plots.
# Example:
#   Rscript scripts/validate_run.R --out /workspace/sim/validation_handoff --cores 7

suppressPackageStartupMessages(library(tidyverse))
args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else normalizePath("scripts")
source(file.path(script_dir, "validate_lib.R"))
source(file.path(script_dir, "validate_plot.R"))
repo_root <- doe_repo_root(script_dir)

parse_args <- function(argv) {
  opts <- list(out = file.path(repo_root, "results", "validation"),
               cores = max(1L, parallel::detectCores() - 1L),
               mmc_reps = 50L, little_reps = 12L, seed = 42L, help = FALSE)
  if (!length(argv) || "--help" %in% argv) { opts$help <- TRUE; return(opts) }
  i <- 1L
  while (i <= length(argv)) {
    key <- argv[[i]]
    take <- function() { i <<- i + 1L; argv[[i]] }
    if (key == "--out") opts$out <- take()
    else if (key == "--cores") opts$cores <- as.integer(take())
    else if (key == "--mmc-reps") opts$mmc_reps <- as.integer(take())
    else if (key == "--little-reps") opts$little_reps <- as.integer(take())
    else if (key == "--seed") opts$seed <- as.integer(take())
    else stop("Unknown: ", key)
    i <- i + 1L
  }
  opts
}
opts <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help)) {
  cat("Usage: Rscript scripts/validate_run.R [--out DIR] [--cores N]\n"); quit(status = 0)
}
out_dir <- opts$out
if (!grepl("^/", out_dir)) out_dir <- file.path(repo_root, out_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# M/M/c cases: c=2, μ=1, vary λ → ρ in {0.5, 0.7, 0.85}
mu <- 1; c <- 2L
lambdas <- c(1.0, 1.4, 1.7)  # ρ = 0.50, 0.70, 0.85
cat("M/M/c cases...\n", file = stderr())
mmc_summaries <- list(); mmc_reps_all <- list()
for (i in seq_along(lambdas)) {
  lam <- lambdas[[i]]
  cat(sprintf("  λ=%.2f ρ=%.2f\n", lam, lam / (c * mu)), file = stderr())
  res <- mm_c_run_case(lam, mu, c, n_reps = opts$mmc_reps, horizon = 25000,
                       warmup = 2500, base_seed = opts$seed + i * 1000L,
                       workers = opts$cores)
  mmc_summaries[[i]] <- res$summary
  mmc_reps_all[[i]] <- res$reps
}
mmc_summary <- bind_rows(mmc_summaries)
mmc_reps <- bind_rows(mmc_reps_all)
readr::write_csv(mmc_summary, file.path(out_dir, "mmc_summary.csv"))
readr::write_csv(mmc_reps, file.path(out_dir, "mmc_reps.csv.gz"))
ggsave(file.path(out_dir, "mmc_check.png"), plot_mmc_check(mmc_summary),
       width = 10, height = 9, dpi = 150, bg = "white")

cat("Little's Law scenarios...\n", file = stderr())
little_reps <- littles_run(n_reps = opts$little_reps, base_seed = opts$seed,
                           workers = opts$cores)
little_sum <- littles_summarise(little_reps)
readr::write_csv(little_reps, file.path(out_dir, "littles_reps.csv.gz"))
readr::write_csv(little_sum, file.path(out_dir, "littles_summary.csv"))
ggsave(file.path(out_dir, "littles_law.png"), plot_littles(little_reps, little_sum),
       width = 10, height = 8, dpi = 150, bg = "white")

cat("Done. Key M/M/c:\n", file = stderr())
print(mmc_summary %>% select(rho, metric, analytic_value, sim_mean, rel_err, covers),
      n = 20)
cat("Little mean |rel err|:\n", file = stderr())
print(little_sum %>% select(setup, offered_demand_per_hr, L_obs_mean, L_pred_mean, rel_err_abs_mean))
