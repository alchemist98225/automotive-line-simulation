#!/usr/bin/env Rscript
# Convergence and parameter-diff plots for an opt_run.R output directory.
#
# Example:
#   Rscript scripts/opt_plot.R --in results/opt_example

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else normalizePath("scripts")
source(file.path(script_dir, "opt_lib.R"))

parse_args <- function(argv) {
  opts <- list(input = NULL, help = FALSE)
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
    if (key == "--in") opts$input <- take()
    else stop("Unknown argument: ", key, call. = FALSE)
    i <- i + 1L
  }
  opts
}

opts <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help) || is.null(opts$input)) {
  cat("Usage: Rscript scripts/opt_plot.R --in DIR\n")
  quit(status = if (is.null(opts$input)) 2 else 0)
}

in_dir <- opts$input
if (!grepl("^/", in_dir)) in_dir <- file.path(opt_repo_root(script_dir), in_dir)
stopifnot(dir.exists(in_dir))
plot_dir <- file.path(in_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

hist <- readr::read_csv(file.path(in_dir, "convergence.csv"), show_col_types = FALSE)
diff <- readr::read_csv(file.path(in_dir, "param_diff.csv"), show_col_types = FALSE)
summary <- readr::read_csv(file.path(in_dir, "best_vs_baseline.csv"), show_col_types = FALSE)

# Chart: Optimizer convergence (best and mean fitness by generation)
conv_long <- hist %>%
  select(generation, best = best_fitness, mean = mean_fitness) %>%
  pivot_longer(-generation, names_to = "series", values_to = "fitness") %>%
  mutate(series = recode(series, best = "Best", mean = "Mean"))

p_conv <- ggplot(conv_long, aes(generation, fitness, color = series, linetype = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = c(Best = "#0072B2", Mean = "#D55E00")) +
  scale_linetype_manual(values = c(Best = "solid", Mean = "dashed")) +
  labs(
    title = "Optimizer convergence (best and mean fitness by generation)",
    subtitle = "Fitness = mean HPU + penalty × throughput shortfall vs baseline",
    x = "Generation", y = "Fitness (lower is better)",
    color = NULL, linetype = NULL,
    caption = "Illustrative only — placeholder parameters, not plant predictions."
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.caption = element_text(hjust = 0, size = rel(0.7))
  )

ggsave(file.path(plot_dir, "convergence.png"), p_conv, width = 9, height = 5.5, dpi = 120)
readr::write_csv(conv_long, file.path(plot_dir, "convergence_data.csv"))

# Chart: Best vs baseline decision variables
diff_plot <- diff %>%
  mutate(label = factor(label, levels = rev(label)))

p_diff <- ggplot(diff_plot, aes(y = label)) +
  geom_segment(aes(x = baseline, xend = best, yend = label), linewidth = 0.8, color = "grey50") +
  geom_point(aes(x = baseline, color = "Baseline"), size = 3) +
  geom_point(aes(x = best, color = "Best"), size = 3) +
  scale_color_manual(values = c(Baseline = "#0072B2", Best = "#D55E00")) +
  labs(
    title = "Best vs baseline decision variables",
    x = "Value", y = NULL, color = NULL,
    caption = "Queues are waiting positions; capacity is paint bays; operators are zone crews."
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave(file.path(plot_dir, "param_diff.png"), p_diff, width = 9, height = 5, dpi = 120)
readr::write_csv(diff, file.path(plot_dir, "param_diff_data.csv"))
readr::write_csv(summary, file.path(plot_dir, "best_vs_baseline_data.csv"))

cat(sprintf("Wrote plots under %s\n", plot_dir), file = stderr())
