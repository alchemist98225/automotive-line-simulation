#!/usr/bin/env Rscript
# Example plots for a DOE output directory produced by scripts/doe_run.R.
#
#   Rscript scripts/doe_plot.R --in results/doe_example
#
# Shows every replication faintly and the mean / loess in bold.
# Marks the minimum mean-HPU design point on the heatmap.
# Results are illustrative only — not predictions for DRI.

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else normalizePath("scripts")
source(file.path(script_dir, "doe_lib.R"))
repo_root <- doe_repo_root(script_dir)

argv <- commandArgs(trailingOnly = TRUE)
in_dir <- "results/doe_example"
if ("--in" %in% argv) {
  i <- match("--in", argv)
  if (is.na(i) || i == length(argv)) stop("--in needs a path", call. = FALSE)
  in_dir <- argv[[i + 1]]
}
if (!grepl("^/", in_dir)) in_dir <- file.path(repo_root, in_dir)
if (!dir.exists(in_dir)) stop("Input directory not found: ", in_dir, call. = FALSE)

rep_path <- file.path(in_dir, "rep_metrics.csv.gz")
sum_path <- file.path(in_dir, "summary.csv")
if (!file.exists(rep_path)) stop("Missing ", rep_path, call. = FALSE)
if (!file.exists(sum_path)) stop("Missing ", sum_path, call. = FALSE)

reps <- readr::read_csv(rep_path, show_col_types = FALSE) %>%
  mutate(hpu_plot = if_else(is.infinite(hpu) | is.na(hpu), NA_real_, hpu))
summary_tbl <- readr::read_csv(sum_path, show_col_types = FALSE)

plot_dir <- file.path(in_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

theme_doe <- function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(size = 10, color = "#444444", hjust = 0, margin = margin(t = 8)),
      legend.position = "bottom",
      plot.margin = margin(10, 14, 10, 14)
    )
}

caption_note <- paste(
  "Illustrative only — placeholder params, not DRI predictions.",
  "HPU = (operators × observation hours) / units completed. Lower is better."
)

# Infer x / colour factors from columns that actually vary
vary_cols <- reps %>%
  select(any_of(c("cap_paint", "cap_body_shop", "cap_final_assembly", "cap_inspection",
                  "op_upstream", "op_downstream", "op_shared",
                  "mean_paint", "wip_limit", "repair_technicians"))) %>%
  summarise(across(everything(), ~ n_distinct(.x))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "n") %>%
  filter(n > 1) %>%
  pull(col)

x_col <- if ("op_upstream" %in% vary_cols) "op_upstream" else if (length(vary_cols)) vary_cols[[1]] else "design_id"
colour_col <- if ("cap_paint" %in% vary_cols) "cap_paint" else {
  other <- setdiff(vary_cols, x_col)
  if (length(other)) other[[1]] else NULL
}

reps_plot <- reps %>%
  mutate(
    x_factor = factor(.data[[x_col]]),
    colour_factor = if (is.null(colour_col)) "all" else factor(.data[[colour_col]])
  )

means_plot <- reps_plot %>%
  group_by(x_factor, colour_factor, design_id) %>%
  summarise(
    hpu_mean = mean(hpu_plot, na.rm = TRUE),
    throughput_mean = mean(throughput_per_hr, na.rm = TRUE),
    .groups = "drop"
  )

# 1) HPU vs primary factor, coloured by secondary factor
p_hpu <- ggplot(reps_plot, aes(x = x_factor, y = hpu_plot, color = colour_factor)) +
  geom_point(alpha = 0.35, size = 2.8, position = position_jitter(width = 0.08, height = 0)) +
  geom_line(
    data = means_plot,
    aes(x = x_factor, y = hpu_mean, group = colour_factor, color = colour_factor),
    linewidth = 1.1, alpha = 0.95
  ) +
  geom_point(
    data = means_plot,
    aes(x = x_factor, y = hpu_mean, color = colour_factor),
    size = 3.5, shape = 18
  ) +
  scale_color_manual(
    values = c("#2c7fb8", "#d95f0e", "#31a354", "#756bb1", "#e7298a", "#666666")
  ) +
  labs(
    title = "HPU across the design (all replications)",
    subtitle = paste0("Faint points = each rep; diamonds + line = mean | x = ", x_col,
                      if (!is.null(colour_col)) paste0(" | colour = ", colour_col) else ""),
    x = x_col, y = "HPU (labor-hours / unit)", color = colour_col %||% NULL,
    caption = caption_note
  ) +
  theme_doe()

ggsave(file.path(plot_dir, "hpu_by_factor.png"), p_hpu, width = 9, height = 6, dpi = 130)

# 2) Throughput companion (same layout)
p_tp <- ggplot(reps_plot, aes(x = x_factor, y = throughput_per_hr, color = colour_factor)) +
  geom_point(alpha = 0.35, size = 2.8, position = position_jitter(width = 0.08, height = 0)) +
  geom_line(
    data = means_plot,
    aes(x = x_factor, y = throughput_mean, group = colour_factor, color = colour_factor),
    linewidth = 1.1
  ) +
  geom_point(
    data = means_plot,
    aes(x = x_factor, y = throughput_mean, color = colour_factor),
    size = 3.5, shape = 18
  ) +
  scale_color_manual(
    values = c("#2c7fb8", "#d95f0e", "#31a354", "#756bb1", "#e7298a", "#666666")
  ) +
  labs(
    title = "Throughput across the design (all replications)",
    subtitle = paste0("Faint points = each rep; diamonds + line = mean | x = ", x_col),
    x = x_col, y = "Throughput (units / hour)", color = colour_col %||% NULL,
    caption = caption_note
  ) +
  theme_doe()

ggsave(file.path(plot_dir, "throughput_by_factor.png"), p_tp, width = 9, height = 6, dpi = 130)

# 3) Heatmap of mean HPU over two factors (when available)
if (!is.null(colour_col) && x_col != colour_col) {
  heat <- summary_tbl %>%
    mutate(
      x = .data[[x_col]],
      y = .data[[colour_col]],
      hpu_mean = hpu_mean
    )
  best <- heat %>% slice_min(hpu_mean, n = 1, with_ties = FALSE)

  p_heat <- ggplot(heat, aes(x = factor(x), y = factor(y), fill = hpu_mean)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.2f", hpu_mean)), size = 4.2, color = "#111111") +
    geom_point(
      data = best, aes(x = factor(x), y = factor(y)),
      shape = 1, size = 10, stroke = 1.4, color = "#111111", inherit.aes = FALSE
    ) +
    scale_fill_gradient(low = "#edf8b1", high = "#2c7fb8", name = "Mean HPU") +
    labs(
      title = "Mean HPU heatmap",
      subtitle = paste0("Circle = lowest mean HPU | ", x_col, " × ", colour_col),
      x = x_col, y = colour_col,
      caption = caption_note
    ) +
    theme_doe() +
    theme(panel.grid = element_blank())

  ggsave(file.path(plot_dir, "hpu_heatmap.png"), p_heat, width = 8, height = 5.5, dpi = 130)
}

# 4) Save the data behind the plots
readr::write_csv(reps_plot, file.path(plot_dir, "hpu_by_factor_data.csv"))
readr::write_csv(means_plot, file.path(plot_dir, "hpu_by_factor_means.csv"))

message("Wrote plots under ", normalizePath(plot_dir))
message(paste(list.files(plot_dir, pattern = "\\.png$"), collapse = ", "))
