# tornado_plot.R

suppressPackageStartupMessages({ library(tidyverse); library(ggplot2); library(patchwork) })

tor_theme <- function(...) {
  theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.caption = element_text(size = 12, hjust = 0),
      plot.caption.position = "plot",
      legend.position = "bottom", legend.direction = "horizontal",
      panel.grid.minor = element_blank(), ...
    )
}

plot_tornado <- function(effects, metric = c("hpu", "thr")) {
  metric <- match.arg(metric)
  if (metric == "hpu") {
    d <- effects %>%
      mutate(label = forcats::fct_reorder(label, hpu_abs_span),
             low = hpu_low, high = hpu_high, base = hpu_base)
    title <- "Tornado: HPU"
    xlab <- "HPU (labor-hr / unit)"
  } else {
    d <- effects %>%
      mutate(label = forcats::fct_reorder(label, thr_abs_span),
             low = thr_low, high = thr_high, base = thr_base)
    title <- "Tornado: throughput"
    xlab <- "Throughput (veh/hr)"
  }
  ggplot(d, aes(y = label)) +
    geom_vline(aes(xintercept = base), linetype = "dashed", color = "grey40", linewidth = 0.9) +
    geom_segment(aes(x = low, xend = high, yend = label), linewidth = 2.2, color = "#4C78A8") +
    geom_point(aes(x = low, color = "Low"), size = 3.5) +
    geom_point(aes(x = high, color = "High"), size = 3.5) +
    scale_color_manual(values = c(Low = "#0072B2", High = "#D55E00")) +
    labs(title = title, x = xlab, y = NULL, color = NULL,
         caption = "2-bay balanced baseline at 10 veh/hr demand. OAT low/high; dashed = baseline. CRN. Illustrative.") +
    tor_theme()
}

plot_tornado_pair <- function(effects) {
  plot_tornado(effects, "hpu") / plot_tornado(effects, "thr") +
    plot_layout(heights = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom", legend.direction = "horizontal")
}
