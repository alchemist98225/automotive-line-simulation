# break_plot.R
# Headline chart for the demand break-sweep: throughput vs offered demand with
# y = x reference, rep-spread ribbons, lead-time panel, and annotated break
# points. Sourced by scripts/break_sweep.R (or call break_plot_sweep() directly).
#
# Chart title: "Where the line breaks"
# Style: base size 16, legend bottom one row, large points, dashed references.

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(patchwork)
  library(ggtext)
})

break_theme <- function(...) {
  theme_minimal(base_size = 16) +
    theme(
      text = element_text(size = 16, colour = "#1a1a1a"),
      plot.title = element_text(face = "bold", size = 16, colour = "#1a1a1a"),
      plot.subtitle = element_text(size = 14, colour = "#1a1a1a"),
      plot.caption = element_text(size = 12, colour = "#1a1a1a", hjust = 0,
                                 margin = margin(8, 0, 0, 0)),
      plot.caption.position = "plot",
      axis.title = element_text(size = 16, colour = "#1a1a1a"),
      axis.text = element_text(size = 16, colour = "#1a1a1a"),
      legend.title = element_blank(),
      legend.text = element_text(size = 16, colour = "#1a1a1a"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text = element_text(size = 16, colour = "#1a1a1a"),
      panel.grid.minor = element_blank(),
      ...
    )
}

break_setup_colors <- function() {
  c("Default line" = "#0072B2", "2-bay balanced" = "#D55E00")
}

# Build the two-panel headline figure; returns a patchwork object.
break_plot_sweep <- function(summary_tbl, breakpoints, fill_threshold = 0.95) {
  cols <- break_setup_colors()
  summary_tbl <- summary_tbl %>%
    mutate(setup_label = factor(setup_label, levels = names(cols)))
  breakpoints <- breakpoints %>%
    mutate(setup_label = factor(setup_label, levels = names(cols)))

  x_max <- max(summary_tbl$offered_demand_per_hr, na.rm = TRUE)
  y_max <- max(c(summary_tbl$throughput_p90, summary_tbl$offered_demand_per_hr), na.rm = TRUE)
  x_breaks <- pretty(c(0, x_max), n = 8)
  y_breaks <- pretty(c(0, y_max), n = 8)

  thr_panel <- ggplot(summary_tbl, aes(offered_demand_per_hr, throughput_mean,
                                       color = setup_label, fill = setup_label)) +
    geom_ribbon(aes(ymin = throughput_p10, ymax = throughput_p90),
                alpha = 0.18, color = NA) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                linewidth = 0.8, color = "grey40") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.2) +
    geom_vline(
      data = breakpoints %>% filter(found),
      aes(xintercept = break_demand_per_hr, color = setup_label),
      linetype = "dashed", linewidth = 0.9, show.legend = FALSE
    ) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(breaks = x_breaks, limits = c(0, NA)) +
    scale_y_continuous(breaks = y_breaks, limits = c(0, NA)) +
    labs(
      title = "Where the line breaks",
      subtitle = "Throughput vs offered demand (dashed: thr = demand)",
      x = NULL,
      y = "Throughput (veh/hr)"
    ) +
    break_theme()

  # Lead-time blow-up when the line saturates.
  lt_max <- max(summary_tbl$lead_time_p90, na.rm = TRUE)
  lt_breaks <- pretty(c(0, lt_max), n = 6)
  lt_panel <- ggplot(summary_tbl, aes(offered_demand_per_hr, lead_time_mean,
                                      color = setup_label, fill = setup_label)) +
    geom_ribbon(aes(ymin = lead_time_p10, ymax = lead_time_p90),
                alpha = 0.18, color = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.2) +
    geom_vline(
      data = breakpoints %>% filter(found),
      aes(xintercept = break_demand_per_hr, color = setup_label),
      linetype = "dashed", linewidth = 0.9, show.legend = FALSE
    ) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(breaks = x_breaks, limits = c(0, NA)) +
    scale_y_continuous(breaks = lt_breaks, limits = c(0, NA)) +
    labs(
      x = "Offered demand (veh/hr)",
      y = "Mean lead time (min)"
    ) +
    break_theme()

  # Short caption; break values are marked by the dashed verticals on the chart.
  # str_wrap width ~95 fits an 11in PNG at size 12 with plot margins.
  n_reps_caption <- if ("n_reps" %in% names(summary_tbl)) {
    max(summary_tbl$n_reps, na.rm = TRUE)
  } else {
    NA_integer_
  }
  caption_raw <- paste0(
    "Break point: first demand where mean throughput < ",
    round(100 * fill_threshold),
    "% of demand. Bands: 10th–90th percentile across ",
    if (is.finite(n_reps_caption)) paste0(n_reps_caption, " replications") else "replications",
    ", after warm-up. Illustrative parameters, not plant predictions."
  )
  caption <- stringr::str_wrap(caption_raw, width = 95)

  (thr_panel / lt_panel) +
    plot_layout(heights = c(1.15, 1), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.caption = ggtext::element_textbox_simple(
        size = 12,
        colour = "#1a1a1a",
        hjust = 0,
        halign = 0,
        lineheight = 1.25,
        padding = margin(2, 2, 2, 2),
        margin = margin(8, 0, 2, 0)
      ),
      plot.margin = margin(8, 14, 10, 14)
    ) &
    plot_annotation(caption = caption)
}

break_save_plot <- function(plot_obj, path, width = 11, height = 10, dpi = 150) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = plot_obj, width = width, height = height, dpi = dpi,
                  bg = "white")
  invisible(path)
}
