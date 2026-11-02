# warmup_plot.R — Welch ensemble WIP with current settling mark.

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(ggtext)
})

wu_theme <- function(...) {
  theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = ggtext::element_textbox_simple(
        size = 12, colour = "#1a1a1a", hjust = 0, halign = 0,
        lineheight = 1.25, padding = margin(2, 2, 2, 2),
        margin = margin(8, 0, 2, 0)
      ),
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 14, 10, 14),
      ...
    )
}

plot_welch_warmup <- function(smooth_tbl, recommend_tbl, current_warmup = 300) {
  cols <- c("Default line" = "#0072B2", "2-bay balanced" = "#D55E00")
  smooth_tbl <- smooth_tbl %>%
    mutate(setup_label = factor(setup_label, levels = names(cols)))

  # Prefer smoothed ribbon columns when present
  has_sm_ci <- all(c("wip_lo_smooth", "wip_hi_smooth") %in% names(smooth_tbl))
  if (has_sm_ci) {
    smooth_tbl <- smooth_tbl %>%
      mutate(ymin = wip_lo_smooth, ymax = wip_hi_smooth)
  } else {
    smooth_tbl <- smooth_tbl %>% mutate(ymin = wip_lo, ymax = wip_hi)
  }

  y_min <- min(smooth_tbl$ymin, na.rm = TRUE)
  y_max <- max(smooth_tbl$ymax, na.rm = TRUE)
  y_pad <- 0.08 * (y_max - y_min)
  y_lo <- max(0, y_min - y_pad)
  # If data sits well above 0, don't force a 0 floor
  if (y_min > 0.6) y_lo <- y_min - y_pad

  rec <- recommend_tbl %>%
    filter(is.finite(recommend_warmup_min)) %>%
    mutate(setup_label = factor(setup_label, levels = names(cols)))

  # Annotation y near top of panel
  ann_y <- y_max - 0.05 * (y_max - y_lo)
  ann_cur <- tibble(
    time_min = current_warmup,
    lab = sprintf("Current settle\n%d min", current_warmup),
    y = ann_y
  )
  ann_rec <- rec %>%
    mutate(
      lab = sprintf("Recommend\n%.0f min", recommend_warmup_min),
      y = ann_y - 0.12 * (y_max - y_lo) * (as.integer(setup_label) - 1)
    )

  caption <- stringr::str_wrap(
    "Solid = Welch smooth of ensemble-mean WIP; ribbon = same window on 95% CI bounds. Offered demand 6 veh/hr. Illustrative.",
    width = 95
  )

  ggplot(smooth_tbl, aes(time_min, wip_smooth, color = setup_label, fill = setup_label)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.18, color = NA) +
    geom_line(linewidth = 1.15) +
    geom_vline(xintercept = current_warmup, linetype = "dashed",
               linewidth = 0.9, color = "grey30") +
    geom_vline(
      data = rec,
      aes(xintercept = recommend_warmup_min, color = setup_label),
      linetype = "dotted", linewidth = 0.9, show.legend = FALSE
    ) +
    geom_text(
      data = ann_cur, aes(x = time_min, y = y, label = lab),
      inherit.aes = FALSE, hjust = -0.05, vjust = 1, size = 3.6, color = "grey25",
      lineheight = 0.95
    ) +
    geom_text(
      data = ann_rec, aes(x = recommend_warmup_min, y = y, label = lab, color = setup_label),
      inherit.aes = FALSE, hjust = -0.05, vjust = 1, size = 3.6, lineheight = 0.95,
      show.legend = FALSE
    ) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    coord_cartesian(ylim = c(y_lo, y_max + y_pad)) +
    labs(
      title = "Welch warm-up: ensemble WIP",
      subtitle = "Moving-window smooth of mean WIP (ribbon uses the same window)",
      x = "Time (min)", y = "WIP (vehicles)",
      caption = caption
    ) +
    wu_theme()
}
