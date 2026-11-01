# plots_bottleneck.R
# Bottleneck & capacity charts: wait-reason bars (ggiraph) and related helpers.

# Chart: Why occupied bays wait (Results → Bottleneck & capacity).
wait_reason_bars_interactive <- function(waits) {
  type_lab <- c(
    blocked = "Blocked after processing",
    waiting_for_operator = "Waiting for operator",
    waiting_for_automatic_capacity = "Awaiting automatic capacity"
  )
  df <- waits %>%
    dplyr::mutate(
      station = factor(label_station(station), levels = unname(station_labels)),
      type = factor(
        unname(type_lab[as.character(type)]),
        levels = unname(type_lab)
      )
    )
  summary_df <- df %>%
    dplyr::group_by(station, type) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(minutes, na.rm = TRUE),
      sd = stats::sd(minutes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = dplyr::if_else(n > 1L, sd / sqrt(n), NA_real_),
      tcrit = dplyr::if_else(n > 1L, stats::qt(0.975, n - 1L), NA_real_),
      half = se * tcrit,
      lower = dplyr::if_else(is.finite(half), mean - half, mean),
      upper = dplyr::if_else(is.finite(half), mean + half, mean),
      tooltip = sprintf(
        "%s · %s<br/>Mean: %.1f bay-min<br/>95%% CI: %.1f – %.1f",
        as.character(station), as.character(type), mean, lower, upper
      ),
      data_id = paste(station, type, sep = "::")
    )
  fill_cols <- c(
    "Blocked after processing" = "#d95f0e",
    "Waiting for operator" = "#2c7fb8",
    "Awaiting automatic capacity" = "#31a354"
  )
  p <- ggplot(summary_df, aes(x = station, y = mean, fill = type)) +
    ggiraph::geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = position_dodge(width = 0.8), width = 0.72,
      colour = "#1a1a1a", linewidth = 0.25
    ) +
    ggiraph::geom_errorbar_interactive(
      aes(ymin = lower, ymax = upper, tooltip = tooltip, data_id = data_id),
      position = position_dodge(width = 0.8), width = 0.2,
      linewidth = 0.65, colour = "#1a1a1a"
    ) +
    scale_fill_manual(values = fill_cols, drop = FALSE) +
    labeled_continuous_scale("y", percent = FALSE, n = 3L, expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = NULL, y = "Mean bay-minutes per measured run", fill = NULL,
      caption = "Bars = mean across replications; error bars = 95% CI. Hover or tap a bar for its value."
    ) +
    theme_sim(legend.position = "bottom", plot.margin = margin(8, 12, 18, 10)) +
    theme_plot_width_text() +
    theme_minor_ticks() +
    theme(axis.text.x = element_text(size = 13, colour = "#1a1a1a", angle = 28, hjust = 1, vjust = 1)) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
    coord_cartesian(clip = "off")
  ggiraph::girafe(
    ggobj = p,
    width_svg = 9, height_svg = 6.6,
    options = list(
      ggiraph::opts_tooltip(
        css = "background:#1a1a1a;color:#fff;padding:8px 10px;border-radius:6px;font-size:14px;line-height:1.35;",
        opacity = 0.95
      ),
      ggiraph::opts_hover(css = "filter:brightness(1.08);stroke-width:1.2px;"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_toolbar(saveaspng = FALSE)
    )
  )
}
