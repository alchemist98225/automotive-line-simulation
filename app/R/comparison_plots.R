# comparison_plots.R
# Compare-page chart builders: overlaid densities, ridgelines, highlight
# densities, and flow (queue/WIP) overlays for two parameter sets.

# Overlaid densities for two sets. Color follows the title-aware set palette.
# When y is a station or pool with more than one level, each category gets its
# own panel so the sets stay blue and orange. A set with no spread is a vertical
# line. Non-negative metrics start at 0; occupancy stays inside [0, 1].
# Chart: Output distributions / station & operator densities (Compare).
comparison_densities <- function(df, y, x, x_label, subtitle, percent = TRUE, palette = comparison_colors,
                                 subtitle_width = 32L, bounds = NULL) {
  df <- df %>%
    filter(is.finite(.data[[x]])) %>%
    mutate(set = factor(set, levels = names(palette)))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot")))
  }
  if (!identical(y, "set")) {
    if (is.factor(df[[y]])) {
      df[[y]] <- factor(df[[y]], levels = levels(df[[y]]))
    } else {
      df[[y]] <- factor(df[[y]], levels = unique(df[[y]]))
    }
  }
  if (is.null(bounds)) bounds <- if (isTRUE(percent)) c(0, 1) else c(0, Inf)
  colors <- ridge_fill_values(levels(df$set), palette)
  facet <- if (!identical(y, "set") && dplyr::n_distinct(df[[y]]) > 1L) y else NULL
  overlay_densities(
    df, x, "set", colors, x_label, subtitle,
    percent = percent, bounds = bounds, facet = facet, subtitle_width = subtitle_width
  )
}

# The Simulate page clamps .shiny-plot-output at 720px. That still crowds four
# overlaid ridges once the text is larger, and leaves Inspection against the axis.
# Scale the comparison ridgelines with the number of stations or pools.
comparison_ridge_n <- function(x) {
  n <- length(unique(x[!is.na(x)]))
  if (n < 1L) 1L else as.integer(n)
}
comparison_ridge_height <- function(n_ridges) {
  n <- suppressWarnings(as.integer(n_ridges[[1]]))
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 1L
  # Single-column facets need one panel stack; keep phone strips readable.
  as.integer(max(640L, 240L * n + 260L))
}
# Chart helper: ridgeline height wrapper for Compare station ridges.
comparison_ridge_plot <- function(id, n_ridges) {
  h <- comparison_ridge_height(n_ridges)
  div(
    class = "comparison-ridge-plot",
    style = sprintf("--ridge-h:%dpx;", h),
    plotOutput(id, height = paste0(h, "px"), width = "100%", fill = FALSE)
  )
}
# Overlaid densities for one ranked metric. Rug ticks are every replication.
# A set with no spread is a vertical line in that set's color.
comparison_highlight_height <- 460L
# One decimal at or above 1; two significant digits below 1 (0.000338 -> 0.00034).
format_highlight_mean <- function(x) {
  if (!is.finite(x)) return("unavailable")
  if (abs(x) >= 1) sprintf("%.1f", x) else formatC(signif(x, 2), format = "fg", digits = 2)
}
# Per-plot subtitle: just the two means and the change. The shared reading
# notes (rug ticks, no-spread line, normal curves, 95% window) sit once in
# the section intro above the plots instead of on every chart.
comparison_highlight_subtitle <- function(mean_1, mean_2, delta = NA_real_) {
  text <- sprintf(
    "Baseline mean %s. Experiment mean %s.",
    format_highlight_mean(mean_1),
    format_highlight_mean(mean_2)
  )
  pct <- if (is.finite(mean_1) && abs(mean_1) > 1e-8 && is.finite(delta)) 100 * delta / mean_1 else NA_real_
  if (is.finite(pct) && abs(pct) <= 1000) text <- sprintf("%s Change %+.0f%%.", text, pct)
  text
}
# Chart: Biggest changes highlight densities (Compare).
comparison_highlight_plot <- function(df, x_label, subtitle, palette, percent = FALSE, bounds = c(0, Inf)) {
  df <- df %>% mutate(set = factor(.data$set, levels = names(palette)))
  if (isTRUE(percent)) bounds <- c(0, 1)
  comparison_densities(
    df, "set", "value", x_label, subtitle,
    percent = percent, palette = palette,
    subtitle_width = 32L, bounds = bounds
  ) +
    # The 95% window note lives in the section intro, not on each chart.
    labs(caption = NULL)
}
comparison_highlight_frame <- function(id) {
  h <- comparison_highlight_height
  div(
    class = "comparison-ridge-plot",
    style = sprintf("--ridge-h:%dpx;", h),
    plotOutput(id, height = paste0(h, "px"), width = "100%", fill = FALSE)
  )
}

# Every replication is a faint line. The ribbon is a loess fit on all of them.
# Chart: Queue / WIP flow overlay for two sets (Compare).
comparison_flow <- function(df, column, y_label, facet = NULL, palette = comparison_colors) {
  df <- df %>% mutate(set = factor(set, levels = names(palette)))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No time-series observations.")))
  }
  df$.line <- interaction(
    df$set,
    df$replication,
    if (is.null(facet)) 1L else df[[facet]],
    drop = TRUE
  )
  p <- ggplot(df, aes(x = time, y = .data[[column]], color = set, fill = set)) +
    geom_line(aes(group = .line), alpha = 0.12, linewidth = 0.4, na.rm = TRUE) +
    geom_smooth(
      aes(group = set),
      method = "loess", formula = y ~ x, se = TRUE,
      linewidth = 1.15, alpha = 0.18, span = 0.35, na.rm = TRUE
    ) +
    scale_color_manual(values = palette, drop = FALSE, name = NULL) +
    scale_fill_manual(values = palette, drop = FALSE, name = NULL) +
    labs(
      x = "Simulation time (min)",
      y = y_label,
      subtitle = plot_subtitle("Faint lines = each replication; bold = loess + ribbon. Each set uses its own window.")
    ) +
    theme_sim(legend.position = "bottom")
  if (!is.null(facet)) p <- p + facet_wrap(vars(.data[[facet]]), ncol = 1, labeller = label_wrap_gen(width = 14))
  p
}

