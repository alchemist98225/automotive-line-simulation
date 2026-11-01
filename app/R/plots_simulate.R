# plots_simulate.R
# Overlay / replication density builders.
# Used by Compare "Output distributions" and related density charts
# (via comparison_densities), and available for Simulate-style overlays.

# Each curve is scaled to its own peak so a tight set cannot flatten the other.
# A series with no spread is a labeled vertical line: the kernel has no bandwidth.
# The line is opaque; the fill uses the same color at alpha 0.30.
# Chart helper: overlaid densities with rug ticks (Compare Output distributions and faceted station densities).
overlay_densities <- function(df, value, series, colors, x_label, subtitle,
                              percent = FALSE, bounds = NULL, facet = NULL,
                              subtitle_width = 32L) {
  empty <- ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot"))
  if (!nrow(df) || !value %in% names(df) || !series %in% names(df)) return(empty)
  df <- df %>% dplyr::filter(is.finite(.data[[value]]))
  if (!nrow(df)) return(empty)
  if (is.factor(df[[series]])) {
    df$series <- factor(df[[series]], levels = intersect(levels(df[[series]]), unique(df[[series]])))
  } else {
    df$series <- factor(df[[series]], levels = unique(df[[series]]))
  }
  df$.value <- df[[value]]
  group_cols <- "series"
  if (!is.null(facet)) group_cols <- c(facet, "series")
  spread <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      marker = stats::median(.value, na.rm = TRUE),
      n = dplyr::n(),
      s = stats::sd(.value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(constant = n < 2L | !is.finite(s) | s <= sqrt(.Machine$double.eps))
  varying <- df %>% dplyr::semi_join(dplyr::filter(spread, !constant), by = group_cols)
  constant <- spread %>% dplyr::filter(constant)
  if (is.null(bounds)) bounds <- if (isTRUE(percent)) c(0, 1) else c(0, Inf)
  bnds <- density_bounds(if (nrow(varying)) varying$.value else df$.value, bounds)
  p <- ggplot(df, aes(x = .value, colour = series,
                      fill = series, group = series))
  if (nrow(varying)) {
    bw_of <- function(x) {
      w <- 2 * IQR(x) / length(x)^(1/3)
      # IQR is 0 when most reps tie; fall back to about sqrt(n) bins.
      if (!is.finite(w) || w <= 0) w <- diff(range(x)) / ceiling(sqrt(length(x)))
      if (all(x == round(x))) max(1, round(w)) else w
    }
    p <- p +
      geom_histogram(
        data = varying,
        mapping = aes(
          x = .value, y = after_stat(count),
          fill = series, colour = series,
          group = series
        ),
        inherit.aes = FALSE,
        position = "identity",
        binwidth = bw_of,
        boundary = 0,
        alpha = 0.30,
        linewidth = 0.6,
        na.rm = TRUE
      )
    norm_df <- varying %>%
      dplyr::group_by(dplyr::across(
        dplyr::all_of(group_cols)
      )) %>%
      dplyr::reframe(
        x = seq(min(.value), max(.value),
                length.out = 200),
        y = dnorm(x, mean(.value), sd(.value)) *
          dplyr::n() * bw_of(.value)
      )
    p <- p +
      geom_line(
        data = norm_df,
        aes(x = x, y = y, colour = series,
            group = series),
        inherit.aes = FALSE,
        linetype = "dashed",
        linewidth = 0.5
      )
  }
  if (nrow(constant)) {
    x_mid <- mean(range(df$.value))
    if (is.null(facet)) {
      constant <- constant %>%
        dplyr::arrange(marker, series) %>%
        dplyr::mutate(label_i = dplyr::row_number())
    } else {
      constant <- constant %>%
        dplyr::group_by(.data[[facet]]) %>%
        dplyr::arrange(marker, series, .by_group = TRUE) %>%
        dplyr::mutate(label_i = dplyr::row_number()) %>%
        dplyr::ungroup()
    }
    constant <- constant %>%
      dplyr::mutate(
        label_x = dplyr::if_else(marker <= x_mid, marker, marker),
        label_y = 1.12 - (label_i - 1L) * 0.16,
        label_hjust = dplyr::if_else(marker <= x_mid, -0.08, 1.08)
      )
    p <- p +
      geom_vline(
        data = constant,
        aes(xintercept = marker, colour = series),
        inherit.aes = FALSE,
        linewidth = 1,
        show.legend = !nrow(varying)
      ) +
      geom_text(
        data = constant,
        aes(
          x = label_x, y = label_y, label = "No spread",
          colour = series, hjust = label_hjust
        ),
        inherit.aes = FALSE,
        vjust = 0,
        size = 4.2,
        show.legend = FALSE
      )
  }
  p <- p +
    geom_rug(
      mapping = aes(x = .value, colour = series),
      inherit.aes = FALSE,
      alpha = 0.85,
      linewidth = 0.45,
      sides = "b",
      length = grid::unit(0.035, "npc"),
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    scale_colour_manual(
      values = colors, aesthetics = c("colour", "fill"),
      limits = names(colors), drop = FALSE, name = NULL
    ) +
    labs(
      x = x_label,
      y = "Replications",
      colour = NULL,
      fill = NULL,
      subtitle = subtitle
    ) +
    theme_sim(legend.position = "bottom", plot.margin = margin(12, 12, 14, 14)) +
    theme_plot_width_text() +
    theme_minor_ticks() +
    guides(
      # One column so long station names are not clipped on a 390px phone.
      colour = guide_legend(ncol = 1, byrow = TRUE),
      fill = guide_legend(ncol = 1, byrow = TRUE)
    )
  xlim <- density_xlim(df$.value, level = 0.95, bounds = bounds)
  faceted <- !is.null(facet) && dplyr::n_distinct(df[[facet]]) > 1L
  if (faceted) {
    # One column so phone (~390px) keeps full strip titles and x labels.
    # Percent metrics share [0, 1] so a 100% panel cannot expand past the edge.
    # Non-percent keep free_x so a high station does not re-squash the rest.
    facet_scales <- if (isTRUE(percent)) "fixed" else "free_x"
    if (isTRUE(percent)) {
      p <- p + labeled_continuous_scale(
        "x", percent = TRUE, n = 3L,
        expand = expansion(mult = c(0.06, 0.18)),
        limits = c(0, 1)
      )
    } else {
      p <- p + sparse_x_continuous(percent = FALSE, n = 3L, expand = expansion(mult = c(0.08, 0.12)))
    }
    p <- p +
      facet_wrap(
        # Width 22 avoids mid-word wraps like "Final assemb" on phone.
        vars(.data[[facet]]), ncol = 1, scales = facet_scales,
        labeller = label_wrap_gen(width = 22)
      ) +
      theme(
        strip.text = element_text(
          size = 15, colour = "#1a1a1a", hjust = 0,
          margin = margin(8, 8, 10, 4)
        ),
        plot.margin = margin(14, 32, 18, 12),
        # Match other axis text (16) so phone occupancy ticks are not tiny.
        axis.text.x = element_text(size = 16, colour = "#1a1a1a"),
        axis.text.y = element_text(size = 16, colour = "#1a1a1a"),
        axis.title.x = element_text(size = 16, colour = "#1a1a1a")
      )
    p + coord_cartesian(expand = FALSE, clip = "off")
  } else {
    p <- p +
      sparse_x_continuous(
        percent = percent, n = 3L,
        # Extra right expand so phone edge labels (e.g. 0.75) are not clipped.
        expand = expansion(mult = c(0.10, 0.22))
      ) +
      labs(caption = "x-axis shows the central 95% of values") +
      theme(
        plot.margin = margin(14, 40, 22, 12),
        axis.text.x = element_text(size = 16, colour = "#1a1a1a"),
        axis.text.y = element_text(size = 16, colour = "#1a1a1a"),
        axis.title.x = element_text(size = 16, colour = "#1a1a1a")
      )
    p + coord_cartesian(xlim = xlim, expand = FALSE, clip = "off")
  }
}
# One overlaid density per category. A category with no spread is a vertical line.
# Chart helper: one density per category (station/pool), with rug ticks.
replication_densities <- function(df, y, x, x_label, subtitle, percent = TRUE, fill_values = NULL) {
  y_sym <- rlang::sym(y)
  x_sym <- rlang::sym(x)
  df <- df %>%
    dplyr::filter(is.finite(!!x_sym)) %>%
    dplyr::mutate(series_label = !!y_sym)
  if (is.factor(df$series_label)) {
    df$series <- factor(df$series_label, levels = levels(df$series_label))
  } else {
    df$series <- factor(df$series_label, levels = unique(df$series_label))
  }
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot")))
  }
  colors <- ridge_fill_values(levels(df$series), fill_values)
  bounds <- if (isTRUE(percent)) c(0, 1) else c(0, Inf)
  overlay_densities(
    df, x, "series", colors, x_label, subtitle,
    percent = percent, bounds = bounds
  )
}
