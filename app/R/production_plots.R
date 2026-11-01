# production_plots.R
# Production-schedule charts and helpers: theoretical shift capacity,
# histogram breaks, and shift_output_plot (vehicles per shift).

# Slowest station if each bay ran its load, automatic, and unload time with no
# waiting, failures, or support work. Vehicles per measured shift.
theoretical_shift_capacity <- function(params) {
  st <- params$stations
  cycle <- vapply(seq_len(nrow(st)), function(i) {
    timing_mean(activity_timing(st[i, ], "load")) +
      timing_mean(activity_timing(st[i, ], "auto")) +
      timing_mean(activity_timing(st[i, ], "unload"))
  }, numeric(1))
  cycle <- pmax(cycle, 1e-9)
  min(st$capacity * params$shift_length_min / cycle)
}
# Integer ticks, every vehicle on a short axis and every other vehicle after that.
integer_shift_breaks <- function(lims) {
  if (length(lims) < 2L || any(!is.finite(lims))) return(numeric())
  lo <- floor(min(lims))
  hi <- ceiling(max(lims))
  if (hi < lo) return(numeric())
  by <- if ((hi - lo) <= 10) 1L else 2L
  seq(lo, hi, by = by)
}
# A long label to the right of a line near the right edge runs off the panel.
# Right-align it so the text stays left of that line.
capacity_label_hjust <- function(cap, x_lo, x_hi) {
  if (!is.finite(cap) || !is.finite(x_lo) || !is.finite(x_hi) || cap >= (x_lo + x_hi) / 2) 1.08 else 0
}
capacity_label <- function(n) sprintf("Theoretical capacity: %.0f vehicles", round(n))
# One vehicle per bar when the counts span about 20 or fewer. Wider spreads use
# Freedman–Diaconis, rounded to a whole number of vehicles.
shift_binwidth <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(1L)
  if (diff(range(x)) <= 20) return(1L)
  iqr <- stats::IQR(x)
  bw <- if (!is.finite(iqr) || iqr <= 0) 1 else 2 * iqr / length(x)^(1 / 3)
  max(1L, as.integer(round(bw)))
}
# Bins centered on integers when the width is 1, so each vehicle count is one bar.
# seq(from, to, by) stops before `to` when the next step would pass it. A wide
# Freedman–Diaconis width (few replications, large IQR) then leaves the largest
# counts outside every bin, and the empty group draws a bar with a missing height.
shift_hist_breaks <- function(x, bw) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(-0.5, 0.5))
  bw <- max(1L, as.integer(bw))
  start <- floor(min(x)) - bw / 2
  n_steps <- max(1L, as.integer(ceiling((max(x) - start) / bw)))
  # ceiling can undershoot by a hair when the span is just under a whole step.
  if (start + bw * n_steps < max(x)) n_steps <- n_steps + 1L
  start + bw * seq(0, n_steps)
}
# Same fill, outline, and line as the cycle-time chart. Comparison uses the set palette.
shift_hist_fill <- "#41b6c4"
shift_line_color <- "#253494"
# Histogram of completed vehicles per shift, one row per replication, plus the
# pooled density. A dashed line is theoretical bay capacity. Several shifts are
# faceted. Two parameter sets are overlaid in the title-aware palette.
# Chart: Vehicles completed per shift (Results → Production schedule).
shift_output_plot <- function(df, capacity, palette = NULL) {
  df <- df %>% dplyr::filter(is.finite(n_completed), is.finite(shift))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No measured shifts to plot.")))
  }
  df <- df %>% mutate(shift_label = factor(
    paste("Shift", shift),
    levels = paste("Shift", sort(unique(shift)))
  ))
  has_set <- !is.null(palette) && "set" %in% names(df)
  cap_df <- NULL
  if (is.data.frame(capacity)) {
    cap_df <- capacity %>% dplyr::filter(is.finite(.data$capacity))
    cap_value <- if (nrow(cap_df)) mean(cap_df$capacity) else NA_real_
  } else {
    cap_value <- if (length(capacity) == 1L && is.finite(capacity)) capacity else NA_real_
  }
  mean_out <- mean(df$n_completed)
  # One paragraph. A text box wraps it to the plot width instead of a fixed 32 characters.
  caption <- "Each rug tick is one replication. The dashed line is theoretical vehicles per shift at the slowest station, ignoring labor, failures, support work, and blocking."
  bw <- shift_binwidth(df$n_completed)
  breaks <- shift_hist_breaks(df$n_completed, bw)
  spread_ok <- function(x) {
    x <- x[is.finite(x)]
    length(x) >= 2L && is.finite(stats::sd(x)) && stats::sd(x) > sqrt(.Machine$double.eps)
  }
  if (has_set) {
    by_set <- df %>% group_by(set) %>% summarise(mean_out = mean(n_completed), .groups = "drop")
    hi <- by_set$set[[which.max(by_set$mean_out)]]
    lo <- by_set$set[[which.min(by_set$mean_out)]]
    title <- plot_subtitle(if (abs(diff(range(by_set$mean_out))) < 0.5) {
      "The two sets complete about the same number of vehicles per shift"
    } else {
      sprintf("%s completes more vehicles per shift than %s", hi, lo)
    }, 32)
    df <- df %>% mutate(set = factor(set, levels = names(palette)))
    varying <- df %>% group_by(set, shift_label) %>% filter(spread_ok(n_completed)) %>% ungroup()
    p <- ggplot(df, aes(x = n_completed, fill = set)) +
      geom_histogram(
        aes(y = after_stat(density)),
        breaks = breaks, position = "identity", alpha = 0.55, colour = "white"
      ) +
      geom_rug(
        aes(y = 0, colour = set),
        sides = "b",
        alpha = 0.6,
        length = grid::unit(0.03, "npc"),
        position = position_jitter(width = 0.15, height = 0, seed = 42),
        show.legend = FALSE
      ) +
      scale_fill_manual(values = palette, drop = FALSE, name = NULL) +
      scale_colour_manual(values = palette, drop = FALSE, name = NULL)
    if (nrow(varying)) {
      p <- p + geom_density(
        data = varying, aes(colour = set), linewidth = 1.15, trim = FALSE,
        na.rm = TRUE, show.legend = FALSE
      )
    }
    split_caps <- !is.null(cap_df) && nrow(cap_df) > 1L && diff(range(cap_df$capacity)) >= 0.5
    cap_vals <- if (!is.null(cap_df)) cap_df$capacity else cap_value
    x_span <- range(c(df$n_completed, cap_vals), na.rm = TRUE)
    if (isTRUE(split_caps)) {
      cap_df <- cap_df %>%
        dplyr::arrange(dplyr::desc(.data$capacity)) %>%
        dplyr::mutate(
          hjust = vapply(capacity, capacity_label_hjust, numeric(1), x_lo = x_span[[1]], x_hi = x_span[[2]]),
          vjust = -0.35 - (dplyr::row_number() - 1L) * 1.15
        )
      p <- p +
        geom_vline(
          data = cap_df, aes(xintercept = .data$capacity, colour = set),
          inherit.aes = FALSE, linetype = "dashed", linewidth = 0.9
        ) +
        geom_text(
          data = cap_df,
          aes(
            x = .data$capacity, label = capacity_label(.data$capacity),
            colour = set, hjust = hjust, vjust = vjust
          ),
          y = Inf, inherit.aes = FALSE, size = 4.2, show.legend = FALSE
        )
    } else if (is.finite(cap_value)) {
      p <- p +
        geom_vline(xintercept = cap_value, linetype = "dashed", linewidth = 0.9, colour = shift_line_color) +
        annotate(
          "text", x = cap_value, y = Inf, label = capacity_label(cap_value),
          hjust = capacity_label_hjust(cap_value, min(df$n_completed), max(df$n_completed, cap_value)),
          vjust = -0.35, colour = shift_line_color, size = 4.2
        )
    }
    p <- p + theme_sim(legend.position = "bottom", plot.margin = margin(6, 10, 8, 8))
  } else {
    ratio <- if (is.finite(cap_value) && cap_value > 0) mean_out / cap_value else NA_real_
    title <- plot_subtitle(if (!is.finite(ratio)) {
      "Completed vehicles in each measured shift"
    } else if (ratio < 0.85) {
      sprintf("Shift output averages %.0f%% of theoretical bay capacity", 100 * ratio)
    } else if (ratio > 1.05) {
      "Shift output is above the simple bay-capacity estimate"
    } else {
      "Shift output is close to theoretical bay capacity"
    }, 32)
    varying <- df %>% group_by(shift_label) %>% filter(spread_ok(n_completed)) %>% ungroup()
    p <- ggplot(df, aes(x = n_completed)) +
      geom_histogram(
        aes(y = after_stat(density)),
        breaks = breaks, fill = shift_hist_fill, colour = "white", alpha = 0.55
      ) +
      geom_rug(
        aes(y = 0),
        sides = "b",
        colour = shift_line_color, alpha = 0.6, length = grid::unit(0.03, "npc"),
        position = position_jitter(width = 0.15, height = 0, seed = 42)
      )
    if (nrow(varying)) {
      p <- p + geom_density(
        data = varying, colour = shift_line_color, linewidth = 1.15,
        trim = FALSE, na.rm = TRUE
      )
    }
    if (is.finite(cap_value)) {
      p <- p +
        geom_vline(xintercept = cap_value, linetype = "dashed", linewidth = 0.9, colour = shift_line_color) +
        annotate(
          "text", x = cap_value, y = Inf, label = capacity_label(cap_value),
          hjust = capacity_label_hjust(cap_value, min(df$n_completed), max(df$n_completed, cap_value)),
          vjust = -0.35, colour = shift_line_color, size = 4.2
        )
    }
    p <- p + theme_sim(plot.margin = margin(6, 10, 8, 8))
  }
  if (nlevels(df$shift_label) > 1L) {
    p <- p + facet_wrap(vars(shift_label), ncol = 2)
  }
  # One to two vehicles of padding. The label stays inside that window.
  x_lo <- min(df$n_completed)
  x_hi <- max(c(df$n_completed, cap_value, if (!is.null(cap_df)) cap_df$capacity), na.rm = TRUE)
  p +
    scale_x_continuous(breaks = integer_shift_breaks) +
    coord_cartesian(xlim = c(x_lo - 1.5, x_hi + 1.5), clip = "off") +
    labs(
      title = title,
      x = "Completed vehicles per shift",
      y = "Density",
      caption = caption
    ) +
    theme(
      plot.title = element_text(colour = "#1a1a1a", margin = margin(0, 0, 20, 0)),
      plot.caption = ggtext::element_textbox_simple(
        colour = "#1a1a1a",
        hjust = 0,
        halign = 0,
        size = 16,
        lineheight = 1.15,
        width = grid::unit(1, "npc"),
        margin = margin(8, 0, 0, 0),
        padding = margin(0, 0, 0, 0)
      )
    )
}
