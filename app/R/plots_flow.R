# plots_flow.R
# Occupancy / operator / reliability summary tables, downtime bars, and
# Flow-over-time / lead-time charts.
# Drives: Station occupancy, Operators, Reliability losses, Downtime bars,
# Queues, WIP, Cycle time.

# can be compared directly; put far-apart stations (e.g. a bottleneck) on their
# own panel with a free occupancy axis. At most three bands. A single panel when
# there is no clear separation.
occupancy_band_labels <- c(
  "1" = "Lower occupancy",
  "2" = "Mid occupancy",
  "3" = "Higher occupancy"
)
occupancy_groups <- function(per_rep, gap = 0.15, max_groups = 3L) {
  med <- per_rep %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(median_util = stats::median(avg_util, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(median_util)
  n <- nrow(med)
  if (n <= 1L || !any(is.finite(med$median_util))) {
    med$band <- factor("All stations", levels = "All stations")
    return(med)
  }
  vals <- med$median_util
  gaps <- diff(vals)
  # Candidate splits: largest gaps that clear the absolute threshold.
  # Cap at max_groups - 1 splits. When no gap clears the threshold, stay as one.
  order_gaps <- order(gaps, decreasing = TRUE)
  keep <- order_gaps[gaps[order_gaps] >= gap]
  n_split <- min(length(keep), as.integer(max_groups) - 1L)
  if (n_split < 1L) {
    med$band <- factor("All stations", levels = "All stations")
    return(med)
  }
  split_after <- sort(keep[seq_len(n_split)])
  band_id <- integer(n)
  band_id[[1]] <- 1L
  next_id <- 1L
  for (i in seq_len(n - 1L)) {
    if (i %in% split_after) next_id <- next_id + 1L
    band_id[[i + 1L]] <- next_id
  }
  n_bands <- max(band_id)
  if (n_bands == 2L) {
    lab <- c("1" = "Lower occupancy", "2" = "Higher occupancy")
  } else if (n_bands == 3L) {
    lab <- occupancy_band_labels
  } else {
    lab <- setNames(paste("Occupancy group", seq_len(n_bands)), as.character(seq_len(n_bands)))
  }
  med$band <- factor(
    unname(lab[as.character(band_id)]),
    levels = unname(lab[as.character(seq_len(n_bands))])
  )
  med
}
# Mean ± 95% CI across replications for a numeric column, one row per category.
rep_mean_ci <- function(df, category, value, conf = 0.95) {
  cat_sym <- rlang::sym(category)
  val_sym <- rlang::sym(value)
  df %>%
    dplyr::filter(is.finite(!!val_sym)) %>%
    dplyr::group_by(!!cat_sym) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!val_sym, na.rm = TRUE),
      sd = stats::sd(!!val_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = dplyr::if_else(n > 1L, sd / sqrt(n), NA_real_),
      tcrit = dplyr::if_else(n > 1L, stats::qt(1 - (1 - conf) / 2, n - 1L), NA_real_),
      half = se * tcrit,
      lower = mean - half,
      upper = mean + half,
      lower = dplyr::if_else(is.finite(lower), lower, mean),
      upper = dplyr::if_else(is.finite(upper), upper, mean)
    )
}

# Vertical bars of bay occupancy with 95% CI error bars. Stations with similar
# medians still share a free-y panel so a bottleneck does not squash the rest.
# Faint points keep every replication visible without cluttering the mean bars.
# Chart helper: Bay occupancy bars with 95% CI (Quarto / legacy; Shiny uses the occupancy table).
occupancy_boxes <- function(per_rep) {
  groups <- occupancy_groups(per_rep)
  plot_df <- per_rep %>%
    dplyr::left_join(groups %>% dplyr::select(label, band), by = "label")
  station_order <- groups %>% dplyr::arrange(median_util) %>% dplyr::pull(label)
  wrap_lab <- function(x) stringr::str_wrap(as.character(x), width = 10)
  plot_df <- plot_df %>%
    dplyr::mutate(
      label = factor(label, levels = station_order),
      label_wrapped = factor(wrap_lab(label), levels = wrap_lab(station_order))
    )
  summary_df <- rep_mean_ci(plot_df, "label_wrapped", "avg_util") %>%
    dplyr::left_join(
      plot_df %>% dplyr::distinct(label_wrapped, label, band),
      by = "label_wrapped"
    )
  n_bands <- dplyr::n_distinct(plot_df$band)
  p <- ggplot() +
    geom_col(
      data = summary_df,
      aes(x = label_wrapped, y = mean, fill = label),
      width = 0.72, colour = "#1a1a1a", linewidth = 0.25
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = label_wrapped, ymin = lower, ymax = upper),
      width = 0.22, linewidth = 0.7, colour = "#1a1a1a"
    ) +
    geom_point(
      data = plot_df,
      aes(x = label_wrapped, y = avg_util, colour = label),
      size = 2.2, alpha = 0.45,
      position = position_jitter(width = 0.08, height = 0, seed = 42)
    ) +
    scale_fill_manual(values = station_colors, drop = FALSE, guide = "none") +
    scale_colour_manual(values = station_colors, drop = FALSE, guide = "none") +
    labeled_continuous_scale("y", percent = TRUE, n = 3L, expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = NULL, y = "Bay occupancy",
      caption = if (n_bands > 1L) {
        "Bars = mean across replications; error bars = 95% CI. Faint points = each replication. Similar-occupancy stations share a panel with its own axis."
      } else {
        "Bars = mean across replications; error bars = 95% CI. Faint points = each replication."
      }
    ) +
    theme_sim(legend.position = "none", plot.margin = margin(8, 12, 14, 10)) +
    theme_plot_width_text(panel.spacing.y = grid::unit(18, "pt")) +
    theme_minor_ticks() +
    theme(
      strip.text = element_text(
        size = 16, colour = "#1a1a1a", hjust = 0, margin = margin(8, 4, 8, 4)
      ),
      # Angle keeps four station names readable at ~390px phone width.
      axis.text.x = element_text(size = 13, colour = "#1a1a1a", angle = 28, hjust = 1, vjust = 1)
    )
  if (n_bands > 1L) {
    p <- p + facet_wrap(
      ~band, scales = "free", ncol = 1,
      labeller = label_wrap_gen(width = 18)
    )
  }
  p + coord_cartesian(clip = "on")
}

# Generic per-entity summary across replications: mean, 95% CI, min, max.
# Sorted highest-first. clamp = c(lo, hi) for fractions; NULL leaves CI unbounded.
# Used by the bay-occupancy table (which keeps CI / min / max).
rep_summary_df <- function(
  per_rep,
  value_col = "avg_util",
  label_col = "label",
  entity_name = "Station",
  conf = 0.95,
  clamp = c(0, 1)
) {
  stopifnot(all(c(label_col, value_col) %in% names(per_rep)))
  alpha <- (1 - conf) / 2
  lab_sym <- rlang::sym(label_col)
  val_sym <- rlang::sym(value_col)
  out <- per_rep %>%
    dplyr::filter(is.finite(!!val_sym)) %>%
    dplyr::group_by(!!lab_sym) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!val_sym),
      sd = stats::sd(!!val_sym),
      min = min(!!val_sym),
      max = max(!!val_sym),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = dplyr::if_else(.data$n > 1L, .data$sd / sqrt(.data$n), NA_real_),
      tcrit = dplyr::if_else(
        .data$n > 1L,
        stats::qt(1 - alpha, .data$n - 1L),
        NA_real_
      ),
      half = .data$se * .data$tcrit,
      ci_low = dplyr::if_else(is.finite(.data$half), .data$mean - .data$half, .data$mean),
      ci_high = dplyr::if_else(is.finite(.data$half), .data$mean + .data$half, .data$mean)
    )
  if (!is.null(clamp)) {
    stopifnot(length(clamp) == 2L, is.finite(clamp[[1]]), is.finite(clamp[[2]]))
    out <- out %>%
      dplyr::mutate(
        ci_low = pmax(clamp[[1]], .data$ci_low),
        ci_high = pmin(clamp[[2]], .data$ci_high)
      )
  }
  out %>%
    dplyr::arrange(dplyr::desc(.data$mean), !!lab_sym) %>%
    dplyr::transmute(
      !!entity_name := as.character(!!lab_sym),
      Mean = .data$mean,
      `CI low` = .data$ci_low,
      `CI high` = .data$ci_high,
      Min = .data$min,
      Max = .data$max
    )
}

# Compact DT look shared by occupancy / operators / reliability tables:
# no search box, full-contrast text, optional percent/round columns, and an
# in-cell styleColorBar on the main metric (the "geom" flavor Eddie wants).
compact_metric_dt <- function(
  tbl,
  percent_cols = character(),
  round_cols = character(),
  round_digits = 2,
  bar_col = NULL,
  bar_range = c(0, 1)
) {
  stopifnot(is.data.frame(tbl), ncol(tbl) >= 1L)
  n_cols <- ncol(tbl)
  right_targets <- if (n_cols >= 2L) seq.int(1L, n_cols - 1L) else integer()
  dt <- DT::datatable(
    tbl,
    rownames = FALSE,
    class = "compact stripe hover",
    escape = TRUE,
    width = "100%",
    height = "auto",
    options = list(
      dom = "t",
      ordering = FALSE,
      paging = FALSE,
      info = FALSE,
      scrollX = FALSE,
      autoWidth = FALSE,
      columnDefs = list(
        list(className = "dt-left", targets = 0),
        list(className = "dt-right", targets = right_targets)
      )
    )
  )
  pct_keep <- intersect(percent_cols, names(tbl))
  if (length(pct_keep)) {
    dt <- DT::formatPercentage(dt, pct_keep, digits = 1)
  }
  rnd_keep <- intersect(round_cols, names(tbl))
  if (length(rnd_keep)) {
    dt <- DT::formatRound(dt, rnd_keep, digits = round_digits)
  }
  if (!is.null(bar_col)) {
    stopifnot(bar_col %in% names(tbl))
    dt <- DT::formatStyle(
      dt,
      bar_col,
      background = DT::styleColorBar(bar_range, "#6baed6"),
      backgroundSize = "98% 62%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center",
      color = "#1a1a1a",
      fontWeight = "700"
    )
  }
  # htmlwidgets often ships a 400px inline height; collapse to content.
  htmlwidgets::onRender(
    dt,
    "function(el, x) {
       el.style.height = 'auto';
       el.style.minHeight = '0';
       var wrap = el.querySelector('.dataTables_wrapper');
       if (wrap) { wrap.style.height = 'auto'; wrap.style.minHeight = '0'; }
     }"
  )
}

# Chart/table: Bay occupancy by station (Bottleneck → Station occupancy).
occupancy_summary_df <- function(per_rep, conf = 0.95) {
  rep_summary_df(
    per_rep,
    value_col = "avg_util",
    label_col = "label",
    entity_name = "Station",
    conf = conf,
    clamp = c(0, 1)
  )
}

# Chart/table: Bay occupancy by station (Bottleneck → Station occupancy).
occupancy_summary_dt <- function(tbl) {
  keep <- c("Station", "Mean", "CI low", "CI high", "Min", "Max")
  stopifnot(all(keep %in% names(tbl)))
  compact_metric_dt(
    tbl[, keep, drop = FALSE],
    percent_cols = keep[-1],
    bar_col = "Mean",
    bar_range = c(0, 1)
  )
}

# Reliability losses: same content as before (means of repair / wait / total
# minutes and unavailable fraction). Display labels only; sorted worst-first.
# Bar on Unavailable fraction scales to the column max so small fractions show.
reliability_losses_df <- function(downtime_by_rep) {
  stopifnot(all(c(
    "station", "downtime_min", "waiting_repair_min",
    "total_downtime_min", "total_downtime_fraction"
  ) %in% names(downtime_by_rep)))
  downtime_by_rep %>%
    dplyr::group_by(.data$station) %>%
    dplyr::summarise(
      `Active repair minutes` = mean(.data$downtime_min),
      `Waiting for repair minutes` = mean(.data$waiting_repair_min),
      `Total unavailable minutes` = mean(.data$total_downtime_min),
      `Unavailable fraction` = mean(.data$total_downtime_fraction),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Unavailable fraction`), .data$station) %>%
    dplyr::mutate(Station = label_station(.data$station), .before = 1L) %>%
    dplyr::select(-"station")
}

reliability_losses_dt <- function(tbl) {
  stopifnot("Unavailable fraction" %in% names(tbl))
  frac <- tbl[["Unavailable fraction"]]
  bar_max <- max(frac[is.finite(frac)], na.rm = TRUE)
  if (!is.finite(bar_max) || bar_max <= 0) bar_max <- 1
  compact_metric_dt(
    tbl,
    percent_cols = "Unavailable fraction",
    round_cols = c(
      "Active repair minutes",
      "Waiting for repair minutes",
      "Total unavailable minutes"
    ),
    round_digits = 2,
    bar_col = "Unavailable fraction",
    bar_range = c(0, bar_max)
  )
}

# Operator pool table: same content as the old mean-utilization chart / labor
# table (mean utilization + mean queue), no CI. Readable labels; bar on Utilization.
operator_pool_df <- function(labor) {
  stopifnot(all(c("resource", "avg_util", "avg_queue") %in% names(labor)))
  labor %>%
    dplyr::group_by(.data$resource) %>%
    dplyr::summarise(
      Utilization = mean(.data$avg_util),
      Queue = mean(.data$avg_queue),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Utilization), .data$resource) %>%
    dplyr::transmute(
      Resource = label_resource(.data$resource),
      Utilization = .data$Utilization,
      Queue = .data$Queue
    )
}

operator_pool_dt <- function(tbl) {
  stopifnot("Utilization" %in% names(tbl))
  compact_metric_dt(
    tbl,
    percent_cols = "Utilization",
    round_cols = "Queue",
    round_digits = 2,
    bar_col = "Utilization",
    bar_range = c(0, 1)
  )
}

# Chart helper: mean bars with 95% CI (Operator utilization, Downtime fraction).
replication_bars <- function(
  df, category, value, x_label, percent = TRUE, fill_values = NULL,
  caption = "Bars = mean across replications; error bars = 95% CI. Faint points = each replication."
) {
  cat_sym <- rlang::sym(category)
  val_sym <- rlang::sym(value)
  df <- df %>% dplyr::filter(is.finite(!!val_sym))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot")))
  }
  if (is.factor(df[[category]])) {
    df[[category]] <- factor(df[[category]], levels = levels(df[[category]]))
  } else {
    df[[category]] <- factor(df[[category]], levels = unique(df[[category]]))
  }
  summary_df <- rep_mean_ci(df, category, value)
  colors <- ridge_fill_values(levels(df[[category]]), fill_values)
  ggplot() +
    geom_col(
      data = summary_df,
      aes(x = !!cat_sym, y = mean, fill = !!cat_sym),
      width = 0.72, colour = "#1a1a1a", linewidth = 0.25
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = !!cat_sym, ymin = lower, ymax = upper),
      width = 0.22, linewidth = 0.7, colour = "#1a1a1a"
    ) +
    geom_point(
      data = df,
      aes(x = !!cat_sym, y = !!val_sym, colour = !!cat_sym),
      size = 2.2, alpha = 0.45,
      position = position_jitter(width = 0.08, height = 0, seed = 42)
    ) +
    scale_fill_manual(values = colors, drop = FALSE, guide = "none") +
    scale_colour_manual(values = colors, drop = FALSE, guide = "none") +
    labeled_continuous_scale("y", percent = percent, n = 3L, expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = x_label, caption = caption) +
    theme_sim(legend.position = "none", plot.margin = margin(8, 12, 14, 10)) +
    theme_plot_width_text() +
    theme_minor_ticks() +
    theme(axis.text.x = element_text(size = 13, colour = "#1a1a1a", angle = 28, hjust = 1, vjust = 1)) +
    coord_cartesian(clip = "on")
}


# Chart: "Queues" (Results → Flow over time → Queues)
# Chart: Queues (Results → Flow over time).
queue_timeseries_plot <- function(df, station = "all", layout = "overlay") {
  if (!identical(station, "all")) {
    df <- dplyr::filter(df, resource == station)
  }
  if (!"hours" %in% names(df) && "time" %in% names(df)) {
    # Fallback if an older table lacks hours: treat time as already zero-based minutes.
    df <- dplyr::mutate(df, hours = .data$time / 60)
  }
  max_h <- max(df$hours, na.rm = TRUE)
  p <- ggplot(df, aes(x = hours, y = queue, color = label)) +
    geom_line(aes(group = rep_station), alpha = 0.12, linewidth = 0.4, na.rm = TRUE) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                linewidth = 1.15, alpha = 0.18, span = 0.35, na.rm = TRUE) +
    scale_color_manual(values = station_colors) +
    scale_x_continuous(
      breaks = observation_hour_breaks(max_h),
      expand = expansion(mult = c(0.01, 0.03))
    ) +
    labs(x = "Hours since settling ended", y = "Queue length", color = "Station",
         subtitle = plot_subtitle("Faint lines = each replication; bold = loess + ribbon")) +
    theme_sim(legend.position = "bottom")
  if (identical(layout, "facet") && identical(station, "all")) {
    p <- p + facet_wrap(~label, ncol = 2) + theme(legend.position = "none")
  }
  p
}

# Chart: "WIP" (Results → Flow over time → WIP)
# Chart: WIP (Results → Flow over time).
wip_timeseries_plot <- function(wip_timeseries) {
  df <- wip_timeseries
  if (!"hours" %in% names(df) && "time" %in% names(df)) {
    df <- dplyr::mutate(df, hours = .data$time / 60)
  }
  max_h <- max(df$hours, na.rm = TRUE)
  ggplot(df, aes(x = hours, y = wip)) +
    geom_line(aes(group = replication), alpha = 0.15, linewidth = 0.45,
              color = "#b35900", na.rm = TRUE) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                linewidth = 1.2, color = "#b35900", fill = "#fdcc8a",
                alpha = 0.25, span = 0.35, na.rm = TRUE) +
    scale_x_continuous(
      breaks = observation_hour_breaks(max_h),
      expand = expansion(mult = c(0.01, 0.03))
    ) +
    labs(x = "Hours since settling ended", y = "WIP (vehicles)",
         subtitle = plot_subtitle("Faint lines = each replication; bold = loess + ribbon")) +
    theme_sim()
}

# Chart: "Cycle time" (Results → Cycle time & data → Cycle time)
# Chart: Cycle time (Results → Cycle time & data).
cycle_time_plot <- function(cycles) {
  mean_ct <- mean(cycles$cycle_time, na.rm = TRUE)
  xlim <- density_xlim(cycles$cycle_time, level = 0.95, bounds = c(0, Inf))
  cycles %>%
    ggplot(aes(x = cycle_time)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "#41b6c4", color = "white", alpha = 0.55) +
    geom_density(aes(group = replication), alpha = 0.25, linewidth = 0.4,
                 color = "#7fcdbb", na.rm = TRUE) +
    geom_density(linewidth = 1.15, color = "#253494", na.rm = TRUE) +
    geom_vline(xintercept = mean_ct, linetype = "dashed", color = "#253494") +
    sparse_x_continuous(percent = FALSE, n = 4L) +
    coord_cartesian(xlim = xlim, clip = "off") +
    labs(
      x = "Cycle time (minutes)", y = "Density",
      subtitle = paste0(
        "Faint = per-rep density; bold = pooled; mean = ",
        round(mean_ct, 1), " min"
      ),
      caption = "x-axis shows the central 95% of values"
    ) +
    theme_sim() +
    theme_plot_width_text() +
    theme_minor_ticks()
}
