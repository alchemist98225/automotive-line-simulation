# analysis_plots.R
# ggplot builders for the Analysis page. Ported from scripts/*_plot.R so the
# app/ deploy stays self-contained. House style: base size 16 (phone tornado /
# break charts may drop to ~12–13), legend at the bottom in one row, dashed
# references, large points, no rugs.

# Lazy-load plot extras used only on the Analysis page.
ensure_analysis_plot_pkgs <- function() {
  requireNamespace("ggtext", quietly = TRUE)
  requireNamespace("patchwork", quietly = TRUE)
  invisible(TRUE)
}

# --- from scripts/break_plot.R ---

break_theme <- function(base_size = 16, ...) {
  # Desktop default 16; Analysis phone (<576px plot width) may pass ~12–13.
  sub_size <- max(11, round(base_size * 14 / 16))
  cap_size <- max(10, round(base_size * 12 / 16))
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(size = base_size, colour = "#1a1a1a"),
      plot.title = element_text(face = "bold", size = base_size, colour = "#1a1a1a"),
      plot.subtitle = element_text(size = sub_size, colour = "#1a1a1a"),
      plot.caption = element_text(size = cap_size, colour = "#1a1a1a", hjust = 0,
                                 margin = margin(8, 0, 0, 0)),
      plot.caption.position = "plot",
      axis.title = element_text(size = base_size, colour = "#1a1a1a"),
      axis.text = element_text(size = base_size, colour = "#1a1a1a"),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size, colour = "#1a1a1a"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text = element_text(size = base_size, colour = "#1a1a1a"),
      panel.grid.minor = element_blank(),
      ...
    )
}

break_setup_colors <- function() {
  c("Default line" = "#0072B2", "2-bay balanced" = "#D55E00")
}

# Build the two-panel headline figure; returns a patchwork object.
break_plot_sweep <- function(summary_tbl, breakpoints, fill_threshold = 0.95, base_size = 16) {
  ensure_analysis_plot_pkgs()

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
    break_theme(base_size = base_size)

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
      y = "Mean cycle time (min)"
    ) +
    break_theme(base_size = base_size)

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
  wrap_w <- if (base_size < 15) 55 else 95
  caption <- stringr::str_wrap(caption_raw, width = wrap_w)
  cap_size <- max(10, round(base_size * 12 / 16))

  (thr_panel / lt_panel) +
    plot_layout(heights = c(1.15, 1), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.caption = ggtext::element_textbox_simple(
        size = cap_size,
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

# --- from scripts/tornado_plot.R ---

tor_theme <- function(base_size = 16, ...) {
  cap_size <- max(10, round(base_size * 12 / 16))
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size),
      plot.caption = element_text(size = cap_size, hjust = 0),
      plot.caption.position = "plot",
      legend.position = "bottom", legend.direction = "horizontal",
      panel.grid.minor = element_blank(), ...
    )
}

plot_tornado <- function(effects, metric = c("hpu", "thr"), base_size = 16) {
  metric <- match.arg(metric)
  # Compact single-line y labels so phone widths (~390px) keep a usable plot.
  short_factor <- function(x) {
    x <- as.character(x)
    x <- sub("^Paint capacity$", "Paint cap.", x)
    x <- sub("^Upstream operators$", "Upstr. ops", x)
    x <- sub("^Downstream operators$", "Downstr. ops", x)
    x <- sub("^Assembly cycle time$", "Asm. cycle", x)
    x <- sub("^Body shop cycle time$", "Body cycle", x)
    x <- sub("^Inspection cycle time$", "Insp. cycle", x)
    x <- sub("^Queue before paint$", "Paint queue", x)
    x <- sub("^Queue before assembly$", "Asm. queue", x)
    x <- sub("^Demand rate$", "Demand", x)
    x <- sub("^MTBF scale$", "MTBF", x)
    x <- sub("^MTTR scale$", "MTTR", x)
    x
  }
  if (metric == "hpu") {
    d <- effects %>%
      mutate(
        label = forcats::fct_reorder(short_factor(label), hpu_abs_span),
        low = hpu_low, high = hpu_high, base = hpu_base
      )
    title <- "Tornado: HPU"
    xlab <- "HPU (labor-hr / unit)"
  } else {
    d <- effects %>%
      mutate(
        label = forcats::fct_reorder(short_factor(label), thr_abs_span),
        low = thr_low, high = thr_high, base = thr_base
      )
    title <- "Tornado: throughput"
    xlab <- "Throughput (veh/hr)"
  }
  y_size <- max(10, round(base_size * 13 / 16))
  ggplot(d, aes(y = label)) +
    geom_vline(aes(xintercept = base), linetype = "dashed", color = "grey40", linewidth = 0.9) +
    geom_segment(aes(x = low, xend = high, yend = label), linewidth = 2.2, color = "#4C78A8") +
    geom_point(aes(x = low, color = "Low"), size = 3.5) +
    geom_point(aes(x = high, color = "High"), size = 3.5) +
    scale_color_manual(values = c(Low = "#0072B2", High = "#D55E00")) +
    labs(title = title, x = xlab, y = NULL, color = NULL,
         caption = "2-bay balanced baseline at 10 veh/hr demand. OAT low/high; dashed = baseline. CRN. Illustrative.") +
    tor_theme(
      base_size = base_size,
      axis.text.y = element_text(size = y_size, lineheight = 0.92),
      plot.margin = margin(6, 10, 8, 4)
    )
}


plot_tornado_pair <- function(effects, base_size = 16) {
  ensure_analysis_plot_pkgs()

  plot_tornado(effects, "hpu", base_size = base_size) /
    plot_tornado(effects, "thr", base_size = base_size) +
    plot_layout(heights = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom", legend.direction = "horizontal")
}

# --- from scripts/validate_plot.R ---

val_theme <- function(...) {
  theme_minimal(base_size = 16) +
    theme(
      text = element_text(size = 16, colour = "#1a1a1a"),
      plot.title = element_text(face = "bold", size = 16),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = ggtext::element_textbox_simple(
        size = 12, colour = "#1a1a1a", hjust = 0, halign = 0,
        lineheight = 1.25, padding = margin(2, 2, 2, 2),
        margin = margin(8, 0, 2, 0)
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 14, 10, 14),
      ...
    )
}

plot_mmc_check <- function(summary_tbl) {
  ensure_analysis_plot_pkgs()

  d <- summary_tbl %>%
    filter(metric %in% c("Wq_mean", "Lq", "util")) %>%
    mutate(
      metric_lab = recode(metric,
        Wq_mean = "Wait in queue Wq",
        Lq = "Queue length Lq",
        util = "Utilization"
      )
    )
  caption <- stringr::str_wrap(
    "Simmer mean ± 95% CI (large points); dashed + × = Erlang-C. Standalone M/M/c, not the automotive line. Illustrative.",
    width = 95
  )
  ggplot(d, aes(rho, sim_mean, color = metric_lab)) +
    geom_ribbon(aes(ymin = sim_lo, ymax = sim_hi, fill = metric_lab),
                alpha = 0.2, color = NA) +
    geom_line(aes(y = analytic_value), linetype = "dashed", linewidth = 1.0,
              color = "grey30", show.legend = FALSE) +
    geom_point(aes(y = analytic_value), shape = 4, size = 5.5, stroke = 1.6,
               color = "grey15", show.legend = FALSE) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 5.5) +
    facet_wrap(~metric_lab, scales = "free_y", ncol = 1) +
    labs(
      title = "M/M/c check vs Erlang-C",
      subtitle = "Large points/ribbons = simmer; dashed + × = analytic",
      x = "Utilization ρ = λ / (c μ)",
      y = NULL,
      caption = caption
    ) +
    val_theme() +
    theme(strip.text = element_text(hjust = 0), legend.position = "none")
}

plot_littles <- function(rep_tbl, summary_tbl) {
  ensure_analysis_plot_pkgs()

  caption <- stringr::str_wrap(
    "Faint points = replications; large points = scenario means. Dashed: y = x. Post-warmup. Illustrative parameters.",
    width = 95
  )
  ggplot(rep_tbl, aes(L_pred, L_obs, color = setup)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                linewidth = 0.9, color = "grey40") +
    geom_point(alpha = 0.35, size = 2.8) +
    geom_point(data = summary_tbl, aes(L_pred_mean, L_obs_mean), size = 4.5) +
    scale_color_manual(values = c(default = "#0072B2", guided_2 = "#D55E00"),
                       labels = c(default = "Default line", guided_2 = "2-bay balanced")) +
    labs(
      title = "Little's Law check on the full line",
      subtitle = "Observed WIP vs λ × W (throughput/hr ÷ 60 × mean cycle time)",
      x = "Predicted L = λ W",
      y = "Observed average WIP",
      caption = caption
    ) +
    val_theme()
}

# --- from scripts/warmup_plot.R ---

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
      # Extra left margin so "WIP (vehicles)" clears the tick labels.
      axis.title.y = element_text(margin = margin(r = 14)),
      plot.margin = margin(8, 16, 10, 40),
      ...
    )
}

plot_welch_warmup <- function(smooth_tbl, recommend_tbl, current_warmup = 300) {
  ensure_analysis_plot_pkgs()

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

