# optimize_plots.R
# Live convergence and parameter-diff charts for the Optimize page.

optimize_convergence_plot <- function(history) {
  if (is.null(history) || !nrow(history)) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Run Optimize to see convergence.", size = 5) +
        theme_void(base_size = 16)
    )
  }
  has_feas <- "mean_fitness_feasible" %in% names(history)
  long <- history %>%
    transmute(
      generation,
      Best = best_fitness,
      Mean = if (has_feas) mean_fitness_feasible else mean_fitness
    ) %>%
    tidyr::pivot_longer(-generation, names_to = "series", values_to = "fitness")
  gens <- sort(unique(long$generation))
  # Integer ticks; thin if many generations
  x_breaks <- if (length(gens) <= 12) gens else pretty(gens, n = 8)
  x_breaks <- unique(as.integer(round(x_breaks)))
  ggplot(long, aes(generation, fitness, color = series, linetype = series)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 5.5) +
    scale_color_manual(values = c(Best = "#0072B2", Mean = "#D55E00")) +
    scale_linetype_manual(values = c(Best = "solid", Mean = "dashed")) +
    scale_x_continuous(breaks = x_breaks) +
    labs(
      title = "Optimizer convergence",
      x = "Generation", y = "Fitness (lower is better)",
      color = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom", legend.direction = "horizontal")
}

optimize_diversity_plot <- function(history) {
  if (is.null(history) || !nrow(history) ||
      !all(c("n_unique", "mean_pairwise_l1") %in% names(history))) {
    return(
      ggplot() + annotate("text", x = 0, y = 0, label = "No diversity data yet.", size = 5) +
        theme_void(base_size = 16)
    )
  }
  long <- history %>%
    dplyr::select(generation, n_unique, mean_pairwise_l1) %>%
    tidyr::pivot_longer(-generation, names_to = "metric", values_to = "value") %>%
    mutate(metric = dplyr::recode(metric,
      n_unique = "Unique genomes",
      mean_pairwise_l1 = "Mean pairwise L1 distance"))
  gens <- sort(unique(long$generation))
  x_breaks <- if (length(gens) <= 12) gens else unique(as.integer(round(pretty(gens, n = 8))))
  ggplot(long, aes(generation, value, color = metric)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 5.5) +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    scale_color_manual(values = c(
      "Unique genomes" = "#0072B2",
      "Mean pairwise L1 distance" = "#D55E00")) +
    scale_x_continuous(breaks = x_breaks) +
    labs(title = "Population diversity", x = "Generation", y = NULL, color = NULL) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none", strip.text = element_text(hjust = 0))
}

optimize_param_diff_plot <- function(diff) {
  if (is.null(diff) || !nrow(diff)) {
    return(
      ggplot() + annotate("text", x = 0, y = 0, label = "No parameter diff yet.", size = 5) +
        theme_void(base_size = 16)
    )
  }
  diff_plot <- diff %>%
    mutate(
      label = factor(label, levels = rev(as.character(label))),
      # Slight vertical dodge when values match so both series stay visible.
      y_base = as.numeric(label) + ifelse(baseline == best, 0.12, 0),
      y_best = as.numeric(label) + ifelse(baseline == best, -0.12, 0)
    )
  x_vals <- c(diff_plot$baseline, diff_plot$best)
  x_min <- floor(min(x_vals, na.rm = TRUE))
  x_max <- ceiling(max(x_vals, na.rm = TRUE))
  if (!is.finite(x_min) || !is.finite(x_max)) {
    x_min <- 0L; x_max <- 1L
  }
  if (x_max <= x_min) x_max <- x_min + 1L
  ggplot(diff_plot, aes(y = label)) +
    geom_segment(aes(x = baseline, xend = best, y = y_base, yend = y_best),
                 linewidth = 1.2, color = "grey50") +
    # Open ring for Baseline (visible under Best when values match).
    geom_point(aes(x = baseline, y = y_base, color = "Baseline"),
               size = 8, shape = 21, fill = "white", stroke = 1.8) +
    geom_point(aes(x = best, y = y_best, color = "Best"), size = 6.5) +
    scale_color_manual(values = c(Baseline = "#0072B2", Best = "#D55E00")) +
    scale_x_continuous(breaks = seq(x_min, x_max), minor_breaks = NULL) +
    labs(
      title = "Best vs baseline",
      x = "Value", y = NULL, color = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.text.y = element_text(size = 14)
    )
}

# Compact results table: Set | HPU | Cost per unit | Fitness | Throughput/hr
# Optional CI shown as "mean (lo–hi)" text in HPU / Throughput; bars on Fitness.
optimize_summary_dt <- function(summary_tbl) {
  if (is.null(summary_tbl) || !nrow(summary_tbl)) return(NULL)
  fmt_ci <- function(m, lo, hi) {
    if (all(is.finite(c(m, lo, hi)))) {
      sprintf("%.3f (%.3f–%.3f)", m, lo, hi)
    } else if (is.finite(m)) {
      sprintf("%.3f", m)
    } else {
      NA_character_
    }
  }
  # Lower fitness is better: bar "% better than baseline" so the best set gets the longest bar.
  base_fit <- summary_tbl$fitness[summary_tbl$set == "baseline"][[1]]
  if (!is.finite(base_fit) || abs(base_fit) < 1e-12) base_fit <- 1
  show <- summary_tbl %>%
    transmute(
      Set = set,
      HPU = vapply(seq_len(n()), function(i) fmt_ci(hpu_mean[[i]], hpu_lo[[i]], hpu_hi[[i]]), character(1)),
      `Cost per unit` = round(cost_hpu, 3),
      Fitness = round(fitness, 3),
      `Throughput/hr` = vapply(seq_len(n()), function(i) fmt_ci(thr_mean[[i]], thr_lo[[i]], thr_hi[[i]]), character(1)),
      `Improvement %` = pmax(0, 100 * (base_fit - fitness) / abs(base_fit))
    )
  compact_metric_dt(
    show,
    round_cols = c("Cost per unit", "Fitness", "Improvement %"),
    round_digits = 3,
    bar_col = "Improvement %",
    bar_range = c(0, max(10, max(show$`Improvement %`, na.rm = TRUE)))
  )
}
