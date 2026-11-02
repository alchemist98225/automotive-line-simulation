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
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
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
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
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
  diff_plot <- diff %>% mutate(label = factor(label, levels = rev(as.character(label))))
  ggplot(diff_plot, aes(y = label)) +
    geom_segment(aes(x = baseline, xend = best, yend = label), linewidth = 0.8, color = "grey50") +
    geom_point(aes(x = baseline, color = "Baseline"), size = 3) +
    geom_point(aes(x = best, color = "Best"), size = 3) +
    scale_color_manual(values = c(Baseline = "#0072B2", Best = "#D55E00")) +
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
  tbl <- summary_tbl %>%
    transmute(
      Set = set,
      HPU = vapply(seq_len(n()), function(i) fmt_ci(hpu_mean[[i]], hpu_lo[[i]], hpu_hi[[i]]), character(1)),
      `Cost per unit` = round(cost_hpu, 3),
      Fitness = fitness,
      `Throughput/hr` = vapply(seq_len(n()), function(i) fmt_ci(thr_mean[[i]], thr_lo[[i]], thr_hi[[i]]), character(1)),
      .fitness_bar = fitness
    )
  # Bar on Fitness via hidden numeric helper — show Fitness rounded, bar from .fitness_bar
  # compact_metric_dt bars on named column; keep Fitness numeric for bar, drop helper from display
  show <- tbl %>% dplyr::select(Set, HPU, `Cost per unit`, Fitness, `Throughput/hr`)
  bar_range <- range(show$Fitness, na.rm = TRUE)
  if (diff(bar_range) < 1e-9) bar_range <- bar_range + c(-0.05, 0.05)
  compact_metric_dt(
    show,
    round_cols = c("Cost per unit", "Fitness"),
    round_digits = 3,
    bar_col = "Fitness",
    bar_range = bar_range
  )
}
