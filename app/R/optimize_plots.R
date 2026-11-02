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
  ggplot(long, aes(generation, fitness, color = series, linetype = series)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_color_manual(values = c(Best = "#0072B2", Mean = "#D55E00")) +
    scale_linetype_manual(values = c(Best = "solid", Mean = "dashed")) +
    labs(
      title = "Optimizer convergence",
      subtitle = paste(
        "Fitness = HPU + capacity and queue cost per unit + penalty for throughput below baseline.",
        "Mean uses feasible setups only."
      ),
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
  ggplot(long, aes(generation, value, color = metric)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    scale_color_manual(values = c(
      "Unique genomes" = "#0072B2",
      "Mean pairwise L1 distance" = "#D55E00")) +
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
  diff_plot <- diff %>% mutate(label = factor(label, levels = rev(label)))
  ggplot(diff_plot, aes(y = label)) +
    geom_segment(aes(x = baseline, xend = best, yend = label), linewidth = 0.8, color = "grey50") +
    geom_point(aes(x = baseline, color = "Baseline"), size = 3) +
    geom_point(aes(x = best, color = "Best"), size = 3) +
    scale_color_manual(values = c(Baseline = "#0072B2", Best = "#D55E00")) +
    labs(
      title = "Best vs baseline decision variables",
      x = "Value", y = NULL, color = NULL,
      caption = "Queue = inter-station buffer; operators are zone crews."
    ) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom", legend.direction = "horizontal")
}

optimize_summary_dt <- function(summary_tbl) {
  if (is.null(summary_tbl) || !nrow(summary_tbl)) return(NULL)
  keep <- intersect(
    c("set", "hpu_mean", "cost_hpu", "fitness", "thr_mean", "hpu_lo", "hpu_hi", "thr_lo", "thr_hi", "n_reps", "feasible"),
    names(summary_tbl)
  )
  tbl <- summary_tbl[, keep, drop = FALSE] %>%
    rename(
      Set = set,
      HPU = hpu_mean,
      `Cost / unit` = cost_hpu,
      Fitness = fitness,
      `Throughput / hr` = thr_mean,
      `HPU lo` = hpu_lo,
      `HPU hi` = hpu_hi,
      `Thr lo` = thr_lo,
      `Thr hi` = thr_hi,
      Reps = n_reps,
      Feasible = feasible
    )
  bar_col <- if ("Fitness" %in% names(tbl)) "Fitness" else NULL
  bar_range <- if (!is.null(bar_col)) range(tbl[[bar_col]], na.rm = TRUE) else c(0, 1)
  if (diff(bar_range) < 1e-9) bar_range <- bar_range + c(-0.05, 0.05)
  compact_metric_dt(
    tbl,
    round_cols = setdiff(names(tbl), c("Set", "Reps", "Feasible")),
    round_digits = 3,
    bar_col = bar_col,
    bar_range = bar_range
  )
}
