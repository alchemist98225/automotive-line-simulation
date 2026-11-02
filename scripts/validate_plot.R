# validate_plot.R — M/M/c and Little's Law charts (base size 16).

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(patchwork)
  library(ggtext)
})

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
      subtitle = "Observed WIP vs λ × W (throughput/hr ÷ 60 × mean lead time)",
      x = "Predicted L = λ W",
      y = "Observed average WIP",
      caption = caption
    ) +
    val_theme()
}
