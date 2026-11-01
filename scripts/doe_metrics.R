# doe_metrics.R
# Per-replication metric extraction and design-level summaries for the DOE CLI.
# HPU = (n_operators × observation_hours) / n_completed.
# Sourced by doe_lib.R.

doe_rep_seed <- function(base_seed, row_index, rep, n_reps) {
  as.integer(base_seed + (as.integer(row_index) - 1L) * as.integer(n_reps) +
               (as.integer(rep) - 1L))
}

# Compact one-row-per-rep metrics, with design parameters joined in.
doe_extract_rep_metrics <- function(result, design_row, seeds) {
  p <- result$params
  obs_hours <- (p$sim_length_min - p$warmup_min) / 60
  n_ops <- sum(p$operators$count)

  summaries <- result$summaries %>%
    mutate(
      n_operators = n_ops,
      observation_hours = obs_hours,
      # HPU: labor hours on the clock per completed unit (Inf when none finish)
      hpu = if_else(n_completed > 0, n_ops * obs_hours / n_completed, Inf),
      units_per_labor_hr = throughput_per_labor_hr
    )

  # Mean completed units per measured shift (across shifts in this rep)
  shift_out <- result$shift_summaries %>%
    group_by(replication) %>%
    summarise(output_per_shift = mean(n_completed), .groups = "drop")

  util_wide <- result$utilization_by_rep %>%
    transmute(replication, station = paste0("util_", station), avg_util) %>%
    tidyr::pivot_wider(names_from = station, values_from = avg_util)

  op_wide <- result$operator_by_rep %>%
    transmute(
      replication,
      pool = paste0("util_op_", sub("^operator_", "", resource)),
      avg_util
    ) %>%
    tidyr::pivot_wider(names_from = pool, values_from = avg_util) %>%
    mutate(util_op_mean = rowMeans(across(starts_with("util_op_")), na.rm = TRUE))

  design_cols <- design_row %>%
    dplyr::select(dplyr::any_of(doe_known_columns())) %>%
    dplyr::select(-dplyr::any_of("design_id"))

  summaries %>%
    left_join(shift_out, by = "replication") %>%
    left_join(util_wide, by = "replication") %>%
    left_join(op_wide, by = "replication") %>%
    mutate(design_id = as.character(design_row$design_id[[1]]), .before = 1) %>%
    bind_cols(design_cols[rep(1, nrow(summaries)), , drop = FALSE]) %>%
    dplyr::relocate(design_id, replication, seed)
}

doe_summarise_reps <- function(rep_metrics) {
  metric_cols <- c(
    "n_completed", "throughput_per_hr", "output_per_shift",
    "cycle_time_mean", "cycle_time_p50", "cycle_time_p95",
    "avg_wip", "hpu", "units_per_labor_hr", "util_op_mean",
    grep("^util_", names(rep_metrics), value = TRUE)
  )
  metric_cols <- unique(metric_cols[metric_cols %in% names(rep_metrics)])
  design_cols <- intersect(doe_known_columns(), names(rep_metrics))
  design_cols <- setdiff(design_cols, "design_id")

  # Keep finite HPU for CI math; Inf rows (zero completions) become NA
  data <- rep_metrics %>%
    mutate(across(any_of("hpu"), ~ if_else(is.infinite(.x), NA_real_, .x)))

  stats <- data %>%
    group_by(design_id) %>%
    summarise(
      n_reps = n(),
      across(
        all_of(metric_cols),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          lo = ~ {
            x <- .x[!is.na(.x)]
            if (length(x) < 2) return(NA_real_)
            mean(x) - qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
          },
          hi = ~ {
            x <- .x[!is.na(.x)]
            if (length(x) < 2) return(NA_real_)
            mean(x) + qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
          }
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  design_unique <- rep_metrics %>%
    distinct(design_id, across(any_of(design_cols)))

  left_join(design_unique, stats, by = "design_id")
}
