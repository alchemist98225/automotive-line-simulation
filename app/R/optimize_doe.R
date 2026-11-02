# optimize_doe.R
# Minimal DOE helpers the optimizer needs inside the Shiny app (no scripts/ on shinyapps).
# CLI still uses scripts/doe_*.R; function names match so either path works.

doe_station_names <- function() {
  c("body_shop", "paint", "final_assembly", "inspection")
}

doe_known_columns <- function() {
  stations <- doe_station_names()
  c(
    "design_id",
    # Run horizon / admission
    "warmup_min", "shift_length_min", "n_shifts", "arrival_mean_min",
    "release_policy", "entry_buffer", "wip_limit", "order_spacing",
    "repair_technicians",
    # Operator counts (zones by default; op_shared switches to one shared team)
    "op_upstream", "op_downstream", "op_shared",
    paste0("op_", stations),
    # Bay capacities and automatic means
    paste0("cap_", stations),
    paste0("mean_", stations),
    # Interstation buffers (no buffer before the first station)
    "buffer_paint", "buffer_final_assembly", "buffer_inspection"
  )
}

doe_params_from_row <- function(row, n_replications = 1L, base_seed = 1L) {
  p <- default_params()
  getv <- function(name, default = NULL) {
    if (!name %in% names(row) || is.null(row[[name]]) || (length(row[[name]]) == 1 && is.na(row[[name]]))) {
      return(default)
    }
    row[[name]]
  }

  if (!is.null(getv("warmup_min"))) p$warmup_min <- as.numeric(getv("warmup_min"))
  if (!is.null(getv("shift_length_min"))) p$shift_length_min <- as.numeric(getv("shift_length_min"))
  if (!is.null(getv("n_shifts"))) p$n_shifts <- as.integer(getv("n_shifts"))
  if (!is.null(getv("arrival_mean_min"))) p$arrival_mean_min <- as.numeric(getv("arrival_mean_min"))
  if (!is.null(getv("release_policy"))) p$release_policy <- as.character(getv("release_policy"))
  if (!is.null(getv("entry_buffer"))) p$entry_buffer <- as.integer(getv("entry_buffer"))
  if (!is.null(getv("wip_limit"))) p$wip_limit <- as.integer(getv("wip_limit"))
  if (!is.null(getv("order_spacing"))) p$order_spacing <- as.character(getv("order_spacing"))
  if (!is.null(getv("repair_technicians"))) p$repair_technicians <- as.integer(getv("repair_technicians"))

  # Operators: dedicated per station > shared team > zones
  dedicated_cols <- paste0("op_", doe_station_names())
  has_dedicated <- any(vapply(dedicated_cols, function(n) !is.null(getv(n)), logical(1)))
  if (has_dedicated) {
    pools <- character(); counts <- integer()
    for (st in doe_station_names()) {
      col <- paste0("op_", st)
      val <- getv(col)
      if (!is.null(val) && as.integer(val) > 0) {
        pools <- c(pools, st)
        counts <- c(counts, as.integer(val))
        p$stations$operator_pool[p$stations$name == st] <- st
      }
    }
    p$operators <- tibble(pool = pools, count = counts)
  } else if (!is.null(getv("op_shared"))) {
    n <- as.integer(getv("op_shared"))
    p$operators <- tibble(pool = "shared", count = n)
    p$stations$operator_pool <- "shared"
  } else {
    up <- getv("op_upstream", p$operators$count[p$operators$pool == "upstream"])
    down <- getv("op_downstream", p$operators$count[p$operators$pool == "downstream"])
    p$operators <- tibble(pool = c("upstream", "downstream"),
                          count = c(as.integer(up), as.integer(down)))
    p$stations$operator_pool <- ifelse(
      p$stations$name %in% c("body_shop", "paint"), "upstream", "downstream"
    )
  }

  for (st in doe_station_names()) {
    cap <- getv(paste0("cap_", st))
    if (!is.null(cap)) p$stations$capacity[p$stations$name == st] <- as.integer(cap)
    mu <- getv(paste0("mean_", st))
    if (!is.null(mu)) p$stations$mean_min[p$stations$name == st] <- as.numeric(mu)
  }
  for (st in c("paint", "final_assembly", "inspection")) {
    buf <- getv(paste0("buffer_", st))
    if (!is.null(buf)) p$stations$buffer_before[p$stations$name == st] <- as.numeric(buf)
  }

  p$n_replications <- as.integer(n_replications)
  p$base_seed <- as.integer(base_seed)
  p$time_mode <- "shifts"
  # Force timing list-columns to rebuild from (possibly updated) scalars
  p$stations$load_timing <- NULL
  p$stations$auto_timing <- NULL
  p$stations$unload_timing <- NULL
  p$stations$task_timing <- NULL
  p
}


doe_rep_seed <- function(base_seed, row_index, rep, n_reps) {
  as.integer(base_seed + (as.integer(row_index) - 1L) * as.integer(n_reps) +
               (as.integer(rep) - 1L))
}

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

doe_format_duration <- function(secs) {
  secs <- max(0, as.numeric(secs))
  if (secs < 60) return(sprintf("%.0fs", secs))
  if (secs < 3600) return(sprintf("%.1fmin", secs / 60))
  sprintf("%.2fh", secs / 3600)
}

