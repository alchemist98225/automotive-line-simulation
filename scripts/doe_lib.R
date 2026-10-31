# Shared helpers for the design-of-experiments (DOE) CLI.
# Sourced by scripts/doe_make_design.R, doe_run.R, and doe_plot.R.
# Portfolio demonstration; results are illustrative only, not predictions for DRI.
#
# HPU definition (hours per unit):
#   HPU = (n_operators × observation_hours) / n_completed
#   observation_hours = (sim_length_min - warmup_min) / 60
# This is the inverse of the model's throughput_per_labor_hr.
# Lower HPU means more units per labor-hour. "Peak productivity" is minimum HPU.

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
doe_repo_root <- function(script_dir = NULL) {
  if (is.null(script_dir)) {
    args_full <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args_full, value = TRUE)
    script_dir <- if (length(file_arg)) {
      dirname(normalizePath(sub("^--file=", "", file_arg)))
    } else {
      normalizePath(".")
    }
  }
  if (basename(script_dir) == "scripts") dirname(script_dir) else script_dir
}

doe_source_model <- function(repo_root = doe_repo_root()) {
  path <- file.path(repo_root, "app", "R", "model.R")
  if (!file.exists(path)) stop("Could not find app/R/model.R at ", path, call. = FALSE)
  source(path, local = FALSE)
  # simmer::select masks dplyr::select; restore tidyverse verbs for DOE scripts.
  assign("select", dplyr::select, envir = .GlobalEnv)
  assign("filter", dplyr::filter, envir = .GlobalEnv)
  assign("mutate", dplyr::mutate, envir = .GlobalEnv)
  assign("summarise", dplyr::summarise, envir = .GlobalEnv)
  assign("summarize", dplyr::summarize, envir = .GlobalEnv)
  assign("arrange", dplyr::arrange, envir = .GlobalEnv)
  assign("rename", dplyr::rename, envir = .GlobalEnv)
  assign("relocate", dplyr::relocate, envir = .GlobalEnv)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Known design columns and defaults
# ---------------------------------------------------------------------------
# Flat CSV columns that map onto default_params(). Any omitted column keeps
# the app default. Unknown columns are rejected by doe_validate_design().

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

doe_default_design_row <- function() {
  p <- default_params() %>% normalize_params()
  stations <- p$stations
  row <- list(
    design_id = "baseline",
    warmup_min = p$warmup_min,
    shift_length_min = p$shift_length_min,
    n_shifts = as.integer(p$n_shifts),
    arrival_mean_min = p$arrival_mean_min,
    release_policy = p$release_policy,
    entry_buffer = as.integer(p$entry_buffer),
    wip_limit = as.integer(p$wip_limit),
    order_spacing = p$order_spacing,
    repair_technicians = as.integer(p$repair_technicians),
    op_upstream = as.integer(p$operators$count[p$operators$pool == "upstream"]),
    op_downstream = as.integer(p$operators$count[p$operators$pool == "downstream"])
  )
  for (i in seq_len(nrow(stations))) {
    nm <- stations$name[[i]]
    row[[paste0("cap_", nm)]] <- as.integer(stations$capacity[[i]])
    row[[paste0("mean_", nm)]] <- as.numeric(stations$mean_min[[i]])
  }
  row$buffer_paint <- as.integer(stations$buffer_before[stations$name == "paint"])
  row$buffer_final_assembly <- as.integer(stations$buffer_before[stations$name == "final_assembly"])
  row$buffer_inspection <- as.integer(stations$buffer_before[stations$name == "inspection"])
  as_tibble(row)
}

doe_validate_design <- function(design) {
  stopifnot(is.data.frame(design), nrow(design) > 0)
  if (!"design_id" %in% names(design)) {
    stop("Design CSV must include a design_id column.", call. = FALSE)
  }
  if (anyDuplicated(design$design_id)) {
    dups <- unique(design$design_id[duplicated(design$design_id)])
    stop("design_id values must be unique. Duplicates: ",
         paste(dups, collapse = ", "), call. = FALSE)
  }
  if (any(is.na(design$design_id) | !nzchar(as.character(design$design_id)))) {
    stop("design_id must be non-empty for every row.", call. = FALSE)
  }
  known <- doe_known_columns()
  unknown <- setdiff(names(design), known)
  if (length(unknown)) {
    stop(
      "Unknown design column(s): ", paste(unknown, collapse = ", "),
      "\nKnown columns:\n  ", paste(known, collapse = ", "),
      call. = FALSE
    )
  }
  design %>% mutate(design_id = as.character(design_id))
}

# Overlay one design row onto a copy of default_params().
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

# Reproducible seed for design row index × replication.
# seed = base_seed + (row_index - 1) * n_reps + (rep - 1)
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

doe_chunk_path <- function(out_dir, design_id) {
  # Safe filename from design_id
  safe <- gsub("[^A-Za-z0-9._-]+", "_", design_id)
  file.path(out_dir, "chunks", paste0(safe, ".csv.gz"))
}

doe_write_chunk <- function(metrics, out_dir, design_id) {
  dir.create(file.path(out_dir, "chunks"), recursive = TRUE, showWarnings = FALSE)
  path <- doe_chunk_path(out_dir, design_id)
  tmp <- paste0(path, ".tmp.gz")
  # Atomic replace: write to temp then rename into place.
  readr::write_csv(metrics, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
  invisible(path)
}

doe_read_chunk <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

doe_format_duration <- function(secs) {
  secs <- max(0, as.numeric(secs))
  if (secs < 60) return(sprintf("%.0fs", secs))
  if (secs < 3600) return(sprintf("%.1fmin", secs / 60))
  sprintf("%.2fh", secs / 3600)
}
