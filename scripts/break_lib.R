# break_lib.R
# Shared helpers for the demand break-sweep CLI (scripts/break_sweep.R).
# Reuses DOE model loading + metric extraction; parallelizes with furrr/future.
# Portfolio demonstration; results are illustrative only, not plant predictions.
#
# Break-point rule (stated on the chart caption too):
#   Ascending offered demand, the first grid point where mean throughput/hr
#   falls below 95% of offered demand. That is where the line no longer keeps
#   up with arrivals.

suppressPackageStartupMessages({
  library(tidyverse)
})

# Prefer the caller's script_dir (CLI sets it); fall back to this file's folder.
.break_lib_dir <- local({
  if (exists("script_dir", inherits = TRUE)) {
    sd <- get("script_dir", inherits = TRUE)
    if (is.character(sd) && length(sd) == 1L && dir.exists(sd)) return(sd)
  }
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  normalizePath("scripts")
})

source(file.path(.break_lib_dir, "doe_lib.R"), local = FALSE)

# ---------------------------------------------------------------------------
# Line setups compared on the sweep
# ---------------------------------------------------------------------------
break_setup_defs <- function() {
  tibble::tribble(
    ~setup_id,   ~setup_label,      ~cap_paint,
    "default",   "Default line",    1L,
    "guided_2",  "2-bay balanced",  2L
  )
}

# One design row overlay: default_params() with paint capacity + arrival mean.
break_design_row <- function(setup_id, arrival_mean_min, setup_defs = break_setup_defs()) {
  stopifnot(setup_id %in% setup_defs$setup_id)
  cap <- setup_defs$cap_paint[setup_defs$setup_id == setup_id][[1]]
  tibble::tibble(
    design_id = paste(setup_id, sprintf("iat%.3f", arrival_mean_min), sep = "__"),
    setup_id = as.character(setup_id),
    arrival_mean_min = as.numeric(arrival_mean_min),
    cap_paint = as.integer(cap)
  )
}

# Offered demand (veh/hr) ↔ mean interarrival (min). Fixed spacing by default.
break_demand_to_iat <- function(demand_per_hr) {
  60 / as.numeric(demand_per_hr)
}

break_iat_to_demand <- function(arrival_mean_min) {
  60 / as.numeric(arrival_mean_min)
}

# Fine grid from below capacity to well past both setups' paint limits.
# Default paint ≈ 6–7/hr; 2-bay ≈ 12–14/hr. Sweep 3–18 veh/hr.
break_default_demand_grid <- function(demand_min = 3, demand_max = 18, step = 0.5) {
  seq(demand_min, demand_max, by = step)
}

# Crossing of setups × demand → design tibble with offered_demand_per_hr.
break_make_grid <- function(demand_per_hr = break_default_demand_grid(),
                            setups = break_setup_defs()) {
  tidyr::crossing(
    setups %>% dplyr::select(setup_id, setup_label, cap_paint),
    offered_demand_per_hr = as.numeric(demand_per_hr)
  ) %>%
    mutate(
      arrival_mean_min = break_demand_to_iat(offered_demand_per_hr),
      design_id = paste(setup_id, sprintf("d%.3f", offered_demand_per_hr), sep = "__"),
      point_index = dplyr::row_number()
    ) %>%
    dplyr::relocate(design_id, point_index, setup_id, setup_label)
}

# ---------------------------------------------------------------------------
# Parallel run (furrr multisession)
# ---------------------------------------------------------------------------
# One design point: n_reps via run_line_sim. Seed is CRN across setups at the
# same demand: seed depends on demand_index × rep, not on setup_id.
# future::run masks simmer::run once {future} is attached. The engine calls
# run(env, until=...) unqualified, so restore simmer::run in every worker.
break_ensure_simmer_run <- function() {
  assign("run", simmer::run, envir = .GlobalEnv)
  invisible(TRUE)
}

break_run_point <- function(row, n_reps, base_seed) {
  break_ensure_simmer_run()
  row <- as_tibble(row)[1, , drop = FALSE]
  demand_index <- as.integer(row$demand_index[[1]])
  # Shared CRN base for this demand across every setup.
  point_seed <- doe_rep_seed(base_seed, demand_index, 1L, n_reps)
  design_row <- tibble::tibble(
    design_id = as.character(row$design_id[[1]]),
    arrival_mean_min = as.numeric(row$arrival_mean_min[[1]]),
    cap_paint = as.integer(row$cap_paint[[1]])
  )
  params <- doe_params_from_row(design_row, n_replications = n_reps, base_seed = point_seed)
  result <- run_line_sim(params)
  metrics <- doe_extract_rep_metrics(result, design_row, result$seeds) %>%
    mutate(
      setup_id = as.character(row$setup_id[[1]]),
      setup_label = as.character(row$setup_label[[1]]),
      offered_demand_per_hr = as.numeric(row$offered_demand_per_hr[[1]]),
      demand_index = demand_index,
      arrival_mean_min = as.numeric(row$arrival_mean_min[[1]]),
      cap_paint = as.integer(row$cap_paint[[1]]),
      thr_gap_pct = 100 * (offered_demand_per_hr - throughput_per_hr) / offered_demand_per_hr
    )
  metrics
}

# Assign a demand_index shared by all setups at the same offered demand (CRN).
break_attach_demand_index <- function(grid) {
  demand_levels <- sort(unique(grid$offered_demand_per_hr))
  grid %>%
    mutate(demand_index = match(offered_demand_per_hr, demand_levels))
}

break_run_sweep <- function(grid,
                            n_reps = 10L,
                            base_seed = 42L,
                            workers = max(1L, parallel::detectCores() - 1L),
                            progress = TRUE) {
  stopifnot(n_reps >= 1L, workers >= 1L)
  grid <- break_attach_demand_index(grid)
  n_points <- nrow(grid)

  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("furrr", quietly = TRUE)) {
    stop("Packages future and furrr are required for break_run_sweep().", call. = FALSE)
  }

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  if (workers == 1L) {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession, workers = workers)
  }
  # Parent session also sees future::run after plan(); restore simmer.
  break_ensure_simmer_run()

  # Explicit per-point seeds (CRN); disable furrr's own L'Ecuyer stream.
  # packages= ensures workers have simmer/tidyverse; globals carry helpers.
  opts <- furrr::furrr_options(
    seed = NULL,
    packages = c("simmer", "tidyverse", "dplyr", "tidyr", "purrr", "tibble")
  )

  rows <- split(grid, seq_len(nrow(grid)))
  t0 <- proc.time()[["elapsed"]]
  chunks <- furrr::future_map(
    rows,
    function(row) break_run_point(row, n_reps = n_reps, base_seed = base_seed),
    .options = opts,
    .progress = progress
  )
  elapsed <- proc.time()[["elapsed"]] - t0

  rep_metrics <- dplyr::bind_rows(chunks)
  list(
    rep_metrics = rep_metrics,
    elapsed_sec = elapsed,
    workers = workers,
    n_reps = as.integer(n_reps),
    base_seed = as.integer(base_seed),
    n_points = n_points
  )
}

# ---------------------------------------------------------------------------
# Summaries + break-point detection
# ---------------------------------------------------------------------------
break_summarise <- function(rep_metrics) {
  rep_metrics %>%
    mutate(
      hpu = if_else(is.infinite(hpu), NA_real_, hpu),
      cycle_time_mean = if_else(is.finite(cycle_time_mean), cycle_time_mean, NA_real_)
    ) %>%
    group_by(design_id, setup_id, setup_label, offered_demand_per_hr,
             arrival_mean_min, cap_paint, demand_index) %>%
    summarise(
      n_reps = n(),
      throughput_mean = mean(throughput_per_hr, na.rm = TRUE),
      throughput_sd = sd(throughput_per_hr, na.rm = TRUE),
      throughput_lo = {
        x <- throughput_per_hr[!is.na(throughput_per_hr)]
        if (length(x) < 2) NA_real_ else mean(x) - qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
      },
      throughput_hi = {
        x <- throughput_per_hr[!is.na(throughput_per_hr)]
        if (length(x) < 2) NA_real_ else mean(x) + qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
      },
      throughput_p10 = as.numeric(stats::quantile(throughput_per_hr, 0.10, na.rm = TRUE, names = FALSE)),
      throughput_p90 = as.numeric(stats::quantile(throughput_per_hr, 0.90, na.rm = TRUE, names = FALSE)),
      lead_time_mean = mean(cycle_time_mean, na.rm = TRUE),
      lead_time_sd = sd(cycle_time_mean, na.rm = TRUE),
      lead_time_p10 = as.numeric(stats::quantile(cycle_time_mean, 0.10, na.rm = TRUE, names = FALSE)),
      lead_time_p90 = as.numeric(stats::quantile(cycle_time_mean, 0.90, na.rm = TRUE, names = FALSE)),
      wip_mean = mean(avg_wip, na.rm = TRUE),
      wip_p10 = as.numeric(stats::quantile(avg_wip, 0.10, na.rm = TRUE, names = FALSE)),
      wip_p90 = as.numeric(stats::quantile(avg_wip, 0.90, na.rm = TRUE, names = FALSE)),
      hpu_mean = mean(hpu, na.rm = TRUE),
      fill_rate_mean = mean(throughput_per_hr / offered_demand_per_hr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(setup_id, offered_demand_per_hr)
}

# First ascending demand where fill rate (thr / demand) drops below threshold.
break_find_breakpoints <- function(summary_tbl, fill_threshold = 0.95) {
  summary_tbl %>%
    arrange(setup_id, offered_demand_per_hr) %>%
    group_by(setup_id, setup_label, cap_paint) %>%
    group_modify(function(d, ...) {
      hit <- which(d$fill_rate_mean < fill_threshold)
      if (!length(hit)) {
        return(tibble::tibble(
          break_demand_per_hr = NA_real_,
          break_throughput_mean = NA_real_,
          break_fill_rate = NA_real_,
          break_lead_time_mean = NA_real_,
          fill_threshold = fill_threshold,
          found = FALSE
        ))
      }
      i <- hit[[1]]
      tibble::tibble(
        break_demand_per_hr = d$offered_demand_per_hr[[i]],
        break_throughput_mean = d$throughput_mean[[i]],
        break_fill_rate = d$fill_rate_mean[[i]],
        break_lead_time_mean = d$lead_time_mean[[i]],
        fill_threshold = fill_threshold,
        found = TRUE
      )
    }) %>%
    ungroup()
}
