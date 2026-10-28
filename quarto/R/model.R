# =============================================================================
# model.R — reusable automotive line discrete-event simulation (simmer)
#
# All numeric defaults are PLACEHOLDER EXAMPLE VALUES for a portfolio demo.
# Swap in real plant data before using for decisions.
#
# Public API:
#   default_params()              -> named list of inputs
#   run_line_sim(params)          -> tidy list of results (all replications)
#   write_sim_exports(res, dir)   -> write plot/table CSVs (+ optional zip)
#   params_to_tibble(params)      -> flat run_parameters tibble
# =============================================================================

suppressPackageStartupMessages({
  library(simmer)
  library(tidyverse)
})

# Qualify simmer time helper (lubridate::now is on the search path via tidyverse).
sim_now <- function(env) simmer::now(env)

# -----------------------------------------------------------------------------
# Default parameters (PLACEHOLDER EXAMPLE VALUES)
# -----------------------------------------------------------------------------

default_params <- function() {
  list(
    sim_length_min   = 480,        # one 8-hour shift
    n_replications   = 5L,         # keep modest for responsive Shiny demos
    base_seed        = 20261027L,
    warmup_min       = 60,
    arrival_mean_min = 4,          # ~15 vehicles/hour
    # Grid step (minutes) used when resampling step-function resource monitors
    # onto a common time axis for multi-rep queue / WIP plots.
    grid_dt_min      = 2,
    stations = tribble(
      ~name,              ~capacity, ~dist,         ~mean_min, ~sd_min,
      "body_shop",        2L,        "exponential", 6.0,       NA_real_,
      "paint",            1L,        "normal",      8.0,       1.5,
      "final_assembly",   2L,        "normal",      7.0,       1.0,
      "inspection",       1L,        "exponential", 3.0,       NA_real_
    )
  )
}

# Flatten params into a tidy table for CSV export / reproducibility.
params_to_tibble <- function(params) {
  st <- params$stations %>%
    mutate(
      param_group = "station",
      .before = 1
    ) %>%
    rename(station = name)

  run_row <- tibble(
    param_group      = "run",
    station          = NA_character_,
    capacity         = NA_integer_,
    dist             = NA_character_,
    mean_min         = NA_real_,
    sd_min           = NA_real_,
    sim_length_min   = params$sim_length_min,
    n_replications   = as.integer(params$n_replications),
    base_seed        = as.integer(params$base_seed),
    warmup_min       = params$warmup_min,
    arrival_mean_min = params$arrival_mean_min,
    grid_dt_min      = params$grid_dt_min %||% 2,
    note             = "PLACEHOLDER EXAMPLE PARAMETERS — not real plant data"
  )

  st2 <- st %>%
    mutate(
      sim_length_min   = params$sim_length_min,
      n_replications   = as.integer(params$n_replications),
      base_seed        = as.integer(params$base_seed),
      warmup_min       = params$warmup_min,
      arrival_mean_min = params$arrival_mean_min,
      grid_dt_min      = params$grid_dt_min %||% 2,
      note             = "PLACEHOLDER EXAMPLE PARAMETERS — not real plant data"
    )

  bind_rows(run_row, st2)
}

# Build a sealed service-time function for one station (closure over params).
make_service_fn <- function(dist, mean_min, sd_min) {
  force(dist); force(mean_min); force(sd_min)
  if (identical(dist, "exponential")) {
    function() rexp(1L, rate = 1 / mean_min)
  } else if (identical(dist, "normal")) {
    function() max(0.1, rnorm(1L, mean = mean_min, sd = sd_min))
  } else {
    stop("Unknown dist: ", dist)
  }
}

# Carry-forward (step) interpolate a resource monitor onto a regular grid.
# rule = 2 extends the first/last observed value to the grid edges.
resample_step <- function(time, value, grid) {
  if (length(time) == 0L || all(is.na(time))) {
    return(rep(NA_real_, length(grid)))
  }
  ord <- order(time)
  time  <- time[ord]
  value <- value[ord]
  # Drop duplicate times (keep last) so approx() is happy.
  keep <- !duplicated(time, fromLast = TRUE)
  time  <- time[keep]
  value <- value[keep]
  if (length(time) == 1L) {
    return(rep(as.numeric(value), length(grid)))
  }
  as.numeric(approx(
    x = time, y = value, xout = grid,
    method = "constant", f = 0, rule = 2
  )$y)
}

# Run one independent replication with a fixed seed (parallel-safe: no shared RNG).
run_one_replication <- function(rep_id, seed, params) {
  set.seed(seed)

  stations <- params$stations
  sim_length <- params$sim_length_min
  warmup <- params$warmup_min
  arrival_mean <- params$arrival_mean_min

  env <- simmer(paste0("auto_line_rep", rep_id))

  for (i in seq_len(nrow(stations))) {
    env <- env %>%
      add_resource(stations$name[[i]], capacity = stations$capacity[[i]])
  }

  traj <- trajectory("vehicle") %>%
    set_attribute("start_time", function() sim_now(env))

  for (i in seq_len(nrow(stations))) {
    st <- stations[i, ]
    svc <- make_service_fn(st$dist, st$mean_min, st$sd_min)
    traj <- traj %>%
      seize(st$name, 1) %>%
      timeout(svc) %>%
      release(st$name, 1)
  }

  traj <- traj %>%
    set_attribute("end_time", function() sim_now(env)) %>%
    set_attribute("cycle_time", function() {
      get_attribute(env, "end_time") - get_attribute(env, "start_time")
    })

  env <- env %>%
    add_generator(
      name_prefix  = "veh",
      trajectory   = traj,
      distribution = function() rexp(1L, rate = 1 / arrival_mean),
      mon = 2
    ) %>%
    run(until = sim_length)

  # --- Arrivals / completions (post-warmup) ---
  arr <- get_mon_arrivals(env, per_resource = FALSE) %>%
    as_tibble() %>%
    filter(start_time >= warmup, finished)

  n_completed <- nrow(arr)
  horizon     <- sim_length - warmup
  throughput  <- if (horizon > 0) n_completed / (horizon / 60) else NA_real_

  cycle_times_vec <- arr$end_time - arr$start_time
  ct_mean <- if (length(cycle_times_vec)) mean(cycle_times_vec) else NA_real_
  ct_p50  <- if (length(cycle_times_vec)) as.numeric(quantile(cycle_times_vec, 0.50)) else NA_real_
  ct_p90  <- if (length(cycle_times_vec)) as.numeric(quantile(cycle_times_vec, 0.90)) else NA_real_
  ct_p95  <- if (length(cycle_times_vec)) as.numeric(quantile(cycle_times_vec, 0.95)) else NA_real_

  # Full resource monitor (kept from t=0 so step-carry onto the post-warmup
  # grid still has a correct starting level).
  res_full <- get_mon_resources(env) %>%
    as_tibble() %>%
    mutate(replication = rep_id, seed = seed)

  res <- res_full %>% filter(time >= warmup)

  # Time-weighted average utilization and queue length per station
  station_stats <- res %>%
    group_by(resource) %>%
    group_modify(~{
      d <- arrange(.x, time)
      if (nrow(d) < 2) {
        return(tibble(avg_util = NA_real_, avg_queue = NA_real_, avg_server = NA_real_))
      }
      t0 <- d$time
      t1 <- lead(t0, default = sim_length)
      dt <- pmax(t1 - t0, 0)
      total_dt <- sum(dt)
      if (total_dt <= 0) {
        return(tibble(avg_util = NA_real_, avg_queue = NA_real_, avg_server = NA_real_))
      }
      cap <- d$capacity
      util <- ifelse(cap > 0, d$server / cap, NA_real_)
      tibble(
        avg_util   = sum(util * dt, na.rm = TRUE) / total_dt,
        avg_queue  = sum(d$queue * dt) / total_dt,
        avg_server = sum(d$server * dt) / total_dt
      )
    }) %>%
    ungroup()

  avg_wip <- station_stats %>%
    summarise(wip = sum(avg_server + avg_queue, na.rm = TRUE)) %>%
    pull(wip)

  cycle_times <- if (nrow(arr) == 0) {
    tibble(
      replication = integer(),
      seed        = integer(),
      vehicle     = character(),
      start_time  = numeric(),
      end_time    = numeric(),
      cycle_time  = numeric()
    )
  } else {
    tibble(
      replication = rep_id,
      seed        = seed,
      vehicle     = arr$name,
      start_time  = arr$start_time,
      end_time    = arr$end_time,
      cycle_time  = cycle_times_vec
    )
  }

  list(
    summary = tibble(
      replication       = rep_id,
      seed              = seed,
      n_completed       = n_completed,
      throughput_per_hr = throughput,
      cycle_time_mean   = ct_mean,
      cycle_time_p50    = ct_p50,
      cycle_time_p90    = ct_p90,
      cycle_time_p95    = ct_p95,
      avg_wip           = avg_wip
    ),
    stations = station_stats %>%
      mutate(replication = rep_id, seed = seed, .before = 1),
    cycle_times  = cycle_times,
    resources_ts = res_full
  )
}

# Resample every replication's resource monitor onto a common time grid.
# Returns list(queue_grid, wip_grid).
build_grid_series <- function(resources_ts, params) {
  warmup     <- params$warmup_min
  sim_length <- params$sim_length_min
  dt         <- params$grid_dt_min %||% 2
  if (dt <= 0) dt <- 2

  grid <- seq(from = warmup, to = sim_length, by = dt)
  if (length(grid) == 0L || tail(grid, 1) < sim_length) {
    grid <- c(grid, sim_length)
  }

  station_names <- params$stations$name
  reps <- sort(unique(resources_ts$replication))

  queue_rows <- list()
  wip_rows   <- list()
  k <- 0L

  for (r in reps) {
    res_r <- resources_ts %>% filter(replication == r)
    seed_r <- res_r$seed[[1]]
    wip_acc <- rep(0, length(grid))

    for (st in station_names) {
      d <- res_r %>%
        filter(resource == st) %>%
        arrange(time)
      q_grid <- resample_step(d$time, d$queue, grid)
      s_grid <- resample_step(d$time, d$server, grid)
      wip_acc <- wip_acc + coalesce(q_grid, 0) + coalesce(s_grid, 0)

      k <- k + 1L
      queue_rows[[k]] <- tibble(
        replication = r,
        seed        = seed_r,
        resource    = st,
        time        = grid,
        queue       = q_grid,
        server      = s_grid
      )
    }

    wip_rows[[as.character(r)]] <- tibble(
      replication = r,
      seed        = seed_r,
      time        = grid,
      wip         = wip_acc
    )
  }

  list(
    queue_timeseries = bind_rows(queue_rows),
    wip_timeseries   = bind_rows(wip_rows)
  )
}

# -----------------------------------------------------------------------------
# Public API: run_line_sim(params) -> tidy list of results
# -----------------------------------------------------------------------------

#' Run the automotive manufacturing line DES for N replications.
#'
#' @param params Named list (see default_params()).
#' @return A list with tidy tibbles used by plots, tables, and CSV exports:
#'   summaries, stations (utilization_by_rep), cycles, resources_ts (raw events,
#'   all reps), queue_timeseries (resampled), wip_timeseries (resampled),
#'   headline, station_headline, bottleneck, params, run_parameters,
#'   station_metrics, seeds
run_line_sim <- function(params = default_params()) {
  stopifnot(is.list(params))
  required <- c("sim_length_min", "n_replications", "base_seed",
                "warmup_min", "arrival_mean_min", "stations")
  missing <- setdiff(required, names(params))
  if (length(missing)) stop("Missing params: ", paste(missing, collapse = ", "))
  if (is.null(params$grid_dt_min)) params$grid_dt_min <- 2

  n_reps <- as.integer(params$n_replications)
  seeds  <- as.integer(params$base_seed) + seq_len(n_reps) - 1L

  results <- lapply(seq_along(seeds), function(i) {
    run_one_replication(rep_id = i, seed = seeds[[i]], params = params)
  })

  summaries    <- map_dfr(results, "summary")
  stations     <- map_dfr(results, "stations")
  cycles       <- map_dfr(results, "cycle_times")
  resources_ts <- map_dfr(results, "resources_ts")

  grids <- build_grid_series(resources_ts, params)

  headline <- summaries %>%
    summarise(
      across(
        c(throughput_per_hr, cycle_time_mean, cycle_time_p50, cycle_time_p90,
          cycle_time_p95, avg_wip, n_completed),
        list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      )
    )

  station_headline <- stations %>%
    group_by(resource) %>%
    summarise(
      avg_util_mean  = mean(avg_util, na.rm = TRUE),
      avg_util_sd    = sd(avg_util, na.rm = TRUE),
      avg_queue_mean = mean(avg_queue, na.rm = TRUE),
      avg_queue_sd   = sd(avg_queue, na.rm = TRUE),
      .groups = "drop"
    )

  # Bottleneck = highest mean utilization
  bottleneck <- station_headline %>%
    slice_max(avg_util_mean, n = 1, with_ties = FALSE) %>%
    pull(resource)

  run_parameters <- params_to_tibble(params)

  # Alias-friendly names for CSV exports
  utilization_by_rep <- stations %>%
    rename(station = resource)

  station_metrics <- station_headline %>%
    rename(station = resource)

  list(
    summaries           = summaries,
    stations            = stations,
    utilization_by_rep  = utilization_by_rep,
    cycles              = cycles,
    cycle_times         = cycles,
    resources_ts        = resources_ts,
    queue_timeseries    = grids$queue_timeseries,
    wip_timeseries      = grids$wip_timeseries,
    headline            = headline,
    station_headline    = station_headline,
    station_metrics     = station_metrics,
    bottleneck          = bottleneck,
    params              = params,
    run_parameters       = run_parameters,
    seeds               = seeds
  )
}

# -----------------------------------------------------------------------------
# CSV / zip export helpers (shared by Shiny, Quarto, and the standalone script)
# -----------------------------------------------------------------------------

#' Build a named list of tibbles ready for CSV export.
sim_export_tables <- function(res) {
  list(
    queue_timeseries   = res$queue_timeseries,
    wip_timeseries     = res$wip_timeseries,
    utilization_by_rep = res$utilization_by_rep,
    cycle_times        = res$cycle_times,
    station_metrics    = res$station_metrics,
    summaries          = res$summaries,
    run_parameters      = res$run_parameters
  )
}

#' Write every export table as CSV under `dir`. Optionally also write a zip.
#'
#' @param res Result of run_line_sim().
#' @param dir Output directory (created if missing).
#' @param zip_path Optional path for a zip bundling the CSVs. NULL = skip.
#' @return Invisibly, a character vector of written CSV paths.
write_sim_exports <- function(res, dir, zip_path = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tables <- sim_export_tables(res)
  paths <- character(0)

  for (nm in names(tables)) {
    p <- file.path(dir, paste0(nm, ".csv"))
    readr::write_csv(tables[[nm]], p)
    paths <- c(paths, p)
  }

  if (!is.null(zip_path)) {
    # Prefer zip::zipr when available (declared dependency for shinyapps.io);
    # fall back to utils::zip.
    zip_dir <- dirname(zip_path)
    if (!dir.exists(zip_dir) && nzchar(zip_dir) && zip_dir != ".") {
      dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (file.exists(zip_path)) file.remove(zip_path)
    csv_basenames <- paste0(names(tables), ".csv")
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::zipr(zipfile = zip_path, files = csv_basenames, root = dir)
    } else {
      owd <- setwd(dir)
      on.exit(setwd(owd), add = TRUE)
      utils::zip(zipfile = zip_path, files = csv_basenames, flags = "-jq")
    }
  }

  invisible(paths)
}
