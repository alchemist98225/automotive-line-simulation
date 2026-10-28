# =============================================================================
# model.R — reusable automotive line discrete-event simulation (simmer)
#
# All numeric defaults are PLACEHOLDER EXAMPLE VALUES for a portfolio demo.
# Swap in real plant data before using for decisions.
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
    stations = tribble(
      ~name,              ~capacity, ~dist,         ~mean_min, ~sd_min,
      "body_shop",        2L,        "exponential", 6.0,       NA_real_,
      "paint",            1L,        "normal",      8.0,       1.5,
      "final_assembly",   2L,        "normal",      7.0,       1.0,
      "inspection",       1L,        "exponential", 3.0,       NA_real_
    )
  )
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

  cycle_times <- arr$end_time - arr$start_time
  ct_mean <- if (length(cycle_times)) mean(cycle_times) else NA_real_
  ct_p50  <- if (length(cycle_times)) as.numeric(quantile(cycle_times, 0.50)) else NA_real_
  ct_p90  <- if (length(cycle_times)) as.numeric(quantile(cycle_times, 0.90)) else NA_real_
  ct_p95  <- if (length(cycle_times)) as.numeric(quantile(cycle_times, 0.95)) else NA_real_

  res <- get_mon_resources(env) %>%
    as_tibble() %>%
    filter(time >= warmup)

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
    cycle_times = tibble(
      replication = rep_id,
      cycle_time  = if (length(cycle_times)) cycle_times else numeric(0)
    ),
    resources_ts = res %>%
      mutate(replication = rep_id)
  )
}

# -----------------------------------------------------------------------------
# Public API: run_line_sim(params) -> tidy list of results
# -----------------------------------------------------------------------------

#' Run the automotive manufacturing line DES for N replications.
#'
#' @param params Named list (see default_params()). Must include:
#'   sim_length_min, n_replications, base_seed, warmup_min, arrival_mean_min,
#'   stations (tibble with name, capacity, dist, mean_min, sd_min).
#' @return A list with:
#'   summaries, stations, cycles, resources_ts (rep 1),
#'   headline, station_headline, bottleneck, params
run_line_sim <- function(params = default_params()) {
  stopifnot(is.list(params))
  required <- c("sim_length_min", "n_replications", "base_seed",
                "warmup_min", "arrival_mean_min", "stations")
  missing <- setdiff(required, names(params))
  if (length(missing)) stop("Missing params: ", paste(missing, collapse = ", "))

  n_reps <- as.integer(params$n_replications)
  seeds  <- as.integer(params$base_seed) + seq_len(n_reps) - 1L

  results <- lapply(seq_along(seeds), function(i) {
    run_one_replication(rep_id = i, seed = seeds[[i]], params = params)
  })

  summaries <- map_dfr(results, "summary")
  stations  <- map_dfr(results, "stations")
  cycles    <- map_dfr(results, "cycle_times")
  res_ts    <- results[[1]]$resources_ts

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

  list(
    summaries        = summaries,
    stations         = stations,
    cycles           = cycles,
    resources_ts     = res_ts,
    headline         = headline,
    station_headline = station_headline,
    bottleneck       = bottleneck,
    params           = params
  )
}
