#!/usr/bin/env Rscript
# =============================================================================
# manufacturing_sim.R
# Starter discrete-event simulation of a serial automotive manufacturing line
# using simmer + tidyverse.
#
# ALL numeric inputs below are PLACEHOLDER EXAMPLE VALUES — replace with real
# plant data before using for decisions.
# =============================================================================

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.plot)
  library(tidyverse)  # dplyr::select masks simmer::select; lubridate::now masks simmer::now
  # tidymodels installed for downstream modeling; not required for this DES
})
# Qualify simmer time helper (lubridate::now is on the search path).
sim_now <- function(env) simmer::now(env)

# -----------------------------------------------------------------------------
# PARAMETERS (PLACEHOLDER EXAMPLE VALUES)
# -----------------------------------------------------------------------------

# --- Simulation control ---
SIM_LENGTH_MIN   <- 480        # PLACEHOLDER: one 8-hour shift (minutes)
N_REPLICATIONS   <- 10         # PLACEHOLDER: number of independent replications
BASE_SEED        <- 20261027L  # PLACEHOLDER: base RNG seed for reproducibility
WARMUP_MIN       <- 60         # PLACEHOLDER: warmup to discard (minutes); 0 = none

# --- Arrivals (bodies entering the line) ---
# PLACEHOLDER: mean interarrival ~ 4 min (15 vehicles/hour)
ARRIVAL_MEAN_MIN <- 4

# --- Serial stations (automotive-flavoured placeholders) ---
# capacity = number of parallel servers / bays at that station
# cycle time distributions are PLACEHOLDER examples only
STATIONS <- tribble(
  ~name,              ~capacity, ~dist,         ~mean_min, ~sd_min,
  "body_shop",        2L,        "exponential", 6.0,       NA_real_,   # PLACEHOLDER
  "paint",            1L,        "normal",      8.0,       1.5,        # PLACEHOLDER
  "final_assembly",   2L,        "normal",      7.0,       1.0,        # PLACEHOLDER
  "inspection",       1L,        "exponential", 3.0,       NA_real_    # PLACEHOLDER
)

# --- Output paths ---
# Regenerated artifacts (gitignored). Run from the repository root:
#   Rscript scripts/manufacturing_sim.R
OUT_DIR        <- "output"
METRICS_CSV    <- file.path(OUT_DIR, "metrics_summary.csv")
PLOT_UTIL      <- file.path(OUT_DIR, "utilization.png")
PLOT_QUEUE     <- file.path(OUT_DIR, "queue_length.png")
PLOT_CYCLE     <- file.path(OUT_DIR, "cycle_time_dist.png")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

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
run_one_replication <- function(rep_id, seed) {
  set.seed(seed)

  env <- simmer(paste0("auto_line_rep", rep_id))

  # Register resources
  for (i in seq_len(nrow(STATIONS))) {
    env <- env %>%
      add_resource(STATIONS$name[[i]], capacity = STATIONS$capacity[[i]])
  }

  # Trajectory: serial seize -> timeout -> release through each station
  traj <- trajectory("vehicle") %>%
    set_attribute("start_time", function() sim_now(env))

  for (i in seq_len(nrow(STATIONS))) {
    st <- STATIONS[i, ]
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
      name_prefix = "veh",
      trajectory  = traj,
      distribution = function() rexp(1L, rate = 1 / ARRIVAL_MEAN_MIN),
      mon = 2
    ) %>%
    run(until = SIM_LENGTH_MIN)

  # --- Arrivals / completions (post-warmup) ---
  arr <- get_mon_arrivals(env, per_resource = FALSE) %>%
    as_tibble() %>%
    filter(start_time >= WARMUP_MIN, finished)

  n_completed <- nrow(arr)
  horizon     <- SIM_LENGTH_MIN - WARMUP_MIN
  throughput  <- if (horizon > 0) n_completed / (horizon / 60) else NA_real_  # per hour

  cycle_times <- arr$end_time - arr$start_time
  ct_mean     <- if (length(cycle_times)) mean(cycle_times) else NA_real_
  ct_p50      <- if (length(cycle_times)) as.numeric(quantile(cycle_times, 0.50)) else NA_real_
  ct_p90      <- if (length(cycle_times)) as.numeric(quantile(cycle_times, 0.90)) else NA_real_
  ct_p95      <- if (length(cycle_times)) as.numeric(quantile(cycle_times, 0.95)) else NA_real_

  # --- Resource monitors: utilization & queue over time ---
  res <- get_mon_resources(env) %>%
    as_tibble() %>%
    filter(time >= WARMUP_MIN)

  # Time-weighted average utilization and queue length per station
  station_stats <- res %>%
    group_by(resource) %>%
    group_modify(~{
      d <- arrange(.x, time)
      if (nrow(d) < 2) {
        return(tibble(avg_util = NA_real_, avg_queue = NA_real_, avg_server = NA_real_))
      }
      t0 <- d$time
      t1 <- lead(t0, default = SIM_LENGTH_MIN)
      dt <- pmax(t1 - t0, 0)
      total_dt <- sum(dt)
      if (total_dt <= 0) {
        return(tibble(avg_util = NA_real_, avg_queue = NA_real_, avg_server = NA_real_))
      }
      cap <- d$capacity
      # utilization = busy servers / capacity
      util <- ifelse(cap > 0, d$server / cap, NA_real_)
      tibble(
        avg_util   = sum(util * dt, na.rm = TRUE) / total_dt,
        avg_queue  = sum(d$queue * dt) / total_dt,
        avg_server = sum(d$server * dt) / total_dt
      )
    }) %>%
    ungroup()

  # Average WIP ≈ average number in system (servers + queues), summed over stations
  avg_wip <- station_stats %>%
    summarise(wip = sum(avg_server + avg_queue, na.rm = TRUE)) %>%
    pull(wip)

  list(
    summary = tibble(
      replication   = rep_id,
      seed          = seed,
      n_completed   = n_completed,
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
      cycle_time  = cycle_times
    ),
    resources_ts = res %>%
      mutate(replication = rep_id)
  )
}

# -----------------------------------------------------------------------------
# Run replications (lapply over seeds — each replication sets its own seed)
# -----------------------------------------------------------------------------

seeds <- BASE_SEED + seq_len(N_REPLICATIONS) - 1L

message("Running ", N_REPLICATIONS, " replications of automotive line DES ...")
results <- lapply(seq_along(seeds), function(i) {
  run_one_replication(rep_id = i, seed = seeds[[i]])
})

summaries <- map_dfr(results, "summary")
stations  <- map_dfr(results, "stations")
cycles    <- map_dfr(results, "cycle_times")
# Use first replication's resource time series for illustrative plots
res_ts_plot <- results[[1]]$resources_ts

# -----------------------------------------------------------------------------
# Aggregate headline metrics across replications
# -----------------------------------------------------------------------------

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

# Combined metrics table for CSV
metrics_out <- bind_rows(
  summaries %>%
    mutate(metric_group = "per_replication_system") %>%
    pivot_longer(
      cols = -c(replication, seed, metric_group),
      names_to = "metric",
      values_to = "value"
    ),
  stations %>%
    mutate(metric_group = "per_replication_station") %>%
    pivot_longer(
      cols = c(avg_util, avg_queue, avg_server),
      names_to = "metric",
      values_to = "value"
    ) %>%
    rename(station = resource) %>%
    dplyr::select(replication, seed, metric_group, station, metric, value),
  headline %>%
    mutate(replication = NA_integer_, seed = NA_integer_, metric_group = "across_reps_system") %>%
    pivot_longer(
      cols = -c(replication, seed, metric_group),
      names_to = "metric",
      values_to = "value"
    ),
  station_headline %>%
    mutate(replication = NA_integer_, seed = NA_integer_,
           metric_group = "across_reps_station") %>%
    rename(station = resource) %>%
    pivot_longer(
      cols = -c(replication, seed, metric_group, station),
      names_to = "metric",
      values_to = "value"
    )
) %>%
  mutate(
    note = "PLACEHOLDER EXAMPLE PARAMETERS — not real plant data",
    sim_length_min = SIM_LENGTH_MIN,
    warmup_min = WARMUP_MIN,
    n_replications = N_REPLICATIONS,
    base_seed = BASE_SEED
  )

write_csv(metrics_out, METRICS_CSV)
message("Wrote ", METRICS_CSV)

# -----------------------------------------------------------------------------
# Plots (PNG)
# -----------------------------------------------------------------------------

# 1) Utilization by station (mean across replications +/- sd)
p_util <- station_headline %>%
  mutate(resource = factor(resource, levels = STATIONS$name)) %>%
  ggplot(aes(x = resource, y = avg_util_mean)) +
  geom_col(fill = "#2c7fb8", width = 0.7) +
  geom_errorbar(
    aes(ymin = pmax(0, avg_util_mean - avg_util_sd),
        ymax = pmin(1, avg_util_mean + avg_util_sd)),
    width = 0.2
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Station utilization (placeholder automotive line)",
    subtitle = paste0(
      "Mean +/- SD over ", N_REPLICATIONS, " replications | PLACEHOLDER params"
    ),
    x = "Station",
    y = "Utilization"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(PLOT_UTIL, p_util, width = 8, height = 5, dpi = 120)
message("Wrote ", PLOT_UTIL)

# 2) Queue length over time (replication 1, illustrative)
p_queue <- res_ts_plot %>%
  mutate(resource = factor(resource, levels = STATIONS$name)) %>%
  ggplot(aes(x = time, y = queue, color = resource)) +
  geom_step(linewidth = 0.6) +
  labs(
    title = "Queue length over time (replication 1)",
    subtitle = "PLACEHOLDER automotive line | illustrative single run",
    x = "Time (minutes)",
    y = "Queue length",
    color = "Station"
  ) +
  theme_minimal(base_size = 12)

ggsave(PLOT_QUEUE, p_queue, width = 9, height = 5, dpi = 120)
message("Wrote ", PLOT_QUEUE)

# 3) Cycle time distribution across all replications
p_cycle <- cycles %>%
  ggplot(aes(x = cycle_time)) +
  geom_histogram(bins = 30, fill = "#41b6c4", color = "white") +
  geom_vline(
    xintercept = mean(cycles$cycle_time, na.rm = TRUE),
    linetype = "dashed",
    color = "#253494"
  ) +
  labs(
    title = "Vehicle cycle time distribution",
    subtitle = paste0(
      "All completed vehicles across ", N_REPLICATIONS,
      " replications | PLACEHOLDER params"
    ),
    x = "Cycle time (minutes)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

ggsave(PLOT_CYCLE, p_cycle, width = 8, height = 5, dpi = 120)
message("Wrote ", PLOT_CYCLE)

# -----------------------------------------------------------------------------
# Console headline (PLACEHOLDER results)
# -----------------------------------------------------------------------------

cat("\n========== HEADLINE METRICS (PLACEHOLDER EXAMPLE PARAMETERS) ==========\n")
cat(sprintf("R simmer DES | %d reps | sim length %.0f min | warmup %.0f min\n",
            N_REPLICATIONS, SIM_LENGTH_MIN, WARMUP_MIN))
cat(sprintf("Stations: %s\n", paste(STATIONS$name, collapse = " -> ")))
cat(sprintf("Throughput (veh/hr):     mean=%.2f  sd=%.2f\n",
            headline$throughput_per_hr_mean, headline$throughput_per_hr_sd))
cat(sprintf("Cycle time mean (min):   mean=%.2f  sd=%.2f\n",
            headline$cycle_time_mean_mean, headline$cycle_time_mean_sd))
cat(sprintf("Cycle time p50 / p90 / p95 (min, across-rep means): %.2f / %.2f / %.2f\n",
            headline$cycle_time_p50_mean, headline$cycle_time_p90_mean,
            headline$cycle_time_p95_mean))
cat(sprintf("Average WIP (veh):       mean=%.2f  sd=%.2f\n",
            headline$avg_wip_mean, headline$avg_wip_sd))
cat("\nPer-station utilization & avg queue (across-rep means):\n")
print(as.data.frame(station_headline), row.names = FALSE)
cat("=======================================================================\n")
cat("NOTE: All inputs are placeholder example values, not real plant data.\n")
