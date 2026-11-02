# analysis_data.R
# Load precomputed Analysis-page datasets and the startup scenario pair.
# Files live under app/data/ so a deploy from app/ includes them.

analysis_data_path <- function(filename) {
  candidates <- c(
    file.path("data", filename),
    file.path("app", "data", filename)
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("Missing precomputed file: ", filename, " (looked in data/ and app/data/)", call. = FALSE)
  }
  hit[[1]]
}

load_analysis_precomputed <- function() {
  readRDS(analysis_data_path("analysis_precomputed.rds"))
}

load_preload_scenarios <- function() {
  readRDS(analysis_data_path("preload_scenarios.rds"))
}

# Scenario builders (same shapes the precompute script uses).
analysis_station_names <- c("body_shop", "paint", "final_assembly", "inspection")

scenario_dedicated_one_paint <- function(n_replications = 20L, base = default_params(),
                                         demand_per_hr = preload_stretch_demand_per_hr) {
  p <- base
  p$n_replications <- as.integer(n_replications)
  p$arrival_mean_min <- 60 / as.numeric(demand_per_hr)
  p$operators <- tibble::tibble(
    pool = analysis_station_names,
    count = rep(1L, length(analysis_station_names))
  )
  p$stations$operator_pool <- p$stations$name
  p$stations$capacity[p$stations$name == "paint"] <- 1L
  normalize_params(p)
}

scenario_shared_two_paint <- function(n_replications = 20L, base = default_params(),
                                      demand_per_hr = preload_stretch_demand_per_hr) {
  p <- base
  p$n_replications <- as.integer(n_replications)
  p$arrival_mean_min <- 60 / as.numeric(demand_per_hr)
  p$operators <- tibble::tibble(
    pool = c("upstream", "downstream"),
    count = c(1L, 2L)
  )
  p$stations$operator_pool <- c("upstream", "upstream", "downstream", "downstream")
  p$stations$capacity[p$stations$name == "paint"] <- 2L
  normalize_params(p)
}

preload_scenario_labels <- c(
  "Dedicated operators, 1 paint bay",
  "Shared operators, 2 paint bays"
)

# Shared demand for both preloaded scenarios and Try it (5 reps).
# 10 veh/hr keeps Scenario 2 below its break (~12) so Compare tells a clear
# backlog story vs Scenario 1 without saturating the better line.
preload_stretch_demand_per_hr <- 10

# Return scenario1/scenario2 list when params fingerprint matches a preload, else NULL.
match_preload_scenario <- function(params, bundle) {
  if (is.null(bundle) || is.null(params)) return(NULL)
  fp <- params_fingerprint(params)
  for (nm in c("scenario1", "scenario2")) {
    sc <- bundle[[nm]]
    if (is.null(sc) || is.null(sc$params)) next
    if (identical(fp, params_fingerprint(sc$params))) return(sc)
  }
  NULL
}

