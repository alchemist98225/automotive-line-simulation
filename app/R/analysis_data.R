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

scenario_dedicated_one_paint <- function(n_replications = 20L, base = default_params()) {
  p <- base
  p$n_replications <- as.integer(n_replications)
  p$operators <- tibble::tibble(
    pool = analysis_station_names,
    count = rep(1L, length(analysis_station_names))
  )
  p$stations$operator_pool <- p$stations$name
  p$stations$capacity[p$stations$name == "paint"] <- 1L
  normalize_params(p)
}

scenario_shared_two_paint <- function(n_replications = 20L, base = default_params()) {
  p <- base
  p$n_replications <- as.integer(n_replications)
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
