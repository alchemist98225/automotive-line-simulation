# opt_fitness.R
# Noise-aware fitness: mean HPU with a soft penalty when throughput falls below baseline.
# Reuses doe_params_from_row / run_line_sim / doe_extract_rep_metrics.
# Sourced by opt_lib.R.

opt_eval_cache_new <- function() {
  new.env(parent = emptyenv())
}

# Evaluate one genome. Shared seed schedule: base_seed + (cand_index-1)*n_reps + (rep-1)
# matches the DOE CRN idea so candidates share common random numbers across the same
# replication slot when cand_index is held fixed for a given design identity.
opt_evaluate_genome <- function(genome,
                                n_reps,
                                base_seed,
                                cand_index,
                                thr_floor,
                                penalty,
                                cache = NULL,
                                defs = opt_knob_defs()) {
  genome <- opt_clip_genome(genome, defs)
  key <- paste(opt_genome_key(genome), n_reps, base_seed, cand_index, sep = "|")
  if (!is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
    return(cache[[key]])
  }

  row <- opt_genome_to_row(genome, design_id = sprintf("g%04d", as.integer(cand_index)), defs)
  seed0 <- doe_rep_seed(base_seed, as.integer(cand_index), 1L, as.integer(n_reps))
  params <- doe_params_from_row(row, n_replications = as.integer(n_reps), base_seed = seed0)
  result <- run_line_sim(params)
  metrics <- doe_extract_rep_metrics(result, row, result$seeds)

  hpu_mean <- mean(if_else(is.finite(metrics$hpu), metrics$hpu, NA_real_), na.rm = TRUE)
  thr_mean <- mean(metrics$throughput_per_hr, na.rm = TRUE)
  if (!is.finite(hpu_mean)) hpu_mean <- 1e6
  if (!is.finite(thr_mean)) thr_mean <- 0

  shortfall <- max(0, thr_floor - thr_mean)
  fitness <- hpu_mean + as.numeric(penalty) * shortfall

  out <- list(
    genome = genome,
    design_id = row$design_id[[1]],
    n_reps = as.integer(n_reps),
    cand_index = as.integer(cand_index),
    base_seed = as.integer(base_seed),
    hpu_mean = hpu_mean,
    thr_mean = thr_mean,
    thr_floor = thr_floor,
    shortfall = shortfall,
    fitness = fitness,
    feasible = shortfall <= 0,
    metrics = metrics,
    params = result$params,
    run_parameters = result$run_parameters
  )
  if (!is.null(cache)) cache[[key]] <- out
  out
}

opt_baseline_metrics <- function(n_reps, base_seed, defs = opt_knob_defs()) {
  g <- opt_default_genome(defs)
  opt_evaluate_genome(
    genome = g,
    n_reps = n_reps,
    base_seed = base_seed,
    cand_index = 1L,
    thr_floor = 0,
    penalty = 0,
    cache = NULL,
    defs = defs
  )
}
