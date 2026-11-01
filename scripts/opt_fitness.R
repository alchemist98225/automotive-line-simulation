# opt_fitness.R
# Noise-aware fitness: mean HPU with a soft penalty when throughput falls below baseline.
# HPU and throughput come from run_line_sim()$summaries (same as the app).
# Baseline uses pristine default_params(); candidates overlay knobs via doe_params_from_row().
# Sourced by opt_lib.R.

opt_eval_cache_new <- function() {
  new.env(parent = emptyenv())
}

opt_summarise_result <- function(result) {
  hpu <- result$summaries$hpu
  thr <- result$summaries$throughput_per_hr
  hpu_mean <- mean(if_else(is.finite(hpu), hpu, NA_real_), na.rm = TRUE)
  thr_mean <- mean(thr, na.rm = TRUE)
  if (!is.finite(hpu_mean)) hpu_mean <- 1e6
  if (!is.finite(thr_mean)) thr_mean <- 0
  list(hpu_mean = hpu_mean, thr_mean = thr_mean, summaries = result$summaries)
}

# Pristine app defaults — only n_replications / base_seed change.
# Seeds for final CRN comparisons: base_seed + 0:(n_reps-1), same as run_line_sim().
opt_evaluate_default_params <- function(n_reps,
                                        base_seed,
                                        thr_floor = 0,
                                        penalty = 0,
                                        cache = NULL) {
  n_reps <- as.integer(n_reps)
  base_seed <- as.integer(base_seed)
  key <- paste("default", n_reps, base_seed, sep = "|")
  if (!is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
    return(cache[[key]])
  }

  params <- default_params()
  params$n_replications <- n_reps
  params$base_seed <- base_seed
  result <- run_line_sim(params)
  sm <- opt_summarise_result(result)
  shortfall <- max(0, thr_floor - sm$thr_mean)
  fitness <- sm$hpu_mean + as.numeric(penalty) * shortfall

  # Design-row view of the default knobs (for DOE-shaped exports)
  defs <- opt_knob_defs()
  row <- opt_genome_to_row(opt_default_genome(defs), design_id = "baseline", defs = defs)
  metrics <- doe_extract_rep_metrics(result, row, result$seeds)

  out <- list(
    genome = opt_default_genome(defs),
    design_id = "baseline",
    n_reps = n_reps,
    cand_index = 1L,
    base_seed = base_seed,
    hpu_mean = sm$hpu_mean,
    thr_mean = sm$thr_mean,
    thr_floor = thr_floor,
    shortfall = shortfall,
    fitness = fitness,
    feasible = shortfall <= 0,
    metrics = metrics,
    params = result$params,
    run_parameters = result$run_parameters,
    operator_by_rep = result$operator_by_rep,
    utilization_by_rep = result$utilization_by_rep
  )
  if (!is.null(cache)) cache[[key]] <- out
  out
}

# Candidate genome. Search uses cand_index-derived seeds; final CRN compare
# can pass shared_seed=TRUE to use base_seed..base_seed+n_reps-1 like the baseline.
opt_evaluate_genome <- function(genome,
                                n_reps,
                                base_seed,
                                cand_index,
                                thr_floor,
                                penalty,
                                cache = NULL,
                                defs = opt_knob_defs(),
                                shared_seed = FALSE) {
  genome <- opt_clip_genome(genome, defs)
  n_reps <- as.integer(n_reps)
  base_seed <- as.integer(base_seed)
  cand_index <- as.integer(cand_index)
  key <- paste(opt_genome_key(genome), n_reps, base_seed, cand_index,
               if (isTRUE(shared_seed)) "shared" else "indexed", sep = "|")
  if (!is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
    return(cache[[key]])
  }

  row <- opt_genome_to_row(genome, design_id = sprintf("g%04d", cand_index), defs)
  seed0 <- if (isTRUE(shared_seed)) {
    base_seed
  } else {
    doe_rep_seed(base_seed, cand_index, 1L, n_reps)
  }
  params <- doe_params_from_row(row, n_replications = n_reps, base_seed = seed0)
  result <- run_line_sim(params)
  sm <- opt_summarise_result(result)
  shortfall <- max(0, thr_floor - sm$thr_mean)
  fitness <- sm$hpu_mean + as.numeric(penalty) * shortfall
  metrics <- doe_extract_rep_metrics(result, row, result$seeds)

  out <- list(
    genome = genome,
    design_id = row$design_id[[1]],
    n_reps = n_reps,
    cand_index = cand_index,
    base_seed = seed0,
    hpu_mean = sm$hpu_mean,
    thr_mean = sm$thr_mean,
    thr_floor = thr_floor,
    shortfall = shortfall,
    fitness = fitness,
    feasible = shortfall <= 0,
    metrics = metrics,
    params = result$params,
    run_parameters = result$run_parameters,
    operator_by_rep = result$operator_by_rep,
    utilization_by_rep = result$utilization_by_rep
  )
  if (!is.null(cache)) cache[[key]] <- out
  out
}

opt_baseline_metrics <- function(n_reps, base_seed, defs = opt_knob_defs()) {
  opt_evaluate_default_params(
    n_reps = n_reps,
    base_seed = base_seed,
    thr_floor = 0,
    penalty = 0,
    cache = NULL
  )
}
