# opt_fitness.R
# Fitness = HPU + resource cost-equivalent per unit + throughput-shortfall penalty.
# Baseline = guided Experiment (paint capacity 2 via opt_guided_experiment_params()).
# HPU from run_line_sim()$summaries$hpu (app definition).
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

# Build run params: guided experiment, then overlay genome knobs.
opt_params_from_genome <- function(genome, n_reps, base_seed, defs = opt_knob_defs()) {
  row <- opt_genome_to_row(genome, design_id = "candidate", defs = defs)
  # Start from guided experiment so non-knob fields match the app Try-it set.
  base <- opt_guided_experiment_params()
  # doe_params_from_row starts from default_params(); overlay onto guided by
  # writing knobs onto a design row that also carries guided non-defaults.
  # Simpler: doe_params_from_row then force paint/queues/ops from genome
  # (already in row) — but default_params has paint=1. Row includes cap_paint
  # so doe_params_from_row sets it. Guided == default except paint cap, so
  # with default genome (cap_paint=2) we match guided. Good.
  doe_params_from_row(row, n_replications = as.integer(n_reps), base_seed = as.integer(base_seed))
}

opt_pack_eval <- function(genome, result, n_reps, base_seed, cand_index,
                          thr_floor, penalty,
                          baseline_genome,
                          capacity_cost_h, queue_cost_h,
                          defs, design_id) {
  sm <- opt_summarise_result(result)
  cost <- opt_cost_hpu(
    genome, sm$thr_mean,
    baseline_genome = baseline_genome,
    capacity_cost_h = capacity_cost_h,
    queue_cost_h = queue_cost_h,
    defs = defs
  )
  shortfall <- max(0, thr_floor - sm$thr_mean)
  fitness <- sm$hpu_mean + cost$cost_hpu + as.numeric(penalty) * shortfall
  row <- opt_genome_to_row(genome, design_id = design_id, defs = defs)
  metrics <- doe_extract_rep_metrics(result, row, result$seeds)
  list(
    genome = opt_clip_genome(genome, defs),
    design_id = design_id,
    n_reps = as.integer(n_reps),
    cand_index = as.integer(cand_index),
    base_seed = as.integer(base_seed),
    hpu_mean = sm$hpu_mean,
    thr_mean = sm$thr_mean,
    cost_hpu = cost$cost_hpu,
    cost_rate_h = cost$cost_rate_h,
    extra_paint_cap = cost$extra_paint_cap,
    extra_queue_spots = cost$extra_queue_spots,
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
}

# Baseline = guided Experiment params (not 1-paint defaults).
opt_evaluate_baseline <- function(n_reps,
                                  base_seed,
                                  thr_floor = 0,
                                  penalty = 0,
                                  capacity_cost_h = opt_default_capacity_cost_h,
                                  queue_cost_h = opt_default_queue_cost_h,
                                  cache = NULL,
                                  defs = opt_knob_defs()) {
  n_reps <- as.integer(n_reps)
  base_seed <- as.integer(base_seed)
  key <- paste("guided_baseline", n_reps, base_seed, capacity_cost_h, queue_cost_h, sep = "|")
  if (!is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
    return(cache[[key]])
  }

  genome <- opt_default_genome(defs)
  params <- opt_guided_experiment_params()
  params$n_replications <- n_reps
  params$base_seed <- base_seed
  # Ensure paint capacity matches guided default genome
  params$stations$capacity[params$stations$name == "paint"] <- as.integer(genome[defs$name == "cap_paint"])
  result <- run_line_sim(params)
  out <- opt_pack_eval(
    genome, result, n_reps, base_seed, 1L, thr_floor, penalty,
    baseline_genome = genome,
    capacity_cost_h, queue_cost_h, defs, design_id = "baseline"
  )
  # Cost vs self should be ~0
  if (!is.null(cache)) cache[[key]] <- out
  out
}

opt_evaluate_genome <- function(genome,
                                n_reps,
                                base_seed,
                                cand_index,
                                thr_floor,
                                penalty,
                                cache = NULL,
                                defs = opt_knob_defs(),
                                shared_seed = FALSE,
                                baseline_genome = opt_default_genome(defs),
                                capacity_cost_h = opt_default_capacity_cost_h,
                                queue_cost_h = opt_default_queue_cost_h) {
  genome <- opt_clip_genome(genome, defs)
  n_reps <- as.integer(n_reps)
  base_seed <- as.integer(base_seed)
  cand_index <- as.integer(cand_index)
  key <- paste(
    opt_genome_key(genome), n_reps, base_seed, cand_index,
    if (isTRUE(shared_seed)) "shared" else "indexed",
    capacity_cost_h, queue_cost_h,
    sep = "|"
  )
  if (!is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
    return(cache[[key]])
  }

  seed0 <- if (isTRUE(shared_seed)) base_seed else doe_rep_seed(base_seed, cand_index, 1L, n_reps)
  params <- opt_params_from_genome(genome, n_reps, seed0, defs)
  result <- run_line_sim(params)
  out <- opt_pack_eval(
    genome, result, n_reps, seed0, cand_index, thr_floor, penalty,
    baseline_genome = baseline_genome,
    capacity_cost_h, queue_cost_h, defs,
    design_id = sprintf("g%04d", cand_index)
  )
  if (!is.null(cache)) cache[[key]] <- out
  out
}

# Back-compat name used by opt_ga.R
opt_baseline_metrics <- function(n_reps, base_seed, defs = opt_knob_defs(),
                                 capacity_cost_h = opt_default_capacity_cost_h,
                                 queue_cost_h = opt_default_queue_cost_h) {
  opt_evaluate_baseline(
    n_reps = n_reps,
    base_seed = base_seed,
    thr_floor = 0,
    penalty = 0,
    capacity_cost_h = capacity_cost_h,
    queue_cost_h = queue_cost_h,
    cache = NULL,
    defs = defs
  )
}

# Keep name for tests that still call the old pristine-default helper.
opt_evaluate_default_params <- function(n_reps, base_seed, thr_floor = 0, penalty = 0, cache = NULL) {
  opt_evaluate_baseline(n_reps, base_seed, thr_floor, penalty, cache = cache)
}
