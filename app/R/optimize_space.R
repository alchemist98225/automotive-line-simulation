# optimize_space.R
# Decision variables and cost-equivalent rates for the line optimizer.
# Baseline knobs match Scenario 2 (shared 2+2 operators, 2 paint, 10 veh/hr start).
# Sourced by opt_lib.R.

# Illustrative labor-hour equivalents per clock-hour of resource held.
# capacity_cost_h: one extra paint capacity unit ≈ one operator-hour per hour.
# queue_cost_h: one extra queue spot ≈ 0.1 operator-hour per hour.
# Tunable via opt_run.R --capacity-cost / --queue-cost.
opt_default_capacity_cost_h <- 1.0
opt_default_queue_cost_h <- 0.1

# Scenario 2 / Compare Experiment: shared upstream=2, downstream=2, paint=2.
# Throughput floor starts near 10 veh/hr; the GA searches for lower HPU/cost
# while staying at least that productive.
opt_guided_experiment_params <- function() {
  # Scenario 2 staffing/paint at 10 veh/hr demand (same as the preloads; S2 breaks near 12),
  # At 10/hr a 3-operator setup can hold throughput, which the search should find.
  p <- default_params()
  p$arrival_mean_min <- 60 / 10
  p$operators <- tibble::tibble(
    pool = c("upstream", "downstream"),
    count = c(2L, 2L)
  )
  p$stations$operator_pool <- c("upstream", "upstream", "downstream", "downstream")
  p$stations$capacity[p$stations$name == "paint"] <- 2L
  normalize_params(p)
}

opt_knob_defs <- function() {
  # Defaults = Scenario 2 (~10 veh/hr shared 2-paint set).
  tibble::tribble(
    ~name,                    ~lo, ~hi, ~default, ~label,
    "buffer_paint",              0L,  8L,       3L, "Queue before paint",
    "buffer_final_assembly",     0L,  8L,       3L, "Queue before final assembly",
    "buffer_inspection",         0L,  8L,       3L, "Queue before inspection",
    "op_upstream",               1L,  5L,       2L, "Upstream operators",
    "op_downstream",             1L,  5L,       2L, "Downstream operators",
    "cap_paint",                 1L,  4L,       2L, "Paint capacity"
  )
}

opt_clip_genome <- function(genome, defs = opt_knob_defs()) {
  genome <- as.integer(round(genome))
  stopifnot(length(genome) == nrow(defs))
  pmax(defs$lo, pmin(defs$hi, genome))
}

opt_default_genome <- function(defs = opt_knob_defs()) {
  as.integer(defs$default)
}

opt_random_genome <- function(defs = opt_knob_defs()) {
  as.integer(purrr::map2_int(defs$lo, defs$hi, ~ sample.int(.y - .x + 1L, 1L) + .x - 1L))
}

opt_genome_to_row <- function(genome, design_id = "candidate", defs = opt_knob_defs()) {
  genome <- opt_clip_genome(genome, defs)
  row <- as.list(stats::setNames(as.integer(genome), defs$name))
  row$design_id <- as.character(design_id)
  tibble::as_tibble(row)
}

opt_row_to_genome <- function(row, defs = opt_knob_defs()) {
  vals <- vapply(defs$name, function(nm) {
    if (!nm %in% names(row) || is.na(row[[nm]][[1]])) {
      as.integer(defs$default[defs$name == nm])
    } else {
      as.integer(row[[nm]][[1]])
    }
  }, integer(1))
  opt_clip_genome(vals, defs)
}

opt_genome_key <- function(genome) {
  paste(opt_clip_genome(genome), collapse = "_")
}

# Extra paint capacity and queue spots vs baseline genome (can be negative = savings).
opt_resource_delta <- function(genome, baseline_genome = opt_default_genome(),
                               defs = opt_knob_defs()) {
  genome <- opt_clip_genome(genome, defs)
  baseline_genome <- opt_clip_genome(baseline_genome, defs)
  names(genome) <- defs$name
  names(baseline_genome) <- defs$name
  queue_names <- c("buffer_paint", "buffer_final_assembly", "buffer_inspection")
  list(
    extra_paint_cap = as.integer(genome[["cap_paint"]] - baseline_genome[["cap_paint"]]),
    extra_queue_spots = as.integer(sum(genome[queue_names] - baseline_genome[queue_names]))
  )
}

# Clock-hour cost-equivalent rate, then per completed unit via throughput.
opt_cost_hpu <- function(genome, thr_mean,
                         baseline_genome = opt_default_genome(),
                         capacity_cost_h = opt_default_capacity_cost_h,
                         queue_cost_h = opt_default_queue_cost_h,
                         defs = opt_knob_defs()) {
  d <- opt_resource_delta(genome, baseline_genome, defs)
  rate <- as.numeric(capacity_cost_h) * d$extra_paint_cap +
    as.numeric(queue_cost_h) * d$extra_queue_spots
  thr <- if (is.finite(thr_mean) && thr_mean > 0) thr_mean else 1e-6
  list(
    extra_paint_cap = d$extra_paint_cap,
    extra_queue_spots = d$extra_queue_spots,
    cost_rate_h = rate,
    cost_hpu = rate / thr
  )
}
