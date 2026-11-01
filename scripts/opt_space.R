# opt_space.R
# Integer decision variables for the line optimizer (queues, zone ops, paint capacity).
# Sourced by opt_lib.R.

opt_knob_defs <- function() {
  tibble::tribble(
    ~name,                    ~lo, ~hi, ~default, ~label,
    "buffer_paint",              0L,  8L,       3L, "Queue before paint",
    "buffer_final_assembly",     0L,  8L,       3L, "Queue before final assembly",
    "buffer_inspection",         0L,  8L,       3L, "Queue before inspection",
    "op_upstream",               1L,  5L,       2L, "Upstream operators",
    "op_downstream",             1L,  5L,       2L, "Downstream operators",
    "cap_paint",                 1L,  4L,       1L, "Paint capacity"
  )
}

# One genome is an integer vector aligned with opt_knob_defs()$name.
opt_clip_genome <- function(genome, defs = opt_knob_defs()) {
  genome <- as.integer(round(genome))
  stopifnot(length(genome) == nrow(defs))
  pmax(defs$lo, pmin(defs$hi, genome))
}

opt_default_genome <- function(defs = opt_knob_defs()) {
  as.integer(defs$default)
}

opt_random_genome <- function(defs = opt_knob_defs()) {
  as.integer(map2_int(defs$lo, defs$hi, ~ sample.int(.y - .x + 1L, 1L) + .x - 1L))
}

opt_genome_to_row <- function(genome, design_id = "candidate", defs = opt_knob_defs()) {
  genome <- opt_clip_genome(genome, defs)
  row <- as.list(setNames(as.integer(genome), defs$name))
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
