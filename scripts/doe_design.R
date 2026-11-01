# doe_design.R
# Design-table schema for the DOE CLI: known CSV columns, defaults,
# validation, and mapping one design row → default_params() for run_line_sim().
# Sourced by doe_lib.R (do not source this file directly from the CLI scripts).

doe_station_names <- function() {
  c("body_shop", "paint", "final_assembly", "inspection")
}

doe_known_columns <- function() {
  stations <- doe_station_names()
  c(
    "design_id",
    # Run horizon / admission
    "warmup_min", "shift_length_min", "n_shifts", "arrival_mean_min",
    "release_policy", "entry_buffer", "wip_limit", "order_spacing",
    "repair_technicians",
    # Operator counts (zones by default; op_shared switches to one shared team)
    "op_upstream", "op_downstream", "op_shared",
    paste0("op_", stations),
    # Bay capacities and automatic means
    paste0("cap_", stations),
    paste0("mean_", stations),
    # Interstation buffers (no buffer before the first station)
    "buffer_paint", "buffer_final_assembly", "buffer_inspection"
  )
}

doe_default_design_row <- function() {
  p <- default_params() %>% normalize_params()
  stations <- p$stations
  row <- list(
    design_id = "baseline",
    warmup_min = p$warmup_min,
    shift_length_min = p$shift_length_min,
    n_shifts = as.integer(p$n_shifts),
    arrival_mean_min = p$arrival_mean_min,
    release_policy = p$release_policy,
    entry_buffer = as.integer(p$entry_buffer),
    wip_limit = as.integer(p$wip_limit),
    order_spacing = p$order_spacing,
    repair_technicians = as.integer(p$repair_technicians),
    op_upstream = as.integer(p$operators$count[p$operators$pool == "upstream"]),
    op_downstream = as.integer(p$operators$count[p$operators$pool == "downstream"])
  )
  for (i in seq_len(nrow(stations))) {
    nm <- stations$name[[i]]
    row[[paste0("cap_", nm)]] <- as.integer(stations$capacity[[i]])
    row[[paste0("mean_", nm)]] <- as.numeric(stations$mean_min[[i]])
  }
  row$buffer_paint <- as.integer(stations$buffer_before[stations$name == "paint"])
  row$buffer_final_assembly <- as.integer(stations$buffer_before[stations$name == "final_assembly"])
  row$buffer_inspection <- as.integer(stations$buffer_before[stations$name == "inspection"])
  as_tibble(row)
}

doe_validate_design <- function(design) {
  stopifnot(is.data.frame(design), nrow(design) > 0)
  if (!"design_id" %in% names(design)) {
    stop("Design CSV must include a design_id column.", call. = FALSE)
  }
  if (anyDuplicated(design$design_id)) {
    dups <- unique(design$design_id[duplicated(design$design_id)])
    stop("design_id values must be unique. Duplicates: ",
         paste(dups, collapse = ", "), call. = FALSE)
  }
  if (any(is.na(design$design_id) | !nzchar(as.character(design$design_id)))) {
    stop("design_id must be non-empty for every row.", call. = FALSE)
  }
  known <- doe_known_columns()
  unknown <- setdiff(names(design), known)
  if (length(unknown)) {
    stop(
      "Unknown design column(s): ", paste(unknown, collapse = ", "),
      "\nKnown columns:\n  ", paste(known, collapse = ", "),
      call. = FALSE
    )
  }
  design %>% mutate(design_id = as.character(design_id))
}

# Overlay one design row onto a copy of default_params().
doe_params_from_row <- function(row, n_replications = 1L, base_seed = 1L) {
  p <- default_params()
  getv <- function(name, default = NULL) {
    if (!name %in% names(row) || is.null(row[[name]]) || (length(row[[name]]) == 1 && is.na(row[[name]]))) {
      return(default)
    }
    row[[name]]
  }

  if (!is.null(getv("warmup_min"))) p$warmup_min <- as.numeric(getv("warmup_min"))
  if (!is.null(getv("shift_length_min"))) p$shift_length_min <- as.numeric(getv("shift_length_min"))
  if (!is.null(getv("n_shifts"))) p$n_shifts <- as.integer(getv("n_shifts"))
  if (!is.null(getv("arrival_mean_min"))) p$arrival_mean_min <- as.numeric(getv("arrival_mean_min"))
  if (!is.null(getv("release_policy"))) p$release_policy <- as.character(getv("release_policy"))
  if (!is.null(getv("entry_buffer"))) p$entry_buffer <- as.integer(getv("entry_buffer"))
  if (!is.null(getv("wip_limit"))) p$wip_limit <- as.integer(getv("wip_limit"))
  if (!is.null(getv("order_spacing"))) p$order_spacing <- as.character(getv("order_spacing"))
  if (!is.null(getv("repair_technicians"))) p$repair_technicians <- as.integer(getv("repair_technicians"))

  # Operators: dedicated per station > shared team > zones
  dedicated_cols <- paste0("op_", doe_station_names())
  has_dedicated <- any(vapply(dedicated_cols, function(n) !is.null(getv(n)), logical(1)))
  if (has_dedicated) {
    pools <- character(); counts <- integer()
    for (st in doe_station_names()) {
      col <- paste0("op_", st)
      val <- getv(col)
      if (!is.null(val) && as.integer(val) > 0) {
        pools <- c(pools, st)
        counts <- c(counts, as.integer(val))
        p$stations$operator_pool[p$stations$name == st] <- st
      }
    }
    p$operators <- tibble(pool = pools, count = counts)
  } else if (!is.null(getv("op_shared"))) {
    n <- as.integer(getv("op_shared"))
    p$operators <- tibble(pool = "shared", count = n)
    p$stations$operator_pool <- "shared"
  } else {
    up <- getv("op_upstream", p$operators$count[p$operators$pool == "upstream"])
    down <- getv("op_downstream", p$operators$count[p$operators$pool == "downstream"])
    p$operators <- tibble(pool = c("upstream", "downstream"),
                          count = c(as.integer(up), as.integer(down)))
    p$stations$operator_pool <- ifelse(
      p$stations$name %in% c("body_shop", "paint"), "upstream", "downstream"
    )
  }

  for (st in doe_station_names()) {
    cap <- getv(paste0("cap_", st))
    if (!is.null(cap)) p$stations$capacity[p$stations$name == st] <- as.integer(cap)
    mu <- getv(paste0("mean_", st))
    if (!is.null(mu)) p$stations$mean_min[p$stations$name == st] <- as.numeric(mu)
  }
  for (st in c("paint", "final_assembly", "inspection")) {
    buf <- getv(paste0("buffer_", st))
    if (!is.null(buf)) p$stations$buffer_before[p$stations$name == st] <- as.numeric(buf)
  }

  p$n_replications <- as.integer(n_replications)
  p$base_seed <- as.integer(base_seed)
  p$time_mode <- "shifts"
  # Force timing list-columns to rebuild from (possibly updated) scalars
  p$stations$load_timing <- NULL
  p$stations$auto_timing <- NULL
  p$stations$unload_timing <- NULL
  p$stations$task_timing <- NULL
  p
}

# Reproducible seed for design row index × replication.
