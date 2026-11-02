# tornado_lib.R
# One-at-a-time sensitivity around the 2-bay balanced baseline at ~10 veh/hr.
# CRN across low/high of each factor; reports HPU and throughput effects.
# Does not modify app/R/model.R.

suppressPackageStartupMessages({ library(tidyverse) })

.tor_lib_dir <- local({
  if (exists("script_dir", inherits = TRUE)) {
    sd <- get("script_dir", inherits = TRUE)
    if (is.character(sd) && length(sd) == 1L && dir.exists(sd)) return(sd)
  }
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  normalizePath("scripts")
})
source(file.path(.tor_lib_dir, "doe_lib.R"), local = FALSE)

tor_ensure_simmer_run <- function() {
  assign("run", simmer::run, envir = .GlobalEnv)
  invisible(TRUE)
}

# Baseline: guided 2-bay at demand 10/hr (iat = 6 min)
tor_baseline_row <- function() {
  tibble(
    design_id = "baseline",
    arrival_mean_min = 6,
    cap_paint = 2L,
    # guided defaults already match default_params buffers/ops; paint cap 2
    buffer_paint = 3L,
    buffer_final_assembly = 3L,
    buffer_inspection = 3L,
    op_upstream = 2L,
    op_downstream = 2L,
    mean_body_shop = 6,
    mean_paint = 8,
    mean_final_assembly = 7,
    mean_inspection = 3
  )
}

# Factor definitions: name, column or special handler, low, high, label
tor_factor_defs <- function() {
  tibble::tribble(
    ~factor,              ~kind,     ~low,  ~high, ~label,
    "demand",             "demand",  8.0,  12.0,  "Demand (veh/hr)",
    "mean_body_shop",     "col",     4.8,   7.2,  "Body cycle time",
    "mean_paint",         "col",     6.4,   9.6,  "Paint cycle time",
    "mean_final_assembly","col",     5.6,   8.4,  "Assembly cycle time",
    "mean_inspection",    "col",     2.4,   3.6,  "Inspection cycle time",
    "cv_scale",           "cv",      0.5,   1.5,  "Cycle-time CV scale",
    "mtbf_scale",         "mtbf",    0.7,   1.5,  "MTBF scale",
    "mttr_scale",         "mttr",    0.7,   1.5,  "MTTR scale",
    "buffer_paint",       "col",     1L,    5L,   "Queue before paint",
    "buffer_final_assembly","col",   1L,    5L,   "Queue before assembly",
    "op_upstream",        "col",     1L,    3L,   "Upstream operators",
    "op_downstream",      "col",     1L,    3L,   "Downstream operators",
    "cap_paint",          "col",     1L,    3L,   "Paint capacity"
  )
}

tor_apply_factor <- function(base_row, factor_row, level = c("low", "high", "base")) {
  level <- match.arg(level)
  row <- base_row
  row$design_id <- paste(factor_row$factor, level, sep = "__")
  if (identical(level, "base")) return(row)
  val <- if (level == "low") factor_row$low else factor_row$high
  if (factor_row$kind == "demand") {
    # store as veh/hr in a helper col; arrival from demand
    row$arrival_mean_min <- 60 / as.numeric(val)
  } else if (factor_row$kind == "col") {
    row[[factor_row$factor]] <- val
  } else if (factor_row$kind %in% c("cv", "mtbf", "mttr")) {
    row[[paste0(".", factor_row$kind)]] <- as.numeric(val)
  }
  row
}

# Build params from row, applying cv/mtbf/mttr scales onto default_params stations
tor_params_from_row <- function(row, n_reps, base_seed) {
  design_cols <- setdiff(names(row), c(".cv", ".mtbf", ".mttr"))
  design_row <- row[, intersect(doe_known_columns(), design_cols), drop = FALSE]
  # ensure design_id
  if (!"design_id" %in% names(design_row)) design_row$design_id <- row$design_id
  params <- doe_params_from_row(as_tibble(design_row), n_replications = n_reps, base_seed = base_seed)
  get_scale <- function(nm) {
    if (!nm %in% names(row)) return(NA_real_)
    v <- row[[nm]][[1]]
    if (is.null(v) || !is.finite(v)) NA_real_ else as.numeric(v)
  }
  cv <- get_scale(".cv")
  if (is.finite(cv)) {
    params$stations <- params$stations %>%
      mutate(sd_min = if_else(is.finite(sd_min) & !is.na(sd_min), sd_min * cv, sd_min))
  }
  mtbf <- get_scale(".mtbf")
  if (is.finite(mtbf)) {
    params$stations <- params$stations %>%
      mutate(mtbf_min = if_else(is.finite(mtbf_min), mtbf_min * mtbf, mtbf_min))
  }
  mttr <- get_scale(".mttr")
  if (is.finite(mttr)) {
    params$stations <- params$stations %>%
      mutate(mttr_min = if_else(is.finite(mttr_min) & mttr_min > 0, mttr_min * mttr, mttr_min))
  }
  params
}

tor_eval_row <- function(row, n_reps, base_seed, factor_index) {
  tor_ensure_simmer_run()
  # CRN: same seed stream for low/high/base of a factor uses factor_index
  point_seed <- doe_rep_seed(base_seed, as.integer(factor_index), 1L, n_reps)
  params <- tor_params_from_row(row, n_reps, point_seed)
  result <- run_line_sim(params)
  n_ops <- sum(params$operators$count)
  obs_h <- (params$sim_length_min - params$warmup_min) / 60
  result$summaries %>%
    transmute(
      design_id = row$design_id[[1]],
      replication, seed,
      throughput_per_hr,
      cycle_time_mean,
      avg_wip,
      hpu = if_else(n_completed > 0, n_ops * obs_h / n_completed, Inf)
    )
}

tor_run <- function(n_reps = 10L, base_seed = 42L, workers = 7L) {
  doe_source_model(doe_repo_root(.tor_lib_dir))
  tor_ensure_simmer_run()
  base <- tor_baseline_row()
  factors <- tor_factor_defs() %>% mutate(factor_index = row_number())

  # Jobs: baseline once + each factor × {low, high}
  base_job <- tibble(factor = "baseline", level = "base", factor_index = 0L,
                     label = "Baseline", kind = "base")
  jobs <- factors %>%
    select(factor, label, kind, factor_index, low, high) %>%
    tidyr::crossing(level = c("low", "high")) %>%
    bind_rows(base_job) %>%
    mutate(job_id = row_number())

  make_row <- function(job) {
    if (job$factor == "baseline") return(base)
    f <- factors %>% filter(factor == job$factor)
    tor_apply_factor(base, f[1, ], level = job$level)
  }

  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  if (workers <= 1L) future::plan(future::sequential) else
    future::plan(future::multisession, workers = workers)
  tor_ensure_simmer_run()
  opts <- furrr::furrr_options(seed = NULL, packages = c("simmer", "tidyverse"))

  job_list <- split(jobs, seq_len(nrow(jobs)))
  rep_metrics <- bind_rows(furrr::future_map(job_list, function(j) {
    j <- j[1, ]
    row <- make_row(j)
    tor_eval_row(row, n_reps, base_seed, max(1L, as.integer(j$factor_index))) %>%
      mutate(factor = j$factor, level = j$level, label = j$label, kind = j$kind)
  }, .options = opts, .progress = TRUE))

  list(rep_metrics = rep_metrics, jobs = jobs, factors = factors)
}

tor_summarise <- function(rep_metrics) {
  rep_metrics %>%
    mutate(hpu = if_else(is.infinite(hpu), NA_real_, hpu)) %>%
    group_by(factor, level, label, kind) %>%
    summarise(
      n_reps = n(),
      thr_mean = mean(throughput_per_hr, na.rm = TRUE),
      thr_sd = sd(throughput_per_hr, na.rm = TRUE),
      hpu_mean = mean(hpu, na.rm = TRUE),
      hpu_sd = sd(hpu, na.rm = TRUE),
      .groups = "drop"
    )
}

tor_effects <- function(summary_tbl) {
  base <- summary_tbl %>% filter(factor == "baseline")
  stopifnot(nrow(base) == 1L)
  summary_tbl %>%
    filter(factor != "baseline") %>%
    select(factor, level, label, thr_mean, hpu_mean) %>%
    tidyr::pivot_wider(names_from = level, values_from = c(thr_mean, hpu_mean)) %>%
    mutate(
      thr_low = thr_mean_low, thr_high = thr_mean_high,
      hpu_low = hpu_mean_low, hpu_high = hpu_mean_high,
      thr_base = base$thr_mean[[1]],
      hpu_base = base$hpu_mean[[1]],
      thr_span = thr_high - thr_low,
      hpu_span = hpu_high - hpu_low,
      thr_abs_span = abs(thr_span),
      hpu_abs_span = abs(hpu_span)
    ) %>%
    arrange(desc(hpu_abs_span))
}
