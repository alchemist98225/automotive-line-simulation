# warmup_lib.R
# Welch's method: ensemble-average WIP from t=0, moving-window smooth,
# compare to the app's current settling period (warmup_min = 300).
# Does not modify app/R/model.R.

suppressPackageStartupMessages({
  library(tidyverse)
  library(simmer)
})

.wu_lib_dir <- local({
  if (exists("script_dir", inherits = TRUE)) {
    sd <- get("script_dir", inherits = TRUE)
    if (is.character(sd) && length(sd) == 1L && dir.exists(sd)) return(sd)
  }
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  normalizePath("scripts")
})
source(file.path(.wu_lib_dir, "doe_lib.R"), local = FALSE)

wu_ensure_simmer_run <- function() {
  assign("run", simmer::run, envir = .GlobalEnv)
  invisible(TRUE)
}

wu_setups <- function() {
  tibble::tribble(
    ~setup_id, ~setup_label, ~cap_paint,
    "default", "Default line", 1L,
    "guided_2", "2-bay balanced", 2L
  )
}

# WIP count from t=0 on a fine grid (includes settling period).
wu_wip_from_arrivals <- function(arrivals, horizon, dt = 2) {
  grid <- seq(0, horizon, by = dt)
  if (!nrow(arrivals)) {
    return(tibble(time_min = grid, wip = 0))
  }
  a <- arrivals %>%
    mutate(end = if_else(finished, end_time, horizon))
  wip <- map_dbl(grid, function(t) {
    sum(a$start_time <= t & a$end > t)
  })
  tibble(time_min = grid, wip = wip)
}

# One replication with warmup_min=0 so observation includes start-up;
# we still run the usual horizon length (warmup+shift of defaults).
wu_one_rep <- function(setup_id, cap_paint, seed, arrival_mean_min = 4,
                       sim_length_min = 780, dt = 2) {
  wu_ensure_simmer_run()
  design_row <- tibble(
    design_id = paste(setup_id, seed, sep = "_"),
    arrival_mean_min = arrival_mean_min,
    cap_paint = as.integer(cap_paint),
    warmup_min = 0,
    shift_length_min = sim_length_min,
    n_shifts = 1L
  )
  params <- doe_params_from_row(design_row, n_replications = 1L, base_seed = as.integer(seed))
  # Force legacy-style continuous horizon with no internal warmup cut for series
  params$warmup_min <- 0
  params$time_mode <- "legacy"
  params$sim_length_min <- sim_length_min
  result <- run_line_sim(params)
  arr <- result$arrivals
  series <- wu_wip_from_arrivals(arr, sim_length_min, dt = dt) %>%
    mutate(setup_id = setup_id, seed = as.integer(seed), replication = 1L)
  series
}

wu_run_ensemble <- function(n_reps = 30L, base_seed = 42L, workers = 7L,
                            sim_length_min = 780, dt = 2,
                            arrival_mean_min = 4) {
  doe_source_model(doe_repo_root(.wu_lib_dir))
  wu_ensure_simmer_run()
  setups <- wu_setups()
  jobs <- tidyr::crossing(setups, rep = seq_len(n_reps)) %>%
    mutate(seed = as.integer(base_seed + rep - 1L))

  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  if (workers <= 1L) future::plan(future::sequential) else
    future::plan(future::multisession, workers = workers)
  wu_ensure_simmer_run()
  opts <- furrr::furrr_options(seed = NULL, packages = c("simmer", "tidyverse"))

  rows <- split(jobs, seq_len(nrow(jobs)))
  bind_rows(furrr::future_map(rows, function(r) {
    r <- r[1, ]
    wu_one_rep(r$setup_id, r$cap_paint, r$seed,
               arrival_mean_min = arrival_mean_min,
               sim_length_min = sim_length_min, dt = dt) %>%
      mutate(setup_label = r$setup_label)
  }, .options = opts, .progress = TRUE))
}

# Welch moving-window average of ensemble mean
wu_welch_smooth <- function(ensemble_tbl, window = 31L) {
  # ensemble_tbl: time_min, setup_id, wip (many reps)
  mean_ts <- ensemble_tbl %>%
    group_by(setup_id, setup_label, time_min) %>%
    summarise(
      wip_mean = mean(wip, na.rm = TRUE),
      wip_lo = {
        x <- wip
        if (length(x) < 2) NA_real_ else mean(x) - qt(0.975, length(x)-1)*sd(x)/sqrt(length(x))
      },
      wip_hi = {
        x <- wip
        if (length(x) < 2) NA_real_ else mean(x) + qt(0.975, length(x)-1)*sd(x)/sqrt(length(x))
      },
      n_reps = n(),
      .groups = "drop"
    ) %>%
    arrange(setup_id, time_min)

  # Odd window moving average (Welch)
  k <- as.integer(window)
  if (k %% 2L == 0L) k <- k + 1L
  half <- (k - 1L) %/% 2L
  mean_ts %>%
    group_by(setup_id, setup_label) %>%
    group_modify(function(d, ...) {
      n <- nrow(d)
      roll <- function(y) {
        map_dbl(seq_len(n), function(i) {
          lo <- max(1L, i - half); hi <- min(n, i + half)
          mean(y[lo:hi], na.rm = TRUE)
        })
      }
      # Same Welch window on mean and CI bounds so the ribbon is a smooth band.
      d %>% mutate(
        wip_smooth = roll(wip_mean),
        wip_lo_smooth = roll(wip_lo),
        wip_hi_smooth = roll(wip_hi),
        welch_window = k
      )
    }) %>%
    ungroup()
}

# Recommend warm-up: first time smoothed WIP stays within band of end-window mean
wu_recommend <- function(smooth_tbl, current_warmup = 300,
                         settle_frac = 0.05, end_window_min = 120,
                         hold_min = 60) {
  smooth_tbl %>%
    group_by(setup_id, setup_label) %>%
    group_modify(function(d, ...) {
      tmax <- max(d$time_min)
      end_mean <- d %>% filter(time_min >= tmax - end_window_min) %>%
        summarise(m = mean(wip_smooth, na.rm = TRUE)) %>% pull(m)
      band <- max(abs(end_mean) * settle_frac, 0.35)
      ok <- abs(d$wip_smooth - end_mean) <= band
      dt <- if (nrow(d) > 1) median(diff(d$time_min)) else 2
      hold_n <- max(1L, as.integer(round(hold_min / dt)))
      recommend <- NA_real_
      for (i in seq_len(nrow(d) - hold_n + 1L)) {
        if (all(ok[i:(i + hold_n - 1L)])) { recommend <- d$time_min[[i]]; break }
      }
      # Fallback: first time within band even if later noise returns
      if (!is.finite(recommend)) {
        hit <- which(ok)
        if (length(hit)) recommend <- d$time_min[[hit[[1]]]]
      }
      tibble(
        current_warmup_min = current_warmup,
        steady_wip = end_mean,
        recommend_warmup_min = recommend,
        adequate = is.finite(recommend) && recommend <= current_warmup,
        settle_frac = settle_frac,
        hold_min = hold_min
      )
    }) %>%
    ungroup()
}
