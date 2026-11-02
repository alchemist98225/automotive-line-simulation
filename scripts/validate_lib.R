# validate_lib.R
# Analytic checks for the portfolio: M/M/c (Erlang-C) vs a standalone simmer
# trajectory, and Little's Law on the full automotive line model.
# Does not modify app/R/model.R. Sourced by scripts/validate_run.R.

suppressPackageStartupMessages({
  library(tidyverse)
  library(simmer)
})

.val_lib_dir <- local({
  if (exists("script_dir", inherits = TRUE)) {
    sd <- get("script_dir", inherits = TRUE)
    if (is.character(sd) && length(sd) == 1L && dir.exists(sd)) return(sd)
  }
  # Walk callers for source() ofile
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile) && nzchar(ofile)) {
      d <- dirname(normalizePath(ofile, mustWork = FALSE))
      if (file.exists(file.path(d, "doe_lib.R"))) return(d)
    }
  }
  if (file.exists("scripts/doe_lib.R")) return(normalizePath("scripts"))
  if (file.exists("doe_lib.R")) return(normalizePath("."))
  stop("Could not locate scripts/ for validate_lib.R")
})
source(file.path(.val_lib_dir, "doe_lib.R"), local = FALSE)

# future::run masks simmer::run
val_ensure_simmer_run <- function() {
  assign("run", simmer::run, envir = .GlobalEnv)
  invisible(TRUE)
}

# Same integral as app/R/model.R::resource_stats (kept local so M/M/c
# does not require sourcing the full automotive engine).
val_resource_stats <- function(res, warmup, horizon) {
  res %>%
    group_by(resource) %>%
    group_modify(~{
      .x %>%
        arrange(time) %>%
        mutate(dt = pmax(0, pmin(lead(time, default = horizon), horizon) - pmax(time, warmup))) %>%
        summarise(
          avg_util = sum(if_else(capacity > 0, server / capacity, 0) * dt) / (horizon - warmup),
          avg_queue = sum(queue * dt) / (horizon - warmup),
          avg_server = sum(server * dt) / (horizon - warmup)
        )
    }) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Erlang-C analytics for M/M/c
# ---------------------------------------------------------------------------
# a = λ/μ (offered load in Erlangs), c servers, ρ = a/c < 1.
erlang_c <- function(lambda, mu, c) {
  stopifnot(lambda > 0, mu > 0, c >= 1L)
  a <- lambda / mu
  rho <- a / c
  if (rho >= 1) {
    return(list(rho = rho, a = a, P0 = NA_real_, C = NA_real_,
                Wq = Inf, Lq = Inf, W = Inf, L = Inf, util = rho))
  }
  # P0 via log-sum for stability at moderate c
  terms <- map_dbl(0:(c - 1L), function(n) a^n / factorial(n))
  last <- (a^c / factorial(c)) / (1 - rho)
  P0 <- 1 / (sum(terms) + last)
  C <- last * P0
  Wq <- C / (c * mu - lambda)          # time units of 1/μ
  Lq <- lambda * Wq
  W <- Wq + 1 / mu
  L <- lambda * W
  list(rho = rho, a = a, P0 = P0, C = C, Wq = Wq, Lq = Lq, W = W, L = L, util = rho)
}

# ---------------------------------------------------------------------------
# Standalone simmer M/M/c (not the automotive line)
# ---------------------------------------------------------------------------
mm_c_one_rep <- function(lambda, mu, c, horizon, warmup, seed) {
  val_ensure_simmer_run()
  set.seed(seed)
  env <- simmer("mmc")
  traj <- trajectory("cust") %>%
    seize("server", 1) %>%
    timeout(function() rexp(1, mu)) %>%
    release("server", 1)
  env <- env %>%
    add_resource("server", capacity = as.integer(c), queue_size = Inf) %>%
    add_generator("cust", traj, function() rexp(1, lambda))
  env %>% run(until = horizon)

  arr <- get_mon_arrivals(env) %>% as_tibble()
  # Sojourn / wait after warmup (departures in window)
  done <- arr %>%
    filter(finished, end_time >= warmup, end_time < horizon) %>%
    mutate(
      W = end_time - start_time,
      service = activity_time,
      Wq = pmax(0, W - service)
    )
  # Include t=0 state so the integral covers the warmup boundary.
  initial <- tibble(
    resource = "server", time = 0, server = 0L, queue = 0L,
    capacity = as.integer(c), queue_size = Inf, system = 0L, limit = Inf
  )
  res <- bind_rows(initial, get_mon_resources(env) %>% as_tibble())
  stats <- val_resource_stats(res, warmup, horizon) %>% filter(resource == "server")
  tibble(
    n_completed = nrow(done),
    Wq_mean = if (nrow(done)) mean(done$Wq) else NA_real_,
    W_mean = if (nrow(done)) mean(done$W) else NA_real_,
    Lq = stats$avg_queue[[1]],
    util = stats$avg_util[[1]],
    L = stats$avg_queue[[1]] + stats$avg_server[[1]]
  )
}

mm_c_run_case <- function(lambda, mu, c, n_reps = 40L, horizon = 20000,
                         warmup = 2000, base_seed = 100L, workers = 1L) {
  analytic <- erlang_c(lambda, mu, c)
  seeds <- as.integer(base_seed + seq_len(n_reps) - 1L)
  if (!requireNamespace("furrr", quietly = TRUE)) stop("furrr required")
  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  if (workers <= 1L) future::plan(future::sequential) else
    future::plan(future::multisession, workers = workers)
  val_ensure_simmer_run()
  opts <- furrr::furrr_options(seed = NULL, packages = c("simmer", "tidyverse"))
  reps <- furrr::future_map_dfr(
    seeds,
    function(s) {
      mm_c_one_rep(lambda, mu, c, horizon, warmup, s) %>%
        mutate(seed = s, lambda = lambda, mu = mu, c = c)
    },
    .options = opts
  )
  summarise_ci <- function(x) {
    x <- x[is.finite(x)]
    n <- length(x)
    m <- mean(x)
    se <- if (n >= 2) sd(x) / sqrt(n) else NA_real_
    half <- if (n >= 2) qt(0.975, n - 1) * se else NA_real_
    c(mean = m, lo = m - half, hi = m + half, n = n)
  }
  metrics <- c("Wq_mean", "Lq", "util", "W_mean", "L")
  sim_sum <- map_dfr(metrics, function(m) {
    s <- summarise_ci(reps[[m]])
    tibble(metric = m, sim_mean = s[["mean"]], sim_lo = s[["lo"]], sim_hi = s[["hi"]],
           n_reps = as.integer(s[["n"]]))
  })
  theory <- analytic  # keep list; column below is also named carefully
  analytic_tbl <- tibble(
    metric = c("Wq_mean", "Lq", "util", "W_mean", "L"),
    analytic_value = c(theory$Wq, theory$Lq, theory$util, theory$W, theory$L)
  )
  summary <- sim_sum %>%
    left_join(analytic_tbl, by = "metric") %>%
    mutate(
      lambda = lambda, mu = mu, c = as.integer(c), rho = theory$rho,
      abs_err = sim_mean - analytic_value,
      rel_err = if_else(abs(analytic_value) > 1e-12, abs_err / analytic_value, NA_real_),
      covers = sim_lo <= analytic_value & analytic_value <= sim_hi
    )
  list(reps = reps, summary = summary, analytic = theory)
}

# ---------------------------------------------------------------------------
# Little's Law on the full line: L ≈ λ_out * W
# λ_out = throughput_per_hr / 60 (veh/min), W = cycle_time_mean (min)
# ---------------------------------------------------------------------------
littles_scenarios <- function() {
  tidyr::crossing(
    setup = c("default", "guided_2"),
    arrival_mean_min = c(8, 6, 5, 4)
  ) %>%
    mutate(
      design_id = paste(setup, paste0("iat", arrival_mean_min), sep = "__"),
      cap_paint = if_else(setup == "guided_2", 2L, 1L),
      offered_demand_per_hr = 60 / arrival_mean_min
    )
}

littles_run <- function(scenarios = littles_scenarios(), n_reps = 12L,
                        base_seed = 20261027L, workers = 7L) {
  doe_source_model(doe_repo_root(.val_lib_dir))
  val_ensure_simmer_run()
  scenarios <- scenarios %>% mutate(row_index = row_number())
  if (!requireNamespace("furrr", quietly = TRUE)) stop("furrr required")
  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  if (workers <= 1L) future::plan(future::sequential) else
    future::plan(future::multisession, workers = workers)
  val_ensure_simmer_run()
  opts <- furrr::furrr_options(seed = NULL, packages = c("simmer", "tidyverse"))

  run_one <- function(row) {
    val_ensure_simmer_run()
    design_row <- tibble(
      design_id = row$design_id,
      arrival_mean_min = row$arrival_mean_min,
      cap_paint = row$cap_paint
    )
    # CRN across setups at same arrival: seed by arrival index
    point_seed <- doe_rep_seed(base_seed, as.integer(row$row_index), 1L, n_reps)
    params <- doe_params_from_row(design_row, n_replications = n_reps, base_seed = point_seed)
    result <- run_line_sim(params)
    result$summaries %>%
      transmute(
        design_id = row$design_id,
        setup = row$setup,
        arrival_mean_min = row$arrival_mean_min,
        offered_demand_per_hr = row$offered_demand_per_hr,
        cap_paint = row$cap_paint,
        replication,
        seed,
        throughput_per_hr,
        cycle_time_mean,
        avg_wip,
        # Little: L_pred = λ * W with λ in /min, W in min
        lambda_per_min = throughput_per_hr / 60,
        L_pred = lambda_per_min * cycle_time_mean,
        L_obs = avg_wip,
        rel_err = (L_obs - L_pred) / L_pred
      )
  }

  rows <- split(scenarios, seq_len(nrow(scenarios)))
  bind_rows(furrr::future_map(rows, function(r) run_one(r[1, ]), .options = opts))
}

littles_summarise <- function(rep_tbl) {
  rep_tbl %>%
    group_by(design_id, setup, arrival_mean_min, offered_demand_per_hr, cap_paint) %>%
    summarise(
      n_reps = n(),
      L_obs_mean = mean(L_obs, na.rm = TRUE),
      L_pred_mean = mean(L_pred, na.rm = TRUE),
      thr_mean = mean(throughput_per_hr, na.rm = TRUE),
      W_mean = mean(cycle_time_mean, na.rm = TRUE),
      rel_err_mean = mean(rel_err, na.rm = TRUE),
      rel_err_abs_mean = mean(abs(rel_err), na.rm = TRUE),
      .groups = "drop"
    )
}
