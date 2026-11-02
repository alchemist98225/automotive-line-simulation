# optimize_ga.R
# Tiny integer genetic algorithm (tidyverse/purrr). No GA package dependency.
# Seeds stay fixed for a genome within search (cand_index-derived); only elite
# re-eval refreshes to the shared CRN schedule base_seed..(base_seed+n-1).
# Sourced by opt_lib.R.

opt_tournament <- function(fitness, k = 3L) {
  idx <- sample.int(length(fitness), size = min(k, length(fitness)))
  idx[[which.min(fitness[idx])]]
}

opt_crossover <- function(a, b) {
  stopifnot(length(a) == length(b))
  if (length(a) < 2L) return(a)
  cut <- sample.int(length(a) - 1L, 1L)
  c(a[seq_len(cut)], b[seq(cut + 1L, length(b))])
}

opt_mutate <- function(genome, defs = opt_knob_defs(), p_mut = 0.25) {
  genome <- opt_clip_genome(genome, defs)
  original <- genome
  for (i in seq_along(genome)) {
    if (runif(1) < p_mut) {
      choices <- seq.int(defs$lo[[i]], defs$hi[[i]])
      choices <- choices[choices != genome[[i]]]
      if (length(choices)) genome[[i]] <- as.integer(sample(choices, 1L))
    }
  }
  if (identical(as.integer(genome), as.integer(original))) {
    i <- sample.int(length(genome), 1L)
    choices <- seq.int(defs$lo[[i]], defs$hi[[i]])
    choices <- choices[choices != original[[i]]]
    if (length(choices)) genome[[i]] <- as.integer(sample(choices, 1L))
  }
  opt_clip_genome(genome, defs)
}

opt_mean_pairwise_distance <- function(genomes) {
  n <- length(genomes)
  if (n < 2L) return(0)
  total <- 0
  count <- 0L
  for (i in seq_len(n - 1L)) {
    for (j in seq.int(i + 1L, n)) {
      total <- total + sum(abs(genomes[[i]] - genomes[[j]]))
      count <- count + 1L
    }
  }
  total / count
}

opt_map_eval <- function(genomes, eval_fn, cores = 1L) {
  cores <- max(1L, as.integer(cores))
  if (cores == 1L || length(genomes) <= 1L) {
    return(lapply(genomes, eval_fn))
  }
  parallel::mclapply(genomes, eval_fn, mc.cores = min(cores, length(genomes)), mc.preschedule = FALSE)
}

opt_run_ga <- function(pop_size = 24L,
                       n_gen = 15L,
                       n_reps = 3L,
                       elite_reps = 15L,
                       n_elites = 5L,
                       base_seed = 42L,
                       penalty = 10,
                       p_mut = 0.25,
                       tournament_k = 3L,
                       cores = 1L,
                       capacity_cost_h = opt_default_capacity_cost_h,
                       queue_cost_h = opt_default_queue_cost_h,
                       cache = opt_eval_cache_new(),
                       defs = opt_knob_defs(),
                       verbose = TRUE) {
  pop_size <- as.integer(pop_size)
  n_gen <- as.integer(n_gen)
  n_reps <- as.integer(n_reps)
  elite_reps <- as.integer(elite_reps)
  n_elites <- as.integer(n_elites)
  capacity_cost_h <- as.numeric(capacity_cost_h)
  queue_cost_h <- as.numeric(queue_cost_h)
  baseline_genome <- opt_default_genome(defs)

  if (verbose) {
    cat(sprintf(
      "Baseline (guided Experiment, paint capacity 2): %d reps, seed=%d\n",
      n_reps, base_seed
    ), file = stderr())
    cat(sprintf(
      "Cost rates (illustrative): capacity_cost_h=%.3g, queue_cost_h=%.3g\n",
      capacity_cost_h, queue_cost_h
    ), file = stderr())
  }
  baseline <- opt_baseline_metrics(
    n_reps = n_reps, base_seed = base_seed, defs = defs,
    capacity_cost_h = capacity_cost_h, queue_cost_h = queue_cost_h
  )
  thr_floor <- baseline$thr_mean
  if (verbose) {
    cat(sprintf(
      "Baseline HPU=%.4f thr=%.3f/hr cost_hpu=%.4f fitness=%.4f\n",
      baseline$hpu_mean, thr_floor, baseline$cost_hpu, baseline$fitness
    ), file = stderr())
  }

  # Stable cand_index from the genome itself (fork-safe). Seeds stay fixed
  # for a given genome across generations (no mid-search CRN refresh).
  assign_index <- function(genome) {
    genome <- opt_clip_genome(genome, defs)
    if (identical(opt_genome_key(genome), opt_genome_key(baseline_genome))) return(1L)
    weights <- as.integer(cumprod(c(1L, defs$hi - defs$lo + 1L)))[seq_along(genome)]
    as.integer(2L + sum((genome - defs$lo) * weights))
  }

  eval_one <- function(genome, reps) {
    opt_evaluate_genome(
      genome = genome,
      n_reps = reps,
      base_seed = base_seed,
      cand_index = assign_index(genome),
      thr_floor = thr_floor,
      penalty = penalty,
      cache = cache,
      defs = defs,
      baseline_genome = baseline_genome,
      capacity_cost_h = capacity_cost_h,
      queue_cost_h = queue_cost_h
    )
  }

  # All-random initial population (no seeded baseline). Seeding the guided
  # Experiment made gen-0 best = baseline and often a single early jump to a
  # near-optimum staffing cut, then a flat best line. Random start lets best
  # improve across generations while thr_floor still comes from the baseline eval.
  pop <- lapply(seq_len(pop_size), function(i) opt_random_genome(defs))

  history <- list()
  all_evals <- list()

  for (gen in seq_len(n_gen)) {
    t0 <- proc.time()[["elapsed"]]
    evaluated <- opt_map_eval(pop, function(g) eval_one(g, n_reps), cores = cores)
    fitness <- vapply(evaluated, `[[`, numeric(1), "fitness")
    hpu <- vapply(evaluated, `[[`, numeric(1), "hpu_mean")
    cost <- vapply(evaluated, `[[`, numeric(1), "cost_hpu")
    thr <- vapply(evaluated, `[[`, numeric(1), "thr_mean")
    feasible <- vapply(evaluated, `[[`, logical(1), "feasible")

    keys <- vapply(evaluated, function(ev) opt_genome_key(ev$genome), character(1))
    n_unique <- length(unique(keys))
    mean_l1 <- opt_mean_pairwise_distance(lapply(evaluated, `[[`, "genome"))
    mean_fit_feas <- if (any(feasible)) mean(fitness[feasible]) else NA_real_
    history[[gen]] <- tibble(
      generation = gen - 1L,
      best_fitness = min(fitness),
      mean_fitness = mean(fitness),
      mean_fitness_feasible = mean_fit_feas,
      best_hpu = hpu[[which.min(fitness)]],
      best_cost_hpu = cost[[which.min(fitness)]],
      mean_hpu = mean(hpu),
      mean_cost_hpu = mean(cost),
      best_thr = thr[[which.min(fitness)]],
      n_feasible = sum(feasible),
      pct_feasible = 100 * mean(feasible),
      n_unique = n_unique,
      mean_pairwise_l1 = mean_l1,
      wall_sec = proc.time()[["elapsed"]] - t0
    )
    all_evals[[gen]] <- map2_dfr(evaluated, seq_along(evaluated), function(ev, i) {
      tibble(
        generation = gen - 1L,
        member = i,
        design_id = ev$design_id,
        cand_index = ev$cand_index,
        fitness = ev$fitness,
        hpu_mean = ev$hpu_mean,
        cost_hpu = ev$cost_hpu,
        cost_rate_h = ev$cost_rate_h,
        extra_paint_cap = ev$extra_paint_cap,
        extra_queue_spots = ev$extra_queue_spots,
        thr_mean = ev$thr_mean,
        feasible = ev$feasible,
        genome_key = opt_genome_key(ev$genome),
        !!!setNames(as.list(ev$genome), defs$name)
      )
    })

    if (verbose) {
      cat(sprintf(
        paste0(
          "[gen %d/%d] best_fit=%.4f mean_fit=%.4f mean_fit_feas=%.4f ",
          "best_hpu=%.4f best_cost=%.4f feasible=%d/%d unique=%d mean_L1=%.2f wall=%s\n"
        ),
        gen - 1L, n_gen - 1L,
        history[[gen]]$best_fitness, history[[gen]]$mean_fitness,
        history[[gen]]$mean_fitness_feasible,
        history[[gen]]$best_hpu, history[[gen]]$best_cost_hpu,
        history[[gen]]$n_feasible, pop_size,
        history[[gen]]$n_unique, history[[gen]]$mean_pairwise_l1,
        doe_format_duration(history[[gen]]$wall_sec)
      ), file = stderr())
    }

    if (gen == n_gen) break

    ord <- order(fitness)
    n_keep <- 1L  # single elite; rest are tournament offspring
    next_pop <- pop[ord[seq_len(n_keep)]]
    while (length(next_pop) < pop_size) {
      i <- opt_tournament(fitness, k = tournament_k)
      j <- opt_tournament(fitness, k = tournament_k)
      child <- opt_mutate(opt_crossover(pop[[i]], pop[[j]]), defs = defs, p_mut = p_mut)
      next_pop[[length(next_pop) + 1L]] <- child
    }
    pop <- next_pop
  }

  hist_tbl <- bind_rows(history)
  eval_tbl <- bind_rows(all_evals)
  generation0 <- eval_tbl %>% dplyr::filter(generation == 0L)

  uniq <- eval_tbl %>%
    group_by(genome_key) %>%
    slice_min(fitness, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(fitness)
  top <- uniq %>% slice_head(n = n_elites)

  if (verbose) {
    cat(sprintf(
      "Re-evaluating guided baseline + top %d elites with %d reps (shared CRN)...\n",
      nrow(top), elite_reps
    ), file = stderr())
  }

  # Elite CRN: shared base_seed..(base_seed+elite_reps-1) for baseline and elites.
  baseline_elite <- opt_evaluate_baseline(
    n_reps = elite_reps,
    base_seed = base_seed,
    thr_floor = 0,
    penalty = 0,
    capacity_cost_h = capacity_cost_h,
    queue_cost_h = queue_cost_h,
    cache = cache,
    defs = defs
  )
  thr_floor_elite <- baseline_elite$thr_mean

  elite_genomes <- top %>%
    dplyr::select(all_of(defs$name)) %>%
    pmap(function(...) as.integer(c(...)))

  elite_evals <- opt_map_eval(
    elite_genomes,
    function(g) {
      opt_evaluate_genome(
        genome = g,
        n_reps = elite_reps,
        base_seed = base_seed,
        cand_index = assign_index(g),
        thr_floor = thr_floor_elite,
        penalty = penalty,
        cache = cache,
        defs = defs,
        shared_seed = TRUE,
        baseline_genome = baseline_genome,
        capacity_cost_h = capacity_cost_h,
        queue_cost_h = queue_cost_h
      )
    },
    cores = cores
  )

  elite_tbl <- map_dfr(elite_evals, function(ev) {
    tibble(
      design_id = ev$design_id,
      cand_index = ev$cand_index,
      fitness = ev$fitness,
      hpu_mean = ev$hpu_mean,
      cost_hpu = ev$cost_hpu,
      cost_rate_h = ev$cost_rate_h,
      thr_mean = ev$thr_mean,
      feasible = ev$feasible,
      n_reps = ev$n_reps,
      genome_key = opt_genome_key(ev$genome),
      !!!setNames(as.list(ev$genome), defs$name)
    )
  }) %>% arrange(fitness)

  best_ev <- elite_evals[[which.min(vapply(elite_evals, `[[`, numeric(1), "fitness"))]]

  ci <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 2) return(c(mean = mean(x), lo = NA_real_, hi = NA_real_, sd = NA_real_))
    m <- mean(x); s <- sd(x); h <- qt(0.975, length(x) - 1) * s / sqrt(length(x))
    c(mean = m, lo = m - h, hi = m + h, sd = s)
  }
  best_hpu_ci <- ci(if_else(is.finite(best_ev$metrics$hpu), best_ev$metrics$hpu, NA_real_))
  best_thr_ci <- ci(best_ev$metrics$throughput_per_hr)
  base_hpu_ci <- ci(if_else(is.finite(baseline_elite$metrics$hpu), baseline_elite$metrics$hpu, NA_real_))
  base_thr_ci <- ci(baseline_elite$metrics$throughput_per_hr)

  op_util <- function(ev) {
    ev$operator_by_rep %>%
      mutate(pool = sub("^operator_", "", resource)) %>%
      group_by(pool) %>%
      summarise(util_mean = mean(avg_util), .groups = "drop")
  }

  list(
    history = hist_tbl,
    generation0 = generation0,
    evaluations = eval_tbl,
    elites = elite_tbl,
    best = best_ev,
    baseline = baseline_elite,
    baseline_search = baseline,
    thr_floor = thr_floor,
    thr_floor_elite = thr_floor_elite,
    capacity_cost_h = capacity_cost_h,
    queue_cost_h = queue_cost_h,
    defs = defs,
    best_summary = tibble(
      set = c("baseline", "best"),
      hpu_mean = c(base_hpu_ci[["mean"]], best_hpu_ci[["mean"]]),
      hpu_lo = c(base_hpu_ci[["lo"]], best_hpu_ci[["lo"]]),
      hpu_hi = c(base_hpu_ci[["hi"]], best_hpu_ci[["hi"]]),
      cost_hpu = c(baseline_elite$cost_hpu, best_ev$cost_hpu),
      cost_rate_h = c(baseline_elite$cost_rate_h, best_ev$cost_rate_h),
      fitness = c(baseline_elite$fitness, best_ev$fitness),
      thr_mean = c(base_thr_ci[["mean"]], best_thr_ci[["mean"]]),
      thr_lo = c(base_thr_ci[["lo"]], best_thr_ci[["lo"]]),
      thr_hi = c(base_thr_ci[["hi"]], best_thr_ci[["hi"]]),
      n_reps = c(baseline_elite$n_reps, best_ev$n_reps),
      feasible = c(TRUE, best_ev$feasible)
    ),
    param_diff = {
      b <- baseline_genome
      g <- best_ev$genome
      tibble(
        parameter = defs$name,
        label = defs$label,
        baseline = as.integer(b),
        best = as.integer(g),
        delta = as.integer(g - b)
      )
    },
    operator_util = bind_rows(
      op_util(baseline_elite) %>% mutate(set = "baseline"),
      op_util(best_ev) %>% mutate(set = "best")
    )
  )
}
