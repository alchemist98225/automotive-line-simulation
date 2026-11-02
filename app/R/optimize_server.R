# optimize_server.R
# Optimize page server: sequential GA with live convergence and Cancel.

optimize_page_server <- function(id = "optimize_page",
                                 build_params,
                                 compare_draft,
                                 compare_results,
                                 compare_titles,
                                 load_compare_pair) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    state <- reactiveValues(
      running = FALSE,
      cancel = FALSE,
      phase = "idle", # idle | search | elite | done
      gen_i = 0L,
      n_gen = 0L,
      pop = NULL,
      evaluated = NULL,
      history = NULL,
      all_evals = NULL,
      thr_floor = NA_real_,
      baseline_genome = NULL,
      baseline_params = NULL,
      capacity_cost_h = 1,
      queue_cost_h = 0.1,
      penalty = 10,
      p_mut = 0.25,
      n_reps = 2L,
      elite_reps = 5L,
      base_seed = 42L,
      cache = NULL,
      defs = NULL,
      best_summary = NULL,
      improved = FALSE,
      param_diff = NULL,
      best_ev = NULL,
      baseline_elite = NULL,
      status = "Idle",
      progress = 0
    )

    # ---- Bounds UI (defaults from opt_knob_defs) ----
    output$bounds_ui <- renderUI({
      defs <- opt_knob_defs()
      tagList(
        div(class = "row g-1 mb-1 small fw-semibold text-muted",
            div(class = "col-6", "Knob"),
            div(class = "col-3", "Min"),
            div(class = "col-3", "Max")),
        lapply(seq_len(nrow(defs)), function(i) {
          nm <- defs$name[[i]]
          div(
            class = "row g-1 align-items-center mb-1",
            div(class = "col-6 small", defs$label[[i]]),
            div(class = "col-3",
                numericInput(ns(paste0("lo_", nm)), label = NULL,
                             value = defs$lo[[i]], min = 0, step = 1, width = "100%")),
            div(class = "col-3",
                numericInput(ns(paste0("hi_", nm)), label = NULL,
                             value = defs$hi[[i]], min = 1, step = 1, width = "100%"))
          )
        })
      )
    })

    custom_defs <- reactive({
      defs <- opt_knob_defs()
      for (i in seq_len(nrow(defs))) {
        nm <- defs$name[[i]]
        lo <- isolate(input[[paste0("lo_", nm)]])
        hi <- isolate(input[[paste0("hi_", nm)]])
        if (!is.null(lo)) defs$lo[[i]] <- as.integer(lo)
        if (!is.null(hi)) defs$hi[[i]] <- as.integer(hi)
        if (defs$hi[[i]] < defs$lo[[i]]) defs$hi[[i]] <- defs$lo[[i]]
        defs$default[[i]] <- as.integer(pmax(defs$lo[[i]], pmin(defs$hi[[i]], defs$default[[i]])))
      }
      defs
    })

    # ---- Baseline resolution ----
    resolve_baseline_params <- reactive({
      src <- input$baseline_source %||% "guided"
      if (identical(src, "guided")) {
        return(list(params = opt_guided_experiment_params(), label = "Guided balanced set"))
      }
      if (identical(src, "current")) {
        p <- tryCatch(build_params(), error = function(e) NULL)
        if (is.null(p)) p <- default_params()
        return(list(params = p, label = "Current Parameters"))
      }
      draft <- tryCatch(compare_draft(), error = function(e) NULL)
      idx <- if (identical(src, "compare_2")) 2L else 1L
      if (!is.null(draft) && !is.null(draft$one) && !is.null(draft$two)) {
        p <- if (idx == 1L) draft$one else draft$two
        titles <- tryCatch(compare_titles(), error = function(e) c("Baseline", "Experiment"))
        return(list(params = p, label = paste0("Compare: ", titles[[idx]])))
      }
      list(params = opt_guided_experiment_params(),
           label = "Guided balanced set (Compare not ready)")
    })

    output$baseline_note <- renderUI({
      info <- resolve_baseline_params()
      p(class = "small mb-0", paste("Using:", info$label))
    })

    genome_from_params <- function(params, defs) {
      st <- params$stations
      ops <- params$operators
      up <- if ("upstream" %in% ops$pool) ops$count[ops$pool == "upstream"][[1]] else 2L
      down <- if ("downstream" %in% ops$pool) ops$count[ops$pool == "downstream"][[1]] else 2L
      g <- c(
        as.integer(st$buffer_before[st$name == "paint"][[1]]),
        as.integer(st$buffer_before[st$name == "final_assembly"][[1]]),
        as.integer(st$buffer_before[st$name == "inspection"][[1]]),
        as.integer(up),
        as.integer(down),
        as.integer(st$capacity[st$name == "paint"][[1]])
      )
      opt_clip_genome(g, defs)
    }

    # ---- Presets ----
    observeEvent(input$preset_quick, {
      updateNumericInput(session, "pop", value = 10)
      updateNumericInput(session, "gen", value = 6)
      updateNumericInput(session, "reps", value = 2)
      updateNumericInput(session, "elite_reps", value = 15)
    })
    observeEvent(input$preset_thorough, {
      updateNumericInput(session, "pop", value = 24)
      updateNumericInput(session, "gen", value = 15)
      updateNumericInput(session, "reps", value = 3)
      updateNumericInput(session, "elite_reps", value = 15)
    })

    observeEvent(input$cancel, {
      state$cancel <- TRUE
      state$status <- "Cancel requested…"
    })

    # ---- Start run ----
    observeEvent(input$run, {
      if (isTRUE(state$running)) return()
      defs <- custom_defs()
      base_info <- resolve_baseline_params()
      baseline_genome <- genome_from_params(base_info$params, defs)
      # Align knob defaults to this baseline so cost deltas are vs the chosen set
      defs$default <- as.integer(baseline_genome)

      pop_size <- as.integer(input$pop %||% 8)
      n_gen <- as.integer(input$gen %||% 5)
      n_reps <- as.integer(input$reps %||% 2)
      elite_reps <- as.integer(input$elite_reps %||% 5)
      base_seed <- as.integer(input$seed %||% 42)
      capacity_cost_h <- as.numeric(input$capacity_cost %||% 1)
      queue_cost_h <- as.numeric(input$queue_cost %||% 0.1)

      cache <- opt_eval_cache_new()
      # Baseline thr floor (search reps)
      base_eval <- opt_evaluate_baseline(
        n_reps = n_reps,
        base_seed = base_seed,
        thr_floor = 0,
        penalty = 0,
        capacity_cost_h = capacity_cost_h,
        queue_cost_h = queue_cost_h,
        cache = cache,
        defs = defs
      )
      # Override baseline eval genome/params to match chosen baseline knobs
      # (opt_evaluate_baseline uses guided defaults; re-eval chosen genome)
      base_eval <- opt_evaluate_genome(
        genome = baseline_genome,
        n_reps = n_reps,
        base_seed = base_seed,
        cand_index = 1L,
        thr_floor = 0,
        penalty = 0,
        cache = cache,
        defs = defs,
        shared_seed = TRUE,
        baseline_genome = baseline_genome,
        capacity_cost_h = capacity_cost_h,
        queue_cost_h = queue_cost_h
      )

      pop <- lapply(seq_len(pop_size), function(i) opt_random_genome(defs))
      pop[[1]] <- baseline_genome
      # Local neighbors of baseline so staffing/queue cuts show up early
      # Explicit one-step staffing cut (often improves HPU when thr holds under elite reps)
      up_i <- which(defs$name == "op_upstream")[[1]]
      if (length(up_i) && baseline_genome[[up_i]] > defs$lo[[up_i]] && pop_size >= 2L) {
        g_cut <- baseline_genome
        g_cut[[up_i]] <- baseline_genome[[up_i]] - 1L
        pop[[2]] <- g_cut
      }
      n_local <- min(4L, pop_size - 1L)
      for (i in seq_len(n_local)) {
        idx <- i + 1L
        if (idx == 2L && !is.null(pop[[2]])) next
        pop[[idx]] <- opt_mutate(baseline_genome, defs = defs, p_mut = 0.5)
      }

      state$running <- TRUE
      state$improved <- FALSE
      state$cancel <- FALSE
      state$phase <- "search"
      state$gen_i <- 0L
      state$n_gen <- n_gen
      state$pop <- pop
      state$history <- NULL
      state$all_evals <- NULL
      state$thr_floor <- base_eval$thr_mean
      # Soft floor for search only (2-rep noise); elite re-eval uses hard CRN floor
      state$thr_floor_search <- 0.95 * base_eval$thr_mean
      state$baseline_genome <- baseline_genome
      state$baseline_params <- base_info$params
      state$capacity_cost_h <- capacity_cost_h
      state$queue_cost_h <- queue_cost_h
      state$n_reps <- n_reps
      state$elite_reps <- elite_reps
      state$base_seed <- base_seed
      state$cache <- cache
      state$defs <- defs
      state$best_summary <- NULL
      state$param_diff <- NULL
      state$best_ev <- NULL
      state$baseline_elite <- NULL
      state$status <- sprintf("Baseline thr %.2f/hr — starting search…", base_eval$thr_mean)
      state$progress <- 0.02
    })

    assign_index <- function(genome, defs, baseline_genome) {
      genome <- opt_clip_genome(genome, defs)
      if (identical(opt_genome_key(genome), opt_genome_key(baseline_genome))) return(1L)
      weights <- as.integer(cumprod(c(1L, defs$hi - defs$lo + 1L)))[seq_along(genome)]
      as.integer(2L + sum((genome - defs$lo) * weights))
    }

    # ---- Step one generation (or elite finish) ----
    observe({
      req(isTRUE(state$running))
      if (isTRUE(state$cancel) && identical(state$phase, "search")) {
        state$running <- FALSE
        state$phase <- "idle"
        state$status <- "Cancelled."
        state$progress <- 0
        return()
      }

      isolate({
        if (identical(state$phase, "search")) {
          defs <- state$defs
          pop <- state$pop
          gen <- state$gen_i + 1L
          n_gen <- state$n_gen
          n_reps <- state$n_reps
          thr_floor <- state$thr_floor_search %||% (0.95 * state$thr_floor)
          baseline_genome <- state$baseline_genome
          cache <- state$cache

          evaluated <- lapply(pop, function(g) {
            opt_evaluate_genome(
              genome = g,
              n_reps = n_reps,
              base_seed = state$base_seed,
              cand_index = assign_index(g, defs, baseline_genome),
              thr_floor = thr_floor,
              penalty = state$penalty,
              cache = cache,
              defs = defs,
              baseline_genome = baseline_genome,
              capacity_cost_h = state$capacity_cost_h,
              queue_cost_h = state$queue_cost_h
            )
          })
          fitness <- vapply(evaluated, `[[`, numeric(1), "fitness")
          hpu <- vapply(evaluated, `[[`, numeric(1), "hpu_mean")
          cost <- vapply(evaluated, `[[`, numeric(1), "cost_hpu")
          thr <- vapply(evaluated, `[[`, numeric(1), "thr_mean")
          feasible <- vapply(evaluated, `[[`, logical(1), "feasible")
          keys <- vapply(evaluated, function(ev) opt_genome_key(ev$genome), character(1))
          mean_fit_feas <- if (any(feasible)) mean(fitness[feasible]) else NA_real_

          hist_row <- tibble(
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
            n_unique = length(unique(keys)),
            mean_pairwise_l1 = opt_mean_pairwise_distance(lapply(evaluated, `[[`, "genome")),
            wall_sec = NA_real_
          )
          eval_rows <- map2_dfr(evaluated, seq_along(evaluated), function(ev, i) {
            tibble(
              generation = gen - 1L,
              member = i,
              fitness = ev$fitness,
              hpu_mean = ev$hpu_mean,
              cost_hpu = ev$cost_hpu,
              thr_mean = ev$thr_mean,
              feasible = ev$feasible,
              genome_key = opt_genome_key(ev$genome),
              !!!setNames(as.list(ev$genome), defs$name)
            )
          })

          state$history <- bind_rows(state$history, hist_row)
          state$all_evals <- bind_rows(state$all_evals, eval_rows)
          state$gen_i <- gen
          state$progress <- 0.05 + 0.75 * (gen / n_gen)
          state$status <- sprintf(
            "Generation %d / %d — best fitness %.3f (feasible %d/%d)",
            gen - 1L, n_gen - 1L, hist_row$best_fitness, hist_row$n_feasible, length(pop)
          )

          if (gen >= n_gen) {
            state$phase <- "elite"
            state$status <- "Re-evaluating top elites…"
          } else {
            ord <- order(fitness)
            next_pop <- list(state$baseline_genome, pop[[ord[1L]]])
            # Drop duplicate if the elite is already the baseline
            if (identical(opt_genome_key(next_pop[[1]]), opt_genome_key(next_pop[[2]]))) {
              next_pop <- next_pop[1L]
            }
            while (length(next_pop) < length(pop)) {
              i <- opt_tournament(fitness, k = 3L)
              j <- opt_tournament(fitness, k = 3L)
              child <- opt_mutate(opt_crossover(pop[[i]], pop[[j]]), defs = defs, p_mut = state$p_mut)
              next_pop[[length(next_pop) + 1L]] <- child
            }
            state$pop <- next_pop
          }
        } else if (identical(state$phase, "elite")) {
          defs <- state$defs
          eval_tbl <- state$all_evals
          uniq <- eval_tbl %>%
            dplyr::group_by(genome_key) %>%
            dplyr::slice_min(fitness, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(fitness) %>%
            dplyr::slice_head(n = 5)
          baseline_genome <- state$baseline_genome
          elite_reps <- state$elite_reps
          cache <- state$cache

          baseline_elite <- opt_evaluate_genome(
            genome = baseline_genome,
            n_reps = elite_reps,
            base_seed = state$base_seed,
            cand_index = 1L,
            thr_floor = 0,
            penalty = 0,
            cache = cache,
            defs = defs,
            shared_seed = TRUE,
            baseline_genome = baseline_genome,
            capacity_cost_h = state$capacity_cost_h,
            queue_cost_h = state$queue_cost_h
          )
          thr_floor_elite <- baseline_elite$thr_mean

          elite_genomes <- uniq %>%
            dplyr::select(all_of(defs$name)) %>%
            pmap(function(...) as.integer(c(...)))
          # Always include the baseline genome in the elite re-eval pool
          base_key <- opt_genome_key(baseline_genome)
          elite_keys <- vapply(elite_genomes, opt_genome_key, character(1))
          if (!base_key %in% elite_keys) {
            elite_genomes <- c(list(baseline_genome), elite_genomes)
          }
          elite_evals <- lapply(elite_genomes, function(g) {
            opt_evaluate_genome(
              genome = g,
              n_reps = elite_reps,
              base_seed = state$base_seed,
              cand_index = assign_index(g, defs, baseline_genome),
              thr_floor = thr_floor_elite,
              penalty = state$penalty,
              cache = cache,
              defs = defs,
              shared_seed = TRUE,
              baseline_genome = baseline_genome,
              capacity_cost_h = state$capacity_cost_h,
              queue_cost_h = state$queue_cost_h
            )
          })
          # Include pristine baseline_elite in the comparison set
          all_cands <- c(list(baseline_elite), elite_evals)
          fits <- vapply(all_cands, `[[`, numeric(1), "fitness")
          best_ev <- all_cands[[which.min(fits)]]
          # Guard: never report a best worse than baseline
          if (best_ev$fitness > baseline_elite$fitness) best_ev <- baseline_elite
          improved <- best_ev$fitness < baseline_elite$fitness - 1e-12 &&
            !identical(opt_genome_key(best_ev$genome), opt_genome_key(baseline_genome))

          ci <- function(x) {
            x <- x[is.finite(x)]
            if (length(x) < 2) return(c(mean = mean(x), lo = NA_real_, hi = NA_real_))
            m <- mean(x); s <- sd(x); h <- qt(0.975, length(x) - 1) * s / sqrt(length(x))
            c(mean = m, lo = m - h, hi = m + h)
          }
          best_hpu_ci <- ci(if_else(is.finite(best_ev$metrics$hpu), best_ev$metrics$hpu, NA_real_))
          best_thr_ci <- ci(best_ev$metrics$throughput_per_hr)
          base_hpu_ci <- ci(if_else(is.finite(baseline_elite$metrics$hpu), baseline_elite$metrics$hpu, NA_real_))
          base_thr_ci <- ci(baseline_elite$metrics$throughput_per_hr)

          state$baseline_elite <- baseline_elite
          state$best_ev <- best_ev
          state$best_summary <- tibble(
            set = c("baseline", "best"),
            hpu_mean = c(base_hpu_ci[["mean"]], best_hpu_ci[["mean"]]),
            hpu_lo = c(base_hpu_ci[["lo"]], best_hpu_ci[["lo"]]),
            hpu_hi = c(base_hpu_ci[["hi"]], best_hpu_ci[["hi"]]),
            cost_hpu = c(baseline_elite$cost_hpu, best_ev$cost_hpu),
            fitness = c(baseline_elite$fitness, best_ev$fitness),
            thr_mean = c(base_thr_ci[["mean"]], best_thr_ci[["mean"]]),
            thr_lo = c(base_thr_ci[["lo"]], best_thr_ci[["lo"]]),
            thr_hi = c(base_thr_ci[["hi"]], best_thr_ci[["hi"]]),
            n_reps = c(baseline_elite$n_reps, best_ev$n_reps),
            feasible = c(TRUE, best_ev$feasible)
          )
          state$param_diff <- tibble(
            parameter = defs$name,
            label = defs$label,
            baseline = as.integer(baseline_genome),
            best = as.integer(best_ev$genome),
            delta = as.integer(best_ev$genome - baseline_genome)
          )
          state$improved <- isTRUE(improved)
          state$phase <- "done"
          state$running <- FALSE
          state$progress <- 1
          state$status <- if (isTRUE(improved)) {
            sprintf("Done. Best fitness %.3f (baseline %.3f).",
                    best_ev$fitness, baseline_elite$fitness)
          } else {
            sprintf("Done. No better setup found (baseline fitness %.3f). Try Thorough or wider bounds.",
                    baseline_elite$fitness)
          }
        }
      })

      if (isTRUE(state$running)) invalidateLater(50, session)
    })

    output$progress_ui <- renderUI({
      if (!isTRUE(state$running) && identical(state$phase, "idle") && state$progress < 1) return(NULL)
      pct <- round(100 * (state$progress %||% 0))
      div(
        class = "mt-2",
        div(class = "progress", style = "height: 1.25rem;",
            div(class = "progress-bar", role = "progressbar",
                style = sprintf("width:%d%%;", pct),
                sprintf("%d%%", pct))),
        p(class = "small mb-0 mt-1", state$status)
      )
    })

    output$status_line <- renderUI({
      p(class = "small text-muted mb-0", state$status)
    })

    output$convergence <- renderPlot({
      optimize_convergence_plot(state$history)
    })
    output$diversity <- renderPlot({
      optimize_diversity_plot(state$history)
    })

    output$results_block <- renderUI({
      if (is.null(state$best_summary)) {
        return(p(class = "text-muted", "Results appear here after a finished run."))
      }
      improved <- isTRUE(state$improved)
      tagList(
        if (!improved) {
          div(class = "alert alert-warning py-2 px-3",
              "No better setup found — try Thorough or wider bounds.")
        },
        h5("Best vs baseline"),
        DTOutput(ns("summary_table")),
        h5("Parameter changes"),
        plotOutput(ns("param_diff"), height = "320px"),
        div(
          class = "mt-3",
          actionButton(
            ns("send_compare"), "Send to Compare",
            class = "btn-success",
            disabled = !improved
          )
        ),
        uiOutput(ns("send_note"))
      )
    })

    output$summary_table <- renderDT({
      req(state$best_summary)
      optimize_summary_dt(state$best_summary)
    })
    output$param_diff <- renderPlot({
      optimize_param_diff_plot(state$param_diff)
    })

    send_note <- reactiveVal(NULL)
    output$send_note <- renderUI({
      note <- send_note()
      if (is.null(note)) return(NULL)
      div(class = "alert alert-success py-2 px-3 mt-2", note)
    })

    observeEvent(input$send_compare, {
      req(state$best_ev, state$baseline_elite, isTRUE(state$improved))
      # Build run-like objects for Compare load_pair
      base_run <- list(
        params = state$baseline_elite$params,
        summaries = state$baseline_elite$metrics %>%
          transmute(
            replication,
            n_completed,
            throughput_per_hr,
            hpu = hpu
          ),
        run_parameters = state$baseline_elite$run_parameters,
        operator_by_rep = state$baseline_elite$operator_by_rep,
        utilization_by_rep = state$baseline_elite$utilization_by_rep,
        seeds = state$baseline_elite$metrics$seed %||% seq_len(state$baseline_elite$n_reps)
      )
      # Prefer full sim objects if present on elite evals
      if (!is.null(state$baseline_elite$params)) {
        # Re-run once? elites already have metrics; load_pair expects full run_line_sim shape.
        # Safest: re-simulate both with shared seeds for Compare fidelity.
        base_params <- state$baseline_elite$params
        base_params$n_replications <- state$baseline_elite$n_reps
        base_params$base_seed <- state$base_seed
        best_params <- state$best_ev$params
        best_params$n_replications <- state$best_ev$n_reps
        best_params$base_seed <- state$base_seed
        send_note("Running baseline and best for Compare…")
        base_full <- run_line_sim(base_params)
        best_full <- run_line_sim(best_params)
        load_compare_pair(
          base_full,
          best_full,
          c("Optimize baseline", "Optimize best")
        )
        send_note("Sent to Compare as Baseline = optimize baseline, Experiment = optimize best.")
      }
    })
  })
}
