# server_main.R
# Main Shiny server: Simulate run lifecycle, KPIs, Results plot/table outputs,
# downloads, guided Try-it, and wiring to Compare / Production modules.
# Chart drawing lives in plots_*.R; this file binds inputs → those functions.

server <- function(input, output, session) {
  # idle | running | done — drives Run simulation / Try it button colors.
  sim_btn_state <- reactiveVal("idle")
  # Startup scenario pair (dedicated 1-paint vs shared 2-paint). Missing file = normal empty start.
  preload_bundle <- tryCatch(load_preload_scenarios(), error = function(e) NULL)
  preload_consumed <- reactiveVal(FALSE)
  # When set, assemble_params returns this list so fingerprints match the preloaded run
  # until the sidebar inputs catch up via apply_params_to_inputs().
  params_overlay <- reactiveVal(
    if (!is.null(preload_bundle)) preload_bundle$scenario2$params else NULL
  )

  set_sim_btn_state <- function(state) {
    sim_btn_state(state)
    session$sendCustomMessage("simRunButtons", list(state = state))
  }
  show_sim_progress <- function(detail = "", value = NULL) {
    set_sim_btn_state("running")
    session$sendCustomMessage(
      "simProgress",
      list(show = TRUE, detail = detail, value = value)
    )
  }
  hide_sim_progress <- function(done = TRUE) {
    # Defer Ready/Done until this flush reaches the browser so KPI tiles are
    # not still showing the previous run when the button flips green.
    if (isTRUE(done)) {
      session$onFlushed(function() {
        session$sendCustomMessage("simProgress", list(show = FALSE))
        set_sim_btn_state("done")
      }, once = TRUE)
    } else {
      session$sendCustomMessage("simProgress", list(show = FALSE))
    }
  }
  jump_to_params <- function(section_id) {
    nav_select("main_page", "parameters", session = session)
    session$sendCustomMessage("scrollToSection", list(id = section_id))
  }
  observeEvent(input$jump_capacity, jump_to_params("params_capacity"), ignoreInit = TRUE)
  observeEvent(input$jump_cycle_times, jump_to_params("params_cycle_times"), ignoreInit = TRUE)
  observeEvent(input$jump_operators, jump_to_params("params_operators"), ignoreInit = TRUE)
  observeEvent(input$jump_reliability, jump_to_params("params_reliability"), ignoreInit = TRUE)

  # "try" forces Paint to 2 bays. "defaults" ignores the widgets and uses the
  # first-load placeholders. NULL follows the sidebar. The overlay stays until
  # the widgets catch up, so the run is not marked stale during that round trip.
  guide_mode <- reactiveVal(NULL)
  guide_run <- reactiveVal(0L)
  # Holds the 1-bay run until the Try it simulation finishes, then the
  # comparison page consumes the pair. NULL means this run is not that experiment.
  guide_pair <- reactiveVal(NULL)

  output$operator_controls <- renderUI({
    switch(input$staffing,
      zones=tagList(numericInput("op_a","Zone A operators",2,min=1,step=1),numericInput("op_b","Zone B operators",2,min=1,step=1)),
      shared=numericInput("op_all","Shared team operators",4,min=1,step=1),
      dedicated=tagList(lapply(names(station_labels),function(n) numericInput(paste0("op_",n),paste(station_labels[[n]],"operators"),1,min=1,step=1))))
  })
  # Collapsed panels still create operator counts, timing, and reliability inputs.
  outputOptions(output, "operator_controls", suspendWhenHidden = FALSE)
  register_station_panels(output)
  read_timing <- function(st, activity, ignore_inputs = FALSE) {
    legacy <- activity_timing(st, activity)
    if (ignore_inputs) return(legacy)
    id <- paste0(activity, "_", st$name)
    method <- input[[paste0(id, "_method")]]
    if (is.null(method)) return(legacy)
    spec <- tryCatch({
      if (method=="empirical") timing_spec("empirical",observations=parse_observations(input[[paste0(id,"_observations")]] %||% ""))
      else timing_spec(method,legacy$mean_min,
        sd_min=input[[paste0(id,"_sd")]] %||% legacy$sd_min,
        distribution=input[[paste0(id,"_distribution")]] %||% "normal")
    },error=function(e) e)
    validate(need(!inherits(spec,"error"),paste(station_labels[[st$name]],activity,if(inherits(spec,"error")) conditionMessage(spec) else "")))
    valid <- tryCatch({validate_timing(spec); TRUE},error=function(e) conditionMessage(e))
    validate(need(isTRUE(valid),paste(station_labels[[st$name]],activity,valid)))
    spec
  }
  # Build params from sidebar inputs. Dynamic renderUI inputs are still NULL
  # on the first bindEvent fire (ignoreNULL = FALSE runs before the client
  # registers op_*, load_*, mtbf_*, and the rest), including when those
  # outputs sit in a collapsed accordion panel. Fall back to the same
  # placeholder defaults those controls display so the first paint is filled.
  # suspendWhenHidden = FALSE lets a later run see values from panels that
  # are still collapsed.
  assemble_params <- function(mode = NULL) {
    # Preload overlay keeps fingerprints aligned with Scenario 2 until inputs update.
    if (is.null(mode)) {
      overlay <- params_overlay()
      if (!is.null(overlay)) return(overlay)
    }
    ignore <- identical(mode, "defaults")
    defaults <- default_params()
    st0 <- defaults$stations
    val <- function(id, default) {
      if (ignore) return(default)
      x <- input[[id]]
      if (is.null(x)) default else x
    }
    flag <- function(id, default) {
      if (ignore) return(default)
      x <- input[[id]]
      if (is.null(x)) default else isTRUE(x)
    }
    station_field <- function(station_name, field) st0[[field]][[match(station_name, st0$name)]]
    staffing <- val("staffing", "zones")
    if (!staffing %in% c("zones", "shared", "dedicated")) staffing <- "zones"
    reliability <- flag("reliability", TRUE)
    noncyclic <- flag("noncyclic", TRUE)
    p <- list(
      time_mode="shifts",shift_length_min=val("sim_length",defaults$shift_length_min),
      n_shifts=val("n_shifts",defaults$n_shifts),
      sim_length_min=val("warmup",defaults$warmup_min)+val("sim_length",defaults$shift_length_min)*val("n_shifts",defaults$n_shifts),
      release_policy=val("release_policy",defaults$release_policy),entry_buffer=val("entry_buffer",defaults$entry_buffer),
      wip_limit=val("wip_limit",defaults$wip_limit),order_spacing=val("order_spacing",defaults$order_spacing),
      n_replications   = as.integer(val("n_reps", defaults$n_replications)),
      base_seed        = as.integer(val("seed", defaults$base_seed)),
      warmup_min       = val("warmup", defaults$warmup_min),
      arrival_mean_min = 60/val("planned_rate",60/defaults$arrival_mean_min),
      grid_dt_min      = defaults$grid_dt_min %||% 2,
      repair_technicians = as.integer(val("repair_staff", defaults$repair_technicians)),
      stations = tribble(
        ~name,              ~capacity,              ~dist,         ~mean_min,        ~sd_min,
        "body_shop",        as.integer(val("body_cap", 2)),  "exponential", val("body_mean", 6),  NA_real_,
        "paint",            as.integer(val("paint_cap", 1)), "normal",      val("paint_mean", 8), 1.5,
        "final_assembly",   as.integer(val("fa_cap", 2)),    "normal",      val("fa_mean", 7),    1.0,
        "inspection",       as.integer(val("insp_cap", 1)),  "exponential", val("insp_mean", 3),  NA_real_
      )
    )
    p$operators <- switch(staffing,
      zones=tibble(pool=c("upstream","downstream"),count=c(val("op_a", 2), val("op_b", 2))),
      shared=tibble(pool="shared",count=val("op_all", 4)),
      dedicated=tibble(pool=names(station_labels),count=map_dbl(names(station_labels),~val(paste0("op_",.x), 1))))
    p$stations <- p$stations %>% mutate(
      buffer_before=map_dbl(name,~if(.x=="body_shop") 0 else val(paste0("buffer_",.x),station_field(.x,"buffer_before"))),
      operator_pool=switch(staffing,zones=c("upstream","upstream","downstream","downstream"),shared=rep("shared",4),dedicated=name),
      load_min=map_dbl(name,~val(paste0("load_",.x), station_field(.x, "load_min"))),
      unload_min=map_dbl(name,~val(paste0("unload_",.x), station_field(.x, "unload_min"))),
      mtbf_min=if(reliability) map_dbl(name,~val(paste0("mtbf_",.x), station_field(.x, "mtbf_min"))) else Inf,
      mttr_min=map_dbl(name,~val(paste0("mttr_",.x), station_field(.x, "mttr_min"))),
      task_interval_min=if(noncyclic) map_dbl(name,~val(paste0("interval_",.x), station_field(.x, "task_interval_min"))) else Inf,
      task_duration_min=if(noncyclic) map_dbl(name,~val(paste0("task_",.x), station_field(.x, "task_duration_min"))) else 0,
      task_every_n=map_dbl(name,~val(paste0("every_",.x), 0)))
    p$stations <- p$stations %>% mutate(
      load_timing=map(seq_len(n()),~read_timing(p$stations[.x,],"load", ignore)),
      auto_timing=map(seq_len(n()),~read_timing(p$stations[.x,],"auto", ignore)),
      unload_timing=map(seq_len(n()),~read_timing(p$stations[.x,],"unload", ignore)),
      task_timing=map(seq_len(n()),~if(noncyclic) read_timing(p$stations[.x,],"task", ignore) else timing_spec()))
    if (identical(mode, "try")) {
      p$stations$capacity[p$stations$name == "paint"] <- 2L
    }
    validate(need(p$warmup_min<p$sim_length_min,"Warmup must be shorter than the simulation."))
    normalize_params(p)
  }
  build_params <- reactive(assemble_params(guide_mode()))
  observeEvent(input$guide_try, {
    updateNumericInput(session, "paint_cap", value = 2)
    guide_mode("try")
    exp_params <- tryCatch(assemble_params("try"), error = function(e) NULL)
    if (!is.null(exp_params)) {
      base_params <- comparison_baseline_params(exp_params)
      current <- tryCatch(isolate(sim_result()), error = function(e) NULL)
      # Reuse the run already on screen when it is the 1-bay line with the
      # same seeds and replications. Otherwise simulate that baseline once.
      baseline_run <- if (!is.null(current) && comparison_same(current$params, base_params)) {
        current
      } else {
        local({
          show_sim_progress(
            sprintf("%d replications · 1-bay baseline · horizon %.0f min",
                    base_params$n_replications, base_params$sim_length_min),
            0.25
          )
          # withProgress forces a websocket flush so the fixed panel paints;
          # its own toast stays hidden by CSS. Keep Running… through the
          # experiment leg — do not flip Done between the two sims.
          tryCatch(
            withProgress(message = "", value = 0.25, run_line_sim(base_params)),
            error = function(e) {
              hide_sim_progress(done = FALSE)
              stop(e)
            }
          )
        })
      }
      guide_pair(list(baseline = baseline_run, experiment_params = exp_params))
    }
    guide_run(isolate(guide_run()) + 1L)
  })
  observeEvent(input$guide_reset, {
    guide_pair(NULL)
    restore_default_inputs(session)
    guide_mode("defaults")
    guide_run(isolate(guide_run()) + 1L)
  })
  # Drop the overlay once the sidebar shows the same line the run used.
  observe({
    mode <- guide_mode()
    if (is.null(mode)) return()
    matched <- tryCatch(
      identical(params_fingerprint(assemble_params(NULL)), params_fingerprint(assemble_params(mode))),
      error = function(e) FALSE
    )
    if (isTRUE(matched)) guide_mode(NULL)
  })
  # While every sidebar input is still NULL, the overlay keeps the first
  # fingerprint aligned with the preloaded Scenario 2. As soon as any control
  # is registered, stop masking so edits (and tests that set only some inputs)
  # are honored.
  observe({
    if (is.null(params_overlay())) return()
    hydrated <- !is.null(input$paint_cap) || !is.null(input$n_reps) ||
      !is.null(input$sim_length) || !is.null(input$n_shifts) ||
      !is.null(input$staffing)
    if (!hydrated) return()
    params_overlay(NULL)
  })
  two_set_study <- comparison_page_server("two_set_study", build_params, reactive(input$main_page))
  optimize_page_server(
    "optimize_page",
    build_params = build_params,
    compare_draft = two_set_study$draft,
    compare_results = two_set_study$results,
    compare_titles = two_set_study$titles,
    load_compare_pair = two_set_study$load_pair
  )
  analysis_page_server("analysis_page")

  # Seed Compare once from the startup preload, then push Scenario 2 into the sidebars.
  # Tied to the first sim_result so it does not race later guided load_pair calls.
  preload_compare_done <- reactiveVal(FALSE)
  observeEvent(sim_result(), {
    if (is.null(preload_bundle) || isTRUE(isolate(preload_compare_done()))) return()
    preload_compare_done(TRUE)
    s1 <- preload_bundle$scenario1$result
    s1$params <- preload_bundle$scenario1$params
    s2 <- preload_bundle$scenario2$result
    s2$params <- preload_bundle$scenario2$params
    r1 <- stamp_run(s1, s1$params, 0L)
    r2 <- stamp_run(s2, s2$params, 0L)
    two_set_study$load_pair(
      r1, r2,
      c(preload_bundle$scenario1$label, preload_bundle$scenario2$label)
    )
    apply_params_to_inputs(session, preload_bundle$scenario2$params)
  }, once = TRUE, ignoreInit = FALSE)

  run_seq <- new.env(parent = emptyenv())
  run_seq$main <- 0L
  run_seq$staffing <- 0L
  run_seq$timing <- 0L
  staffing_trigger <- reactiveVal(0L)
  timing_trigger <- reactiveVal(0L)
  # ignoreInit would swallow the first click when the input is created late.
  # Value 0 is the button's initial state, not a request to rerun.
  bump <- function(value, trigger) {
    if (is.null(value) || value < 1) return()
    trigger(isolate(trigger()) + 1L)
  }
  observeEvent(input$compare, bump(input$compare, staffing_trigger), ignoreNULL = TRUE)
  observeEvent(input$rerun_staffing, bump(input$rerun_staffing, staffing_trigger), ignoreNULL = TRUE)
  observeEvent(input$compare_timing, bump(input$compare_timing, timing_trigger), ignoreNULL = TRUE)
  observeEvent(input$rerun_timing, bump(input$rerun_timing, timing_trigger), ignoreNULL = TRUE)
  comparison <- eventReactive(staffing_trigger(), {
    req(isTRUE(staffing_trigger() > 0))
    p <- build_params()
    fp <- params_fingerprint(p)
    settings <- format_run_settings(p)
    alternatives <- list(p,p,p,p)
    alternatives[[2]]$operators$count[[1]] <- alternatives[[2]]$operators$count[[1]]+1L
    alternatives[[3]]$operators$count[[nrow(p$operators)]] <- alternatives[[3]]$operators$count[[nrow(p$operators)]]+1L
    alternatives[[4]]$operators <- tibble(pool="shared",count=sum(p$operators$count))
    alternatives[[4]]$stations$operator_pool <- "shared"
    show_sim_progress(
      sprintf("Staffing comparison · %d replications", p$n_replications),
      0.25
    )
    out <- tryCatch(
      withProgress(message = "", value = 0.25, run_scenarios(tibble(
        scenario=c("Current","Add one to first team","Add one to last team","Same headcount, shared team"),params=alternatives))),
      finally = hide_sim_progress()
    )
    run_seq$staffing <- run_seq$staffing + 1L
    tag_comparison(out, p, run_seq$staffing, fingerprint=fp, summary=settings)
  }, ignoreNULL = TRUE)
  timing_comparison <- eventReactive(timing_trigger(), {
    req(isTRUE(timing_trigger() > 0))
    p <- build_params()
    fp <- params_fingerprint(p)
    settings <- format_run_settings(p)
    show_sim_progress(
      sprintf("Timing comparison · %d replications", p$n_replications),
      0.25
    )
    out <- tryCatch(
      withProgress(message = "", value = 0.25, run_scenarios(variability_scenarios(p))),
      finally = hide_sim_progress()
    )
    run_seq$timing <- run_seq$timing + 1L
    tag_comparison(out, p, run_seq$timing, fingerprint=fp, summary=settings)
  }, ignoreNULL = TRUE)
  observeEvent(input$compare, {
    nav_select("main_page", "results_capacity", session = session)
    nav_select("capacity_tabs", "staffing", session = session)
  }, ignoreInit = TRUE)
  observeEvent(input$compare_timing, {
    nav_select("main_page", "results_lead", session = session)
    nav_select("lead_tabs", "timing", session = session)
  }, ignoreInit = TRUE)
  observeEvent(input$kpi_nav, {
    page <- input$kpi_nav
    if (!is.null(page) && nzchar(page)) {
      nav_select("main_page", page, session = session)
      session$sendCustomMessage("closeResultsNav", list())
    }
  }, ignoreInit = TRUE)
  output$timing_table <- renderDT({
    req(isTRUE(timing_trigger() > 0))
    timing_comparison() %>% dplyr::select(scenario,comparison) %>% unnest(comparison) %>%
      datatable(rownames=FALSE,options=list(dom="t")) %>% formatRound(columns=3:10,digits=2)
  })
  output$scenario_table <- renderDT({
    req(isTRUE(staffing_trigger() > 0))
    comparison() %>% dplyr::select(scenario,comparison) %>% unnest(comparison) %>%
      datatable(rownames=FALSE,options=list(dom="t")) %>% formatRound(columns=3:10,digits=2)
  })
  # Same mean utilization + queue content as before; occupancy-style bar on utilization.
  # fillContainer=FALSE keeps the card height to the table (no tall empty widget).
  output$labor_table <- renderDT({
    operator_pool_dt(operator_pool_df(sim_result()$labor))
  }, fillContainer = FALSE)

  # Readable labels only; same columns/content as before (no CI).
  output$reliability_table <- renderDT({
    r <- sim_result()
    w <- r$params$warmup_min
    h <- r$params$sim_length_min
    failures_in_window <- r$failure_events %>%
      dplyr::filter(is.finite(.data$time), .data$time >= w, .data$time < h) %>%
      dplyr::count(station, name = "observed_failures")
    tbl <- r$params$stations %>%
      dplyr::select(name, mtbf_min, mttr_min) %>%
      dplyr::left_join(failures_in_window, by = c("name" = "station")) %>%
      dplyr::mutate(
        observed_failures = tidyr::replace_na(.data$observed_failures, 0L),
        Station = label_station(.data$name),
        `MTBF (min)` = .data$mtbf_min,
        `MTTR (min)` = .data$mttr_min,
        `Observed failures` = .data$observed_failures
      ) %>%
      dplyr::select(Station, `MTBF (min)`, `MTTR (min)`, `Observed failures`)
    # Integers / Inf from inputs — skip formatRound so Inf stays readable.
    compact_metric_dt(tbl)
  }, fillContainer = FALSE)
  # Compare can load a finished set into Results without rerunning.
  # When non-NULL, this overrides the Simulate bindEvent result.
  compare_result_override <- reactiveVal(NULL)
  results_display <- reactiveVal(list(kind = "main", label = NULL))
  # Run on button click; also fire once on load so KPIs appear immediately.
  # The fixed progress panel is opened from a click handler in JS so it paints
  # before this reactive blocks the session. hide_sim_progress() clears it.
  main_sim_result <- reactive({
    # First paint: serve the precomputed Scenario 2 without re-running 20 reps.
    if (!is.null(preload_bundle) && !isTRUE(isolate(preload_consumed()))) {
      preload_consumed(TRUE)
      p <- preload_bundle$scenario2$params
      fp <- params_fingerprint(p)
      settings <- format_run_settings(p)
      run_seq$main <- run_seq$main + 1L
      res <- preload_bundle$scenario2$result
      res$params <- p
      return(stamp_run(res, p, run_seq$main, fingerprint = fp, summary = settings))
    }
    # Real Run / Try-it: drop the startup overlay so sidebar edits are honored.
    # Call assemble_params directly; build_params() may still hold a cached overlay
    # value from this same flush.
    params_overlay(NULL)
    p <- assemble_params(guide_mode())
    fp <- params_fingerprint(p)
    settings <- format_run_settings(p)
    show_sim_progress(
      sprintf("%d replications · horizon %.0f min", p$n_replications, p$sim_length_min),
      0.35
    )
    res <- tryCatch(
      withProgress(message = "", value = 0.35, run_line_sim(p)),
      finally = hide_sim_progress()
    )
    run_seq$main <- run_seq$main + 1L
    stamp_run(res, p, run_seq$main, fingerprint = fp, summary = settings)
  }) %>% bindEvent(input$run, guide_run(), ignoreNULL = FALSE)

  # A fresh Simulate / Try-it run clears any Compare override.
  observeEvent(main_sim_result(), {
    compare_result_override(NULL)
    results_display(list(kind = "main", label = NULL))
  }, ignoreInit = FALSE)

  sim_result <- reactive({
    override <- compare_result_override()
    if (!is.null(override)) override else main_sim_result()
  })

  # The experiment is the guided run that just finished. Pair it with the
  # baseline captured above instead of simulating the 2-bay line again.
  # ignoreInit is omitted on purpose: this observer is created after the
  # initial simulation is scheduled, and skipping its first execution would
  # drop the guided run when that run is the first time the observer fires.
  observeEvent(sim_result(), {
    pair <- isolate(guide_pair())
    if (is.null(pair)) return()
    # Only pair when this result came from Simulate, not a Compare import.
    if (!is.null(isolate(compare_result_override()))) return()
    guide_pair(NULL)
    experiment <- sim_result()
    if (!comparison_same(experiment$params, pair$experiment_params)) return()
    two_set_study$load_pair(pair$baseline, experiment, comparison_guided_labels)
  })

  # Import-from-Compare dropdown labels. Defaults stay "Baseline" / "Experiment";
  # custom titles become "Baseline: <title>" / "Experiment: <title>".
  # Titles that already start with those prefixes (guided Try-it labels) stay as-is
  # so we never emit "Baseline: Baseline: …".
  compare_import_choice_labels <- function(titles) {
    titles <- as.character(titles[1:2])
    defaults <- as.character(comparison_default_labels[1:2])
    with_role <- function(title, default, role) {
      if (identical(title, default)) return(role)
      if (startsWith(title, paste0(role, ":"))) return(title)
      paste0(role, ": ", title)
    }
    labs <- c(
      with_role(titles[[1]], defaults[[1]], "Baseline"),
      with_role(titles[[2]], defaults[[2]], "Experiment")
    )
    stats::setNames(c("1", "2"), labs)
  }
  compare_set_choices <- reactive({
    titles <- tryCatch(two_set_study$titles(), error = function(e) comparison_default_labels)
    compare_import_choice_labels(titles)
  })
  compare_has_results <- reactive({
    r <- tryCatch(two_set_study$results(), error = function(e) NULL)
    !is.null(r) && !is.null(r$runs) && length(r$runs) >= 2L
  })
  # Draft params exist once Compare has initialized (visit or import-main).
  compare_has_params <- reactive({
    d <- tryCatch(two_set_study$draft(), error = function(e) NULL)
    !is.null(d) && !inherits(d, "error") && !is.null(d$one) && !is.null(d$two)
  })

  # Keep the imported set selected across renderUI refreshes (choice labels change
  # when titles edit). Values stay "1"/"2".
  import_params_selection <- reactiveVal("1")
  import_results_selection <- reactiveVal("1")
  observeEvent(input$import_params_set, {
    v <- as.character(input$import_params_set %||% "1")
    if (v %in% c("1", "2")) import_params_selection(v)
  }, ignoreNULL = TRUE)
  observeEvent(input$import_results_set, {
    v <- as.character(input$import_results_set %||% "1")
    if (v %in% c("1", "2")) import_results_selection(v)
  }, ignoreNULL = TRUE)

  params_import_note <- reactiveVal(NULL)
  output$params_import_compare_ui <- renderUI({
    choices <- compare_set_choices()
    ready <- isTRUE(compare_has_params())
    selected <- isolate(import_params_selection())
    if (!selected %in% c("1", "2")) selected <- "1"
    tagList(
      div(
        class = "d-flex flex-wrap gap-3 align-items-end",
        selectInput(
          "import_params_set",
          "Compare set",
          choices = choices,
          selected = selected,
          width = "22rem"
        ),
        actionButton(
          "import_params_compare",
          "Import settings",
          class = "btn-primary",
          disabled = !ready
        )
      ),
      if (!ready) {
        p(class = "small mb-0 mt-2 results-context-hint",
          "Open Compare two sets once so both parameter sets are available to import.")
      }
    )
  })
  output$params_import_compare_note <- renderUI({
    note <- params_import_note()
    if (is.null(note) || !nzchar(note)) return(NULL)
    div(class = "alert alert-success py-2 px-3 mb-0 mt-2", note)
  })
  observeEvent(input$import_params_compare, {
    d <- two_set_study$draft()
    if (inherits(d, "error") || is.null(d)) return()
    idx <- suppressWarnings(as.integer(import_params_selection() %||% input$import_params_set %||% "1"))
    if (!idx %in% c(1L, 2L)) idx <- 1L
    import_params_selection(as.character(idx))
    p <- if (identical(idx, 1L)) d$one else d$two
    apply_params_to_inputs(session, p)
    titles <- two_set_study$titles()
    params_import_note(sprintf(
      "Imported parameters from %s. Adjust anything you need, then Run simulation.",
      titles[[idx]]
    ))
  }, ignoreInit = TRUE)

  results_pages <- c(
    "results_capacity", "results_flow", "results_lead", "production_schedule"
  )
  output$results_context_bar <- renderUI({
    page <- input$main_page
    if (is.null(page) || !page %in% results_pages) return(NULL)
    choices <- compare_set_choices()
    ready <- isTRUE(compare_has_results())
    disp <- results_display()
    selected <- isolate(import_results_selection())
    if (!selected %in% c("1", "2")) selected <- "1"
    showing <- if (identical(disp$kind, "compare") && !is.null(disp$label)) {
      span(class = "results-context-showing",
           sprintf("Showing: %s from Compare", disp$label))
    } else {
      span(class = "results-context-showing", "Showing: latest Simulate run")
    }
    div(
      class = "results-context-bar",
      div(
        class = "d-flex flex-wrap gap-3 align-items-end",
        showing,
        selectInput(
          "import_results_set",
          "Import results from Compare",
          choices = choices,
          selected = selected,
          width = "22rem"
        ),
        actionButton(
          "import_results_compare",
          "Import results",
          class = "btn-primary",
          disabled = !ready
        )
      ),
      if (!ready) {
        div(class = "results-context-hint mt-1",
            "Run a comparison first to import either set into these Results pages.")
      }
    )
  })
  observeEvent(input$import_results_compare, {
    r <- two_set_study$results()
    if (is.null(r) || is.null(r$runs) || length(r$runs) < 2L) return()
    idx <- suppressWarnings(as.integer(import_results_selection() %||% input$import_results_set %||% "1"))
    if (!idx %in% c(1L, 2L)) idx <- 1L
    import_results_selection(as.character(idx))
    run <- r$runs[[idx]]
    titles <- two_set_study$titles()
    label <- titles[[idx]]
    p <- run$params
    fp <- params_fingerprint(p)
    settings <- format_run_settings(p)
    run_seq$main <- run_seq$main + 1L
    compare_result_override(stamp_run(run, p, run_seq$main, fingerprint = fp, summary = settings))
    results_display(list(kind = "compare", label = label))
    # Stay on the current Results page when possible; otherwise open Flow.
    page <- isolate(input$main_page)
    if (is.null(page) || !page %in% results_pages) {
      nav_select("main_page", "results_flow", session = session)
    }
  }, ignoreInit = TRUE)

  current_fingerprint <- reactive(params_fingerprint(build_params()))
  production_page_server("production_results",sim_result,current_fingerprint)
  main_settings_stale <- reactive({
    !identical(sim_result()$provenance$fingerprint, current_fingerprint())
  })
  observe({
    if (isTRUE(main_settings_stale()) && identical(isolate(sim_btn_state()), "done")) {
      set_sim_btn_state("idle")
    }
  })
  snapshot_is_stale <- function(comp) {
    fp <- attr(comp, "fingerprint")
    if (is.null(fp)) return(FALSE)
    !identical(fp, current_fingerprint()) || !identical(fp, sim_result()$provenance$fingerprint)
  }
  staffing_stale <- reactive({
    if (!isTRUE(staffing_trigger() > 0)) return(FALSE)
    snapshot_is_stale(comparison())
  })
  timing_stale <- reactive({
    if (!isTRUE(timing_trigger() > 0)) return(FALSE)
    snapshot_is_stale(timing_comparison())
  })
  output$main_stale_flag <- renderText(if (isTRUE(main_settings_stale())) "stale" else "fresh")
  output$staffing_stale_flag <- renderText(if (isTRUE(staffing_stale())) "stale" else "fresh")
  output$timing_stale_flag <- renderText(if (isTRUE(timing_stale())) "stale" else "fresh")
  output$main_stale_notice <- renderUI({
    if (!isTRUE(main_settings_stale())) return(NULL)
    provenance_warning("Settings changed. Click Run to update results.")
  })
  output$run_settings_body <- renderUI({
    prov <- sim_result()$provenance
    tagList(
      p(
        class = "mb-1",
        tags$strong(sprintf("Run %d", prov$run_number)),
        sprintf(" · %s", format(prov$run_at, "%Y-%m-%d %H:%M:%S"))
      ),
      p(class = "small mb-0", prov$summary)
    )
  })
  output$run_settings_text <- renderText({
    prov <- sim_result()$provenance
    paste0("Run ", prov$run_number, " · ", format(prov$run_at, "%Y-%m-%d %H:%M:%S"), " · ", prov$summary)
  })
  output$result_freshness_style <- renderUI({
    opacity <- function(stale) if (isTRUE(stale)) "0.45" else "1"
    main_ids <- c(
      "headline_kpis", "plot_down", "plot_wip", "plot_queue", "plot_cycle",
      "occupancy_summary_table", "labor_table", "reliability_losses",
      "reliability_table", "table_metrics"
    )
    css <- paste0("#", main_ids, "{opacity:", opacity(main_settings_stale()), ";}", collapse = "")
    css <- paste0(
      css,
      "#scenario_table_wrap{opacity:", opacity(staffing_stale()), ";}",
      "#timing_table_wrap{opacity:", opacity(timing_stale()), ";}"
    )
    tags$style(HTML(css))
  })
  output$staffing_warning <- renderUI({
    if (!isTRUE(staffing_stale())) return(NULL)
    provenance_warning(
      "Settings changed since this comparison was run. Rerun comparison.",
      "rerun_staffing"
    )
  })
  output$staffing_meta <- renderUI({
    if (!isTRUE(staffing_trigger() > 0)) return(NULL)
    p(class = "small text-muted mb-2", comparison_caption(comparison()))
  })
  output$timing_warning <- renderUI({
    if (!isTRUE(timing_stale())) return(NULL)
    provenance_warning(
      "Settings changed since this comparison was run. Rerun comparison.",
      "rerun_timing"
    )
  })
  output$timing_meta <- renderUI({
    if (!isTRUE(timing_trigger() > 0)) return(NULL)
    p(class = "small text-muted mb-2", comparison_caption(timing_comparison()))
  })

  output$kpi_throughput <- renderText({
    res <- sim_result()
    sprintf("%.1f", res$headline$throughput_per_hr_mean)
  })

  output$kpi_cycle <- renderText({
    res <- sim_result()
    sprintf("%.0f", res$headline$cycle_time_mean_mean)
  })

  # HPU = operators x observed hours / units completed. Reps with no
  # completions have infinite HPU, so the mean uses finite reps only.
  output$kpi_hpu <- renderText({
    res <- sim_result()
    hpu <- res$summaries$hpu
    hpu <- hpu[is.finite(hpu)]
    if (!length(hpu)) "n/a" else sprintf("%.2f", mean(hpu))
  })

  output$kpi_bottleneck <- renderText({
    res <- sim_result()
    lab <- station_labels[[res$bottleneck]]
    if (is.null(lab)) res$bottleneck else lab
  })

  # Mean shop-floor WIP across replications (arrival-based count).
  output$kpi_wip <- renderText({
    res <- sim_result()
    sprintf("%.1f", res$headline$avg_wip_mean)
  })

  output$occupancy_summary_table <- renderDT({
    res <- sim_result()
    per_rep <- res$utilization_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    occupancy_summary_dt(occupancy_summary_df(per_rep))
  }, fillContainer = FALSE)

  output$plot_queue <- sim_render_plot({
    res <- sim_result()
    station <- input$queue_station %||% "all"
    layout <- input$queue_layout %||% "overlay"
    df <- res$queue_timeseries %>%
      mutate(label = factor(label_station(resource), levels = unname(station_labels)),
             rep_station = paste(replication, resource, sep = "_"))
    plot_df <- if (!identical(station, "all")) dplyr::filter(df, resource == station) else df
    req(nrow(plot_df) > 0)
    # Chart: "Queues" — see queue_timeseries_plot() in plots_flow.R
    queue_timeseries_plot(df, station = station, layout = layout)
  })

  output$plot_wip <- sim_render_plot({
    res <- sim_result()
    # Chart: "WIP" — see wip_timeseries_plot() in plots_flow.R
    wip_timeseries_plot(res$wip_timeseries)
  })

  output$plot_down <- sim_render_plot({
    res <- sim_result()
    per_rep <- res$downtime_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    # Caption lives in the card HTML so it matches the waits-card note size.
    replication_bars(
      per_rep, "label", "downtime_fraction",
      x_label = "Downtime fraction",
      percent = TRUE,
      fill_values = station_colors,
      caption = NULL
    )
  })

  output$plot_cycle <- sim_render_plot({
    res <- sim_result()
    req(nrow(res$cycles) > 0)
    # Chart: "Cycle time" — see cycle_time_plot() in plots_flow.R
    cycle_time_plot(res$cycles)
  })

  output$plot_waits <- ggiraph::renderGirafe({
    res <- sim_result()
    req(nrow(res$station_waits) > 0)
    wait_reason_bars_interactive(res$station_waits)
  })

  output$reliability_losses <- renderDT({
    res <- sim_result()
    reliability_losses_dt(reliability_losses_df(res$downtime_by_rep))
  }, fillContainer = FALSE)

  output$table_metrics <- renderDT({
    res <- sim_result()
    tbl <- res$station_metrics %>%
      mutate(
        Station = label_station(station),
        `Occupancy %` = round(100 * avg_util_mean, 1),
        `Occupancy SD` = round(100 * avg_util_sd, 1),
        `Avg queue` = round(avg_queue_mean, 2),
        `Queue SD` = round(avg_queue_sd, 2),
        `Downtime %` = round(100 * downtime_fraction_mean, 1),
        `Downtime SD` = round(100 * downtime_fraction_sd, 1)
      ) %>%
      dplyr::select(Station, `Occupancy %`, `Occupancy SD`, `Avg queue`, `Queue SD`,
                    `Downtime %`, `Downtime SD`)
    datatable(tbl, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  write_download <- function(id, filename, table) {
    output[[id]] <- downloadHandler(
      filename = function() filename,
      content = function(file) readr::write_csv(table(), file)
    )
  }
  write_download("dl_queue", "queue_timeseries.csv", function() sim_result()$queue_timeseries)
  write_download("dl_wip", "wip_timeseries.csv", function() sim_result()$wip_timeseries)
  write_download("dl_util", "utilization_by_rep.csv", function() sim_result()$utilization_by_rep)
  write_download("dl_occupancy_reps", "utilization_by_rep.csv", function() sim_result()$utilization_by_rep)
  write_download("dl_cycle", "cycle_times.csv", function() sim_result()$cycle_times)
  write_download("dl_metrics", "station_metrics.csv", function() sim_result()$station_metrics)
  write_download("dl_down", "downtime_by_rep.csv", function() sim_result()$downtime_by_rep)
  write_download("dl_repairs", "repair_events.csv", function() sim_result()$repair_events)
  write_download("dl_ops", "operator_by_rep.csv", function() sim_result()$operator_by_rep)
  write_download("dl_waits_main", "station_waits_by_rep.csv", function() sim_result()$station_waits)
  write_download("dl_util_summary", "occupancy_summary.csv", function() {
    res <- sim_result()
    per_rep <- res$utilization_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    occupancy_summary_df(per_rep)
  })
  write_download("dl_operator_reps", "operator_by_rep.csv", function() sim_result()$operator_by_rep)
  write_download("dl_labor_summary", "operator_pool_summary.csv", function() {
    operator_pool_df(sim_result()$labor)
  })
  write_download("dl_reliability_reps", "downtime_by_rep.csv", function() sim_result()$downtime_by_rep)
  write_download("dl_reliability_summary", "reliability_losses_summary.csv", function() {
    reliability_losses_df(sim_result()$downtime_by_rep)
  })
  write_download("dl_down_summary", "downtime_mean_ci.csv", function() {
    res <- sim_result()
    per_rep <- res$downtime_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    rep_mean_ci(per_rep, "label", "downtime_fraction")
  })
  write_download("dl_params", "run_parameters.csv", function() sim_result()$run_parameters)
  output$dl_all <- downloadHandler(
    filename = function() paste0("automotive_line_sim_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      tmp <- tempfile("sim_export_")
      dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      write_sim_exports(sim_result(), dir = tmp, zip_path = file)
    }
  )
}
