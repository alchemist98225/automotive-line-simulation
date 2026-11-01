# server_main.R
# Main Shiny server: Simulate run lifecycle, KPIs, Results plot/table outputs,
# downloads, guided Try-it, and wiring to Compare / Production modules.
# Chart drawing lives in plots_*.R; this file binds inputs → those functions.

server <- function(input, output, session) {
  # idle | running | done — drives Run simulation / Try it button colors.
  sim_btn_state <- reactiveVal("idle")
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
    session$sendCustomMessage("simProgress", list(show = FALSE))
    if (isTRUE(done)) set_sim_btn_state("done")
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
          # its own toast stays hidden by CSS.
          tryCatch(
            withProgress(message = "", value = 0.25, run_line_sim(base_params)),
            finally = hide_sim_progress()
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
  two_set_study <- comparison_page_server("two_set_study", build_params, reactive(input$main_page))

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
  output$labor_table <- renderDT({sim_result()$labor %>% group_by(resource) %>% summarise(utilization=mean(avg_util),queue=mean(avg_queue),.groups="drop") %>% datatable(rownames=FALSE,options=list(dom="t")) %>% formatPercentage("utilization",1)})

  output$reliability_table <- renderDT({
    r <- sim_result()
    r$params$stations %>% dplyr::select(name,mtbf_min,mttr_min) %>%
      left_join(r$failure_events %>% count(station,name="observed_failures"),by=c("name"="station")) %>%
      mutate(observed_failures=replace_na(observed_failures,0L)) %>%
      datatable(rownames=FALSE,options=list(dom="t"))
  })
  # Run on button click; also fire once on load so KPIs appear immediately.
  # The fixed progress panel is opened from a click handler in JS so it paints
  # before this reactive blocks the session. hide_sim_progress() clears it.
  sim_result <- reactive({
    p <- build_params()
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

  # The experiment is the guided run that just finished. Pair it with the
  # baseline captured above instead of simulating the 2-bay line again.
  # ignoreInit is omitted on purpose: this observer is created after the
  # initial simulation is scheduled, and skipping its first execution would
  # drop the guided run when that run is the first time the observer fires.
  observeEvent(sim_result(), {
    pair <- isolate(guide_pair())
    if (is.null(pair)) return()
    guide_pair(NULL)
    experiment <- sim_result()
    if (!comparison_same(experiment$params, pair$experiment_params)) return()
    two_set_study$load_pair(pair$baseline, experiment, comparison_guided_labels)
  })

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
      "headline_kpis", "plot_util", "plot_labor", "plot_down", "plot_wip",
      "plot_queue", "plot_cycle", "labor_table", "reliability_table", "table_metrics"
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

  output$occupancy_summary_table <- renderDT({
    res <- sim_result()
    per_rep <- res$utilization_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    occupancy_summary_dt(occupancy_summary_df(per_rep))
  })

  output$plot_labor <- sim_render_plot({
    res <- sim_result()
    req(nrow(res$operator_by_rep) > 0)
    pools <- sub("^operator_", "", res$operator_by_rep$resource)
    levels <- if (all(pools %in% c("upstream", "downstream"))) {
      c("upstream", "downstream")
    } else if (all(pools %in% names(station_labels))) {
      names(station_labels)[names(station_labels) %in% unique(pools)]
    } else unique(pools)
    df <- res$operator_by_rep %>%
      mutate(label = factor(sub("^operator_", "", resource), levels = levels))
    replication_bars(
      df, "label", "avg_util",
      x_label = "Operator utilization",
      percent = TRUE
    )
  })

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
    replication_bars(
      per_rep, "label", "downtime_fraction",
      x_label = "Downtime fraction",
      percent = TRUE,
      fill_values = station_colors
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
    res$downtime_by_rep %>%
      group_by(station) %>%
      summarise(
        `Active repair minutes` = mean(downtime_min),
        `Waiting for repair minutes` = mean(waiting_repair_min),
        `Total unavailable minutes` = mean(total_downtime_min),
        `Unavailable fraction` = mean(total_downtime_fraction),
        .groups = "drop"
      ) %>%
      mutate(station = label_station(station)) %>%
      datatable(rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
      formatRound(columns = 2:4, digits = 2) %>%
      formatPercentage("Unavailable fraction", 1)
  })

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
  write_download("dl_labor_summary", "operator_util_mean_ci.csv", function() {
    res <- sim_result()
    df <- res$operator_by_rep %>%
      mutate(label = sub("^operator_", "", resource))
    rep_mean_ci(df, "label", "avg_util")
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
