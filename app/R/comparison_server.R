# comparison_server.R
# Compare page server module: run both sets, stale detection, plot/table
# outputs, downloads, and guided Try-it pairing.

comparison_page_server <- function(id, main_params, active_page) {
  moduleServer(id, function(input, output, session) {
    one <- parameter_editor_server("one")
    two <- parameter_editor_server("two")
    initialized <- reactiveVal(FALSE)
    results <- reactiveVal(NULL)
    error <- reactiveVal(NULL)
    # Titles are display state. Editing one does not rerun the simulation
    # and does not mark a saved comparison stale.
    titles <- reactiveVal(comparison_default_labels)
    # idle | running | done — drives the compare status banner and run button.
    run_state <- reactiveVal("idle")
    observeEvent(input$title_one, {
      updated <- sanitize_set_labels(c(input$title_one, isolate(titles())[[2]]))
      if (!identical(updated, isolate(titles()))) titles(updated)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(input$title_two, {
      updated <- sanitize_set_labels(c(isolate(titles())[[1]], input$title_two))
      if (!identical(updated, isolate(titles()))) titles(updated)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    palette <- reactive(comparison_palette(titles()))
    present_sets <- function(df, labels = titles()) {
      if (is.null(df) || !nrow(df) || !"set" %in% names(df)) return(df)
      mapped <- stats::setNames(as.character(labels), names(comparison_colors))
      df %>% mutate(set = unname(mapped[as.character(.data$set)]))
    }
    import_main <- function() {
      p <- isolate(main_params()) %>% normalize_params() %>% validate_params()
      one$set_params(p)
      two$set_params(p)
      initialized(TRUE)
      error(NULL)
    }
    observeEvent(active_page(), {
      if (identical(active_page(), "compare_sets") && !initialized()) import_main()
    }, ignoreNULL = TRUE)
    observeEvent(input$import, import_main())
    observeEvent(input$clone, {
      two$set_params(one$params())
      error(NULL)
    })
    observeEvent(input$match_reps, {
      p <- two$params()
      p$n_replications <- one$params()$n_replications
      two$set_params(p)
      error(NULL)
    })
    draft <- reactive({
      tryCatch(list(one = one$params(), two = two$params()), error = function(e) e)
    })
    dirty <- reactive({
      r <- results()
      d <- draft()
      !is.null(r) && (inherits(d, "error") || !comparison_same(d$one, r$params[[1]]) || !comparison_same(d$two, r$params[[2]]))
    })
    observe({
      # Inputs changed after a completed run: allow another run without looking "done".
      if (isTRUE(dirty()) && identical(isolate(run_state()), "done")) run_state("idle")
    })
    output$run_button <- renderUI({
      state <- run_state()
      if (identical(state, "running")) {
        actionButton(
          session$ns("run"),
          tagList(icon("spinner", class = "fa-spin"), span(class = "compare-run-label", "Running…")),
          class = "btn-warning compare-run-btn",
          disabled = TRUE
        )
      } else if (identical(state, "done") && !isTRUE(dirty())) {
        actionButton(
          session$ns("run"),
          tagList(icon("check"), span(class = "compare-run-label", "Done – run again")),
          class = "btn-success compare-run-btn"
        )
      } else {
        actionButton(
          session$ns("run"),
          tagList(icon("play"), span(class = "compare-run-label", "Run comparison")),
          class = "btn-primary compare-run-btn"
        )
      }
    })
    output$compare_progress <- renderUI({
      state <- run_state()
      # Clear the JS running class once Shiny can paint again.
      if (!identical(state, "running")) {
        session$sendCustomMessage(
          "two-set-compare-running",
          list(running = FALSE)
        )
      }
      card <- if (identical(state, "running")) {
        div(
          id = session$ns("compare_progress_card"),
          class = "compare-progress-card compare-progress-running",
          div(
            class = "compare-progress-title",
            icon("spinner", class = "fa-spin"),
            span("Comparison running")
          ),
          div(class = "compare-progress-detail", "Running both parameter sets…"),
          div(
            class = "progress",
            div(
              class = "progress-bar progress-bar-striped progress-bar-animated bg-primary",
              role = "progressbar", style = "width:100%", "In progress"
            )
          )
        )
      } else if (identical(state, "done") && !isTRUE(dirty())) {
        div(
          id = session$ns("compare_progress_card"),
          class = "compare-progress-card compare-progress-done",
          div(
            class = "compare-progress-title",
            icon("check-circle"),
            span("Comparison complete")
          ),
          div(class = "compare-progress-detail", "Done. Both parameter sets finished — charts below are current.")
        )
      } else {
        div(
          id = session$ns("compare_progress_card"),
          class = "compare-progress-card compare-progress-idle",
          div(class = "compare-progress-title", "Ready"),
          div(class = "compare-progress-detail", "Press Run comparison when both sets are ready.")
        )
      }
      div(class = "compare-status-bar", card)
    })
    bind_runs <- function(r, field) {
      map2_dfr(r$runs, names(comparison_colors), ~ mutate(.x[[field]], set = .y))
    }
    run_both <- function() {
      d <- draft()
      if (inherits(d, "error")) {
        error(conditionMessage(d))
        run_state("idle")
        return()
      }
      error(NULL)
      run_state("running")
      session$sendCustomMessage("two-set-compare-running", list(running = TRUE))
      # Flush UI so the amber button and banner paint before the sim blocks.
      tryCatch(session$requestFlush(), error = function(cond) NULL)
      succeeded <- FALSE
      tryCatch({
        r <- withProgress(message = "Running both parameter sets…", value = 0, {
          a <- run_line_sim(d$one)
          incProgress(0.5, detail = paste(titles()[[1]], "complete"))
          b <- run_line_sim(d$two)
          incProgress(0.5, detail = paste(titles()[[2]], "complete"))
          list(
            params = list(a$params, b$params),
            runs = list(a, b),
            at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
          )
        })
        results(r)
        succeeded <- TRUE
      }, error = function(e) error(conditionMessage(e)))
      # Invalid input returns before this, and a failed sim leaves succeeded FALSE,
      # so open editors stay open when the run does not update.
      if (isTRUE(succeeded)) {
        run_state("done")
        collapse_editors(scroll = TRUE)
      } else {
        run_state("idle")
      }
    }
    # Close both editors after a saved comparison. Scroll only when this page
    # is the one on screen, so a guided run started from Simulate does not
    # pull that page down to a hidden tab.
    collapse_editors <- function(scroll = TRUE) {
      one$collapse()
      two$collapse()
      if (!isTRUE(scroll)) return(invisible())
      session$sendCustomMessage(
        "two-set-collapse-editors",
        list(
          ids = c(session$ns("one-editor_panels"), session$ns("two-editor_panels")),
          scrollId = session$ns("saved_results")
        )
      )
    }
    load_pair <- function(run_one, run_two, labels = comparison_guided_labels) {
      labels <- sanitize_set_labels(labels)
      one$set_params(run_one$params)
      two$set_params(run_two$params)
      titles(labels)
      updateTextInput(session, "title_one", value = labels[[1]])
      updateTextInput(session, "title_two", value = labels[[2]])
      error(NULL)
      initialized(TRUE)
      results(list(
        params = list(run_one$params, run_two$params),
        runs = list(run_one, run_two),
        at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      ))
      collapse_editors(scroll = identical(isolate(active_page()), "compare_sets"))
      invisible(results())
    }
    # Value 0 is the button's initial state, not a request to run.
    observeEvent(input$run, {
      if (is.null(input$run) || input$run < 1) return()
      run_both()
    }, ignoreNULL = TRUE)
    observeEvent(input$rerun, {
      if (is.null(input$rerun) || input$rerun < 1) return()
      run_both()
    }, ignoreNULL = TRUE)
    output$status <- renderUI({
      d <- draft()
      r <- results()
      if (!is.null(error())) {
        return(div(class = "alert alert-danger", paste("Comparison not updated:", error())))
      }
      if (inherits(d, "error")) {
        return(div(class = "alert alert-warning", paste("Check the parameter inputs:", conditionMessage(d))))
      }
      design <- c("shift_length_min", "n_shifts", "warmup_min", "n_replications", "planned_rate")
      d$one$planned_rate<-60/d$one$arrival_mean_min;d$two$planned_rate<-60/d$two$arrival_mean_min
      mismatches <- design[map_lgl(design, ~ !identical(as.numeric(d$one[[.x]]), as.numeric(d$two[[.x]])))]
      tagList(
        if (dirty()) {
          div(
            class = "alert alert-warning provenance-warning d-flex flex-wrap align-items-center justify-content-between gap-2",
            role = "alert",
            div(
              class = "d-flex align-items-center gap-2",
              bs_icon("exclamation-triangle-fill"),
              span("Settings changed since this comparison was run. Rerun comparison. Charts below still show the previous saved run.")
            ),
            actionButton(session$ns("rerun"), "Rerun comparison", class = "btn-warning btn-sm")
          )
        },
        if (is.null(r) && !identical(run_state(), "running")) {
          div(
            class = "alert alert-info compare-idle-alert",
            "Both sets are ready to edit. Press Run comparison to generate results."
          )
        },
        if (length(mismatches)) {
          div(
            class = "alert alert-info",
            paste(
              "Different run conditions:",
              paste(unname(comparison_globals[mismatches]), collapse = ", "),
              ". Interpret differences in that context."
            )
          )
        }
      )
    })
    output$result_fade <- renderUI({
      opacity <- if (isTRUE(dirty())) "0.45" else "1"
      tags$style(HTML(sprintf(
        "#%s{opacity:%s;transition:opacity 0.15s ease;}",
        session$ns("saved_results"), opacity
      )))
    })
    output$draft_diff <- renderUI({
      d <- draft()
      if (inherits(d, "error")) {
        return(div(
          class = "alert alert-light border py-2 mb-3 comparison-draft-diff",
          "Fix the parameter inputs to compare settings."
        ))
      }
      comparison_diff_table(d$one, d$two, titles())
    })
    output$differences <- renderDT({
      d <- draft()
      req(!inherits(d, "error"))
      datatable(comparison_diff(d$one, d$two, titles()), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    table_for <- function(field) {
      r <- results()
      req(!is.null(r))
      present_sets(bind_runs(r, field))
    }
    highlight_state <- reactive({
      r <- results()
      req(!is.null(r))
      comparison_rank_changes(r$runs, n = comparison_highlight_n)
    })
    output$change_story <- renderUI({
      r <- results()
      req(!is.null(r))
      div(
        class = "comparison-change-story mb-3",
        h4("What changed most"),
        p(class = "mb-0", comparison_change_caption(r$runs, highlight_state()$scores))
      )
    })
    output$highlights <- renderUI({
      state <- highlight_state()
      shown <- state$shown
      if (!nrow(shown)) {
        return(p("These two sets match on throughput, lead time, WIP, bay occupancy, and queues."))
      }
      n <- nrow(shown)
      intro <- if (n == 1L) {
        "The metric below moved the most between the two sets."
      } else {
        "The metrics below moved the most between the two sets."
      }
      plots <- lapply(seq_len(n), function(i) {
        tagList(
          comparison_highlight_frame(session$ns(paste0("highlight_plot_", i))),
          downloadButton(
            session$ns(paste0("dl_highlight_", i)),
            "Download this chart's data",
            class = "btn-outline-secondary btn-sm mb-3"
          )
        )
      })
      tagList(
        p(class = "mb-1", intro),
        p(
          class = "mb-1",
          "Rug ticks are every replication, and a vertical line means no spread. Dashed lines are normal curves, and the x-axis shows the central 95% of values."
        ),
        p(
          class = "mb-2",
          "Ranked by percent change from the first set. When that mean is smaller than the spread across replications, the change is measured against the second set's mean instead. Every replication is drawn."
        ),
        layout_columns(col_widths = if (n == 1L) 12 else 6, !!!plots)
      )
    })
    highlight_subtitle <- function(row) {
      comparison_highlight_subtitle(row$mean_1, row$mean_2, row$delta)
    }
    local({
      for (i in seq_len(comparison_highlight_n)) {
        local({
          ii <- i
          output[[paste0("highlight_plot_", ii)]] <- renderPlot({
            state <- highlight_state()
            req(nrow(state$shown) >= ii)
            row <- state$shown[ii, ]
            df <- state$frame %>%
              filter(.data$metric_id == row$metric_id) %>%
              mutate(set = titles()[.data$set_index])
            comparison_highlight_plot(
              df,
              x_label = row$x_label,
              subtitle = highlight_subtitle(row),
              palette = palette(),
              percent = isTRUE(row$percent)
            ) +
              labs(title = row$label)
          })
          output[[paste0("dl_highlight_", ii)]] <- downloadHandler(
            filename = function() sprintf("comparison_largest_change_%d.csv", ii),
            content = function(file) {
              r <- isolate(results())
              if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
              state <- comparison_rank_changes(r$runs, n = comparison_highlight_n)
              if (ii > nrow(state$shown)) stop("That chart is not in this comparison.", call. = FALSE)
              readr::write_csv(
                comparison_highlight_export(state, isolate(titles()), state$shown$metric_id[[ii]]),
                file
              )
            }
          )
        })
      }
    })
    output$kpis <- renderUI({
      r <- results()
      req(!is.null(r))
      metrics <- c(
        throughput_per_hr = "Throughput / hour",
        cycle_time_mean = "Mean lead time / min",
        avg_wip = "Average WIP",
        hpu = "HPU (labor-h/unit)"
      )
      layout_columns(
        fill = FALSE,
        col_widths = c(3, 3, 3, 3),
        !!!map(names(metrics), function(m) {
          values <- map_dbl(r$runs, ~ mean(.x$summaries[[m]], na.rm = TRUE))
          delta <- values[[2]] - values[[1]]
          show <- function(x) if (is.finite(x)) sprintf("%.2f", x) else "Unavailable"
          value_box(
            title = metrics[[m]],
            value = tags$div(
              style = "font-size:1.1rem",
              tags$div(style = "color:#2474A5", paste0(titles()[[1]], ": ", show(values[[1]]))),
              tags$div(style = "color:#D46B25", paste0(titles()[[2]], ": ", show(values[[2]])))
            ),
            p(paste0(titles()[[2]], " − ", titles()[[1]], ": ", show(delta))),
            p(if (is.finite(values[[1]]) && values[[1]] != 0 && is.finite(delta)) {
              sprintf("Change: %+.1f%%", 100 * delta / values[[1]])
            } else {
              "Percent change unavailable"
            })
          )
        })
      )
    })
    output$captured_settings <- renderUI({
      r <- results()
      req(!is.null(r))
      div(
        class = "small text-muted mb-3",
        p(class = "mb-1", tags$strong(paste("Results captured:", r$at))),
        p(class = "mb-1", tags$strong(titles()[[1]]), " · ", format_run_settings(r$params[[1]])),
        p(class = "mb-0", tags$strong(titles()[[2]]), " · ", format_run_settings(r$params[[2]]))
      )
    })
    ridge_frame <- function(output_name, plot_name, field, column, fallback_n) {
      output[[output_name]] <- renderUI({
        r <- results()
        n <- if (is.null(r)) fallback_n else comparison_ridge_n(r$runs[[1]][[field]][[column]])
        comparison_ridge_plot(session$ns(plot_name), n)
      })
    }
    ridge_frame(
      "occupancy_frame", "occupancy", "utilization_by_rep", "station",
      nrow(default_params()$stations)
    )
    ridge_frame(
      "labor_frame", "labor", "operator_by_rep", "resource",
      nrow(default_params()$operators)
    )
    ridge_frame(
      "downtime_frame", "downtime", "downtime_by_rep", "station",
      nrow(default_params()$stations)
    )
    output$distribution <- renderPlot({
      labels <- c(
        throughput_per_hr = "Throughput (vehicles/hour)",
        cycle_time_mean = "Mean lead time (min)",
        avg_wip = "Average WIP (vehicles)",
        hpu = "HPU (labor-h/unit)"
      )
      metric <- input$metric %||% "throughput_per_hr"
      df <- table_for("summaries")
      validate(need(any(is.finite(df[[metric]])), "No completed observations for this metric."))
      caption_extra <- if (identical(metric, "hpu")) {
        " HPU = (operators × observation hours) / units completed. Lower is better."
      } else {
        ""
      }
      comparison_densities(
        df %>% mutate(value = .data[[metric]]),
        "set", "value", labels[[metric]],
        paste0(
          "Overlaid densities of every replication. Rug ticks are each run. A vertical line means that set did not vary.",
          caption_extra
        ),
        percent = FALSE, palette = palette(), bounds = c(0, Inf)
      )
    })
    output$occupancy <- renderPlot({
      df <- table_for("utilization_by_rep") %>%
        mutate(label = factor(label_station(station), levels = unname(station_labels)))
      comparison_densities(
        df, "label", "avg_util",
        x_label = "Bay occupancy",
        subtitle = "One panel per station. Rug ticks are every replication. A vertical line means that set did not vary.",
        percent = TRUE,
        palette = palette()
      )
    })
    output$labor <- renderPlot({
      df <- table_for("operator_by_rep")
      req(nrow(df) > 0)
      pools <- sub("^operator_", "", df$resource)
      levels <- if (all(pools %in% c("upstream", "downstream"))) {
        c("upstream", "downstream")
      } else if (all(pools %in% names(station_labels))) {
        names(station_labels)[names(station_labels) %in% unique(pools)]
      } else {
        unique(pools)
      }
      df <- df %>% mutate(label = factor(sub("^operator_", "", resource), levels = levels))
      comparison_densities(
        df, "label", "avg_util",
        x_label = "Operator utilization",
        subtitle = "One panel per pool. Rug ticks are every replication. A vertical line means that set did not vary.",
        percent = TRUE,
        palette = palette()
      )
    })
    output$downtime <- renderPlot({
      df <- table_for("downtime_by_rep") %>%
        mutate(label = factor(label_station(station), levels = unname(station_labels)))
      comparison_densities(
        df, "label", "downtime_fraction",
        x_label = "Active repair fraction",
        subtitle = "One panel per station. Rug ticks are every replication. A vertical line means that set did not vary.",
        percent = TRUE,
        palette = palette()
      )
    })
    output$cycles <- renderPlot({
      df <- table_for("cycle_times") %>%
        filter(is.finite(cycle_time)) %>%
        mutate(set = factor(set, levels = names(palette())))
      validate(need(nrow(df) > 0, "No completed vehicles in either set."))
      counts <- df %>% count(set)
      xlim <- density_xlim(df$cycle_time, level = 0.95, bounds = c(0, Inf))
      p <- ggplot(df, aes(x = cycle_time, fill = set, color = set))
      if (all(counts$n >= 2)) {
        p <- p + geom_density(alpha = 0.18, linewidth = 1, na.rm = TRUE)
      } else {
        p <- p + geom_histogram(position = "identity", alpha = 0.3, bins = 20)
      }
      p +
        scale_fill_manual(values = palette(), drop = FALSE, name = NULL) +
        scale_color_manual(values = palette(), drop = FALSE, name = NULL) +
        sparse_x_continuous(percent = FALSE, n = 4L) +
        coord_cartesian(xlim = xlim, clip = "off") +
        labs(
          x = "Vehicle lead time (min)",
          y = "Density",
          subtitle = "Pooled departures from every replication. Unfinished vehicles are excluded.",
          caption = "x-axis shows the central 95% of values"
        ) +
        theme_sim(legend.position = "bottom") +
        theme_plot_width_text() +
        theme_minor_ticks()
    })
    output$shift_output <- renderPlot({
      r <- results()
      req(!is.null(r))
      caps <- tibble::tibble(
        set = titles(),
        capacity = vapply(r$runs, function(run) theoretical_shift_capacity(run$params), numeric(1))
      )
      shift_output_plot(table_for("shift_summaries"), caps, palette())
    })
    output$backlog <- renderPlot({comparison_flow(table_for("release_timeseries"),"unreleased_backlog","Orders waiting for admission",palette=palette())})
    output$wip <- renderPlot({
      comparison_flow(table_for("wip_timeseries"), "wip", "WIP (vehicles)", palette = palette())
    })
    output$queues <- renderPlot({
      df <- table_for("queue_timeseries") %>%
        mutate(label = factor(label_station(resource), levels = unname(station_labels)))
      comparison_flow(df, "queue", "Queue (vehicles)", facet = "label", palette = palette())
    })
    summary_data <- reactive({
      table_for("summaries") %>%
        group_by(set) %>%
        summarise(
          replications = n(),
          throughput = mean(throughput_per_hr),
          throughput_se = sd(throughput_per_hr) / sqrt(n()),
          ci_half = if (n() > 1) qt(0.975, n() - 1) * throughput_se else NA_real_,
          throughput_lower = throughput - ci_half,
          throughput_upper = throughput + ci_half,
          mean_lead_time = mean(cycle_time_mean, na.rm = TRUE),
          mean_wip = mean(avg_wip),
          hpu = mean(hpu[is.finite(hpu)], na.rm = TRUE),
          .groups = "drop"
        )
    })
    output$summary <- renderDT({
      df <- summary_data()
      names(df) <- c(
        "Parameter set", "Replications", "Throughput / hour", "Standard error",
        "95% CI half-width", "95% CI lower", "95% CI upper", "Mean lead time / min",
        "Average WIP", "HPU (labor-hours / unit)"
      )
      datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
        formatRound(columns = 3:10, digits = 2)
    })
    output$stations <- renderDT({
      df <- table_for("station_metrics") %>%
        mutate(station = label_station(station)) %>%
        rename(
          Station = station,
          `Parameter set` = set,
          `Mean occupancy` = avg_util_mean,
          `Occupancy SD` = avg_util_sd,
          `Mean queue` = avg_queue_mean,
          `Queue SD` = avg_queue_sd,
          `Mean active repair fraction` = downtime_fraction_mean,
          `Repair fraction SD` = downtime_fraction_sd,
          `Mean repairs` = repairs_per_rep_mean
        )
      datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
        formatRound(columns = names(df)[vapply(df, is.numeric, logical(1))], digits = 3)
    })
    write_comparison_csv <- function(id, filename, field) {
      output[[id]] <- downloadHandler(
        filename = function() filename,
        content = function(file) {
          r <- isolate(results())
          if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
          readr::write_csv(present_sets(bind_runs(r, field), isolate(titles())), file)
        }
      )
    }
    write_comparison_csv("dl_shifts","comparison_shift_summaries.csv","shift_summaries")
    write_comparison_csv("dl_orders","comparison_order_ledger.csv","orders")
    write_comparison_csv("dl_summaries", "comparison_summaries.csv", "summaries")
    write_comparison_csv("dl_cycle", "comparison_cycle_times.csv", "cycle_times")
    write_comparison_csv("dl_util", "comparison_utilization_by_rep.csv", "utilization_by_rep")
    write_comparison_csv("dl_ops", "comparison_operator_by_rep.csv", "operator_by_rep")
    write_comparison_csv("dl_wip", "comparison_wip_timeseries.csv", "wip_timeseries")
    write_comparison_csv("dl_queue", "comparison_queue_timeseries.csv", "queue_timeseries")
    write_comparison_csv("dl_down", "comparison_downtime_by_rep.csv", "downtime_by_rep")
    write_comparison_csv("dl_repairs", "comparison_repair_events.csv", "repair_events")
    write_comparison_csv("dl_metrics", "comparison_station_metrics.csv", "station_metrics")
    output$dl_highlights <- downloadHandler(
      filename = function() "comparison_largest_changes.csv",
      content = function(file) {
        r <- isolate(results())
        if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
        state <- comparison_rank_changes(r$runs, n = comparison_highlight_n)
        readr::write_csv(comparison_highlight_export(state, isolate(titles())), file)
      }
    )
    output$download <- downloadHandler(
      filename = function() "two_parameter_sets.rds",
      content = function(file) {
        r <- isolate(results())
        if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
        saveRDS(r, file)
      }
    )
    list(
      one = one,
      two = two,
      results = results,
      draft = draft,
      dirty = dirty,
      summary = summary_data,
      titles = titles,
      load_pair = load_pair
    )
  })
}
