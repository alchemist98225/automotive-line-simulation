# analysis_server.R
# Render precomputed Analysis charts. No simulation calls.

analysis_page_server <- function(id = "analysis_page") {
  moduleServer(id, function(input, output, session) {
    # Load once. A reactive() that re-reads on every plot invalidation was fine
    # for correctness but kept plots in .recalculating longer than needed.
    data <- shiny::reactiveVal(NULL)
    observe({
      if (!is.null(isolate(data()))) return()
      loaded <- tryCatch(
        load_analysis_precomputed(),
        error = function(e) {
          showNotification(paste("Analysis data missing:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      data(loaded)
    })

    # Precomputed charts: skip redraw-on-resize so Updating… does not stick
    # when the phone layout reflows after first paint.
    analysis_render_plot <- function(expr) {
      shiny::renderPlot(
        substitute(expr),
        res = 96,
        quoted = TRUE,
        env = parent.frame(),
        alt = "Analysis chart",
        execOnResize = FALSE
      )
    }

    output$break_plot <- analysis_render_plot({
      d <- data()
      req(!is.null(d))
      break_plot_sweep(
        d$break_sweep$summary,
        d$break_sweep$breakpoints,
        fill_threshold = d$break_sweep$fill_threshold %||% 0.95
      )
    })

    output$tornado_plot <- analysis_render_plot({
      d <- data()
      req(!is.null(d))
      plot_tornado_pair(d$tornado$effects)
    })

    output$mmc_plot <- analysis_render_plot({
      d <- data()
      req(!is.null(d))
      plot_mmc_check(d$validation$mmc_summary)
    })

    output$littles_plot <- analysis_render_plot({
      d <- data()
      req(!is.null(d))
      plot_littles(d$validation$littles_reps, d$validation$littles_summary)
    })

    output$warmup_plot <- analysis_render_plot({
      d <- data()
      req(!is.null(d))
      plot_welch_warmup(
        d$warmup$smooth,
        d$warmup$recommend,
        current_warmup = d$warmup$current_warmup %||% 300
      )
    })
  })
}
