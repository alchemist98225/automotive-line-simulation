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

    # Phone-only denser type for tornado / break (desktop stays 16).
    # Wait for plot width so the first paint uses the right size; execOnResize
    # stays FALSE so Updating… does not stick on reflow.
    analysis_phone_base_size <- function(output_id) {
      w <- session$clientData[[paste0("output_", session$ns(output_id), "_width")]]
      req(!is.null(w), is.finite(w), w > 0)
      if (w < 576) 12.5 else 16
    }

    output$break_plot <- analysis_render_plot({
      d <- data()
      req(!is.null(d))
      break_plot_sweep(
        d$break_sweep$summary,
        d$break_sweep$breakpoints,
        fill_threshold = d$break_sweep$fill_threshold %||% 0.95,
        base_size = analysis_phone_base_size("break_plot")
      )
    })

    output$tornado_plot <- analysis_render_plot({
      d <- data()
      req(!is.null(d))
      plot_tornado_pair(d$tornado$effects, base_size = analysis_phone_base_size("tornado_plot"))
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
