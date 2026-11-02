# analysis_server.R
# Render precomputed Analysis charts. No simulation calls.

analysis_page_server <- function(id = "analysis_page") {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      tryCatch(
        load_analysis_precomputed(),
        error = function(e) {
          showNotification(paste("Analysis data missing:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    output$break_plot <- sim_render_plot({
      d <- data()
      req(!is.null(d))
      break_plot_sweep(
        d$break_sweep$summary,
        d$break_sweep$breakpoints,
        fill_threshold = d$break_sweep$fill_threshold %||% 0.95
      )
    })

    output$tornado_plot <- sim_render_plot({
      d <- data()
      req(!is.null(d))
      plot_tornado_pair(d$tornado$effects)
    })

    output$mmc_plot <- sim_render_plot({
      d <- data()
      req(!is.null(d))
      plot_mmc_check(d$validation$mmc_summary)
    })

    output$littles_plot <- sim_render_plot({
      d <- data()
      req(!is.null(d))
      plot_littles(d$validation$littles_reps, d$validation$littles_summary)
    })

    output$warmup_plot <- sim_render_plot({
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
