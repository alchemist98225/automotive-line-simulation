# optimize_ui.R
# Optimize nav page: GA search over queues, zone ops, and paint capacity.
# Page chrome mirrors Simulate: status bar above the sidebar, then a green
# "Run optimize" accordion that holds the run button and Search size inputs.

optimize_status_bar_ui <- function(ns) {
  # Reuse Simulate's .sim-status-bar / .sim-progress-card classes (defined in
  # ui_simulate.R) so fonts, border, and spacing match exactly.
  div(
    class = "px-3 pt-3",
    div(
      id = ns("opt_progress_slot"),
      class = "sim-status-bar",
      uiOutput(ns("progress_ui"))
    )
  )
}

optimize_page_ui <- function(id = "optimize_page") {
  ns <- NS(id)
  nav_panel(
    title = "Optimize",
    value = "optimize",
    icon = bs_icon("sliders"),
    optimize_status_bar_ui(ns),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        id = ns("optimize_sidebar"),
        title = "Optimizer settings",
        width = 320,
        open = "desktop",
        # Green first accordion + run-button states (same pattern as #line_controls / #run).
        tags$style(HTML(sprintf("
          #%1$s .accordion-item:first-child > .accordion-header .accordion-button {
            background: #198754;
            color: #fff;
            font-weight: 700;
          }
          #%1$s .accordion-item:first-child > .accordion-header .accordion-button:not(.collapsed) {
            background: #157347;
            color: #fff;
            box-shadow: none;
          }
          #%1$s .accordion-item:first-child > .accordion-header .accordion-button::after {
            filter: brightness(0) invert(1);
          }
          #%2$s.sim-run-btn.btn-warning,
          #%2$s.sim-run-btn.btn-warning:disabled {
            background-color: #e67e00 !important;
            border-color: #c96e00 !important;
            color: #ffffff !important;
            font-weight: 700;
            opacity: 1 !important;
          }
          #%2$s.sim-run-btn.btn-success {
            background-color: #198754 !important;
            border-color: #146c43 !important;
            color: #ffffff !important;
            font-weight: 700;
            opacity: 1 !important;
          }
          #%2$s.sim-run-btn,
          #%2$s.sim-run-btn.recalculating,
          .shiny-busy #%2$s.sim-run-btn {
            opacity: 1 !important;
            filter: none !important;
          }
        ", ns("optimize_controls"), ns("run")))),
        accordion(
          id = ns("optimize_controls"),
          open = "run_optimize",
          multiple = TRUE,
          accordion_panel(
            title = tagList(bs_icon("play-fill"), " Run optimize"),
            value = "run_optimize",
            actionButton(
              ns("run"),
              tagList(icon("play"), span(class = "sim-run-label", "Run optimize")),
              class = "btn-primary w-100 mb-3 sim-run-btn"
            ),
            actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger w-100 mb-3"),
            p(class = "small text-muted mb-2",
              "Population × generations × replications. Sequential on one core."),
            div(
              class = "d-flex flex-wrap gap-2 mb-2",
              actionButton(ns("preset_quick"), "Quick (~5 min)", class = "btn-outline-primary btn-sm"),
              actionButton(ns("preset_thorough"), "Thorough", class = "btn-outline-secondary btn-sm")
            ),
            numericInput(ns("pop"), "Population", value = 10, min = 4, max = 48, step = 1),
            numericInput(ns("gen"), "Generations", value = 6, min = 2, max = 40, step = 1),
            numericInput(ns("reps"), "Replications (search)", value = 2, min = 1, max = 20, step = 1),
            numericInput(ns("elite_reps"), "Recheck replications", value = 15, min = 2, max = 40, step = 1),
            numericInput(ns("elite_n"), "Recheck top candidates", value = 3, min = 1, max = 10, step = 1),
            numericInput(ns("seed"), "Seed", value = 42, min = 1, step = 1)
          ),
          accordion_panel(
            title = "Search setup",
            value = "search_setup",
            p(class = "small text-muted mb-2",
              "Starts from Scenario 2 settings at 10 veh/hr demand. Lowest hours per vehicle without losing throughput."),
            selectInput(
              ns("baseline_source"),
              "Baseline",
              choices = c(
                "Current Parameters" = "current",
                "Shared 2-paint @ 10/hr" = "guided",
                "Compare Baseline" = "compare_1",
                "Compare Experiment" = "compare_2"
              ),
              selected = "guided"
            ),
            uiOutput(ns("baseline_note"))
          ),
          accordion_panel(
            title = "Costs (illustrative)",
            value = "costs",
            p(class = "small text-muted mb-2",
              "Labor-hour equivalents per clock-hour. Ranking only."),
            numericInput(ns("capacity_cost"), "Paint capacity cost", value = 1, min = 0, step = 0.1, width = "100%"),
            numericInput(ns("queue_cost"), "Queue spot cost", value = 0.1, min = 0, step = 0.05, width = "100%")
          ),
          accordion_panel(
            title = "Knob bounds",
            value = "knob_bounds",
            p(class = "small text-muted mb-2",
              "Queues are vehicle spots before each station; operators are per zone."),
            uiOutput(ns("bounds_ui"))
          )
        )
      ),
      div(
        class = "optimize-main",
        card(
          card_header("How this search works"),
          card_body(
            p(
              class = "mb-0",
              "Each generation keeps the best setup found so far and breeds new candidates by mixing and lightly mutating queue, staffing, and paint-capacity choices. ",
              "Search uses a few replications per candidate; the top few are re-checked with more replications under shared random numbers so the final table is fair to the baseline."
            )
          )
        ),
        card(
          card_header("Convergence"),
          card_body(
            plotOutput(ns("convergence"), height = "480px"),
            p(class = "small text-muted mb-1",
              "Fitness = HPU + capacity and queue cost per unit + penalty for throughput below baseline. Mean uses feasible setups only."),
            uiOutput(ns("status_line"))
          )
        ),
        accordion(
          id = ns("extra_panels"),
          open = FALSE,
          accordion_panel(
            "Diversity",
            plotOutput(ns("diversity"), height = "360px")
          )
        ),
        card(
          card_header("Results"),
          card_body(
            uiOutput(ns("results_block"))
          )
        )
      )
    )
  )
}
