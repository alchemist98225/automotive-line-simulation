# optimize_ui.R
# Optimize nav page: GA search over queues, zone ops, and paint capacity.
# Sidebar mirrors Simulate: run controls on top, then bslib accordion panels.

optimize_page_ui <- function(id = "optimize_page") {
  ns <- NS(id)
  nav_panel(
    title = "Optimize",
    value = "optimize",
    icon = bs_icon("sliders"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        id = ns("optimize_sidebar"),
        title = "Search setup",
        width = 320,
        open = "desktop",
        # Match Simulate's green first-accordion / run-button styling.
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
          .opt-progress-card {
            background: #fff;
            border: 2px solid #2c7fb8;
            border-radius: 0.5rem;
            box-shadow: 0 4px 12px rgba(0,0,0,.10);
            padding: 0.45rem 0.75rem;
            margin-top: 0.5rem;
          }
          .opt-progress-card.opt-progress-idle {
            border-color: #adb5bd;
            box-shadow: none;
            background: #f8f9fa;
          }
          .opt-progress-title {
            font-size: 1.05rem;
            font-weight: 700;
            color: #1b4f72;
            margin: 0 0 0.25rem 0;
          }
          .opt-progress-idle .opt-progress-title { color: #495057; }
          .opt-progress-card .progress {
            height: 1.15rem;
            margin: 0.35rem 0 0.15rem 0;
          }
        ", ns("optimize_controls"), ns("run")))),
        # Run + Cancel at top (Simulate puts Run inside the first accordion;
        # Optimize keeps Cancel+progress visible while Search size stays open).
        actionButton(
          ns("run"),
          tagList(icon("play"), span(class = "sim-run-label", "Run optimize")),
          class = "btn-primary w-100 mb-2 sim-run-btn"
        ),
        actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger w-100 mb-2"),
        uiOutput(ns("progress_ui")),
        accordion(
          id = ns("optimize_controls"),
          open = "search_size",
          multiple = TRUE,
          accordion_panel(
            title = tagList(bs_icon("sliders"), " Search size"),
            value = "search_size",
            p(class = "small text-muted mb-2",
              "Population × generations × replications. Sequential on one core."),
            div(
              class = "d-flex flex-wrap gap-2 mb-2",
              actionButton(ns("preset_quick"), "Quick (~2 min)", class = "btn-outline-primary btn-sm"),
              actionButton(ns("preset_thorough"), "Thorough", class = "btn-outline-secondary btn-sm")
            ),
            numericInput(ns("pop"), "Population", value = 10, min = 4, max = 48, step = 1),
            numericInput(ns("gen"), "Generations", value = 6, min = 2, max = 40, step = 1),
            numericInput(ns("reps"), "Replications (search)", value = 2, min = 1, max = 20, step = 1),
            numericInput(ns("elite_reps"), "Elite replications", value = 15, min = 2, max = 40, step = 1),
            numericInput(ns("seed"), "Seed", value = 42, min = 1, step = 1)
          ),
          accordion_panel(
            title = "Search setup",
            value = "search_setup",
            p(class = "small text-muted mb-2",
              "Baseline for the throughput floor and cost deltas."),
            selectInput(
              ns("baseline_source"),
              "Baseline",
              choices = c(
                "Current Parameters" = "current",
                "Guided balanced set" = "guided",
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
      # Main
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
