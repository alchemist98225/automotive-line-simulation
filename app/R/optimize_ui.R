# optimize_ui.R
# Optimize nav page: GA search over queues, zone ops, and paint capacity.

optimize_page_ui <- function(id = "optimize_page") {
  ns <- NS(id)
  nav_panel(
    title = "Optimize",
    value = "optimize",
    icon = bs_icon("sliders"),
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        open = "open",
        h4("Search setup"),
        p(
          class = "small text-muted",
          "A genetic algorithm tries staffing and queue setups, keeping throughput at least as high as the baseline while cutting labor-equivalent cost per unit."
        ),
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
        uiOutput(ns("baseline_note")),
        hr(),
        h5("Costs (illustrative)"),
        p(class = "small text-muted mb-1",
          "Labor-hour equivalents per clock-hour. Used only for ranking setups."),
        numericInput(ns("capacity_cost"), "Paint capacity cost", value = 1, min = 0, step = 0.1, width = "100%"),
        numericInput(ns("queue_cost"), "Queue spot cost", value = 0.1, min = 0, step = 0.05, width = "100%"),
        hr(),
        h5("Search size"),
        p(class = "small text-muted mb-1",
          "Population × generations × replications. Sequential on one core (shinyapps free tier)."),
        div(
          class = "d-flex flex-wrap gap-2 mb-2",
          actionButton(ns("preset_quick"), "Quick (~2 min)", class = "btn-outline-primary btn-sm"),
          actionButton(ns("preset_thorough"), "Thorough", class = "btn-outline-secondary btn-sm")
        ),
        numericInput(ns("pop"), "Population", value = 10, min = 4, max = 48, step = 1),
        numericInput(ns("gen"), "Generations", value = 6, min = 2, max = 40, step = 1),
        numericInput(ns("reps"), "Replications (search)", value = 2, min = 1, max = 20, step = 1),
        numericInput(ns("elite_reps"), "Elite replications", value = 15, min = 2, max = 40, step = 1),
        numericInput(ns("seed"), "Seed", value = 42, min = 1, step = 1),
        hr(),
        h5("Knob bounds"),
        p(class = "small text-muted mb-2",
          "Queues are vehicle spots before each station; operators are per zone."),
        uiOutput(ns("bounds_ui")),
        hr(),
        div(
          class = "d-flex flex-wrap gap-2",
          actionButton(ns("run"), "Run optimize", class = "btn-primary", width = "100%"),
          actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger", width = "100%")
        ),
        uiOutput(ns("progress_ui"))
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
