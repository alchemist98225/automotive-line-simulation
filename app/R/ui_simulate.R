# ui_simulate.R
# Simulate page chrome: guided example card and slim run-status banner
# (plus Run / Try it button CSS/JS embedded in the status bar UI).

guided_example_ui <- function() {
  tagList(
    tags$style(HTML("
      .guided-example {
        border: 1px solid #9ecae1;
        border-left: 5px solid #2c7fb8;
        background: #f0f7fc;
        border-radius: 0.5rem;
      }
      .guided-example summary {
        cursor: pointer;
        font-weight: 600;
        color: #1b4f72;
        list-style: none;
        display: flex;
        align-items: center;
        gap: 0.45rem;
        padding: 0.15rem 0;
      }
      .guided-example summary::-webkit-details-marker { display: none; }
      .guided-example summary .guided-label { font-size: 1.02rem; }
      #guided_example.bslib-card { flex: 0 0 auto; height: auto; width: 100%; }
    ")),
    card(
      id = "guided_example",
      class = "guided-example",
      fill = FALSE,
      card_body(
        fill = FALSE,
        tags$details(
          open = NA,
          tags$summary(
            tagList(
              bs_icon("lightbulb"),
              span(class = "guided-label", "Start here: guided example")
            )
          ),
          p(class = "mt-2 mb-2",
            "This app compares two ways to run the same four-station line: body shop, paint, final assembly, and inspection. Both get 10 vehicles an hour."),
          p(class = "mb-2",
            tags$strong("Baseline (already loaded). "),
            "Each station has its own operator, so four people in total. There is one paint booth."),
          p(class = "mb-2",
            tags$strong("Experiment. "),
            "Same four stations and the same four people, but they're shared. Two operators cover the upstream stations (body shop and paint), and two share the downstream stations (final assembly and inspection). Paint also gets a second booth."),
          p(class = "mb-2",
            "Click Try it to run the Experiment five times. The numbers above will change so you can see the difference."),
          div(
            class = "d-flex flex-wrap gap-2 mb-2",
            actionButton("guide_try", tagList(icon("play"), span(class = "sim-run-label", "Try it")), class = "btn-primary btn-sm sim-run-btn"),
            actionButton("guide_reset", "Reset to defaults", class = "btn-outline-secondary btn-sm")
          ),
          p(class = "mb-0",
            "Then open Results for a closer look at that run. On Results you can also import the saved 20-run Baseline or Experiment for a fuller picture.")
        )
      )
    )
  )
}

# Slim full-width status bar at the top of Simulate (fixed height; no layout jump).
sim_status_bar_ui <- function() {
  tagList(
    tags$style(HTML("
      .sim-status-bar {
        width: 100%;
        margin-bottom: 0.85rem;
      }
      .sim-status-bar .sim-progress-card {
        min-height: 3.4rem;
        height: 3.4rem;
        box-sizing: border-box;
        display: flex;
        align-items: center;
        gap: 1rem;
        padding: 0.45rem 1rem;
        margin: 0;
      }
      .sim-progress-card {
        background: #fff;
        border: 2px solid #2c7fb8;
        border-radius: 0.5rem;
        box-shadow: 0 4px 12px rgba(0,0,0,.10);
      }
      .sim-progress-card.sim-progress-idle {
        border-color: #adb5bd;
        box-shadow: none;
        background: #f8f9fa;
      }
      .sim-progress-title {
        font-size: 1.05rem;
        font-weight: 700;
        color: #1b4f72;
        margin: 0;
        white-space: nowrap;
      }
      .sim-progress-idle .sim-progress-title { color: #495057; }
      .sim-progress-detail {
        flex: 1 1 auto;
        min-width: 0;
        margin: 0 !important;
        font-size: 0.95rem;
      }
      .sim-status-bar .progress {
        width: 11rem;
        max-width: 30%;
        height: 1.15rem;
        margin: 0;
        flex: 0 0 auto;
      }
      .sim-progress-card .progress-bar {
        font-size: 0.75rem;
        font-weight: 600;
      }
      @media (max-width: 576px) {
        .sim-status-bar .sim-progress-card {
          height: auto;
          min-height: 3.4rem;
          flex-wrap: wrap;
          gap: 0.35rem 0.75rem;
          padding: 0.55rem 0.85rem;
        }
        .sim-status-bar .progress { width: 100%; max-width: none; }
      }
      /* Clickable KPI value boxes → Results chart pages */
      .kpi-nav-card {
        cursor: pointer;
        position: relative;
        border-radius: 0.5rem;
        transition: transform 0.12s ease, box-shadow 0.12s ease;
      }
      .kpi-nav-card:hover, .kpi-nav-card:focus {
        transform: translateY(-2px);
        box-shadow: 0 8px 18px rgba(0,0,0,.14);
        outline: none;
      }
      .kpi-nav-card .kpi-view-hint {
        display: block;
        font-size: 0.78rem;
        font-weight: 600;
        color: #1a1a1a;
        opacity: 0.7;
        margin-top: 0.15rem;
      }
      .kpi-nav-card:hover .kpi-view-hint { opacity: 1; text-decoration: underline; }

      /* Run / Try it button states (same pattern as Compare). */
      #run.sim-run-btn.btn-warning,
      #run.sim-run-btn.btn-warning:disabled,
      #guide_try.sim-run-btn.btn-warning,
      #guide_try.sim-run-btn.btn-warning:disabled {
        background-color: #e67e00 !important;
        border-color: #c96e00 !important;
        color: #ffffff !important;
        font-weight: 700;
        opacity: 1 !important;
      }
      #run.sim-run-btn.btn-success,
      #guide_try.sim-run-btn.btn-success {
        background-color: #198754 !important;
        border-color: #146c43 !important;
        color: #ffffff !important;
        font-weight: 700;
        opacity: 1 !important;
      }
      #run.sim-run-btn,
      #guide_try.sim-run-btn,
      #run.sim-run-btn.recalculating,
      #guide_try.sim-run-btn.recalculating,
      .shiny-busy #run.sim-run-btn,
      .shiny-busy #guide_try.sim-run-btn {
        opacity: 1 !important;
        filter: none !important;
      }
      /* Run simulation accordion header stands out from jump links. */
      #line_controls .accordion-item:first-child > .accordion-header .accordion-button {
        background: #198754;
        color: #fff;
        font-weight: 700;
      }
      #line_controls .accordion-item:first-child > .accordion-header .accordion-button:not(.collapsed) {
        background: #157347;
        color: #fff;
        box-shadow: none;
      }
      #line_controls .accordion-item:first-child > .accordion-header .accordion-button::after {
        filter: brightness(0) invert(1);
      }
    ")),
    div(
      id = "sim_progress_slot",
      class = "sim-status-bar",
      div(
        id = "sim_progress_idle",
        class = "sim-progress-card sim-progress-idle",
        div(class = "sim-progress-title", "Ready"),
        div(class = "sim-progress-detail text-dark",
            "Status for the next run appears here.")
      ),
      div(
        id = "sim_progress_panel",
        class = "d-none",
        div(
          class = "sim-progress-card",
          div(class = "sim-progress-title", "Simulation running"),
          div(class = "sim-progress-detail text-dark", ""),
          div(
            class = "progress",
            div(
              class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar",
              style = "width: 100%;",
              "In progress"
            )
          )
        )
      )
    )
  )
}
