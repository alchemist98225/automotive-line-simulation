# =============================================================================
# Automotive Manufacturing Line DES — Shiny portfolio demo
# Discrete-event simulation with simmer. Defaults are PLACEHOLDER values.
# =============================================================================
#
# WHERE THINGS LIVE (structural map — no behavior here)
# -------------------------------------------------------
# app.R              This file: libraries, ordered source(), ui layout, shinyApp()
# R/model.R          Simulation engine (params, run_line). Keep path stable for DOE.
# R/helpers_plots.R  Shared theme, ticks, density zoom, station labels/colors
# R/plots_simulate.R Overlay / replication density builders
# R/plots_flow.R     Occupancy helpers, operator/downtime bars, Queues / WIP / Cycle time
# R/plots_bottleneck.R  Wait-reason bars (ggiraph)
# R/ui_widgets.R     Plot cards, downloads, timing editors, KPI nav boxes
# R/ui_simulate.R    Guided example + Simulate run-status banner / button JS
# R/ui_css_js.R      Page header CSS/JS
# R/ui_pages.R       Simulate / Parameters / Results pages / About nav_panel builders
# R/server_main.R    Main server() wiring (KPIs, plots, downloads, Try-it)
# R/comparison_*.R   Compare-two-sets logic / plots / UI / server
# R/production_*.R   Production-schedule plots + page module
# R/optimize_*.R     Optimize page (GA core + UI/server)
# docs/CODE_MAP.md   File → purpose → key functions; chart-title → function index
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(ggtext) # declared so shinyapps.io installs it; the shift caption wraps to the plot width
library(ggiraph) # interactive hover tooltips on bottleneck wait bars
library(DT)
library(zip)   # declared so shinyapps.io installs it for Download-all (zip)
library(patchwork) # Analysis page multi-panel charts

# Explicit load order (also safe under Shiny auto-source of R/ by name).
# model.R path must stay app/R/model.R — scripts/doe_lib.R and Quarto depend on it.
source("R/model.R", local = TRUE)
source("R/helpers_plots.R", local = TRUE)
source("R/plots_simulate.R", local = TRUE)
source("R/plots_flow.R", local = TRUE)
source("R/plots_bottleneck.R", local = TRUE)
source("R/ui_widgets.R", local = TRUE)
source("R/ui_simulate.R", local = TRUE)
source("R/ui_css_js.R", local = TRUE)
source("R/ui_pages.R", local = TRUE)
source("R/comparison_logic.R", local = TRUE)
source("R/comparison_plots.R", local = TRUE)
source("R/comparison_ui.R", local = TRUE)
source("R/comparison_server.R", local = TRUE)
source("R/production_plots.R", local = TRUE)
source("R/production_page.R", local = TRUE)
source("R/optimize_doe.R", local = TRUE)
source("R/optimize_space.R", local = TRUE)
source("R/optimize_fitness.R", local = TRUE)
source("R/optimize_ga.R", local = TRUE)
source("R/optimize_plots.R", local = TRUE)
source("R/optimize_ui.R", local = TRUE)
source("R/optimize_server.R", local = TRUE)
source("R/analysis_data.R", local = TRUE)
source("R/analysis_plots.R", local = TRUE)
source("R/analysis_ui.R", local = TRUE)
source("R/analysis_server.R", local = TRUE)
source("R/server_main.R", local = TRUE)

ui <- page_navbar(
  id = "main_page",
  title = tags$span(
    class = "d-flex flex-column",
    tags$span("Automotive Line Simulator"),
    tags$span(
      class = "app-title-note",
      "Illustrative only — example inputs, not a calibrated plant."
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c7fb8",
    "navbar-bg" = "#1b4f72"
  ),
  # plotOutput cannot take clamp() directly. This overrides the 640px fallback.
  header = tagList(
    app_header_tags(),
    uiOutput("results_context_bar")
  ),
  fillable = FALSE,

  simulate_page_ui(),
  parameters_page_ui(),

  nav_menu(
    "Results",
    icon = bs_icon("bar-chart"),
    results_bottleneck_ui(),
    results_flow_ui(),
    results_lead_ui(),
    production_page_ui("production_results")
  ),

  comparison_page_ui("two_set_study"),
  optimize_page_ui("optimize_page"),
  analysis_page_ui("analysis_page"),
  about_page_ui()
)

shinyApp(ui, server)
