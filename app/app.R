# =============================================================================
# Automotive Manufacturing Line DES — Shiny portfolio demo
# Discrete-event simulation with simmer. Defaults are PLACEHOLDER values.
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(DT)

source("R/model.R", local = TRUE)

# Station display labels
station_labels <- c(
  body_shop      = "Body shop",
  paint          = "Paint",
  final_assembly = "Final assembly",
  inspection     = "Inspection"
)

ui <- page_navbar(
  title = "Automotive Line Simulator",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c7fb8",
    "navbar-bg" = "#1b4f72"
  ),
  fillable = TRUE,

  nav_panel(
    title = "Simulate",
    layout_sidebar(
      sidebar = sidebar(
        title = "Line parameters",
        width = 320,
        accordion(
          open = c("Arrivals & run", "Stations"),
          accordion_panel(
            "Arrivals & run",
            numericInput("arrival_mean", "Mean interarrival (min)",
                         value = 4, min = 0.5, max = 60, step = 0.5),
            numericInput("sim_length", "Sim length (min)",
                         value = 480, min = 60, max = 4800, step = 30),
            numericInput("warmup", "Warmup (min)",
                         value = 60, min = 0, max = 2400, step = 15),
            numericInput("n_reps", "Replications",
                         value = 5, min = 1, max = 30, step = 1),
            numericInput("seed", "Base seed",
                         value = 20261027, min = 1, step = 1)
          ),
          accordion_panel(
            "Stations",
            h6("Body shop"),
            numericInput("body_cap", "Capacity (bays)", value = 2, min = 1, max = 20, step = 1),
            numericInput("body_mean", "Mean cycle (min)", value = 6, min = 0.5, max = 60, step = 0.5),
            h6("Paint"),
            numericInput("paint_cap", "Capacity (bays)", value = 1, min = 1, max = 20, step = 1),
            numericInput("paint_mean", "Mean cycle (min)", value = 8, min = 0.5, max = 60, step = 0.5),
            h6("Final assembly"),
            numericInput("fa_cap", "Capacity (bays)", value = 2, min = 1, max = 20, step = 1),
            numericInput("fa_mean", "Mean cycle (min)", value = 7, min = 0.5, max = 60, step = 0.5),
            h6("Inspection"),
            numericInput("insp_cap", "Capacity (bays)", value = 1, min = 1, max = 20, step = 1),
            numericInput("insp_mean", "Mean cycle (min)", value = 3, min = 0.5, max = 60, step = 0.5)
          )
        ),
        actionButton("run", "Run simulation", class = "btn-primary w-100", icon = icon("play")),
        helpText("Defaults are placeholder example values — not real plant data.")
      ),

      # Main area
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Throughput",
          value = textOutput("kpi_throughput", inline = TRUE),
          showcase = bs_icon("speedometer2"),
          theme = "primary",
          p("vehicles / hour (mean across reps)")
        ),
        value_box(
          title = "Mean cycle time",
          value = textOutput("kpi_cycle", inline = TRUE),
          showcase = bs_icon("hourglass-split"),
          theme = "info",
          p("minutes")
        ),
        value_box(
          title = "Avg WIP",
          value = textOutput("kpi_wip", inline = TRUE),
          showcase = bs_icon("boxes"),
          theme = "warning",
          p("vehicles on the floor")
        ),
        value_box(
          title = "Bottleneck",
          value = textOutput("kpi_bottleneck", inline = TRUE),
          showcase = bs_icon("exclamation-triangle"),
          theme = "danger",
          p("highest utilization")
        )
      ),

      layout_columns(
        card(
          card_header("Station utilization"),
          plotOutput("plot_util", height = 280),
          full_screen = TRUE
        ),
        card(
          card_header("Queue length over time (rep 1)"),
          plotOutput("plot_queue", height = 280),
          full_screen = TRUE
        )
      ),

      layout_columns(
        card(
          card_header("Cycle time distribution"),
          plotOutput("plot_cycle", height = 280),
          full_screen = TRUE
        ),
        card(
          card_header("Station metrics"),
          DTOutput("table_metrics"),
          full_screen = TRUE
        )
      )
    )
  ),

  nav_panel(
    title = "About this model",
    card(
      card_header("About this model"),
      markdown("
This is a **discrete-event simulation** of a serial automotive manufacturing line,
built in R with [**simmer**](https://r-simmer.org/). Vehicles arrive to the line and
pass through four stations in order:

1. **Body shop** → 2. **Paint** → 3. **Final assembly** → 4. **Inspection**

Each station has a configurable number of parallel servers (bays) and a mean
processing time. Service times are drawn from exponential or normal distributions
(placeholders). Arrivals follow an exponential interarrival process.

The simulator runs multiple independent replications with fixed seeds for
reproducibility, discards a warmup period, and reports:

- Throughput (vehicles/hour)
- Cycle time (mean and distribution)
- Average work-in-process (WIP)
- Time-weighted utilization and queue length per station

### Important caveat

**All default numeric inputs are placeholder example values**, chosen so the
demo has a clear bottleneck (paint) and responsive run times. They are **not**
real plant data. Swap in your own stations, capacities, and time distributions
before using this for any decision.

### Stack

- R + simmer (DES engine)
- tidyverse (wrangling & ggplot2)
- Shiny + bslib (interactive UI)

This portfolio foundation is intentionally lean; polish (breakdowns, rework,
shifts, scrap, staffing) can be layered on later.
      ")
    )
  )
)

server <- function(input, output, session) {

  # Build params from sidebar inputs
  build_params <- reactive({
    list(
      sim_length_min   = input$sim_length,
      n_replications   = as.integer(input$n_reps),
      base_seed        = as.integer(input$seed),
      warmup_min       = input$warmup,
      arrival_mean_min = input$arrival_mean,
      stations = tribble(
        ~name,              ~capacity,              ~dist,         ~mean_min,        ~sd_min,
        "body_shop",        as.integer(input$body_cap),  "exponential", input$body_mean,  NA_real_,
        "paint",            as.integer(input$paint_cap), "normal",      input$paint_mean, 1.5,
        "final_assembly",   as.integer(input$fa_cap),    "normal",      input$fa_mean,    1.0,
        "inspection",       as.integer(input$insp_cap),  "exponential", input$insp_mean,  NA_real_
      )
    )
  })

  # Run on button click; also fire once on load so KPIs appear immediately
  sim_result <- reactive({
    withProgress(message = "Running simulation…", value = 0.3, {
      run_line_sim(build_params())
    })
  }) %>% bindEvent(input$run, ignoreNULL = FALSE)

  output$kpi_throughput <- renderText({
    res <- sim_result()
    sprintf("%.1f", res$headline$throughput_per_hr_mean)
  })

  output$kpi_cycle <- renderText({
    res <- sim_result()
    sprintf("%.0f", res$headline$cycle_time_mean_mean)
  })

  output$kpi_wip <- renderText({
    res <- sim_result()
    sprintf("%.1f", res$headline$avg_wip_mean)
  })

  output$kpi_bottleneck <- renderText({
    res <- sim_result()
    lab <- station_labels[[res$bottleneck]]
    if (is.null(lab)) res$bottleneck else lab
  })

  output$plot_util <- renderPlot({
    res <- sim_result()
    res$station_headline %>%
      mutate(
        resource = factor(resource, levels = names(station_labels)),
        label = station_labels[as.character(resource)]
      ) %>%
      ggplot(aes(x = label, y = avg_util_mean)) +
      geom_col(fill = "#2c7fb8", width = 0.7) +
      geom_errorbar(
        aes(ymin = pmax(0, avg_util_mean - avg_util_sd),
            ymax = pmin(1, avg_util_mean + avg_util_sd)),
        width = 0.2
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(x = NULL, y = "Utilization",
           subtitle = "Mean ± SD across replications") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
  })

  output$plot_queue <- renderPlot({
    res <- sim_result()
    res$resources_ts %>%
      mutate(
        resource = factor(resource, levels = names(station_labels)),
        label = station_labels[as.character(resource)]
      ) %>%
      ggplot(aes(x = time, y = queue, color = label)) +
      geom_step(linewidth = 0.7) +
      labs(x = "Time (minutes)", y = "Queue length", color = "Station",
           subtitle = "Illustrative single replication") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$plot_cycle <- renderPlot({
    res <- sim_result()
    req(nrow(res$cycles) > 0)
    mean_ct <- mean(res$cycles$cycle_time, na.rm = TRUE)
    res$cycles %>%
      ggplot(aes(x = cycle_time)) +
      geom_histogram(bins = 30, fill = "#41b6c4", color = "white") +
      geom_vline(xintercept = mean_ct, linetype = "dashed", color = "#253494") +
      labs(x = "Cycle time (minutes)", y = "Count",
           subtitle = paste0("Mean = ", round(mean_ct, 1), " min (dashed)")) +
      theme_minimal(base_size = 13)
  })

  output$table_metrics <- renderDT({
    res <- sim_result()
    tbl <- res$station_headline %>%
      mutate(
        Station = station_labels[resource],
        `Util %` = round(100 * avg_util_mean, 1),
        `Util SD` = round(100 * avg_util_sd, 1),
        `Avg queue` = round(avg_queue_mean, 2),
        `Queue SD` = round(avg_queue_sd, 2)
      ) %>%
      dplyr::select(Station, `Util %`, `Util SD`, `Avg queue`, `Queue SD`)
    datatable(tbl, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
}

shinyApp(ui, server)
