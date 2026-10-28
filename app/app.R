# =============================================================================
# Automotive Manufacturing Line DES — Shiny portfolio demo
# Discrete-event simulation with simmer. Defaults are PLACEHOLDER values.
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(DT)
library(zip)   # declared so shinyapps.io installs it for Download-all (zip)

source("R/model.R", local = TRUE)

# Station display labels
station_labels <- c(
  body_shop      = "Body shop",
  paint          = "Paint",
  final_assembly = "Final assembly",
  inspection     = "Inspection"
)

station_colors <- c(
  "Body shop"      = "#2c7fb8",
  "Paint"          = "#d95f0e",
  "Final assembly" = "#31a354",
  "Inspection"     = "#756bb1"
)

label_station <- function(resource) {
  lab <- station_labels[as.character(resource)]
  ifelse(is.na(lab), as.character(resource), unname(lab))
}

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
          ),
          accordion_panel(
            "Download data",
            helpText("CSV of the tidy data behind each plot/table, including seeds and parameters."),
            downloadButton("dl_queue", "Queue timeseries", class = "btn-outline-secondary btn-sm w-100 mb-1"),
            downloadButton("dl_wip", "WIP timeseries", class = "btn-outline-secondary btn-sm w-100 mb-1"),
            downloadButton("dl_util", "Utilization by rep", class = "btn-outline-secondary btn-sm w-100 mb-1"),
            downloadButton("dl_cycle", "Cycle times", class = "btn-outline-secondary btn-sm w-100 mb-1"),
            downloadButton("dl_metrics", "Station metrics", class = "btn-outline-secondary btn-sm w-100 mb-1"),
            downloadButton("dl_params", "Run parameters", class = "btn-outline-secondary btn-sm w-100 mb-1"),
            downloadButton("dl_all", "Download all (zip)", class = "btn-secondary btn-sm w-100 mt-2")
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
          card_header("Station utilization (all replications)"),
          plotOutput("plot_util", height = 280),
          full_screen = TRUE
        ),
        card(
          card_header("Queue length over time (all reps + loess)"),
          plotOutput("plot_queue", height = 280),
          full_screen = TRUE
        )
      ),

      layout_columns(
        card(
          card_header("WIP over time (all reps + loess)"),
          plotOutput("plot_wip", height = 280),
          full_screen = TRUE
        ),
        card(
          card_header("Cycle time distribution"),
          plotOutput("plot_cycle", height = 280),
          full_screen = TRUE
        )
      ),

      card(
        card_header("Station metrics"),
        DTOutput("table_metrics"),
        full_screen = TRUE
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

### Plots across replications

Queue and WIP charts show **every replication** as a faint line, with a bold
**loess** trend overlaid. Resource monitors are step functions at irregular
event times; before smoothing they are resampled onto a common time grid
(step carry-forward) so the aggregate is well-defined. Utilization shows
per-replication points plus the across-rep mean. Use the **Download data**
panel to export the tidy CSVs behind every plot (and a zip of all of them).

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
      grid_dt_min      = 2,
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

  # Utilization: per-rep jittered points (low alpha) + mean point with ±SD interval
  output$plot_util <- renderPlot({
    res <- sim_result()
    per_rep <- res$utilization_by_rep %>%
      mutate(label = label_station(station),
             label = factor(label, levels = unname(station_labels)))
    means <- res$station_metrics %>%
      mutate(label = label_station(station),
             label = factor(label, levels = unname(station_labels)))

    ggplot() +
      geom_jitter(
        data = per_rep,
        aes(x = label, y = avg_util),
        width = 0.12, height = 0, alpha = 0.35, size = 2.2, color = "#4a4a4a"
      ) +
      geom_errorbar(
        data = means,
        aes(x = label,
            ymin = pmax(0, avg_util_mean - avg_util_sd),
            ymax = pmin(1, avg_util_mean + avg_util_sd)),
        width = 0.15, linewidth = 0.6, color = "#2c7fb8"
      ) +
      geom_point(
        data = means,
        aes(x = label, y = avg_util_mean),
        size = 3.2, color = "#2c7fb8"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(x = NULL, y = "Utilization",
           subtitle = "Faint points = each replication; blue = mean ± SD") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
  })

  # Queue: every rep × station as a faint line; bold loess trend per station
  output$plot_queue <- renderPlot({
    res <- sim_result()
    df <- res$queue_timeseries %>%
      mutate(
        label = label_station(resource),
        label = factor(label, levels = unname(station_labels)),
        rep_station = paste(replication, resource, sep = "_")
      )
    req(nrow(df) > 0)

    ggplot(df, aes(x = time, y = queue, color = label)) +
      geom_line(
        aes(group = rep_station),
        alpha = 0.12, linewidth = 0.4, na.rm = TRUE
      ) +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = TRUE,
        linewidth = 1.15,
        alpha = 0.18,
        span = 0.35,
        na.rm = TRUE
      ) +
      scale_color_manual(values = station_colors) +
      labs(
        x = "Time (minutes)", y = "Queue length", color = "Station",
        subtitle = "Faint lines = each replication (resampled grid); bold = loess + ribbon"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # WIP over time: faint per-rep lines + bold loess
  output$plot_wip <- renderPlot({
    res <- sim_result()
    df <- res$wip_timeseries
    req(nrow(df) > 0)

    ggplot(df, aes(x = time, y = wip)) +
      geom_line(
        aes(group = replication),
        alpha = 0.15, linewidth = 0.45, color = "#b35900", na.rm = TRUE
      ) +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = TRUE,
        linewidth = 1.2,
        color = "#b35900",
        fill = "#fdcc8a",
        alpha = 0.25,
        span = 0.35,
        na.rm = TRUE
      ) +
      labs(
        x = "Time (minutes)", y = "WIP (vehicles)",
        subtitle = "Faint lines = each replication; bold = loess + ribbon"
      ) +
      theme_minimal(base_size = 13)
  })

  # Cycle time: faint per-rep densities + bold pooled density + mean line
  output$plot_cycle <- renderPlot({
    res <- sim_result()
    req(nrow(res$cycles) > 0)
    mean_ct <- mean(res$cycles$cycle_time, na.rm = TRUE)
    n_reps  <- n_distinct(res$cycles$replication)

    p <- res$cycles %>%
      ggplot(aes(x = cycle_time)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30, fill = "#41b6c4", color = "white", alpha = 0.55
      )

    # Per-rep densities only when we have enough points per rep
    if (n_reps > 1 && nrow(res$cycles) >= n_reps * 5) {
      p <- p +
        geom_density(
          aes(group = replication),
          alpha = 0.25, linewidth = 0.4, color = "#7fcdbb", na.rm = TRUE
        )
    }

    p +
      geom_density(linewidth = 1.15, color = "#253494", na.rm = TRUE) +
      geom_vline(xintercept = mean_ct, linetype = "dashed", color = "#253494") +
      labs(
        x = "Cycle time (minutes)", y = "Density",
        subtitle = paste0(
          "Faint = per-rep density; bold = pooled | mean = ",
          round(mean_ct, 1), " min (dashed)"
        )
      ) +
      theme_minimal(base_size = 13)
  })

  output$table_metrics <- renderDT({
    res <- sim_result()
    tbl <- res$station_metrics %>%
      mutate(
        Station = label_station(station),
        `Util %` = round(100 * avg_util_mean, 1),
        `Util SD` = round(100 * avg_util_sd, 1),
        `Avg queue` = round(avg_queue_mean, 2),
        `Queue SD` = round(avg_queue_sd, 2)
      ) %>%
      dplyr::select(Station, `Util %`, `Util SD`, `Avg queue`, `Queue SD`)
    datatable(tbl, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  # ---- Downloads -------------------------------------------------------------
  output$dl_queue <- downloadHandler(
    filename = function() "queue_timeseries.csv",
    content = function(file) {
      readr::write_csv(sim_result()$queue_timeseries, file)
    }
  )
  output$dl_wip <- downloadHandler(
    filename = function() "wip_timeseries.csv",
    content = function(file) {
      readr::write_csv(sim_result()$wip_timeseries, file)
    }
  )
  output$dl_util <- downloadHandler(
    filename = function() "utilization_by_rep.csv",
    content = function(file) {
      readr::write_csv(sim_result()$utilization_by_rep, file)
    }
  )
  output$dl_cycle <- downloadHandler(
    filename = function() "cycle_times.csv",
    content = function(file) {
      readr::write_csv(sim_result()$cycle_times, file)
    }
  )
  output$dl_metrics <- downloadHandler(
    filename = function() "station_metrics.csv",
    content = function(file) {
      readr::write_csv(sim_result()$station_metrics, file)
    }
  )
  output$dl_params <- downloadHandler(
    filename = function() "run_parameters.csv",
    content = function(file) {
      readr::write_csv(sim_result()$run_parameters, file)
    }
  )
  output$dl_all <- downloadHandler(
    filename = function() {
      paste0("automotive_line_sim_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      tmp <- tempfile("sim_export_")
      dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      write_sim_exports(sim_result(), dir = tmp, zip_path = file)
    }
  )
}

shinyApp(ui, server)
