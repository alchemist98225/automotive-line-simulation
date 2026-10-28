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
            numericInput("body_mean", "Mean automatic cycle (min)", value = 6, min = 0.5, max = 60, step = 0.5),
            h6("Paint"),
            numericInput("paint_cap", "Capacity (bays)", value = 1, min = 1, max = 20, step = 1),
            numericInput("paint_mean", "Mean automatic cycle (min)", value = 8, min = 0.5, max = 60, step = 0.5),
            h6("Final assembly"),
            numericInput("fa_cap", "Capacity (bays)", value = 2, min = 1, max = 20, step = 1),
            numericInput("fa_mean", "Mean automatic cycle (min)", value = 7, min = 0.5, max = 60, step = 0.5),
            h6("Inspection"),
            numericInput("insp_cap", "Capacity (bays)", value = 1, min = 1, max = 20, step = 1),
            numericInput("insp_mean", "Mean automatic cycle (min)", value = 3, min = 0.5, max = 60, step = 0.5)
          )
        ),
        h5("Operators & reliability"),
        selectInput("staffing", "Operator assignment", c("Two shared zones"="zones", "One shared team"="shared", "Dedicated by station"="dedicated")),
        helpText("Zone A serves body and paint; Zone B serves assembly and inspection."),
        uiOutput("operator_controls"),
        checkboxInput("reliability", "Enable equipment failures", TRUE),
        numericInput("repair_staff", "Repair technicians", 1, min=1, step=1),
        checkboxInput("noncyclic", "Enable random support tasks", TRUE),
        accordion(accordion_panel("Manual work & reliability by station", uiOutput("station_controls"))),
        actionButton("compare", "Compare staffing scenarios", class="w-100"),
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
          p("highest bay occupancy; bottleneck candidate")
        )
      ),

      layout_columns(
        card(
          card_header("Bay occupancy"),
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

  nav_panel(title="Staffing comparison",
    card(card_header("Staffing experiments"),
      p("Uses the current station settings. Intervals show uncertainty in each scenario's mean throughput, not individual shifts. Multiple replications are needed. Scenarios reuse base seeds; random draws are not synchronized by event."),
      DTOutput("scenario_table")),
    card(card_header("Operator utilization"), DTOutput("labor_table")),
    card(card_header("Reliability inputs and observed failures"), DTOutput("reliability_table"))),
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

Operators load a bay, leave during automatic processing, and return to unload.
The bay remains occupied throughout. Shared teams serve assigned stations FIFO.
Support tasks arrive at exponential intervals and use the same operators.

Failures interrupt automatic processing, which resumes after repair. Machines
within a station are interchangeable: this is pooled automatic capacity, not
tracking an individual vehicle to a particular physical machine. Failure clocks
advance in calendar up-time (including idle time), pause during their own repair,
and use exponential up and repair durations. Technician queues add downtime.

Buffers are unlimited; physical blocking, walking time, shifts, and scrap are
not implemented. Bay occupancy identifies a bottleneck candidate; confirm it
by testing whether improving that station increases system throughput.
      ")
    )
  )
)

server <- function(input, output, session) {

  output$operator_controls <- renderUI({
    switch(input$staffing,
      zones=tagList(numericInput("op_a","Zone A operators",2,min=1,step=1),numericInput("op_b","Zone B operators",2,min=1,step=1)),
      shared=numericInput("op_all","Shared team operators",4,min=1,step=1),
      dedicated=tagList(lapply(names(station_labels),function(n) numericInput(paste0("op_",n),paste(station_labels[[n]],"operators"),1,min=1,step=1))))
  })
  output$station_controls <- renderUI({
    tagList(lapply(seq_len(nrow(default_params()$stations)),function(i) {
      st <- default_params()$stations[i,]; n <- st$name
      tagList(h6(station_labels[[n]]),
        numericInput(paste0("load_",n),"Manual load (min)",st$load_min,min=0,step=.25),
        numericInput(paste0("unload_",n),"Manual unload (min)",st$unload_min,min=0,step=.25),
        numericInput(paste0("mtbf_",n),"Mean up-time between failures (min)",if(is.finite(st$mtbf_min)) st$mtbf_min else 10000,min=1),
        numericInput(paste0("mttr_",n),"Mean active repair (min; 0 disables)",st$mttr_min,min=0),
        numericInput(paste0("interval_",n),"Mean support-task interval (min)",st$task_interval_min,min=1),
        numericInput(paste0("every_",n),"Support task every N units (0 = random)",0,min=0,step=1),
        numericInput(paste0("task_",n),"Support-task duration (min)",st$task_duration_min,min=0))
    }))
  })
  # Build params from sidebar inputs
  build_params <- reactive({
    p <- list(
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
    get_input <- function(id) {req(!is.null(input[[id]])); input[[id]]}
    p$repair_technicians <- input$repair_staff
    p$operators <- switch(input$staffing,
      zones=tibble(pool=c("upstream","downstream"),count=c(get_input("op_a"),get_input("op_b"))),
      shared=tibble(pool="shared",count=get_input("op_all")),
      dedicated=tibble(pool=names(station_labels),count=map_dbl(names(station_labels),~get_input(paste0("op_",.x)))))
    p$stations <- p$stations %>% mutate(
      operator_pool=switch(input$staffing,zones=c("upstream","upstream","downstream","downstream"),shared=rep("shared",4),dedicated=name),
      load_min=map_dbl(name,~get_input(paste0("load_",.x))),
      unload_min=map_dbl(name,~get_input(paste0("unload_",.x))),
      mtbf_min=if(input$reliability) map_dbl(name,~get_input(paste0("mtbf_",.x))) else Inf,
      mttr_min=map_dbl(name,~get_input(paste0("mttr_",.x))),
      task_interval_min=if(input$noncyclic) map_dbl(name,~get_input(paste0("interval_",.x))) else Inf,
      task_duration_min=if(input$noncyclic) map_dbl(name,~get_input(paste0("task_",.x))) else 0,
      task_every_n=map_dbl(name,~get_input(paste0("every_",.x))))
    validate(need(p$warmup_min<p$sim_length_min,"Warmup must be shorter than the simulation."))
    p
  })
  comparison <- reactive({
    p <- build_params()
    alternatives <- list(p,p,p,p)
    alternatives[[2]]$operators$count[[1]] <- alternatives[[2]]$operators$count[[1]]+1L
    alternatives[[3]]$operators$count[[nrow(p$operators)]] <- alternatives[[3]]$operators$count[[nrow(p$operators)]]+1L
    alternatives[[4]]$operators <- tibble(pool="shared",count=sum(p$operators$count))
    alternatives[[4]]$stations$operator_pool <- "shared"
    withProgress(message="Comparing staffing…",run_scenarios(tibble(
      scenario=c("Current","Add one to first team","Add one to last team","Same headcount, shared team"),params=alternatives)))
  }) %>% bindEvent(input$compare)
  output$scenario_table <- renderDT({comparison() %>% dplyr::select(scenario,comparison) %>% unnest(comparison) %>% datatable(rownames=FALSE,options=list(dom="t")) %>% formatRound(columns=3:10,digits=2)})
  output$labor_table <- renderDT({sim_result()$labor %>% group_by(resource) %>% summarise(utilization=mean(avg_util),queue=mean(avg_queue),.groups="drop") %>% datatable(rownames=FALSE,options=list(dom="t")) %>% formatPercentage("utilization",1)})

  output$reliability_table <- renderDT({
    r <- sim_result()
    r$params$stations %>% dplyr::select(name,mtbf_min,mttr_min) %>%
      left_join(r$failure_events %>% count(station,name="observed_failures"),by=c("name"="station")) %>%
      mutate(observed_failures=replace_na(observed_failures,0L)) %>%
      datatable(rownames=FALSE,options=list(dom="t"))
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
