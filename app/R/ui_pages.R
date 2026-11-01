# ui_pages.R
# Top-level page UIs returned as nav_panel() objects and assembled in app.R.
# Pages: Simulate, Parameters, Bottleneck & capacity, Flow over time,
# Lead time & data, About this model.
# (Compare and Production Schedule live in comparison_ui.R / production_page.R.)

simulate_page_ui <- function() {
nav_panel(
  title = "Simulate",
  div(class = "px-3 pt-3", sim_status_bar_ui()),
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      id = "simulate_sidebar",
      title = "Line parameters",
      width = 320,
      open = "desktop",
      accordion(
        id = "line_controls",
        open = FALSE,
        multiple = TRUE,
        accordion_panel(
          title = tagList(bs_icon("play-fill"), " Run simulation"),
          value = "run_simulation",
          actionButton("run", tagList(icon("play"), span(class = "sim-run-label", "Run simulation")), class = "btn-primary w-100 mb-3 sim-run-btn"),
          numericInput("n_reps", "Number of replications",
                       value = 5, min = 1, max = 100, step = 1),
          numericInput("sim_length", "Shift duration (production minutes)",
                       value = 480, min = 1, max = 1440, step = 30),
          numericInput("warmup", "Initial settling period (minutes)",
                       value = 300, min = 0, max = 10080, step = 15),
          helpText("Settling happens first. Shifts are measured separately and run consecutively; nights and weekends are not simulated."),
          numericInput("n_shifts","Number of measured shifts",value=1,min=1,max=100,step=1),
          numericInput("planned_rate","Planned work rate (vehicles/hour)",value=15,min=.01,max=600,step=1),
          selectInput("order_spacing","Order spacing",c("Evenly scheduled"="fixed","Random (exponential)"="exponential"),selected="fixed"),
          selectInput("release_policy","Admission policy",c("When entrance space is available"="space","Limit total line WIP (CONWIP)"="conwip","Release all scheduled work"="scheduled"),selected="space"),
          numericInput("entry_buffer","Entrance waiting positions",3,min=0,step=1),
          numericInput("wip_limit","Maximum line WIP (CONWIP only)",20,min=1,step=1),
          helpText("Planned work waits outside production until admitted. Release-all allows an unlimited entrance queue. Interstation buffers apply to every policy."),
          numericInput("seed", "Base seed",
                       value = 20261027, min = 1, step = 1),
          actionButton("compare_timing", "Compare timing variability", class = "w-100"),
          actionButton("compare", "Compare staffing scenarios", class = "w-100"),
          helpText("Staffing comparison opens under Results → Bottleneck & capacity. Timing comparison and CSV downloads open under Results → Lead time & data."),
          helpText("Defaults are placeholder example values — not real plant data.")
        )
      ),
      div(
        class = "mt-2",
        p(class = "small mb-1 text-dark", "Station detail opens on the Parameters page:"),
        actionLink(
          "jump_capacity",
          tagList("Capacity", span(class = "jump-hint", "Bays and interstation waiting positions")),
          class = "param-jump"
        ),
        actionLink(
          "jump_cycle_times",
          tagList("Cycle times", span(class = "jump-hint", "Load, automatic, and unload timing")),
          class = "param-jump"
        ),
        actionLink(
          "jump_operators",
          tagList("Operators", span(class = "jump-hint", "Crew assignment and headcount")),
          class = "param-jump"
        ),
        actionLink(
          "jump_reliability",
          tagList("Failures & support", span(class = "jump-hint", "MTBF, MTTR, repair staff, support tasks")),
          class = "param-jump"
        )
      )
    ),

    uiOutput("result_freshness_style"),
    uiOutput("main_stale_notice"),
    div(
      id = "headline_kpis",
      layout_columns(
        fill = FALSE,
        col_widths = breakpoints(sm = 12, md = 6, xl = 3),
        kpi_nav_box(
          "production_schedule",
          title = "Throughput",
          value = textOutput("kpi_throughput", inline = TRUE),
          showcase = bs_icon("speedometer2"),
          theme = "primary",
          p("departures / hour (mean across reps)")
        ),
        kpi_nav_box(
          "results_lead",
          title = "Mean production lead time",
          value = textOutput("kpi_cycle", inline = TRUE),
          showcase = bs_icon("hourglass-split"),
          theme = "info",
          p("minutes")
        ),
        kpi_nav_box(
          "results_capacity",
          title = "HPU",
          value = textOutput("kpi_hpu", inline = TRUE),
          showcase = bs_icon("person-gear"),
          theme = "warning",
          p("labor hours per unit (lower is better)"),
          box_key = "hpu"
        ),
        kpi_nav_box(
          "results_capacity",
          title = "Bottleneck candidate",
          value = textOutput("kpi_bottleneck", inline = TRUE),
          showcase = bs_icon("exclamation-triangle"),
          theme = "danger",
          p("highest occupancy excluding blocked time")
        )
      )
    ),
    div(class = "mt-3", guided_example_ui())
  )
)
}

parameters_page_ui <- function() {
nav_panel(
  title = "Parameters",
  value = "parameters",
  div(
    class = "px-3 py-3",
    h3("Line parameters"),
    p(class = "mb-3",
      "Edit station capacity, timing, crews, and reliability here in a wider layout. ",
      "Arrivals, shifts, and replications stay under Run simulation on Simulate. The Run button still uses these values."),
    card(
      id = "params_capacity",
      class = "params-section mb-3",
      fill = FALSE,
      card_header("Capacity (bays)"),
      card_body(
        fill = FALSE,
        layout_columns(
          fill = FALSE,
          col_widths = c(3, 3, 3, 3),
          station_group("Body shop",
            numericInput("body_cap", "Capacity (bays)", value = 2, min = 1, max = 20, step = 1)),
          station_group("Paint",
            numericInput("paint_cap", "Capacity (bays)", value = 1, min = 1, max = 20, step = 1)),
          station_group("Final assembly",
            numericInput("fa_cap", "Capacity (bays)", value = 2, min = 1, max = 20, step = 1)),
          station_group("Inspection",
            numericInput("insp_cap", "Capacity (bays)", value = 1, min = 1, max = 20, step = 1))
        )
      )
    ),
    card(
      id = "params_buffers",
      class = "params-section mb-3",
      fill = FALSE,
      card_header("Interstation buffers"),
      card_body(
        fill = FALSE,
        p(class = "small mb-2",
          "Waiting positions before each downstream process. Zero means direct transfer only. ",
          "A completed vehicle holds its upstream bay until space opens."),
        layout_columns(
          fill = FALSE,
          col_widths = c(4, 4, 4),
          numericInput("buffer_paint","Waiting positions before paint",3,min=0,step=1),
          numericInput("buffer_final_assembly","Waiting positions before final assembly",3,min=0,step=1),
          numericInput("buffer_inspection","Waiting positions before inspection",3,min=0,step=1)
        )
      )
    ),
    card(
      id = "params_cycle_times",
      class = "params-section mb-3",
      fill = FALSE,
      card_header("Cycle times"),
      card_body(
        fill = FALSE,
        p(class = "small mb-2",
          "Automatic duration / mean. Placeholder minutes, not a measured cycle. ",
          "Load, unload, and distribution choices sit under each station."),
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          station_group("Body shop",
            numericInput("body_mean", "Automatic duration / mean (min)", value = 6, min = 0.5, max = 60, step = 0.5),
            uiOutput("cycle_body_shop")),
          station_group("Paint",
            numericInput("paint_mean", "Automatic duration / mean (min)", value = 8, min = 0.5, max = 60, step = 0.5),
            uiOutput("cycle_paint")),
          station_group("Final assembly",
            numericInput("fa_mean", "Automatic duration / mean (min)", value = 7, min = 0.5, max = 60, step = 0.5),
            uiOutput("cycle_final_assembly")),
          station_group("Inspection",
            numericInput("insp_mean", "Automatic duration / mean (min)", value = 3, min = 0.5, max = 60, step = 0.5),
            uiOutput("cycle_inspection"))
        )
      )
    ),
    card(
      id = "params_operators",
      class = "params-section mb-3",
      fill = FALSE,
      card_header("Operators"),
      card_body(
        fill = FALSE,
        p(class = "small mb-2",
          "Placeholder crew sizes — not measured plant data. Zone A serves body and paint; Zone B serves assembly and inspection."),
        layout_columns(
          fill = FALSE,
          col_widths = c(4, 8),
          selectInput("staffing", "Operator assignment", c("Two shared zones"="zones", "One shared team"="shared", "Dedicated by station"="dedicated")),
          uiOutput("operator_controls")
        )
      )
    ),
    card(
      id = "params_reliability",
      class = "params-section mb-3",
      fill = FALSE,
      card_header("Failures & support"),
      card_body(
        fill = FALSE,
        p(class = "small mb-2",
          "Placeholder MTBF/MTTR and task times. Mean up-time (MTBF) is calendar up-time in minutes. ",
          "Mean active repair (MTTR) of 0 disables failures at that station. Support every N units uses 0 for a random exponential interval."),
        layout_columns(
          fill = FALSE,
          col_widths = c(4, 4, 4),
          checkboxInput("reliability", "Enable equipment failures", TRUE),
          numericInput("repair_staff", "Repair technicians", 1, min = 1, step = 1),
          checkboxInput("noncyclic", "Enable random support tasks", TRUE)
        ),
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          station_group("Body shop", uiOutput("reliability_body_shop")),
          station_group("Paint", uiOutput("reliability_paint")),
          station_group("Final assembly", uiOutput("reliability_final_assembly")),
          station_group("Inspection", uiOutput("reliability_inspection"))
        )
      )
    )
  )
)
}

results_bottleneck_ui <- function() {
nav_panel(
  title = "Bottleneck & capacity",
  value = "results_capacity",
  p(class = "text-muted small px-2 pt-2 mb-1",
    "The Bottleneck KPI is the station with the highest occupancy excluding blocked time. These charts show whether waits, reliability, bays, or operators explain it."),
  layout_columns(
    fill = FALSE,
    col_widths = c(6, 6),
    plot_card(
      "Why occupied bays wait",
      p(class = "small mb-2",
        "Machine-minutes blocked after processing, waiting for operators, or awaiting automatic capacity. These do not include active processing or ongoing failure interruptions. Hover or tap a bar for its value."),
      ggiraph::girafeOutput("plot_waits", width = "100%", height = "420px"),
      downloadButton("dl_waits_main", "Download wait data (.csv)", class = "btn-outline-secondary btn-sm mt-2")
    ),
    card(
      fill = FALSE,
      card_header("Reliability losses"),
      card_body(
        fill = FALSE,
        p(class = "small mb-2",
          "Total unavailability = active repair plus waiting for a technician. Includes failures still waiting at the end of the run."),
        DTOutput("reliability_losses")
      )
    )
  ),
  navset_pill(
    id = "capacity_tabs",
    selected = "occupancy",
    nav_panel(
      title = "Station occupancy",
      value = "occupancy",
      card(
        fill = FALSE,
        card_header("Bay occupancy by station"),
        card_body(
          fill = FALSE,
          p(
            class = "small mb-2",
            "Mean bay occupancy across replications, with 95% CI, min, and max. Sorted highest-first so the bottleneck candidate is on top. Blue bar inside the Mean cell scales from 0% to 100%."
          ),
          # Compact DT; CSS keeps headers wrapping on phone instead of horizontal scroll.
          tags$style(HTML("
            #occupancy_summary_table { width: 100% !important; }
            #occupancy_summary_table table.dataTable { width: 100% !important; table-layout: fixed; }
            #occupancy_summary_table table.dataTable th,
            #occupancy_summary_table table.dataTable td {
              white-space: normal !important;
              word-break: break-word;
              vertical-align: middle;
              font-size: 0.92rem;
              color: #1a1a1a;
            }
            #occupancy_summary_table table.dataTable th { font-weight: 700; }
            @media (max-width: 576px) {
              #occupancy_summary_table table.dataTable th,
              #occupancy_summary_table table.dataTable td { font-size: 0.82rem; padding: 0.35rem 0.3rem; }
            }
          ")),
          DTOutput("occupancy_summary_table"),
          div(
            class = "d-flex flex-wrap gap-2 mt-2",
            downloadButton("dl_occupancy_reps", "Download by replication (.csv)", class = "btn-outline-secondary btn-sm"),
            downloadButton("dl_util_summary", "Download summary (.csv)", class = "btn-outline-secondary btn-sm")
          )
        )
      )
    ),
    nav_panel(
      title = "Operators",
      value = "operators",
      plot_card(
        "Operator utilization (mean ± 95% CI)",
        roomy_plot("plot_labor"),
        downloadButton("dl_labor_summary", "Download mean ± 95% CI (.csv)", class = "btn-outline-secondary btn-sm mt-2")
      ),
      card(
        fill = FALSE,
        card_header("Mean utilization by pool"),
        card_body(fill = FALSE, DTOutput("labor_table"))
      )
    ),
    nav_panel(
      title = "Downtime",
      value = "downtime",
      plot_card(
        "Downtime fraction (mean ± 95% CI)",
        roomy_plot("plot_down"),
        downloadButton("dl_down_summary", "Download mean ± 95% CI (.csv)", class = "btn-outline-secondary btn-sm mt-2")
      ),
      card(
        fill = FALSE,
        card_header("MTBF, MTTR, and observed failures"),
        card_body(
          fill = FALSE,
          p(class = "text-muted small",
            "Mean up-time and active repair from the inputs, with failure counts from this run. MTTR of 0 disables failures at that station."),
          DTOutput("reliability_table")
        )
      )
    ),
    nav_panel(
      title = "Staffing comparison",
      value = "staffing",
      card(
        fill = FALSE,
        card_header("Staffing experiments"),
        card_body(
          fill = FALSE,
          p("Uses the station settings from the comparison run, not later sidebar edits. Intervals show uncertainty in each scenario's mean throughput, not individual shifts. Multiple replications are needed. Scenarios reuse base seeds; random draws are not synchronized by event."),
          p(class = "text-muted small", "Click Compare staffing scenarios in the sidebar to fill this table."),
          uiOutput("staffing_warning"),
          uiOutput("staffing_meta"),
          div(id = "scenario_table_wrap", DTOutput("scenario_table"))
        )
      )
    )
  )
)
}

results_flow_ui <- function() {
nav_panel(
  title = "Flow over time",
  value = "results_flow",
  p(class = "text-muted small px-2 pt-2 mb-1",
    "Does the line settle after warmup, or does work keep building? Faint lines are each replication. The ribbon is a loess smooth on the 2-minute grid."),
  navset_pill(
    id = "flow_tabs",
    selected = "wip",
    nav_panel(
      title = "WIP",
      value = "wip",
      plot_card(
        "WIP over time (all reps + loess)",
        roomy_plot("plot_wip")
      )
    ),
    nav_panel(
      title = "Queues",
      value = "queues",
      plot_card(
        "Queue length over time (all reps + loess)",
        div(
          class = "d-flex justify-content-end mb-2",
          plot_options_menu(
            radioButtons(
              "queue_layout", "Layout",
              choices = c("Overlay stations" = "overlay", "Facet by station" = "facet"),
              selected = "overlay", inline = TRUE
            ),
            selectInput(
              "queue_station", "Station",
              choices = c(
                "All stations" = "all",
                setNames(names(station_labels), unname(station_labels))
              )
            )
          )
        ),
        roomy_plot("plot_queue")
      )
    )
  )
)
}

results_lead_ui <- function() {
nav_panel(
  title = "Lead time & data",
  value = "results_lead",
  navset_pill(
    id = "lead_tabs",
    selected = "cycle",
    nav_panel(
      title = "Cycle time",
      value = "cycle",
      plot_card(
        "Cycle time distribution",
        roomy_plot("plot_cycle")
      )
    ),
    nav_panel(
      title = "Station metrics",
      value = "metrics",
      card(
        fill = FALSE,
        full_screen = TRUE,
        card_header("Station metrics"),
        card_body(fill = FALSE, DTOutput("table_metrics"))
      )
    ),
    nav_panel(
      title = "Timing comparison",
      value = "timing",
      card(
        fill = FALSE,
        card_header("Same work content, different variability"),
        card_body(
          fill = FALSE,
          p("Compare current timing, fixed expected times, and lognormal durations with SD equal to half the mean. Staffing, arrivals, failures, and trigger rules remain the same. Mean-throughput confidence intervals use independent replications. The table keeps the settings from the comparison run."),
          p(class = "text-muted small", "Click Compare timing variability in the sidebar to fill this table."),
          uiOutput("timing_warning"),
          uiOutput("timing_meta"),
          div(id = "timing_table_wrap", DTOutput("timing_table"))
        )
      )
    ),
    nav_panel(
      title = "Downloads",
      value = "downloads",
      card(
        fill = FALSE,
        card_header("Download the tidy tables"),
        card_body(
          fill = FALSE,
          p("Each CSV is the data behind the charts for the current run, including seeds. Download all also adds per-replication summaries and failure events, which do not have their own button."),
          download_catalog(),
          div(
            class = "mt-3",
            downloadButton("dl_all", "Download all (zip)", class = "btn-secondary")
          )
        )
      )
    )
  ),
  tags$details(
    class = "run-settings border rounded mt-3 mb-2 mx-2",
    tags$summary(class = "px-3 py-2", "Run settings"),
    div(class = "px-3 pb-2", uiOutput("run_settings_body"))
  )
)
}

about_page_ui <- function() {
nav_panel(
  title = "About this model",
  card(
    card_header("About this model"),
    markdown("
This is a **discrete-event simulation** of a serial automotive manufacturing line,
built in R with [**simmer**](https://r-simmer.org/). Vehicles arrive to the line and
pass through four stations in order:

1. **Body shop** → 2. **Paint** → 3. **Final assembly** → 4. **Inspection**

Each station has a configurable number of parallel bays. Load, automatic,
unload, and support times can be a fixed duration, a distribution (normal,
lognormal, or exponential), or measured observations. Planned orders use evenly spaced or exponential
intervals. Admission can depend on entrance space, a total-WIP limit (CONWIP),
or release all planned work. Unreleased orders stay outside shop-floor WIP.

The simulator runs multiple independent replications with fixed seeds for
reproducibility, runs the settling period first, then measures every complete production shift and reports:

- Throughput (vehicles/hour)
- Cycle time (mean and distribution)
- Average work-in-process (WIP)
- Time-weighted utilization and queue length per station

### Important caveat

**All default numeric inputs are placeholder example values**, chosen so the
demo has a clear bottleneck (paint) and responsive run times. The opening
defaults are 5 replications, 300 minutes of settling, and a CONWIP limit of
20 vehicles. They are **not** real plant data. Results are illustrative only.
They are not predictions for any employer. Swap in your own stations,
capacities, and time distributions before using this for any decision.

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

Interstation waiting positions are finite. A completed vehicle holds its upstream bay until downstream space opens. Operators are freed after unloading. Shifts are consecutive production periods; nights, weekends, walking time, and scrap are not implemented. Bay occupancy identifies a bottleneck candidate; confirm it
by testing whether improving that station increases system throughput.

Every chart uses **all replications**. Bay occupancy on Bottleneck & capacity is a
compact table (mean, 95% CI, min, max) sorted highest-first. Operator utilization
and downtime are bar charts with 95% CI; Compare overlays densities with a rug tick
for every replication and a vertical line when every replication is identical. Queue and WIP series are faint lines plus a loess
ribbon, resampled onto a 2-minute grid before the loess. WIP counts each vehicle
once. Downtime is machine-time in repair during the observation window, divided
by capacity. **Lead time & data → Downloads** exports the tidy tables behind the
charts, including repair events, plus a zip of all of them. Staffing experiments
sit under **Bottleneck & capacity**; the timing-variability comparison sits under
**Lead time & data**.

**Compare two sets** keeps two editable copies of the line. Each set has a title
used on the charts, tables, and downloads. A table above the editors lists only
the settings that differ. After a run, the metrics that moved the most are
overlaid densities of every replication, with a rug tick on each run. Throughput,
lead time, WIP, output per operator-hour, bay occupancy, operator utilization,
and active repair use the same density. A vertical line means that set did not
vary. CSV files there are the data behind those charts. The guided example fills
this page with the 1-bay baseline and the 2-bay experiment.

**All default numbers are placeholder example values**, not real plant data.
    ")
  )
)
}
