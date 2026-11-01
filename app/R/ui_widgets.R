# ui_widgets.R
# Small UI builders shared across pages: plot cards, downloads, timing editors,
# KPI nav boxes, restore-defaults helper, provenance warning.

provenance_warning <- function(message, button_id = NULL) {
  div(
    class = "alert alert-warning provenance-warning d-flex flex-wrap align-items-center justify-content-between gap-2 mb-3",
    role = "alert",
    div(
      class = "d-flex align-items-center gap-2",
      bs_icon("exclamation-triangle-fill"),
      span(message)
    ),
    if (!is.null(button_id)) actionButton(button_id, "Rerun comparison", class = "btn-warning btn-sm")
  )
}

# plotOutput only accepts a single CSS length. The theme rule below replaces it
# with clamp(520px, 60vh, 720px). The extra height keeps larger station names,
# wrapped subtitles, and the occupancy clouds from crowding each other.
plot_height <- "640px"
# res=96 makes base-size-16 text read at a comfortable on-screen size inside
# the tall plotOutput; Shiny's default res=72 understates 16pt on large plots.
sim_render_plot <- function(expr) {
  shiny::renderPlot(
    substitute(expr),
    res = 96,
    quoted = TRUE,
    env = parent.frame(),
    alt = "Simulation chart"
  )
}

download_entry <- function(id, label, description) {
  div(
    class = "d-flex flex-column flex-md-row align-items-md-center justify-content-between gap-2 py-2 border-bottom",
    div(
      strong(label),
      p(class = "text-muted small mb-0", description)
    ),
    downloadButton(id, label, class = "btn-outline-secondary btn-sm text-nowrap")
  )
}

download_catalog <- function() {
  tagList(
    download_entry(
      "dl_queue", "Queue timeseries",
      "Resampled bay queue and servers by replication, seed, station, and time."
    ),
    download_entry(
      "dl_wip", "WIP timeseries",
      "Resampled vehicle count, each vehicle once, by replication, seed, and time."
    ),
    download_entry(
      "dl_util", "Bay occupancy by rep",
      "Time-weighted bay occupancy, queue, and servers for each replication and station."
    ),
    download_entry(
      "dl_cycle", "Cycle times",
      "One row per departure in the observation window."
    ),
    download_entry(
      "dl_metrics", "Station metrics",
      "Across-replication mean and SD of occupancy, queue, and downtime fraction."
    ),
    download_entry(
      "dl_down", "Downtime by rep",
      "Repair minutes, repair count, and downtime fraction by replication and station."
    ),
    download_entry(
      "dl_repairs", "Repair events",
      "One row per repair: station, start time, sampled duration, and replication."
    ),
    download_entry(
      "dl_ops", "Operator utilization",
      "Time-weighted utilization and queue for each operator pool and replication."
    ),
    download_entry(
      "dl_params", "Run parameters",
      "Horizon, seeds, crew pools, and each station's capacity, timing, MTBF/MTTR, and support settings for this run, including the run number and settings summary."
    )
  )
}


# Compact plot-option controls: one "Plot options" button opens a popover so
# radios/selects stop crowding the chart. CSV downloads stay outside the menu.
plot_options_menu <- function(..., title = "Plot options") {
  popover(
    tags$button(
      type = "button",
      class = "btn btn-outline-secondary btn-sm plot-options-btn",
      bs_icon("sliders"),
      " Plot options"
    ),
    title = title,
    ...,
    placement = "bottom",
    options = list(customClass = "plot-options-popover")
  )
}

# One chart per tab, full width. Inner cards size to the plot instead of shrinking in a grid.
plot_card <- function(title, ...) {
  card(
    fill = FALSE,
    full_screen = TRUE,
    card_header(title),
    card_body(fill = FALSE, fillable = FALSE, ...)
  )
}

roomy_plot <- function(id) {
  plotOutput(id, height = plot_height, width = "100%", fill = FALSE)
}

# Measurements are pasted as minutes, separated by commas, spaces, or newlines.
parse_observations <- function(text) {
  values <- suppressWarnings(as.numeric(strsplit(trimws(text),"[,;[:space:]]+")[[1]]))
  if (!length(values) || any(!is.finite(values) | values<0))
    stop("Enter at least one finite, nonnegative observation in minutes.",call.=FALSE)
  values
}
timing_controls <- function(st,activity,label) {
  id <- paste0(activity,"_",st$name)
  auto <- activity=="auto"
  tagList(h6(paste(label,"timing")),
    selectInput(paste0(id,"_method"),"Timing method",c("Fixed"="fixed","Distribution"="distribution","Measured observations"="empirical"),selected=if(auto) "distribution" else "fixed"),
    conditionalPanel(sprintf("input['%s_method'] === 'distribution'",id),
      selectInput(paste0(id,"_distribution"),"Distribution",c("Normal (negative draws become zero)"="normal","Lognormal"="lognormal","Exponential"="exponential"),selected=if(auto) st$dist else "normal"),
      numericInput(paste0(id,"_sd"),"SD (min; exponential ignores SD)",if(auto && is.finite(st$sd_min)) st$sd_min else .25,min=0,step=.1)),
    conditionalPanel(sprintf("input['%s_method'] === 'empirical'",id),
      textAreaInput(paste0(id,"_observations"),"Observed durations (min)",placeholder="e.g. 1.1, 0.9, 1.4",rows=3),
      helpText("Sampled with replacement each occurrence. Mean and SD inputs are ignored.")))
}

station_group <- function(title, ...) {
  div(class = "border-bottom pb-2 mb-3", h6(class = "mt-2", title), ...)
}

# Load/unload/automatic timing and MTBF/support inputs are renderUI outputs.
# Collapsed accordion panels still build them so a run uses the values on
# screen even when that panel has not been opened.
register_station_panels <- function(output) {
  stations <- default_params()$stations
  lapply(seq_len(nrow(stations)), function(i) {
    st <- stations[i, ]
    n <- st$name
    cycle_id <- paste0("cycle_", n)
    rel_id <- paste0("reliability_", n)
    output[[cycle_id]] <- renderUI({
      tagList(
        numericInput(paste0("load_", n), "Load duration / mean (min)", st$load_min, min = 0, step = .25),
        numericInput(paste0("unload_", n), "Unload duration / mean (min)", st$unload_min, min = 0, step = .25),
        timing_controls(st, "load", "Loading"),
        timing_controls(st, "auto", "Automatic processing"),
        timing_controls(st, "unload", "Unloading")
      )
    })
    output[[rel_id]] <- renderUI({
      tagList(
        numericInput(paste0("mtbf_", n), "Mean up-time between failures (min)", if (is.finite(st$mtbf_min)) st$mtbf_min else 10000, min = 1),
        numericInput(paste0("mttr_", n), "Mean active repair (min; 0 disables)", st$mttr_min, min = 0),
        numericInput(paste0("interval_", n), "Mean support-task interval (min)", st$task_interval_min, min = 1),
        numericInput(paste0("every_", n), "Support task every N units (0 = random)", 0, min = 0, step = 1),
        numericInput(paste0("task_", n), "Support duration / mean (min)", st$task_duration_min, min = 0),
        timing_controls(st, "task", "Support work")
      )
    })
    outputOptions(output, cycle_id, suspendWhenHidden = FALSE)
    outputOptions(output, rel_id, suspendWhenHidden = FALSE)
    NULL
  })
  invisible(output)
}
kpi_nav_box <- function(page_value, title, value, showcase, theme, ...,
                        box_key = NULL) {
  box_id <- paste0(
    "kpi_nav_",
    if (!is.null(box_key)) box_key else switch(page_value,
      production_schedule = "throughput",
      results_lead = "lead",
      results_flow = "wip",
      results_capacity = "bottleneck",
      page_value
    )
  )
  div(
    id = box_id,
    class = "kpi-nav-card",
    role = "link",
    tabindex = "0",
    `data-page` = page_value,
    `aria-label` = paste("Open", title, "charts"),
    value_box(
      title = title,
      value = value,
      showcase = showcase,
      theme = theme,
      ...,
      span(class = "kpi-view-hint", "View charts →")
    )
  )
}


# Push every sidebar control back to the value it shows on first load.
restore_default_inputs <- function(session) {
  stations <- default_params()$stations
  updateNumericInput(session,"planned_rate",value=15)
  updateNumericInput(session,"n_shifts",value=1)
  updateSelectInput(session,"release_policy",selected="space")
  updateSelectInput(session,"order_spacing",selected="fixed")
  updateNumericInput(session,"entry_buffer",value=3)
  updateNumericInput(session,"wip_limit",value=20)
  walk(default_params()$stations$name[-1],~updateNumericInput(session,paste0("buffer_",.x),value=3))
  updateNumericInput(session, "sim_length", value = 480)
  updateNumericInput(session, "warmup", value = 300)
  updateNumericInput(session, "n_reps", value = 5)
  updateNumericInput(session, "seed", value = 20261027)
  updateNumericInput(session, "body_cap", value = 2)
  updateNumericInput(session, "paint_cap", value = 1)
  updateNumericInput(session, "fa_cap", value = 2)
  updateNumericInput(session, "insp_cap", value = 1)
  updateNumericInput(session, "body_mean", value = 6)
  updateNumericInput(session, "paint_mean", value = 8)
  updateNumericInput(session, "fa_mean", value = 7)
  updateNumericInput(session, "insp_mean", value = 3)
  updateSelectInput(session, "staffing", selected = "zones")
  updateCheckboxInput(session, "reliability", value = TRUE)
  updateCheckboxInput(session, "noncyclic", value = TRUE)
  updateNumericInput(session, "repair_staff", value = 1)
  updateNumericInput(session, "op_a", value = 2)
  updateNumericInput(session, "op_b", value = 2)
  updateNumericInput(session, "op_all", value = 4)
  for (nm in names(station_labels)) {
    updateNumericInput(session, paste0("op_", nm), value = 1)
  }
  for (i in seq_len(nrow(stations))) {
    st <- stations[i, ]
    n <- st$name
    updateNumericInput(session, paste0("load_", n), value = st$load_min)
    updateNumericInput(session, paste0("unload_", n), value = st$unload_min)
    updateNumericInput(session, paste0("mtbf_", n), value = if (is.finite(st$mtbf_min)) st$mtbf_min else 10000)
    updateNumericInput(session, paste0("mttr_", n), value = st$mttr_min)
    updateNumericInput(session, paste0("interval_", n), value = st$task_interval_min)
    updateNumericInput(session, paste0("every_", n), value = 0)
    updateNumericInput(session, paste0("task_", n), value = st$task_duration_min)
    for (activity in c("load", "auto", "unload", "task")) {
      id <- paste0(activity, "_", n)
      auto <- identical(activity, "auto")
      updateSelectInput(session, paste0(id, "_method"), selected = if (auto) "distribution" else "fixed")
      updateSelectInput(session, paste0(id, "_distribution"), selected = if (auto) st$dist else "normal")
      updateNumericInput(session, paste0(id, "_sd"), value = if (auto && is.finite(st$sd_min)) st$sd_min else 0.25)
      updateTextAreaInput(session, paste0(id, "_observations"), value = "")
    }
  }
}
