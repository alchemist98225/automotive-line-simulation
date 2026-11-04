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
  tagList(h6(label),
    selectInput(paste0(id,"_method"),"Method",c("Fixed"="fixed","Distribution"="distribution","Measured observations"="empirical"),selected=if(auto) "distribution" else "fixed"),
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
        numericInput(paste0("load_", n), "Load", st$load_min, min = 0, step = .25),
        numericInput(paste0("unload_", n), "Unload", st$unload_min, min = 0, step = .25),
        timing_controls(st, "load", "Load"),
        timing_controls(st, "auto", "Auto"),
        timing_controls(st, "unload", "Unload")
      )
    })
    output[[rel_id]] <- renderUI({
      tagList(
        numericInput(paste0("mtbf_", n), "MTBF", if (is.finite(st$mtbf_min)) st$mtbf_min else 10000, min = 1),
        numericInput(paste0("mttr_", n), "MTTR", st$mttr_min, min = 0),
        numericInput(paste0("interval_", n), "Support interval", st$task_interval_min, min = 1),
        numericInput(paste0("every_", n), "Every N", 0, min = 0, step = 1),
        numericInput(paste0("task_", n), "Support duration", st$task_duration_min, min = 0),
        timing_controls(st, "task", "Support")
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


# Map a params list onto every main-page / Parameters control.
# Used by Restore defaults and by "Import settings from Compare".
apply_params_to_inputs <- function(session, params) {
  p <- normalize_params(params)
  stations <- p$stations
  updateNumericInput(session, "planned_rate", value = 60 / p$arrival_mean_min)
  updateNumericInput(session, "n_shifts", value = as.integer(p$n_shifts))
  updateSelectInput(session, "release_policy", selected = p$release_policy)
  updateSelectInput(session, "order_spacing", selected = p$order_spacing)
  updateNumericInput(session, "entry_buffer", value = p$entry_buffer)
  updateNumericInput(session, "wip_limit", value = p$wip_limit)
  walk(stations$name[stations$name != "body_shop"], function(n) {
    buf <- stations$buffer_before[stations$name == n]
    updateNumericInput(session, paste0("buffer_", n), value = if (is.finite(buf)) buf else 3)
  })
  updateNumericInput(session, "sim_length", value = p$shift_length_min)
  updateNumericInput(session, "warmup", value = p$warmup_min)
  updateNumericInput(session, "n_reps", value = as.integer(p$n_replications))
  updateNumericInput(session, "seed", value = as.integer(p$base_seed))
  # Keep the Parameters-page run-setting copies in step (import, reset, Try it).
  params_run_fields_update(session, params_run_values(p))
  cap_ids <- c(
    body_shop = "body_cap", paint = "paint_cap",
    final_assembly = "fa_cap", inspection = "insp_cap"
  )
  mean_ids <- c(
    body_shop = "body_mean", paint = "paint_mean",
    final_assembly = "fa_mean", inspection = "insp_mean"
  )
  for (n in names(cap_ids)) {
    row <- stations[stations$name == n, , drop = FALSE]
    if (!nrow(row)) next
    updateNumericInput(session, cap_ids[[n]], value = as.integer(row$capacity[[1]]))
    auto <- activity_timing(row, "auto")
    mean_val <- if (identical(auto$method, "empirical")) mean(auto$observations) else auto$mean_min
    updateNumericInput(session, mean_ids[[n]], value = mean_val)
  }
  pools <- p$operators$pool
  counts <- p$operators$count
  pool_count <- function(name, default = 1) {
    if (name %in% pools) counts[[match(name, pools)]] else default
  }
  # A staffing switch re-renders operator_controls, which drops updates sent
  # to the old widgets. Stash the counts so the new widgets start with them.
  session$userData$op_pending <- purrr::set_names(as.list(counts), pools)
  if (setequal(pools, c("upstream", "downstream"))) {
    updateSelectInput(session, "staffing", selected = "zones")
    updateNumericInput(session, "op_a", value = pool_count("upstream", 2))
    updateNumericInput(session, "op_b", value = pool_count("downstream", 2))
  } else if (length(pools) == 1L && identical(pools[[1]], "shared")) {
    updateSelectInput(session, "staffing", selected = "shared")
    updateNumericInput(session, "op_all", value = pool_count("shared", 4))
  } else {
    updateSelectInput(session, "staffing", selected = "dedicated")
    for (nm in names(station_labels)) {
      updateNumericInput(session, paste0("op_", nm), value = pool_count(nm, 1))
    }
  }
  reliability <- any(is.finite(stations$mtbf_min) & stations$mttr_min > 0)
  noncyclic <- any(vapply(seq_len(nrow(stations)), function(i) {
    timing_mean(activity_timing(stations[i, ], "task")) > 0 &&
      (stations$task_every_n[[i]] > 0 || is.finite(stations$task_interval_min[[i]]))
  }, logical(1)))
  updateCheckboxInput(session, "reliability", value = reliability)
  updateCheckboxInput(session, "noncyclic", value = noncyclic)
  updateNumericInput(session, "repair_staff", value = as.integer(p$repair_technicians %||% 1L))
  for (i in seq_len(nrow(stations))) {
    st <- stations[i, ]
    n <- st$name
    updateNumericInput(session, paste0("load_", n), value = st$load_min)
    updateNumericInput(session, paste0("unload_", n), value = st$unload_min)
    updateNumericInput(session, paste0("mtbf_", n), value = if (is.finite(st$mtbf_min)) st$mtbf_min else 10000)
    updateNumericInput(session, paste0("mttr_", n), value = st$mttr_min)
    updateNumericInput(
      session, paste0("interval_", n),
      value = if (is.finite(st$task_interval_min)) st$task_interval_min else 60
    )
    updateNumericInput(session, paste0("every_", n), value = as.integer(st$task_every_n %||% 0L))
    task <- activity_timing(st, "task")
    task_mean <- if (identical(task$method, "empirical")) mean(task$observations) else task$mean_min
    updateNumericInput(session, paste0("task_", n), value = task_mean)
    for (activity in c("load", "auto", "unload", "task")) {
      id <- paste0(activity, "_", n)
      spec <- activity_timing(st, activity)
      updateSelectInput(session, paste0(id, "_method"), selected = spec$method)
      updateSelectInput(
        session, paste0(id, "_distribution"),
        selected = if (identical(spec$method, "distribution")) spec$distribution else "normal"
      )
      updateNumericInput(
        session, paste0(id, "_sd"),
        value = if (is.finite(spec$sd_min)) spec$sd_min else 0.25
      )
      obs <- if (identical(spec$method, "empirical")) {
        paste(format(spec$observations, digits = 17), collapse = ", ")
      } else {
        ""
      }
      updateTextAreaInput(session, paste0(id, "_observations"), value = obs)
    }
  }
  invisible(TRUE)
}

# Push every sidebar control back to the value it shows on first load.
restore_default_inputs <- function(session) {
  apply_params_to_inputs(session, default_params())
}
