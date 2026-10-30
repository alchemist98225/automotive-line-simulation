e <- new.env()
source("app/app.R",local=e,chdir=TRUE)
# Count ggplot manual-scale mismatches for the whole script, including plot renders.
scale_warns <- new.env(parent = emptyenv())
scale_warns$msgs <- character()
globalCallingHandlers(warning = function(w) {
  msg <- conditionMessage(w)
  if (grepl("No shared levels found", msg, fixed = TRUE)) {
    scale_warns$msgs <- c(scale_warns$msgs, msg)
    invokeRestart("muffleWarning")
  }
})
# First paint: no input registration and no Run click. Dynamic renderUI
# values are NULL (collapsed panels have not registered them yet); the
# session must still produce the placeholder run.
shiny::testServer(e$server, {
  stopifnot(is.null(input$run), is.null(input$op_a), is.null(input$load_body_shop), is.null(input$mtbf_paint))
  res <- sim_result()
  d <- e$default_params()
  stopifnot(nrow(res$summaries)==d$n_replications)
  stopifnot(nrow(res$labor)==nrow(d$operators)*d$n_replications)
  stopifnot(res$bottleneck %in% d$stations$name)
  stopifnot(identical(res$params$operators$pool, c("upstream","downstream")))
  stopifnot(all(res$params$operators$count==2))
  stopifnot(res$params$stations$mttr_min[res$params$stations$name=="inspection"]==0)
  stopifnot(all(res$params$stations$mttr_min[res$params$stations$name!="inspection"]>0))
  stopifnot(d$n_replications == 5L)
  stopifnot(d$warmup_min == 300)
  stopifnot(d$wip_limit == 20L)
  stopifnot(nrow(res$utilization_by_rep) == d$n_replications * nrow(d$stations))
  stopifnot(all(c("station", "replication", "avg_util") %in% names(res$utilization_by_rep)))
  stopifnot(nrow(res$queue_timeseries)>0, nrow(res$wip_timeseries)>0, nrow(res$downtime_by_rep)>0)
  stopifnot(is.character(output$kpi_throughput), nzchar(output$kpi_throughput))
  stopifnot(identical(staffing_trigger(), 0L), identical(timing_trigger(), 0L))
  stopifnot(identical(output$main_stale_flag, "fresh"))
  stopifnot(identical(output$staffing_stale_flag, "fresh"))
  stopifnot(identical(output$timing_stale_flag, "fresh"))
  settings <- paste(as.character(output$run_settings_body), collapse = " ")
  stopifnot(grepl("Run 1", settings, fixed = TRUE))
  stopifnot(grepl("Runs 5", settings, fixed = TRUE))
  stopifnot(grepl("Paint 1", settings, fixed = TRUE))
  stopifnot(grepl("cycle-time:", settings, fixed = TRUE))
  stopifnot(grepl("operators:", settings, fixed = TRUE))
  stopifnot(grepl("MTBF/MTTR", settings, fixed = TRUE))
  stopifnot(all(res$run_parameters$run_number == 1L))
  stopifnot(all(res$run_parameters$settings_summary == res$provenance$summary))
  stopifnot(grepl("Paint 1", res$run_parameters$settings_summary[[1]], fixed = TRUE))
  stopifnot(grepl(res$provenance$summary, output$run_settings_text, fixed = TRUE))
  invisible(output$plot_util)
  invisible(output$plot_queue)
  invisible(output$plot_wip)
  invisible(output$plot_down)
  invisible(output$plot_labor)
  invisible(output$plot_cycle)
  invisible(output$table_metrics)
  invisible(output$reliability_table)
  invisible(output$labor_table)
})
ui_html <- as.character(e$ui)
page_has <- function(label) {
  grepl(label, ui_html, fixed = TRUE) ||
    grepl(gsub("&", "&amp;", label, fixed = TRUE), ui_html, fixed = TRUE)
}
for (label in c(
  "Bottleneck & capacity", "Flow over time", "Lead time & data",
  "Production schedule & flow", "Station occupancy", "Operators", "Downtime",
  "Staffing comparison", "WIP", "Queues", "Cycle time", "Station metrics",
  "Timing comparison", "Downloads", "640px", "clamp(520px, 60vh, 720px)",
  "gap: 0.5rem", "Run settings", "Compare two sets",
  "Results are illustrative only", "Parameters", "Interstation buffers",
  "Failures & support", "Results", "View charts", "Run simulation",
  "Number of replications"
)) stopifnot(page_has(label))
# Downloads stay on the Lead time & data page, not in the sidebar.
stopifnot(!grepl("Download data", ui_html, fixed = TRUE))
# Simulate main body: slim status bar, then KPI boxes, then guided example.
# Chart tabs live on their own Results pages (no inline results_tabs).
kpi_at <- regexpr('id="headline_kpis"', ui_html)
guide_at <- regexpr('id="guided_example"', ui_html)
status_at <- regexpr('id="sim_progress_slot"', ui_html)
settings_at <- regexpr('class="run-settings', ui_html)
stopifnot(status_at[[1]] > 0L, kpi_at[[1]] > status_at[[1]], guide_at[[1]] > kpi_at[[1]])
stopifnot(!grepl('id="results_tabs"', ui_html, fixed = TRUE))
stopifnot(grepl('data-value="results_capacity"', ui_html, fixed = TRUE) || grepl("results_capacity", ui_html, fixed = TRUE))
stopifnot(grepl('data-value="results_flow"', ui_html, fixed = TRUE) || grepl("results_flow", ui_html, fixed = TRUE))
stopifnot(grepl('data-value="results_lead"', ui_html, fixed = TRUE) || grepl("results_lead", ui_html, fixed = TRUE))
stopifnot(grepl("kpi-nav-card", ui_html, fixed = TRUE))
stopifnot(grepl("kpi_nav_throughput", ui_html, fixed = TRUE))
stopifnot(grepl("kpi_nav_lead", ui_html, fixed = TRUE))
stopifnot(grepl("kpi_nav_wip", ui_html, fixed = TRUE))
stopifnot(grepl("kpi_nav_bottleneck", ui_html, fixed = TRUE))
# Run settings lives on the Lead time & data page and starts collapsed.
stopifnot(settings_at[[1]] > 0L)
stopifnot(!grepl('class="run-settings[^"]*" open', ui_html))
stopifnot(!grepl("<details open>", ui_html, fixed = TRUE))
stopifnot(page_has("Start here: guided example"))
stopifnot(page_has("Try it"))
stopifnot(page_has("Reset to defaults"))
stopifnot(page_has("not a prediction for any real plant or company"))
stopifnot(grepl("guided-example", ui_html, fixed = TRUE))
stopifnot(grepl("lightbulb", ui_html) || grepl("Start here: guided example", ui_html, fixed = TRUE))
stopifnot(page_has("fixed duration"))
stopifnot(page_has("measured observations"))
stopifnot(!page_has("exponential or normal distributions"))
cat("Results page layout check passed\n")
cat("First-load check passed\n")

# Simulate sidebar keeps only Run simulation as editable controls. Other
# groups are jump links to the Parameters page (no duplicate inputs).
sidebar_html <- as.character(xml2::xml_find_first(xml2::read_html(ui_html), "//*[@id='line_controls']"))
stopifnot(grepl("Run simulation", sidebar_html, fixed = TRUE))
stopifnot(!grepl("Arrivals & run", sidebar_html, fixed = TRUE))
stopifnot(grepl('id="line_controls"', ui_html, fixed = TRUE))
stopifnot(!grepl('data-value="Stations"', ui_html, fixed = TRUE))
stopifnot(!page_has("Activity timing & reliability by station"))
items <- strsplit(sidebar_html, '<div class="accordion-item" ', fixed = TRUE)[[1]][-1]
stopifnot(length(items) == 1L)
panel_open <- function(chunk) {
  header <- sub('<div class="accordion-body".*', "", chunk)
  collapse <- regmatches(chunk, regexpr('class="accordion-collapse [^"]*"', chunk))
  grepl('aria-expanded="true"', header, fixed = TRUE) &&
    identical(collapse, 'class="accordion-collapse collapse show"')
}
stopifnot(!panel_open(items[[1]]))
# Run simulation starts collapsed. Start button comes before Number of replications.
run_at <- regexpr('id="run"', items[[1]], fixed = TRUE)
reps_at <- regexpr('id="n_reps"', items[[1]], fixed = TRUE)
stopifnot(run_at[[1]] > 0L, reps_at[[1]] > run_at[[1]])
stopifnot(grepl('id="n_reps" type="number" class="shiny-input-number form-control" value="5"', items[[1]], fixed = TRUE))
stopifnot(grepl('id="warmup" type="number" class="shiny-input-number form-control" value="300"', items[[1]], fixed = TRUE))
stopifnot(grepl('id="wip_limit" type="number" class="shiny-input-number form-control" value="20"', items[[1]], fixed = TRUE))
stopifnot(grepl('id="planned_rate"', items[[1]], fixed = TRUE))
stopifnot(grepl("Number of replications", items[[1]], fixed = TRUE))
stopifnot(!grepl('id="body_cap"', items[[1]], fixed = TRUE))
stopifnot(!grepl('id="body_mean"', items[[1]], fixed = TRUE))
stopifnot(!grepl('id="staffing"', items[[1]], fixed = TRUE))
# Jump links stay in the sidebar chrome, outside the accordion body.
stopifnot(grepl('id="jump_capacity"', ui_html, fixed = TRUE))
stopifnot(grepl('id="jump_cycle_times"', ui_html, fixed = TRUE))
stopifnot(grepl('id="jump_operators"', ui_html, fixed = TRUE))
stopifnot(grepl('id="jump_reliability"', ui_html, fixed = TRUE))
# Desktop sidebar starts open; phone may collapse.
stopifnot(grepl('data-open-desktop="open"', ui_html, fixed = TRUE))
stopifnot(grepl('data-open-mobile="closed"', ui_html, fixed = TRUE))
# Page is not fillable, so the document can scroll.
stopifnot(!grepl("bslib-page-fill", ui_html, fixed = TRUE) || grepl("height: auto !important", ui_html, fixed = TRUE))
stopifnot(grepl("overflow-y: auto !important", ui_html, fixed = TRUE))
# Parameters page holds the shared station inputs exactly once.
stopifnot(page_has("Parameters"))
stopifnot(grepl('data-value="parameters"', ui_html, fixed = TRUE) || grepl(">Parameters<", ui_html, fixed = TRUE))
stopifnot(grepl('id="params_capacity"', ui_html, fixed = TRUE))
stopifnot(grepl('id="params_buffers"', ui_html, fixed = TRUE))
stopifnot(grepl('id="params_cycle_times"', ui_html, fixed = TRUE))
stopifnot(grepl('id="params_operators"', ui_html, fixed = TRUE))
stopifnot(grepl('id="params_reliability"', ui_html, fixed = TRUE))
stopifnot(length(gregexpr('id="body_cap"', ui_html, fixed = TRUE)[[1]]) == 1L, gregexpr('id="body_cap"', ui_html, fixed = TRUE)[[1]][[1]] > 0L)
stopifnot(grepl('id="body_cap"', ui_html, fixed = TRUE), grepl('id="paint_cap"', ui_html, fixed = TRUE))
stopifnot(grepl('id="body_mean"', ui_html, fixed = TRUE), grepl("cycle_body_shop", ui_html, fixed = TRUE))
stopifnot(grepl('id="staffing"', ui_html, fixed = TRUE), grepl("operator_controls", ui_html, fixed = TRUE))
stopifnot(grepl('id="reliability"', ui_html, fixed = TRUE), grepl("reliability_body_shop", ui_html, fixed = TRUE))
# Slim full-width status bar at the top of Simulate. Default Shiny toast suppressed.
stopifnot(grepl('id="sim_progress_panel"', ui_html, fixed = TRUE))
stopifnot(grepl('id="sim_progress_idle"', ui_html, fixed = TRUE))
stopifnot(grepl('id="sim_progress_slot"', ui_html, fixed = TRUE))
stopifnot(grepl("sim-status-bar", ui_html, fixed = TRUE))
stopifnot(grepl("Ready", ui_html, fixed = TRUE))
stopifnot(grepl("Simulation running", ui_html, fixed = TRUE))
stopifnot(grepl("simProgress", ui_html, fixed = TRUE))
stopifnot(grepl(".shiny-progress-container", ui_html, fixed = TRUE))
stopifnot(grepl("shiny-progress-notification", ui_html, fixed = TRUE))
stopifnot(grepl("display: none !important", ui_html, fixed = TRUE))
stopifnot(!grepl("sim-progress-label", ui_html, fixed = TRUE))
# Title appears once in the panel markup (not duplicated as a second heading).
stopifnot(length(gregexpr("Simulation running", ui_html, fixed = TRUE)[[1]]) == 1L)
# withProgress may remain as a flush pump, but must not show a toast message.
app_src <- paste(readLines("app/app.R"), collapse = "\n")
stopifnot(!grepl("Running simulation", app_src, fixed = TRUE))
stopifnot(!grepl('message="Comparing staffing', app_src, fixed = TRUE))
stopifnot(!grepl('message="Comparing variability', app_src, fixed = TRUE))
# ggplot axis/strip text must be absolute 16pt (not theme_minimal's rel(0.8)).
stopifnot(grepl("axis.text = element_text(size = 16", app_src, fixed = TRUE))
stopifnot(grepl("strip.text = element_text(size = 16", app_src, fixed = TRUE))
# Slim status bar replaces the old 280px guided-row side slot.
stopifnot(grepl("sim-status-bar", app_src, fixed = TRUE))
stopifnot(grepl("sim_progress_slot", app_src, fixed = TRUE))
stopifnot(grepl("sim_progress_idle", app_src, fixed = TRUE))
stopifnot(!grepl("guided-row-status", app_src, fixed = TRUE))
stopifnot(!grepl("flex: 0 0 280px", app_src, fixed = TRUE))
stopifnot(grepl("setSimProgressVisible", app_src, fixed = TRUE))
stopifnot(!grepl("padding-right: 23.5rem", app_src, fixed = TRUE))
# Run simulation accordion uses a standout green header.
stopifnot(grepl("#198754", app_src, fixed = TRUE) || grepl("run-sim-panel", app_src, fixed = TRUE))
cat("Sidebar accordion check passed\n")
# Plot option controls live in a compact "Plot options" popover, not inline.
stopifnot(grepl("Plot options", ui_html, fixed = TRUE))
stopifnot(grepl("plot-options-btn", ui_html, fixed = TRUE) || grepl("bslib-popover", ui_html, fixed = TRUE))
stopifnot(grepl('id="queue_layout"', ui_html, fixed = TRUE))
stopifnot(grepl('id="queue_station"', ui_html, fixed = TRUE))
cat("Plot options menu check passed\n")
shiny::testServer(e$server, {
  session$setInputs(staffing="zones",op_a=2,op_b=2,repair_staff=1,reliability=TRUE,noncyclic=TRUE,
    sim_length=480,warmup=60,planned_rate=15,n_reps=2,seed=20261027,
    body_cap=2,body_mean=6,paint_cap=1,paint_mean=8,fa_cap=2,fa_mean=7,insp_cap=1,insp_mean=3)
  ids <- c(load="load_min",unload="unload_min",mtbf="mtbf_min",mttr="mttr_min",interval="task_interval_min",task="task_duration_min")
  for(i in seq_len(nrow(e$default_params()$stations))) {
    st <- e$default_params()$stations[i,]
    for(id in names(ids)) do.call(session$setInputs,setNames(list(st[[ids[[id]]]]),paste0(id,"_",st$name)))
    do.call(session$setInputs,setNames(list(0L),paste0("every_",st$name)))
  }
  session$setInputs(run=1)
  stopifnot(nrow(sim_result()$summaries)==2,nrow(sim_result()$labor)==4)
  session$setInputs(queue_layout="facet", queue_station="all")
  invisible(output$plot_queue)
  session$setInputs(queue_layout="overlay", queue_station="paint")
  invisible(output$plot_queue)
  session$setInputs(queue_station="all")
  invisible(output$plot_queue)
  session$setInputs(compare=1)
  stopifnot(nrow(comparison())==4)
  stopifnot(identical(output$staffing_stale_flag, "fresh"))
  stopifnot(grepl("Comparison run 1", paste(as.character(output$staffing_meta), collapse = " "), fixed = TRUE))
  cur <- comparison() %>% dplyr::select(scenario, comparison) %>% tidyr::unnest(comparison)
  stopifnot(isTRUE(all.equal(cur$throughput[cur$scenario=="Current"], sim_result()$headline$throughput_per_hr_mean, tolerance=1e-8)))
  session$setInputs(paint_cap=2)
  stopifnot(identical(output$main_stale_flag, "stale"))
  stopifnot(grepl("Settings changed. Click Run to update results.", paste(as.character(output$main_stale_notice), collapse = " "), fixed = TRUE))
  stopifnot(identical(output$staffing_stale_flag, "stale"))
  session$setInputs(run=4)
  stopifnot(identical(output$main_stale_flag, "fresh"))
  stopifnot(identical(output$staffing_stale_flag, "stale"))
  warn <- paste(as.character(output$staffing_warning), collapse = " ")
  stopifnot(grepl("Settings changed since this comparison was run. Rerun comparison.", warn, fixed = TRUE))
  stopifnot(grepl("Rerun comparison", warn, fixed = TRUE))
  stopifnot(grepl("rerun_staffing", warn, fixed = TRUE))
  settings_txt <- paste(as.character(output$run_settings_body), collapse = " ")
  stopifnot(grepl("Paint 2", settings_txt, fixed = TRUE))
  stopifnot(grepl("Runs 2", settings_txt, fixed = TRUE))
  stopifnot(grepl("Run ", settings_txt, fixed = TRUE))
  stopifnot(grepl("cycle-time:", settings_txt, fixed = TRUE))
  stopifnot(grepl("operators:", settings_txt, fixed = TRUE))
  stopifnot(grepl("MTBF/MTTR", settings_txt, fixed = TRUE))
  rp <- sim_result()$run_parameters
  stopifnot(all(rp$settings_summary == sim_result()$provenance$summary))
  stopifnot(all(rp$run_number == sim_result()$provenance$run_number))
  stopifnot(grepl("Paint 2", rp$settings_summary[[1]], fixed = TRUE))
  stale_cur <- comparison() %>% dplyr::select(scenario, comparison) %>% tidyr::unnest(comparison)
  stopifnot(!isTRUE(all.equal(stale_cur$throughput[stale_cur$scenario=="Current"], sim_result()$headline$throughput_per_hr_mean, tolerance=1e-6)))
  session$setInputs(rerun_staffing=1)
  stopifnot(identical(output$staffing_stale_flag, "fresh"))
  fresh_cur <- comparison() %>% dplyr::select(scenario, comparison) %>% tidyr::unnest(comparison)
  stopifnot(isTRUE(all.equal(fresh_cur$throughput[fresh_cur$scenario=="Current"], sim_result()$headline$throughput_per_hr_mean, tolerance=1e-8)))
  stopifnot(sprintf("%.1f", fresh_cur$throughput[fresh_cur$scenario=="Current"]) == output$kpi_throughput)
  session$setInputs(paint_cap=1)
  invisible(output$scenario_table)
  invisible(output$reliability_table)
  session$setInputs(load_body_shop_method="empirical",load_body_shop_observations="0.5, 1.0\n1.5",
    auto_paint_method="distribution",auto_paint_distribution="lognormal",auto_paint_sd=2,
    unload_paint_method="fixed",task_paint_method="empirical",task_paint_observations="3 4 5",run=10)
  stopifnot(identical(sim_result()$params$stations$load_timing[[1]]$observations,c(.5,1,1.5)))
  session$setInputs(compare_timing=1)
  stopifnot(nrow(timing_comparison())==3)
  stopifnot(identical(output$timing_stale_flag, "fresh"))
  invisible(output$timing_table)
  session$setInputs(paint_cap=2)
  stopifnot(identical(output$timing_stale_flag, "stale"))
  stopifnot(grepl("Settings changed since this comparison was run. Rerun comparison.", paste(as.character(output$timing_warning), collapse = " "), fixed = TRUE))
  session$setInputs(paint_cap=1)
  stopifnot(identical(output$timing_stale_flag, "fresh"))
  session$setInputs(noncyclic=FALSE,run=11)
  stopifnot(all(purrr::map_dbl(sim_result()$params$stations$task_timing,e$timing_mean)==0))
  session$setInputs(staffing="shared",op_all=3,run=2)
  stopifnot(nrow(sim_result()$params$operators)==1)
  session$setInputs(staffing="dedicated",op_body_shop=1,op_paint=1,op_final_assembly=2,op_inspection=1,run=3)
  stopifnot(sum(sim_result()$params$operators$count)==5)
})
stopifnot(identical(e$parse_observations("1, 2; 3\n4"),c(1,2,3,4)))
stopifnot(inherits(try(e$parse_observations("1, bad, 2"),silent=TRUE),"try-error"))
# Sidebar defaults the controls send after first paint must match the NULL fallback,
# or the auto-run would be flagged stale before the user edits anything.
shiny::testServer(e$server, {
  fp0 <- e$params_fingerprint(build_params())
  session$setInputs(staffing="zones", op_a=2, op_b=2, repair_staff=1, reliability=TRUE, noncyclic=TRUE,
    sim_length=480, warmup=300, planned_rate=15, n_reps=5, seed=20261027,
    body_cap=2, body_mean=6, paint_cap=1, paint_mean=8, fa_cap=2, fa_mean=7, insp_cap=1, insp_mean=3)
  for (i in seq_len(nrow(e$default_params()$stations))) {
    st <- e$default_params()$stations[i,]
    vals <- list(
      load=st$load_min, unload=st$unload_min,
      mtbf=if (is.finite(st$mtbf_min)) st$mtbf_min else 10000,
      mttr=st$mttr_min, interval=st$task_interval_min, task=st$task_duration_min
    )
    for (id in names(vals)) do.call(session$setInputs, setNames(list(vals[[id]]), paste0(id, "_", st$name)))
    do.call(session$setInputs, setNames(list(0L), paste0("every_", st$name)))
    for (activity in c("load", "auto", "unload", "task")) {
      method <- if (activity=="auto") "distribution" else "fixed"
      dist <- if (activity=="auto") st$dist else "normal"
      sd <- if (activity=="auto" && is.finite(st$sd_min)) st$sd_min else 0.25
      do.call(session$setInputs, setNames(list(method), paste0(activity, "_", st$name, "_method")))
      do.call(session$setInputs, setNames(list(dist), paste0(activity, "_", st$name, "_distribution")))
      do.call(session$setInputs, setNames(list(sd), paste0(activity, "_", st$name, "_sd")))
    }
  }
  stopifnot(identical(fp0, e$params_fingerprint(build_params())))
  # Shiny may return a whole-number SD as an integer. That is the same setting.
  session$setInputs(auto_final_assembly_sd=1L)
  stopifnot(identical(fp0, e$params_fingerprint(build_params())))
  session$setInputs(paint_cap=2)
  stopifnot(!identical(fp0, e$params_fingerprint(build_params())))
})
cat("UI default signature matches first load\n")
set.seed(1)
ridge_zero <- tibble::tibble(group = rep("Inspection", 6), metric = 0)
ridge_mixed <- tibble::tibble(
  group = factor(rep(c("Paint", "Inspection"), each = 6), levels = c("Paint", "Inspection")),
  metric = c(stats::rnorm(6, 0.2, 0.02), rep(0, 6))
)
layer_geoms <- function(built) {
  vapply(built$plot$layers, function(layer) class(layer$geom)[1], character(1))
}
density_xs <- function(built) {
  geoms <- layer_geoms(built)
  idx <- which(geoms == "GeomDensity")
  stopifnot(length(idx) >= 1L)
  dplyr::bind_rows(built$data[idx])
}
rug_rows <- function(built) {
  geoms <- layer_geoms(built)
  stopifnot(!any(geoms == "GeomPoint"))
  idx <- which(geoms == "GeomRug")
  stopifnot(length(idx) == 1L)
  built$data[[idx]]
}
for (d in list(ridge_zero, ridge_mixed)) {
  built <- ggplot2::ggplot_build(e$replication_densities(d, "group", "metric", "Downtime fraction", "check"))
  stopifnot(length(built$data) >= 1L)
  rugs <- rug_rows(built)
  stopifnot(nrow(rugs) == nrow(d))
}
built <- ggplot2::ggplot_build(e$replication_densities(ridge_zero, "group", "metric", "Downtime fraction", "check"))
stopifnot(!any(layer_geoms(built) == "GeomDensity"))
stopifnot(any(layer_geoms(built) == "GeomVline"))
built <- ggplot2::ggplot_build(e$replication_densities(ridge_mixed, "group", "metric", "Downtime fraction", "check"))
stopifnot(any(layer_geoms(built) == "GeomDensity"), any(layer_geoms(built) == "GeomVline"))
dens <- density_xs(built)
stopifnot(min(dens$x, na.rm = TRUE) >= -1e-8, max(dens$x, na.rm = TRUE) <= 1 + 1e-6)
palette <- c(Paint = "#d95f0e", Inspection = "#756bb1")
built <- ggplot2::ggplot_build(e$replication_densities(
  ridge_mixed, "group", "metric", "Downtime fraction", "check", fill_values = palette
))
rugs <- rug_rows(built)
stopifnot(length(unique(rugs$colour)) >= 2L)
stopifnot(all(unique(rugs$colour) %in% unname(palette)))
# Each operator pool has its own color, and the rug uses that same color.
# A station palette passed by mistake must not collapse both pools to one color
# or warn that the manual scale shares no levels with the data.
op <- tibble::tibble(
  label = factor(rep(c("upstream", "downstream"), each = 6), levels = c("upstream", "downstream")),
  avg_util = c(stats::rnorm(6, 0.25, 0.02), stats::rnorm(6, 0.55, 0.02))
)
built <- ggplot2::ggplot_build(e$replication_densities(op, "label", "avg_util", "Operator utilization", "pools"))
# The opaque line layer maps colour; the fill layer fixes colour to NA.
line_idx <- which(vapply(seq_along(built$plot$layers), function(i) {
  layer_geoms(built)[[i]] == "GeomDensity" && is.null(built$plot$layers[[i]]$aes_params$colour)
}, logical(1)))
stopifnot(length(line_idx) == 1L)
line_cols <- unique(built$data[[line_idx]]$colour)
stopifnot(length(line_cols) == 2L)
rugs <- rug_rows(built)
stopifnot(setequal(unique(rugs$colour), line_cols))
stopifnot(nrow(rugs) == nrow(op))
built <- ggplot2::ggplot_build(e$replication_densities(
  op, "label", "avg_util", "Operator utilization", "pools", fill_values = e$station_colors
))
line_idx <- which(vapply(seq_along(built$plot$layers), function(i) {
  layer_geoms(built)[[i]] == "GeomDensity" && is.null(built$plot$layers[[i]]$aes_params$colour)
}, logical(1)))
stopifnot(length(unique(built$data[[line_idx]]$colour)) == 2L)
# Quarto defines the same mapper. Upstream and downstream stay distinct there too.
qmd <- readLines("quarto/line_sim.qmd")
qstart <- grep("^ridge_fill_values <- function", qmd)[1]
qend <- grep("^density_bounds <- function", qmd)[1] - 1L
qenv <- new.env(parent = baseenv())
qenv$station_colors <- e$station_colors
qenv$station_labels <- e$station_labels
eval(parse(text = qmd[qstart:qend]), qenv)
qcols <- qenv$ridge_fill_values(c("upstream", "downstream"))
stopifnot(length(unique(unname(qcols))) == 2L)
stopifnot(identical(names(qcols), c("upstream", "downstream")))
cat("Density checks passed\n")
# One station piled at 100% must not hide the station that still has spread.
# Boxes and points share a color. Jitter stays on the station axis. Outliers
# are not drawn a second time by the box.
flat <- tibble::tibble(
  label = factor(rep(c("Body shop", "Paint"), each = 8), levels = c("Body shop", "Paint")),
  avg_util = c(stats::rnorm(8, 0.5, 0.04), rep(1, 8))
)
built_flat <- ggplot2::ggplot_build(e$occupancy_boxes(flat))
flat_geoms <- layer_geoms(built_flat)
box_idx <- which(flat_geoms == "GeomBoxplot")
point_idx <- which(flat_geoms == "GeomPoint")
stopifnot(length(box_idx) == 1L, length(point_idx) == 1L)
outlier_shape <- built_flat$plot$layers[[box_idx]]$geom_params$outlier.shape
if (is.null(outlier_shape)) outlier_shape <- built_flat$plot$layers[[box_idx]]$geom_params$outlier_gp$shape
stopifnot(identical(outlier_shape, NA))
stopifnot(identical(built_flat$plot$layers[[point_idx]]$aes_params$size, 3))
stopifnot(identical(built_flat$plot$layers[[point_idx]]$aes_params$alpha, 0.5))
stopifnot(identical(built_flat$plot$layers[[box_idx]]$aes_params$alpha, 0.5))
stopifnot(identical(built_flat$plot$layers[[point_idx]]$position$height, 0))
stopifnot(inherits(built_flat$plot$coordinates, "CoordFlip"))
stopifnot(is.null(built_flat$plot$scales$get_scales("y")$limits))
stopifnot(nrow(built_flat$data[[point_idx]]) == nrow(flat))
stopifnot(any(abs(built_flat$data[[point_idx]]$y - 1) < 1e-8))
point_cols <- built_flat$data[[point_idx]] %>%
  dplyr::distinct(group, colour)
box_cols <- built_flat$data[[box_idx]] %>%
  dplyr::filter(!is.na(fill)) %>%
  dplyr::distinct(group, colour, fill)
stopifnot(nrow(point_cols) == 2L)
stopifnot(all(box_cols$colour == box_cols$fill))
stopifnot(setequal(point_cols$colour, box_cols$colour))
# The spread station's box is not squashed by the station sitting at 100%.
box_ranges <- built_flat$data[[box_idx]] %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(span = diff(range(c(ymin, ymax), na.rm = TRUE)), .groups = "drop")
stopifnot(any(box_ranges$span > 0), any(box_ranges$span < 1e-8))
qmd_lines <- readLines("quarto/line_sim.qmd")
stopifnot(any(grepl("outlier.shape = NA", qmd_lines, fixed = TRUE)))
stopifnot(any(grepl("occupancy_boxes", qmd_lines, fixed = TRUE)))
stopifnot(!any(grepl("ggridges", qmd_lines, fixed = TRUE)))
stopifnot(!any(grepl("ggrain", qmd_lines, fixed = TRUE)))
stopifnot(!any(grepl("base_size = 13", qmd_lines, fixed = TRUE)))
stopifnot(!any(grepl("base_size = 13", readLines("app/app.R"), fixed = TRUE)))
stopifnot(!any(grepl("base_size = 13", readLines("app/R/comparison.R"), fixed = TRUE)))
stopifnot(any(grepl("theme_sim", qmd_lines, fixed = TRUE)))
cat("Occupancy boxplot checks passed\n")
one_shift <- tibble::tibble(shift = 1L, n_completed = c(48, 50, 51, 49, 52, 47))
shift_built <- ggplot2::ggplot_build(e$shift_output_plot(one_shift, 53.3))
shift_geoms <- vapply(shift_built$plot$layers, function(layer) class(layer$geom)[1], character(1))
shift_stats <- vapply(shift_built$plot$layers, function(layer) class(layer$stat)[1], character(1))
stopifnot(any(shift_stats == "StatBin"), any(shift_geoms == "GeomDensity"), any(shift_geoms == "GeomRug"))
stopifnot(any(shift_geoms == "GeomVline"))
hist_layer <- shift_built$plot$layers[[which(shift_stats == "StatBin")[1]]]
stopifnot(identical(hist_layer$aes_params$fill, "#41b6c4"))
stopifnot(identical(hist_layer$aes_params$alpha, 0.55))
dens_layer <- shift_built$plot$layers[[which(shift_geoms == "GeomDensity")[1]]]
stopifnot(identical(dens_layer$aes_params$colour, "#253494"))
stopifnot(abs(dens_layer$aes_params$linewidth - 1.15) < 1e-8)
hist_breaks <- hist_layer$stat_params$breaks
stopifnot(length(hist_breaks) >= 2L, all(abs(diff(hist_breaks) - 1) < 1e-8))
stopifnot(identical(e$shift_binwidth(one_shift$n_completed), 1L))
stopifnot(e$shift_binwidth(c(1, 10, 40, 80)) > 1L)
rug_layer <- shift_built$plot$layers[[which(shift_geoms == "GeomRug")[1]]]
stopifnot(abs(rug_layer$aes_params$alpha - 0.6) < 1e-8)
stopifnot(abs(rug_layer$position$width - 0.15) < 1e-8)
stopifnot(identical(rug_layer$position$height, 0))
stopifnot(nrow(shift_built$data[[which(shift_geoms == "GeomRug")[1]]]) == nrow(one_shift))
stopifnot(isFALSE(dens_layer$stat_params$trim))
label_layer <- shift_built$plot$layers[[which(shift_geoms == "GeomText")[1]]]
stopifnot(label_layer$aes_params$hjust > 0.9)
x_range <- shift_built$layout$panel_params[[1]]$x.range
x_edge <- max(one_shift$n_completed, 53.3)
stopifnot(x_range[[2]] <= x_edge + 2.5, x_range[[2]] >= x_edge + 0.5)
shift_labels <- unlist(lapply(shift_built$data, function(d) if ("label" %in% names(d)) d$label else character()))
stopifnot(any(grepl("Theoretical capacity", shift_labels, fixed = TRUE)))
stopifnot(any(grepl("53 vehicles", shift_labels, fixed = TRUE)))
x_breaks <- shift_built$layout$panel_params[[1]]$x$breaks
x_breaks <- x_breaks[is.finite(x_breaks)]
stopifnot(length(x_breaks) >= 2L, all(abs(x_breaks - round(x_breaks)) < 1e-8))
stopifnot(all(diff(sort(x_breaks)) %in% c(1, 2)))
stopifnot(grepl("ignoring labor", shift_built$plot$labels$caption, fixed = TRUE))
stopifnot(grepl("blocking", shift_built$plot$labels$caption, fixed = TRUE))
stopifnot(!grepl("\n", shift_built$plot$labels$caption, fixed = TRUE))
stopifnot(nchar(shift_built$plot$labels$caption) > 32L)
stopifnot(inherits(shift_built$plot$theme$plot.caption, "element_textbox"))
title_lines <- strsplit(shift_built$plot$labels$title, "\n", fixed = TRUE)[[1]]
stopifnot(all(nchar(title_lines) <= 32L))
stopifnot(identical(shift_built$plot$theme$text$size, 16))
many_shift <- tibble::tibble(shift = rep(1:3, each = 4), n_completed = 40 + seq_len(12))
many_built <- ggplot2::ggplot_build(e$shift_output_plot(many_shift, 53))
stopifnot(inherits(many_built$plot$facet, "FacetWrap"))
cmp_shift <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 6),
  shift = 1L,
  n_completed = c(48, 49, 50, 51, 47, 52, 70, 72, 74, 71, 69, 73)
)
cmp_built <- ggplot2::ggplot_build(e$shift_output_plot(
  cmp_shift,
  tibble::tibble(set = c("Set 1", "Set 2"), capacity = c(53, 80)),
  e$comparison_colors
))
cmp_stats <- vapply(cmp_built$plot$layers, function(layer) class(layer$stat)[1], character(1))
cmp_hist <- cmp_built$data[[which(cmp_stats == "StatBin")[1]]]
stopifnot(all(c("#2474A5", "#D46B25") %in% unique(cmp_hist$fill)))
# Five replications can make the Freedman–Diaconis width wider than a seq()
# grid that stops short of the largest count. Every count has to fall in a bin.
wide5 <- c(10, 12, 14, 40, 42)
br5 <- e$shift_hist_breaks(wide5, e$shift_binwidth(wide5))
stopifnot(br5[[1]] <= min(wide5), tail(br5, 1) >= max(wide5), all(diff(br5) > 0))
wide_df <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 2),
  shift = 1L,
  n_completed = c(50, 52, 88, 87)
)
wide_warns <- character()
wide_built <- withCallingHandlers(
  ggplot2::ggplot_build(e$shift_output_plot(
    wide_df,
    tibble::tibble(set = c("Set 1", "Set 2"), capacity = c(53, 96)),
    e$comparison_colors
  )),
  warning = function(w) {
    wide_warns <<- c(wide_warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(length(wide_warns) == 0L)
wide_stats <- vapply(wide_built$plot$layers, function(layer) class(layer$stat)[1], character(1))
wide_hist <- wide_built$data[[which(wide_stats == "StatBin")[1]]]
stopifnot(sum(wide_hist$count) == nrow(wide_df))
stopifnot(any(wide_hist$count[wide_hist$fill == "#2474A5"] > 0))
stopifnot(any(wide_hist$count[wide_hist$fill == "#D46B25"] > 0))
cat("Shift output chart checks passed\n")
# Try it raises Paint to 2 bays and runs. Reset returns the placeholder line and runs.
shiny::testServer(e$server, {
  base <- sim_result()
  paint_cap <- function(res) res$params$stations$capacity[res$params$stations$name == "paint"]
  body_cap <- function(res) res$params$stations$capacity[res$params$stations$name == "body_shop"]
  stopifnot(base$provenance$run_number == 1L)
  stopifnot(paint_cap(base) == 1L, body_cap(base) == 2L)
  stopifnot(identical(base$bottleneck, "paint"))
  session$setInputs(guide_try = 1)
  tried <- sim_result()
  stopifnot(tried$provenance$run_number == 2L)
  stopifnot(paint_cap(tried) == 2L)
  stopifnot(identical(tried$bottleneck, "final_assembly"))
  stopifnot(tried$headline$throughput_per_hr_mean > base$headline$throughput_per_hr_mean)
  stopifnot(tried$headline$end_unreleased_backlog_mean < base$headline$end_unreleased_backlog_mean)
  stopifnot(tried$headline$cycle_time_mean_mean < base$headline$cycle_time_mean_mean)
  stopifnot(identical(output$main_stale_flag, "fresh"))
  comp <- two_set_study$results()
  stopifnot(!is.null(comp))
  stopifnot(identical(comp$runs[[1]], base))
  stopifnot(identical(comp$runs[[2]], tried))
  stopifnot(paint_cap(comp$runs[[1]]) == 1L, paint_cap(comp$runs[[2]]) == 2L)
  stopifnot(identical(two_set_study$titles(), e$comparison_guided_labels))
  stopifnot(!isTRUE(two_set_study$dirty()))
  ranked <- e$comparison_rank_changes(comp$runs)
  cap <- e$comparison_change_caption(comp$runs, ranked$scores)
  stopifnot(grepl("shifted from Paint to Final assembly", cap, fixed = TRUE))
  stopifnot(grepl("Throughput rose", cap, fixed = TRUE))
  stopifnot(grepl("mean lead time fell", cap, fixed = TRUE))
  # Shop-floor WIP can rise when extra paint capacity admits more work.
  if (tried$headline$avg_wip_mean < base$headline$avg_wip_mean) {
    stopifnot(grepl("WIP fell", cap, fixed = TRUE))
  } else if (tried$headline$avg_wip_mean > base$headline$avg_wip_mean) {
    stopifnot(grepl("WIP rose", cap, fixed = TRUE))
  }
  stopifnot(nrow(ranked$shown) >= 1L, nrow(ranked$shown) <= e$comparison_highlight_n)
  exported <- e$comparison_highlight_export(ranked, two_set_study$titles())
  stopifnot(nrow(exported) == nrow(ranked$shown) * base$params$n_replications * 2L)
  stopifnot(all(e$comparison_guided_labels %in% exported$set))
  stopifnot(all(seq_len(base$params$n_replications) %in% exported$replication[exported$rank == 1]))
  diff <- e$comparison_diff(comp$runs[[1]]$params, comp$runs[[2]]$params, two_set_study$titles())
  stopifnot(nrow(diff) == 1L)
  stopifnot(grepl("Paint", diff$Parameter), grepl("Parallel bays", diff$Parameter))
  stopifnot(identical(names(diff), c("Parameter", e$comparison_guided_labels)))
  cat("Guided comparison metrics:", paste(ranked$shown$label, collapse = " | "), "\n")
  # The result on screen is already 2 bays, so Try it cannot reuse it as the
  # 1-bay baseline. That baseline is simulated with the same seed and
  # replication count; the 2-bay run is the guided simulation itself.
  session$setInputs(n_reps = 2)
  session$setInputs(run = 1)
  held <- sim_result()
  stopifnot(paint_cap(held) == 2L, held$params$n_replications == 2L)
  session$setInputs(guide_try = 1)
  retried <- sim_result()
  comp2 <- two_set_study$results()
  stopifnot(identical(comp2$runs[[2]], retried))
  stopifnot(paint_cap(comp2$runs[[1]]) == 1L, paint_cap(comp2$runs[[2]]) == 2L)
  stopifnot(comp2$runs[[1]]$params$n_replications == 2L)
  stopifnot(comp2$runs[[1]]$params$base_seed == retried$params$base_seed)
  stopifnot(!identical(comp2$runs[[1]], held))
  stopifnot(!identical(comp2$runs[[1]], base))
  stopifnot(identical(two_set_study$titles(), e$comparison_guided_labels))
  session$setInputs(body_cap = 4, n_reps = 2)
  session$setInputs(guide_reset = 1)
  reset <- sim_result()
  stopifnot(reset$provenance$run_number == 5L)
  stopifnot(paint_cap(reset) == 1L, body_cap(reset) == 2L)
  stopifnot(reset$params$n_replications == 5L)
  stopifnot(reset$params$warmup_min == 300)
  stopifnot(reset$params$wip_limit == 20L)
  stopifnot(identical(reset$bottleneck, base$bottleneck))
  stopifnot(isTRUE(all.equal(reset$headline$throughput_per_hr_mean, base$headline$throughput_per_hr_mean)))
  stopifnot(isTRUE(all.equal(reset$headline$avg_wip_mean, base$headline$avg_wip_mean)))
  stopifnot(identical(output$main_stale_flag, "fresh"))
  stopifnot(grepl("Paint 1", reset$provenance$summary, fixed = TRUE))
  stopifnot(grepl("Runs 5", reset$provenance$summary, fixed = TRUE))
})
qmd_guide <- readLines("quarto/line_sim.qmd")
stopifnot(any(grepl('id = "guided_example"', qmd_guide, fixed = TRUE)))
stopifnot(any(grepl("Try it", qmd_guide, fixed = TRUE)))
stopifnot(any(grepl("Reset to defaults", qmd_guide, fixed = TRUE)))
stopifnot(any(grepl("guide_try", qmd_guide, fixed = TRUE)))
stopifnot(any(grepl("guide_reset", qmd_guide, fixed = TRUE)))
stopifnot(any(grepl("guided_example_ui()", qmd_guide, fixed = TRUE)))
cat("Guided example checks passed\n")
cat("Manual-scale level-mismatch warnings:", length(scale_warns$msgs), "\n")
stopifnot(length(scale_warns$msgs) == 0L)
cat("Shiny server checks passed\n")
