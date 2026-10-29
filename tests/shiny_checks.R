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
  stopifnot(d$n_replications == 15L)
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
  stopifnot(grepl("Runs 15", settings, fixed = TRUE))
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
  "Station occupancy", "Operators", "Downtime", "Staffing comparison",
  "WIP", "Queues", "Cycle time", "Station metrics", "Timing comparison", "Downloads",
  "640px", "clamp(520px, 60vh, 720px)", "gap: 0.5rem", "Run settings",
  "Compare two sets", "Results are illustrative only"
)) stopifnot(page_has(label))
# Downloads moved out of the sidebar accordion into the Lead time tab.
stopifnot(!grepl("Download data", ui_html, fixed = TRUE))
# Run settings sits under the results tabs and starts collapsed.
kpi_at <- regexpr('id="headline_kpis"', ui_html)
tabs_at <- regexpr('id="results_tabs"', ui_html)
settings_at <- regexpr('class="run-settings', ui_html)
guide_at <- regexpr('id="guided_example"', ui_html)
stopifnot(kpi_at[[1]] > 0L, tabs_at[[1]] > kpi_at[[1]], settings_at[[1]] > tabs_at[[1]])
stopifnot(guide_at[[1]] > 0L, guide_at[[1]] < kpi_at[[1]])
# Run settings stays closed. The guided example is the disclosure that starts open.
stopifnot(!grepl('class="run-settings[^"]*" open', ui_html))
stopifnot(grepl("<details open>", ui_html, fixed = TRUE))
stopifnot(page_has("Guided example"))
stopifnot(page_has("Try it"))
stopifnot(page_has("Reset to defaults"))
stopifnot(page_has("not a prediction for any real plant or company"))
stopifnot(page_has("fixed duration"))
stopifnot(page_has("measured observations"))
stopifnot(!page_has("exponential or normal distributions"))
cat("Results tab layout check passed\n")
cat("First-load check passed\n")

# Sidebar accordion: four panels, in order. Only Arrivals & run starts open,
# and more than one panel may be open (multiple = TRUE).
accordion_titles <- function(html) {
  titles <- unlist(regmatches(html, gregexpr('<div class="accordion-title">[^<]*</div>', html)))
  gsub("&amp;", "&", gsub("</div>", "", gsub('<div class="accordion-title">', "", titles, fixed = TRUE), fixed = TRUE), fixed = TRUE)
}
panel_titles <- c("Arrivals & run", "Capacity", "Cycle times", "Operators & reliability")
# Compare two sets adds its own accordions. Scope the four-panel contract to the sidebar.
sidebar_html <- as.character(xml2::xml_find_first(xml2::read_html(ui_html), "//*[@id='line_controls']"))
stopifnot(identical(accordion_titles(sidebar_html), panel_titles))
stopifnot(grepl('id="line_controls"', ui_html, fixed = TRUE))
stopifnot(!grepl("accordion autoclose", sidebar_html, fixed = TRUE))
stopifnot(!grepl("data-bs-parent=", sidebar_html, fixed = TRUE))
stopifnot(!grepl('data-value="Stations"', ui_html, fixed = TRUE))
stopifnot(!page_has("Activity timing & reliability by station"))
items <- strsplit(sidebar_html, '<div class="accordion-item" ', fixed = TRUE)[[1]][-1]
stopifnot(length(items) == 4L)
panel_open <- function(chunk) {
  header <- sub('<div class="accordion-body".*', "", chunk)
  collapse <- regmatches(chunk, regexpr('class="accordion-collapse [^"]*"', chunk))
  grepl('aria-expanded="true"', header, fixed = TRUE) &&
    identical(collapse, 'class="accordion-collapse collapse show"')
}
stopifnot(panel_open(items[[1]]))
stopifnot(!any(vapply(items[-1], panel_open, logical(1))))
# Run settings live in the open panel. Capacity, timing, and crews are grouped by station.
stopifnot(grepl('id="n_reps"', items[[1]], fixed = TRUE), grepl('id="n_reps" type="number" class="shiny-input-number form-control" value="15"', items[[1]], fixed = TRUE))
stopifnot(grepl('id="run"', items[[1]], fixed = TRUE), grepl('id="planned_rate"', items[[1]], fixed = TRUE))
stopifnot(grepl('id="body_cap"', items[[2]], fixed = TRUE), grepl(">Body shop<", items[[2]], fixed = TRUE))
stopifnot(grepl('id="paint_cap"', items[[2]], fixed = TRUE), grepl('id="fa_cap"', items[[2]], fixed = TRUE), grepl('id="insp_cap"', items[[2]], fixed = TRUE))
stopifnot(!grepl('id="body_mean"', items[[2]], fixed = TRUE))
stopifnot(grepl('id="body_mean"', items[[3]], fixed = TRUE), grepl("cycle_body_shop", items[[3]], fixed = TRUE))
stopifnot(grepl('id="paint_mean"', items[[3]], fixed = TRUE), grepl("cycle_paint", items[[3]], fixed = TRUE))
stopifnot(grepl('id="fa_mean"', items[[3]], fixed = TRUE), grepl("cycle_final_assembly", items[[3]], fixed = TRUE))
stopifnot(grepl('id="insp_mean"', items[[3]], fixed = TRUE), grepl("cycle_inspection", items[[3]], fixed = TRUE))
stopifnot(grepl('id="staffing"', items[[4]], fixed = TRUE), grepl("operator_controls", items[[4]], fixed = TRUE))
stopifnot(grepl('id="reliability"', items[[4]], fixed = TRUE), grepl('id="repair_staff"', items[[4]], fixed = TRUE), grepl('id="noncyclic"', items[[4]], fixed = TRUE))
stopifnot(grepl("reliability_body_shop", items[[4]], fixed = TRUE), grepl("reliability_paint", items[[4]], fixed = TRUE))
stopifnot(grepl("reliability_final_assembly", items[[4]], fixed = TRUE), grepl("reliability_inspection", items[[4]], fixed = TRUE))
cat("Sidebar accordion check passed\n")
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
    sim_length=480, warmup=60, planned_rate=15, n_reps=15, seed=20261027,
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
ridge_geoms <- function(built) {
  vapply(built$plot$layers, function(layer) class(layer$geom)[1], character(1))
}
ridge_rugs <- function(built) {
  geoms <- ridge_geoms(built)
  stopifnot(!any(grepl("Point", geoms)))
  idx <- which(grepl("Segment", geoms))
  stopifnot(length(idx) >= 1L)
  seg <- dplyr::bind_rows(built$data[idx])
  stopifnot(nrow(seg) >= 1L, all(seg$yend > seg$y))
  seg
}
for (d in list(ridge_zero, ridge_mixed)) {
  built <- ggplot2::ggplot_build(e$replication_ridges(d, "group", "metric", "Downtime fraction", "check"))
  stopifnot(length(built$data) >= 1L)
  ridge_rugs(built)
}
built <- ggplot2::ggplot_build(e$replication_ridges(ridge_mixed, "group", "metric", "Downtime fraction", "check"))
seg <- ridge_rugs(built)
stopifnot(length(unique(round(seg$y, 5))) >= 2L)
palette <- c(Paint = "#d95f0e", Inspection = "#756bb1")
built <- ggplot2::ggplot_build(e$replication_ridges(
  ridge_mixed, "group", "metric", "Downtime fraction", "check", fill_values = palette
))
seg <- ridge_rugs(built)
stopifnot(length(unique(seg$colour)) >= 2L)
stopifnot(all(unique(seg$colour) %in% unname(palette)))
# Each operator pool has its own fill, and the rug uses that same color.
# A station palette passed by mistake must not collapse both pools to one color
# or warn that the manual scale shares no levels with the data.
op <- tibble::tibble(
  label = factor(rep(c("upstream", "downstream"), each = 6), levels = c("upstream", "downstream")),
  avg_util = c(stats::rnorm(6, 0.25, 0.02), stats::rnorm(6, 0.55, 0.02))
)
built <- ggplot2::ggplot_build(e$replication_ridges(op, "label", "avg_util", "Operator utilization", "pools"))
dens <- built$data[[which(ridge_geoms(built) == "GeomDensityRidges")[1]]]
fills <- unique(dens$fill[!is.na(dens$fill)])
stopifnot(length(fills) == 2L)
seg <- ridge_rugs(built)
stopifnot(setequal(unique(seg$colour), fills))
built <- ggplot2::ggplot_build(e$replication_ridges(
  op, "label", "avg_util", "Operator utilization", "pools", fill_values = e$station_colors
))
dens <- built$data[[which(ridge_geoms(built) == "GeomDensityRidges")[1]]]
stopifnot(length(unique(dens$fill[!is.na(dens$fill)])) == 2L)
# Quarto defines the same mapper. Upstream and downstream stay distinct there too.
qmd <- readLines("quarto/line_sim.qmd")
qstart <- grep("^ridge_fill_values <- function", qmd)[1]
qend <- grep("^replication_ridges <- function", qmd)[1] - 1L
qenv <- new.env(parent = baseenv())
qenv$station_colors <- e$station_colors
qenv$station_labels <- e$station_labels
eval(parse(text = qmd[qstart:qend]), qenv)
qcols <- qenv$ridge_fill_values(c("upstream", "downstream"))
stopifnot(length(unique(unname(qcols))) == 2L)
stopifnot(identical(names(qcols), c("upstream", "downstream")))
cat("Ridgeline checks passed\n")
# 50 replications. Paint's occupancy piles up at 100%. geom_rain's default
# violin scale ("area") divides every density by that peak, so the stations
# that still have spread lose their half-violins. scale = "width" keeps a cloud
# for each of those stations. A station with no spread keeps its points.
p50 <- e$default_params()
p50$n_replications <- 50L
res50 <- e$run_line_sim(p50)
per50 <- res50$utilization_by_rep %>%
  dplyr::mutate(label = factor(e$label_station(station), levels = unname(e$station_labels)))
means50 <- res50$station_metrics %>%
  dplyr::mutate(label = factor(e$label_station(station), levels = unname(e$station_labels)))
spread_labels <- per50 %>%
  dplyr::group_by(label) %>%
  dplyr::summarise(s = stats::sd(avg_util), .groups = "drop") %>%
  dplyr::filter(is.finite(s), s > sqrt(.Machine$double.eps)) %>%
  dplyr::pull(label)
stopifnot(length(spread_labels) >= 1L)
built50 <- ggplot2::ggplot_build(e$occupancy_raincloud(per50, means50))
geoms50 <- vapply(built50$plot$layers, function(layer) class(layer$geom)[1], character(1))
violin_idx <- which(grepl("Violin", geoms50))
stopifnot(length(violin_idx) >= 1L)
vd <- built50$data[[violin_idx[[1]]]]
clouds <- vd %>%
  dplyr::mutate(station = levels(per50$label)[group]) %>%
  dplyr::group_by(station) %>%
  dplyr::summarise(
    n = dplyr::n(),
    max_vw = max(violinwidth, na.rm = TRUE),
    yrange = diff(range(y, na.rm = TRUE)),
    .groups = "drop"
  )
for (station in spread_labels) {
  row <- clouds[clouds$station == station, ]
  stopifnot(nrow(row) == 1L, row$n >= 3L, is.finite(row$max_vw), row$max_vw > 0.5, row$yrange > 0)
}
flat <- tibble::tibble(
  label = factor(rep(c("Body shop", "Paint"), each = 8), levels = c("Body shop", "Paint")),
  avg_util = c(stats::rnorm(8, 0.5, 0.04), rep(1, 8))
)
flat_means <- flat %>%
  dplyr::group_by(label) %>%
  dplyr::summarise(avg_util_mean = mean(avg_util), .groups = "drop")
built_flat <- ggplot2::ggplot_build(e$occupancy_raincloud(flat, flat_means))
flat_geoms <- vapply(built_flat$plot$layers, function(layer) class(layer$geom)[1], character(1))
point_idx <- which(grepl("Point", flat_geoms))
stopifnot(length(point_idx) >= 1L)
stopifnot(any(abs(built_flat$data[[point_idx[[1]]]]$y - 1) < 1e-8))
# Quarto draws the same cloud and the same text size.
qmd_lines <- readLines("quarto/line_sim.qmd")
stopifnot(any(grepl('scale = "width"', qmd_lines, fixed = TRUE)))
stopifnot(!any(grepl("base_size = 13", qmd_lines, fixed = TRUE)))
stopifnot(!any(grepl("base_size = 13", readLines("app/app.R"), fixed = TRUE)))
stopifnot(!any(grepl("base_size = 13", readLines("app/R/comparison.R"), fixed = TRUE)))
stopifnot(any(grepl("theme_sim", qmd_lines, fixed = TRUE)))
cat("Occupancy raincloud checks passed\n")
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
  stopifnot(reset$params$n_replications == 15L)
  stopifnot(identical(reset$bottleneck, base$bottleneck))
  stopifnot(isTRUE(all.equal(reset$headline$throughput_per_hr_mean, base$headline$throughput_per_hr_mean)))
  stopifnot(isTRUE(all.equal(reset$headline$avg_wip_mean, base$headline$avg_wip_mean)))
  stopifnot(identical(output$main_stale_flag, "fresh"))
  stopifnot(grepl("Paint 1", reset$provenance$summary, fixed = TRUE))
  stopifnot(grepl("Runs 15", reset$provenance$summary, fixed = TRUE))
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
