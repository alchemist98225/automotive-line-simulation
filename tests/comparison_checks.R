e<-new.env();source("app/app.R",local=e,chdir=TRUE)
# Every flow chart must preserve renamed and guided scenario labels.
flow_original <- e$comparison_flow
e$comparison_flow <- function(...) {
  chart <- flow_original(...)
  stopifnot(!anyNA(chart$data$set))
  chart
}
p<-e$default_params();p$n_replications<-3L
shiny::testServer(e$parameter_editor_server,{
  set_params(p)
  stopifnot(e$comparison_same(params(),e$normalize_params(p)))
  invisible(output$controls) # includes collapsed nested activity controls
  session$setInputs(v1_capacity_paint=2,v1_n_replications=4)
  stopifnot(params()$stations$capacity[2]==2,params()$n_replications==4)
  session$setInputs(v1_paint_auto_method="empirical",v1_paint_auto_observations="4, 5, 6")
  stopifnot(identical(params()$stations$auto_timing[[2]]$observations,c(4,5,6)))
  # Import after edits must ignore all controls from the previous revision.
  set_params(p)
  stopifnot(params()$stations$capacity[2]==1,params()$n_replications==3)
  stopifnot(e$comparison_same(params(),e$normalize_params(p)))
  session$setInputs(v2_paint_auto_method="distribution",v2_paint_auto_distribution="normal",v2_paint_auto_mean=1,v2_paint_auto_sd=3)
  stopifnot(params()$stations$auto_timing[[2]]$mean_min==1)
  session$setInputs(v2_pool_upstream=0)
  stopifnot(inherits(try(params(),silent=TRUE),"try-error"))
})
# Record editor-collapse requests. A successful run must both ask bslib to close
# the accordions and schedule the flush that actually delivers that message.
close_log <- new.env(parent = emptyenv())
close_log$calls <- list()
e$accordion_panel_close <- function(id, values = NULL, session = NULL) {
  if (is.null(session)) session <- shiny::getDefaultReactiveDomain()
  close_log$calls[[length(close_log$calls) + 1L]] <- list(id = id, values = values)
  bslib::accordion_panel_close(id = id, values = values, session = session)
}
frame_has_height <- function(html, px) {
  html <- paste(as.character(html), collapse = "")
  grepl(sprintf("--ridge-h:%dpx", px), html, fixed = TRUE) &&
    grepl(sprintf("%dpx", px), html, fixed = TRUE) &&
    grepl("comparison-ridge-plot", html, fixed = TRUE)
}
shiny::testServer(e$comparison_page_server,args=list(main_params=shiny::reactive(p),active_page=shiny::reactive("compare_sets")),{
  session$flushReact()
  stopifnot(initialized(),e$comparison_same(one$params(),two$params()),nrow(e$comparison_diff(one$params(),two$params()))==0)
  same_html <- paste(as.character(output$draft_diff), collapse = "")
  stopifnot(grepl(e$comparison_identical_note, same_html, fixed = TRUE))
  stopifnot(length(close_log$calls) == 0L)
  root <- session
  while (inherits(root, "session_proxy")) root <- .subset2(root, "parent")
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  sent$custom <- list()
  sent$flushes <- 0L
  root$requestFlush <- function() { sent$flushes <- sent$flushes + 1L }
  root$sendInputMessage <- function(inputId, message) {
    sent$msgs[[length(sent$msgs) + 1L]] <- list(id = inputId, message = message)
  }
  root$sendCustomMessage <- function(type, message) {
    sent$custom[[length(sent$custom) + 1L]] <- list(type = type, message = message)
  }
  expect_collapse_message <- function(n_expected) {
    hits <- Filter(function(x) identical(x$type, "two-set-collapse-editors"), sent$custom)
    stopifnot(length(hits) == n_expected)
    if (!n_expected) return(invisible())
    msg <- hits[[n_expected]]$message
    stopifnot(identical(msg$ids, c(session$ns("one-editor_panels"), session$ns("two-editor_panels"))))
    stopifnot(identical(msg$scrollId, session$ns("saved_results")))
  }
  # Mock sessions run onFlush before observers, so the close message from a run
  # is delivered by the next flush. That matches the real session, where
  # requestFlush makes that next flush happen.
  deliver_closes <- function() session$flushReact()
  session$setInputs(run=1)
  stopifnot(length(close_log$calls) == 2L)
  stopifnot(isTRUE(close_log$calls[[1]]$values), isTRUE(close_log$calls[[2]]$values))
  stopifnot(identical(close_log$calls[[1]]$id, session$ns("one-editor_panels")))
  stopifnot(identical(close_log$calls[[2]]$id, session$ns("two-editor_panels")))
  stopifnot(sent$flushes >= 2L)
  deliver_closes()
  stopifnot(length(sent$msgs) == 2L)
  stopifnot(identical(sent$msgs[[1]]$id, session$ns("one-editor_panels")))
  stopifnot(identical(sent$msgs[[2]]$id, session$ns("two-editor_panels")))
  stopifnot(identical(sent$msgs[[1]]$message$method, "close"), isTRUE(sent$msgs[[1]]$message$values))
  stopifnot(identical(sent$msgs[[2]]$message$method, "close"), isTRUE(sent$msgs[[2]]$message$values))
  expect_collapse_message(1L)
  occ_h <- e$comparison_ridge_height(4L)
  labor_h <- e$comparison_ridge_height(2L)
  stopifnot(occ_h > 720L, labor_h >= 520L, occ_h > labor_h)
  stopifnot(frame_has_height(output$occupancy_frame, occ_h))
  stopifnot(frame_has_height(output$labor_frame, labor_h))
  stopifnot(frame_has_height(output$downtime_frame, occ_h))
  r<-results(); stopifnot(!is.null(r),!dirty())
  stopifnot(isTRUE(all.equal(r$runs[[1]]$summaries,r$runs[[2]]$summaries,tolerance=1e-12)))
  story_same <- paste(as.character(output$change_story), collapse = " ")
  stopifnot(grepl("illustrative only", story_same, fixed = TRUE))
  stopifnot(grepl("busiest station", story_same, fixed = TRUE))
  stopifnot(grepl("not predictions for any employer", story_same, fixed = TRUE))
  invisible(output$highlights)
  invisible(output$kpis);invisible(output$summary);invisible(output$stations)
  invisible(output$distribution);invisible(output$cycles);invisible(output$occupancy)
  invisible(output$labor);invisible(output$downtime);invisible(output$wip);invisible(output$queues)
  b<-two$params();b$stations$capacity[2]<-2L;two$set_params(b)
  stopifnot(dirty(),results()$params[[2]]$stations$capacity[2]==1,one$params()$stations$capacity[2]==1)
  session$flushReact()
  diff_one <- paste(as.character(output$draft_diff), collapse = "")
  stopifnot(grepl("Paint", diff_one, fixed = TRUE), grepl("Parallel bays", diff_one, fixed = TRUE))
  stopifnot(!grepl(e$comparison_identical_note, diff_one, fixed = TRUE))
  session$setInputs(title_one = "Alpha line", title_two = "Beta line")
  session$flushReact()
  stopifnot(identical(results(), r))
  stopifnot(identical(titles(), c("Alpha line", "Beta line")))
  invisible(output$backlog)
  diff_named <- paste(as.character(output$draft_diff), collapse = "")
  stopifnot(grepl("Alpha line", diff_named, fixed = TRUE), grepl("Beta line", diff_named, fixed = TRUE))
  kpis <- paste(as.character(output$kpis), collapse = " ")
  stopifnot(grepl("Alpha line", kpis, fixed = TRUE), grepl("Beta line", kpis, fixed = TRUE))
  stopifnot(all(c("Alpha line", "Beta line") %in% summary_data()$set))
  session$setInputs(title_one = paste(rep("Z", 80), collapse = ""), title_two = "Beta\nline")
  session$flushReact()
  stopifnot(nchar(titles()[[1]]) == 60L, identical(titles()[[2]], "Beta line"))
  stopifnot(identical(results(), r))
  invisible(output$status)
  session$setInputs(run=2)
  stopifnot(length(close_log$calls) == 4L)
  stopifnot(identical(close_log$calls[[3]]$id, session$ns("one-editor_panels")))
  stopifnot(identical(close_log$calls[[4]]$id, session$ns("two-editor_panels")))
  stopifnot(isTRUE(close_log$calls[[3]]$values), isTRUE(close_log$calls[[4]]$values))
  stopifnot(sent$flushes >= 4L)
  deliver_closes()
  stopifnot(length(sent$msgs) == 4L)
  stopifnot(identical(sent$msgs[[3]]$message$method, "close"), isTRUE(sent$msgs[[3]]$message$values))
  stopifnot(identical(sent$msgs[[4]]$message$method, "close"), isTRUE(sent$msgs[[4]]$message$values))
  expect_collapse_message(2L)
  stopifnot(!dirty(),results()$params[[2]]$stations$capacity[2]==2)
  hi <- paste(as.character(output$highlights), collapse = "")
  stopifnot(grepl("highlight_plot_1", hi, fixed = TRUE))
  stopifnot(grepl("moved the most", hi, fixed = TRUE))
  stopifnot(grepl("Every replication is drawn", hi, fixed = TRUE))
  invisible(output$highlight_plot_1)
  invisible(output$change_story)
  # Match counts without overwriting other study changes.
  b<-two$params();b$n_replications<-1L;two$set_params(b)
  session$setInputs(match_reps=1)
  stopifnot(two$params()$n_replications==3,two$params()$stations$capacity[2]==2)
  session$setInputs(clone=1)
  stopifnot(e$comparison_same(one$params(),two$params()))
  session$setInputs(import=1)
  stopifnot(e$comparison_same(one$params(),e$normalize_params(p)))
  # Import, copy, and match-reps must not collapse the editors.
  stopifnot(length(close_log$calls) == 4L)
  # An invalid empirical edit must retain old results and flag the draft.
  rev<-two$state()$revision
  do.call(session$setInputs,setNames(list("empirical"),paste0("two-v",rev,"_paint_auto_method")))
  do.call(session$setInputs,setNames(list("invalid"),paste0("two-v",rev,"_paint_auto_observations")))
  flushes_before_error <- sent$flushes
  msgs_before_error <- length(sent$msgs)
  custom_before_error <- length(sent$custom)
  before<-results();session$setInputs(run=3)
  stopifnot(!is.null(error()),identical(results(),before),dirty())
  # A failed run leaves the parameter editors open and does not schedule another close.
  stopifnot(length(close_log$calls) == 4L)
  deliver_closes()
  stopifnot(identical(sent$flushes, flushes_before_error))
  stopifnot(identical(length(sent$msgs), msgs_before_error))
  stopifnot(identical(length(sent$custom), custom_before_error))
  expect_collapse_message(2L)
  before_load <- results()
  n_close <- length(close_log$calls)
  load_pair(before_load$runs[[1]], before_load$runs[[2]], e$comparison_guided_labels)
  stopifnot(identical(titles(), e$comparison_guided_labels))
  stopifnot(identical(results()$runs[[1]], before_load$runs[[1]]))
  stopifnot(identical(results()$runs[[2]], before_load$runs[[2]]))
  stopifnot(!dirty())
  stopifnot(length(close_log$calls) == n_close + 2L)
  deliver_closes()
  expect_collapse_message(3L)
  guided_diff <- paste(as.character(output$draft_diff), collapse = "")
  stopifnot(grepl("Baseline: 1 paint bay", guided_diff, fixed = TRUE))
  stopifnot(grepl("Experiment: 2 paint bays", guided_diff, fixed = TRUE))
})
# Same empirical mean/count is insufficient for treating two sets as identical.
a<-e$normalize_params(p);b<-a
 a$stations$auto_timing[[1]]<-e$timing_spec("empirical",observations=c(1,3))
 b$stations$auto_timing[[1]]<-e$timing_spec("empirical",observations=c(2,2))
stopifnot(!e$comparison_same(a,b))
set.seed(1)
rain_df <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 6),
  throughput_per_hr = c(stats::rnorm(6, 10, 0.4), rep(8, 6))
)
built <- ggplot2::ggplot_build(e$comparison_raincloud(rain_df, "throughput_per_hr", "Throughput (vehicles/hour)"))
stopifnot(length(built$data) >= 1L)
# A tight set used to erase the wider set's cloud (violin scale = "area").
set.seed(1)
peaked <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 50),
  throughput_per_hr = c(stats::rnorm(50, 12, 0.02), stats::rnorm(50, 9, 1.1))
)
peaked_built <- ggplot2::ggplot_build(e$comparison_raincloud(peaked, "throughput_per_hr", "Throughput (vehicles/hour)"))
peaked_geoms <- vapply(peaked_built$plot$layers, function(layer) class(layer$geom)[1], character(1))
peaked_violin <- which(grepl("Violin", peaked_geoms))
stopifnot(length(peaked_violin) >= 1L)
peaked_widths <- tapply(peaked_built$data[[peaked_violin[[1]]]]$violinwidth, peaked_built$data[[peaked_violin[[1]]]]$group, max, na.rm = TRUE)
stopifnot(length(peaked_widths) >= 2L, all(peaked_widths > 0.5))
flat <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 4),
  station = "Inspection",
  downtime_fraction = 0
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
built <- ggplot2::ggplot_build(e$comparison_ridges(
  flat, "station", "downtime_fraction", "Active repair fraction", "check"
))
stopifnot(length(built$data) >= 1L)
flat_seg <- ridge_rugs(built)
stopifnot(all(c("#2474A5", "#D46B25") %in% unique(flat_seg$colour)))
mixed <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 8),
  station = rep(rep(c("Paint", "Inspection"), each = 4), 2),
  downtime_fraction = c(stats::rnorm(4, 0.2, 0.03), rep(0, 4), stats::rnorm(4, 0.35, 0.03), rep(0, 4))
)
built <- ggplot2::ggplot_build(e$comparison_ridges(
  mixed, "station", "downtime_fraction", "Active repair fraction", "check"
))
stopifnot(length(built$data) >= 1L)
mixed_seg <- ridge_rugs(built)
stopifnot(length(unique(round(mixed_seg$y, 5))) >= 2L)
stopifnot(all(c("#2474A5", "#D46B25") %in% unique(mixed_seg$colour)))
# Set colors stay on the set levels. A station-name palette must not be required.
cmp_warns <- character()
cmp <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 6),
  station = "Paint",
  avg_util = c(stats::rnorm(6, 0.4, 0.03), stats::rnorm(6, 0.7, 0.03))
)
cmp_built <- withCallingHandlers(
  ggplot2::ggplot_build(e$comparison_ridges(cmp, "station", "avg_util", "Operator utilization", "sets")),
  warning = function(w) {
    cmp_warns <<- c(cmp_warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
cmp_geoms <- vapply(cmp_built$plot$layers, function(layer) class(layer$geom)[1], character(1))
cmp_fills <- unique(cmp_built$data[[which(cmp_geoms == "GeomDensityRidges")[1]]]$fill)
stopifnot(setequal(cmp_fills[!is.na(cmp_fills)], unname(e$comparison_colors)))
stopifnot(!any(grepl("No shared levels found", cmp_warns, fixed = TRUE)))
# Height grows with the number of ridges and stays above the 720px app-wide clamp
# once there are enough stations for Inspection to be crowded.
heights <- vapply(1:6, e$comparison_ridge_height, integer(1))
stopifnot(all(diff(heights) >= 0L))
stopifnot(heights[[4]] > 720L, heights[[4]] > heights[[2]], heights[[1]] >= 520L)
stopifnot(identical(e$comparison_ridge_height(0), e$comparison_ridge_height(1)))
stopifnot(e$comparison_ridge_n(c("Paint", "Paint", "Inspection", NA)) == 2L)
stopifnot(e$comparison_ridge_n(character()) == 1L)
# Four stations (bay occupancy and repair) are taller than two operator pools.
# Four dedicated pools use the same height as four stations.
stopifnot(identical(e$comparison_ridge_height(4L), heights[[4]]))
plot_html <- paste(as.character(e$comparison_ridge_plot("bay", 4L)), collapse = "")
stopifnot(frame_has_height(plot_html, heights[[4]]))
page <- paste(as.character(e$comparison_page_ui("two_set_study")), collapse = "")
stopifnot(grepl(".comparison-ridge-plot > .shiny-plot-output", page, fixed = TRUE))
stopifnot(grepl("var(--ridge-h)", page, fixed = TRUE))
stopifnot(grepl("max-height: none", page, fixed = TRUE))
stopifnot(grepl("two-set-collapse-editors", page, fixed = TRUE))
stopifnot(grepl("accordion-collapse.show", page, fixed = TRUE))
diff_at <- regexpr('id="two_set_study-draft_diff"', page)
edit_at <- regexpr('id="two_set_study-one-controls"', page)
stopifnot(diff_at[[1]] > 0L, edit_at[[1]] > diff_at[[1]])
stopifnot(grepl('id="two_set_study-title_one"', page, fixed = TRUE))
stopifnot(grepl('id="two_set_study-title_two"', page, fixed = TRUE))
stopifnot(grepl("dl_highlights", page, fixed = TRUE))
stopifnot(grepl("change_story", page, fixed = TRUE))
# Diff table: none, one change, and several changes. Titles are the column headers.
base_params <- e$default_params()
none <- e$comparison_diff(base_params, base_params, c("Alpha", "Beta"))
stopifnot(nrow(none) == 0L)
stopifnot(identical(names(none), c("Parameter", "Alpha", "Beta")))
none_html <- paste(as.character(e$comparison_diff_table(base_params, base_params, c("Alpha", "Beta"))), collapse = "")
stopifnot(grepl(e$comparison_identical_note, none_html, fixed = TRUE))
stopifnot(!grepl("<th>", none_html, fixed = TRUE))
one_params <- base_params
one_params$stations$capacity[one_params$stations$name == "paint"] <- 2L
one_diff <- e$comparison_diff(base_params, one_params, e$comparison_guided_labels)
stopifnot(nrow(one_diff) == 1L)
stopifnot(grepl("Paint", one_diff$Parameter), grepl("Parallel bays", one_diff$Parameter))
stopifnot(identical(names(one_diff), c("Parameter", e$comparison_guided_labels)))
stopifnot(one_diff[[2]] != one_diff[[3]])
one_html <- paste(as.character(e$comparison_diff_table(base_params, one_params, e$comparison_guided_labels)), collapse = "")
stopifnot(grepl("Baseline: 1 paint bay", one_html, fixed = TRUE))
stopifnot(grepl("Experiment: 2 paint bays", one_html, fixed = TRUE))
stopifnot(grepl("Parallel bays", one_html, fixed = TRUE))
stopifnot(!grepl(e$comparison_identical_note, one_html, fixed = TRUE))
multi_params <- one_params
multi_params$arrival_mean_min <- 5
multi_params$n_shifts <- 2L
multi_params$release_policy <- "conwip"
multi_params$order_spacing <- "exponential"
multi_params$entry_buffer <- 5L
multi_params$stations$buffer_before[multi_params$stations$name == "paint"] <- 1
multi <- e$comparison_diff(base_params, multi_params)
stopifnot(nrow(multi) >= 5L)
stopifnot(any(grepl("Planned work rate (vehicles/hour)", multi$Parameter, fixed = TRUE)))
stopifnot(any(grepl("Number of measured shifts", multi$Parameter, fixed = TRUE)))
stopifnot(any(grepl("Admission policy", multi$Parameter, fixed = TRUE)))
stopifnot(any(grepl("Order spacing", multi$Parameter, fixed = TRUE)))
stopifnot(any(grepl("Entrance waiting positions", multi$Parameter, fixed = TRUE)))
stopifnot(any(grepl("Incoming waiting positions", multi$Parameter, fixed = TRUE)))
stopifnot(any(grepl("Parallel bays", multi$Parameter, fixed = TRUE)))
policy <- multi[multi$Parameter == "Admission policy", ]
stopifnot(nrow(policy) == 1L)
stopifnot(any(grepl("Entrance space", as.character(unlist(policy)))))
stopifnot(any(grepl("WIP-limited (CONWIP)", as.character(unlist(policy)), fixed = TRUE)))
spacing <- multi[multi$Parameter == "Order spacing", ]
stopifnot(any(grepl("Evenly scheduled", as.character(unlist(spacing)), fixed = TRUE)))
stopifnot(any(grepl("Random", as.character(unlist(spacing)), fixed = TRUE)))
stopifnot(!any(grepl("Body shop", multi$Parameter, fixed = TRUE)))
stopifnot(identical(e$sanitize_set_label("  hello \n world ", "Set 1"), "hello world"))
stopifnot(identical(e$sanitize_set_label("   ", "Set 1"), "Set 1"))
stopifnot(nchar(e$sanitize_set_label(paste(rep("a", 80), collapse = ""), "Set 1")) == 60L)
stopifnot(!identical(e$sanitize_set_labels(c("Same", "Same"))[[1]], e$sanitize_set_labels(c("Same", "Same"))[[2]]))
# Near-zero baseline with replication noise does not use the exploded percent.
# The score stays within the new level; standardized change is still recorded.
noisy <- e$comparison_shift(c(0, 0.02), c(1, 3))
stopifnot(is.finite(noisy$relative), noisy$relative > 10)
stopifnot(noisy$score > 0.9, noisy$score < 1.05)
stopifnot(noisy$standardized > 1, is.finite(noisy$standardized))
# A baseline larger than the noise is a relative change.
steady <- e$comparison_shift(c(10, 10.2), c(14, 14.2))
stopifnot(abs(steady$score - steady$relative) < 1e-6)
n <- 6L
mk_run <- function(throughput, lead, wip, util, queue, bottleneck) {
  stations <- c("body_shop", "paint", "final_assembly", "inspection")
  list(
    summaries = tibble::tibble(
      replication = seq_len(n),
      throughput_per_hr = throughput,
      cycle_time_mean = lead,
      avg_wip = wip
    ),
    utilization_by_rep = tibble::tibble(
      station = rep(stations, each = n),
      replication = rep(seq_len(n), length(stations)),
      avg_util = rep(util, each = n),
      avg_queue = rep(queue, each = n)
    ),
    bottleneck = bottleneck
  )
}
run1 <- mk_run(rep(10, n), rep(80, n), rep(20, n), c(0.4, 0.95, 0.5, 0.3), c(0, 4, 0, 0), "paint")
run2 <- mk_run(rep(14, n), rep(50, n), rep(8, n), c(0.4, 0.50, 0.7, 0.3), c(0, 0.5, 0, 0), "final_assembly")
ranked <- e$comparison_rank_changes(list(run1, run2), n = 4L)
stopifnot(identical(
  ranked$shown$metric_id,
  c("queue_paint", "avg_wip", "util_paint", "throughput_per_hr")
))
stopifnot(all(diff(ranked$shown$score) <= 1e-8))
stopifnot(nrow(ranked$frame %>% dplyr::filter(metric_id == "queue_paint")) == n * 2L)
cap <- e$comparison_change_caption(list(run1, run2), ranked$scores)
stopifnot(grepl("shifted from Paint to Final assembly", cap, fixed = TRUE))
stopifnot(grepl("Throughput rose", cap, fixed = TRUE))
stopifnot(grepl("mean lead time fell", cap, fixed = TRUE))
stopifnot(grepl("WIP fell", cap, fixed = TRUE))
stopifnot(grepl("Paint queue", cap, fixed = TRUE))
stopifnot(grepl("fell", cap, fixed = TRUE))
empty_scores <- ranked$scores[0, ]
cap_other <- e$comparison_change_caption(
  list(list(bottleneck = "body_shop"), list(bottleneck = "inspection")),
  empty_scores
)
stopifnot(grepl("shifted from Body shop to Inspection", cap_other, fixed = TRUE))
stopifnot(!grepl("Paint", cap_other, fixed = TRUE))
cap_same <- e$comparison_change_caption(
  list(list(bottleneck = "paint"), list(bottleneck = "paint")),
  empty_scores
)
stopifnot(grepl("stayed at Paint", cap_same, fixed = TRUE))
exported <- e$comparison_highlight_export(ranked, e$comparison_guided_labels)
stopifnot(nrow(exported) == 4L * n * 2L)
stopifnot(all(e$comparison_guided_labels %in% exported$set))
stopifnot(all(seq_len(n) %in% exported$replication[exported$rank == 1L]))
labels <- e$comparison_guided_labels
pal <- e$comparison_palette(labels)
plot_df <- tibble::tibble(
  set = rep(labels, each = 8),
  value = c(stats::rnorm(8, 4, 0.35), stats::rnorm(8, 0.5, 0.15))
)
built <- ggplot2::ggplot_build(e$comparison_highlight_plot(
  plot_df, "Mean queue (vehicles)",
  "One ridge per set. Rug ticks are every replication.",
  pal, percent = FALSE
))
geoms <- vapply(built$plot$layers, function(layer) class(layer$geom)[1], character(1))
stopifnot(any(grepl("DensityRidges", geoms)))
stopifnot(!any(grepl("Point", geoms)))
stopifnot(identical(built$plot$theme$text$size, 16))
seg <- dplyr::bind_rows(built$data[which(grepl("Segment", geoms))])
stopifnot(nrow(seg) >= 16L)
stopifnot(all(seg$yend > seg$y))
stopifnot(all(c("#2474A5", "#D46B25") %in% unique(seg$colour)))
sub <- e$comparison_highlight_subtitle(0.000338, 3.998, 3.998 - 0.000338)
sub_lines <- strsplit(sub, "\n", fixed = TRUE)[[1]]
# comparison_ridges wraps this at 36 characters for the half-width charts.
wrapped <- strwrap(sub, width = 36)
stopifnot(length(wrapped) >= 2L, all(nchar(wrapped) <= 36L))
stopifnot(grepl("0.00034", sub, fixed = TRUE))
stopifnot(grepl("4.0", sub, fixed = TRUE))
stopifnot(!grepl("Baseline", sub, fixed = TRUE))
stopifnot(length(sub_lines) == 1L)
near0 <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 12),
  value = c(c(0, 0.01, 0.02, 0.04, 0.08, 0.12, 0.05, 0.03, 0.07, 0.09, 0.01, 0.06),
            stats::rnorm(12, 3.2, 0.35))
)
near_built <- ggplot2::ggplot_build(e$comparison_highlight_plot(
  near0, "Mean queue (vehicles)", sub, e$comparison_colors, percent = FALSE
))
near_geoms <- vapply(near_built$plot$layers, function(layer) class(layer$geom)[1], character(1))
near_dens <- near_built$data[[which(grepl("DensityRidges", near_geoms))[1]]]
stopifnot(min(near_dens$x, na.rm = TRUE) >= -1e-8)
occ <- tibble::tibble(
  set = rep(c("Set 1", "Set 2"), each = 16),
  value = c(stats::rnorm(16, 0.25, 0.05), stats::rnorm(16, 0.72, 0.05))
)
occ$value <- pmin(1, pmax(0, occ$value))
occ_built <- ggplot2::ggplot_build(e$comparison_highlight_plot(
  occ, "Bay occupancy", "check", e$comparison_colors, percent = TRUE
))
occ_geoms <- vapply(occ_built$plot$layers, function(layer) class(layer$geom)[1], character(1))
occ_dens <- occ_built$data[[which(grepl("DensityRidges", occ_geoms))[1]]]
stopifnot(min(occ_dens$x, na.rm = TRUE) >= -1e-8, max(occ_dens$x, na.rm = TRUE) <= 1 + 1e-6)
flat0 <- tibble::tibble(set = rep(c("Set 1", "Set 2"), each = 6), value = 0)
flat_built <- ggplot2::ggplot_build(e$comparison_highlight_plot(
  flat0, "Mean queue (vehicles)", "check", e$comparison_colors, percent = FALSE
))
flat_geoms <- vapply(flat_built$plot$layers, function(layer) class(layer$geom)[1], character(1))
stopifnot(any(grepl("Segment", flat_geoms)))
stopifnot(!any(grepl("DensityRidges", flat_geoms)))
stopifnot(identical(flat_built$plot$theme$text$size, 16))
cat("Comparison ridgeline checks passed\n")
cat("Two-set comparison checks passed\n")
