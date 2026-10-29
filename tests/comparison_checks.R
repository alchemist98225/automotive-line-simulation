e<-new.env();source("app/app.R",local=e,chdir=TRUE)
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
  invisible(output$kpis);invisible(output$summary);invisible(output$stations)
  invisible(output$distribution);invisible(output$cycles);invisible(output$occupancy)
  invisible(output$labor);invisible(output$downtime);invisible(output$wip);invisible(output$queues)
  b<-two$params();b$stations$capacity[2]<-2L;two$set_params(b)
  stopifnot(dirty(),results()$params[[2]]$stations$capacity[2]==1,one$params()$stations$capacity[2]==1)
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
cat("Comparison ridgeline checks passed\n")
cat("Two-set comparison checks passed\n")
