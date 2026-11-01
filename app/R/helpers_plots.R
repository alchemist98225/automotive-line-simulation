# helpers_plots.R
# Shared plot plumbing used across Simulate, Results, and Compare.
# Station labels/colors, theme_sim, axis ticks, density zoom helpers,
# params fingerprints / run stamping used by the server when saving a run.
# Chart titles: (none — helpers only; search plot files for chart titles).

# Station display labels
station_labels <- c(
  body_shop      = "Body shop",
  paint          = "Paint",
  final_assembly = "Final assembly",
  inspection     = "Inspection"
)
station_colors <- c(
  "Body shop" = "#2c7fb8", "Paint" = "#d95f0e",
  "Final assembly" = "#31a354", "Inspection" = "#756bb1"
)
label_station <- function(resource) {
  lab <- station_labels[as.character(resource)]
  ifelse(is.na(lab), as.character(resource), unname(lab))
}
# Labor resource ids are "operator_<pool>" from the model. Display only —
# CSV/helpers keep calling this rather than rewriting model output.
label_resource <- function(resource) {
  raw <- as.character(resource)
  pool <- ifelse(
    startsWith(raw, "operator_"),
    sub("^operator_", "", raw),
    raw
  )
  known <- c(
    upstream = "Upstream operators",
    downstream = "Downstream operators",
    shared = "Shared operators"
  )
  out <- unname(known[pool])
  station_lab <- unname(station_labels[pool])
  ifelse(
    !is.na(out),
    out,
    ifelse(!is.na(station_lab), paste(station_lab, "operators"), raw)
  )
}

# Snapshot comparison. UI defaults that do not change the run (fixed-timing SD,
# exponential SD, and MTBF while MTTR is 0) match the first-load fallback.
canonical_timing <- function(spec) {
  if (is.null(spec) || !is.list(spec) || is.null(spec$method)) {
    return(list(method = "fixed", mean_min = 0, sd_min = 0, distribution = "normal", observations = numeric()))
  }
  if (identical(spec$method, "empirical")) {
    return(list(
      method = "empirical", mean_min = 0, sd_min = 0, distribution = "normal",
      observations = as.numeric(spec$observations)
    ))
  }
  if (identical(spec$method, "distribution")) {
    dist <- spec$distribution %||% "normal"
    sd <- spec$sd_min %||% 0
    if (!is.finite(sd) || identical(dist, "exponential")) sd <- 0
    return(list(
      method = "distribution", mean_min = as.numeric(spec$mean_min), sd_min = as.numeric(sd),
      distribution = dist, observations = numeric()
    ))
  }
  list(
    method = "fixed", mean_min = as.numeric(spec$mean_min %||% 0), sd_min = 0,
    distribution = "normal", observations = numeric()
  )
}
params_signature <- function(p) {
  st <- p$stations
  list(
    sim_length_min = as.numeric(p$sim_length_min),
    shift_length_min=as.numeric(p$shift_length_min),n_shifts=as.integer(p$n_shifts),
    time_mode=p$time_mode,release_policy=p$release_policy,entry_buffer=p$entry_buffer,wip_limit=p$wip_limit,order_spacing=p$order_spacing,
    n_replications = as.integer(p$n_replications),
    base_seed = as.integer(p$base_seed),
    warmup_min = as.numeric(p$warmup_min),
    arrival_mean_min = as.numeric(p$arrival_mean_min),
    grid_dt_min = as.numeric(p$grid_dt_min %||% 2),
    repair_technicians = as.integer(p$repair_technicians %||% 1L),
    operators = lapply(seq_len(nrow(p$operators)), function(i) {
      list(pool = as.character(p$operators$pool[[i]]), count = as.integer(p$operators$count[[i]]))
    }),
    stations = lapply(seq_len(nrow(st)), function(i) {
      mttr <- st$mttr_min[[i]]
      mtbf <- st$mtbf_min[[i]]
      if (!is.finite(mttr) || mttr <= 0) mtbf <- Inf
      list(
        name = as.character(st$name[[i]]),
        capacity = as.integer(st$capacity[[i]]),
        buffer_before=as.numeric(st$buffer_before[[i]]),
        dist = as.character(st$dist[[i]]),
        mean_min = as.numeric(st$mean_min[[i]]),
        sd_min = if (is.finite(st$sd_min[[i]])) as.numeric(st$sd_min[[i]]) else NA_real_,
        operator_pool = as.character(st$operator_pool[[i]]),
        load_min = as.numeric(st$load_min[[i]]),
        unload_min = as.numeric(st$unload_min[[i]]),
        mtbf_min = as.numeric(mtbf),
        mttr_min = as.numeric(mttr),
        task_interval_min = as.numeric(st$task_interval_min[[i]]),
        task_duration_min = as.numeric(st$task_duration_min[[i]]),
        task_every_n = as.integer(st$task_every_n[[i]]),
        load_timing = canonical_timing(st$load_timing[[i]]),
        auto_timing = canonical_timing(st$auto_timing[[i]]),
        unload_timing = canonical_timing(st$unload_timing[[i]]),
        task_timing = canonical_timing(st$task_timing[[i]])
      )
    })
  )
}
params_fingerprint <- function(p) rlang::hash(params_signature(p))
cycle_time_mode <- function(spec, fallback_dist) {
  if (is.null(spec) || !is.list(spec) || is.null(spec$method)) return(as.character(fallback_dist %||% "unknown"))
  if (identical(spec$method, "distribution")) return(as.character(spec$distribution %||% "distribution"))
  as.character(spec$method)
}
operator_mode_label <- function(operators) {
  pools <- as.character(operators$pool)
  if (length(pools) == 1L && identical(pools[[1]], "shared")) return("shared")
  if (setequal(pools, c("upstream", "downstream"))) return("zones")
  "dedicated"
}
format_run_settings <- function(p) {
  st <- p$stations
  bay_txt <- paste(sprintf("%s %d", unname(label_station(st$name)), as.integer(st$capacity)), collapse = ", ")
  cycle_txt <- paste(vapply(seq_len(nrow(st)), function(i) {
    sprintf("%s %s", unname(label_station(st$name[[i]])), cycle_time_mode(st$auto_timing[[i]], st$dist[[i]]))
  }, character(1)), collapse = ", ")
  op_txt <- if (nrow(p$operators)) {
    paste(sprintf("%s %d", p$operators$pool, as.integer(p$operators$count)), collapse = ", ")
  } else "none"
  fail_on <- is.finite(st$mtbf_min) & st$mttr_min > 0
  if (any(fail_on)) {
    off <- unname(label_station(st$name[!fail_on]))
    mtbf_txt <- if (length(off)) paste0("on (", paste(off, collapse = ", "), " off)") else "on"
  } else {
    mtbf_txt <- "off"
  }
  sprintf(
    "Runs %d · seed %d · %g min total / %g min settling · planned order interval %g min · bays: %s · cycle-time: %s · operators: %s (%s) · MTBF/MTTR %s · repair techs %d",
    as.integer(p$n_replications), as.integer(p$base_seed),
    p$sim_length_min, p$warmup_min, p$arrival_mean_min,
    bay_txt, cycle_txt, operator_mode_label(p$operators), op_txt, mtbf_txt,
    as.integer(p$repair_technicians %||% 1L)
  )
}
stamp_run <- function(res, p, run_number, run_at = Sys.time(), fingerprint = NULL, summary = NULL) {
  fp <- fingerprint %||% params_fingerprint(p)
  summary <- summary %||% format_run_settings(p)
  res$provenance <- list(
    run_number = as.integer(run_number),
    run_at = run_at,
    fingerprint = fp,
    summary = summary,
    params = p
  )
  res$run_parameters <- res$run_parameters %>%
    dplyr::mutate(
      run_number = as.integer(run_number),
      run_at = format(run_at, "%Y-%m-%d %H:%M:%S"),
      settings_summary = summary,
      params_fingerprint = fp
    )
  res
}
tag_comparison <- function(out, p, run_number, run_at = Sys.time(), fingerprint = NULL, summary = NULL) {
  attr(out, "run_number") <- as.integer(run_number)
  attr(out, "run_at") <- run_at
  attr(out, "fingerprint") <- fingerprint %||% params_fingerprint(p)
  attr(out, "summary") <- summary %||% format_run_settings(p)
  attr(out, "params") <- p
  out
}
comparison_caption <- function(comp) {
  sprintf(
    "Comparison run %d · %s · %s",
    attr(comp, "run_number"),
    format(attr(comp, "run_at"), "%Y-%m-%d %H:%M:%S"),
    attr(comp, "summary")
  )
}
# Colors for the categories on one ridgeline. Names are the levels in the
# data, so a manual scale is never keyed only to station labels when the
# ridges are operator pools. Known stations and pools keep their colors;
# anything else gets its own distinct fallback.
ridge_fill_values <- function(levels, fill_values = NULL) {
  levels <- as.character(levels)
  known <- c(upstream = "#2c7fb8", downstream = "#d95f0e", shared = "#31a354")
  if (exists("station_colors", inherits = TRUE)) {
    sc <- get("station_colors", inherits = TRUE)
    known <- c(known, sc)
    if (exists("station_labels", inherits = TRUE)) {
      labs <- get("station_labels", inherits = TRUE)
      for (id in names(labs)) {
        label <- unname(labs[[id]])
        if (label %in% names(sc)) known[[id]] <- unname(sc[[label]])
      }
    }
  }
  if (!is.null(fill_values)) {
    fv <- fill_values
    if (is.null(names(fv))) names(fv) <- levels[seq_along(fv)]
    known[names(fv)] <- unname(fv)
  }
  known <- known[!duplicated(names(known), fromLast = TRUE)]
  fallback <- c("#2c7fb8", "#d95f0e", "#31a354", "#756bb1", "#e7298a",
                "#66a61e", "#e6ab02", "#a6761d", "#1f78b4", "#6a3d9a")
  out <- stats::setNames(character(length(levels)), levels)
  used <- character()
  fb <- 1L
  for (lv in levels) {
    col <- if (lv %in% names(known)) unname(known[[lv]]) else NA_character_
    if (!length(col) || is.na(col) || !nzchar(col) || col %in% used) {
      repeat {
        col <- if (fb <= length(fallback)) fallback[[fb]] else grDevices::hcl((fb * 37) %% 360, 75, 48)
        fb <- fb + 1L
        if (!col %in% used) break
      }
    }
    out[[lv]] <- col
    used <- c(used, col)
  }
  out
}
# Charts share one text size. 16 is about half again the ggplot2 default (11).
# theme_minimal() sets axis.text / strip.text to rel(0.8); pin every role to
# size 16 so axes and facets match titles rather than rendering ~13pt.
# Titles, subtitles, axes, legends, strips, and captions stay near-black.
theme_sim <- function(...) {
  theme_minimal(base_size = 16) +
    theme(
      text = element_text(size = 16, colour = "#1a1a1a"),
      plot.title = element_text(size = 16, colour = "#1a1a1a", margin = margin(0, 0, 6, 0)),
      plot.subtitle = element_text(size = 16, colour = "#1a1a1a", margin = margin(0, 0, 8, 0)),
      plot.caption = element_text(size = 16, colour = "#1a1a1a", hjust = 0, margin = margin(10, 0, 0, 0)),
      axis.title = element_text(size = 16, colour = "#1a1a1a"),
      axis.text = element_text(size = 16, colour = "#1a1a1a"),
      legend.title = element_text(size = 16, colour = "#1a1a1a"),
      legend.text = element_text(size = 16, colour = "#1a1a1a"),
      strip.text = element_text(size = 16, colour = "#1a1a1a"),
      ...
    )
}
# Default for any ggplot that forgets theme_sim() on Simulate / Parameters.
# Skip during Shiny's early auto-source of R/ (ggplot2 not loaded yet).
# app.R re-sources this file after library(tidyverse), which sets the theme.
if ("ggplot2" %in% loadedNamespaces()) {
  ggplot2::theme_set(theme_sim())
}
# 32 characters fits a 390px plot at base size 16, including a half-width card.
plot_subtitle <- function(text, width = 32L) {
  paste(strwrap(text, width = width), collapse = "\n")
}
# Subtitle/caption that reflow to the full plot width (not just the panel),
# so long notes stay readable on a 390px phone.
theme_plot_width_text <- function(...) {
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(
      size = 16,
      colour = "#1a1a1a",
      hjust = 0,
      halign = 0,
      lineheight = 1.15,
      padding = margin(0, 2, 0, 2),
      margin = margin(0, 0, 10, 0)
    ),
    plot.caption = ggtext::element_textbox_simple(
      size = 16,
      colour = "#1a1a1a",
      hjust = 0,
      halign = 0,
      lineheight = 1.15,
      padding = margin(0, 2, 0, 2),
      margin = margin(10, 0, 0, 0)
    ),
    strip.text = element_text(
      size = 16, colour = "#1a1a1a", hjust = 0, margin = margin(6, 4, 6, 4)
    ),
    axis.title.y = element_text(size = 16, colour = "#1a1a1a", margin = margin(r = 8)),
    axis.title.x = element_text(size = 16, colour = "#1a1a1a", margin = margin(t = 6)),
    axis.text = element_text(size = 16, colour = "#1a1a1a"),
    ...
  )
}
# Labeled major breaks plus visible minor ticks between them.
# At least `n` labeled majors (default 3). Minor ticks: about 3 between each pair.
labeled_continuous_scale <- function(aesthetic = "x", percent = FALSE, n = 3L,
                                      expand = expansion(mult = 0.08), limits = NULL, ...) {
  n <- max(3L, as.integer(n))
  major_fn <- function(lim) {
    br <- scales::breaks_pretty(n = n)(lim)
    if (length(br) < n) br <- scales::breaks_pretty(n = n + 1L)(lim)
    # Prefer at least 3 labeled majors; drop out-of-limit breaks when clamped.
    if (!is.null(limits)) {
      br <- br[is.finite(br) & br >= limits[[1]] - 1e-9 & br <= limits[[2]] + 1e-9]
    }
    br
  }
  # Exactly three minor ticks between each pair of labeled majors.
  minor_fn <- function(lim, breaks) {
    br <- breaks[is.finite(breaks)]
    if (length(br) < 2L) return(numeric())
    unlist(lapply(seq_len(length(br) - 1L), function(i) {
      seq(br[[i]], br[[i + 1L]], length.out = 5L)[2:4]
    }), use.names = FALSE)
  }
  labs <- if (isTRUE(percent)) scales::percent_format(accuracy = 1) else waiver()
  guide <- ggplot2::guide_axis(minor.ticks = TRUE, check.overlap = TRUE)
  if (identical(aesthetic, "y")) {
    scale_y_continuous(
      labels = labs, breaks = major_fn, minor_breaks = minor_fn,
      expand = expand, guide = guide, limits = limits, ...
    )
  } else {
    scale_x_continuous(
      labels = labs, breaks = major_fn, minor_breaks = minor_fn,
      expand = expand, guide = guide, limits = limits, ...
    )
  }
}
# Back-compat alias used by existing call sites.
# Major ticks for hours-since-settling axes. Prefer 1h or 2h steps so an
# 8-hour default shift stays readable; step grows with longer horizons.
observation_hour_breaks <- function(max_hours) {
  max_hours <- suppressWarnings(as.numeric(max_hours))
  if (!length(max_hours) || !is.finite(max_hours[[1]]) || max_hours[[1]] <= 0) {
    return(0)
  }
  max_hours <- max_hours[[1]]
  step <- if (max_hours <= 10) 1 else if (max_hours <= 24) 2 else if (max_hours <= 48) 4 else 8
  seq(0, ceiling(max_hours / step) * step, by = step)
}
sparse_x_continuous <- function(percent = FALSE, n = 3L, expand = expansion(mult = 0.08), limits = NULL) {
  labeled_continuous_scale("x", percent = percent, n = n, expand = expand, limits = limits)
}
# Theme rules that make minor ticks visible (ggplot2 3.5+ / 4.x).
theme_minor_ticks <- function() {
  theme(
    axis.ticks = element_line(colour = "#1a1a1a", linewidth = 0.55),
    axis.minor.ticks.x.bottom = element_line(colour = "#1a1a1a", linewidth = 0.45),
    axis.minor.ticks.y.left = element_line(colour = "#1a1a1a", linewidth = 0.45),
    axis.ticks.length = grid::unit(6, "pt"),
    axis.minor.ticks.length = grid::unit(3.5, "pt")
  )
}
# Central fraction of the data for coord_cartesian xlim. Densities stay
# computed on the full sample; values outside the window may clip. Bounded
# metrics (utilization, occupancy) stay inside the supplied bounds.
density_xlim <- function(x, level = 0.95, bounds = NULL, pad = 0.05) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    lo <- if (!is.null(bounds) && is.finite(bounds[[1]])) bounds[[1]] else 0
    hi <- if (!is.null(bounds) && is.finite(bounds[[2]])) bounds[[2]] else 1
    return(c(lo, hi))
  }
  alpha <- (1 - level) / 2
  q <- stats::quantile(x, probs = c(alpha, 1 - alpha), names = FALSE, type = 7)
  lo <- q[[1]]
  hi <- q[[2]]
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    v <- stats::median(x, na.rm = TRUE)
    if (!is.finite(v)) v <- 0
    pad_abs <- max(abs(v) * pad, 0.05)
    lo <- v - pad_abs
    hi <- v + pad_abs
  } else {
    span <- hi - lo
    lo <- lo - pad * span
    hi <- hi + pad * span
  }
  if (!is.null(bounds)) {
    if (is.finite(bounds[[1]])) lo <- max(lo, bounds[[1]])
    if (is.finite(bounds[[2]])) hi <- min(hi, bounds[[2]])
  }
  if (!is.finite(hi) || hi <= lo) hi <- lo + max(abs(lo) * pad, 0.05)
  c(lo, hi)
}
# Finite kernel window. An infinite upper bound becomes a little past the data
# so stats::density never receives Inf. Occupancy passes both ends already finite.
density_bounds <- function(x, bounds) {
  x <- x[is.finite(x)]
  lo <- if (is.null(bounds)) -Inf else bounds[[1]]
  hi <- if (is.null(bounds)) Inf else bounds[[2]]
  bw <- if (length(x) >= 2L) stats::bw.nrd0(x) else NA_real_
  if (!is.finite(bw) || bw <= 0) bw <- if (length(x) >= 2L) stats::mad(x, constant = 1) else NA_real_
  if (!is.finite(bw) || bw <= 0) bw <- if (length(x) >= 2L) diff(range(x)) / 10 else NA_real_
  if (!is.finite(bw) || bw <= 0) bw <- 1e-3
  if (!is.finite(lo)) lo <- if (length(x)) min(x) - 3 * bw else 0
  if (!is.finite(hi)) hi <- if (length(x)) max(x) + 3 * bw else lo + bw
  if (!is.finite(lo)) lo <- 0
  if (!is.finite(hi) || hi <= lo) hi <- lo + bw
  c(lo, hi)
}
