# =============================================================================
# Automotive Manufacturing Line DES — Shiny portfolio demo
# Discrete-event simulation with simmer. Defaults are PLACEHOLDER values.
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(ggtext) # declared so shinyapps.io installs it; the shift caption wraps to the plot width
library(ggiraph) # interactive hover tooltips on bottleneck wait bars
library(DT)
library(zip)   # declared so shinyapps.io installs it for Download-all (zip)

source("R/model.R", local = TRUE)
source("R/comparison.R", local = TRUE)
source("R/production.R", local = TRUE)

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
if (requireNamespace("ggplot2", quietly = TRUE)) {
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
                                      expand = expansion(mult = 0.08), ...) {
  n <- max(3L, as.integer(n))
  major_fn <- function(lim) {
    br <- scales::breaks_pretty(n = n)(lim)
    if (length(br) < n) br <- scales::breaks_pretty(n = n + 1L)(lim)
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
  guide <- ggplot2::guide_axis(minor.ticks = TRUE)
  if (identical(aesthetic, "y")) {
    scale_y_continuous(
      labels = labs, breaks = major_fn, minor_breaks = minor_fn,
      expand = expand, guide = guide, ...
    )
  } else {
    scale_x_continuous(
      labels = labs, breaks = major_fn, minor_breaks = minor_fn,
      expand = expand, guide = guide, ...
    )
  }
}
# Back-compat alias used by existing call sites.
sparse_x_continuous <- function(percent = FALSE, n = 3L, expand = expansion(mult = 0.08)) {
  labeled_continuous_scale("x", percent = percent, n = n, expand = expand)
}
# Theme rules that make minor ticks visible (ggplot2 3.5+ / 4.x).
theme_minor_ticks <- function() {
  theme(
    axis.ticks = element_line(colour = "#1a1a1a", linewidth = 0.4),
    axis.minor.ticks.x.bottom = element_line(colour = "#1a1a1a", linewidth = 0.3),
    axis.minor.ticks.y.left = element_line(colour = "#1a1a1a", linewidth = 0.3),
    axis.ticks.length = grid::unit(5, "pt"),
    axis.minor.ticks.length = grid::unit(2.5, "pt")
  )
}
# Central fraction of the data for coord_cartesian xlim. Densities stay
# computed on the full sample; rugs outside the window may clip. Bounded
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
# Overlaid densities, one curve per series, plus one rug tick per row.
# Each curve is scaled to its own peak so a tight set cannot flatten the other.
# A series with no spread is a labeled vertical line: the kernel has no bandwidth.
# The line is opaque; the fill uses the same color at alpha 0.30.
overlay_densities <- function(df, value, series, colors, x_label, subtitle,
                              percent = FALSE, bounds = NULL, facet = NULL,
                              subtitle_width = 32L) {
  empty <- ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot"))
  if (!nrow(df) || !value %in% names(df) || !series %in% names(df)) return(empty)
  df <- df %>% dplyr::filter(is.finite(.data[[value]]))
  if (!nrow(df)) return(empty)
  if (is.factor(df[[series]])) {
    df$series <- factor(df[[series]], levels = intersect(levels(df[[series]]), unique(df[[series]])))
  } else {
    df$series <- factor(df[[series]], levels = unique(df[[series]]))
  }
  df$.value <- df[[value]]
  group_cols <- "series"
  if (!is.null(facet)) group_cols <- c(facet, "series")
  spread <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      marker = stats::median(.value, na.rm = TRUE),
      n = dplyr::n(),
      s = stats::sd(.value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(constant = n < 2L | !is.finite(s) | s <= sqrt(.Machine$double.eps))
  varying <- df %>% dplyr::semi_join(dplyr::filter(spread, !constant), by = group_cols)
  constant <- spread %>% dplyr::filter(constant)
  if (is.null(bounds)) bounds <- if (isTRUE(percent)) c(0, 1) else c(0, Inf)
  bnds <- density_bounds(if (nrow(varying)) varying$.value else df$.value, bounds)
  p <- ggplot(df, aes(x = .value, colour = series, fill = series, group = series))
  if (nrow(varying)) {
    p <- p +
      geom_density(
        data = varying,
        mapping = aes(x = .value, y = after_stat(scaled), fill = series, group = series),
        inherit.aes = FALSE,
        colour = NA,
        alpha = 0.30,
        linewidth = 0,
        bounds = bnds,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      geom_density(
        data = varying,
        mapping = aes(x = .value, y = after_stat(scaled), colour = series, group = series),
        inherit.aes = FALSE,
        fill = NA,
        alpha = 1,
        linewidth = 1,
        bounds = bnds,
        na.rm = TRUE
      )
  }
  if (nrow(constant)) {
    x_mid <- mean(range(df$.value))
    if (is.null(facet)) {
      constant <- constant %>%
        dplyr::arrange(marker, series) %>%
        dplyr::mutate(label_i = dplyr::row_number())
    } else {
      constant <- constant %>%
        dplyr::group_by(.data[[facet]]) %>%
        dplyr::arrange(marker, series, .by_group = TRUE) %>%
        dplyr::mutate(label_i = dplyr::row_number()) %>%
        dplyr::ungroup()
    }
    constant <- constant %>%
      dplyr::mutate(
        label_x = dplyr::if_else(marker <= x_mid, marker, marker),
        label_y = 1.12 - (label_i - 1L) * 0.16,
        label_hjust = dplyr::if_else(marker <= x_mid, -0.08, 1.08)
      )
    p <- p +
      geom_vline(
        data = constant,
        aes(xintercept = marker, colour = series),
        inherit.aes = FALSE,
        linewidth = 1,
        show.legend = !nrow(varying)
      ) +
      geom_text(
        data = constant,
        aes(
          x = label_x, y = label_y, label = "No spread",
          colour = series, hjust = label_hjust
        ),
        inherit.aes = FALSE,
        vjust = 0,
        size = 4.2,
        show.legend = FALSE
      )
  }
  p <- p +
    geom_rug(
      mapping = aes(x = .value, colour = series),
      inherit.aes = FALSE,
      alpha = 0.85,
      linewidth = 0.45,
      sides = "b",
      length = grid::unit(0.035, "npc"),
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    scale_colour_manual(
      values = colors, aesthetics = c("colour", "fill"),
      limits = names(colors), drop = FALSE, name = NULL
    ) +
    sparse_x_continuous(percent = percent, n = 3L, expand = expansion(mult = c(0.04, 0.06))) +
    labs(
      x = x_label,
      y = "Scaled density",
      colour = NULL,
      fill = NULL,
      subtitle = subtitle
    ) +
    theme_sim(legend.position = "bottom", plot.margin = margin(12, 12, 14, 14)) +
    theme_plot_width_text() +
    theme_minor_ticks() +
    guides(
      # One column so long station names are not clipped on a 390px phone.
      colour = guide_legend(ncol = 1, byrow = TRUE),
      fill = guide_legend(ncol = 1, byrow = TRUE)
    )
  xlim <- density_xlim(df$.value, level = 0.95, bounds = bounds)
  faceted <- !is.null(facet) && dplyr::n_distinct(df[[facet]]) > 1L
  if (faceted) {
    # Free x per panel so a high-occupancy station does not squash the rest.
    # Shared 95% zoom would reintroduce that squash across panels.
    p <- p + facet_wrap(
      vars(.data[[facet]]), ncol = 2, scales = "free_x",
      labeller = label_wrap_gen(width = 18)
    )
    p + coord_cartesian(ylim = c(0, 1.32), expand = FALSE, clip = "off")
  } else {
    p <- p + labs(caption = "x-axis shows the central 95% of values")
    p + coord_cartesian(xlim = xlim, ylim = c(0, 1.32), expand = FALSE, clip = "off")
  }
}
# One overlaid density per category. A category with no spread is a vertical line.
replication_densities <- function(df, y, x, x_label, subtitle, percent = TRUE, fill_values = NULL) {
  y_sym <- rlang::sym(y)
  x_sym <- rlang::sym(x)
  df <- df %>%
    dplyr::filter(is.finite(!!x_sym)) %>%
    dplyr::mutate(series_label = !!y_sym)
  if (is.factor(df$series_label)) {
    df$series <- factor(df$series_label, levels = levels(df$series_label))
  } else {
    df$series <- factor(df$series_label, levels = unique(df$series_label))
  }
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot")))
  }
  colors <- ridge_fill_values(levels(df$series), fill_values)
  bounds <- if (isTRUE(percent)) c(0, 1) else c(0, Inf)
  overlay_densities(
    df, x, "series", colors, x_label, subtitle,
    percent = percent, bounds = bounds
  )
}
# Group stations whose median bay occupancy is similar onto one panel so they
# can be compared directly; put far-apart stations (e.g. a bottleneck) on their
# own panel with a free occupancy axis. At most three bands. A single panel when
# there is no clear separation.
occupancy_band_labels <- c(
  "1" = "Lower occupancy",
  "2" = "Mid occupancy",
  "3" = "Higher occupancy"
)
occupancy_groups <- function(per_rep, gap = 0.15, max_groups = 3L) {
  med <- per_rep %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(median_util = stats::median(avg_util, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(median_util)
  n <- nrow(med)
  if (n <= 1L || !any(is.finite(med$median_util))) {
    med$band <- factor("All stations", levels = "All stations")
    return(med)
  }
  vals <- med$median_util
  gaps <- diff(vals)
  # Candidate splits: largest gaps that clear the absolute threshold.
  # Cap at max_groups - 1 splits. When no gap clears the threshold, stay as one.
  order_gaps <- order(gaps, decreasing = TRUE)
  keep <- order_gaps[gaps[order_gaps] >= gap]
  n_split <- min(length(keep), as.integer(max_groups) - 1L)
  if (n_split < 1L) {
    med$band <- factor("All stations", levels = "All stations")
    return(med)
  }
  split_after <- sort(keep[seq_len(n_split)])
  band_id <- integer(n)
  band_id[[1]] <- 1L
  next_id <- 1L
  for (i in seq_len(n - 1L)) {
    if (i %in% split_after) next_id <- next_id + 1L
    band_id[[i + 1L]] <- next_id
  }
  n_bands <- max(band_id)
  if (n_bands == 2L) {
    lab <- c("1" = "Lower occupancy", "2" = "Higher occupancy")
  } else if (n_bands == 3L) {
    lab <- occupancy_band_labels
  } else {
    lab <- setNames(paste("Occupancy group", seq_len(n_bands)), as.character(seq_len(n_bands)))
  }
  med$band <- factor(
    unname(lab[as.character(band_id)]),
    levels = unname(lab[as.character(seq_len(n_bands))])
  )
  med
}
# Mean ± 95% CI across replications for a numeric column, one row per category.
rep_mean_ci <- function(df, category, value, conf = 0.95) {
  cat_sym <- rlang::sym(category)
  val_sym <- rlang::sym(value)
  df %>%
    dplyr::filter(is.finite(!!val_sym)) %>%
    dplyr::group_by(!!cat_sym) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!val_sym, na.rm = TRUE),
      sd = stats::sd(!!val_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = dplyr::if_else(n > 1L, sd / sqrt(n), NA_real_),
      tcrit = dplyr::if_else(n > 1L, stats::qt(1 - (1 - conf) / 2, n - 1L), NA_real_),
      half = se * tcrit,
      lower = mean - half,
      upper = mean + half,
      lower = dplyr::if_else(is.finite(lower), lower, mean),
      upper = dplyr::if_else(is.finite(upper), upper, mean)
    )
}

# Vertical bars of bay occupancy with 95% CI error bars. Stations with similar
# medians still share a free-y panel so a bottleneck does not squash the rest.
# Faint points keep every replication visible without cluttering the mean bars.
occupancy_boxes <- function(per_rep) {
  groups <- occupancy_groups(per_rep)
  plot_df <- per_rep %>%
    dplyr::left_join(groups %>% dplyr::select(label, band), by = "label")
  station_order <- groups %>% dplyr::arrange(median_util) %>% dplyr::pull(label)
  wrap_lab <- function(x) stringr::str_wrap(as.character(x), width = 10)
  plot_df <- plot_df %>%
    dplyr::mutate(
      label = factor(label, levels = station_order),
      label_wrapped = factor(wrap_lab(label), levels = wrap_lab(station_order))
    )
  summary_df <- rep_mean_ci(plot_df, "label_wrapped", "avg_util") %>%
    dplyr::left_join(
      plot_df %>% dplyr::distinct(label_wrapped, label, band),
      by = "label_wrapped"
    )
  n_bands <- dplyr::n_distinct(plot_df$band)
  p <- ggplot() +
    geom_col(
      data = summary_df,
      aes(x = label_wrapped, y = mean, fill = label),
      width = 0.72, colour = "#1a1a1a", linewidth = 0.25
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = label_wrapped, ymin = lower, ymax = upper),
      width = 0.22, linewidth = 0.7, colour = "#1a1a1a"
    ) +
    geom_point(
      data = plot_df,
      aes(x = label_wrapped, y = avg_util, colour = label),
      size = 2.2, alpha = 0.45,
      position = position_jitter(width = 0.08, height = 0, seed = 42)
    ) +
    scale_fill_manual(values = station_colors, drop = FALSE, guide = "none") +
    scale_colour_manual(values = station_colors, drop = FALSE, guide = "none") +
    labeled_continuous_scale("y", percent = TRUE, n = 3L, expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = NULL, y = "Bay occupancy",
      caption = if (n_bands > 1L) {
        "Bars = mean across replications; error bars = 95% CI. Faint points = each replication. Similar-occupancy stations share a panel with its own axis."
      } else {
        "Bars = mean across replications; error bars = 95% CI. Faint points = each replication."
      }
    ) +
    theme_sim(legend.position = "none", plot.margin = margin(8, 12, 14, 10)) +
    theme_plot_width_text(panel.spacing.y = grid::unit(18, "pt")) +
    theme_minor_ticks() +
    theme(
      strip.text = element_text(
        size = 16, colour = "#1a1a1a", hjust = 0, margin = margin(8, 4, 8, 4)
      ),
      # Angle keeps four station names readable at ~390px phone width.
      axis.text.x = element_text(size = 13, colour = "#1a1a1a", angle = 28, hjust = 1, vjust = 1)
    )
  if (n_bands > 1L) {
    p <- p + facet_wrap(
      ~band, scales = "free_y", ncol = 1,
      labeller = label_wrap_gen(width = 18)
    )
  }
  p + coord_cartesian(clip = "on")
}

# Category bars with 95% CI for operator / downtime style metrics.
replication_bars <- function(df, category, value, x_label, percent = TRUE, fill_values = NULL) {
  cat_sym <- rlang::sym(category)
  val_sym <- rlang::sym(value)
  df <- df %>% dplyr::filter(is.finite(!!val_sym))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot")))
  }
  if (is.factor(df[[category]])) {
    df[[category]] <- factor(df[[category]], levels = levels(df[[category]]))
  } else {
    df[[category]] <- factor(df[[category]], levels = unique(df[[category]]))
  }
  summary_df <- rep_mean_ci(df, category, value)
  colors <- ridge_fill_values(levels(df[[category]]), fill_values)
  ggplot() +
    geom_col(
      data = summary_df,
      aes(x = !!cat_sym, y = mean, fill = !!cat_sym),
      width = 0.72, colour = "#1a1a1a", linewidth = 0.25
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = !!cat_sym, ymin = lower, ymax = upper),
      width = 0.22, linewidth = 0.7, colour = "#1a1a1a"
    ) +
    geom_point(
      data = df,
      aes(x = !!cat_sym, y = !!val_sym, colour = !!cat_sym),
      size = 2.2, alpha = 0.45,
      position = position_jitter(width = 0.08, height = 0, seed = 42)
    ) +
    scale_fill_manual(values = colors, drop = FALSE, guide = "none") +
    scale_colour_manual(values = colors, drop = FALSE, guide = "none") +
    labeled_continuous_scale("y", percent = percent, n = 3L, expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = NULL, y = x_label,
      caption = "Bars = mean across replications; error bars = 95% CI. Faint points = each replication."
    ) +
    theme_sim(legend.position = "none", plot.margin = margin(8, 12, 14, 10)) +
    theme_plot_width_text() +
    theme_minor_ticks() +
    theme(axis.text.x = element_text(size = 13, colour = "#1a1a1a", angle = 28, hjust = 1, vjust = 1)) +
    coord_cartesian(clip = "on")
}

# Dodged wait-reason bars with ggiraph hover tooltips (mean and 95% CI).
wait_reason_bars_interactive <- function(waits) {
  type_lab <- c(
    blocked = "Blocked after processing",
    waiting_for_operator = "Waiting for operator",
    waiting_for_automatic_capacity = "Awaiting automatic capacity"
  )
  df <- waits %>%
    dplyr::mutate(
      station = factor(label_station(station), levels = unname(station_labels)),
      type = factor(
        unname(type_lab[as.character(type)]),
        levels = unname(type_lab)
      )
    )
  summary_df <- df %>%
    dplyr::group_by(station, type) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(minutes, na.rm = TRUE),
      sd = stats::sd(minutes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = dplyr::if_else(n > 1L, sd / sqrt(n), NA_real_),
      tcrit = dplyr::if_else(n > 1L, stats::qt(0.975, n - 1L), NA_real_),
      half = se * tcrit,
      lower = dplyr::if_else(is.finite(half), mean - half, mean),
      upper = dplyr::if_else(is.finite(half), mean + half, mean),
      tooltip = sprintf(
        "%s · %s<br/>Mean: %.1f bay-min<br/>95%% CI: %.1f – %.1f",
        as.character(station), as.character(type), mean, lower, upper
      ),
      data_id = paste(station, type, sep = "::")
    )
  fill_cols <- c(
    "Blocked after processing" = "#d95f0e",
    "Waiting for operator" = "#2c7fb8",
    "Awaiting automatic capacity" = "#31a354"
  )
  p <- ggplot(summary_df, aes(x = station, y = mean, fill = type)) +
    ggiraph::geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      position = position_dodge(width = 0.8), width = 0.72,
      colour = "#1a1a1a", linewidth = 0.25
    ) +
    ggiraph::geom_errorbar_interactive(
      aes(ymin = lower, ymax = upper, tooltip = tooltip, data_id = data_id),
      position = position_dodge(width = 0.8), width = 0.2,
      linewidth = 0.65, colour = "#1a1a1a"
    ) +
    scale_fill_manual(values = fill_cols, drop = FALSE) +
    labeled_continuous_scale("y", percent = FALSE, n = 3L, expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = NULL, y = "Mean bay-minutes per measured run", fill = NULL,
      caption = "Bars = mean across replications; error bars = 95% CI. Hover or tap a bar for its value."
    ) +
    theme_sim(legend.position = "bottom", plot.margin = margin(8, 12, 14, 10)) +
    theme_plot_width_text() +
    theme_minor_ticks() +
    theme(axis.text.x = element_text(size = 13, colour = "#1a1a1a", angle = 28, hjust = 1, vjust = 1)) +
    coord_cartesian(clip = "on")
  ggiraph::girafe(
    ggobj = p,
    width_svg = 9, height_svg = 5.2,
    options = list(
      ggiraph::opts_tooltip(
        css = "background:#1a1a1a;color:#fff;padding:8px 10px;border-radius:6px;font-size:14px;line-height:1.35;",
        opacity = 0.95
      ),
      ggiraph::opts_hover(css = "filter:brightness(1.08);stroke-width:1.2px;"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_toolbar(saveaspng = FALSE)
    )
  )
}

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

# Starts collapsed with a highlight so it still stands out. The buttons drive one experiment.
# Sits below the KPI boxes on Simulate. Status lives in the slim top bar instead.
guided_example_ui <- function() {
  tagList(
    tags$style(HTML("
      .guided-example {
        border: 1px solid #9ecae1;
        border-left: 5px solid #2c7fb8;
        background: #f0f7fc;
        border-radius: 0.5rem;
      }
      .guided-example summary {
        cursor: pointer;
        font-weight: 600;
        color: #1b4f72;
        list-style: none;
        display: flex;
        align-items: center;
        gap: 0.45rem;
        padding: 0.15rem 0;
      }
      .guided-example summary::-webkit-details-marker { display: none; }
      .guided-example summary .guided-label { font-size: 1.02rem; }
      #guided_example.bslib-card { flex: 0 0 auto; height: auto; width: 100%; }
    ")),
    card(
      id = "guided_example",
      class = "guided-example",
      fill = FALSE,
      card_body(
        fill = FALSE,
        tags$details(
          open = NA,
          tags$summary(
            tagList(
              bs_icon("lightbulb"),
              span(class = "guided-label", "Start here: guided example")
            )
          ),
          p(class = "mt-2 mb-2",
            "This model asks where the line's capacity constraint is, and what changes if you add capacity there."),
          p(class = "mb-2", "Raise Paint from 1 bay to 2."),
          div(
            class = "d-flex flex-wrap gap-2 mb-2",
            actionButton("guide_try", "Try it", class = "btn-primary btn-sm", icon = icon("play")),
            actionButton("guide_reset", "Reset to defaults", class = "btn-outline-secondary btn-sm")
          ),
          p(class = "mb-2",
            "Under these example inputs, paint capacity increases output and reduces production lead time. More work may be admitted as space opens, so shop-floor WIP need not fall. Check unreleased work and the next bottleneck candidate on Production schedule & flow. Compare two sets keeps that run beside the 1-bay baseline."),
          p(class = "text-muted small mb-0",
            "All data is illustrative placeholder data, not a prediction for any real plant or company.")
        )
      )
    )
  )
}

# Slim full-width status bar at the top of Simulate (fixed height; no layout jump).
sim_status_bar_ui <- function() {
  tagList(
    tags$style(HTML("
      .sim-status-bar {
        width: 100%;
        margin-bottom: 0.85rem;
      }
      .sim-status-bar .sim-progress-card {
        min-height: 3.4rem;
        height: 3.4rem;
        box-sizing: border-box;
        display: flex;
        align-items: center;
        gap: 1rem;
        padding: 0.45rem 1rem;
        margin: 0;
      }
      .sim-progress-card {
        background: #fff;
        border: 2px solid #2c7fb8;
        border-radius: 0.5rem;
        box-shadow: 0 4px 12px rgba(0,0,0,.10);
      }
      .sim-progress-card.sim-progress-idle {
        border-color: #adb5bd;
        box-shadow: none;
        background: #f8f9fa;
      }
      .sim-progress-title {
        font-size: 1.05rem;
        font-weight: 700;
        color: #1b4f72;
        margin: 0;
        white-space: nowrap;
      }
      .sim-progress-idle .sim-progress-title { color: #495057; }
      .sim-progress-detail {
        flex: 1 1 auto;
        min-width: 0;
        margin: 0 !important;
        font-size: 0.95rem;
      }
      .sim-status-bar .progress {
        width: 11rem;
        max-width: 30%;
        height: 1.15rem;
        margin: 0;
        flex: 0 0 auto;
      }
      .sim-progress-card .progress-bar {
        font-size: 0.75rem;
        font-weight: 600;
      }
      @media (max-width: 576px) {
        .sim-status-bar .sim-progress-card {
          height: auto;
          min-height: 3.4rem;
          flex-wrap: wrap;
          gap: 0.35rem 0.75rem;
          padding: 0.55rem 0.85rem;
        }
        .sim-status-bar .progress { width: 100%; max-width: none; }
      }
      /* Clickable KPI value boxes → Results chart pages */
      .kpi-nav-card {
        cursor: pointer;
        position: relative;
        border-radius: 0.5rem;
        transition: transform 0.12s ease, box-shadow 0.12s ease;
      }
      .kpi-nav-card:hover, .kpi-nav-card:focus {
        transform: translateY(-2px);
        box-shadow: 0 8px 18px rgba(0,0,0,.14);
        outline: none;
      }
      .kpi-nav-card .kpi-view-hint {
        display: block;
        font-size: 0.78rem;
        font-weight: 600;
        color: #1a1a1a;
        opacity: 0.7;
        margin-top: 0.15rem;
      }
      .kpi-nav-card:hover .kpi-view-hint { opacity: 1; text-decoration: underline; }
      /* Run simulation accordion header stands out from jump links. */
      #line_controls .accordion-item:first-child > .accordion-header .accordion-button {
        background: #198754;
        color: #fff;
        font-weight: 700;
      }
      #line_controls .accordion-item:first-child > .accordion-header .accordion-button:not(.collapsed) {
        background: #157347;
        color: #fff;
        box-shadow: none;
      }
      #line_controls .accordion-item:first-child > .accordion-header .accordion-button::after {
        filter: brightness(0) invert(1);
      }
    ")),
    div(
      id = "sim_progress_slot",
      class = "sim-status-bar",
      div(
        id = "sim_progress_idle",
        class = "sim-progress-card sim-progress-idle",
        div(class = "sim-progress-title", "Ready"),
        div(class = "sim-progress-detail text-dark",
            "Status for the next run appears here.")
      ),
      div(
        id = "sim_progress_panel",
        class = "d-none",
        div(
          class = "sim-progress-card",
          div(class = "sim-progress-title", "Simulation running"),
          div(class = "sim-progress-detail text-dark", ""),
          div(
            class = "progress",
            div(
              class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar",
              style = "width: 100%;",
              "In progress"
            )
          )
        )
      )
    )
  )
}

# Value box that navigates to a Results chart page when clicked.
kpi_nav_box <- function(page_value, title, value, showcase, theme, ...) {
  box_id <- paste0(
    "kpi_nav_",
    switch(page_value,
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

ui <- page_navbar(
  id = "main_page",
  title = "Automotive Line Simulator",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c7fb8",
    "navbar-bg" = "#1b4f72"
  ),
  # plotOutput cannot take clamp() directly. This overrides the 640px fallback.
  header = tagList(
    tags$style(HTML("
    .shiny-plot-output {
      width: 100% !important;
      height: clamp(520px, 60vh, 720px) !important;
      min-height: 520px !important;
    }
    /* Page scrolls as a normal document. Fillable nesting was trapping overflow. */
    html, body {
      height: auto !important;
      max-height: none !important;
      overflow-y: auto !important;
      overflow-x: hidden !important;
    }
    .bslib-page-fill, .html-fill-container {
      height: auto !important;
      min-height: 0 !important;
      max-height: none !important;
      overflow: visible !important;
    }
    .bslib-sidebar-layout {
      height: auto !important;
      max-height: none !important;
      overflow: visible !important;
    }
    .bslib-sidebar-layout > .main,
    .bslib-sidebar-layout > .sidebar {
      overflow: visible !important;
      max-height: none !important;
    }
    /* Sub-tab pills wrap instead of collapsing into one run-on label. */
    #capacity_tabs.nav-pills,
    #flow_tabs.nav-pills,
    #lead_tabs.nav-pills {
      display: flex;
      flex-wrap: wrap;
      gap: 0.5rem;
    }
    #capacity_tabs.nav-pills > .nav-item,
    #flow_tabs.nav-pills > .nav-item,
    #lead_tabs.nav-pills > .nav-item {
      flex: 0 0 auto;
      width: auto;
    }
    .two-set-study .nav-tabs,
    .two-set-study .nav-pills {
      display: flex;
      flex-wrap: wrap;
      gap: 0.35rem;
    }
    .two-set-study .nav-item {
      flex: 0 0 auto;
      width: auto;
    }
    .provenance-warning {
      border-left: 6px solid #8a4b08;
      font-weight: 500;
    }
    .run-settings summary { cursor: pointer; font-weight: 600; }
    .text-muted,
    .text-body-secondary,
    .form-text,
    .help-block,
    caption,
    .caption {
      color: #1a1a1a !important;
    }
    #headline_kpis, #plot_util, #plot_labor, #plot_down, #plot_wip,
    #plot_queue, #plot_cycle, #labor_table, #reliability_table, #table_metrics,
    #scenario_table_wrap, #timing_table_wrap {
      transition: opacity 0.15s ease;
    }
    /* Sidebar section jumps look like collapsed accordion headers. */
    .param-jump {
      display: block;
      width: 100%;
      text-align: left;
      border: 1px solid rgba(0,0,0,.125);
      border-radius: 0.375rem;
      background: #f8f9fa;
      color: #1a1a1a;
      padding: 0.65rem 0.85rem;
      margin: 0.35rem 0;
      font-weight: 600;
      text-decoration: none !important;
    }
    .param-jump:hover, .param-jump:focus {
      background: #e9ecef;
      color: #1a1a1a;
    }
    .param-jump .jump-hint {
      display: block;
      font-weight: 400;
      font-size: 0.8rem;
      color: #333;
      margin-top: 0.15rem;
    }
    .params-section {
      scroll-margin-top: 5.5rem;
    }
    /* Compact plot-option menus on chart pages. */
    .plot-options-btn { white-space: nowrap; }
    .plot-options-popover { min-width: 16rem; max-width: 22rem; }

    /* Hide Shiny's default progress UI; the slim top status bar is the only
       progress surface. withProgress remains only as a websocket flush pump. */
    .shiny-progress-container {
      display: none !important;
    }
    .shiny-notification:has(.shiny-progress-notification),
    .shiny-notification .shiny-progress-notification {
      display: none !important;
    }
  ")),
    tags$script(HTML("
      function setSimProgressVisible(on) {
        var panel = document.getElementById('sim_progress_panel');
        var idle = document.getElementById('sim_progress_idle');
        if (panel) panel.classList.toggle('d-none', !on);
        if (idle) idle.classList.toggle('d-none', !!on);
        document.body.classList.toggle('sim-running', !!on);
      }
      Shiny.addCustomMessageHandler('simProgress', function(msg) {
        var el = document.getElementById('sim_progress_panel');
        if (!el) return;
        if (msg && msg.show) {
          setSimProgressVisible(true);
          var detail = el.querySelector('.sim-progress-detail');
          var bar = el.querySelector('.progress-bar');
          if (detail) detail.textContent = msg.detail || '';
          if (bar && msg.value != null) {
            var pct = Math.max(0, Math.min(1, msg.value)) * 100;
            bar.style.width = pct + '%';
            bar.textContent = Math.round(pct) + '%';
          } else if (bar) {
            bar.style.width = '100%';
            bar.textContent = 'In progress';
          }
        } else {
          setSimProgressVisible(false);
        }
      });
      function showSimProgressLocal(detail) {
        var el = document.getElementById('sim_progress_panel');
        if (!el) return;
        setSimProgressVisible(true);
        var d = el.querySelector('.sim-progress-detail');
        if (d) d.textContent = detail || '';
        var bar = el.querySelector('.progress-bar');
        if (bar) { bar.style.width = '100%'; bar.textContent = 'In progress'; }
      }
      function repsLabel(extra) {
        var reps = (document.getElementById('n_reps') || {}).value || '5';
        var base = reps + ' replications';
        return extra ? (base + ' · ' + extra) : base;
      }
      // Open the panel immediately on click so it is visible while Shiny blocks.
      $(document).on('click', '#run', function() {
        showSimProgressLocal(repsLabel());
      });
      $(document).on('click', '#guide_try', function() {
        showSimProgressLocal(repsLabel('guided example'));
      });
      $(document).on('click', '#compare', function() {
        showSimProgressLocal(repsLabel('staffing comparison'));
      });
      $(document).on('click', '#compare_timing', function() {
        showSimProgressLocal(repsLabel('timing comparison'));
      });
      Shiny.addCustomMessageHandler('scrollToSection', function(msg) {
        var id = (msg && msg.id) ? msg.id : msg;
        var tries = 0;
        function go() {
          var el = document.getElementById(id);
          if (el) {
            el.scrollIntoView({behavior: 'smooth', block: 'start'});
            return;
          }
          if (tries++ < 40) setTimeout(go, 50);
        }
        setTimeout(go, 50);
      });
      function closeResultsNavChrome() {
        document.querySelectorAll('.navbar .dropdown-menu.show').forEach(function(m) {
          m.classList.remove('show');
        });
        document.querySelectorAll('.navbar .nav-item.dropdown.show, .navbar .dropdown.show').forEach(function(d) {
          d.classList.remove('show');
          var toggle = d.querySelector('[data-bs-toggle=dropdown], .dropdown-toggle');
          if (toggle) {
            toggle.classList.remove('show');
            toggle.setAttribute('aria-expanded', 'false');
          }
        });
        var collapse = document.querySelector('.navbar-collapse.show');
        if (collapse) {
          if (window.bootstrap && bootstrap.Collapse) {
            try { bootstrap.Collapse.getOrCreateInstance(collapse, {toggle: false}).hide(); }
            catch (err) {
              collapse.classList.remove('show');
            }
          } else {
            collapse.classList.remove('show');
          }
        }
      }
      function kpiNavigate(el) {
        var page = el && el.getAttribute('data-page');
        if (!page) return;
        closeResultsNavChrome();
        Shiny.setInputValue('kpi_nav', page, {priority: 'event'});
        // Nav may re-open the menu while switching; close again after paint.
        setTimeout(closeResultsNavChrome, 0);
        setTimeout(closeResultsNavChrome, 120);
      }
      $(document).on('click', '.kpi-nav-card', function(e) {
        kpiNavigate(this);
      });
      $(document).on('keydown', '.kpi-nav-card', function(e) {
        if (e.key === 'Enter' || e.key === ' ') {
          e.preventDefault();
          kpiNavigate(this);
        }
      });
      Shiny.addCustomMessageHandler('closeResultsNav', function(msg) {
        closeResultsNavChrome();
        [0, 50, 150, 300, 500].forEach(function(ms) {
          setTimeout(closeResultsNavChrome, ms);
        });
      });
      // If a Results nav item becomes active, force the dropdown/toggler shut.
      $(document).on('shown.bs.tab shown.bs.dropdown', function() {
        var active = document.querySelector('.navbar .nav-link.active, .navbar .dropdown-item.active');
        if (!active) return;
        var val = active.getAttribute('data-value') || '';
        if (val.indexOf('results_') === 0 || val === 'production_schedule') {
          setTimeout(closeResultsNavChrome, 0);
          setTimeout(closeResultsNavChrome, 100);
        }
      });
    "))
  ),
  fillable = FALSE,

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
            actionButton("run", "Run simulation", class = "btn-primary w-100 mb-3", icon = icon("play")),
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
            "results_flow",
            title = "Avg WIP",
            value = textOutput("kpi_wip", inline = TRUE),
            showcase = bs_icon("boxes"),
            theme = "warning",
            p("vehicles in system, counted once")
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
  ),

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
  ),

  nav_menu(
    "Results",
    icon = bs_icon("bar-chart"),
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
        plot_card(
          "Bay occupancy (mean ± 95% CI)",
          roomy_plot("plot_util"),
          downloadButton("dl_util_summary", "Download mean ± 95% CI (.csv)", class = "btn-outline-secondary btn-sm mt-2")
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
,
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
,
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
  ),
    production_page_ui("production_results")
  ),

  comparison_page_ui("two_set_study"),

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

Every chart uses **all replications**. Bay occupancy on Simulate is a boxplot
with one point per replication. Operator utilization and downtime are overlaid
densities, with a rug tick for every replication and a vertical line when every
replication is identical. Queue and WIP series are faint lines plus a loess
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
)

server <- function(input, output, session) {
  show_sim_progress <- function(detail = "", value = NULL) {
    session$sendCustomMessage(
      "simProgress",
      list(show = TRUE, detail = detail, value = value)
    )
  }
  hide_sim_progress <- function() {
    session$sendCustomMessage("simProgress", list(show = FALSE))
  }
  jump_to_params <- function(section_id) {
    nav_select("main_page", "parameters", session = session)
    session$sendCustomMessage("scrollToSection", list(id = section_id))
  }
  observeEvent(input$jump_capacity, jump_to_params("params_capacity"), ignoreInit = TRUE)
  observeEvent(input$jump_cycle_times, jump_to_params("params_cycle_times"), ignoreInit = TRUE)
  observeEvent(input$jump_operators, jump_to_params("params_operators"), ignoreInit = TRUE)
  observeEvent(input$jump_reliability, jump_to_params("params_reliability"), ignoreInit = TRUE)

  # "try" forces Paint to 2 bays. "defaults" ignores the widgets and uses the
  # first-load placeholders. NULL follows the sidebar. The overlay stays until
  # the widgets catch up, so the run is not marked stale during that round trip.
  guide_mode <- reactiveVal(NULL)
  guide_run <- reactiveVal(0L)
  # Holds the 1-bay run until the Try it simulation finishes, then the
  # comparison page consumes the pair. NULL means this run is not that experiment.
  guide_pair <- reactiveVal(NULL)

  output$operator_controls <- renderUI({
    switch(input$staffing,
      zones=tagList(numericInput("op_a","Zone A operators",2,min=1,step=1),numericInput("op_b","Zone B operators",2,min=1,step=1)),
      shared=numericInput("op_all","Shared team operators",4,min=1,step=1),
      dedicated=tagList(lapply(names(station_labels),function(n) numericInput(paste0("op_",n),paste(station_labels[[n]],"operators"),1,min=1,step=1))))
  })
  # Collapsed panels still create operator counts, timing, and reliability inputs.
  outputOptions(output, "operator_controls", suspendWhenHidden = FALSE)
  register_station_panels(output)
  read_timing <- function(st, activity, ignore_inputs = FALSE) {
    legacy <- activity_timing(st, activity)
    if (ignore_inputs) return(legacy)
    id <- paste0(activity, "_", st$name)
    method <- input[[paste0(id, "_method")]]
    if (is.null(method)) return(legacy)
    spec <- tryCatch({
      if (method=="empirical") timing_spec("empirical",observations=parse_observations(input[[paste0(id,"_observations")]] %||% ""))
      else timing_spec(method,legacy$mean_min,
        sd_min=input[[paste0(id,"_sd")]] %||% legacy$sd_min,
        distribution=input[[paste0(id,"_distribution")]] %||% "normal")
    },error=function(e) e)
    validate(need(!inherits(spec,"error"),paste(station_labels[[st$name]],activity,if(inherits(spec,"error")) conditionMessage(spec) else "")))
    valid <- tryCatch({validate_timing(spec); TRUE},error=function(e) conditionMessage(e))
    validate(need(isTRUE(valid),paste(station_labels[[st$name]],activity,valid)))
    spec
  }
  # Build params from sidebar inputs. Dynamic renderUI inputs are still NULL
  # on the first bindEvent fire (ignoreNULL = FALSE runs before the client
  # registers op_*, load_*, mtbf_*, and the rest), including when those
  # outputs sit in a collapsed accordion panel. Fall back to the same
  # placeholder defaults those controls display so the first paint is filled.
  # suspendWhenHidden = FALSE lets a later run see values from panels that
  # are still collapsed.
  assemble_params <- function(mode = NULL) {
    ignore <- identical(mode, "defaults")
    defaults <- default_params()
    st0 <- defaults$stations
    val <- function(id, default) {
      if (ignore) return(default)
      x <- input[[id]]
      if (is.null(x)) default else x
    }
    flag <- function(id, default) {
      if (ignore) return(default)
      x <- input[[id]]
      if (is.null(x)) default else isTRUE(x)
    }
    station_field <- function(station_name, field) st0[[field]][[match(station_name, st0$name)]]
    staffing <- val("staffing", "zones")
    if (!staffing %in% c("zones", "shared", "dedicated")) staffing <- "zones"
    reliability <- flag("reliability", TRUE)
    noncyclic <- flag("noncyclic", TRUE)
    p <- list(
      time_mode="shifts",shift_length_min=val("sim_length",defaults$shift_length_min),
      n_shifts=val("n_shifts",defaults$n_shifts),
      sim_length_min=val("warmup",defaults$warmup_min)+val("sim_length",defaults$shift_length_min)*val("n_shifts",defaults$n_shifts),
      release_policy=val("release_policy",defaults$release_policy),entry_buffer=val("entry_buffer",defaults$entry_buffer),
      wip_limit=val("wip_limit",defaults$wip_limit),order_spacing=val("order_spacing",defaults$order_spacing),
      n_replications   = as.integer(val("n_reps", defaults$n_replications)),
      base_seed        = as.integer(val("seed", defaults$base_seed)),
      warmup_min       = val("warmup", defaults$warmup_min),
      arrival_mean_min = 60/val("planned_rate",60/defaults$arrival_mean_min),
      grid_dt_min      = defaults$grid_dt_min %||% 2,
      repair_technicians = as.integer(val("repair_staff", defaults$repair_technicians)),
      stations = tribble(
        ~name,              ~capacity,              ~dist,         ~mean_min,        ~sd_min,
        "body_shop",        as.integer(val("body_cap", 2)),  "exponential", val("body_mean", 6),  NA_real_,
        "paint",            as.integer(val("paint_cap", 1)), "normal",      val("paint_mean", 8), 1.5,
        "final_assembly",   as.integer(val("fa_cap", 2)),    "normal",      val("fa_mean", 7),    1.0,
        "inspection",       as.integer(val("insp_cap", 1)),  "exponential", val("insp_mean", 3),  NA_real_
      )
    )
    p$operators <- switch(staffing,
      zones=tibble(pool=c("upstream","downstream"),count=c(val("op_a", 2), val("op_b", 2))),
      shared=tibble(pool="shared",count=val("op_all", 4)),
      dedicated=tibble(pool=names(station_labels),count=map_dbl(names(station_labels),~val(paste0("op_",.x), 1))))
    p$stations <- p$stations %>% mutate(
      buffer_before=map_dbl(name,~if(.x=="body_shop") 0 else val(paste0("buffer_",.x),station_field(.x,"buffer_before"))),
      operator_pool=switch(staffing,zones=c("upstream","upstream","downstream","downstream"),shared=rep("shared",4),dedicated=name),
      load_min=map_dbl(name,~val(paste0("load_",.x), station_field(.x, "load_min"))),
      unload_min=map_dbl(name,~val(paste0("unload_",.x), station_field(.x, "unload_min"))),
      mtbf_min=if(reliability) map_dbl(name,~val(paste0("mtbf_",.x), station_field(.x, "mtbf_min"))) else Inf,
      mttr_min=map_dbl(name,~val(paste0("mttr_",.x), station_field(.x, "mttr_min"))),
      task_interval_min=if(noncyclic) map_dbl(name,~val(paste0("interval_",.x), station_field(.x, "task_interval_min"))) else Inf,
      task_duration_min=if(noncyclic) map_dbl(name,~val(paste0("task_",.x), station_field(.x, "task_duration_min"))) else 0,
      task_every_n=map_dbl(name,~val(paste0("every_",.x), 0)))
    p$stations <- p$stations %>% mutate(
      load_timing=map(seq_len(n()),~read_timing(p$stations[.x,],"load", ignore)),
      auto_timing=map(seq_len(n()),~read_timing(p$stations[.x,],"auto", ignore)),
      unload_timing=map(seq_len(n()),~read_timing(p$stations[.x,],"unload", ignore)),
      task_timing=map(seq_len(n()),~if(noncyclic) read_timing(p$stations[.x,],"task", ignore) else timing_spec()))
    if (identical(mode, "try")) {
      p$stations$capacity[p$stations$name == "paint"] <- 2L
    }
    validate(need(p$warmup_min<p$sim_length_min,"Warmup must be shorter than the simulation."))
    normalize_params(p)
  }
  build_params <- reactive(assemble_params(guide_mode()))
  observeEvent(input$guide_try, {
    updateNumericInput(session, "paint_cap", value = 2)
    guide_mode("try")
    exp_params <- tryCatch(assemble_params("try"), error = function(e) NULL)
    if (!is.null(exp_params)) {
      base_params <- comparison_baseline_params(exp_params)
      current <- tryCatch(isolate(sim_result()), error = function(e) NULL)
      # Reuse the run already on screen when it is the 1-bay line with the
      # same seeds and replications. Otherwise simulate that baseline once.
      baseline_run <- if (!is.null(current) && comparison_same(current$params, base_params)) {
        current
      } else {
        local({
          show_sim_progress(
            sprintf("%d replications · 1-bay baseline · horizon %.0f min",
                    base_params$n_replications, base_params$sim_length_min),
            0.25
          )
          # withProgress forces a websocket flush so the fixed panel paints;
          # its own toast stays hidden by CSS.
          tryCatch(
            withProgress(message = "", value = 0.25, run_line_sim(base_params)),
            finally = hide_sim_progress()
          )
        })
      }
      guide_pair(list(baseline = baseline_run, experiment_params = exp_params))
    }
    guide_run(isolate(guide_run()) + 1L)
  })
  observeEvent(input$guide_reset, {
    guide_pair(NULL)
    restore_default_inputs(session)
    guide_mode("defaults")
    guide_run(isolate(guide_run()) + 1L)
  })
  # Drop the overlay once the sidebar shows the same line the run used.
  observe({
    mode <- guide_mode()
    if (is.null(mode)) return()
    matched <- tryCatch(
      identical(params_fingerprint(assemble_params(NULL)), params_fingerprint(assemble_params(mode))),
      error = function(e) FALSE
    )
    if (isTRUE(matched)) guide_mode(NULL)
  })
  two_set_study <- comparison_page_server("two_set_study", build_params, reactive(input$main_page))

  run_seq <- new.env(parent = emptyenv())
  run_seq$main <- 0L
  run_seq$staffing <- 0L
  run_seq$timing <- 0L
  staffing_trigger <- reactiveVal(0L)
  timing_trigger <- reactiveVal(0L)
  # ignoreInit would swallow the first click when the input is created late.
  # Value 0 is the button's initial state, not a request to rerun.
  bump <- function(value, trigger) {
    if (is.null(value) || value < 1) return()
    trigger(isolate(trigger()) + 1L)
  }
  observeEvent(input$compare, bump(input$compare, staffing_trigger), ignoreNULL = TRUE)
  observeEvent(input$rerun_staffing, bump(input$rerun_staffing, staffing_trigger), ignoreNULL = TRUE)
  observeEvent(input$compare_timing, bump(input$compare_timing, timing_trigger), ignoreNULL = TRUE)
  observeEvent(input$rerun_timing, bump(input$rerun_timing, timing_trigger), ignoreNULL = TRUE)
  comparison <- eventReactive(staffing_trigger(), {
    req(isTRUE(staffing_trigger() > 0))
    p <- build_params()
    fp <- params_fingerprint(p)
    settings <- format_run_settings(p)
    alternatives <- list(p,p,p,p)
    alternatives[[2]]$operators$count[[1]] <- alternatives[[2]]$operators$count[[1]]+1L
    alternatives[[3]]$operators$count[[nrow(p$operators)]] <- alternatives[[3]]$operators$count[[nrow(p$operators)]]+1L
    alternatives[[4]]$operators <- tibble(pool="shared",count=sum(p$operators$count))
    alternatives[[4]]$stations$operator_pool <- "shared"
    show_sim_progress(
      sprintf("Staffing comparison · %d replications", p$n_replications),
      0.25
    )
    out <- tryCatch(
      withProgress(message = "", value = 0.25, run_scenarios(tibble(
        scenario=c("Current","Add one to first team","Add one to last team","Same headcount, shared team"),params=alternatives))),
      finally = hide_sim_progress()
    )
    run_seq$staffing <- run_seq$staffing + 1L
    tag_comparison(out, p, run_seq$staffing, fingerprint=fp, summary=settings)
  }, ignoreNULL = TRUE)
  timing_comparison <- eventReactive(timing_trigger(), {
    req(isTRUE(timing_trigger() > 0))
    p <- build_params()
    fp <- params_fingerprint(p)
    settings <- format_run_settings(p)
    show_sim_progress(
      sprintf("Timing comparison · %d replications", p$n_replications),
      0.25
    )
    out <- tryCatch(
      withProgress(message = "", value = 0.25, run_scenarios(variability_scenarios(p))),
      finally = hide_sim_progress()
    )
    run_seq$timing <- run_seq$timing + 1L
    tag_comparison(out, p, run_seq$timing, fingerprint=fp, summary=settings)
  }, ignoreNULL = TRUE)
  observeEvent(input$compare, {
    nav_select("main_page", "results_capacity", session = session)
    nav_select("capacity_tabs", "staffing", session = session)
  }, ignoreInit = TRUE)
  observeEvent(input$compare_timing, {
    nav_select("main_page", "results_lead", session = session)
    nav_select("lead_tabs", "timing", session = session)
  }, ignoreInit = TRUE)
  observeEvent(input$kpi_nav, {
    page <- input$kpi_nav
    if (!is.null(page) && nzchar(page)) {
      nav_select("main_page", page, session = session)
      session$sendCustomMessage("closeResultsNav", list())
    }
  }, ignoreInit = TRUE)
  output$timing_table <- renderDT({
    req(isTRUE(timing_trigger() > 0))
    timing_comparison() %>% dplyr::select(scenario,comparison) %>% unnest(comparison) %>%
      datatable(rownames=FALSE,options=list(dom="t")) %>% formatRound(columns=3:10,digits=2)
  })
  output$scenario_table <- renderDT({
    req(isTRUE(staffing_trigger() > 0))
    comparison() %>% dplyr::select(scenario,comparison) %>% unnest(comparison) %>%
      datatable(rownames=FALSE,options=list(dom="t")) %>% formatRound(columns=3:10,digits=2)
  })
  output$labor_table <- renderDT({sim_result()$labor %>% group_by(resource) %>% summarise(utilization=mean(avg_util),queue=mean(avg_queue),.groups="drop") %>% datatable(rownames=FALSE,options=list(dom="t")) %>% formatPercentage("utilization",1)})

  output$reliability_table <- renderDT({
    r <- sim_result()
    r$params$stations %>% dplyr::select(name,mtbf_min,mttr_min) %>%
      left_join(r$failure_events %>% count(station,name="observed_failures"),by=c("name"="station")) %>%
      mutate(observed_failures=replace_na(observed_failures,0L)) %>%
      datatable(rownames=FALSE,options=list(dom="t"))
  })
  # Run on button click; also fire once on load so KPIs appear immediately.
  # The fixed progress panel is opened from a click handler in JS so it paints
  # before this reactive blocks the session. hide_sim_progress() clears it.
  sim_result <- reactive({
    p <- build_params()
    fp <- params_fingerprint(p)
    settings <- format_run_settings(p)
    show_sim_progress(
      sprintf("%d replications · horizon %.0f min", p$n_replications, p$sim_length_min),
      0.35
    )
    res <- tryCatch(
      withProgress(message = "", value = 0.35, run_line_sim(p)),
      finally = hide_sim_progress()
    )
    run_seq$main <- run_seq$main + 1L
    stamp_run(res, p, run_seq$main, fingerprint = fp, summary = settings)
  }) %>% bindEvent(input$run, guide_run(), ignoreNULL = FALSE)

  # The experiment is the guided run that just finished. Pair it with the
  # baseline captured above instead of simulating the 2-bay line again.
  # ignoreInit is omitted on purpose: this observer is created after the
  # initial simulation is scheduled, and skipping its first execution would
  # drop the guided run when that run is the first time the observer fires.
  observeEvent(sim_result(), {
    pair <- isolate(guide_pair())
    if (is.null(pair)) return()
    guide_pair(NULL)
    experiment <- sim_result()
    if (!comparison_same(experiment$params, pair$experiment_params)) return()
    two_set_study$load_pair(pair$baseline, experiment, comparison_guided_labels)
  })

  current_fingerprint <- reactive(params_fingerprint(build_params()))
  production_page_server("production_results",sim_result,current_fingerprint)
  main_settings_stale <- reactive({
    !identical(sim_result()$provenance$fingerprint, current_fingerprint())
  })
  snapshot_is_stale <- function(comp) {
    fp <- attr(comp, "fingerprint")
    if (is.null(fp)) return(FALSE)
    !identical(fp, current_fingerprint()) || !identical(fp, sim_result()$provenance$fingerprint)
  }
  staffing_stale <- reactive({
    if (!isTRUE(staffing_trigger() > 0)) return(FALSE)
    snapshot_is_stale(comparison())
  })
  timing_stale <- reactive({
    if (!isTRUE(timing_trigger() > 0)) return(FALSE)
    snapshot_is_stale(timing_comparison())
  })
  output$main_stale_flag <- renderText(if (isTRUE(main_settings_stale())) "stale" else "fresh")
  output$staffing_stale_flag <- renderText(if (isTRUE(staffing_stale())) "stale" else "fresh")
  output$timing_stale_flag <- renderText(if (isTRUE(timing_stale())) "stale" else "fresh")
  output$main_stale_notice <- renderUI({
    if (!isTRUE(main_settings_stale())) return(NULL)
    provenance_warning("Settings changed. Click Run to update results.")
  })
  output$run_settings_body <- renderUI({
    prov <- sim_result()$provenance
    tagList(
      p(
        class = "mb-1",
        tags$strong(sprintf("Run %d", prov$run_number)),
        sprintf(" · %s", format(prov$run_at, "%Y-%m-%d %H:%M:%S"))
      ),
      p(class = "small mb-0", prov$summary)
    )
  })
  output$run_settings_text <- renderText({
    prov <- sim_result()$provenance
    paste0("Run ", prov$run_number, " · ", format(prov$run_at, "%Y-%m-%d %H:%M:%S"), " · ", prov$summary)
  })
  output$result_freshness_style <- renderUI({
    opacity <- function(stale) if (isTRUE(stale)) "0.45" else "1"
    main_ids <- c(
      "headline_kpis", "plot_util", "plot_labor", "plot_down", "plot_wip",
      "plot_queue", "plot_cycle", "labor_table", "reliability_table", "table_metrics"
    )
    css <- paste0("#", main_ids, "{opacity:", opacity(main_settings_stale()), ";}", collapse = "")
    css <- paste0(
      css,
      "#scenario_table_wrap{opacity:", opacity(staffing_stale()), ";}",
      "#timing_table_wrap{opacity:", opacity(timing_stale()), ";}"
    )
    tags$style(HTML(css))
  })
  output$staffing_warning <- renderUI({
    if (!isTRUE(staffing_stale())) return(NULL)
    provenance_warning(
      "Settings changed since this comparison was run. Rerun comparison.",
      "rerun_staffing"
    )
  })
  output$staffing_meta <- renderUI({
    if (!isTRUE(staffing_trigger() > 0)) return(NULL)
    p(class = "small text-muted mb-2", comparison_caption(comparison()))
  })
  output$timing_warning <- renderUI({
    if (!isTRUE(timing_stale())) return(NULL)
    provenance_warning(
      "Settings changed since this comparison was run. Rerun comparison.",
      "rerun_timing"
    )
  })
  output$timing_meta <- renderUI({
    if (!isTRUE(timing_trigger() > 0)) return(NULL)
    p(class = "small text-muted mb-2", comparison_caption(timing_comparison()))
  })

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

  output$plot_util <- sim_render_plot({
    res <- sim_result()
    per_rep <- res$utilization_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    occupancy_boxes(per_rep)
  })

  output$plot_labor <- sim_render_plot({
    res <- sim_result()
    req(nrow(res$operator_by_rep) > 0)
    pools <- sub("^operator_", "", res$operator_by_rep$resource)
    levels <- if (all(pools %in% c("upstream", "downstream"))) {
      c("upstream", "downstream")
    } else if (all(pools %in% names(station_labels))) {
      names(station_labels)[names(station_labels) %in% unique(pools)]
    } else unique(pools)
    df <- res$operator_by_rep %>%
      mutate(label = factor(sub("^operator_", "", resource), levels = levels))
    replication_bars(
      df, "label", "avg_util",
      x_label = "Operator utilization",
      percent = TRUE
    )
  })

  output$plot_queue <- sim_render_plot({
    res <- sim_result()
    station <- input$queue_station %||% "all"
    layout <- input$queue_layout %||% "overlay"
    df <- res$queue_timeseries %>%
      mutate(label = factor(label_station(resource), levels = unname(station_labels)),
             rep_station = paste(replication, resource, sep = "_"))
    if (!identical(station, "all")) {
      df <- dplyr::filter(df, resource == station)
    }
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = time, y = queue, color = label)) +
      geom_line(aes(group = rep_station), alpha = 0.12, linewidth = 0.4, na.rm = TRUE) +
      geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                  linewidth = 1.15, alpha = 0.18, span = 0.35, na.rm = TRUE) +
      scale_color_manual(values = station_colors) +
      labs(x = "Time (minutes)", y = "Queue length", color = "Station",
           subtitle = plot_subtitle("Faint lines = each replication; bold = loess + ribbon")) +
      theme_sim(legend.position = "bottom")
    if (identical(layout, "facet") && identical(station, "all")) {
      p <- p + facet_wrap(~label, ncol = 2) + theme(legend.position = "none")
    }
    p
  })

  output$plot_wip <- sim_render_plot({
    res <- sim_result()
    ggplot(res$wip_timeseries, aes(x = time, y = wip)) +
      geom_line(aes(group = replication), alpha = 0.15, linewidth = 0.45,
                color = "#b35900", na.rm = TRUE) +
      geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                  linewidth = 1.2, color = "#b35900", fill = "#fdcc8a",
                  alpha = 0.25, span = 0.35, na.rm = TRUE) +
      labs(x = "Time (minutes)", y = "WIP (vehicles)",
           subtitle = plot_subtitle("Faint lines = each replication; bold = loess + ribbon")) +
      theme_sim()
  })

  output$plot_down <- sim_render_plot({
    res <- sim_result()
    per_rep <- res$downtime_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    replication_bars(
      per_rep, "label", "downtime_fraction",
      x_label = "Downtime fraction",
      percent = TRUE,
      fill_values = station_colors
    )
  })

  output$plot_cycle <- sim_render_plot({
    res <- sim_result()
    req(nrow(res$cycles) > 0)
    mean_ct <- mean(res$cycles$cycle_time, na.rm = TRUE)
    xlim <- density_xlim(res$cycles$cycle_time, level = 0.95, bounds = c(0, Inf))
    res$cycles %>%
      ggplot(aes(x = cycle_time)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = "#41b6c4", color = "white", alpha = 0.55) +
      geom_density(aes(group = replication), alpha = 0.25, linewidth = 0.4,
                   color = "#7fcdbb", na.rm = TRUE) +
      geom_density(linewidth = 1.15, color = "#253494", na.rm = TRUE) +
      geom_vline(xintercept = mean_ct, linetype = "dashed", color = "#253494") +
      sparse_x_continuous(percent = FALSE, n = 4L) +
      coord_cartesian(xlim = xlim, clip = "off") +
      labs(
        x = "Cycle time (minutes)", y = "Density",
        subtitle = paste0(
          "Faint = per-rep density; bold = pooled; mean = ",
          round(mean_ct, 1), " min"
        ),
        caption = "x-axis shows the central 95% of values"
      ) +
      theme_sim() +
      theme_plot_width_text() +
      theme_minor_ticks()
  })

  output$plot_waits <- ggiraph::renderGirafe({
    res <- sim_result()
    req(nrow(res$station_waits) > 0)
    wait_reason_bars_interactive(res$station_waits)
  })

  output$reliability_losses <- renderDT({
    res <- sim_result()
    res$downtime_by_rep %>%
      group_by(station) %>%
      summarise(
        `Active repair minutes` = mean(downtime_min),
        `Waiting for repair minutes` = mean(waiting_repair_min),
        `Total unavailable minutes` = mean(total_downtime_min),
        `Unavailable fraction` = mean(total_downtime_fraction),
        .groups = "drop"
      ) %>%
      mutate(station = label_station(station)) %>%
      datatable(rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
      formatRound(columns = 2:4, digits = 2) %>%
      formatPercentage("Unavailable fraction", 1)
  })

  output$table_metrics <- renderDT({
    res <- sim_result()
    tbl <- res$station_metrics %>%
      mutate(
        Station = label_station(station),
        `Occupancy %` = round(100 * avg_util_mean, 1),
        `Occupancy SD` = round(100 * avg_util_sd, 1),
        `Avg queue` = round(avg_queue_mean, 2),
        `Queue SD` = round(avg_queue_sd, 2),
        `Downtime %` = round(100 * downtime_fraction_mean, 1),
        `Downtime SD` = round(100 * downtime_fraction_sd, 1)
      ) %>%
      dplyr::select(Station, `Occupancy %`, `Occupancy SD`, `Avg queue`, `Queue SD`,
                    `Downtime %`, `Downtime SD`)
    datatable(tbl, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  write_download <- function(id, filename, table) {
    output[[id]] <- downloadHandler(
      filename = function() filename,
      content = function(file) readr::write_csv(table(), file)
    )
  }
  write_download("dl_queue", "queue_timeseries.csv", function() sim_result()$queue_timeseries)
  write_download("dl_wip", "wip_timeseries.csv", function() sim_result()$wip_timeseries)
  write_download("dl_util", "utilization_by_rep.csv", function() sim_result()$utilization_by_rep)
  write_download("dl_cycle", "cycle_times.csv", function() sim_result()$cycle_times)
  write_download("dl_metrics", "station_metrics.csv", function() sim_result()$station_metrics)
  write_download("dl_down", "downtime_by_rep.csv", function() sim_result()$downtime_by_rep)
  write_download("dl_repairs", "repair_events.csv", function() sim_result()$repair_events)
  write_download("dl_ops", "operator_by_rep.csv", function() sim_result()$operator_by_rep)
  write_download("dl_waits_main", "station_waits_by_rep.csv", function() sim_result()$station_waits)
  write_download("dl_util_summary", "occupancy_mean_ci.csv", function() {
    res <- sim_result()
    per_rep <- res$utilization_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    rep_mean_ci(per_rep, "label", "avg_util")
  })
  write_download("dl_labor_summary", "operator_util_mean_ci.csv", function() {
    res <- sim_result()
    df <- res$operator_by_rep %>%
      mutate(label = sub("^operator_", "", resource))
    rep_mean_ci(df, "label", "avg_util")
  })
  write_download("dl_down_summary", "downtime_mean_ci.csv", function() {
    res <- sim_result()
    per_rep <- res$downtime_by_rep %>%
      mutate(label = factor(label_station(station), levels = unname(station_labels)))
    rep_mean_ci(per_rep, "label", "downtime_fraction")
  })
  write_download("dl_params", "run_parameters.csv", function() sim_result()$run_parameters)
  output$dl_all <- downloadHandler(
    filename = function() paste0("automotive_line_sim_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      tmp <- tempfile("sim_export_")
      dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      write_sim_exports(sim_result(), dir = tmp, zip_path = file)
    }
  )
}

shinyApp(ui, server)
