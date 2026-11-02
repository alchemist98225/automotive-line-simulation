# comparison_logic.R
# Compare-two-sets: labels, lossless flatten, diff table, shift scores,
# metric frames, change captions, and the per-set parameter editor module.
# Used by the Compare page (no chart drawing here).

# Two editable parameter snapshots; no changes to the simulation engine.
# "Set 1" / "Set 2" stay the internal keys. Charts, tables, and downloads
# show the sanitized titles, which default to those keys.
comparison_colors <- c("Set 1"="#2474A5", "Set 2"="#D46B25")
comparison_default_labels <- names(comparison_colors)
comparison_guided_labels <- c("1 paint bay", "2 paint bays")
comparison_label_max <- 60L
comparison_highlight_n <- 4L
comparison_identical_note <- "The two sets have identical settings."
comparison_activities <- c(load="Load",auto="Auto",unload="Unload",task="Support")
comparison_globals <- c(shift_length_min="Shift duration (production minutes)",n_shifts="Number of measured shifts",warmup_min="Initial settling period (min)",
  n_replications="Simulation runs",base_seed="Base seed",planned_rate="Planned work rate (vehicles/hour)",entry_buffer="Entrance waiting positions",wip_limit="Maximum WIP (CONWIP)",
  grid_dt_min="Time-series interval (min)",repair_technicians="Repair technicians")
comparison_pools <- function(p) unique(c(p$operators$pool,"upstream","downstream","shared",p$stations$name))
# Lossless representations are also used to detect stale results, including empirical vectors.
comparison_flatten <- function(p) {
  p <- normalize_params(p);p$planned_rate<-60/p$arrival_mean_min
  scalar <- function(x) paste(format(x,digits=17,trim=TRUE),collapse=", ")
  timing <- function(x) switch(x$method,
    fixed=paste("fixed",scalar(x$mean_min)),
    empirical=paste("empirical",scalar(x$observations)),
    distribution=paste(x$distribution,scalar(x$mean_min),if(x$distribution!="exponential") scalar(x$sd_min) else ""))
  run <- tibble(parameter=paste0("Run / ",unname(comparison_globals)),value=map_chr(names(comparison_globals),~scalar(p[[.x]] %||% 2)))
  ops <- p$operators %>% transmute(parameter=paste0("Operators / ",pool),value=map_chr(count,scalar))
  stations <- map_dfr(seq_len(nrow(p$stations)),function(i) {
    st <- p$stations[i,]
    cols <- c("capacity","buffer_before","operator_pool","mtbf_min","mttr_min","task_interval_min","task_every_n")
    bind_rows(tibble(parameter=paste(label_station(st$name),c("Capacity","Queue","Operator pool","MTBF","MTTR","Support interval","Every N"),sep=" / "),value=map_chr(cols,~scalar(st[[.x]]))),
      tibble(parameter=paste(label_station(st$name),unname(comparison_activities),sep=" / "),
        value=map_chr(names(comparison_activities),~timing(activity_timing(st,.x)))))
  })
  bind_rows(run,ops,stations,tibble(parameter=c("Run / Admission policy","Run / Order spacing"),value=c(p$release_policy,p$order_spacing))) %>% arrange(parameter)
}
comparison_same <- function(a,b) identical(comparison_flatten(a),comparison_flatten(b))
# Blank titles fall back. Control characters and extra whitespace are removed.
# A very long title is cut so legends and column headers stay readable.
sanitize_set_label <- function(text, fallback) {
  fallback <- as.character(fallback)[[1]]
  text <- paste(as.character(text %||% ""), collapse = " ")
  text <- gsub("[[:cntrl:]]+", " ", text)
  text <- gsub("[[:space:]]+", " ", text)
  text <- trimws(text)
  if (!nzchar(text)) return(fallback)
  if (nchar(text) > comparison_label_max) {
    text <- trimws(substr(text, 1L, comparison_label_max))
  }
  if (!nzchar(text)) fallback else text
}
# Column headers and scale limits have to stay distinct, so a repeated title
# gets a short suffix instead of collapsing both sets into one level.
sanitize_set_labels <- function(labels, fallback = comparison_default_labels) {
  fallback <- as.character(fallback)
  if (length(fallback) < 2L) fallback <- comparison_default_labels
  first <- sanitize_set_label(if (length(labels) >= 1L) labels[[1]] else "", fallback[[1]])
  second <- sanitize_set_label(if (length(labels) >= 2L) labels[[2]] else "", fallback[[2]])
  if (identical(first, second)) {
    suffix <- " (2)"
    room <- comparison_label_max - nchar(suffix)
    base <- if (room > 0L && nchar(second) > room) trimws(substr(second, 1L, room)) else second
    second <- paste0(base, suffix)
    if (!nzchar(base)) second <- paste0(fallback[[2]], suffix)
  }
  c(first, second)
}
comparison_palette <- function(labels = comparison_default_labels) {
  labels <- as.character(labels)
  stats::setNames(unname(comparison_colors)[seq_along(labels)], labels)
}
pretty_parameter <- function(x) {
  x <- sub("^Run / ", "", x)
  gsub(" / ", " — ", x, fixed = TRUE)
}
# Policy codes stay in the lossless snapshot. The difference table shows the
# same words as the editors, with units already in the parameter name.
comparison_display_value <- function(parameter, value) {
  value <- as.character(value)
  if (identical(parameter, "Run / Admission policy")) {
    labels <- c(
      space = "Entrance space",
      conwip = "WIP-limited (CONWIP)",
      scheduled = "Release all planned work"
    )
    hit <- unname(labels[value])
    return(if (length(hit) == 1L && !is.na(hit)) hit else value)
  }
  if (identical(parameter, "Run / Order spacing")) {
    labels <- c(fixed = "Evenly scheduled", exponential = "Random")
    hit <- unname(labels[value])
    return(if (length(hit) == 1L && !is.na(hit)) hit else value)
  }
  if (grepl("Queue", parameter, fixed = TRUE) && value %in% c("Inf", "inf")) {
    return("Unlimited")
  }
  value
}
comparison_diff <- function(a, b, labels = comparison_default_labels) {
  labels <- sanitize_set_labels(labels)
  diff <- full_join(
    comparison_flatten(a) %>% rename(value_1 = value),
    comparison_flatten(b) %>% rename(value_2 = value),
    by = "parameter"
  ) %>%
    mutate(
      value_1 = replace_na(value_1, "Not used"),
      value_2 = replace_na(value_2, "Not used")
    ) %>%
    filter(value_1 != value_2) %>%
    arrange(parameter) %>%
    mutate(
      value_1 = map2_chr(parameter, value_1, comparison_display_value),
      value_2 = map2_chr(parameter, value_2, comparison_display_value)
    ) %>%
    transmute(Parameter = pretty_parameter(parameter), value_1, value_2)
  names(diff)[2:3] <- labels
  diff
}
# Compact HTML table of the draft difference. No rows means the sets match.
comparison_diff_table <- function(a, b, labels = comparison_default_labels) {
  diff <- comparison_diff(a, b, labels)
  if (!nrow(diff)) {
    return(div(
      class = "alert alert-light border py-2 mb-3 comparison-draft-diff",
      comparison_identical_note
    ))
  }
  headers <- names(diff)
  div(
    class = "table-responsive mb-3 comparison-draft-diff",
    tags$table(
      class = "table table-sm table-striped align-middle mb-0",
      tags$caption(class = "caption-top text-start pb-1", "Settings that differ"),
      tags$thead(tags$tr(lapply(headers, tags$th))),
      tags$tbody(lapply(seq_len(nrow(diff)), function(i) {
        tags$tr(lapply(as.list(diff[i, ]), function(cell) tags$td(as.character(cell))))
      }))
    )
  )
}
# Paint-bay experiment: same seeds, replications, and every other setting.
comparison_baseline_params <- function(experiment) {
  p <- normalize_params(experiment)
  idx <- which(p$stations$name == "paint")
  if (length(idx) != 1L) stop("Baseline needs one paint station.", call. = FALSE)
  p$stations$capacity[idx] <- 1L
  validate_params(p)
}
# Relative size of a shift between two replication vectors.
# score = |mean2 - mean1| / |mean1| when the first set's mean is larger than
# the pooled replication SD: a percent change on a stable baseline.
# When that mean is near zero or smaller than the noise (an empty queue),
# divide by the larger of |mean2| and the pooled SD. The score then stays on
# a 0–1 relative scale instead of exploding, and standardized change is still
# reported for the CSV. Ranking uses score only.
comparison_shift <- function(x1, x2) {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  empty <- tibble(
    mean_1 = NA_real_, mean_2 = NA_real_, delta = NA_real_,
    relative = NA_real_, standardized = NA_real_, score = NA_real_
  )
  if (!length(x1) || !length(x2)) return(empty)
  m1 <- mean(x1)
  m2 <- mean(x2)
  delta <- m2 - m1
  n1 <- length(x1)
  n2 <- length(x2)
  s1 <- if (n1 >= 2L) stats::sd(x1) else NA_real_
  s2 <- if (n2 >= 2L) stats::sd(x2) else NA_real_
  pooled <- if (n1 >= 2L && n2 >= 2L && is.finite(s1) && is.finite(s2)) {
    sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  } else {
    NA_real_
  }
  noise <- if (is.finite(pooled)) pooled else 0
  relative <- if (abs(m1) > 1e-8) abs(delta) / abs(m1) else NA_real_
  standardized <- if (is.finite(pooled) && pooled > 1e-8) {
    abs(delta) / pooled
  } else if (abs(delta) > 1e-8) {
    Inf
  } else {
    0
  }
  stable <- abs(m1) > max(noise, 1e-8)
  denom <- if (stable) abs(m1) else max(abs(m2), noise, 1e-8)
  tibble(
    mean_1 = m1, mean_2 = m2, delta = delta,
    relative = relative, standardized = standardized,
    score = abs(delta) / denom
  )
}
comparison_metric_frame <- function(runs) {
  line <- tibble::tribble(
    ~metric_id, ~label, ~x_label, ~percent, ~priority, ~column,
    "throughput_per_hr", "Throughput", "Throughput (vehicles/hour)", FALSE, 1L, "throughput_per_hr",
    "cycle_time_mean", "Mean lead time", "Mean lead time (min)", FALSE, 2L, "cycle_time_mean",
    "avg_wip", "Average WIP", "Average WIP (vehicles)", FALSE, 3L, "avg_wip"
  )
  line_df <- purrr::map_dfr(seq_along(runs), function(i) {
    summaries <- runs[[i]]$summaries
    purrr::map_dfr(seq_len(nrow(line)), function(k) {
      tibble(
        metric_id = line$metric_id[[k]],
        label = line$label[[k]],
        x_label = line$x_label[[k]],
        percent = line$percent[[k]],
        priority = line$priority[[k]],
        set_index = i,
        replication = summaries$replication,
        value = summaries[[line$column[[k]]]]
      )
    })
  })
  station_priority <- function(station, offset) {
    offset + replace_na(match(station, names(station_labels)), 50L)
  }
  station_df <- purrr::map_dfr(seq_along(runs), function(i) {
    util <- runs[[i]]$utilization_by_rep
    if (is.null(util) || !nrow(util)) return(tibble())
    bind_rows(
      util %>% transmute(
        metric_id = paste0("util_", station),
        label = paste(label_station(station), "bay occupancy"),
        x_label = "Bay occupancy",
        percent = TRUE,
        priority = station_priority(station, 10L),
        set_index = i,
        replication = replication,
        value = avg_util
      ),
      util %>% transmute(
        metric_id = paste0("queue_", station),
        label = paste(label_station(station), "queue"),
        x_label = "Mean queue (vehicles)",
        percent = FALSE,
        priority = station_priority(station, 20L),
        set_index = i,
        replication = replication,
        value = avg_queue
      )
    )
  })
  bind_rows(line_df, station_df)
}
comparison_rank_changes <- function(runs, n = comparison_highlight_n) {
  frame <- comparison_metric_frame(runs)
  n <- suppressWarnings(as.integer(n[[1]]))
  if (length(n) != 1L || is.na(n) || n < 1L) n <- comparison_highlight_n
  if (!nrow(frame)) {
    empty <- tibble(
      metric_id = character(), label = character(), x_label = character(),
      percent = logical(), priority = integer(), mean_1 = double(), mean_2 = double(),
      delta = double(), relative = double(), standardized = double(), score = double()
    )
    return(list(frame = frame, scores = empty, shown = empty))
  }
  scores <- frame %>%
    group_by(metric_id, label, x_label, percent, priority) %>%
    summarise(
      shift = list(comparison_shift(value[set_index == 1L], value[set_index == 2L])),
      .groups = "drop"
    ) %>%
    tidyr::unnest(shift) %>%
    mutate(score = if_else(is.na(score), 0, score)) %>%
    arrange(desc(score), priority, metric_id)
  shown <- scores %>%
    filter(score > 0) %>%
    slice_head(n = n)
  list(frame = frame, scores = scores, shown = shown)
}
comparison_highlight_export <- function(state, labels, metric_id = NULL) {
  labels <- as.character(labels)
  shown <- state$shown
  empty <- tibble(
    rank = integer(), metric = character(), set = character(),
    replication = integer(), value = double(),
    relative_change = double(), standardized_change = double(), score = double()
  )
  if (!nrow(shown)) return(empty)
  if (!is.null(metric_id)) shown <- shown %>% filter(.data$metric_id == metric_id)
  if (!nrow(shown)) return(empty)
  state$frame %>%
    inner_join(
      shown %>% transmute(
        metric_id,
        rank = row_number(),
        relative_change = relative,
        standardized_change = standardized,
        score,
        metric = label
      ),
      by = "metric_id"
    ) %>%
    transmute(
      rank,
      metric,
      set = labels[set_index],
      replication,
      value,
      relative_change,
      standardized_change,
      score
    ) %>%
    arrange(rank, set, replication)
}
# Plain language from the two runs. Station names and directions come from
# the results; the sentence frames are the only fixed wording.
comparison_change_caption <- function(runs, scores) {
  station_name <- function(x) {
    x <- as.character(x)
    if (!length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) return(NULL)
    if (exists("label_station", inherits = TRUE)) unname(label_station(x[[1]])) else x[[1]]
  }
  parts <- character()
  b1 <- station_name(runs[[1]]$bottleneck)
  b2 <- station_name(runs[[2]]$bottleneck)
  if (!is.null(b1) && !is.null(b2)) {
    parts <- c(parts, if (!identical(b1, b2)) {
      sprintf("The busiest station shifted from %s to %s.", b1, b2)
    } else {
      sprintf("The busiest station stayed at %s.", b1)
    })
  }
  line_word <- c(
    throughput_per_hr = "throughput",
    cycle_time_mean = "mean lead time",
    avg_wip = "WIP"
  )
  bits <- character()
  for (id in names(line_word)) {
    row <- scores[scores$metric_id == id, ]
    if (!nrow(row) || !is.finite(row$delta[[1]]) || abs(row$delta[[1]]) <= 1e-8) next
    verb <- if (row$delta[[1]] > 0) "rose" else "fell"
    bits <- c(bits, paste(line_word[[id]], verb))
  }
  if (length(bits)) {
    bits[[1]] <- paste0(toupper(substr(bits[[1]], 1, 1)), substr(bits[[1]], 2, nchar(bits[[1]])))
    sentence <- if (length(bits) == 1L) {
      paste0(bits, ".")
    } else if (length(bits) == 2L) {
      paste0(bits[[1]], " and ", bits[[2]], ".")
    } else {
      paste0(paste(bits[-length(bits)], collapse = ", "), ", and ", bits[[length(bits)]], ".")
    }
    parts <- c(parts, sentence)
  }
  if (nrow(scores)) {
    top <- scores[1, ]
    if (is.finite(top$score) && top$score > 0 && !top$metric_id %in% names(line_word)) {
      verb <- if (!is.finite(top$delta)) "changed" else if (top$delta > 0) "rose" else if (top$delta < 0) "fell" else "stayed flat"
      parts <- c(parts, sprintf("The largest shift was %s, which %s.", top$label, verb))
    }
  }
  if (!length(parts)) {
    "Throughput, lead time, WIP, bay occupancy, and queues did not move."
  } else {
    paste(parts, collapse = " ")
  }
}

parameter_editor_ui <- function(id) uiOutput(NS(id,"controls"))
parameter_editor_server <- function(id) moduleServer(id,function(input,output,session) {
  state <- reactiveVal(list(revision=0L,params=normalize_params(default_params())))
  set_params <- function(p) {
    p <- normalize_params(p) %>% validate_params()
    state(list(revision=isolate(state()$revision)+1L,params=p))
  }
  output$controls <- renderUI({
    p <- state()$params;p$planned_rate<-60/p$arrival_mean_min; revision <- state()$revision
    key <- function(x) paste0("v",revision,"_",x)
    ns <- function(x) session$ns(key(x))
    pools <- comparison_pools(p)
    activity_ui <- function(st,a) {
      spec <- activity_timing(st,a); prefix <- paste(st$name,a,sep="_")
      method_id <- key(paste0(prefix,"_method"))
      mean_value <- if(spec$method=="empirical") mean(spec$observations) else spec$mean_min
      tagList(
        selectInput(ns(paste0(prefix,"_method")),"Method",
          c("Fixed"="fixed","Distribution"="distribution","Measured observations"="empirical"),selected=spec$method),
        conditionalPanel(sprintf("input['%s'] !== 'empirical'",method_id),ns=session$ns,
          numericInput(ns(paste0(prefix,"_mean")),"Mean",mean_value,min=0,step=.1)),
        conditionalPanel(sprintf("input['%s'] === 'distribution'",method_id),ns=session$ns,
          selectInput(ns(paste0(prefix,"_distribution")),"Distribution",c("Normal, floored at zero"="normal","Lognormal"="lognormal","Exponential"="exponential"),
            selected=if(spec$method=="distribution") spec$distribution else "normal"),
          numericInput(ns(paste0(prefix,"_sd")),"SD (min; ignored for exponential)",if(is.finite(spec$sd_min)) spec$sd_min else 0,min=0,step=.1)),
        conditionalPanel(sprintf("input['%s'] === 'empirical'",method_id),ns=session$ns,
          textAreaInput(ns(paste0(prefix,"_observations")),"Observed minutes (commas, spaces or newlines)",
            value=if(spec$method=="empirical") paste(format(spec$observations,digits=17),collapse=", ") else "",rows=3)))
    }
    accordion(id=session$ns("editor_panels"),open=FALSE,multiple=TRUE,
      accordion_panel("Run settings",tagList(map(names(comparison_globals),function(n) {
        numericInput(ns(n),comparison_globals[[n]],p[[n]] %||% 2,
          min=if(n %in% c("warmup_min","base_seed","entry_buffer")) 0 else if(n %in% c("shift_length_min","planned_rate","grid_dt_min")) .001 else 1,
          step=if(n %in% c("planned_rate","grid_dt_min")) .1 else 1)
      })),
        selectInput(ns("release_policy"),"Admission policy",c("Entrance space"="space","WIP-limited (CONWIP)"="conwip","Release all planned work"="scheduled"),selected=p$release_policy),
        selectInput(ns("order_spacing"),"Order spacing",c("Evenly scheduled"="fixed","Random"="exponential"),selected=p$order_spacing),
        helpText("Shifts are consecutive production periods after settling, without nights or weekends. Release-all leaves the entrance queue unlimited.")),
      accordion_panel("Operators & assignments",
        p("A pool can serve several stations. Count 0 removes an unused pool. Every assigned pool needs at least one operator."),
        tagList(map(pools,function(pool) numericInput(ns(paste0("pool_",pool)),paste(pool,"operators"),
          if(pool %in% p$operators$pool) p$operators$count[match(pool,p$operators$pool)] else 0,min=0,step=1))),
        tagList(map(seq_len(nrow(p$stations)),function(i) selectInput(ns(paste0("assignment_",p$stations$name[i])),
          paste(label_station(p$stations$name[i]),"operator pool"),choices=c("No labor constraint"="",setNames(pools,pools)),selected=if(is.na(p$stations$operator_pool[i])) "" else p$stations$operator_pool[i])))),
      accordion_panel("Stations & activity timing",
        p("Capacity is parallel bays. Queue is vehicles that can wait before the station. Timing values are mean minutes (Load / Auto / Unload / Support)."),
        tagList(map(seq_len(nrow(p$stations)),function(i) {
        st <- p$stations[i,]
        accordion(open=FALSE,multiple=TRUE,accordion_panel(label_station(st$name),
          numericInput(ns(paste0("capacity_",st$name)),"Capacity",st$capacity,min=1,step=1),
          if(i>1) numericInput(ns(paste0("buffer_",st$name)),"Queue",if(is.finite(st$buffer_before)) st$buffer_before else 3,min=0,step=1),
          accordion(open=FALSE,multiple=TRUE,!!!map(names(comparison_activities),function(a)
            accordion_panel(comparison_activities[[a]],activity_ui(st,a))))))
      }))),
      accordion_panel("Reliability & support triggers",
        p("MTBF / MTTR are mean minutes. MTTR of 0 disables failures. Support interval is mean minutes; Every N of 0 uses a random exponential interval."),
        tagList(map(seq_len(nrow(p$stations)),function(i) {
        st <- p$stations[i,]; n <- st$name
        tagList(h6(label_station(n)),
          checkboxInput(ns(paste0("failures_",n)),"Equipment failures",is.finite(st$mtbf_min) && st$mttr_min>0),
          numericInput(ns(paste0("mtbf_",n)),"MTBF",if(is.finite(st$mtbf_min)) st$mtbf_min else 10000,min=.001),
          numericInput(ns(paste0("mttr_",n)),"MTTR",st$mttr_min,min=0),
          checkboxInput(ns(paste0("support_",n)),"Support enabled",timing_mean(activity_timing(st,"task"))>0 && (st$task_every_n>0 || is.finite(st$task_interval_min))),
          numericInput(ns(paste0("interval_",n)),"Support interval",if(is.finite(st$task_interval_min)) st$task_interval_min else 60,min=.001),
          numericInput(ns(paste0("every_",n)),"Every N",st$task_every_n,min=0,step=1))
      }))))
  })
  outputOptions(output,"controls",suspendWhenHidden=FALSE)
  params <- reactive({
    p <- state()$params;p$planned_rate<-60/p$arrival_mean_min; revision <- state()$revision
    val <- function(id,fallback) input[[paste0("v",revision,"_",id)]] %||% fallback
    pools <- comparison_pools(p)
    walk(names(comparison_globals),function(n) p[[n]] <<- val(n,p[[n]] %||% 2))
    p$time_mode<-"shifts";p$arrival_mean_min<-60/p$planned_rate;p$planned_rate<-NULL
    p$release_policy<-val("release_policy",p$release_policy);p$order_spacing<-val("order_spacing",p$order_spacing)
    p<-normalize_params(p)
    p$operators <- tibble(pool=pools,count=map_dbl(pools,function(pool)
      val(paste0("pool_",pool),if(pool %in% p$operators$pool) p$operators$count[match(pool,p$operators$pool)] else 0)))
    stopifnot(all(is.finite(p$operators$count) & p$operators$count>=0 & p$operators$count==floor(p$operators$count)))
    p$operators <- p$operators %>% filter(count>0)
    st <- p$stations
    read_spec <- function(i,a) {
      spec <- activity_timing(st[i,],a); prefix <- paste(st$name[i],a,sep="_")
      method <- val(paste0(prefix,"_method"),spec$method)
      if(method=="empirical") return(timing_spec("empirical",observations=parse_observations(
        val(paste0(prefix,"_observations"),if(spec$method=="empirical") paste(spec$observations,collapse=",") else ""))))
      timing_spec(method,mean_min=val(paste0(prefix,"_mean"),if(spec$method=="empirical") mean(spec$observations) else spec$mean_min),
        sd_min=val(paste0(prefix,"_sd"),if(is.finite(spec$sd_min)) spec$sd_min else 0),
        distribution=val(paste0(prefix,"_distribution"),if(spec$method=="distribution") spec$distribution else "normal"))
    }
    walk(seq_len(nrow(st)),function(i) {
      n <- st$name[i]
      if(i>1) p$stations$buffer_before[i] <<- val(paste0("buffer_",n),st$buffer_before[i])
      p$stations$capacity[i] <<- val(paste0("capacity_",n),st$capacity[i])
      pool <- val(paste0("assignment_",n),if(is.na(st$operator_pool[i])) "" else st$operator_pool[i])
      p$stations$operator_pool[i] <<- if(pool=="") NA_character_ else pool
      enabled <- val(paste0("failures_",n),is.finite(st$mtbf_min[i]) && st$mttr_min[i]>0)
      p$stations$mtbf_min[i] <<- if(enabled) val(paste0("mtbf_",n),st$mtbf_min[i]) else Inf
      p$stations$mttr_min[i] <<- val(paste0("mttr_",n),st$mttr_min[i])
      support <- val(paste0("support_",n),timing_mean(activity_timing(st[i,],"task"))>0 && (st$task_every_n[i]>0 || is.finite(st$task_interval_min[i])))
      p$stations$task_interval_min[i] <<- if(support) val(paste0("interval_",n),st$task_interval_min[i]) else Inf
      p$stations$task_every_n[i] <<- val(paste0("every_",n),st$task_every_n[i])
      walk(names(comparison_activities),function(a) p$stations[[paste0(a,"_timing")]][[i]] <<- if(a=="task" && !support) timing_spec() else read_spec(i,a))
    })
    # Import/read roundtrips preserve normal input means, not their floored expectations.
    validate_params(p)
  })
  # Close every section of this set's editor. Nested station panels have no
  # id; closing these four sections is what gets the results on screen.
  # accordion_panel_close waits for a later flush. The run that just finished
  # may not schedule one, which leaves the sections open. Ask for that flush.
  # testServer's mock rejects requestFlush; a real session uses it.
  collapse <- function() {
    accordion_panel_close(session$ns("editor_panels"), values = TRUE)
    tryCatch(session$requestFlush(), error = function(cond) NULL)
    session$ns("editor_panels")
  }
  list(params=params,set_params=set_params,state=state,collapse=collapse)
})
