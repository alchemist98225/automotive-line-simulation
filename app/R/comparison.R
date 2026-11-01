# Two editable parameter snapshots; no changes to the simulation engine.
# "Set 1" / "Set 2" stay the internal keys. Charts, tables, and downloads
# show the sanitized titles, which default to those keys.
comparison_colors <- c("Set 1"="#2474A5", "Set 2"="#D46B25")
comparison_default_labels <- names(comparison_colors)
comparison_guided_labels <- c("Baseline: 1 paint bay", "Experiment: 2 paint bays")
comparison_label_max <- 60L
comparison_highlight_n <- 4L
comparison_identical_note <- "The two sets have identical settings."
comparison_activities <- c(load="Loading",auto="Automatic processing",unload="Unloading",task="Support work")
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
    bind_rows(tibble(parameter=paste(label_station(st$name),c("Parallel bays","Incoming waiting positions","Operator pool","Mean up-time (min)","Mean repair (min)","Task interval (min)","Task every N units"),sep=" / "),value=map_chr(cols,~scalar(st[[.x]]))),
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
  if (grepl("Incoming waiting positions", parameter, fixed = TRUE) && value %in% c("Inf", "inf")) {
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
        selectInput(ns(paste0(prefix,"_method")),"Timing method",
          c("Fixed"="fixed","Distribution"="distribution","Measured observations"="empirical"),selected=spec$method),
        conditionalPanel(sprintf("input['%s'] !== 'empirical'",method_id),ns=session$ns,
          numericInput(ns(paste0(prefix,"_mean")),"Duration / mean (min)",mean_value,min=0,step=.1)),
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
      accordion_panel("Stations & activity timing",tagList(map(seq_len(nrow(p$stations)),function(i) {
        st <- p$stations[i,]
        accordion(open=FALSE,multiple=TRUE,accordion_panel(label_station(st$name),
          numericInput(ns(paste0("capacity_",st$name)),"Parallel bays",st$capacity,min=1,step=1),
          if(i>1) numericInput(ns(paste0("buffer_",st$name)),"Incoming waiting positions",if(is.finite(st$buffer_before)) st$buffer_before else 3,min=0,step=1),
          accordion(open=FALSE,multiple=TRUE,!!!map(names(comparison_activities),function(a)
            accordion_panel(comparison_activities[[a]],activity_ui(st,a))))))
      }))),
      accordion_panel("Reliability & support triggers",tagList(map(seq_len(nrow(p$stations)),function(i) {
        st <- p$stations[i,]; n <- st$name
        tagList(h6(label_station(n)),
          checkboxInput(ns(paste0("failures_",n)),"Equipment failures",is.finite(st$mtbf_min) && st$mttr_min>0),
          numericInput(ns(paste0("mtbf_",n)),"Mean up-time (min)",if(is.finite(st$mtbf_min)) st$mtbf_min else 10000,min=.001),
          numericInput(ns(paste0("mttr_",n)),"Mean active repair (min; 0 disables)",st$mttr_min,min=0),
          checkboxInput(ns(paste0("support_",n)),"Support work enabled",timing_mean(activity_timing(st,"task"))>0 && (st$task_every_n>0 || is.finite(st$task_interval_min))),
          numericInput(ns(paste0("interval_",n)),"Random task mean interval (min)",if(is.finite(st$task_interval_min)) st$task_interval_min else 60,min=.001),
          numericInput(ns(paste0("every_",n)),"Task every N completions (0 = random)",st$task_every_n,min=0,step=1))
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

# Overlaid densities for two sets. Color follows the title-aware set palette.
# When y is a station or pool with more than one level, each category gets its
# own panel so the sets stay blue and orange. A set with no spread is a vertical
# line. Non-negative metrics start at 0; occupancy stays inside [0, 1].
comparison_densities <- function(df, y, x, x_label, subtitle, percent = TRUE, palette = comparison_colors,
                                 subtitle_width = 32L, bounds = NULL) {
  df <- df %>%
    filter(is.finite(.data[[x]])) %>%
    mutate(set = factor(set, levels = names(palette)))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No replications to plot")))
  }
  if (!identical(y, "set")) {
    if (is.factor(df[[y]])) {
      df[[y]] <- factor(df[[y]], levels = levels(df[[y]]))
    } else {
      df[[y]] <- factor(df[[y]], levels = unique(df[[y]]))
    }
  }
  if (is.null(bounds)) bounds <- if (isTRUE(percent)) c(0, 1) else c(0, Inf)
  colors <- ridge_fill_values(levels(df$set), palette)
  facet <- if (!identical(y, "set") && dplyr::n_distinct(df[[y]]) > 1L) y else NULL
  overlay_densities(
    df, x, "set", colors, x_label, subtitle,
    percent = percent, bounds = bounds, facet = facet, subtitle_width = subtitle_width
  )
}

# The Simulate page clamps .shiny-plot-output at 720px. That still crowds four
# overlaid ridges once the text is larger, and leaves Inspection against the axis.
# Scale the comparison ridgelines with the number of stations or pools.
comparison_ridge_n <- function(x) {
  n <- length(unique(x[!is.na(x)]))
  if (n < 1L) 1L else as.integer(n)
}
comparison_ridge_height <- function(n_ridges) {
  n <- suppressWarnings(as.integer(n_ridges[[1]]))
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 1L
  # Single-column facets need one panel stack; keep phone strips readable.
  as.integer(max(640L, 240L * n + 260L))
}
comparison_ridge_plot <- function(id, n_ridges) {
  h <- comparison_ridge_height(n_ridges)
  div(
    class = "comparison-ridge-plot",
    style = sprintf("--ridge-h:%dpx;", h),
    plotOutput(id, height = paste0(h, "px"), width = "100%", fill = FALSE)
  )
}
# Overlaid densities for one ranked metric. Rug ticks are every replication.
# A set with no spread is a vertical line in that set's color.
comparison_highlight_height <- 460L
# One decimal at or above 1; two significant digits below 1 (0.000338 -> 0.00034).
format_highlight_mean <- function(x) {
  if (!is.finite(x)) return("unavailable")
  if (abs(x) >= 1) sprintf("%.1f", x) else formatC(signif(x, 2), format = "fg", digits = 2)
}
# Short enough to wrap inside a half-width chart at base size 16. Means follow
# ridge order (top set, then lower set) and omit the full titles.
comparison_highlight_subtitle <- function(mean_1, mean_2, delta = NA_real_) {
  text <- sprintf(
    "Rug ticks are every replication. A vertical line means no spread. Each curve is scaled to its own peak. Means %s then %s.",
    format_highlight_mean(mean_1),
    format_highlight_mean(mean_2)
  )
  pct <- if (is.finite(mean_1) && abs(mean_1) > 1e-8 && is.finite(delta)) 100 * delta / mean_1 else NA_real_
  if (is.finite(pct) && abs(pct) <= 1000) text <- sprintf("%s Change %+.0f%%.", text, pct)
  text
}
comparison_highlight_plot <- function(df, x_label, subtitle, palette, percent = FALSE, bounds = c(0, Inf)) {
  df <- df %>% mutate(set = factor(.data$set, levels = names(palette)))
  if (isTRUE(percent)) bounds <- c(0, 1)
  comparison_densities(
    df, "set", "value", x_label, subtitle,
    percent = percent, palette = palette,
    subtitle_width = 32L, bounds = bounds
  )
}
comparison_highlight_frame <- function(id) {
  h <- comparison_highlight_height
  div(
    class = "comparison-ridge-plot",
    style = sprintf("--ridge-h:%dpx;", h),
    plotOutput(id, height = paste0(h, "px"), width = "100%", fill = FALSE)
  )
}

# Every replication is a faint line. The ribbon is a loess fit on all of them.
comparison_flow <- function(df, column, y_label, facet = NULL, palette = comparison_colors) {
  df <- df %>% mutate(set = factor(set, levels = names(palette)))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No time-series observations.")))
  }
  df$.line <- interaction(
    df$set,
    df$replication,
    if (is.null(facet)) 1L else df[[facet]],
    drop = TRUE
  )
  p <- ggplot(df, aes(x = time, y = .data[[column]], color = set, fill = set)) +
    geom_line(aes(group = .line), alpha = 0.12, linewidth = 0.4, na.rm = TRUE) +
    geom_smooth(
      aes(group = set),
      method = "loess", formula = y ~ x, se = TRUE,
      linewidth = 1.15, alpha = 0.18, span = 0.35, na.rm = TRUE
    ) +
    scale_color_manual(values = palette, drop = FALSE, name = NULL) +
    scale_fill_manual(values = palette, drop = FALSE, name = NULL) +
    labs(
      x = "Simulation time (min)",
      y = y_label,
      subtitle = plot_subtitle("Faint lines = each replication; bold = loess + ribbon. Each set uses its own window.")
    ) +
    theme_sim(legend.position = "bottom")
  if (!is.null(facet)) p <- p + facet_wrap(vars(.data[[facet]]), ncol = 1, labeller = label_wrap_gen(width = 14))
  p
}

comparison_download_catalog <- function(ns) {
  entry <- function(id, label, description) download_entry(ns(id), label, description)
  tagList(
    h5("Data behind the charts"),
    p(
      class = "text-muted small",
      "Each CSV stacks both parameter sets and every replication from the captured comparison. Results are illustrative only."
    ),
    entry(
      "dl_summaries", "Replication summaries",
      "Throughput, lead time, WIP, and HPU (labor-hours per unit) for every replication."
    ),
    entry(
      "dl_cycle", "Cycle times",
      "One row per departure in the observation window, from both sets."
    ),
    entry(
      "dl_util", "Bay occupancy by rep",
      "Time-weighted bay occupancy for each replication, station, and set."
    ),
    entry(
      "dl_ops", "Operator utilization",
      "Time-weighted utilization for each operator pool, replication, and set."
    ),
    entry(
      "dl_wip", "WIP timeseries",
      "Resampled vehicle count by replication, time, and set."
    ),
    entry(
      "dl_queue", "Queue timeseries",
      "Resampled bay queue by replication, station, time, and set."
    ),
    entry(
      "dl_down", "Downtime by rep",
      "Active repair fraction by replication, station, and set."
    ),
    entry(
      "dl_repairs", "Repair events",
      "One row per repair: station, start time, sampled duration, replication, and set."
    ),
    entry(
      "dl_metrics", "Station metrics",
      "Across-replication mean and SD of occupancy, queue, and downtime fraction."
    ),
    entry(
      "dl_highlights", "Largest changes",
      "Every replication behind the ranked charts: throughput, lead time, WIP, bay occupancy, and queues."
    ),
    div(
      class = "mt-3",
      downloadButton(ns("download"), "Download both runs and exact inputs (.rds)", class = "btn-secondary")
    )
  )
}

comparison_page_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Compare two sets",
    value = "compare_sets",
    div(
      class = "p-3 two-set-study",
      # Beats the app-wide plot clamp for these ridgelines only.
      tags$style(HTML("
        .comparison-ridge-plot > .shiny-plot-output {
          height: var(--ridge-h) !important;
          min-height: var(--ridge-h) !important;
          max-height: none !important;
        }
        .comparison-set-title .form-group { margin-bottom: 0; }
        .comparison-draft-diff { max-width: 100%; }
        .comparison-change-story { border-left: 4px solid #1b4f72; padding-left: 0.9rem; }
        .comparison-change-story,
        .comparison-change-story h4,
        .comparison-change-story p { color: #1a1a1a; }
        /* Match Simulate status bar: strong border, full-contrast text, progress. */
        .compare-status-bar { width: 100%; margin-bottom: 0.85rem; }
        .compare-progress-card {
          background: #eef6fb;
          border: 2px solid #2c7fb8;
          border-radius: 0.5rem;
          box-shadow: 0 4px 12px rgba(0,0,0,.10);
          min-height: 3.4rem;
          height: 3.4rem;
          box-sizing: border-box;
          display: flex;
          align-items: center;
          gap: 1rem;
          padding: 0.45rem 1rem;
          margin: 0;
        }
        .compare-progress-card.compare-progress-idle {
          border-color: #adb5bd;
          box-shadow: none;
          background: #f8f9fa;
        }
        .compare-progress-card.compare-progress-done {
          border-color: #198754;
          background: #f3faf5;
          box-shadow: 0 4px 12px rgba(25,135,84,.12);
        }
        .compare-progress-title {
          font-size: 1.05rem;
          font-weight: 700;
          color: #1b4f72 !important;
          margin: 0;
          white-space: nowrap;
          display: flex;
          align-items: center;
          gap: 0.45rem;
        }
        .compare-progress-title i,
        .compare-progress-title .fa,
        .compare-progress-title .fas {
          color: #1b4f72 !important;
        }
        .compare-progress-idle .compare-progress-title { color: #495057 !important; }
        .compare-progress-done .compare-progress-title { color: #146c43 !important; }
        .compare-progress-detail {
          flex: 1 1 auto;
          min-width: 0;
          margin: 0 !important;
          font-size: 0.95rem;
          color: #1a1a1a !important;
          font-weight: 600;
        }
        .compare-status-bar .progress {
          width: 11rem;
          max-width: 30%;
          height: 1.15rem;
          margin: 0;
          flex: 0 0 auto;
        }
        .compare-progress-card .progress-bar {
          font-size: 0.75rem;
          font-weight: 700;
          color: #fff !important;
          background-color: #2c7fb8 !important;
        }
        @media (max-width: 576px) {
          .compare-progress-card {
            height: auto;
            min-height: 3.4rem;
            flex-wrap: wrap;
            gap: 0.35rem 0.75rem;
            padding: 0.55rem 0.85rem;
          }
          .compare-status-bar .progress { width: 100%; max-width: none; }
        }
        /* Saturated run-button states (flatly warning is washed out). */
        .two-set-study .compare-run-btn.btn-warning,
        .two-set-study .compare-run-btn.btn-warning:disabled {
          background-color: #e67e00 !important;
          border-color: #c96e00 !important;
          color: #ffffff !important;
          font-weight: 700;
          opacity: 1 !important;
        }
        .two-set-study .compare-run-btn.btn-success {
          background-color: #198754 !important;
          border-color: #146c43 !important;
          color: #ffffff !important;
          font-weight: 700;
        }
        .two-set-study.compare-is-running .compare-idle-alert { display: none !important; }
      ")),
      tags$script(HTML("
        (function() {
          function hideAccordion(root) {
            if (!root) return;
            var panels = root.querySelectorAll(':scope > .accordion-item > .accordion-collapse.show');
            Array.prototype.forEach.call(panels, function(panel) {
              var btn = panel.parentElement.querySelector(':scope > .accordion-header .accordion-button');
              if (window.bootstrap && bootstrap.Collapse) {
                try {
                  bootstrap.Collapse.getOrCreateInstance(panel, {toggle: false}).hide();
                  return;
                } catch (e) {}
              }
              panel.classList.remove('show');
              if (btn) {
                btn.classList.add('collapsed');
                btn.setAttribute('aria-expanded', 'false');
              }
            });
          }
          function register() {
            if (window.__twoSetCollapseEditors) return;
            if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
            window.__twoSetCollapseEditors = true;
            Shiny.addCustomMessageHandler('two-set-compare-running', function(message) {
              var root = document.querySelector('.two-set-study');
              if (!root) return;
              root.classList.toggle('compare-is-running', !!(message && message.running));
            });
            Shiny.addCustomMessageHandler('two-set-collapse-editors', function(message) {
              var ids = message.ids || [];
              ids.forEach(function(id) { hideAccordion(document.getElementById(id)); });
              if (!ids.length) {
                document.querySelectorAll('.two-set-study .accordion.bslib-accordion-input').forEach(hideAccordion);
              }
              var el = document.getElementById(message.scrollId);
              if (!el) return;
              window.setTimeout(function() {
                el.scrollIntoView({behavior: 'smooth', block: 'start'});
              }, 450);
            });
          }
          document.addEventListener('shiny:connected', register);
          if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', register);
          else register();
          var tries = 0;
          var timer = window.setInterval(function() {
            register();
            if (window.__twoSetCollapseEditors || ++tries > 50) window.clearInterval(timer);
          }, 100);
          $(document).on('click', '.two-set-study .compare-run-btn', function() {
            var root = document.querySelector('.two-set-study');
            if (root) root.classList.add('compare-is-running');
            var card = document.querySelector('.two-set-study .compare-progress-card');
            if (card) {
              card.classList.remove('d-none', 'compare-progress-idle', 'compare-progress-done');
              card.style.borderColor = '#2c7fb8';
              card.style.background = '#eef6fb';
              card.style.boxShadow = '0 4px 12px rgba(0,0,0,.10)';
              var title = card.querySelector('.compare-progress-title');
              var detail = card.querySelector('.compare-progress-detail');
              var barWrap = card.querySelector('.progress');
              if (!barWrap) {
                barWrap = document.createElement('div');
                barWrap.className = 'progress';
                var bar = document.createElement('div');
                bar.className = 'progress-bar progress-bar-striped progress-bar-animated bg-primary';
                bar.setAttribute('role', 'progressbar');
                bar.style.width = '100%';
                bar.textContent = 'In progress';
                barWrap.appendChild(bar);
                card.appendChild(barWrap);
              } else {
                barWrap.classList.remove('d-none');
                var bar = card.querySelector('.progress-bar');
                if (bar) {
                  bar.className = 'progress-bar progress-bar-striped progress-bar-animated bg-primary';
                  bar.style.width = '100%';
                  bar.textContent = 'In progress';
                }
              }
              if (title) {
                title.style.color = '#1b4f72';
                title.textContent = '';
                var ic = document.createElement('i');
                ic.className = 'fa fa-spinner fa-spin';
                ic.setAttribute('aria-hidden', 'true');
                var sp = document.createElement('span');
                sp.textContent = 'Comparison running';
                title.appendChild(ic);
                title.appendChild(sp);
              }
              if (detail) {
                detail.textContent = 'Running both parameter sets…';
                detail.style.color = '#1a1a1a';
                detail.style.fontWeight = '600';
              }
            }
            document.querySelectorAll('.two-set-study .compare-idle-alert, .two-set-study .alert-info').forEach(function(el) {
              if ((el.textContent || '').indexOf('ready to edit') >= 0) el.classList.add('d-none');
            });
            var btn = this;
            btn.classList.remove('btn-primary', 'btn-success');
            btn.classList.add('btn-warning');
            btn.disabled = true;
            btn.style.backgroundColor = '#e67e00';
            btn.style.borderColor = '#c96e00';
            btn.style.color = '#ffffff';
            btn.style.fontWeight = '700';
            // Replace any existing icon with a spinning spinner for instant feedback.
            var oldIcon = btn.querySelector('i, .fa, .fas, svg');
            if (oldIcon) oldIcon.remove();
            var spin = document.createElement('i');
            spin.className = 'fa fa-spinner fa-spin';
            spin.setAttribute('aria-hidden', 'true');
            spin.style.marginRight = '0.35rem';
            btn.insertBefore(spin, btn.firstChild);
            var label = btn.querySelector('.compare-run-label');
            if (label) label.textContent = 'Running…';
          });
        })();
      ")),
      h3("Compare two parameter sets"),
      p("Start from the main page, change one parameter at a time, and compare output and flow. The two sets stay independent of the main page after import. Each title is editable and is used on the charts, tables, and downloads."),
      p("Results are illustrative only. They are not predictions for any employer."),
      uiOutput(ns("compare_progress")),
      div(
        class = "d-flex flex-wrap gap-2 mb-3 align-items-center",
        actionButton(ns("import"), "Import main settings into both sets"),
        actionButton(ns("clone"), "Copy Set 1 to Set 2"),
        actionButton(ns("match_reps"), "Match Set 2 replications to Set 1"),
        uiOutput(ns("run_button"))
      ),
      uiOutput(ns("status")),
      uiOutput(ns("result_fade")),
      uiOutput(ns("draft_diff")),
      layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        card(
          fill = FALSE,
          card_header(
            tags$div(
              class = "comparison-set-title",
              style = "color:#2474A5",
              textInput(ns("title_one"), "Title", value = comparison_default_labels[[1]], width = "100%")
            )
          ),
          parameter_editor_ui(ns("one"))
        ),
        card(
          fill = FALSE,
          card_header(
            tags$div(
              class = "comparison-set-title",
              style = "color:#D46B25",
              textInput(ns("title_two"), "Title", value = comparison_default_labels[[2]], width = "100%")
            )
          ),
          parameter_editor_ui(ns("two"))
        )
      ),
      accordion(
        open = FALSE,
        accordion_panel("Parameter differences", DTOutput(ns("differences")))
      ),
      p(
        class = "text-muted small",
        "Each run freezes both parameter sets. Reused seeds do not synchronize random draws by event. Differences are descriptive. Intervals describe each set's mean, not the uncertainty of the difference."
      ),
      div(
        id = ns("saved_results"),
        uiOutput(ns("change_story")),
        uiOutput(ns("highlights")),
        uiOutput(ns("kpis")),
        uiOutput(ns("captured_settings")),
        navset_card_tab(
          id = ns("compare_tabs"),
          nav_panel(
            "Output distributions",
            div(
              class = "d-flex justify-content-end mb-2",
              plot_options_menu(
                selectInput(
                  ns("metric"), "Metric",
                  c(
                    "Throughput (vehicles/hour)" = "throughput_per_hr",
                    "Lead time (min)" = "cycle_time_mean",
                    "Average WIP (vehicles)" = "avg_wip",
                    "HPU (labor-hours / unit)" = "hpu"
                  )
                )
              )
            ),
            roomy_plot(ns("distribution"))
          ),
          nav_panel("Production schedule",p("Shifts retain WIP. An order backlog outside production is not included in shop-floor WIP."),
            plotOutput(ns("shift_output"),height=400),plotOutput(ns("backlog"),height=400)),
          nav_panel("Lead-time curves", roomy_plot(ns("cycles"))),
          nav_panel(
            "Bay occupancy",
            p("One panel per station. Blue is the first set and orange is the second. A vertical line means that set did not vary."),
            uiOutput(ns("occupancy_frame"))
          ),
          nav_panel(
            "Operators",
            p("One panel per pool. Rug ticks are every replication. A vertical line means that set did not vary."),
            uiOutput(ns("labor_frame"))
          ),
          nav_panel(
            "WIP & queues",
            roomy_plot(ns("wip")),
            roomy_plot(ns("queues"))
          ),
          nav_panel(
            "Repair time",
            p("Active repair fraction only. Technician waiting is not included in this existing engine metric."),
            uiOutput(ns("downtime_frame"))
          ),
          nav_panel(
            "Tables & download",
            DTOutput(ns("summary")),
            h5("Station metrics"),
            DTOutput(ns("stations")),
            comparison_download_catalog(ns)
          )
        )
      )
    )
  )
}

comparison_page_server <- function(id, main_params, active_page) {
  moduleServer(id, function(input, output, session) {
    one <- parameter_editor_server("one")
    two <- parameter_editor_server("two")
    initialized <- reactiveVal(FALSE)
    results <- reactiveVal(NULL)
    error <- reactiveVal(NULL)
    # Titles are display state. Editing one does not rerun the simulation
    # and does not mark a saved comparison stale.
    titles <- reactiveVal(comparison_default_labels)
    # idle | running | done — drives the compare status banner and run button.
    run_state <- reactiveVal("idle")
    observeEvent(input$title_one, {
      updated <- sanitize_set_labels(c(input$title_one, isolate(titles())[[2]]))
      if (!identical(updated, isolate(titles()))) titles(updated)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(input$title_two, {
      updated <- sanitize_set_labels(c(isolate(titles())[[1]], input$title_two))
      if (!identical(updated, isolate(titles()))) titles(updated)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    palette <- reactive(comparison_palette(titles()))
    present_sets <- function(df, labels = titles()) {
      if (is.null(df) || !nrow(df) || !"set" %in% names(df)) return(df)
      mapped <- stats::setNames(as.character(labels), names(comparison_colors))
      df %>% mutate(set = unname(mapped[as.character(.data$set)]))
    }
    import_main <- function() {
      p <- isolate(main_params()) %>% normalize_params() %>% validate_params()
      one$set_params(p)
      two$set_params(p)
      initialized(TRUE)
      error(NULL)
    }
    observeEvent(active_page(), {
      if (identical(active_page(), "compare_sets") && !initialized()) import_main()
    }, ignoreNULL = TRUE)
    observeEvent(input$import, import_main())
    observeEvent(input$clone, {
      two$set_params(one$params())
      error(NULL)
    })
    observeEvent(input$match_reps, {
      p <- two$params()
      p$n_replications <- one$params()$n_replications
      two$set_params(p)
      error(NULL)
    })
    draft <- reactive({
      tryCatch(list(one = one$params(), two = two$params()), error = function(e) e)
    })
    dirty <- reactive({
      r <- results()
      d <- draft()
      !is.null(r) && (inherits(d, "error") || !comparison_same(d$one, r$params[[1]]) || !comparison_same(d$two, r$params[[2]]))
    })
    observe({
      # Inputs changed after a completed run: allow another run without looking "done".
      if (isTRUE(dirty()) && identical(isolate(run_state()), "done")) run_state("idle")
    })
    output$run_button <- renderUI({
      state <- run_state()
      if (identical(state, "running")) {
        actionButton(
          session$ns("run"),
          tagList(icon("spinner", class = "fa-spin"), span(class = "compare-run-label", "Running…")),
          class = "btn-warning compare-run-btn",
          disabled = TRUE
        )
      } else if (identical(state, "done") && !isTRUE(dirty())) {
        actionButton(
          session$ns("run"),
          tagList(icon("check"), span(class = "compare-run-label", "Done – run again")),
          class = "btn-success compare-run-btn"
        )
      } else {
        actionButton(
          session$ns("run"),
          tagList(icon("play"), span(class = "compare-run-label", "Run comparison")),
          class = "btn-primary compare-run-btn"
        )
      }
    })
    output$compare_progress <- renderUI({
      state <- run_state()
      # Clear the JS running class once Shiny can paint again.
      if (!identical(state, "running")) {
        session$sendCustomMessage(
          "two-set-compare-running",
          list(running = FALSE)
        )
      }
      card <- if (identical(state, "running")) {
        div(
          id = session$ns("compare_progress_card"),
          class = "compare-progress-card",
          div(
            class = "compare-progress-title",
            icon("spinner", class = "fa-spin"),
            span("Comparison running")
          ),
          div(class = "compare-progress-detail", "Running both parameter sets…"),
          div(
            class = "progress",
            div(
              class = "progress-bar progress-bar-striped progress-bar-animated bg-primary",
              role = "progressbar", style = "width:100%", "In progress"
            )
          )
        )
      } else if (identical(state, "done") && !isTRUE(dirty())) {
        div(
          id = session$ns("compare_progress_card"),
          class = "compare-progress-card compare-progress-done",
          div(
            class = "compare-progress-title",
            icon("check-circle"),
            span("Comparison complete")
          ),
          div(class = "compare-progress-detail", "Done. Both parameter sets finished — charts below are current.")
        )
      } else {
        div(
          id = session$ns("compare_progress_card"),
          class = "compare-progress-card compare-progress-idle",
          div(class = "compare-progress-title", "Ready"),
          div(class = "compare-progress-detail", "Press Run comparison when both sets are ready.")
        )
      }
      div(class = "compare-status-bar", card)
    })
    bind_runs <- function(r, field) {
      map2_dfr(r$runs, names(comparison_colors), ~ mutate(.x[[field]], set = .y))
    }
    run_both <- function() {
      d <- draft()
      if (inherits(d, "error")) {
        error(conditionMessage(d))
        run_state("idle")
        return()
      }
      error(NULL)
      run_state("running")
      session$sendCustomMessage("two-set-compare-running", list(running = TRUE))
      # Flush UI so the amber button and banner paint before the sim blocks.
      tryCatch(session$requestFlush(), error = function(cond) NULL)
      succeeded <- FALSE
      tryCatch({
        r <- withProgress(message = "Running both parameter sets…", value = 0, {
          a <- run_line_sim(d$one)
          incProgress(0.5, detail = paste(titles()[[1]], "complete"))
          b <- run_line_sim(d$two)
          incProgress(0.5, detail = paste(titles()[[2]], "complete"))
          list(
            params = list(a$params, b$params),
            runs = list(a, b),
            at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
          )
        })
        results(r)
        succeeded <- TRUE
      }, error = function(e) error(conditionMessage(e)))
      # Invalid input returns before this, and a failed sim leaves succeeded FALSE,
      # so open editors stay open when the run does not update.
      if (isTRUE(succeeded)) {
        run_state("done")
        collapse_editors(scroll = TRUE)
      } else {
        run_state("idle")
      }
    }
    # Close both editors after a saved comparison. Scroll only when this page
    # is the one on screen, so a guided run started from Simulate does not
    # pull that page down to a hidden tab.
    collapse_editors <- function(scroll = TRUE) {
      one$collapse()
      two$collapse()
      if (!isTRUE(scroll)) return(invisible())
      session$sendCustomMessage(
        "two-set-collapse-editors",
        list(
          ids = c(session$ns("one-editor_panels"), session$ns("two-editor_panels")),
          scrollId = session$ns("saved_results")
        )
      )
    }
    load_pair <- function(run_one, run_two, labels = comparison_guided_labels) {
      labels <- sanitize_set_labels(labels)
      one$set_params(run_one$params)
      two$set_params(run_two$params)
      titles(labels)
      updateTextInput(session, "title_one", value = labels[[1]])
      updateTextInput(session, "title_two", value = labels[[2]])
      error(NULL)
      initialized(TRUE)
      results(list(
        params = list(run_one$params, run_two$params),
        runs = list(run_one, run_two),
        at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      ))
      collapse_editors(scroll = identical(isolate(active_page()), "compare_sets"))
      invisible(results())
    }
    # Value 0 is the button's initial state, not a request to run.
    observeEvent(input$run, {
      if (is.null(input$run) || input$run < 1) return()
      run_both()
    }, ignoreNULL = TRUE)
    observeEvent(input$rerun, {
      if (is.null(input$rerun) || input$rerun < 1) return()
      run_both()
    }, ignoreNULL = TRUE)
    output$status <- renderUI({
      d <- draft()
      r <- results()
      if (!is.null(error())) {
        return(div(class = "alert alert-danger", paste("Comparison not updated:", error())))
      }
      if (inherits(d, "error")) {
        return(div(class = "alert alert-warning", paste("Check the parameter inputs:", conditionMessage(d))))
      }
      design <- c("shift_length_min", "n_shifts", "warmup_min", "n_replications", "planned_rate")
      d$one$planned_rate<-60/d$one$arrival_mean_min;d$two$planned_rate<-60/d$two$arrival_mean_min
      mismatches <- design[map_lgl(design, ~ !identical(as.numeric(d$one[[.x]]), as.numeric(d$two[[.x]])))]
      tagList(
        if (dirty()) {
          div(
            class = "alert alert-warning provenance-warning d-flex flex-wrap align-items-center justify-content-between gap-2",
            role = "alert",
            div(
              class = "d-flex align-items-center gap-2",
              bs_icon("exclamation-triangle-fill"),
              span("Settings changed since this comparison was run. Rerun comparison. Charts below still show the previous saved run.")
            ),
            actionButton(session$ns("rerun"), "Rerun comparison", class = "btn-warning btn-sm")
          )
        },
        if (is.null(r) && !identical(run_state(), "running")) {
          div(
            class = "alert alert-info compare-idle-alert",
            "Both sets are ready to edit. Press Run comparison to generate results."
          )
        },
        if (length(mismatches)) {
          div(
            class = "alert alert-info",
            paste(
              "Different run conditions:",
              paste(unname(comparison_globals[mismatches]), collapse = ", "),
              ". Interpret differences in that context."
            )
          )
        }
      )
    })
    output$result_fade <- renderUI({
      opacity <- if (isTRUE(dirty())) "0.45" else "1"
      tags$style(HTML(sprintf(
        "#%s{opacity:%s;transition:opacity 0.15s ease;}",
        session$ns("saved_results"), opacity
      )))
    })
    output$draft_diff <- renderUI({
      d <- draft()
      if (inherits(d, "error")) {
        return(div(
          class = "alert alert-light border py-2 mb-3 comparison-draft-diff",
          "Fix the parameter inputs to compare settings."
        ))
      }
      comparison_diff_table(d$one, d$two, titles())
    })
    output$differences <- renderDT({
      d <- draft()
      req(!inherits(d, "error"))
      datatable(comparison_diff(d$one, d$two, titles()), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    table_for <- function(field) {
      r <- results()
      req(!is.null(r))
      present_sets(bind_runs(r, field))
    }
    highlight_state <- reactive({
      r <- results()
      req(!is.null(r))
      comparison_rank_changes(r$runs, n = comparison_highlight_n)
    })
    output$change_story <- renderUI({
      r <- results()
      req(!is.null(r))
      div(
        class = "comparison-change-story mb-3",
        h4("What changed most"),
        p(class = "mb-2", comparison_change_caption(r$runs, highlight_state()$scores)),
        p(class = "mb-0", "Results are illustrative only. They are not predictions for any employer.")
      )
    })
    output$highlights <- renderUI({
      state <- highlight_state()
      shown <- state$shown
      if (!nrow(shown)) {
        return(p("These two sets match on throughput, lead time, WIP, bay occupancy, and queues."))
      }
      n <- nrow(shown)
      intro <- if (n == 1L) {
        "The metric below moved the most between the two sets."
      } else {
        "The metrics below moved the most between the two sets."
      }
      plots <- lapply(seq_len(n), function(i) {
        tagList(
          comparison_highlight_frame(session$ns(paste0("highlight_plot_", i))),
          downloadButton(
            session$ns(paste0("dl_highlight_", i)),
            "Download this chart's data",
            class = "btn-outline-secondary btn-sm mb-3"
          )
        )
      })
      tagList(
        p(class = "mb-1", intro),
        p(
          class = "mb-2",
          "Ranked by percent change from the first set. When that mean is smaller than the spread across replications, the change is measured against the second set's mean instead. Every replication is drawn."
        ),
        layout_columns(col_widths = if (n == 1L) 12 else 6, !!!plots)
      )
    })
    highlight_subtitle <- function(row) {
      comparison_highlight_subtitle(row$mean_1, row$mean_2, row$delta)
    }
    local({
      for (i in seq_len(comparison_highlight_n)) {
        local({
          ii <- i
          output[[paste0("highlight_plot_", ii)]] <- renderPlot({
            state <- highlight_state()
            req(nrow(state$shown) >= ii)
            row <- state$shown[ii, ]
            df <- state$frame %>%
              filter(.data$metric_id == row$metric_id) %>%
              mutate(set = titles()[.data$set_index])
            comparison_highlight_plot(
              df,
              x_label = row$x_label,
              subtitle = highlight_subtitle(row),
              palette = palette(),
              percent = isTRUE(row$percent)
            ) +
              labs(title = row$label)
          })
          output[[paste0("dl_highlight_", ii)]] <- downloadHandler(
            filename = function() sprintf("comparison_largest_change_%d.csv", ii),
            content = function(file) {
              r <- isolate(results())
              if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
              state <- comparison_rank_changes(r$runs, n = comparison_highlight_n)
              if (ii > nrow(state$shown)) stop("That chart is not in this comparison.", call. = FALSE)
              readr::write_csv(
                comparison_highlight_export(state, isolate(titles()), state$shown$metric_id[[ii]]),
                file
              )
            }
          )
        })
      }
    })
    output$kpis <- renderUI({
      r <- results()
      req(!is.null(r))
      metrics <- c(
        throughput_per_hr = "Throughput / hour",
        cycle_time_mean = "Mean lead time / min",
        avg_wip = "Average WIP",
        hpu = "HPU (labor-hours / unit)"
      )
      layout_columns(
        fill = FALSE,
        col_widths = c(3, 3, 3, 3),
        !!!map(names(metrics), function(m) {
          values <- map_dbl(r$runs, ~ mean(.x$summaries[[m]], na.rm = TRUE))
          delta <- values[[2]] - values[[1]]
          show <- function(x) if (is.finite(x)) sprintf("%.2f", x) else "Unavailable"
          value_box(
            title = metrics[[m]],
            value = tags$div(
              style = "font-size:1.1rem",
              tags$div(style = "color:#2474A5", paste0(titles()[[1]], ": ", show(values[[1]]))),
              tags$div(style = "color:#D46B25", paste0(titles()[[2]], ": ", show(values[[2]])))
            ),
            p(paste0(titles()[[2]], " − ", titles()[[1]], ": ", show(delta))),
            p(if (is.finite(values[[1]]) && values[[1]] != 0 && is.finite(delta)) {
              sprintf("Change: %+.1f%%", 100 * delta / values[[1]])
            } else {
              "Percent change unavailable"
            })
          )
        })
      )
    })
    output$captured_settings <- renderUI({
      r <- results()
      req(!is.null(r))
      div(
        class = "small text-muted mb-3",
        p(class = "mb-1", tags$strong(paste("Results captured:", r$at))),
        p(class = "mb-1", tags$strong(titles()[[1]]), " · ", format_run_settings(r$params[[1]])),
        p(class = "mb-0", tags$strong(titles()[[2]]), " · ", format_run_settings(r$params[[2]]))
      )
    })
    ridge_frame <- function(output_name, plot_name, field, column, fallback_n) {
      output[[output_name]] <- renderUI({
        r <- results()
        n <- if (is.null(r)) fallback_n else comparison_ridge_n(r$runs[[1]][[field]][[column]])
        comparison_ridge_plot(session$ns(plot_name), n)
      })
    }
    ridge_frame(
      "occupancy_frame", "occupancy", "utilization_by_rep", "station",
      nrow(default_params()$stations)
    )
    ridge_frame(
      "labor_frame", "labor", "operator_by_rep", "resource",
      nrow(default_params()$operators)
    )
    ridge_frame(
      "downtime_frame", "downtime", "downtime_by_rep", "station",
      nrow(default_params()$stations)
    )
    output$distribution <- renderPlot({
      labels <- c(
        throughput_per_hr = "Throughput (vehicles/hour)",
        cycle_time_mean = "Mean lead time (min)",
        avg_wip = "Average WIP (vehicles)",
        hpu = "HPU (labor-hours / unit)"
      )
      metric <- input$metric %||% "throughput_per_hr"
      df <- table_for("summaries")
      validate(need(any(is.finite(df[[metric]])), "No completed observations for this metric."))
      caption_extra <- if (identical(metric, "hpu")) {
        " HPU = (operators × observation hours) / units completed. Lower is better."
      } else {
        ""
      }
      comparison_densities(
        df %>% mutate(value = .data[[metric]]),
        "set", "value", labels[[metric]],
        paste0(
          "Overlaid densities of every replication. Rug ticks are each run. A vertical line means that set did not vary.",
          caption_extra
        ),
        percent = FALSE, palette = palette(), bounds = c(0, Inf)
      )
    })
    output$occupancy <- renderPlot({
      df <- table_for("utilization_by_rep") %>%
        mutate(label = factor(label_station(station), levels = unname(station_labels)))
      comparison_densities(
        df, "label", "avg_util",
        x_label = "Bay occupancy",
        subtitle = "One panel per station. Rug ticks are every replication. A vertical line means that set did not vary.",
        percent = TRUE,
        palette = palette()
      )
    })
    output$labor <- renderPlot({
      df <- table_for("operator_by_rep")
      req(nrow(df) > 0)
      pools <- sub("^operator_", "", df$resource)
      levels <- if (all(pools %in% c("upstream", "downstream"))) {
        c("upstream", "downstream")
      } else if (all(pools %in% names(station_labels))) {
        names(station_labels)[names(station_labels) %in% unique(pools)]
      } else {
        unique(pools)
      }
      df <- df %>% mutate(label = factor(sub("^operator_", "", resource), levels = levels))
      comparison_densities(
        df, "label", "avg_util",
        x_label = "Operator utilization",
        subtitle = "One panel per pool. Rug ticks are every replication. A vertical line means that set did not vary.",
        percent = TRUE,
        palette = palette()
      )
    })
    output$downtime <- renderPlot({
      df <- table_for("downtime_by_rep") %>%
        mutate(label = factor(label_station(station), levels = unname(station_labels)))
      comparison_densities(
        df, "label", "downtime_fraction",
        x_label = "Active repair fraction",
        subtitle = "One panel per station. Rug ticks are every replication. A vertical line means that set did not vary.",
        percent = TRUE,
        palette = palette()
      )
    })
    output$cycles <- renderPlot({
      df <- table_for("cycle_times") %>%
        filter(is.finite(cycle_time)) %>%
        mutate(set = factor(set, levels = names(palette())))
      validate(need(nrow(df) > 0, "No completed vehicles in either set."))
      counts <- df %>% count(set)
      xlim <- density_xlim(df$cycle_time, level = 0.95, bounds = c(0, Inf))
      p <- ggplot(df, aes(x = cycle_time, fill = set, color = set))
      if (all(counts$n >= 2)) {
        p <- p + geom_density(alpha = 0.18, linewidth = 1, na.rm = TRUE)
      } else {
        p <- p + geom_histogram(position = "identity", alpha = 0.3, bins = 20)
      }
      p +
        scale_fill_manual(values = palette(), drop = FALSE, name = NULL) +
        scale_color_manual(values = palette(), drop = FALSE, name = NULL) +
        sparse_x_continuous(percent = FALSE, n = 4L) +
        coord_cartesian(xlim = xlim, clip = "off") +
        labs(
          x = "Vehicle lead time (min)",
          y = "Density",
          subtitle = "Pooled departures from every replication. Unfinished vehicles are excluded.",
          caption = "x-axis shows the central 95% of values"
        ) +
        theme_sim(legend.position = "bottom") +
        theme_plot_width_text() +
        theme_minor_ticks()
    })
    output$shift_output <- renderPlot({
      r <- results()
      req(!is.null(r))
      caps <- tibble::tibble(
        set = titles(),
        capacity = vapply(r$runs, function(run) theoretical_shift_capacity(run$params), numeric(1))
      )
      shift_output_plot(table_for("shift_summaries"), caps, palette())
    })
    output$backlog <- renderPlot({comparison_flow(table_for("release_timeseries"),"unreleased_backlog","Orders waiting for admission",palette=palette())})
    output$wip <- renderPlot({
      comparison_flow(table_for("wip_timeseries"), "wip", "WIP (vehicles)", palette = palette())
    })
    output$queues <- renderPlot({
      df <- table_for("queue_timeseries") %>%
        mutate(label = factor(label_station(resource), levels = unname(station_labels)))
      comparison_flow(df, "queue", "Queue (vehicles)", facet = "label", palette = palette())
    })
    summary_data <- reactive({
      table_for("summaries") %>%
        group_by(set) %>%
        summarise(
          replications = n(),
          throughput = mean(throughput_per_hr),
          throughput_se = sd(throughput_per_hr) / sqrt(n()),
          ci_half = if (n() > 1) qt(0.975, n() - 1) * throughput_se else NA_real_,
          throughput_lower = throughput - ci_half,
          throughput_upper = throughput + ci_half,
          mean_lead_time = mean(cycle_time_mean, na.rm = TRUE),
          mean_wip = mean(avg_wip),
          hpu = mean(hpu[is.finite(hpu)], na.rm = TRUE),
          .groups = "drop"
        )
    })
    output$summary <- renderDT({
      df <- summary_data()
      names(df) <- c(
        "Parameter set", "Replications", "Throughput / hour", "Standard error",
        "95% CI half-width", "95% CI lower", "95% CI upper", "Mean lead time / min",
        "Average WIP", "HPU (labor-hours / unit)"
      )
      datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
        formatRound(columns = 3:10, digits = 2)
    })
    output$stations <- renderDT({
      df <- table_for("station_metrics") %>%
        mutate(station = label_station(station)) %>%
        rename(
          Station = station,
          `Parameter set` = set,
          `Mean occupancy` = avg_util_mean,
          `Occupancy SD` = avg_util_sd,
          `Mean queue` = avg_queue_mean,
          `Queue SD` = avg_queue_sd,
          `Mean active repair fraction` = downtime_fraction_mean,
          `Repair fraction SD` = downtime_fraction_sd,
          `Mean repairs` = repairs_per_rep_mean
        )
      datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
        formatRound(columns = names(df)[vapply(df, is.numeric, logical(1))], digits = 3)
    })
    write_comparison_csv <- function(id, filename, field) {
      output[[id]] <- downloadHandler(
        filename = function() filename,
        content = function(file) {
          r <- isolate(results())
          if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
          readr::write_csv(present_sets(bind_runs(r, field), isolate(titles())), file)
        }
      )
    }
    write_comparison_csv("dl_shifts","comparison_shift_summaries.csv","shift_summaries")
    write_comparison_csv("dl_orders","comparison_order_ledger.csv","orders")
    write_comparison_csv("dl_summaries", "comparison_summaries.csv", "summaries")
    write_comparison_csv("dl_cycle", "comparison_cycle_times.csv", "cycle_times")
    write_comparison_csv("dl_util", "comparison_utilization_by_rep.csv", "utilization_by_rep")
    write_comparison_csv("dl_ops", "comparison_operator_by_rep.csv", "operator_by_rep")
    write_comparison_csv("dl_wip", "comparison_wip_timeseries.csv", "wip_timeseries")
    write_comparison_csv("dl_queue", "comparison_queue_timeseries.csv", "queue_timeseries")
    write_comparison_csv("dl_down", "comparison_downtime_by_rep.csv", "downtime_by_rep")
    write_comparison_csv("dl_repairs", "comparison_repair_events.csv", "repair_events")
    write_comparison_csv("dl_metrics", "comparison_station_metrics.csv", "station_metrics")
    output$dl_highlights <- downloadHandler(
      filename = function() "comparison_largest_changes.csv",
      content = function(file) {
        r <- isolate(results())
        if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
        state <- comparison_rank_changes(r$runs, n = comparison_highlight_n)
        readr::write_csv(comparison_highlight_export(state, isolate(titles())), file)
      }
    )
    output$download <- downloadHandler(
      filename = function() "two_parameter_sets.rds",
      content = function(file) {
        r <- isolate(results())
        if (is.null(r)) stop("Run the comparison before downloading.", call. = FALSE)
        saveRDS(r, file)
      }
    )
    list(
      one = one,
      two = two,
      results = results,
      draft = draft,
      dirty = dirty,
      summary = summary_data,
      titles = titles,
      load_pair = load_pair
    )
  })
}
