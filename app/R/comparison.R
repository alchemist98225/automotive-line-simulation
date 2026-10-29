# Two editable parameter snapshots; no changes to the simulation engine.
comparison_colors <- c("Set 1"="#2474A5", "Set 2"="#D46B25")
comparison_activities <- c(load="Loading",auto="Automatic processing",unload="Unloading",task="Support work")
comparison_globals <- c(sim_length_min="Run length (min)",warmup_min="Warmup (min)",
  n_replications="Replications",base_seed="Base seed",arrival_mean_min="Mean interarrival (min)",
  grid_dt_min="Time-series interval (min)",repair_technicians="Repair technicians")
comparison_pools <- function(p) unique(c(p$operators$pool,"upstream","downstream","shared",p$stations$name))
# Lossless representations are also used to detect stale results, including empirical vectors.
comparison_flatten <- function(p) {
  p <- normalize_params(p)
  scalar <- function(x) paste(format(x,digits=17,trim=TRUE),collapse=", ")
  timing <- function(x) switch(x$method,
    fixed=paste("fixed",scalar(x$mean_min)),
    empirical=paste("empirical",scalar(x$observations)),
    distribution=paste(x$distribution,scalar(x$mean_min),if(x$distribution!="exponential") scalar(x$sd_min) else ""))
  run <- tibble(parameter=paste0("Run / ",unname(comparison_globals)),value=map_chr(names(comparison_globals),~scalar(p[[.x]] %||% 2)))
  ops <- p$operators %>% transmute(parameter=paste0("Operators / ",pool),value=map_chr(count,scalar))
  stations <- map_dfr(seq_len(nrow(p$stations)),function(i) {
    st <- p$stations[i,]
    cols <- c("capacity","operator_pool","mtbf_min","mttr_min","task_interval_min","task_every_n")
    bind_rows(tibble(parameter=paste(label_station(st$name),c("Parallel bays","Operator pool","Mean up-time (min)","Mean repair (min)","Task interval (min)","Task every N units"),sep=" / "),value=map_chr(cols,~scalar(st[[.x]]))),
      tibble(parameter=paste(label_station(st$name),unname(comparison_activities),sep=" / "),
        value=map_chr(names(comparison_activities),~timing(activity_timing(st,.x)))))
  })
  bind_rows(run,ops,stations) %>% arrange(parameter)
}
comparison_same <- function(a,b) identical(comparison_flatten(a),comparison_flatten(b))
comparison_diff <- function(a,b) {
  full_join(comparison_flatten(a) %>% rename(`Set 1`=value),
    comparison_flatten(b) %>% rename(`Set 2`=value),by="parameter") %>%
    mutate(across(c(`Set 1`,`Set 2`),~replace_na(.x,"Not used"))) %>%
    filter(`Set 1`!=`Set 2`)
}

parameter_editor_ui <- function(id) uiOutput(NS(id,"controls"))
parameter_editor_server <- function(id) moduleServer(id,function(input,output,session) {
  state <- reactiveVal(list(revision=0L,params=normalize_params(default_params())))
  set_params <- function(p) {
    p <- normalize_params(p) %>% validate_params()
    state(list(revision=isolate(state()$revision)+1L,params=p))
  }
  output$controls <- renderUI({
    p <- state()$params; revision <- state()$revision
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
          min=if(n %in% c("warmup_min","base_seed")) 0 else if(n %in% c("sim_length_min","arrival_mean_min","grid_dt_min")) .001 else 1,
          step=if(n %in% c("arrival_mean_min","grid_dt_min")) .1 else 1)
      }))),
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
    p <- state()$params; revision <- state()$revision
    val <- function(id,fallback) input[[paste0("v",revision,"_",id)]] %||% fallback
    pools <- comparison_pools(p)
    walk(names(comparison_globals),function(n) p[[n]] <<- val(n,p[[n]] %||% 2))
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

# Horizontal raincloud for replication-level summaries. A set with no spread,
# or fewer than three replications, does not become a strip of points: a flat
# set is a line at its median, and a short set with spread is a box.
comparison_raincloud <- function(df, value, label) {
  df <- df %>%
    mutate(
      value = .data[[value]],
      set = factor(set, levels = names(comparison_colors))
    ) %>%
    filter(is.finite(value))
  if (!nrow(df)) {
    return(ggplot() + theme_void() + labs(subtitle = "No completed observations for this metric."))
  }
  spread <- df %>%
    group_by(set) %>%
    summarise(
      n = n(),
      s = stats::sd(value),
      marker = stats::median(value),
      .groups = "drop"
    ) %>%
    mutate(
      flat = n < 2L | !is.finite(s) | s <= sqrt(.Machine$double.eps),
      rain = !flat & n >= 3L
    )
  rain_df <- df %>% semi_join(filter(spread, rain), by = "set")
  box_df <- df %>% semi_join(filter(spread, !rain & !flat), by = "set")
  flat_df <- spread %>% filter(flat)
  means <- df %>%
    group_by(set) %>%
    summarise(value = mean(value), .groups = "drop")
  rng <- range(df$value)
  pad <- if (diff(rng) == 0) max(abs(rng[[1]]) * 0.05, 0.05) else 0
  # geom_rain draws every row of the plot data, including a flat set, so the
  # raincloud layer only sees sets that have enough spread.
  p <- ggplot(if (nrow(rain_df)) rain_df else df, aes(x = set, y = value, fill = set, color = set))
  if (nrow(rain_df)) {
    p <- p + ggrain::geom_rain(
      rain.side = "r",
      seed = 42,
      point.args = list(alpha = 0.35, size = 1.8, color = "#4a4a4a"),
      point.args.pos = list(position = position_jitter(width = 0.06, height = 0, seed = 42)),
      boxplot.args = list(outlier.shape = NA, linewidth = 0.4),
      violin.args = list(alpha = 0.55, color = NA)
    )
  }
  if (nrow(box_df)) {
    p <- p + geom_boxplot(
      data = box_df,
      width = 0.35,
      outlier.shape = NA,
      linewidth = 0.4
    )
  }
  if (nrow(flat_df)) {
    p <- p + geom_crossbar(
      data = flat_df,
      aes(x = set, y = marker, ymin = marker, ymax = marker),
      width = 0.55,
      linewidth = 0.8,
      middle.linewidth = 0
    )
  }
  p +
    geom_point(
      data = means,
      aes(x = set, y = value),
      inherit.aes = FALSE,
      shape = 18,
      size = 3.8,
      color = "#08306b"
    ) +
    scale_fill_manual(values = comparison_colors, drop = FALSE, name = NULL) +
    scale_color_manual(values = comparison_colors, drop = FALSE, name = NULL) +
    scale_x_discrete(limits = rev, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = 0.05, add = pad)) +
    coord_flip(clip = "off") +
    labs(
      x = NULL, y = label, fill = NULL, color = NULL,
      subtitle = "Half-violin and box across replications; faint points = each rep; diamond = mean"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", plot.margin = margin(8, 16, 8, 8))
}

# One ridge per station or pool, both sets overlaid. A set with no spread is a
# rug tick at that value, matching the Simulate page ridgelines.
comparison_ridges <- function(df, y, x, x_label, subtitle, percent = TRUE) {
  df <- df %>%
    filter(is.finite(.data[[x]])) %>%
    mutate(
      set = factor(set, levels = names(comparison_colors)),
      ridge_label = .data[[y]]
    )
  if (!nrow(df)) {
    return(ggplot() + theme_void() + labs(subtitle = "No replications to plot"))
  }
  if (is.factor(df$ridge_label)) {
    df$ridge <- factor(df$ridge_label, levels = levels(df$ridge_label))
  } else {
    df$ridge <- factor(df$ridge_label, levels = unique(df$ridge_label))
  }
  df <- df %>% mutate(y_num = nlevels(ridge) - as.numeric(ridge) + 1)
  spread <- df %>%
    group_by(ridge, set) %>%
    summarise(
      marker = stats::median(.data[[x]], na.rm = TRUE),
      n = n(),
      s = stats::sd(.data[[x]], na.rm = TRUE),
      y_num = first(y_num),
      .groups = "drop"
    ) %>%
    mutate(constant = n < 2L | !is.finite(s) | s <= sqrt(.Machine$double.eps))
  varying <- df %>% semi_join(filter(spread, !constant), by = c("ridge", "set"))
  constant <- spread %>% filter(constant)
  p <- ggplot(df, aes(x = .data[[x]], y = y_num))
  if (nrow(varying)) {
    bw <- stats::bw.nrd0(varying[[x]])
    if (!is.finite(bw) || bw <= 0) bw <- stats::mad(varying[[x]], constant = 1)
    if (!is.finite(bw) || bw <= 0) bw <- diff(range(varying[[x]])) / 10
    if (!is.finite(bw) || bw <= 0) bw <- 1e-3
    p <- p + ggridges::geom_density_ridges(
      data = varying,
      # y is numeric so spikes can share the scale. Group by ridge and set,
      # or ggplot drops y when one set spans several ridges.
      aes(fill = set, group = interaction(ridge, set)),
      alpha = 0.55,
      color = "white",
      linewidth = 0.35,
      scale = 0.9,
      rel_min_height = 0.01,
      bandwidth = bw,
      quantile_lines = TRUE,
      quantiles = 0.5,
      na.rm = TRUE
    ) +
      # Rug ticks sit on each station or pool baseline. Color follows the set.
      geom_segment(
        data = varying,
        aes(x = .data[[x]], xend = .data[[x]], y = y_num, yend = y_num + 0.16, color = set),
        inherit.aes = FALSE,
        linewidth = 0.55,
        lineend = "butt"
      ) +
      scale_fill_manual(
        values = comparison_colors,
        limits = names(comparison_colors),
        name = NULL
      )
  }
  if (nrow(constant)) {
    p <- p + geom_segment(
      data = constant,
      aes(x = marker, xend = marker, y = y_num, yend = y_num + 0.42, color = set),
      inherit.aes = FALSE,
      linewidth = 1.05,
      lineend = "butt"
    )
  }
  if (nrow(varying) || nrow(constant)) {
    p <- p + scale_color_manual(
      values = comparison_colors,
      limits = names(comparison_colors),
      name = NULL,
      guide = if (nrow(varying)) "none" else "legend"
    )
  }
  lv <- levels(df$ridge)
  p +
    scale_y_continuous(
      breaks = nlevels(df$ridge) - seq_along(lv) + 1,
      labels = lv,
      # Extra room under the lowest ridge (Inspection) and above the highest density.
      expand = expansion(add = c(0.85, 1.2))
    ) +
    scale_x_continuous(
      labels = if (percent) scales::percent_format(accuracy = 1) else waiver(),
      expand = expansion(mult = 0.05)
    ) +
    labs(x = x_label, y = NULL, subtitle = subtitle) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", plot.margin = margin(10, 18, 16, 8))
}

# The Simulate page clamps .shiny-plot-output at 550px. That crowds four
# overlaid ridges and leaves the lowest station (Inspection) against the axis.
# Scale the comparison ridgelines with the number of stations or pools.
comparison_ridge_n <- function(x) {
  n <- length(unique(x[!is.na(x)]))
  if (n < 1L) 1L else as.integer(n)
}
comparison_ridge_height <- function(n_ridges) {
  n <- suppressWarnings(as.integer(n_ridges[[1]]))
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 1L
  as.integer(max(560L, 170L * n + 180L))
}
comparison_ridge_plot <- function(id, n_ridges) {
  h <- comparison_ridge_height(n_ridges)
  div(
    class = "comparison-ridge-plot",
    style = sprintf("--ridge-h:%dpx;", h),
    plotOutput(id, height = paste0(h, "px"), width = "100%", fill = FALSE)
  )
}

# Every replication is a faint line. The ribbon is a loess fit on all of them.
comparison_flow <- function(df, column, y_label, facet = NULL) {
  df <- df %>% mutate(set = factor(set, levels = names(comparison_colors)))
  if (!nrow(df)) {
    return(ggplot() + theme_void() + labs(subtitle = "No time-series observations."))
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
    scale_color_manual(values = comparison_colors, drop = FALSE, name = NULL) +
    scale_fill_manual(values = comparison_colors, drop = FALSE, name = NULL) +
    labs(
      x = "Simulation time (min)",
      y = y_label,
      subtitle = "Faint lines = each replication; bold = loess + ribbon. Each set uses its own window."
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  if (!is.null(facet)) p <- p + facet_wrap(vars(.data[[facet]]), ncol = 2)
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
      "Throughput, lead time, WIP, and output per operator-hour for every replication."
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
      # Beats the app-wide 550px plot clamp for these ridgelines only.
      tags$style(HTML("
        .comparison-ridge-plot > .shiny-plot-output {
          height: var(--ridge-h) !important;
          min-height: var(--ridge-h) !important;
          max-height: none !important;
        }
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
        })();
      ")),
      h3("Compare two parameter sets"),
      p("Start from the main page, change one parameter at a time, and compare output and flow. Set 1 and Set 2 stay independent of the main page after import."),
      p("Results are illustrative only. They are not predictions for any employer."),
      div(
        class = "d-flex flex-wrap gap-2 mb-3",
        actionButton(ns("import"), "Import main settings into both sets"),
        actionButton(ns("clone"), "Copy Set 1 to Set 2"),
        actionButton(ns("match_reps"), "Match Set 2 replications to Set 1"),
        actionButton(ns("run"), "Run comparison", class = "btn-primary")
      ),
      uiOutput(ns("status")),
      uiOutput(ns("result_fade")),
      layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        card(
          fill = FALSE,
          card_header(tags$span(style = "color:#2474A5", "Set 1 — baseline")),
          parameter_editor_ui(ns("one"))
        ),
        card(
          fill = FALSE,
          card_header(tags$span(style = "color:#D46B25", "Set 2 — study")),
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
        uiOutput(ns("kpis")),
        uiOutput(ns("captured_settings")),
        navset_card_tab(
          id = ns("compare_tabs"),
          nav_panel(
            "Output distributions",
            selectInput(
              ns("metric"), "Metric",
              c(
                "Throughput (vehicles/hour)" = "throughput_per_hr",
                "Lead time (min)" = "cycle_time_mean",
                "Average WIP (vehicles)" = "avg_wip",
                "Output per operator-hour" = "throughput_per_labor_hr"
              )
            ),
            roomy_plot(ns("distribution"))
          ),
          nav_panel("Lead-time curves", roomy_plot(ns("cycles"))),
          nav_panel(
            "Bay occupancy",
            p(class = "text-muted small", "One ridge per station. Blue is Set 1 and orange is Set 2. A spike means that set did not vary."),
            uiOutput(ns("occupancy_frame"))
          ),
          nav_panel(
            "Operators",
            p(class = "text-muted small", "One ridge per pool. Rug ticks are every replication."),
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
    bind_runs <- function(r, field) {
      map2_dfr(r$runs, names(comparison_colors), ~ mutate(.x[[field]], set = .y))
    }
    run_both <- function() {
      d <- draft()
      if (inherits(d, "error")) {
        error(conditionMessage(d))
        return()
      }
      error(NULL)
      succeeded <- FALSE
      tryCatch({
        r <- withProgress(message = "Running both parameter sets…", value = 0, {
          a <- run_line_sim(d$one)
          incProgress(0.5, detail = "Set 1 complete")
          b <- run_line_sim(d$two)
          incProgress(0.5, detail = "Set 2 complete")
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
        # bslib queues a close on the next flush; also close in the browser
        # immediately so a finished run does not leave the editors open.
        one$collapse()
        two$collapse()
        session$sendCustomMessage(
          "two-set-collapse-editors",
          list(
            ids = c(session$ns("one-editor_panels"), session$ns("two-editor_panels")),
            scrollId = session$ns("saved_results")
          )
        )
      }
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
      design <- c("sim_length_min", "warmup_min", "n_replications", "arrival_mean_min")
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
        if (is.null(r)) {
          div(class = "alert alert-info", "Both sets are ready to edit. Press Run comparison to generate results.")
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
    output$differences <- renderDT({
      d <- draft()
      req(!inherits(d, "error"))
      datatable(comparison_diff(d$one, d$two), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
    table_for <- function(field) {
      r <- results()
      req(!is.null(r))
      bind_runs(r, field)
    }
    output$kpis <- renderUI({
      r <- results()
      req(!is.null(r))
      metrics <- c(
        throughput_per_hr = "Throughput / hour",
        cycle_time_mean = "Mean lead time / min",
        avg_wip = "Average WIP",
        throughput_per_labor_hr = "Output / operator-hour"
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
              tags$div(style = "color:#2474A5", paste("Set 1:", show(values[[1]]))),
              tags$div(style = "color:#D46B25", paste("Set 2:", show(values[[2]])))
            ),
            p(paste("Set 2 − Set 1:", show(delta))),
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
        p(class = "mb-1", tags$strong("Set 1"), " · ", format_run_settings(r$params[[1]])),
        p(class = "mb-0", tags$strong("Set 2"), " · ", format_run_settings(r$params[[2]]))
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
        throughput_per_labor_hr = "Output per operator-hour"
      )
      metric <- input$metric %||% "throughput_per_hr"
      df <- table_for("summaries")
      validate(need(any(is.finite(df[[metric]])), "No completed observations for this metric."))
      comparison_raincloud(df, metric, labels[[metric]])
    })
    output$occupancy <- renderPlot({
      df <- table_for("utilization_by_rep") %>%
        mutate(label = factor(label_station(station), levels = unname(station_labels)))
      comparison_ridges(
        df, "label", "avg_util",
        x_label = "Bay occupancy",
        subtitle = "One ridge per station; both sets overlaid. Rug = each rep at the baseline; line = median. Spike = no spread.",
        percent = TRUE
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
      comparison_ridges(
        df, "label", "avg_util",
        x_label = "Operator utilization",
        subtitle = "One ridge per pool; both sets overlaid. Rug = each rep at the baseline; line = median. Spike = no spread.",
        percent = TRUE
      )
    })
    output$downtime <- renderPlot({
      df <- table_for("downtime_by_rep") %>%
        mutate(label = factor(label_station(station), levels = unname(station_labels)))
      comparison_ridges(
        df, "label", "downtime_fraction",
        x_label = "Active repair fraction",
        subtitle = "One ridge per station; both sets overlaid. Rug = each rep at the baseline; line = median. Spike = no spread.",
        percent = TRUE
      )
    })
    output$cycles <- renderPlot({
      df <- table_for("cycle_times") %>%
        filter(is.finite(cycle_time)) %>%
        mutate(set = factor(set, levels = names(comparison_colors)))
      validate(need(nrow(df) > 0, "No completed vehicles in either set."))
      counts <- df %>% count(set)
      p <- ggplot(df, aes(x = cycle_time, fill = set, color = set))
      if (all(counts$n >= 2)) {
        p <- p + geom_density(alpha = 0.18, linewidth = 1, na.rm = TRUE)
      } else {
        p <- p + geom_histogram(position = "identity", alpha = 0.3, bins = 20)
      }
      p +
        scale_fill_manual(values = comparison_colors, drop = FALSE, name = NULL) +
        scale_color_manual(values = comparison_colors, drop = FALSE, name = NULL) +
        labs(
          x = "Vehicle lead time (min)",
          y = "Density",
          subtitle = "Pooled departures from every replication. Unfinished vehicles are excluded."
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
    })
    output$wip <- renderPlot({
      comparison_flow(table_for("wip_timeseries"), "wip", "WIP (vehicles)")
    })
    output$queues <- renderPlot({
      df <- table_for("queue_timeseries") %>%
        mutate(label = factor(label_station(resource), levels = unname(station_labels)))
      comparison_flow(df, "queue", "Queue (vehicles)", facet = "label")
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
          output_per_operator_hour = mean(throughput_per_labor_hr, na.rm = TRUE),
          .groups = "drop"
        )
    })
    output$summary <- renderDT({
      df <- summary_data()
      names(df) <- c(
        "Parameter set", "Replications", "Throughput / hour", "Standard error",
        "95% CI half-width", "95% CI lower", "95% CI upper", "Mean lead time / min",
        "Average WIP", "Output / operator-hour"
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
          readr::write_csv(bind_runs(r, field), file)
        }
      )
    }
    write_comparison_csv("dl_summaries", "comparison_summaries.csv", "summaries")
    write_comparison_csv("dl_cycle", "comparison_cycle_times.csv", "cycle_times")
    write_comparison_csv("dl_util", "comparison_utilization_by_rep.csv", "utilization_by_rep")
    write_comparison_csv("dl_ops", "comparison_operator_by_rep.csv", "operator_by_rep")
    write_comparison_csv("dl_wip", "comparison_wip_timeseries.csv", "wip_timeseries")
    write_comparison_csv("dl_queue", "comparison_queue_timeseries.csv", "queue_timeseries")
    write_comparison_csv("dl_down", "comparison_downtime_by_rep.csv", "downtime_by_rep")
    write_comparison_csv("dl_repairs", "comparison_repair_events.csv", "repair_events")
    write_comparison_csv("dl_metrics", "comparison_station_metrics.csv", "station_metrics")
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
      summary = summary_data
    )
  })
}
