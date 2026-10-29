# Slowest station if each bay ran its load, automatic, and unload time with no
# waiting, failures, or support work. Vehicles per measured shift.
theoretical_shift_capacity <- function(params) {
  st <- params$stations
  cycle <- vapply(seq_len(nrow(st)), function(i) {
    timing_mean(activity_timing(st[i, ], "load")) +
      timing_mean(activity_timing(st[i, ], "auto")) +
      timing_mean(activity_timing(st[i, ], "unload"))
  }, numeric(1))
  cycle <- pmax(cycle, 1e-9)
  min(st$capacity * params$shift_length_min / cycle)
}
# Integer tick marks for a vehicle count. pretty() can return halves when the
# range is short; those are rounded onto a short integer sequence.
integer_shift_breaks <- function(lims) {
  if (length(lims) < 2L || any(!is.finite(lims))) return(numeric())
  lo <- floor(min(lims))
  hi <- ceiling(max(lims))
  if (hi < lo) return(numeric())
  span <- hi - lo
  if (span <= 6) return(seq(lo, hi))
  by <- max(1L, ceiling(span / 5))
  seq(lo, hi, by = by)
}
capacity_label <- function(n) sprintf("Theoretical capacity:\n%.0f vehicles", round(n))
# Freedman–Diaconis width, rounded to a whole number of vehicles. A point mass uses 1.
shift_binwidth <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(1L)
  iqr <- stats::IQR(x)
  bw <- if (!is.finite(iqr) || iqr <= 0) 1 else 2 * iqr / length(x)^(1 / 3)
  max(1L, as.integer(round(bw)))
}
# Bins centered on integers when the width is 1, so each vehicle count is one bar.
shift_hist_breaks <- function(x, bw) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(-0.5, 0.5))
  bw <- max(1L, as.integer(bw))
  seq(floor(min(x)) - bw / 2, ceiling(max(x)) + bw / 2, by = bw)
}
# Same fill, outline, and line as the cycle-time chart. Comparison uses the set palette.
shift_hist_fill <- "#41b6c4"
shift_line_color <- "#253494"
# Histogram of completed vehicles per shift, one row per replication, plus the
# pooled density. A dashed line is theoretical bay capacity. Several shifts are
# faceted. Two parameter sets are overlaid in the title-aware palette.
shift_output_plot <- function(df, capacity, palette = NULL) {
  df <- df %>% dplyr::filter(is.finite(n_completed), is.finite(shift))
  if (!nrow(df)) {
    return(ggplot() + theme_void(base_size = 16) + labs(subtitle = plot_subtitle("No measured shifts to plot.")))
  }
  df <- df %>% mutate(shift_label = factor(
    paste("Shift", shift),
    levels = paste("Shift", sort(unique(shift)))
  ))
  has_set <- !is.null(palette) && "set" %in% names(df)
  cap_df <- NULL
  if (is.data.frame(capacity)) {
    cap_df <- capacity %>% dplyr::filter(is.finite(.data$capacity))
    cap_value <- if (nrow(cap_df)) mean(cap_df$capacity) else NA_real_
  } else {
    cap_value <- if (length(capacity) == 1L && is.finite(capacity)) capacity else NA_real_
  }
  mean_out <- mean(df$n_completed)
  caption <- plot_subtitle(
    "Each rug tick is one replication. The dashed line is theoretical vehicles per shift at the slowest station, ignoring labor, failures, support work, and blocking.",
    32
  )
  bw <- shift_binwidth(df$n_completed)
  breaks <- shift_hist_breaks(c(df$n_completed, cap_value, if (!is.null(cap_df)) cap_df$capacity), bw)
  spread_ok <- function(x) {
    x <- x[is.finite(x)]
    length(x) >= 2L && is.finite(stats::sd(x)) && stats::sd(x) > sqrt(.Machine$double.eps)
  }
  if (has_set) {
    by_set <- df %>% group_by(set) %>% summarise(mean_out = mean(n_completed), .groups = "drop")
    hi <- by_set$set[[which.max(by_set$mean_out)]]
    lo <- by_set$set[[which.min(by_set$mean_out)]]
    title <- plot_subtitle(if (abs(diff(range(by_set$mean_out))) < 0.5) {
      "The two sets complete about the same number of vehicles per shift"
    } else {
      sprintf("%s completes more vehicles per shift than %s", hi, lo)
    }, 32)
    df <- df %>% mutate(set = factor(set, levels = names(palette)))
    varying <- df %>% group_by(set, shift_label) %>% filter(spread_ok(n_completed)) %>% ungroup()
    p <- ggplot(df, aes(x = n_completed, fill = set)) +
      geom_histogram(
        aes(y = after_stat(density)),
        breaks = breaks, position = "identity", alpha = 0.55, colour = "white"
      ) +
      geom_rug(
        aes(colour = set), alpha = 0.85, length = grid::unit(0.03, "npc"),
        show.legend = FALSE
      ) +
      scale_fill_manual(values = palette, drop = FALSE, name = NULL) +
      scale_colour_manual(values = palette, drop = FALSE, name = NULL)
    if (nrow(varying)) {
      p <- p + geom_density(
        data = varying, aes(colour = set), linewidth = 1.15, na.rm = TRUE, show.legend = FALSE
      )
    }
    split_caps <- !is.null(cap_df) && nrow(cap_df) > 1L && diff(range(cap_df$capacity)) >= 0.5
    if (isTRUE(split_caps)) {
      cap_df <- cap_df %>%
        dplyr::arrange(.data$capacity) %>%
        dplyr::mutate(
          hjust = dplyr::if_else(.data$capacity >= median(df$n_completed), 1.02, -0.02),
          vjust = 1.15 + (dplyr::row_number() - 1L) * 0.15
        )
      p <- p +
        geom_vline(
          data = cap_df, aes(xintercept = .data$capacity, colour = set),
          inherit.aes = FALSE, linetype = "dashed", linewidth = 0.9
        ) +
        geom_label(
          data = cap_df,
          aes(x = .data$capacity, label = capacity_label(.data$capacity), colour = set, hjust = hjust, vjust = vjust),
          y = Inf, inherit.aes = FALSE, fill = "white", linewidth = 0, size = 4.2,
          show.legend = FALSE, label.padding = grid::unit(0.12, "lines")
        )
    } else if (is.finite(cap_value)) {
      hjust <- if (cap_value >= median(df$n_completed)) 1.02 else -0.02
      p <- p +
        geom_vline(xintercept = cap_value, linetype = "dashed", linewidth = 0.9, colour = "#1a1a1a") +
        annotate(
          "label", x = cap_value, y = Inf, label = capacity_label(cap_value),
          hjust = hjust, vjust = 1.15, colour = "#1a1a1a", fill = "white",
          linewidth = 0, size = 4.2, label.padding = grid::unit(0.12, "lines")
        )
    }
    p <- p + theme_sim(legend.position = "bottom", plot.margin = margin(10, 18, 8, 8))
  } else {
    ratio <- if (is.finite(cap_value) && cap_value > 0) mean_out / cap_value else NA_real_
    title <- plot_subtitle(if (!is.finite(ratio)) {
      "Completed vehicles in each measured shift"
    } else if (ratio < 0.85) {
      sprintf("Shift output averages %.0f%% of theoretical bay capacity", 100 * ratio)
    } else if (ratio > 1.05) {
      "Shift output is above the simple bay-capacity estimate"
    } else {
      "Shift output is close to theoretical bay capacity"
    }, 32)
    varying <- df %>% group_by(shift_label) %>% filter(spread_ok(n_completed)) %>% ungroup()
    p <- ggplot(df, aes(x = n_completed)) +
      geom_histogram(
        aes(y = after_stat(density)),
        breaks = breaks, fill = shift_hist_fill, colour = "white", alpha = 0.55
      ) +
      geom_rug(colour = shift_line_color, alpha = 0.85, length = grid::unit(0.03, "npc"))
    if (nrow(varying)) {
      p <- p + geom_density(data = varying, colour = shift_line_color, linewidth = 1.15, na.rm = TRUE)
    }
    if (is.finite(cap_value)) {
      hjust <- if (cap_value >= median(df$n_completed)) 1.02 else -0.02
      p <- p +
        geom_vline(xintercept = cap_value, linetype = "dashed", linewidth = 0.9, colour = shift_line_color) +
        annotate(
          "label", x = cap_value, y = Inf, label = capacity_label(cap_value),
          hjust = hjust, vjust = 1.15, colour = shift_line_color, fill = "white",
          linewidth = 0, size = 4.2, label.padding = grid::unit(0.12, "lines")
        )
    }
    p <- p + theme_sim(plot.margin = margin(10, 18, 8, 8))
  }
  if (nlevels(df$shift_label) > 1L) {
    p <- p + facet_wrap(vars(shift_label), ncol = 2)
  }
  p +
    scale_x_continuous(breaks = integer_shift_breaks) +
    labs(
      title = title,
      x = "Completed vehicles per shift",
      y = "Density",
      caption = caption
    )
}
# Results from the saved main run. No independent resimulation or mutable settings.
production_page_ui <- function(id) {
  ns<-NS(id)
  nav_panel("Production schedule & flow",value="production_schedule",
    div(class="p-3",h3("Production schedule & flow"),uiOutput(ns("status")),
      uiOutput(ns("plan")),
      p("Shifts are consecutive production periods. Work carries over between shifts; nights, weekends and off-shift maintenance are not simulated. The settling period is excluded from all measured shift totals."),
      layout_columns(fill=FALSE,col_widths=c(6,6),
        plot_card("Output per measured shift",plotOutput(ns("shifts"),height=400)),
        plot_card("Waiting for admission versus work in production",plotOutput(ns("inventory"),height=400))),
      card(fill=FALSE,card_header("Shift results"),DTOutput(ns("shift_table")),downloadButton(ns("dl_shifts"),"Download every shift (.csv)")),
      layout_columns(fill=FALSE,col_widths=c(6,6),
        plot_card("Why occupied bays wait",p("Machine-minutes blocked after processing, waiting for operators, or awaiting automatic capacity. These do not include active processing or ongoing failure interruptions."),plotOutput(ns("waits"),height=400)),
        card(fill=FALSE,card_header("Reliability losses"),p("Total unavailability = active repair plus waiting for a technician. Includes failures still waiting at the end of the run."),DTOutput(ns("repairs")))),
      accordion(open=FALSE,
        accordion_panel("Admission and buffer assumptions",DTOutput(ns("buffers")),
          p("A downstream position is reserved before the upstream bay is released. A zero-position buffer permits direct transfer only. Unloading releases its operator before the bay blocks. Production-count support tasks can delay the vehicle after transfer while retaining its reserved downstream position.")),
        accordion_panel("Order and release data",downloadButton(ns("dl_orders"),"Download order/release ledger (.csv)"),
          downloadButton(ns("dl_flow"),"Download inventory and backlog (.csv)"),downloadButton(ns("dl_waits"),"Download station waiting time (.csv)")))))
}
production_page_server <- function(id,result,current_fingerprint) moduleServer(id,function(input,output,session) {
  saved<-reactive({r<-result();req(!is.null(r));r})
  output$status<-renderUI({r<-saved();if(!is.null(r$provenance) && !identical(r$provenance$fingerprint,current_fingerprint()))
    div(class="alert alert-warning","Main settings changed. These results describe the last saved run; use Run simulation to update them.")})
  output$plan<-renderUI({
    r<-saved();p<-r$params
    layout_columns(fill=FALSE,col_widths=c(4,4,4),
      value_box(title="Measured production",value=paste(p$n_shifts,"shifts"),p(paste(p$shift_length_min,"minutes per shift")),p(sprintf("%.1f production hours per run",p$n_shifts*p$shift_length_min/60))),
      value_box(title="Completed vehicles per run",value=sprintf("%.1f",mean(r$summaries$n_completed)),p(paste(p$n_replications,"independent simulation runs"))),
      value_box(title="Unreleased work at end",value=sprintf("%.1f",mean(r$summaries$end_unreleased_backlog)),p("Mean across runs; outside shop-floor WIP")))
  })
  output$shifts<-renderPlot({
    r <- saved()
    shift_output_plot(r$shift_summaries, theoretical_shift_capacity(r$params))
  })
  output$inventory<-renderPlot({
    saved()$release_timeseries %>% group_by(time) %>% summarise(
      `Unreleased orders`=mean(unreleased_backlog),`Shop-floor WIP`=mean(wip),.groups="drop") %>%
      pivot_longer(-time,names_to="location",values_to="vehicles") %>%
      ggplot(aes(time,vehicles,color=location))+geom_step(linewidth=1)+
      scale_color_manual(values=c("Unreleased orders"="#D46B25","Shop-floor WIP"="#2474A5"))+
      labs(x="Time including settling (min)",y="Vehicles",color=NULL,
        subtitle=plot_subtitle("Mean across runs. The chart begins after the settling period."))+theme_sim()
  })
  output$shift_table<-renderDT({
    saved()$shift_summaries %>% group_by(shift) %>% summarise(
      `Mean planned orders`=mean(orders_due),`Mean admitted`=mean(n_released),`Mean completed`=mean(n_completed),
      `Completion SD`=sd(n_completed),`Mean ending WIP`=mean(end_wip),`Mean ending unreleased`=mean(end_backlog),
      `Production lead time (min)`=mean(release_lead_time,na.rm=TRUE),
      `Order-to-completion (min)`=mean(order_lead_time,na.rm=TRUE),.groups="drop") %>%
      datatable(rownames=FALSE,options=list(scrollX=TRUE,pageLength=20)) %>% formatRound(columns=2:9,digits=2)
  })
  output$waits<-renderPlot({
    saved()$station_waits %>% mutate(station=label_station(station),type=recode(type,
      blocked="Blocked after processing",waiting_for_operator="Waiting for operator",waiting_for_automatic_capacity="Awaiting automatic capacity")) %>%
      group_by(station,type) %>% summarise(minutes=mean(minutes),.groups="drop") %>%
      ggplot(aes(station,minutes,fill=type))+geom_col(position="dodge")+coord_flip()+
      labs(x=NULL,y="Mean bay-minutes per measured run",fill=NULL)+theme_sim()
  })
  output$repairs<-renderDT({
    saved()$downtime_by_rep %>% group_by(station) %>% summarise(
      `Active repair minutes`=mean(downtime_min),`Waiting for repair minutes`=mean(waiting_repair_min),
      `Total unavailable minutes`=mean(total_downtime_min),`Unavailable fraction`=mean(total_downtime_fraction),.groups="drop") %>%
      mutate(station=label_station(station)) %>% datatable(rownames=FALSE,options=list(dom="t",scrollX=TRUE)) %>%
      formatRound(columns=2:4,digits=2) %>% formatPercentage("Unavailable fraction",1)
  })
  output$buffers<-renderDT({r<-saved();p<-r$params
    p$stations %>% transmute(Station=label_station(name),Bays=capacity,
      `Waiting positions before station`=if_else(row_number()==1,if(p$release_policy=="scheduled") Inf else as.numeric(p$entry_buffer),buffer_before)) %>%
      datatable(rownames=FALSE,options=list(dom="t"))
  })
  walk(c("shifts","orders","flow","waits"),function(x) {
    field<-c(shifts="shift_summaries",orders="orders",flow="release_timeseries",waits="station_waits")[[x]]
    output[[paste0("dl_",x)]]<-downloadHandler(filename=function() paste0(field,".csv"),content=function(file) write_csv(saved()[[field]],file))
  })
})
