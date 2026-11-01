# comparison_ui.R
# Compare page UI: download catalog entries and comparison_page_ui().
# Assembled into the navbar from app.R via comparison_page_ui("two_set_study").

comparison_download_catalog <- function(ns) {
  entry <- function(id, label, description) download_entry(ns(id), label, description)
  tagList(
    h5("Data behind the charts"),
    p(
      class = "text-muted small",
      "Each CSV stacks both parameter sets and every replication from the captured comparison."
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
          background: #cfe8f6;
          border: 3px solid #1b6ca8;
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
        .compare-progress-card.compare-progress-running {
          border-color: #1b6ca8 !important;
          background: #cfe8f6 !important;
          box-shadow: 0 4px 12px rgba(0,0,0,.12) !important;
          opacity: 1 !important;
        }
        .compare-progress-card.compare-progress-running .compare-progress-title,
        .compare-progress-card.compare-progress-running .compare-progress-detail,
        .compare-progress-card.compare-progress-running .compare-progress-title i {
          color: #0a2540 !important;
          opacity: 1 !important;
        }
        .compare-progress-card.compare-progress-done {
          border-color: #198754;
          background: #f3faf5;
          box-shadow: 0 4px 12px rgba(25,135,84,.12);
        }
        .compare-progress-title {
          font-size: 1.1rem;
          font-weight: 800;
          color: #0a2540 !important;
          margin: 0;
          white-space: nowrap;
          display: flex;
          align-items: center;
          gap: 0.45rem;
        }
        .compare-progress-title i,
        .compare-progress-title .fa,
        .compare-progress-title .fas {
          color: #0a2540 !important;
        }
        .compare-progress-idle .compare-progress-title { color: #495057 !important; }
        .compare-progress-done .compare-progress-title { color: #146c43 !important; }
        .compare-progress-detail {
          flex: 1 1 auto;
          min-width: 0;
          margin: 0 !important;
          font-size: 1rem;
          color: #0a2540 !important;
          font-weight: 700;
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
        .compare-shared-reps {
          display: flex;
          flex-direction: column;
          gap: 0.15rem;
        }
        .compare-shared-reps .form-group { margin-bottom: 0; }
        .compare-shared-reps label {
          font-weight: 700;
          color: #1a1a1a;
          margin-bottom: 0.15rem;
        }
        .compare-shared-reps-note {
          font-size: 0.8rem;
          font-weight: 700;
          color: #1a1a1a;
          line-height: 1.2;
          min-height: 1rem;
        }
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
        /* Shiny fades .recalculating outputs to ~0.3; the status banner must stay full-strength
           for the whole blocking run (same visual weight as the Done banner). */
        #two_set_study-compare_progress,
        #two_set_study-compare_progress.recalculating,
        #two_set_study-compare_progress.recalculating *,
        #two_set_study-run_button.recalculating,
        #two_set_study-run_button.recalculating *,
        .two-set-study .compare-status-bar,
        .two-set-study .compare-status-bar.recalculating,
        .two-set-study .compare-status-bar .recalculating,
        .shiny-busy #two_set_study-compare_progress,
        .shiny-busy #two_set_study-compare_progress * {
          opacity: 1 !important;
          filter: none !important;
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
              card.classList.add('compare-progress-running');
              card.style.setProperty('border-color', '#1b6ca8', 'important');
              card.style.setProperty('background', '#cfe8f6', 'important');
              card.style.setProperty('box-shadow', '0 4px 12px rgba(0,0,0,.10)', 'important');
              card.style.setProperty('opacity', '1', 'important');
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
                title.style.setProperty('color', '#0a2540', 'important');
                title.style.setProperty('opacity', '1', 'important');
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
                detail.style.setProperty('color', '#0a2540', 'important');
                detail.style.setProperty('font-weight', '700', 'important');
                detail.style.setProperty('opacity', '1', 'important');
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
      uiOutput(ns("compare_progress")),
      div(
        class = "d-flex flex-wrap gap-3 mb-3 align-items-end",
        actionButton(ns("import"), "Import main settings into both sets"),
        actionButton(ns("clone"), "Copy Set 1 to Set 2"),
        div(
          class = "compare-shared-reps",
          numericInput(
            ns("shared_reps"),
            "Replications",
            value = default_params()$n_replications,
            min = 1,
            step = 1,
            width = "8.5rem"
          ),
          uiOutput(ns("shared_reps_note"))
        ),
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
        uiOutput(ns("kpis")),
        uiOutput(ns("change_story")),
        uiOutput(ns("highlights")),
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
            p("One panel per pool. A vertical line means that set did not vary."),
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
