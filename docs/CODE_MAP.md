# Code map — Automotive Line Simulator

Structural layout after the pure-move split (`tom/split-app-modules`). Behavior and visuals are unchanged; files exist so a person can find chart code by title.

## Files

| File | Lines | Purpose | Key functions |
|------|------:|---------|---------------|
| `app/app.R` | 79 | Libraries, ordered `source()`, navbar layout, `shinyApp()` | `ui`, (server from `server_main.R`) |
| `app/R/comparison_logic.R` | 479 | Compare labels, flatten/diff/shift/metrics, parameter editor module. | `comparison_pools, comparison_flatten, comparison_same, sanitize_set_label, sanitize_set_labels, comparison_palette, pretty_parameter, comparison_display_value, comparison_diff, comparison_diff_table, comparison_baseline_params, comparison_shift…` |
| `app/R/comparison_plots.R` | 126 | Compare densities, ridges, highlight, flow overlays. | `comparison_densities, comparison_ridge_n, comparison_ridge_height, comparison_ridge_plot, format_highlight_mean, comparison_highlight_subtitle, comparison_highlight_plot, comparison_highlight_frame, comparison_flow` |
| `app/R/comparison_server.R` | — | Compare page server; shared Replications; headline KPIs Throughput/lead/HPU/Bottleneck. | `comparison_page_server` |
| `app/R/comparison_ui.R` | 460 | Compare download catalog + comparison_page_ui(); saved-results order is KPIs above highlight graphs. | `comparison_download_catalog, comparison_page_ui` |
| `app/R/helpers_plots.R` | — | Shared plot plumbing: station labels/colors, theme_sim, ticks, hour breaks, fingerprints. | `label_station, observation_hour_breaks, params_fingerprint, stamp_run, theme_sim…` |
| `app/R/model.R` | 515 | Simulation engine: default params, validate/normalize, run_line, exports. Path stable for DOE/Quarto. | `sim_now, default_params, timing_spec, validate_timing, make_duration_fn, timing_mean, make_service_fn, activity_timing, normalize_params, validate_params, resource_stats, run_one_replication…` |
| `app/R/plots_bottleneck.R` | 80 | Wait-reason interactive bars (Bottleneck page). | `wait_reason_bars_interactive` |
| `app/R/plots_flow.R` | 340 | Occupancy helpers, compact metric tables, downtime bars, Queues / WIP / Cycle time charts. | `occupancy_groups, rep_mean_ci, occupancy_boxes, rep_summary_df, compact_metric_dt, occupancy_summary_df, occupancy_summary_dt, reliability_losses_df, reliability_losses_dt, operator_pool_df, operator_pool_dt, replication_bars, queue_timeseries_plot, wip_timeseries_plot, cycle_time_plot` |
| `app/R/plots_simulate.R` | 205 | Overlay and replication density builders (Compare Output distributions). | `overlay_densities, replication_densities` |
| `app/R/production_page.R` | 64 | Production schedule page UI + server module. | `production_page_ui, production_page_server` |
| `app/R/production_plots.R` | 222 | Shift capacity helpers + shift_output_plot. | `theoretical_shift_capacity, integer_shift_breaks, capacity_label_hjust, capacity_label, shift_binwidth, shift_hist_breaks, shift_output_plot` |
| `app/R/server_main.R` | — | Main server(): run lifecycle, Compare→Results/Parameters import, KPIs, downloads. | `server` |
| `app/R/ui_css_js.R` | 340 | Global page header CSS and Results-nav / KPI JS. | `app_header_tags` |
| `app/R/ui_pages.R` | 558 | nav_panel builders: Simulate, Parameters, Bottleneck, Flow, Lead, About. | `simulate_page_ui, parameters_page_ui, results_bottleneck_ui, results_flow_ui, results_lead_ui, about_page_ui` |
| `app/R/ui_simulate.R` | 219 | Guided example card + Simulate run-status banner / Run button CSS+JS. | `guided_example_ui, sim_status_bar_ui` |
| `app/R/ui_widgets.R` | — | Plot cards, download catalog, timing editors, KPI nav boxes, apply/restore params. | `apply_params_to_inputs, restore_default_inputs, provenance_warning, sim_render_plot, plot_card…` |

## Chart title → function

Search the app UI (or this table) for the chart title, then open the function.

| Chart title / role | Function | File |
|--------------------|----------|------|
| Chart: Output distributions / station & operator densities (Compare). | `comparison_densities` | `app/R/comparison_plots.R` |
| Chart helper: ridgeline height wrapper for Compare station ridges. | `comparison_ridge_plot` | `app/R/comparison_plots.R` |
| Chart: Biggest changes highlight densities (Compare). | `comparison_highlight_plot` | `app/R/comparison_plots.R` |
| Chart: Queue / WIP flow overlay for two sets (Compare). | `comparison_flow` | `app/R/comparison_plots.R` |
| Chart titles: (none — helpers only; search plot files for chart titles). | `` | `app/R/helpers_plots.R` |
| Charts share one text size. 16 is about half again the ggplot2 default (11). | `theme_sim` | `app/R/helpers_plots.R` |
| Chart: Why occupied bays wait (Results → Bottleneck & capacity). | `wait_reason_bars_interactive` | `app/R/plots_bottleneck.R` |
| Chart helper: Bay occupancy bars with 95% CI (Quarto / legacy; Shiny uses the occupancy table). | `occupancy_boxes` | `app/R/plots_flow.R` |
| Chart/table: Bay occupancy by station (Bottleneck → Station occupancy). | `occupancy_summary_dt` | `app/R/plots_flow.R` |
| Chart helper: mean bars with 95% CI (Downtime fraction). | `replication_bars` | `app/R/plots_flow.R` |
| Chart: "Queues" (Results → Flow over time → Queues) | `queue_timeseries_plot` | `app/R/plots_flow.R` |
| Chart: Queues (Results → Flow over time). | `queue_timeseries_plot` | `app/R/plots_flow.R` |
| Chart: "WIP" (Results → Flow over time → WIP) | `wip_timeseries_plot` | `app/R/plots_flow.R` |
| Chart: WIP (Results → Flow over time). | `wip_timeseries_plot` | `app/R/plots_flow.R` |
| Chart: "Cycle time" (Results → Lead time & data → Cycle time) | `cycle_time_plot` | `app/R/plots_flow.R` |
| Chart: Cycle time (Results → Lead time & data). | `cycle_time_plot` | `app/R/plots_flow.R` |
| Chart helper: overlaid histograms (Compare Output distributions and faceted station densities). | `overlay_densities` | `app/R/plots_simulate.R` |
| Chart helper: one histogram per category (station/pool). | `replication_densities` | `app/R/plots_simulate.R` |
| Chart: Vehicles completed per shift (Results → Production schedule). | `shift_output_plot` | `app/R/production_plots.R` |
| Chart drawing lives in plots_*.R; this file binds inputs → those functions. | `server` | `app/R/server_main.R` |
| Bay occupancy by station (table) | `occupancy_summary_dt` / `occupancy_summary_df` | `app/R/plots_flow.R` |
| Mean utilization by pool (table) | `operator_pool_dt` / `operator_pool_df` | `app/R/plots_flow.R` |
| Downtime fraction (mean ± 95% CI) | `replication_bars` | `app/R/plots_flow.R` |
| Reliability losses (table) | `reliability_losses_dt` / `reliability_losses_df` | `app/R/plots_flow.R` |

## Load order

`app.R` sources modules explicitly in dependency order (see the source() block).
`model.R` keeps its path for `scripts/doe_lib.R` and Quarto copies.

## Quarto

`quarto/` has its own copies of model/helpers; this split does not restructure it.

## Compare → main handoff

Parameters and Results pages import from Compare (no send buttons on Compare). `apply_params_to_inputs()` writes a set into the main controls; Results uses `sim_result_store` so a finished Compare run can be viewed without rerunning.
