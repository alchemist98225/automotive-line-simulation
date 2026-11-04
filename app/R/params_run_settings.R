# params_run_settings.R
# Parameters page: run settings card (replications, seed, shift length,
# settling period, shifts, demand, admission) plus an always-visible Save bar.
# The Simulate sidebar inputs stay the single source of truth: Save copies the
# p_* fields into them with update*Input(); nothing runs until the user clicks
# Run on Simulate. All controls are static UI (no renderUI), so Save cannot
# rebuild or reset a widget mid-click.

# Parameters-page field id -> Simulate sidebar id, and the update function each uses.
params_run_fields <- tibble::tribble(
  ~page_id,            ~sim_id,           ~kind,
  "p_n_reps",          "n_reps",          "numeric",
  "p_seed",            "seed",            "numeric",
  "p_sim_length",      "sim_length",      "numeric",
  "p_warmup",          "warmup",          "numeric",
  "p_n_shifts",        "n_shifts",        "numeric",
  "p_planned_rate",    "planned_rate",    "numeric",
  "p_order_spacing",   "order_spacing",   "select",
  "p_release_policy",  "release_policy",  "select",
  "p_entry_buffer",    "entry_buffer",    "numeric",
  "p_wip_limit",       "wip_limit",       "numeric"
)

params_run_settings_ui <- function() {
  card(
    id = "params_run_settings",
    class = "params-section mb-3",
    fill = FALSE,
    card_header("Run settings"),
    card_body(
      fill = FALSE,
      p(class = "small mb-2",
        "How long to run, how many times, and how much work arrives. ",
        "These match the Run simulation panel on Simulate."),
      layout_columns(
        fill = FALSE,
        col_widths = breakpoints(sm = 12, md = 6, xl = 3),
        numericInput("p_n_reps", "Number of replications", value = 5, min = 1, max = 100, step = 1),
        numericInput("p_seed", "Base seed", value = 20261027, min = 1, step = 1),
        numericInput("p_planned_rate", "Planned work rate (vehicles/hour)", value = 15, min = .01, max = 600, step = 1),
        numericInput("p_n_shifts", "Number of measured shifts", value = 1, min = 1, max = 100, step = 1)
      ),
      layout_columns(
        fill = FALSE,
        col_widths = breakpoints(sm = 12, md = 6, xl = 3),
        numericInput("p_sim_length", "Shift duration (production minutes)", value = 480, min = 1, max = 1440, step = 30),
        numericInput("p_warmup", "Initial settling period (minutes)", value = 300, min = 0, max = 10080, step = 15),
        selectInput("p_order_spacing", "Order spacing",
                    c("Evenly scheduled" = "fixed", "Random (exponential)" = "exponential"),
                    selected = "fixed", selectize = FALSE),
        selectInput("p_release_policy", "Admission policy",
                    c("When entrance space is available" = "space",
                      "Limit total line WIP (CONWIP)" = "conwip",
                      "Release all scheduled work" = "scheduled"),
                    selected = "space", selectize = FALSE)
      ),
      layout_columns(
        fill = FALSE,
        col_widths = breakpoints(sm = 12, md = 6, xl = 3),
        numericInput("p_entry_buffer", "Entrance waiting positions", 3, min = 0, step = 1),
        numericInput("p_wip_limit", "Maximum line WIP (CONWIP only)", 20, min = 1, step = 1)
      )
    )
  )
}

# Fixed footer on the Parameters page (hidden with its tab) so Save stays on screen.
params_save_bar_ui <- function() {
  tagList(
    tags$style(HTML("
      .params-save-bar {
        position: fixed; left: 0; right: 0; bottom: 0; z-index: 1030;
        background: #ffffff; border-top: 2px solid #2c7fb8;
        box-shadow: 0 -2px 8px rgba(0,0,0,0.08);
        padding: 0.6rem 1.25rem;
        display: flex; flex-wrap: wrap; align-items: center; gap: 0.75rem;
      }
      /* Room so the fixed bar never covers the last card. */
      .params-page-body { padding-bottom: 5.5rem !important; }
      .params-save-bar .params-save-help { margin: 0; color: #1b4f72; }
    ")),
    div(
      id = "params_save_bar",
      class = "params-save-bar",
      actionButton("params_save", tagList(icon("floppy-disk"), " Save settings"), class = "btn-primary"),
      p(class = "params-save-help small",
        "Saving fills in the Simulate page. Go to Simulate and click Run to use these settings.")
    )
  )
}

# Push values (named by sim_id) into the Parameters-page copies.
params_run_fields_update <- function(session, values) {
  purrr::pwalk(params_run_fields, function(page_id, sim_id, kind) {
    v <- values[[sim_id]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) return()
    if (identical(kind, "select")) updateSelectInput(session, page_id, selected = v)
    else updateNumericInput(session, page_id, value = v)
  })
}

# Run-setting values from a params list, keyed by sidebar id.
params_run_values <- function(p) {
  list(
    n_reps = as.integer(p$n_replications), seed = as.integer(p$base_seed),
    sim_length = p$shift_length_min, warmup = p$warmup_min,
    n_shifts = as.integer(p$n_shifts), planned_rate = 60 / p$arrival_mean_min,
    order_spacing = p$order_spacing, release_policy = p$release_policy,
    entry_buffer = p$entry_buffer, wip_limit = p$wip_limit
  )
}

params_save_server <- function(input, session) {
  # Entering Parameters: copy the current Simulate values so the page never
  # shows stale numbers.
  observeEvent(input$main_page, {
    if (!identical(input$main_page, "parameters")) return()
    vals <- params_run_fields$sim_id %>% purrr::set_names() %>% purrr::map(~ isolate(input[[.x]]))
    params_run_fields_update(session, vals)
  }, ignoreInit = TRUE)

  # Save: Parameters copies -> Simulate sidebar. Station inputs on this page
  # are already the ones Run reads, so only the run settings need copying.
  observeEvent(input$params_save, {
    purrr::pwalk(params_run_fields, function(page_id, sim_id, kind) {
      v <- isolate(input[[page_id]])
      if (is.null(v) || (length(v) == 1L && is.na(v))) return()
      if (identical(kind, "select")) updateSelectInput(session, sim_id, selected = v)
      else updateNumericInput(session, sim_id, value = v)
    })
    showNotification(
      "Saved. Go to Simulate and click Run to use these settings.",
      type = "message", duration = 5
    )
  }, ignoreInit = TRUE)
}
