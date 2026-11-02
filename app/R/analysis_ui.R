# analysis_ui.R
# Analysis nav page: precomputed break-sweep, tornado, validation, and warm-up
# charts. No live runs — data comes from app/data/analysis_precomputed.rds.

analysis_page_ui <- function(id = "analysis_page") {
  ns <- NS(id)
  nav_panel(
    title = "Analysis",
    value = "analysis",
    icon = bs_icon("clipboard2-data"),
    div(
      class = "px-3 pt-3",
      h3("Analysis"),
      p(
        class = "text-muted",
        "Precomputed studies shipped with the app. Charts use the same ggplot style ",
        "as the rest of the demo. Regenerate the backing files with ",
        code("scripts/analysis_precompute.R"),
        "."
      )
    ),
    layout_columns(
      col_widths = 12,
      class = "px-3",
      card(
        fill = FALSE,
        card_header("Where the line breaks"),
        card_body(
          fill = FALSE,
          p(
            class = "text-muted small",
            "Demand rises until mean throughput falls below 95% of offered demand. ",
            "Default line (1 paint capacity) and 2-bay balanced (2 paint capacity), ",
            "10 replications per point, common seeds."
          ),
          plotOutput(ns("break_plot"), height = "560px")
        )
      ),
      card(
        fill = FALSE,
        card_header("Sensitivity (tornado)"),
        card_body(
          fill = FALSE,
          p(
            class = "text-muted small",
            "One-at-a-time low/high swings around the 2-bay set at 10 vehicles/hr. ",
            "Bars show how far HPU and throughput move; the dashed line is the baseline."
          ),
          plotOutput(ns("tornado_plot"), height = "720px")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          fill = FALSE,
          card_header("M/M/c check vs Erlang-C"),
          card_body(
            fill = FALSE,
            p(
              class = "text-muted small",
              "A single station against the textbook formula at 50%, 70%, and 85% load. ",
              "This is a standalone M/M/c check, not the full automotive line."
            ),
            plotOutput(ns("mmc_plot"), height = "520px")
          )
        ),
        card(
          fill = FALSE,
          card_header("Little's Law on the full line"),
          card_body(
            fill = FALSE,
            p(
              class = "text-muted small",
              "Observed WIP versus λ × W across Default and 2-bay setups. ",
              "Points near the dashed diagonal mean the accounting holds."
            ),
            plotOutput(ns("littles_plot"), height = "520px")
          )
        )
      ),
      card(
        fill = FALSE,
        card_header("Welch warm-up"),
        card_body(
          fill = FALSE,
          p(
            class = "text-muted small",
            "Ensemble WIP from t = 0 at 6 vehicles/hr. The vertical mark is the ",
            "current 300-minute settle. The 2-bay set settles early; the default ",
            "line near its break needs longer under a strict reading."
          ),
          plotOutput(ns("warmup_plot"), height = "480px")
        )
      )
    )
  )
}
