# Run from repository root. Uses the same engine and defaults as the Shiny app.
source("app/R/model.R")
dir.create("output",showWarnings=FALSE)
run_line_sim() %>% {
  write_csv(.$summaries,"output/metrics_summary.csv")
  write_csv(.$stations,"output/station_metrics.csv")
  write_csv(.$labor,"output/operator_metrics.csv")
  write_csv(.$failure_events,"output/failure_events.csv")
  saveRDS(.,"output/simulation_results.rds")
  print(.$headline)
}
