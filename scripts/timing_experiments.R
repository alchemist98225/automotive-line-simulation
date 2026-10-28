# Run from repository root: Rscript scripts/timing_experiments.R
source("app/R/model.R")
default_params() %>%
  modifyList(list(n_replications=20L)) %>%
  variability_scenarios() %>% run_scenarios() %>% {
    dir.create("output",showWarnings=FALSE)
    saveRDS(.,"output/timing_results.rds")
    dplyr::select(.,scenario,comparison) %>% unnest(comparison)
  } %>% write_csv("output/timing_comparison.csv")
