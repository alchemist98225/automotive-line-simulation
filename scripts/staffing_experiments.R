# Run from repository root: Rscript scripts/staffing_experiments.R
source("app/R/model.R")
# Keep inputs, replications, and complete results in a nested scenario database.
tibble(upstream=c(1L,2L,3L,1L,2L,3L),downstream=c(1L,1L,1L,2L,2L,2L)) %>%
  mutate(scenario=paste0("A=",upstream,", B=",downstream),
    params=map2(upstream,downstream,function(a,b) {
      p <- default_params(); p$n_replications <- 20L
      p$operators$count <- c(a,b); p
    })) %>% run_scenarios() %>%
  {dir.create("output",showWarnings=FALSE)
   saveRDS(.,"output/staffing_results.rds")
   dplyr::select(.,scenario,comparison) %>% unnest(comparison)} %>%
  write_csv("output/staffing_comparison.csv")
