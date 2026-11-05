# Optimize page smoke: tiny pop/gen through the GA core the page uses.
# Run from repo root: Rscript tests/optimize_page_checks.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/opt_lib.R")
source("app/R/optimize_plots.R")
stopifnot(exists("optimize_convergence_plot"))
# Core still works after move into app/R
stopifnot(nrow(opt_knob_defs()) == 6L)
stopifnot(identical(as.integer(opt_knob_defs()$default[opt_knob_defs()$name == "cap_paint"]), 2L))

base <- opt_evaluate_baseline(n_reps = 3L, base_seed = 11L)
stopifnot(base$thr_mean > 8, abs(base$cost_hpu) < 1e-9)

res <- opt_run_ga(
  pop_size = 4L, n_gen = 2L, n_reps = 1L, elite_reps = 2L, n_elites = 2L,
  base_seed = 3L, cores = 1L, verbose = FALSE
)
stopifnot(nrow(res$history) == 2L, is.finite(res$best$fitness))

# Recheck pool: top N + one-operator cuts, deduplicated, baseline excluded
source("app/R/optimize_recheck.R")
defs <- opt_knob_defs()
bg <- as.integer(defs$default)
cut_up <- bg; cut_up[match("op_upstream", defs$name)] <- cut_up[match("op_upstream", defs$name)] - 1L
ev <- purrr::map2_dfr(list(bg, cut_up, bg + 1L), c(0.3, 0.2, 0.4), function(g, f) {
  tibble(fitness = f, genome_key = opt_genome_key(g), !!!setNames(as.list(g), defs$name))
})
pool <- opt_recheck_pool(ev, bg, defs, n_top = 3L)
keys <- purrr::map_chr(pool, opt_genome_key)
stopifnot(!anyDuplicated(keys), !opt_genome_key(bg) %in% keys, opt_genome_key(cut_up) %in% keys)

# UI/server files parse
for (f in c(
  "app/R/optimize_ui.R", "app/R/optimize_server.R", "app/R/optimize_plots.R",
  "app/R/optimize_doe.R", "app/R/optimize_space.R", "app/R/optimize_fitness.R",
  "app/R/optimize_ga.R", "app/R/optimize_recheck.R"
)) {
  parse(f)
}
# app.R mentions Optimize page
app_txt <- paste(readLines("app/app.R"), collapse = "\n")
stopifnot(grepl("optimize_page_ui", app_txt), grepl("optimize_page_server", paste(readLines("app/R/server_main.R"), collapse = "\n")) || grepl("optimize_page_server", paste(readLines("app/R/server_main.R"), collapse="\n")))
stopifnot(grepl("optimize_page_server", paste(readLines("app/R/server_main.R"), collapse = "\n")))

message("optimize_page_checks OK")
