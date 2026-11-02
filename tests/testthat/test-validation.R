# testthat checks for M/M/c and Little's Law helpers.

find_repo_root <- function() {
  candidates <- c(
    getwd(),
    normalizePath(".", mustWork = FALSE),
    normalizePath("..", mustWork = FALSE),
    normalizePath(file.path("..", ".."), mustWork = FALSE)
  )
  for (r in unique(candidates)) {
    if (file.exists(file.path(r, "scripts", "validate_lib.R"))) return(r)
  }
  stop("Could not locate repo root with scripts/validate_lib.R")
}
root <- find_repo_root()
# validate_lib.R looks for script_dir in the caller environment
script_dir <<- file.path(root, "scripts")
source(file.path(script_dir, "validate_lib.R"), local = FALSE)
doe_source_model(doe_repo_root(script_dir))

test_that("Erlang-C matches known M/M/1 limit", {
  a <- erlang_c(0.5, 1, 1L)
  expect_equal(a$rho, 0.5)
  expect_equal(a$Wq, 1, tolerance = 1e-9)
  expect_equal(a$Lq, 0.5, tolerance = 1e-9)
})

test_that("M/M/c simmer is near Erlang-C at moderate rho", {
  skip_on_cran()
  res <- mm_c_run_case(lambda = 1.0, mu = 1, c = 2L, n_reps = 20L,
                       horizon = 12000, warmup = 1500, base_seed = 7L, workers = 1L)
  wq <- res$summary %>% dplyr::filter(metric == "Wq_mean")
  util <- res$summary %>% dplyr::filter(metric == "util")
  expect_lt(abs(wq$rel_err[[1]]), 0.15)
  expect_lt(abs(util$rel_err[[1]]), 0.05)
})

test_that("Little's Law relative error is small on a light scenario", {
  skip_on_cran()
  scen <- littles_scenarios() %>% dplyr::filter(setup == "guided_2", arrival_mean_min == 8)
  reps <- littles_run(scen, n_reps = 4L, base_seed = 11L, workers = 1L)
  expect_lt(mean(abs(reps$rel_err), na.rm = TRUE), 0.08)
})
