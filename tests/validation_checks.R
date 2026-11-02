# Validation smoke + testthat. Run from repo root: Rscript tests/validation_checks.R
suppressPackageStartupMessages(library(tidyverse))
Sys.setenv(NOT_CRAN = "true")
if (!dir.exists("tests/testthat")) dir.create("tests/testthat", recursive = TRUE)
res <- testthat::test_file("tests/testthat/test-validation.R", reporter = "progress")
df <- as.data.frame(res)
if (nrow(df) && any(df$failed > 0 | df$error > 0)) quit(status = 1)
cat("validation_checks: OK\n")
