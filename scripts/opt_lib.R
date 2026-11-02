# opt_lib.R
# Shared entry for optimizer CLI: model + DOE + app/R optimize_* core.
# Sourced by opt_run.R, opt_plot.R, and tests/opt_checks.R.

opt_this_file <- (function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) return(normalizePath(ofile, mustWork = FALSE))
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE))
  NA_character_
})()

opt_script_dir <- if (!is.na(opt_this_file)) dirname(opt_this_file) else {
  if (dir.exists("scripts")) normalizePath("scripts") else normalizePath(".")
}

opt_repo_root <- function(script_dir = opt_script_dir) {
  normalizePath(file.path(script_dir, ".."))
}

opt_app_r <- function(repo = opt_repo_root()) {
  file.path(repo, "app", "R")
}

# Model (byte-identical with quarto/R/model.R)
source(file.path(opt_app_r(), "model.R"), local = FALSE)

# DOE helpers used by CLI writes + (also provided by optimize_doe for the GA)
source(file.path(opt_script_dir, "doe_lib.R"), local = FALSE)

# Shared GA core lives under app/R so the Shiny app can ship it.
app_r <- opt_app_r()
source(file.path(app_r, "optimize_doe.R"), local = FALSE)
source(file.path(app_r, "optimize_space.R"), local = FALSE)
source(file.path(app_r, "optimize_fitness.R"), local = FALSE)
source(file.path(app_r, "optimize_ga.R"), local = FALSE)
