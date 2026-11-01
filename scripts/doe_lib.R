# doe_lib.R
# Shared entry point for the design-of-experiments (DOE) CLI.
# Sourced by scripts/doe_make_design.R, doe_run.R, and doe_plot.R.
# Portfolio demonstration; results are illustrative only, not predictions for DRI.
#
# Loads sibling modules:
#   doe_design.R      — CSV schema, validate, params_from_row
#   doe_metrics.R     — extract / summarise HPU and throughput
#   doe_checkpoint.R  — chunk resume under out/chunks/
#
# HPU definition (hours per unit):
#   HPU = (n_operators × observation_hours) / n_completed
#   observation_hours = (sim_length_min - warmup_min) / 60
# This is the inverse of the model's throughput_per_labor_hr.
# Lower HPU means more units per labor-hour. "Peak productivity" is minimum HPU.

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
doe_repo_root <- function(script_dir = NULL) {
  if (is.null(script_dir)) {
    args_full <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args_full, value = TRUE)
    script_dir <- if (length(file_arg)) {
      dirname(normalizePath(sub("^--file=", "", file_arg)))
    } else {
      normalizePath(".")
    }
  }
  if (basename(script_dir) == "scripts") dirname(script_dir) else script_dir
}

doe_source_model <- function(repo_root = doe_repo_root()) {
  path <- file.path(repo_root, "app", "R", "model.R")
  if (!file.exists(path)) stop("Could not find app/R/model.R at ", path, call. = FALSE)
  source(path, local = FALSE)
  # simmer::select masks dplyr::select; restore tidyverse verbs for DOE scripts.
  assign("select", dplyr::select, envir = .GlobalEnv)
  assign("filter", dplyr::filter, envir = .GlobalEnv)
  assign("mutate", dplyr::mutate, envir = .GlobalEnv)
  assign("summarise", dplyr::summarise, envir = .GlobalEnv)
  assign("summarize", dplyr::summarize, envir = .GlobalEnv)
  assign("arrange", dplyr::arrange, envir = .GlobalEnv)
  assign("rename", dplyr::rename, envir = .GlobalEnv)
  assign("relocate", dplyr::relocate, envir = .GlobalEnv)
  invisible(path)
}

# Sibling modules live next to this file. Prefer the caller's script_dir
# (every DOE CLI sets it before source()); fall back to source() ofile.
.doe_lib_dir <- local({
  if (exists("script_dir", inherits = TRUE)) {
    sd <- get("script_dir", inherits = TRUE)
    if (is.character(sd) && length(sd) == 1L && dir.exists(sd)) return(sd)
  }
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  normalizePath("scripts")
})
source(file.path(.doe_lib_dir, "doe_design.R"), local = FALSE)
source(file.path(.doe_lib_dir, "doe_metrics.R"), local = FALSE)
source(file.path(.doe_lib_dir, "doe_checkpoint.R"), local = FALSE)

doe_format_duration <- function(secs) {
  secs <- max(0, as.numeric(secs))
  if (secs < 60) return(sprintf("%.0fs", secs))
  if (secs < 3600) return(sprintf("%.1fmin", secs / 60))
  sprintf("%.2fh", secs / 3600)
}
