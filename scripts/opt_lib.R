# opt_lib.R
# Shared entry for the integer-GA line optimizer CLI.
# Sourced by scripts/opt_run.R, opt_plot.R, and tests/opt_checks.R.
# Portfolio demonstration; results are illustrative only, not plant predictions.
#
# Loads DOE helpers (doe_lib.R) then:
#   opt_space.R    — knobs / genome encode-decode
#   opt_fitness.R  — HPU + throughput-floor penalty
#   opt_ga.R       — tidyverse/purrr integer GA

suppressPackageStartupMessages({
  library(tidyverse)
})

opt_repo_root <- function(script_dir = NULL) {
  doe_repo_root(script_dir)
}

# Prefer the directory of this file (works when sourced from tests/ or CLI).
.opt_lib_dir <- local({
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) return(dirname(normalizePath(ofile)))
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg)) {
    d <- dirname(normalizePath(sub("^--file=", "", file_arg)))
    if (basename(d) == "scripts") return(d)
  }
  if (dir.exists("scripts")) return(normalizePath("scripts"))
  normalizePath(".")
})
script_dir <- .opt_lib_dir

source(file.path(script_dir, "doe_lib.R"), local = FALSE)
doe_source_model(opt_repo_root(script_dir))

source(file.path(script_dir, "opt_space.R"), local = FALSE)
source(file.path(script_dir, "opt_fitness.R"), local = FALSE)
source(file.path(script_dir, "opt_ga.R"), local = FALSE)
