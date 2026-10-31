#!/usr/bin/env Rscript
# Build an example design CSV with tidyr::crossing() over real model knobs.
# Edit the crossing() block below, then:
#   Rscript scripts/doe_make_design.R
#   Rscript scripts/doe_make_design.R --out designs/my_doe.csv
#
# Results are illustrative only — not predictions for DRI.
suppressPackageStartupMessages({library(tidyverse)})

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else normalizePath("scripts")
source(file.path(script_dir, "doe_lib.R"))
doe_source_model(doe_repo_root(script_dir))

# --- CLI: optional --out path ----------------------------------------------
out_path <- "designs/example_paint_ops.csv"
argv <- commandArgs(trailingOnly = TRUE)
if ("--out" %in% argv) {
  i <- match("--out", argv)
  if (is.na(i) || i == length(argv)) stop("--out needs a path", call. = FALSE)
  out_path <- argv[[i + 1]]
}

# --- Edit this crossing() to explore a different space ---------------------
# Example: paint bay capacity × Zone A (upstream) operators.
# Other columns keep the app defaults when omitted from the CSV.
design <- tidyr::crossing(
  cap_paint     = c(1L, 2L),          # equipment: paint bays
  op_upstream   = c(1L, 2L, 3L, 4L),  # Zone A (body + paint)
  op_downstream = 2L                  # Zone B held at the default
) %>%
  mutate(
    design_id = sprintf("paint%d_up%d_down%d", cap_paint, op_upstream, op_downstream),
    .before = 1
  )

# Validate against the known column list (catches typos early).
design <- doe_validate_design(design)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(design, out_path)

message("Wrote ", nrow(design), " design points to ", normalizePath(out_path))
message("Columns: ", paste(names(design), collapse = ", "))
message("Next:\n  Rscript scripts/doe_run.R --design ", out_path,
        " --reps 5 --out results/doe_example --seed 42 --cores 4")
