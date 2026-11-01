# doe_checkpoint.R
# Per-design_id chunk CSV read/write under out/chunks/ for DOE resume.
# Sourced by doe_lib.R.

doe_chunk_path <- function(out_dir, design_id) {
  # Safe filename from design_id
  safe <- gsub("[^A-Za-z0-9._-]+", "_", design_id)
  file.path(out_dir, "chunks", paste0(safe, ".csv.gz"))
}

doe_write_chunk <- function(metrics, out_dir, design_id) {
  dir.create(file.path(out_dir, "chunks"), recursive = TRUE, showWarnings = FALSE)
  path <- doe_chunk_path(out_dir, design_id)
  tmp <- paste0(path, ".tmp.gz")
  # Atomic replace: write to temp then rename into place.
  readr::write_csv(metrics, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
  invisible(path)
}

doe_read_chunk <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}
