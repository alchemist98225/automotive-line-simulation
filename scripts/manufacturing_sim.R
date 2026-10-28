#!/usr/bin/env Rscript
# =============================================================================
# manufacturing_sim.R
# Standalone runner for the automotive line DES.
# Sources the shared model (shiny/R/model.R), runs with PLACEHOLDER defaults,
# writes tidy CSV exports + multi-rep ggplot PNGs into output/.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# Resolve paths relative to this script so it works from any cwd.
args_full <- commandArgs(trailingOnly = FALSE)
file_arg  <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  normalizePath(".")
}

# Prefer sibling shiny/R/model.R (workspace layout); fall back to app/R (repo).
model_candidates <- c(
  file.path(script_dir, "shiny", "R", "model.R"),
  file.path(script_dir, "app", "R", "model.R"),
  file.path(script_dir, "R", "model.R"),
  file.path(script_dir, "..", "app", "R", "model.R")
)
model_path <- model_candidates[file.exists(model_candidates)][1]
if (is.na(model_path)) stop("Could not find model.R near ", script_dir)
source(model_path)

# Output directory: relative to repo root (parent of scripts/), or ./output.
if (basename(script_dir) == "scripts") {
  OUT_DIR <- file.path(dirname(script_dir), "output")
} else {
  OUT_DIR <- file.path(script_dir, "output")
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

station_labels <- c(
  body_shop      = "Body shop",
  paint          = "Paint",
  final_assembly = "Final assembly",
  inspection     = "Inspection"
)
station_colors <- c(
  "Body shop"      = "#2c7fb8",
  "Paint"          = "#d95f0e",
  "Final assembly" = "#31a354",
  "Inspection"     = "#756bb1"
)
label_station <- function(resource) {
  lab <- station_labels[as.character(resource)]
  ifelse(is.na(lab), as.character(resource), unname(lab))
}

# -----------------------------------------------------------------------------
# PARAMETERS (PLACEHOLDER EXAMPLE VALUES) — override default_params() slightly
# for a richer standalone batch (10 reps).
# -----------------------------------------------------------------------------
params <- default_params()
params$n_replications <- 10L
params$grid_dt_min    <- 2

message("Running ", params$n_replications,
        " replications of automotive line DES (PLACEHOLDER params) ...")
res <- run_line_sim(params)

# -----------------------------------------------------------------------------
# CSV exports (same set the Shiny app downloads)
# -----------------------------------------------------------------------------
zip_path <- file.path(OUT_DIR, "sim_exports.zip")
write_sim_exports(res, dir = OUT_DIR, zip_path = zip_path)
message("Wrote CSV exports + ", zip_path)

# -----------------------------------------------------------------------------
# Plots (PNG) — all replications represented
# -----------------------------------------------------------------------------
N_REPLICATIONS <- params$n_replications

# 1) Utilization: per-rep points + mean ± SD
per_rep <- res$utilization_by_rep %>%
  mutate(label = factor(label_station(station), levels = unname(station_labels)))
means <- res$station_metrics %>%
  mutate(label = factor(label_station(station), levels = unname(station_labels)))

p_util <- ggplot() +
  geom_jitter(
    data = per_rep, aes(x = label, y = avg_util),
    width = 0.12, height = 0, alpha = 0.35, size = 2.2, color = "#4a4a4a"
  ) +
  geom_errorbar(
    data = means,
    aes(x = label,
        ymin = pmax(0, avg_util_mean - avg_util_sd),
        ymax = pmin(1, avg_util_mean + avg_util_sd)),
    width = 0.15, linewidth = 0.6, color = "#2c7fb8"
  ) +
  geom_point(data = means, aes(x = label, y = avg_util_mean),
             size = 3.2, color = "#2c7fb8") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Station utilization (placeholder automotive line)",
    subtitle = paste0("Per-rep points + mean ± SD over ", N_REPLICATIONS,
                      " replications | PLACEHOLDER params"),
    x = NULL, y = "Utilization"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(OUT_DIR, "utilization.png"), p_util, width = 8, height = 5, dpi = 120)

# 2) Queue length: faint per-rep lines + loess
qdf <- res$queue_timeseries %>%
  mutate(
    label = factor(label_station(resource), levels = unname(station_labels)),
    rep_station = paste(replication, resource, sep = "_")
  )

p_queue <- ggplot(qdf, aes(x = time, y = queue, color = label)) +
  geom_line(aes(group = rep_station), alpha = 0.12, linewidth = 0.4, na.rm = TRUE) +
  geom_smooth(
    method = "loess", formula = y ~ x, se = TRUE,
    linewidth = 1.15, alpha = 0.18, span = 0.35, na.rm = TRUE
  ) +
  scale_color_manual(values = station_colors) +
  labs(
    title = "Queue length over time (all replications)",
    subtitle = "Faint = each rep (resampled grid); bold = loess + ribbon | PLACEHOLDER",
    x = "Time (minutes)", y = "Queue length", color = "Station"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_DIR, "queue_length.png"), p_queue, width = 9, height = 5.5, dpi = 120)

# 3) WIP over time
p_wip <- ggplot(res$wip_timeseries, aes(x = time, y = wip)) +
  geom_line(aes(group = replication), alpha = 0.15, linewidth = 0.45,
            color = "#b35900", na.rm = TRUE) +
  geom_smooth(
    method = "loess", formula = y ~ x, se = TRUE,
    linewidth = 1.2, color = "#b35900", fill = "#fdcc8a",
    alpha = 0.25, span = 0.35, na.rm = TRUE
  ) +
  labs(
    title = "WIP over time (all replications)",
    subtitle = "Faint = each rep; bold = loess + ribbon | PLACEHOLDER",
    x = "Time (minutes)", y = "WIP (vehicles)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(OUT_DIR, "wip_over_time.png"), p_wip, width = 9, height = 5, dpi = 120)

# 4) Cycle time: per-rep densities faint + pooled bold
mean_ct <- mean(res$cycles$cycle_time, na.rm = TRUE)
p_cycle <- res$cycles %>%
  ggplot(aes(x = cycle_time)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#41b6c4", color = "white", alpha = 0.55) +
  geom_density(aes(group = replication), alpha = 0.25, linewidth = 0.4,
               color = "#7fcdbb", na.rm = TRUE) +
  geom_density(linewidth = 1.15, color = "#253494", na.rm = TRUE) +
  geom_vline(xintercept = mean_ct, linetype = "dashed", color = "#253494") +
  labs(
    title = "Vehicle cycle time distribution",
    subtitle = paste0("Faint = per-rep density; bold = pooled across ",
                      N_REPLICATIONS, " reps | mean = ", round(mean_ct, 1),
                      " min | PLACEHOLDER"),
    x = "Cycle time (minutes)", y = "Density"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(OUT_DIR, "cycle_time_dist.png"), p_cycle, width = 8, height = 5, dpi = 120)

message("Wrote plots to ", OUT_DIR)

# -----------------------------------------------------------------------------
# Console headline
# -----------------------------------------------------------------------------
cat("\n========== HEADLINE METRICS (PLACEHOLDER EXAMPLE PARAMETERS) ==========\n")
cat(sprintf("R simmer DES | %d reps | sim length %.0f min | warmup %.0f min\n",
            N_REPLICATIONS, params$sim_length_min, params$warmup_min))
cat(sprintf("Stations: %s\n", paste(params$stations$name, collapse = " -> ")))
cat(sprintf("Throughput (veh/hr):     mean=%.2f  sd=%.2f\n",
            res$headline$throughput_per_hr_mean, res$headline$throughput_per_hr_sd))
cat(sprintf("Cycle time mean (min):   mean=%.2f  sd=%.2f\n",
            res$headline$cycle_time_mean_mean, res$headline$cycle_time_mean_sd))
cat(sprintf("Cycle time p50 / p90 / p95 (min, across-rep means): %.2f / %.2f / %.2f\n",
            res$headline$cycle_time_p50_mean, res$headline$cycle_time_p90_mean,
            res$headline$cycle_time_p95_mean))
cat(sprintf("Average WIP (veh):       mean=%.2f  sd=%.2f\n",
            res$headline$avg_wip_mean, res$headline$avg_wip_sd))
cat("\nPer-station utilization & avg queue (across-rep means):\n")
print(as.data.frame(res$station_metrics), row.names = FALSE)
cat("=======================================================================\n")
cat("NOTE: All inputs are placeholder example values, not real plant data.\n")
