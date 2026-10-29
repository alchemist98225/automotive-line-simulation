#!/usr/bin/env Rscript
# Run from the repository root, or from any working directory:
#   Rscript scripts/manufacturing_sim.R
# Sources app/R/model.R, runs placeholder defaults for 32 replications,
# and writes all-replication plots plus the CSV/zip export bundle.

suppressPackageStartupMessages(library(tidyverse))

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else normalizePath(".")
model_candidates <- c(
  file.path(script_dir, "..", "app", "R", "model.R"),
  file.path(script_dir, "app", "R", "model.R"),
  "app/R/model.R"
)
model_path <- model_candidates[file.exists(model_candidates)][1]
if (is.na(model_path)) stop("Could not find app/R/model.R from ", script_dir)
source(model_path)

if (basename(script_dir) == "scripts") {
  OUT_DIR <- file.path(dirname(script_dir), "output")
} else {
  OUT_DIR <- "output"
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

station_labels <- c(
  body_shop = "Body shop", paint = "Paint",
  final_assembly = "Final assembly", inspection = "Inspection"
)
station_colors <- c(
  "Body shop" = "#2c7fb8", "Paint" = "#d95f0e",
  "Final assembly" = "#31a354", "Inspection" = "#756bb1"
)
label_station <- function(resource) {
  lab <- station_labels[as.character(resource)]
  ifelse(is.na(lab), as.character(resource), unname(lab))
}
station_factor <- function(x) factor(label_station(x), levels = unname(station_labels))

params <- default_params()
params$grid_dt_min <- 2

message("Running ", params$n_replications,
        " replications (PLACEHOLDER params, operators + reliability) ...")
res <- run_line_sim(params)

zip_path <- file.path(OUT_DIR, "sim_exports.zip")
write_sim_exports(res, dir = OUT_DIR, zip_path = zip_path)
message("Wrote CSV exports and ", zip_path)

per_rep <- res$utilization_by_rep %>% mutate(label = station_factor(station))
means <- res$station_metrics %>% mutate(label = station_factor(station))
p_util <- ggplot() +
  geom_jitter(data = per_rep, aes(x = label, y = avg_util),
              width = 0.12, height = 0, alpha = 0.35, size = 2.2, color = "#4a4a4a") +
  geom_errorbar(data = means, aes(x = label,
      ymin = pmax(0, avg_util_mean - avg_util_sd),
      ymax = pmin(1, avg_util_mean + avg_util_sd)),
      width = 0.15, linewidth = 0.6, color = "#2c7fb8") +
  geom_point(data = means, aes(x = label, y = avg_util_mean), size = 3.2, color = "#2c7fb8") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Bay occupancy (placeholder automotive line)",
       subtitle = paste0("Points = each replication; blue = mean ± SD | ",
                         params$n_replications, " reps | PLACEHOLDER"),
       x = NULL, y = "Bay occupancy") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(OUT_DIR, "utilization.png"), p_util, width = 8, height = 5, dpi = 120)

ops <- res$operator_by_rep %>% mutate(label = factor(sub("^operator_", "", resource)))
op_means <- ops %>% group_by(label) %>%
  summarise(mean = mean(avg_util), sd = sd(avg_util), .groups = "drop")
p_ops <- ggplot() +
  geom_jitter(data = ops, aes(x = label, y = avg_util),
              width = 0.12, height = 0, alpha = 0.35, size = 2.2, color = "#4a4a4a") +
  geom_errorbar(data = op_means, aes(x = label,
      ymin = pmax(0, mean - sd), ymax = pmin(1, mean + sd)),
      width = 0.15, linewidth = 0.6, color = "#756bb1") +
  geom_point(data = op_means, aes(x = label, y = mean), size = 3.2, color = "#756bb1") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Operator utilization (all replications)",
       subtitle = "Points = each replication; purple = mean ± SD | PLACEHOLDER",
       x = NULL, y = "Operator utilization") +
  theme_minimal(base_size = 16)
ggsave(file.path(OUT_DIR, "operator_utilization.png"), p_ops, width = 8, height = 5, dpi = 120)

qdf <- res$queue_timeseries %>%
  mutate(label = station_factor(resource),
         rep_station = paste(replication, resource, sep = "_"))
p_queue <- ggplot(qdf, aes(x = time, y = queue, color = label)) +
  geom_line(aes(group = rep_station), alpha = 0.12, linewidth = 0.4, na.rm = TRUE) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
              linewidth = 1.15, alpha = 0.18, span = 0.35, na.rm = TRUE) +
  scale_color_manual(values = station_colors) +
  labs(title = "Queue length over time (all replications)",
       subtitle = "Faint = each rep; bold = loess + ribbon | PLACEHOLDER",
       x = "Time (minutes)", y = "Queue length", color = "Station") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")
ggsave(file.path(OUT_DIR, "queue_length.png"), p_queue, width = 9, height = 5.5, dpi = 120)

p_wip <- ggplot(res$wip_timeseries, aes(x = time, y = wip)) +
  geom_line(aes(group = replication), alpha = 0.15, linewidth = 0.45, color = "#b35900", na.rm = TRUE) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
              linewidth = 1.2, color = "#b35900", fill = "#fdcc8a",
              alpha = 0.25, span = 0.35, na.rm = TRUE) +
  labs(title = "WIP over time (all replications)",
       subtitle = "Vehicle count, one per vehicle; faint = each rep; bold = loess | PLACEHOLDER",
       x = "Time (minutes)", y = "WIP (vehicles)") +
  theme_minimal(base_size = 16)
ggsave(file.path(OUT_DIR, "wip_over_time.png"), p_wip, width = 9, height = 5, dpi = 120)

down_rep <- res$downtime_by_rep %>% mutate(label = station_factor(station))
down_means <- res$station_metrics %>% mutate(label = station_factor(station))
p_down <- ggplot() +
  geom_jitter(data = down_rep, aes(x = label, y = downtime_fraction),
              width = 0.12, height = 0, alpha = 0.35, size = 2.2, color = "#4a4a4a") +
  geom_errorbar(data = down_means, aes(x = label,
      ymin = pmax(0, downtime_fraction_mean - downtime_fraction_sd),
      ymax = pmin(1, downtime_fraction_mean + downtime_fraction_sd)),
      width = 0.15, linewidth = 0.6, color = "#e34a33") +
  geom_point(data = down_means, aes(x = label, y = downtime_fraction_mean),
             size = 3.2, color = "#e34a33") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Downtime fraction by station (all replications)",
       subtitle = "Machine-time in repair after warmup | PLACEHOLDER MTBF/MTTR",
       x = NULL, y = "Downtime fraction") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(OUT_DIR, "downtime.png"), p_down, width = 8, height = 5, dpi = 120)

mean_ct <- mean(res$cycles$cycle_time, na.rm = TRUE)
p_cycle <- res$cycles %>%
  ggplot(aes(x = cycle_time)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#41b6c4", color = "white", alpha = 0.55) +
  geom_density(aes(group = replication), alpha = 0.25, linewidth = 0.4,
               color = "#7fcdbb", na.rm = TRUE) +
  geom_density(linewidth = 1.15, color = "#253494", na.rm = TRUE) +
  geom_vline(xintercept = mean_ct, linetype = "dashed", color = "#253494") +
  labs(title = "Vehicle cycle time distribution",
       subtitle = paste0("Faint = per-rep density; bold = pooled | mean = ",
                         round(mean_ct, 1), " min | PLACEHOLDER"),
       x = "Cycle time (minutes)", y = "Density") +
  theme_minimal(base_size = 16)
ggsave(file.path(OUT_DIR, "cycle_time_dist.png"), p_cycle, width = 8, height = 5, dpi = 120)

cat("\n========== HEADLINE (PLACEHOLDER EXAMPLE PARAMETERS) ==========\n")
cat(sprintf("Throughput (veh/hr):   mean=%.2f  sd=%.2f\n",
            res$headline$throughput_per_hr_mean, res$headline$throughput_per_hr_sd))
cat(sprintf("Cycle time mean (min): mean=%.2f  sd=%.2f\n",
            res$headline$cycle_time_mean_mean, res$headline$cycle_time_mean_sd))
cat(sprintf("Average WIP (veh):     mean=%.2f  sd=%.2f\n",
            res$headline$avg_wip_mean, res$headline$avg_wip_sd))
cat(sprintf("Bottleneck candidate (highest bay occupancy): %s\n", res$bottleneck))
cat("\nBay occupancy, queue, and downtime fraction:\n")
print(as.data.frame(res$station_metrics), row.names = FALSE)
cat("\nOperator utilization:\n")
print(as.data.frame(res$operator_by_rep %>%
  group_by(resource) %>%
  summarise(util_mean = mean(avg_util), util_sd = sd(avg_util), .groups = "drop")),
  row.names = FALSE)
cat("NOTE: Inputs are placeholder example values, not real plant data.\n")
