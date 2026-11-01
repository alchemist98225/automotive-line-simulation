# Design of experiments (DOE) CLI

Batch runner for systematically exploring the automotive line parameter space
outside Shiny. Feed it a table of design points (for example from
`tidyr::crossing()`), run many replications, and graph the compact results
together.

> **Illustrative only.** Capacities, crews, and times are placeholder examples.
> Results are illustrative only — placeholder parameters, not plant predictions.

## Quick start

```bash
# 1. Build an example design CSV (edit the crossing() inside the script)
Rscript scripts/doe_make_design.R
#    → designs/example_paint_ops.csv

# 2. Run the design (checkpointed; safe to interrupt and re-run)
Rscript scripts/doe_run.R \
  --design designs/example_paint_ops.csv \
  --reps 5 \
  --out results/doe_example \
  --seed 42 \
  --cores 4

# 3. Plot HPU / throughput / heatmap
Rscript scripts/doe_plot.R --in results/doe_example
```

## HPU definition

**HPU (hours per unit)** =

```text
(n_operators × observation_hours) / n_completed
```

where `observation_hours = (sim_length_min − warmup_min) / 60`.

This is the inverse of the model's `throughput_per_labor_hr`. Lower HPU means
more units per labor-hour. When a replication completes zero units, HPU is
recorded as `Inf` and dropped from confidence intervals.

Labor hours count every operator on the clock for the whole observation window
(busy or idle). Equipment time is not included in HPU.

## Design CSV columns

`design_id` is required and must be unique. Any other column that is omitted
falls back to the app default from `default_params()`. Unknown columns are
rejected with a clear error.

| Column | Default | Meaning |
| --- | ---: | --- |
| `design_id` | — | Unique id for the design point |
| `warmup_min` | 300 | Settling period (minutes) |
| `shift_length_min` | 480 | Measured shift length |
| `n_shifts` | 1 | Number of measured shifts |
| `arrival_mean_min` | 4 | Mean order spacing (minutes) |
| `release_policy` | `space` | `space`, `conwip`, or `scheduled` |
| `entry_buffer` | 3 | Entrance waiting positions |
| `wip_limit` | 20 | CONWIP cap (when policy is `conwip`) |
| `order_spacing` | `fixed` | `fixed` or `exponential` |
| `repair_technicians` | 1 | Shared maintenance crew |
| `op_upstream` | 2 | Zone A operators (body + paint) |
| `op_downstream` | 2 | Zone B operators (assembly + inspection) |
| `op_shared` | — | If set, one shared team of this size |
| `op_<station>` | — | Dedicated crew for that station |
| `cap_body_shop` | 2 | Body shop bays |
| `cap_paint` | 1 | Paint bays |
| `cap_final_assembly` | 2 | Final assembly bays |
| `cap_inspection` | 1 | Inspection bays |
| `mean_<station>` | (see app) | Automatic-cycle mean (minutes) |
| `buffer_paint` | 3 | Waiting positions before paint |
| `buffer_final_assembly` | 3 | Waiting positions before final assembly |
| `buffer_inspection` | 3 | Waiting positions before inspection |

Stations: `body_shop`, `paint`, `final_assembly`, `inspection`.

Operator mode precedence: dedicated `op_<station>` columns → `op_shared` →
zone pair `op_upstream` / `op_downstream`.

## Seeds

```text
seed(design_row, rep) = base_seed + (row_index - 1) × n_reps + (rep - 1)
```

`row_index` is the 1-based row order in the design CSV. Re-running with the
same `--seed`, `--reps`, and design file reproduces the same replications.

## Outputs

| File | Contents |
| --- | --- |
| `rep_metrics.csv.gz` | One row per `design_id × replication`, with design columns, throughput, output per shift, cycle-time mean/p50/p95, WIP, per-station occupancy (`util_*`), operator utilization, HPU |
| `summary.csv` | Per `design_id`: mean, SD, and 95% Student-t CI across reps |
| `run_info.json` | Args, git sha, timestamp, R/simmer versions, HPU definition |
| `chunks/<design_id>.csv.gz` | Per-point checkpoint used for resume |
| `detail/*.rds` | Optional raw engine tables (`--detail`) |
| `plots/*.png` | From `doe_plot.R` |

Raw per-entity arrival / repair logs are **not** saved by default. Pass
`--detail` only when you need them — they are large.

## Resume

`doe_run.R` writes each design point to `chunks/` as soon as it finishes. If
you interrupt a long run, re-invoke the same command with the same `--out`
directory. Finished design points are skipped; remaining points continue.

## Parallelism

Uses `parallel::mclapply` (fork), which works on macOS and Linux. Start with
`--cores` equal to physical cores minus one. Each design point runs its
replications inside one worker via `run_line_sim()`.

## Model reuse

All three scripts source `app/R/model.R`. Changing the engine updates the DOE
runner automatically — there is no forked copy of the simmer logic.
