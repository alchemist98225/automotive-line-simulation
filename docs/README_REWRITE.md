# Automotive Line Simulation

A discrete-event model of a serial four-station line (Body shop → Paint → Final assembly → Inspection), written in R with [simmer](https://r-simmer.org/). The same engine powers a Shiny app, a Quarto companion document, and batch scripts for design of experiments, optimization, and scenario sweeps.

> **Illustrative parameters.** Capacities, crews, cycle times, and reliability numbers shipped here are examples so the demo runs quickly and shows a paint-bay constraint. Swap in your own data to study a real line.

**Live Shiny app:** https://2o6mmo-eddie-benjamin.shinyapps.io/automotive-line-sim/

## Problem

Before changing staffing, buffers, or paint capacity on a line, you want a fast way to ask: where does throughput stop keeping up with demand? what does that cost in labor hours per unit? which inputs move the needle? Commercial DES tools answer these well; this project shows the same questions as **plain R functions** you can map, parallelize, and optimize without leaving a statistical workflow.

## Model (short)

- Vehicles seize a bay, load with an operator, run an automatic cycle, unload, and move on.
- Operator pools (zones or shared), support tasks, and equipment failures with a shared repair tech are included.
- Metrics after a settling period: throughput/hr, lead time, WIP, HPU (labor-hours per completed unit), utilization, queues.
- Each replication uses seed `base_seed + replication - 1` (common random numbers across comparable runs).

Placeholder defaults: one 480-minute measured shift after **300 minutes** settling; paint capacity 1 on the default line (2 on the guided “2-bay balanced” set); two operators per zone.

## App pages

| Page | What it does |
| --- | --- |
| Simulate | Run the line; status chrome and KPIs |
| Parameters | Edit stations, crews, reliability, timing |
| Results | Bottleneck, flow, lead time, production schedule |
| Compare | Two parameter sets side by side |
| Optimize | Integer GA over queues, zone ops, and paint capacity (HPU + cost with a throughput floor) |
| About | Model notes |

## Optimizer

A small genetic search over buffers, operators, and paint capacity. Fitness is mean HPU (plus a cost-equivalent term) with a throughput floor at a chosen baseline (often the guided 2-bay set). Elite candidates are re-evaluated with more replications. Available in the app and as `scripts/opt_run.R`.

## Programmatic scenarios: where the line breaks

`scripts/break_sweep.R` sweeps offered demand for the Default and 2-bay setups in parallel (`furrr` multisession). Break point = first demand where mean throughput &lt; 95% of offered demand.

**From the overnight run (10 reps, seed 42, demand 3–18 by 0.5, 7 workers, ~3 min):**

| Setup | Break (veh/hr) | Throughput at break | Fill | Mean lead time |
| --- | ---: | ---: | ---: | ---: |
| Default line (paint 1) | **7.0** | 6.55 | 94% | 80 min |
| 2-bay balanced (paint 2) | **12.0** | 10.8 | 90% | 75 min |

Chart: see break-sweep handoff `where_the_line_breaks.png`.

## Validation

- **M/M/c:** standalone simmer trajectory vs Erlang-C (c=2, μ=1, ρ ∈ {0.5, 0.7, 0.85}, 50 reps). Queue wait / length / utilization relative errors about **0.1–1.2%**; almost all analytic values fall inside the simulation 95% CIs.
- **Little's Law:** on the full line, observed WIP vs λ×W across Default and 2-bay × four demands (12 reps). Mean absolute relative error about **1.5–5%**.

## Warm-up

Welch ensemble WIP from t=0 (30 reps) at 6 veh/hr offered demand. Current settling **300 min** is **adequate for the 2-bay set** (recommend ~46 min). On the default line near its break, the same strict rule suggests a longer settle — for warm-up studies near a knee, prefer a lighter demand or keep ≥300–400 min.

## Sensitivity (tornado)

One-at-a-time low/high around 2-bay @ **10 veh/hr** (below its break), 10 reps, CRN. Baseline ≈ **10.2 veh/hr**, HPU ≈ **0.39**. Largest HPU swings: **paint capacity**, **upstream operators**, **downstream operators**, then **demand**. Cycle-time means, CV, queues, and MTBF/MTTR scales were much smaller in this design.

## How to run

```bash
# Shiny app
R -e 'shiny::runApp("app")'

# Break sweep
Rscript scripts/break_sweep.R --reps 10 --out results/break_sweep --cores 7

# Validation / warm-up / tornado
Rscript scripts/validate_run.R --out results/validation --cores 7
Rscript scripts/warmup_run.R   --out results/warmup --reps 30 --cores 7
Rscript scripts/tornado_run.R  --out results/tornado --reps 10 --cores 7

# DOE / optimizer (see docs/DOE.md, docs/OPTIMIZE.md)
Rscript scripts/doe_run.R --design designs/example_paint_ops.csv --reps 5 --out results/doe
Rscript scripts/opt_run.R --pop 10 --gen 6 --reps 2
```

Tests: `Rscript tests/validation_checks.R`, `tests/warmup_checks.R`, `tests/tornado_checks.R`, plus the existing model/opt/shiny checks.

## What this is not

Not a replacement for a plant-calibrated digital twin or a full commercial DES suite. It is a transparent R model you can read, wrap, and optimize — useful for teaching, portfolio demos, and method development alongside tools that already do 3D animation and enterprise integration well.
