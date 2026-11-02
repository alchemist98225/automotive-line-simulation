# Demand break-sweep CLI

Programmatic scenario: sweep offered demand and mark where each line setup
stops keeping up with arrivals. Parallel replications use `furrr` /
`future::multisession`. Reuses `app/R/model.R` through the DOE helpers — does
not fork the engine, and does not touch Quarto.

> **Illustrative only.** Capacities, crews, and times are placeholder examples.

## Quick start

```bash
Rscript scripts/break_sweep.R \
  --reps 10 \
  --out results/break_sweep \
  --seed 42 \
  --cores 7
```

Outputs under `--out`:

- `summary.csv.gz` — mean / CI / p10–p90 by setup × demand
- `rep_metrics.csv.gz` — one row per replication
- `breakpoints.csv` — break demand per setup
- `where_the_line_breaks.png` — headline chart
- `run_meta.csv` — workers, elapsed time, seed

## Break-point rule

Ascending offered demand, the **first** grid point where

```text
mean(throughput_per_hr) < 0.95 × offered_demand_per_hr
```

That fill-rate threshold is configurable with `--fill-threshold` (default 0.95).
Metrics use the post-warmup observation window only, same as the app / DOE.

## Setups compared

| Label | Paint capacity | Source |
| --- | ---: | --- |
| Default line | 1 | `default_params()` |
| 2-bay balanced | 2 | Guided Experiment (`opt_guided_experiment_params`) |

Common random numbers: at a given demand point, every setup shares the same
replication seeds (`doe_rep_seed` on the demand index, not the setup).

## Smoke test

```bash
Rscript tests/break_checks.R
```
