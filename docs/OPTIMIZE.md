# Line optimizer CLI

Integer genetic algorithm over a small set of staffing and buffer knobs.
Minimizes mean **HPU** (hours per unit) subject to mean throughput at least
matching the default baseline. Reuses `app/R/model.R` through the DOE helpers.

> Illustrative only — placeholder parameters, not plant predictions.

## Objective

```text
fitness = mean(HPU) + penalty × max(0, thr_floor − mean(throughput_per_hr))
```

`thr_floor` is the baseline mean throughput (same seed schedule, same search
replication count). Default `penalty = 10`. Lower fitness is better.


## Baseline

The optimizer baseline is pristine `default_params()` (1 paint capacity, zone
crews of 2+2, space release, fixed arrivals) — the same as a fresh Simulate /
Parameters run. That yields about **6 vehicles/hour**. The guided example’s
**2 paint capacity** experiment is closer to **11/hour**; do not confuse that
Compare experiment set with the optimizer baseline.

Final `best_vs_baseline.csv` re-evaluates baseline and elites with the same
`--elite-reps` count and **shared CRN seeds** (`base_seed` … `base_seed+n-1`).
Search generations still use indexed seeds per genome for exploration.

## Decision variables

| Knob | Bounds | Default | Meaning |
|------|-------:|--------:|---------|
| `buffer_paint` | 0–8 | 3 | Queue before paint |
| `buffer_final_assembly` | 0–8 | 3 | Queue before final assembly |
| `buffer_inspection` | 0–8 | 3 | Queue before inspection |
| `op_upstream` | 1–5 | 2 | Body + paint operators |
| `op_downstream` | 1–5 | 2 | Assembly + inspection operators |
| `cap_paint` | 1–4 | 1 | Paint capacity |

## Noise handling

- Search uses 2–3 replications per candidate (CLI `--reps`).
- Common-random-number style seeds: each distinct genome gets a stable
  `cand_index`, then `seed = base + (cand_index−1)×n_reps + (rep−1)` via
  `doe_rep_seed()`.
- After the last generation, the top `--elites` genomes are re-run with
  `--elite-reps` (default 15) and ranked on that fresher estimate. This
  **mitigates false positives** by re-screening top candidates at higher
  replication; it does not eliminate estimation error.

## Quick start

```bash
# Modest CLI search (~5–15 min wall on 4 cores at defaults)
Rscript scripts/opt_run.R \
  --pop 24 --gen 15 --reps 3 --elite-reps 15 \
  --out results/opt_example --seed 42 --cores 4

Rscript scripts/opt_plot.R --in results/opt_example
```

Tiny smoke (for tests):

```bash
Rscript scripts/opt_run.R --pop 4 --gen 2 --reps 2 --elite-reps 3 \
  --elites 2 --out results/opt_smoke --seed 1 --cores 2
```

## Outputs

| File | Contents |
|------|----------|
| `convergence.csv` | Best / mean fitness (and HPU) per generation |
| `evaluations.csv.gz` | Every search evaluation with genome columns |
| `elites.csv` | Top genomes after elite re-evaluation |
| `best_vs_baseline.csv` | HPU and throughput means with 95% Student-t CIs |
| `param_diff.csv` | Baseline vs best for each knob |
| `best_params_design.csv` | One-row DOE design CSV (`design_id` + knobs) |
| `best_run_parameters.csv` | Same shape as the app's `run_parameters.csv` download |
| `baseline_run_parameters.csv` | Baseline `run_parameters` table |
| `chunks/optimized.csv.gz` | Elite-rep metrics for the winner |
| `plots/convergence.png` | Best + mean fitness by generation |
| `plots/param_diff.png` | Dumbbell of changed knobs |

### Importing the best set

- **Design row:** `best_params_design.csv` overlays onto `default_params()` with
  `doe_params_from_row()` (same path the DOE runner uses).
- **App table:** `best_run_parameters.csv` matches the Simulate download. A future
  Optimize page will send this set into Compare as Experiment; until then, copy
  the knob values into Parameters or load the design row in a script.

## Parallelism

`parallel::mclapply` on macOS/Linux (`--cores`). Each worker evaluates one
genome (all of its replications inside `run_line_sim()`). Use 1 core on
hosts that disallow forks.

## File map

| File | Role |
|------|------|
| `scripts/opt_lib.R` | Entry: source DOE + optimizer modules |
| `scripts/opt_space.R` | Knobs, genome encode/decode |
| `scripts/opt_fitness.R` | Evaluate + cache |
| `scripts/opt_ga.R` | Integer GA loop + elite re-eval |
| `scripts/opt_run.R` | CLI runner |
| `scripts/opt_plot.R` | Convergence / param-diff plots |

## Mutation and diversity

Default `--p-mut` is **0.25** per gene. Mutation always changes at least one
locus (never a no-op). Each generation records `n_unique` genomes and mean
pairwise L1 distance in `diversity.csv` / `plots/diversity.png`. Generation
**0** is the initial population (`generation0.csv`).
