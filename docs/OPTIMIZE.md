# Line optimizer CLI

Integer genetic algorithm over a small set of staffing and buffer knobs.
Minimizes a **labor-equivalent cost per unit** against the app's guided Compare
Experiment baseline. Reuses `app/R/model.R` through the DOE helpers.

## Objective

```text
fitness = mean(HPU)
        + cost_hpu
        + penalty × max(0, thr_floor − mean(throughput_per_hr))

cost_hpu = (capacity_cost_h × extra paint capacity
          + queue_cost_h × extra queue spots) / mean(throughput_per_hr)
```

Extras are relative to the guided Experiment knobs and can be **negative**
(credit for reductions). Default `penalty = 10`. Lower fitness is better.

### Cost rates (illustrative)

| Flag | Default | Meaning |
|------|--------:|---------|
| `--capacity-cost` | 1.0 | Labor-hour per clock-hour per extra Paint capacity unit |
| `--queue-cost` | 0.1 | Labor-hour per clock-hour per extra Queue spot |

These are **illustrative** labor-hour equivalents for portfolio demos, not plant
costs. Outputs report `hpu_mean` and `cost_hpu` separately.

## Baseline

The optimizer baseline is the **guided Compare Experiment**:
`opt_guided_experiment_params()` mirrors `assemble_params("try")` in
`app/R/server_main.R` (pristine `default_params()` with **Paint capacity = 2**).
That set yields about **11 vehicles/hour** and HPU near **0.36**. Throughput
floor = that baseline's mean throughput (search reps during GA; elite reps for
the final CRN table).

Final `best_vs_baseline.csv` re-evaluates baseline and elites with the same
`--elite-reps` count and **shared CRN seeds** (`base_seed` … `base_seed+n-1`).
Search generations keep **fixed indexed seeds per genome** (no mid-search CRN
refresh). Mean-fitness spikes on raw population means come from rare
throughput-penalized genomes, not seed refresh; plots show mean among feasible
members when available.

## Decision variables

| Knob | Bounds | Default | Meaning |
|------|-------:|--------:|---------|
| `buffer_paint` | 0–8 | 3 | Queue before paint |
| `buffer_final_assembly` | 0–8 | 3 | Queue before final assembly |
| `buffer_inspection` | 0–8 | 3 | Queue before inspection |
| `op_upstream` | 1–5 | 2 | Body + paint operators |
| `op_downstream` | 1–5 | 2 | Assembly + inspection operators |
| `cap_paint` | 1–4 | 2 | Paint capacity |

Defaults match the guided Experiment.

## Noise handling

- Search uses 2–3 replications per candidate (CLI `--reps`).
- Common-random-number style seeds: each distinct genome gets a stable
  `cand_index`, then `seed = base + (cand_index−1)×n_reps + (rep−1)` via
  `doe_rep_seed()`. That schedule stays fixed for a genome across generations.
- After the last generation, the top `--elites` genomes are re-run with
  `--elite-reps` (default 15) on the shared CRN schedule and ranked on that
  fresher estimate. This **mitigates false positives**; it does not eliminate
  estimation error.

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
| `convergence.csv` | Best / mean fitness, HPU, cost_hpu, diversity per generation |
| `evaluations.csv.gz` | Every search evaluation with genome + cost columns |
| `elites.csv` | Top genomes after elite re-evaluation |
| `best_vs_baseline.csv` | HPU, cost_hpu, fitness, throughput with 95% Student-t CIs |
| `param_diff.csv` | Baseline vs best for each knob |
| `best_params_design.csv` | One-row DOE design CSV (`design_id` + knobs) |
| `best_run_parameters.csv` | Same shape as the app's `run_parameters.csv` download |
| `baseline_run_parameters.csv` | Baseline `run_parameters` table |
| `chunks/optimized.csv.gz` | Elite-rep metrics for the winner |
| `plots/convergence.png` | Best + mean (feasible) fitness by generation |
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
| `scripts/opt_space.R` | Knobs, cost rates, genome encode/decode |
| `scripts/opt_fitness.R` | Evaluate + cache (guided baseline, cost_hpu) |
| `scripts/opt_ga.R` | Integer GA loop + elite re-eval |
| `scripts/opt_run.R` | CLI runner |
| `scripts/opt_plot.R` | Convergence / param-diff plots |

## Mutation and diversity

Default `--p-mut` is **0.25** per gene. Mutation always changes at least one
locus (never a no-op). The initial population is **all random** (the guided
baseline is evaluated for `thr_floor` but not seeded into gen 0), and each
generation keeps a single elite. That avoids a one-step jump from the seeded
baseline to a near-optimum staffing cut, so the best line can improve across
generations. Each generation records `n_unique` genomes and mean pairwise L1
distance in `diversity.csv` / `plots/diversity.png`. Generation **0** is the
initial population (`generation0.csv`).

## Optimize page (Shiny)

The **Optimize** nav page runs the same GA core (`app/R/optimize_*.R`) sequentially
(one core) with a live convergence chart, Cancel, and **Send to Compare**.
Presets: Quick (about 5 min on 1 core; pop 10 × gen 6 × 2 reps, recheck top 3 at 15 reps) and Thorough (24 × 15 × 3, recheck top 5 at 15 reps). After the search, the page rechecks the baseline, the top N search genomes, and the one-operator cuts of the baseline (deduplicated) on 15 shared seeds. It runs one candidate per tick, so the progress bar and status keep updating and Cancel works during the recheck. The baseline genome stays in the pool so reported best is never worse than baseline. Search uses a soft throughput floor (95% of baseline) because 2-rep estimates are noisy; the recheck uses the hard CRN floor.
Cost rates are labeled illustrative. Baseline can be Current Parameters, the guided
balanced set, or a Compare set. CLI shims under `scripts/opt_*.R` source the app core.
