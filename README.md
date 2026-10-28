# Automotive Line Simulation

Discrete-event simulation of a serial automotive manufacturing line, written in R with [simmer](https://r-simmer.org/). Vehicles arrive and flow through four stations in order:

**Body shop → Paint → Final assembly → Inspection**

The same line is available three ways: a batch script that writes metrics and plots, an interactive [Shiny](https://shiny.posit.co/) app, and a [Quarto](https://quarto.org/) document with `server: shiny`. This repository is the portfolio foundation. The model runs; later polish (real station data, shifts, breakdowns, rework, scenario comparison) is listed under [Roadmap](#roadmap).

> **Placeholder example values.** Every capacity, cycle time, and arrival rate shipped here is an example chosen so the demo stays fast and shows a clear paint bottleneck. These are not real plant data. Replace them with measured station data before using any result to make a decision.

## Live demos

| Surface | Link |
| --- | --- |
| Shiny app | <https://2o6mmo-eddie-benjamin.shinyapps.io/automotive-line-sim/> |
| Quarto document | <https://2o6mmo-eddie-benjamin.shinyapps.io/automotive-line-sim-quarto/> |

![Shiny app on the placeholder defaults: throughput, cycle time, WIP, and paint as the bottleneck](docs/images/deploy_app.png)

*Deployed Shiny app, placeholder defaults (5 replications, base seed 20261027).*

## What the model does

Arrivals are exponential. Each station has a configurable number of parallel bays and a service-time distribution (exponential, or normal floored at 0.1 minutes). A vehicle seizes a bay, holds it for the drawn service time, releases it, and moves to the next station.

Each replication sets its own seed (`base_seed + replication - 1`) so a run is reproducible and safe to execute independently. Statistics are computed after a warmup window is discarded:

- Completions and throughput use arrivals that started at or after warmup and finished inside the horizon.
- Utilization and queue length are time-weighted from the resource monitor.
- Work-in-process is the sum across stations of average busy servers plus average queue.
- The bottleneck is the station with the highest mean utilization across replications.

Placeholder defaults:

| Station | Bays | Distribution | Mean (min) | SD (min) |
| --- | ---: | --- | ---: | ---: |
| Body shop | 2 | exponential | 6 | — |
| Paint | 1 | normal | 8 | 1.5 |
| Final assembly | 2 | normal | 7 | 1.0 |
| Inspection | 1 | exponential | 3 | — |

Mean interarrival time is 4 minutes. The horizon is 480 minutes with 60 minutes of warmup. The app and Quarto doc default to 5 replications; the batch script defaults to 10. Base seed is `20261027`.

## Features

- Serial four-station line with parallel bays and configurable service distributions.
- Multiple independent replications with fixed, per-replication seeds.
- Warmup discard before KPI calculations.
- Interactive controls for arrival rate, horizon, warmup, replication count, seed, capacity, and mean service time.
- Headline KPI cards, utilization with replication spread, queue-length trace for replication 1, cycle-time histogram, and a station metrics table.
- Batch run that writes a long metrics CSV and three PNG figures.
- Self-contained `app/` and `quarto/` directories, each publishable with `rsconnect` on its own.

## KPIs

| KPI | Definition in this model |
| --- | --- |
| Throughput | Completed vehicles per hour after warmup |
| Cycle time | Departure time minus arrival time; mean, and in the batch script p50 / p90 / p95 |
| Average WIP | Time-weighted vehicles on the floor (in service and in queue) |
| Queue length | Time-weighted queue at each station, plus a trace over time for replication 1 |
| Utilization | Time-weighted busy servers / capacity, mean and SD across replications |
| Bottleneck | Station with the highest mean utilization |

### Example plots

These figures are the batch script’s placeholder run (10 replications). Regenerating them writes new files under `output/`, which is gitignored; the copies below are the committed examples.

![Station utilization, mean plus or minus SD across 10 replications](docs/images/utilization.png)

![Queue length over time for replication 1](docs/images/queue_length.png)

![Cycle time distribution of completed vehicles](docs/images/cycle_time_dist.png)

### Quarto document

The Quarto doc is the same simulation as a single scrolling report. Because it uses `server: shiny`, the live copy is hosted on shinyapps.io.

![Quarto document: parameters, headline KPIs, and the same charts](docs/images/deploy_quarto.png)

## Tech stack

| Piece | Role |
| --- | --- |
| R 4.5 | Runtime |
| [simmer](https://r-simmer.org/) | Discrete-event engine |
| simmer.plot | Attached by the batch script |
| tidyverse | Wrangling and ggplot2 figures |
| tidymodels | Part of the stack for later modeling work. Runs today use simmer, simmer.plot, and tidyverse |
| shiny, bslib, bsicons | Interactive app (bslib theme, value boxes, icons) |
| DT | Station metrics table |
| Quarto (`server: shiny`) | Interactive document |
| rsconnect | Deploy to shinyapps.io or Posit Connect |

## Run locally

Install the R packages once:

```r
install.packages(c(
  "simmer",
  "simmer.plot",
  "tidyverse",
  "tidymodels",
  "shiny",
  "bslib",
  "bsicons",
  "DT",
  "rsconnect"
))
```

Quarto CLI is required only for the document: <https://quarto.org/docs/get-started/>.

### Batch script

From the repository root:

```bash
Rscript scripts/manufacturing_sim.R
```

Writes `output/metrics_summary.csv`, `output/utilization.png`, `output/queue_length.png`, and `output/cycle_time_dist.png`. The `output/` directory is gitignored.

### Shiny app

```bash
cd app
Rscript -e 'shiny::runApp(".")'
```

Open the URL printed in the console. The app runs once on load, then again each time you press **Run simulation**.

### Quarto document

```bash
cd quarto
quarto preview line_sim.qmd
```

## Deploy

`app/` and `quarto/` each carry their own `R/model.R`, so either directory can be published on its own:

```r
# Shiny app — working directory app/
rsconnect::deployApp(appDir = ".", appName = "automotive-line-sim")

# Quarto doc — working directory quarto/
rsconnect::deployDoc("line_sim.qmd", appName = "automotive-line-sim-quarto")
```

Account setup, Posit Connect, and the Quarto CLI publish commands are in [DEPLOY.md](DEPLOY.md). Keep `rsconnect/` and any `.Renviron` / `.env` files out of git; they can hold account tokens.

If you change the simulation engine, copy it to both folders before the next publish:

```bash
cp app/R/model.R quarto/R/model.R
```

## Project structure

```text
app/
  app.R                 # bslib Shiny app
  R/model.R             # simmer engine (deployed with the app)
quarto/
  line_sim.qmd          # Quarto doc, server: shiny
  R/model.R             # same engine (deployed with the doc)
scripts/
  manufacturing_sim.R   # standalone batch script (CSV + PNGs)
docs/images/            # deployed-app screenshots and example plots
DEPLOY.md               # shinyapps.io and Posit Connect notes
LICENSE                 # MIT, Eddie Benjamin, 2026
README.md
```

`scripts/manufacturing_sim.R` is the original standalone script: the simulation loop is inlined, and the script writes the CSV and PNGs itself. The app and the Quarto doc both call `run_line_sim(params)` and `default_params()` from their local copy of `R/model.R`.

`run_line_sim()` returns replication summaries, per-station stats, cycle times, the replication-1 resource trace, headline means and SDs, a station headline, the bottleneck name, and the parameter list that was used.

## Roadmap

- Replace placeholder capacities and cycle times with measured station data.
- Shift calendars, breaks, and planned downtime.
- Equipment breakdowns with MTBF and MTTR.
- Rework loops and scrap.
- Side-by-side scenario comparison (capacity, staffing, arrival rate).

## License

[MIT](LICENSE) © 2026 Eddie Benjamin.
