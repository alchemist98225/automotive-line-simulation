# Deploying the Automotive Line Simulator

Live deployments of this repository:

- Shiny app: <https://2o6mmo-eddie-benjamin.shinyapps.io/automotive-line-sim/>
- Quarto document: <https://2o6mmo-eddie-benjamin.shinyapps.io/automotive-line-sim-quarto/>

## Prerequisites

- R with: `shiny`, `bslib`, `bsicons`, `tidyverse`, `simmer`, `DT`, `zip`, and `rsconnect`. The app and the Quarto doc call `library(zip)` so shinyapps.io installs it for **Download all**. `tidymodels` is not required to run the simulation.
- A free [shinyapps.io](https://www.shinyapps.io/) account (or Posit Connect / Posit Connect Cloud).
- [Quarto](https://quarto.org/) for the interactive document.

`app/` is self-contained: `app.R` plus `R/model.R`, with relative paths only. Deploy from inside `app/`.

`quarto/` is self-contained the same way: `line_sim.qmd` plus `R/model.R`. Deploy from inside `quarto/`.

`app/R/model.R` and `quarto/R/model.R` are copies of the same engine so each folder can be published on its own. After any edit to the engine, copy it to both places before deploying:

```bash
cp app/R/model.R quarto/R/model.R
```

Do not commit credentials. `rsconnect::setAccountInfo()` writes a token under `rsconnect/`, and that directory is gitignored.

---

## Option A — shinyapps.io (Shiny app)

1. Create an account at <https://www.shinyapps.io/> and copy your token (Account → Tokens).

2. In R, authenticate once:

```r
rsconnect::setAccountInfo(
  name   = "<your-account-name>",
  token  = "<your-token>",
  secret = "<your-secret>"
)
```

3. Deploy from the app directory:

```r
setwd("path/to/app")   # directory that contains app.R
rsconnect::deployApp(
  appDir  = ".",
  appName = "automotive-line-sim",
  launch.browser = TRUE
)
```

Or from the shell:

```bash
cd /path/to/app
Rscript -e 'rsconnect::deployApp(appDir = ".", appName = "automotive-line-sim")'
```

---

## Option B — Posit Connect Cloud (alternative)

1. Push this repo to GitHub.
2. At <https://connect.posit.cloud/>, create a new content item from the GitHub repo and point it at the `app/` directory (`app.R`).
3. Connect Cloud builds and hosts the app; share the public URL.

---

## Quarto document (`server: shiny`)

The Quarto doc at `quarto/line_sim.qmd` is an **interactive Shiny document**. Host it on a Shiny-capable server such as shinyapps.io or Posit Connect.

### Deploy via rsconnect

```r
# From the quarto/ directory (needs R/model.R beside the qmd)
setwd("path/to/quarto")
rsconnect::deployDoc(
  "line_sim.qmd",
  appName = "automotive-line-sim-quarto"
)
```

### Deploy via Quarto CLI

```bash
cd path/to/quarto
quarto publish shinyapps line_sim.qmd
# or: quarto publish connect line_sim.qmd
```

Local preview (requires Quarto + Shiny):

```bash
cd path/to/quarto
quarto preview line_sim.qmd
# or: quarto serve line_sim.qmd
```

---

## Notes for employers / reviewers

- Defaults are **placeholder** manufacturing parameters for a portfolio demo, not real plant data. That includes crew sizes, MTBF/MTTR, and support-task times.
- The DES engine for the app and the Quarto doc is `R/model.R` (`run_line_sim()`, `default_params()`), copied into `app/` and `quarto/`. It models shared or dedicated operators, load/unload separated from the automatic cycle, support work, and MTBF/MTTR failures with shared repair technicians.
- Both UIs expose those inputs. Charts use every replication. **Download all** writes the plot tables plus `failure_events.csv`, `repair_events.csv`, `downtime_by_rep.csv`, and `operator_by_rep.csv`.
- `scripts/manufacturing_sim.R` sources `app/R/model.R`, runs 10 replications, and writes that zip and the PNG figures under `output/`. `scripts/staffing_experiments.R` and `scripts/timing_experiments.R` write scenario comparisons. `tests/` checks the engine and the Shiny server.
- The live shinyapps.io URLs at the top of this file predate operators, breakdowns, and the all-replication charts. Redeploy from `app/` and `quarto/` to publish this version.
- Keep replications modest (≤ 10) for snappy interactive demos.
