# Automotive Line Simulation

An industrial engineering portfolio project in R, simmer, tidyverse, and Shiny.
Vehicles flow through body shop, paint, final assembly, and inspection.
**All defaults are illustrative assumptions, not measured plant data.**

## Engineering questions

- How much labor does each station need, and where should operators be assigned?
- When does adding labor increase throughput, and when is equipment the constraint?
- How do support work, machine failures, and repair staffing affect flow?

## Model behavior

A vehicle occupies a bay while an operator loads it, an automatic cycle runs,
and an operator unloads it. Operators are released during the automatic cycle.
The bay remains occupied while waiting for an operator or automatic capacity.
Teams can serve two zones, the whole line, or individual stations. Team queues
are FIFO. Support activities either arrive at independent exponential intervals
or occur after every N completed station operations. Count-triggered work happens
after bay release; it is a separate operator task and delays that vehicle's
onward transfer. It does not represent a mandatory machine shutdown.

Each station has a pool of interchangeable automatic units equal to its bay
count. Independent failure processes interrupt this automatic capacity; unfinished
processing resumes after repair. Exponential up-time clocks advance during idle
and busy time and pause during that unit's own repair. MTBF here means mean
calendar **up-time**, excluding repair downtime. MTTR is mean active repair time;
waiting for a shared technician adds downtime. Repair does not consume production
operators. Loading and unloading are not interrupted by automatic-unit failures.
Vehicles are not tied to an individually identified physical machine.

Buffers and the external arrival queue are unlimited. Walking time, physical
blocking, shifts, scrap, and rework are not implemented. This is an open queueing
line: excessive release rates can cause growing WIP. An eight-hour experiment
with a warmup is not automatically a steady-state analysis.

## Interactive experiments

Run `shiny::runApp("app")` from the repository root. Set manual and automatic
cycle times, team sizes/assignments, repair staffing, MTBF/MTTR, and support work.
The Staffing comparison tab compares current staffing, an extra operator in the
first or last team, and the same headcount pooled across the line. Press
**Compare staffing scenarios** after changing inputs. With one team, the two
extra-operator alternatives are intentionally identical.

Throughput, lead time, WIP, labor productivity, operator utilization, and bay
occupancy expose different constraints. Highest bay occupancy is only a
**bottleneck candidate**: it includes waiting while holding a bay. Confirm an
actual throughput constraint by changing its capacity, manual time, or automatic
time while holding other assumptions constant.

Scenario tables report 95% Student-t confidence intervals for mean throughput
across independent replications. They are not prediction intervals for a shift.
Scenarios reuse seeds, but events are not assigned synchronized random streams;
do not assume a paired common-random-number variance reduction design.

## Run and validate

Dependencies: `simmer`, `tidyverse`, `shiny`, `bslib`, `bsicons`, `DT`.

```r
install.packages(c("simmer","tidyverse","shiny","bslib","bsicons","DT"))
shiny::runApp("app")
```

```sh
Rscript scripts/manufacturing_sim.R
Rscript scripts/staffing_experiments.R
Rscript tests/model_checks.R
Rscript tests/shiny_checks.R
```

Batch outputs are written to ignored `output/`. Staffing experiments preserve
scenario inputs and complete run results in an RDS with nested list-columns,
and export a flat comparison CSV. The Quarto document retains its simpler
legacy controls; its engine supports the expanded API, but its UI does not yet
expose staffing and reliability. Both deployed folders carry identical engine
copies; checks detect drift. Batch runs use the app's engine directly.

## Measurement definitions

- Throughput counts vehicle departures during `[warmup, horizon)` regardless of
  entry time. Lead times use the same departure cohort and can be censored by the
  finite horizon; queued, unfinished vehicles are not included in lead-time means.
- WIP is the integral of each vehicle's time in the system over the observation
  window, counting vehicles once even when they hold multiple resources.
- Resource averages integrate piecewise constant states over the full observation
  window, including the state carried into the warmup boundary.
- Failure events log occurrence times; repair events log repair start times and
  sampled durations, which may extend beyond the run. Inputs are not empirical
  MTBF/MTTR estimates. Reliability tables count failures over the entire run.

## Validation and remaining work

Checks cover reproducibility to numerical tolerance, warmup integration,
zero-completion runs, invalid staffing, old parameter structures, and analytic
single-bay capacity/labor utilization. Repairs and support work have directional
checks; Shiny server checks exercise all staffing layouts and comparison output.
Browser visual QA remains outstanding because the development session cannot
open a listening port. Existing production demos and images predate this branch.

Next: finite buffers and blocking, station-state decomposition, sensitivity-based
bottleneck confirmation, independent event random streams, measured input fitting,
and scenario deltas with uncertainty. Follow with a documented portfolio case:
problem → assumptions → validated baseline → alternatives → recommendation.

See DEPLOY.md for deployment. Keep credentials and rsconnect configuration out of
git. MIT license; see LICENSE.
