# Collaboration handoff

The staffing, reliability, and activity-timing engine (PR #2, `codex/simulation-enhancements`) is integrated with the all-replication plots and CSV/zip exports (PR #3) on `cursor/all-rep-exports-0f61`. `app/R/model.R` and `quarto/R/model.R` are identical copies. Quarto now exposes the same staffing, MTBF/MTTR, support-work, and activity-timing inputs as the Shiny app.

Still worth a review before anyone treats the numbers as plant guidance:

- Automatic machines at a station are interchangeable pooled capacity. A failure seizes one unit of that pool; a vehicle is not tied to a particular physical machine.
- Failure clocks advance in calendar up-time, including idle time, and pause during that unit's own repair. MTTR is active repair time. Waiting for a shared technician adds downtime.
- Normal activity draws are floored at zero, so the entered mean and SD describe the underlying normal, not the floored sample.
- Buffers are unlimited. Highest bay occupancy is a bottleneck candidate, not a confirmed constraint.
- Browser visual QA of the combined layout is still outstanding. No production redeploy is included in this branch.

Finite buffers, blocking, and sensitivity-based bottleneck confirmation remain future work.

Two-set comparison is the Shiny page **Compare two sets** (`app/R/comparison.R`).
It was brought forward from `codex/two-set-comparison` onto main, which already
has the sidebar accordion, stale-result flags, run-settings summary, and
ridgelines. No engine changes and no production deployment. `app/R/model.R` and
`quarto/R/model.R` stay identical. Quarto keeps its existing controls. Snapshot
imports use revision-specific input IDs so a reset cannot read stale controls.
Bay occupancy, operator utilization, and active repair on the comparison page
are ridgelines (a spike when a set has no spread). Replication summaries stay
horizontal rainclouds. Results are illustrative only and are not predictions
for any employer. Tom: review the new page on desktop and mobile.

Production plan/release work: codex/production-plan-release starts at merged PR #10.
Adds consecutive measured shifts after separate settling, planned-order spacing,
entrance-space and CONWIP admission, finite downstream buffers (blocking after
processing), release/order ledgers, shift totals, station wait intervals and total
repair downtime including technician waiting. Both engine copies remain identical.
Shiny and its two-set comparison expose the new configuration. Quarto retains its
legacy total-horizon controls. Review the reservation/after-release support-task
assumption and the explicit absence of off-shift calendars. No production deploy.
