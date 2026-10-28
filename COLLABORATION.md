# Collaboration handoff

The staffing, reliability, and activity-timing engine (PR #2, `codex/simulation-enhancements`) is integrated with the all-replication plots and CSV/zip exports (PR #3) on `cursor/all-rep-exports-0f61`. `app/R/model.R` and `quarto/R/model.R` are identical copies. Quarto now exposes the same staffing, MTBF/MTTR, support-work, and activity-timing inputs as the Shiny app.

Still worth a review before anyone treats the numbers as plant guidance:

- Automatic machines at a station are interchangeable pooled capacity. A failure seizes one unit of that pool; a vehicle is not tied to a particular physical machine.
- Failure clocks advance in calendar up-time, including idle time, and pause during that unit's own repair. MTTR is active repair time. Waiting for a shared technician adds downtime.
- Normal activity draws are floored at zero, so the entered mean and SD describe the underlying normal, not the floored sample.
- Buffers are unlimited. Highest bay occupancy is a bottleneck candidate, not a confirmed constraint.
- Browser visual QA of the combined layout is still outstanding. No production redeploy is included in this branch.

Finite buffers, blocking, and sensitivity-based bottleneck confirmation remain future work.
