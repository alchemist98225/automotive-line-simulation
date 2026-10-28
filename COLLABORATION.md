# Collaboration handoff

Baseline: main commit 3c1563e. Development branch: codex/simulation-enhancements.
Codex changes the simmer engine, Shiny staffing/reliability controls, batch
experiments, and validation. Use separate branches and review before merging.
Do not overwrite another agent's branch. No production deployment is included.

Suggested Tom follow-up: review pooled-machine failure assumptions; improve
Shiny layout and run browser visual QA; expose expanded controls in Quarto.
Review README measurement definitions before interpreting outputs. Finite
buffers/blocking and sensitivity-based bottleneck confirmation remain future work.

Timing follow-up: the same branch now supports fixed/distribution/empirical
activity timing through four list-columns. New Shiny controls accept pasted
observations and add expected-time-matched variability presets. Review normal
zero-floor semantics and UI layout; see README Activity timing methods. Repair
and failure intervals retain their existing exponential models.
