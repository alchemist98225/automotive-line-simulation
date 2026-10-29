# DRI demonstration release review

Review date: October 29, 2026. Verdict: **CONDITIONAL GO**.

The simulation is credible as an illustrative manufacturing demonstration. I found no release-blocking engine defect in the targeted tests. Before sharing, merge and deploy the small chart-label fix in this review, then complete a desktop/mobile live smoke test. This is a short release check, not a request for another feature cycle.

## Release identity

- Reviewed main: `bee8b1baf6cb3a8866891e52ac1e7c58b6a4c637`, merged PR #12. It includes PR #11 (production shifts, buffers and release policies) and PR #10 (plot readability). No open PRs were present at the start of review. The starting local working tree was clean.
- Review patch branch: `codex/dri-release-review-fixes`; based on that exact main. Changes only the comparison backlog palette, its regression check, and outdated arrival-policy wording in About.
- Intended public URL: https://2o6mmo-eddie-benjamin.shinyapps.io/automotive-line-sim/
- Live HTTP returned 200 and 118,102 bytes of HTML. It contains the production schedule page, admission controls, and PR #12's guided-comparison wording. This is evidence of the recent UI, **not proof of an exact deployed Git commit**. No commit identifier is exposed in the retrieved page.
- Interactive browser testing was unavailable: the web fetch service could not open this URL; direct HTTP retrieval worked, but the installed Chrome headless launch failed. Desktop/mobile interactions, chart rendering, live scenario execution, and session stability are therefore **not certified** by this review. Mobile viewport metadata and a collapsible mobile sidebar are present, but markup alone does not establish mobile usability.

## Tests and observed results

All six existing scripts passed against reviewed main: `model_checks.R`, `timing_checks.R`, `production_checks.R`, `shiny_checks.R`, `production_shiny_checks.R`, and `comparison_checks.R`. Comparison checks also passed after the narrow patch, with a regression assertion that renamed sets survive chart factor conversion.

| Check | Result |
|---|---|
| Deterministic analytic capacity | One bay with 2 min load + 6 automatic + 2 unload gives 6 vehicles/hour and 40% operator utilization; adding operators does not exceed machine capacity. |
| Shifts and warmup | Separate settling and measured shifts; 1,440 min settling + 20 × 480 min produces 20 measured rows per replication. Shift completions reconcile with the run total; weighted WIP reconciles. |
| Admission and conservation | Per-shift WIP and external backlog balance exactly. CONWIP is bounded at exact ledger event times; no duplicated orders or admission before order creation. |
| Buffers and blocking | Zero waiting positions give no positive-duration downstream queue. Two-position buffers respect their queue bound. Completed vehicles block upstream bays; admission controls keep excess work outside WIP. |
| Sampling | Fixed and empirical sampling, including a singleton vector, pass. 50,000-draw lognormal mean/SD and floored-normal expected mean checks pass. Invalid empirical observations fail deliberately. |
| Reproducibility | Same parameters/seeds reproduce summary results. Comparison imports, saved results, stale flags, editable titles, guided pair loading and exports are exercised by Shiny tests. |
| Failures and support labor | Frequent repairs and heavy support work reduce output. Shared repair queues increase total downtime, including failures waiting at the horizon. |
| Independent reliability benchmark | Eight runs of 20,000 measured minutes, one machine, mean uptime 100 min and repair 20 min: observed availability 83.04%, versus theoretical 83.33%. |
| Default and capacity experiment | Fifteen runs: default output 49.3 vehicles/shift (6.16/hour); two paint bays 84.7 (10.59/hour). Mean production lead time 98.1 → 76.5 min; WIP 10.6 → 14.6; ending external backlog 71.7 → 28.7. Candidate moves from paint to final assembly. These are descriptive results, not a significance test. |
| Local responsiveness | An earlier review measured the previous 15-run, 60-minute settling default at about 4.2 seconds on that computer. On a later VM, that same previous setting took 11.5 seconds, and the current default (32 runs, 300 minutes of settling, 780-minute horizon) took 32.4 seconds, about 2.8 times as long. That is a material increase in first-load and rerun time. It does not establish shinyapps.io latency; a small hosted instance will be slower than a fast laptop, but the work is still one simulation rather than a change in architecture. |

One local Shiny test emitted a filesystem warning about its Sass cache location, then passed. This is not evidence of a hosted app failure. The main chart tests reported zero manual-scale level-mismatch warnings; they did not cover the renamed backlog chart until the new regression.

## Release-blocking finding and minimum remaining work

**Comparison backlog chart loses renamed scenarios.** PR #12 introduces editable/guided titles. The backlog renderer still uses the default palette names, `Set 1` and `Set 2`. Its factor conversion turns renamed scenario values into missing values, so the chart can be blank or misleading precisely during the guided demonstration. A focused reproduction retained 0 of 8 renamed labels. The patch passes the current palette into that renderer; underlying model results are unchanged. A regression renders the backlog after renaming and rejects missing scenario labels.

The About page also incorrectly says all arrivals are exponential. The patch describes both spacing choices and admission policies.

Minimum release procedure:

1. Review, merge and deploy the patch using the existing Shiny app URL.
2. Confirm the deployed bundle corresponds to the merged release commit using deployment records; this review cannot infer that identity from HTTP alone.
3. In a fresh desktop browser session: load defaults, click the guided paint example, open Compare, and verify both backlog curves and titles. Change a setting and confirm stale-result messaging; rerun and download a CSV.
4. On a phone: expand controls, run a modest scenario, navigate all three result pages, and check legends, plot labels, horizontal table scrolling, and sidebar access. Confirm no blank charts, errors, or disconnections.

After those checks pass, **GO: share the application**. Do not delay for additional manufacturing features.

## Interpretation and applicability

**Verification is not DRI validation.** Tests support correct execution of the implemented assumptions. There is no DRI routing, cycle-time, reliability, staffing or production-history dataset here, and no demonstrated calibration or validation against DRI performance. Present the app as an example of the author's modeling and problem-solving approach, never as a prediction of DRI output.

Key assumptions to state accurately:

- Shifts are consecutive operating periods with WIP carryover, not a calendar with nights, weekends or off-shift maintenance. Replications are independent experiments; shifts within one run are correlated.
- Planned order rate is offered work, not achieved throughput. Defaults intentionally overload the original line. Bounded WIP can coexist with growing external backlog. Warmup cannot make an overloaded order backlog stationary. The default settling period is 300 minutes; that length has not been established as a sufficient steady-state warmup. The default replication count is 32 and the default CONWIP limit is 20 vehicles. The capacity numbers in the table above were measured under the previous 15-run, 60-minute settling defaults.
- Production lead time begins at admission; order lead time includes external waiting. Only completed departures contribute to lead-time distributions. Unfinished work is excluded, so severe congestion and short runs can bias the apparent lead-time picture.
- Bay occupancy includes manual work, repair-related waiting and blocking; it is not automatic-machine utilization or OEE. The candidate score subtracts blocked time but is still a heuristic. Confirm a constraint by intervention and throughput response.
- Failure clocks use calendar uptime including idle time. MTTR is sampled active repair time; total unavailability also includes waiting for a technician. Automatic capacity is interchangeable within a station; there is no physical-machine/vehicle identity. Failure interruptions resume work rather than scrapping/restarting it.
- Operators leave during automatic processing and return to unload. Shared operators serve FIFO; no walking time, skills matrix, breaks or shift calendar is modeled. Count-triggered support work occurs after bay release and can retain a reserved downstream position; it is not a station shutdown.
- Normal draws are floored at zero; entered normal mean/SD are underlying distribution parameters. Default timing values make this distinction small, but high SD can materially change the realized mean.
- Reused seeds reproduce each scenario but do not synchronize draws by event across changed models. Per-set throughput confidence intervals are not confidence intervals for the difference. Vehicle-level pooled densities are descriptive; they do not make vehicles independent statistical replications.
- Loess ribbons use correlated time-grid observations and are visual smoothers, not validated uncertainty bands for future production. Use run-level distributions and replication summaries for uncertainty judgments.

## Engineering-manager assessment

The strongest evidence is the combination of explicit machine/labor separation, admission versus backlog accounting, finite-buffer blocking, technician queues, reproducible experiments, and the guided capacity intervention. It demonstrates manufacturing judgment beyond a generic animation. The fact that WIP can increase while throughput improves is correctly explained rather than hidden.

The largest credibility risks are overclaiming DRI applicability, treating bay occupancy as productive utilization, presenting a candidate as a proven bottleneck, interpreting a smoothing ribbon as a forecast interval, and sharing before checking the actual deployed guided workflow. The interface has sensible grouped controls, first-load results, a guided experiment, saved-run provenance, and comparisons that freeze both parameter sets. It remains dense for a first-time visitor; recommend the guided example as the first action. Visual/mobile usability remains unverified in this session.

Optional improvements can wait: rename the remaining “Arrivals & run” heading to “Production plan”; hide admission controls that are inactive under the selected policy; expose a release commit stamp; replace loess ribbons with replication-wise quantile bands if uncertainty becomes a central story; streamline extra staffing/timing comparison entry points; refresh older collaboration notes that still describe buffers as unlimited. None requires adding a new model subsystem before sharing.

## Email description

I put together an interactive manufacturing simulation to explore how staffing, machine reliability, buffer capacity, and work-release decisions affect throughput and flow. It uses an illustrative automotive assembly line rather than DRI process data, and includes a guided comparison showing the effect of relieving a capacity constraint.
