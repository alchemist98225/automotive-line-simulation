# Companion video storyboard + narration (≈3.5 minutes)

Tone: plain, confident, no hype. Show the app and one chart from each overnight study. Real numbers only.

---

## 0:00–0:25 — Cold open (app UI)
**Visual:** Shiny Optimize or Simulate page; zoom on the line story (four stations).
**Narration:**
“This is a discrete-event model of a four-station automotive line, written in R with simmer. Same engine in a Shiny app, a Quarto doc, and batch scripts. The point isn’t fancy 3D — it’s that the whole plant is just an R function you can map over and optimize.”

## 0:25–0:55 — Model in one breath
**Visual:** Parameters page → paint capacity 1 vs 2; short Simulate run.
**Narration:**
“Vehicles move Body shop, Paint, Final assembly, Inspection. Operators load and unload; automatic cycles can fail and wait on a shared repair tech. We measure throughput, lead time, WIP, and HPU — labor hours per finished vehicle — after a settling period.”

## 0:55–1:35 — Where the line breaks
**Visual:** `where_the_line_breaks.png` (throughput panel, then lead-time panel).
**Narration:**
“With furrr we sweep offered demand for two setups. Break point: first demand where mean throughput falls below ninety-five percent of demand. On this illustrative model the default line — one paint bay — breaks around **seven vehicles per hour**. The guided two-bay set holds to about **twelve**. Lead time blows up at the same place. That’s one chart that tells the capacity story.”

## 1:35–2:10 — Validation, briefly
**Visual:** `mmc_check.png` then `littles_law.png`.
**Narration:**
“Before we trust scenario sweeps, two checks. A standalone M/M/c simmer run sits within about one percent of Erlang-C queue formulas across three utilizations. On the full line, Little’s Law — observed WIP versus throughput times lead time — lands within a few percent across eight scenarios. Not a plant calibration; it is a sanity check that the accounting matches the theory.”

## 2:10–2:40 — Warm-up + tornado
**Visual:** `welch_warmup.png`, then `tornado.png` (HPU panel).
**Narration:**
“Welch’s method on ensemble WIP says the app’s three-hundred-minute settle is comfortable for the two-bay line at moderate demand. Near the default line’s knee, be more careful. A one-at-a-time tornado around the two-bay set at ten per hour shows HPU most sensitive to **paint capacity and operator counts**, then demand — cycle-time noise and MTBF tweaks matter less in this design.”

## 2:40–3:20 — Optimize + close
**Visual:** Optimize page Run accordion; quick convergence sketch or finished summary table.
**Narration:**
“The optimizer is a small genetic search over queues, staffing, and paint capacity. It minimizes labor-hours per unit with a throughput floor, averaging several replications and re-checking elites on fresh effort. Commercial tools already do animation and enterprise hooks well; what this stack adds is a model you can read, parallelize with furrr, and plug into the same tidyverse workflow you use for everything else.”

## 3:20–3:35 — End card
**Visual:** Live URL + repo name.
**Narration:**
“Links to the live app and the scripts are in the description. Parameters are illustrative — bring your own station data when you want plant numbers.”

---

### Shot list / assets
1. App Simulate / Optimize screenshots (live or local)
2. `/workspace/sim/break_handoff/where_the_line_breaks.png`
3. `/workspace/sim/validation_handoff/mmc_check.png`
4. `/workspace/sim/validation_handoff/littles_law.png`
5. `/workspace/sim/warmup_handoff/welch_warmup.png`
6. `/workspace/sim/tornado_handoff/tornado.png`

### On-screen callouts (exact figures)
- Break: Default **7.0**/hr; 2-bay **12.0**/hr (10 reps, seed 42)
- M/M/c |rel err| ≲ **1.2%** (50 reps)
- Little |rel err| ~ **1.5–5%**
- Warm-up: 2-bay recommend ~**46** min; keep **300** for demos
- Tornado baseline ~**10.2**/hr, HPU ~**0.39**; top drivers paint cap & operators
