# optimize_recheck.R
# Optimize page recheck stage: build the deduplicated pool of candidates to
# re-run on shared seeds, then pick the best against the baseline. The server
# evaluates the pool one candidate per tick so progress keeps flowing.

# Top search genomes plus the one-operator cuts of the baseline, deduplicated.
# The baseline itself is evaluated separately (it sets the throughput floor).
opt_recheck_pool <- function(all_evals, baseline_genome, defs, n_top = 3L) {
  top <- all_evals %>%
    dplyr::group_by(genome_key) %>%
    dplyr::slice_min(fitness, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(fitness) %>%
    dplyr::slice_head(n = as.integer(n_top)) %>%
    dplyr::select(all_of(defs$name)) %>%
    purrr::pmap(function(...) as.integer(c(...)))

  # A 2-rep search draw can wrongly drop a cut that holds throughput, so the
  # one-operator cuts are always rechecked; the objective and floor are unchanged.
  op_cuts <- c("op_upstream", "op_downstream") %>%
    purrr::keep(~ .x %in% defs$name) %>%
    purrr::map(function(nm) {
      i <- match(nm, defs$name)
      if (baseline_genome[[i]] <= defs$lo[[i]]) return(NULL)
      g <- baseline_genome
      g[[i]] <- g[[i]] - 1L
      g
    }) %>%
    purrr::compact()

  base_key <- opt_genome_key(baseline_genome)
  pool <- c(top, op_cuts)
  keys <- purrr::map_chr(pool, opt_genome_key)
  pool[!duplicated(keys) & keys != base_key]
}

# Best of baseline + rechecked candidates; never worse than baseline.
opt_recheck_pick <- function(baseline_elite, elite_evals, baseline_genome) {
  all_cands <- c(list(baseline_elite), elite_evals)
  fits <- purrr::map_dbl(all_cands, "fitness")
  best_ev <- all_cands[[which.min(fits)]]
  if (best_ev$fitness > baseline_elite$fitness) best_ev <- baseline_elite
  improved <- best_ev$fitness < baseline_elite$fitness - 1e-12 &&
    !identical(opt_genome_key(best_ev$genome), opt_genome_key(baseline_genome))
  list(best_ev = best_ev, improved = isTRUE(improved))
}
