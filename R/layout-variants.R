# Local rank/route refinements cannot escape every coordinate-assignment basin.
# Try two deterministic starting points for small layouts, plus a third for V
# detours. Every trial uses the complete existing pipeline for the same `ranks`
# mode, with no recursive variant search, so supplied ranks stay fixed.
# Alignment groups and ordering = "none" retain their supplied constraints.
.refine_layout_variants <- function(graph, layer, geometry, ordering, sweeps,
  max_candidates = 2L, ranks = "automatic", trials = .layout_trials()) {
  if (!nrow(geometry$edges) || ordering == "none" || !sweeps || max_candidates <= 0L)
    return(geometry)
  work <- nrow(geometry$edges) * igraph::ecount(graph)
  max_candidates <- .metro_stage_attempts("restart_variants", work, max_candidates)
  if (max_candidates < 1L) return(geometry)
  baseline <- .measure_variant(geometry)
  if (prod(baseline$extent) == 0) return(geometry)
  current <- baseline
  for (variant in head(c("children", "reverse"), max_candidates)) {
    trial <- .metro_unrecorded(.layout_component(graph, layer, ordering, sweeps,
      ranks = ranks, variant = variant, trials = trials))
    candidate <- .measure_variant(trial)
    if (!.layout_variant_improves(candidate, current, baseline) ||
        !.variant_clear(graph, trial)) next
    geometry <- trial
    current <- candidate
  }
  # a third start for drawings with a V overshoot (a route leaving its
  # endpoints' transverse range by more than the P13 two-unit lane margin).
  # Same pipeline, but initial straightening measures lanes from the endpoints.
  # Tried only when such a route exists and the work budget allows a third run.
  overshoot <- .route_overshoot(geometry$edges)
  if (overshoot > 0 && floor(.metro_budget("restart_variants") / work) >= 3L) {
    trial <- .metro_unrecorded(.layout_component(graph, layer, ordering, sweeps,
      ranks = ranks, variant = "endpoint_lanes", trials = trials))
    if (.route_overshoot(trial$edges) < overshoot - 1e-8 &&
        .overshoot_variant_improves(.measure_variant(trial), current) &&
        .variant_clear(graph, trial))
      geometry <- trial
  }
  geometry
}

# Longest-path ranks inflate the span of bypass arrows, sink-aligned ranks can
# pull them in, and neither is best for every graph. Each other automatic
# layering runs the pipeline without rank refinement and is screened against
# the best start so far by the restart rule. Only a winner receives rank
# refinement and placement relaxation, and it is kept only when the rule still
# prefers it to the relaxed drawing from the component's own layering:
# relaxation can recover what the screen credited to the new layering
# (Thoemmes with sink-aligned ranks won with 12 bends against 11, and the
# relaxed default then had 9). Returns the relaxed drawing that is kept.
.refine_layering_variants <- function(graph, layer, geometry, ordering, sweeps,
  max_layerings = 2L, trials = .layout_trials()) {
  relaxed <- .relax_placements(graph, geometry)
  if (!nrow(geometry$edges) || max_layerings <= 0L || !.metro_stage("layering_variants"))
    return(relaxed)
  work <- nrow(geometry$edges) * igraph::ecount(graph)
  attempts <- .metro_stage_attempts("layering_variants", work, max_layerings)
  baseline <- .measure_variant(geometry)
  if (attempts < 1L || prod(baseline$extent) == 0) return(relaxed)
  current <- baseline
  winner <- NULL
  for (start in head(.layout_layering_starts(graph, layer), attempts)) {
    trial <- .metro_unrecorded(.layout_component(graph, start, ordering, sweeps,
      variant = "layering", trials = trials))
    candidate <- .measure_variant(trial)
    if (!.layout_variant_improves(candidate, current, baseline) ||
        !.variant_clear(graph, trial)) next
    winner <- trial
    current <- candidate
  }
  if (is.null(winner)) return(relaxed)
  winner <- .metro_unrecorded({
    if (.metro_stage("rank_refinement"))
      winner <- .refine_rank_assignment(graph, winner, ordering, sweeps, variant = "layering",
        trials = trials)
    .relax_placements(graph, winner)
  })
  # The relaxed default is now the drawing to beat, and the bend, length and
  # shared-track limits count from it. A crossing reduction keeps the extent
  # allowance the screen gave it, from the drawing the search started with:
  # relaxation compacts that drawing first, and from its compacted width a
  # random 12-node DAG would keep 8 crossings instead of 2 at 2 more bends.
  reference <- .measure_variant(relaxed)
  limits <- list(quality = reference$quality, extent = baseline$extent)
  if (.layout_variant_improves(.measure_variant(winner), reference, limits) &&
      .variant_clear(graph, winner)) winner else relaxed
}

# Route quality and routed extent of a component drawing, as the start rules
# compare them.
.measure_variant <- function(g) {
  routes <- unname(split(g$edges, g$edges$edge_id))
  extent <- c(diff(range(c(g$x, g$edges$x, g$edges$xend))),
    diff(range(c(g$y, g$edges$y, g$edges$yend))))
  list(quality = .routed_geometry_quality(routes), extent = extent)
}

# Four-unit station spacing, and routes clear of every station but their own.
.variant_clear <- function(graph, g) {
  nodes <- data.frame(node = igraph::V(graph)$name, x = g$x, y = g$y)
  if (any(as.numeric(stats::dist(nodes[c("x", "y")])) < 4 - 1e-8)) return(FALSE)
  routes <- unname(split(g$edges, g$edges$edge_id))
  all(vapply(routes, function(e) .route_clear(e,
    nodes[!nodes$node %in% c(e$from[1L], e$to[1L]), , drop = FALSE]), logical(1)))
}

# The minimum-span layering, then the other of the source- and sink-aligned
# layerings, skipping any that equals one already tried; ranks are compared as
# orders, not values.
.layout_layering_starts <- function(graph, layer) {
  normal <- function(z) as.integer(match(z, sort(unique(z))) - 1L)
  method <- if (identical(normal(layer), normal(.assign_layers(graph, "sinks")))) "longest" else "sinks"
  candidates <- list(.minimum_span_layers(graph, layer), .assign_layers(graph, method))
  seen <- list(normal(layer))
  starts <- list()
  for (candidate in candidates) {
    key <- normal(candidate)
    if (any(vapply(seen, identical, logical(1), key))) next
    seen[[length(seen) + 1L]] <- key
    starts[[length(starts) + 1L]] <- candidate
  }
  starts
}

# Summed transverse excursion beyond each route's endpoint range,
# ignoring the first `margin` units (normal bypasses around one station).
.route_overshoot <- function(edges, margin = 2) {
  if (!nrow(edges)) return(0)
  sum(vapply(split(edges, edges$edge_id), function(e) {
    e <- e[order(e$segment, method = "radix"), , drop = FALSE]
    ends <- c(e$x[1L], e$xend[nrow(e)]); xs <- c(e$x, e$xend)
    max(0, max(xs) - max(ends) - margin, min(ends) - min(xs) - margin)
  }, numeric(1)))
}

# Removing an overshoot may cost one bend only when it buys a clearly
# smaller drawing: no extra crossings, shared track, line length or either
# extent, and at least 15% less area. Otherwise the ordinary variant rule.
.overshoot_variant_improves <- function(candidate, current) {
  if (.layout_variant_improves(candidate, current, current)) return(TRUE)
  q <- candidate$quality; old <- current$quality
  all(q[c("crossings", "shared", "length")] <= old[c("crossings", "shared", "length")] + 1e-8) &&
    q[["bends"]] <= old[["bends"]] + 1 &&
    all(candidate$extent <= current$extent + 1e-8) &&
    prod(candidate$extent) <= prod(current$extent) * 0.85 + 1e-8
}

# Never spend crossings or shared track for space. A strict crossing reduction
# may spend one bend per removed crossing (at most two overall), 5% line length
# and 10% of either extent relative to the original drawing. At equal crossings,
# at most two extra bends require >=15% less area, no extra line length, and a
# better combined bend/ink/extent score. Small area gains cannot buy wiggles.
.layout_variant_improves <- function(candidate, current, baseline) {
  q <- candidate$quality; old <- current$quality; original <- baseline$quality
  if (any(q[c("crossings", "shared")] > old[c("crossings", "shared")] + 1e-8)) return(FALSE)
  fewer_crossings <- q[["crossings"]] < old[["crossings"]]
  if (fewer_crossings) {
    budget <- min(2, original[["crossings"]] - q[["crossings"]])
    return(q[["bends"]] <= original[["bends"]] + budget &&
      q[["length"]] <= original[["length"]] * 1.05 + 1e-8 &&
      all(candidate$extent <= baseline$extent * 1.1 + 1e-8))
  }
  if (any(candidate$extent > current$extent + 1e-8) ||
      q[["length"]] > old[["length"]] + 1e-8 ||
      q[["bends"]] > original[["bends"]] + 2) return(FALSE)
  area <- prod(candidate$extent); old_area <- prod(current$extent)
  if (q[["bends"]] > old[["bends"]] && area > old_area * 0.85) return(FALSE)
  score <- function(z) z$quality[["bends"]] + z$quality[["length"]] / 20 +
    sqrt(prod(z$extent)) / 4
  # A shared-track improvement takes priority within the same geometry guards.
  q[["shared"]] < old[["shared"]] - 1e-8 || score(candidate) < score(current) - 1e-8
}
