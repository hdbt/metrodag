# Reconsider automatic ranks after local compaction has exhausted its room.
# Move one node towards its neighbours, rebuild the complete component, and
# keep improving within a shared rebuild budget. Crossings and bends cannot
# increase. A crossing reduction may spend up to 5% more line length and 10%
# more of either extent, measured against the input to this refinement, for
# a smaller area. Shared tracks only gain length by changing unit orientation.
.refine_rank_assignment <- function(graph, geometry, ordering, sweeps, max_candidates = 3L,
  variant = "default", trials = .layout_trials()) {
  if (!nrow(geometry$edges) || max_candidates <= 0L) return(geometry)
  # A full rebuild is more expensive than moving a bend. Bound the number of
  # rebuilds by routed steps times edges; large components skip this search.
  work <- nrow(geometry$edges) * igraph::ecount(graph)
  max_candidates <- .metro_stage_attempts("rank_refinement", work, max_candidates)
  if (max_candidates < 1L) return(geometry)
  extent <- function(g) c(
    width = diff(range(c(g$x, g$edges$x, g$edges$xend))),
    depth = diff(range(c(g$y, g$edges$y, g$edges$yend))))
  bounds <- extent(geometry)
  if (prod(bounds) == 0) return(geometry)
  original_bounds <- bounds
  original_length <- NULL
  seen <- character()
  attempts <- 0L
  repeat {
    candidates <- .rank_assignment_candidates(graph, geometry$layer)
    candidates <- Filter(function(z) !paste(z, collapse = ",") %in% seen, candidates)
    if (!length(candidates) || attempts >= max_candidates) break
    improved <- FALSE
    limit <- NULL
    for (layer in head(candidates, max_candidates - attempts)) {
      seen <- c(seen, paste(layer, collapse = ","))
      attempts <- attempts + 1L
      # Fixed trial ranks prevent recursive searches and fan placement from
      # undoing the proposed move. Supplied user ranks never enter here.
      trial <- .metro_unrecorded(.layout_component(graph, layer, ordering, sweeps,
        ranks = "fixed", variant = variant, trials = trials))
      size <- extent(trial)
      if (prod(size) >= prod(bounds) || any(size > original_bounds * 1.1 + 1e-8)) next
      nodes <- data.frame(node = igraph::V(graph)$name, x = trial$x, y = trial$y)
      if (any(as.numeric(stats::dist(nodes[c("x", "y")])) < 4 - 1e-8)) next
      routes <- unname(split(trial$edges, trial$edges$edge_id))
      clear <- all(vapply(routes, function(e) .route_clear(e, nodes[!nodes$node %in% c(e$from[1L], e$to[1L]), , drop = FALSE]), logical(1)))
      if (!clear) next
      if (is.null(limit)) limit <- .routed_geometry_quality(
        unname(split(geometry$edges, geometry$edges$edge_id)))
      if (is.null(original_length)) original_length <- limit[["length"]]
      quality <- .routed_geometry_quality(routes)
      if (any(quality[c("crossings", "bends")] >
          limit[c("crossings", "bends")] + 1e-8)) next
      if (any(size > bounds + 1e-8) &&
          quality[["crossings"]] >= limit[["crossings"]]) next
      if (quality[["shared"]] > limit[["shared"]] + 1e-8) {
        # A diagonal shared unit is sqrt(2) long, a vertical unit is one. Permit
        # that orientation change only when crossing reduction retains the same
        # or fewer shared grid steps, counting every overlapping edge pair.
        shared_steps <- function(routes) {
          keys <- .route_grid_keys(do.call(rbind, routes))
          count <- tabulate(match(keys, unique(keys)))
          sum(count * (count - 1) / 2)
        }
        if (quality[["crossings"]] >= limit[["crossings"]] ||
            shared_steps(routes) > shared_steps(unname(split(geometry$edges,
              geometry$edges$edge_id)))) next
      }
      length_limit <- if (quality[["crossings"]] < limit[["crossings"]])
        original_length * 1.05 else limit[["length"]]
      if (quality[["length"]] > length_limit + 1e-8) next
      geometry <- trial
      bounds <- size
      improved <- TRUE
      break
    }
    if (!improved) break
  }
  geometry
}

# A node can move between its latest parent and earliest child. Prefer the
# endpoint that reduces total incident rank span; break ties by degree/name.
# Balanced intermediates also try either legal endpoint: equal rank span can
# still give the router a less spacious arrangement.
# This also handles sink-aligned layouts by moving late sinks upstream.
.rank_assignment_candidates <- function(graph, layer) {
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  names <- igraph::V(graph)$name
  target <- layer
  gain <- degree <- numeric(length(layer))
  for (i in seq_along(layer)) {
    parents <- ends[ends[, 2L] == i, 1L]
    children <- ends[ends[, 1L] == i, 2L]
    degree[i] <- length(parents) + length(children)
    balance <- length(children) - length(parents)
    if (balance > 0) target[i] <- min(layer[children]) - 1L
    if (balance < 0) target[i] <- max(layer[parents]) + 1L
    gain[i] <- (target[i] - layer[i]) * balance
  }
  ids <- which(gain > 0)
  # Moving a lightly connected parent is less disruptive than rebuilding a
  # large fan for the same per-edge rank saving. Spend the bounded trials there.
  ids <- ids[order(-gain[ids] / degree[ids], degree[ids], names[ids], method = "radix")]
  candidates <- lapply(ids, function(i) {
    candidate <- layer
    candidate[i] <- target[i]
    stats::setNames(match(candidate, sort(unique(candidate))) - 1L, names)
  })
  for (i in order(names, method = "radix")) {
    parents <- ends[ends[, 2L] == i, 1L]
    children <- ends[ends[, 1L] == i, 2L]
    if (length(parents) != 1L || length(children) != 1L) next
    for (target in unique(c(max(layer[parents]) + 1L, min(layer[children]) - 1L))) {
      if (target == layer[i]) next
      candidate <- layer
      candidate[i] <- target
      candidates[[length(candidates) + 1L]] <- stats::setNames(
        match(candidate, sort(unique(candidate))) - 1L, names)
    }
  }
  candidates
}
