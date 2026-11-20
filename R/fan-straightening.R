# Bring the outer branches of a balanced fan onto straight diagonals. This is
# a soft placement preference. With supplied ranks (`layer`, in node order) a
# placement must keep every node in its rank: whole ranks may move together,
# a branch may not leave its rank or join another one.
.straighten_fans <- function(nodes, routes, spacing = 4, max_candidates = 24L, layer = NULL) {
  if (length(routes) < 2L) return(list(nodes = nodes, routes = routes))
  route_order <- order(.route_from(routes),
    .route_to(routes), method = "radix")
  routes <- routes[route_order]
  from <- match(.route_from(routes), nodes$node)
  to <- match(.route_to(routes), nodes$node)
  finish <- function() list(nodes = nodes, routes = routes[order(route_order, method = "radix")])
  limit <- NULL
  attempts <- 0L
  for (pass in seq_len(2L)) {
    changed <- FALSE
    for (junction in order(nodes$node, method = "radix")) for (direction in c("in", "out")) {
      adjacent <- unique(if (direction == "in") from[to == junction] else to[from == junction])
      if (length(adjacent) < 2L) next
      adjacent <- adjacent[order(nodes$x[adjacent], nodes$node[adjacent], method = "radix")]
      outer <- adjacent[c(1L, length(adjacent))]
      offset <- nodes$x[outer] - nodes$x[junction]
      if (offset[1L] >= 0 || offset[2L] <= 0 || sum(offset) != 0) next
      fan_edges <- if (direction == "in") which(to == junction & from %in% outer) else
        which(from == junction & to %in% outer)
      if (!any(.route_bent(routes[fan_edges]))) next
      sign <- if (direction == "in") 1 else -1
      depth <- sign * nodes$y
      nearest <- min(depth[outer])
      # First compact the gap and align the two branches with the nearer one.
      # Translate the complete downstream (or upstream) region together, so
      # existing through-routes and the opposite outer bends keep their shape.
      shift <- nearest - abs(offset[1L]) - depth[junction]
      candidates <- list()
      if (shift >= 0) {
        compact <- nodes
        region <- which(depth <= depth[junction])
        compact$y[region] <- compact$y[region] + sign * shift
        compact$y[outer] <- sign * nearest
        candidates[[length(candidates) + 1L]] <- compact
      }
      # If compaction is blocked, moving only the branches may still help.
      aligned <- nodes
      aligned$y[outer] <- nodes$y[junction] + sign * abs(offset[1L])
      if (all(sign * aligned$y[outer] <= depth[outer]))
        candidates[[length(candidates) + 1L]] <- aligned
      seen <- character()
      for (candidate_nodes in candidates) {
        key <- paste(candidate_nodes$y, collapse = ",")
        if (key %in% seen || identical(candidate_nodes, nodes)) next
        seen <- c(seen, key)
        if (!is.null(layer) && !.ranks_kept(candidate_nodes$y, layer)) next
        if (attempts >= max_candidates) return(finish())
        attempts <- attempts + 1L
        heights <- candidate_nodes$y[from] - candidate_nodes$y[to]
        delta <- candidate_nodes$x[to] - candidate_nodes$x[from]
        if (any(heights <= 0 | heights < abs(delta))) next
        # Protect station spacing, including nodes from formerly different ranks.
        if (any(as.numeric(stats::dist(candidate_nodes[c("x", "y")])) < spacing - 1e-8)) next
        affected <- which(candidate_nodes$y[from] != nodes$y[from] |
          candidate_nodes$y[to] != nodes$y[to])
        straight <- !.route_bent(routes)
        minimum <- as.integer(delta != 0 & abs(delta) != heights)
        if (any(minimum[straight] > 0) ||
            sum(minimum[affected]) >= sum(.route_bends(routes[affected]))) next
        trial <- .reroute_moved_nodes(routes, candidate_nodes, affected, from, to)
        if (is.null(trial)) next
        trial <- .straighten_routes(trial, candidate_nodes, eligible = affected)
        clear <- all(.routes_clear(trial, candidate_nodes))
        if (!clear || any(.route_bent(trial[straight]))) next
        if (is.null(limit)) limit <- .routed_geometry_quality(routes)
        quality <- .routed_geometry_quality(trial)
        if (quality["bends"] >= limit["bends"] || any(quality > limit + 1e-8)) next
        nodes <- candidate_nodes
        routes <- trial
        limit <- quality
        changed <- TRUE
        break
      }
    }
    if (!changed) break
  }
  finish()
}

# Route newly positioned endpoints together; do not score against stale routes
# for edges whose endpoints have also moved. Forced straight diagonals go first.
# With an environment `alternative`, also follow the choice that ranks
# crossings before overlap (crossings_first = TRUE) in the same scan, and set
# alternative$differs when it would pick another route for some arrow. While
# it does not, a crossings-first call returns the same trial.
.reroute_moved_nodes <- function(routes, nodes, affected, from, to, crossings_first = FALSE,
  clearance = 0.8, alternative = NULL) {
  trial <- routes
  occupied <- .bind_route_columns(routes[-affected])
  if (is.null(occupied)) occupied <- .empty_routes()
  height <- nodes$y[from] - nodes$y[to]
  delta <- nodes$x[to] - nodes$x[from]
  minimum <- as.integer(delta != 0 & abs(delta) != height)
  priority <- affected[order(minimum[affected], height[affected],
    nodes$node[from[affected]], nodes$node[to[affected]], method = "radix")]
  # Obstacles grow by each rerouted arrow; earlier rows keep their order.
  obstacles <- .prepare_route_obstacles(occupied)
  for (id in priority) {
    original <- routes[[id]]
    skeleton <- .route_from_moves(nodes$x[from[id]], nodes$y[from[id]], rep(0, height[id]))
    skeleton$xend[nrow(skeleton)] <- nodes$x[to[id]]
    unrelated <- .unrelated_stations(nodes, original)
    stack <- .simple_route_stack(skeleton)
    ids <- seq_len(stack$k)
    ids <- ids[.stacked_clear(stack, ids, unrelated, clearance)]
    conflicts <- .stacked_conflicts(stack, ids, obstacles)
    shared_sink <- sum(to == to[id]) > 1L
    best <- best_j <- NULL
    best_score <- rep(Inf, 5L)
    other <- NULL
    other_score <- rep(Inf, 5L)
    for (j in seq_along(ids)) {
      steps <- stack$offset[ids[j]] + seq_len(stack$size[ids[j]])
      overlap <- conflicts$overlap[j]; crossings <- conflicts$crossings[j]
      moves <- stack$dx[steps]
      diagonal <- which(moves != 0)
      tie <- if (!length(diagonal)) 0 else if (shared_sink) -mean(diagonal) else mean(diagonal)
      rest <- c(sum(diff(moves) != 0), sum(sqrt(moves^2 + 1)), tie)
  # optionally rank crossings before shared track, as the final
      # .routed_geometry_quality() acceptance test does. Overlap-first can trade
      # a fraction of a unit of shared track for extra crossings, so the trial
      # is rejected even when a conflict-neutral route exists.
      score <- if (crossings_first) c(crossings, overlap, rest) else c(overlap, crossings, rest)
      if (!is.null(alternative) && !crossings_first) {
        swapped <- c(crossings, overlap, rest)
        if (.lexically_better(swapped, other_score)) {
          other <- j
          other_score <- swapped
        }
      }
      if (!.lexically_better(score, best_score)) next
      best <- ids[j]
      best_j <- j
      best_score <- score
    }
    if (!is.null(other) && !identical(other, best_j)) alternative$differs <- TRUE
    if (is.null(best)) return(NULL)
    best <- .stacked_route(stack, best)
    for (field in c("from", "to", "edge_id", "path_id")) best[[field]] <- original[[field]][1L]
    best$segment <- seq_len(nrow(best))
    best$bend <- c(FALSE, diff(best$xend - best$x) != 0)
    trial[[id]] <- best[, names(original)]
    obstacles <- .merge_route_obstacles(obstacles, .prepare_route_obstacles(trial[[id]]))
  }
  trial
}
