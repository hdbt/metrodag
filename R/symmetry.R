# Balance a fan around its central through-route. Work after initial routing so
# a more symmetric node placement can be rejected when it harms route clarity.
.balance_geometry <- function(graph, nodes, routes, spacing = 4) {
  if (!length(routes)) return(list(nodes = nodes, routes = routes))
  incoming <- lapply(seq_len(nrow(nodes)), function(i)
    unique(as.integer(igraph::neighbors(graph, i, "in"))))
  outgoing <- lapply(seq_len(nrow(nodes)), function(i)
    unique(as.integer(igraph::neighbors(graph, i, "out"))))
  fans <- list()
  for (i in order(nodes$node, method = "radix")) for (direction in c("in", "out")) {
    adjacent <- if (direction == "in") incoming[[i]] else outgoing[[i]]
    # Two outer branches and a middle branch define the axis to balance.
    if (length(adjacent) >= 3L)
      fans[[length(fans) + 1L]] <- list(node = i, adjacent = adjacent,
        direction = direction)
  }
  if (!length(fans)) return(list(nodes = nodes, routes = routes))
  # Use one refinement order for equivalent inputs, retaining original edge
  # IDs and restoring the caller's route-list order for the completed geometry.
  route_order <- order(.route_from(routes),
    .route_to(routes), method = "radix")
  routes <- routes[route_order]
  imbalance <- function(x) sum(vapply(fans, function(fan) {
    bounds <- range(x[fan$adjacent])
    abs(2 * x[fan$node] - sum(bounds))
  }, numeric(1)))
  from <- match(.route_from(routes), nodes$node)
  to <- match(.route_to(routes), nodes$node)
  clearance <- function(z, n) all(.routes_clear(z, n))
  limit <- NULL
  score <- imbalance(nodes$x)
  for (pass in seq_len(2L)) {
    changed <- FALSE
    for (fan in fans) {
      bounds <- range(nodes$x[fan$adjacent])
      target <- mean(bounds)
      positions <- unique(c(floor(target), ceiling(target)))
      positions <- positions[order(abs(positions - nodes$x[fan$node]), positions, method = "radix")]
      middle <- fan$adjacent[nodes$x[fan$adjacent] > bounds[1L] &
        nodes$x[fan$adjacent] < bounds[2L]]
      if (!length(middle)) next
      middle <- middle[order(abs(nodes$x[middle] - target), nodes$node[middle], method = "radix")][1L]
      group <- fan$node
      current <- middle
      while (length(incoming[[current]]) == 1L && length(outgoing[[current]]) == 1L) {
        group <- c(group, current)
        current <- if (fan$direction == "in") incoming[[current]] else outgoing[[current]]
      }
      for (position in positions) {
        candidate_nodes <- nodes
        candidate_nodes$x[group] <- position
        candidate_score <- imbalance(candidate_nodes$x)
        if (candidate_score >= score - 1e-8) next
        separated <- all(vapply(unique(nodes$y), function(y) {
          ids <- which(nodes$y == y)
          ids <- ids[order(nodes$x[ids], method = "radix")]
          all(diff(candidate_nodes$x[ids]) >= spacing)
        }, logical(1)))
        if (!separated) next
        affected <- which(from %in% group | to %in% group)
        delta <- candidate_nodes$x[to[affected]] - candidate_nodes$x[from[affected]]
        heights <- candidate_nodes$y[from[affected]] - candidate_nodes$y[to[affected]]
        if (any(abs(delta) > heights)) next
        minimum <- sum(delta != 0 & abs(delta) != heights)
        if (minimum > sum(.route_bends(routes[affected]))) next
        trial <- routes
        possible <- TRUE
        for (id in affected) {
          e <- routes[[id]]
          e$x[1L] <- candidate_nodes$x[from[id]]
          e$xend[nrow(e)] <- candidate_nodes$x[to[id]]
          candidates <- .simple_route_candidates(e)
          stack <- .stack_routes(candidates)
          bends <- stack$bends
          owner <- factor(stack$owner, levels = seq_len(stack$k))
          ink <- vapply(split(abs(stack$dx), owner), sum, numeric(1), USE.NAMES = FALSE)
          # Late approaches keep a common sink's central track free. Reverse
          # this preference for departures from a shared source.
          steps <- seq_along(stack$owner) - stack$offset[stack$owner]
          diagonal <- stack$xend != stack$x
          tie <- vapply(split(steps[diagonal], owner[diagonal]), function(d) {
            if (!length(d)) return(0)
            if (length(incoming[[to[id]]]) > 1L) -mean(d) else mean(d)
          }, numeric(1), USE.NAMES = FALSE)
          unrelated <- .unrelated_stations(candidate_nodes, e)
          selected <- NULL
          # Take the first clear candidate in this order, testing a few at a time.
          ranked <- order(bends, ink, tie, method = "radix")
          for (block in split(ranked, (seq_along(ranked) - 1L) %/% 8L)) {
            clear <- block[.stacked_clear(stack, block, unrelated)]
            if (!length(clear)) next
            z <- candidates[[clear[1L]]]
            for (key in c("from", "to", "edge_id", "path_id")) z[[key]] <- e[[key]][1L]
            z$segment <- seq_len(nrow(z))
            z$bend <- c(FALSE, diff(z$xend - z$x) != 0)
            selected <- z[, names(e)]
            break
          }
          if (is.null(selected)) { possible <- FALSE; break }
          trial[[id]] <- selected
        }
        if (!possible || !clearance(trial, candidate_nodes)) next
        trial <- .straighten_routes(trial, candidate_nodes, eligible = affected)
        if (!clearance(trial, candidate_nodes)) next
        if (is.null(limit)) limit <- .routed_geometry_quality(routes)
        candidate_quality <- .routed_geometry_quality(trial)
        if (any(candidate_quality > limit + 1e-8)) next
        nodes <- candidate_nodes
        routes <- trial
        score <- candidate_score
        limit <- candidate_quality
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  list(nodes = nodes, routes = routes[order(route_order, method = "radix")])
}
