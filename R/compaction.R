# Distances to unrelated stations, retaining existing close passages while
# protecting generous whitespace up to one station-spacing unit.
.route_clearances <- function(runs, nodes) {
  x <- .subset2(nodes, "x"); y <- .subset2(nodes, "y")
  rx <- .subset2(runs, "x"); ry <- .subset2(runs, "y")
  rxend <- .subset2(runs, "xend"); ryend <- .subset2(runs, "yend")
  clearance <- rep(4, length(x)); k <- length(rx)
  # Stations further than 4 from every run keep 4.
  near <- if (k) .stations_near(x, y, rx, ry, rxend, ryend, 4) else integer()
  m <- length(near)
  if (!m) return(pmax(0.8, clearance))
  # One distance per station and run (station index fastest), then the
  # running minimum over the runs in their order.
  distance <- .point_segment_distance(rep.int(x[near], k), rep.int(y[near], k),
    rep(rx, each = m), rep(ry, each = m), rep(rxend, each = m), rep(ryend, each = m))
  close <- clearance[near]
  for (i in seq_len(k)) close <- pmin(close, distance[(i - 1L) * m + seq_len(m)])
  clearance[near] <- close
  pmax(0.8, clearance)
}

# Remove slack between whole ranks, without scaling diagonals or moving a node
# across another rank. Each spanning route loses the same number of straight
# steps inside the gap; its other steps and endpoint identities are retained.
.compact_route_gaps <- function(nodes, routes, spacing = 4, max_candidates = 24L) {
  finish <- function() list(nodes = nodes, routes = routes)
  if (!length(routes) || max_candidates <= 0L) return(finish())
  limit <- NULL
  attempts <- 0L
  for (pass in seq_len(2L)) {
    changed <- FALSE
    levels <- sort(unique(-nodes$y))
    for (j in order(-diff(levels), method = "radix")) {
      levels <- sort(unique(-nodes$y))
      upper <- -levels[j]; lower <- -levels[j + 1L]
      gap <- upper - lower
      if (gap <= spacing) next
      top <- vapply(lapply(routes, .subset2, "y"), `[`, numeric(1), 1L)
      ends <- lapply(routes, .subset2, "yend")
      spanning <- which(top >= upper & unlist(ends, use.names = FALSE)[cumsum(lengths(ends))] <= lower)
      if (!length(spanning)) next
      slots <- lapply(routes[spanning], function(e) {
        moves <- e$xend - e$x
        ids <- which(e$y <= upper & e$yend >= lower & moves == 0)
        # Do not shorten an existing destination approach below three steps.
        # Already shorter approaches keep their existing length.
        if (tail(moves, 1L) == 0) ids <- setdiff(ids,
          tail(seq_along(moves), min(3L, tail(rle(moves)$lengths, 1L))))
        ids
      })
      maximum <- min(gap - spacing, lengths(slots))
      if (maximum < 1L) next
      margins <- lapply(routes, function(e) .route_clearances(.route_runs(e),
        .unrelated_stations(nodes, e)))
      accepted <- FALSE
      for (amount in unique(c(maximum, ceiling(maximum / 2), 1L))) {
        for (late in c(FALSE, TRUE)) {
          if (attempts >= max_candidates) return(finish())
          attempts <- attempts + 1L
          candidate_nodes <- nodes
          region <- candidate_nodes$y <= lower
          candidate_nodes$y[region] <- candidate_nodes$y[region] + amount
          if (any(as.numeric(stats::dist(candidate_nodes[c("x", "y")])) < spacing - 1e-8)) next
          trial <- routes
          for (id in seq_along(routes)) {
            e <- routes[[id]]
            if (id %in% spanning) {
              ids <- slots[[match(id, spanning)]]
              remove <- if (late) tail(ids, amount) else head(ids, amount)
              moves <- (e$xend - e$x)[-remove]
              geometry <- .route_from_moves(e$x[1L], e$y[1L], moves)
              e <- e[seq_along(moves), , drop = FALSE]
              e[c("x", "y", "xend", "yend")] <- geometry
              e$segment <- seq_along(moves)
              e$bend <- c(FALSE, diff(moves) != 0)
            } else if (e$y[1L] <= lower) {
              # As `$<-.data.frame` assigns, on the underlying list.
              classes <- oldClass(e); e <- unclass(e)
              e$y <- e$y + amount; e$yend <- e$yend + amount
              class(e) <- classes
            }
            trial[[id]] <- e
          }
          clear <- all(.routes_clear(trial, candidate_nodes, margins))
          if (!clear) next
          if (is.null(limit)) limit <- .routed_geometry_quality(routes)
          quality <- .routed_geometry_quality(trial)
          if (any(quality > limit + 1e-8)) next
          nodes <- candidate_nodes; routes <- trial; limit <- quality
          changed <- accepted <- TRUE
          break
        }
        if (accepted) break
      }
    }
    if (!changed) break
  }
  finish()
}

# Shift an earlier run of the same direction into a short final approach.
# Moving the intervening bends upstream retains octilinearity and line length.
.lengthen_route_approaches <- function(routes, nodes, minimum = 3L, max_candidates = 24L,
  clearance = 0.8) {
  if (!length(routes) || max_candidates <= 0L) return(routes)
  prepared <- lapply(routes, .prepare_route_obstacles)
  index <- .obstacle_index(prepared)
  end_length <- vapply(routes, function(e) tail(rle(e$xend - e$x)$lengths, 1L), integer(1))
  from <- .route_from(routes)
  to <- .route_to(routes)
  priority <- order(end_length, from, to, method = "radix")
  attempts <- 0L
  limit <- NULL
  for (id in priority[end_length[priority] < minimum]) {
    e <- routes[[id]]
    moves <- e$xend - e$x
    runs <- rle(moves)
    last <- length(runs$lengths)
    if (last < 3L) next
    donors <- which(head(runs$values, -1L) == tail(runs$values, 1L))
    # Nearest donor first; keep at least one step in every earlier run.
    donors <- rev(donors[runs$lengths[donors] > 1L])
    if (!length(donors)) next
    obstacles <- .obstacles_except(index, id)
    conflicts <- .route_conflicts(e, obstacles)
    unrelated <- .unrelated_stations(nodes, e)
    margin <- pmax(clearance, .route_clearances(.route_runs(e), unrelated))
    accepted <- FALSE
    for (donor in head(donors, 3L)) {
      maximum <- min(minimum - end_length[id], runs$lengths[donor] - 1L)
      for (amount in unique(c(maximum, 1L))) {
        if (attempts >= max_candidates) return(routes)
        attempts <- attempts + 1L
        lengths <- runs$lengths
        lengths[donor] <- lengths[donor] - amount
        lengths[last] <- lengths[last] + amount
        candidate <- e
        candidate[c("x", "y", "xend", "yend")] <- .route_from_moves(e$x[1L], e$y[1L],
          rep(runs$values, lengths))
        candidate$bend <- c(FALSE, diff(candidate$xend - candidate$x) != 0)
        geometry <- .route_runs(candidate)
        if (!.route_obstacle_free(geometry, unrelated, margin) ||
            any(.route_conflicts(candidate, obstacles, geometry) > conflicts + 1e-8)) next
        trial <- routes; trial[[id]] <- candidate
        if (is.null(limit)) limit <- .routed_geometry_quality(routes)
        quality <- .routed_geometry_quality(trial)
        if (any(quality > limit + 1e-8)) next
        routes <- trial; limit <- quality
        prepared[[id]] <- .prepare_route_obstacles(candidate)
        index <- .obstacle_index(prepared)
        accepted <- TRUE
        break
      }
      if (accepted) break
    }
  }
  routes
}
