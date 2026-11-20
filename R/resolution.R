# A coarse grid can force arrows to share a complete terminal step. Search the
# same route grammars at half-step resolution, after all node placement. Keep
# the search on an integer grid so occupancy keys and candidate heights retain
# their meaning. Physical station clearance is scaled along with coordinates.
# The straight-track attempt below roughly doubles the pass, so it has its own,
# smaller work bound (`spine_work`): on 40-node DAGs between the two bounds the
# guards rejected it while it added 4-8 s.
.refine_route_resolution <- function(nodes, edges, max_work = 12000L, spine_work = 250000) {
  if (!nrow(edges)) return(edges)
  routes <- unname(split(edges, edges$edge_id))
  # Straight arrows give the pass nothing to refine, whatever the budget.
  straight <- !.route_bent(routes)
  if (all(straight)) return(edges)
  work <- 2 * nrow(edges) * length(routes)
  if (work > max_work) {
    .metro_note_budget("half_grid", attempted = FALSE)
    return(edges)
  }
  .metro_note_budget("half_grid", attempted = TRUE)
  coordinates <- c("x", "y", "xend", "yend")
  fine <- lapply(routes, function(e) {
    out <- .route_from_moves(2 * e$x[1L], 2 * e$y[1L],
      rep(e$xend - e$x, each = 2L))
    for (key in c("from", "to", "edge_id", "path_id")) out[[key]] <- e[[key]][1L]
    out$segment <- seq_len(nrow(out))
    out$bend <- c(FALSE, diff(out$xend - out$x) != 0)
    out[, names(e)]
  })
  obstacles <- nodes; obstacles$x <- 2 * nodes$x; obstacles$y <- 2 * nodes$y
  baseline <- .routed_geometry_quality(fine)
  extent <- function(r) {
    e <- do.call(rbind, r)
    c(range(c(obstacles$x, e$x, e$xend)), range(c(obstacles$y, e$y, e$yend)))
  }
  before <- extent(fine)
  reversals <- function(e) {
    directions <- rle(e$xend - e$x)$values
    sum(diff(directions[directions != 0]) != 0)
  }
  limit <- vapply(fine, reversals, integer(1))
  refine <- function(candidate, eligible) {
    candidate <- .straighten_routes(candidate, obstacles, eligible = eligible, clearance = 1.6)
    .lengthen_route_approaches(candidate, obstacles, minimum = 6L, clearance = 1.6)
  }
  admissible <- function(candidate) {
    quality <- .routed_geometry_quality(candidate)
    if (any(quality > baseline + 1e-8) || !any(quality < baseline - 1e-8)) return(FALSE)
    after <- extent(candidate)
    if (any(after[c(1L, 3L)] < before[c(1L, 3L)] - 1e-8) ||
        any(after[c(2L, 4L)] > before[c(2L, 4L)] + 1e-8)) return(FALSE)
    if (any(vapply(candidate, reversals, integer(1)) > limit)) return(FALSE)
    all(.routes_clear(candidate, obstacles, 1.6))
  }
  candidate <- refine(fine, which(!straight))
  if (!admissible(candidate)) candidate <- NULL
  # Unit-grid routing places one arrow at a time, so a bending sibling can
  # claim the straight track between two stations on one line and push the
  # arrow joining them onto a parallel lane with a hook at each end (the middle
  # chain of a five-mediator fan). Return such arrows to their straight track,
  # reroute the bending arrows that now share track with them, and prefer that
  # drawing unless the plain search has fewer crossings or bends. Rerouting
  # every bending arrow at the spine's stations instead tangled a nine-mediator
  # fan. The reroute ranks crossings before overlap, as the guards below do.
  spine <- if (work <= spine_work) .restore_straight_tracks(fine, obstacles, clearance = 1.6)
  if (!is.null(spine)) {
    restored <- spine$restored
    from <- match(.route_from(fine), obstacles$node)
    to <- match(.route_to(fine), obstacles$node)
    track <- .route_grid_keys(do.call(rbind, spine$routes[restored]))
    siblings <- setdiff(which(!straight), restored)
    siblings <- siblings[vapply(spine$routes[siblings], function(e)
      any(.route_grid_keys(e) %in% track), logical(1))]
    spine <- if (length(siblings)) .reroute_moved_nodes(spine$routes, obstacles, siblings,
      from, to, crossings_first = TRUE, clearance = 1.6) else spine$routes
    # Restored tracks are straight, so neither refinement step moves them.
    if (!is.null(spine)) spine <- refine(spine, setdiff(which(!straight), restored))
    if (!is.null(spine) && admissible(spine) && (is.null(candidate) || all(
        .routed_geometry_quality(spine)[c("crossings", "bends")] <=
        .routed_geometry_quality(candidate)[c("crossings", "bends")] + 1e-8)))
      candidate <- spine
  }
  if (is.null(candidate)) return(edges)
  for (i in seq_along(candidate)) {
    # Preserve the original rows of unchanged arrows, especially straight main
    # paths. Only geometrically changed routes need half-grid segment rows.
    a <- unname(as.matrix(.route_runs(candidate[[i]])[coordinates]))
    b <- unname(as.matrix(.route_runs(fine[[i]])[coordinates]))
    if (identical(a, b)) candidate[[i]] <- routes[[i]] else
      candidate[[i]][coordinates] <- candidate[[i]][coordinates] / 2
  }
  out <- do.call(rbind, candidate)
  out <- out[order(out$edge_id, out$segment, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Replace each bending route whose stations lie on one straight track
# (vertical or diagonal) by that track when it clears unrelated stations.
# Return NULL when no route qualifies.
.restore_straight_tracks <- function(routes, nodes, clearance = 0.8) {
  restored <- integer()
  for (i in seq_along(routes)) {
    e <- routes[[i]]
    if (!any(e$bend)) next
    h <- nrow(e); dx <- tail(e$xend, 1L) - e$x[1L]
    if (dx != 0 && abs(dx) != h) next
    track <- .route_from_moves(e$x[1L], e$y[1L], rep(sign(dx), h))
    if (!.route_clear(track, .unrelated_stations(nodes, e), clearance)) next
    e[c("x", "y", "xend", "yend")] <- track
    e$bend <- FALSE
    routes[[i]] <- e
    restored <- c(restored, i)
  }
  if (!length(restored)) return(NULL)
  list(routes = routes, restored = restored)
}
