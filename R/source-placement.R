# Automatic ranks can leave a lightly connected source far upstream after its
# children have moved. Relax it towards their median, rerouting incident and
# newly obstructed arrows together. The complete route quality guards each move.
# Also run the greedy pass with rerouting that ranks crossings before
# shared track, as the move acceptance test does, and keep that result only when
# it Pareto-dominates the default pass: no more crossings, shared track, bends,
# line length or extent, and strictly less of at least one. Adopting it
# unconditionally improved the gallery but reached worse basins on random DAGs.
# With `transverse = TRUE` (supplied ranks) only transverse positions are
# tried; the compaction after each move shifts complete ranks, so every node
# keeps its rank.
# The two passes differ only in how rerouting ranks its candidates. The
# default pass notes whether ranking crossings first would have chosen another
# route for any arrow it rerouted. If not, the alternative pass would make the
# same calls with the same results and return the same drawing, which never
# Pareto-dominates itself, so it is not run.
.relax_source_positions <- function(nodes, routes, spacing = 4, max_candidates = 64L,
  transverse = FALSE) {
  if (length(routes) >= 2L && max_candidates > 0L) max_candidates <- .metro_stage_attempts(
    "source_relaxation", sum(.route_steps(routes)) * length(routes), max_candidates)
  tracker <- new.env(parent = emptyenv())
  tracker$differs <- FALSE
  default <- .relax_source_pass(nodes, routes, spacing, max_candidates, transverse = transverse,
    alternative = tracker)
  if (length(routes) < 2L || max_candidates <= 0L || !tracker$differs) return(default)
  alternative <- .relax_source_pass(nodes, routes, spacing, max_candidates,
    crossings_first = TRUE, transverse = transverse)
  measure <- function(z) {
    c(.routed_geometry_quality(z$routes), .drawing_extent(z$nodes, z$routes))
  }
  a <- measure(default); b <- measure(alternative)
  if (all(b <= a + 1e-8) && any(b < a - 1e-8)) alternative else default
}

.relax_source_pass <- function(nodes, routes, spacing = 4, max_candidates = 64L,
  crossings_first = FALSE, transverse = FALSE, alternative = NULL) {
  finish <- function() list(nodes = nodes, routes = routes)
  if (length(routes) < 2L || max_candidates <= 0L) return(finish())
  route_order <- order(.route_from(routes),
    .route_to(routes), method = "radix")
  routes <- routes[route_order]
  finish <- function() list(nodes = nodes, routes = routes[order(route_order, method = "radix")])
  from <- match(.route_from(routes), nodes$node)
  to <- match(.route_to(routes), nodes$node)
  sources <- setdiff(unique(from), to)
  sources <- sources[order(nodes$node[sources], method = "radix")]
  extent <- .drawing_extent
  attempts <- 0L
  limit <- .routed_geometry_quality(routes)
  bounds <- extent(nodes, routes)
  reversals <- function(r) sum(vapply(r, function(e) {
    directions <- rle(e$xend - e$x)$values
    sum(diff(directions[directions != 0]) != 0)
  }, integer(1)))
  for (pass in seq_len(4L)) {
    best <- NULL; best_quality <- limit
    straight <- !.route_bent(routes)
    for (i in sources) {
      children <- unique(to[from == i])
      target <- round(stats::median(nodes$x[children]))
      xs <- unique(c(target, round((nodes$x[i] + target) / 2),
        nodes$x[i] + sign(target - nodes$x[i]) * min(2, abs(target - nodes$x[i])), nodes$x[i]))
      for (x in xs) {
        late <- max(nodes$y[children] + pmax(spacing, abs(nodes$x[children] - x)))
        if (late > nodes$y[i]) next
        ys <- if (transverse) nodes$y[i] else unique(c(late, round((nodes$y[i] + late) / 2),
          max(late, nodes$y[i] - spacing), nodes$y[i]))
        for (y in ys) {
          candidate <- nodes; candidate$x[i] <- x; candidate$y[i] <- y
          if (identical(candidate, nodes) ||
              any(as.numeric(stats::dist(candidate[c("x", "y")])) < spacing - 1e-8)) next
          if (any(candidate$y[from] - candidate$y[to] <
              pmax(spacing, abs(candidate$x[to] - candidate$x[from])))) next
          if (attempts >= max_candidates) break
          attempts <- attempts + 1L
          blocked <- which(!.routes_clear(routes, candidate))
          affected <- union(which(from == i), blocked)
          trial <- .reroute_moved_nodes(routes, candidate, affected, from, to,
            crossings_first = crossings_first, alternative = alternative)
          if (is.null(trial)) next
          if (any(.route_bent(trial[straight]))) next
          if (reversals(trial) > reversals(routes)) next
          quality <- .routed_geometry_quality(trial)
          if (any(quality > limit + 1e-8) || any(extent(candidate, trial) > bounds + 1e-8)) next
          if (!.lexically_better(quality, best_quality)) next
          best <- list(nodes = candidate, routes = trial); best_quality <- quality
        }
        if (attempts >= max_candidates) break
      }
      if (attempts >= max_candidates) break
    }
    if (is.null(best)) break
    nodes <- best$nodes; routes <- best$routes
    compact <- .compact_route_gaps(nodes, routes)
    compact <- .metro_unrecorded(.compact_node_arrangement(compact$nodes, compact$routes))
    # A smaller box does not justify turning a simple approach into a V.
    if (reversals(compact$routes) <= reversals(routes)) {
      nodes <- compact$nodes; routes <- compact$routes
    }
    routes <- .lengthen_route_approaches(routes, nodes)
    limit <- .routed_geometry_quality(routes); bounds <- extent(nodes, routes)
    if (attempts >= max_candidates) break
  }
  finish()
}
