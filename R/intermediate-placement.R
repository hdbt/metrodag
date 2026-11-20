# Preserve straight through-chains while allowing a short side arrow to trade
# its bend with the next arrow. Labels and analysis roles do not enter here.
.straight_chain_edges <- function(routes) {
  from <- .route_from(routes)
  to <- .route_to(routes)
  straight <- !.route_bent(routes)
  bearing <- vapply(routes, function(e) e$xend[1L] - e$x[1L], numeric(1))
  protected <- rep(FALSE, length(routes))
  for (i in which(straight)) {
    next_edges <- which(straight & from == to[i] & bearing == bearing[i])
    if (length(next_edges)) protected[c(i, next_edges)] <- TRUE
  }
  # With no through-chain, retain a straight arrow with greatest causal span.
  # Name ties use the caller's canonical from/to ordering.
  if (!any(protected) && any(straight)) {
    lengths <- vapply(routes, function(e) if (any(e$bend)) 0L else nrow(e), integer(1))
    protected[which.max(lengths)] <- TRUE
  }
  protected
}

# Evaluate the geometry that the final half-grid pass would actually return.
# Expand it onto a doubled integer grid for the occupancy-based quality helper;
# neither mixed step sizes nor fractional coordinates enter its grid keys.
.intermediate_geometry_quality <- function(nodes, routes, refine = TRUE) {
  edges <- do.call(rbind, routes)
  if (refine) edges <- .metro_unrecorded(.refine_route_resolution(nodes, edges))
  fine <- lapply(unname(split(edges, edges$edge_id)), function(e) {
    out <- .route_from_moves(2 * e$x[1L], 2 * e$y[1L],
      rep(sign(e$xend - e$x), times = as.integer(2 * (e$y - e$yend))))
    for (key in c("from", "to", "edge_id", "path_id")) out[[key]] <- e[[key]][1L]
    out$segment <- seq_len(nrow(out))
    out$bend <- c(FALSE, diff(out$xend - out$x) != 0)
    out[, names(e)]
  })
  quality <- .routed_geometry_quality(fine)
  quality[c("shared", "length")] <- quality[c("shared", "length")] / 2
  reversals <- vapply(fine, function(e) {
    directions <- rle(e$xend - e$x)$values
    sum(diff(directions[directions != 0]) != 0)
  }, integer(1))
  list(quality = quality, extent = c(diff(range(c(nodes$x, edges$x, edges$xend))),
    diff(range(c(nodes$y, edges$y, edges$yend)))), reversals = reversals)
}

# A material area reduction may spend one bend at >=15%, or two at >=25%.
# Never buy space with extra crossings, shared track, ink or route reversals.
.intermediate_placement_improves <- function(candidate, current, baseline) {
  q <- candidate$quality; old <- baseline$quality
  if (any(q[c("crossings", "shared", "length")] > old[c("crossings", "shared", "length")] + 1e-8) ||
      any(candidate$extent > baseline$extent + 1e-8) ||
      any(candidate$reversals > baseline$reversals)) return(FALSE)
  ratio <- prod(candidate$extent) / prod(baseline$extent)
  if (ratio >= 1 - 1e-8) return(FALSE)
  budget <- if (ratio <= 0.75 + 1e-8) 2L else if (ratio <= 0.85 + 1e-8) 1L else 0L
  if (q[["bends"]] > old[["bends"]] + budget) return(FALSE)
  score <- function(z) c(z$quality[c("crossings", "shared")],
    prod(z$extent) / prod(baseline$extent) + 0.5 * z$quality[["length"]] / old[["length"]] +
      0.1 * (z$quality[["bends"]] - old[["bends"]]))
  .lexically_better(score(candidate), score(current))
}

# Locally place a low-degree intermediate between its parents and children.
# Freeing an isolated straight side arrow can bring its station downstream
# while keeping an entire through-chain fixed and retaining every causal edge.
#
# Also consider pure source nodes (incoming == 0) with outgoing <= 3.
# These are latent/unobserved common causes that the original filter excluded.
# For pure sources there is no through-chain to protect; the upper y-bound
# is the node's own current y-coordinate.
# With `transverse = TRUE` (supplied ranks) every candidate keeps its depth.
.relax_intermediate_positions <- function(nodes, routes, spacing = 4, max_candidates = 24L,
  transverse = FALSE) {
  finish <- function() list(nodes = nodes, routes = routes)
  if (length(routes) < 2L || max_candidates <= 0L) return(finish())
  work <- sum(.route_steps(routes)) * length(routes)
  max_candidates <- .metro_stage_attempts("intermediate_relaxation", work, max_candidates)
  if (max_candidates < 1L) return(finish())
  route_order <- order(.route_from(routes),
    .route_to(routes), method = "radix")
  routes <- routes[route_order]
  finish <- function() list(nodes = nodes, routes = routes[order(route_order, method = "radix")])
  from <- match(.route_from(routes), nodes$node)
  to <- match(.route_to(routes), nodes$node)
  protected <- .straight_chain_edges(routes)
  incoming <- tabulate(to, nrow(nodes)); outgoing <- tabulate(from, nrow(nodes))

  # Original criterion: both-sided nodes (intermediate nodes).
  intermediates <- which(incoming > 0L & outgoing > 0L & incoming + outgoing <= 3L)
  intermediates <- setdiff(intermediates, unique(c(from[protected], to[protected])))

  # pure-source candidates: nodes with no parents and <= 3 children.
  # These are excluded from the original filter but are often latent confounders
  # whose placement strongly affects bypass arrow length.
  pure_sources <- which(incoming == 0L & outgoing > 0L & outgoing <= 3L)

  candidates <- c(intermediates, pure_sources)
  if (!length(candidates)) return(finish())

  ink <- vapply(routes, function(e) sum(sqrt((e$xend - e$x)^2 + 1)), numeric(1))
  priority <- vapply(candidates, function(i) sum(ink[from == i | to == i]) /
    sum(from == i | to == i), numeric(1))
  # Equal mean ink must tie exactly on every platform, so the node name decides.
  candidates <- candidates[order(-.tolerance_rank(priority), nodes$node[candidates],
    method = "radix")]

  coarse <- .intermediate_geometry_quality(nodes, routes, refine = FALSE)
  baseline <- .intermediate_geometry_quality(nodes, routes)
  if (prod(baseline$extent) == 0) return(finish())
  best <- NULL; best_quality <- baseline; attempts <- 0L; seen <- character()

  for (i in candidates) {
    parents  <- unique(from[to   == i])
    children <- unique(to  [from == i])
    parents  <- parents [order(nodes$node[parents ], method = "radix")]
    children <- children[order(nodes$node[children], method = "radix")]

    target <- round(stats::median(nodes$x[c(parents, children)]))
    xs <- unique(c(round((nodes$x[i] + target) / 2), target,
      nodes$x[parents], nodes$x[children], nodes$x[i]))

    for (x in xs) {
  # for pure sources the upper y-bound is the node's own y (it has
      # no parents to constrain it from above).
      if (length(parents) > 0L) {
        early <- min(nodes$y[parents] - pmax(spacing, abs(nodes$x[parents] - x)))
      } else {
        early <- nodes$y[i]
      }
      late <- max(nodes$y[children] + pmax(spacing, abs(nodes$x[children] - x)))
      if (late > early) next

  # for pure sources, also try a y midway between all children
      # (proximity-weighted), so they can settle centrally between two far-apart
      # children rather than defaulting to their assigned rank.
      extra_ys <- if (length(parents) == 0L && length(children) >= 2L) {
        w <- 1 / pmax(1, abs(nodes$x[children] - x))
        round(sum(nodes$y[children] * w) / sum(w))
      } else integer(0)

      ys <- if (transverse) nodes$y[i] else unique(c(round((early + late) / 2), early, late,
        round(stats::median(nodes$y[c(parents, children)])), nodes$y[i],
        extra_ys))

      for (y in ys) {
        if (y < late || y > early) next
        candidate <- nodes; candidate$x[i] <- x; candidate$y[i] <- y
        if (identical(candidate, nodes) ||
            any(as.numeric(stats::dist(candidate[c("x", "y")])) < spacing - 1e-8)) next
        # Routes must contain every station. Skip a placement whose node box
        # alone already rules out a smaller drawing within the extent guards.
        lower <- c(diff(range(candidate$x)), diff(range(candidate$y)))
        if (any(lower > baseline$extent + 1e-8) ||
            prod(lower) >= prod(baseline$extent) - 1e-8) next
        key <- paste(i, x, y, sep = ":")
        if (key %in% seen) next
        seen <- c(seen, key)
        if (attempts >= max_candidates) break
        attempts <- attempts + 1L
        blocked <- which(!.routes_clear(routes, candidate))
        affected <- union(which(from == i | to == i), blocked)
        trial <- .reroute_moved_nodes(routes, candidate, affected, from, to)
        if (is.null(trial)) next
        trial <- .straighten_routes(trial, candidate, eligible = which(!protected))
        trial <- .lengthen_route_approaches(trial, candidate)
        if (any(.route_bent(trial[protected]))) next
        q <- .routed_geometry_quality(trial)
        if (any(q[c("crossings", "shared", "length")] > coarse$quality[c("crossings", "shared", "length")] + 1e-8) ||
            q[["bends"]] > coarse$quality[["bends"]] + 2L) next
        # Component packing can exceed the public half-grid work budget. The
        # original unit-grid result must satisfy the same bounds if that final
        # pass is skipped, including per-arrow reversal and extent guards.
        raw <- .intermediate_geometry_quality(candidate, trial, refine = FALSE)
        if (!.intermediate_placement_improves(raw, coarse, coarse)) next
        quality <- .intermediate_geometry_quality(candidate, trial)
        if (!.intermediate_placement_improves(quality, best_quality, baseline)) next
        best <- list(nodes = candidate, routes = trial); best_quality <- quality
      }
      if (attempts >= max_candidates) break
    }
    if (attempts >= max_candidates) break
  }
  if (!is.null(best)) { nodes <- best$nodes; routes <- best$routes }
  finish()
}
