# Small helpers shared by the layout refinement passes.

# TRUE when score vector `a` is strictly better than `b` under lexicographic
# order: the first component that differs by more than `tolerance` is smaller.
.lexically_better <- function(a, b, tolerance = 1e-8) {
  different <- which(abs(a - b) > tolerance)
  length(different) > 0L && a[different[1L]] < b[different[1L]]
}

# Integer ranks of `x` for use as an order() key. Neighbouring values within
# `tolerance` share a rank, so floating-point sums that differ only by
# platform rounding (sum() has no long double on arm64 macOS) tie exactly.
.tolerance_rank <- function(x, tolerance = 1e-8) {
  if (!length(x)) return(integer())
  o <- order(x, method = "radix")
  rank <- integer(length(x))
  rank[o] <- cumsum(c(TRUE, diff(x[o]) > tolerance))
  rank
}

# A route keeps `clearance` grid units from every station in `nodes`.
.route_clear <- function(e, nodes, clearance = 0.8) {
  .route_obstacle_free(.route_runs(e), nodes, clearance)
}

# For every route at once: .route_clear(e, .unrelated_stations(nodes, e), c)
# with one clearance `c` for all routes, or a list of per-route clearances
# for their unrelated stations in station order (as .route_clearances()
# gives them). Every station/run distance is the one a single test computes.
.routes_clear <- function(routes, nodes, clearance = 0.8) {
  k <- length(routes)
  name <- nodes$node; m <- length(name)
  if (!k || !m || anyDuplicated(name)) return(vapply(seq_len(k), function(i) .route_clear(routes[[i]],
    .unrelated_stations(nodes, routes[[i]]), if (is.list(clearance)) clearance[[i]] else clearance),
    logical(1)))
  first <- match(.route_from(routes), name); last <- match(.route_to(routes), name)
  if (is.list(clearance) && any(lengths(clearance) != m - (!is.na(first)) -
      (!is.na(last) & (is.na(first) | last != first)))) return(vapply(seq_len(k), function(i)
    .route_clear(routes[[i]], .unrelated_stations(nodes, routes[[i]]), clearance[[i]]), logical(1)))
  stack <- .stack_routes(routes)
  r <- length(stack$rx)
  run <- rep(seq_len(r), each = m); j <- rep.int(seq_len(m), r)
  owner <- stack$run_owner[run]
  a <- first[owner]; b <- last[owner]
  endpoint_a <- !is.na(a) & j == a; endpoint_b <- !is.na(b) & j == b
  pair <- which(!(endpoint_a | endpoint_b))
  run <- run[pair]; j <- j[pair]; owner <- owner[pair]
  limit <- if (is.list(clearance)) {
    # Station j is the (j - endpoints before it)-th unrelated station.
    a <- a[pair]; b <- b[pair]
    before <- (!is.na(a) & a < j) + (!is.na(b) & b < j & (is.na(a) | b != a))
    offset <- c(0L, cumsum(lengths(clearance)))
    unlist(clearance, use.names = FALSE)[offset[owner] + j - before]
  } else clearance
  close <- .point_segment_distance(nodes$x[j], nodes$y[j], stack$rx[run], stack$ry[run],
    stack$rxend[run], stack$ryend[run]) < limit
  clear <- rep(TRUE, k)
  clear[unique(owner[which(close)])] <- FALSE
  clear
}

# Supplied ranks hold when nodes share a depth exactly when they share a rank
# and deeper ranks stay deeper. `layer` holds consecutive ranks from zero.
.ranks_kept <- function(y, layer) {
  identical(match(-y, sort(unique(-y))) - 1L, as.integer(layer))
}

# Recompute rank membership, within-rank order and rank gaps after nodes move.
.rerank_geometry <- function(geometry) {
  geometry$layer <- match(-geometry$y, sort(unique(-geometry$y))) - 1L
  for (rank in unique(geometry$layer)) {
    members <- which(geometry$layer == rank)
    geometry$ord[members] <- rank(geometry$x[members], ties.method = "first")
  }
  geometry$gaps <- diff(sort(unique(-geometry$y)))
  geometry
}

# Width and depth of the box holding every station and route step: the
# diff(range()) of the station and bound route coordinates, without binding
# the route tables first.
.drawing_extent <- function(nodes, routes) {
  coordinate <- function(key) unlist(lapply(routes, .subset2, key), use.names = FALSE)
  c(diff(range(c(nodes$x, coordinate("x"), coordinate("xend")))),
    diff(range(c(nodes$y, coordinate("y"), coordinate("yend")))))
}

# Per-route values read without a `$.data.frame` call per route: the source
# and target names, whether a route bends, its bend count and its steps.
.route_from <- function(routes) vapply(lapply(routes, .subset2, "from"), `[`, character(1), 1L)
.route_to <- function(routes) vapply(lapply(routes, .subset2, "to"), `[`, character(1), 1L)
.route_bent <- function(routes) vapply(lapply(routes, .subset2, "bend"), any, logical(1))
.route_bends <- function(routes) vapply(lapply(routes, .subset2, "bend"), sum, numeric(1))
.route_steps <- function(routes) lengths(lapply(routes, .subset2, "x"))
