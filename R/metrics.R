# Merge connected collinear steps before pairwise geometric diagnostics.
.collapse_routes <- function(e) {
  if (!nrow(e)) return(e)
  # Use factor labels without empty groups or unused tapply() levels.
  if (is.factor(e$edge_id)) e$edge_id <- as.character(e$edge_id)
  # Private groups retain numeric identity; merge straight connected runs
  # for all routes at once instead of splitting and rebinding per edge.
  edge_id <- .subset2(e, "edge_id")
  groups <- match(edge_id, unique(edge_id))
  o <- order(groups, .subset2(e, "segment"), method = "radix")
  # Tables already in route order, as internal routes are, keep their rows.
  z <- if (is.unsorted(o)) .take_rows(e, o) else e
  g <- groups[o]
  x <- .subset2(z, "x"); y <- .subset2(z, "y")
  xend <- .subset2(z, "xend"); yend <- .subset2(z, "yend")
  dx <- as.double(xend) - x; dy <- as.double(yend) - y
  bad <- unique(g[!is.finite(dx) | !is.finite(dy)])
  for (b in bad) {
    rows <- which(g == b)
    scale <- max(abs(c(x[rows], y[rows], xend[rows], yend[rows])))
    dx[rows] <- xend[rows] / scale - x[rows] / scale
    dy[rows] <- yend[rows] / scale - y[rows] / scale
  }
  n <- length(x)
  if (n > 1L) {
    same <- g[-n] == g[-1L]
    connected <- xend[-n] == x[-1L] & yend[-n] == y[-1L]
    angle <- atan2(dy, dx)
    split_after <- !same | angle[-1L] - angle[-n] != 0 | !connected
    split_after[is.na(split_after)] <- TRUE
  } else split_after <- logical()
  starts <- c(1L, which(split_after) + 1L)
  ends <- c(starts[-1L] - 1L, n)
  out <- lapply(z, `[`, starts)
  out$xend <- xend[ends]; out$yend <- yend[ends]
  .as_rows(out, length(starts))
}

# Row subset of a plain internal table without `[.data.frame` overhead.
# Row names become automatic; callers never rely on them.
.take_rows <- function(d, i) .as_rows(lapply(d, `[`, i), length(i))

# A named list of equal-length columns as a data frame with automatic row
# names, as structure(columns, class = "data.frame", row.names = ...) makes it.
.as_rows <- function(columns, n) {
  attr(columns, "row.names") <- c(NA_integer_, -n)
  class(columns) <- "data.frame"
  columns
}


# Normalize each axis independently so its units do not set crossing
# tolerances. Ignore points when choosing spans, but retain their coordinates
# for original route endpoints. Translate first to retain large-origin detail.
.normalize_crossing_coordinates <- function(e) {
  # Assign columns of the underlying list; the result is the same data frame.
  classes <- oldClass(e)
  e <- unclass(e)
  keys <- c("x", "y", "xend", "yend")
  for (key in keys) e[[key]] <- as.double(e[[key]])
  nonzero <- e$x != e$xend | e$y != e$yend
  if (any(nonzero)) for (axis in c("x", "y")) {
    end <- paste0(axis, "end")
    bounds <- range(c(e[[axis]][nonzero], e[[end]][nonzero]))
    span <- diff(bounds)
    if (!is.finite(span)) {
      scale <- max(abs(bounds))
      for (key in c(axis, end)) e[[key]] <- e[[key]] / scale
      bounds <- range(c(e[[axis]][nonzero], e[[end]][nonzero]))
      span <- diff(bounds)
    }
    if (span > 0) for (key in c(axis, end))
      e[[key]] <- (e[[key]] - bounds[1L]) / span
  }
  class(e) <- classes
  e
}

#' Count edge crossings
#'
#' Counts the places where one routed edge passes from one side of another to
#' the other side. Two arrows that leave or enter the same station and then
#' part (a fork or merge), touch at a point, or share a section and leave it on
#' the sides they entered are not crossings; shared sections are reported by
#' [layout_metrics()]. An edge that runs through a shared section and leaves
#' on the other side crosses once. For a pair of edges that are not both
#' monotone along one axis (which every layout from [layout_metro_dag()] is),
#' each distinct intersection point other than a shared original endpoint
#' counts. Gaps between segments remain gaps: a route with gaps is compared
#' piece by piece, and only connected collinear segments merge.
#' Calculations normalize each coordinate axis independently and use
#' floating-point precision for boundary contacts and repeated intersections.
#' Distinct nearby crossings are not merged by rounding coordinates to a fixed
#' number of decimal places.
#' Zero-length segments do not contribute crossings or set the normalization
#' scales; their original endpoint positions are retained.
#' @param x A `metrodag_layout`, or its edge-segment data frame. Requires
#'   non-missing `edge_id` values and finite real numeric coordinates `x`, `y`,
#'   `xend`, and `yend`. Factor edge IDs use their labels; unused levels are
#'   ignored. Optional `segment` supplies finite real numeric ordering values
#'   within each edge; otherwise input row order is used.
#' @return A single non-negative integer: the number of crossings between
#'   routes of different edges.
#' @examples
#' layout <- layout_metro_dag(metrodag_examples("diamond"))
#' count_crossings(layout)
#' count_crossings(layout$edges)
#' @export
count_crossings <- function(x) {
  edges <- if (inherits(x, "metrodag_layout")) x$edges else x
  required <- c("edge_id", "x", "y", "xend", "yend")
  if (!is.data.frame(edges) || !all(required %in% names(edges)))
    stop("`x` must be a metrodag layout or edge-segment data frame.", call. = FALSE)
  if (!all(vapply(c("x", "y", "xend", "yend"),
      function(key) .real_finite_vector(edges[[key]]), logical(1))))
    stop("Edge segments must have finite real numeric coordinates.", call. = FALSE)
  if (anyNA(edges$edge_id) ||
      (is.factor(edges$edge_id) && anyNA(as.character(edges$edge_id))))
    stop("`edge_id` must not contain missing identifiers.", call. = FALSE)
  if ("segment" %in% names(edges) && !.real_finite_vector(edges$segment))
    stop("`segment` must contain finite real numeric ordering values.", call. = FALSE)
  .count_prepared_crossings(edges, side_swaps = TRUE)
}

# Distinct contact points between routes, excluding shared original
# endpoints: the measure the layout refinement passes compare. Unlike
# count_crossings(), a fork or merge at a station counts here.
.count_route_contacts <- function(edges) .count_prepared_crossings(edges, side_swaps = FALSE)

.count_prepared_crossings <- function(edges, side_swaps) {
  if (nrow(edges) < 2L) return(0L)
  # Private integer IDs prevent ambiguous event keys for labels with spaces.
  edges$edge_id <- match(edges$edge_id, unique(edges$edge_id))
  if (!"segment" %in% names(edges)) edges$segment <- seq_len(nrow(edges))
  # Edges already in this order, as layouts store them, keep their rows.
  o <- order(edges$edge_id, edges$segment, method = "radix")
  if (is.unsorted(o)) edges <- edges[o, , drop = FALSE]
  e <- .collapse_routes(edges)
  if (nrow(e) < 2L) return(0L)
  nonzero <- e$x != e$xend | e$y != e$yend
  if (sum(nonzero) < 2L) return(0L)
  # Collapse in the original frame: rescaling can introduce tiny direction
  # differences in otherwise exactly collinear grid steps.
  # Side swaps compare positions, whose rounding error grows with the
  # coordinates' magnitude relative to their span (for example after adding
  # a large offset), so their tolerance does too.
  magnitude <- vapply(c("x", "y"), function(axis) {
    values <- as.double(c(e[[axis]][nonzero], e[[paste0(axis, "end")]][nonzero]))
    span <- diff(range(values))
    if (is.finite(span) && span > 0) max(abs(values)) / span else 1
  }, numeric(1))
  e <- .normalize_crossing_coordinates(e)
  tolerance <- 64 * .Machine$double.eps
  .count_segment_crossings(e, nonzero, tolerance, side_swaps,
    swap_tolerance = tolerance * max(1, magnitude[is.finite(magnitude)]))
}

# Vectorised over all candidate segment pairs. Pair order, intersection
# arithmetic, terminal exclusions and per-pair deduplication match the
# reference row-by-row loop exactly.
.count_segment_crossings <- function(e, nonzero, tolerance, side_swaps = TRUE,
  swap_tolerance = tolerance) {
  n <- nrow(e)
  ids <- e$edge_id
  first <- !duplicated(ids); last <- !duplicated(ids, fromLast = TRUE)
  start_x <- e$x[first][match(ids, ids[first])]; start_y <- e$y[first][match(ids, ids[first])]
  end_x <- e$xend[last][match(ids, ids[last])]; end_y <- e$yend[last][match(ids, ids[last])]
  xmin <- pmin(e$x, e$xend); xmax <- pmax(e$x, e$xend)
  ymin <- pmin(e$y, e$yend); ymax <- pmax(e$y, e$yend)
  margin <- 1e-9
  counted <- 0L
  chunk <- max(1L, floor(2e6 / max(1L, n)))
  candidates <- list()
  for (lo in seq.int(1L, max(1L, n - 1L), by = chunk)) {
    rows <- seq.int(lo, min(n - 1L, lo + chunk - 1L))
    if (!length(rows) || lo > n - 1L) break
    I <- rep(rows, times = n - rows)
    J <- unlist(lapply(rows, function(i) seq.int(i + 1L, n)), use.names = FALSE)
    keep <- nonzero[I] & nonzero[J] & ids[I] != ids[J] &
      pmax(xmin[I], xmin[J]) <= pmin(xmax[I], xmax[J]) + margin &
      pmax(ymin[I], ymin[J]) <= pmin(ymax[I], ymax[J]) + margin
    I <- I[keep]; J <- J[keep]
    if (!length(I)) next
    ax <- e$xend[I] - e$x[I]; ay <- e$yend[I] - e$y[I]
    bx <- e$xend[J] - e$x[J]; by <- e$yend[J] - e$y[J]
    a_length <- pmax(abs(ax), abs(ay))
    b_length <- pmax(abs(bx), abs(by))
    a_dx <- ax / a_length; a_dy <- ay / a_length
    b_dx <- bx / b_length; b_dy <- by / b_length
    denominator <- a_dx * b_dy - a_dy * b_dx
    valid <- abs(denominator) > tolerance
    denominator[!valid] <- 1
    t <- ((e$x[J] - e$x[I]) * b_dy - (e$y[J] - e$y[I]) * b_dx) / (denominator * a_length)
    u <- ((e$x[J] - e$x[I]) * a_dy - (e$y[J] - e$y[I]) * a_dx) / (denominator * b_length)
    hit <- valid & t >= -tolerance & t <= 1 + tolerance & u >= -tolerance & u <= 1 + tolerance
    if (!any(hit)) next
    I <- I[hit]; J <- J[hit]; t <- t[hit]
    px <- e$x[I] + t * ax[hit]; py <- e$y[I] + t * ay[hit]
    terminal <- function(k) (abs(start_x[k] - px) + abs(start_y[k] - py) <= tolerance) |
      (abs(end_x[k] - px) + abs(end_y[k] - py) <= tolerance)
    inner <- !(terminal(I) & terminal(J))
    if (any(inner)) candidates[[length(candidates) + 1L]] <-
      list(a = ids[I][inner], b = ids[J][inner], px = px[inner], py = py[inner])
  }
  if (!length(candidates)) return(0L)
  hits <- lapply(c(a = "a", b = "b", px = "px", py = "py"), function(key)
    unlist(lapply(candidates, .subset2, key), use.names = FALSE))
  hits <- .as_rows(hits, length(hits$a))
  # Rows arrive in (i, j) order, as in the reference loop. Keep a point unless
  # an earlier kept point of the same edge pair lies within tolerance.
  key <- paste(hits$a, hits$b, sep = ":")
  contacts <- stats::setNames(as.integer(table(factor(key, levels = unique(key)))), unique(key))
  repeated <- duplicated(key) | duplicated(key, fromLast = TRUE)
  for (group in split(seq_len(nrow(hits))[repeated], key[repeated])) {
    kept <- group[1L]
    for (k in group[-1L])
      if (!any(abs(hits$px[kept] - hits$px[k]) + abs(hits$py[kept] - hits$py[k]) <= tolerance))
        kept <- c(kept, k)
    contacts[[key[group[1L]]]] <- length(kept)
  }
  if (!side_swaps) return(as.integer(sum(contacts)))
  # A pair of routes that touch crosses only where one changes sides of the
  # other. Forks, merges and shared sections that keep the order are not
  # crossings. A route with gaps is compared piece by piece. Pairs of pieces
  # without a common monotone axis keep the route pair's count of distinct
  # contact points.
  # Only routes that touch another matter; others may be far-off points.
  pairs <- unique(hits[c("a", "b")])
  touching <- unique(c(pairs$a, pairs$b))
  routes <- e[ids %in% touching, c("x", "y", "xend", "yend")]
  route_ids <- ids[ids %in% touching]
  pieces <- lapply(split(routes, factor(route_ids, levels = unique(route_ids))),
    .route_pieces, tolerance = tolerance)
  names(pieces) <- as.character(unique(route_ids))
  all_pieces <- unlist(pieces, recursive = FALSE)
  # Use the causal axis of the drawing, along which every route advances, so
  # the count does not depend on the drawing's direction.
  monotone <- vapply(1:2, function(axis) all(vapply(all_pieces, .polyline_monotone,
    logical(1), axis = axis, tolerance = tolerance)), logical(1))
  extent <- vapply(1:2, function(axis) sum(vapply(all_pieces, function(z) diff(range(z[, axis])),
    numeric(1))), numeric(1))
  prefer <- if (any(monotone)) which(monotone)[which.max(extent[monotone])] else 1L
  total <- 0L
  for (k in seq_len(nrow(pairs))) {
    a <- as.character(pairs$a[k]); b <- as.character(pairs$b[k])
    swaps <- 0L
    for (p in pieces[[a]]) for (q in pieces[[b]]) {
      axis <- .polyline_axis(p, q, prefer, tolerance)
      if (is.na(axis)) { swaps <- NA_integer_; break }
      swaps <- swaps + .polyline_side_swaps(p, q, axis, swap_tolerance)
    }
    total <- total + if (is.na(swaps)) contacts[[paste(a, b, sep = ":")]] else swaps
  }
  as.integer(total)
}

# The connected pieces of one route, each as a matrix of points. A gap
# between consecutive segments starts a new piece.
.route_pieces <- function(segments, tolerance = 0) {
  n <- nrow(segments)
  gap <- if (n > 1L) which(abs(segments$x[-1L] - segments$xend[-n]) > tolerance |
    abs(segments$y[-1L] - segments$yend[-n]) > tolerance) else integer()
  starts <- c(1L, gap + 1L); ends <- c(gap, n)
  lapply(seq_along(starts), function(k) {
    z <- segments[starts[k]:ends[k], , drop = FALSE]
    rbind(c(z$x[1L], z$y[1L]), cbind(z$xend, z$yend))
  })
}

.polyline_points <- function(segments)
  rbind(c(segments$x[1L], segments$y[1L]), cbind(segments$xend, segments$yend))

.polyline_monotone <- function(z, axis, tolerance = 0) {
  if (!all(is.finite(z))) return(FALSE)
  d <- diff(z[, axis])
  all(d >= -tolerance) || all(d <= tolerance)
}

# An axis along which both polylines are monotone and extend, trying
# `prefer` first, or NA.
.polyline_axis <- function(p, q, prefer = 1L, tolerance = 0) {
  for (axis in unique(c(prefer, 1:2))) {
    usable <- vapply(list(p, q), function(z) .polyline_monotone(z, axis, tolerance) &&
      diff(range(z[, axis])) > tolerance, logical(1))
    if (all(usable)) return(axis)
  }
  NA_integer_
}

# Number of times q changes sides of p over their common range along `axis`.
# Both curves are read as functions of the axis coordinate. At a run
# perpendicular to the axis a curve has two values, the one it arrives with
# and the one it leaves with; between vertices it is interpolated on its own
# segment. Outside the common range a curve has no side, so at its ends only
# the values inside the range count.
.polyline_side_swaps <- function(p, q, axis, tolerance = 0) {
  other <- 3L - axis
  orient <- function(z) {
    if (z[nrow(z), axis] < z[1L, axis]) z <- z[rev(seq_len(nrow(z))), , drop = FALSE]
    z[, axis] <- cummax(z[, axis])
    z
  }
  p <- orient(p); q <- orient(q)
  lo <- max(p[1L, axis], q[1L, axis]); hi <- min(p[nrow(p), axis], q[nrow(q), axis])
  # Routes that share no interval along the axis cannot change sides.
  if (hi - lo <= tolerance) return(0L)
  ts <- sort(unique(c(p[, axis], q[, axis])))
  ts <- c(lo, ts[ts > lo + tolerance & ts < hi - tolerance], hi)
  starts_together <- abs(p[1L, axis] - q[1L, axis]) <= tolerance
  ends_together <- abs(p[nrow(p), axis] - q[nrow(q), axis]) <= tolerance
  value <- function(z, t, leaving) {
    a <- z[, axis]; v <- z[, other]
    at <- which(abs(a - t) <= tolerance)
    if (length(at)) {
      # Only values inside the common range count: at its start, a curve
      # that continues before it, or both curves when both begin there, are
      # read as they leave; at its end, likewise as they arrive.
      if (t <= lo + tolerance && (a[1L] < lo - tolerance || starts_together)) leaving <- TRUE
      if (t >= hi - tolerance && (a[length(a)] > hi + tolerance || ends_together)) leaving <- FALSE
      return(v[if (leaving) at[length(at)] else at[1L]])
    }
    k <- max(which(a < t))
    v[k] + (v[k + 1L] - v[k]) * (t - a[k]) / (a[k + 1L] - a[k])
  }
  d <- unlist(lapply(ts, function(t) c(value(p, t, FALSE) - value(q, t, FALSE),
    value(p, t, TRUE) - value(q, t, TRUE))), use.names = FALSE)
  s <- sign(d[abs(d) > tolerance])
  as.integer(sum(s[-1L] != s[-length(s)]))
}

.segment_overlap <- function(a, b) {
  if (a$edge_id == b$edge_id) return(FALSE)
  da <- c(a$xend - a$x, a$yend - a$y); db <- c(b$xend - b$x, b$yend - b$y)
  cross <- da[1L] * db[2L] - da[2L] * db[1L]
  collinear <- cross == 0 && (b$x - a$x) * da[2L] == (b$y - a$y) * da[1L]
  if (!collinear) return(FALSE)
  ax <- sort(c(a$x, a$xend)); bx <- sort(c(b$x, b$xend))
  ay <- sort(c(a$y, a$yend)); by <- sort(c(b$y, b$yend))
  min(ax[2L], bx[2L]) > max(ax[1L], bx[1L]) || min(ay[2L], by[2L]) > max(ay[1L], by[1L])
}

#' Quantitative metrics for a MetroDAG drawing
#'
#' Diagnostics describe stored geometry, before physical-size arrow attachments
#' and label placement. `node_edge_collisions` counts distinct unrelated
#' node/edge pairs with distance below half a grid unit. `overlap_segments`
#' counts collinear overlapping pairs after merging straight runs. A graph can
#' have unavoidable crossings and shared terminal tracks at high-degree nodes.
#' `shared_track_length` sums the length of each overlapping edge pair; a track
#' shared by three arrows therefore contributes three times its length.
#' Zero-length steps are ignored when merging straight runs and measuring
#' route lengths, overlaps and node/edge collisions.
#'
#' `directional_consistency` is the share of route steps that advance along
#' the causal direction. It is an invariant check rather than a quality score:
#' every layout computed by [layout_metro_dag()] has the value 1, and lower
#' values indicate an edited layout whose routes double back.
#' `node_spacing_variance` is the variance of all pairwise station distances;
#' it grows with graph size and is only comparable between drawings of the
#' same graph.
#' @param layout A `metrodag_layout`.
#'   Its nodes must match the graph vertices one-to-one. Its edge rows must
#'   describe exactly one continuous route for every graph edge, with finite
#'   coordinates, unambiguous segment order, non-missing route IDs and endpoints
#'   attached to their named nodes; see [plot_metro_dag()]. Route IDs are compared
#'   by value, so distinct numeric IDs remain separate even if they print alike.
#' @return A one-row data frame with these columns. Lengths (Euclidean, so a
#'   diagonal step is longer than a straight one) and areas are in layout
#'   units, the grid on which stations and routes are placed.
#'
#'   * `nodes`, `edges`: the numbers of nodes and edges in the graph.
#'   * `crossings`: the crossings counted by [count_crossings()].
#'   * `total_bends`, `mean_bends`: the bends of all routes, in total and per
#'     edge.
#'   * `total_edge_length`, `maximum_edge_length`: the length of all routes
#'     together, and of the longest route.
#'   * `area`, `aspect_ratio`: the area and the width-to-height ratio of the
#'     box enclosing every station and route (`Inf` for a drawing with no
#'     height).
#'   * `node_spacing_variance`, `directional_consistency`, `overlap_segments`,
#'     `shared_track_length`, `node_edge_collisions`: as described above.
#' @examples
#' layout <- layout_metro_dag(metrodag_examples("causal"))
#' layout_metrics(layout)
#' @export
layout_metrics <- function(layout) {
  if (!inherits(layout, "metrodag_layout")) stop("`layout` must be a metrodag_layout.", call. = FALSE)
  .assert_layout_nodes(layout)
  .assert_layout_edges(layout)
  .assert_layout_geometry(layout)
  # A stationary step must not split a straight run and inflate overlaps.
  # count_crossings() keeps its separate original-endpoint semantics for
  # standalone tables, including disconnected zero-length fragments.
  e <- layout$edges
  e <- .collapse_routes(e[e$x != e$xend | e$y != e$yend, , drop = FALSE])
  n <- layout$nodes
  lengths <- if (nrow(e)) sqrt((e$xend - e$x)^2 + (e$yend - e$y)^2) else numeric()
  edge_lengths <- if (nrow(e)) tapply(lengths, match(e$edge_id, unique(e$edge_id)), sum) else numeric()
  overlaps <- 0L
  shared_length <- 0
  if (nrow(e) > 1L) for (i in seq_len(nrow(e) - 1L)) {
    ids <- seq.int(i + 1L, nrow(e))
    ids <- ids[vapply(ids, function(j) .segment_overlap(e[i, ], e[j, ]), logical(1))]
    overlaps <- overlaps + length(ids)
    if (length(ids)) {
      axis <- if (abs(e$xend[i] - e$x[i]) >= abs(e$yend[i] - e$y[i])) "x" else "y"
      end <- paste0(axis, "end")
      a <- sort(c(e[[axis]][i], e[[end]][i]))
      overlap <- pmin(a[2L], pmax(e[[axis]][ids], e[[end]][ids])) -
        pmax(a[1L], pmin(e[[axis]][ids], e[[end]][ids]))
      shared_length <- shared_length + sum(overlap) * lengths[i] / diff(a)
    }
  }
  collisions <- sum(vapply(seq_len(nrow(n)), function(i) {
    z <- e[e$from != n$node[i] & e$to != n$node[i], , drop = FALSE]
    d <- .point_segment_distance(n$x[i], n$y[i], z$x, z$y, z$xend, z$yend)
    length(unique(z$edge_id[d < 0.5]))
  }, integer(1)))
  width <- diff(range(c(n$x, e$x, e$xend))); height <- diff(range(c(n$y, e$y, e$yend)))
  distances <- as.vector(stats::dist(n[, c("x", "y")]))
  data.frame(nodes = nrow(n), edges = igraph::ecount(layout$graph),
    crossings = count_crossings(layout), total_bends = sum(layout$edges$bend),
    mean_bends = if (igraph::ecount(layout$graph)) sum(layout$edges$bend) / igraph::ecount(layout$graph) else 0,
    total_edge_length = sum(lengths), maximum_edge_length = if (length(edge_lengths)) max(edge_lengths) else 0,
    area = width * height, aspect_ratio = if (height == 0) Inf else width / height,
    node_spacing_variance = if (length(distances) > 1L) stats::var(distances) else 0,
    directional_consistency = if (nrow(e)) mean(switch(layout$direction,
      down = e$yend <= e$y, up = e$yend >= e$y, right = e$xend >= e$x, left = e$xend <= e$x)) else 1,
    overlap_segments = overlaps, shared_track_length = shared_length,
    node_edge_collisions = collisions)
}
