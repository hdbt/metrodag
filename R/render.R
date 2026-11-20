# Geometry here is in physical millimetres, after ggplot transforms the panel.
# Consequently text extents and arrow clearance stay correct across devices.
# Grid line widths use nominal 1/96-inch units.
.metro_lwd <- function(mm) mm * 96 / 25.4

.point_segment_distance <- function(px, py, ax, ay, bx, by) {
  len <- (bx - ax)^2 + (by - ay)^2
  # Protect zero-length denominators: the points or the segments can
  # be vectors, and a scalar ifelse() must not truncate point projections.
  projection <- ((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / (len + (len == 0))
  t <- pmax(0, pmin(1, projection))
  sqrt((px - ax - t * (bx - ax))^2 + (py - ay - t * (by - ay))^2)
}

# Liang-Barsky clipping, including contacts with a box boundary. Segments
# pair up with the rows of `box` (xmin, xmax, ymin, ymax), or all meet the
# one box of a single-row matrix.
.segments_hit_boxes <- function(x, y, xend, yend, box) {
  lo <- rep(0, length(x)); hi <- rep(1, length(x))
  for (axis in 1:2) {
    a <- if (axis == 1L) x else y
    d <- (if (axis == 1L) xend else yend) - a
    lower <- box[, 2L * axis - 1L]; upper <- box[, 2L * axis]
    parallel <- abs(d) < 1e-10
    outside <- parallel & (a < lower | a > upper)
    d[parallel] <- 1
    t1 <- (lower - a) / d; t2 <- (upper - a) / d
    enter <- pmin(t1, t2); enter[parallel] <- -Inf
    leave <- pmax(t1, t2); leave[parallel] <- Inf
    lo <- pmax(lo, enter); hi <- pmin(hi, leave)
    hi[outside] <- -Inf
  }
  hi >= lo
}

.segment_hits_box <- function(e, box) {
  if (!nrow(e)) return(logical())
  .segments_hit_boxes(e$x, e$y, e$xend, e$yend, matrix(box, 1L))
}

# Distance between segments and boxes paired as in .segments_hit_boxes().
.segments_box_distance <- function(x, y, xend, yend, box) {
  distance <- function(px, py) sqrt(
    pmax(box[, 1L] - px, px - box[, 2L], 0)^2 +
    pmax(box[, 3L] - py, py - box[, 4L], 0)^2)
  result <- pmin(distance(x, y), distance(xend, yend))
  # If neither segment endpoint is closest, a rectangle corner is closest.
  for (cx in 1:2) for (cy in 3:4)
    result <- pmin(result, .point_segment_distance(box[, cx], box[, cy], x, y, xend, yend))
  result[.segments_hit_boxes(x, y, xend, yend, box)] <- 0
  result
}

# Distance between segments ab and cd, element by element: 0 where they
# cross, otherwise the nearest endpoint-to-segment distance.
.segments_distance <- function(ax, ay, bx, by, cx, cy, dx, dy) {
  side <- function(px, py, qx, qy, rx, ry) (qx - px) * (ry - py) - (qy - py) * (rx - px)
  result <- pmin(.point_segment_distance(ax, ay, cx, cy, dx, dy),
    .point_segment_distance(bx, by, cx, cy, dx, dy),
    .point_segment_distance(cx, cy, ax, ay, bx, by),
    .point_segment_distance(dx, dy, ax, ay, bx, by))
  crossing <- side(cx, cy, dx, dy, ax, ay) * side(cx, cy, dx, dy, bx, by) < 0 &
    side(ax, ay, bx, by, cx, cy) * side(ax, ay, bx, by, dx, dy) < 0
  result[crossing] <- 0
  result
}

# Signed separation of filled rectangles: negative for interior overlap,
# otherwise the Euclidean distance between their nearest sides or corners.
.metro_rectangle_distance <- function(dx, dy) {
  inside <- pmax(dx, dy)
  ifelse(inside < 0, inside, sqrt(pmax(dx, 0)^2 + pmax(dy, 0)^2))
}

.segment_rectangle_distance <- function(e, box)
  .segments_box_distance(e$x, e$y, e$xend, e$yend, matrix(box, 1L))

.box_overlap <- function(a, b) {
  if (!nrow(b)) return(numeric())
  .box_overlaps(matrix(a, 1L), b)[1L, ]
}

# Overlap areas of every box in `a` with every box in `b`, as an
# nrow(a) x nrow(b) matrix.
.box_overlaps <- function(a, b) {
  m <- nrow(a); p <- nrow(b)
  if (!p) return(matrix(0, m, 0L))
  side <- function(lo, hi) pmax(0, pmin(a[, hi], rep(b[, hi], each = m)) -
    pmax(a[, lo], rep(b[, lo], each = m)))
  matrix(side(1L, 2L) * side(3L, 4L), m, p)
}

# Include the final route/head strokes and filled arrowhead interiors. Route
# segments also contain each head's boundary, so containment is the only filled
# polygon case left after checking the segments against the expanded box.
.metro_label_edge_hits <- function(edges, box, line_width = 0, arrowheads = list()) {
  expanded <- box + c(-1, 1, -1, 1) * line_width / 2
  ids <- edges$edge_id[.segment_hits_box(edges, expanded)]
  corner <- matrix(box[c(1L, 3L)], ncol = 2L)
  for (head in arrowheads) {
    if (head$edge_id %in% ids) next
    bounds <- head$bounds
    if (box[1L] < bounds[1L] || box[1L] > bounds[2L] ||
        box[3L] < bounds[3L] || box[3L] > bounds[4L]) next
    if (.points_in_triangle(corner, head$triangle)) ids <- c(ids, head$edge_id)
  }
  unique(ids)
}

# Restrict the panel's reserved margin area to the page. With panel clipping
# enabled, anything outside the panel is invisible even if it fits the page.
.metro_visible_bounds <- function(bounds, width, height, clip) {
  if (identical(clip, "on")) bounds <- c(0, width, 0, height)
  origin <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE)
  device <- grDevices::dev.size("in") * 25.4
  page <- c(-origin$x * 25.4, device[1L] - origin$x * 25.4,
    -origin$y * 25.4, device[2L] - origin$y * 25.4)
  c(max(bounds[1L], page[1L]), min(bounds[2L], page[2L]),
    max(bounds[3L], page[3L]), min(bounds[4L], page[4L]))
}

# The export size at which the drawing grows by `scale`, as named width and/or
# height in the units of save_metro_dag()'s millimetre arguments, or NULL.
# The panel keeps its aspect ratio, so both of its dimensions grow; the
# figure must grow in each dimension whose available room is too small,
# except that an automatic export height follows the width up to 220 mm.
# A comparison panel's figure size is its cell; the figure repeats the cell
# in its columns and rows, with gaps between them and the legend below.
.metro_needed_size <- function(cfg, panel, scale) {
  if (is.null(cfg$figure_size) || is.null(cfg$available) || !is.finite(scale) || scale <= 1)
    return(NULL)
  needed <- panel * 1.05 * scale
  fixed <- cfg$figure_size - cfg$available
  grow <- needed > cfg$available + 0.5
  automatic <- isTRUE(cfg$export$automatic_height)
  # Automatic heights fit the panel, so a panel limited by an automatic
  # height was limited to 220 mm. A taller figure alone lets it grow until it
  # fills the available width, at the height automatic sizing would have
  # chosen. Advise that first, at the current width; diagnose_metro_dag()
  # checks it and grows the width only if the figure still does not fit.
  if (automatic && identical(cfg$limiting, "height") && panel[1L] > 0) {
    needed[2L] <- min(needed[2L], cfg$available[1L] * panel[2L] / panel[1L])
    grow <- c(FALSE, needed[2L] > cfg$available[2L] + 0.5)
  }
  figure <- fixed + needed
  grid <- cfg$comparison
  if (!is.null(grid)) figure <- c(grid$columns * figure[1L] + grid$gap * (grid$columns - 1L),
    grid$rows * figure[2L] + grid$gap * (grid$rows - 1L) + grid$legend)
  if (automatic && figure[2L] <= 220) grow[2L] <- FALSE
  if (automatic && grow[1L]) grow[2L] <- grow[2L] || figure[2L] > 220
  if (!any(grow)) return(NULL)
  export_scale <- if (is.null(cfg$export$scale)) 1 else cfg$export$scale
  target <- ceiling(figure / export_scale / 5) * 5
  stats::setNames(target, c("width", "height"))[grow]
}

# The larger of two sizes from .metro_needed_size(), dimension by dimension,
# as when several comparison panels each need the figure to grow.
.metro_larger_size <- function(a, b) {
  keys <- intersect(c("width", "height"), c(names(a), names(b)))
  stats::setNames(vapply(keys, function(k) max(c(a[k], b[k]), na.rm = TRUE), numeric(1)), keys)
}

.metro_point_polyline_distance <- function(point, xy) {
  if (nrow(xy) < 2L) return(sqrt(sum((xy[1L, ] - point)^2)))
  a <- xy[-nrow(xy), , drop = FALSE]; d <- xy[-1L, , drop = FALSE] - a
  length2 <- rowSums(d^2)
  t <- pmin(1, pmax(0, ((point[1L] - a[, 1L]) * d[, 1L] + (point[2L] - a[, 2L]) * d[, 2L]) /
    pmax(length2, 1e-12)))
  min(sqrt((a[, 1L] + t * d[, 1L] - point[1L])^2 + (a[, 2L] + t * d[, 2L] - point[2L])^2))
}

.metro_outside_bounds <- function(x, y, radius, bounds) {
  !is.finite(x) | !is.finite(y) |
    x - radius < bounds[1L] - 1e-8 | x + radius > bounds[2L] + 1e-8 |
    y - radius < bounds[3L] - 1e-8 | y + radius > bounds[4L] + 1e-8
}

# Gap between a station's outline and a label box, for every station.
# Square stations (shape 22) are measured to their sides and corners.
.metro_rim_distance <- function(box, nodes, radius)
  .metro_rim_distances(matrix(box, 1L), nodes, radius)$rim[1L, ]

# The same for many boxes: `rim` holds the gaps and `centre` the distances
# to the station centres, one row per box and one column per station.
.metro_rim_distances <- function(boxes, nodes, radius) {
  m <- nrow(boxes); n <- nrow(nodes)
  x <- rep(nodes$x, each = m); y <- rep(nodes$y, each = m)
  radius <- rep(rep_len(radius, n), each = m)
  dx <- pmax(0, boxes[, 1L] - x, x - boxes[, 2L])
  dy <- pmax(0, boxes[, 3L] - y, y - boxes[, 4L])
  centre <- sqrt(dx^2 + dy^2)
  gap <- centre - radius
  square <- rep(!is.null(nodes$shape) & nodes$shape %in% 22L, each = m)
  if (any(square)) gap[square] <- .metro_rectangle_distance(dx[square] - radius[square],
    dy[square] - radius[square])
  list(rim = matrix(pmax(0, gap), m, n), centre = matrix(centre, m, n))
}

# Is a label at gap `own` from its own station read as belonging to a station
# at gap `other`? Only if it is clearly nearer that station: by more than
# `tolerance` mm and either far from its own station or half as far again
# from it. A label tucked 1 mm beside its station in a tight grid, with the
# next station 0.8 mm away, still reads as its own.
.metro_nearer <- function(other, own, tolerance = 0.5)
  other + tolerance < own & (own > 3 | own > 1.5 * other)

# Gap in mm between a station's rim and a label beyond which a label cut off
# from its station by another arrow is reported.
.metro_cut_off_rim <- 7

# Labels that read as another station's: label i is nearer station k than its
# own station ("nearer"), nearer station k than k's own label ("closer"), or
# cut off from its station by the arrow from -> to ("cut off"): the stroke of
# an arrow that neither starts nor ends at the station crosses the line from
# the station centre to the nearest point of the label, and the label is more
# than 7 mm from the station's rim. Readers look for a label's station on its
# side of the lines, so a label far beyond a route reads as a stray. Placed
# just beyond a route one lane from its station, such as a mediator's bypass,
# a label lands in the second or third ring of slots (up to about 6 mm from
# the rim), in line with its station, and still reads as its own; diagonal
# slots of that ring and farther rings start at about 8.5 mm. Cut-offs need
# the drawn `edges`. .place_labels() uses the same threshold.
.metro_label_ambiguity <- function(nodes, boxes, radius, tolerance = 0.5,
  edges = NULL, line_width = 0) {
  none <- data.frame(node = character(), other_node = character(), relation = character(),
    from = character(), to = character())
  active <- which(!is.na(boxes[, 1L]))
  if (nrow(nodes) < 2L || !length(active)) return(none)
  radius <- rep_len(radius, nrow(nodes))
  rim <- matrix(NA_real_, nrow(nodes), nrow(nodes))
  for (i in active) rim[i, ] <- .metro_rim_distance(boxes[i, ], nodes, radius)
  index <- if (!is.null(edges)) .label_route_index(edges)
  rows <- list()
  add <- function(i, relation, other_node = NA_character_, from = NA_character_, to = NA_character_)
    rows[[length(rows) + 1L]] <<- data.frame(node = nodes$node[i], other_node = other_node,
      relation = relation, from = from, to = to)
  for (i in active) {
    nearer <- setdiff(which(.metro_nearer(rim[i, ], rim[i, i], tolerance)), i)
    closer <- setdiff(active[.metro_nearer(rim[i, active], rim[cbind(active, active)], tolerance)], i)
    closer <- setdiff(closer, nearer)
    if (length(nearer)) add(i, "nearer", nodes$node[nearer])
    if (length(closer)) add(i, "closer", nodes$node[closer])
    if (is.null(index) || rim[i, i] <= .metro_cut_off_rim) next
    crossed <- .label_route_terms(boxes[i, , drop = FALSE], c(nodes$x[i], nodes$y[i]), edges,
      index, line_width)$crossed[1L, ] & !(index$from %in% nodes$node[i] | index$to %in% nodes$node[i])
    if (any(crossed)) add(i, "cut off", from = index$from[crossed], to = index$to[crossed])
  }
  if (length(rows)) do.call(rbind, rows) else none
}

# Route segments indexed for label placement: each segment's bounding box and
# arrow, and each arrow's source and destination. Arrowheads count as part
# of their arrow; routes without from/to columns belong to no station.
.label_route_index <- function(edges, arrowheads = list()) {
  ids <- unique(c(edges$edge_id, unlist(lapply(arrowheads, `[[`, "edge_id"))))
  first <- match(ids, edges$edge_id)
  endpoint <- function(key) if (is.null(edges[[key]])) rep(NA_character_, length(ids)) else
    as.character(edges[[key]][first])
  list(ids = ids, edge = match(edges$edge_id, ids), from = endpoint("from"), to = endpoint("to"),
    x1 = pmin(edges$x, edges$xend), x2 = pmax(edges$x, edges$xend),
    y1 = pmin(edges$y, edges$yend), y2 = pmax(edges$y, edges$yend))
}

# Segments whose bounding boxes come within `reach` of the paired boxes
# (rows of `boxes`), among the segments `near`: the box row and segment
# number of each such pair.
.label_segment_pairs <- function(boxes, index, near, reach) {
  m <- nrow(boxes)
  gx <- pmax(boxes[, 1L] - rep(index$x2[near], each = m), rep(index$x1[near], each = m) - boxes[, 2L], 0)
  gy <- pmax(boxes[, 3L] - rep(index$y2[near], each = m), rep(index$y1[near], each = m) - boxes[, 4L], 0)
  pair <- which(gx <= reach & gy <= reach) - 1L
  list(box = pair %% m + 1L, segment = near[pair %/% m + 1L])
}

# Route terms of candidate label boxes for the station at `station`, one row
# per box and one column per arrow: whether the box touches the arrow's
# stroke or head (`hit`), the clearance between box and stroke up to `reach`
# (`clear`, Inf beyond), and whether the stroke crosses the line of sight
# from the station centre to the nearest point of the box (`crossed`).
.label_route_terms <- function(boxes, station, edges, index, line_width = 0,
  arrowheads = list(), reach = line_width / 2) {
  m <- nrow(boxes); arrows <- length(index$ids)
  hit <- crossed <- matrix(FALSE, m, arrows)
  clear <- matrix(Inf, m, arrows)
  # The extent of the boxes and the station holds every line of sight.
  extent <- c(min(boxes[, 1L], station[1L]), max(boxes[, 2L], station[1L]),
    min(boxes[, 3L], station[2L]), max(boxes[, 4L], station[2L])) + c(-1, 1, -1, 1) * reach
  near <- which(index$x2 >= extent[1L] & index$x1 <= extent[2L] &
    index$y2 >= extent[3L] & index$y1 <= extent[4L])
  if (length(near)) {
    pairs <- .label_segment_pairs(boxes, index, near, reach)
    r <- pairs$box; s <- pairs$segment
    cell <- cbind(r, index$edge[s])
    x <- edges$x[s]; y <- edges$y[s]; xend <- edges$xend[s]; yend <- edges$yend[s]
    box <- boxes[r, , drop = FALSE]
    touch <- .segments_hit_boxes(x, y, xend, yend,
      box + rep(c(-1, 1, -1, 1) * line_width / 2, each = length(r)))
    hit[cell[touch, , drop = FALSE]] <- TRUE
    gap <- .segments_box_distance(x, y, xend, yend, box) - line_width / 2
    # Assigning in decreasing order leaves each arrow's smallest gap.
    o <- order(gap, decreasing = TRUE)
    clear[cell[o, , drop = FALSE]] <- pmax(0, gap[o])
    sight <- cbind(pmax(boxes[, 1L], pmin(boxes[, 2L], station[1L])),
      pmax(boxes[, 3L], pmin(boxes[, 4L], station[2L])))
    pairs <- .label_segment_pairs(cbind(pmin(sight[, 1L], station[1L]), pmax(sight[, 1L], station[1L]),
      pmin(sight[, 2L], station[2L]), pmax(sight[, 2L], station[2L])), index, near, line_width / 2)
    r <- pairs$box; s <- pairs$segment
    across <- .segments_distance(station[1L], station[2L], sight[r, 1L], sight[r, 2L],
      edges$x[s], edges$y[s], edges$xend[s], edges$yend[s]) <= line_width / 2
    crossed[cbind(r, index$edge[s])[across, , drop = FALSE]] <- TRUE
  }
  # A box inside a filled head touches no stroke; one corner test finds it.
  for (head in arrowheads) {
    col <- match(head$edge_id, index$ids); b <- head$bounds
    rows <- which(!hit[, col] & boxes[, 1L] >= b[1L] & boxes[, 1L] <= b[2L] &
      boxes[, 3L] >= b[3L] & boxes[, 3L] <= b[4L])
    if (length(rows)) hit[rows[.points_in_triangle(boxes[rows, c(1L, 3L), drop = FALSE],
      head$triangle)], col] <- TRUE
  }
  clear[hit] <- 0
  list(hit = hit, clear = clear, crossed = crossed)
}

.place_labels <- function(nodes, edges, sizes, radius, bounds,
  line_width = 0, arrowheads = list(), outline = 0) {
  n <- nrow(nodes)
  radius <- rep_len(radius, n)
  placed <- matrix(NA_real_, n, 4L)
  centers <- matrix(NA_real_, n, 2L)
  choices <- vector("list", n)
  node_boxes <- cbind(nodes$x - radius - 0.6, nodes$x + radius + 0.6,
    nodes$y - radius - 0.6, nodes$y + radius + 0.6)
  index <- .label_route_index(edges, arrowheads)
  for (i in seq_len(n)) {
    if (!nzchar(nodes$label[i])) next
    w <- sizes[i, 1L]; h <- sizes[i, 2L]
    # Compact junctions can have no clear nearby slot between their spokes.
    # Include one farther ring before accepting a label on top of a route.
    offsets <- do.call(rbind, lapply(c(1, 2.5, 5, 8), function(f) {
      dx <- radius[i] + 1.2 * f + w / 2; dy <- radius[i] + 1.2 * f + h / 2
      rbind(c(0, dy), c(0, -dy), c(dx, 0), c(-dx, 0),
        c(dx, dy), c(-dx, dy), c(dx, -dy), c(-dx, -dy))
    }))
    # Slide labels along the near sides of a station. Fixed compass slots can
    # all hit the spokes even when there is a clear gap a few millimetres away.
    # Include both horizontal and vertical slides for rotated drawings.
    for (f in c(1, 2.5, 5, 8)) {
      dx <- radius[i] + 1.2 * f + w / 2
      dy <- radius[i] + 1.2 * f + h / 2
      for (fraction in c(0.25, 0.5, 0.75)) {
        offsets <- rbind(offsets,
          c(fraction * dx, dy), c(-fraction * dx, dy),
          c(fraction * dx, -dy), c(-fraction * dx, -dy),
          c(dx, fraction * dy), c(-dx, fraction * dy),
          c(dx, -fraction * dy), c(-dx, -fraction * dy))
      }
    }
    cx <- nodes$x[i] + offsets[, 1L]; cy <- nodes$y[i] + offsets[, 2L]
    # Also try moving each candidate inward. Long labels near an outer station
    # can then fit without requiring a larger margin or obscuring a station.
    inset_x <- pmax(bounds[1L] + w / 2 + 0.4, pmin(bounds[2L] - w / 2 - 0.4, cx))
    inset_y <- pmax(bounds[3L] + h / 2 + 0.4, pmin(bounds[4L] - h / 2 - 0.4, cy))
    cx <- c(cx, inset_x); cy <- c(cy, inset_y)
    keep <- !duplicated(cbind(cx, cy))
    cx <- cx[keep]; cy <- cy[keep]
    offsets <- cbind(cx - nodes$x[i], cy - nodes$y[i])
    boxes <- cbind(cx - w / 2 - 0.4, cx + w / 2 + 0.4,
      cy - h / 2 - 0.4, cy + h / 2 + 0.4)
    m <- nrow(boxes)
    outside <- pmax(0, bounds[1L] - boxes[, 1L]) + pmax(0, boxes[, 2L] - bounds[2L]) +
      pmax(0, bounds[3L] - boxes[, 3L]) + pmax(0, boxes[, 4L] - bounds[4L])
    # Measure association by distance from the station to the label's
    # nearest side, so a long nearby label need not lose to a distant one.
    gap <- sqrt(pmax(0, abs(offsets[, 1L]) - w / 2)^2 + pmax(0, abs(offsets[, 2L]) - h / 2)^2)
    rims <- .metro_rim_distances(boxes, nodes, radius)
    association <- numeric(m); ambiguous <- logical(m)
    if (n > 1L) {
      center_distance <- matrix(sqrt((cx - rep(nodes$x, each = m))^2 +
        (cy - rep(nodes$y, each = m))^2), m, n)
      excess <- function(d) pmax(0, d[, i] - do.call(pmin, lapply(seq_len(n)[-i], function(k) d[, k])))
      association <- excess(rims$centre) + excess(center_distance)
      # A label nearer another station than its own reads as that station's.
      # That costs more than touching a route, which the label casing keeps
      # legible.
      ambiguous <- rowSums(.metro_nearer(rims$rim[, -i, drop = FALSE], rims$rim[, i])) > 0
    }
    # Route costs, weighed by drawing the gallery, DAGitty examples and random
    # graphs at 85, 120 and 180 mm: a label across a route (600 plus its full
    # clearance cost) costs more than one cut off by a route at any distance,
    # which costs more than one that nearly touches routes on its station's
    # side. Most cut-off labels have no slot clear of routes;
    # .metro_label_ambiguity() reports those left far from their station.
    routes <- .label_route_terms(boxes, c(nodes$x[i], nodes$y[i]), edges, index,
      line_width, arrowheads, 1.5 + line_width / 2)
    own <- index$from %in% nodes$node[i] | index$to %in% nodes$node[i]
    # A route between label and station cuts the label off: readers look for
    # a label's station on its side of the lines.
    separating <- routes$crossed & rep(!own, each = m)
    crossings <- rowSums(separating)
    # A label that only just clears a route looks underlined by it or glued
    # to it. The cost falls to nothing at 1.5 mm clearance. Routes of the
    # station itself run beside its label by nature and count for a third. A
    # route that already cuts the label off is not counted again, since
    # moving away from it takes the label farther from its station.
    near <- pmax(1 - routes$clear / 1.5, 0)^2
    near[separating] <- 0
    clearance <- rowSums(near * rep(ifelse(own, 50, 150), each = m))
    # Slots up to 3 mm from the station rim are equally near; farther slots
    # cost more, up to the cost of a route nearly touched, so that clearance
    # alone moves a label by a ring or two at most.
    distance <- pmin(150, 15 * pmax(0, gap - radius[i] - 3)^2)
    # Gaps to the station's rim as .metro_label_ambiguity() measures them,
    # without the outline stroke that `radius` includes.
    rim <- .metro_rim_distances(boxes, nodes[i, , drop = FALSE], radius[i] - outline)$rim[, 1L]
    # A cut-off label within .metro_cut_off_rim of the rim still reads as its
    # station's and is not reported. Its distance costs at most 60, so one
    # farther out, which is reported, costs at least 90 more with the same
    # routes: a little clearance does not move a label to where it is
    # reported. Both still cost less than a label across a route.
    within <- crossings > 0 & rim <= .metro_cut_off_rim
    distance[within] <- pmin(distance[within], 60)
    base <- 5000 * rowSums(.box_overlaps(boxes, node_boxes) > 0) + 600 * rowSums(routes$hit) +
      2000 * ambiguous + 10000 * outside + association * 10 + gap * 0.5 + seq_len(m) * 0.0001 +
      (crossings > 0) * 350 + 50 * crossings + clearance + distance
    choices[[i]] <- list(boxes = boxes, centers = cbind(cx, cy), base = base, rims = rims$rim)
    # More than 3 mm from its own station and nearer another, a label reads
    # as that station's at a glance, as "Smoking at baseline" beside H and S
    # did in a 120 mm comparison of gallery 01 (`stray`). A clean slot reads
    # as its own and touches no other station or arrow; it may cover the
    # label's own station and arrows, as "Smoking at baseline" covers its
    # square. .label_detour() moves a label from a stray slot to a clean one.
    stray <- ambiguous & rim > 3
    clean <- !ambiguous & outside < 1e-8 & rowSums(routes$hit & rep(!own, each = m)) == 0 &
      rowSums(.box_overlaps(boxes, node_boxes[-i, , drop = FALSE]) > 0) == 0
    if (any(stray) && any(clean)) choices[[i]][c("stray", "clean")] <- list(stray, clean)
  }
  priority <- order(-sizes[, 1L] * sizes[, 2L], nodes$node, method = "radix")
  # The candidate each placed label uses.
  chosen <- integer(n)
  for (pass in seq_len(4L)) {
    for (i in priority) {
      z <- choices[[i]]
      if (is.null(z)) next
      other <- placed[seq_len(n) != i & !is.na(placed[, 1L]), , drop = FALSE]
      overlap <- .box_overlaps(z$boxes, other)
      costs <- .label_detour(z, z$base + 10000 * rowSums(overlap), rowSums(overlap),
        .label_association_cost(choices, i, placed, chosen = chosen))
      j <- which.min(costs)
      placed[i, ] <- z$boxes[j, ]; centers[i, ] <- z$centers[j, ]; chosen[i] <- j
      choices[[i]]$detour <- attr(costs, "detour")
    }
  }
  .refine_label_pairs(choices, placed, centers, priority, chosen = chosen)
}

# Costs of label i's slots with the other labels in place: `costs` plus
# `association` (.label_association_cost()). When the cheapest slot is a
# stray, every slot but a clean one that is clear of the other labels and of
# their stations costs 2500 more: the label takes such a slot if one costs
# less than 2500 more, and is otherwise left where it was, never moved onto
# another arrow or station instead. `force` keeps the detour that the
# placement passes made while .refine_label_pairs() moves pairs of labels.
.label_detour <- function(z, costs, overlap, association, force = FALSE) {
  costs <- costs + association
  detour <- !is.null(z$stray) && (force || z$stray[which.min(costs)])
  if (detour) costs <- costs + 2500 * !(z$clean & overlap == 0 & association == 0)
  structure(costs, detour = detour)
}

# Cost of each candidate for label i against the other placed labels: 2000 if
# some station's own label would be farther from it than this candidate, or
# another label is nearer station i than this candidate. It is not summed over
# stations: in a dense grid every slot is near several, and a label must never
# prefer covering a station (5000) to a slightly ambiguous slot.
.label_association_cost <- function(choices, i, placed, exclude = integer(), chosen = NULL) {
  z <- choices[[i]]
  others <- which(seq_len(nrow(placed)) != i & !is.na(placed[, 1L]))
  others <- setdiff(others, exclude)
  others <- others[!vapply(choices[others], function(choice) is.null(choice$rims), logical(1))]
  if (!length(others) || is.null(z$rims)) return(numeric(nrow(z$boxes)))
  selected <- if (!is.null(chosen) && all(chosen[others] > 0L)) chosen[others] else
    vapply(others, function(k) .label_choice_index(choices[[k]], placed[k, ]), integer(1))
  own <- vapply(seq_along(others), function(m) choices[[others[m]]]$rims[selected[m], others[m]], numeric(1))
  toward <- vapply(seq_along(others), function(m) choices[[others[m]]]$rims[selected[m], i], numeric(1))
  m <- nrow(z$rims)
  steal <- rowSums(.metro_nearer(z$rims[, others, drop = FALSE], matrix(own, m, length(others), byrow = TRUE)))
  lose <- rowSums(.metro_nearer(matrix(toward, m, length(others), byrow = TRUE), z$rims[, i]))
  2000 * pmin(1, steal + lose)
}

.label_choice_index <- function(choice, box) {
  which.min(rowSums(abs(choice$boxes - matrix(box, nrow(choice$boxes), 4L, byrow = TRUE))))
}

.refine_label_pairs <- function(choices, placed, centers, priority,
  max_pairs = 24L, max_candidates = 16L, chosen = NULL) {
  n <- nrow(placed)
  if (is.null(chosen)) chosen <- integer(n)
  for (id in which(!vapply(choices, is.null, logical(1)) & chosen == 0L))
    chosen[id] <- which.min(rowSums(abs(choices[[id]]$centers -
      matrix(centers[id, ], nrow(choices[[id]]$centers), 2L, byrow = TRUE))))
  # A long label can occupy another station's only nearby clear slot. Moving
  # either label alone then looks worse. Reconsider pairs jointly, using the
  # same physical collision/association costs and a bounded shortlist.
  active <- priority[!vapply(choices[priority], is.null, logical(1))]
  if (length(active) > 32L || max_pairs <= 0L) return(list(centers = centers, boxes = placed))
  attempts <- 0L
  for (pass in seq_len(2L)) {
    changed <- FALSE
    if (length(active) < 2L) break
    selected <- chosen[active]
    regret <- vapply(seq_along(active), function(j) {
      z <- choices[[active[j]]]
      z$base[selected[j]] - min(z$base)
    }, numeric(1))
    pairs <- utils::combn(seq_along(active), 2L)
    priority_pairs <- order(-(regret[pairs[1L, ]] + regret[pairs[2L, ]]),
      pairs[1L, ], pairs[2L, ], method = "radix")
    for (pair in priority_pairs) {
      if (attempts >= max_pairs) break
      i <- active[pairs[1L, pair]]; k <- active[pairs[2L, pair]]
      # Association with the labels that stay put, once per label of the pair.
      association <- list()
      association[[as.character(i)]] <- .label_association_cost(choices, i, placed, c(i, k), chosen)
      association[[as.character(k)]] <- .label_association_cost(choices, k, placed, c(i, k), chosen)
      cost <- function(id, other) {
        overlap <- .box_overlaps(choices[[id]]$boxes, other)
        .label_detour(choices[[id]], choices[[id]]$base + 10000 * rowSums(overlap),
          rowSums(overlap), association[[as.character(id)]], isTRUE(choices[[id]]$detour))
      }
      # Labels i and k reading as each other's, counted from both sides.
      cross <- function(ii, kk) if (is.null(choices[[i]]$rims) || is.null(choices[[k]]$rims))
        matrix(0, length(ii), length(kk)) else 2000 * pmin(
        outer(choices[[i]]$rims[ii, k], choices[[k]]$rims[kk, k], .metro_nearer) +
        outer(choices[[i]]$rims[ii, i], choices[[k]]$rims[kk, i], function(a, b) .metro_nearer(b, a)), 1)
      other <- placed[!seq_len(n) %in% c(i, k) & !is.na(placed[, 1L]), , drop = FALSE]
      current_i <- chosen[i]; current_k <- chosen[k]
      ci <- cost(i, other); ck <- cost(k, other)
      current <- ci[current_i] + ck[current_k] + cross(current_i, current_k)[1L, 1L] +
        10000 * .box_overlap(placed[i, ], placed[k, , drop = FALSE])
      # Most clean labels already use their individual minimum. Avoid scoring
      # every alternative for a pair whose lower bound cannot beat that result.
      if (current <= min(choices[[i]]$base) + min(choices[[k]]$base) + 1e-8) next
      # Independent minima already bound the best pair score from below.
      if (current <= min(ci) + min(ck) + 1e-8) next
      attempts <- attempts + 1L
      ii <- unique(c(current_i, head(order(ci, method = "radix"), max_candidates)))
      kk <- unique(c(current_k, head(order(ck, method = "radix"), max_candidates)))
      score <- outer(ci[ii], ck[kk], "+") + cross(ii, kk)
      score <- score + 10000 * .box_overlaps(choices[[i]]$boxes[ii, , drop = FALSE],
        choices[[k]]$boxes[kk, , drop = FALSE])
      best <- which.min(score)
      if (score[best] >= current - 1e-8) next
      pick <- arrayInd(best, dim(score))
      j <- ii[pick[1L]]; l <- kk[pick[2L]]
      placed[i, ] <- choices[[i]]$boxes[j, ]; centers[i, ] <- choices[[i]]$centers[j, ]
      placed[k, ] <- choices[[k]]$boxes[l, ]; centers[k, ] <- choices[[k]]$centers[l, ]
      chosen[i] <- j; chosen[k] <- l
      changed <- TRUE
    }
    if (!changed || attempts >= max_pairs) break
  }
  list(centers = centers, boxes = placed)
}

# Score actual padded label footprints; a fallback reflow must reduce these
# conflicts. Columns: stations covered, routes touched, labels overlapped,
# outside the bounds, and whether the label reads as another station's: it
# is nearer another station than its own, or nearer it than that station's
# own label, as .metro_label_ambiguity() reports (`outline` is the part of
# `radius` that the report leaves out).
.label_placement_conflicts <- function(nodes, edges, placement, radius, bounds,
  line_width, arrowheads, outline = 0) {
  boxes <- placement$boxes
  active <- which(!is.na(boxes[, 1L]))
  counts <- matrix(0L, nrow(nodes), 5L)
  for (i in active) {
    box <- boxes[i, ]
    counts[i, 1L] <- length(.metro_label_station_hits(nodes, box, radius))
    counts[i, 2L] <- length(.metro_label_edge_hits(edges, box, line_width, arrowheads))
    counts[i, 3L] <- sum(.box_overlap(box, boxes[setdiff(active, i), , drop = FALSE]) > 0)
    counts[i, 4L] <- as.integer(any(box[c(1L, 3L)] < bounds[c(1L, 3L)] - 1e-8 |
      box[c(2L, 4L)] > bounds[c(2L, 4L)] + 1e-8))
  }
  ambiguous <- .metro_label_ambiguity(nodes, boxes, rep_len(radius, nrow(nodes)) - outline)
  counts[nodes$node %in% ambiguous$node, 5L] <- 1L
  counts
}

# Weights of those conflicts, in the order of the placement costs: labels
# that overlap or leave the figure cannot be read at all, a label over a
# station hides it, and a label read as another station's misleads. That
# counts as much as four routes crossed, which the label casing keeps
# legible: a reflow may add up to three crossings to take one label back to
# its own station.
.label_conflict_weights <- c(station = 5, route = 1, overlap = 10, bounds = 10, association = 4)

# Rewrap the labels that have conflicts and place every label again. Labels
# with camelCase breaks are tried without them first (`plain`, the labels
# wrapped at spaces only, NA where they are the same): one long line can fit
# between the rows of a compact layout where two lines reach another
# station. All of them are tried at once, then, while a label still reads as
# another station's, one at a time. Then narrower blocks are tried. A trial
# is kept only when it lowers the weighted conflicts. Large sets keep their
# ordinary diagnostics: the reflow is skipped when more than six labels
# cover stations, cross routes, overlap or leave the figure.
.reflow_colliding_labels <- function(nodes, edges, sizes, texts, gp, radius,
  bounds, line_width, arrowheads, placement, plain = NULL, outline = 0) {
  measure <- function(g) c(grid::convertWidth(grid::grobWidth(g), "mm", valueOnly = TRUE),
    grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE))
  conflicts_of <- function(nodes, placement) .label_placement_conflicts(nodes, edges,
    placement, radius, bounds, line_width, arrowheads, outline)
  conflicts <- conflicts_of(nodes, placement)
  current <- sum(colSums(conflicts) * .label_conflict_weights)
  bad <- which(rowSums(conflicts) > 0L)
  attempt <- function(labels) {
    trial_nodes <- nodes; trial_sizes <- sizes; trial_texts <- texts
    for (i in which(!is.na(labels))) {
      trial_nodes$label[i] <- labels[i]
      trial_texts[[i]] <- grid::textGrob(labels[i], gp = gp)
      trial_sizes[i, ] <- measure(trial_texts[[i]])
    }
    if (identical(trial_nodes$label, nodes$label)) return(FALSE)
    trial <- .place_labels(trial_nodes, edges, trial_sizes, radius, bounds, line_width,
      arrowheads, outline)
    quality <- conflicts_of(trial_nodes, trial)
    score <- sum(colSums(quality) * .label_conflict_weights)
    if (score >= current) return(FALSE)
    nodes <<- trial_nodes; sizes <<- trial_sizes; texts <<- trial_texts
    placement <<- trial; conflicts <<- quality; current <<- score
    TRUE
  }
  if (!length(bad) || sum(rowSums(conflicts[, 1:4, drop = FALSE]) > 0L) > 6L)
    return(list(nodes = nodes, sizes = sizes, texts = texts, placement = placement))
  labels <- rep(NA_character_, nrow(nodes))
  if (!is.null(plain) && any(!is.na(plain[bad]))) {
    camel <- nodes$label
    labels[bad] <- plain[bad]
    attempt(labels)
    single <- bad[!is.na(plain[bad])]
    for (i in single[order(-conflicts[single, 5L], method = "radix")]) {
      if (!any(conflicts[, 5L] > 0L)) break
      labels <- rep(NA_character_, nrow(nodes))
      labels[i] <- if (identical(nodes$label[i], plain[i])) camel[i] else plain[i]
      attempt(labels)
    }
    labels <- rep(NA_character_, nrow(nodes))
  }
  original <- nodes$label
  original_sizes <- sizes
  for (fraction in c(0.75, 0.5)) {
    if (!current) break
    # Narrower blocks rewrap at spaces only; a word is never cut to fit.
    for (i in bad) labels[i] <- .metro_wrap_text(original[i], original_sizes[i, 1L] * fraction,
      gp, split_words = FALSE)
    attempt(labels)
  }
  list(nodes = nodes, sizes = sizes, texts = texts, placement = placement)
}

.points_in_triangle <- function(xy, triangle) {
  cross <- vapply(seq_len(3L), function(i) {
    a <- triangle[i, ]; b <- triangle[if (i == 3L) 1L else i + 1L, ]
    (b[1L] - a[1L]) * (xy[, 2L] - a[2L]) - (b[2L] - a[2L]) * (xy[, 1L] - a[1L])
  }, numeric(nrow(xy)))
  cross <- matrix(cross, ncol = 3L)
  rowSums(cross >= -1e-10) == 3L | rowSums(cross <= 1e-10) == 3L
}

.metro_arrow_triangle <- function(xy, arrow_length, angle = 25) {
  if (arrow_length <= 0 || nrow(xy) < 2L) return(NULL)
  delta <- xy[nrow(xy), ] - xy[nrow(xy) - 1L, ]
  len <- sqrt(sum(delta^2))
  if (len <= 0) return(NULL)
  direction <- delta / len
  base <- xy[nrow(xy), ] - direction * arrow_length * cos(angle * pi / 180)
  wing <- c(-direction[2L], direction[1L]) * arrow_length * sin(angle * pi / 180)
  rbind(xy[nrow(xy), ], base + wing, base - wing)
}

# Distance between two triangles (3 x 2 matrices); 0 when they intersect.
.metro_triangle_gap <- function(a, b) {
  if (any(.points_in_triangle(a, b)) || any(.points_in_triangle(b, a))) return(0)
  side <- function(p, q, r) (q[1L] - p[1L]) * (r[2L] - p[2L]) - (q[2L] - p[2L]) * (r[1L] - p[1L])
  gap <- Inf
  for (i in 1:3) for (j in 1:3) {
    p <- a[i, ]; q <- a[i %% 3L + 1L, ]; r <- b[j, ]; s <- b[j %% 3L + 1L, ]
    if (side(p, q, r) * side(p, q, s) < 0 && side(r, s, p) * side(r, s, q) < 0) return(0)
    gap <- min(gap, .point_segment_distance(p[1L], p[2L], r[1L], r[2L], s[1L], s[2L]),
      .point_segment_distance(r[1L], r[2L], p[1L], p[2L], q[1L], q[2L]))
  }
  gap
}

# Pairs of arrowheads drawn at one station whose filled shapes, outlines
# included, overlap. `heads` holds each drawn head's destination, source,
# triangle and route width.
.metro_arrowhead_overlaps <- function(heads) {
  pairs <- data.frame(node = character(), from = character(), other_node = character())
  stations <- vapply(heads, function(h) h$to, character(1))
  for (station in unique(stations)) {
    at <- heads[stations == station]
    if (length(at) < 2L) next
    for (i in seq_len(length(at) - 1L)) for (j in seq.int(i + 1L, length(at))) {
      gap <- .metro_triangle_gap(at[[i]]$triangle, at[[j]]$triangle)
      if (gap < (at[[i]]$width + at[[j]]$width) / 2 - 1e-8) pairs[nrow(pairs) + 1L, ] <-
        list(station, at[[i]]$from, at[[j]]$from)
    }
  }
  pairs
}

# Test the actual trimmed, fanned route, not the centre-to-centre layout.
# Include the visible route stroke and closed arrowhead as well as node rims.
# Round caps and joins add a circular clearance around the filled station.
# Every station is tested against every segment at once: station by station,
# the distances are the same element-wise computations.
.route_station_collisions <- function(xy, nodes, radius, exclude, line_width, arrow_length = 0,
  triangle = .metro_arrow_triangle(xy, arrow_length), outline = 0) {
  ids <- setdiff(seq_len(nrow(nodes)), exclude)
  if (!length(ids)) return(ids)
  x <- head(xy[, 1L], -1L); y <- head(xy[, 2L], -1L)
  xend <- tail(xy[, 1L], -1L); yend <- tail(xy[, 2L], -1L)
  if (!is.null(triangle)) {
    closed <- rbind(triangle, triangle[1L, ])
    x <- c(x, head(closed[, 1L], -1L)); y <- c(y, head(closed[, 2L], -1L))
    xend <- c(xend, tail(closed[, 1L], -1L)); yend <- c(yend, tail(closed[, 2L], -1L))
  }
  m <- length(x)
  radius <- rep_len(radius, nrow(nodes))[ids]
  clearance <- rep_len(outline, nrow(nodes))[ids] + line_width / 2
  px <- nodes$x[ids]; py <- nodes$y[ids]
  square <- nodes$shape[ids] == 22L
  hit <- logical(length(ids))
  # any() over each station's block of `columns` results, missing values
  # included.
  rows_any <- function(test, columns = m) {
    test <- matrix(test, ncol = columns, byrow = TRUE)
    result <- rowSums(test, na.rm = TRUE) > 0
    if (anyNA(test)) result[!result & rowSums(is.na(test)) > 0] <- NA
    result
  }
  circle <- which(!square)
  if (length(circle) && m) {
    rr <- radius[circle] + clearance[circle]
    distance <- .point_segment_distance(rep(px[circle], each = m), rep(py[circle], each = m),
      x, y, xend, yend)
    hit[circle] <- rows_any(distance <= rep(rr, each = m))
  }
  if (length(circle) && !is.null(triangle)) hit[circle] <- hit[circle] |
    .points_in_triangle(cbind(px[circle], py[circle]), triangle)
  box <- which(square)
  if (length(box)) {
    rr <- radius[box]
    sides <- cbind(px[box] - rr, px[box] + rr, py[box] - rr, py[box] + rr)
    if (m) {
      q <- length(box)
      distance <- .segments_box_distance(rep(x, q), rep(y, q), rep(xend, q), rep(yend, q),
        sides[rep(seq_len(q), each = m), , drop = FALSE])
      hit[box] <- rows_any(distance <= rep(clearance[box], each = m))
    }
    # Corners in the order of expand.grid(x sides, y sides).
    if (!is.null(triangle)) {
      corners <- cbind(c(t(sides[, c(1L, 2L, 1L, 2L), drop = FALSE])),
        c(t(sides[, c(3L, 3L, 4L, 4L), drop = FALSE])))
      hit[box] <- hit[box] | rows_any(.points_in_triangle(corners, triangle), 4L)
    }
  }
  ids[hit]
}

.trim_polyline <- function(xy, start, end) {
  trim_start <- function(z, distance) {
    while (nrow(z) > 1L) {
      delta <- z[2L, ] - z[1L, ]; len <- sqrt(sum(delta^2))
      if (len > distance) {
        z[1L, ] <- z[1L, ] + delta * distance / len
        return(z)
      }
      distance <- distance - len
      z <- z[-1L, , drop = FALSE]
    }
    z
  }
  xy <- trim_start(xy, start)
  xy <- trim_start(xy[nrow(xy):1L, , drop = FALSE], end)
  xy[nrow(xy):1L, , drop = FALSE]
}

# Reserve a straight radial approach for the whole arrowhead. Replacing only
# the tip can leave a short final shaft pointing away from the head; shortening
# that tail first keeps its bends out of the head and its wings outside the rim.
.metro_arrow_route <- function(xy, arrow_length, target) {
  tip <- xy[nrow(xy), ]
  delta <- target - tip
  direction <- delta / sqrt(sum(delta^2))
  stem <- .trim_polyline(xy, 0, arrow_length)
  stem[nrow(stem), ] <- tip - direction * arrow_length
  rbind(stem, tip)
}

# Blend a displaced attachment over a few physical millimetres instead of
# leaving a one-step hook. The short radial end remains straight for its head;
# the cubic joins the unmodified shaft tangentially. This affects rendering
# only, and does not turn shared centreline tracks into extra causal stations.
#
# Arrows sharing a final track used to start that blend on the shared
# centreline, so a displaced arrow jogged sideways and turned back towards the
# station: an S-shaped hook. Prefer the source-side join instead: walk back to
# where the route has already moved to the port's side and join there, leaving
# the final `straight` millimetres radial for the head. Shared final tracks
# can be long, so arrivals may join up to `max_join` mm back: 20 mm, or more
# in a large figure (see .metro_grob_content). The fixed blend below remains
# the fallback when no such join exists.
.metro_fan_end <- function(xy, tip, target, straight = 1,
  bearing = xy[nrow(xy) - 1L, ] - xy[nrow(xy), ], max_join = 20) {
  old_tip <- xy[nrow(xy), ]
  if (sqrt(sum((tip - old_tip)^2)) < 1e-8) return(xy)
  fallback <- xy; fallback[nrow(fallback), ] <- tip
  inward <- target - tip
  inward <- inward / sqrt(sum(inward^2))
  head_base <- tip - inward * straight
  if (nrow(xy) >= 2L) {
    # A port within 3 degrees of the arrow's own final bearing (outwards from
    # the station) tilts the final straight run when that run is the route's
    # last step and the head base stays within 3 degrees of it: a join or
    # blend over the last few millimetres would bend a straight arrival twice
    # for an invisible shift. The head base, not the tip, bounds the drawn
    # shaft. A run that is not the last step (one trimmed away inside the
    # station clearance) would instead turn into the head at the full angle
    # between them, so it is joined or blended.
    start <- .metro_final_run(xy)
    run <- xy[nrow(xy), ] - xy[start, ]
    angle <- function(a, b) abs(atan2(a[1L] * b[2L] - a[2L] * b[1L], sum(a * b)))
    if (angle(-run, bearing) <= 1e-6 && angle(run, head_base - xy[start, ]) <= 0.05 &&
        angle(bearing, tip - target) <= 0.05)
      return(rbind(xy[seq_len(start), , drop = FALSE], tip))
    reversed <- xy[nrow(xy):1L, , drop = FALSE]
    joined <- .metro_fan_join(reversed, head_base, target, reversed[2L, ] - reversed[1L, ],
      max_join = max_join, convex = TRUE)
    if (!is.null(joined)) return(rbind(joined[nrow(joined):1L, , drop = FALSE], tip))
  }
  length <- sum(sqrt(rowSums((xy[-1L, , drop = FALSE] -
    xy[-nrow(xy), , drop = FALSE])^2)))
  blend <- max(4, straight * 3)
  if (length <= blend + straight + 1) return(fallback)
  stem <- .trim_polyline(xy, 0, blend)
  if (nrow(stem) < 2L) return(fallback)
  radial <- target - tip
  radial <- radial / sqrt(sum(radial^2))
  tangent <- stem[nrow(stem), ] - stem[nrow(stem) - 1L, ]
  tangent <- tangent / sqrt(sum(tangent^2))
  a <- stem[nrow(stem), ]; b <- tip - radial * straight
  delta <- b - a
  # A very short or reversed approach must retain the original safe path.
  if (sum(delta * tangent) <= 0 || sum(delta * radial) <= 0) return(fallback)
  handle <- min(2, sqrt(sum(delta^2)) / 3)
  control1 <- a + tangent * handle; control2 <- b - radial * handle
  t <- seq(0.125, 1, by = 0.125)
  curve <- outer((1 - t)^3, a) + outer(3 * (1 - t)^2 * t, control1) +
    outer(3 * (1 - t) * t^2, control2) + outer(t^3, b)
  rbind(stem, curve, tip)
}

# The shape of a drawn arrival along its last `window` millimetres: `back`,
# the smaller of its total left and right turning in degrees (above 8 it
# reads as an S-shaped hook, the measure of inst/benchmarks/arrow_hooks.R),
# and `sharp`, its sharpest single turn (above 90 it folds back on itself).
.metro_arrival_shape <- function(xy, clear = TRUE, window = 10) {
  step <- sqrt(rowSums(diff(xy)^2))
  first <- max(1L, which(rev(cumsum(rev(step))) <= window + 1e-9)[1L] - 1L)
  if (is.na(first)) first <- nrow(xy) - 1L
  v <- diff(xy[first:nrow(xy), , drop = FALSE])
  v <- v[rowSums(v^2) > 1e-12, , drop = FALSE]
  a <- atan2(v[, 2L], v[, 1L])
  turn <- if (nrow(v) > 1L) atan2(sin(diff(a)), cos(diff(a))) * 180 / pi else 0
  c(back = min(sum(turn[turn > 0]), -sum(turn[turn < 0])), sharp = max(abs(turn)),
    clear = clear)
}

# Whether arrival shape `a` is no worse than `b` (.metro_arrival_shape): it
# turns back no more than 8 degrees or than `b` (unless `hooks` is FALSE),
# turns no more sharply than 90 degrees or than `b`, and keeps its clearance
# unless `b` does not.
.metro_no_worse <- function(a, b, hooks = TRUE)
  (!hooks || a[["back"]] <= max(8, b[["back"]])) && a[["sharp"]] <= max(90, b[["sharp"]]) &&
    (a[["clear"]] || !b[["clear"]])

# Index of the point where the final straight run of `xy` starts; routes
# carry a point at every grid step.
.metro_final_run <- function(xy) {
  k <- nrow(xy)
  last <- xy[k, ] - xy[k - 1L, ]
  while (k > 2L) {
    step <- xy[k - 1L, ] - xy[k - 2L, ]
    if (sum(last * step) <= 0 || abs(last[1L] * step[2L] - last[2L] * step[1L]) >
      1e-9 * sqrt(sum(last^2) * sum(step^2))) break
    k <- k - 1L
  }
  k - 1L
}

# Length of the straight run that ends a route at its destination.
.metro_final_run_length <- function(route) {
  xy <- rbind(c(route$x[1L], route$y[1L]), cbind(route$xend, route$yend))
  sqrt(sum((xy[nrow(xy), ] - xy[.metro_final_run(xy), ])^2))
}

# A displaced source port should join the branch after it has diverged, rather
# than return to a shared track and leave it again. Bound the join in physical
# millimetres and require monotone Bezier control points in both local axes.
# The complete rendered stroke still goes through the station/bounds checks.
.metro_fan_start <- function(xy, port, source, bearing = xy[2L, ] - xy[1L, ]) {
  if (sqrt(sum((port - xy[1L, ])^2)) < 1e-8) return(xy)
  fallback <- xy; fallback[1L, ] <- port
  joined <- .metro_fan_join(xy, port, source, bearing)
  if (is.null(joined)) fallback else joined
}

# Join a displaced attachment point `port` (beside the station centre
# `source`) to the route where it has already moved to the port's side, with a
# cubic whose control points are monotone in both local axes. `xy` starts at
# the station; the result starts at `port`. NULL when no such join exists.
#
# With `convex = TRUE` the join also requires the chord from `port` to the
# join to lie between the port's radial direction and the route's direction
# there, so the cubic turns one way only and cannot form an S.
.metro_fan_join <- function(xy, port, source, bearing, max_join = 12, convex = FALSE) {
  if (nrow(xy) < 2L || sum(bearing^2) <= 0) return(NULL)
  direction <- bearing / sqrt(sum(bearing^2))
  normal <- c(-direction[2L], direction[1L])
  v <- drop(sweep(xy, 2L, source, "-") %*% normal)
  offset <- sum((port - source) * normal)
  if (abs(offset) < 1e-8) return(NULL)
  if (convex) {
    found <- .metro_convex_join(xy, port, source, normal, offset, max_join)
    if (is.null(found)) return(NULL)
    join <- found$join; j <- found$j
  } else {
    candidates <- which(v * sign(offset) > abs(offset) + 0.5)
    if (!length(candidates) || candidates[1L] < 2L) return(NULL)
    j <- candidates[1L]
    fraction <- (sign(offset) * (abs(offset) + 0.5) - v[j - 1L]) / (v[j] - v[j - 1L])
    join <- xy[j - 1L, ] + fraction * (xy[j, ] - xy[j - 1L, ])
    prefix <- rbind(xy[seq_len(j - 1L), , drop = FALSE], join)
    if (sum(sqrt(rowSums(diff(prefix)^2))) > max_join) return(NULL)
  }
  tangent <- xy[j, ] - xy[j - 1L, ]
  tangent <- tangent / sqrt(sum(tangent^2))
  radial <- port - source; radial <- radial / sqrt(sum(radial^2))
  delta <- join - port
  if (sum(delta * radial) <= 0 || sum(delta * tangent) <= 0) return(NULL)
  du <- sum(delta * direction); dv <- sum(delta * normal) * sign(offset)
  handle <- min(2, sqrt(sum(delta^2)) / 3,
    du / sum((radial + tangent) * direction),
    dv / (sum((radial + tangent) * normal) * sign(offset)))
  if (!is.finite(handle) || handle <= 1e-6) return(NULL)
  controls <- rbind(port, port + radial * handle, join - tangent * handle, join)
  if (any(diff(drop(controls %*% direction)) < -1e-8) ||
      any(diff(drop(controls %*% normal)) * sign(offset) < -1e-8)) return(NULL)
  t <- seq(0, 1, by = 0.125)
  curve <- outer((1 - t)^3, controls[1L, ]) + outer(3 * (1 - t)^2 * t, controls[2L, ]) +
    outer(3 * (1 - t) * t^2, controls[3L, ]) + outer(t^3, controls[4L, ])
  rbind(curve, xy[seq.int(j, nrow(xy)), , drop = FALSE])
}

# First point within `max_join` mm along `xy` that lies at least 0.5 mm beyond
# the port on its side and whose chord from `port` lies between the port's
# radial direction and the route's local direction. Sampled every 0.25 mm.
.metro_convex_join <- function(xy, port, source, normal, offset, max_join) {
  seg <- diff(xy); len <- sqrt(rowSums(seg^2))
  cum <- c(0, cumsum(len))
  radial <- port - source; radial <- radial / sqrt(sum(radial^2))
  limit <- min(max_join, cum[length(cum)])
  if (limit < 0.25) return(NULL)
  for (s in seq(0.25, limit, by = 0.25)) {
    k <- min(findInterval(s, cum, rightmost.closed = TRUE), nrow(seg))
    if (k < 1L || len[k] <= 0) next
    p <- xy[k, ] + (s - cum[k]) / len[k] * seg[k, ]
    if (sum((p - source) * normal) * sign(offset) < abs(offset) + 0.5) next
    chord <- p - port; tangent <- seg[k, ] / len[k]
    turn_in <- radial[1L] * chord[2L] - radial[2L] * chord[1L]
    turn_out <- chord[1L] * tangent[2L] - chord[2L] * tangent[1L]
    if (turn_in * turn_out < -1e-9) next
    return(list(join = p, j = k + 1L))
  }
  NULL
}

# Look outward from an endpoint to the first departure from its bearing. Tied
# ports follow the side of that departure, keeping the fan's local route order.
# Names are only a final tie-break for geometrically indistinguishable arrows.
.metro_port_turn <- function(route, end = FALSE) {
  xy <- rbind(c(route$x[1L], route$y[1L]), cbind(route$xend, route$yend))
  if (end) xy <- xy[nrow(xy):1L, , drop = FALSE]
  v <- sweep(xy, 2L, xy[1L, ], "-")
  direction <- v[2L, ]
  cross <- direction[1L] * v[, 2L] - direction[2L] * v[, 1L]
  away <- which(abs(cross) > 1e-8 * sqrt(sum(direction^2)) * pmax(1, sqrt(rowSums(v^2))))
  if (!length(away)) return(0)
  j <- away[1L]
  atan2(cross[j], sum(direction * v[j, ]))
}

# Separate incident arrow tips around a station, including degree > 3 where
# a strict three-direction grid necessarily has shared terminal segments.
# Only the short device-space attachment is adjusted; stored routes stay
# monotone/octilinear and graph topology is unchanged.
# Spread sorted attachment angles `rel` so neighbours are at least
# `separation` apart. Only clusters of arrows that are too close move; each
# cluster is centred on its own bearings, so an arrow with room keeps its
# bearing and tied arrows straddle their shared track. Within a cluster, one
# complete straight arrow keeps its bearing: centring would otherwise bend the
# through-route as well as the bypass. Clusters that grow into each other merge.
.metro_spread_ports <- function(rel, separation, straight) {
  n <- length(rel)
  fit <- rel
  if (n < 2L) return(fit)
  breaks <- c(0L, which(diff(rel) >= separation - 1e-9), n)
  repeat {
    for (k in seq_len(length(breaks) - 1L)) {
      members <- seq.int(breaks[k] + 1L, breaks[k + 1L])
      if (length(members) < 2L) { fit[members] <- rel[members]; next }
      offset <- seq_along(members) * separation
      spread <- stats::isoreg(rel[members] - offset)$yf + offset
      spread <- spread - (mean(range(spread)) - mean(range(rel[members])))
      anchors <- which(straight[members])
      if (length(anchors)) {
        anchor <- anchors[which.min(abs(spread[anchors] - rel[members][anchors]))]
        spread <- spread + rel[members][anchor] - spread[anchor]
      }
      fit[members] <- spread
    }
    gaps <- diff(fit)
    crowded <- which(gaps < separation - 1e-9)
    crowded <- crowded[crowded %in% breaks[-c(1L, length(breaks))]]
    if (!length(crowded)) break
    breaks <- setdiff(breaks, crowded)
  }
  fit
}

# Weighted least-squares fit of `y` by a non-decreasing sequence (pool
# adjacent violators).
.metro_isotonic <- function(y, weight) {
  value <- numeric(); total <- numeric(); size <- integer()
  for (i in seq_along(y)) {
    value <- c(value, y[i]); total <- c(total, weight[i]); size <- c(size, 1L)
    k <- length(value)
    while (k > 1L && value[k - 1L] > value[k]) {
      pooled <- total[k - 1L] + total[k]
      value[k - 1L] <- (value[k - 1L] * total[k - 1L] + value[k] * total[k]) / pooled
      total[k - 1L] <- pooled; size[k - 1L] <- size[k - 1L] + size[k]
      value <- value[-k]; total <- total[-k]; size <- size[-k]; k <- k - 1L
    }
  }
  rep(value, size)
}

# Largest factor, at most 1, by which the gaps `gap` between consecutive
# sorted ports can be scaled while every port keeps its order within its
# bounds; -Inf when a port has no room at all.
.metro_bounded_scale <- function(lower, upper, gap) {
  if (any(lower > upper)) return(-Inf)
  offset <- c(0, cumsum(gap))
  scale <- 1
  for (j in seq_along(lower)[-1L]) {
    i <- seq_len(j - 1L)
    scale <- min(scale, (upper[j] - lower[i]) / (offset[j] - offset[i]))
  }
  scale
}

# Ports nearest their bearings `rel` (weighted least squares) whose
# consecutive gaps are at least `gap` and that stay within bounds admitting
# those gaps. The two clamping passes keep both the bounds and the gaps.
.metro_bounded_ports <- function(rel, gap, weight, lower, upper) {
  n <- length(rel)
  offset <- c(0, cumsum(gap))
  fit <- .metro_isotonic(rel - offset, weight) + offset
  for (i in seq_len(n)) fit[i] <- max(fit[i], lower[i], if (i > 1L) fit[i - 1L] + gap[i - 1L])
  for (i in rev(seq_len(n))) fit[i] <- min(fit[i], upper[i], if (i < n) fit[i + 1L] - gap[i])
  fit
}

# Spread arrival ports, sorted as in .station_ports. Arrowheads point at the
# station centre, so neighbouring tips need one head width of rim arc
# (`preferred`, one value per pair of neighbours) to read as separate heads;
# below `minimum` their outlines touch. Moving a port off its arrow's
# bearing bends the arrival. Every arrow may move within `room` of its
# bearing, where .metro_fan_end tilts its straight run, and further below or
# above it as far as `reach` (two columns) allows, where its join or blend
# draws it no worse than before (see .station_ports). So take the widest
# separation up to `preferred` that keeps every arrow within those clean
# bounds and within `lower` and `upper` (one octilinear direction, or one
# `preferred` separation, either way and clear of departing routes), holding
# `through` arrows, which keep their bearing up to the station, nearest it.
# Heads whose outlines touch read better than S-shaped hooks, so that
# separation stands down to `floor`: filled heads that just meet, or the
# narrow fan's separation where that is tighter. Below it the heads would
# merge, so bend arrows within `lower` and `upper` as far as `minimum` needs
# (attribute `bent`).
.metro_spread_arrivals <- function(rel, through, room, reach, preferred, minimum, floor,
  lower, upper) {
  clean_lower <- pmax(lower, rel - pmax(room, reach[, 1L]))
  clean_upper <- pmin(upper, rel + pmax(room, reach[, 2L]))
  weight <- ifelse(through, 1, 1e-6)
  scale <- .metro_bounded_scale(clean_lower, clean_upper, preferred)
  if (all(scale * preferred >= floor - 1e-12))
    return(.metro_bounded_ports(rel, scale * preferred, weight, clean_lower, clean_upper))
  gap <- pmin(minimum, max(0, .metro_bounded_scale(lower, upper, preferred)) * preferred)
  structure(.metro_bounded_ports(rel, gap, weight, lower, upper), bent = TRUE)
}

# Attachment points of routes at their stations, by edge ID: `start` and
# `end`, and `narrow`, the arrival ends spread only as far as distinct ports
# need. Arrivals drawn with heads (`arrow_length` per station) are spread by
# the size of those heads. `shape(edge_id, point)`, when given, gives the
# .metro_arrival_shape() of an arrival drawn to `point`. An arrival reaches a
# spread port cleanly when it is drawn there no worse (.metro_no_worse) than
# on its bearing, or, for an arrival that left a shared track, than at its
# narrow port; one that cannot reach its spread port has its `reach`
# narrowed until it can, and the station is spread again.
.station_ports <- function(nodes, edges, radius, clearance, arrow_length = 0, arrow_angle = 25,
  shape = NULL) {
  empty <- matrix(numeric(), 0, 2)
  if (!nrow(edges)) return(list(start = empty, end = empty, narrow = empty))
  arrow_length <- rep_len(arrow_length, nrow(nodes))
  half <- arrow_angle * pi / 180
  first <- edges[!duplicated(edges$edge_id), , drop = FALSE]
  last <- edges[!duplicated(edges$edge_id, fromLast = TRUE), , drop = FALSE]
  straight <- vapply(split(edges, edges$edge_id), function(e) {
    bearing <- atan2(e$yend - e$y, e$xend - e$x)
    all(abs(atan2(sin(bearing - bearing[1L]), cos(bearing - bearing[1L]))) < 1e-8)
  }, logical(1))
  point <- function(i, a) {
    rr <- radius[i] + clearance
    square <- nodes$shape[i] == 22L
    rr[square] <- rr[square] / pmax(abs(cos(a[square])), abs(sin(a[square])))
    matrix(c(nodes$x[i] + rr * cos(a), nodes$y[i] + rr * sin(a)), ncol = 2L)
  }
  result <- list()
  for (side in c("start", "end")) {
    z <- if (side == "start") first else last
    ids <- match(if (side == "start") z$from else z$to, nodes$node)
    dx <- if (side == "start") z$xend - z$x else z$x - z$xend
    dy <- if (side == "start") z$yend - z$y else z$y - z$yend
    angle <- narrow <- atan2(dy, dx)
    for (id in unique(ids)) {
      ix <- which(ids == id)
      if (length(ix) < 2L) next
      center <- atan2(mean(sin(angle[ix])), mean(cos(angle[ix])))
      rel <- atan2(sin(angle[ix] - center), cos(angle[ix] - center))
      turn <- vapply(ix, function(k) .metro_port_turn(
        edges[edges$edge_id == z$edge_id[k], , drop = FALSE], end = side == "end"), numeric(1))
      # Device scaling and mixed step lengths can give equal bearings slightly
      # different floating-point angles. Let geometry order those ties too.
      sorted <- order(rel, method = "radix"); group <- integer(length(ix))
      group[sorted] <- cumsum(c(TRUE, diff(rel[sorted]) > 1e-8))
      order <- order(group, turn, z$from[ix], z$to[ix], method = "radix")
      ix <- ix[order]; rel <- rel[order]; turn <- turn[order]; group <- group[order]
      # Keep a group sharing one approach within a narrow fan: spreading all
      # the way around the station could send an attachment through its centre.
      separation <- min(0.48, 1 / (length(ix) - 1L))
      is_straight <- straight[as.character(z$edge_id[ix])]
      # Departures, and arrivals drawn without heads, need distinct ports only.
      narrow[ix] <- angle[ix] <- center + .metro_spread_ports(rel, separation, is_straight)
      if (side == "start" || arrow_length[id] <= 0) next
      # Heads on the port circle `rim`: neighbouring wings, `wing` either side
      # of a head's base at radius `base`, are closest. Their outlines (the
      # mean width of the two routes) keep 0.1 mm apart at `minimum`, which a
      # head width of arc does not ensure on a large station.
      rim <- radius[id] + clearance
      wing <- arrow_length[id] * sin(half)
      base <- rim + arrow_length[id] * cos(half)
      width <- if (is.null(z$width)) rep(0, length(ix)) else z$width[ix]
      outline <- (width[-length(ix)] + width[-1L]) / 2
      filled <- 2 * atan(wing / base)
      minimum <- filled + 2 * asin(pmin(1, (outline + 0.1) / (2 * sqrt(base^2 + wing^2))))
      preferred <- pmax(separation, 2 * wing / rim, minimum)
      # Neighbours on different octilinear bearings are at least 45 degrees
      # apart. Where their outlines clear at that gap, moving them for a
      # slightly wider arc between tips would only tilt straight runs, so they
      # keep it (on a default station one head width needs 47.7 degrees).
      # Only where no arrival has to move: at a station whose tied arrivals
      # must spread, the lower target would pack the spread unevenly.
      apart <- diff(rel)
      settled <- apart >= pi / 4 - 1e-8 & apart >= minimum
      if (all(settled | apart >= preferred - 1e-12))
        preferred[settled] <- pmin(preferred[settled], apart[settled])
      floor <- rep(min(filled, separation), length(outline))
      # Departing routes bound the arrivals on either side, as does the far
      # side of the station; one already sharing an arrival's bearing cannot.
      own <- first$from == nodes$node[id]
      departures <- atan2(result$start[own, 2L] - nodes$y[id], result$start[own, 1L] - nodes$x[id])
      obstacles <- c(-pi, pi, atan2(sin(departures - center), cos(departures - center)))
      gap <- max(minimum) / 2 + max(width) / (2 * rim)
      limit <- max(pi / 4, preferred)
      lower <- vapply(rel, function(r)
        max(r - limit, obstacles[obstacles <= r - gap] + gap), numeric(1))
      upper <- vapply(rel, function(r)
        min(r + limit, obstacles[obstacles >= r + gap] - gap), numeric(1))
      # An arrow may move 2 degrees, less when the stem between its last bend
      # and its head is short: .metro_fan_end tilts that stem, and beyond 3
      # degrees the head would turn back into a hook.
      run <- vapply(ix, function(k) .metro_final_run_length(
        edges[edges$edge_id == z$edge_id[k], , drop = FALSE]), numeric(1))
      stem <- pmax(0, run - rim - arrow_length[id])
      room <- pmin(2 * pi / 180, asin(pmin(1, stem * tan(0.05) / (rim + arrow_length[id]))))
      # Reaches are tried, not assumed: an arrival that cannot reach its port
      # cleanly has its reach on that side bisected down to what it can reach
      # (to within 1/64 of the move), and the station is spread again; each
      # pass narrows a reach below the port it missed. A through arrow (alone
      # on its bearing, or the tied arrow with the smallest turn) is compared
      # with its own unmoved route, any other with its narrow port, since it
      # cannot share the through arrow's.
      # Without a shape to test, assume an arrival reaches beyond its room
      # only on the side where it left its bearing. A complete straight route
      # keeps to its room either way: even a blend that does not hook would
      # put the only bend of the route next to its head.
      through <- as.logical(stats::ave(abs(turn), group, FUN = function(a) a <= min(a) + 1e-8))
      reach <- if (is.null(shape)) cbind(ifelse(turn < 0, Inf, 0), ifelse(turn > 0, Inf, 0)) else
        matrix(Inf, length(ix), 2L)
      reach[is_straight, ] <- 0
      before <- vector("list", length(ix))
      clean <- function(k, offset) {
        if (is.null(before[[k]])) before[[k]] <<- shape(z$edge_id[ix[k]],
          drop(point(id, if (through[k]) center + rel[k] else narrow[ix[k]])))
        .metro_no_worse(shape(z$edge_id[ix[k]], drop(point(id, center + rel[k] + offset))),
          before[[k]])
      }
      for (pass in seq_len(4L * length(ix))) {
        fit <- .metro_spread_arrivals(rel, through, room, reach, preferred, minimum, floor,
          lower, upper)
        if (isTRUE(attr(fit, "bent")) || is.null(shape)) break
        offset <- abs(fit - rel); above <- 1L + (fit > rel)
        moved <- which(offset > room + 1e-9 & offset <= reach[cbind(seq_along(ix), above)])
        missed <- moved[!vapply(moved, function(k) clean(k, fit[k] - rel[k]), logical(1))]
        if (!length(missed)) break
        for (k in missed) {
          direction <- sign(fit[k] - rel[k]); low <- room[k]; high <- offset[k]
          for (step in 1:6) {
            middle <- (low + high) / 2
            if (clean(k, direction * middle)) low <- middle else high <- middle
          }
          reach[k, above[k]] <- low
        }
      }
      angle[ix] <- center + fit
    }
    result[[side]] <- point(ids, angle)
    rownames(result[[side]]) <- as.character(z$edge_id)
    if (side == "end") {
      result$narrow <- point(ids, narrow)
      rownames(result$narrow) <- as.character(z$edge_id)
    }
  }
  result
}

#' @export
#' @importFrom grid makeContent
makeContent.metrodag_grob <- function(x)
  .metro_capture_draw_error(x$config$diagnostic_state, x, .metro_grob_content(x))

.metro_grob_content <- function(x) {
  n <- x$nodes; e <- x$edges; cfg <- x$config
  cfg$diagnostic_state <- .metro_live_state(cfg$diagnostic_state)
  # Ports use matrix row names. Private integer IDs keep empty labels and
  # distinct numbers with identical printed representations separate.
  e$edge_id <- match(e$edge_id, unique(e$edge_id))
  # Storage order is arbitrary in reused layouts. Attachments and polylines
  # must follow each edge's segment order, using a private rendering copy.
  e <- e[order(e$edge_id, e$segment, method = "radix"), , drop = FALSE]
  width <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  height <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  if (!is.null(cfg$diagnostic_state)) {
    state <- cfg$diagnostic_state
    state$dimensions <- stats::setNames(grDevices::dev.size("in") * 25.4, c("width", "height"))
    state$panels[[length(state$panels) + 1L]] <- data.frame(
      panel = cfg$diagnostic_panel, width = width, height = height)
  }
  if (identical(cfg$on_error, "warn") && is.null(cfg$diagnostic_state)) {
    cfg$warn_collector <- new.env(parent = emptyenv())
    cfg$warn_collector$issues <- list()
  }
  report <- function(type, severity, message, ...)
    .metro_render_report(cfg, type, severity, message, ...)
  # Size advice. Geometry scales with the panel while stations, strokes and
  # arrowheads keep their physical size, so a failing distance d that needs
  # `need` mm requires the drawing to grow by about need / d. A single plot
  # knows its figure size and which dimension limits the panel.
  needed_scale <- 1
  size_advice <- function(scale = NA_real_) {
    limiting <- if (is.null(cfg$limiting)) "" else cfg$limiting
    if (is.finite(scale) && scale > 1) needed_scale <<- max(needed_scale, scale)
    target <- .metro_needed_size(cfg, c(width, height), scale)
    if (length(target) == 2L) return(sprintf("Export at least about %d x %d mm (width x height)",
      as.integer(target[["width"]]), as.integer(target[["height"]])))
    if (length(target)) return(sprintf("Export at least about %d mm %s", as.integer(target),
      if (names(target) == "width") "wide" else "tall"))
    switch(limiting, width = "Increase the export width", height = "Increase the export height",
      "Increase export width/height")
  }
  lower_advice <- function() {
    advice <- size_advice()
    paste0(tolower(substr(advice, 1L, 1L)), substring(advice, 2L))
  }
  n$x <- n$x * width; n$y <- n$y * height
  e$x <- e$x * width; e$xend <- e$xend * width
  e$y <- e$y * height; e$yend <- e$yend * height
  mm <- function(z) grid::unit(z, "mm")
  incoming <- table(factor(e$to[!duplicated(e$edge_id)], levels = n$node))
  outgoing <- table(factor(e$from[!duplicated(e$edge_id)], levels = n$node))
  style <- if (is.null(cfg$style)) metro_style() else cfg$style
  degree <- pmax(as.numeric(incoming), as.numeric(outgoing))
  radius <- switch(style$station_scaling,
    none = rep(cfg$node_size / 2, length(degree)),
    degree = pmax(cfg$node_size / 2, 0.55 * degree),
    capped = pmax(cfg$node_size / 2, pmin(0.55 * degree, 0.75 * cfg$node_size)))
  stroke <- if (is.null(style$outline_width)) max(0.32, cfg$line_width * 0.8) else style$outline_width
  if (is.null(e$width)) e$width <- rep(cfg$line_width, nrow(e))
  gp_text <- grid::gpar(fontsize = cfg$label_size * 72.27 / 25.4,
    fontfamily = cfg$family, fontface = cfg$text_options$fontface,
    lineheight = cfg$text_options$lineheight, col = cfg$text_options$colour)
  # Only margins from the final theme can extend the safe label area. When a
  # raw panel grob is drawn on its own, stay within that panel conservatively.
  margin <- cfg$label_margin
  if (is.null(margin)) margin <- cfg$label_margin_default
  if (is.null(margin)) margin <- grid::unit(rep(0, 4L), "mm")
  horizontal <- pmax(0, grid::convertWidth(margin[c(4L, 2L)], "mm", valueOnly = TRUE))
  vertical <- pmax(0, grid::convertHeight(margin[c(3L, 1L)], "mm", valueOnly = TRUE))
  bounds <- c(-horizontal[1L], width + horizontal[2L], -vertical[1L], height + vertical[2L])
  bounds <- .metro_visible_bounds(bounds, width, height, cfg$clip)
  # Rank headers are drawn by ggplot2 as axis labels outside the panel. Report
  # neighbours that overlap along the axis at this size.
  text <- cfg$header_text
  if (!is.null(x$headers) && !is.null(cfg$rank_header) && nrow(x$headers) > 1L && !isTRUE(text$blank)) {
    gp_header <- if (is.null(text)) grid::gpar(fontsize = cfg$label_size * 72.27 / 25.4,
      fontfamily = cfg$family, fontface = "bold", lineheight = cfg$text_options$lineheight) else
      grid::gpar(fontsize = text$size, fontfamily = if (is.null(text$family)) cfg$family else text$family,
        fontface = if (is.null(text$face)) "plain" else text$face,
        lineheight = if (is.null(text$lineheight)) 1 else text$lineheight)
    along_x <- (cfg$rank_header$side == "top") != isTRUE(cfg$flipped)
    # Rotated headers are measured by their rotated extent along the axis.
    grobs <- lapply(x$headers$label, grid::textGrob, gp = gp_header,
      rot = if (is.null(text$angle)) 0 else text$angle)
    extent <- vapply(grobs, function(g) if (along_x)
      grid::convertWidth(grid::grobWidth(g), "mm", valueOnly = TRUE) else
      grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE), numeric(1))
    at <- if (along_x) x$headers$x * width else x$headers$y * height
    ord <- order(at, method = "radix")
    for (k in seq_len(length(ord) - 1L)) {
      i <- ord[k]; j <- ord[k + 1L]
      if (at[j] - at[i] < (extent[i] + extent[j]) / 2 + 1)
        report("rank_header_overlap", "warning", sprintf(
          "Rank headers \"%s\" and \"%s\" overlap at this size; shorten them or break them over lines with \"\\n\".",
          x$headers$label[i], x$headers$label[j]))
    }
  }
  # A single-column export should not inherit two-column line lengths.
  if (!is.null(cfg$label_wrap)) {
    wrap <- min(cfg$label_wrap, max(12L, floor(width / (cfg$label_size * 3.5))))
    # Character counts alone do not account for wide glyphs or larger fonts.
    fit <- function(labels) vapply(labels, .metro_wrap_text, character(1),
      width = max(1, diff(bounds[1:2]) - 0.8), gp = gp_text, USE.NAMES = FALSE)
    # The same labels without camelCase breaks, for the reflow below; NA
    # where they are the same.
    plain <- vapply(n$label, .metro_wrap_label, character(1), width = wrap, camel = FALSE,
      USE.NAMES = FALSE)
    n$label <- vapply(n$label, .metro_wrap_label, character(1), width = wrap, USE.NAMES = FALSE)
    camel <- plain != n$label
    plain[!camel] <- NA_character_
    plain[camel] <- fit(plain[camel])
    n$label <- fit(n$label)
  }
  texts <- lapply(n$label, function(z) grid::textGrob(z, gp = gp_text))
  sizes <- t(vapply(texts, function(g) c(
    grid::convertWidth(grid::grobWidth(g), "mm", valueOnly = TRUE),
    grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE)), numeric(2)))
  outside_nodes <- .metro_outside_bounds(n$x, n$y, radius + stroke / 2, bounds)
  if (any(outside_nodes)) report("station_bounds", "error", sprintf(
    "Station %s extends beyond the visible figure bounds. Expand coordinate/scale limits, %s or reduce node_size.",
    paste(n$node[outside_nodes], collapse = ", "), lower_advice()), node = n$node[outside_nodes])
  children <- list(); arrowheads <- list(); drawn_heads <- list()
  label_routes <- list(); label_heads <- list()
  add <- function(g) children[[length(children) + 1L]] <<- g
  first <- e[!duplicated(e$edge_id), , drop = FALSE]
  # Stable painting order keeps the same route on top at every crossing.
  edge_ids <- first$edge_id[order(first$from, first$to, method = "radix")]
  # Stationary rows have no bearing and no visible ink. Retain every route's
  # identity above so an entirely collapsed edge is reported, never omitted.
  e <- e[e$x != e$xend | e$y != e$yend, , drop = FALSE]
  arrow_lengths <- if (is.null(style$arrow_length))
    pmax(1.3, pmin(2, 3 / sqrt(pmax(1, as.numeric(incoming))))) else rep(style$arrow_length, nrow(n))
  # Square stations use the distance to the square boundary in the final
  # segment direction; circular stations use their radius.
  clearance <- function(idx, delta) {
    rr <- radius[idx] + stroke / 2 + 0.35
    if (n$shape[idx] == 22L) rr <- rr * sqrt(sum(delta^2)) / max(abs(delta))
    rr
  }
  # Each route's centreline, trimmed by both station clearances.
  routes <- lapply(split(e, e$edge_id), function(z) {
    xy <- rbind(as.matrix(z[1L, c("x", "y"), drop = FALSE]), cbind(z$xend, z$yend))
    from <- match(z$from[1L], n$node); to <- match(z$to[1L], n$node)
    trim <- c(clearance(from, xy[2L, ] - xy[1L, ]),
      clearance(to, xy[nrow(xy), ] - xy[nrow(xy) - 1L, ]))
    list(z = z, from = from, to = to, trim = trim, full_length = sum(sqrt(rowSums(diff(xy)^2))),
      xy = .trim_polyline(xy, trim[1L], trim[2L]),
      bearing = c(z$x[nrow(z)] - z$xend[nrow(z)], z$y[nrow(z)] - z$yend[nrow(z)]))
  })
  # Arrivals join their shared tracks up to 20 mm back, or three grid steps
  # in a large figure: a fixed window would cut off at a larger export the
  # joins that a smaller export draws.
  steps <- pmax(abs(e$xend - e$x), abs(e$yend - e$y))
  join_window <- max(20, 3 * if (length(steps)) stats::median(steps) else 0)
  # A drawn route and head must stay in the figure, keep their full stroke
  # clearance from unrelated stations, and come no closer to their own
  # stations than their ports do: ports sit 0.35 mm outside the outline, so a
  # route wider than 0.7 mm could never keep its full clearance there.
  stays_clear <- function(path, head, from, to, width) {
    visible <- rbind(path, head)
    !any(.metro_outside_bounds(visible[, 1L], visible[, 2L], width / 2, bounds)) &&
      !length(.route_station_collisions(path, n, radius, c(from, to), width,
        triangle = head, outline = stroke / 2)) &&
      !length(.route_station_collisions(path, n, radius, setdiff(seq_len(nrow(n)), c(from, to)),
        min(width, 0.68), triangle = head, outline = stroke / 2))
  }
  # The shape of an arrival drawn to `point`, tested on its trimmed
  # centreline (.station_ports).
  arrival_shape <- function(id, point) {
    route <- routes[[as.character(id)]]
    if (nrow(route$xy) < 2L) return(c(back = Inf, sharp = Inf, clear = FALSE))
    target <- c(n$x[route$to], n$y[route$to]); arrow_length <- arrow_lengths[route$to]
    path <- .metro_fan_end(route$xy, point, target, arrow_length, route$bearing, join_window)
    path <- .metro_arrow_route(path, arrow_length, target)
    .metro_arrival_shape(path, stays_clear(path, .metro_arrow_triangle(path, arrow_length,
      style$arrow_angle), route$from, route$to, route$z$width[1L]))
  }
  ports <- if (cfg$arrows) .station_ports(n, e, radius, stroke / 2 + 0.35, arrow_lengths,
    style$arrow_angle, arrival_shape) else .station_ports(n, e, radius, stroke / 2 + 0.35)
  drawn_edges <- 0L; drawn_routes <- list()
  for (id in edge_ids) {
    key <- as.character(id)
    if (is.null(routes[[key]])) {
      route <- first[match(id, first$edge_id), , drop = FALSE]
      report("edge_zero_length", "error", sprintf(
        "Edge %s -> %s has zero length. Recompute the layout or provide a nonzero route between distinct endpoint positions.",
        route$from, route$to), from = route$from, to = route$to)
      next
    }
    z <- routes[[key]]$z; from <- routes[[key]]$from; to <- routes[[key]]$to
    trim <- routes[[key]]$trim; full_length <- routes[[key]]$full_length; xy <- routes[[key]]$xy
    too_small <- function(head = if (cfg$arrows) 2 else 0) report("edge_short", "error", sprintf(
      "Figure is too small to draw edge %s -> %s with station and arrow clearance. %s or reduce `node_size`.",
      z$from[1L], z$to[1L], size_advice((sum(trim) + head + 0.2) / max(full_length, 1e-6))),
      from = z$from[1L], to = z$to[1L])
    if (nrow(xy) < 2L) { too_small(); next }
    unfanned <- xy
    xy[1L, ] <- ports$start[key, ]
    xy[nrow(xy), ] <- ports$end[key, ]
    arrow_length <- arrow_lengths[to]
    edge_width <- z$width[1L]
    remaining <- sum(sqrt(rowSums((xy[-1L, , drop = FALSE] - xy[-nrow(xy), , drop = FALSE])^2)))
    if (remaining < if (cfg$arrows) arrow_length + 0.2 else 0.2) {
      too_small(if (cfg$arrows) arrow_length else 0); next
    }
    target <- c(n$x[to], n$y[to])
    # Blend the attachments, but retain the plain one if blending would lose
    # station or page clearance. The normal diagnostics still validate every
    # visible arrow.
    draw_arrival <- function(end) {
      plain <- unfanned; plain[1L, ] <- ports$start[key, ]; plain[nrow(plain), ] <- end
      if (cfg$arrows) plain <- .metro_arrow_route(plain, arrow_length, target)
      smooth <- .metro_fan_start(unfanned, ports$start[key, ], c(n$x[from], n$y[from]),
        c(z$xend[1L] - z$x[1L], z$yend[1L] - z$y[1L]))
      smooth <- .metro_fan_end(smooth, end, target, if (cfg$arrows) arrow_length else 1,
        routes[[key]]$bearing, join_window)
      if (cfg$arrows) smooth <- .metro_arrow_route(smooth, arrow_length, target)
      head <- .metro_arrow_triangle(smooth, if (cfg$arrows) arrow_length else 0, style$arrow_angle)
      clear <- stays_clear(smooth, head, from, to, edge_width)
      drawn <- if (clear) smooth else plain
      list(xy = drawn, shape = .metro_arrival_shape(drawn, clear))
    }
    drawn <- draw_arrival(ports$end[key, ])
    # A port that the station had to bend arrows to, or one tested only on
    # the trimmed centreline, can still fold the drawn shaft back on itself or
    # lose its clearance, notably when no blend fits and the shaft simply ends
    # at the port. Such an arrival keeps its narrow-fan port, the one used
    # before arrowheads were spaced, and diagnostics report any heads that
    # overlap.
    if (any(ports$end[key, ] != ports$narrow[key, ])) {
      before <- draw_arrival(ports$narrow[key, ])
      if (!.metro_no_worse(drawn$shape, before$shape, hooks = FALSE)) drawn <- before
    }
    xy <- drawn$xy
    triangle <- .metro_arrow_triangle(xy, if (cfg$arrows) arrow_length else 0, style$arrow_angle)
    visible <- rbind(xy, triangle)
    if (any(.metro_outside_bounds(visible[, 1L], visible[, 2L], edge_width / 2, bounds))) {
      report("edge_bounds", "error", sprintf(
        "Edge %s -> %s extends beyond the visible figure bounds. Expand coordinate/scale limits or %s.",
        z$from[1L], z$to[1L], lower_advice()),
        from = z$from[1L], to = z$to[1L])
      next
    }
    conflicts <- .route_station_collisions(xy, n, radius, c(from, to),
      edge_width, triangle = triangle, outline = stroke / 2)
    if (length(conflicts)) {
      gaps <- vapply(conflicts, function(k)
        .metro_point_polyline_distance(c(n$x[k], n$y[k]), xy), numeric(1))
      clear <- (radius[conflicts] + stroke / 2 + edge_width / 2 + 0.2) / pmax(gaps, 1e-6)
      # A route through a station's centre stays there at any size.
      if (any(gaps < 1e-6) && !is.null(cfg$diagnostic_state)) cfg$diagnostic_state$coincident <- TRUE
      report("edge_station", "error", sprintf(
      "Figure is too small to keep edge %s -> %s clear of unrelated station %s. %s or reduce `node_size`.",
        z$from[1L], z$to[1L], paste(n$node[conflicts], collapse = ", "),
        size_advice(if (all(is.finite(clear)) && max(clear) <= 10) max(clear) else NA_real_)),
        node = n$node[conflicts], from = z$from[1L], to = z$to[1L])
      next
    }
    # Fanning a port after trimming can turn the attachment back through its
    # own station. Attachment strokes may meet the rim, but the centreline
    # and arrowhead must not enter the station's interior.
    endpoints <- .route_station_collisions(xy, n, radius,
      setdiff(seq_len(nrow(n)), c(from, to)), 0, triangle = triangle)
    if (length(endpoints)) {
      report("edge_endpoint", "error", sprintf(
        "Edge %s -> %s enters endpoint station %s after attachment placement. %s or reduce node_size.",
        z$from[1L], z$to[1L], paste(n$node[endpoints], collapse = ", "), size_advice()),
        node = n$node[endpoints], from = z$from[1L], to = z$to[1L])
      next
    }
    segments <- function(points) data.frame(edge_id = id, from = z$from[1L], to = z$to[1L],
      x = head(points[, 1L], -1L), y = head(points[, 2L], -1L),
      xend = tail(points[, 1L], -1L), yend = tail(points[, 2L], -1L))
    label_routes[[length(label_routes) + 1L]] <- segments(xy)
    if (!is.null(triangle)) {
      label_routes[[length(label_routes) + 1L]] <- segments(rbind(triangle, triangle[1L, ]))
      label_heads[[length(label_heads) + 1L]] <- list(edge_id = id, triangle = triangle,
        bounds = c(range(triangle[, 1L]), range(triangle[, 2L])))
      drawn_heads[[length(drawn_heads) + 1L]] <- list(to = z$to[1L], from = z$from[1L],
        triangle = triangle, width = edge_width)
    }
    # Keep crossing casings interleaved with the shafts, but paint all heads
    # afterwards so a later casing cannot erase an earlier edge's direction.
    if (style$casing_width > 0) add(grid::polylineGrob(mm(xy[, 1L]), mm(xy[, 2L]),
      gp = grid::gpar(col = cfg$background, lwd = .metro_lwd(edge_width + style$casing_width),
        lineend = "round", linejoin = "round")))
    add(grid::polylineGrob(mm(xy[, 1L]), mm(xy[, 2L]),
      gp = grid::gpar(col = z$colour[1L], fill = z$colour[1L],
        lwd = .metro_lwd(edge_width), lty = z$linetype[1L],
        lineend = "round", linejoin = "round")))
    if (!is.null(triangle)) {
      arrowheads[[length(arrowheads) + 1L]] <- grid::polygonGrob(
        mm(triangle[, 1L]), mm(triangle[, 2L]), gp = grid::gpar(
          col = z$colour[1L], fill = z$colour[1L], lwd = .metro_lwd(edge_width),
          linejoin = "round"))
    }
    drawn_edges <- drawn_edges + 1L
    drawn_routes[[length(drawn_routes) + 1L]] <- data.frame(from = z$from[1L], to = z$to[1L],
      point = seq_len(nrow(xy)), x = xy[, 1L], y = xy[, 2L])
  }
  for (head in arrowheads) add(head)
  # Ports keep converging heads apart unless a station has too many arrivals
  # within one octilinear direction of their bearings (.station_ports).
  overlapping <- .metro_arrowhead_overlaps(drawn_heads)
  for (station in unique(overlapping$node)) {
    at <- overlapping[overlapping$node == station, , drop = FALSE]
    report("arrowhead_overlap", "warning", sprintf(
      paste("The arrowheads of %s and %s overlap at station %s%s. Enlarge the station with",
        "node_size or metro_style(station_scaling = \"degree\"); arrivals from one side may need",
        "a much larger station. Another export size can also change the result, either way."),
      .metro_arrow_text(at$from[1L], station), .metro_arrow_text(at$other_node[1L], station),
      station,
      if (nrow(at) > 1L) sprintf("; %d pairs overlap there", nrow(at)) else ""),
      node = at$node, other_node = at$other_node, from = at$from, to = at$node)
  }
  label_edges <- if (length(label_routes)) do.call(rbind, label_routes) else e[0L, ]
  # Reserve the complete station footprint, including its outline stroke.
  placement <- .place_labels(n, label_edges, sizes, radius + stroke / 2, bounds,
    cfg$line_width, label_heads, stroke / 2)
  if (!is.null(cfg$label_wrap)) {
    fitted <- .reflow_colliding_labels(n, label_edges, sizes, texts, gp_text,
      radius + stroke / 2, bounds, cfg$line_width, label_heads, placement, plain, stroke / 2)
    n <- fitted$nodes; sizes <- fitted$sizes; texts <- fitted$texts; placement <- fitted$placement
  }
  if (!is.null(cfg$label_nudge)) {
    delta <- cfg$label_nudge[n$node, , drop = FALSE]
    placement$centers <- placement$centers + delta
    placement$boxes <- placement$boxes + delta[, c(1L, 1L, 2L, 2L), drop = FALSE]
  }
  # Warn about unresolved collisions instead of silently calling a dense figure
  # publication-ready. This only runs at draw/export time, when size is known.
  active <- which(!is.na(placement$boxes[, 1L]))
  boxes <- placement$boxes[active, , drop = FALSE]
  label_nodes <- n$node[active]
  collision <- matrix(integer(), ncol = 2L)
  if (nrow(boxes) > 1L) for (i in seq_len(nrow(boxes) - 1L)) {
    j <- seq.int(i + 1L, nrow(boxes))
    hit <- j[.box_overlap(boxes[i, ], boxes[j, , drop = FALSE]) > 0]
    if (length(hit)) collision <- rbind(collision, cbind(i, hit))
  }
  # Label warnings name the first label and what it meets, and count the
  # labels affected; the issue rows list every pair.
  label_warning <- function(problem, detail, affected) sprintf(
    "MetroDAG labels %s at this device size. %s%s. %s or shorten labels.", problem, detail,
    if (length(unique(affected)) > 1L) sprintf("; %d labels are affected", length(unique(affected))) else "",
    size_advice())
  if (nrow(collision)) report("label_overlap", "warning", label_warning("overlap",
    sprintf("The label of %s overlaps the label of %s", label_nodes[collision[1L, 1L]],
      label_nodes[collision[1L, 2L]]), label_nodes[c(collision)]),
    node = label_nodes[collision[, 1L]], other_node = label_nodes[collision[, 2L]])
  obscured <- lapply(seq_len(nrow(boxes)), function(i) {
    ids <- .metro_label_edge_hits(label_edges, boxes[i, ], cfg$line_width, label_heads)
    hit <- match(ids, e$edge_id)
    if (length(hit)) data.frame(node = label_nodes[i], from = e$from[hit], to = e$to[hit])
  })
  obscured <- do.call(rbind, obscured)
  if (!is.null(obscured)) report("label_route", "warning", label_warning("intersect routes",
    sprintf("The label of %s crosses arrow %s", obscured$node[1L],
      .metro_arrow_text(obscured$from[1L], obscured$to[1L])), obscured$node),
    node = obscured$node, from = obscured$from, to = obscured$to)
  outside <- boxes[, 1L] < bounds[1L] - 1e-8 | boxes[, 2L] > bounds[2L] + 1e-8 |
    boxes[, 3L] < bounds[3L] - 1e-8 | boxes[, 4L] > bounds[4L] + 1e-8
  if (any(outside)) report("label_bounds", "warning", label_warning("extend beyond the figure margin",
    sprintf("The label of %s does not fit", label_nodes[outside][1L]), label_nodes[outside]),
    node = label_nodes[outside])
  obscured <- lapply(seq_len(nrow(boxes)), function(i) {
    hit <- .metro_label_station_hits(n, boxes[i, ], radius, stroke / 2)
    if (length(hit)) data.frame(node = label_nodes[i], other_node = n$node[hit])
  })
  obscured <- do.call(rbind, obscured)
  if (!is.null(obscured)) report("label_station", "warning", label_warning("overlap stations",
    sprintf("The label of %s overlaps %s", obscured$node[1L],
      if (obscured$other_node[1L] == obscured$node[1L]) "its own station" else
        paste("station", obscured$other_node[1L])), obscured$node),
    node = obscured$node, other_node = obscured$other_node)
  ambiguous <- .metro_label_ambiguity(n, placement$boxes, radius, edges = label_edges,
    line_width = cfg$line_width)
  if (nrow(ambiguous)) report("label_association", "warning", sprintf(
    "The label of %s %s; readers may attach it to the wrong variable. %s, shorten labels, or use label_nudge.",
    ambiguous$node[1L], switch(ambiguous$relation[1L],
      nearer = sprintf("is nearer station %s than its own station", ambiguous$other_node[1L]),
      closer = sprintf("is nearer station %s than that station's own label", ambiguous$other_node[1L]),
      sprintf("is cut off from its station by arrow %s",
        .metro_arrow_text(ambiguous$from[1L], ambiguous$to[1L]))),
    size_advice()),
    node = ambiguous$node, other_node = ambiguous$other_node,
    from = ambiguous$from, to = ambiguous$to)
  pairs <- .station_overlap_pairs(n, radius, stroke / 2)
  if (nrow(pairs)) {
    distance <- sqrt((n$x[pairs[, 1L]] - n$x[pairs[, 2L]])^2 + (n$y[pairs[, 1L]] - n$y[pairs[, 2L]])^2)
    apart <- (radius[pairs[, 1L]] + radius[pairs[, 2L]] + stroke + 0.2) / pmax(distance, 1e-6)
    # Stations drawn at one point overlap at any size.
    if (any(distance < 1e-6) && !is.null(cfg$diagnostic_state)) cfg$diagnostic_state$coincident <- TRUE
    report("station_overlap", "error", sprintf(
      "Figure is too small to keep stations %s and %s separate (%d overlapping pair%s). %s or reduce node_size.",
      n$node[pairs[1L, 1L]], n$node[pairs[1L, 2L]], nrow(pairs), if (nrow(pairs) == 1L) "" else "s",
      size_advice(if (max(apart) <= 10) max(apart) else NA_real_)),
      node = n$node[pairs[, 1L]], other_node = n$node[pairs[, 2L]])
  }
  for (i in seq_len(nrow(n))) {
    gp <- grid::gpar(col = n$colour[i], fill = n$fill[i], lwd = .metro_lwd(stroke),
      lty = if (n$role[i] == "unobserved") "dashed" else "solid", linejoin = "round")
    add(.metro_station_grob(mm(n$x[i]), mm(n$y[i]), mm(radius[i]), n$shape[i],
      if (is.null(n$marker)) "plain" else n$marker[i], gp, cfg$background))
    if (!is.null(n$group_mark) && nzchar(n$group_mark[i])) add(.metro_group_mark_grob(
      n$group_mark[i], mm(n$x[i]), mm(n$y[i]), radius[i], n$fill[i], cfg$background, cfg$family))
  }
  for (i in seq_len(nrow(n))) if (nzchar(n$label[i])) {
    center <- placement$centers[i, ]
    add(grid::editGrob(texts[[i]], x = mm(center[1L]), y = mm(center[2L])))
  }
  if (!is.null(cfg$warn_collector) && length(cfg$warn_collector$issues)) {
    issues <- do.call(rbind, cfg$warn_collector$issues)
    warning(structure(list(message = sprintf(
      "%d drawing problem%s at this size; the figure was drawn without the affected elements (on_error = \"warn\"). First: %s",
      nrow(issues), if (nrow(issues) == 1L) "" else "s", issues$message[1L]),
      call = NULL, issues = issues),
      class = c("metrodag_render_warning", "warning", "condition")))
  }
  routes <- if (length(drawn_routes)) do.call(rbind, drawn_routes) else
    data.frame(from = character(), to = character(), point = integer(), x = numeric(), y = numeric())
  if (!is.null(cfg$diagnostic_state) && needed_scale > 1) {
    target <- .metro_needed_size(cfg, c(width, height), needed_scale)
    if (length(target)) cfg$diagnostic_state$min_size <-
      .metro_larger_size(cfg$diagnostic_state$min_size, target)
  }
  if (!is.null(cfg$diagnostic_state))
    cfg$diagnostic_state$routes[[cfg$diagnostic_panel]] <-
      cbind(panel = rep(as.character(cfg$diagnostic_panel), nrow(routes)), routes)
  x$diagnostics <- list(drawn_edges = drawn_edges, label_boxes = placement$boxes,
    label_centers = placement$centers, label_bounds = bounds, nodes = n[, c("node", "x", "y")],
    routes = routes)
  grid::setChildren(x, do.call(grid::gList, children))
}
