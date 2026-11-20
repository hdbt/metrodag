# Simplify a whole edge after routing, rather than requiring it to turn at
# every private waypoint. Waypoints still guide ordering and the initial route.
.routed_geometry_quality <- function(routes) {
  e <- .bind_route_columns(routes)
  if (is.null(e)) e <- .empty_routes()
  keys <- .route_grid_keys(e)
  ids <- match(keys, unique(keys))
  counts <- tabulate(ids)
  lengths <- sqrt((e$xend - e$x)^2 + (e$yend - e$y)^2)
  shared <- sum(counts * (counts - 1) / 2 * lengths[!duplicated(ids)])
  c(crossings = .count_route_contacts(e), shared = shared,
    bends = sum(e$bend), length = sum(lengths))
}

.route_runs <- function(e) {
  x <- .subset2(e, "x"); xend <- .subset2(e, "xend")
  dx <- xend - x
  n <- length(dx)
  # dx[-1L] - dx[-n] is diff(dx), without the generic's dispatch and checks.
  starts <- c(1L, which(dx[-1L] - dx[-n] != 0) + 1L)
  ends <- c(starts[-1L] - 1L, n)
  .route_frame(x = x[starts], y = .subset2(e, "y")[starts],
    xend = xend[ends], yend = .subset2(e, "yend")[ends])
}

.route_from_moves <- function(x, y, moves) {
  k <- length(moves)
  xs <- c(x, x + cumsum(moves)); ys <- y - seq.int(0L, k)
  .route_frame(x = xs[seq_len(k)], y = ys[seq_len(k)], xend = xs[-1L], yend = ys[-1L])
}

# Both useful two-bend grammars: diagonal/vertical/diagonal and
# vertical/diagonal/vertical, including their straight and one-bend limits.
.simple_route_candidates <- function(e, endpoint_lanes = FALSE) {  # opt-in endpoint lanes
  moves <- .simple_route_moves(.subset2(e, "x"), .subset2(e, "xend"), endpoint_lanes)
  x0 <- .subset2(e, "x")[1L]; y0 <- .subset2(e, "y")[1L]
  lapply(moves, function(z) .route_from_moves(x0, y0, z))
}

# The unit moves of .simple_route_candidates() for a route whose steps start
# at `x` and end at `xend`, in the same order.
.simple_route_moves <- function(x, xend, endpoint_lanes = FALSE) {
  set <- .simple_route_set(x, xend, endpoint_lanes)
  lapply(set$selected, function(i) rep(set$values[, i], set$times[, i]))
}

# .simple_route_candidates(e) stacked as .stack_routes() stacks route frames,
# without building a frame per candidate. .stacked_route() returns one.
.simple_route_stack <- function(e, endpoint_lanes = FALSE) {
  set <- .simple_route_set(.subset2(e, "x"), .subset2(e, "xend"), endpoint_lanes)
  i <- set$selected
  times <- set$times[, i, drop = FALSE]
  .stack_moves(.subset2(e, "x")[1L], .subset2(e, "y")[1L],
    rep(as.vector(set$values[, i]), as.vector(times)), as.integer(colSums(times)))
}

# Both two-bend grammars for a route whose steps start at `x` and end at
# `xend`: every candidate as three runs of equal moves (`values` repeated
# `times`, one column per candidate) and the candidates to use, `selected`,
# in order. A candidate's run-length code identifies its moves exactly and
# gives its bend count and diagonal travel without listing every move.
.simple_route_set <- function(x, xend, endpoint_lanes = FALSE) {
  h <- length(x)
  x0 <- x[1L]; x1 <- xend[h]
  delta <- x1 - x0; diagonal <- abs(delta)
  slack <- h - diagonal
  v1 <- v2 <- v3 <- l1 <- l2 <- l3 <- numeric()
  if (slack >= 0) {
    before <- if (diagonal == 0) 0L else unique(c(0L, slack, seq.int(0L, slack)))
    v1 <- rep(0, length(before)); v2 <- rep(sign(delta), length(before)); v3 <- v1
    l1 <- before; l2 <- rep(diagonal, length(before)); l3 <- slack - before
  }
  # lanes stay within +-2 of the route's x-range. Wider lanes admitted
  # one-bend V detours with no vertical run and increased gallery bends/area.
  # optionally measure the margin from the endpoints, not the current
  # route, so a waypoint-displaced route cannot widen its own lane window.
  span <- if (endpoint_lanes) c(x0, x1) else c(x, xend)
  lanes <- seq.int(min(span) - 2L, max(span) + 2L)
  first <- abs(lanes - x0); last <- abs(x1 - lanes)
  lanes <- lanes[first + last <= h]
  first <- abs(lanes - x0); last <- abs(x1 - lanes)
  v1 <- c(v1, sign(lanes - x0)); v2 <- c(v2, rep(0, length(lanes))); v3 <- c(v3, sign(x1 - lanes))
  l1 <- c(l1, first); l2 <- c(l2, h - first - last); l3 <- c(l3, last)
  k <- length(v1)
  if (!k) return(list(values = matrix(numeric(), 3L, 0L), times = matrix(integer(), 3L, 0L),
    selected = integer()))
  # Candidate i is c(rep(values[1, i], times[1, i]), rep(values[2, i], ...), ...).
  values <- rbind(v1, v2, v3, deparse.level = 0L)
  times <- matrix(as.integer(rbind(l1, l2, l3)), 3L)
  size <- colSums(times)
  # Its run-length code: drop empty runs and merge equal neighbours. Encode
  # (value, length) of up to three runs in base 3 * (n + 1): exact integers in
  # double precision, so equal codes mean equal move vectors.
  n <- max(size)
  base <- 3 * (n + 1)
  if (any(size == 0L) || base^3 >= 2^52) {
    out <- lapply(seq_len(k), function(i) rep(values[, i], times[, i]))
    keep <- which(!duplicated(vapply(out, paste, collapse = ",", character(1))))
    bends <- vapply(out[keep], function(z) sum(diff(z) != 0), integer(1))
    ink <- vapply(out[keep], function(z) sum(abs(z)), numeric(1))
  } else {
    present <- times > 0L
    code <- numeric(k); runs <- integer(k); length <- integer(k)
    current <- previous <- rep(NA_real_, k)
    for (r in 1:3) {
      # A present run continues the current one when its value repeats.
      value <- values[r, ]
      start <- present[r, ] & (is.na(previous) | value != previous)
      done <- start & runs > 0L
      code[done] <- code[done] + ((current[done] + 1) * (n + 1) + length[done]) * base^(runs[done] - 1L)
      runs[start] <- runs[start] + 1L
      current[start] <- value[start]
      more <- present[r, ] & !start
      length[more] <- length[more] + times[r, more]
      length[start] <- times[r, start]
      previous[present[r, ]] <- value[present[r, ]]
    }
    code <- code + ((current + 1) * (n + 1) + length) * base^(runs - 1L)
    keep <- which(!duplicated(code))
    bends <- runs[keep] - 1L
    ink <- as.numeric(colSums(times * (values != 0)))[keep]
  }
  # Fewer bends first, then less diagonal travel. Preserve a deterministic tie.
  list(values = values, times = times, selected = keep[order(bends, ink, method = "radix")])
}

# Routes from (x0, y0) with the stacked unit `moves` (candidate i has size[i]),
# stacked as .stack_routes() stacks the frames .route_from_moves() builds.
# Sums of moves of -1, 0 and 1 are exact, so positions match those frames.
.stack_moves <- function(x0, y0, moves, size) {
  k <- length(size)
  if (!k || !all(moves %in% c(-1, 0, 1)))
    return(.stack_routes(lapply(seq_len(k), function(i)
      .route_from_moves(x0, y0, moves[c(0L, cumsum(size))[i] + seq_len(size[i])]))))
  owner <- rep.int(seq_len(k), size)
  offset <- c(0L, cumsum(size))[seq_len(k)]
  step <- seq_along(owner) - offset[owner]
  total <- cumsum(moves)
  sums <- total - c(0, total)[offset + 1L][owner]
  xend <- x0 + sums
  x <- c(x0, xend[-length(xend)])
  x[step == 1L] <- x0
  .stack_steps(x, y0 - (step - 1L), xend, y0 - step, owner, size)
}

.route_obstacle_free <- function(runs, nodes, clearance = 0.8) {
  x <- .subset2(nodes, "x"); rx <- .subset2(runs, "x")
  m <- length(x); k <- length(rx)
  if (!m || !k) return(TRUE)
  ry <- .subset2(runs, "y"); rxend <- .subset2(runs, "xend"); ryend <- .subset2(runs, "yend")
  near <- .stations_near(x, .subset2(nodes, "y"), rx, ry, rxend, ryend, max(clearance))
  if (!length(near)) return(TRUE)
  if (length(clearance) > 1L) clearance <- clearance[near]
  m <- length(near)
  # All node/run pairs at once: node index varies fastest within each run.
  !any(.point_segment_distance(rep.int(x[near], k), rep.int(.subset2(nodes, "y")[near], k),
    rep(rx, each = m), rep(ry, each = m), rep(rxend, each = m), rep(ryend, each = m)) < clearance)
}

# Stations that can come within `reach` of some run: those inside the runs'
# joint bounding box widened by `reach` and a margin far above rounding
# error. A station outside it is further than that from every run, so no
# computed distance to it falls below `reach`.
.stations_near <- function(x, y, rx, ry, rxend, ryend, reach) {
  lo_x <- min(rx, rxend); hi_x <- max(rx, rxend)
  lo_y <- min(ry, ryend); hi_y <- max(ry, ryend)
  margin <- reach + 1e-6 * (1 + max(abs(c(lo_x, hi_x, lo_y, hi_y))))
  which(x >= lo_x - margin & x <= hi_x + margin & y >= lo_y - margin & y <= hi_y + margin)
}

# Bind internal tables column by column. rbind.data.frame dominates the
# candidate-search loops; these tables are scratch data and never returned.
# Tables with other column names, or with factor (or other classed) columns,
# use rbind() itself.
.bind_route_columns <- function(tables) {
  rows <- integer(length(tables))
  columns <- lengths(tables) > 0L
  rows[columns] <- lengths(lapply(tables[columns], .subset2, 1L))
  tables <- tables[rows > 0L]
  if (!length(tables)) return(NULL)
  if (length(tables) == 1L) return(tables[[1L]])
  columns <- names(tables[[1L]])
  labels <- lapply(tables, names)
  if (!all(lengths(labels) == length(columns)) || !all(unlist(labels, use.names = FALSE) == columns) ||
      any(vapply(unlist(tables, recursive = FALSE, use.names = FALSE), is.object, logical(1))))
    return(do.call(rbind, tables))
  out <- lapply(columns, function(key) unlist(lapply(tables, .subset2, key), use.names = FALSE))
  names(out) <- columns
  .as_rows(out, length(out[[1L]]))
}

# Bind every route's obstacles once, remembering which route owns each row,
# so the obstacles for one route are a cheap vector subset of the rest.
.obstacle_index <- function(prepared) {
  segments <- lapply(prepared, .subset2, "segments")
  keys <- lapply(prepared, .subset2, "keys")
  list(segments = .bind_route_columns(segments),
    owner = rep.int(seq_along(prepared), lengths(lapply(segments, .subset2, "x"))),
    keys = unlist(keys, use.names = FALSE),
    key_owner = rep.int(seq_along(prepared), lengths(keys)))
}

# The obstacles for route `id`. Only the columns .route_conflicts() reads.
.obstacles_except <- function(index, id) {
  keep <- index$owner != id
  segments <- if (is.null(index$segments) || !any(keep)) .empty_routes() else {
    columns <- lapply(.subset(index$segments, .conflict_columns), `[`, keep)
    .as_rows(columns, sum(keep))
  }
  list(segments = segments, keys = unique(index$keys[index$key_owner != id]))
}

.conflict_columns <- c("edge_id", "x", "y", "xend", "yend",
  "source_x", "source_y", "target_x", "target_y")

.prepare_route_obstacles <- function(other) {
  if (!nrow(other)) return(list(segments = other, keys = numeric()))
  edge_id <- .subset2(other, "edge_id")
  first <- !duplicated(edge_id)
  last <- !duplicated(edge_id, fromLast = TRUE)
  # Collapse only the columns the conflict tests read; the collapsed runs of
  # these columns are those of the whole table.
  runs <- .collapse_routes(.as_rows(.subset(other, c("edge_id", "segment", "x", "y", "xend", "yend")),
    length(edge_id)))
  run_id <- .subset2(runs, "edge_id")
  a <- match(run_id, edge_id[first]); b <- match(run_id, edge_id[last])
  segments <- list(edge_id = run_id, x = .subset2(runs, "x"), y = .subset2(runs, "y"),
    xend = .subset2(runs, "xend"), yend = .subset2(runs, "yend"),
    source_x = .subset2(other, "x")[first][a], source_y = .subset2(other, "y")[first][a],
    target_x = .subset2(other, "xend")[last][b], target_y = .subset2(other, "yend")[last][b])
  list(segments = .as_rows(segments, length(run_id)), keys = unique(.route_grid_keys(other)))
}

# These internal routes consist of integer unit steps: (x, y, dx) identifies a
# segment. Signed Cantor pairing avoids allocating a string for every step.
.route_grid_keys <- function(e) .grid_keys(.subset2(e, "x"), .subset2(e, "y"), .subset2(e, "xend"))

.grid_keys <- function(x0, y0, xend) {
  x <- 2 * abs(x0) - (x0 < 0)
  y <- 2 * abs(y0) - (y0 < 0)
  3 * ((x + y) * (x + y + 1) / 2 + y) + xend - x0 + 1
}

# Construct known, equal-length internal columns without data.frame's repeated
# name/deparse and coercion work in the candidate-search inner loop.
.route_frame <- function(x, y, xend, yend) {
  out <- list(x = x, y = y, xend = xend, yend = yend)
  attr(out, "row.names") <- c(NA_integer_, -length(x))
  class(out) <- "data.frame"
  out
}

# Count intersections against other edges only. As in .count_route_contacts(),
# internal grid/bend contacts count, common original endpoints do not.
.route_conflicts <- function(e, obstacles, runs = .route_runs(e)) {
  other <- obstacles$segments
  x <- .subset2(e, "x"); y <- .subset2(e, "y")
  xend <- .subset2(e, "xend"); yend <- .subset2(e, "yend")
  shared <- .grid_keys(x, y, xend) %in% obstacles$keys
  overlap <- sum(sqrt((xend[shared] - x[shared])^2 + (yend[shared] - y[shared])^2))
  if (!length(.subset2(other, "x"))) return(c(overlap = overlap, crossings = 0))
  n <- length(x)
  rx <- .subset2(runs, "x")
  crossings <- .route_crossing_counts(rx, .subset2(runs, "y"), .subset2(runs, "xend"),
    .subset2(runs, "yend"), rep.int(1L, length(rx)), 1L, x[1L], y[1L], xend[n], yend[n], other)
  c(overlap = overlap, crossings = crossings)
}

# Distinct crossing events of candidate routes with `other`, per candidate.
# Run i of the stacked runs belongs to candidate owner[i], whose own route
# starts at (x0, y0)[owner[i]] and ends at (x1, y1)[owner[i]]. Every run meets
# every obstacle segment with the arithmetic of a per-run loop, so a count
# equals the length of the unique "edge x y" event strings of that candidate.
.route_crossing_counts <- function(rx, ry, rxend, ryend, owner, k, x0, y0, x1, y1, other) {
  ox <- .subset2(other, "x"); oy <- .subset2(other, "y")
  m <- length(ox); r <- length(rx)
  counts <- numeric(k)
  if (!m || !r) return(counts)
  oxend <- .subset2(other, "xend"); oyend <- .subset2(other, "yend")
  bx <- oxend - ox; by <- oyend - oy
  ax <- rxend - rx; ay <- ryend - ry
  # With integer coordinates, a pair can only meet when the segments'
  # bounding boxes touch: t and u lie within 1e-10 of [0, 1], so the exact
  # intersection is far less than half a unit outside either segment, while
  # boxes with integer corners that do not touch are a unit apart. Skip other
  # pairs; their arithmetic below could not produce an event. A segment must
  # then also touch the joint box of all runs.
  corners <- c(rx, ry, rxend, ryend, ox, oy, oxend, oyend)
  if (all(corners == round(corners)) && max(abs(corners)) < 2^20) {
    olo_x <- pmin(ox, oxend); ohi_x <- pmax(ox, oxend); olo_y <- pmin(oy, oyend); ohi_y <- pmax(oy, oyend)
    reach <- which(olo_y <= max(ry, ryend) & ohi_y >= min(ry, ryend) &
      olo_x <= max(rx, rxend) & ohi_x >= min(rx, rxend))
    if (!length(reach)) return(counts)
    i <- rep(seq_len(r), each = length(reach)); j <- rep.int(reach, r)
    near <- which(pmin(ry, ryend)[i] <= ohi_y[j] & olo_y[j] <= pmax(ry, ryend)[i] &
      pmin(rx, rxend)[i] <= ohi_x[j] & olo_x[j] <= pmax(rx, rxend)[i])
    i <- i[near]; j <- j[near]
    if (!length(i)) return(counts)
  } else {
    i <- rep(seq_len(r), each = m); j <- rep.int(seq_len(m), r)
  }
  ai <- ax[i]; bi <- ay[i]
  denominator <- ai * by[j] - bi * bx[j]
  valid <- abs(denominator) > 1e-10
  denominator[!valid] <- 1
  dx <- ox[j] - rx[i]; dy <- oy[j] - ry[i]
  t <- (dx * by[j] - dy * bx[j]) / denominator
  u <- (dx * bi - dy * ai) / denominator
  hit <- which(valid & t >= -1e-10 & t <= 1 + 1e-10 & u >= -1e-10 & u <= 1 + 1e-10)
  if (!length(hit)) return(counts)
  i <- i[hit]; j <- j[hit]
  px <- rx[i] + t[hit] * ax[i]; py <- ry[i] + t[hit] * ay[i]
  of <- owner[i]
  terminal <- (abs(px - x0[of]) + abs(py - y0[of]) < 1e-8) |
    (abs(px - x1[of]) + abs(py - y1[of]) < 1e-8)
  other_terminal <- (abs(px - .subset2(other, "source_x")[j]) +
      abs(py - .subset2(other, "source_y")[j]) < 1e-8) |
    (abs(px - .subset2(other, "target_x")[j]) + abs(py - .subset2(other, "target_y")[j]) < 1e-8)
  keep <- which(!(terminal & other_terminal))
  if (!length(keep)) return(counts)
  of <- of[keep]
  counts <- tabulate(of, k)
  # A candidate with one event needs no key.
  repeated <- counts[of] > 1L
  if (any(repeated)) {
    keep <- keep[repeated]; of <- of[repeated]
    id <- .subset2(other, "edge_id")[j[keep]]
    ex <- round(px[keep], 8); ey <- round(py[keep], 8)
    counts[unique(of)] <- 0L
    counts <- counts + tabulate(of[.distinct_events(of, id, ex, ey)], k)
  }
  as.numeric(counts)
}

# First occurrences of events (candidate, edge, x, y), with the equality of
# their paste() strings. Coordinates rounded to 8 decimals below 1e6 in size
# print 15 significant digits, enough to tell any two such values apart, so
# numeric equality is the same relation there. Equal values sort together.
.distinct_events <- function(of, id, ex, ey) {
  n <- length(of)
  if (max(abs(c(ex, ey))) >= 1e6 || any(id != round(id)) || max(abs(id)) >= 1e15)
    return(!duplicated(paste(of, id, ex, ey)))
  o <- order(of, id, ex, ey, method = "radix")
  same <- c(FALSE, of[o][-1L] == of[o][-n] & id[o][-1L] == id[o][-n] &
    ex[o][-1L] == ex[o][-n] & ey[o][-1L] == ey[o][-n])
  first <- logical(n)
  first[o[!same]] <- TRUE
  first
}

# Candidate routes stacked into one table of steps and one of runs, so a
# search scores them all with a few long vector operations instead of a loop
# of short ones. The runs of candidate k are those .route_runs() finds, and
# bends[k] is its bend count, sum(diff(z$xend - z$x) != 0).
.stack_routes <- function(candidates) {
  k <- length(candidates)
  x <- lapply(candidates, .subset2, "x")
  size <- lengths(x)
  x <- unlist(x, use.names = FALSE)
  y <- unlist(lapply(candidates, .subset2, "y"), use.names = FALSE)
  xend <- unlist(lapply(candidates, .subset2, "xend"), use.names = FALSE)
  yend <- unlist(lapply(candidates, .subset2, "yend"), use.names = FALSE)
  .stack_steps(x, y, xend, yend, rep.int(seq_len(k), size), size)
}

.stack_steps <- function(x, y, xend, yend, owner, size) {
  k <- length(size)
  n <- length(x)
  dx <- xend - x
  start <- if (n) c(TRUE, dx[-1L] - dx[-n] != 0 | owner[-1L] != owner[-n]) else logical()
  first <- which(start); last <- c(first[-1L] - 1L, n)
  run_owner <- owner[first]
  list(k = k, size = size, offset = c(0L, cumsum(size))[seq_len(k)], owner = owner,
    x = x, y = y, xend = xend, yend = yend, dx = dx,
    rx = x[first], ry = y[first], rxend = xend[last], ryend = yend[last],
    run_owner = run_owner, run_start = first, bends = tabulate(run_owner, k) - 1L)
}

# Candidate k of a stack as its route frame.
.stacked_route <- function(stack, k) {
  i <- stack$offset[k] + seq_len(stack$size[k])
  .route_frame(x = stack$x[i], y = stack$y[i], xend = stack$xend[i], yend = stack$yend[i])
}

# For stacked candidates `ids`: .route_obstacle_free(.route_runs(z), nodes,
# clearance) of each. `clearance` is one value or one per station.
.stacked_clear <- function(stack, ids, nodes, clearance = 0.8) {
  free <- rep(TRUE, length(ids))
  nx <- .subset2(nodes, "x"); m <- length(nx)
  position <- match(stack$run_owner, ids)
  runs <- which(!is.na(position))
  r <- length(runs)
  if (!m || !r) return(free)
  rx <- stack$rx[runs]; ry <- stack$ry[runs]; rxend <- stack$rxend[runs]; ryend <- stack$ryend[runs]
  ny <- .subset2(nodes, "y")
  near <- .stations_near(nx, ny, rx, ry, rxend, ryend, max(clearance))
  if (!length(near)) return(free)
  if (length(clearance) > 1L) clearance <- clearance[near]
  m <- length(near)
  # Node index varies fastest within each run, as in .route_obstacle_free().
  close <- which(.point_segment_distance(rep.int(nx[near], r), rep.int(ny[near], r),
    rep(rx, each = m), rep(ry, each = m), rep(rxend, each = m), rep(ryend, each = m)) < clearance)
  if (length(close)) free[position[runs[unique((close - 1L) %/% m + 1L)]]] <- FALSE
  free
}

# For stacked candidates `ids`: overlap[j] and crossings[j] are the two values
# of .route_conflicts(z, obstacles) for candidate z = ids[j].
.stacked_conflicts <- function(stack, ids, obstacles) {
  k <- length(ids)
  if (!k) return(list(overlap = numeric(), crossings = numeric()))
  position <- match(stack$owner, ids)
  steps <- which(!is.na(position))
  x <- stack$x[steps]; y <- stack$y[steps]; xend <- stack$xend[steps]; yend <- stack$yend[steps]
  shared <- .grid_keys(x, y, xend) %in% obstacles$keys
  owner <- position[steps][shared]
  lengths <- sqrt((xend[shared] - x[shared])^2 + (yend[shared] - y[shared])^2)
  overlap <- .candidate_sums(lengths, owner, k)
  other <- obstacles$segments
  if (!length(.subset2(other, "x"))) return(list(overlap = overlap, crossings = numeric(k)))
  position <- match(stack$run_owner, ids)
  runs <- which(!is.na(position))
  first <- stack$offset[ids] + 1L; last <- stack$offset[ids] + stack$size[ids]
  list(overlap = overlap, crossings = .route_crossing_counts(stack$rx[runs], stack$ry[runs],
    stack$rxend[runs], stack$ryend[runs], position[runs], k, stack$x[first], stack$y[first],
    stack$xend[last], stack$yend[last], other))
}

# One sum per candidate 1..k of the values it owns, in their order, as each
# candidate's own sum() (0 for a candidate owning none).
.candidate_sums <- function(values, owner, k) {
  if (!length(owner)) return(numeric(k))
  vapply(split(values, structure(owner, levels = as.character(seq_len(k)), class = "factor")),
    sum, numeric(1), USE.NAMES = FALSE)
}

# Coordinates of the stations other than an arrow's own two endpoints.
.unrelated_stations <- function(nodes, e) {
  keep <- !nodes$node %in% c(.subset2(e, "from")[1L], .subset2(e, "to")[1L])
  list(x = nodes$x[keep], y = nodes$y[keep])
}

# Obstacles of a growing drawing: the rows of `b`'s routes follow those of `a`
# (other edges), and occupancy keys are only ever matched against.
.merge_route_obstacles <- function(a, b) {
  segments <- .bind_route_columns(list(a$segments, b$segments))
  if (is.null(segments)) segments <- a$segments
  list(segments = segments, keys = c(a$keys, b$keys))
}

.straighten_routes <- function(routes, nodes, eligible = seq_along(routes), clearance = 0.8,
  endpoint_lanes = FALSE) {  # forwarded to .simple_route_candidates()
  if (!length(routes)) return(routes)
  prepared <- lapply(routes, .prepare_route_obstacles)
  index <- .obstacle_index(prepared)
  from <- .route_from(routes)
  to <- .route_to(routes)
  length_of <- function(z) sum(sqrt((z$xend - z$x)^2 + (z$yend - z$y)^2))
  for (pass in seq_len(4L)) {
    changed <- FALSE
    priority <- order(-.route_bends(routes),
      -.route_steps(routes), from, to, method = "radix")
    priority <- priority[priority %in% eligible]
    for (id in priority) {
      e <- routes[[id]]
      bends <- sum(e$bend)
      x <- .subset2(e, "x"); h <- length(x)
      dx <- .subset2(e, "xend")[h] - x[1L]
      minimum <- if (dx == 0 || abs(dx) == h) 0L else 1L
      obstacles <- .obstacles_except(index, id)
      limit <- .route_conflicts(e, obstacles)
      if (bends <= minimum && !any(limit > 0)) next
      best <- e
      best_score <- c(limit, bends, length_of(e))
      unrelated <- .unrelated_stations(nodes, e)
      stack <- .simple_route_stack(e, endpoint_lanes)
      # Candidates come with fewer bends first. Score each group with one bend
      # count together; the loop below keeps the per-candidate decisions.
      group <- cumsum(c(TRUE, stack$bends[-1L] != stack$bends[-stack$k]))[seq_len(stack$k)]
      for (g in unique(group)) {
        ids <- which(group == g)
        candidate_bends <- stack$bends[ids[1L]]
        # When there are no conflicts, extra bends cannot improve this route.
        if (!any(limit > 0) && candidate_bends > best_score[3L]) break
        ids <- ids[.stacked_clear(stack, ids, unrelated, clearance)]
        if (!length(ids)) next
        conflicts <- .stacked_conflicts(stack, ids, obstacles)
        for (j in seq_along(ids)) {
          quality <- c(overlap = conflicts$overlap[j], crossings = conflicts$crossings[j])
          if (any(quality > limit + 1e-8)) next
          candidate <- .stacked_route(stack, ids[j])
          score <- c(quality, candidate_bends, length_of(candidate))
          if (!.lexically_better(score, best_score)) next
          candidate$from <- e$from[1L]; candidate$to <- e$to[1L]
          candidate$edge_id <- e$edge_id[1L]; candidate$path_id <- e$path_id[1L]
          candidate$segment <- seq_len(nrow(candidate))
          candidate$bend <- c(FALSE, diff(candidate$xend - candidate$x) != 0)
          best <- candidate[, names(e)]
          best_score <- score
        }
      }
      if (!identical(best, e)) {
        routes[[id]] <- best
        prepared[[id]] <- .prepare_route_obstacles(best)
        index <- .obstacle_index(prepared)
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  routes
}
