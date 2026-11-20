# The batched route scorers must reproduce one-candidate-at-a-time scoring
# exactly. Layout searches compare these values with tolerances and break
# ties by candidate order, so any difference could change a drawing. The
# references below are the per-candidate implementations they replaced.

.scoring_route <- function(moves, x, y, id) {
  e <- .route_from_moves(x, y, moves)
  e$from <- paste0("s", id); e$to <- paste0("t", id); e$edge_id <- id
  e$segment <- seq_along(moves); e$bend <- c(FALSE, diff(moves) != 0)
  e$path_id <- NA_integer_
  e[, names(.empty_routes())]
}

.random_scoring_routes <- function(count, half = FALSE) {
  lapply(seq_len(count), function(id) {
    h <- sample(1:12, 1L)
    moves <- if (stats::runif(1L) < 0.5) sample(c(-1, 0, 1), h, replace = TRUE) else
      rep(sample(c(-1, 0, 1), 3L, replace = TRUE), c(h %/% 3L, h %/% 3L, h - 2L * (h %/% 3L)))
    .scoring_route(moves, sample(-6:6, 1L) + if (half && id %% 2L) 0.5 else 0, sample(0:8, 1L), id)
  })
}

.reference_simple_candidates <- function(e, endpoint_lanes = FALSE) {
  x0 <- e$x[1L]; y0 <- e$y[1L]
  x1 <- tail(e$xend, 1L); h <- nrow(e)
  delta <- x1 - x0; diagonal <- abs(delta)
  moves <- list()
  add <- function(z) moves[[length(moves) + 1L]] <<- z
  slack <- h - diagonal
  if (slack >= 0) {
    before <- if (diagonal == 0) 0L else unique(c(0L, slack, seq.int(0L, slack)))
    for (a in before) add(c(rep(0, a), rep(sign(delta), diagonal), rep(0, slack - a)))
  }
  span <- if (endpoint_lanes) c(x0, x1) else c(e$x, e$xend)
  for (lane in seq.int(min(span) - 2L, max(span) + 2L)) {
    first <- abs(lane - x0); last <- abs(x1 - lane)
    if (first + last > h) next
    add(c(rep(sign(lane - x0), first), rep(0, h - first - last), rep(sign(x1 - lane), last)))
  }
  moves <- moves[!duplicated(vapply(moves, paste, collapse = ",", character(1)))]
  bends <- vapply(moves, function(z) sum(diff(z) != 0), integer(1))
  ink <- vapply(moves, function(z) sum(abs(z)), numeric(1))
  lapply(moves[order(bends, ink, method = "radix")], function(z) .route_from_moves(x0, y0, z))
}

.reference_conflicts <- function(e, obstacles, runs = .route_runs(e)) {
  other <- obstacles$segments
  shared <- .route_grid_keys(e) %in% obstacles$keys
  overlap <- sum(sqrt((e$xend[shared] - e$x[shared])^2 + (e$yend[shared] - e$y[shared])^2))
  if (!nrow(other)) return(c(overlap = overlap, crossings = 0))
  event_id <- list(); event_x <- list(); event_y <- list()
  bx <- other$xend - other$x; by <- other$yend - other$y
  x0 <- e$x[1L]; y0 <- e$y[1L]; x1 <- tail(e$xend, 1L); y1 <- tail(e$yend, 1L)
  for (i in seq_len(nrow(runs))) {
    ax <- runs$xend[i] - runs$x[i]; ay <- runs$yend[i] - runs$y[i]
    denominator <- ax * by - ay * bx
    valid <- abs(denominator) > 1e-10
    denominator[!valid] <- 1
    t <- ((other$x - runs$x[i]) * by - (other$y - runs$y[i]) * bx) / denominator
    u <- ((other$x - runs$x[i]) * ay - (other$y - runs$y[i]) * ax) / denominator
    ids <- which(valid & t >= -1e-10 & t <= 1 + 1e-10 & u >= -1e-10 & u <= 1 + 1e-10)
    if (!length(ids)) next
    px <- runs$x[i] + t[ids] * ax; py <- runs$y[i] + t[ids] * ay
    terminal <- (abs(px - x0) + abs(py - y0) < 1e-8) | (abs(px - x1) + abs(py - y1) < 1e-8)
    other_terminal <- (abs(px - other$source_x[ids]) + abs(py - other$source_y[ids]) < 1e-8) |
      (abs(px - other$target_x[ids]) + abs(py - other$target_y[ids]) < 1e-8)
    keep <- !(terminal & other_terminal)
    event_id[[i]] <- other$edge_id[ids[keep]]
    event_x[[i]] <- round(px[keep], 8); event_y[[i]] <- round(py[keep], 8)
  }
  events <- paste(unlist(event_id, use.names = FALSE), unlist(event_x, use.names = FALSE),
    unlist(event_y, use.names = FALSE))
  c(overlap = overlap, crossings = length(unique(events)))
}

.reference_clear <- function(e, nodes, clearance = 0.8) {
  nodes <- nodes[!nodes$node %in% c(e$from[1L], e$to[1L]), , drop = FALSE]
  runs <- .route_runs(e)
  m <- nrow(nodes); k <- nrow(runs)
  if (!m || !k) return(TRUE)
  !any(.point_segment_distance(rep.int(nodes$x, k), rep.int(nodes$y, k),
    rep(runs$x, each = m), rep(runs$y, each = m),
    rep(runs$xend, each = m), rep(runs$yend, each = m)) < clearance)
}

test_that("route candidates match the per-candidate grammar exactly", {
  set.seed(11)
  for (trial in 1:40) for (e in .random_scoring_routes(2L, half = trial %% 4L == 0L)) {
    for (lanes in c(FALSE, TRUE)) {
      reference <- .reference_simple_candidates(e, lanes)
      expect_identical(.simple_route_candidates(e, lanes), reference)
      stack <- .simple_route_stack(e, lanes)
      expect_identical(stack, .stack_routes(reference))
      expect_identical(lapply(seq_along(reference), .stacked_route, stack = stack), reference)
    }
  }
})

test_that("stacked clearance and conflict scores equal one-at-a-time scores", {
  set.seed(12)
  for (trial in 1:40) {
    routes <- .random_scoring_routes(sample(2:8, 1L), half = trial %% 5L == 0L)
    e <- routes[[1L]]
    obstacles <- .prepare_route_obstacles(do.call(rbind, routes[-1L]))
    candidates <- c(list(e), .simple_route_candidates(e))
    m <- sample(1:10, 1L)
    nodes <- data.frame(node = paste0("v", seq_len(m)),
      x = sample(-8:8, m, TRUE) + sample(c(0, 0.5), m, TRUE), y = sample(-6:10, m, TRUE))
    clearance <- if (trial %% 2L) 0.8 else stats::runif(m, 0.5, 2)
    stack <- .stack_routes(candidates)
    ids <- sort(sample(seq_along(candidates), sample(seq_along(candidates), 1L)))
    expect_identical(.stacked_clear(stack, ids, nodes, clearance), vapply(candidates[ids],
      function(z) .route_obstacle_free(.route_runs(z), nodes, clearance), logical(1)))
    scores <- .stacked_conflicts(stack, ids, obstacles)
    reference <- vapply(candidates[ids], .reference_conflicts, numeric(2), obstacles = obstacles)
    expect_identical(scores$overlap, unname(reference["overlap", ]))
    expect_identical(scores$crossings, unname(reference["crossings", ]))
    expect_identical(lapply(candidates, .route_conflicts, obstacles = obstacles),
      lapply(candidates, .reference_conflicts, obstacles = obstacles))
  }
})

test_that("clearance of all routes at once equals per-route tests", {
  set.seed(13)
  for (trial in 1:30) {
    routes <- .random_scoring_routes(sample(2:8, 1L))
    ends <- unique(c(vapply(routes, function(e) e$from[1L], ""), vapply(routes, function(e) e$to[1L], "")))
    nodes <- data.frame(node = c(ends, paste0("v", 1:4)),
      x = sample(-8:8, length(ends) + 4L, TRUE), y = sample(-6:10, length(ends) + 4L, TRUE))
    nodes <- nodes[sample(nrow(nodes), nrow(nodes) - trial %% 2L), ]
    expect_identical(.routes_clear(routes, nodes),
      vapply(routes, .reference_clear, logical(1), nodes = nodes))
    margins <- lapply(routes, function(e) .route_clearances(.route_runs(e),
      nodes[!nodes$node %in% c(e$from[1L], e$to[1L]), , drop = FALSE]))
    expect_identical(.routes_clear(routes, nodes, margins), vapply(seq_along(routes),
      function(i) .reference_clear(routes[[i]], nodes, margins[[i]]), logical(1)))
  }
})

test_that("an unflagged default source pass equals the crossings-first pass", {
  layout <- local({
    old <- options(metrodag.control = list(stages = c(half_grid = FALSE)))
    on.exit(options(old))
    layout_metro_dag(metrodag_examples("causal"), direction = "down")
  })
  nodes <- data.frame(node = layout$nodes$node, x = layout$nodes$x, y = layout$nodes$y)
  routes <- unname(split(layout$edges, layout$edges$edge_id))
  tracker <- new.env(parent = emptyenv()); tracker$differs <- FALSE
  default <- .relax_source_pass(nodes, routes, alternative = tracker)
  alternative <- .relax_source_pass(nodes, routes, crossings_first = TRUE)
  # Otherwise the alternative pass runs and may or may not differ.
  if (!tracker$differs) expect_identical(alternative, default) else succeed()
  # Rerouting single moved stations: the tracked choice is the crossings-first one.
  from <- match(vapply(routes, function(e) e$from[1L], ""), nodes$node)
  to <- match(vapply(routes, function(e) e$to[1L], ""), nodes$node)
  for (i in seq_len(nrow(nodes))) for (shift in c(-2, 2)) {
    moved <- nodes; moved$x[i] <- moved$x[i] + shift
    affected <- which(from == i | to == i)
    if (!length(affected)) next
    flag <- new.env(parent = emptyenv()); flag$differs <- FALSE
    a <- .reroute_moved_nodes(routes, moved, affected, from, to, alternative = flag)
    expect_identical(a, .reroute_moved_nodes(routes, moved, affected, from, to))
    if (!flag$differs) expect_identical(
      .reroute_moved_nodes(routes, moved, affected, from, to, crossings_first = TRUE), a)
  }
})

test_that("the default source pass flags where crossings-first chooses another route", {
  # Laying out gallery DAG 1, the crossings-first pass returns another drawing,
  # so the default pass must have flagged it; were it unflagged, that pass
  # would be skipped and the layout would change.
  captured <- NULL
  relax <- .relax_source_positions
  testthat::local_mocked_bindings(.relax_source_positions = function(nodes, routes, ...) {
    if (is.null(captured)) captured <<- list(nodes = nodes, routes = routes, args = list(...))
    relax(nodes, routes, ...)
  }, .package = "metrodag")
  layout_metro_dag(.test_gallery_graph(1L))
  expect_false(is.null(captured))
  tracker <- new.env(parent = emptyenv()); tracker$differs <- FALSE
  default <- do.call(.relax_source_pass, c(list(captured$nodes, captured$routes,
    alternative = tracker), captured$args))
  alternative <- do.call(.relax_source_pass, c(list(captured$nodes, captured$routes,
    crossings_first = TRUE), captured$args))
  expect_false(identical(alternative, default))
  expect_true(tracker$differs)
})

.reference_pair_candidates <- function(e, nodes, other, max_candidates, fixed = FALSE) {
  obstacles <- .prepare_route_obstacles(other)
  keys <- .route_grid_keys(other)
  unique_keys <- unique(keys)
  counts <- tabulate(match(keys, unique_keys))
  length_of <- function(z) sqrt((z$xend - z$x)^2 + (z$yend - z$y)^2)
  quality <- function(z) {
    conflicts <- .reference_conflicts(z, obstacles)
    hits <- match(.route_grid_keys(z), unique_keys)
    shared <- sum(length_of(z)[!is.na(hits)] * counts[hits[!is.na(hits)]])
    c(crossings = unname(conflicts["crossings"]), shared = shared,
      bends = sum(diff(z$xend - z$x) != 0), length = sum(length_of(z)))
  }
  limit <- quality(e)
  candidates <- c(list(e), if (!fixed) .reference_simple_candidates(e))
  seen <- character()
  accepted <- list()
  scores <- list()
  for (z in candidates) {
    key <- paste(z$xend - z$x, collapse = ",")
    if (key %in% seen) next
    seen <- c(seen, key)
    if (!.reference_clear(transform(z, from = e$from[1L], to = e$to[1L]), nodes)) next
    score <- quality(z)
    if (any(score[c("crossings", "shared", "length")] >
        limit[c("crossings", "shared", "length")] + 1e-8) ||
        score["bends"] > limit["bends"] + 1L) next
    for (field in c("from", "to", "edge_id", "path_id")) z[[field]] <- e[[field]][1L]
    z$segment <- seq_len(nrow(z))
    z$bend <- c(FALSE, diff(z$xend - z$x) != 0)
    z <- z[, names(e)]
    accepted[[length(accepted) + 1L]] <- list(edge = z, score = score,
      obstacles = .prepare_route_obstacles(z))
    scores[[length(scores) + 1L]] <- score
  }
  if (!length(accepted)) return(accepted)
  scores <- do.call(rbind, scores)
  # Floating lengths tie within 1e-8, as in .pair_route_candidates(), so the
  # order is the same on platforms without long double.
  priority <- order(scores[, "crossings"], .tolerance_rank(scores[, "shared"]),
    scores[, "bends"], .tolerance_rank(scores[, "length"]), seq_len(nrow(scores)), method = "radix")
  accepted[head(priority, max_candidates)]
}

test_that("pair-move candidates are scored and ranked as one at a time", {
  set.seed(14)
  for (trial in 1:40) {
    routes <- .random_scoring_routes(sample(2:8, 1L), half = trial %% 5L == 0L)
    e <- routes[[1L]]
    other <- do.call(rbind, routes[-1L])
    m <- sample(0:8, 1L)
    nodes <- data.frame(node = c(e$from[1L], e$to[1L], sprintf("v%d", seq_len(m))),
      x = c(e$x[1L], e$xend[nrow(e)], sample(-8:8, m, TRUE)),
      y = c(e$y[1L], e$yend[nrow(e)], sample(-6:10, m, TRUE)))
    fixed <- trial %% 7L == 0L
    max_candidates <- sample(c(1L, 3L, 24L), 1L)
    expect_identical(.pair_route_candidates(e, nodes, other, max_candidates, fixed),
      .reference_pair_candidates(e, nodes, other, max_candidates, fixed))
  }
})

test_that("section candidates leave no symbols behind", {
  # R never frees a symbol. Section candidates were once deduplicated by
  # naming entries of an environment after them, which installs every key as a
  # symbol: one large layout left tens of thousands behind (50 MB), and later
  # layouts in the same R session ran much slower.
  route <- function(h, x) {
    .scoring_route(rep(c(1, 1, 0, -1, -1, 0, 1, 0), length.out = h), x, 0, 1L)
  }
  for (x in c(0, 0.5)) invisible(.section_route_candidates(route(6L, x)))
  # Off-grid routes take the general path; a new length gives new move keys.
  for (x in c(0, 0.5)) {
    e <- route(if (x == 0) 29L else 31L, x)
    before <- memory.profile()[["symbol"]]
    candidates <- .section_route_candidates(e)
    added <- memory.profile()[["symbol"]] - before
    expect_gt(length(candidates), 100L)
    expect_lt(added, 10L)
  }
})
