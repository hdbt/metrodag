# Frozen coarse-grid converging routes isolate the final refinement from node
# placement. No labels or analysis roles enter this geometric search.
.test_resolution_geometry <- function() {
  nodes <- data.frame(node = c("I", "X", "Y", "H", "D", "S", "M"),
    x = c(-4, 0, 0, 4, 0, 4, 0), y = c(0, -6, -17, -2, -2, -10, -10))
  ends <- unname(.metro_gallery_examples[[1L]]$edges)  # gallery DAG 1
  moves <- list(rep(c(1, 0, 1), 1:3), rep(c(0, 1), c(13, 4)), rep(-1, 4),
    rep(c(1, 0, -1), c(1, 9, 5)), rep(0, 4), rep(c(1, 0), c(4, 4)),
    rep(0, 4), rep(0, 7), rep(1, 4), rep(c(0, -1, 0), c(2, 4, 1)),
    rep(c(-1, 0, 1), c(1, 9, 1)))
  routes <- lapply(seq_len(nrow(ends)), function(i) {
    n <- nodes[match(ends[i, 1L], nodes$node), ]
    e <- .route_from_moves(n$x, n$y, moves[[i]])
    e$from <- ends[i, 1L]; e$to <- ends[i, 2L]; e$edge_id <- i
    e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves[[i]]) != 0)
    e$path_id <- NA_integer_
    e[, names(.empty_routes())]
  })
  list(nodes = nodes, edges = do.call(rbind, routes),
    graph = igraph::graph_from_edgelist(ends), direction = "down")
}

test_that("half-grid routing shortens shared tracks while retaining coarse geometry guards", {
  fixture <- .test_resolution_geometry(); original <- fixture
  before <- structure(fixture, class = "metrodag_layout")
  after <- before; after$edges <- .refine_route_resolution(before$nodes, before$edges)
  a <- layout_metrics(before); b <- layout_metrics(after)
  expect_equal(a$shared_track_length - b$shared_track_length, 0.5)
  for (field in c("crossings", "total_bends", "total_edge_length", "area", "overlap_segments"))
    expect_lte(b[[field]], a[[field]] + 1e-8)
  expect_equal(b$node_edge_collisions, 0L)
  expect_equal(b$directional_consistency, 1)
  expect_identical(fixture, original)
  old <- split(before$edges, before$edges$edge_id)
  for (e in split(after$edges, after$edges$edge_id)) {
    previous <- old[[as.character(e$edge_id[1L])]]
    if (!any(previous$bend)) expect_equal(e, previous, ignore_attr = TRUE)
    for (field in c("from", "to", "edge_id", "path_id"))
      expect_identical(e[[field]][1L], previous[[field]][1L])
    expect_equal(head(e$xend, -1L), tail(e$x, -1L))
    expect_equal(head(e$yend, -1L), tail(e$y, -1L))
    dy <- e$y - e$yend; dx <- abs(e$xend - e$x)
    expect_true(all(dy %in% c(0.5, 1) & (dx == 0 | dx == dy)))
    expect_identical(e$segment, seq_len(nrow(e)))
    expect_true(.route_obstacle_free(.collapse_routes(e), before$nodes[
      !before$nodes$node %in% c(e$from[1L], e$to[1L]), ], clearance = 0.8))
  }
  expect_no_error(.assert_layout_geometry(after))
})

# Frozen coarse routes of a five-mediator fan, as unit-grid routing leaves
# them: X -> M2 and M2 -> Y claim the centre line first, so X -> M3 and
# M3 -> Y detour on lane 1 with a bend at each end.
.test_spine_geometry <- function() {
  nodes <- data.frame(node = c("P", "X", "M1", "M2", "M3", "M4", "M5", "Y"),
    x = c(0, 0, -8, -4, 0, 4, 8, 0), y = c(0, -4, -14, -14, -14, -14, -14, -24))
  ends <- matrix(strsplit(paste("P X X M1 X M2 X M3 X M4 X M5",
    "M1 Y M2 Y M3 Y M4 Y M5 Y"), " ")[[1L]], ncol = 2L, byrow = TRUE)
  moves <- list(rep(0, 4), rep(c(-1, 0), c(8, 2)), rep(c(0, -1), c(6, 4)),
    rep(c(1, 0, -1), c(1, 8, 1)), rep(c(1, 0), c(4, 6)), rep(c(0, 1, 0), c(1, 8, 1)),
    rep(c(0, 1), c(2, 8)), rep(c(1, 0), c(4, 6)), rep(c(1, 0, -1), c(1, 8, 1)),
    rep(c(0, -1), c(6, 4)), rep(c(0, -1, 0), c(1, 8, 1)))
  routes <- lapply(seq_len(nrow(ends)), function(i) {
    n <- nodes[match(ends[i, 1L], nodes$node), ]
    e <- .route_from_moves(n$x, n$y, moves[[i]])
    e$from <- ends[i, 1L]; e$to <- ends[i, 2L]; e$edge_id <- i
    e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves[[i]]) != 0)
    e$path_id <- NA_integer_
    e[, names(.empty_routes())]
  })
  list(nodes = nodes, edges = do.call(rbind, routes))
}

test_that("half-grid routing returns a displaced spine to its straight track", {
  f <- .test_spine_geometry()
  after <- .refine_route_resolution(f$nodes, f$edges)
  for (pair in list(c("X", "M3"), c("M3", "Y"))) {
    e <- after[after$from == pair[1L] & after$to == pair[2L], ]
    expect_false(any(e$bend))
    expect_equal(c(e$x, e$xend), rep(0, 2L * nrow(e)))
  }
  a <- .routed_geometry_quality(unname(split(f$edges, f$edges$edge_id)))
  b <- .routed_geometry_quality(unname(split(after, after$edge_id)))
  expect_lt(b[["crossings"]], a[["crossings"]])
  expect_lt(b[["bends"]], a[["bends"]])
  expect_lte(b[["shared"]], a[["shared"]] + 1e-8)
  expect_lte(b[["length"]], a[["length"]] + 1e-8)
  for (e in split(after, after$edge_id)) {
    expect_equal(head(e$xend, -1L), tail(e$x, -1L))
    expect_equal(head(e$yend, -1L), tail(e$y, -1L))
    expect_true(.route_obstacle_free(.route_runs(e), f$nodes[
      !f$nodes$node %in% c(e$from[1L], e$to[1L]), ], clearance = 0.8))
  }
  # A station on the straight track keeps the detour.
  blocked <- rbind(f$nodes, data.frame(node = "Z", x = 0, y = -9))
  routes <- unname(split(f$edges, f$edges$edge_id))
  expect_null(.restore_straight_tracks(routes[c(1L, 4L)], blocked))
  restored <- .restore_straight_tracks(routes, f$nodes)
  expect_equal(restored$restored, c(4L, 9L))
})

test_that("resolution refinement is bounded and deterministic across reordered inputs", {
  f <- .test_resolution_geometry()
  expect_identical(.refine_route_resolution(f$nodes, f$edges, max_work = 0L), f$edges)
  expect_identical(.refine_route_resolution(f$nodes, f$edges, max_work = 1L), f$edges)
  expect_identical(.refine_route_resolution(f$nodes, .empty_routes()), .empty_routes())
  expected <- .refine_route_resolution(f$nodes, f$edges)
  reversed <- f$edges
  reversed$edge_id <- 12L - reversed$edge_id
  reversed <- reversed[order(reversed$edge_id, reversed$segment), ]
  actual <- .refine_route_resolution(f$nodes[7:1, ], reversed)
  normalize <- function(e) {
    e <- e[order(e$from, e$to, e$segment), setdiff(names(e), "edge_id")]
    rownames(e) <- NULL; e
  }
  expect_identical(normalize(actual), normalize(expected))
  straight <- f$edges[!f$edges$edge_id %in% unique(f$edges$edge_id[f$edges$bend]), ]
  expect_identical(.refine_route_resolution(f$nodes, straight), straight)
})

test_that("the public final refinement preserves fixed ranks, alignment and all rotations", {
  graph <- igraph::graph_from_edgelist(matrix(strsplit(
    "A B A C A D B D B E C E D F E F", " ")[[1L]], ncol = 2L, byrow = TRUE))
  ranks <- c(A = 0, B = 1, C = 1, D = 2, E = 2, F = 3)
  align <- list(c("A", "B", "D", "F"))
  down <- layout_metro_dag(direction = "down", graph, rank = ranks, align = align)
  expect_equal(down$nodes$layer, unname(ranks[down$nodes$node]))
  expect_equal(length(unique(down$nodes$x[down$nodes$node %in% align[[1L]]])), 1L)
  for (direction in c("down", "right", "up", "left")) {
    actual <- layout_metro_dag(graph, rank = ranks, align = align, direction = direction)
    expected <- .rotate_geometry(down$nodes, down$edges, direction)
    expect_equal(actual$nodes, expected$nodes)
    expect_equal(actual$edges, expected$edges)
    expect_no_error(.assert_layout_geometry(actual))
    expect_equal(layout_metrics(actual)$directional_consistency, 1)
    expect_identical(actual$graph, graph)
  }
})

test_that("the half-grid budget covers DAGitty's Sebastiani example", {
  # Sebastiani (60 arrows, 2 * 164340 half-grid work units) skipped this pass;
  # running it removes 9 bends and over half of the shared track for about 7 s.
  budget <- 0
  testthat::local_mocked_bindings(.refine_route_resolution = function(nodes, edges, max_work = 12000L) {
    budget <<- max(budget, max_work)
    edges
  }, .package = "metrodag")
  layout_metro_dag(metrodag_examples("diamond"))
  expect_gte(budget, 2 * 164340)
})

test_that("the straight-track attempt has its own, smaller work bound", {
  # It roughly doubles the half-grid pass, so drawings between its bound and
  # the pass's own budget get the plain search only.
  old <- options(metrodag.control = list(stages = c(half_grid = FALSE)))
  on.exit(options(old))
  layout <- layout_metro_dag(metrodag_examples("dense"), direction = "down")
  expect_true(any(layout$edges$bend))
  tried <- FALSE
  testthat::local_mocked_bindings(.restore_straight_tracks = function(...) {
    tried <<- TRUE
    NULL
  }, .package = "metrodag")
  work <- 2 * nrow(layout$edges) * length(unique(layout$edges$edge_id))
  .refine_route_resolution(layout$nodes, layout$edges, max_work = work, spine_work = work - 1)
  expect_false(tried)
  .refine_route_resolution(layout$nodes, layout$edges, max_work = work, spine_work = work)
  expect_true(tried)
})
