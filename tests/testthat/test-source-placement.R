# Frozen pre-refinement geometry of a converging-parent graph. Labels/roles play
# no part in source placement. This isolates the new stage from its initializers.
.test_source_geometry <- function() {
  nodes <- data.frame(node = c("W", "O", "Y", "A", "S", "C", "D", "U", "H"),
    x = c(6, -1, -1, -8, -4, -8, -12, 7, 3),
    y = c(-8, -15, -25, -8, -4, 0, -4, 0, 0))
  ends <- unname(.metro_gallery_examples[[3L]]$edges)  # gallery DAG 3
  moves <- list(rep(-1, 7), rep(c(0, -1, 0), c(9, 7, 1)), rep(0, 10), rep(1, 7),
    rep(c(1, 0), c(3, 8)), rep(0, 8), rep(1, 4), rep(-1, 4),
    rep(c(0, 1), c(10, 11)), rep(c(-1, 0, -1), c(7, 7, 1)),
    rep(c(0, -1), c(17, 8)), rep(c(0, -1, 0), c(10, 4, 1)),
    rep(c(-1, 0, 1), c(5, 19, 1)))
  routes <- lapply(seq_len(nrow(ends)), function(i) {
    n <- nodes[match(ends[i, 1L], nodes$node), ]
    e <- .route_from_moves(n$x, n$y, moves[[i]])
    e$from <- ends[i, 1L]; e$to <- ends[i, 2L]; e$edge_id <- i
    e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves[[i]]) != 0)
    e$path_id <- NA_integer_
    e[, names(.empty_routes())]
  })
  list(nodes = nodes, routes = routes)
}

test_that("source relaxation shortens converging branches without buying wiggles", {
  before <- .test_source_geometry()
  after <- .relax_source_positions(before$nodes, before$routes)
  a <- .routed_geometry_quality(before$routes); b <- .routed_geometry_quality(after$routes)
  expect_lte(b[["crossings"]], a[["crossings"]] - 2L)
  expect_lt(b[["bends"]], a[["bends"]])
  expect_lt(b[["length"]], a[["length"]] * 0.85)
  expect_lte(b[["shared"]], a[["shared"]] + 1e-8)
  reversals <- function(routes) sum(vapply(routes, function(e) {
    directions <- rle(e$xend - e$x)$values
    sum(diff(directions[directions != 0]) != 0)
  }, integer(1)))
  expect_lte(reversals(after$routes), reversals(before$routes))
  expect_identical(after$nodes$node, before$nodes$node)
  expect_gte(min(stats::dist(after$nodes[c("x", "y")])), 4)
  for (i in seq_along(after$routes)) {
    e <- after$routes[[i]]; old <- before$routes[[i]]
    expect_equal(e[1L, c("from", "to", "edge_id", "path_id")],
      old[1L, c("from", "to", "edge_id", "path_id")], ignore_attr = TRUE)
    expect_true(.route_obstacle_free(.route_runs(e), after$nodes[
      !after$nodes$node %in% c(e$from[1L], e$to[1L]), ]))
    if (!any(old$bend)) expect_false(any(e$bend))
  }
  # Reordering either table retains the chosen geometry and causal identities.
  reordered <- .relax_source_positions(before$nodes[9:1, ], before$routes[13:1])
  expect_equal(after$nodes[order(after$nodes$node), ],
    reordered$nodes[order(reordered$nodes$node), ], ignore_attr = TRUE)
  for (i in 1:13) expect_equal(after$routes[[i]], reordered$routes[[14L - i]])
})

test_that("source work budgets skip costly drawings and fixed constraints", {
  before <- .test_source_geometry()
  expect_identical(.relax_source_positions(before$nodes, before$routes, max_candidates = 0L), before)
  big <- rep(before$routes, 100L)
  expect_identical(.relax_source_positions(before$nodes, big)$routes, big)
  graph <- metrodag_examples("fork")
  calls <- logical()
  testthat::local_mocked_bindings(.relax_source_positions = function(nodes, routes, ...,
      transverse = FALSE) {
    calls <<- c(calls, transverse)
    list(nodes = nodes, routes = routes)
  }, .package = "metrodag")
  # Supplied ranks keep this pass, restricted to moves within each rank.
  layout_metro_dag(direction = "down", graph, rank = .assign_layers(graph))
  expect_identical(calls, TRUE)
  testthat::local_mocked_bindings(
    .relax_source_positions = function(...) stop("Source placement must be skipped"), .package = "metrodag")
  expect_no_error(layout_metro_dag(direction = "down", graph, ordering = "none"))
  expect_no_error(layout_metro_dag(direction = "down", graph, sweeps = 0L))
  chain <- metrodag_examples("chain")
  expect_no_error(layout_metro_dag(direction = "down", chain, align = list(igraph::V(chain)$name)))
})

test_that("transverse source relaxation keeps every node in its rank", {
  before <- .test_source_geometry()
  layer <- match(-before$nodes$y, sort(unique(-before$nodes$y))) - 1L
  after <- .relax_source_positions(before$nodes, before$routes, transverse = TRUE)
  expect_true(.ranks_kept(after$nodes$y, layer))
  expect_false(identical(after$nodes$x, before$nodes$x))
  a <- .routed_geometry_quality(before$routes); b <- .routed_geometry_quality(after$routes)
  expect_true(all(b <= a + 1e-8))
  expect_lt(b[["length"]], a[["length"]])
  # Unrestricted, the same pass moves sources across ranks.
  free <- .relax_source_positions(before$nodes, before$routes)
  expect_false(.ranks_kept(free$nodes$y, layer))
})

test_that("source refinement runs in canonical coordinates before every rotation", {
  graph <- metrodag_examples("long_edge")
  down <- layout_metro_dag(direction = "down", graph)
  for (direction in c("right", "left", "up")) {
    actual <- layout_metro_dag(graph, direction = direction)
    expected <- .rotate_geometry(down$nodes, down$edges, direction)
    expect_equal(actual$nodes, expected$nodes)
    expect_equal(actual$edges, expected$edges)
    expect_no_error(.assert_layout_geometry(actual))
  }
})
