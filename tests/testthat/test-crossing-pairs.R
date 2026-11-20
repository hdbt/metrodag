.test_crossing_pair_fixture <- function() {
  graph <- metrodag_examples("causal")
  nodes <- data.frame(node = igraph::V(graph)$name,
    x = c(6, 1, 1, 1, 4, -4), y = c(-4, -11, -21, -15, 0, 0))
  moves <- list(c(rep(-1, 5), 0, 0), c(rep(0, 12), rep(-1, 5)),
    rep(0, 4), rep(0, 6), c(1, 1, 0, 0), c(rep(0, 8), rep(-1, 3)),
    c(rep(0, 16), rep(1, 5)), c(1, 1, rep(0, 6), 1, 1, 1))
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  routes <- lapply(seq_along(moves), function(id) {
    u <- ends[id, 1L]; v <- ends[id, 2L]
    e <- .route_from_moves(nodes$x[u], nodes$y[u], moves[[id]])
    e$from <- nodes$node[u]; e$to <- nodes$node[v]
    e$edge_id <- id; e$path_id <- id
    e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves[[id]]) != 0)
    e[, names(.empty_routes())]
  })
  list(graph = graph, nodes = nodes, routes = routes)
}

test_that("joint routing escapes a crossing that single-arrow moves cannot remove", {
  fixture <- .test_crossing_pair_fixture()
  original <- fixture
  greedy <- .straighten_routes(fixture$routes, fixture$nodes)
  expect_equal(count_crossings(do.call(rbind, greedy)), 1L)
  routes <- .reroute_crossing_pairs(fixture$routes, fixture$nodes)
  layout <- structure(list(graph = fixture$graph, nodes = fixture$nodes,
    edges = do.call(rbind, routes), direction = "down"), class = "metrodag_layout")
  metrics <- layout_metrics(layout)
  expect_equal(metrics$crossings, 0L)
  expect_equal(metrics$total_bends, 8L)
  expect_equal(metrics$overlap_segments, 0L)
  expect_equal(metrics$node_edge_collisions, 0L)
  expect_equal(metrics$total_edge_length, 56 + 25 * sqrt(2))
  expect_identical(fixture, original)
  expect_identical(routes[-c(1, 6)], fixture$routes[-c(1, 6)])
  for (i in seq_along(routes)) {
    e <- routes[[i]]; before <- fixture$routes[[i]]
    expect_identical(e[c("from", "to", "edge_id", "path_id", "segment")],
      before[c("from", "to", "edge_id", "path_id", "segment")])
    expect_equal(c(e$x[1], e$y[1], tail(e$xend, 1), tail(e$yend, 1)),
      c(before$x[1], before$y[1], tail(before$xend, 1), tail(before$yend, 1)))
    expect_true(all(e$yend == e$y - 1))
    expect_true(all(abs(e$xend - e$x) %in% 0:1))
  }
})

test_that("joint routing preserves identities and does not depend on list order", {
  fixture <- .test_crossing_pair_fixture()
  expected <- .reroute_crossing_pairs(fixture$routes, fixture$nodes)
  reversed <- .reroute_crossing_pairs(rev(fixture$routes),
    fixture$nodes[nrow(fixture$nodes):1, ])
  expect_identical(rev(reversed), expected)
  expect_identical(.reroute_crossing_pairs(expected, fixture$nodes), expected)
  labels <- stats::setNames(paste("variable", seq_len(nrow(fixture$nodes))), fixture$nodes$node)
  fixture$nodes$node <- unname(labels[fixture$nodes$node])
  fixture$routes <- lapply(fixture$routes, function(e) {
    e$from <- unname(labels[e$from]); e$to <- unname(labels[e$to]); e
  })
  renamed <- .reroute_crossing_pairs(fixture$routes, fixture$nodes)
  expect_equal(count_crossings(do.call(rbind, renamed)), 0L)
  expect_equal(.routed_geometry_quality(renamed)["shared"], c(shared = 0))
})

test_that("joint routing cannot move the crossing onto a third arrow", {
  fixture <- .test_crossing_pair_fixture()
  # This arrow crosses the proposed straight U -> X track between its ends.
  e <- .route_from_moves(0, -5, c(1, 1))
  e$from <- "P"; e$to <- "Q"; e$edge_id <- 9L; e$path_id <- NA_integer_
  e$segment <- 1:2; e$bend <- FALSE
  fixture$routes[[9L]] <- e[, names(.empty_routes())]
  fixture$nodes <- rbind(fixture$nodes,
    data.frame(node = c("P", "Q"), x = c(0, 2), y = c(-5, -7)))
  routes <- .reroute_crossing_pairs(fixture$routes, fixture$nodes)
  expect_identical(routes, fixture$routes)
  expect_equal(count_crossings(do.call(rbind, routes)), 1L)
})

test_that("joint routing keeps a crossing when a station blocks the alternative", {
  fixture <- .test_crossing_pair_fixture()
  fixture$nodes <- rbind(fixture$nodes, data.frame(node = "obstacle", x = 1, y = -4))
  routes <- .reroute_crossing_pairs(fixture$routes, fixture$nodes)
  expect_identical(routes, fixture$routes)
})

test_that("bounded pair refinement leaves clear and unavoidable routes alone", {
  fixture <- .test_crossing_pair_fixture()
  expect_identical(.reroute_crossing_pairs(fixture$routes, fixture$nodes, max_pairs = 0L),
    fixture$routes)
  clear <- fixture$routes[-6L]
  expect_identical(.reroute_crossing_pairs(clear, fixture$nodes), clear)
  expect_identical(.reroute_crossing_pairs(list(), fixture$nodes), list())
  expect_identical(.reroute_crossing_pairs(clear[1], fixture$nodes), clear[1])
  a <- .route_from_moves(0, 0, c(1, 1))
  b <- .route_from_moves(2, 0, c(-1, -1))
  routes <- lapply(seq_len(2L), function(i) {
    e <- list(a, b)[[i]]
    e$from <- c("A", "C")[i]; e$to <- c("B", "D")[i]
    e$edge_id <- i; e$path_id <- i; e$segment <- 1:2; e$bend <- FALSE
    e[, names(.empty_routes())]
  })
  nodes <- data.frame(node = c("A", "B", "C", "D"),
    x = c(0, 2, 2, 0), y = c(0, -2, 0, -2))
  expect_identical(.reroute_crossing_pairs(routes, nodes), routes)
  expect_equal(count_crossings(do.call(rbind, routes)), 1L)
})

test_that("shared-track safeguards count every pair of arrows", {
  fixture <- .test_crossing_pair_fixture()
  routes <- lapply(1:3, function(i) {
    e <- fixture$routes[[3L]]; e$edge_id <- i; e
  })
  expect_equal(.routed_geometry_quality(routes)["shared"], c(shared = 12))
})
