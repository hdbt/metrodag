.test_straight_fan <- function() {
  # This fixture isolates fan placement from the later rank-gap compactor and
  # from the rank-preserving placement passes that supplied ranks also run.
  .test_without_stages(c(".refine_route_resolution", ".compact_route_gaps",
    ".lengthen_route_approaches", ".compact_node_arrangement"))
  old <- options(metrodag.control = list(stages = c(fan_straighten = FALSE,
    restart_variants = FALSE, source_relaxation = FALSE, intermediate_relaxation = FALSE)))
  on.exit(options(old), add = TRUE)
  graph <- metrodag_examples("causal")
  layout <- layout_metro_dag(direction = "down", graph, rank = c(C = 1, X = 2, Y = 4, M = 3, U = 0, S = 0))
  list(nodes = layout$nodes[c("node", "x", "y")],
    routes = unname(split(layout$edges, layout$edges$edge_id)))
}

test_that("automatic placement straightens the two outer arrows into a junction", {
  graph <- metrodag_examples("causal")
  # Laid out once and turned to every direction (test-layout.R).
  layouts <- .test_layouts_by_direction(graph)
  for (direction in c("down", "right", "up", "left")) {
    layout <- layouts[[direction]]
    for (source in c("C", "S")) {
      e <- layout$edges[layout$edges$from == source & layout$edges$to == "X", ]
      expect_false(any(e$bend))
      expect_equal(nrow(.collapse_routes(e)), 1L)
    }
    layers <- stats::setNames(layout$nodes$layer, layout$nodes$node)
    ends <- igraph::ends(graph, igraph::E(graph))
    expect_true(all(layers[ends[, 1L]] < layers[ends[, 2L]]))
    expect_equal(layers[["C"]], layers[["S"]])
    expect_equal(layout_metrics(layout)$total_bends, 4L)
    expect_lte(layout_metrics(layout)$total_edge_length, 38 + 25 * sqrt(2))
    expect_equal(layout$parameters$layer_gap, c(4, 4, 4, 6))
  }
})

test_that("fan straightening preserves topology, route identities and clear tracks", {
  fixture <- .test_straight_fan()
  original <- fixture
  refined <- .straighten_fans(fixture$nodes, fixture$routes)
  before <- .routed_geometry_quality(fixture$routes)
  after <- .routed_geometry_quality(refined$routes)
  expect_equal(unname(before["bends"]), 8L)
  expect_equal(unname(after["bends"]), 4L)
  expect_true(all(after <= before))
  expect_equal(unname(after[c("crossings", "shared")]), c(0, 0))
  expect_identical(fixture, original)
  for (i in seq_along(refined$routes)) {
    e <- refined$routes[[i]]
    for (field in c("from", "to", "edge_id", "path_id"))
      expect_identical(e[[field]][1L], fixture$routes[[i]][[field]][1L])
    source <- match(e$from[1L], refined$nodes$node)
    target <- match(e$to[1L], refined$nodes$node)
    expect_equal(c(e$x[1L], e$y[1L]), unname(unlist(refined$nodes[source, c("x", "y")])))
    expect_equal(c(tail(e$xend, 1L), tail(e$yend, 1L)),
      unname(unlist(refined$nodes[target, c("x", "y")])))
    expect_equal(e$segment, seq_len(nrow(e)))
    expect_true(all(e$yend == e$y - 1 & abs(e$xend - e$x) %in% 0:1))
    unrelated <- refined$nodes[-c(source, target), ]
    expect_true(.route_obstacle_free(.route_runs(e), unrelated))
  }
})

test_that("the preference also handles forks and is independent of names and row order", {
  fixture <- .test_straight_fan()
  expected <- .straighten_fans(fixture$nodes, fixture$routes)
  reversed <- .straighten_fans(fixture$nodes[6:1, ], rev(fixture$routes))
  expect_identical(rev(reversed$routes), expected$routes)
  expect_equal(reversed$nodes[6:1, ], expected$nodes)
  expect_identical(.straighten_fans(expected$nodes, expected$routes), expected)
  labels <- stats::setNames(paste("variable", seq_len(6)), fixture$nodes$node)
  fixture$nodes$node <- unname(labels[fixture$nodes$node])
  fixture$routes <- lapply(fixture$routes, function(e) {
    e$from <- unname(labels[e$from]); e$to <- unname(labels[e$to]); e
  })
  renamed <- .straighten_fans(fixture$nodes, fixture$routes)
  expect_equal(.routed_geometry_quality(renamed$routes), .routed_geometry_quality(expected$routes))
  # Reverse every arrow and reflect the depth axis: the same motif is a fork.
  fixture$nodes$y <- -fixture$nodes$y
  fixture$routes <- lapply(fixture$routes, function(e) {
    z <- e[nrow(e):1, ]
    z$x <- rev(e$xend); z$xend <- rev(e$x)
    z$y <- -rev(e$yend); z$yend <- -rev(e$y)
    z$from <- e$to[1L]; z$to <- e$from[1L]
    z$segment <- seq_len(nrow(z)); z$bend <- c(FALSE, diff(z$xend - z$x) != 0)
    z
  })
  fork <- .straighten_fans(fixture$nodes, fixture$routes)
  expect_equal(unname(.routed_geometry_quality(fork$routes)[c("crossings", "shared", "bends")]), c(0, 0, 4))
})

test_that("explicit user ranks retain their order and ties", {
  graph <- metrodag_examples("causal")
  ranks <- c(C = 1, X = 2, Y = 4, M = 3, U = 0, S = 0)
  for (method in c("longest", "sinks")) {
    layout <- layout_metro_dag(direction = "down", graph, rank = ranks, layering = method)
    expect_equal(stats::setNames(layout$nodes$layer, layout$nodes$node), ranks[layout$nodes$node])
    # Rank-gap compaction can remove bends while retaining these rank ties.
    expect_equal(layout_metrics(layout)$total_bends, 6L)
    expect_equal(layout$nodes$y[layout$nodes$node == "S"], layout$nodes$y[layout$nodes$node == "U"])
  }
})

test_that("with supplied ranks fan placement cannot move a branch out of its rank", {
  fixture <- .test_straight_fan()
  layer <- c(C = 1L, X = 2L, Y = 4L, M = 3L, U = 0L, S = 0L)[fixture$nodes$node]
  expect_true(.ranks_kept(fixture$nodes$y, layer))
  # Unconstrained, both outer parents of X move onto one rank (see above).
  free <- .straighten_fans(fixture$nodes, fixture$routes)
  expect_false(.ranks_kept(free$nodes$y, layer))
  ranked <- .straighten_fans(fixture$nodes, fixture$routes, layer = layer)
  expect_true(.ranks_kept(ranked$nodes$y, layer))
  expect_true(all(.routed_geometry_quality(ranked$routes) <=
    .routed_geometry_quality(fixture$routes) + 1e-8))
})

test_that("a mediator fan keeps its exposure-mediator-outcome spine on the station line", {
  # The half-grid pass returns the middle arrows to their straight track and
  # bends their siblings instead. At main 849f62b, k = 5 drew both spine arrows
  # half a unit off the station line with a hook at each end and 4 crossings,
  # and k = 9 drew them a unit off the line with 21 crossings.
  for (k in c(3L, 5L, 7L, 9L)) {
    mediators <- paste("Mediator", seq_len(k))
    fan <- data.frame(from = c("Pre", rep("Exposure", k), mediators),
      to = c("Exposure", mediators, rep("Outcome", k)))
    layout <- layout_metro_dag(fan)
    middle <- mediators[(k + 1L) / 2L]
    y <- layout$nodes$y[match(c("Exposure", middle, "Outcome"), layout$nodes$node)]
    expect_equal(y, rep(y[1L], 3L))
    for (pair in list(c("Exposure", middle), c(middle, "Outcome"))) {
      e <- layout$edges[layout$edges$from == pair[1L] & layout$edges$to == pair[2L], ]
      expect_false(any(e$bend))
      expect_equal(c(e$y, e$yend), rep(y[1L], 2L * nrow(e)))
    }
    if (k == 5L) expect_equal(layout_metrics(layout)$crossings, 0)
    if (k == 9L) expect_lt(layout_metrics(layout)$crossings, 21)
  }
})

test_that("blocked diagonals are rejected without changing the drawing", {
  nodes <- data.frame(node = c("L", "R", "J", "P", "Q"),
    x = c(-8, 8, 0, 4, -4), y = c(0, 0, -12, -4, -8))
  routes <- lapply(1:2, function(i) {
    moves <- if (i == 1L) c(rep(1, 8), rep(0, 4)) else c(rep(0, 4), rep(-1, 8))
    e <- .route_from_moves(nodes$x[i], nodes$y[i], moves)
    e$from <- nodes$node[i]; e$to <- "J"; e$edge_id <- i; e$path_id <- i
    e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves) != 0)
    e[, names(.empty_routes())]
  })
  for (e in routes) expect_true(.route_obstacle_free(.route_runs(e), nodes[4:5, ]))
  expected <- list(nodes = nodes, routes = routes)
  expect_identical(.straighten_fans(nodes, routes), expected)
  expect_lt(.routed_geometry_quality(.straighten_fans(nodes[1:3, ], routes)$routes)["bends"], 2)
})

test_that("search bounds and already straight arrows are respected", {
  fixture <- .test_straight_fan()
  expect_identical(.straighten_fans(fixture$nodes, fixture$routes, max_candidates = 0L), fixture)
  expect_identical(.straighten_fans(fixture$nodes, list()), list(nodes = fixture$nodes, routes = list()))
  expect_identical(.straighten_fans(fixture$nodes, fixture$routes[1]),
    list(nodes = fixture$nodes, routes = fixture$routes[1]))
  refined <- .straighten_fans(fixture$nodes, fixture$routes)
  straight <- !vapply(fixture$routes, function(e) any(e$bend), logical(1))
  expect_false(any(vapply(refined$routes[straight], function(e) any(e$bend), logical(1))))
})

test_that("component packing retains refined layers and original edge identities", {
  graph <- igraph::disjoint_union(
    igraph::delete_vertex_attr(metrodag_examples("causal"), "name"),
    igraph::delete_vertex_attr(metrodag_examples("diamond"), "name"))
  igraph::V(graph)$name <- paste0("node", seq_len(igraph::vcount(graph)))
  layout <- layout_metro_dag(direction = "down", graph)
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  expect_true(layout$parameters$component_packing)
  expect_true(all(layout$nodes$layer[ends[, 1L]] < layout$nodes$layer[ends[, 2L]]))
  expect_equal(layout$nodes$layer[1L], layout$nodes$layer[6L])
  expect_equal(layout_metrics(layout)$total_bends, 8L)
  expect_setequal(unique(layout$edges$edge_id), seq_len(igraph::ecount(graph)))
  expect_equal(layout_metrics(layout)$node_edge_collisions, 0L)
})
