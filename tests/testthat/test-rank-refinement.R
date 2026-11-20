# Gallery DAG 4: statin therapy and vascular events.
.test_rank_graph <- function() .test_gallery_graph(4L)

.test_before_rank_refinement <- function(graph, ...) {
  .test_layout_without(graph, c(".refine_rank_assignment", ".refine_layering_variants",
    .test_late_stages), ...)
}

test_that("automatic ranks bring distant parents closer with bounded geometric tradeoffs", {
  .test_without_stages(c(".refine_layering_variants", .test_late_stages))
  graph <- .test_rank_graph()
  before <- .test_before_rank_refinement(graph)
  after <- layout_metro_dag(direction = "down", graph)
  ranks <- stats::setNames(after$nodes$layer, after$nodes$node)
  expect_identical(after$graph, graph)
  expect_identical(after$nodes$node, before$nodes$node)
  expect_equal(before$nodes$layer[before$nodes$node == "S"], 0L)
  expect_equal(ranks[["S"]], ranks[["O"]] - 1L)
  expect_lt(layout_metrics(after)$area, layout_metrics(before)$area * 0.65)
  expect_equal(ranks[["T"]], ranks[["A"]] - 1L)
  expect_equal(layout_metrics(after)$total_bends, 15L)
  # Four contact points, one of them a fork that is not a crossing.
  expect_equal(.count_route_contacts(after$edges), 4L)
  expect_equal(layout_metrics(after)$crossings, 3L)
  expect_equal(layout_metrics(after)$node_edge_collisions, 0L)
  expect_equal(after$parameters$layer_gap, diff(sort(unique(-after$nodes$y))))
  expect_equal(after$nodes$label_x, after$nodes$x)
  expect_equal(after$nodes$label_y, after$nodes$y)
  expect_gte(min(as.numeric(stats::dist(after$nodes[c("x", "y")]))), 4)
  old <- unname(split(before$edges, before$edges$edge_id))
  routes <- unname(split(after$edges, after$edges$edge_id))
  a <- .routed_geometry_quality(old); b <- .routed_geometry_quality(routes)
  expect_true(all(b[c("crossings", "bends")] <= a[c("crossings", "bends")]))
  expect_lte(b[["length"]], a[["length"]] * 1.05)
  shared_steps <- function(routes) {
    keys <- .route_grid_keys(do.call(rbind, routes))
    count <- tabulate(match(keys, unique(keys)))
    sum(count * (count - 1) / 2)
  }
  expect_lte(shared_steps(routes), shared_steps(old))
  for (id in seq_along(routes)) {
    e <- routes[[id]]
    for (field in c("from", "to", "edge_id"))
      expect_identical(e[[field]][1L], old[[id]][[field]][1L])
    expect_gt(ranks[[e$to[1L]]], ranks[[e$from[1L]]])
    expect_identical(e$segment, seq_len(nrow(e)))
    expect_true(all(e$yend == e$y - 1 & abs(e$xend - e$x) %in% 0:1))
    expect_equal(head(e$xend, -1L), tail(e$x, -1L))
    expect_equal(head(e$yend, -1L), tail(e$y, -1L))
    expect_equal(c(e$x[1L], e$y[1L]),
      unname(unlist(after$nodes[after$nodes$node == e$from[1L], c("x", "y")])))
    expect_equal(c(tail(e$xend, 1L), tail(e$yend, 1L)),
      unname(unlist(after$nodes[after$nodes$node == e$to[1L], c("x", "y")])))
    unrelated <- after$nodes[!after$nodes$node %in% c(e$from[1L], e$to[1L]), ]
    expect_true(.route_obstacle_free(.route_runs(e), unrelated))
    if (e$from[1L] == "I" && e$to[1L] == "Y")
      expect_gte(tail(rle(e$xend - e$x)$lengths, 1L), 3L)
  }
})

test_that("rank refinement remains deterministic across inputs and rotations", {
  graph <- .test_rank_graph()
  ends <- igraph::ends(graph, igraph::E(graph))
  reversed <- igraph::graph_from_edgelist(ends[nrow(ends):1L, ])
  geometry <- function(layout) {
    e <- layout$edges[order(layout$edges$from, layout$edges$to, layout$edges$segment),
      c("from", "to", "x", "y", "xend", "yend", "bend", "path_id")]
    rownames(e) <- NULL
    e
  }
  stations <- function(layout) {
    n <- layout$nodes[order(layout$nodes$node), ]
    rownames(n) <- NULL
    n
  }
  # Each layout of this graph takes several seconds, and direction is only a
  # final rotation (checked for every direction in test-layout.R): lay out the
  # reversed input facing down and compare its rotations with the original
  # input in the default direction.
  right <- layout_metro_dag(graph)
  down <- layout_metro_dag(reversed, direction = "down")
  expected <- .test_rotate_layout(down, "right")
  expect_equal(geometry(right), geometry(expected))
  expect_equal(stations(right), stations(expected))
  expect_equal(layout_metrics(right)$directional_consistency, 1)
  for (direction in .test_directions) {
    rotated <- .test_rotate_layout(down, direction)
    expect_equal(layout_metrics(rotated)$directional_consistency, 1)
    expect_equal(layout_metrics(rotated)[c("crossings", "total_bends", "area")],
      layout_metrics(right)[c("crossings", "total_bends", "area")])
  }
})

test_that("explicit rank ties bypass automatic rank refinement", {
  graph <- .test_rank_graph()
  ranks <- c(X = 1, P = 2, A = 3, O = 4, Y = 5, I = 0, R = 1, T = 0, S = 0, U = 0, G = 0)
  testthat::local_mocked_bindings(
    .refine_route_resolution = function(nodes, edges, ...) edges,
    .refine_rank_assignment = function(...) stop("Explicit ranks must skip this search"),
    .package = "metrodag")
  for (layering in c("longest", "sinks")) {
    a <- layout_metro_dag(direction = "down", graph, rank = ranks, layering = layering)
    expect_equal(a$nodes$layer, unname(ranks[a$nodes$node]))
    expect_equal(a$nodes$y[a$nodes$node == "S"], a$nodes$y[a$nodes$node == "I"])
  }
})

test_that("candidate ranks are legal for both source and sink aligned inputs", {
  for (method in c("longest", "sinks")) {
    graph <- if (method == "longest") .test_rank_graph() else
      igraph::graph_from_edgelist(rbind(c("A", "B"), c("B", "C"), c("C", "D"), c("A", "Y")))
    ends <- igraph::ends(graph, igraph::E(graph))
    original <- .assign_layers(graph, method)
    candidates <- .rank_assignment_candidates(graph, original)
    expect_gt(length(candidates), 0L)
    for (candidate in candidates) {
      expect_true(all(candidate[ends[, 1L]] < candidate[ends[, 2L]]))
      expect_lte(sum(candidate[ends[, 2L]] - candidate[ends[, 1L]]),
        sum(original[ends[, 2L]] - original[ends[, 1L]]))
    }
  }
  expect_length(.rank_assignment_candidates(metrodag_examples("chain"), 0:3), 0L)
  # Stopping the search must not call the component builder, even if the graph
  # otherwise has useful moves. Empty routes have the same fast return.
  sentinel <- list(edges = .empty_routes())
  graph <- .test_rank_graph()
  testthat::local_mocked_bindings(.layout_component = function(...) stop("Unexpected rebuild"),
    .package = "metrodag")
  expect_identical(.refine_rank_assignment(graph, sentinel, "barycenter", 8L), sentinel)
  sentinel$edges <- data.frame(edge_id = 1L)
  expect_identical(.refine_rank_assignment(graph, sentinel, "barycenter", 8L,
    max_candidates = 0L), sentinel)
  # A component whose step/edge work already exceeds the budget must skip.
  sentinel$edges <- data.frame(edge_id = rep(1L, 801L))
  expect_identical(.refine_rank_assignment(graph, sentinel, "barycenter", 8L), sentinel)
})

test_that("packing preserves refined ranks and original causal edge IDs", {
  graph <- .test_rank_graph()
  other <- metrodag_examples("diamond")
  igraph::V(other)$name <- paste0("other_", igraph::V(other)$name)
  graph <- igraph::disjoint_union(graph, other)
  a <- layout_metro_dag(direction = "down", graph)
  ends <- igraph::ends(graph, igraph::E(graph))
  ranks <- stats::setNames(a$nodes$layer, a$nodes$node)
  expect_true(a$parameters$component_packing)
  expect_equal(ranks[["S"]], ranks[["O"]] - 1L)
  expect_true(all(ranks[ends[, 1L]] < ranks[ends[, 2L]]))
  expect_equal(layout_metrics(a)$node_edge_collisions, 0L)
  first <- a$edges[!duplicated(a$edges$edge_id), ]
  expect_setequal(first$edge_id, seq_len(nrow(ends)))
  expect_equal(first$from, ends[first$edge_id, 1L])
  expect_equal(first$to, ends[first$edge_id, 2L])
})
