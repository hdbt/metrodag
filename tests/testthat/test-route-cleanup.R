# Isolate route cleanup from later placement and attachment refinements.
.test_cleanup_stages <- c(".compact_route_gaps", ".lengthen_route_approaches",
  ".compact_node_arrangement", ".refine_rank_assignment", .test_late_stages)
.test_cleanup_layout <- function(graph, direction = "down", ...) {
  .test_layout_without(graph, .test_cleanup_stages, ..., direction = direction)
}
.test_before_cleanup <- function(graph, ...) {
  .test_without_stages(".simplify_route_sections")
  .test_cleanup_layout(graph, ...)
}

# Gallery DAGs 1 and 4 (the same graph objects on every call), and their
# layouts with and without cleanup. The stubs are set inside, so the stored
# layouts are the same wherever they are first computed.
.test_cleanup_graphs <- .test_memo(function(which) .test_gallery_graph(c(1L, 4L)[as.integer(which)]))
.test_cleanup_graph <- function(which = 1L) .test_cleanup_graphs(which)
.test_cleaned <- .test_memo(function(which) .test_cleanup_layout(.test_cleanup_graph(which)))
.test_uncleaned <- .test_memo(function(which) .test_before_cleanup(.test_cleanup_graph(which)))

test_that("annotated long arrows simplify even when two bends are insufficient", {
  for (which in 1:2) {
    before <- .test_uncleaned(which)
    layout <- .test_cleaned(which)
    from <- c("I", "U")[which]; to <- c("Y", "O")[which]
    a <- before$edges[before$edges$from == from & before$edges$to == to, ]
    b <- layout$edges[layout$edges$from == from & layout$edges$to == to, ]
    expect_equal(sum(a$bend), c(6L, 8L)[which])
    expect_equal(sum(b$bend), 3L)
    expect_identical(layout$nodes, before$nodes)
    expect_identical(layout$graph, before$graph)
    expect_identical(layout$parameters, before$parameters)
    original <- split(before$edges, before$edges$edge_id)
    routes <- split(layout$edges, layout$edges$edge_id)
    expect_true(all(.routed_geometry_quality(routes) <=
      .routed_geometry_quality(original) + 1e-8))
    expect_equal(layout_metrics(layout)$node_edge_collisions, 0L)
    expect_identical(.simplify_route_sections(routes, layout$nodes), routes)
    for (i in seq_along(routes)) {
      e <- routes[[i]]; old <- original[[i]]
      expect_identical(e[c("from", "to", "edge_id", "path_id", "segment")],
        old[c("from", "to", "edge_id", "path_id", "segment")])
      expect_equal(c(e$x[1L], e$y[1L], tail(e$xend, 1L), tail(e$yend, 1L)),
        c(old$x[1L], old$y[1L], tail(old$xend, 1L), tail(old$yend, 1L)))
      expect_true(all(e$yend == e$y - 1L & abs(e$xend - e$x) %in% 0:1))
      expect_equal(head(e$xend, -1L), tail(e$x, -1L))
      expect_equal(head(e$yend, -1L), tail(e$y, -1L))
      expect_identical(e$bend, c(FALSE, diff(e$xend - e$x) != 0))
      expect_true(.route_obstacle_free(.route_runs(e),
        layout$nodes[!layout$nodes$node %in% c(e$from[1L], e$to[1L]), ]))
    }
  }
})

test_that("cleanup is stable under route, node and graph input permutations", {
  graph <- .test_cleanup_graph()
  before <- .test_uncleaned(1L)
  routes <- unname(split(before$edges, before$edges$edge_id))
  original <- routes
  out <- .simplify_route_sections(routes, before$nodes)
  reversed <- .simplify_route_sections(rev(routes), before$nodes[nrow(before$nodes):1L, ])
  expect_identical(rev(reversed), out)
  expect_identical(routes, original)
  ends <- igraph::ends(graph, igraph::E(graph))
  reordered <- igraph::graph_from_edgelist(ends[nrow(ends):1L, ])
  geometry <- function(layout) {
    e <- layout$edges[order(layout$edges$from, layout$edges$to, layout$edges$segment),
      c("from", "to", "x", "y", "xend", "yend", "bend", "path_id")]
    rownames(e) <- NULL
    e
  }
  expect_equal(geometry(.test_cleanup_layout(reordered)), geometry(.test_cleaned(1L)))
})

test_that("rotations and explicit ranks retain the simplified routes", {
  graph <- .test_cleanup_graph()
  before <- .test_uncleaned(1L)
  ranks <- stats::setNames(before$nodes$layer, before$nodes$node)
  # Direction is a final rotation (test-layout.R): lay out facing down once.
  cleaned <- .test_cleanup_layout(graph, rank = ranks)
  uncleaned <- .test_before_cleanup(graph, rank = ranks)
  for (direction in c("down", "right", "up", "left")) {
    layout <- .test_rotate_layout(cleaned, direction)
    original <- .test_rotate_layout(uncleaned, direction)
    expect_identical(layout$nodes, original$nodes)
    expect_identical(layout$parameters, original$parameters)
    expect_equal(sum(layout$edges$bend[layout$edges$from == "I" & layout$edges$to == "Y"]), 3L)
    metrics <- layout_metrics(layout)
    expect_equal(metrics$directional_consistency, 1)
    expect_equal(metrics$node_edge_collisions, 0L)
    expect_lte(metrics$crossings, layout_metrics(original)$crossings)
  }
})

test_that("a shorter route cannot pass through an unrelated station", {
  before <- .test_uncleaned(1L)
  routes <- unname(split(before$edges, before$edges$edge_id))
  out <- .simplify_route_sections(routes, before$nodes)
  # Only I -> Y has the largest bend count. Block its improved upper run.
  candidate <- out[[2L]][c("x", "y", "xend", "yend")]
  node <- data.frame(node = "obstacle", x = 6, y = -10)
  expect_true(.route_obstacle_free(.route_runs(routes[[2L]]), node))
  expect_false(.route_obstacle_free(.route_runs(candidate), node))
  testthat::local_mocked_bindings(.section_route_candidates = function(...) list(candidate),
    .package = "metrodag")
  expect_identical(.simplify_route_sections(routes,
    rbind(before$nodes[c("node", "x", "y")], node), max_routes = 1L), routes)
})

test_that("search limits leave valid routes intact and bound the shortlist", {
  before <- .test_uncleaned(1L)
  routes <- unname(split(before$edges, before$edges$edge_id))
  for (limit in c("max_routes", "max_windows", "max_candidates")) {
    args <- c(list(routes = routes, nodes = before$nodes), stats::setNames(list(0L), limit))
    expect_identical(do.call(.simplify_route_sections, args), routes)
  }
  expect_identical(.simplify_route_sections(list(), before$nodes), list())
  straight <- routes[vapply(routes, function(e) sum(e$bend) < 2L, logical(1))]
  expect_identical(.simplify_route_sections(straight, before$nodes), straight)
  candidates <- .section_route_candidates(routes[[2L]], max_candidates = 3L)
  expect_length(candidates, 3L)
  for (e in candidates) {
    expect_lt(sum(diff(e$xend - e$x) != 0), sum(routes[[2L]]$bend))
    expect_equal(nrow(e), nrow(routes[[2L]]))
    expect_equal(c(e$x[1L], e$y[1L], tail(e$xend, 1L), tail(e$yend, 1L)), c(5, 0, 0, -25))
  }
})

test_that("cleanup retains room for visible station outlines", {
  moves <- c(1, 0, 1, 0, 1, 0, 0, -1, 0, -1, 0, -1)
  e <- .route_from_moves(0, 0, moves)
  e$from <- "A"; e$to <- "Y"; e$edge_id <- 1L; e$path_id <- 1L
  e$segment <- seq_along(moves); e$bend <- c(FALSE, diff(moves) != 0)
  e <- e[, names(.empty_routes())]
  nodes <- data.frame(node = c("A", "Y", "P"), x = 0, y = c(0, -12, -6))
  out <- .simplify_route_sections(list(e), nodes)[[1L]]
  expect_lt(sum(out$bend), sum(e$bend))
  # The shortest one-unit detour clears the mathematical point but leaves less
  # room for the visible station. Retain the old nearest distance, sqrt(8).
  expect_gte(min(.point_segment_distance(0, -6,
    out$x, out$y, out$xend, out$yend)), sqrt(8))
})

test_that("component packing preserves simplified routes and edge identities", {
  other <- metrodag_examples("diamond")
  igraph::V(other)$name <- paste0("other_", igraph::V(other)$name)
  graph <- igraph::disjoint_union(.test_cleanup_graph(), other)
  before <- .test_before_cleanup(graph)
  layout <- .test_cleanup_layout(graph)
  expect_true(layout$parameters$component_packing)
  expect_setequal(unique(layout$edges$edge_id), seq_len(igraph::ecount(graph)))
  expect_lt(sum(layout$edges$bend), sum(before$edges$bend))
  expect_equal(layout_metrics(layout)$node_edge_collisions, 0L)
  groups <- split(seq_len(nrow(layout$nodes)), igraph::components(graph)$membership)
  for (ids in groups) for (axis in c("x", "y")) {
    # Packing may translate a component when its routed extent gets smaller.
    expect_equal(layout$nodes[[axis]][ids] - layout$nodes[[axis]][ids[1L]],
      before$nodes[[axis]][ids] - before$nodes[[axis]][ids[1L]])
  }
  expect_equal(layout$nodes$layer, before$nodes$layer)
})
