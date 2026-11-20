# Gallery DAGs 1 and 3.
.test_arrangement_graph <- function(which = 1L) .test_gallery_graph(c(1L, 3L)[which])

.test_before_arrangement <- function(graph, ...) {
  .test_layout_without(graph, c(".compact_node_arrangement", ".refine_rank_assignment",
    .test_late_stages), ...)
}

test_that("moving branches compacts the spacious review DAGs without geometric regressions", {
  .test_without_stages(c(".refine_rank_assignment", .test_late_stages))
  for (which in 1:2) {
    graph <- .test_arrangement_graph(which)
    before <- .test_before_arrangement(graph)
    after <- layout_metro_dag(direction = "down", graph)
    expect_identical(after$graph, before$graph)
    expect_identical(after$nodes[c("node", "layer", "order", "path")],
      before$nodes[c("node", "layer", "order", "path")])
    expect_lt(layout_metrics(after)$area, layout_metrics(before)$area * c(0.9, 0.75)[which])
    expect_lt(diff(range(after$nodes$x)), diff(range(before$nodes$x)))
    expect_lt(diff(range(after$nodes$y)), diff(range(before$nodes$y)))
    expect_equal(layout_metrics(after)$total_bends, c(13L, 11L)[which])
    expect_equal(layout_metrics(after)$node_edge_collisions, 0L)
    expect_equal(after$parameters$layer_gap, diff(sort(unique(-after$nodes$y))))
    expect_equal(after$nodes$label_x, after$nodes$x)
    expect_equal(after$nodes$label_y, after$nodes$y)
    expect_gte(min(as.numeric(stats::dist(after$nodes[c("x", "y")]))), 4)
    old <- unname(split(before$edges, before$edges$edge_id))
    routes <- unname(split(after$edges, after$edges$edge_id))
    expect_true(all(.routed_geometry_quality(routes) <= .routed_geometry_quality(old) + 1e-8))
    for (level in unique(before$nodes$layer)) {
      ids <- which(before$nodes$layer == level)
      expect_identical(order(before$nodes$x[ids]), order(after$nodes$x[ids]))
    }
    for (id in seq_along(routes)) {
      e <- routes[[id]]
      for (field in c("from", "to", "edge_id", "path_id"))
        expect_identical(e[[field]][1L], old[[id]][[field]][1L])
      expect_identical(e$segment, seq_len(nrow(e)))
      expect_true(all(e$yend == e$y - 1 & abs(e$xend - e$x) %in% 0:1))
      expect_equal(head(e$xend, -1L), tail(e$x, -1L))
      expect_equal(head(e$yend, -1L), tail(e$y, -1L))
      expect_equal(c(e$x[1L], e$y[1L]),
        unname(unlist(after$nodes[after$nodes$node == e$from[1L], c("x", "y")])))
      expect_equal(c(tail(e$xend, 1L), tail(e$yend, 1L)),
        unname(unlist(after$nodes[after$nodes$node == e$to[1L], c("x", "y")])))
      if (!any(old[[id]]$bend)) expect_false(any(e$bend))
      unrelated <- after$nodes[!after$nodes$node %in% c(e$from[1L], e$to[1L]), ]
      expect_true(.route_obstacle_free(.route_runs(e), unrelated))
    }
  }
})

test_that("arrangement search is deterministic across rows, input order and rotations", {
  # Assess the arrangement stage before the final half-grid routing pass.
  .test_without_stages(".refine_route_resolution")
  before <- .test_before_arrangement(.test_arrangement_graph(2L))
  nodes <- before$nodes[c("node", "x", "y")]
  routes <- unname(split(before$edges, before$edges$edge_id))
  compact <- .compact_node_arrangement(nodes, routes)
  reversed <- .compact_node_arrangement(nodes[nrow(nodes):1L, ], rev(routes))
  expect_equal(reversed$nodes[nrow(nodes):1L, ], compact$nodes)
  expect_identical(rev(reversed$routes), compact$routes)
  expect_identical(.compact_node_arrangement(nodes, routes), compact)
  graph <- .test_arrangement_graph()
  ranks <- .test_before_arrangement(graph)$nodes
  ranks <- stats::setNames(ranks$layer, ranks$node)
  ends <- igraph::ends(graph, igraph::E(graph))
  reordered <- igraph::graph_from_edgelist(ends[nrow(ends):1L, ])
  geometry <- function(layout) {
    e <- layout$edges[order(layout$edges$from, layout$edges$to, layout$edges$segment),
      c("from", "to", "x", "y", "xend", "yend", "bend", "path_id")]
    rownames(e) <- NULL
    e
  }
  # Direction is a final rotation: lay each input out once and turn it.
  first <- .test_layouts_by_direction(graph, rank = ranks)
  second <- .test_layouts_by_direction(reordered, rank = ranks)
  for (direction in .test_directions) {
    a <- first[[direction]]
    b <- second[[direction]]
    expect_equal(geometry(a), geometry(b))
    expect_equal(a$nodes$layer, unname(ranks[a$nodes$node]))
    expect_equal(layout_metrics(a)$directional_consistency, 1)
    # Supplied ranks keep the rank-preserving placement passes (main 849f62b:
    # area 180 and 13 bends; now 11 bends at the same crossings and shared track).
    expect_equal(layout_metrics(a)$area, 153)
  }
})

test_that("straight-edge constraints reject incompatible cycles and bounded searches stop", {
  nodes <- data.frame(node = c("A", "B", "C", "D"),
    x = c(0, -4, 4, 0), y = c(0, -4, -4, -8))
  ends <- rbind(c(1L, 2L), c(1L, 3L), c(2L, 4L), c(3L, 4L))
  routes <- lapply(seq_len(nrow(ends)), function(id) {
    a <- ends[id, 1L]; b <- ends[id, 2L]
    moves <- rep(sign(nodes$x[b] - nodes$x[a]), 4L)
    e <- .route_from_moves(nodes$x[a], nodes$y[a], moves)
    e$from <- nodes$node[a]; e$to <- nodes$node[b]
    e$edge_id <- id; e$path_id <- NA_integer_
    e$segment <- seq_len(nrow(e)); e$bend <- FALSE
    e[, names(.empty_routes())]
  })
  expect_null(.arrangement_positions(nodes, routes, c(0, -3, -3, -7), nodes$x))
  expected <- list(nodes = nodes, routes = routes)
  expect_identical(.compact_node_arrangement(nodes, routes), expected)
  expect_identical(.compact_node_arrangement(nodes, routes, max_candidates = 0L), expected)
  expect_identical(.compact_node_arrangement(nodes, list()), list(nodes = nodes, routes = list()))
})

test_that("compact review DAGs export clear labels and one arrowhead per edge", {
  skip_if_not_installed("svglite")
  labels <- list(
    c(I = "Household income", H = "Baseline health", D = "Smoking at baseline",
      X = "Usual alcohol use", M = "Systolic BP rise", S = "Blood triglycerides", Y = "Coronary events"),
    c(W = "Sugary drinks", O = "Body fat mass", Y = "HbA1c level",
      A = "Food environment", S = "Fast-food consumption", C = "Household income",
      D = "Primary care", U = "Metabolic genotype", H = "Shift-work hours"))
  for (which in 1:2) {
    graph <- .test_arrangement_graph(which)
    p <- metro_dag(graph, labels = labels[[which]], exposure = c("X", "W")[which],
      outcome = "Y", adjusted = if (which == 1L) c("I", "H", "D") else character(),
      unobserved = if (which == 2L) "U" else character(), unadjusted = which == 2L)
    for (width in c(180, 240)) {
      for (device in c("svg", "png")) {
        report <- diagnose_metro_dag(p, width = width, device = device, dpi = 180)
        expect_true(report$ok)
        expect_equal(nrow(report$issues), 0L, info = paste(which, width, device))
      }
      file <- tempfile(fileext = ".svg")
      expect_no_warning(save_metro_dag(file, p, width = width))
      expect_equal(.svg_count(file, "polygon"), igraph::ecount(graph))
      unlink(file)
    }
  }
})
