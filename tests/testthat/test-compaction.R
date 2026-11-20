# Gallery DAGs 1 and 4, the same graph objects on every call.
.test_compact_graphs <- .test_memo(function(which) .test_gallery_graph(c(1L, 4L)[as.integer(which)]))
.test_compact_graph <- function(which = 1L) .test_compact_graphs(which)

# The layout of `.test_compact_graph(which)` before gap compaction and the later
# refinements. Every refinement it depends on is stubbed inside, so the stored
# layout is the same wherever it is first computed.
.test_before_compaction <- .test_memo(function(which) {
  .test_layout_without(.test_compact_graph(as.integer(which)), c(".compact_route_gaps",
    ".lengthen_route_approaches", ".compact_node_arrangement", ".refine_rank_assignment",
    .test_late_stages))
})

test_that("rank compaction shortens the review drawings without geometric regressions", {
  .test_without_stages(c(".compact_node_arrangement", ".refine_rank_assignment", .test_late_stages))
  for (which in 1:2) {
    graph <- .test_compact_graph(which)
    before <- .test_before_compaction(which)
    after <- layout_metro_dag(direction = "down", graph)
    expect_equal(diff(range(before$nodes$y)), c(25, 39)[which])
    expect_equal(diff(range(after$nodes$y)), c(19, 36)[which])
    expect_identical(before$graph, after$graph)
    expect_identical(before$nodes[c("node", "x", "layer", "order", "path")],
      after$nodes[c("node", "x", "layer", "order", "path")])
    expect_equal(after$nodes$label_y, after$nodes$y)
    expect_equal(after$parameters$layer_gap, diff(sort(unique(-after$nodes$y))))
    old <- unname(split(before$edges, before$edges$edge_id))
    routes <- unname(split(after$edges, after$edges$edge_id))
    expect_true(all(.routed_geometry_quality(routes) <= .routed_geometry_quality(old) + 1e-8))
    expect_lt(layout_metrics(after)$total_edge_length, layout_metrics(before)$total_edge_length)
    expect_equal(layout_metrics(after)$node_edge_collisions, 0L)
    expect_gte(min(as.numeric(stats::dist(after$nodes[c("x", "y")]))), 4)
    for (id in seq_along(routes)) {
      e <- routes[[id]]
      fields <- c("from", "to", "edge_id", "path_id")
      expect_equal(unname(unlist(e[1L, fields])), unname(unlist(old[[id]][1L, fields])))
      expect_identical(e$segment, seq_len(nrow(e)))
      expect_true(all(e$yend == e$y - 1 & abs(e$xend - e$x) %in% 0:1))
      expect_equal(head(e$xend, -1L), tail(e$x, -1L))
      expect_equal(head(e$yend, -1L), tail(e$y, -1L))
      expect_equal(c(e$x[1L], e$y[1L]),
        unname(unlist(after$nodes[after$nodes$node == e$from[1L], c("x", "y")])))
      expect_equal(c(tail(e$xend, 1L), tail(e$yend, 1L)),
        unname(unlist(after$nodes[after$nodes$node == e$to[1L], c("x", "y")])))
      unrelated <- !after$nodes$node %in% c(e$from[1L], e$to[1L])
      margin <- .route_clearances(.route_runs(old[[id]]), before$nodes[unrelated, ])
      expect_true(.route_obstacle_free(.route_runs(e), after$nodes[unrelated, ], margin))
    }
  }
})

test_that("short final approaches improve without adding bends or changing endpoints", {
  before <- .test_before_compaction(2L)
  nodes <- before$nodes[c("node", "x", "y")]
  routes <- unname(split(before$edges, before$edges$edge_id))
  after <- .lengthen_route_approaches(routes, nodes)
  id <- which(vapply(routes, function(e) e$from[1L] == "I" && e$to[1L] == "Y", logical(1)))
  expect_equal(tail(rle(routes[[id]]$xend - routes[[id]]$x)$lengths, 1L), 1L)
  expect_equal(tail(rle(after[[id]]$xend - after[[id]]$x)$lengths, 1L), 3L)
  expect_equal(.routed_geometry_quality(after), .routed_geometry_quality(routes))
  for (i in seq_along(routes)) {
    expect_identical(after[[i]][c("from", "to", "edge_id", "path_id", "segment")],
      routes[[i]][c("from", "to", "edge_id", "path_id", "segment")])
    e <- after[[i]]; old <- routes[[i]]
    expect_equal(c(e$x[1L], e$y[1L], tail(e$xend, 1L), tail(e$yend, 1L)),
      c(old$x[1L], old$y[1L], tail(old$xend, 1L), tail(old$yend, 1L)))
  }
  expect_identical(.lengthen_route_approaches(rev(routes), nodes[nrow(nodes):1L, ]), rev(after))
})

test_that("compaction is deterministic and respects explicit rank ties and rotations", {
  graph <- .test_compact_graph()
  before <- .test_before_compaction(1L)
  nodes <- before$nodes[c("node", "x", "y")]
  routes <- unname(split(before$edges, before$edges$edge_id))
  compact <- .compact_route_gaps(nodes, routes)
  reversed <- .compact_route_gaps(nodes[nrow(nodes):1L, ], rev(routes))
  expect_equal(reversed$nodes[nrow(nodes):1L, ], compact$nodes)
  expect_identical(rev(reversed$routes), compact$routes)
  expect_identical(.compact_route_gaps(compact$nodes, compact$routes), compact)
  ranks <- stats::setNames(before$nodes$layer, before$nodes$node)
  ends <- igraph::ends(graph, igraph::E(graph))
  reordered <- igraph::graph_from_edgelist(ends[nrow(ends):1L, ])
  geometry <- function(layout) {
    e <- layout$edges[order(layout$edges$from, layout$edges$to, layout$edges$segment),
      c("from", "to", "x", "y", "xend", "yend", "bend", "path_id")]
    rownames(e) <- NULL
    e
  }
  # Direction is a final rotation: lay each input out once and turn it.
  layouts <- .test_layouts_by_direction(graph, rank = ranks)
  others <- .test_layouts_by_direction(reordered, rank = ranks)
  for (direction in .test_directions) {
    after <- layouts[[direction]]
    expect_equal(after$nodes$layer, unname(ranks[after$nodes$node]))
    expect_equal(layout_metrics(after)$directional_consistency, 1)
    expect_equal(layout_metrics(after)$node_edge_collisions, 0L)
    expect_equal(geometry(after), geometry(others[[direction]]))
  }
})

test_that("bounded refinements preserve blocked and empty inputs", {
  before <- .test_before_compaction(1L)
  nodes <- before$nodes[c("node", "x", "y")]
  routes <- unname(split(before$edges, before$edges$edge_id))
  expect_identical(.compact_route_gaps(nodes, routes, max_candidates = 0L), list(nodes = nodes, routes = routes))
  expect_identical(.lengthen_route_approaches(routes, nodes, max_candidates = 0L), routes)
  expect_identical(.compact_route_gaps(nodes, list()), list(nodes = nodes, routes = list()))
  expect_identical(.lengthen_route_approaches(list(), nodes), list())
  e <- .route_from_moves(0, 0, c(0, 0, 0, 1, 1, 0))
  e$from <- "A"; e$to <- "Y"; e$edge_id <- 1L; e$path_id <- 1L
  e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(e$xend - e$x) != 0)
  nodes <- data.frame(node = c("A", "Y", "obstacle"), x = c(0, 2, 2), y = c(0, -6, -3))
  expect_true(.route_obstacle_free(.route_runs(e), nodes[3L, ]))
  expect_identical(.lengthen_route_approaches(list(e), nodes), list(e))
  expect_equal(tail(rle(.lengthen_route_approaches(list(e), nodes[1:2, ])[[1L]]$xend -
    .lengthen_route_approaches(list(e), nodes[1:2, ])[[1L]]$x)$lengths, 1L), 3L)
})

test_that("compact annotated examples render with clear labels at both review widths", {
  skip_if_not_installed("svglite")
  labels <- list(
    c(I = "Household income", H = "Baseline health", D = "Smoking at baseline",
      X = "Usual alcohol use", M = "Systolic BP rise", S = "Blood triglycerides", Y = "Coronary events"),
    c(X = "Statin therapy given", P = "Pills collected", A = "Serum LDL cholesterol",
      O = "Arterial plaques", Y = "Vascular events", I = "Household income",
      R = "Comorbidity profile", T = "Dietary fat level", S = "Current smoking",
      U = "Health consciousness", G = "Age at baseline visit"))
  for (which in 1:2) {
    p <- metro_dag(.test_compact_graph(which), labels = labels[[which]],
      exposure = "X", outcome = "Y", adjusted = if (which == 1L) c("I", "H", "D") else c("I", "R", "G"),
      unobserved = if (which == 2L) "U" else character())
    for (width in c(180, 240)) {
      report <- diagnose_metro_dag(p, width = width, device = "svg")
      expect_true(report$ok)
      # At 180 mm no slot near Comorbidity profile is clear of routes. Its
      # label lies either beyond the dashed U -> O route, more than 7 mm from
      # its station and reported cut off, or beside its station across its
      # own arrow R -> A; the font's metrics decide which. No label crosses
      # another station's route.
      issues <- report$issues
      cut_off <- issues$type == "label_association" & is.na(issues$other_node)
      own_route <- issues$type == "label_route" & (issues$from == issues$node | issues$to == issues$node)
      expect_true(all(cut_off | own_route), info = paste(which, width))
      expect_true(all(issues$node == "R" & width == 180), info = paste(which, width))
      file <- tempfile(fileext = ".svg")
      if (nrow(issues)) expect_warning(save_metro_dag(file, p, width = width),
        "The label of R (is cut off from its station by arrow U -> O|crosses arrow R -> A)") else
        expect_no_warning(save_metro_dag(file, p, width = width))
      expect_equal(.svg_count(file, "polygon"), igraph::ecount(.test_compact_graph(which)))
      unlink(file)
    }
  }
})
