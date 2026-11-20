test_that("component packing preserves original graph identities and endpoints", {
  first <- igraph::delete_vertex_attr(metrodag_examples("diamond"), "name")
  second <- igraph::delete_vertex_attr(metrodag_examples("chain"), "name")
  g <- igraph::disjoint_union(first, second, igraph::make_empty_graph(5, directed = TRUE))
  igraph::V(g)$name <- paste0("n", seq_len(igraph::vcount(g)))
  layouts <- .test_layouts_by_direction(g)
  for (direction in c("down", "up", "right", "left")) {
    l <- layouts[[direction]]
    expect_true(l$parameters$component_packing)
    expect_identical(l$graph, g)
    expect_setequal(unique(l$edges$edge_id), seq_len(igraph::ecount(g)))
    for (id in unique(l$edges$edge_id)) {
      z <- l$edges[l$edges$edge_id == id, ]
      original <- igraph::ends(g, id)
      expect_equal(c(z$from[1L], z$to[1L]), as.vector(original))
      source <- l$nodes[match(z$from[1L], l$nodes$node), ]
      target <- l$nodes[match(z$to[1L], l$nodes$node), ]
      expect_equal(c(z$x[1L], z$y[1L]), c(source$x, source$y))
      expect_equal(c(tail(z$xend, 1), tail(z$yend, 1)), c(target$x, target$y))
    }
    expect_equal(layout_metrics(l)$node_edge_collisions, 0)
    expect_equal(layout_metrics(l)$directional_consistency, 1)
    expect_identical(l, layout_metro_dag(g, direction = direction))
  }
})

test_that("isolates pack compactly while explicit ranks remain aligned", {
  g <- igraph::make_empty_graph(30, directed = TRUE)
  l <- layout_metro_dag(direction = "down", g)
  expect_gt(length(unique(l$nodes$x)), 1L)
  expect_gt(length(unique(l$nodes$y)), 1L)
  expect_equal(nrow(unique(l$nodes[, c("x", "y")])), 30L)
  # Thirty stations at single-column width draw without errors, but the rows
  # are about 7 mm apart, which leaves interior labels no room beside their
  # own station; diagnostics say so with label warnings rather than calling
  # the figure clean. Which warnings depends on the text height the device
  # measures: with the taller metrics of Windows and macOS fonts no label
  # slot between two rows is clear of both, so a label can also touch the
  # next station (label_station) there.
  expect_true(diagnose_metro_dag(l, width = 85)$ok)
  # From 100 mm no label covers a station or leaves the figure; that still
  # holds on Windows with labels a quarter larger (label_size = 4).
  r <- diagnose_metro_dag(l, width = 100)
  expect_true(r$ok)
  expect_false(any(r$issues$type %in% c("label_station", "label_bounds")))
  expect_true(diagnose_metro_dag(l, width = 180)$ok)
  aligned <- layout_metro_dag(direction = "down", g, rank = rep(0, 30))
  expect_false(aligned$parameters$component_packing)
  expect_length(unique(aligned$nodes$y), 1L)
})
