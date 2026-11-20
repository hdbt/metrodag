# The three-wave gallery DAG (06_longitudinal in helper-gallery.R).
.test_wave_graph <- function() .test_gallery_graph(6L)

test_that("alignment gives repeated measures straight rows with all causal arrows", {
  graph <- .test_wave_graph()
  ranks <- c(S = 0, C = 0, X1 = 1, Y1 = 1, X2 = 2, Y2 = 2, M2 = 2, X3 = 3, Y3 = 3)
  groups <- list(c("X1", "X2", "X3"), c("Y1", "Y2", "Y3"))
  base <- layout_metro_dag(direction = "down", graph, rank = ranks, align = groups)
  original <- graph
  for (direction in c("down", "right", "up", "left")) {
    a <- layout_metro_dag(graph, direction = direction, rank = ranks, align = groups)
    axis <- if (direction %in% c("right", "left")) "y" else "x"
    for (group in groups) expect_length(unique(a$nodes[[axis]][match(group, a$nodes$node)]), 1L)
    expect_equal(a$nodes$layer, unname(ranks[a$nodes$node]))
    expect_identical(a$graph, original)
    expect_equal(a$parameters$align, groups)
    expect_equal(layout_metrics(a)$node_edge_collisions, 0L)
    expect_equal(layout_metrics(a)$directional_consistency, 1)
    rotated <- .rotate_geometry(base$nodes, base$edges, direction)
    expect_equal(a$nodes, rotated$nodes)
    expect_equal(a$edges, rotated$edges)
    backbone <- subset(a$edges, paste0(from, to) %in% c("X1X2", "X2X3", "Y1Y2", "Y2Y3"))
    expect_false(any(backbone$bend))
    first <- a$edges[!duplicated(a$edges$edge_id), ]
    expect_equal(nrow(first), igraph::ecount(graph))
    expect_equal(as.matrix(first[c("from", "to")]), igraph::as_edgelist(graph), ignore_attr = TRUE)
  }
  edges <- igraph::as_edgelist(graph)
  reversed <- igraph::graph_from_edgelist(edges[nrow(edges):1L, ])
  other <- layout_metro_dag(direction = "down", reversed, rank = ranks, align = rev(groups))
  geometry <- function(a) {
    e <- a$edges[order(a$edges$from, a$edges$to, a$edges$segment),
      c("from", "to", "x", "y", "xend", "yend", "bend")]
    rownames(e) <- NULL
    e
  }
  expect_equal(geometry(base), geometry(other))
})

test_that("alignment does not turn a bypass into an extra causal station", {
  graph <- igraph::graph_from_edgelist(rbind(c("A", "B"), c("B", "C"), c("A", "C")))
  a <- layout_metro_dag(direction = "down", graph, align = list(c("A", "B", "C")))
  expect_length(unique(a$nodes$x), 1L)
  expect_identical(a$graph, graph)
  expect_equal(layout_metrics(a)$node_edge_collisions, 0L)
  bypass <- subset(a$edges, from == "A" & to == "C")
  expect_true(any(bypass$bend))
  expect_true(.route_obstacle_free(.route_runs(bypass), subset(a$nodes, node == "B")))
})

test_that("alignment validates identities and ranks and can be reused across views", {
  graph <- .test_wave_graph()
  ranks <- c(S = 0, C = 0, X1 = 1, Y1 = 1, X2 = 2, Y2 = 2, M2 = 2, X3 = 3, Y3 = 3)
  for (bad in list("X1", list("X1"), list(c("X1", "missing")),
      list(c("X1", NA_character_)), list(c("X1", "X1")),
      list(c("X1", "X2"), c("X2", "X3"))))
    expect_error(layout_metro_dag(direction = "down", graph, rank = ranks, align = bad), "align")
  expect_error(layout_metro_dag(direction = "down", graph, rank = ranks, align = list(c("X1", "Y1"))), "different ranks")
  expect_equal(layout_metro_dag(direction = "down", graph, align = list()), layout_metro_dag(direction = "down", graph))
  p <- metro_dag(graph, rank = ranks, align = list(c("X1", "X2", "X3")))
  layout <- attr(p, "metrodag_layout")
  q <- metro_dag(layout, adjusted = c("S", "C"))
  expect_identical(attr(q, "metrodag_layout")$nodes, layout$nodes)
  expect_identical(attr(q, "metrodag_layout")$edges, layout$edges)
  expect_error(metro_dag(layout, align = NULL), "reusing a layout")
})

test_that("a lone aligned node keeps its group's coordinate", {
  # Y1 is alone in its rank; its only parent and child sit on another column.
  # The singleton slide must not move it off the aligned row.
  graph <- igraph::graph_from_edgelist(rbind(
    c("A1", "Y1"), c("Y1", "A3"), c("A1", "A2"), c("A2", "A3"),
    c("A1", "B2"), c("B2", "A3"), c("A1", "C2"), c("C2", "A3")))
  ranks <- c(A1 = 0, Y1 = 1, A2 = 2, B2 = 2, C2 = 2, A3 = 3)
  for (group in list(list(c("A1", "Y1", "A3")), list(c("A2", "Y1")), list(c("C2", "Y1")))) {
    a <- layout_metro_dag(direction = "down", graph, rank = ranks, align = group)
    expect_length(unique(a$nodes$x[match(group[[1L]], a$nodes$node)]), 1L)
    expect_equal(layout_metrics(a)$node_edge_collisions, 0L)
    first <- a$edges[!duplicated(a$edges$edge_id), ]
    expect_equal(nrow(first), igraph::ecount(graph))
  }
})
