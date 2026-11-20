test_that("longest layers represent causal depth", {
  out <- layout_metro_dag(direction = "down", metrodag_examples("long_edge"))
  layers <- setNames(out$nodes$layer, out$nodes$node)
  expect_equal(unname(layers[c("A", "B", "C", "D", "E")]), 0:4)
})

test_that("user ranks are honored", {
  g <- metrodag_examples("diamond")
  out <- layout_metro_dag(direction = "down", g, rank = c(A = 10, B = 20, C = 20, D = 40))
  expect_equal(setNames(out$nodes$layer, out$nodes$node)[c("A", "B", "C", "D")], c(A = 0L, B = 1L, C = 1L, D = 2L))
})

test_that("minimum-span layers shorten bypass arrows and keep every arrow forward", {
  # S causes only the late node D, so longest-path ranks put it three ranks
  # before D and its arrow needs two waypoints; one rank before D needs none.
  edges <- rbind(c("A", "B"), c("B", "C"), c("C", "D"), c("S", "D"), c("T", "A"),
    c("B", "E"))
  graph <- igraph::graph_from_edgelist(edges)
  ends <- igraph::ends(graph, igraph::E(graph))
  span <- function(layer) sum(layer[ends[, 2L]] - layer[ends[, 1L]])
  longest <- .assign_layers(graph, "longest")
  layer <- .minimum_span_layers(graph, longest)
  expect_true(all(layer[ends[, 2L]] > layer[ends[, 1L]]))
  expect_equal(layer[["S"]], layer[["D"]] - 1L)
  expect_equal(layer[["E"]], layer[["B"]] + 1L)
  expect_equal(span(layer), nrow(edges))
  expect_lt(span(layer), span(longest))
  expect_equal(min(layer), 0L)
  expect_identical(.minimum_span_layers(graph, layer), layer)
  # Visiting nodes by name makes the result independent of input order.
  reordered <- igraph::graph_from_edgelist(edges[nrow(edges):1L, ])
  again <- .minimum_span_layers(reordered, .assign_layers(reordered, "longest"))
  expect_identical(again[names(layer)], layer)
})
