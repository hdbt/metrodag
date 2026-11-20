test_that("tidygraph preserves node and edge attributes across active and grouped contexts", {
  skip_if_not_installed("tidygraph")
  reference <- .compat_reference()
  tbl <- tidygraph::as_tbl_graph(reference)
  inputs <- list(tidygraph::activate(tbl, "nodes"), tidygraph::activate(tbl, "edges"),
    tidygraph::group_by(tbl, role))
  # Each input is laid out once and turned to every direction (test-layout.R);
  # metro_dag() is also called on each input directly, facing down.
  references <- lapply(.test_layouts_by_direction(reference), metro_dag)
  for (input in inputs) {
    original <- input
    a <- references$down
    b <- metro_dag(input, direction = "down")
    expect_identical(.compat_display(b), .compat_display(a))
    expect_equal(.compat_routes(attr(b, "metrodag_layout")), .compat_routes(attr(a, "metrodag_layout")))
    expect_identical(attr(b, "metrodag_analysis"), attr(a, "metrodag_analysis"))
    layouts <- .test_layouts_by_direction(input)
    for (direction in c("down", "up", "right", "left")) {
      a <- references[[direction]]
      b <- metro_dag(layouts[[direction]])
      expect_identical(.compat_display(b), .compat_display(a))
      expect_equal(.compat_routes(attr(b, "metrodag_layout")), .compat_routes(attr(a, "metrodag_layout")))
      expect_identical(attr(b, "metrodag_analysis"), attr(a, "metrodag_analysis"))
      expect_equal(igraph::degree(attr(b, "metrodag_layout")$graph, "I"), c(I = 0))
    }
    expect_identical(input, original)
    converted <- .as_metro_input(input)
    expect_identical(igraph::vertex_attr(converted), igraph::vertex_attr(reference))
    expect_identical(igraph::edge_attr(converted), igraph::edge_attr(reference))
  }
})

test_that("tidygraph inputs keep existing DAG validation and comparison behavior", {
  skip_if_not_installed("tidygraph")
  graph <- tidygraph::as_tbl_graph(.compat_reference())
  compare <- compare_metro_dags(graph, list(Inherit = list(), Crude = list(unadjusted = TRUE)),
    node_groups = list(Context = "C", Unmeasured = "U"))
  expect_equal(igraph::vcount(compare$layout$graph), 5L)
  expect_equal(igraph::ecount(compare$layout$graph), 5L)
  expect_equal(attr(compare$plots[[1L]], "metrodag_analysis")$adjusted, "C")
  expect_length(attr(compare$plots[[2L]], "metrodag_analysis")$adjusted, 0L)
  expect_error(metro_dag(tidygraph::as_tbl_graph(igraph::make_ring(3, directed = TRUE))), "cycle")
  expect_error(metro_dag(tidygraph::as_tbl_graph(igraph::make_ring(3, directed = FALSE))), "directed")
  repeated <- igraph::graph_from_edgelist(rbind(c("X", "Y"), c("X", "Y")), directed = TRUE)
  expect_error(metro_dag(tidygraph::as_tbl_graph(repeated)), "repeats the arrow X -> Y")
})

test_that("tidygraph node and edge tibbles can be constructed with explicit identifiers", {
  skip_if_not_installed("tidygraph")
  graph <- tidygraph::as_tbl_graph(.compat_reference())
  nodes <- tidygraph::as_tibble(graph, active = "nodes")
  edges <- tidygraph::as_tibble(graph, active = "edges")
  edges$from <- nodes$name[edges$from]
  edges$to <- nodes$name[edges$to]
  input <- metro_graph(nodes[, c("cohort", "name", "role", "label")],
    edges[, c("weight", "to", "from")])
  expect_identical(igraph::as_edgelist(input), igraph::as_edgelist(.compat_reference()))
  expect_equal(.compat_display(metro_dag(input)), .compat_display(metro_dag(.compat_reference())))
})
