test_that("ambiguous waypoint names have stable automatic ordering", {
  edges <- data.frame(from = c("Root -> Z", "Root"), to = c("A", "Z -> A"))
  ranks <- c("Root -> Z" = 0, Root = 0, M = 1, A = 2, "Z -> A" = 2)
  graph <- igraph::graph_from_data_frame(edges,
    vertices = data.frame(name = names(ranks)))
  reordered <- igraph::graph_from_data_frame(edges[2:1, ],
    vertices = data.frame(name = names(ranks)))
  first <- .proper_graph(graph, .assign_layers(graph, rank = ranks))
  second <- .proper_graph(reordered, .assign_layers(reordered, rank = ranks))
  expect_gt(anyDuplicated(igraph::V(first$graph)$name), 0L)
  ordering_by_edge <- function(graph, proper, method) {
    ord <- .order_layers(proper$graph, proper$layer, method = method, sweeps = 0L)
    endpoints <- igraph::as_edgelist(graph)
    lapply(proper$chains[order(endpoints[, 1L], endpoints[, 2L])],
      function(chain) ord[chain])
  }
  snapshot <- function(data, keys, exclude = character()) {
    data <- data[do.call(order, unname(data[keys])),
      setdiff(names(data), exclude), drop = FALSE]
    rownames(data) <- NULL
    data
  }
  for (method in c("barycenter", "median")) {
    expect_identical(ordering_by_edge(graph, first, method),
      ordering_by_edge(reordered, second, method))
    # Laid out once per input and turned to every direction (test-layout.R).
    first_layouts <- .test_layouts_by_direction(graph, rank = ranks, ordering = method, sweeps = 0L)
    second_layouts <- .test_layouts_by_direction(reordered, rank = ranks, ordering = method,
      sweeps = 0L)
    for (direction in c("down", "up", "left", "right")) {
      a <- first_layouts[[direction]]
      b <- second_layouts[[direction]]
      expect_identical(snapshot(a$nodes, "node"), snapshot(b$nodes, "node"))
      expect_identical(snapshot(a$edges, c("from", "to", "segment"), "edge_id"),
        snapshot(b$edges, c("from", "to", "segment"), "edge_id"))
      expect_identical(a$graph, graph)
      expect_null(igraph::vertex_attr(a$graph, ".metrodag_order_source"))
      expect_null(igraph::vertex_attr(a$graph, ".metrodag_order_target"))
    }
  }
  # Explicitly disabling ordering still uses the supplied proper-vertex order.
  expect_equal(.order_layers(first$graph, first$layer, method = "none"),
    c(1L, 2L, 1L, 1L, 2L, 2L, 3L))
})

test_that("canonical metrics retain their benchmark values", {
  file <- system.file("benchmarks", "metrics.csv", package = "metrodag")
  expect_true(nzchar(file))
  reference <- utils::read.csv(file, stringsAsFactors = FALSE)
  for (name in c("chain", "fork", "collider", "diamond", "long_edge", "causal", "dense")) {
    actual <- layout_metrics(layout_metro_dag(metrodag_examples(name), direction = "right"))
    stored <- reference[reference$graph == name, names(actual), drop = FALSE]
    rownames(stored) <- NULL
    expect_equal(actual, stored, tolerance = 1e-8, info = name)
  }
})

test_that("ambiguous waypoint labels preserve every station and arrow on export", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  edges <- data.frame(from = c("Root -> Z", "Root"), to = c("A", "Z -> A"))
  ranks <- c("Root -> Z" = 0, Root = 0, M = 1, A = 2, "Z -> A" = 2)
  graph <- igraph::graph_from_data_frame(edges,
    vertices = data.frame(name = names(ranks)))
  layout <- layout_metro_dag(direction = "down", graph, rank = ranks, sweeps = 0L)
  plot <- plot_metro_dag(layout, labels = FALSE, legend = FALSE)
  expect_no_warning(save_metro_dag(file, plot, width = 220, height = 160))
  svg <- readLines(file)
  expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 2L)
  expect_equal(sum(grepl("<circle", svg, fixed = TRUE)), 5L)
})
