test_that("explicit ranks do not bypass layering validation", {
  graph <- metrodag_examples("chain")
  for (rank in list(0:3, c(A = 0, B = 1, C = 2, D = 3))) {
    for (method in list("typo", factor("sinks"), NULL, NA_character_,
        character(), c("longest", "sinks"), TRUE)) {
      expect_error(layout_metro_dag(direction = "down", graph, rank = rank, layering = method), "`layering`")
      expect_error(metro_dag(graph, rank = rank, layering = method), "`layering`")
    }
    for (method in c("longest", "sinks", "shortest")) {
      expect_no_warning(layout <- layout_metro_dag(direction = "down", graph, rank = rank, layering = method))
      expect_equal(layout$nodes$layer, 0:3)
    }
  }
})

test_that("missing inherited factor roles are observed without role conflicts", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  values <- list(factor(c("exposure", NA, NA, "outcome")),
    ordered(c("exposure", NA, NA, "outcome")), factor(rep(NA_character_, 4)))
  for (role in values) {
    graph <- metrodag_examples("chain")
    igraph::V(graph)$role <- role
    original <- graph
    character_graph <- igraph::set_vertex_attr(graph, "role", value = as.character(role))
    reference <- attr(metro_dag(character_graph), "metrodag_analysis")
    fixed <- layout_metro_dag(graph, direction = "right")
    for (input in list(graph, fixed)) {
      expect_no_warning(plot <- metro_dag(input))
      expect_identical(attr(plot, "metrodag_analysis"), reference)
      expected <- as.character(role)
      expected[is.na(expected)] <- "observed"
      expect_equal(plot$layers[[2L]]$geom_params$nodes$role, expected)
      expect_identical(igraph::V(graph)$role, igraph::V(original)$role)
    }
    expect_no_warning(comparison <- compare_metro_dags(fixed,
      list(Inherit = list(), Adjusted = list(adjusted = "B"))))
    expect_identical(attr(comparison$plots[[1L]], "metrodag_analysis"), reference)
    expect_equal(attr(comparison$plots[[2L]], "metrodag_analysis")$adjusted, "B")
    expect_identical(graph, original)
  }
})

test_that("factor role inheritance also works for total-effect checks", {
  skip_if_not_installed("dagitty")
  graph <- metrodag_examples("chain")
  igraph::V(graph)$role <- factor(c("exposure", NA, NA, "outcome"))
  original <- graph
  expect_no_warning(result <- check_metro_adjustment(graph, unadjusted = TRUE))
  expect_true(result$valid)
  expect_equal(result$exposure, "A")
  expect_equal(result$outcome, "D")
  expect_identical(graph, original)
})

test_that("metro path choices and colours survive reordered graph inputs", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  snapshot <- function(data, keys, exclude = character()) {
    result <- data[do.call(order, unname(data[keys])),
      setdiff(names(data), exclude), drop = FALSE]
    rownames(result) <- NULL
    result
  }
  disconnected <- igraph::disjoint_union(
    igraph::delete_vertex_attr(metrodag_examples("diamond"), "name"),
    igraph::delete_vertex_attr(metrodag_examples("chain"), "name"),
    igraph::make_empty_graph(2, directed = TRUE))
  ranks <- c(A = 0, C = 0, E = 0, B = 1, D = 1, F = 1, G = 1)
  tied <- igraph::graph_from_data_frame(
    data.frame(from = c("A", "E", "E"), to = c("D", "D", "G")),
    directed = TRUE, vertices = data.frame(name = names(ranks)))
  graphs <- c(lapply(c("chain", "dense", "causal"), metrodag_examples),
    list(metrodag_examples("random", n = 10L, probability = 0.25, seed = 1L),
      metrodag_examples("random", n = 10L, probability = 0.25, seed = 11L),
      igraph::make_empty_graph(4, directed = TRUE), disconnected, tied))
  for (i in seq_along(graphs)) {
    graph <- .as_dag(graphs[[i]])
    edges <- igraph::as_edgelist(graph)
    names <- if (i == length(graphs)) c("E", "A", "C", "D", "G", "B", "F") else
      rev(igraph::V(graph)$name)
    reordered <- igraph::graph_from_data_frame(edges[rev(seq_len(nrow(edges))), , drop = FALSE],
      directed = TRUE, vertices = data.frame(name = names))
    args <- if (i == length(graphs)) list(rank = ranks, sweeps = 0L) else list()
    first <- do.call(metro_dag, c(list(graph = graph, style = "metro"), args))
    second <- do.call(metro_dag, c(list(graph = reordered, style = "metro"), args))
    a <- attr(first, "metrodag_layout"); b <- attr(second, "metrodag_layout")
    expect_identical(snapshot(a$nodes, "node"), snapshot(b$nodes, "node"))
    expect_identical(snapshot(a$edges, c("from", "to", "segment"), "edge_id"),
      snapshot(b$edges, c("from", "to", "segment"), "edge_id"))
    expect_identical(snapshot(first$layers[[2L]]$geom_params$nodes, "node"),
      snapshot(second$layers[[2L]]$geom_params$nodes, "node"))
    expect_identical(snapshot(first$layers[[2L]]$geom_params$edges,
      c("from", "to", "segment"), "edge_id"),
      snapshot(second$layers[[2L]]$geom_params$edges,
        c("from", "to", "segment"), "edge_id"))
    expect_setequal(unique(b$edges$edge_id), seq_len(igraph::ecount(graph)))
  }
  # Equal-priority edges must choose the same continuations, not just rename colours.
  for (plot in list(first, second)) {
    nodes <- plot$layers[[2L]]$geom_params$nodes
    paths <- stats::setNames(nodes$path, nodes$node)
    expect_equal(paths[["A"]], paths[["D"]])
    expect_equal(paths[["E"]], paths[["G"]])
    expect_false(paths[["E"]] == paths[["D"]])
  }
})
