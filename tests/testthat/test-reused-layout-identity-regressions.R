.test_identity_graph <- function() {
  graph <- metrodag_examples("chain")
  igraph::V(graph)$label <- c("Treatment", "Covariate", "Latent", "Response")
  graph
}

.test_identity_nodes <- function(plot) {
  nodes <- plot$layers[[2L]]$geom_params$nodes
  nodes <- nodes[order(nodes$node), , drop = FALSE]
  rownames(nodes) <- NULL
  nodes
}

.test_identity_edges <- function(plot) {
  edges <- plot$layers[[2L]]$geom_params$edges
  edges <- edges[order(edges$from, edges$to, edges$segment), , drop = FALSE]
  rownames(edges) <- NULL
  edges
}

test_that("reused layouts bind labels and roles to node names", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  original <- metro_dag(.test_identity_graph(), exposure = "A", outcome = "D",
    adjusted = "B", unobserved = "C", estimand = "Total effect of A on D")
  fixed <- attr(original, "metrodag_layout")
  for (reorder_nodes in c(FALSE, TRUE)) for (reorder_graph in c(FALSE, TRUE)) {
    input <- fixed
    if (reorder_nodes) input$nodes <- input$nodes[c(4, 2, 1, 3), , drop = FALSE]
    if (reorder_graph) input$graph <- igraph::permute(input$graph, c(3, 1, 4, 2))
    before <- input
    for (style in c("publication", "monochrome", "metro"))
      for (labels in list(TRUE, c(A = "New treatment", C = "Unmeasured"), FALSE)) {
        reference <- metro_dag(fixed, style = style, labels = labels)
        for (fun in list(metro_dag, plot_metro_dag, ggplot2::autoplot)) {
          expect_no_warning(plot <- fun(input, style = style, labels = labels))
          expect_identical(.test_identity_nodes(plot), .test_identity_nodes(reference))
          expect_identical(.test_identity_edges(plot), .test_identity_edges(reference))
          expect_identical(attr(plot, "metrodag_analysis"), attr(reference, "metrodag_analysis"))
          expect_identical(plot$labels$title, reference$labels$title)
          expect_identical(plot$labels$subtitle, reference$labels$subtitle)
          expect_identical(attr(plot, "metrodag_layout")$nodes, input$nodes)
          expect_identical(attr(plot, "metrodag_layout")$edges, input$edges)
          expect_identical(input, before)
        }
      }
  }
})

test_that("reused comparisons keep role identities across reordered metadata", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  fixed <- layout_metro_dag(.test_identity_graph(), direction = "right")
  views <- list(Unadjusted = list(exposure = "A", outcome = "D", unobserved = "C",
      unadjusted = TRUE),
    Adjusted = list(exposure = "A", outcome = "D", unobserved = "C", adjusted = "B"))
  reference <- compare_metro_dags(fixed, views)
  input <- fixed
  input$nodes <- input$nodes[4:1, , drop = FALSE]
  input$graph <- igraph::permute(input$graph, c(2, 4, 1, 3))
  before <- input
  expect_no_warning(comparison <- compare_metro_dags(input, views))
  for (i in seq_along(views)) {
    expect_identical(.test_identity_nodes(comparison$plots[[i]]),
      .test_identity_nodes(reference$plots[[i]]))
    expect_identical(.test_identity_edges(comparison$plots[[i]]),
      .test_identity_edges(reference$plots[[i]]))
    expect_identical(attr(comparison$plots[[i]], "metrodag_analysis"),
      attr(reference$plots[[i]], "metrodag_analysis"))
  }
  expect_identical(attr(comparison$legend_plot, "metrodag_legend_values"),
    attr(reference$legend_plot, "metrodag_legend_values"))
  expect_identical(comparison$layout, input)
  expect_identical(input, before)
})

test_that("estimand inheritance follows the resolved graph target", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  original <- metro_dag(.test_identity_graph(), exposure = "A", outcome = "D",
    unadjusted = TRUE, estimand = "Total effect of A on D")
  fixed <- attr(original, "metrodag_layout")
  for (target in c("exposure", "outcome")) for (role_type in list(identity, factor, ordered)) {
    changed <- fixed
    role <- igraph::V(changed$graph)$role
    names <- igraph::V(changed$graph)$name
    role[role == target] <- "observed"
    role[names == if (target == "exposure") "B" else "C"] <- target
    changed$graph <- igraph::set_vertex_attr(changed$graph, "role", value = role_type(role))
    before <- changed
    for (fun in list(metro_dag, plot_metro_dag, ggplot2::autoplot)) {
      expect_no_warning(plot <- fun(changed))
      analysis <- attr(plot, "metrodag_analysis")
      expect_null(plot$labels$title)
      expect_null(analysis$estimand)
      expect_identical(analysis[[target]], if (target == "exposure") "B" else "C")
      expect_identical(attr(plot, "metrodag_layout")$nodes, changed$nodes)
      expect_identical(attr(plot, "metrodag_layout")$edges, changed$edges)
      expect_identical(fun(changed, exposure = "A", outcome = "D")$labels$title,
        "Total effect of A on D")
      expect_identical(fun(changed, estimand = "New target")$labels$title, "New target")
      expect_null(fun(changed, estimand = NULL)$labels$title)
      expect_identical(changed, before)
    }
  }
  comparison <- compare_metro_dags(changed,
    list(Inherited = list(), Cleared = list(estimand = NULL),
      Restored = list(exposure = "A", outcome = "D")))
  expect_identical(comparison$plots[[1L]]$labels$title, "Inherited")
  expect_null(attr(comparison$plots[[1L]], "metrodag_analysis")$estimand)
  expect_null(comparison$plots[[2L]]$labels$title)
  expect_identical(comparison$plots[[3L]]$labels$title, "Total effect of A on D")
})

test_that("reordered layout exports retain the same station and label geometry", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  annotated <- metro_dag(.test_identity_graph(), exposure = "A", outcome = "D",
    adjusted = "B", unobserved = "C", legend = FALSE, estimand = "Total effect of A on D")
  fixed <- attr(annotated, "metrodag_layout")
  reordered <- fixed
  reordered$nodes <- reordered$nodes[4:1, , drop = FALSE]
  reordered$graph <- igraph::permute(reordered$graph, c(3, 1, 4, 2))
  before <- reordered
  primitives <- function(file) sort(.svg_primitives(file))
  for (style in c("publication", "monochrome", "metro")) {
    a <- metro_dag(fixed, style = style, legend = FALSE)
    b <- metro_dag(reordered, style = style, legend = FALSE)
    expect_no_warning(save_metro_dag(files[1L], a, width = 120, height = 85))
    expect_no_warning(save_metro_dag(files[2L], b, width = 120, height = 85))
    expect_identical(primitives(files[2L]), primitives(files[1L]))
    expect_no_warning(report <- diagnose_metro_dag(b, width = 120,
      height = 85, device = "svg"))
    expect_true(report$ok)
    expect_equal(nrow(report$issues), 0L)
    # C's dashed outline is a polygon; A and D are circles. Monochrome draws an
    # outcome as a ring with a second circle at its centre.
    rings <- sum(.metro_node_style(igraph::V(attr(b, "metrodag_layout")$graph)$role, "white", style)$marker == "ring")
    expect_equal(.svg_count(files[2L], "circle"), 2L + rings)
    expect_equal(.svg_dashed_outlines(files[2L]), 1L)
    expect_equal(.svg_arrowheads(files[2L]), 3L)
    expect_identical(reordered, before)
  }
  views <- list(Unadjusted = list(unadjusted = TRUE), Adjusted = list(adjusted = "B"))
  a <- compare_metro_dags(fixed, views)
  b <- compare_metro_dags(reordered, views)
  expect_no_warning(save_metro_dag(files[1L], a, width = 180, height = 100))
  expect_no_warning(save_metro_dag(files[2L], b, width = 180, height = 100))
  expect_identical(primitives(files[2L]), primitives(files[1L]))
  expect_no_warning(report <- diagnose_metro_dag(b, width = 180,
    height = 100, device = "svg"))
  expect_true(report$ok)
  expect_equal(nrow(report$issues), 0L)
  expect_identical(reordered, before)
})

test_that("inherited estimand validation and explicit clearing remain intact", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  plot <- metro_dag(.test_identity_graph(), exposure = "A", outcome = "D",
    unadjusted = TRUE, estimand = "Total effect of A on D")
  fixed <- attr(plot, "metrodag_layout")
  for (invalid in list(NA_character_, "", " ", c("one", "two"), 1, factor("label"))) {
    input <- fixed
    input$analysis["estimand"] <- list(invalid)
    before <- input
    expect_error(metro_dag(input), "`estimand`")
    expect_null(metro_dag(input, estimand = NULL)$labels$title)
    expect_identical(metro_dag(input, estimand = "Replacement")$labels$title, "Replacement")
    expect_identical(input, before)
  }
  cleared <- attr(metro_dag(fixed, estimand = NULL), "metrodag_layout")
  result <- metro_dag(cleared)
  expect_null(result$labels$title)
  expect_true("estimand" %in% names(attr(result, "metrodag_analysis")))
  expect_identical(attr(result, "metrodag_analysis"), cleared$analysis)
})

test_that("reused layouts require one node row for each graph vertex", {
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- igraph::make_empty_graph(3L, directed = TRUE)
  igraph::V(graph)$name <- c("A", "B", "C")
  graph <- igraph::add_edges(graph, c("A", "B"))
  fixed <- layout_metro_dag(graph, direction = "right")
  missing <- fixed
  missing$nodes <- missing$nodes[missing$nodes$node != "C", , drop = FALSE]
  duplicate <- fixed
  duplicate$nodes$node[3L] <- "A"
  unknown <- fixed
  unknown$nodes$node[3L] <- "Unknown"
  absent <- fixed
  absent$nodes$node[3L] <- NA_character_
  extra <- fixed
  extra$nodes <- rbind(extra$nodes, extra$nodes[3L, , drop = FALSE])
  no_column <- fixed
  no_column$nodes$node <- NULL
  both_missing <- absent
  both_missing$graph <- igraph::set_vertex_attr(both_missing$graph, "name",
    value = c("A", "B", NA_character_))
  both_empty <- fixed
  both_empty$nodes$node[3L] <- ""
  both_empty$graph <- igraph::set_vertex_attr(both_empty$graph, "name", value = c("A", "B", ""))
  for (input in list(missing, duplicate, unknown, absent, extra, no_column,
      both_missing, both_empty)) {
    before <- input
    for (fun in list(metro_dag, plot_metro_dag, ggplot2::autoplot))
      expect_error(fun(input, labels = FALSE), "node rows must match graph vertices")
    expect_error(compare_metro_dags(input, list(list()), labels = FALSE),
      "node rows must match graph vertices")
    expect_error(layout_metrics(input), "node rows must match graph vertices")
    expect_error(save_metro_dag(file, input, width = 120),
      "node rows must match graph vertices")
    expect_false(file.exists(file))
    expect_identical(input, before)
  }
  empty <- fixed
  empty$nodes <- empty$nodes[FALSE, , drop = FALSE]
  expect_error(metro_dag(empty, labels = FALSE), "node rows must match graph vertices")
  expect_error(layout_metrics(empty), "node rows must match graph vertices")
})
