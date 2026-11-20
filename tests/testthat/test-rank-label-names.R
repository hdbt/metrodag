.header_at <- function(p) {
  scale <- ggplot2::layer_scales(p)$x
  stats::setNames(scale$labels, scale$breaks)
}

.wave_edges <- function() data.frame(
  from = c("C", "C", "X1", "X2", "Y1", "Y2", "X1", "Y1", "X2", "Y2"),
  to = c("X1", "Y1", "X2", "X3", "Y2", "Y3", "Y2", "X2", "Y3", "X3"))

test_that("named rank_labels are matched to the rank values, not to positions", {
  # The longitudinal example from the audit: headers keyed by rank value and
  # listed out of order. The rank-0 column must be headed "Baseline".
  wave_edges <- .wave_edges()
  wave_ranks <- c(C = 0, X1 = 1, Y1 = 1, X2 = 2, Y2 = 2, X3 = 3, Y3 = 3)
  q <- metro_dag(wave_edges, rank = wave_ranks, rank_labels = c(`3` = "Wave 3",
    `0` = "Baseline", `1` = "Wave 1", `2` = "Wave 2"))
  nodes <- attr(q, "metrodag_layout")$nodes
  headers <- .header_at(q)
  column <- function(node) headers[[as.character(nodes$x[nodes$node == node])]]
  expect_identical(column("C"), "Baseline")
  expect_identical(column("X1"), "Wave 1")
  expect_identical(column("Y2"), "Wave 2")
  expect_identical(column("X3"), "Wave 3")
  # The same headers in rank order give the same figure.
  ordered <- metro_dag(wave_edges, rank = wave_ranks,
    rank_labels = c("Baseline", "Wave 1", "Wave 2", "Wave 3"))
  expect_identical(.header_at(q), .header_at(ordered))
})

test_that("names follow the supplied rank values through reuse and comparisons", {
  wave_edges <- .wave_edges()
  years <- c(C = 1990, X1 = 1991, Y1 = 1991, X2 = 1992, Y2 = 1992, X3 = 1993, Y3 = 1993)
  headers <- c(`1993` = "Wave 3", `1990` = "Baseline", `1991` = "Wave 1", `1992` = "Wave 2")
  expected <- c("Baseline", "Wave 1", "Wave 2", "Wave 3")
  p <- metro_dag(wave_edges, rank = years, rank_labels = headers)
  expect_identical(unname(.header_at(p)), expected)
  reused <- metro_dag(attr(p, "metrodag_layout"), rank_labels = headers)
  expect_identical(unname(.header_at(reused)), expected)
  comparison <- compare_metro_dags(wave_edges, list(A = list(exposure = "X1")),
    rank = years, rank_labels = headers)
  expect_identical(unname(.header_at(comparison$plots[[1L]])), expected)
  # An unnamed `rank` follows the graph's vertex order.
  order <- igraph::V(igraph::graph_from_data_frame(wave_edges))$name
  unnamed <- metro_dag(wave_edges, rank = unname(years[order]), rank_labels = headers)
  expect_identical(unname(.header_at(unnamed)), expected)
})

test_that("layouts without supplied ranks use their layer numbers", {
  chain <- metrodag_examples("chain")
  p <- metro_dag(chain, rank_labels = c(`2` = "Third", `0` = "First", `3` = "Fourth", `1` = "Second"))
  expect_identical(unname(.header_at(p)), c("First", "Second", "Third", "Fourth"))
  layout <- layout_metro_dag(chain)
  q <- plot_metro_dag(layout, rank_labels = c(`1` = "Second", `0` = "First", `2` = "Third", `3` = "Fourth"))
  expect_identical(unname(.header_at(q)), c("First", "Second", "Third", "Fourth"))
  expect_error(metro_dag(chain, rank_labels = c(`4` = "A", `0` = "B", `1` = "C", `2` = "D")),
    "`rank_labels` names a rank not drawn: `4`. Names are layer numbers: 0, 1, 2, 3.", fixed = TRUE)
  # A layout that keeps no `rank` values says how to name the headers instead.
  abc <- data.frame(from = c("A", "B"), to = c("B", "C"))
  expect_error(plot_metro_dag(layout_metro_dag(abc),
    rank_labels = c(`1990` = "Baseline", `1991` = "Wave 1", `1992` = "Wave 2")),
    paste("`rank_labels` names ranks not drawn: `1990`, `1991` and `1992`. Names are layer numbers: 0, 1, 2.",
      "This layout keeps no `rank` values; use the layer numbers, or give the headers unnamed in rank order."),
    fixed = TRUE)
  years <- layout_metro_dag(abc, rank = c(A = 1990, B = 1991, C = 1992))
  expect_identical(unname(.header_at(plot_metro_dag(years, rank_labels = c("Baseline", "Wave 1", "Wave 2")))),
    c("Baseline", "Wave 1", "Wave 2"))
})

test_that("named rank_labels name the headers at fault", {
  wave_edges <- .wave_edges()
  ranks <- c(C = 0, X1 = 1, Y1 = 1, X2 = 2, Y2 = 2, X3 = 3, Y3 = 3)
  draw <- function(headers) metro_dag(wave_edges, rank = ranks, rank_labels = headers)
  expect_error(draw(c(`0` = "A", `1` = "B", `2` = "C", `5` = "D", `6` = "E")),
    "`rank_labels` names ranks not drawn: `5` and `6`. Names are the `rank` values: 0, 1, 2, 3.",
    fixed = TRUE)
  expect_error(draw(c(`0` = "A", `1` = "B", `1` = "C", `3` = "D")),
    "`rank_labels` names rank `1` more than once.", fixed = TRUE)
  expect_error(draw(c(`0` = "A", `3` = "D")),
    "`rank_labels` has no header for ranks `1` and `2`.", fixed = TRUE)
  expect_error(draw(stats::setNames(c("A", "B", "C", "D"), c("0", "1", "", "3"))),
    "Name every `rank_labels` header by its rank")
  # Unnamed vectors stay positional and keep their length check.
  expect_error(draw(c("A", "B")), "one label per rank (4 here)", fixed = TRUE)
})

test_that("named rank_labels match the rank values given to layout_metro_dag()", {
  years <- layout_metro_dag(data.frame(from = c("A", "B"), to = c("B", "C")),
    rank = c(A = 1990, B = 1991, C = 1992))
  headers <- c(`1992` = "Wave 2", `1990` = "Baseline", `1991` = "Wave 1")
  expect_identical(unname(.header_at(plot_metro_dag(years, rank_labels = headers))),
    c("Baseline", "Wave 1", "Wave 2"))
  expect_identical(unname(.header_at(metro_dag(years, rank_labels = headers))),
    c("Baseline", "Wave 1", "Wave 2"))
  expect_null(layout_metro_dag(data.frame(from = "A", to = "B"))$parameters$rank)
})

test_that("an unnamed rank stays with its nodes when the graph's vertices are reordered", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  layout <- layout_metro_dag(edges, rank = c(1990, 1991, 1992))
  expect_identical(layout$parameters$rank, c(A = 1990, B = 1991, C = 1992))
  headers <- c(`1990` = "Base", `1991` = "W1", `1992` = "W2")
  expect_identical(unname(.header_at(plot_metro_dag(layout, rank_labels = headers))), c("Base", "W1", "W2"))
  permuted <- layout
  permuted$graph <- igraph::permute(layout$graph, c(3L, 2L, 1L))
  expect_identical(igraph::V(permuted$graph)$name, c("C", "B", "A"))
  expect_identical(unname(.header_at(plot_metro_dag(permuted, rank_labels = headers))), c("Base", "W1", "W2"))
})
