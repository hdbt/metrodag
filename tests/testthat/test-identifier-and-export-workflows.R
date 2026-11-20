test_that("default ggsave uses the completed MetroDAG plot before printing", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  previous <- ggplot2::last_plot()
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  layout <- layout_metro_dag(metrodag_examples("chain"), direction = "right")
  title <- paste(rep("A long annotation for the diagram", 6), collapse = " ")
  for (fun in list(metro_dag, plot_metro_dag, ggplot2::autoplot)) {
    p <- fun(layout, labels = FALSE, legend = FALSE, estimand = title)
    expect_identical(ggplot2::last_plot(), p)
    expect_no_warning(ggplot2::ggsave(file, width = 85, height = 85, units = "mm"))
    svg <- readLines(file)
    expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 3L)
    text <- svg[grepl("<text", svg, fixed = TRUE)]
    expect_gt(length(text), 1L)
    x <- as.numeric(sub(".* x='([^']+)'.*", "\\1", text))
    width <- as.numeric(sub(".* textLength='([^']+)px'.*", "\\1", text))
    expect_true(all(x >= 0 & x + width <= 85 / 25.4 * 72 + 0.1))
  }
})

test_that("numeric igraph names behave as identifiers across public workflows", {
  graphs <- list(
    igraph::make_ring(3, directed = TRUE, circular = FALSE),
    igraph::make_empty_graph(3, directed = TRUE))
  for (g in graphs) {
    igraph::V(g)$name <- c(10, 20, 30)
    expected <- igraph::set_vertex_attr(g, "name", value = c("10", "20", "30"))
    layout <- layout_metro_dag(direction = "down", g)
    expect_identical(layout$nodes, layout_metro_dag(direction = "down", expected)$nodes)
    expect_identical(layout$edges, layout_metro_dag(direction = "down", expected)$edges)
    expect_identical(igraph::V(layout$graph)$name, c("10", "20", "30"))
    expect_identical(igraph::V(g)$name, c(10, 20, 30))
    p <- metro_dag(g, exposure = "10", outcome = "30", unadjusted = TRUE,
      label_nudge = list(`20` = c(0, 1)))
    expect_true(diagnose_metro_dag(p, width = 180)$ok)
    if (requireNamespace("dagitty", quietly = TRUE))
      expect_true(check_metro_adjustment(p)$valid)
  }
  for (names in list(c(10, NA, 30), c(10, NaN, 30), c(10, 10, 30))) {
    g <- igraph::set_vertex_attr(graphs[[1L]], "name", value = names)
    expect_error(layout_metro_dag(direction = "down", g), "non-empty, non-missing and unique")
  }
})

test_that("automatic export height follows flipped coordinates", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  g <- igraph::make_ring(12, directed = TRUE, circular = FALSE)
  p <- metro_dag(g, labels = FALSE, legend = FALSE)
  for (coordinate in list(ggplot2::coord_flip(), ggplot2::coord_flip(clip = "off"))) {
    q <- suppressMessages(p + coordinate)
    report <- diagnose_metro_dag(q, width = 85, device = "svg")
    expect_true(report$ok)
    expect_gt(report$dimensions[["height"]], 200)
    expect_no_warning(save_metro_dag(file, q, width = 85))
    expect_equal(.svg_count(file, "polygon"), 11L)
  }
})
