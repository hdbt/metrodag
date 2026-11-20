test_that("station attachments follow endpoint names across reordered inputs", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  graph <- metrodag_examples("dense")
  endpoints <- igraph::as_edgelist(graph)
  reordered <- igraph::graph_from_data_frame(endpoints[nrow(endpoints):1L, ],
    vertices = data.frame(name = rev(igraph::V(graph)$name)))
  snapshot <- function(args) {
    nodes <- args$nodes; edges <- args$edges
    for (key in c("x", "y")) nodes[[key]] <- nodes[[key]] * 5
    for (key in c("x", "y", "xend", "yend")) edges[[key]] <- edges[[key]] * 5
    ports <- .station_ports(nodes, edges, rep(2.2, nrow(nodes)), 0.5)
    first <- edges[!duplicated(edges$edge_id), ]
    ids <- as.character(first$edge_id[order(first$from, first$to)])
    lapply(ports, function(value) unname(value[ids, , drop = FALSE]))
  }
  # Lay out each input once: direction is a final rotation (test-layout.R)
  # and the adjusted set only decorates the geometry.
  layouts <- list(.test_layouts_by_direction(graph), .test_layouts_by_direction(reordered))
  for (direction in .test_directions) {
    for (adjusted in list(character(), c("A", "E"))) {
      first <- metro_dag(layouts[[1L]][[direction]], adjusted = adjusted,
        labels = FALSE, legend = FALSE)
      second <- metro_dag(layouts[[2L]][[direction]], adjusted = adjusted,
        labels = FALSE, legend = FALSE)
      a <- first$layers[[2L]]$geom_params
      b <- second$layers[[2L]]$geom_params
      original <- a
      expect_equal(snapshot(a), snapshot(b), tolerance = 1e-10,
        info = paste(direction, paste(adjusted, collapse = ",")))
      expect_identical(a, original)
      expect_setequal(rownames(.station_ports(a$nodes, a$edges,
        rep(2.2, nrow(a$nodes)), 0.5)$end), as.character(seq_len(igraph::ecount(graph))))
    }
  }
})

test_that("exported routes and crossing casings retain their drawing order", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  for (kind in c("dense", "causal")) {
    graph <- metrodag_examples(kind)
    endpoints <- igraph::as_edgelist(graph)
    reordered <- igraph::graph_from_data_frame(endpoints[nrow(endpoints):1L, ],
      vertices = data.frame(name = rev(igraph::V(graph)$name)))
    layouts <- lapply(list(graph, reordered), .test_layouts_by_direction,
      directions = c("right", "down"))
    for (direction in c("right", "down")) for (style in c("publication", "metro")) {
      plots <- lapply(layouts, function(input)
        metro_dag(input[[direction]], style = style, labels = FALSE, legend = FALSE))
      originals <- plots
      shapes <- vector("list", 2L)
      for (i in seq_along(plots)) {
        expect_no_warning(save_metro_dag(files[i], plots[[i]], width = 260, height = 220))
        svg <- readLines(files[i])
        shapes[[i]] <- grep("<polyline|<polygon", svg, value = TRUE)
        expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), igraph::ecount(graph))
        expect_equal(sum(grepl("<circle", svg, fixed = TRUE)), igraph::vcount(graph))
      }
      expect_identical(shapes[[1L]], shapes[[2L]], info = paste(kind, direction, style))
      expect_identical(plots, originals)
    }
  }
})

test_that("ordinary ggsave retains ordering for unarrowed plots and comparisons", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- metrodag_examples("causal")
  endpoints <- igraph::as_edgelist(graph)
  reordered <- igraph::graph_from_data_frame(endpoints[nrow(endpoints):1L, ],
    vertices = data.frame(name = rev(igraph::V(graph)$name)))
  layouts <- lapply(list(graph, reordered), layout_metro_dag)
  for (comparison in c(FALSE, TRUE)) {
    plots <- lapply(layouts, function(input) {
      if (comparison) compare_metro_dags(input, list(First = list(), Second = list()),
        labels = FALSE, style = "metro") else
        metro_dag(input, labels = FALSE, arrows = FALSE, legend = FALSE, style = "metro")
    })
    shapes <- vector("list", 2L)
    for (i in seq_along(plots)) {
      expect_no_warning(ggplot2::ggsave(files[i], plots[[i]], width = 260, height = 180, units = "mm"))
      svg <- readLines(files[i])
      shapes[[i]] <- grep("<polyline|<polygon", svg, value = TRUE)
      expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)),
        if (comparison) 2L * igraph::ecount(graph) else 0L)
      expect_equal(sum(grepl("<circle", svg, fixed = TRUE)),
        if (comparison) 2L * igraph::vcount(graph) else igraph::vcount(graph))
    }
    expect_identical(shapes[[1L]], shapes[[2L]])
  }
})
