.test_stationary_steps <- function(layout) {
  pieces <- lapply(unique(layout$edges$edge_id), function(id) {
    z <- layout$edges[layout$edges$edge_id == id, , drop = FALSE]
    z <- z[order(z$segment), , drop = FALSE]
    point <- function(row) {
      row$xend <- row$x; row$yend <- row$y; row$bend <- FALSE
      row
    }
    middle <- max(1L, nrow(z) %/% 2L)
    end <- tail(z, 1L); end$x <- end$xend; end$y <- end$yend
    end$bend <- FALSE
    interior <- z[middle, ]; interior$x <- interior$xend; interior$y <- interior$yend
    interior$bend <- FALSE
    after <- which(seq_len(nrow(z)) > middle)
    result <- rbind(point(z[1L, ]), z[seq_len(middle), ],
      interior, z[after, ], end)
    result$segment <- seq_len(nrow(result)) / 10
    result
  })
  layout$edges <- do.call(rbind, pieces)
  layout$edges <- layout$edges[nrow(layout$edges):1L, , drop = FALSE]
  layout
}

test_that("redundant stationary steps preserve fanned attachments and exports", {
  skip_if_not_installed("svglite")
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  previous <- ggplot2::last_plot()
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- metrodag_examples("diamond")
  for (direction in c("right", "down", "left", "up")) {
    fixed <- layout_metro_dag(graph, direction = direction)
    input <- .test_stationary_steps(fixed)
    before <- input
    for (square in c(FALSE, TRUE)) {
      adjusted <- if (square) igraph::V(graph)$name else character()
      plots <- lapply(list(fixed, input), metro_dag, adjusted = adjusted,
        labels = FALSE, legend = FALSE)
      for (i in seq_along(plots)) {
        expect_no_warning(save_metro_dag(files[i], plots[[i]], width = 180, height = 120))
        expect_no_warning(report <- diagnose_metro_dag(plots[[i]], width = 180,
          height = 120, device = "svg"))
        expect_true(report$ok, info = paste(direction, square))
        expect_equal(nrow(report$issues), 0L)
      }
      expect_identical(.svg_primitives(files[2L]),
        .svg_primitives(files[1L]), info = paste(direction, square))
      expect_equal(.svg_count(files[2L], "polygon"),
        igraph::ecount(graph))
      expect_identical(attr(plots[[2L]], "metrodag_layout")$edges, before$edges)
    }
    expect_equal(layout_metrics(input), layout_metrics(fixed))
    expect_identical(input, before)
  }
})

test_that("ordinary ggsave and comparison panels ignore redundant zero steps", {
  skip_if_not_installed("svglite")
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  previous <- ggplot2::last_plot()
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  fixed <- layout_metro_dag(metrodag_examples("chain"), direction = "left")
  input <- .test_stationary_steps(fixed)
  input$edges$edge_id <- factor(input$edges$edge_id, levels = c("unused", 3:1))
  before <- input
  plots <- lapply(list(fixed, input), function(layout) suppressMessages(
    metro_dag(layout, adjusted = c("A", "D"), legend = FALSE, arrows = FALSE) +
      ggplot2::coord_flip(clip = "off") + ggplot2::scale_x_reverse()))
  for (i in seq_along(plots)) expect_no_warning(ggplot2::ggsave(files[i], plots[[i]],
    width = 180, height = 120, units = "mm"))
  expect_identical(.svg_primitives(files[2L]), .svg_primitives(files[1L]))
  views <- list(Circles = list(), Squares = list(adjusted = c("A", "D")))
  plots <- lapply(list(fixed, input), compare_metro_dags, views = views, labels = FALSE)
  for (i in seq_along(plots)) expect_no_warning(save_metro_dag(files[i], plots[[i]],
    width = 260, height = 120))
  expect_identical(.svg_primitives(files[2L]), .svg_primitives(files[1L]))
  expect_equal(.svg_count(files[2L], "polygon"), 6L)
  expect_identical(input, before)
})

test_that("stationary interior steps do not inflate shared straight-run counts", {
  graph <- igraph::graph_from_edgelist(matrix(c("A", "B", "C", "D"),
    ncol = 2L, byrow = TRUE), directed = TRUE)
  fixed <- layout_metro_dag(graph, direction = "up")
  fixed$nodes$x <- 0
  fixed$nodes$y <- c(A = 0, B = 8, C = 2, D = 6)[fixed$nodes$node]
  fixed$edges <- data.frame(from = c("A", "A", "C"), to = c("B", "B", "D"),
    edge_id = c(1L, 1L, 2L), segment = c(1, 3, 1), x = 0, y = c(0, 4, 2),
    xend = 0, yend = c(4, 8, 6), bend = FALSE, path_id = NA_integer_)
  point <- fixed$edges[2L, ]; point$yend <- point$y; point$segment <- 2
  input <- fixed
  input$edges <- rbind(fixed$edges[1L, ], point, fixed$edges[2:3, ])
  before <- input
  expect_equal(layout_metrics(fixed)$overlap_segments, 1L)
  expect_equal(layout_metrics(input)$overlap_segments, 1L)
  expect_equal(layout_metrics(input), layout_metrics(fixed))
  expect_identical(input, before)
})

test_that("entirely collapsed routes report edge identities and protect saved files", {
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".pdf")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  input <- layout_metro_dag(metrodag_examples("chain"), direction = "right")
  b <- input$nodes[input$nodes$node == "B", ]
  input$nodes[input$nodes$node == "A", c("x", "y")] <- b[, c("x", "y")]
  rows <- input$edges$from == "A"
  input$edges$x[rows] <- input$edges$xend[rows] <- b$x
  input$edges$y[rows] <- input$edges$yend[rows] <- b$y
  before <- input
  for (adjusted in list(character(), c("A", "B"))) {
    plot <- metro_dag(input, adjusted = adjusted, labels = FALSE, legend = FALSE)
    expect_no_warning(report <- diagnose_metro_dag(plot, width = 180, height = 120))
    expect_false(report$ok)
    issues <- report$issues[report$issues$type == "edge_zero_length", ]
    expect_equal(nrow(issues), 1L)
    expect_equal(issues$from, "A")
    expect_equal(issues$to, "B")
    expect_true(all(grepl("Recompute", issues$suggestion)))
    expect_false(any(report$issues$type == "render_error"))
    .expect_failed_export_keeps_file(file, function(file) save_metro_dag(file, plot,
      width = 180, height = 120), "has zero length")
    expect_identical(ggplot2::last_plot(), plot)
  }
  expect_identical(input, before)
})
