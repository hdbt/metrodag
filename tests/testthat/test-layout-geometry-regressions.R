.test_incomplete_geometry <- function(fixed) {
  variants <- list()
  a <- fixed; a$edges <- a$edges[-2L, ]; variants$missing_step <- a
  a <- fixed; a$edges$x[2L] <- a$edges$x[2L] + 0.8; variants$disconnected_step <- a
  a <- fixed; a$edges$x[1L] <- a$edges$x[1L] - 1; variants$wrong_source <- a
  a <- fixed; a$edges$xend[4L] <- a$edges$xend[4L] + 1; variants$wrong_target <- a
  a <- fixed; a$nodes$x[1L] <- a$nodes$x[1L] + 0.5; variants$moved_node <- a
  variants
}

test_that("reused layouts cannot invent missing segments or repair detached endpoints", {
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  fixed <- layout_metro_dag(metrodag_examples("chain"), direction = "right")
  sentinel <- ggplot2::ggplot()
  ggplot2::set_last_plot(sentinel)
  variants <- .test_incomplete_geometry(fixed)
  for (name in names(variants)) {
    input <- variants[[name]]
    before <- input
    for (fun in list(metro_dag, plot_metro_dag, ggplot2::autoplot)) {
      expect_error(fun(input), "Layout routes must be continuous", info = name)
      expect_identical(ggplot2::last_plot(), sentinel)
    }
    expect_error(compare_metro_dags(input, list(list())), "Layout routes must be continuous")
    expect_error(layout_metrics(input), "Layout routes must be continuous")
    expect_error(diagnose_metro_dag(input), "Layout routes must be continuous")
    expect_error(save_metro_dag(file, input), "Layout routes must be continuous")
    expect_false(file.exists(file))
    expect_identical(ggplot2::last_plot(), sentinel)
    expect_identical(input, before)
  }
})

test_that("layout coordinates and segment order are validated before rendering or metrics", {
  fixed <- layout_metro_dag(direction = "down", metrodag_examples("chain"))
  cases <- list()
  for (field in c("x", "y", "xend", "yend")) {
    a <- fixed; a$edges[[field]][1L] <- Inf; cases[[paste0("edge_", field)]] <- a
  }
  for (field in c("x", "y")) {
    a <- fixed; a$nodes[[field]][1L] <- NA_real_; cases[[paste0("node_", field)]] <- a
  }
  a <- fixed; a$edges$x <- NULL; cases$missing_coordinate <- a
  a <- fixed; a$nodes$x <- as.character(a$nodes$x); cases$character_coordinate <- a
  a <- fixed; a$edges$x <- a$edges$x + 1i; cases$complex_coordinate <- a
  a <- fixed; a$edges$x <- I(matrix(a$edges$x, ncol = 1L)); cases$matrix_coordinate <- a
  for (input in cases) {
    before <- input
    expect_error(metro_dag(input), "coordinates must be finite real numeric vectors")
    expect_error(layout_metrics(input), "coordinates must be finite real numeric vectors")
    expect_identical(input, before)
  }
  invalid <- list(NULL, rep(NA_real_, nrow(fixed$edges)),
    factor(fixed$edges$segment), rep(1, nrow(fixed$edges)), fixed$edges$segment + 1i,
    I(matrix(fixed$edges$segment, ncol = 1L)))
  for (segment in invalid) {
    input <- fixed; input$edges$segment <- segment
    before <- input
    expect_error(metro_dag(input), "Layout `segment` must contain unique, finite real numeric")
    expect_error(layout_metrics(input), "Layout `segment` must contain unique, finite real numeric")
    expect_identical(input, before)
  }
})

test_that("continuous fractional geometry and opaque ordering values remain valid", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  for (direction in c("right", "left", "down", "up")) {
    fixed <- layout_metro_dag(metrodag_examples("chain"), direction = direction)
    # Scaling coordinates consistently must remain supported. Segment values
    # need only define a unique order within each edge, not consecutive integers.
    for (axis in c("x", "y")) {
      fixed$nodes[[axis]] <- fixed$nodes[[axis]] * 0.1
      fixed$edges[[axis]] <- fixed$edges[[axis]] * 0.1
      end <- paste0(axis, "end")
      fixed$edges[[end]] <- fixed$edges[[end]] * 0.1
    }
    a <- fixed
    a$edges$segment <- 1 + a$edges$segment * 1e-15
    a$edges <- a$edges[nrow(a$edges):1L, ]
    a$nodes <- a$nodes[nrow(a$nodes):1L, ]
    a$graph <- igraph::permute(a$graph, c(3L, 1L, 4L, 2L))
    before <- a
    expect_no_warning(plot <- metro_dag(a, labels = FALSE, legend = FALSE))
    expect_equal(layout_metrics(a), layout_metrics(fixed))
    expect_equal(layout_metrics(a)$total_edge_length, 1.2)
    expect_identical(attr(plot, "metrodag_layout")$nodes, before$nodes)
    expect_identical(attr(plot, "metrodag_layout")$edges, before$edges)
    expect_identical(a, before)
  }
})

test_that("crossing diagnostics on standalone segments still allow genuine gaps", {
  e <- data.frame(edge_id = c("A", "A", "B"), segment = c(1, 2, 1),
    x = c(0, 2, 1.5), y = c(0, 0, -1),
    xend = c(1, 3, 1.5), yend = c(0, 0, 1))
  expect_equal(count_crossings(e), 0L)
  e$x[2L] <- 1
  expect_equal(count_crossings(e), 1L)
})
