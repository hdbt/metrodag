.test_label_station_geometry <- function(plot) {
  file <- tempfile(fileext = ".svg")
  svglite::svglite(file, width = 85 / 25.4, height = 85 / 25.4)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  state <- new.env(parent = emptyenv())
  state$issues <- state$panels <- list()
  plot <- .metro_diagnostic_plot(plot, state)
  table <- .metro_plot_gtable(plot)
  panel <- table$grobs[[which(table$layout$name == "panel")]]
  grob <- panel$children[[which(vapply(panel$children, inherits,
    logical(1), "metrodag_grob"))]]
  grid::makeContent(grob)$diagnostics
}

.test_label_station_graph <- function() {
  graph <- igraph::make_empty_graph(1L, directed = TRUE)
  igraph::V(graph)$name <- "A"
  graph
}

.test_label_station_export <- function(file, plot) {
  warnings <- list()
  withCallingHandlers(save_metro_dag(file, plot, width = 85, height = 85),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
  warnings
}

test_that("labels at clear circular corners do not report station collisions", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- .test_label_station_graph()
  original <- metro_dag(graph, legend = FALSE)
  geometry <- .test_label_station_geometry(original)
  quadrants <- expand.grid(x = c(-1, 1), y = c(-1, 1))
  for (i in seq_len(nrow(quadrants))) {
    side <- unlist(quadrants[i, ], use.names = FALSE)
    boundary <- c(if (side[1L] > 0) 1L else 2L,
      if (side[2L] > 0) 3L else 4L)
    delta <- c(geometry$nodes$x, geometry$nodes$y) + 1.4 * side -
      geometry$label_boxes[1L, boundary]
    # The padded label box enters the station's enclosing square, but stays
    # outside the circular outline: sqrt(1.4^2 + 1.4^2) > 1.5 + 0.36/2.
    plot <- metro_dag(graph, legend = FALSE, label_nudge = list(A = unname(delta)))
    expect_no_warning(report <- diagnose_metro_dag(plot, width = 85,
      height = 85, device = "svg"))
    expect_true(report$ok)
    expect_equal(nrow(report$issues), 0L)
    expect_no_warning(warnings <- .test_label_station_export(file, plot))
    expect_length(warnings, 0L)
    svg <- readLines(file)
    expect_equal(sum(grepl("<circle", svg, fixed = TRUE)), 1L)
    expect_true(any(grepl(">A</text>", svg, fixed = TRUE)))
  }
})

test_that("label conflicts include the visible circle and square outlines", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- .test_label_station_graph()
  for (square in c(FALSE, TRUE)) for (line_width in c(2, 4)) {
    adjusted <- if (square) "A" else NULL
    base <- metro_dag(graph, adjusted = adjusted, legend = FALSE,
      node_size = 10, line_width = line_width)
    geometry <- .test_label_station_geometry(base)
    outline <- 5 + max(0.32, line_width * 0.8) / 2
    for (overlap in c(TRUE, FALSE)) {
      # With overlap, even the unpadded glyph box starts inside the outline.
      left <- outline + if (overlap) -0.5 else 0.2
      delta <- c(geometry$nodes$x + left - geometry$label_boxes[1L, 1L],
        geometry$nodes$y - geometry$label_centers[1L, 2L])
      plot <- metro_dag(graph, adjusted = adjusted, legend = FALSE,
        node_size = 10, line_width = line_width, label_nudge = list(A = unname(delta)))
      expect_no_warning(report <- diagnose_metro_dag(plot, width = 85,
        height = 85, device = "svg"))
      expect_true(report$ok)
      if (overlap) {
        expect_identical(report$issues$type, "label_station")
        expect_identical(report$issues$severity, "warning")
        expect_identical(report$issues$node, "A")
        expect_identical(report$issues$other_node, "A")
        expect_no_warning(warnings <- .test_label_station_export(file, plot))
        expect_length(warnings, 1L)
        if (length(warnings) == 1L) {
          expect_s3_class(warnings[[1L]], "metrodag_render_warning")
          expect_match(conditionMessage(warnings[[1L]]), "labels overlap stations")
        }
        expect_gt(file.info(file)$size, 100)
      } else {
        expect_equal(nrow(report$issues), 0L)
        expect_no_warning(warnings <- .test_label_station_export(file, plot))
        expect_length(warnings, 0L)
      }
    }
  }
})

test_that("label-station intersections respect shape, radius and boundaries", {
  nodes <- data.frame(x = c(0, 0, 5), y = 0, shape = c(21L, 22L, 21L))
  radius <- rep(1, 3L)
  corner <- c(0.8, 1.5, 0.8, 1.5)
  expect_identical(.metro_label_station_hits(nodes, corner, radius), 2L)
  expect_identical(.metro_label_station_hits(nodes, corner, c(1.5, 0.7, 1)), 1L)
  expect_identical(.metro_label_station_hits(nodes, c(-0.1, 0.1, -0.1, 0.1), radius), 1:2)
  tangent <- c(1, 2, -0.25, 0.25)
  expect_identical(.metro_label_station_hits(nodes, tangent, radius), integer())
  expect_identical(.metro_label_station_hits(nodes, tangent, c(1.2, 1.2, 1)), 1:2)
  expect_identical(.metro_label_station_hits(nodes[FALSE, ], corner, numeric()), integer())
  for (scale in c(0.5, 1, 4)) for (sx in c(-1, 1)) for (sy in c(-1, 1)) {
    shifted <- nodes
    shifted$x <- shifted$x * sx * scale + 12
    shifted$y <- shifted$y * sy * scale - 8
    box <- corner * c(sx, sx, sy, sy) * scale + c(12, 12, -8, -8)
    box <- c(sort(box[1:2]), sort(box[3:4]))
    expect_identical(.metro_label_station_hits(shifted, box, radius * scale), 2L)
  }
})

test_that("automatic label placement clears thick station outlines", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- .test_label_station_graph()
  for (square in c(FALSE, TRUE)) for (line_width in c(0.45, 2, 4)) {
    plot <- metro_dag(graph, adjusted = if (square) "A" else NULL,
      legend = FALSE, node_size = 10, line_width = line_width)
    geometry <- .test_label_station_geometry(plot)
    box <- geometry$label_boxes[1L, ]
    x <- geometry$nodes$x; y <- geometry$nodes$y
    outline <- 5 + max(0.32, line_width * 0.8) / 2
    # The one-node fixture places its label along a horizontal/vertical axis.
    gap <- max(c(box[1L] - x, x - box[2L], box[3L] - y, y - box[4L])) - outline
    expect_gt(gap, 0.4)
    expect_no_warning(report <- diagnose_metro_dag(plot, width = 85,
      height = 85, device = "svg"))
    expect_true(report$ok)
    expect_equal(nrow(report$issues), 0L)
    expect_no_warning(warnings <- .test_label_station_export(file, plot))
    expect_length(warnings, 0L)
  }
})

test_that("comparison reports distinguish circular and square station corners", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  graph <- .test_label_station_graph()
  geometry <- .test_label_station_geometry(metro_dag(graph, legend = FALSE))
  delta <- c(geometry$nodes$x, geometry$nodes$y) + 1.4 -
    geometry$label_boxes[1L, c(1L, 3L)]
  comparison <- compare_metro_dags(graph,
    list(Circle = list(), Square = list(adjusted = "A")),
    label_nudge = list(A = unname(delta)))
  for (units in c("mm", "cm", "in")) {
    factor <- switch(units, mm = 1, cm = 10, `in` = 25.4)
    expect_no_warning(report <- diagnose_metro_dag(comparison,
      width = 180 / factor, height = 120 / factor, units = units, device = "svg"))
    expect_true(report$ok)
    expect_identical(report$issues$type, "label_station")
    expect_identical(report$issues$panel, "2")
    expect_identical(report$issues$node, "A")
    expect_identical(report$issues$other_node, "A")
    expect_equal(unname(report$dimensions), c(180, 120), tolerance = 0.1)
  }
})
