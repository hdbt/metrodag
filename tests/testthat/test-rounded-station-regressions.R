.test_rounded_station_graph <- function(n = 1L) {
  graph <- igraph::make_empty_graph(n, directed = TRUE)
  igraph::V(graph)$name <- LETTERS[seq_len(n)]
  graph
}

.test_rounded_station_grob <- function(plot, linejoin = "round") {
  file <- tempfile(fileext = ".svg")
  svglite::svglite(file, width = 85 / 25.4, height = 85 / 25.4)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  grid::pushViewport(grid::viewport(gp = grid::gpar(linejoin = linejoin)))
  state <- new.env(parent = emptyenv())
  state$issues <- state$panels <- list()
  table <- .metro_plot_gtable(.metro_diagnostic_plot(plot, state))
  panel <- table$grobs[[which(table$layout$name == "panel")]]
  grob <- panel$children[[which(vapply(panel$children, inherits,
    logical(1), "metrodag_grob"))]]
  grid::makeContent(grob)
}

.test_rounded_station_export <- function(file, plot) {
  warnings <- list()
  withCallingHandlers(save_metro_dag(file, plot, width = 85, height = 85),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
  warnings
}

test_that("clear rounded square corners allow station diagnostics and exports", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  layout <- layout_metro_dag(direction = "down", .test_rounded_station_graph(2L), rank = c(0, 1))
  layout$nodes$x <- layout$nodes$y <- c(0, 1)
  layout$nodes$label_x <- layout$nodes$label_y <- c(0, 1)
  for (mixed in c(FALSE, TRUE)) for (line_width in c(0.45, 2, 4)) {
    adjusted <- if (mixed) "A" else c("A", "B")
    probe <- metro_dag(layout, adjusted = adjusted, labels = FALSE,
      legend = FALSE, line_width = line_width)
    report <- diagnose_metro_dag(probe, width = 85, height = 85, device = "svg")
    # Use the trained range so these physical corner contacts do not depend
    # on the plot's default expansion.
    panel <- ggplot2::ggplot_build(probe)$layout$panel_params[[1L]]
    spacing <- report$panels$width[1L] / diff(panel$x.range)
    stroke <- max(0.32, line_width * 0.8)
    for (overlap in c(FALSE, TRUE)) {
      size <- if (mixed) 2 * (sqrt(2) * spacing / (sqrt(2) + 1) -
        (if (overlap) 0.7 else 0.9) * stroke / 2) else
        spacing - (if (overlap) 0.6 else 0.75) * stroke
      plot <- metro_dag(layout, adjusted = adjusted, labels = FALSE,
        legend = FALSE, node_size = size, line_width = line_width)
      expect_no_warning(report <- diagnose_metro_dag(plot, width = 85,
        height = 85, device = "svg"))
      if (overlap) {
        expect_false(report$ok)
        expect_identical(report$issues$type, "station_overlap")
        expect_identical(report$issues$node, "A")
        expect_identical(report$issues$other_node, "B")
        unlink(file)
        expect_error(.test_rounded_station_export(file, plot),
          "overlapping pair")
        expect_false(file.exists(file))
      } else {
        expect_true(report$ok)
        expect_equal(nrow(report$issues), 0L)
        expect_no_warning(save_metro_dag(file, plot, width = 85, height = 85))
        expect_gt(file.info(file)$size, 100)
      }
    }
  }
})

test_that("nudged labels distinguish clear and overlapping rounded square corners", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- .test_rounded_station_graph()
  quadrants <- expand.grid(x = c(-1, 1), y = c(-1, 1))
  for (line_width in c(2, 4)) {
    base <- metro_dag(graph, adjusted = "A", legend = FALSE,
      node_size = 10, line_width = line_width)
    geometry <- .test_rounded_station_grob(base)$diagnostics
    half_stroke <- max(0.32, line_width * 0.8) / 2
    for (i in seq_len(nrow(quadrants))) for (overlap in c(FALSE, TRUE)) {
      side <- unlist(quadrants[i, ], use.names = FALSE)
      boundary <- c(if (side[1L] > 0) 1L else 2L,
        if (side[2L] > 0) 3L else 4L)
      gap <- (if (overlap) 0.6 else 0.9) * half_stroke
      delta <- c(geometry$nodes$x, geometry$nodes$y) + (5 + gap) * side -
        geometry$label_boxes[1L, boundary]
      plot <- metro_dag(graph, adjusted = "A", legend = FALSE,
        node_size = 10, line_width = line_width, label_nudge = list(A = unname(delta)))
      expect_no_warning(report <- diagnose_metro_dag(plot, width = 85,
        height = 85, device = "svg"))
      expect_true(report$ok)
      warnings <- .test_rounded_station_export(file, plot)
      expect_gt(file.info(file)$size, 100)
      if (overlap) {
        expect_identical(report$issues$type, "label_station")
        expect_identical(report$issues$node, "A")
        expect_identical(report$issues$other_node, "A")
        expect_identical(report$issues$severity, "warning")
        expect_length(warnings, 1L)
        if (length(warnings) == 1L)
          expect_s3_class(warnings[[1L]], "metrodag_render_warning")
      } else {
        expect_equal(nrow(report$issues), 0L)
        expect_length(warnings, 0L)
      }
    }
  }
})

test_that("routes can clear the rounded corner of a square outline", {
  node <- data.frame(x = 0, y = 0, shape = 22L)
  route <- rbind(c(1.35, 1.35), c(1.39, 1.35))
  # The nearest filled-square corner is sqrt(2)*0.35 away, more than the
  # combined 0.2 mm station half-stroke and 0.2 mm route half-stroke.
  expect_length(.route_station_collisions(route, node, 1, integer(), 0.4,
    outline = 0.2), 0L)
  route <- rbind(c(1.2, 1.2), c(1.3, 1.2))
  expect_identical(.route_station_collisions(route, node, 1, integer(), 0.4,
    outline = 0.2), 1L)
  # An outline-only contact must still be detected without a route stroke.
  expect_identical(.route_station_collisions(route, node, 1, integer(), 0,
    outline = 0.3), 1L)
  expect_length(.route_station_collisions(route, node, 1, 1L, 0.4,
    outline = 0.2), 0L)
  # The filled arrowhead contains the whole square, without boundary contact.
  triangle <- rbind(c(-4, -3), c(4, -3), c(0, 5))
  far_route <- rbind(c(5, 5), c(6, 6))
  expect_identical(.route_station_collisions(far_route, node, 1, integer(), 0,
    triangle = triangle), 1L)
  # A head wing can touch only the rounded outline, with its shaft clear.
  triangle <- rbind(c(1.2, 1.2), c(1.5, 1.2), c(1.2, 1.5))
  expect_identical(.route_station_collisions(far_route, node, 1, integer(), 0,
    triangle = triangle, outline = 0.3), 1L)
  expect_length(.route_station_collisions(far_route, node, 1, integer(), 0,
    triangle = triangle, outline = 0.2), 0L)
  # A long parallel segment can have its closest points along a square side.
  parallel <- rbind(c(-5, 1.4), c(5, 1.4))
  expect_identical(.route_station_collisions(parallel, node, 1, integer(), 0.4,
    outline = 0.2), 1L)
  parallel[, 2L] <- 1.5
  expect_length(.route_station_collisions(parallel, node, 1, integer(), 0.4,
    outline = 0.2), 0L)
  point <- rbind(c(1.25, 1.25), c(1.25, 1.25))
  expect_identical(.route_station_collisions(point, node, 1, integer(), 0.4,
    outline = 0.2), 1L)
})

test_that("rendered routes clear rounded square corners without losing real collisions", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- igraph::add_edges(.test_rounded_station_graph(3L), c("A", "B"))
  layout <- layout_metro_dag(direction = "down", graph)
  layout$nodes$x <- layout$nodes$label_x <- c(-4, 6, 0)
  layout$nodes$y <- layout$nodes$label_y <- c(6, -4, 0)
  # The fixture is one complete diagonal, not repeated disconnected copies
  # of that diagonal in every row of the original unit-step route.
  layout$edges <- layout$edges[1L, , drop = FALSE]
  layout$edges$segment <- 1L
  layout$edges$bend <- FALSE
  layout$edges$x <- -4; layout$edges$y <- 6
  layout$edges$xend <- 6; layout$edges$yend <- -4
  for (line_width in c(0.45, 2)) for (arrows in c(FALSE, TRUE)) {
    probe <- metro_dag(layout, adjusted = "C", labels = FALSE,
      legend = FALSE, line_width = line_width, arrows = arrows)
    report <- diagnose_metro_dag(probe, width = 85, height = 85, device = "svg")
    panel <- ggplot2::ggplot_build(probe)$layout$panel_params[[1L]]
    step <- report$panels$width[1L] / diff(panel$x.range)
    clearance <- max(0.32, line_width * 0.8) / 2 + line_width / 2
    for (overlap in c(FALSE, TRUE)) {
      # The diagonal route x+y=2 passes the square's upper-right corner.
      gap <- (if (overlap) 0.6 else 0.9) * clearance
      plot <- metro_dag(layout, adjusted = "C", labels = FALSE, legend = FALSE,
        line_width = line_width, arrows = arrows, node_size = 2 * (step - gap))
      expect_no_warning(report <- diagnose_metro_dag(plot, width = 85,
        height = 85, device = "svg"))
      if (overlap) {
        expect_false(report$ok)
        expect_identical(report$issues$type, "edge_station")
        expect_identical(report$issues$node, "C")
        expect_identical(report$issues$from, "A")
        expect_identical(report$issues$to, "B")
        unlink(file)
        expect_error(.test_rounded_station_export(file, plot), "unrelated station C")
        expect_false(file.exists(file))
      } else {
        expect_true(report$ok)
        expect_equal(nrow(report$issues), 0L)
        expect_no_warning(warnings <- .test_rounded_station_export(file, plot))
        expect_length(warnings, 0L)
        svg <- readLines(file)
        expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), as.integer(arrows))
        expect_gt(file.info(file)$size, 100)
      }
    }
  }
})

test_that("rounded station checks retain fill intersections and tangencies", {
  for (scale in c(0.5, 1, 4)) for (sx in c(-1, 1)) for (sy in c(-1, 1)) {
    node <- data.frame(x = 12, y = -8, shape = 22L)
    box <- function(gap) {
      b <- c(1 + gap, 2, 1 + gap, 2) * c(sx, sx, sy, sy) * scale +
        c(12, 12, -8, -8)
      c(sort(b[1:2]), sort(b[3:4]))
    }
    expect_identical(.metro_label_station_hits(node, box(0.3), scale, 0.4 * scale),
      integer())
    expect_identical(.metro_label_station_hits(node, box(0.2), scale, 0.4 * scale), 1L)
    expect_identical(.metro_label_station_hits(node, box(0.4 / sqrt(2)),
      scale, 0.4 * scale), integer())
    expect_identical(.metro_label_station_hits(node, box(-0.1), scale), 1L)
    second <- node
    second$x <- 12 + 2.3 * sx * scale
    second$y <- -8 + 2.3 * sy * scale
    pair <- rbind(node, second)
    expect_equal(nrow(.station_overlap_pairs(pair, scale, 0.2 * scale)), 0L)
    expect_equal(nrow(.station_overlap_pairs(pair, scale, c(0.1, 0.4) * scale)), 1L)
  }
  pair <- data.frame(x = c(0, 2.72), y = c(0, 2.72), shape = c(22L, 21L))
  expect_equal(nrow(.station_overlap_pairs(pair, c(1, 2), c(0.3, 0.1))), 0L)
  expect_equal(nrow(.station_overlap_pairs(pair[2:1, ], c(2, 1), c(0.1, 0.3))), 0L)
  pair$x[2L] <- pair$y[2L] <- 2.6
  expect_equal(nrow(.station_overlap_pairs(pair, c(1, 2), c(0.3, 0.1))), 1L)
  pair$shape <- 21L
  pair$x[2L] <- 3.4; pair$y[2L] <- 0
  expect_equal(nrow(.station_overlap_pairs(pair, c(1, 2), c(0.3, 0.1))), 0L)
  pair$x[2L] <- 3.3
  expect_equal(nrow(.station_overlap_pairs(pair, c(1, 2), c(0.3, 0.1))), 1L)
})

test_that("station and legend joins stay rounded in a mitre viewport", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  plot <- metro_dag(.test_rounded_station_graph(2L), adjusted = "A",
    unobserved = "B", labels = FALSE, legend = FALSE)
  grob <- .test_rounded_station_grob(plot, linejoin = "mitre")
  # The graph has no arrows: the polygon is B's dashed outline.
  stations <- Filter(function(g) inherits(g, "rect") || inherits(g, "circle") ||
    inherits(g, "polygon"), grob$children)
  expect_length(stations, 2L)
  expect_true(all(vapply(stations, function(g) identical(g$gp$linejoin, "round"), logical(1))))
  for (linetype in c("solid", "dashed")) for (shape in c(21L, 22L)) {
    key <- .draw_key_metro_node(list(colour = "black", fill = "white",
      linetype = linetype, shape = shape), list(), 4)
    expect_identical(key$gp$linejoin, "round")
  }
})
