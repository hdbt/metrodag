.test_scale_crossing_routes <- function(edges, scale, offset = c(0, 0)) {
  for (axis in c("x", "y")) for (key in c(axis, paste0(axis, "end")))
    edges[[key]] <- edges[[key]] * scale + offset[if (axis == "x") 1L else 2L]
  edges
}

test_that("crossing counts survive tiny, huge and translated coordinates", {
  edges <- data.frame(edge_id = c("first", "second"),
    x = c(-1, -1), y = c(-1, 1), xend = c(1, 1), yend = c(1, -1))
  for (scale in c(1e-150, 1e-6, 1, 1e150, 1e200, .Machine$double.xmax)) {
    input <- .test_scale_crossing_routes(edges, scale)
    original <- input
    expect_no_warning(value <- count_crossings(input))
    expect_equal(value, 1L, info = paste("scale", scale))
    expect_identical(input, original)
  }
  for (offset in list(c(1e12, -1e12), c(-1e12, 1e12), c(10, -20))) {
    input <- .test_scale_crossing_routes(edges, 1, offset)
    expect_equal(count_crossings(input), 1L)
  }
  integer_edges <- edges
  for (key in c("x", "y", "xend", "yend"))
    integer_edges[[key]] <- as.integer(integer_edges[[key]] * .Machine$integer.max)
  expect_no_warning(value <- count_crossings(integer_edges))
  expect_equal(value, 1L)
})

test_that("nearby crossings of the same edge pair stay distinct", {
  # One horizontal route crosses the two vertical sides of a narrow arch.
  edges <- data.frame(edge_id = c("straight", "arch", "arch", "arch"),
    segment = c(1, 1, 2, 3), x = c(-1, -1e-10, -1e-10, 1e-10),
    y = c(0, -1, 1, 1), xend = c(1, -1e-10, 1e-10, 1e-10),
    yend = c(0, 1, 1, -1))
  for (scale in c(1e-100, 1e-6, 1, 1e100)) {
    input <- .test_scale_crossing_routes(edges, scale)
    expect_no_warning(value <- count_crossings(input))
    expect_equal(value, 2L)
    expect_equal(count_crossings(input[c(4, 2, 1, 3), ]), 2L)
  }
})

test_that("close original endpoints do not erase an internal crossing", {
  # The two endpoints are close to the intersection, but they are distinct.
  edges <- data.frame(edge_id = c("horizontal", "vertical"),
    x = c(-1, 0), y = c(0, -1), xend = c(1e-9, 0), yend = c(0, 1e-9))
  for (scale in c(1e-100, 1e-6, 1, 1e100)) {
    input <- .test_scale_crossing_routes(edges, scale)
    expect_equal(count_crossings(input), 1L)
    input$xend[1L] <- input$yend[2L] <- 0
    expect_equal(count_crossings(input), 0L)
  }
})

test_that("an intersection beyond a route endpoint is not a crossing", {
  edges <- data.frame(edge_id = c("horizontal", "vertical"),
    x = c(0, 1 + 5e-11), y = c(0, -1),
    xend = c(1, 1 + 5e-11), yend = c(0, 1))
  for (scale in c(1e-100, 1e-6, 1, 1e100)) {
    input <- .test_scale_crossing_routes(edges, scale)
    expect_equal(count_crossings(input), 0L)
    input$x[2L] <- input$xend[2L] <- scale
    # A terminal touching the middle of another edge is a crossing.
    expect_equal(count_crossings(input), 1L)
  }
})

test_that("bend contacts are deduplicated and shared terminals remain excluded", {
  # A route that crosses another exactly at its bend counts once.
  bends <- data.frame(edge_id = c("zigzag", "zigzag", "across"),
    segment = c(1, 2, 1), x = c(-1, 0, -1), y = c(-1, 0, 0),
    xend = c(0, 1, 1), yend = c(0, 1, 0))
  # A bend that only touches another route is not a crossing.
  touch <- bends
  touch$yend[2L] <- -1
  shared <- data.frame(edge_id = c("one", "two"),
    x = c(-1, 1), y = c(-1, -1), xend = 0, yend = 0)
  overlaps <- data.frame(edge_id = c("one", "two"),
    x = c(-1, -0.5), y = 0, xend = c(0.5, 1), yend = 0)
  for (scale in c(1e-150, 1e-6, 1, 1e150, 1e200)) {
    expect_equal(count_crossings(.test_scale_crossing_routes(bends, scale)), 1L)
    expect_equal(count_crossings(.test_scale_crossing_routes(touch, scale)), 0L)
    expect_equal(count_crossings(.test_scale_crossing_routes(shared, scale)), 0L)
    expect_equal(count_crossings(.test_scale_crossing_routes(overlaps, scale)), 0L)
  }
})

test_that("finite huge-coordinate bends are not collapsed after overflow", {
  edges <- data.frame(edge_id = c("bent", "bent", "across"), segment = c(1, 2, 1),
    x = c(-1, 0.75, 0.84), y = c(-1, 0.5, 0.6),
    xend = c(0.75, 1, 0.86), yend = c(0.5, 0.75, 0.6))
  for (scale in c(1, 2^1023 * 1.75)) {
    input <- .test_scale_crossing_routes(edges, scale)
    expect_no_warning(value <- count_crossings(input))
    expect_equal(value, 1L)
    expect_equal(nrow(.collapse_routes(input)), 3L)
  }
})

test_that("close nonparallel directions still contribute a crossing", {
  edges <- data.frame(edge_id = c("first", "second"),
    x = -1, y = c(-1e-12, 1e-12), xend = 1, yend = c(1e-12, -1e-12))
  for (scale in c(1e-100, 1e-6, 1, 1e100)) {
    expect_equal(count_crossings(.test_scale_crossing_routes(edges, scale)), 1L)
  }
})

test_that("canonical layout crossing counts retain unit and direction invariance", {
  for (kind in c("fork", "diamond", "causal", "dense")) {
    # The same geometry as a fresh layout in each direction (test-layout.R).
    layouts <- .test_layouts_by_direction(metrodag_examples(kind))
    reference <- count_crossings(layouts$down)
    for (direction in .test_directions) {
      layout <- layouts[[direction]]
      original <- layout
      expect_equal(count_crossings(layout), reference)
      expect_identical(layout, original)
      for (scale in c(1e-100, 1e-6, 1e100, 1e200)) {
        scaled <- layout
        scaled$edges <- .test_scale_crossing_routes(layout$edges, scale)
        expect_no_warning(value <- count_crossings(scaled))
        expect_equal(value, reference)
        expect_identical(layout, original)
      }
    }
  }
})

test_that("duplicate geometric contacts and zero-length routes remain harmless", {
  loop <- data.frame(edge_id = c("straight", rep("loop", 5)), segment = c(1, 1:5),
    x = c(-2, 0, 0, 1, 1, 0), y = c(0, -1, 1, 1, -1, -1),
    xend = c(2, 0, 1, 1, 0, 0), yend = c(0, 1, 1, -1, -1, 1))
  for (scale in c(1e-100, 1, 1e100)) {
    input <- .test_scale_crossing_routes(loop, scale)
    expect_equal(count_crossings(input), 2L)
    expect_equal(count_crossings(input[c(6, 4, 2, 1, 3, 5), ]), 2L)
  }
  points <- data.frame(edge_id = 1:2, x = 1e308, y = -1e308,
    xend = 1e308, yend = -1e308)
  expect_no_warning(value <- count_crossings(points))
  expect_identical(value, 0L)
  points <- data.frame(edge_id = c("line", "point"), x = c(-1, 0), y = 0,
    xend = c(1, 0), yend = 0)
  expect_identical(count_crossings(points), 0L)
})
