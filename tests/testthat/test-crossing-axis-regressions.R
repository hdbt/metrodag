.test_axis_crossing <- function(width, height) data.frame(
  edge_id = c("first", "second"), x = c(0, 0), y = c(0, height),
  xend = c(width, width), yend = c(height, 0))

test_that("distant zero-length segments cannot erase genuine crossings", {
  for (scale in c(1e-150, 1, 1e150)) {
    edges <- .test_axis_crossing(scale, scale)
    for (distance in c(1e15, 1e100, .Machine$double.xmax)) {
      points <- data.frame(edge_id = c("point", "other point"),
        x = c(distance, -distance), y = c(-distance, distance),
        xend = c(distance, -distance), yend = c(-distance, distance))
      input <- rbind(edges, points)
      original <- input
      for (rows in list(seq_len(4L), c(4L, 2L, 3L, 1L))) {
        expect_no_warning(value <- count_crossings(input[rows, ]))
        expect_equal(value, 1L, info = paste(scale, distance))
      }
      expect_identical(input, original)
    }
  }
})

test_that("independent axis scales retain shallow and steep crossings", {
  for (width in c(1e-150, 1, 1e150, .Machine$double.xmax))
    for (height in c(1e-150, 1, 1e150, .Machine$double.xmax)) {
      edges <- .test_axis_crossing(width, height)
      original <- edges
      expect_no_warning(value <- count_crossings(edges))
      expect_equal(value, 1L, info = paste(width, height))
      expect_identical(edges, original)
      expect_equal(count_crossings(edges[2:1, ]), 1L)
      # A shared destination is still an original endpoint, not a crossing.
      shared <- edges
      shared$yend <- height / 2
      expect_equal(count_crossings(shared), 0L)
      # Collinear tracks and separated parallel tracks remain non-crossings.
      parallel <- edges
      parallel$yend <- parallel$y
      expect_equal(count_crossings(parallel), 0L)
      overlap <- edges
      overlap$y <- overlap$yend <- 0
      expect_equal(count_crossings(overlap), 0L)
    }
})

test_that("zero-length terminal steps keep the route's original endpoints", {
  for (scale in c(1e-150, 1, 1e150)) {
    edges <- data.frame(edge_id = c("one", "one", "two", "two"),
      segment = c(1, 2, 1, 2), x = c(-scale, 0, scale, 0),
      y = c(-scale, 0, -scale, 0), xend = 0, yend = 0)
    expect_equal(count_crossings(edges), 0L)
    edges$segment <- c(2, 1, 2, 1)
    edges$x[2L] <- edges$y[2L] <- 100 * scale
    edges$xend[2L] <- edges$yend[2L] <- 100 * scale
    edges$x[4L] <- edges$y[4L] <- -100 * scale
    edges$xend[4L] <- edges$yend[4L] <- -100 * scale
    # Each original source is now a separate zero-length fragment. The active
    # routes still share their original destination at the origin.
    expect_equal(count_crossings(edges), 0L)
    edges$segment <- c(1, 2, 1, 2)
    # The origin is internal to both routes when their original destinations
    # are the isolated points instead, so the contact contributes a crossing.
    expect_equal(count_crossings(edges), 1L)
  }
})
