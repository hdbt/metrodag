# .route_station_collisions() tests every station against a drawn route at
# once; it must give exactly the answers of the station-by-station test.

# The station-by-station form of .route_station_collisions().
.test_route_station_collisions <- function(xy, nodes, radius, exclude, line_width, arrow_length = 0,
  triangle = .metro_arrow_triangle(xy, arrow_length), outline = 0) {
  segments <- function(z) data.frame(x = head(z[, 1L], -1L), y = head(z[, 2L], -1L),
    xend = tail(z[, 1L], -1L), yend = tail(z[, 2L], -1L))
  route <- segments(xy)
  if (!is.null(triangle)) route <- rbind(route, segments(rbind(triangle, triangle[1L, ])))
  ids <- setdiff(seq_len(nrow(nodes)), exclude)
  radius <- rep_len(radius, nrow(nodes))
  outline <- rep_len(outline, nrow(nodes))
  ids[vapply(ids, function(i) {
    clearance <- outline[i] + line_width / 2
    if (nodes$shape[i] == 22L) {
      rr <- radius[i]
      box <- c(nodes$x[i] - rr, nodes$x[i] + rr, nodes$y[i] - rr, nodes$y[i] + rr)
      any(.segment_rectangle_distance(route, box) <= clearance) || (!is.null(triangle) &&
        any(.points_in_triangle(as.matrix(expand.grid(box[1:2], box[3:4])), triangle)))
    } else {
      rr <- radius[i] + clearance
      any(.point_segment_distance(nodes$x[i], nodes$y[i], route$x, route$y, route$xend, route$yend) <= rr) ||
        (!is.null(triangle) && .points_in_triangle(matrix(c(nodes$x[i], nodes$y[i]), ncol = 2L), triangle))
    }
  }, logical(1))]
}

test_that("route-station collisions match the station-by-station test", {
  set.seed(20261110)
  for (trial in seq_len(400L)) {
    n <- sample(1:10, 1L)
    nodes <- data.frame(x = round(runif(n, 0, 40)), y = round(runif(n, 0, 40)),
      shape = sample(c(21L, 22L), n, replace = TRUE))
    k <- sample(1:5, 1L)
    # Octilinear grid routes touch station rims exactly, as drawn routes do.
    xy <- cbind(cumsum(c(20, sample(c(-4, 0, 4), k - 1L, replace = TRUE))),
      cumsum(c(20, sample(c(-4, 0, 4), k - 1L, replace = TRUE))))
    radius <- if (trial %% 2L) 2 else sample(c(1.5, 2, 3), n, replace = TRUE)
    outline <- if (trial %% 3L) 0.25 else 0
    exclude <- sample(seq_len(n), min(n, sample(0:2, 1L)))
    head <- if (trial %% 4L) 1.5 else 0
    triangle <- .metro_arrow_triangle(xy, head)
    expect_identical(
      .route_station_collisions(xy, nodes, radius, exclude, 0.45, triangle = triangle, outline = outline),
      .test_route_station_collisions(xy, nodes, radius, exclude, 0.45, triangle = triangle, outline = outline))
  }
})
