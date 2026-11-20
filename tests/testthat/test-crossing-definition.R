.route_rows <- function(id, ...) {
  m <- matrix(c(...), ncol = 2L, byrow = TRUE)
  data.frame(edge_id = id, segment = seq_len(nrow(m) - 1L), x = m[-nrow(m), 1L],
    y = m[-nrow(m), 2L], xend = m[-1L, 1L], yend = m[-1L, 2L])
}

test_that("crossings are side swaps, not forks, merges or touches", {
  count <- function(...) count_crossings(do.call(rbind, list(...)))
  # Two arrows leave one station along a shared step and then part.
  expect_equal(count(.route_rows(1, 0, 0, 1, 0, 2, 1), .route_rows(2, 0, 0, 1, 0, 2, -1)), 0L)
  # Two arrows from different stations merge into one station.
  expect_equal(count(.route_rows(1, 0, 1, 2, 0), .route_rows(2, 0, -1, 2, 0)), 0L)
  expect_equal(count(.route_rows(1, 0, 0, 2, 2), .route_rows(2, 0, 2, 2, 0)), 1L)
  # Arrows from one station that swap sides do cross.
  expect_equal(count(.route_rows(1, 0, 0, 1, 1, 3, -1), .route_rows(2, 0, 0, 1, -1, 3, 1)), 1L)
  # A shared section counts once when it is left on the other side, never
  # when it is left on the side it was entered from.
  expect_equal(count(.route_rows(1, 0, 1, 1, 0, 2, 0, 3, -1), .route_rows(2, 0, -1, 1, 0, 2, 0, 3, 1)), 1L)
  expect_equal(count(.route_rows(1, 0, 1, 1, 0, 2, 0, 3, 1), .route_rows(2, 0, -1, 1, 0, 2, 0, 3, -1)), 0L)
  # Touching at a bend, and consecutive arrows through one station.
  expect_equal(count(.route_rows(1, 0, 1, 1, 0, 2, 1), .route_rows(2, 0, -1, 2, -1)), 0L)
  expect_equal(count(.route_rows(1, 0, 0, 1, 0), .route_rows(2, 1, 0, 2, 1)), 0L)
  # A turn perpendicular to the causal axis that passes another arrow.
  expect_equal(count(.route_rows(1, 0, -1, 1, -1, 1, 1, 2, 1), .route_rows(2, 0, 0, 2, 0)), 1L)
  expect_equal(count(.route_rows(1, 0, 0, 1, 1, 2, 0), .route_rows(2, 0, 0.5, 2, 0.5)), 2L)
  # Routes that double back on both axes keep the count of contact points.
  loop <- .route_rows(1, 0, 0, 2, 2, 0, 2, 2, 0)
  expect_equal(count(loop, .route_rows(2, 1, -1, 1, 3)), 2L)
})

test_that("side swaps read runs, range ends, tiny offsets and gaps correctly", {
  count <- function(...) {
    edges <- do.call(rbind, list(...))
    flipped <- edges
    flipped[c("x", "y", "xend", "yend")] <- edges[c("y", "x", "yend", "xend")]
    # The count does not depend on which axis is causal.
    expect_equal(count_crossings(flipped), count_crossings(edges))
    count_crossings(edges)
  }
  # A run perpendicular to the axis next to a vertex of the other route.
  expect_equal(count(.route_rows(1, 0, 0, 2, 0, 2, 10, 10, 10), .route_rows(2, 0, 5, 4, 5, 10, 6)), 1L)
  expect_equal(count(.route_rows(1, 0, 0, 4, 0, 10, 0.5),
    .route_rows(2, 0, 1, 2, -1, 5, -1, 5, 1, 10, 1)), 2L)
  # Starting or ending on another route's perpendicular run is a touch, but
  # leaving it on the other side is a crossing.
  run <- .route_rows(1, 0, 0, 5, 0, 5, 10, 10, 10)
  expect_equal(count(run, .route_rows(2, 5, 5, 10, 5)), 0L)
  expect_equal(count(run, .route_rows(2, 0, 5, 5, 5)), 0L)
  expect_equal(count(run, .route_rows(2, 5, 5, 10, 15)), 1L)
  expect_equal(count(.route_rows(1, 1, 2, 2, 1, 3, 1, 4, 0), .route_rows(2, 2, 1, 3, 2, 5, 2)), 0L)
  # Crossings closer together than any fixed tolerance still count.
  expect_equal(count(.route_rows(1, -1, -1e-12, 1, 1e-12), .route_rows(2, -1, 1e-12, 1, -1e-12),
    .route_rows(3, -1, 5, 1, 5)), 1L)
  expect_equal(count(.route_rows(1, -1, 1, 0, -1e-10, 1, 1), .route_rows(2, -1, 0, 1, 0)), 2L)
  # Passing through a gap in another route is not a crossing, and each piece
  # of a route with gaps is compared on its own.
  gap <- rbind(.route_rows(1, 0, 0, 4, 0), .route_rows(1, 6, 0, 10, 0))
  gap$segment <- 1:2
  expect_equal(count(gap, .route_rows(2, 0, 1, 2, -1, 4.5, -1, 5.5, 1, 10, 1)), 1L)
  expect_equal(count(gap, .route_rows(2, 0, 1, 7, 1, 8, 0, 9, 1, 10, 1)), 0L)
  expect_equal(count(gap, .route_rows(2, 0, 1, 1, 0, 3, 0, 4, 1, 10, 1)), 0L)
  expect_equal(count(gap, .route_rows(2, 0, 1, 1, 0, 3, 0, 4, -1, 10, -1)), 1L)
  expect_equal(count(gap, .route_rows(2, 0, 1, 6, 1, 7, 0, 10, 0)), 0L)
  # Routes that start or end together on a perpendicular run only touch.
  expect_equal(count(.route_rows(1, 5, 0, 5, 10, 10, 10), .route_rows(2, 5, 5, 10, 5)), 0L)
  expect_equal(count(.route_rows(1, 0, 0, 5, 0, 5, 10), .route_rows(2, 0, 5, 5, 5)), 0L)
  expect_equal(count(.route_rows(1, 5, 0, 5, 10, 10, 10), .route_rows(2, 0, 5, 10, 5)), 1L)
  # Rounding noise from a large offset does not turn a fork into a crossing.
  fork <- rbind(.route_rows(1, 0, 7, 1, 6, 2, 5, 3, 4),
    .route_rows(2, 0, 7, 0.5, 6.5, 1, 6.5, 2, 6.5, 3, 6.5))
  shifted <- fork
  for (key in c("x", "y", "xend", "yend")) shifted[[key]] <- fork[[key]] * 0.1 + 1e4
  expect_equal(count_crossings(fork), 0L)
  expect_equal(count_crossings(shifted), 0L)
})

test_that("layout refinement keeps comparing contact points", {
  layout <- layout_metro_dag(metrodag_examples("dense"))
  expect_gt(.count_route_contacts(layout$edges), count_crossings(layout))
  fork <- rbind(.route_rows(1, 0, 0, 1, 0, 2, 1), .route_rows(2, 0, 0, 1, 0, 2, -1))
  expect_equal(.count_route_contacts(fork), 1L)
  expect_equal(count_crossings(fork), 0L)
})

test_that("a tree has no crossings and the count does not depend on direction", {
  star <- layout_metro_dag(igraph::graph_from_literal(A-+c1, A-+c2, A-+c3, A-+c4))
  expect_equal(count_crossings(star), 0L)
  expect_equal(layout_metrics(star)$crossings, 0L)
  layout <- layout_metro_dag(metrodag_examples("dense"))
  rotated <- layout$edges
  rotated[c("x", "y", "xend", "yend")] <- layout$edges[c("y", "x", "yend", "xend")]
  flipped <- layout$edges
  flipped[c("y", "yend")] <- -layout$edges[c("y", "yend")]
  expect_equal(count_crossings(rotated), count_crossings(layout))
  expect_equal(count_crossings(flipped), count_crossings(layout))
})
