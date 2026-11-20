.test_route <- function(moves, x = 0, y = 0, from = 'A', to = 'Y', id = 1L) {
  e <- .route_from_moves(x, y, moves)
  e$from <- from; e$to <- to; e$edge_id <- id
  e$segment <- seq_len(nrow(e)); e$path_id <- NA_integer_
  e$bend <- c(FALSE, diff(moves) != 0)
  e[, names(.empty_routes())]
}

test_that("unobstructed staircases reduce to a single bend", {
  e <- .test_route(c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0))
  nodes <- data.frame(node = c('A', 'Y'), x = c(0, 3), y = c(0, -12))
  out <- .straighten_routes(list(e), nodes)[[1L]]
  expect_equal(sum(out$bend), 1L)
  expect_equal(c(out$x[1], out$y[1], tail(out$xend, 1), tail(out$yend, 1)), c(0, 0, 3, -12))
  expect_true(all(out$yend == out$y - 1))
  expect_true(all(abs(out$xend - out$x) %in% 0:1))
})

test_that("a necessary detour around an unrelated node is retained", {
  e <- .test_route(c(1, 1, rep(0, 8), -1, -1))
  nodes <- data.frame(node = c('A', 'Y', 'B'), x = c(0, 0, 0), y = c(0, -12, -6))
  out <- .straighten_routes(list(e), nodes)[[1L]]
  expect_equal(sum(out$bend), 2L)
  expect_true(all(.point_segment_distance(0, -6, out$x, out$y, out$xend, out$yend) >= 0.8))
})

test_that("simplification preserves existing route clarity", {
  # A straight A -> Y would coincide with P -> Q. Keep the clear detour.
  e <- .test_route(c(1, 1, rep(0, 8), -1, -1))
  blocker <- .test_route(rep(0, 8), x = 0, y = -2, from = 'P', to = 'Q', id = 2L)
  nodes <- data.frame(node = c('A', 'Y', 'P', 'Q'), x = 0, y = c(0, -12, -2, -10))
  out <- .straighten_routes(list(e, blocker), nodes)
  expect_equal(.route_conflicts(out[[1L]], .prepare_route_obstacles(out[[2L]])),
    c(overlap = 0, crossings = 0))
  expect_equal(sum(out[[1L]]$bend), 2L)
})

test_that("the publication example avoids unnecessary long-edge wiggles", {
  l <- layout_metro_dag(direction = "down", metrodag_examples('causal'))
  for (from in c('C', 'S')) {
    e <- l$edges[l$edges$from == from & l$edges$to == 'Y', ]
    expect_lte(sum(e$bend), 1L)
  }
  m <- layout_metrics(l)
  expect_lte(m$total_bends, 8L) # Previously 16, including six on S -> Y.
  expect_lte(m$crossings, 1L)
  expect_equal(m$overlap_segments, 0L)
  expect_equal(m$node_edge_collisions, 0L)
})
