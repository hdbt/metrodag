test_that("factor edge IDs ignore unused levels in both metric APIs", {
  edges <- data.frame(edge_id = c("lower", "lower", "upper", "upper"),
    segment = c(1, 2, 1, 2), x = c(0, 1, 2, 1), y = c(2, 1, 2, 1),
    xend = c(1, 2, 1, 0), yend = c(1, 0, 1, 0))
  for (make_factor in list(factor, ordered)) {
    input <- edges
    input$edge_id <- make_factor(input$edge_id,
      levels = c("unused", "upper", "lower", "another unused"))
    original <- input
    expect_no_warning(value <- count_crossings(input))
    expect_equal(value, 1L)
    expect_identical(input, original)
    layout <- layout_metro_dag(direction = "down", metrodag_examples("diamond"))
    reference <- layout_metrics(layout)
    layout$edges$edge_id <- make_factor(layout$edges$edge_id,
      levels = c("unused", seq_len(igraph::ecount(layout$graph))))
    original <- layout
    expect_no_warning(result <- layout_metrics(layout))
    expect_equal(result, reference)
    expect_true(is.finite(result$maximum_edge_length))
    expect_identical(layout, original)
  }
})

test_that("crossing metrics do not invent tracks across gaps", {
  edges <- data.frame(edge_id = c("broken", "broken", "crossing"),
    segment = c(1, 2, 1), x = c(0, 4, 2), y = c(0, 0, -1),
    xend = c(1, 5, 2), yend = c(0, 0, 1))
  original <- edges
  expect_equal(count_crossings(edges), 0L)
  expect_identical(edges, original)
  # Rotate the same disconnected tracks by swapping axes.
  vertical <- edges
  vertical$x <- edges$y; vertical$y <- edges$x
  vertical$xend <- edges$yend; vertical$yend <- edges$xend
  expect_equal(count_crossings(vertical), 0L)
  actual_hit <- edges
  actual_hit$x[3L] <- actual_hit$xend[3L] <- 4.5
  expect_equal(count_crossings(actual_hit), 1L)
  connected <- edges
  connected$x[2L] <- 1
  expect_equal(count_crossings(connected), 1L)
})

test_that("edge labels cannot alias distinct crossing pairs", {
  # Four nonparallel tracks meet internally: there are six edge-pair events.
  edges <- data.frame(edge_id = c("A", "A B", "B C", "C"),
    x = c(-2, 0, -2, -2), y = c(0, -2, -2, 2),
    xend = c(2, 0, 2, 2), yend = c(0, 2, 2, -2))
  original <- edges
  expect_equal(count_crossings(edges), 6L)
  expect_equal(count_crossings(edges[c(4, 2, 1, 3), ]), 6L)
  edges$edge_id <- factor(edges$edge_id, levels = c("unused", "C", "B C", "A B", "A"))
  expect_equal(count_crossings(edges), 6L)
  expect_identical(original$edge_id, c("A", "A B", "B C", "C"))
})

test_that("crossing metrics reject incomplete geometry before short-table exits", {
  edges <- data.frame(edge_id = 1:2, x = c(0, 0), y = c(0, 1),
    xend = c(1, 1), yend = c(1, 0))
  invalid <- list(NA_real_, NaN, Inf, -Inf, "0", factor("0"), 1i, TRUE)
  for (rows in 1:2) for (axis in c("x", "y", "xend", "yend")) for (value in invalid) {
    input <- edges[seq_len(rows), , drop = FALSE]
    input[[axis]] <- rep(value, rows)
    expect_error(count_crossings(input), "finite real numeric coordinates")
  }
  for (rows in 1:2) for (value in list(NA_integer_, NA_character_,
      factor(NA_character_), factor(NA_character_, exclude = NULL))) {
    input <- edges[seq_len(rows), , drop = FALSE]
    input$edge_id <- rep(value, rows)
    expect_error(count_crossings(input), "edge_id")
  }
  for (rows in 1:2) for (value in list(NA_real_, Inf, "one", factor("one"), 1i, TRUE)) {
    input <- edges[seq_len(rows), , drop = FALSE]
    input$segment <- rep(value, rows)
    expect_error(count_crossings(input), "segment")
  }
  expect_equal(count_crossings(edges[FALSE, ]), 0L)
  expect_equal(count_crossings(edges[1L, , drop = FALSE]), 0L)
  expect_equal(count_crossings(edges), 1L)
  edges$edge_id <- c(TRUE, FALSE)
  expect_equal(count_crossings(edges), 1L)
})
