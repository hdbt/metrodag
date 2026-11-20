test_that("joint label placement escapes a mutually blocking pair", {
  # First label prefers the second label's only close slot. Independent greedy
  # moves cannot improve either, but moving both cuts their total distance.
  boxes <- rbind(c(0, 8, 0, 2), c(0, 8, 4, 6))
  centers <- cbind(rowMeans(boxes[, 1:2]), rowMeans(boxes[, 3:4]))
  choices <- list(list(boxes = boxes, centers = centers, base = c(0, 2)),
    list(boxes = rbind(boxes[1L, ], c(20, 28, 0, 2)),
      centers = rbind(centers[1L, ], c(24, 1)), base = c(0, 20)))
  placed <- rbind(boxes[1L, ], choices[[2L]]$boxes[2L, ])
  current <- rbind(centers[1L, ], choices[[2L]]$centers[2L, ])
  after <- .refine_label_pairs(choices, placed, current, 1:2)
  expect_equal(after$boxes, rbind(boxes[2L, ], boxes[1L, ]), ignore_attr = TRUE)
  expect_equal(after$centers, rbind(centers[2L, ], centers[1L, ]), ignore_attr = TRUE)
  expect_equal(.box_overlap(after$boxes[1L, ], after$boxes[2L, , drop = FALSE]), 0)
  expect_equal(.refine_label_pairs(choices, placed, current, 1:2, max_pairs = 0L),
    list(centers = current, boxes = placed))
  # Equivalent priorities preserve the same physical solution.
  expect_equal(.refine_label_pairs(choices, placed, current, 2:1), after)
})

test_that("empty labels and single stations do not require a pair search", {
  nodes <- data.frame(node = c("a", "b"), label = c("", ""), x = c(0, 10), y = c(0, 0))
  result <- .place_labels(nodes, .empty_routes(), matrix(0, 2L, 2L), c(1, 1), c(-20, 20, -20, 20))
  expect_true(all(is.na(result$centers)))
  nodes <- nodes[1L, ]; nodes$label <- "A"
  result <- .place_labels(nodes, .empty_routes(), matrix(c(3, 2), 1L), 1, c(-20, 20, -20, 20))
  expect_true(all(is.finite(result$centers)))
})

test_that("collision-driven reflow preserves words and intentional line breaks", {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file, width = 4, height = 4)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  gp <- grid::gpar(fontsize = 9)
  nodes <- data.frame(node = "x", x = 50, y = 50, shape = 21L,
    label = "Long station\nName label")
  angle <- seq(0, 2 * pi, length.out = 7L)[1:6]
  edges <- data.frame(x = 50, y = 50, xend = 50 + 40 * cos(angle),
    yend = 50 + 40 * sin(angle), edge_id = 1:6)
  texts <- list(grid::textGrob(nodes$label, gp = gp))
  sizes <- matrix(c(grid::convertWidth(grid::grobWidth(texts[[1L]]), "mm", valueOnly = TRUE),
    grid::convertHeight(grid::grobHeight(texts[[1L]]), "mm", valueOnly = TRUE)), 1L)
  bounds <- c(0, 100, 0, 100)
  # Start with an obstructed physical footprint, as supplied by the ordinary
  # placement pass when its current candidate set cannot resolve a collision.
  placement <- list(centers = matrix(c(50, 56), 1L), boxes = matrix(c(
    50 - sizes[1L] / 2 - 0.4, 50 + sizes[1L] / 2 + 0.4,
    56 - sizes[2L] / 2 - 0.4, 56 + sizes[2L] / 2 + 0.4), 1L))
  expect_gt(sum(.label_placement_conflicts(nodes, edges, placement, 2, bounds, 0.45, list())), 0)
  fitted <- .reflow_colliding_labels(nodes, edges, sizes, texts, gp, 2,
    bounds, 0.45, list(), placement)
  expect_equal(sum(.label_placement_conflicts(nodes, edges, fitted$placement,
    2, bounds, 0.45, list())), 0)
  expect_equal(gsub("[[:space:]]+", " ", fitted$nodes$label), "Long station Name label")
  expect_match(fitted$nodes$label, "station\nName", fixed = TRUE)
  expect_identical(nodes$label, "Long station\nName label")
})
