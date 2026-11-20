.test_shared_terminal_layout <- function() {
  graph <- igraph::graph_from_edgelist(rbind(c("A", "B"), c("B", "C"), c("A", "C")))
  layout <- layout_metro_dag(direction = "down", graph, rank = c(A = 0, B = 1, C = 2))
  layout$nodes$x <- layout$nodes$label_x <- 0
  layout$nodes$y <- layout$nodes$label_y <- c(0, -8, -16)
  moves <- list(rep(0, 8), rep(0, 8), c(0, rep(1, 4), rep(0, 6), rep(-1, 4), 0))
  ends <- igraph::as_edgelist(graph)
  routes <- lapply(seq_len(3L), function(i) {
    n <- layout$nodes[match(ends[i, 1L], layout$nodes$node), ]
    e <- .route_from_moves(n$x, n$y, moves[[i]])
    e$from <- ends[i, 1L]; e$to <- ends[i, 2L]; e$edge_id <- i
    e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves[[i]]) != 0)
    e$path_id <- NA_integer_
    e[, names(.empty_routes())]
  })
  layout$edges <- do.call(rbind, routes)
  layout
}

test_that("fanned bypasses leave a complete straight through-route on its bearing", {
  skip_if_not_installed("svglite")
  base <- .test_shared_terminal_layout()
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  points <- function(line) matrix(as.numeric(strsplit(trimws(sub(
    ".* points='([^']+)'.*", "\\1", line)), "[, ]+")[[1L]]), ncol = 2L, byrow = TRUE)
  for (direction in c("down", "right", "up", "left")) {
    layout <- .test_rotate_layout(base, direction)
    p <- plot_metro_dag(layout, labels = FALSE, legend = FALSE)
    original <- p
    expect_no_warning(save_metro_dag(file, p, width = 220, height = 220))
    svg <- readLines(file)
    shafts <- lapply(svg[grepl("<polyline", svg, fixed = TRUE) &
      grepl("stroke: #58636D", svg, fixed = TRUE)], points)
    expect_length(shafts, 3L)
    expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 3L)
    # Painting order is A->B, A->C, B->C. Bypass attachments may fan;
    # neither of the two straight arrows should inherit their hooks. B->C may
    # tilt within its 2-degree port allowance (0.14 mm, 0.4 pt here) so that
    # A->C reaches its spread port without a hook.
    transverse <- if (direction %in% c("right", "left")) 2L else 1L
    for (id in c(1L, 3L)) expect_lt(diff(range(shafts[[id]][, transverse])), 0.5)
    report <- diagnose_metro_dag(p, width = 220, height = 220, device = "svg")
    expect_true(report$ok)
    expect_false(any(.metro_hook_score(report$routes)$hook), label = direction)
    expect_identical(p, original)
  }
})

test_that("attachment blending retains a radial head and cannot reverse a short tail", {
  xy <- cbind(seq(0, 20), 0)
  same <- .metro_fan_end(xy, c(20, 0), c(22, 0), 2)
  expect_identical(same, xy)
  smooth <- .metro_fan_end(xy, c(20, 0.5), c(22, 0), 2)
  tip <- smooth[nrow(smooth), ]; shaft <- tip - smooth[nrow(smooth) - 1L, ]
  radial <- c(22, 0) - tip
  expect_equal(sum(shaft * radial) / sqrt(sum(shaft^2) * sum(radial^2)), 1)
  expect_equal(sqrt(sum(shaft^2)), 2)
  expect_true(all(diff(smooth[, 1L]) > 0))
  expect_equal(unname(head(smooth, 10)), unname(head(xy, 10)))
  short <- xy[1:3, ]; fallback <- short; fallback[3L, ] <- c(2, 0.5)
  expect_identical(.metro_fan_end(short, c(2, 0.5), c(4, 0), 2), fallback)
})
