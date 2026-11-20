test_that("shared-track length counts each pair over partial diagonal overlaps", {
  graph <- igraph::graph_from_edgelist(rbind(c("A", "D"), c("A", "C"), c("B", "D")))
  layout <- layout_metro_dag(direction = "down", graph, rank = c(A = 0, B = 1, C = 2, D = 3))
  position <- c(A = 0, B = 2, C = 4, D = 6)[layout$nodes$node]
  layout$nodes$x <- layout$nodes$label_x <- unname(position)
  layout$nodes$y <- layout$nodes$label_y <- -unname(position)
  ends <- igraph::as_edgelist(graph)
  layout$edges <- do.call(rbind, lapply(seq_len(nrow(ends)), function(i) {
    a <- match(ends[i, 1L], layout$nodes$node); b <- match(ends[i, 2L], layout$nodes$node)
    count <- layout$nodes$x[b] - layout$nodes$x[a]
    e <- .route_from_moves(layout$nodes$x[a], layout$nodes$y[a], rep(1, count))
    e$from <- ends[i, 1L]; e$to <- ends[i, 2L]; e$edge_id <- i
    e$segment <- seq_len(nrow(e)); e$bend <- FALSE; e$path_id <- NA_integer_
    e[, names(.empty_routes())]
  }))
  for (direction in c("down", "right", "up", "left")) {
    trial <- .test_rotate_layout(layout, direction)
    metrics <- layout_metrics(trial)
    expect_equal(metrics$shared_track_length, 10 * sqrt(2))
    expect_equal(metrics$overlap_segments, 3L)
    trial$edges <- trial$edges[nrow(trial$edges):1L, ]
    trial$edges$edge_id <- factor(trial$edges$edge_id, levels = 1:4)
    expect_equal(layout_metrics(trial), metrics)
  }
})
