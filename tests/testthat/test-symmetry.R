.test_unbalanced_fan <- function() {
  graph <- metrodag_examples("causal")
  nodes <- data.frame(node = igraph::V(graph)$name,
    x = c(6, 1, 0, 0, 4, -4), y = c(-4, -11, -21, -15, 0, 0))
  moves <- list(c(rep(-1, 5), 0, 0), c(rep(0, 11), rep(-1, 6)),
    c(-1, 0, 0, 0), rep(0, 6), c(1, 1, 0, 0),
    c(rep(0, 8), rep(-1, 3)), c(rep(0, 17), rep(1, 4)),
    c(1, 1, rep(0, 6), 1, 1, 1))
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  routes <- lapply(seq_along(moves), function(id) {
    from <- ends[id, 1L]; to <- ends[id, 2L]
    e <- .route_from_moves(nodes$x[from], nodes$y[from], moves[[id]])
    e$from <- nodes$node[from]; e$to <- nodes$node[to]; e$edge_id <- id
    e$segment <- seq_len(nrow(e)); e$path_id <- NA_integer_
    e$bend <- c(FALSE, diff(moves[[id]]) != 0)
    e[, names(.empty_routes())]
  })
  list(graph = graph, nodes = nodes, routes = routes)
}

.test_fan_layout <- function(fixture) structure(list(graph = fixture$graph,
  nodes = fixture$nodes, edges = do.call(rbind, fixture$routes), direction = "down"),
  class = "metrodag_layout")

test_that("clearance projects each station onto a shared segment independently", {
  x <- c(5, -1, 11); y <- c(0.5, 0, 0)
  for (rows in list(1:3, 3:1, c(2, 3, 1)))
    expect_equal(.point_segment_distance(x[rows], y[rows], 0, 0, 10, 0),
      c(0.5, 1, 1)[rows])
  expect_equal(.point_segment_distance(3, 4, c(0, 0, 0), c(0, 0, 0),
    c(10, 0, 0), c(0, 10, 0)), c(4, 3, 5))
  expect_equal(.point_segment_distance(c(-1, 2), c(3, 4), 0, 0, 0, 0), sqrt(c(10, 20)))
  expect_length(.point_segment_distance(numeric(), numeric(), 0, 0, 10, 0), 0L)
  runs <- data.frame(x = 0, y = 0, xend = 10, yend = 0)
  nodes <- data.frame(node = c("clear", "blocked"), x = c(1, 9), y = c(2, 0))
  expect_false(.route_obstacle_free(runs, nodes))
  expect_false(.route_obstacle_free(runs, nodes[2:1, ]))
})

test_that("publication outer bends align and its middle chain is straight", {
  graph <- metrodag_examples("causal")
  original <- graph
  # Laid out once and turned to every direction (test-layout.R).
  layouts <- .test_layouts_by_direction(graph)
  for (direction in c("down", "up", "right", "left")) {
    layout <- layouts[[direction]]
    across <- if (direction %in% c("down", "up")) "x" else "y"
    along <- if (across == "x") "yend" else "xend"
    position <- stats::setNames(layout$nodes[[across]], layout$nodes$node)
    expect_equal(position[["Y"]], mean(position[c("C", "S")]))
    expect_equal(unname(position[c("X", "M", "Y")]), rep(position[["Y"]], 3))
    bends <- lapply(c("C", "S"), function(from) {
      route <- layout$edges[layout$edges$from == from & layout$edges$to == "Y", ]
      runs <- .collapse_routes(route)
      expect_equal(nrow(runs), 2L)
      runs[[along]][1L]
    })
    expect_equal(bends[[1L]], bends[[2L]])
    metrics <- layout_metrics(layout)
    expect_equal(metrics$crossings, 0L)
    expect_equal(metrics$total_bends, 4L)
    expect_equal(metrics$overlap_segments, 0L)
    expect_equal(metrics$node_edge_collisions, 0L)
    expect_equal(metrics$directional_consistency, 1)
    expect_identical(graph, original)
  }
})

test_that("fan balancing uses graph structure and preserves its inputs", {
  fixture <- .test_unbalanced_fan()
  labels <- paste0("variable ", seq_len(nrow(fixture$nodes)))
  names(labels) <- fixture$nodes$node
  igraph::V(fixture$graph)$name <- unname(labels)
  fixture$nodes$node <- unname(labels[fixture$nodes$node])
  fixture$routes <- lapply(fixture$routes, function(e) {
    e$from <- unname(labels[e$from]); e$to <- unname(labels[e$to]); e
  })
  original <- fixture
  refined <- .balance_geometry(fixture$graph, fixture$nodes, fixture$routes)
  expect_equal(refined$nodes$x[c(2, 3, 4)], c(1, 1, 1))
  before <- layout_metrics(.test_fan_layout(fixture))
  after <- layout_metrics(.test_fan_layout(c(list(graph = fixture$graph), refined)))
  expect_equal(after$total_bends, before$total_bends - 1L)
  expect_equal(after$crossings, before$crossings)
  expect_lte(after$total_edge_length, before$total_edge_length)
  expect_equal(after$overlap_segments, 0L)
  expect_equal(after$node_edge_collisions, 0L)
  expect_identical(fixture, original)
})

test_that("balancing cannot squeeze or reorder stations in the same rank", {
  fixture <- .test_unbalanced_fan()
  fixture$graph <- igraph::add_vertices(fixture$graph, 1, name = "blocked lane")
  fixture$nodes <- rbind(fixture$nodes, data.frame(node = "blocked lane", x = 4, y = -15))
  original <- fixture
  refined <- .balance_geometry(fixture$graph, fixture$nodes, fixture$routes)
  expect_identical(refined$nodes, fixture$nodes)
  expect_identical(refined$routes, fixture$routes)
  expect_identical(fixture, original)
})

test_that("an unchanged edge can prevent centering through an unrelated station", {
  fixture <- .test_unbalanced_fan()
  fixture$graph <- igraph::add_vertices(fixture$graph, 2, name = c("P", "Q"))
  fixture$graph <- igraph::add_edges(fixture$graph, c(7L, 8L))
  fixture$nodes <- rbind(fixture$nodes,
    data.frame(node = c("P", "Q"), x = 1, y = c(-14, -16)))
  e <- .route_from_moves(1, -14, c(0, 0))
  e$from <- "P"; e$to <- "Q"; e$edge_id <- 9L; e$segment <- 1:2
  e$path_id <- NA_integer_; e$bend <- FALSE
  fixture$routes[[9L]] <- e[, names(.empty_routes())]
  original <- fixture
  refined <- .balance_geometry(fixture$graph, fixture$nodes, fixture$routes)
  expect_identical(refined$nodes, fixture$nodes)
  expect_identical(refined$routes, fixture$routes)
  expect_identical(fixture, original)
})

test_that("balancing keeps existing crossing and shared-track limits", {
  fixture <- .test_unbalanced_fan()
  fixture$graph <- igraph::add_vertices(fixture$graph, 2, name = c("P", "Q"))
  fixture$graph <- igraph::add_edges(fixture$graph, c(7L, 8L))
  fixture$nodes <- rbind(fixture$nodes,
    data.frame(node = c("P", "Q"), x = c(2, 4), y = c(-16, -22)))
  moves <- c(-1, 0, 0, 1, 1, 1)
  e <- .route_from_moves(2, -16, moves)
  e$from <- "P"; e$to <- "Q"; e$edge_id <- 9L; e$segment <- seq_along(moves)
  e$path_id <- NA_integer_; e$bend <- c(FALSE, diff(moves) != 0)
  fixture$routes[[9L]] <- e[, names(.empty_routes())]
  original <- fixture
  refined <- .balance_geometry(fixture$graph, fixture$nodes, fixture$routes)
  before <- layout_metrics(.test_fan_layout(fixture))
  after <- layout_metrics(.test_fan_layout(c(list(graph = fixture$graph), refined)))
  expect_lte(after$crossings, before$crossings)
  expect_lte(after$total_bends, before$total_bends)
  expect_lte(after$total_edge_length, before$total_edge_length)
  expect_equal(after$overlap_segments, 0L)
  expect_equal(after$node_edge_collisions, 0L)
  expect_identical(fixture, original)
})
