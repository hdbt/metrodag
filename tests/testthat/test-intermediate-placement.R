# Frozen converging-parent geometry before the final placement/resolution
# stages. Explicit values isolate the new algorithm from its initializers.
.test_intermediate_geometry <- function() {
  nodes <- data.frame(node = c("W", "O", "Y", "A", "S", "C", "D", "U", "H"),
    x = c(5, 0, 0, -7, -3, -7, -11, 6, 0),
    y = c(-10, -15, -25, -8, -4, 0, -4, -6, -11))
  ends <- unname(.metro_gallery_examples[[3L]]$edges)  # gallery DAG 3
  moves <- list(rep(-1, 5), rep(c(0, -1, 0), c(9, 5, 1)), rep(0, 10), rep(1, 7),
    rep(c(0, 1, 0), c(7, 3, 1)), rep(0, 8), rep(1, 4), rep(-1, 4),
    rep(c(0, 1), c(10, 11)), rep(c(-1, 0, -1), c(5, 3, 1)),
    rep(c(0, -1), c(13, 6)), rep(0, 4), rep(c(-1, 0, 1), c(1, 12, 1)))
  routes <- lapply(seq_len(nrow(ends)), function(i) {
    n <- nodes[match(ends[i, 1L], nodes$node), ]
    e <- .route_from_moves(n$x, n$y, moves[[i]])
    e$from <- ends[i, 1L]; e$to <- ends[i, 2L]; e$edge_id <- i
    e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves[[i]]) != 0)
    e$path_id <- NA_integer_
    e[, names(.empty_routes())]
  })
  list(nodes = nodes, routes = routes)
}

test_that("an intermediate can exchange a straight side arrow for a shorter through route", {
  before <- .test_intermediate_geometry(); original <- before
  protected <- .straight_chain_edges(before$routes)
  expect_equal(which(protected), c(3L, 12L))
  after <- .relax_intermediate_positions(before$nodes, before$routes)
  a <- .intermediate_geometry_quality(before$nodes, before$routes)
  b <- .intermediate_geometry_quality(after$nodes, after$routes)
  expect_lte(prod(b$extent), prod(a$extent) * 0.85)
  expect_true(all(b$quality <= a$quality + 1e-8))
  expect_true(all(b$reversals <= a$reversals))
  coarse_before <- .intermediate_geometry_quality(before$nodes, before$routes, refine = FALSE)
  coarse_after <- .intermediate_geometry_quality(after$nodes, after$routes, refine = FALSE)
  expect_true(.intermediate_placement_improves(coarse_after, coarse_before, coarse_before))
  expect_equal(after$nodes[after$nodes$node %in% c("H", "O", "Y"), ],
    before$nodes[before$nodes$node %in% c("H", "O", "Y"), ])
  expect_true(any(after$routes[[8L]]$bend))
  expect_false(any(after$routes[[9L]]$bend))
  expect_gt(before$nodes$y[before$nodes$node == "D"], after$nodes$y[after$nodes$node == "D"])
  expect_gte(min(stats::dist(after$nodes[c("x", "y")])), 4)
  expect_identical(before, original)
  for (i in seq_along(after$routes)) {
    e <- after$routes[[i]]; old <- before$routes[[i]]
    for (field in c("from", "to", "edge_id", "path_id"))
      expect_identical(e[[field]][1L], old[[field]][1L])
    expect_identical(e$segment, seq_len(nrow(e)))
    expect_equal(head(e$xend, -1L), tail(e$x, -1L))
    expect_equal(head(e$yend, -1L), tail(e$y, -1L))
    expect_equal(c(e$x[1L], e$y[1L]),
      unname(unlist(after$nodes[after$nodes$node == e$from[1L], c("x", "y")])))
    expect_equal(c(tail(e$xend, 1L), tail(e$yend, 1L)),
      unname(unlist(after$nodes[after$nodes$node == e$to[1L], c("x", "y")])))
    expect_true(all(e$yend == e$y - 1 & abs(e$xend - e$x) %in% 0:1))
    expect_true(.route_obstacle_free(.route_runs(e), after$nodes[
      !after$nodes$node %in% c(e$from[1L], e$to[1L]), ]))
  }
})

test_that("intermediate placement is deterministic and bounded without semantic node names", {
  before <- .test_intermediate_geometry()
  after <- .relax_intermediate_positions(before$nodes, before$routes)
  reverse <- .relax_intermediate_positions(before$nodes[9:1, ], rev(before$routes))
  expect_equal(after$nodes, reverse$nodes[9:1, ], ignore_attr = TRUE)
  expect_equal(after$routes, rev(reverse$routes))
  renamed <- before
  names <- stats::setNames(paste0("node-", seq_len(nrow(before$nodes))), before$nodes$node)
  renamed$nodes$node <- unname(names[renamed$nodes$node])
  renamed$routes <- lapply(renamed$routes, function(e) {
    e$from <- unname(names[e$from]); e$to <- unname(names[e$to]); e
  })
  actual <- .relax_intermediate_positions(renamed$nodes, renamed$routes)
  expect_equal(actual$nodes[c("x", "y")], after$nodes[c("x", "y")])
  expect_equal(.intermediate_geometry_quality(actual$nodes, actual$routes),
    .intermediate_geometry_quality(after$nodes, after$routes))
  expect_identical(.relax_intermediate_positions(before$nodes, before$routes, max_candidates = 0L), before)
  expect_identical(.relax_intermediate_positions(before$nodes, list()),
    list(nodes = before$nodes, routes = list()))
  large <- rep(before$routes, 100L)
  expect_identical(.relax_intermediate_positions(before$nodes, large)$routes, large)
})

test_that("transverse intermediate placement keeps every node in its rank", {
  before <- .test_intermediate_geometry()
  layer <- match(-before$nodes$y, sort(unique(-before$nodes$y))) - 1L
  after <- .relax_intermediate_positions(before$nodes, before$routes, transverse = TRUE)
  expect_identical(after$nodes$y, before$nodes$y)
  expect_true(.ranks_kept(after$nodes$y, layer))
  a <- .intermediate_geometry_quality(before$nodes, before$routes)
  b <- .intermediate_geometry_quality(after$nodes, after$routes)
  expect_true(all(b$quality[c("crossings", "shared", "length")] <=
    a$quality[c("crossings", "shared", "length")] + 1e-8))
  expect_true(all(b$extent <= a$extent + 1e-8))
  # Unrestricted, the best placement moves D to another rank.
  free <- .relax_intermediate_positions(before$nodes, before$routes)
  expect_false(.ranks_kept(free$nodes$y, layer))
})

test_that("smaller intermediate drawings cannot purchase extra crossings or wiggles", {
  base <- list(quality = c(crossings = 3, shared = 2, bends = 10, length = 100),
    extent = c(10, 10), reversals = c(0L, 0L))
  candidate <- base; candidate$extent <- c(9, 10); candidate$quality["bends"] <- 11
  expect_false(.intermediate_placement_improves(candidate, base, base))
  candidate$extent <- c(8, 10)
  expect_true(.intermediate_placement_improves(candidate, base, base))
  candidate$quality["bends"] <- 12
  expect_false(.intermediate_placement_improves(candidate, base, base))
  candidate$extent <- c(7, 10)
  expect_true(.intermediate_placement_improves(candidate, base, base))
  for (field in c("crossings", "shared", "length")) {
    worse <- candidate; worse$quality[field] <- base$quality[field] + 1
    expect_false(.intermediate_placement_improves(worse, base, base))
  }
  candidate$reversals[1L] <- 1L
  expect_false(.intermediate_placement_improves(candidate, base, base))
})

test_that("alignment and disabled ordering skip intermediate placement", {
  graph <- metrodag_examples("chain")
  calls <- logical()
  testthat::local_mocked_bindings(.relax_intermediate_positions = function(nodes, routes, ...,
      transverse = FALSE) {
    calls <<- c(calls, transverse)
    list(nodes = nodes, routes = routes)
  }, .package = "metrodag")
  # Supplied ranks keep this pass, restricted to moves within each rank.
  layout_metro_dag(direction = "down", graph, rank = c(A = 0, B = 1, C = 2, D = 3))
  expect_identical(calls, TRUE)
  testthat::local_mocked_bindings(.relax_intermediate_positions = function(...)
    stop("This placement must be skipped"), .package = "metrodag")
  expect_no_error(layout_metro_dag(direction = "down", graph, align = list(c("A", "B", "C", "D"))))
  expect_no_error(layout_metro_dag(direction = "down", graph, ordering = "none"))
  expect_no_error(layout_metro_dag(direction = "down", graph, sweeps = 0L))
})

test_that("the final public stage updates automatic metadata before rotation", {
  fixture <- .test_intermediate_geometry()
  ends <- do.call(rbind, lapply(fixture$routes, function(e) c(e$from[1L], e$to[1L])))
  graph <- igraph::graph_from_edgelist(ends)
  # Hold earlier searches at the frozen geometry; exercise the real final
  # placement, public half-grid pass, identity validation and direction rotation.
  testthat::local_mocked_bindings(
    .compact_node_arrangement = function(nodes, routes, ...) list(nodes = nodes, routes = routes),
    .refine_layout_variants = function(graph, layer, geometry, ...) geometry,
    .relax_source_positions = function(nodes, routes, ...) list(nodes = nodes, routes = routes),
    .refine_rank_assignment = function(graph, geometry, ...) {
      n <- fixture$nodes[match(igraph::V(graph)$name, fixture$nodes$node), ]
      ee <- igraph::as_edgelist(graph)
      keys <- vapply(fixture$routes, function(e) paste(e$from[1L], e$to[1L]), character(1))
      routes <- lapply(seq_len(nrow(ee)), function(i) {
        e <- fixture$routes[[match(paste(ee[i, 1L], ee[i, 2L]), keys)]]
        e$edge_id <- i; e
      })
      layers <- match(-n$y, sort(unique(-n$y))) - 1L
      list(x = n$x, y = n$y, layer = layers, ord = seq_len(nrow(n)),
        paths = match(n$node, sort(n$node)), edges = do.call(rbind, routes),
        gaps = diff(sort(unique(-n$y))))
    }, .package = "metrodag")
  down <- layout_metro_dag(direction = "down", graph)
  expect_equal(down$nodes$layer, match(-down$nodes$y, sort(unique(-down$nodes$y))) - 1L)
  expect_equal(down$parameters$layer_gap, diff(sort(unique(-down$nodes$y))))
  expect_gt(down$nodes$layer[down$nodes$node == "D"], down$nodes$layer[down$nodes$node == "O"])
  expect_equal(down$nodes$label_x, down$nodes$x)
  expect_equal(down$nodes$label_y, down$nodes$y)
  for (direction in c("right", "up", "left")) {
    actual <- layout_metro_dag(graph, direction = direction)
    expected <- .rotate_geometry(down$nodes, down$edges, direction)
    expect_equal(actual$nodes, expected$nodes)
    expect_equal(actual$edges, expected$edges)
    expect_no_error(.assert_layout_geometry(actual))
    expect_equal(layout_metrics(actual)$directional_consistency, 1)
  }
  reversed <- igraph::graph_from_data_frame(ends[nrow(ends):1L, ], directed = TRUE,
    vertices = data.frame(name = rev(igraph::V(graph)$name)))
  actual <- layout_metro_dag(direction = "down", reversed)
  normalize <- function(e) {
    e <- e[order(e$from, e$to, e$segment), setdiff(names(e), "edge_id")]
    rownames(e) <- NULL; e
  }
  expect_equal(normalize(actual$edges), normalize(down$edges))
  expect_equal(actual$nodes[order(actual$nodes$node), ], down$nodes[order(down$nodes$node), ],
    ignore_attr = TRUE)
})
