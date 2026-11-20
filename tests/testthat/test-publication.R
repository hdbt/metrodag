test_that("long routes preserve endpoints without passing through unrelated nodes", {
  for (kind in c("chain", "fork", "collider", "diamond", "long_edge", "causal", "dense")) {
    g <- metrodag_examples(kind)
    # The same geometry as a fresh layout in each direction (test-layout.R).
    layouts <- .test_layouts_by_direction(g)
    for (direction in .test_directions) {
      l <- layouts[[direction]]
      expect_equal(layout_metrics(l)$node_edge_collisions, 0L, info = paste(kind, direction))
      for (id in unique(l$edges$edge_id)) {
        e <- l$edges[l$edges$edge_id == id, ]
        a <- l$nodes[match(e$from[1L], l$nodes$node), ]
        b <- l$nodes[match(e$to[1L], l$nodes$node), ]
        expect_equal(c(e$x[1L], e$y[1L]), c(a$x, a$y))
        expect_equal(c(tail(e$xend, 1), tail(e$yend, 1)), c(b$x, b$y))
        expect_equal(head(e$xend, -1), tail(e$x, -1))
        expect_equal(head(e$yend, -1), tail(e$y, -1))
      }
    }
  }
})

test_that("long edges participate in crossing reduction without exposing waypoints", {
  g <- metrodag_examples("long_edge")
  p <- .proper_graph(g, .assign_layers(g))
  expect_gt(igraph::vcount(p$graph), igraph::vcount(g))
  ends <- igraph::ends(p$graph, igraph::E(p$graph), names = FALSE)
  expect_true(all(p$layer[ends[, 2L]] - p$layer[ends[, 1L]] == 1L))
  l <- layout_metro_dag(direction = "down", g)
  expect_setequal(l$nodes$node, igraph::V(g)$name)
  expect_equal(igraph::as_edgelist(l$graph), igraph::as_edgelist(g))
  expect_equal(layout_metrics(l)$overlap_segments, 0L)
})

test_that("crossings at grid points count once and common endpoints do not", {
  e <- data.frame(edge_id = c(1, 1, 2, 2), segment = c(1, 2, 1, 2),
    x = c(0, 1, 2, 1), y = c(2, 1, 2, 1),
    xend = c(1, 2, 1, 0), yend = c(1, 0, 1, 0))
  expect_equal(count_crossings(e), 1L)
  common <- e[c(1, 3), ]
  expect_equal(count_crossings(common), 0L)
  g <- igraph::add_vertices(metrodag_examples("chain"), 1L, name = "isolated")
  l <- layout_metro_dag(direction = "down", g)
  # Place an unrelated station on C -> D while keeping every route attached
  # to its endpoints. Detaching B would now be rejected before diagnostics.
  route <- l$edges[l$edges$from == "C" & l$edges$to == "D", ]
  l$nodes[l$nodes$node == "isolated", c("x", "y")] <- c(
    mean(c(route$x[1L], tail(route$xend, 1L))),
    mean(c(route$y[1L], tail(route$yend, 1L))))
  expect_equal(layout_metrics(l)$node_edge_collisions, 1L)
})

test_that("fractional ranks, isolated nodes and empty edge sets work", {
  g <- igraph::make_empty_graph(3, directed = TRUE)
  igraph::V(g)$name <- c("A", "B", "C")
  l <- layout_metro_dag(direction = "down", g)
  expect_equal(nrow(l$edges), 0L)
  expect_equal(layout_metrics(l)$directional_consistency, 1)
  expect_equal(nrow(unique(l$nodes[, c("x", "y")])), 3L)
  chain <- metrodag_examples("chain")
  l <- layout_metro_dag(direction = "down", chain, rank = c(A = 0.1, B = 0.2, C = 0.3, D = 0.4))
  expect_equal(l$nodes$layer, 0:3)
  one <- igraph::make_empty_graph(1, directed = TRUE)
  expect_equal(layout_metrics(layout_metro_dag(direction = "down", one))$node_spacing_variance, 0)
})

test_that("one-call input preserves names and explicit causal annotations", {
  edges <- data.frame(from = c("X", "M", "C", "C"), to = c("M", "Y", "X", "Y"))
  p <- metro_dag(edges, exposure = "X", outcome = "Y", adjusted = "C")
  l <- attr(p, "metrodag_layout")
  expect_s3_class(p, "ggplot")
  expect_equal(l$direction, "right")
  expect_setequal(l$nodes$node, c("X", "M", "C", "Y"))
  expect_equal(igraph::vertex_attr(l$graph, "role", "C"), "adjusted")
  g <- metrodag_examples("causal")
  original <- g
  metro_dag(g, unobserved = "U")
  expect_identical(g, original)
  expect_s3_class(metro_dag(as.matrix(edges)), "ggplot")
  expect_error(metro_dag(g, exposure = "missing"), "`exposure` names a node not in the graph: `missing`")
  expect_error(metro_dag(g, exposure = "X", outcome = "X"), "disjoint")
  expect_error(metro_dag(data.frame(a = "X", b = "Y")), "from.*to")
  expect_error(metro_dag(g, labels = c(missing = "Label")), "`labels` names a node not in the graph: `missing`")
  expect_error(layout_metro_dag(direction = "down", g, sweeps = NA_real_), "sweeps")
  expect_error(layout_metro_dag(direction = "down", g, sweeps = 0.5), "integer")
  expect_error(layout_metro_dag(direction = "down", igraph::graph_from_edgelist(matrix(c("A", "B", "A", "B"), 2, byrow = TRUE))), "repeats the arrow A -> B")
})

test_that("display labels use attributes, wrapping and partial named overrides", {
  g <- metrodag_examples("chain")
  igraph::V(g)$label <- c("Exposure variable", "Mediator", NA, "Outcome")
  expect_equal(.display_labels(g, TRUE, 12), c("Exposure\nvariable", "Mediator", "C", "Outcome"))
  expect_equal(.display_labels(g, c(A = "Treatment"), NULL), c("Treatment", "Mediator", "C", "Outcome"))
  expect_equal(.display_labels(g, FALSE, 24), rep("", 4))
})

test_that("arrow tips are separated and clear every station rim", {
  layouts <- .test_layouts_by_direction(metrodag_examples("dense"))
  for (direction in c("right", "down", "left", "up")) {
    p <- metro_dag(layouts[[direction]])
    args <- p$layers[[2L]]$geom_params
    n <- args$nodes; e <- args$edges
    # Scale the stored grid to millimetres for this device-independent fixture.
    for (key in c("x", "y")) n[[key]] <- n[[key]] * 5
    for (key in c("x", "y", "xend", "yend")) e[[key]] <- e[[key]] * 5
    ports <- .station_ports(n, e, rep(2.2, nrow(n)), 0.5)
    for (side in c("start", "end")) {
      z <- e[if (side == "start") !duplicated(e$edge_id) else !duplicated(e$edge_id, fromLast = TRUE), ]
      index <- match(if (side == "start") z$from else z$to, n$node)
      distance <- sqrt((ports[[side]][, 1] - n$x[index])^2 + (ports[[side]][, 2] - n$y[index])^2)
      expect_equal(unname(distance), rep(2.7, nrow(z)))
      expect_equal(nrow(unique(round(ports[[side]], 6))), nrow(z))
    }
  }
})

test_that("publication exports render to vector and raster devices", {
  skip_if_not_installed("svglite")
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y",
    unobserved = "U", labels = c(C = "Socioeconomic background", X = "Cigarette consumption",
      M = "Tar deposit in lung", Y = "Lung cancer risk", U = "Unmeasured inheritance", S = "Occupational hazards"))
  p <- p + ggplot2::labs(title = "A causal model")
  for (ext in c("pdf", "svg", "png")) {
    file <- tempfile(fileext = paste0(".", ext))
    expect_no_warning(save_metro_dag(file, p, dpi = 96))
    expect_gt(file.info(file)$size, 100)
    if (ext == "svg") {
      content <- paste(readLines(file, warn = FALSE), collapse = "\n")
      expect_match(content, "Socioeconomic")
      expect_match(content, "cancer")
    }
    unlink(file)
  }
  # Short labels should also work at one-column width and without labels/arrows.
  file <- tempfile(fileext = ".pdf")
  expect_no_warning(save_metro_dag(file, metro_dag(metrodag_examples("causal")), width = 85))
  expect_no_warning(save_metro_dag(file, metro_dag(metrodag_examples("diamond"), labels = FALSE, arrows = FALSE)))
  expect_no_warning(save_metro_dag(file, metro_dag(igraph::make_empty_graph(1, directed = TRUE))))
  unlink(file)
  expect_error(save_metro_dag("x.html", p), "pdf")
  expect_error(save_metro_dag("x.pdf", p, width = 0), "positive")
})

test_that("a crowded common approach does not fan attachments through a station", {
  nodes <- data.frame(node = c(paste0('P', 1:9), 'Y'), x = c(rep(-20, 9), 0),
    y = c(rep(20, 9), 0), shape = 21L)
  edges <- data.frame(edge_id = 1:9, from = paste0('P', 1:9), to = 'Y',
    x = -10, y = 10, xend = 0, yend = 0)
  ports <- .station_ports(nodes, edges, rep(5, 10), 0.5)
  distance <- .point_segment_distance(0, 0, rep(-10, 9), rep(10, 9),
    ports$end[, 1L], ports$end[, 2L])
  expect_true(all(distance > 5))
  expect_equal(nrow(unique(round(ports$end, 6))), 9)
})
