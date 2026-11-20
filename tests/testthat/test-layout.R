test_that("canonical layouts preserve graph structure", {
  # The dense example takes longest; CRAN checks the others.
  kinds <- c("chain", "fork", "collider", "diamond", "long_edge", "causal",
    if (!.test_on_cran()) "dense")
  for (kind in kinds) {
    g <- metrodag_examples(kind)
    out <- layout_metro_dag(direction = "down", g)
    expect_setequal(out$nodes$node, igraph::V(g)$name)
    expect_equal(nrow(out$nodes), igraph::vcount(g))
    expect_equal(length(unique(paste(out$nodes$x, out$nodes$y))), igraph::vcount(g))
    expected <- apply(igraph::ends(g, igraph::E(g)), 1, paste, collapse = "->")
    actual <- unique(paste(out$edges$from, out$edges$to, sep = "->"))
    expect_setequal(actual, expected)
    expect_true(all(out$edges$yend <= out$edges$y))
    dx <- abs(out$edges$xend - out$edges$x)
    dy <- abs(out$edges$yend - out$edges$y)
    expect_true(all(dy %in% c(0.5, 1)))
    expect_true(all(dx == 0 | dx == dy))
  }
})

test_that("layout is deterministic", {
  g <- metrodag_examples("causal")
  first <- layout_metro_dag(direction = "down", g)
  second <- layout_metro_dag(direction = "down", g)
  expect_identical(first$nodes, second$nodes)
  expect_identical(first$edges, second$edges)
})

test_that("directions rotate monotonic geometry", {
  g <- metrodag_examples("diamond")
  right <- layout_metro_dag(g, direction = "right")
  up <- layout_metro_dag(g, direction = "up")
  expect_true(all(right$edges$xend >= right$edges$x))
  expect_true(all(up$edges$yend >= up$edges$y))
  expect_equal(layout_metrics(right)$directional_consistency, 1)
  # Direction is only the final rotation, so tests may lay a graph out once and
  # turn it (helper-layouts.R): every direction matches a fresh layout exactly,
  # for packed components, supplied ranks and aligned groups as well.
  chain <- metrodag_examples("chain")
  igraph::V(chain)$name <- paste0("k", igraph::V(chain)$name)
  packed <- igraph::disjoint_union(metrodag_examples("collider"), chain)
  ranked <- list(rank = c(A = 0, B = 2, C = 3, D = 5))
  aligned <- list(rank = c(A = 0, B = 1, C = 1, D = 2), align = list(c("A", "B", "D")))
  cases <- list(list(g), list(packed), c(list(metrodag_examples("chain")), ranked),
    c(list(metrodag_examples("diamond")), aligned))
  # Where the canonical (down) coordinates go in each direction.
  turned <- list(down = function(x, y) cbind(x, y), up = function(x, y) cbind(x, -y),
    right = function(x, y) cbind(-y, x), left = function(x, y) cbind(y, x))
  for (case in cases) {
    rotations <- do.call(.test_layouts_by_direction, case)
    expect_named(rotations, c("down", "up", "right", "left"))
    down <- rotations$down
    for (direction in names(rotations)) {
      fresh <- do.call(layout_metro_dag, c(case, direction = direction))
      expect_identical(rotations[[direction]], fresh)
      expect_equal(layout_metrics(fresh)$directional_consistency, 1)
      expect_equal(unname(cbind(fresh$nodes$x, fresh$nodes$y)),
        unname(turned[[direction]](down$nodes$x, down$nodes$y)))
      expect_equal(unname(cbind(fresh$edges$xend, fresh$edges$yend)),
        unname(turned[[direction]](down$edges$xend, down$edges$yend)))
      expect_identical(fresh[c("graph", "parameters")], down[c("graph", "parameters")])
    }
  }
})

test_that("cycles and invalid ranks fail informatively", {
  cyclic <- igraph::make_ring(4, directed = TRUE)
  expect_error(layout_metro_dag(direction = "down", cyclic), "cycle")
  g <- metrodag_examples("chain")
  expect_error(layout_metro_dag(direction = "down", g, rank = c(A = 0, B = 1)), "every node")
  expect_error(layout_metro_dag(direction = "down", g, rank = c(A = 0, B = 1, C = 1, D = 2)), "strictly increase")
})

test_that("metrics are internally consistent", {
  out <- layout_metro_dag(direction = "down", metrodag_examples("diamond"))
  m <- layout_metrics(out)
  expect_equal(m$nodes, 4)
  expect_equal(m$edges, 4)
  expect_gte(m$total_edge_length, 4)
  expect_equal(count_crossings(out), m$crossings)
})

test_that("publication and metro styles are available", {
  out <- layout_metro_dag(direction = "down", metrodag_examples("diamond"))
  p <- plot_metro_dag(out)
  expect_s3_class(p, "ggplot")
  expect_equal(p$coordinates$clip, "off")
  expect_equal(p$theme$plot.background$fill, "white")
  expect_equal(length(unique(p$layers[[2L]]$geom_params$edges$colour)), 1L)
  coloured <- plot_metro_dag(out, style = "metro")
  expect_gt(length(unique(coloured$layers[[2L]]$geom_params$nodes$colour)), 1L)
})

test_that("plot appearance arguments are validated", {
  out <- layout_metro_dag(direction = "down", metrodag_examples("chain"))
  expect_error(plot_metro_dag(out, line_width = 0), "positive")
  expect_error(plot_metro_dag(out, node_size = NA_real_), "positive")
  expect_error(plot_metro_dag(out, palette = character()), "non-empty")
})

