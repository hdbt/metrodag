test_that("only crowded attachments spread, and tied arrows straddle their track", {
  # A lone arrow with room keeps its bearing even beside a spread pair.
  rel <- c(-0.8, 0, 0)
  fit <- .metro_spread_ports(rel, separation = 0.2, straight = c(FALSE, FALSE, FALSE))
  expect_equal(fit[1L], -0.8)
  expect_equal(fit[2:3], c(-0.1, 0.1))
  # A straight member of a tied pair keeps its bearing; its partner moves.
  fit <- .metro_spread_ports(c(0, 0), 0.2, straight = c(FALSE, TRUE))
  expect_equal(fit, c(-0.2, 0))
  # Clusters that grow into each other merge and keep the minimum separation.
  rel <- c(0, 0, 0.25, 0.25)
  fit <- .metro_spread_ports(rel, 0.2, straight = rep(FALSE, 4L))
  expect_true(all(diff(fit) >= 0.2 - 1e-9))
  expect_equal(mean(range(fit)), mean(range(rel)))
  expect_identical(.metro_spread_ports(numeric(0), 0.2, logical(0)), numeric(0))
})

test_that("a displaced arrival joins where its route has already diverged", {
  # The route arrives horizontally after a diagonal step from below; its port
  # sits below the centreline, on the side the route came from.
  xy <- rbind(c(-20, -10), c(-10, 0), c(-2, 0))
  target <- c(0, 0)
  tip <- c(-1.9, -0.6)
  joined <- .metro_fan_end(xy, tip, target, straight = 1.5)
  expect_equal(joined[nrow(joined), ], tip)
  routes <- data.frame(panel = "1", from = "A", to = "B", point = seq_len(nrow(joined)),
    x = joined[, 1L], y = joined[, 2L])
  expect_false(.metro_hook_score(routes)$hook)
  # The final straight end still points at the station centre.
  final <- joined[nrow(joined), ] - joined[nrow(joined) - 1L, ]
  inward <- target - tip
  expect_gt(sum(final * inward) / sqrt(sum(final^2) * sum(inward^2)), 0.999)
  # With no divergence on the port's side the original blend is kept.
  straight_route <- rbind(c(-20, 0), c(-2, 0))
  fallback <- .metro_fan_end(straight_route, tip, target, straight = 1.5)
  expect_equal(fallback[nrow(fallback), ], tip)
})

test_that("gallery arrows into busy stations keep within their hook ceilings", {
  skip_on_cran()
  skip_if_not_installed("svglite")
  # Hook counts with physically spaced arrival ports; the narrow fan with the
  # convex arrival join left 10 across these figures, and 32 without the join.
  # Large exports join shared tracks further back, so no arrow turns back on
  # itself by much more than the 8-degree hook threshold: a fixed 20 mm join
  # window gave 01 and 05 a 41-degree S at 240 and 300 mm.
  ceilings <- list(`180` = c(0, 0, 0, 0, 0, 0), `240` = c(0, 0, 0, 0, 0, 0),
    `300` = c(0, 0, 0, 0, 0, 0))
  # Each figure is laid out once; only the export width changes.
  for (i in seq_along(.metro_gallery_examples)) {
    example <- .metro_gallery_examples[[i]]
    p <- .test_gallery_plot(i)
    for (width in names(ceilings)) {
      report <- diagnose_metro_dag(p, width = as.numeric(width), device = "svg")
      expect_true(report$ok, info = paste(example$id, width))
      score <- .metro_hook_score(report$routes)
      expect_lte(sum(score$hook), ceilings[[width]][i], label = paste(example$id, width, "hooks"))
      expect_lte(max(pmin(score$positive, score$negative)), 10,
        label = paste(example$id, width, "worst turn back"))
      expect_equal(length(unique(paste(report$routes$from, report$routes$to))),
        igraph::ecount(attr(p, "metrodag_layout")$graph))
    }
  }
})

test_that("crowded stations part their heads without trading them for hooks", {
  skip_on_cran()
  skip_if_not_installed("svglite")
  # Bending arrows that keep their bearing to part touching heads drew hooks
  # into six parents (three at every width) and into the dense example.
  star <- metro_dag(cbind(paste0("P", 1:6), "Y"), labels = FALSE, legend = FALSE)
  layouts <- .test_layouts_by_direction(metrodag_examples("dense"), directions = c("right", "down"))
  dense <- metro_dag(layouts$right)
  down <- metro_dag(layouts$down)
  for (width in c(85, 120, 180)) {
    hooks <- function(p) sum(.metro_hook_score(diagnose_metro_dag(p, width = width,
      device = "svg")$routes)$hook)
    expect_identical(hooks(star), 0L, label = paste("six parents", width))
    # The narrow fan drew 4, 2 and 2 hooks into the dense example, and 2 at
    # every width facing down.
    expect_lte(hooks(dense), c(`85` = 4L, `120` = 2L, `180` = 2L)[[as.character(width)]],
      label = paste("dense", width))
    expect_lte(hooks(down), 2L, label = paste("dense down", width))
  }
})

test_that("diagnostics report the drawn arrow shafts", {
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y")
  report <- diagnose_metro_dag(p, width = 120)
  expect_named(report$routes, c("panel", "from", "to", "point", "x", "y"))
  expect_equal(nrow(unique(report$routes[c("from", "to")])), 8L)
  empty <- diagnose_metro_dag(metro_dag(igraph::make_empty_graph(2) |>
    igraph::set_vertex_attr("name", value = c("A", "B"))), width = 60)
  expect_equal(nrow(empty$routes), 0L)
})
