# Gallery DAG 1, or DAG 5 with `work`.
.test_variant_graph <- function(work = FALSE) .test_gallery_graph(if (work) 5L else 1L)

.test_without_variants <- function(graph) {
  .test_layout_without(graph, c(".refine_route_resolution", ".refine_layout_variants",
    ".relax_source_positions", ".relax_intermediate_positions"))
}

test_that("alternative starts escape compact-layout dead ends without losing arrows", {
  # Compare starting points before the later final-resolution routing pass.
  .test_without_stages(c(".refine_route_resolution", ".relax_intermediate_positions"))
  for (work in c(FALSE, TRUE)) {
    graph <- .test_variant_graph(work)
    before <- .test_without_variants(graph)
    after <- layout_metro_dag(direction = "down", graph)
    a <- layout_metrics(before); b <- layout_metrics(after)
    expect_identical(after$graph, graph)
    expect_identical(after$nodes$node, before$nodes$node)
    expect_lt(b$area, a$area * 0.9)
    expect_lt(b$total_bends, a$total_bends)
    expect_lte(b$crossings, a$crossings)
    expect_lte(b$shared_track_length, a$shared_track_length + 1e-8)
    expect_lte(b$total_edge_length, a$total_edge_length)
    expect_equal(b$node_edge_collisions, 0L)
    expect_equal(b$directional_consistency, 1)
    expect_gte(min(stats::dist(after$nodes[c("x", "y")])), 4)
    expect_equal(after$parameters$layer_gap, diff(sort(unique(-after$nodes$y))))
    expect_identical(after$nodes$label_x, after$nodes$x)
    expect_identical(after$nodes$label_y, after$nodes$y)
    expect_no_error(.assert_layout_geometry(after))
    first <- after$edges[!duplicated(after$edges$edge_id), ]
    expect_equal(as.matrix(first[c("from", "to")]), igraph::as_edgelist(graph),
      ignore_attr = TRUE)
  }
})

test_that("alternative starts preserve input-order determinism and rotated geometry", {
  graph <- .test_variant_graph()
  ends <- igraph::as_edgelist(graph)
  reversed <- igraph::graph_from_edgelist(ends[nrow(ends):1L, ])
  a <- layout_metro_dag(direction = "down", graph)
  b <- layout_metro_dag(reversed, direction = "right")
  rotated <- .rotate_geometry(a$nodes, a$edges, "right")
  normalize <- function(e) {
    e <- e[order(e$from, e$to, e$segment),
      c("from", "to", "x", "y", "xend", "yend", "bend", "path_id")]
    rownames(e) <- NULL
    e
  }
  expect_equal(normalize(b$edges), normalize(rotated$edges))
  expect_equal(b$nodes[order(b$nodes$node), ], rotated$nodes[order(rotated$nodes$node), ],
    ignore_attr = TRUE)
})

test_that("variant budgets and explicit layout constraints are respected", {
  graph <- .test_variant_graph()
  sentinel <- list(edges = data.frame(x = seq_len(12001L)))
  testthat::local_mocked_bindings(
    .layout_component = function(...) stop("No rebuild is affordable"), .package = "metrodag")
  expect_identical(.refine_layout_variants(graph, integer(), sentinel, "barycenter", 8L), sentinel)
  expect_identical(.refine_layout_variants(graph, integer(), sentinel, "none", 8L), sentinel)
  expect_identical(.refine_layout_variants(graph, integer(), sentinel, "barycenter", 0L), sentinel)
  expect_identical(.refine_layout_variants(graph, integer(), sentinel, "barycenter", 8L,
    max_candidates = 0L), sentinel)
})

test_that("small area savings cannot purchase extra bends or shared tracks", {
  base <- list(quality = c(crossings = 2, shared = 1, bends = 10, length = 100), extent = c(10, 10))
  small <- base; small$extent <- c(9, 10); small$quality["bends"] <- 11
  expect_false(.layout_variant_improves(small, base, base))
  shared <- base; shared$extent <- c(5, 5); shared$quality["shared"] <- 2
  expect_false(.layout_variant_improves(shared, base, base))
  compact <- base; compact$extent <- c(7, 10)
  compact$quality[c("bends", "length")] <- c(11, 80)
  expect_true(.layout_variant_improves(compact, base, base))
  crossing <- base; crossing$extent <- c(10.5, 10)
  crossing$quality[c("crossings", "bends", "length")] <- c(1, 11, 104)
  expect_true(.layout_variant_improves(crossing, base, base))
  crossing$quality["length"] <- 106
  expect_false(.layout_variant_improves(crossing, base, base))
})

test_that("other automatic layerings are tried once each, as orders of ranks", {
  # A chain has a single layering: nothing is tried twice.
  chain <- metrodag_examples("chain")
  expect_length(.layout_layering_starts(chain, .assign_layers(chain, "longest")), 0L)
  # S feeds only D: the minimum-span layering moves it next to D. Sink-aligned
  # ranks do the same here, so that start is not repeated.
  graph <- igraph::graph_from_edgelist(rbind(c("A", "B"), c("B", "C"), c("C", "D"),
    c("S", "D")))
  longest <- .assign_layers(graph, "longest")
  starts <- .layout_layering_starts(graph, longest)
  expect_length(starts, 1L)
  expect_equal(unname(starts[[1L]][c("S", "D")]), c(2L, 3L))
  # From sink-aligned ranks the source-aligned layering is the alternative,
  # also when the supplied ranks are offset, as in a packed component.
  starts <- .layout_layering_starts(graph, .assign_layers(graph, "sinks") + 5L)
  expect_length(starts, 1L)
  expect_equal(unname(starts[[1L]]), unname(longest))
})

test_that("a better automatic layering removes crossings the default ranks cause", {
  # Largest component of metrodag_examples("random", n = 20, probability = 1.5 / 20,
  # seed = 2): longest-path ranks put the sources v1 and v10 far upstream.
  graph <- igraph::graph_from_edgelist(rbind(c("v5", "v7"), c("v6", "v11"), c("v7", "v11"),
    c("v4", "v13"), c("v7", "v13"), c("v5", "v14"), c("v6", "v16"), c("v1", "v17"),
    c("v13", "v17"), c("v10", "v19"), c("v16", "v19")))
  after <- layout_metro_dag(graph)
  b <- layout_metrics(after)
  expect_equal(b$crossings, 0L)
  old <- options(metrodag.control = list(stages = c(layering_variants = FALSE)))
  on.exit(options(old), add = TRUE)
  a <- layout_metrics(layout_metro_dag(graph))
  expect_equal(a$crossings, 2L)
  expect_lte(b$total_bends, a$total_bends)
  expect_lte(b$shared_track_length, a$shared_track_length + 1e-8)
  expect_lt(b$total_edge_length, a$total_edge_length)
  expect_equal(b$node_edge_collisions, 0L)
  expect_identical(after$parameters$stages[["layering_variants"]], "ran")
  ranks <- stats::setNames(after$nodes$layer, after$nodes$node)
  ends <- igraph::as_edgelist(graph)
  expect_true(all(ranks[ends[, 2L]] > ranks[ends[, 1L]]))
  expect_lt(ranks[["v10"]], ranks[["v19"]])
  expect_gt(ranks[["v10"]], 0L)
})

test_that("a layering start must still win after rank refinement and relaxation", {
  # The screen compares drawings before placement relaxation, which can give
  # the default drawing back what the new layering seemed to gain.
  graph <- igraph::graph_from_edgelist(rbind(c("A", "B"), c("B", "C"), c("C", "D"),
    c("S", "D")))
  quality <- function(bends, length, extent, crossings = 2) list(extent = extent,
    quality = c(crossings = crossings, shared = 1, bends = bends, length = length))
  measured <- list(default = quality(11, 160, c(20, 21)), trial = quality(12, 142, c(17.5, 20)),
    "relaxed default" = quality(9, 149.5, c(17.5, 20)))
  tagged <- function(tag) list(tag = tag, edges = data.frame(x = seq_len(3L)))
  testthat::local_mocked_bindings(
    .layout_component = function(...) tagged("trial"),
    .refine_rank_assignment = function(graph, geometry, ...) {
      geometry$tag <- paste("refined", geometry$tag)
      geometry
    },
    .relax_placements = function(graph, geometry) {
      geometry$tag <- paste("relaxed", sub("refined ", "", geometry$tag))
      geometry
    },
    .measure_variant = function(g) measured[[g$tag]],
    .variant_clear = function(...) TRUE,
    .package = "metrodag")
  layer <- .assign_layers(graph, "longest")
  kept <- function(relaxed_trial) {
    measured[["relaxed trial"]] <<- relaxed_trial
    .refine_layering_variants(graph, layer, tagged("default"), "barycenter", 8L)$tag
  }
  # The trial passes the screen (one bend for 17% less area), but after both
  # placement passes it would keep 12 bends where the default drawing has 9.
  expect_true(.layout_variant_improves(measured$trial, measured$default, measured$default))
  expect_identical(kept(quality(12, 132, c(17.5, 20))), "relaxed default")
  # Still better after relaxation: the refined, relaxed layering is kept.
  expect_identical(kept(quality(9, 140, c(17.5, 20))), "relaxed trial")
  # A crossing reduction may spend one bend per removed crossing, counted from
  # the relaxed default (9), not from the 11 bends the screen started with.
  expect_identical(kept(quality(10, 140, c(17.5, 20), crossings = 1)), "relaxed trial")
  expect_identical(kept(quality(11, 140, c(17.5, 20), crossings = 1)), "relaxed default")
  # Its extent allowance stays 10% over the drawing the search started with
  # (20 x 21), although relaxation compacted the default to 17.5 x 20.
  expect_identical(kept(quality(9, 140, c(22, 18), crossings = 1)), "relaxed trial")
  expect_identical(kept(quality(9, 140, c(22.5, 18), crossings = 1)), "relaxed default")
})

test_that("DAGitty's Thoemmes example from sink-aligned ranks keeps its relaxed bends", {
  skip_on_cran()
  # A minimum-span start won the screen here with 12 bends against 11; the
  # relaxed sink-aligned drawing has 9 at the same crossings and area.
  graph <- igraph::graph_from_edgelist(rbind(c("e0", "x"), c("e1", "s1"), c("e2", "s2"),
    c("e2", "y"), c("e3", "s3"), c("e4", "y"), c("s1", "s2"), c("s1", "z"), c("s2", "s3"),
    c("s2", "z2"), c("s3", "y"), c("x", "s1"), c("x", "y"), c("y", "z3")))
  after <- layout_metrics(layout_metro_dag(graph, layering = "sinks"))
  old <- options(metrodag.control = list(stages = c(layering_variants = FALSE)))
  on.exit(options(old), add = TRUE)
  before <- layout_metrics(layout_metro_dag(graph, layering = "sinks"))
  expect_lte(after$crossings, before$crossings)
  expect_lte(after$total_bends, before$total_bends)
  expect_lte(after$shared_track_length, before$shared_track_length + 1e-8)
})

test_that("a restart variant is refined even when it starts with more shared track", {
  skip_on_cran()
  # The "children" start of this review case has as many crossings as the
  # component's own drawing and more shared track before rank refinement;
  # refinement removes both, and the refined start wins. Skipping refinement
  # for such starts left 3 crossings instead of 2 and 23% more area.
  graph <- metrodag_examples("random", n = 18, probability = 2.5 / 18, seed = 501)
  igraph::V(graph)$name <- c("Age (years)", "BMI", "a b", "smoking status", "10", "2", "Z",
    "z", "_x", "X.1", "A", "a", "b", "B", "NA", "1e5", "y", "Y")
  refine <- metrodag:::.refine_rank_assignment
  restart <- metrodag:::.refine_layout_variants
  seen <- list()
  testthat::local_mocked_bindings(
    .refine_rank_assignment = function(graph, geometry, ordering, sweeps, ...) {
      out <- refine(graph, geometry, ordering, sweeps, ...)
      if (identical(list(...)$variant, "children")) seen$children <<- list(
        before = .measure_variant(geometry)$quality, after = .measure_variant(out)$quality)
      out
    },
    .refine_layout_variants = function(graph, layer, geometry, ...) {
      out <- restart(graph, layer, geometry, ...)
      if (is.null(seen$own)) seen[c("own", "kept")] <<- list(.measure_variant(geometry)$quality,
        .measure_variant(out)$quality)
      out
    }, .package = "metrodag")
  layout_metro_dag(graph)
  conflicts <- c("crossings", "shared")
  expect_false(all(seen$children$before[conflicts] <= seen$own[conflicts] + 1e-8))
  expect_lt(seen$children$after[["crossings"]], seen$own[["crossings"]])
  expect_identical(seen$kept, seen$children$after)
})
