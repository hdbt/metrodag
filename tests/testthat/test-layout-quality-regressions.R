# Layout-quality regression tests: fan routing, crossing pairs, sifting,
# source placement, route cleanup and gallery baselines.
# Run from package root: testthat::test_file("tests/testthat/test-layout-quality-regressions.R")
# or as part of the full suite: testthat::test_local()
#
# Each test uses only public functions (layout_metro_dag, layout_metrics,
# metro_dag) and asserts on layout_metrics() output.  They document the
# *expected improvement* from each refinement so a regression is immediately visible.

library(testthat)
library(metrodag)

# ---------------------------------------------------------------------------
# Helper: build a layout from a data.frame of edges
# layout_metro_dag() accepts only igraph/dagitty input; normalise edge tables
# and matrices exactly as metro_dag() does before laying them out.
layout_from <- function(edges, ...) {
  layout_metro_dag(metrodag:::.as_metro_input(edges), direction = "right", ...)
}
metrics_from <- function(edges, ...) {
  layout_metrics(layout_from(edges, ...))
}
edge_list <- function(text) {
  matrix(strsplit(text, "[[:space:]]+")[[1L]], ncol = 2L, byrow = TRUE,
    dimnames = list(NULL, c("from", "to")))
}
# Layouts that several tests inspect are computed once for the file: the named
# examples (facing right, as layout_from() lays them out) and the annotated
# gallery DAGs of helper-gallery.R, laid out facing down with their supplied
# ranks and alignment and turned right where a test needs that direction.
example_layout <- .test_memo(function(name) layout_from(metrodag_examples(name)))
example_metrics <- function(name) layout_metrics(example_layout(name))
gallery_down <- .test_memo(function(k) .test_gallery_layout(as.integer(k), direction = "down"))
gallery_layout <- function(k) .test_rotate_layout(gallery_down(k), "right")

# ===========================================================================
# pairwise shared-track cost in initial router
# ---------------------------------------------------------------------------
# A station can be entered from only three monotone octilinear directions
# (two diagonals and straight on), so a pure fan of in-degree k >= 4 must merge
# or touch near its target: at least k - 3 contacts are unavoidable.
test_that("4-parent fan reaches the octilinear contact minimum", {
  edges <- data.frame(
    from = c("A", "B", "C", "D"),
    to   = c("Y", "Y", "Y", "Y")
  )
  m <- metrics_from(edges)
  expect_lte(m$crossings, 1)
  expect_lte(m$shared_track_length, 0.5)
})

test_that("5-parent fan reaches the octilinear contact minimum", {
  edges <- data.frame(
    from = c("A", "B", "C", "D", "E"),
    to   = c("Y", "Y", "Y", "Y", "Y")
  )
  m <- metrics_from(edges)
  expect_lte(m$crossings, 2)
})

# ===========================================================================
# crossing-pair refinement sorted by crossing count
# ---------------------------------------------------------------------------
# K(2,2) on two ranks: two monotone arrows with swapped end orders must cross
# once, so one crossing is the layered optimum.
test_that("two-rank K(2,2) keeps its single unavoidable crossing", {
  edges <- data.frame(
    from = c("X", "A", "X", "A"),
    to   = c("M", "Y", "Y", "M")
  )
  m <- metrics_from(edges)
  expect_lte(m$crossings, 1)
  expect_equal(m$total_bends, 0)
})

# ===========================================================================
# sifting within ranks
# ---------------------------------------------------------------------------
test_that("sifting finds optimal ordering for 4-node rank", {
  edges <- data.frame(
    from = c("A", "A", "B", "B"),
    to   = c("P", "R", "Q", "S")
  )
  m <- metrics_from(edges)
  expect_lte(m$crossings, 0)
})

# K(2,3) on two ranks: every pair of the three children contributes one
# crossing in any order, so three crossings is the layered optimum.
test_that("3-node rank with crossed connectivity is optimally ordered", {
  edges <- data.frame(
    from = c("S", "S", "S", "T", "T", "T"),
    to   = c("X", "Y", "Z", "Z", "Y", "X")
  )
  m <- metrics_from(edges)
  expect_lte(m$crossings, 3)
})

# ===========================================================================
# latent/unobserved source placement
# ---------------------------------------------------------------------------
test_that("latent source is placed between its children (not at an extreme)", {
  edges <- data.frame(
    from = c("U", "U", "X", "X"),
    to   = c("A", "B", "A", "B")
  )
  layout <- layout_from(edges)
  # direction = "right": ranks run along x, so placement is measured on y.
  u_y <- layout$nodes$y[layout$nodes$node == "U"]
  a_y <- layout$nodes$y[layout$nodes$node == "A"]
  b_y <- layout$nodes$y[layout$nodes$node == "B"]
  expect_gte(u_y, min(a_y, b_y) - 1)
  expect_lte(u_y, max(a_y, b_y) + 1)
})

test_that("latent source spanning 3 ranks gets a shorter total bypass", {
  edges <- data.frame(
    from = c("U", "U", "A", "M"),
    to   = c("A", "Y", "M", "Y")
  )
  layout <- layout_from(edges)
  routes <- split(layout$edges, layout$edges$edge_id)
  bypass <- Filter(function(e) e$from[1L] == "U" && e$to[1L] == "Y", routes)
  if (length(bypass)) expect_lte(sum(bypass[[1L]]$bend), 2)
})

# ===========================================================================
# wider lane search and span-weighted route cleanup
# ---------------------------------------------------------------------------
test_that("4-rank bypass arrow has at most 2 bends", {
  edges <- data.frame(
    from = c("I", "I", "A", "B", "C"),
    to   = c("A", "Y", "B", "C", "Y")
  )
  layout <- layout_from(edges)
  routes <- split(layout$edges, layout$edges$edge_id)
  bypass <- Filter(function(e) e$from[1L] == "I" && e$to[1L] == "Y", routes)
  if (length(bypass)) expect_lte(sum(bypass[[1L]]$bend), 2)
})

test_that("span-weighted cleanup reduces bends on long staircase arrows", {
  edges <- data.frame(
    from = c("I", "I", "X", "X", "A", "B"),
    to   = c("A", "B", "A", "B", "Y", "Y")
  )
  m <- metrics_from(edges)
  expect_lte(m$total_bends, 5)
})

# ===========================================================================
# node-spacing variance and singleton-rank median pass
# ---------------------------------------------------------------------------
test_that("singleton-rank node aligns near its neighbours' median x", {
  edges <- data.frame(
    from = c("A", "B", "S", "S"),
    to   = c("S", "S", "C", "D")
  )
  layout <- layout_from(edges)
  s_x <- layout$nodes$x[layout$nodes$node == "S"]
  a_x <- layout$nodes$x[layout$nodes$node == "A"]
  b_x <- layout$nodes$x[layout$nodes$node == "B"]
  c_x <- layout$nodes$x[layout$nodes$node == "C"]
  d_x <- layout$nodes$x[layout$nodes$node == "D"]
  med_x <- stats::median(c(a_x, b_x, c_x, d_x))
  expect_lte(abs(s_x - med_x), 4)
})

test_that("node_spacing_variance is reasonable for the causal example", {
  m <- example_metrics("causal")
  expect_lte(m$node_spacing_variance, 20)
})

# ===========================================================================
# coordinated fan routing for high-degree convergent nodes
# ---------------------------------------------------------------------------
test_that("3-parent fan has zero crossings", {
  edges <- data.frame(
    from = c("A", "B", "C"),
    to   = c("Y", "Y", "Y")
  )
  m <- metrics_from(edges)
  expect_equal(m$crossings, 0)
  expect_equal(m$shared_track_length, 0)
})

# Routing fans last was tried and removed; these are stability guards at the
# values produced without it (in-degree 5 and 6 need >= 2 and >= 3 contacts).
test_that("5-parent fan (DAG 5 pattern) stays within its guard", {
  edges <- data.frame(
    from = c("E", "I", "M", "X", "U", "E", "X"),
    to   = c("Y", "Y", "Y", "Y", "Y", "X", "I")
  )
  m <- metrics_from(edges)
  expect_lte(m$crossings, 3)
})

test_that("6-parent fan (DAG 3 pattern) stays within its guard", {
  edges <- data.frame(
    from = c("W", "A", "S", "C", "H", "U", "O", "C", "C"),
    to   = c("O", "O", "O", "O", "O", "O", "Y", "S", "A")
  )
  m <- metrics_from(edges)
  expect_lte(m$crossings, 4)
})

test_that("2-parent collider is crossing-free with one bend per arrow", {
  # Straight diagonals would need the parents 8 units apart; the compact
  # 4-unit spacing costs one bend per arrow instead.
  edges <- data.frame(from = c("A", "B"), to = c("Y", "Y"))
  m <- metrics_from(edges)
  expect_equal(m$crossings, 0)
  expect_lte(m$total_bends, 2)
})

# ===========================================================================
# alignment crossing budget scaling
# ---------------------------------------------------------------------------
test_that("aligned 2-wave panel crossing cost is bounded", {
  edges <- data.frame(
    from = c("X1", "Y1", "X1", "Y1"),
    to   = c("X2", "Y2", "Y2", "X2")
  )
  ranks  <- c(X1 = 0, Y1 = 0, X2 = 1, Y2 = 1)
  aligns <- list(c("X1", "X2"), c("Y1", "Y2"))
  m_aligned   <- metrics_from(edges, rank = ranks, align = aligns)
  m_rank_only <- metrics_from(edges, rank = ranks)
  expect_lte(m_aligned$crossings - m_rank_only$crossings, 2)
})

test_that("3-wave panel alignment crossing cost bounded", {
  edges <- edge_list("X1 X2 X2 X3 Y1 Y2 Y2 Y3 X1 Y2 Y1 X2 X2 Y3 Y2 X3")
  ranks  <- c(X1 = 0, Y1 = 0, X2 = 1, Y2 = 1, X3 = 2, Y3 = 2)
  aligns <- list(c("X1", "X2", "X3"), c("Y1", "Y2", "Y3"))
  m_aligned   <- metrics_from(edges, rank = ranks, align = aligns)
  m_rank_only <- metrics_from(edges, rank = ranks)
  expect_lte(m_aligned$crossings - m_rank_only$crossings, 4)
})

# ===========================================================================
# Stability: built-in named examples must not regress
# ---------------------------------------------------------------------------
test_that("chain has 0 crossings and 0 bends", {
  m <- example_metrics("chain")
  expect_equal(m$crossings, 0)
  expect_equal(m$total_bends, 0)
})

test_that("fork has 0 crossings and 0 bends", {
  m <- example_metrics("fork")
  expect_equal(m$crossings, 0)
  expect_equal(m$total_bends, 0)
})

test_that("collider has 0 crossings and 0 bends", {
  m <- example_metrics("collider")
  expect_equal(m$crossings, 0)
  expect_equal(m$total_bends, 0)
})

test_that("diamond has 0 crossings and at most 4 bends", {
  m <- example_metrics("diamond")
  expect_equal(m$crossings, 0)
  expect_lte(m$total_bends, 4)
})

test_that("causal example has 0 crossings and at most 4 bends", {
  m <- example_metrics("causal")
  expect_equal(m$crossings, 0)
  expect_lte(m$total_bends, 4)
})

test_that("long_edge example has 0 crossings", {
  m <- example_metrics("long_edge")
  expect_equal(m$crossings, 0)
})

# ===========================================================================
# Gallery smoke tests: no annotated DAG may exceed its committed baseline. The
# per-DAG crossing and bend guards are the "gallery ... committed baseline"
# tests below, which lay each DAG out once.
# ---------------------------------------------------------------------------

# ===========================================================================
# Route integrity, determinism and attachment regressions
# ---------------------------------------------------------------------------
routes_attached <- function(nodes, routes) {
  all(vapply(routes, function(e) {
    u <- match(e$from[1L], nodes$node); v <- match(e$to[1L], nodes$node)
    e$x[1L] == nodes$x[u] && e$y[1L] == nodes$y[u] &&
      tail(e$xend, 1L) == nodes$x[v] && tail(e$yend, 1L) == nodes$y[v] &&
      all(head(e$xend, -1L) == e$x[-1L]) && all(head(e$yend, -1L) == e$y[-1L])
  }, logical(1)))
}
unit_route <- function(nodes, from, to, moves, id) {
  u <- match(from, nodes$node)
  e <- metrodag:::.route_from_moves(nodes$x[u], nodes$y[u], moves)
  e$from <- from; e$to <- to; e$edge_id <- id; e$path_id <- NA_integer_
  e$segment <- seq_len(nrow(e)); e$bend <- c(FALSE, diff(moves) != 0)
  e[, names(metrodag:::.empty_routes())]
}

test_that("an accepted singleton slide returns routes that follow the node", {
  nodes <- data.frame(node = c("A", "B"), x = c(0, 4), y = c(0, -4),
    stringsAsFactors = FALSE)
  routes <- list(unit_route(nodes, "A", "B", c(1, 1, 1, 1), 1L))
  out <- metrodag:::.slide_singleton_ranks(nodes, routes)
  expect_named(out, c("nodes", "routes"))
  expect_equal(out$nodes$x[out$nodes$node == "A"], 4)
  expect_true(routes_attached(out$nodes, out$routes))
  expect_equal(sum(out$routes[[1L]]$bend), 0)
})

test_that("layouts with singleton slides keep every arrow attached", {
  graphs <- list(metrodag_examples("long_edge"),
    metrodag_examples("random", n = 6L, probability = 0.3, seed = 1L),
    metrodag_examples("random", n = 6L, probability = 0.3, seed = 5L),
    metrodag_examples("random", n = 8L, probability = 0.3, seed = 1L),
    metrodag_examples("random", n = 8L, probability = 0.3, seed = 11L))
  layouts <- c(lapply(graphs, function(g) layout_metro_dag(direction = "down", g)),
    list(gallery_down(2L)))
  for (layout in layouts) {
    expect_silent(metrodag:::.assert_layout_geometry(layout))
  }
})

test_that("accepted crossing-pair moves never worsen the whole drawing", {
  graph <- metrodag_examples("causal")
  nodes <- data.frame(node = igraph::V(graph)$name,
    x = c(6, 1, 1, 1, 4, -4), y = c(-4, -11, -21, -15, 0, 0))
  moves <- list(c(rep(-1, 5), 0, 0), c(rep(0, 12), rep(-1, 5)),
    rep(0, 4), rep(0, 6), c(1, 1, 0, 0), c(rep(0, 8), rep(-1, 3)),
    c(rep(0, 16), rep(1, 5)), c(1, 1, rep(0, 6), 1, 1, 1))
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  routes <- lapply(seq_along(moves), function(id)
    unit_route(nodes, nodes$node[ends[id, 1L]], nodes$node[ends[id, 2L]], moves[[id]], id))
  before <- metrodag:::.routed_geometry_quality(routes)
  out <- metrodag:::.reroute_crossing_pairs(routes, nodes)
  after <- metrodag:::.routed_geometry_quality(out)
  expect_lt(after[["crossings"]], before[["crossings"]])
  expect_equal(after[["crossings"]], 0)
  expect_lte(after[["shared"]], before[["shared"]] + 1e-8)
  expect_lte(after[["length"]], before[["length"]] + 1e-8)
  expect_true(routes_attached(nodes, out))
  # Fixed (backbone) edges are never rerouted.
  fixed <- metrodag:::.reroute_crossing_pairs(routes, nodes, fixed = seq_along(routes))
  expect_identical(fixed, routes)
})

test_that("reordered equivalent inputs give identical layouts", {
  snapshot <- function(layout) {
    n <- layout$nodes[order(layout$nodes$node), c("node", "x", "y")]
    rownames(n) <- NULL
    n
  }
  for (seed in c(1L, 3L, 7L, 11L)) {
    g <- metrodag_examples("random", n = 10L, probability = 0.3, seed = seed)
    edges <- igraph::as_edgelist(g)
    reordered <- igraph::graph_from_data_frame(edges[rev(seq_len(nrow(edges))), , drop = FALSE],
      directed = TRUE, vertices = data.frame(name = rev(igraph::V(g)$name)))
    expect_identical(snapshot(layout_metro_dag(direction = "down", reordered)), snapshot(layout_metro_dag(direction = "down", g)))
  }
})

test_that("sifting order is independent of vertex numbering", {
  # Two ranks: a permutation of vertex ids must not change crossings or orders.
  g <- igraph::graph_from_literal(A -+ Q, A -+ S, B -+ P, B -+ R, C -+ Q, C -+ P)
  layer <- c(A = 0, B = 0, C = 0, P = 1, Q = 1, R = 1, S = 1)[igraph::V(g)$name]
  ord <- metrodag:::.order_layers(g, layer)
  perm <- rev(seq_len(igraph::vcount(g)))
  h <- igraph::permute(g, perm)
  ord_h <- metrodag:::.order_layers(h, layer[order(perm)])
  names(ord) <- igraph::V(g)$name; names(ord_h) <- igraph::V(h)$name
  expect_identical(ord_h[names(ord)], ord)
  ends <- igraph::ends(g, igraph::E(g), names = FALSE)
  expect_equal(metrodag:::.crossings_for_orders(ends, layer, ord), 0L)
})

test_that("a balanced three-parent fan is routed without crossings or bends", {
  m <- example_metrics("collider")
  expect_equal(m$crossings, 0)
  expect_equal(m$total_bends, 0)
  expect_equal(m$shared_track_length, 0)
})

test_that("simple route candidates stay within two lanes of the route", {
  nodes <- data.frame(node = c("A", "Y"), x = c(0, 0), y = c(0, -12))
  e <- unit_route(nodes, "A", "Y", c(1, 1, rep(0, 8), -1, -1), 1L)
  for (z in metrodag:::.simple_route_candidates(e)) {
    expect_gte(min(z$x, z$xend), -2)
    expect_lte(max(z$x, z$xend), 4)
  }
  # A blocked vertical arrow keeps a two-bend detour instead of a wide V.
  blocked <- rbind(nodes, data.frame(node = "B", x = 0, y = -6))
  out <- metrodag:::.straighten_routes(list(e), blocked)[[1L]]
  expect_equal(sum(out$bend), 2L)
  expect_lte(max(abs(out$xend)), 2)
})

# ===========================================================================
# rerouting a moved node ranks crossings before shared track
# ---------------------------------------------------------------------------
# Minimal fixture reduced from DAG 1: source I moves two units downstream so
# its arrow to X can become straight. Its other arrow, I -> Y, joins a sink
# that M -> Y enters straight and X -> Y enters on the final diagonal.
p16_fixture <- function() {
  nodes <- data.frame(node = c("I", "X", "Y", "M"), x = c(-4, 0, 0, 0),
    y = c(0, -6, -17, -10), stringsAsFactors = FALSE)
  routes <- list(
    unit_route(nodes, "I", "Y", c(rep(0, 13), 1, 1, 1, 1), 1L),
    unit_route(nodes, "M", "Y", rep(0, 7), 2L),
    unit_route(nodes, "X", "Y", c(-1, rep(0, 9), 1), 3L))
  list(nodes = nodes, routes = routes)
}

test_that("rerouting a moved source keeps a conflict-neutral route", {
  f <- p16_fixture()
  before <- metrodag:::.routed_geometry_quality(f$routes)
  moved <- f$nodes
  moved$y[moved$node == "I"] <- -2
  from <- match(vapply(f$routes, function(e) e$from[1L], ""), moved$node)
  to <- match(vapply(f$routes, function(e) e$to[1L], ""), moved$node)
  out <- metrodag:::.reroute_moved_nodes(f$routes, moved, 1L, from, to,
    crossings_first = TRUE)
  expect_true(routes_attached(moved, out))
  after <- metrodag:::.routed_geometry_quality(out)
  # Overlap-first scoring (still the default for every other caller) chooses a
  # late straight approach: 0.41 units less shared track for one extra crossing,
  # which the source-placement acceptance test then rejects.
  default <- metrodag:::.reroute_moved_nodes(f$routes, moved, 1L, from, to)
  expect_equal(metrodag:::.routed_geometry_quality(default)[["crossings"]],
    before[["crossings"]] + 1)
  expect_equal(after[["crossings"]], before[["crossings"]])
  expect_lte(after[["shared"]], before[["shared"]] + 1e-8)
  expect_lte(after[["bends"]], before[["bends"]])
  expect_equal(tail(out[[1L]]$xend - out[[1L]]$x, 4L), c(1, 1, 1, 1))
})

test_that("the rerouted arrow has the fewest crossings of its clear candidates", {
  f <- p16_fixture()
  moved <- f$nodes
  moved$y[moved$node == "I"] <- -2
  from <- match(vapply(f$routes, function(e) e$from[1L], ""), moved$node)
  to <- match(vapply(f$routes, function(e) e$to[1L], ""), moved$node)
  out <- metrodag:::.reroute_moved_nodes(f$routes, moved, 1L, from, to,
    crossings_first = TRUE)
  obstacles <- metrodag:::.prepare_route_obstacles(do.call(rbind, f$routes[-1L]))
  unrelated <- moved[!moved$node %in% c("I", "Y"), , drop = FALSE]
  skeleton <- metrodag:::.route_from_moves(-4, -2, rep(0, 15))
  skeleton$xend[15L] <- 0
  crossings <- vapply(metrodag:::.simple_route_candidates(skeleton), function(z) {
    if (!metrodag:::.route_obstacle_free(metrodag:::.route_runs(z), unrelated)) return(Inf)
    metrodag:::.route_conflicts(z, obstacles)[["crossings"]]
  }, numeric(1))
  chosen <- metrodag:::.route_conflicts(out[[1L]], obstacles)[["crossings"]]
  expect_equal(chosen, min(crossings))
})

test_that("source placement never returns worse than its default pass", {
  measure <- function(z) {
    e <- do.call(rbind, z$routes)
    c(metrodag:::.routed_geometry_quality(z$routes),
      diff(range(c(z$nodes$x, e$x, e$xend))), diff(range(c(z$nodes$y, e$y, e$yend))))
  }
  graphs <- list(metrodag_examples("random", n = 8L, probability = 0.3, seed = 5L),
    metrodag_examples("random", n = 10L, probability = 0.3, seed = 5L))
  layouts <- c(list(gallery_down(1L)),
    lapply(graphs, function(g) layout_metro_dag(direction = "down", g)))
  for (layout in layouts) {
    nodes <- layout$nodes[c("node", "x", "y")]
    # Re-run the pass on a deliberately stretched copy: every source moves upstream.
    sources <- setdiff(layout$edges$from, layout$edges$to)
    shift <- ifelse(nodes$node %in% sources, 4, 0)
    nodes$y <- nodes$y + shift
    routes <- lapply(unname(split(layout$edges, layout$edges$edge_id)), function(e) {
      u <- match(e$from[1L], nodes$node); v <- match(e$to[1L], nodes$node)
      h <- nodes$y[u] - nodes$y[v]; dx <- nodes$x[v] - nodes$x[u]
      unit_route(nodes, e$from[1L], e$to[1L], c(rep(0, h - abs(dx)), rep(sign(dx), abs(dx))),
        e$edge_id[1L])
    })
    if (any(vapply(routes, function(e) any(e$x != round(e$x)), logical(1)))) next
    default <- metrodag:::.relax_source_pass(nodes, routes)
    chosen <- metrodag:::.relax_source_positions(nodes, routes)
    expect_true(routes_attached(chosen$nodes, chosen$routes))
    a <- measure(default); b <- measure(chosen)
    expect_true(identical(chosen, default) || (all(b <= a + 1e-8) && any(b < a - 1e-8)))
  }
})

test_that("DAG 1's household-income arrow becomes a straight diagonal", {
  layout <- gallery_layout(1L)
  e <- layout$edges[layout$edges$from == "I" & layout$edges$to == "X", ]
  expect_equal(sum(e$bend), 0)
  m <- layout_metrics(layout)
  expect_lte(m$crossings, 3)
  expect_lte(m$total_bends, 8)
  expect_lte(m$shared_track_length, 1.9142135623731 + 1e-8)
})

# ===========================================================================
# Stability guards: committed gallery baseline (annotated_gallery_metrics.csv)
# and route integrity of the named examples. Source relaxation reduced DAG 1 bends from 10 to 8.
# ---------------------------------------------------------------------------
# Committed baselines (annotated_gallery_metrics.csv) of the gallery DAGs in
# helper-gallery.R, by position.
gallery_baselines <- list(
  `01_alcohol_heart` = c(crossings = 3, bends = 8, shared = 1.9142135623731),
  `02_air_pollution` = c(crossings = 2, bends = 7, shared = 1.4142135623731),
  `03_sugary_drinks` = c(crossings = 6, bends = 10, shared = 3.82842712474619),
  `04_statin_events` = c(crossings = 4, bends = 13, shared = 0.5),
  `05_obesity_admission` = c(crossings = 5, bends = 10, shared = 2.41421356237309),
  `06_longitudinal` = c(crossings = 8, bends = 15, shared = 0))
stopifnot(identical(names(gallery_baselines),
  vapply(.metro_gallery_examples, `[[`, "", "id")))

for (k in seq_along(gallery_baselines)) local({
  k <- k
  id <- names(gallery_baselines)[k]
  case <- gallery_baselines[[k]]
  test_that(paste("gallery", id, "does not exceed its committed baseline"), {
    layout <- gallery_layout(k)
    expect_silent(metrodag:::.assert_layout_geometry(layout))
    m <- layout_metrics(layout)
    expect_lte(m$crossings, case[["crossings"]])
    expect_lte(m$total_bends, case[["bends"]])
    expect_lte(m$shared_track_length, case[["shared"]] + 1e-8)
    expect_equal(m$node_edge_collisions, 0)
  })
})

test_that("named examples keep attached routes and their committed metrics", {
  expected <- list(chain = c(0, 0), fork = c(0, 0), collider = c(0, 0),
    diamond = c(0, 4), causal = c(0, 4), long_edge = c(0, NA))
  for (name in names(expected)) {
    layout <- example_layout(name)
    expect_silent(metrodag:::.assert_layout_geometry(layout))
    m <- layout_metrics(layout)
    expect_equal(m$crossings, expected[[name]][1L])
    if (!is.na(expected[[name]][2L])) expect_lte(m$total_bends, expected[[name]][2L])
    expect_equal(m$node_edge_collisions, 0)
  }
})

# ===========================================================================
# V-overshoot start in the layout-variant search
# ---------------------------------------------------------------------------
# Initial straightening ranked a one-bend V detour (no vertical run, apex far
# outside its endpoints) above a short two-bend lane. Later passes could not
# remove it without an extra bend. The README confounding example is the
# minimal reproduction: on main U -> Y climbed four units past its endpoints.
readme_graph <- function(isolate = FALSE) {
  names <- c("C", "X", "Y", "U", if (isolate) "I")
  metro_graph(data.frame(name = names),
    data.frame(from = c("C", "C", "X", "U", "U"), to = c("X", "Y", "Y", "X", "Y")))
}

test_that("route overshoot measures excursion beyond the endpoint range", {
  v <- data.frame(edge_id = 1L, segment = 1:10,
    x = c(2, 3, 4, 5, 6, 5, 4, 3, 2, 1), xend = c(3, 4, 5, 6, 5, 4, 3, 2, 1, 0))
  lane <- data.frame(edge_id = 2L, segment = 1:9,
    x = c(2, 3, 3, 3, 3, 3, 3, 2, 1), xend = c(3, 3, 3, 3, 3, 3, 2, 1, 0))
  expect_equal(metrodag:::.route_overshoot(v), 2)      # apex 6, endpoints [0, 2], margin 2
  expect_equal(metrodag:::.route_overshoot(lane), 0)   # one unit outside: within margin
  expect_equal(metrodag:::.route_overshoot(rbind(lane, v)), 2)
  expect_equal(metrodag:::.route_overshoot(v[0, ]), 0)
})

test_that("endpoint lanes exclude the V detour but keep the route's own window by default", {
  e <- data.frame(x = c(2, 3, 4, 4, 4, 4, 4, 4, 3, 2), y = -(0:9),
    xend = c(3, 4, 4, 4, 4, 4, 4, 3, 2, 1), yend = -(1:10))
  widest <- function(cands) max(vapply(cands, function(z) max(c(z$x, z$xend)), numeric(1)))
  expect_equal(widest(metrodag:::.simple_route_candidates(e)), 6)
  expect_equal(widest(metrodag:::.simple_route_candidates(e, endpoint_lanes = TRUE)), 4)
})

test_that("the overshoot guard never buys space with conflicts, ink or a wider box", {
  improves <- metrodag:::.overshoot_variant_improves
  current <- list(quality = c(crossings = 0, shared = 0, bends = 4, length = 40.63), extent = c(8, 10))
  make <- function(crossings = 0, shared = 0, bends = 5, length = 35.14, extent = c(5, 9))
    list(quality = c(crossings = crossings, shared = shared, bends = bends, length = length),
      extent = extent)
  expect_true(improves(make(), current))
  expect_false(improves(make(crossings = 1), current))
  expect_false(improves(make(shared = 0.5), current))
  expect_false(improves(make(bends = 6), current))          # at most one extra bend
  expect_false(improves(make(length = 41), current))        # no extra line length
  expect_false(improves(make(extent = c(9, 5)), current))   # neither extent may grow
  expect_false(improves(make(extent = c(7.5, 9.5)), current))  # < 15% smaller area
})

test_that("the endpoint-lane start runs only when a V overshoot exists", {
  calls <- character()
  real <- metrodag:::.layout_component
  local_mocked_bindings(.layout_component = function(...) {
    variant <- list(...)$variant
    calls <<- c(calls, if (is.null(variant)) "default" else variant)
    real(...)
  }, .package = "metrodag")
  layout_metro_dag(direction = "down", metrodag_examples("diamond"))
  expect_false("endpoint_lanes" %in% calls)
  calls <- character()
  layout_metro_dag(direction = "down", readme_graph())
  expect_true("endpoint_lanes" %in% calls)
})

test_that("the README confounding example loses its V detour and half its area", {
  layout <- layout_metro_dag(readme_graph(), direction = "down")
  expect_silent(metrodag:::.assert_layout_geometry(layout))
  expect_equal(metrodag:::.route_overshoot(layout$edges), 0)
  m <- layout_metrics(layout)
  # main @ 0f98c08: crossings 0, shared 0, bends 4, length 40.63, area 80.
  expect_equal(m$crossings, 0)
  expect_equal(m$shared_track_length, 0)
  expect_lte(m$total_bends, 5)
  expect_lt(m$total_edge_length, 40.62742 - 1)
  expect_lte(m$area, 0.85 * 80)
  expect_equal(m$node_edge_collisions, 0)
})

# ===========================================================================
# component packing keeps extra components at the first rank
# ---------------------------------------------------------------------------
test_that("an isolated variable shares the first rank instead of following the outcome", {
  g <- readme_graph(isolate = TRUE)
  layout <- layout_metro_dag(g, direction = "down")
  expect_silent(metrodag:::.assert_layout_geometry(layout))
  n <- layout$nodes
  isolate <- n$node == "I"
  expect_equal(n$y[isolate], max(n$y))                       # top rank, with the sources
  expect_gt(n$y[isolate], n$y[n$node == "Y"])                # not downstream of the outcome
  distance <- sqrt((n$x[!isolate] - n$x[isolate])^2 + (n$y[!isolate] - n$y[isolate])^2)
  expect_gte(min(distance), 4)
  expect_identical(layout, layout_metro_dag(g, direction = "down"))
})

test_that("shelf packing never enlarges drawings relative to main", {
  # Areas measured on main @ 0f98c08 (direction = "right"). The square-shelf
  # packing remains the fallback whenever the depth-fitting shelf is larger.
  baseline <- list(c(6, 3, 119), c(8, 4, 216), c(6, 1, 130), c(10, 7, 518))
  for (case in baseline) {
    g <- metrodag_examples("random", case[1L], probability = 0.3, seed = case[2L])
    layout <- layout_metro_dag(g, direction = "right")
    expect_silent(metrodag:::.assert_layout_geometry(layout))
    expect_lte(layout_metrics(layout)$area, case[3L])
    nodes <- layout$nodes
    expect_gte(min(stats::dist(nodes[c("x", "y")])), 4 - 1e-8)
  }
})
