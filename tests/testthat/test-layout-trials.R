# Rebuilds of one component share their drawings: a start that is drawn again
# (the same proper layering, layer order, coordinates and fan mode) must give
# exactly the drawing a fresh pipeline gives, without being routed again.

.test_trial_graph <- function() .test_gallery_graph(4L)

.test_count_finishes <- function(env) {
  finish <- metrodag:::.finish_component
  testthat::local_mocked_bindings(.finish_component = function(...) {
    env$finished <- env$finished + 1L
    finish(...)
  }, .package = "metrodag", .env = parent.frame())
}

test_that("a repeated rebuild reuses its drawing and equals a fresh one", {
  graph <- .test_trial_graph()
  layer <- .assign_layers(graph, "longest")
  candidate <- .rank_assignment_candidates(graph, layer)[[1L]]
  fresh <- .layout_component(graph, candidate, "barycenter", 8L, ranks = "fixed")
  counter <- new.env(); counter$finished <- 0L
  .test_count_finishes(counter)
  trials <- .layout_trials()
  first <- .layout_component(graph, candidate, "barycenter", 8L, ranks = "fixed",
    trials = trials)
  # Unnamed ranks with the same values are the same start; the drawing keeps
  # the ranks of the call that asks for it.
  again <- .layout_component(graph, unname(candidate), "barycenter", 8L, ranks = "fixed",
    variant = "layering", trials = trials)
  expect_identical(first, fresh)
  expect_identical(again$layer, unname(candidate))
  expect_identical(again[names(again) != "layer"], fresh[names(fresh) != "layer"])
  expect_identical(counter$finished, 1L)
  expect_length(trials$entries, 1L)
})

test_that("fan straightening that moves nothing reuses the fixed-rank drawing", {
  graph <- .test_trial_graph()
  layer <- .assign_layers(graph, "longest")
  starts <- .layout_layering_starts(graph, layer)
  unrecorded <- function(...) .metro_unrecorded(.layout_component(...))
  counter <- new.env()
  .test_count_finishes(counter)
  reused <- 0L
  for (start in starts) {
    counter$finished <- 0L
    fresh <- unrecorded(graph, start, "barycenter", 8L, variant = "layering")
    expect_identical(counter$finished, 1L)
    trials <- .layout_trials()
    fixed <- unrecorded(graph, start, "barycenter", 8L, ranks = "fixed", trials = trials)
    again <- unrecorded(graph, start, "barycenter", 8L, variant = "layering", trials = trials)
    expect_identical(again, fresh)
    # Unmoved, both modes share one drawing; moved, the automatic one is its own.
    entry <- trials$entries[[1L]]
    shared <- identical(entry$drawings$automatic, entry$drawings$plain)
    expect_identical(counter$finished, if (shared) 2L else 3L)
    if (shared) {
      expect_identical(fixed, fresh)
      reused <- reused + 1L
    }
  }
  # Both layering starts of this graph leave fan straightening nothing to do.
  expect_identical(reused, 2L)
  # Different starts keep separate entries.
  trials <- .layout_trials()
  unrecorded(graph, layer, "barycenter", 8L, ranks = "fixed", trials = trials)
  unrecorded(graph, layer, "barycenter", 8L, ranks = "fixed", variant = "endpoint_lanes",
    trials = trials)
  expect_length(trials$entries, 2L)
})

test_that("fan straightening that moves a start keeps its own automatic drawing", {
  graph <- metrodag_examples("random", n = 5, probability = 3 / 5, seed = 2)
  start <- .layout_layering_starts(graph, .assign_layers(graph, "longest"))[[1L]]
  unrecorded <- function(...) .metro_unrecorded(.layout_component(...))
  counter <- new.env(); counter$finished <- 0L
  .test_count_finishes(counter)
  fresh <- unrecorded(graph, start, "barycenter", 8L, variant = "layering")
  trials <- .layout_trials()
  fixed <- unrecorded(graph, start, "barycenter", 8L, ranks = "fixed", trials = trials)
  again <- unrecorded(graph, start, "barycenter", 8L, variant = "layering", trials = trials)
  # Fans move on this start after its fixed-rank drawing exists, so that
  # drawing cannot stand in for the automatic one.
  entry <- trials$entries[[1L]]
  expect_false(identical(entry$drawings$automatic, entry$drawings$plain))
  expect_false(identical(fixed, fresh))
  expect_identical(again, fresh)
  expect_identical(counter$finished, 3L)
})

test_that("layouts are unchanged by the shared trial drawings", {
  skip_on_cran()
  # The second graph has starts whose fans move after a fixed-rank trial.
  graphs <- list(.test_trial_graph(),
    metrodag_examples("random", n = 11, probability = 2 / 11, seed = 3))
  shared <- lapply(graphs, layout_metro_dag)
  # Without sharing, every rebuild draws its start from scratch.
  testthat::local_mocked_bindings(.layout_trial = function(trials, start) {
    entry <- new.env(parent = emptyenv())
    entry$start <- start
    entry$drawings <- list()
    entry
  }, .package = "metrodag")
  expect_identical(lapply(graphs, layout_metro_dag), shared)
})
