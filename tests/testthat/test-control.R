test_that("internal layout controls default to the published pipeline", {
  withr_options <- function(value, code) {
    old <- options(metrodag.control = value)
    on.exit(options(old), add = TRUE)
    force(code)
  }
  control <- .metro_control()
  expect_true(all(control$stages))
  expect_setequal(names(control$stages), .metro_layout_stages)
  graph <- metrodag_examples("causal")
  reference <- layout_metro_dag(graph)
  same <- withr_options(list(stages = c(restart_variants = TRUE)), layout_metro_dag(graph))
  expect_identical(same$edges, reference$edges)
  # Every stage can be disabled independently and still yields a valid layout.
  for (stage in .metro_layout_stages) {
    switched <- stats::setNames(FALSE, stage)
    layout <- withr_options(list(stages = switched), layout_metro_dag(graph))
    metrics <- layout_metrics(layout)
    expect_equal(metrics$edges, igraph::ecount(graph))
    expect_equal(metrics$node_edge_collisions, 0L)
  }
  expect_error(withr_options(list(stages = c(no_such_stage = FALSE)), layout_metro_dag(graph)),
    "Unknown metrodag.control stages")
  expect_error(withr_options(list(budgets = c(nope = 1)), layout_metro_dag(graph)),
    "Unknown metrodag.control budgets")
  expect_error(withr_options(list(speed = 1), layout_metro_dag(graph)), "metrodag.control")
})

test_that("layouts record which refinement stages ran or were skipped by budget", {
  old <- options(metrodag.control = NULL)
  on.exit(options(old), add = TRUE)
  graph <- metrodag_examples("causal")
  layout <- layout_metro_dag(graph)
  stages <- layout$parameters$stages
  expect_identical(names(stages), .metro_layout_stages)
  expect_true(all(stages == "ran"))
  expect_identical(capture.output(print(layout)),
    sprintf("<metrodag_layout> %d nodes, %d edges, direction: right",
      igraph::vcount(graph), igraph::ecount(graph)))
  # A budget too small for one attempt is reported, not silently skipped.
  options(metrodag.control = list(budgets = c(rank_refinement = 1, restart_variants = 1),
    stages = c(arrangement = FALSE)))
  tight <- layout_metro_dag(graph)
  expect_identical(unname(tight$parameters$stages[c("rank_refinement", "restart_variants",
    "arrangement", "straighten")]),
    c("skipped (budget)", "skipped (budget)", "disabled", "ran"))
  expect_match(capture.output(print(tight))[2L],
    "skipped by work budget: rank_refinement, restart_variants", fixed = TRUE)
  # Explicit ranks skip rank reassignment and the other layerings, but keep
  # the refinements that leave every node in its rank; alignment groups skip
  # those as well.
  options(metrodag.control = NULL)
  rank <- tight$nodes$layer[match(igraph::V(graph)$name, tight$nodes$node)]
  ranked <- layout_metro_dag(graph, rank = rank)
  automatic_only <- c("rank_refinement", "layering_variants")
  rank_preserving <- c("fan_straighten", "restart_variants", "source_relaxation",
    "intermediate_relaxation")
  expect_true(all(ranked$parameters$stages[automatic_only] == "not applicable"))
  expect_false(any(ranked$parameters$stages[rank_preserving] == "not applicable"))
  aligned <- layout_metro_dag(graph, rank = rank, align = list(c("X", "M")))
  expect_true(all(aligned$parameters$stages[c(automatic_only, rank_preserving)] ==
    "not applicable"))
  # A budgeted stage that no component has work for is not applicable, as is
  # every budgeted stage on a graph without arrows.
  lone <- layout_metro_dag(igraph::make_empty_graph(1) |> igraph::set_vertex_attr("name", value = "A"))
  budgeted <- names(.metro_control_defaults()$budgets)
  expect_true(all(lone$parameters$stages[budgeted] == "not applicable"))
  straight <- layout_metro_dag(metrodag_examples("chain"))
  expect_identical(unname(straight$parameters$stages["half_grid"]), "not applicable")
  # That holds whatever the budget, and a budget too small for a drawing that
  # has bends is a skip.
  options(metrodag.control = list(budgets = c(half_grid = 1)))
  expect_identical(unname(layout_metro_dag(metrodag_examples("chain"))$parameters$stages["half_grid"]),
    "not applicable")
  options(metrodag.control = list(budgets = c(half_grid = 0)))
  expect_identical(unname(layout_metro_dag(graph)$parameters$stages["half_grid"]), "skipped (budget)")
  options(metrodag.control = NULL)
  # The record does not leak into a later layout or survive an error.
  local({
    testthat::local_mocked_bindings(.refine_route_resolution = function(...) stop("interrupted"),
      .package = "metrodag")
    expect_error(layout_metro_dag(graph), "interrupted")
  })
  expect_null(.metro_stage_record$depth)
  expect_identical(layout_metro_dag(graph)$parameters$stages, stages)
})

test_that("only components with work for a budgeted stage decide whether it was skipped", {
  old <- options(metrodag.control = list(budgets = c(arrangement = 1, rank_refinement = 1)))
  on.exit(options(old), add = TRUE)
  # An isolated node has no arrows to refine, so the stages stay skipped
  # rather than turning "partly skipped" in a packed drawing.
  isolated <- igraph::add_vertices(metrodag_examples("causal"), 1L, name = "Z")
  layout <- layout_metro_dag(isolated)
  expect_true(layout$parameters$component_packing)
  expect_identical(unname(layout$parameters$stages[c("arrangement", "rank_refinement")]),
    rep("skipped (budget)", 2L))
  expect_match(capture.output(print(layout))[2L],
    "skipped by work budget: arrangement, rank_refinement$")
  # A short chain fits a budget the larger component exceeds: partly skipped.
  options(metrodag.control = list(budgets = c(arrangement = 100, rank_refinement = 100)))
  both <- igraph::disjoint_union(
    igraph::graph_from_edgelist(rbind(c("P", "Q"), c("Q", "R"))), metrodag_examples("causal"))
  stages <- layout_metro_dag(both)$parameters$stages
  expect_identical(unname(stages[c("arrangement", "rank_refinement")]),
    rep("partly skipped (budget)", 2L))
})
