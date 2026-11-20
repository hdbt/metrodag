# Isolate refinement stages by replacing them with identity stubs, so a test
# can compare a layout before and after the stage it is about. Each stub
# returns its geometry unchanged, with the stage function's own signature.
.test_stage_stubs <- list(
  .refine_route_resolution = function(nodes, edges, ...) edges,
  .simplify_route_sections = function(routes, nodes) routes,
  .compact_route_gaps = function(nodes, routes) list(nodes = nodes, routes = routes),
  .lengthen_route_approaches = function(routes, nodes) routes,
  .compact_node_arrangement = function(nodes, routes) list(nodes = nodes, routes = routes),
  .refine_rank_assignment = function(graph, geometry, ...) geometry,
  .refine_layout_variants = function(graph, layer, geometry, ...) geometry,
  .refine_layering_variants = function(graph, layer, geometry, ...) geometry,
  .relax_source_positions = function(nodes, routes, ...) list(nodes = nodes, routes = routes),
  .relax_intermediate_positions = function(nodes, routes, ...) list(nodes = nodes, routes = routes)
)

# Stub the named stages until the calling test (or function) returns.
.test_without_stages <- function(stages, env = parent.frame()) {
  unknown <- setdiff(stages, names(.test_stage_stubs))
  if (length(unknown)) stop("No stub for ", paste(unknown, collapse = ", "))
  do.call(testthat::local_mocked_bindings, c(.test_stage_stubs[stages],
    list(.package = "metrodag", .env = env)))
}

# Lay out `graph` (facing down by default) with the named stages stubbed.
.test_layout_without <- function(graph, stages, ..., direction = "down") {
  .test_without_stages(stages)
  layout_metro_dag(graph, direction = direction, ...)
}

# The stages that run after placement and rank refinement, stubbed together by
# the tests of the earlier refinements.
.test_late_stages <- c(".refine_route_resolution", ".refine_layout_variants",
  ".relax_source_positions", ".relax_intermediate_positions")
