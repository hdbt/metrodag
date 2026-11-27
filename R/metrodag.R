#' Compute a monotone schematic DAG layout
#'
#' Long edges are expanded into private waypoints before crossing reduction.
#' Ordered ranks are compacted and each rank gap is sized independently.
#' Routes use vertical and 45-degree segments, avoid unrelated nodes, and
#' penalize shared tracks. A final whole-edge pass removes unnecessary bends
#' without increasing crossings or shared track length. Geometry is computed
#' downward, then rotated.
#' A soft symmetry refinement centers junctions with at least three branches
#' and their middle through-routes between the outer branches. It preserves
#' rank order and station spacing, and accepts a move only when crossings,
#' shared track length, bends and total edge length do not increase and all
#' routes remain clear of unrelated stations. Exact symmetry is not guaranteed
#' when these constraints prevent balancing the fan.
#' A subsequent bounded joint refinement reroutes crossing arrow pairs without
#' moving nodes. Each accepted move strictly reduces crossings, adds at most
#' one bend, and does not increase pairwise shared-track length or total edge
#' length. Candidate routes must remain clear of unrelated stations. The search
#' considers at most 24 crossing-pair attempts (48 when `align` groups are
#' given) and 24 shortlisted candidates per arrow per component;
#' it does not guarantee a crossing-free drawing.
#' A placement pass then tries to straighten the
#' two outer arrows of balanced incoming and outgoing fans. It may align their
#' endpoints and compact the intervening gap. At most 24 placement candidates
#' are tried per component. Each accepted move reduces bends without increasing
#' crossings, shared-track length or total edge length, preserves already straight
#' arrows and station spacing, and keeps routes clear of unrelated stations.
#' Automatic layers and their gaps reflect the refined positions. With explicit
#' user ranks a move must keep every node in its rank, so only complete ranks
#' move; alignment groups skip this placement pass.
#' A route cleanup replaces sections between existing bends and nearby
#' grid points. This can simplify arrows that still need more than two bends.
#' It keeps nodes fixed and strictly reduces bends without increasing crossings,
#' pairwise shared-track length or total line length, while retaining station
#' clearance (at least four grid units or the previous distance if smaller, with
#' a floor of 0.8). Per component, at most 24 route attempts are made over two passes,
#' with 24 sections and 128 shortlisted candidates per attempt. These search
#' limits do not guarantee a global minimum bend count.
#' A subsequent compaction removes straight steps between complete ranks,
#' preserving rank order and ties (including supplied ranks), station spacing,
#' and existing clearance up to four grid units with a floor of 0.8. Final
#' approaches shorter than three grid steps may be lengthened by moving an
#' earlier bend upstream. Each refinement checks at most 24 candidates per
#' component and accepts no increase in crossings, pairwise shared-track length,
#' bends or total line length. They are bounded preferences; some gaps and
#' short approaches must remain. Plot labels are placed separately at draw time.
#' A node arrangement refinement tries up to eight feasible placements per
#' component, each with two routing orders. A route-step work budget reduces
#' attempts for larger components and can skip this refinement entirely.
#' Connected straight arrows constrain
#' joint node offsets. Rank membership, ties, within-rank order, four-unit station
#' spacing and already straight arrows are preserved. Accept only a smaller
#' transverse extent without increasing causal extent, crossings, pairwise shared
#' track length, bends or total line length. Existing whitespace may shrink;
#' routes retain at least 0.8 grid units from unrelated stations. Successful moves
#' are followed by the same bounded gap and final-approach refinements. This
#' geometric search does not guarantee label clearance at every export size.
#' Finally, automatic ranks may be reassigned to bring a node closer to its
#' neighbours. Candidate moves reduce total incident rank span or reposition
#' a one-parent, one-child intermediate with equal span, keeping every edge
#' strictly forward. Prefer larger per-edge rank savings and lower degrees.
#' Try at most three complete component rebuilds,
#' further limited to floor(12000 / (route steps * edges)); larger components
#' can skip this search. Trial ranks are fixed during rebuilding to prevent
#' recursive search. Continue from accepted moves within the same rebuild budget.
#' Accept only smaller routed area with no increase in crossings or total bends.
#' A strict crossing reduction can increase either extent by up to 10% and total
#' line length by up to 5% relative to this refinement's input. Otherwise neither
#' extent nor line length may increase. Pairwise shared-track length may increase
#' only for a crossing reduction with no more shared unit steps (a diagonal
#' unit is longer than a vertical unit). All other moves retain the length limit.
#' Four-unit station spacing and 0.8-unit route clearance are required. Individual
#' routes, within-rank orders and metro paths can change. Explicit user ranks
#' skip this reassignment; their order and ties remain fixed. This bounded
#' heuristic does not guarantee minimum area or label clearance.
#' Small layouts additionally try two deterministic starting points:
#' child-directed coordinate assignment and a reversed initial rank order. Each
#' runs the same routing/refinement pipeline, including rank reassignment for
#' automatic ranks, with no recursive starting-point search. Explicit ranks stay
#' fixed in every start. Try at most
#' `floor(4000 / (route steps * edges))` alternatives, capped at two. Alignment
#' groups, `ordering = "none"` and zero sweeps skip this search.
#' Crossings and pairwise shared-track length cannot increase. A strict crossing
#' reduction may spend one extra bend per removed crossing (at most two total),
#' 5% more line length and 10% of either extent relative to the input drawing.
#' At equal crossings, neither extent nor line length can grow; up to two extra
#' bends require at least 15% less area and a smaller combined score
#' `bends + length / 20 + sqrt(area) / 4`. Smaller shared-track length takes
#' priority within those guards. Four-unit station spacing and 0.8-unit route
#' clearance remain required. This is a bounded preference, not a global optimum.
#' When a route leaves its endpoints' transverse range by more than two units
#' (a V detour) and the work budget allows a third run, one further start
#' measures initial straightening lanes from the endpoints. It must reduce that
#' overshoot without extra crossings, shared track, line length or either
#' extent, and may spend one extra bend only for at least 15% less area.
#' A final placement pass relaxes source nodes towards their children.
#' It tries median-directed transverse positions and, with automatic ranks,
#' later causal positions, rerouting incident arrows and arrows obstructed by a
#' moved station together. With explicit ranks every source keeps its rank.
#' At most 64 candidates over four passes are tried, further limited to
#' `floor(120000 / (route steps * edges))`. Alignment groups,
#' `ordering = "none"` and zero sweeps skip this pass. Crossings, shared-track
#' length, bends, line length and either extent cannot increase. Already straight
#' arrows, four-unit station spacing and 0.8-unit route clearance are retained.
#' Transverse direction reversals cannot increase, including during subsequent
#' gap/arrangement compaction: a smaller box cannot buy a V-shaped approach.
#' Automatic source ranks and within-rank orders may change; identities and
#' roles do not.
#' A subsequent placement pass moves low-degree intermediate nodes
#' between their parents and children, within their rank when ranks are
#' explicit. It protects collinear straight through-
#' chains (or the straight arrow with greatest causal span when none exists),
#' but can exchange a bend between two side arrows. Try at most 24 placements,
#' further limited to
#' `floor(60000 / (route steps * edges))`, and accept one best move. Alignment
#' groups, disabled ordering and zero sweeps skip this pass. Compare
#' the final half-grid routes: crossings, shared-track length, line length,
#' either extent and every arrow's transverse reversals cannot increase.
#' Require smaller area; one extra bend needs at least 15% less area and two
#' need at least 25%, with a lower combined area/length/bend score. Preserve
#' four-unit station spacing and 0.8-unit unrelated-station clearance. Automatic
#' ranks and within-rank orders may change; no identity, label, role or causal
#' edge is changed.
#' The same bounds apply before half-grid refinement, which can be skipped
#' when a packed drawing exceeds its work budget.
#' Automatic ranks then also start from up to two other automatic layerings: the
#' minimum-span layering, which moves each node to the end of its legal rank
#' interval on the side with more arrows until the total rank span of all
#' arrows stops decreasing, and the other of the source- and sink-aligned
#' layerings. A layering equal to one already tried is skipped. Try at most
#' `floor(8000 / (route steps * edges))` of these. Each is screened before rank
#' reassignment under the starting-point rule above; the best then receives rank
#' reassignment and both placement passes, and is kept only when the same rule
#' prefers it to the relaxed drawing from `layering`; for a crossing reduction,
#' either extent may still grow by 10% over the drawing the search started
#' from. Explicit ranks/alignment, `ordering = "none"` and zero sweeps skip
#' this search.
#' After all placement, a final routing pass searches the same route grammars
#' at half-grid resolution. It keeps nodes, ranks, alignment and already straight
#' arrows fixed. It accepts a strict geometry improvement with no increase in
#' crossings, pairwise shared-track length, total bends, line length, routed
#' extents or any arrow's transverse reversals, and retains 0.8-unit station
#' clearance. It skips drawings where `2 * route steps * arrows > 400000`.
#' Routing one arrow at a time can let a bending arrow take the straight track
#' between two stations on one vertical or diagonal line, so that the arrow
#' joining them detours on a parallel lane. This pass also offers such arrows
#' their straight track and reroutes, crossings first, the bending arrows that
#' then share track with them. That result is kept when it meets the same
#' guards and has no more crossings or bends than the plain search. This
#' attempt roughly doubles the pass, so it is made only where
#' `2 * route steps * arrows <= 250000`.
#' Unchanged arrows keep their original rows; changed routes can have half-unit
#' steps and bend positions. This final pass runs before direction rotation;
#' earlier placement/routing searches continue to use integer unit steps.
#' Disconnected components are laid out separately and packed by their routed
#' extents, top-aligned at each shelf's first rank. Shelves widen to keep extra
#' components beside the deepest one when that packs no larger. Supplying explicit ranks keeps components aligned on common ranks.
#' Refinement ties use node names rather than input edge order. The default
#' ordering retains geometry and metro path colours across reordered equivalent
#' inputs; `ordering = "none"` intentionally uses the supplied vertex order.
#'
#' @param graph A directed acyclic `igraph` (including `tidygraph::tbl_graph`),
#'   `dagitty` or `ggdag::tidy_dagitty` object, a two-column edge matrix or a
#'   `from`/`to` data frame, including graphs made by
#'   [metro_graph()] or [metro_graph_from_adjacency()]. As in a data frame, an
#'   edge matrix with columns named `from` and `to` is read by those names, in
#'   either order, and other columns are ignored; otherwise the matrix needs
#'   exactly two columns and the first holds the source of each arrow (a
#'   matrix naming only its first column `to`, or only its second `from`, is
#'   rejected).
#'   Self-loops, repeated arrows and cycles are rejected with a message that
#'   names them. DAGitty input requires
#'   the optional dagitty package; ggdag tidy input also requires ggdag. Both
#'   must contain only directed edges with supported analysis annotations.
#'   Vertex names are normalized to character identifiers, including numeric
#'   igraph names. The supplied graph is not modified.
#' @param direction One of `"right"`, `"down"`, `"up"`, or `"left"`. Defaults
#'   to `"right"`, matching [metro_dag()].
#' @param rank Optional numeric vector of user-supplied ranks, named by node or
#'   supplied in vertex order. A named vector must give every node a rank and
#'   name no other nodes. Values must be finite and strictly increase along
#'   every edge. Only their ordering and ties determine layers; numeric gaps do
#'   not determine physical spacing. Every node stays in its supplied rank:
#'   nodes with equal ranks share a layer and higher ranks are drawn further
#'   downstream. The layout still chooses the gaps between layers and the
#'   order and transverse positions of nodes within each layer, so rank
#'   reassignment is skipped while fan straightening, the restart variants and
#'   the source and intermediate placement passes run with moves that keep
#'   every rank. With `align`, these placement refinements are skipped as well.
#' @param layering `"longest"` for initial source-aligned topological ranks or
#'   `"sinks"` for initial sink-aligned ranks. Automatic ranks may then be refined
#'   to straighten balanced fans and reduce routed area while preserving every
#'   edge's causal direction, and other automatic layerings are tried as
#'   starting points; one is kept only when its drawing is better.
#'   `"shortest"` is a deprecated alias for `"sinks"`;
#'   shortest-path ranks do not in general define a valid DAG layering.
#'   The choice is validated even when explicit `rank` values determine layers.
#' @param ordering `"barycenter"`, `"median"`, or `"none"`.
#' @param sweeps Number of crossing-reduction sweeps; a non-negative integer.
#' @param align Optional list of disjoint character vectors naming nodes to
#'   align across ranks, for example `list(c("X1", "X2", "X3"))`. Each group
#'   shares a row in a right/left drawing or a column in a down/up drawing.
#'   Members must occupy different ranks. Their ranks stay fixed during layout;
#'   supply `rank` for survey waves. Straight arrows between aligned members
#'   are retained when unrelated stations do not block them. Alignment prioritizes
#'   consistent rows/columns and may increase area, crossings or bends.
#'   Alignment does not change the graph or imply a causal relationship.
#' @param bidirected How to import DAGitty bidirected arcs (`A <-> B`):
#'   `"error"` (the default) rejects them; `"latent"` draws each as an
#'   unlabelled unobserved common cause with dashed arrows into both
#'   variables, which encodes the same assumption for adjustment checks.
#' @return A `metrodag_layout` list containing the nodes (`nodes`), routed
#'   edge segments (`edges`, unit or half-unit octilinear steps), the graph
#'   topology (`graph`), the `direction` and the layout `parameters`. Nodes and
#'   topology are those of the input graph, plus the unlabelled common cause
#'   drawn for each bidirected arc when `bidirected = "latent"`. The layout is
#'   deterministic: ties are broken by node name, not by input order.
#'   `parameters$stages` records, for each refinement stage, whether it
#'   `"ran"`, was `"skipped (budget)"` because its work budget allowed no
#'   attempt (`"partly skipped (budget)"` when that held for only some of the
#'   components with arrows for it to refine), was `"not applicable"` (for
#'   example rank refinement with explicit ranks, or a budgeted stage with
#'   nothing to refine, such as the half-grid pass when every arrow is
#'   straight) or was `"disabled"`.
#'   Printing the layout lists the stages its work budgets skipped.
#' @examples
#' layout <- layout_metro_dag(metrodag_examples("long_edge"))
#' layout
#' layout_metrics(layout)
#' plot_metro_dag(layout, exposure = "A", outcome = "E")
#' @export
layout_metro_dag <- function(graph, direction = "right", rank = NULL,
  layering = "longest", ordering = "barycenter", sweeps = 8L, align = NULL,
  bidirected = "error") {
  direction <- .assert_scalar_choice(direction, c("down", "right", "up", "left"), "direction")
  .assert_number(sweeps, "sweeps", minimum = 0, integer = TRUE)
  graph <- .as_metro_input(graph, bidirected, layout = FALSE)
  layer <- .assign_layers(graph, layering, rank)
  .metro_record_stages()
  on.exit(.metro_clear_record(), add = TRUE)
  align <- .alignment_groups(align, igraph::V(graph)$name, layer)
  packed <- is.null(rank) && !length(align) && igraph::components(graph, mode = "weak")$no > 1L
  geom <- if (packed) .layout_components(graph, layer, ordering, sweeps) else
    .layout_component(graph, layer, ordering, sweeps, ranks = if (length(align))
      "fixed" else if (!is.null(rank)) "supplied" else "automatic", align = align)
  nodes <- data.frame(node = igraph::V(graph)$name, x = geom$x, y = geom$y,
    layer = as.integer(geom$layer), order = geom$ord, path = geom$paths,
    label_x = geom$x, label_y = geom$y, stringsAsFactors = FALSE)
  # Run only after every placement/rebuild: earlier searches use integer steps.
  edges <- if (.metro_stage("half_grid")) .refine_route_resolution(nodes, geom$edges,
    max_work = .metro_budget("half_grid")) else geom$edges
  rotated <- .rotate_geometry(nodes, edges, direction)
  parameters <- list(layering = layering, ordering = ordering,
    sweeps = sweeps, layer_gap = geom$gaps, component_packing = packed, align = align,
    stages = .metro_recorded_stages())
  # Named rank_labels refer to ranks by the values supplied here, kept named
  # by node so that they still match if the graph's vertices are reordered.
  if (!is.null(rank)) parameters$rank <- .metro_named_rank(rank,
    igraph::V(graph)$name, .metro_hidden_nodes(graph))
  structure(list(nodes = rotated$nodes, edges = rotated$edges, graph = graph,
    direction = direction, parameters = parameters), class = "metrodag_layout")
}

#' @export
print.metrodag_layout <- function(x, ...) {
  cat(sprintf("<metrodag_layout> %d nodes, %d edges, direction: %s\n",
    nrow(x$nodes), igraph::ecount(x$graph), x$direction))
  stages <- x$parameters$stages
  skipped <- names(stages)[grepl("budget", stages)]
  if (length(skipped)) cat(sprintf("Refinements skipped by work budget: %s\n",
    paste0(skipped, ifelse(grepl("^partly", stages[skipped]), " (some components)", ""),
      collapse = ", ")))
  invisible(x)
}
