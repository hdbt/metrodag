#' metrodag: publication-oriented causal DAG figures
#'
#' metrodag draws directed acyclic graphs (DAGs) for causal inference and
#' epidemiology in a schematic "metro map" style. A deterministic layered
#' layout places variables and routes arrows along vertical and diagonal
#' tracks; labels, arrowheads and clearances are then measured on the actual
#' output device so that exported figures remain legible at manuscript sizes.
#'
#' Start with [metro_dag()] to draw a graph in one call, reuse its layout to
#' compare analysis views with [compare_metro_dags()], check the figure at its
#' intended size with [diagnose_metro_dag()], and export it with
#' [save_metro_dag()]. [metro_style()] and the `edge_style` argument control
#' appearance. The vignettes `vignette("metrodag")`,
#' `vignette("analysis-views")`, `vignette("publication")` and
#' `vignette("algorithm")` cover the workflow.
#'
#' @section Workflow:
#' 1. Supply a graph: an igraph or tidygraph object, a dagitty or ggdag DAG, a
#'    two-column edge matrix, a data frame with `from` and `to` columns, or
#'    tables built with [metro_graph()] or [metro_graph_from_adjacency()].
#'    [metrodag_examples()] returns small teaching graphs to try.
#' 2. Draw it with [metro_dag()], giving roles (`exposure`, `outcome`,
#'    `adjusted`, `unobserved`, `selected`) and display `labels`. For more
#'    control, compute the geometry with [layout_metro_dag()] (`rank`,
#'    `align`, `direction`) and draw it with [plot_metro_dag()].
#' 3. Show other analyses of the same graph on the same geometry: pass
#'    `attr(p, "metrodag_layout")` back to [metro_dag()], or use
#'    [compare_metro_dags()] for aligned panels.
#' 4. Optionally check an adjustment set with [check_metro_adjustment()] and
#'    emphasise open paths with [highlight_paths()] (both need dagitty).
#' 5. Check the figure with [diagnose_metro_dag()] at its final width and
#'    device, then export it with [save_metro_dag()], or compose it with other
#'    plots through [as_metro_grob()]. [layout_metrics()] and
#'    [count_crossings()] quantify the drawing.
#'
#' @section How metrodag differs from other DAG tools:
#' * Layout and drawing are separate steps, and the layout is deterministic:
#'   the same graph gives the same drawing on every platform.
#' * Roles are annotations supplied by the analyst. Nothing is inferred from
#'   the graph, and annotations never move nodes or arrows.
#' * `estimand` is a descriptive label, not an identification check; plotting
#'   never runs [check_metro_adjustment()] implicitly.
#' * metrodag is not built on ggdag. dagitty and ggdag objects are accepted as
#'   input, but metrodag draws its own layers: restyle with [metro_style()] and
#'   `edge_style` rather than ggdag geoms. Ordinary ggplot2 additions such as
#'   [ggplot2::labs()] still work.
#' * Text is measured on the output device. [ggplot2::ggsave()] works, but
#'   [save_metro_dag()] adds journal column widths (`"single"`, `"1.5"`,
#'   `"double"`) and automatic height, and refuses to write a figure that
#'   cannot fit every arrow.
#'
#' @seealso [metro_dag()], [layout_metro_dag()], [plot_metro_dag()],
#'   [save_metro_dag()], [metro_style()]
#' @examples
#' edges <- data.frame(
#'   from = c("C", "C", "X", "M"),
#'   to   = c("X", "Y", "M", "Y")
#' )
#' p <- metro_dag(edges, exposure = "X", outcome = "Y", adjusted = "C")
#'
#' # Reuse the same geometry for another analysis view
#' fixed <- attr(p, "metrodag_layout")
#' crude <- metro_dag(fixed, exposure = "X", outcome = "Y", unadjusted = TRUE)
#'
#' out <- tempfile(fileext = ".pdf")
#' save_metro_dag(out, p, width = "single")
#' unlink(out)
#' @importFrom utils head tail
"_PACKAGE"

utils::globalVariables(c("x", "y", "xend", "yend", "role", "edge_type"))
