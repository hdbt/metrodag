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
#' @seealso [metro_dag()], [layout_metro_dag()], [plot_metro_dag()],
#'   [save_metro_dag()], [metro_style()]
#' @importFrom utils head tail
"_PACKAGE"

utils::globalVariables(c("x", "y", "xend", "yend", "role", "edge_type"))
