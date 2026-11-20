.require_dagitty <- function() {
  if (!requireNamespace("dagitty", quietly = TRUE))
    stop("Install the optional 'dagitty' package to import DAGitty graphs or check adjustment sets.", call. = FALSE)
}

# DAGitty has no public R accessor for selection status. Its canonical DOT
# serialization lists node attributes as `id [attr,...]`, with quoted ids for
# names containing spaces. Quoted attribute values are removed before matching
# so display text cannot be mistaken for the bare `selected` flag.
.dagitty_selected <- function(graph) {
  lines <- strsplit(as.character(dagitty::dagitty(as.character(graph))), "\n", fixed = TRUE)[[1L]]
  pattern <- '^\\s*("(?:[^"\\\\]|\\\\.)*"|[^\\s\\[;{}]+)\\s*\\[(.*)\\]\\s*;?\\s*$'
  hits <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  selected <- character()
  for (hit in hits) if (length(hit) == 3L) {
    attrs <- gsub('"([^"\\\\]|\\\\.)*"', '""', hit[3L], perl = TRUE)
    if (!grepl("(^|,)\\s*selected\\s*(,|$)", attrs, perl = TRUE)) next
    id <- hit[2L]
    if (startsWith(id, '"')) id <- gsub('\\\\(.)', "\\1", substring(id, 2L, nchar(id) - 1L), perl = TRUE)
    selected <- c(selected, id)
  }
  selected
}

.from_dagitty <- function(graph, bidirected = "error") {
  .require_dagitty()
  bidirected <- .assert_scalar_choice(bidirected, c("error", "latent"), "bidirected")
  edges <- dagitty::edges(graph)
  if (dagitty::graphType(graph) != "dag" || (nrow(edges) && any(!edges$e %in% c("->", "<->"))))
    stop("MetroDAG imports DAGitty DAGs with directed edges only; undirected and partially directed edges are not supported.", call. = FALSE)
  # names() of an empty DAGitty graph is an empty list, and edges() has no columns.
  nm <- as.character(unlist(names(graph), use.names = FALSE))
  if (!length(nm)) stop("`graph` must contain at least one node.", call. = FALSE)
  pairs <- edges$e == "<->"
  if (any(pairs) && bidirected == "error")
    stop("This DAGitty graph contains bidirected arcs. Use `bidirected = \"latent\"` to draw each arc as an unobserved common cause, or represent those causes explicitly.", call. = FALSE)
  directed <- edges[!pairs, , drop = FALSE]
  edge_data <- if (nrow(directed)) data.frame(from = directed$v, to = directed$w) else
    data.frame(from = character(), to = character())
  # Each bidirected arc A <-> B becomes an unlabelled unobserved node with
  # arrows into A and B: the same assumption for adjustment and drawing.
  hidden <- character()
  if (any(pairs)) {
    ends <- edges[pairs, , drop = FALSE]
    hidden <- make.unique(c(nm, paste0(".latent_", ends$v, "_", ends$w)), sep = "_")[-seq_along(nm)]
    edge_data <- rbind(edge_data, data.frame(from = c(hidden, hidden), to = c(ends$v, ends$w)))
  }
  g <- igraph::graph_from_data_frame(edge_data, directed = TRUE,
    vertices = data.frame(name = c(nm, hidden)))
  roles <- list(exposure = dagitty::exposures(graph), outcome = dagitty::outcomes(graph),
    adjusted = dagitty::adjustedNodes(graph), unobserved = c(dagitty::latents(graph), hidden),
    selected = .dagitty_selected(graph))
  if (anyDuplicated(unlist(roles, use.names = FALSE)))
    stop("DAGitty node roles must be disjoint for MetroDAG's visual annotations.", call. = FALSE)
  role <- rep("observed", igraph::vcount(g))
  names_all <- igraph::V(g)$name
  for (key in names(roles)) role[match(roles[[key]], names_all)] <- key
  g <- igraph::set_vertex_attr(g, "role", value = role)
  g <- igraph::set_vertex_attr(g, ".metrodag_hidden", value = names_all %in% hidden)
  .metro_import_labels(g, attr(graph, "labels"))
}

#' Check a total-effect adjustment set with DAGitty
#'
#' Runs DAGitty's adjustment criterion on the assumed graph, separately from
#' plotting. This checks graphical identification by covariate adjustment for
#' a total effect; it does not validate the graph, positivity, measurement, or
#' the statistical estimator. It does not check direct or indirect effects.
#' Plotting never calls this function implicitly.
#' Graphs with selected (conditioned-on) nodes are rejected: this check
#' covers covariate adjustment only, and selection must not be ignored.
#'
#' @param graph Any input accepted by [metro_dag()], or a MetroDAG ggplot.
#' @param exposure,outcome,adjusted,unobserved Character vectors of node names.
#'   NULL inherits roles. Supply `adjusted = character(0)` or `unadjusted = TRUE`
#'   to explicitly check an empty set. An inherited explicit empty set is kept.
#' @param unadjusted Whether to explicitly check no adjustment covariates.
#' @param bidirected For DAGitty input with bidirected arcs: `"error"` (the
#'   default) or `"latent"` to treat each arc as an unobserved common cause,
#'   as in [metro_dag()].
#' @return A `metrodag_adjustment_check` list with `valid`, `effect`, `exposure`,
#'   `outcome`, and `adjusted`. `valid` is DAGitty's logical criterion result.
#' @examples
#' if (requireNamespace("dagitty", quietly = TRUE)) {
#'   g <- dagitty::dagitty("dag { C -> X; C -> Y; X -> Y }")
#'   check_metro_adjustment(g, exposure = "X", outcome = "Y", adjusted = "C")
#' }
#' @export
check_metro_adjustment <- function(graph, exposure = NULL, outcome = NULL,
  adjusted = NULL, unobserved = NULL, unadjusted = FALSE, bidirected = "error") {
  .require_dagitty()
  if (inherits(graph, "ggplot") && !is.null(attr(graph, "metrodag_layout")))
    graph <- attr(graph, "metrodag_layout")
  # Reuse validation/annotation rules without computing or drawing a layout.
  layout <- if (inherits(graph, "metrodag_layout")) graph else
    list(graph = .as_metro_input(graph, bidirected, layout = TRUE, plot = TRUE))
  layout <- .annotate_metro_layout(layout, exposure, outcome, adjusted, unobserved, unadjusted, NULL)
  analysis <- layout$analysis
  if (length(analysis$selected))
    stop("The total-effect check does not model selection on ", paste(analysis$selected, collapse = ", "),
      ". Check selection assumptions with DAGitty directly; MetroDAG will not drop them.", call. = FALSE)
  if (!length(analysis$exposure) || !length(analysis$outcome))
    stop("Specify at least one exposure and outcome for the total-effect check.", call. = FALSE)
  if (!analysis$adjustment_specified)
    stop("Specify `adjusted`, or use `unadjusted = TRUE` to check an empty set.", call. = FALSE)
  # Private identifiers avoid quoting/parser ambiguities in arbitrary labels.
  translated <- .metro_dagitty_graph(layout$graph, analysis$unobserved)
  ids <- translated$ids; dg <- translated$dag
  valid <- dagitty::isAdjustmentSet(dg, Z = unname(ids[analysis$adjusted]),
    exposure = unname(ids[analysis$exposure]), outcome = unname(ids[analysis$outcome]))
  structure(list(valid = isTRUE(valid), effect = "total", exposure = analysis$exposure,
    outcome = analysis$outcome, adjusted = analysis$adjusted), class = "metrodag_adjustment_check")
}

#' @export
print.metrodag_adjustment_check <- function(x, ...) {
  cat(sprintf("<metrodag_adjustment_check> total effect: %s -> %s\n",
    paste(x$exposure, collapse = ", "), paste(x$outcome, collapse = ", ")))
  cat("Adjustment set: {", paste(x$adjusted, collapse = ", "), "}\n", sep = "")
  cat("Meets DAGitty's adjustment criterion: ", x$valid, "\n", sep = "")
  invisible(x)
}
