#' Construct a DAG from node and edge tables
#'
#' An explicit node table retains isolated variables and their metadata. Node
#' identifiers are matched by name, never by row number. Other columns become
#' igraph attributes: `label` supplies display text and `role` may explicitly
#' identify `observed`, `exposure`, `outcome`, `adjusted`, `unobserved` or
#' `selected` nodes.
#' Missing roles remain unspecified and display as observed unless overridden.
#' No role or adjustment recommendation is inferred from identifiers or labels.
#'
#' @param nodes A data frame with a unique, non-missing, non-empty `name` column.
#' @param edges A data frame with `from` and `to` columns naming existing nodes.
#'   Other columns are retained as edge attributes. NULL creates isolated nodes.
#' @return A validated directed acyclic igraph, accepted by every MetroDAG graph
#'   entry point. Input tables are not modified. Duplicate edges and cycles are
#'   rejected rather than simplified.
#' @examples
#' nodes <- data.frame(name = c("X", "Y", "I"),
#'   label = c("Exposure", "Outcome", "Isolated variable"),
#'   role = c("exposure", "outcome", "observed"))
#' g <- metro_graph(nodes, data.frame(from = "X", to = "Y"))
#' metro_dag(g)
#' @export
metro_graph <- function(nodes, edges = NULL) .metro_graph(nodes, edges)

# `argument` names the input that defines the arrows in structural messages.
.metro_graph <- function(nodes, edges = NULL, argument = "edges") {
  .metro_input_table(nodes, "name", "nodes")
  nodes <- as.data.frame(nodes)
  nodes$name <- .metro_input_ids(nodes$name, "nodes$name")
  if (!nrow(nodes) || anyDuplicated(nodes$name))
    stop("`nodes$name` must contain at least one node and must be unique.", call. = FALSE)
  if ("role" %in% names(nodes) &&
      (!(is.character(nodes$role) || is.factor(nodes$role) ||
         (is.atomic(nodes$role) && all(is.na(nodes$role)))) || !is.null(dim(nodes$role)) ||
       any(!is.na(nodes$role) & !nodes$role %in% c("observed", "exposure", "outcome", "adjusted", "unobserved", "selected"))))
    stop("`nodes$role` must use observed, exposure, outcome, adjusted, unobserved or selected (or NA).", call. = FALSE)
  if ("label" %in% names(nodes) && (!is.atomic(nodes$label) || !is.null(dim(nodes$label))))
    stop("`nodes$label` must contain one atomic display label per node.", call. = FALSE)
  if (is.null(edges)) edges <- data.frame(from = character(), to = character())
  .metro_input_table(edges, c("from", "to"), "edges")
  edges <- as.data.frame(edges)
  .metro_check_endpoints(edges$from, edges$to, "edges")
  edges$from <- .metro_input_ids(edges$from, "edges$from")
  edges$to <- .metro_input_ids(edges$to, "edges$to")
  unknown <- setdiff(c(edges$from, edges$to), nodes$name)
  if (length(unknown))
    stop(sprintf("Every edge endpoint must exist in `nodes$name`; %s %s not there.",
      .metro_quote_list(unknown), if (length(unknown) == 1L) "is" else "are"), call. = FALSE)
  nodes <- nodes[, c("name", setdiff(names(nodes), "name")), drop = FALSE]
  edges <- edges[, c("from", "to", setdiff(names(edges), c("from", "to"))), drop = FALSE]
  .as_dag(igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes),
    argument = argument)
}

.metro_input_table <- function(x, required, argument) {
  if (!is.data.frame(x) || anyNA(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x)) || !all(required %in% names(x)))
    stop(sprintf("`%s` must be a data frame with unique column names including %s.",
      argument, paste(sprintf("`%s`", required), collapse = " and ")), call. = FALSE)
}

.metro_input_ids <- function(x, argument) {
  if (!is.atomic(x) || !is.null(dim(x)) || anyNA(x))
    stop(sprintf("`%s` must contain non-missing, non-empty node identifiers.", argument), call. = FALSE)
  x <- as.character(x)
  if (anyNA(x) || any(!nzchar(x)))
    stop(sprintf("`%s` must contain non-missing, non-empty node identifiers.", argument), call. = FALSE)
  x
}

#' Construct a DAG from a named adjacency matrix
#'
#' Each TRUE or 1 in row A, column B specifies A -> B. Rows and columns must
#' name the same nodes; their order may differ. Isolated nodes are retained.
#' Only binary, dense matrices are accepted. Supply weighted edges through
#' [metro_graph()] to retain their values as attributes.
#'
#' This constructor is explicit because a two-column matrix passed directly to
#' [metro_dag()] remains an edge list, including a 2 by 2 matrix. An edge
#' matrix with columns named `from` and `to` is read by those names, in either
#' order and ignoring other columns; otherwise its first column holds the
#' source of each arrow, and a matrix naming only its first column `to`, or
#' only its second `from`, is rejected.
#'
#' @param adjacency A non-empty square numeric or logical matrix containing
#'   only 0/1 or FALSE/TRUE, with unique, non-missing, non-empty row/column names.
#' @return A validated directed acyclic igraph. Matrix values and names are not
#'   modified. Self-loops and other cycles are rejected.
#' @examples
#' adjacency <- matrix(0L, 3, 3, dimnames = list(c("X", "Y", "I"), c("X", "Y", "I")))
#' adjacency["X", "Y"] <- 1L
#' metro_dag(metro_graph_from_adjacency(adjacency), exposure = "X", outcome = "Y")
#' @export
metro_graph_from_adjacency <- function(adjacency) {
  if (!is.matrix(adjacency) || !nrow(adjacency) || nrow(adjacency) != ncol(adjacency) ||
      !(is.numeric(adjacency) || is.logical(adjacency)) || is.complex(adjacency) ||
      anyNA(adjacency) || any(!is.finite(adjacency)) || any(!adjacency %in% c(0, 1)))
    stop("`adjacency` must be a non-empty square numeric or logical matrix containing only 0 and 1.", call. = FALSE)
  rows <- rownames(adjacency); columns <- colnames(adjacency)
  if (is.null(rows) || is.null(columns))
    stop("`adjacency` must have row and column names identifying the same nodes.", call. = FALSE)
  rows <- .metro_input_ids(rows, "adjacency row names")
  columns <- .metro_input_ids(columns, "adjacency column names")
  if (anyDuplicated(rows) || anyDuplicated(columns) || !setequal(rows, columns))
    stop("`adjacency` row and column names must be unique and identify the same nodes.", call. = FALSE)
  pairs <- which(adjacency == 1, arr.ind = TRUE)
  .metro_graph(data.frame(name = rows), data.frame(
    from = rows[pairs[, 1L]], to = columns[pairs[, 2L]]), "adjacency")
}
