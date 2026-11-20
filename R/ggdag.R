# Read through ggdag's public accessors. The embedded DAGitty graph is the
# causal model; a mutated plotting table must not silently replace that model.
.from_ggdag <- function(graph, bidirected = "error") {
  if (!requireNamespace("ggdag", quietly = TRUE))
    stop("Install the optional 'ggdag' package to import tidy_dagitty objects.", call. = FALSE)
  dag <- ggdag::pull_dag(graph)
  if (!inherits(dag, "dagitty"))
    stop("The ggdag object must contain a DAGitty graph.", call. = FALSE)
  if (any(dagitty::edges(dag)$e == "<->")) {
    if (bidirected == "error") .from_dagitty(dag, bidirected)
    # Bidirected arcs add hidden nodes absent from the tidy table, so draw the
    # embedded DAGitty graph and keep the tidy table's display labels.
    g <- .from_dagitty(dag, bidirected)
    data <- ggdag::pull_dag_data(graph)
    if ("label" %in% names(data)) {
      labels <- as.character(data$label[!duplicated(data$name)])
      names(labels) <- as.character(data$name[!duplicated(data$name)])
      labels <- labels[!is.na(labels)]
      if (length(labels)) g <- .metro_import_labels(g, labels)
    }
    return(g)
  }
  g <- .from_dagitty(dag)
  data <- ggdag::pull_dag_data(graph)
  .metro_input_table(data, c("name", "to", "direction"), "ggdag data")
  nm <- igraph::V(g)$name
  nodes <- .metro_input_ids(data$name, "ggdag data$name")
  edge <- !is.na(data$to)
  to <- .metro_input_ids(data$to[edge], "ggdag data$to")
  direction <- as.character(data$direction[edge])
  disagreement <- function() stop(paste(
    "The ggdag data and its embedded DAGitty graph disagree.",
    "Supply a synchronized ggdag object or the intended DAGitty graph explicitly."), call. = FALSE)
  if (!setequal(c(nodes, to), nm) || anyNA(direction) || any(direction != "->")) disagreement()
  pairs <- function(from, to) {
    x <- unique(cbind(as.double(from), as.double(to)))
    unname(x[order(x[, 1L], x[, 2L], method = "radix"), , drop = FALSE])
  }
  ends <- igraph::as_edgelist(g, names = FALSE)
  if (!identical(pairs(match(nodes[edge], nm), match(to, nm)), pairs(ends[, 1L], ends[, 2L])))
    disagreement()
  # Status/adjustment columns are explicit ggdag annotations, not new role
  # suggestions. Conflicting views cannot be collapsed into one causal model.
  role <- igraph::vertex_attr(g, "role")
  if ("status" %in% names(data)) {
    status <- as.character(data$status)
    status[is.na(status)] <- "observed"
    expected <- role[match(nodes, nm)]
    expected[!expected %in% c("exposure", "outcome", "unobserved")] <- "observed"
    expected[expected == "unobserved"] <- "latent"
    if (any(status != expected)) disagreement()
  }
  if ("adjusted" %in% names(data)) {
    adjusted <- as.character(data$adjusted)
    adjusted[is.na(adjusted)] <- "unadjusted"
    expected <- ifelse(role[match(nodes, nm)] == "adjusted", "adjusted", "unadjusted")
    if (any(adjusted != expected)) disagreement()
  }
  if ("label" %in% names(data)) {
    if (!(is.character(data$label) || is.factor(data$label)))
      stop("ggdag display labels must be character or factor values.", call. = FALSE)
    labels <- as.character(data$label)
    use <- !is.na(labels)
    by_node <- split(labels[use], match(nodes[use], nm))
    if (any(vapply(by_node, function(x) length(unique(x)) > 1L, logical(1))))
      stop("ggdag data contains conflicting display labels for the same node.", call. = FALSE)
    values <- igraph::vertex_attr(g, "label")
    if (is.null(values)) values <- rep(NA_character_, length(nm))
    for (id in names(by_node)) values[as.integer(id)] <- by_node[[id]][1L]
    g <- igraph::set_vertex_attr(g, "label", value = values)
  }
  g
}

# ggdag's documented label attribute is also present on plain dagify() output,
# which can be imported without loading ggdag. Keep partial and blank labels.
.metro_import_labels <- function(graph, labels) {
  if (is.null(labels)) return(graph)
  # A named factor, as read from a node table, is used as text.
  if (is.factor(labels)) labels <- stats::setNames(as.character(labels), names(labels))
  nm <- igraph::V(graph)$name
  if (!is.character(labels) || (length(labels) && (is.null(names(labels)) ||
      anyNA(names(labels)) || anyDuplicated(names(labels)) || any(!names(labels) %in% nm))))
    stop("Imported DAGitty labels must be a named character vector using existing node names.", call. = FALSE)
  if (!length(labels)) return(graph)
  values <- rep(NA_character_, length(nm))
  values[match(names(labels), nm)] <- labels
  igraph::set_vertex_attr(graph, "label", value = values)
}
