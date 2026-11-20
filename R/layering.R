#' Assign layers to a DAG
#' @keywords internal
.assign_layers <- function(graph, method = "longest", rank = NULL) {
  method <- .assert_scalar_choice(method, c("longest", "sinks", "shortest"), "layering")
  names <- igraph::V(graph)$name
  if (!is.null(rank)) {
    hidden <- .metro_hidden_nodes(graph)
    rank <- .metro_named_rank(rank, names, hidden)
    # A node drawn for a bidirected arc joins the nearest supplied rank before
    # both of its children, or a new rank just before them.
    for (h in setdiff(hidden, names(rank))) {
      first <- min(rank[igraph::neighbors(graph, h, mode = "out")$name])
      before <- rank[rank < first]
      rank[[h]] <- if (length(before)) max(before) else first - 1
    }
    value <- rank[names]
    layer <- match(value, sort(unique(value))) - 1L
  } else {
    if (method == "shortest") {
      warning("`layering = \"shortest\"` is deprecated; using \"sinks\" (sink-aligned topological ranks).", call. = FALSE)
      method <- "sinks"
    }
    topo <- as.integer(igraph::topo_sort(graph, mode = "out"))
    layer <- integer(length(names))
    if (method == "longest") {
      for (v in topo) {
        p <- as.integer(igraph::neighbors(graph, v, mode = "in"))
        if (length(p)) layer[v] <- max(layer[p]) + 1L
      }
    } else {
      # Distance to the farthest sink gives strict ranks even with shortcuts.
      for (v in rev(topo)) {
        children <- as.integer(igraph::neighbors(graph, v, mode = "out"))
        if (length(children)) layer[v] <- max(layer[children]) + 1L
      }
      layer <- max(layer) - layer
    }
    value <- layer
  }
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  bad <- if (nrow(ends)) which(layer[ends[, 2L]] <= layer[ends[, 1L]]) else integer()
  if (length(bad)) {
    from <- ends[bad, 1L]; to <- ends[bad, 2L]
    stop(sprintf("Ranks must strictly increase along every arrow, but %s.", .metro_quote_list(
      sprintf("%s goes from rank %s to rank %s", .metro_arrow_text(names[from], names[to]),
        as.character(value[from]), as.character(value[to])), max = 3L, quote = "")),
      call. = FALSE)
  }
  stats::setNames(layer, names)
}

# Longest-path ranks put every source as far upstream as its longest chain
# allows, so a source with one late child needs a long bypass arrow and every
# extra rank it spans adds a routing waypoint. Moving a node to the end of its
# feasible interval on its heavier side (more children: as late as possible;
# more parents: as early as possible) strictly reduces the total edge span.
# Repeat until no node moves (vertex promotion); visit nodes by name.
.minimum_span_layers <- function(graph, layer) {
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  names <- igraph::V(graph)$name
  ids <- factor(seq_along(layer))
  parents <- split(ends[, 1L], factor(ends[, 2L], levels = levels(ids)))
  children <- split(ends[, 2L], factor(ends[, 1L], levels = levels(ids)))
  balance <- lengths(children) - lengths(parents)
  visit <- which(balance != 0)
  visit <- visit[order(names[visit], method = "radix")]
  layer <- as.integer(layer)
  repeat {
    moved <- FALSE
    for (v in visit) {
      target <- if (balance[v] > 0) min(layer[children[[v]]]) - 1L else
        max(layer[parents[[v]]]) + 1L
      if (target != layer[v]) {
        layer[v] <- target
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  stats::setNames(match(layer, sort(unique(layer))) - 1L, names)
}

# Validate user ranks and name them by node: by name, or in visible vertex
# order when unnamed. Hidden nodes of bidirected arcs may be named or omitted.
.metro_named_rank <- function(rank, nodes, hidden) {
  if (!is.numeric(rank))
    stop("`rank` must be a numeric vector, named by node or in vertex order.", call. = FALSE)
  visible <- setdiff(nodes, hidden)
  given <- names(rank)
  if (is.null(given)) {
    if (length(rank) != length(visible))
      stop(sprintf("Unnamed `rank` has %d value%s for %d node%s; give one value per node, in vertex order, or name the values.",
        length(rank), if (length(rank) == 1L) "" else "s",
        length(visible), if (length(visible) == 1L) "" else "s"), call. = FALSE)
    given <- names(rank) <- visible
  }
  if (anyNA(given) || any(!nzchar(given)))
    stop("`rank` must name every value or none.", call. = FALSE)
  if (anyDuplicated(given))
    stop(sprintf("`rank` names %s more than once; give each node one rank.",
      .metro_quote_list(given[duplicated(given)])), call. = FALSE)
  unknown <- setdiff(given, nodes)
  if (length(unknown))
    stop(sprintf("`rank` names %s, which %s not in the graph.%s", .metro_quote_list(unknown),
      if (length(unknown) == 1L) "is" else "are", .metro_node_hint(visible)), call. = FALSE)
  missing <- setdiff(visible, given)
  if (length(missing))
    stop(sprintf("Named `rank` must give every node a rank; it has none for %s.",
      .metro_quote_list(missing)), call. = FALSE)
  not_finite <- !is.finite(rank)
  if (any(not_finite))
    stop(sprintf("`rank` must give every node a finite rank, but it gives %s.", .metro_quote_list(
      sprintf("`%s` = %s", given[not_finite], as.character(rank[not_finite])), quote = "")),
      call. = FALSE)
  rank
}

