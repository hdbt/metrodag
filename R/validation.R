#' Validate and normalize a DAG
#'
#' `warn_spaces = FALSE` leaves the warning about names that differ only by
#' white space to a later check of the same graph.
#' @noRd
.as_dag <- function(graph, bidirected = "error", argument = "graph", warn_spaces = TRUE) {
  if (inherits(graph, "tidy_dagitty")) graph <- .from_ggdag(graph, bidirected)
  if (inherits(graph, "dagitty")) graph <- .from_dagitty(graph, bidirected)
  if (!inherits(graph, "igraph")) .metro_graph_input_error(graph, argument)
  if (!igraph::is_directed(graph)) stop(sprintf("`%s` must be directed.", argument), call. = FALSE)
  if (igraph::vcount(graph) == 0L)
    stop(sprintf("`%s` must contain at least one node.", argument), call. = FALSE)
  nm <- igraph::V(graph)$name
  if (is.null(nm)) {
    # A tidygraph node table without `name` is easy to mistake for one whose
    # identifiers MetroDAG reads, so say how its nodes are named instead.
    if (inherits(graph, "tbl_graph") && length(igraph::vertex_attr_names(graph)))
      message(sprintf(paste("`%s` has no `name` node column, so its nodes are named 1 to %d",
        "in row order. Add a `name` column to refer to nodes by name."),
        argument, igraph::vcount(graph)))
    nm <- as.character(seq_len(igraph::vcount(graph)))
  }
  # igraph permits numeric name attributes. Treat them as identifiers, as we
  # do for edge tables, rather than numeric row/column indices during rendering.
  missing_names <- anyNA(nm)
  nm <- .metro_node_names(nm)
  if (missing_names || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm))
    stop("Vertex names must be non-empty, non-missing and unique.", call. = FALSE)
  igraph::V(graph)$name <- nm
  .metro_assert_simple_dag(graph, argument)
  if (warn_spaces) .metro_warn_padded_names(nm)
  graph
}

# Self-loops, repeated arrows and cycles are reported in that order, naming the
# arrows at fault so a user can find them in a large edge table.
.metro_assert_simple_dag <- function(graph, argument = "graph") {
  loops <- igraph::which_loop(graph)
  if (any(loops)) {
    at <- unique(igraph::ends(graph, igraph::E(graph)[loops], names = TRUE)[, 1L])
    one <- length(at) == 1L
    stop(sprintf("`%s` has %s at %s; a DAG cannot contain %s.", argument,
      if (one) "a self-loop" else "self-loops", .metro_quote_list(at),
      if (one) "one" else "them"), call. = FALSE)
  }
  repeated <- igraph::which_multiple(graph)
  if (any(repeated)) {
    ends <- igraph::ends(graph, igraph::E(graph)[repeated], names = TRUE)
    arrows <- unique(.metro_arrow_text(ends[, 1L], ends[, 2L]))
    stop(sprintf("`%s` repeats the arrow%s %s; supply each arrow once.", argument,
      if (length(arrows) == 1L) "" else "s", .metro_quote_list(arrows, quote = "")),
      call. = FALSE)
  }
  if (igraph::is_dag(graph)) return(invisible(graph))
  nm <- igraph::V(graph)$name
  removal <- igraph::ends(graph, .metro_cycle_breakers(graph), names = FALSE)
  # Show the shortest cycle closed by the first removable arrow on a cycle.
  cycle <- integer()
  for (i in seq_len(nrow(removal))) {
    path <- suppressWarnings(igraph::shortest_paths(graph, removal[i, 2L], removal[i, 1L],
      mode = "out", weights = NA, output = "vpath")$vpath[[1L]])
    if (length(path)) {
      cycle <- c(as.integer(path), removal[i, 2L])
      break
    }
  }
  shown <- nm[cycle]
  if (length(shown) > 10L)
    shown <- c(head(shown, 5L), "...", tail(shown, 4L))
  stop(sprintf(paste("`%s` contains the directed cycle %s; MetroDAG only lays out DAGs.",
    "Removing %s would make it acyclic."), argument, paste(shown, collapse = " -> "),
    .metro_quote_list(.metro_arrow_text(nm[removal[, 1L]], nm[removal[, 2L]]), quote = "",
      distinct = FALSE)),
    call. = FALSE)
}

# Arrows whose removal makes a cyclic `graph` acyclic, as edge ids. Each lies
# on a cycle and, unless a cyclic part is very large, none is redundant; one
# arrow that breaks every cycle of its part (an arrow typed the wrong way
# round, say) is named alone. Every cycle lies within one strongly connected
# component, so the heuristic's arrows are taken per component, and only a
# component given several is looked at again, on its own subgraph.
# `weights = NA` ignores any `weight` edge attribute (a path coefficient, say),
# which igraph would otherwise use and reject when missing or negative.
.metro_cycle_breakers <- function(graph) {
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  member <- igraph::components(graph, mode = "strong")$membership
  found <- as.integer(igraph::feedback_arc_set(graph, weights = NA, algo = "approx_eades"))
  # Arrows between components lie on no cycle.
  found <- found[member[ends[found, 1L]] == member[ends[found, 2L]]]
  # The heuristic's arrows by component, in the order it gave them.
  part_of <- member[ends[found, 1L]]
  parts <- split(found, factor(part_of, levels = unique(part_of)))
  several <- names(parts)[lengths(parts) > 1L]
  if (!length(several)) return(found)
  graph <- igraph::set_edge_attr(graph, "metrodag_edge", value = seq_len(nrow(ends)))
  vertices <- split(seq_along(member), member)
  for (component in several) {
    part <- igraph::induced_subgraph(graph, vertices[[component]])
    ids <- igraph::edge_attr(part, "metrodag_edge")
    mine <- match(parts[[component]], ids)
    # An arrow that breaks every cycle lies on each of them, so only the
    # arrows of one cycle need trying: the first arrow and its way back.
    first <- igraph::ends(part, 1L, names = FALSE)
    back <- suppressWarnings(igraph::shortest_paths(part, first[1L, 2L], first[1L, 1L],
      mode = "out", weights = NA, output = "epath")$epath[[1L]])
    single <- NULL
    if ((length(back) + 1) * igraph::ecount(part) <= 2e7) for (arrow in c(1L, as.integer(back)))
      if (igraph::is_dag(igraph::delete_edges(part, arrow))) {
        single <- arrow
        break
      }
    if (!is.null(single)) mine <- single
    # Drop arrows that the others make redundant, while that stays quick.
    else if (length(mine) * igraph::ecount(part) <= 2e7) for (arrow in mine) {
      rest <- setdiff(mine, arrow)
      if (igraph::is_dag(igraph::delete_edges(part, rest))) mine <- rest
    }
    parts[[component]] <- ids[mine]
  }
  unlist(parts, use.names = FALSE)
}

# Edge endpoints must name a node: report rows with a missing or empty one.
.metro_check_endpoints <- function(from, to, argument) {
  rows <- function(which) sprintf("row%s %s of `%s`", if (length(which) == 1L) "" else "s",
    .metro_quote_list(which, quote = ""), argument)
  # Test the text, so that an NA factor level counts as missing too.
  from <- as.character(from); to <- as.character(to)
  missing <- which(is.na(from) | is.na(to))
  if (length(missing))
    stop(sprintf("Edge endpoints cannot be missing; see %s.", rows(missing)), call. = FALSE)
  empty <- which(!nzchar(from) | !nzchar(to))
  if (length(empty))
    stop(sprintf("Edge endpoints cannot be empty; see %s.", rows(empty)), call. = FALSE)
  invisible(TRUE)
}

# Node identifiers as node names, for graph inputs and for the role and
# edge_style arguments that name nodes. as.character() writes the number
# 100000 as "1e+05", so whole numbers below 2^53, which doubles hold exactly,
# are written in full. Other numbers, zero (sprintf() writes -0 as "-0") and
# classed values such as dates are written as as.character() writes them.
# Names are kept in UTF-8: they break layout ties in byte order, which must
# not depend on the encoding the input declared.
.metro_node_names <- function(x) {
  text <- as.character(x)
  if (is.numeric(x) && !is.object(x)) {
    whole <- is.finite(x) & x == round(x) & abs(x) < 2^53 & x != 0
    text[whole] <- sprintf("%.0f", x[whole])
  }
  enc2utf8(text)
}

# Names that differ only by white space at their ends, such as "Smoking " and
# "Smoking", are separate nodes that look alike in a figure, and separate
# variables for adjustment checks. Say which, but keep the names as given.
.metro_warn_padded_names <- function(nm) {
  key <- trimws(nm)
  padded <- unique(key[duplicated(key)])
  if (!length(padded)) return(invisible(FALSE))
  # Show at most five sets, each quoted and escaped so that the white space shows.
  sets <- vapply(head(padded, 5L), function(k) .metro_quote_list(encodeString(nm[key == k],
    quote = "\""), quote = ""), character(1), USE.NAMES = FALSE)
  if (length(padded) == 1L) {
    warning(sprintf(paste("Node names %s differ only by white space at their ends, so they are",
      "separate nodes. Remove the white space if they name the same variable."), sets), call. = FALSE)
  } else {
    if (length(padded) > 5L) sets <- c(sets, sprintf("and %d more", length(padded) - 5L))
    warning(sprintf(paste("Node names differ only by white space at their ends, so these are",
      "separate nodes: %s. Remove the white space where they name the same variable."),
      paste(sets, collapse = "; ")), call. = FALSE)
  }
  invisible(TRUE)
}

# What a graph argument received, for messages that reject it.
.metro_describe_input <- function(x) {
  if (is.null(x)) return("NULL")
  if (inherits(x, "metrodag_layout")) return("a metrodag_layout")
  if (inherits(x, "ggplot")) return(if (is.null(attr(x, "metrodag_layout")))
    "a ggplot that MetroDAG did not draw" else "a MetroDAG plot")
  if (inherits(x, "formula")) return("a formula")
  if (is.function(x)) return("a function")
  if (is.factor(x)) return("a factor")
  if (is.character(x) && is.null(dim(x)))
    return(if (length(x) == 1L) "a character string" else "a character vector")
  if (is.list(x) && is.null(attr(x, "class"))) return("a list")
  if (is.atomic(x) && is.null(attr(x, "class")) && is.null(dim(x))) {
    type <- if (is.numeric(x)) "numeric" else typeof(x)
    return(sprintf("%s %s vector", if (grepl("^[aeiou]", type)) "an" else "a", type))
  }
  sprintf("an object of class `%s`", class(x)[1L])
}

# Reject an unsupported graph input, listing what this entry point accepts.
# `layout` and `plot` add a metrodag_layout and a MetroDAG plot to the list.
.metro_graph_input_error <- function(graph, argument = "graph", layout = FALSE, plot = FALSE) {
  accepted <- c("an igraph or tidygraph graph", "a dagitty or ggdag object",
    "a two-column edge matrix", "a data frame with `from` and `to` columns",
    if (layout) "a metrodag_layout", if (plot) "a MetroDAG plot")
  accepted <- paste0(paste(accepted[-length(accepted)], collapse = ", "), ", or ",
    accepted[length(accepted)])
  hint <- ""
  if (is.character(graph) && length(graph) == 1L && !is.na(graph) &&
      (grepl("^\\s*dag\\b", graph, ignore.case = TRUE) || grepl("->", graph, fixed = TRUE))) {
    hint <- " It looks like DAGitty code: pass dagitty::dagitty(text) first."
  } else if (is.list(graph) && !is.data.frame(graph) && all(c("from", "to") %in% names(graph))) {
    hint <- " To use its `from` and `to` elements, pass data.frame(from = ..., to = ...)."
  } else if (inherits(graph, "formula")) {
    hint <- " To build a DAG from formulas, use ggdag::dagify() first."
  } else if (inherits(graph, "metrodag_layout")) {
    hint <- " To lay out its graph again, pass its `graph` element."
  } else if (inherits(graph, "ggplot") && !is.null(attr(graph, "metrodag_layout"))) {
    hint <- if (layout) " To reuse its layout, pass attr(plot, \"metrodag_layout\")." else
      " To lay out its graph again, pass attr(plot, \"metrodag_layout\")$graph."
  }
  stop(sprintf("`%s` is %s. Supply %s.%s", argument, .metro_describe_input(graph),
    accepted, hint), call. = FALSE)
}

# Normalize all public graph inputs consistently, including numeric identifiers.
# `layout` and `plot` only describe what the calling entry point also accepts
# in error messages: metro_dag() and compare_metro_dags() take a layout too.
# `warn_spaces` is passed to .as_dag().
.as_metro_input <- function(graph, bidirected = "error", argument = "graph",
  layout = TRUE, plot = FALSE, warn_spaces = TRUE) {
  if (inherits(graph, "tidy_dagitty") || inherits(graph, "dagitty"))
    return(.as_dag(graph, bidirected, argument, warn_spaces))
  if (is.data.frame(graph)) {
    if (!all(c("from", "to") %in% names(graph)))
      stop(sprintf("An edge data frame must contain `from` and `to` columns; `%s` has %s.",
        argument, if (length(names(graph))) paste("columns", .metro_quote_list(names(graph)))
        else "no columns"), call. = FALSE)
    .metro_check_endpoints(graph$from, graph$to, argument)
    # Name the nodes here: igraph would write the node 100000 as "1e+05".
    graph <- igraph::graph_from_data_frame(data.frame(from = .metro_node_names(graph$from),
      to = .metro_node_names(graph$to)), directed = TRUE)
  } else if (is.matrix(graph)) {
    # Named `from` and `to` columns are matched by name, as for data frames,
    # so a matrix such as as.matrix(edges[, c("to", "from")]) keeps direction.
    # Otherwise columns are read by position; a lone `to` first or `from`
    # second would be read against its name.
    named <- c("from", "to") %in% colnames(graph)
    if (all(named)) graph <- graph[, c("from", "to"), drop = FALSE] else
    if (any(named) && ncol(graph) == 2L) {
      misplaced <- c("to", "from")[c(identical(colnames(graph)[1L], "to"), identical(colnames(graph)[2L], "from"))]
      if (length(misplaced))
        stop(sprintf(paste("An edge matrix without both `from` and `to` columns is read by position,",
          "with the source of each arrow first, but this one names its %s column `%s`.",
          "Name the columns `from` and `to`, or put the sources first."),
          if (misplaced[1L] == "to") "first" else "second", misplaced[1L]), call. = FALSE)
    }
    if (ncol(graph) != 2L) {
      square <- nrow(graph) == ncol(graph) && (is.numeric(graph) || is.logical(graph))
      stop(paste0("An edge matrix must have two columns.",
        if (square) " For an adjacency matrix, use metro_graph_from_adjacency()." else ""),
        call. = FALSE)
    }
    .metro_check_endpoints(graph[, 1L], graph[, 2L], argument)
    graph <- igraph::graph_from_edgelist(matrix(.metro_node_names(graph), ncol = 2L), directed = TRUE)
  } else if (!inherits(graph, "igraph")) {
    .metro_graph_input_error(graph, argument, layout, plot)
  }
  .as_dag(graph, argument = argument, warn_spaces = warn_spaces)
}

# Rendering and metrics must refer to the same complete set of stations.
.assert_layout_nodes <- function(layout) {
  nodes <- layout$nodes
  nm <- igraph::V(layout$graph)$name
  fail <- function() stop(paste(
    "Layout node rows must match graph vertices by unique, non-missing, non-empty names."),
    call. = FALSE)
  if (!is.data.frame(nodes) || !"node" %in% names(nodes) ||
      !is.atomic(nodes$node) || !is.null(dim(nodes$node))) fail()
  ids <- match(nodes$node, nm)
  if (!length(ids) || length(ids) != length(nm) || anyNA(nodes$node) ||
      any(!nzchar(nodes$node)) || anyNA(ids) || anyDuplicated(ids)) fail()
  ids
}

.real_finite_vector <- function(x) {
  is.numeric(x) && !is.complex(x) && is.null(dim(x)) && all(is.finite(x))
}

# A polyline renderer connects successive rows. Reject incomplete geometry
# instead of silently inventing connectors or repairing detached endpoints.
.assert_layout_geometry <- function(layout) {
  n <- layout$nodes; e <- layout$edges
  coordinates <- function(x, keys) all(keys %in% names(x)) &&
    all(vapply(x[keys], .real_finite_vector, logical(1)))
  if (!coordinates(n, c("x", "y")))
    stop("Layout node coordinates must be finite real numeric vectors.", call. = FALSE)
  if (!coordinates(e, c("x", "y", "xend", "yend")))
    stop("Layout route coordinates must be finite real numeric vectors.", call. = FALSE)
  segment_error <- function() stop(paste(
    "Layout `segment` must contain unique, finite real numeric ordering values within each edge."),
    call. = FALSE)
  if (!"segment" %in% names(e) || !.real_finite_vector(e$segment)) segment_error()
  if (!nrow(e)) return(invisible(layout))
  # Do not format IDs as text: distinct numeric IDs can print identically.
  group <- match(e$edge_id, unique(e$edge_id))
  order <- order(group, e$segment, method = "radix")
  e <- e[order, , drop = FALSE]; group <- group[order]
  same <- head(group, -1L) == tail(group, -1L)
  if (any(same & head(e$segment, -1L) == tail(e$segment, -1L))) segment_error()
  first <- !duplicated(group); last <- !duplicated(group, fromLast = TRUE)
  from <- match(e$from[first], n$node); to <- match(e$to[last], n$node)
  connected <- all(!same | (head(e$xend, -1L) == tail(e$x, -1L) &
    head(e$yend, -1L) == tail(e$y, -1L)))
  attached <- all(e$x[first] == n$x[from] & e$y[first] == n$y[from]) &&
    all(e$xend[last] == n$x[to] & e$yend[last] == n$y[to])
  if (!connected || !attached) stop(paste(
    "Layout routes must be continuous and join their named source and destination nodes.",
    "Recompute the layout or update the complete route after editing geometry."), call. = FALSE)
  invisible(layout)
}

# Reused layouts may reorder or relabel route IDs, but must retain the graph's
# complete edge set. Compare vertex pairs directly: pasted names can alias.
.assert_layout_edges <- function(layout) {
  e <- layout$edges
  fail <- function() stop(paste(
    "Layout edge rows must match graph edges one-to-one through non-missing",
    "edge_id values and consistent from/to endpoints."), call. = FALSE)
  keys <- c("edge_id", "from", "to")
  if (!is.data.frame(e) || !all(keys %in% names(e)) ||
      !all(vapply(e[keys], function(z) is.atomic(z) && is.null(dim(z)), logical(1))))
    fail()
  ids <- if (is.factor(e$edge_id)) as.character(e$edge_id) else e$edge_id
  if (anyNA(ids)) fail()
  nm <- igraph::V(layout$graph)$name
  from <- match(e$from, nm); to <- match(e$to, nm)
  if (anyNA(from) || anyNA(to)) fail()
  first <- !duplicated(ids)
  group <- match(ids, ids[first])
  if (any(from != from[first][group]) || any(to != to[first][group])) fail()
  routed <- cbind(from[first], to[first])
  expected <- igraph::ends(layout$graph, igraph::E(layout$graph), names = FALSE)
  if (nrow(routed) != nrow(expected) || !all(
      routed[order(routed[, 1L], routed[, 2L], method = "radix"), , drop = FALSE] ==
      expected[order(expected[, 1L], expected[, 2L], method = "radix"), , drop = FALSE])) fail()
  invisible(layout)
}

