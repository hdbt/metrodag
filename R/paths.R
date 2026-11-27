# Translate a layout's graph into a DAGitty graph with private identifiers,
# marking unobserved variables as latent. Shared by adjustment checks and
# path highlighting so both see the same causal model.
.metro_dagitty_graph <- function(graph, unobserved) {
  nm <- igraph::V(graph)$name
  ids <- stats::setNames(paste0("v", seq_along(nm)), nm)
  nodes <- unname(ids)
  latent <- nm %in% unobserved
  nodes[latent] <- paste(nodes[latent], "[latent]")
  edges <- igraph::as_edgelist(graph, names = TRUE)
  arrows <- if (nrow(edges)) paste(ids[edges[, 1L]], "->", ids[edges[, 2L]]) else character()
  list(dag = dagitty::dagitty(paste("dag {", paste(c(nodes, arrows), collapse = "; "), "}")),
    ids = ids)
}

#' Highlight open paths between exposure and outcome
#'
#' Finds the paths between two sets of variables with DAGitty, given the
#' plot's adjustment set and any selected (conditioned-on) variables, and
#' emphasises the arrows on the chosen kind of path. The layout, roles and
#' labels are unchanged; only the arrows' colour, width or linetype change,
#' exactly as with `edge_style`. Requires the optional dagitty package.
#'
#' @param plot A MetroDAG plot from [metro_dag()] or [plot_metro_dag()].
#' @param from,to Node names; default to the plot's exposure and outcome.
#' @param paths Which paths to highlight: `"open"` (the default), every path
#'   that is open given the conditioning set; `"biasing"`, open paths that are
#'   not directed from `from` to `to` and do not pass through another `from`
#'   node (DAGitty's proper paths), usually the target when judging an
#'   adjustment set; `"causal"`, open directed paths from `from` to `to`; or
#'   `"all"`, every path whether open or not.
#' @param colour,width,linetype Appearance of highlighted arrows. `width` in
#'   millimetres defaults to twice the plot's line width; `colour = NULL`
#'   keeps each arrow's colour and `linetype = NULL` its linetype.
#' @param limit Maximum number of paths DAGitty lists between each pair of
#'   nodes. Dense graphs can have very many paths; if the search stops at the
#'   limit, a warning says so. For `"open"` and `"causal"`, whether any such
#'   path exists is still decided exactly, so a truncated search is reported
#'   only when it may have missed one.
#' @return The plot with highlighted arrows. The attribute
#'   `metrodag_paths` holds a data frame of the paths found, whether each is
#'   open, whether it is causal (directed from `from` to `to`) and whether it
#'   was highlighted. Its attribute `truncated` is `TRUE` if the search
#'   stopped at `limit` for some pair. Bidirected arcs drawn as hidden latent
#'   variables appear as `<->` in the path text.
#' @examples
#' if (requireNamespace("dagitty", quietly = TRUE)) {
#'   p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y",
#'     unobserved = "U", adjusted = "C")
#'   print(highlight_paths(p, paths = "biasing"))
#'   attr(highlight_paths(p), "metrodag_paths")
#' }
#' @export
highlight_paths <- function(plot, from = NULL, to = NULL, paths = "open",
  colour = "#C0392B", width = NULL, linetype = NULL, limit = 10000) {
  .require_dagitty()
  if (!inherits(plot, "metrodag_plot") || inherits(plot, "patchwork"))
    stop("`plot` must be a MetroDAG plot from metro_dag() or plot_metro_dag().", call. = FALSE)
  kind <- .assert_scalar_choice(paths, c("open", "biasing", "causal", "all"), "paths")
  if (!identical(limit, Inf)) .assert_number(limit, "limit", minimum = 1, integer = TRUE)
  limit <- as.integer(min(limit, .Machine$integer.max - 1L))
  layout <- attr(plot, "metrodag_layout")
  analysis <- layout$analysis
  graph <- layout$graph
  nm <- igraph::V(graph)$name
  hidden <- .metro_hidden_nodes(graph)
  if (is.null(from)) from <- analysis$exposure
  if (is.null(to)) to <- analysis$outcome
  for (key in c("from", "to")) {
    value <- get(key)
    if (!is.character(value) || !length(value) || anyNA(value) || any(!value %in% setdiff(nm, hidden)))
      stop(sprintf("`%s` must name existing nodes; set exposure/outcome or supply it.", key), call. = FALSE)
  }
  index <- which(vapply(plot$layers, function(l) inherits(l$geom, "GeomMetroDag"), logical(1)))
  if (length(index) != 1L) stop("`plot` has no MetroDAG layer to highlight.", call. = FALSE)
  layer <- plot$layers[[index]]
  if (is.null(width)) width <- 2 * layer$geom_params$config$line_width
  .assert_number(width, "width", strict = TRUE)
  if (!is.null(colour) && (!is.character(colour) || length(colour) != 1L || is.na(colour) ||
      inherits(try(grDevices::col2rgb(colour), silent = TRUE), "try-error")))
    stop(paste("`colour` must be one valid colour, such as \"#C0392B\" or \"red\",",
      "or NULL to keep each arrow's colour."), call. = FALSE)
  if (!is.null(linetype) && (!is.character(linetype) || length(linetype) != 1L ||
      is.na(linetype) || !.metro_valid_linetype(linetype)))
    stop(paste("`linetype` must be NULL or one of solid, dashed, dotted, dotdash, longdash,",
      "twodash or a hex dash pattern such as \"44\"."), call. = FALSE)
  translated <- .metro_dagitty_graph(graph, union(analysis$unobserved, hidden))
  ids <- translated$ids
  conditioned_names <- c(analysis$adjusted, analysis$selected)
  conditioned <- unname(ids[conditioned_names])
  names_of <- stats::setNames(names(ids), ids)
  found <- list(); truncated <- character()
  for (a in from) for (b in to) if (a != b) {
    # One path more than `limit` shows whether the search was cut off.
    result <- dagitty::paths(translated$dag, ids[[a]], ids[[b]], Z = conditioned,
      limit = limit + 1L, directed = kind == "causal")
    cut <- length(result$paths) > limit
    keep <- seq_len(min(length(result$paths), limit))
    if (length(keep)) found[[length(found) + 1L]] <-
      data.frame(from = a, to = b, path = result$paths[keep], open = result$open[keep])
    # A complete search is exact. Otherwise ask whether a relevant path exists
    # at all, so a truncated search never claims there is none.
    if (cut) {
      relevant <- switch(kind,
        open = , biasing = dagitty::dconnected(translated$dag, ids[[a]], ids[[b]], conditioned),
        causal = .metro_open_directed(graph, a, b, conditioned_names),
        all = TRUE)
      if (relevant) truncated <- c(truncated, paste(a, "and", b))
    }
  }
  table <- if (length(found)) do.call(rbind, found) else
    data.frame(from = character(), to = character(), path = character(), open = logical())
  tokens <- lapply(table$path, function(text) strsplit(text, " ", fixed = TRUE)[[1L]])
  table$causal <- vapply(tokens, function(t) length(t) < 2L ||
    all(t[seq.int(2L, length(t) - 1L, by = 2L)] == "->"), logical(1))
  # As in DAGitty's adjustment criterion, a path through another `from` node
  # is not a biasing path for the joint effect of all of them.
  from_ids <- unname(ids[from])
  proper <- vapply(tokens, function(t) {
    nodes <- t[seq.int(1L, length(t), by = 2L)]
    !any(nodes[-c(1L, length(nodes))] %in% from_ids)
  }, logical(1))
  table$highlighted <- switch(kind, open = table$open, biasing = table$open & !table$causal & proper,
    causal = table$open & table$causal, all = rep(TRUE, nrow(table)))
  # Arrows are kept as from/to pairs: node names may themselves contain "->".
  tail <- character(); head <- character()
  for (t in tokens[table$highlighted]) {
    for (k in seq.int(2L, length(t) - 1L, by = 2L)) {
      ends <- c(names_of[[t[k - 1L]]], names_of[[t[k + 1L]]])
      if (t[k] != "->") ends <- rev(ends)
      tail <- c(tail, ends[1L]); head <- c(head, ends[2L])
    }
  }
  arrows <- unique(data.frame(from = tail, to = head, stringsAsFactors = FALSE))
  # Show readable paths with the plot's node names, not DAGitty identifiers;
  # a hidden latent variable between two arrowheads is a bidirected arc.
  table$path <- vapply(tokens, function(t) {
    nodes <- seq.int(1L, length(t), by = 2L)
    t[nodes] <- names_of[t[nodes]]
    for (k in rev(nodes[t[nodes] %in% hidden])) if (k > 1L && k < length(t) &&
        t[k - 1L] == "<-" && t[k + 1L] == "->") t <- c(t[seq_len(k - 2L)], "<->", t[-seq_len(k + 1L)])
    paste(t, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
  table <- table[c("from", "to", "path", "open", "causal", "highlighted")]
  attr(table, "truncated") <- length(truncated) > 0L
  description <- switch(kind, open = "open ", biasing = "open biasing ", causal = "open causal ", all = "")
  between <- paste(paste(from, collapse = ", "), "and", paste(to, collapse = ", "))
  if (length(truncated)) {
    warning(sprintf(if (nrow(arrows))
      "The path search stopped at `limit` = %d between %s; some %spaths may not be highlighted. Increase `limit` to check every path." else
      "The path search stopped at `limit` = %d between %s and found no %spaths among those listed. Increase `limit` to check every path.",
      limit, paste(truncated, collapse = ", "), description), call. = FALSE)
  } else if (!nrow(arrows)) warning("No ", description, "paths found between ", between, ".", call. = FALSE)
  if (nrow(arrows)) {
    styles <- arrows
    if (!is.null(colour)) styles$colour <- colour
    styles$width <- width
    if (!is.null(linetype)) styles$linetype <- linetype
    # Layers are environments; give the plot its own copy before changing it.
    copy <- ggplot2::ggproto(NULL, layer)
    params <- layer$geom_params
    params$edges <- .metro_apply_edge_style(params$edges, styles)
    copy$geom_params <- params
    plot$layers[[index]] <- copy
    attr(plot, "metrodag_edge_style") <- .metro_merge_edge_style(attr(plot, "metrodag_edge_style"), styles)
  }
  attr(plot, "metrodag_paths") <- table
  plot
}

# Is there a directed path from `from` to `to` that avoids every conditioned
# node? Such a path has no colliders, so it is open exactly then.
.metro_open_directed <- function(graph, from, to, conditioned) {
  blocked <- setdiff(conditioned, c(from, to))
  if (length(blocked)) graph <- igraph::delete_vertices(graph, blocked)
  is.finite(igraph::distances(graph, from, to, mode = "out", weights = NA)[1L, 1L])
}
