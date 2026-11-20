# Display categories are independent of analysis annotations. They never enter
# layout, observability, adjustment checks or DAGitty's selection semantics.
.annotate_node_groups <- function(layout, node_groups = NULL, node_group_colours = NULL) {
  groups <- if (is.null(node_groups)) layout$node_groups else node_groups
  if (is.null(groups)) groups <- list()
  nm <- igraph::V(layout$graph)$name
  if (!is.list(groups))
    stop("`node_groups` must be a named list of character vectors of node names, for example list(Context = c(\"A\", \"B\")).", call. = FALSE)
  if (length(groups)) {
    keys <- names(groups)
    if (is.null(keys) || anyNA(keys) || any(!nzchar(trimws(keys))))
      stop("Every `node_groups` category needs a name, for example list(Context = c(\"A\", \"B\")).", call. = FALSE)
    if (anyDuplicated(keys))
      stop(sprintf("`node_groups` names category %s more than once.", .metro_quote_list(keys[duplicated(keys)])),
        call. = FALSE)
    for (key in keys) {
      members <- groups[[key]]
      if (!is.character(members) || !length(members) || anyNA(members))
        stop(sprintf("`node_groups` category `%s` must be a non-empty character vector of node names.", key),
          call. = FALSE)
      unknown <- setdiff(members, nm)
      if (length(unknown)) .metro_unknown_nodes(sprintf("`node_groups` category `%s`", key), unknown, layout$graph)
      if (anyDuplicated(members))
        stop(sprintf("`node_groups` category `%s` lists %s more than once.", key,
          .metro_quote_list(members[duplicated(members)])), call. = FALSE)
    }
    members <- unlist(groups, use.names = FALSE)
    if (anyDuplicated(members)) {
      node <- members[duplicated(members)][[1L]]
      within <- keys[vapply(groups, function(x) node %in% x, logical(1))]
      stop(sprintf("Node `%s` is in more than one `node_groups` category (%s); each node may belong to only one.",
        node, .metro_quote_list(within)), call. = FALSE)
    }
    groups <- groups[order(names(groups), method = "radix")]
    groups <- lapply(groups, function(x) sort(unname(x), method = "radix"))
  }
  colours <- node_group_colours
  if (is.null(colours)) {
    colours <- layout$node_group_colours
    if (!is.null(colours) && !is.null(node_groups))
      colours <- colours[names(colours) %in% names(groups)]
  }
  if (!is.null(colours) && length(colours)) {
    if (!is.character(colours) || is.null(names(colours)))
      stop("`node_group_colours` must be a character vector of colours named by group, for example c(Context = \"darkgreen\").",
        call. = FALSE)
    keys <- names(colours)
    if (anyNA(keys) || any(!nzchar(trimws(keys))))
      stop("Every `node_group_colours` colour needs a group name.", call. = FALSE)
    if (anyDuplicated(keys))
      stop(sprintf("`node_group_colours` names group %s more than once.", .metro_quote_list(keys[duplicated(keys)])),
        call. = FALSE)
    unknown <- setdiff(keys, names(groups))
    if (length(unknown))
      stop(sprintf("`node_group_colours` names %s not in `node_groups`: %s.%s",
        if (length(unknown) == 1L) "a group" else "groups", .metro_quote_list(unknown),
        if (length(groups)) sprintf(" Groups are: %s.", .metro_quote_list(names(groups), max = 20L)) else
          " Define the groups with `node_groups`."), call. = FALSE)
    if (anyNA(colours))
      stop(sprintf("`node_group_colours` has a missing colour for %s.", .metro_quote_list(keys[is.na(colours)])),
        call. = FALSE)
    invalid <- !vapply(colours, function(x) tryCatch({ grDevices::col2rgb(x); TRUE },
      error = function(e) FALSE), logical(1))
    if (any(invalid))
      stop(sprintf("`node_group_colours` has invalid colours for %s: %s.", .metro_quote_list(keys[invalid]),
        .metro_quote_list(colours[invalid], quote = "\"")), call. = FALSE)
    colours <- colours[order(names(colours), method = "radix")]
  } else if (!is.null(colours) && !is.character(colours))
    stop("`node_group_colours` must be a character vector of colours named by group, for example c(Context = \"darkgreen\").",
      call. = FALSE)
  # Keep the old, ungrouped layout structure when no categories are present.
  layout$node_groups <- if (length(groups)) groups else NULL
  layout$node_group_colours <- if (length(colours)) colours else NULL
  layout
}

.metro_group_membership <- function(layout, nodes) {
  groups <- layout$node_groups
  membership <- rep(NA_integer_, length(nodes))
  for (i in seq_along(groups)) membership[match(groups[[i]], nodes)] <- i
  membership
}

# Choose black/white against the station fill, including transparent fills.
.metro_group_ink <- function(fill, background) {
  rgb <- grDevices::col2rgb(fill, alpha = TRUE)[, 1L] / 255
  bg <- grDevices::col2rgb(background, alpha = TRUE)[, 1L] / 255
  bg[1:3] <- bg[1:3] * bg[4L] + 1 - bg[4L]
  rgb <- rgb[1:3] * rgb[4L] + bg[1:3] * (1 - rgb[4L])
  linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  luminance <- sum(linear * c(0.2126, 0.7152, 0.0722))
  if (luminance > 0.179) "#000000" else "#FFFFFF"
}

# Numeric category marks fit inside the existing station, so no extra outline
# or badge changes its attachment radius or creates a new causal station.
.metro_group_mark_grob <- function(mark, x, y, radius, fill, background, family = "") {
  if (is.null(mark) || is.na(mark) || !nzchar(mark)) return(NULL)
  gp <- grid::gpar(col = .metro_group_ink(fill, background),
    fontsize = 2.1 * 72.27 / 25.4, fontfamily = family, fontface = "bold")
  text <- grid::textGrob(mark, x = x, y = y, gp = gp)
  size <- c(grid::convertWidth(grid::grobWidth(text), "mm", valueOnly = TRUE),
    grid::convertHeight(grid::grobHeight(text), "mm", valueOnly = TRUE))
  # Fit in an inscribed square, leaving the station's outline clear.
  available <- max(0.01, radius - 0.25) * sqrt(2)
  gp$fontsize <- gp$fontsize * min(1, available / max(size))
  grid::editGrob(text, gp = gp)
}
