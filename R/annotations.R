.assert_estimand <- function(estimand) {
  if (!is.null(estimand) && (!is.character(estimand) || length(estimand) != 1L ||
      is.na(estimand) || !nzchar(trimws(estimand))))
    stop("`estimand` must be NULL or a single non-empty label.", call. = FALSE)
  invisible(estimand)
}

# Analysis annotations operate on a copy of the graph, after layout. A supplied
# role replaces its previous set, including character(0) for an empty set.
.annotate_metro_layout <- function(layout, exposure, outcome, adjusted,
  unobserved, unadjusted, estimand, selected = NULL) {
  .assert_flag(unadjusted, "unadjusted")
  .assert_estimand(estimand)
  nm <- igraph::V(layout$graph)$name
  role <- igraph::vertex_attr(layout$graph, "role")
  if (is.null(role)) role <- rep("observed", length(nm))
  # Factor attributes need their labels before adding the missing-value default,
  # which may not be one of the factor's declared levels. NA elements of a list
  # attribute are missing too, although as.character() writes them as "NA".
  absent <- is.na(role)
  role <- as.character(role)
  role[absent] <- "observed"
  roles <- list(exposure = exposure, outcome = outcome, adjusted = adjusted,
    unobserved = unobserved, selected = selected)
  supplied <- !vapply(roles, is.null, logical(1))
  # Check the attribute whenever a role is read from it; `unadjusted = TRUE`
  # replaces the adjusted role as a supplied set would.
  inherited <- !supplied
  inherited[["adjusted"]] <- inherited[["adjusted"]] && !unadjusted
  if (any(inherited)) .metro_check_role_attribute(role, nm)
  hidden <- .metro_hidden_nodes(layout$graph)
  for (key in names(roles)) {
    value <- roles[[key]]
    # Numeric edge tables give nodes such as "1"; accept exposure = 1 for them,
    # naming nodes as edge tables do (exposure = 1e5 is "100000").
    # A bare NA is logical; report it as the missing value it is.
    if (is.numeric(value) || is.factor(value) || (is.logical(value) && length(value) && all(is.na(value))))
      value <- .metro_node_names(value)
    if (!is.null(value)) {
      if (!is.character(value))
        stop(sprintf("`%s` must be a character vector of node names.", key), call. = FALSE)
      if (anyNA(value))
        stop(sprintf("`%s` contains a missing value; give node names only.", key), call. = FALSE)
      unknown <- setdiff(value, nm)
      if (length(unknown)) .metro_unknown_nodes(sprintf("`%s`", key), unknown, layout$graph)
      if (anyDuplicated(value))
        stop(sprintf("`%s` names %s more than once.", key,
          .metro_quote_list(value[duplicated(value)])), call. = FALSE)
    }
    if (key != "unobserved" && any(value %in% hidden))
      stop(sprintf("`%s` cannot include %s: nodes drawn for bidirected arcs are always unobserved.",
        key, .metro_quote_list(intersect(value, hidden))), call. = FALSE)
    roles[key] <- list(if (is.null(value)) nm[role == key] else value)
  }
  # Nodes drawn for bidirected arcs stay unobserved whatever `unobserved` says.
  roles$unobserved <- union(roles$unobserved, hidden)
  if (unadjusted) {
    if (length(adjusted))
      stop(sprintf("`unadjusted = TRUE` cannot be combined with a non-empty `adjusted` set (%s).",
        .metro_quote_list(roles$adjusted)), call. = FALSE)
    roles$adjusted <- character()
  }
  .metro_check_role_clash(roles, supplied)
  role <- rep("observed", length(nm))
  for (key in names(roles)) role[match(roles[[key]], nm)] <- key
  layout$graph <- igraph::set_vertex_attr(layout$graph, "role", value = role)
  specified <- unadjusted || !is.null(adjusted) || length(roles$adjusted) > 0L ||
    isTRUE(layout$analysis$adjustment_specified)
  layout$analysis <- c(roles, list(estimand = estimand,
    adjustment_specified = specified, unadjusted = specified && !length(roles$adjusted)))
  layout
}

# Roles must be disjoint. Name the first node in two roles and, when one of
# them came from the graph or a reused layout rather than this call, how to
# clear it.
.metro_check_role_clash <- function(roles, supplied) {
  members <- unlist(roles, use.names = FALSE)
  if (!anyDuplicated(members)) return(invisible(TRUE))
  node <- members[duplicated(members)][[1L]]
  clash <- names(roles)[vapply(roles, function(r) node %in% r, logical(1))][1:2]
  message <- sprintf("Node `%s` cannot be both %s and %s; roles must be disjoint.", node, clash[[1L]], clash[[2L]])
  inherited <- clash[!supplied[clash]]
  if (length(inherited)) message <- paste0(message, sprintf(paste0(
    " Its %s role is inherited from the graph or a reused layout; clear it with ",
    "`%s = character(0)` or give `%s` without `%s`."), inherited[[1L]], inherited[[1L]],
    inherited[[1L]], node))
  stop(message, call. = FALSE)
}

# An inherited `role` attribute should use the role names metro_graph() accepts
# in `nodes$role`; `role` has NA already read as observed. Any other value is
# drawn as observed and drops its role from the analysis, so warn and name the
# nodes and values at fault. A value is never mapped to a role it resembles.
# A warning rather than an error keeps graphs that use a `role` attribute for
# something else drawing as they did.
.metro_check_role_attribute <- function(role, nm) {
  valid <- c("observed", "exposure", "outcome", "adjusted", "unobserved", "selected")
  bad <- !role %in% valid
  if (!any(bad)) return(invisible(TRUE))
  values <- unique(role[bad])
  # Name the nodes of up to six values, or of five and count the rest.
  shown <- values[seq_len(if (length(values) > 6L) 5L else length(values))]
  found <- vapply(shown, function(value) {
    nodes <- nm[role == value]
    one <- length(nodes) == 1L
    sprintf("%s %s %s \"%s\"", if (one) "node" else "nodes", .metro_quote_list(nodes),
      if (one) "has" else "have", value)
  }, character(1), USE.NAMES = FALSE)
  if (length(values) > length(shown))
    found <- c(found, sprintf("%d more values", length(values) - length(shown)))
  message <- sprintf(paste("The graph's `role` vertex attribute should use observed, exposure, outcome,",
    "adjusted, unobserved or selected (or NA), but %s; these nodes are drawn as observed."),
    .metro_quote_list(found, max = 6L, quote = "", distinct = FALSE))
  if (any(tolower(values) %in% valid)) message <- paste(message, "Role names are lower case.")
  if (any(tolower(values) == "latent")) message <- paste(message, "Use \"unobserved\" for latent variables.")
  warning(message, call. = FALSE)
  invisible(FALSE)
}

# Nodes that carry variables: every node except those drawn for bidirected arcs.
.metro_visible_nodes <- function(graph) setdiff(igraph::V(graph)$name, .metro_hidden_nodes(graph))

# Stop because `subject` (already quoted, such as "`exposure`") names nodes
# the graph does not have. A display label used in place of a node name is
# the commonest cause, so say which node it belongs to.
.metro_unknown_nodes <- function(subject, unknown, graph) {
  nm <- igraph::V(graph)$name
  message <- sprintf("%s names %s not in the graph: %s.", subject,
    if (length(unique(unknown)) == 1L) "a node" else "nodes", .metro_quote_list(unknown))
  label <- igraph::vertex_attr(graph, "label")
  if (!is.null(label)) {
    squash <- function(x) trimws(gsub("[[:space:]]+", " ", as.character(x)))
    hit <- match(squash(unknown), squash(label))
    used <- !is.na(hit) & !nm[hit] %in% .metro_hidden_nodes(graph)
    if (any(used)) message <- paste0(message, if (sum(used) == 1L)
      sprintf(" %s is the display label of node `%s`; use the node name.",
        .metro_quote_list(unknown[used]), nm[hit[used]]) else
      sprintf(" %s are display labels of nodes %s; use the node names.",
        .metro_quote_list(unknown[used]), .metro_quote_list(nm[hit[used]])))
  }
  stop(paste0(message, .metro_node_hint(.metro_visible_nodes(graph))), call. = FALSE)
}

.metro_analysis_subtitle <- function(layout) {
  if (!layout$analysis$adjustment_specified) return(NULL)
  adjusted <- layout$analysis$adjusted
  if (!length(adjusted)) return("Unadjusted: no covariates")
  # Respect display labels even if node labels are hidden in the diagram:
  # plot_metro_dag() stores supplied labels on the graph before this runs.
  shown <- .display_labels(layout$graph, TRUE, NULL)
  shown <- gsub("\n", " ", shown[match(adjusted, igraph::V(layout$graph)$name)], fixed = TRUE)
  blank <- !nzchar(trimws(shown))
  shown[blank] <- adjusted[blank]
  paste("Adjusted for:", paste(shown, collapse = ", "))
}

.metro_node_style <- function(role, background, style) {
  style <- .as_metro_style(style)
  fill <- rep(background, length(role)); colour <- rep(style$node_outline, length(role))
  shape <- rep(21L, length(role))
  shape[role == "adjusted"] <- 22L; fill[role == "adjusted"] <- style$adjusted_fill
  # Selected (conditioned-on) variables: a square with a second inner outline.
  shape[role == "selected"] <- 22L
  if (!is.null(style$selected_fill)) fill[role == "selected"] <- style$selected_fill
  marker <- rep("plain", length(role))
  marker[role == "selected"] <- "double"
  if (identical(style$outcome_marker, "ring")) marker[role == "outcome"] <- "ring"
  if (!is.null(style$unobserved_fill)) fill[role == "unobserved"] <- style$unobserved_fill
  fill[role == "exposure"] <- style$exposure_fill
  colour[role == "exposure"] <- if (is.null(style$exposure_colour)) style$exposure_fill else style$exposure_colour
  fill[role == "outcome"] <- style$outcome_fill
  colour[role == "outcome"] <- if (is.null(style$outcome_colour)) style$outcome_fill else style$outcome_colour
  data.frame(shape = shape, fill = fill, colour = colour, marker = marker,
    linetype = ifelse(role == "unobserved", "dashed", "solid"), stringsAsFactors = FALSE)
}

# Draw one station symbol of the given half-size at (x, y). Ring outcomes are
# hollow with a filled centre; selected variables have a second inner outline.
.metro_station_grob <- function(x, y, r, shape, marker, gp, background) {
  marker <- if (is.null(marker) || is.na(marker)) "plain" else marker
  if (shape == 22L) {
    outer <- grid::rectGrob(x, y, width = 2 * r, height = 2 * r, gp = gp)
    if (marker != "double") return(outer)
    inner_gp <- gp; inner_gp$fill <- NA
    return(grid::grobTree(outer, grid::rectGrob(x, y, width = 1.2 * r, height = 1.2 * r, gp = inner_gp)))
  }
  if (marker != "ring") return(.metro_circle_grob(x, y, r, gp))
  ring_gp <- gp; ring_gp$fill <- background
  dot_gp <- gp; dot_gp$col <- NA
  grid::grobTree(.metro_circle_grob(x, y, r, ring_gp),
    grid::circleGrob(x, y, r = 0.5 * r, gp = dot_gp))
}

# A circle of radius `r` at (x, y), all grid units. The Windows GDI device
# (windows(), and png(), bmp(), jpeg() or tiff() with type = "windows", their
# default on Windows) ignores the line type of a circle and draws it solid, so
# an unobserved station would look observed there. A dashed outline is drawn
# instead as a 72-sided polygon on the same circle, which every device dashes.
# Its vertices run clockwise from three o'clock, as Cairo and ragg draw a
# circle, so the dashes fall where they did on those devices. Solid and blank
# outlines stay circles. Clearances, label placement and diagnostics use the
# station radius, not this grob, so nothing moves.
.metro_circle_grob <- function(x, y, r, gp) {
  lty <- if (is.null(gp$lty)) "solid" else as.character(gp$lty)[1L]
  if (is.na(lty) || lty %in% c("solid", "1", "blank", "0")) return(grid::circleGrob(x, y, r = r, gp = gp))
  angle <- -2 * pi * (seq_len(72L) - 1L) / 72L
  grid::polygonGrob(x + r * cos(angle), y + r * sin(angle), gp = gp)
}

# A custom legend key preserves the square and dashed-circle station symbols,
# drawn as the stations are.
.draw_key_metro_node <- function(data, params, size) {
  gp <- grid::gpar(col = data$colour, fill = data$fill,
    lty = data$linetype, lwd = 0.8, linejoin = "round")
  station <- .metro_station_grob(grid::unit(0.5, "npc"), grid::unit(0.5, "npc"),
    grid::unit(1.15, "mm"), data$shape, data$marker, gp,
    if (is.null(data$background)) "white" else data$background)
  mark <- .metro_group_mark_grob(data$group_mark, grid::unit(0.5, "npc"),
    grid::unit(0.5, "npc"), 1.15, data$fill, data$fill,
    if (is.null(data$group_family)) "" else data$group_family)
  if (is.null(mark)) station else grid::grobTree(station, mark)
}

# `roles` and `specified` describe what the legend explains: the roles drawn
# and whether an adjustment set was specified. They default to those of
# `layout`. For a legend shared by several views, give the roles drawn in any
# view and whether any view specified an adjustment set.
.metro_role_legend <- function(p, layout, background, style, roles = NULL, specified = NULL) {
  style <- .as_metro_style(style)
  present <- if (is.null(roles)) unique(igraph::vertex_attr(layout$graph, "role")) else roles
  # A role has a key only when a station is drawn in that role. An adjustment
  # set changes the observed key's wording, to "Observed, not adjusted", but
  # does not add the key.
  if (is.null(specified)) specified <- isTRUE(layout$analysis$adjustment_specified)
  if (identical(present, "observed") && !specified && !length(layout$node_groups)) return(p)
  order <- c("exposure", "outcome", "adjusted", "selected", "observed", "unobserved")
  keys <- order[order %in% present]
  values <- .metro_node_style(keys, background, style)
  values$background <- background
  key_labels <- style$legend_labels[order]
  # "Observed, not adjusted" only makes sense once an adjustment set exists.
  if (specified || "adjusted" %in% present)
    key_labels[["observed"]] <- style$legend_labels[["observed_unadjusted"]]
  if (length(layout$node_groups)) {
    groups <- names(layout$node_groups)
    group_keys <- paste0(".metro_group_", seq_along(groups))
    group_values <- .metro_node_style(rep("observed", length(groups)), background, style)
    group_values$background <- background
    colours <- layout$node_group_colours[groups]
    if (style$preset != "monochrome" && length(colours)) {
      use <- !is.na(colours)
      group_values$colour[use] <- colours[use]
    }
    values$group_mark <- ""
    group_values$group_mark <- as.character(seq_along(groups))
    family <- ggplot2::calc_element("text", ggplot2::theme_get() + p$theme)$family
    values$group_family <- group_values$group_family <- if (is.null(family)) "" else family
    values <- rbind(values, group_values)
    keys <- c(keys, group_keys)
    key_labels <- c(key_labels, stats::setNames(groups, group_keys))
  }
  # NA coordinates create legend keys without adding stations or altering limits.
  key_data <- data.frame(x = NA_real_, y = NA_real_, role = keys)
  rows <- if (style$legend_position %in% c("left", "right")) length(keys) else
    ceiling(length(keys) / style$legend_columns)
  p <- p + ggplot2::geom_point(data = key_data,
    mapping = ggplot2::aes(x = x, y = y, shape = role), inherit.aes = FALSE,
    na.rm = TRUE, show.legend = NA, key_glyph = .draw_key_metro_node) +
    ggplot2::scale_shape_manual(name = NULL, values = stats::setNames(values$shape, keys),
      breaks = keys, labels = unname(key_labels[keys]), drop = FALSE) +
    # An explicit order keeps the role keys above the edge_legend keys:
    # without one, ggplot2 orders guides by a hash that varies by session.
    # High orders leave 1 to 96 to guides the user orders first.
    ggplot2::guides(shape = ggplot2::guide_legend(nrow = rows, order = 97L,
      byrow = TRUE, override.aes = as.list(values))) +
    ggplot2::theme(legend.position = style$legend_position,
      legend.text = ggplot2::element_text(size = 8),
      legend.key.width = grid::unit(4, "mm"), legend.key.height = grid::unit(4, "mm"),
      legend.margin = .metro_legend_margin(style$legend_position)) +
    .metro_legend_spacing()
  attr(p, "metrodag_legend_rows") <- rows
  attr(p, "metrodag_legend_values") <- values
  if (length(layout$node_groups)) attr(p, "metrodag_legend_labels") <- key_labels[keys]
  p
}

# Rank headers: one label per rank that has visible nodes, drawn as the axis
# along the causal direction (top for horizontal drawings, left for vertical
# ones), so ggplot2 places them between the panel and any title or legend.
.metro_rank_headers <- function(layout, rank_labels) {
  if (is.null(rank_labels)) return(NULL)
  visible <- !layout$nodes$node %in% .metro_hidden_nodes(layout$graph)
  layers <- sort(unique(layout$nodes$layer[visible]))
  if (!is.character(rank_labels) || anyNA(rank_labels) ||
      (is.null(names(rank_labels)) && length(rank_labels) != length(layers)))
    stop(sprintf("`rank_labels` must be a character vector with one label per rank (%d here).",
      length(layers)), call. = FALSE)
  if (!is.null(names(rank_labels))) rank_labels <- .metro_named_rank_labels(layout, rank_labels, layers, visible)
  horizontal <- layout$direction %in% c("left", "right")
  axis <- if (horizontal) "x" else "y"
  at <- split(layout$nodes[[axis]][visible], factor(layout$nodes$layer[visible], levels = layers))
  shared <- vapply(at, function(v) diff(range(v)) < 1e-6, logical(1))
  position <- vapply(at, function(v) v[[1L]], numeric(1))
  if (!all(shared) || (length(position) > 1L && !(all(diff(position) > 0) || all(diff(position) < 0))))
    stop(sprintf(paste0("`rank_labels` needs each rank at one position, but rank %d is drawn at several ",
      "(disconnected components are packed side by side). Supply `rank` so the components share their ranks."),
      which(!shared)[1L] - 1L), call. = FALSE)
  other <- if (horizontal) max(layout$nodes$y) else min(layout$nodes$x)
  data <- if (horizontal) data.frame(x = position, y = other, label = rank_labels) else
    data.frame(x = other, y = position, label = rank_labels)
  list(data = data, side = if (horizontal) "top" else "left", axis = axis,
    breaks = unname(position), labels = rank_labels)
}

# Match named headers to the drawn ranks. Names are the rank values the user
# supplied, as as.character() writes them, or the layout's layer numbers when
# no ranks were supplied. Returns the headers in rank order.
.metro_named_rank_labels <- function(layout, rank_labels, layers, visible) {
  keys <- as.character(layers)
  what <- "layer numbers"
  supplied <- layout$parameters$rank
  if (!is.null(supplied)) {
    if (is.null(names(supplied))) names(supplied) <- .metro_visible_nodes(layout$graph)
    first <- layout$nodes$node[visible][match(layers, layout$nodes$layer[visible])]
    values <- supplied[first]
    if (!anyNA(values) && !anyDuplicated(as.character(values))) {
      keys <- as.character(values)
      what <- "the `rank` values"
    }
  }
  given <- names(rank_labels)
  if (anyNA(given) || any(!nzchar(given)))
    stop("Name every `rank_labels` header by its rank, or name none to give them in rank order.", call. = FALSE)
  if (anyDuplicated(given))
    stop(sprintf("`rank_labels` names rank %s more than once.", .metro_quote_list(given[duplicated(given)])),
      call. = FALSE)
  unknown <- setdiff(given, keys)
  if (length(unknown))
    stop(sprintf("`rank_labels` names %s not drawn: %s. Names are %s: %s.%s",
      if (length(unknown) == 1L) "a rank" else "ranks", .metro_quote_list(unknown), what,
      paste(keys, collapse = ", "), if (is.null(supplied))
        " This layout keeps no `rank` values; use the layer numbers, or give the headers unnamed in rank order." else ""),
      call. = FALSE)
  missing <- setdiff(keys, given)
  if (length(missing))
    stop(sprintf("`rank_labels` has no header for %s %s.", if (length(missing) == 1L) "rank" else "ranks",
      .metro_quote_list(missing)), call. = FALSE)
  unname(rank_labels[keys])
}

# Theme for rank headers drawn as axis text, keeping `gap` mm of label room
# between the panel and the headers. Only the header axis gets text: the top
# x axis (drawn as the right y axis under coord_flip()) or the left y axis
# (drawn as the bottom x axis). Every other axis stays blank.
.metro_rank_header_theme <- function(side, family, size, colour, lineheight, gap = 5.5) {
  text <- function(margin, hjust, vjust) ggplot2::element_text(family = family,
    face = "bold", colour = colour, size = size, lineheight = lineheight,
    hjust = hjust, vjust = vjust, angle = 0, margin = margin, inherit.blank = FALSE)
  elements <- if (side == "top") list(
    axis.text.x.top = text(ggplot2::margin(b = gap, unit = "mm"), 0.5, 0),
    axis.text.y.right = text(ggplot2::margin(l = gap, unit = "mm"), 0, 0.5)) else list(
    axis.text.y.left = text(ggplot2::margin(r = gap, unit = "mm"), 1, 0.5),
    axis.text.x.bottom = text(ggplot2::margin(t = gap, unit = "mm"), 0.5, 1))
  do.call(ggplot2::theme, c(elements, list(axis.ticks.length = grid::unit(0, "pt"))))
}

# The theme element that draws the rank headers, given whether the plot is
# flipped, and the side of the panel it faces.
.metro_header_element <- function(side, flipped) {
  if (side == "top") { if (flipped) c("axis.text.y.right", "l") else c("axis.text.x.top", "b") } else {
    if (flipped) c("axis.text.x.bottom", "t") else c("axis.text.y.left", "r") }
}

.metro_hidden_nodes <- function(graph) {
  hidden <- igraph::vertex_attr(graph, ".metrodag_hidden")
  if (is.null(hidden)) character() else igraph::V(graph)$name[hidden %in% TRUE]
}

# Space between the panel and the legend, on the side facing the panel. With
# the legend box spacing it keeps the labels' 5 mm margin clear.
.metro_legend_margin <- function(position) {
  side <- c(bottom = "t", top = "b", left = "r", right = "l")[position]
  if (is.na(side)) side <- "t"
  do.call(ggplot2::margin, c(stats::setNames(list(3), side), list(unit = "mm")))
}

# Space between legend entries. `legend.key.spacing.x` exists from ggplot2
# 3.5.0; older versions keep their default spacing.
.metro_legend_spacing <- function() {
  if (utils::packageVersion("ggplot2") >= "3.5.0")
    ggplot2::theme(legend.key.spacing.x = grid::unit(4, "mm")) else ggplot2::theme()
}

.metro_valid_linetype <- function(x) {
  x %in% c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash") |
    (grepl("^[1-9A-Fa-f]+$", x) & nchar(x) %% 2L == 0L & nchar(x) <= 8L)
}

# Line keys explaining arrow conventions set with `edge_style`.
.metro_edge_legend <- function(p, edge_legend, style, line_width) {
  if (is.null(edge_legend) || !length(edge_legend)) return(p)
  if (!is.character(edge_legend) || anyNA(edge_legend) || is.null(names(edge_legend)) ||
      anyDuplicated(names(edge_legend)) || !all(.metro_valid_linetype(names(edge_legend))))
    stop("`edge_legend` must be a character vector of labels named by linetype, for example c(dotted = \"Assumed, uncertain\").", call. = FALSE)
  keys <- names(edge_legend)
  key_data <- data.frame(x = NA_real_, y = NA_real_, xend = NA_real_, yend = NA_real_,
    edge_type = factor(keys, levels = keys))
  colour <- if (style$preset == "metro") style$palette[[1L]] else style$edge_colour
  p <- p + ggplot2::geom_segment(data = key_data,
      mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linetype = edge_type),
      inherit.aes = FALSE, na.rm = TRUE, show.legend = NA, colour = colour,
      linewidth = line_width / 0.75) +
    ggplot2::scale_linetype_manual(name = NULL, values = stats::setNames(keys, keys),
      breaks = keys, labels = unname(edge_legend), drop = FALSE) +
    ggplot2::guides(linetype = ggplot2::guide_legend(nrow = ceiling(length(keys) / style$legend_columns),
      order = 98L, byrow = TRUE, override.aes = list(colour = colour))) +
    ggplot2::theme(legend.position = style$legend_position, legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8),
      legend.key.width = grid::unit(8, "mm")) +
    .metro_legend_spacing()
  attr(p, "metrodag_edge_legend_rows") <- ceiling(length(keys) / style$legend_columns)
  p
}
