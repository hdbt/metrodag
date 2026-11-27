#' Plot a MetroDAG layout
#'
#' The default publication style uses fine charcoal routes on white. Labels
#' are wrapped and placed in physical device coordinates, avoiding stations,
#' routes and other labels where possible. Arrows are trimmed to station rims.
#' Arrowheads are drawn above route casings to retain their visibility at
#' crossings, with a straight final shaft aligned to the head. Label placement
#' and collision reports include the final route strokes and filled heads.
#' One complete straight arrow in each incident fan retains its original
#' bearing where possible. Displaced incoming attachments blend tangentially
#' into a short radial shaft; the original attachment is retained if blending
#' would lose station or figure clearance. Stored grid routes remain unchanged.
#' Tied attachment bearings follow each route's first geometric divergence.
#' Displaced source ports can join a nearby diverging branch with a monotone
#' transition, subject to the same station and figure-clearance checks.
#' Default padding is smaller beside the causal direction than at its ends
#' when the drawing spans at least four transverse grid units. Thin chains and
#' singleton drawings retain the same padding on all sides.
#' Labels also try a farther position when nearby slots around a junction are
#' blocked. Long labels and crowded graphs can still require a larger device.
#' Attachments that enter a source or destination station are
#' reported at draw time rather than hidden behind it.
#' Use `style = "metro"` for colourful path-cover lines. Colours describe visual
#' paths, not causal effects or adjustment sets.
#' Path choices break ties by node name, and path colour indices follow the
#' source-node name order, keeping a fixed path cover stable across input order.
#' Geometrically tied attachments and route painting order use endpoint names, keeping
#' fanned ports and crossing casings stable across reordered equivalent inputs.
#'
#' @param layout A `metrodag_layout`.
#'   Each graph vertex must have exactly one node row with a unique, non-missing,
#'   non-empty name.
#'   Routes follow `segment` order within each `edge_id`, so reordering the edge
#'   table's rows preserves the drawing and leaves the stored table unchanged.
#'   Each `edge_id` must identify exactly one graph edge, with consistent `from`
#'   and `to` values across its rows. Missing routes, multiple IDs for an edge,
#'   or retargeted IDs are rejected; recompute the layout after changing edges.
#'   Node and route coordinates must be finite real numeric vectors. `segment`
#'   values must be finite real numbers, unique within each edge. Consecutive
#'   segments must share endpoints exactly, and each complete route must join
#'   the coordinates of its named source and destination. Incomplete routes are
#'   rejected rather than silently connected. Reordering rows and relabelling
#'   route IDs remain supported, including empty string IDs and distinct numeric
#'   IDs that have the same printed representation. Stored tables are unchanged.
#'   Redundant zero-length steps are ignored for attachment bearings and drawing.
#'   A route with no nonzero segment reports an `edge_zero_length` error at
#'   draw time; recompute the layout or give its endpoints distinct positions.
#' @param labels TRUE for vertex `label` attributes (falling back to names),
#'   FALSE to hide labels, or a character vector of display labels named by
#'   node, for example `c(X = "Exposure")`. A named vector overrides only the
#'   nodes it names; the others keep their vertex `label` attribute or name.
#'   The resulting labels are stored as the vertex `label` attribute of the
#'   returned `metrodag_layout`, so a reused layout keeps them, as it keeps
#'   roles and the estimand. FALSE draws no labels and leaves the stored
#'   labels unchanged; adjustment subtitles still use them.
#'   Adjustment subtitles use node identifiers for covariates with blank labels.
#'   Labels and roles are matched by node name when a reused layout's node rows
#'   or graph vertices have been reordered.
#' @param arrows Draw arrows at each edge's destination.
#' @param node_size Minimum station diameter in millimetres; high-degree stations
#'   grow to accommodate their arrow tips.
#' @param line_width Route width in millimetres.
#' @param label_size Text size in millimetres (3.2 is approximately 9 pt).
#' @param palette Colours for path-cover lines in metro style; `NULL` uses the
#'   style's palette (Okabe-Ito by default).
#' @param background Figure background colour.
#' @param style `"publication"`, `"metro"`, `"monochrome"`, or a
#'   [metro_style()] object for custom colours, arrowheads, station scaling and
#'   legend text.
#' @param label_wrap Approximate line width in characters, including long
#'   identifiers without spaces, which break after `_`, `.`, `-` and `/` and
#'   before a capital that starts a camelCase word. A number stays with the
#'   letters before it and lowercase letters after it: `Covid19Severity`
#'   breaks after `Covid19`, `Wave2ndDose` after `Wave2nd`. In labels with
#'   spaces, a capital inside a word is not a boundary. Labels also wrap to
#'   the available physical width on the drawing device. Automatic breaks are
#'   recalculated from the original text for each device; explicit newlines
#'   are retained.
#'   Labels that still collide can try two narrower physical reflows. A reflow
#'   is accepted only when conflicts decrease without increasing station,
#'   route, label or boundary obstructions. A bounded joint placement of label
#'   pairs also helps keep each label close to its own station.
#'   Automatic breaks keep a solitary number with its preceding word when
#'   the pair fits; intentional newlines still take priority.
#'   NULL disables wrapping.
#' @param family Font family, default `"sans"`.
#' @param label_nudge Optional named list of `c(dx, dy)` offsets in millimetres,
#'   for example `list(C = c(0, 2))`. Applied after automatic label placement;
#'   positive offsets move right/up. Nodes and routes are unchanged. Resulting
#'   label collisions are still reported at draw time.
#' @param exposure,outcome,adjusted,unobserved,selected Character vectors of node
#'   names. Numbers and factors are converted with [as.character()], so
#'   `exposure = 1` selects the node named `"1"`.
#'   `selected` marks variables the study conditions on through selection
#'   (for example hospital admission); they are drawn as squares with a
#'   double outline and are not adjustment covariates.
#'   NULL inherits each role from the graph; a supplied vector replaces that
#'   role, and `character(0)` clears it. Roles must be disjoint, including
#'   inherited roles. Adjusted variables use filled squares; observed variables
#'   not adjusted for use hollow circles. Unobserved variables are dashed.
#'   Inherited factor role attributes use their labels; missing roles are observed.
#'   When a role is inherited, every value of the graph's `role` vertex
#'   attribute must be `"observed"`, one of these role names in lower case, or
#'   NA; any other value, such as `"Exposure"` or `"latent"` (use
#'   `"unobserved"`), is an error.
#' @param node_groups Optional named list of non-empty character vectors of node
#'   names, for example `list("Selection factors" = "U1", "Other factors" = "U2")`.
#'   Categories are display annotations, independent of analysis roles and
#'   observability. Each node may belong to one category. Small numbered marks
#'   inside stations identify categories in every style, including monochrome.
#'   Numbers follow group names in UTF-8 byte order, independent of input order.
#'   NULL inherits a reused layout's groups; `list()` clears them. No causal
#'   role, selection assumption or adjustment recommendation is inferred from
#'   a category name. Stored node positions/routes and variable label text stay
#'   unchanged; labels and the combined legend reflow on the drawing device.
#' @param node_group_colours Optional named character vector of station-outline
#'   colours, named by group. Fills, shapes and unobserved dashes are retained;
#'   route colours are unchanged. Monochrome style ignores these colours while
#'   retaining numbered marks. NULL inherits matching overrides from a reused
#'   layout; `character(0)` clears them. Categories need no colour overrides.
#' @param unadjusted TRUE explicitly selects an empty adjustment set, clearing
#'   inherited adjusted roles. Cannot be combined with a non-empty `adjusted`.
#' @param estimand Optional descriptive title, such as `"Total effect of X on Y"`.
#'   This labels the analyst's target; it does not check identification. Changing
#'   adjustment sets alone need not change the estimand, and adjusting for a
#'   mediator does not automatically identify a direct effect.
#'   When omitted, inherits the layout's label if exposure and outcome are
#'   unchanged. Supply a string to replace it or NULL to clear it. Changing
#'   exposure or outcome clears an omitted label to avoid naming a stale target.
#'   This also applies when inherited roles in the layout's graph change.
#' @param legend Show analysis-role and numbered-category legend entries. Adjustment
#'   views also show the selected set, or an explicit unadjusted subtitle.
#' @param edge_style Optional per-arrow appearance: a named list keyed
#'   `"from->to"` whose elements are lists with any of `linetype`, `colour` (or
#'   `color`) and `width` (millimetres), for example
#'   `list("U->X" = list(linetype = "dotted"), "X->Y" = list(width = 0.8))`;
#'   or a data frame with `from` and `to` columns plus any of those columns.
#'   Spaces around the arrow in a key are ignored, so `"U -> X"` names the
#'   same arrow, unless the key names an arrow exactly as written, as for node
#'   names that begin or end with a space. A data frame names nodes exactly.
#'   Unknown edges are rejected. Styles describe assumptions about individual
#'   arrows and do not change layout, roles or causal checks.
#' @param edge_legend Optional legend entries for arrow conventions set with
#'   `edge_style`: a character vector of labels named by linetype, for example
#'   `c(dotted = "Assumed, uncertain")`.
#' @param rank_labels Optional character vector with one header per rank, such
#'   as survey waves, drawn above a horizontal drawing or left of a vertical
#'   one, between the drawing and any title or legend. An unnamed vector gives
#'   the headers in rank order. A named vector is matched to the ranks by name:
#'   names are the values supplied as `rank` to [metro_dag()],
#'   [layout_metro_dag()] or [compare_metro_dags()], as [as.character()] writes
#'   them, for example `c("0" = "Baseline", "1" = "Wave 1")`. For a layout
#'   computed without `rank`, names are the layer numbers in its `nodes$layer`
#'   column. Every drawn rank needs exactly one
#'   header. Ranks that hold only
#'   the hidden nodes of bidirected arcs get no header. Each rank must sit at
#'   one position, so disconnected components packed side by side need
#'   explicit `rank` values in [layout_metro_dag()]. Break long headers over
#'   lines with `"\n"`; [diagnose_metro_dag()] reports headers that overlap.
#' @param on_error What happens when the drawing device is too small to draw an
#'   arrow or keep stations apart: `"stop"` (the default) raises an error;
#'   `"warn"` draws what fits and warns, which suits small previews.
#'   [save_metro_dag()] stops unless it is given `on_error = "warn"` too.
#' @param ... Text options: `fontface`, `lineheight`, and `colour` (or `color`).
#' @return A ggplot object, with geometry in `metrodag_layout` and selected
#'   analysis annotations in `metrodag_analysis`. Standard ggplot titles and
#'   themes can be added. Annotations wrap to the drawing device width, and
#'   label placement respects the plot margins. A device too small to keep
#'   routes clear of unrelated stations produces an informative error.
#'   Identity and reverse position scales are supported. Nonlinear position
#'   scales are rejected; use direction to choose the layout orientation.
#'   Creating or printing a plot registers the completed MetroDAG plot as
#'   ggplot2's last plot, so `ggsave()` without `plot` retains its rendering.
#'   Coordinate and scale limits must retain all stations, routes and arrowheads;
#'   clipping away part of the graph produces an error at draw time.
#' @examples
#' layout <- layout_metro_dag(metrodag_examples("causal"))
#' plot_metro_dag(layout, exposure = "X", outcome = "Y", unobserved = "U")
#' plot_metro_dag(layout, style = "monochrome", labels = c(X = "Exposure"))
#' @export
plot_metro_dag <- function(layout, labels = TRUE, arrows = TRUE, node_size = 3,
  line_width = 0.45, label_size = 3.2, palette = NULL,
  background = "white", style = "publication", label_wrap = 24, family = "sans",
  exposure = NULL, outcome = NULL, adjusted = NULL, unobserved = NULL,
  unadjusted = FALSE, estimand = NULL, legend = TRUE, label_nudge = NULL,
  node_groups = NULL, node_group_colours = NULL, edge_style = NULL,
  selected = NULL, edge_legend = NULL, rank_labels = NULL, on_error = "stop", ...) {
  if (!inherits(layout, "metrodag_layout")) stop("`layout` must be a metrodag_layout.", call. = FALSE)
  .metro_check_arguments(sys.call(), names(formals(sys.function())), "plot_metro_dag()", parent.frame(),
    titles = FALSE)
  .assert_flag(legend, "legend")
  on_error <- .assert_scalar_choice(on_error, c("stop", "warn"), "on_error")
  previous <- layout$analysis
  inherit_estimand <- missing(estimand)
  if (inherit_estimand) estimand <- NULL
  layout <- .metro_store_labels(layout, labels)
  shown <- .display_labels(layout$graph, labels, NULL)
  layout <- .annotate_metro_layout(layout, exposure, outcome, adjusted,
    unobserved, unadjusted, estimand, selected)
  layout <- .annotate_node_groups(layout, node_groups, node_group_colours)
  if (inherit_estimand && setequal(layout$analysis$exposure, previous$exposure) &&
      setequal(layout$analysis$outcome, previous$outcome)) {
    estimand <- previous$estimand
    .assert_estimand(estimand)
    layout$analysis["estimand"] <- list(estimand)
  }
  for (key in c("node_size", "line_width", "label_size"))
    .assert_number(get(key), key, strict = TRUE)
  .assert_flag(arrows, "arrows")
  style <- .as_metro_style(style)
  if (is.null(palette)) palette <- style$palette
  if (!is.character(palette) || !length(palette) || anyNA(palette))
    stop("`palette` must be a non-empty character vector.", call. = FALSE)
  if (!is.character(background) || length(background) != 1L || is.na(background))
    stop("`background` must be a single colour.", call. = FALSE)
  tryCatch(grDevices::col2rgb(c(palette, background)), error = function(e)
    stop("`palette` and `background` must contain valid colours.", call. = FALSE))
  if (!is.character(family) || length(family) != 1L || is.na(family))
    stop("`family` must be a single font family.", call. = FALSE)
  if (!is.null(label_wrap)) .assert_number(label_wrap, "label_wrap", minimum = 1, integer = TRUE)
  text_options <- list(...)
  unknown <- setdiff(names(text_options), c("fontface", "lineheight", "colour", "color"))
  if (length(text_options) && (is.null(names(text_options)) || any(!nzchar(names(text_options)))))
    stop("Text options in `...` must be named: fontface, lineheight and colour. Use `labels` for display text.", call. = FALSE)
  if (length(unknown)) .metro_unknown_arguments(unknown, "plot_metro_dag()")
  if (!is.null(text_options$color)) text_options$colour <- text_options$color
  text_options$color <- NULL
  text_options <- utils::modifyList(list(fontface = "plain", lineheight = 1.05, colour = style$text_colour), text_options)
  nodes <- layout$nodes
  node_ids <- .assert_layout_nodes(layout)
  .assert_layout_edges(layout)
  .assert_layout_geometry(layout)
  # Keep the original text until the drawing device is known. An automatic
  # break made here would become a hard break during the device-size reflow.
  nodes$label <- shown[node_ids]
  # Nodes created for bidirected arcs carry no variable name of their own.
  nodes$label[nodes$node %in% .metro_hidden_nodes(layout$graph)] <- ""
  nudges <- .metro_label_nudges(label_nudge, nodes$node, layout$graph)
  role <- igraph::vertex_attr(layout$graph, "role")
  if (is.null(role)) role <- rep("observed", nrow(nodes))
  role[is.na(role) | !role %in% c("observed", "exposure", "outcome", "adjusted", "unobserved", "selected")] <- "observed"
  nodes$role <- role[node_ids]
  role <- nodes$role
  node_style <- .metro_node_style(role, background, style)
  nodes[, c("colour", "fill", "shape", "marker")] <- node_style[, c("colour", "fill", "shape", "marker")]
  if (style$preset == "metro") {
    neutral <- !role %in% c("exposure", "outcome")
    nodes$colour[neutral] <- rep_len(palette, max(nodes$path))[nodes$path[neutral]]
  }
  if (length(layout$node_groups)) {
    membership <- .metro_group_membership(layout, nodes$node)
    nodes$group_mark <- ifelse(is.na(membership), "", as.character(membership))
    if (style$preset != "monochrome" && length(layout$node_group_colours)) {
      colours <- layout$node_group_colours[names(layout$node_groups)[membership]]
      use <- !is.na(colours)
      nodes$colour[use] <- colours[use]
    }
  }
  e <- layout$edges
  e$colour <- if (style$preset == "metro") rep_len(palette, max(nodes$path))[
    nodes$path[match(e$from, nodes$node)]] else rep(style$edge_colour, nrow(e))
  e$linetype <- ifelse(nodes$role[match(e$from, nodes$node)] == "unobserved", "dashed", "solid")
  e$width <- rep(line_width, nrow(e))
  # Resolve `edge_style` keys against the drawn arrows once; the plot records
  # the resolved table.
  edge_table <- .metro_edge_style_table(edge_style, e)
  e <- .metro_apply_edge_style(e, edge_table)
  config <- list(node_size = node_size, line_width = line_width, label_size = label_size,
    family = family, background = background, arrows = arrows, text_options = text_options,
    label_wrap = label_wrap, label_nudge = nudges, style = style)
  headers <- .metro_rank_headers(layout, rank_labels)
  if (!is.null(headers)) config$rank_header <- list(side = headers$side, axis = headers$axis)
  config$on_error <- on_error
  # Label room when the panel is drawn outside the MetroDAG print path, for
  # example by ggplotGrob() or patchwork: the plot's own default margins.
  config$label_margin_default <- grid::unit(rep(5, 4L), "mm")
  # Include routing waypoints when training the panel, including excursions
  # beyond the original node extent. Label selection happens at draw time.
  limits <- rbind(nodes[, c("x", "y")], e[, c("x", "y")],
    data.frame(x = e$xend, y = e$yend))
  # Reserve label room at the ends of the causal direction, with less empty
  # space on its sides. coord_equal() still preserves the route angles.
  padding <- c(5, 5)
  side <- if (layout$direction %in% c("left", "right")) 2L else 1L
  # Keep the existing breathing room for thin chains and singleton drawings.
  # Their aspect ratio and room for large stations depend on that padding.
  if (diff(range(limits[[c("x", "y")[side]]])) >= 4) padding[side] <- 2
  p <- ggplot2::ggplot(limits, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_blank() +
    ggplot2::layer(geom = .GeomMetroDag, stat = "identity", position = "identity",
      data = nodes, mapping = ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
      params = list(nodes = nodes, edges = e, config = config,
        headers = if (is.null(headers)) NULL else headers$data)) +
    do.call(ggplot2::scale_x_continuous, c(list(expand = ggplot2::expansion(mult = 0, add = padding[1L])),
      .metro_header_scale(headers, "x"))) +
    do.call(ggplot2::scale_y_continuous, c(list(expand = ggplot2::expansion(mult = 0, add = padding[2L])),
      .metro_header_scale(headers, "y"))) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void(base_size = 9, base_family = family) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background, colour = NA),
      panel.background = ggplot2::element_rect(fill = background, colour = NA),
      plot.margin = ggplot2::margin(5, 5, 5, 5, unit = "mm"),
      plot.title = ggplot2::element_text(face = "bold", size = 11, margin = ggplot2::margin(b = 8)),
      plot.subtitle = ggplot2::element_text(colour = "#58636D", size = 9, margin = ggplot2::margin(b = 8)))
  if (!is.null(headers)) p <- p + .metro_rank_header_theme(headers$side, family,
    label_size * 72.27 / 25.4, text_options$colour, text_options$lineheight)
  subtitle <- .metro_analysis_subtitle(layout)
  title <- estimand
  p <- p + ggplot2::labs(title = title, subtitle = subtitle)
  if (legend) p <- .metro_role_legend(p, layout, background, style)
  if (legend) p <- .metro_edge_legend(p, edge_legend, style, line_width)
  lines <- function(x) if (is.null(x)) 0L else length(strsplit(x, "\n", fixed = TRUE)[[1L]])
  legend_rows <- sum(c(attr(p, "metrodag_legend_rows"), attr(p, "metrodag_edge_legend_rows")))
  if (!legend_rows) legend_rows <- NULL
  attr(p, "metrodag_annotation_height") <- 6 * lines(title) + 5 * lines(subtitle) +
    if (is.null(legend_rows) || !style$legend_position %in% c("bottom", "top")) 0 else 4 * legend_rows + 6
  attr(p, "metrodag_layout") <- layout
  attr(p, "metrodag_analysis") <- layout$analysis
  attr(p, "metrodag_style") <- style
  if (!is.null(edge_table)) attr(p, "metrodag_edge_style") <- edge_table
  if (length(layout$node_groups)) attr(p, "metrodag_node_groups") <- layout$node_groups
  attr(p, "metrodag_extent") <- c(diff(range(limits$x)), diff(range(limits$y))) + 2 * padding
  class(p) <- c("metrodag_plot", class(p))
  ggplot2::set_last_plot(p)
  p
}

# Layout settings cannot change when an existing layout is drawn. Name them,
# and any other unrecognised argument, instead of a generic message.
.metro_layout_arguments <- c("direction", "rank", "layering", "ordering", "sweeps", "align", "bidirected")

.metro_unknown_arguments <- function(unknown, where) {
  layout <- intersect(unknown, .metro_layout_arguments)
  if (length(layout)) stop(sprintf(paste0("%s draws a computed layout, so it cannot take %s. ",
    "Pass %s to metro_dag() with a graph, or compute the layout with layout_metro_dag()."),
    where, paste0("`", layout, "`", collapse = ", "), if (length(layout) == 1L) "it" else "them"),
    call. = FALSE)
  stop(sprintf("Unknown argument%s to %s: %s.%s%s",
    if (length(unknown) == 1L) "" else "s", where, paste0("`", unknown, "`", collapse = ", "),
    if (where %in% c("plot_metro_dag()", "metro_dag()"))
      " Text options in `...` are fontface, lineheight and colour." else "",
    .metro_labs_hint(unknown, where)),
    call. = FALSE)
}

# Titles are set with `estimand` or ggplot2::labs(), not as plot arguments.
# Every unknown-argument message from .metro_unknown_arguments() ends with
# this hint; a comparison's panel titles come from its views instead.
.metro_labs_hint <- function(unknown, where = NULL) {
  key <- intersect(c("title", "subtitle", "caption"), unknown)
  if (!length(key)) return("")
  if (identical(where, "compare_metro_dags()")) return(sprintf(paste(
    " Panel titles are the names of `views` or each view's `estimand`.",
    "To change one panel, add ggplot2::labs(%s = ...) to it in the result's `plots`."), key[[1L]]))
  sprintf(" Set the title with `estimand`, or add ggplot2::labs(%s = ...) to the plot.", key[[1L]])
}

# R matches abbreviated argument names; `rank =` would silently become
# `rank_labels =`. Require exact names for every argument.
# `envir` is the caller's frame: names passed on through its `...` (as by
# plot() and autoplot() methods) are checked too.
# No entry point takes `title`, `subtitle` or `caption`, so these are reported
# here, with the hint on how to set them, unless the caller checks every name
# in its `...` itself and so reports them with any other unknown names
# (`titles = FALSE`).
.metro_check_arguments <- function(call, formal_names, where, envir = NULL, titles = TRUE) {
  parts <- as.list(call)[-1L]
  typed <- names(parts)
  if (is.null(typed)) typed <- rep("", length(parts))
  if (!is.null(envir) && any(vapply(parts, identical, logical(1), quote(...)))) {
    dots <- tryCatch(names(eval(quote(substitute(list(...))), envir))[-1L], error = function(e) NULL)
    typed <- c(typed, dots)
  }
  typed <- typed[!is.na(typed) & nzchar(typed)]
  partial <- typed[!typed %in% formal_names]
  partial <- partial[vapply(partial, function(name) any(startsWith(formal_names, name)), logical(1))]
  if (titles) partial <- union(partial, setdiff(intersect(typed, c("title", "subtitle", "caption")), formal_names))
  if (length(partial)) .metro_unknown_arguments(partial, where)
  invisible(TRUE)
}

# Scale arguments that draw rank headers as axis labels on one axis and no
# breaks on the other.
.metro_header_scale <- function(headers, axis) {
  if (is.null(headers)) return(list())
  if (headers$axis != axis) return(list(breaks = NULL))
  list(breaks = headers$breaks, labels = headers$labels,
    position = if (axis == "x") "top" else "left")
}

.metro_label_nudges <- function(value, nodes, graph) {
  result <- matrix(0, length(nodes), 2L, dimnames = list(nodes, c("dx", "dy")))
  if (is.null(value) || (is.list(value) && !length(value))) return(result)
  if (!is.list(value) || is.null(names(value)))
    stop("`label_nudge` must be a named list of c(dx, dy) offsets in millimetres, for example list(C = c(0, 2)).", call. = FALSE)
  keys <- names(value)
  if (anyNA(keys) || any(!nzchar(keys)))
    stop("Every `label_nudge` offset needs a node name, for example list(C = c(0, 2)).", call. = FALSE)
  if (anyDuplicated(keys))
    stop(sprintf("`label_nudge` names %s more than once.", .metro_quote_list(keys[duplicated(keys)])), call. = FALSE)
  unknown <- setdiff(keys, nodes)
  if (length(unknown)) .metro_unknown_nodes("`label_nudge`", unknown, graph)
  bad <- keys[!vapply(value, function(z) is.numeric(z) && length(z) == 2L && all(is.finite(z)), logical(1))]
  if (length(bad))
    stop(sprintf("`label_nudge` for %s must be two finite numbers c(dx, dy) in millimetres.",
      .metro_quote_list(bad)), call. = FALSE)
  for (node in keys) result[node, ] <- value[[node]]
  result
}

# Display labels travel with the layout, as roles and the estimand do: a
# supplied vector is stored, resolved and unwrapped, as the graph's "label"
# attribute, so a reused layout keeps it. TRUE and FALSE leave the layout as
# it is.
.metro_store_labels <- function(layout, labels) {
  if (is.logical(labels) && length(labels) == 1L && !is.na(labels)) return(layout)
  layout$graph <- igraph::set_vertex_attr(layout$graph, "label",
    value = .display_labels(layout$graph, labels, NULL))
  layout
}

.display_labels <- function(graph, labels, wrap) {
  # A factor read from a node table keeps its names, as roles accept factors.
  if (is.factor(labels)) labels <- stats::setNames(as.character(labels), names(labels))
  nm <- igraph::V(graph)$name
  if (identical(labels, FALSE)) return(rep("", length(nm)))
  # A named vector overrides only the nodes it names; the others keep their
  # vertex `label` attribute, as with `labels = TRUE`.
  value <- igraph::vertex_attr(graph, "label")
  value <- if (is.null(value)) nm else as.character(value)
  value[is.na(value)] <- nm[is.na(value)]
  if (!identical(labels, TRUE)) {
    .metro_check_labels(labels, graph)
    value[match(names(labels), nm)] <- unname(labels)
  }
  if (!is.null(wrap)) value <- vapply(value, .metro_wrap_label,
    character(1), width = wrap, USE.NAMES = FALSE)
  value
}

# Say exactly what is wrong with a `labels` vector, naming the entries at fault.
.metro_check_labels <- function(labels, graph) {
  if (!is.character(labels)) {
    what <- if (is.null(labels)) "NULL" else
      if (identical(labels, NA)) "NA" else if (is.list(labels)) "a list" else
      if (is.atomic(labels) && is.null(attr(labels, "class"))) {
        type <- if (is.numeric(labels)) "numeric" else typeof(labels)
        sprintf("%s %s vector", if (grepl("^[aeiou]", type)) "an" else "a", type)
      } else sprintf("an object of class `%s`", class(labels)[[1L]])
    stop(sprintf("`labels` must be TRUE, FALSE or a character vector named by node, not %s.", what), call. = FALSE)
  }
  keys <- names(labels)
  if (is.null(keys))
    stop(paste0("`labels` must be named by node, for example c(X = \"Exposure\").",
      .metro_node_hint(.metro_visible_nodes(graph))), call. = FALSE)
  if (anyNA(keys) || any(!nzchar(keys)))
    stop(sprintf("`labels` has %s without a node name.",
      .metro_quote_list(labels[is.na(keys) | !nzchar(keys)])), call. = FALSE)
  if (anyNA(labels))
    stop(sprintf("`labels` has a missing value for %s; give a label or leave the node out.",
      .metro_quote_list(keys[is.na(labels)])), call. = FALSE)
  if (anyDuplicated(keys))
    stop(sprintf("`labels` names %s more than once.", .metro_quote_list(keys[duplicated(keys)])), call. = FALSE)
  unknown <- setdiff(keys, igraph::V(graph)$name)
  if (length(unknown)) .metro_unknown_nodes("`labels`", unknown, graph)
  invisible(TRUE)
}

# strwrap() preserves whole words. A word somewhat longer than the line stays
# whole, since a long word reads better than a broken one and the device-size
# pass (.metro_wrap_text) still splits anything that cannot fit at all.
# Identifiers break after separators such as "_" or "."; only tokens without
# separators that are more than twice the line length are cut by characters.
# A line without spaces, such as a DAGitty name, also breaks at camelCase
# boundaries unless `camel` is FALSE. Lines with spaces are prose, where a
# capital inside a word (iPhone, McDonald) is not a boundary.
.metro_wrap_label <- function(label, width, camel = TRUE) {
  if (!nzchar(label)) return("")
  paste(vapply(strsplit(label, "\n", fixed = TRUE)[[1L]], function(line) {
    wrapped <- strwrap(line, width = width)
    lines <- unlist(lapply(wrapped, .metro_split_identifier, width = width,
      camel = camel && !grepl("[[:space:]]", line)), use.names = FALSE)
    paste(.metro_numeric_orphans(lines, function(s) nchar(s) <= width), collapse = "\n")
  }, character(1)), collapse = "\n")
}

# Word starts inside an identifier: a capital after a lowercase letter that
# follows a letter or digit ("intraGame", "HbA1cBaseline") or after a number
# that follows a letter ("Covid19Severity"). A number stays with the letters
# before it and lowercase letters after it ("Wave2ndDose", "Dose10mg"), so a
# break never leaves an ordinal, a unit or a leading number ("3DPrinted") on
# a line of its own; a single leading lowercase letter ("iPhone", "eGFR")
# starts no part. A match consumes the capital, so a second pass finds a
# boundary right after a two-letter part ("abCdEf").
.metro_camel_parts <- function(token) {
  pattern <- "([[:alnum:]][[:lower:]]|[[:alpha:]][[:digit:]]+)([[:upper:]])"
  marked <- gsub(pattern, "\\1\001\\2", gsub(pattern, "\\1\001\\2", token))
  strsplit(marked, "\001", fixed = TRUE)[[1L]]
}

.metro_split_identifier <- function(piece, width, camel = FALSE) {
  if (!nzchar(piece) || nchar(piece) <= width || grepl(" ", piece, fixed = TRUE)) return(piece)
  cut <- function(token) {
    if (nchar(token) <= 2L * width) return(token)
    starts <- seq.int(1L, nchar(token), by = width)
    substring(token, starts, pmin(starts + width - 1L, nchar(token)))
  }
  parts <- regmatches(piece, gregexpr("[_./-]*[^_./-]+[_./-]*", piece))[[1L]]
  if (camel) parts <- unlist(lapply(parts, .metro_camel_parts), use.names = FALSE)
  if (length(parts) > 1L && identical(paste(parts, collapse = ""), piece)) {
    lines <- character(); current <- ""
    for (part in parts) {
      if (nchar(part) > 2L * width) {
        # A very long part is cut like a token without separators, starting
        # on the current line.
        room <- max(0L, width - nchar(current))
        current <- paste0(current, substr(part, 1L, room))
        rest <- substring(part, room + 1L)
        while (nzchar(rest)) {
          lines <- c(lines, current)
          current <- substr(rest, 1L, width); rest <- substring(rest, width + 1L)
        }
      } else if (nzchar(current) && nchar(current) + nchar(part) > width) {
        lines <- c(lines, current); current <- part
      } else current <- paste0(current, part)
    }
    return(c(lines[nzchar(lines)], current))
  }
  cut(piece)
}

# Rebalance only breaks introduced within one original line. Keep a solitary
# number with the preceding word when that pair fits, without changing tokens
# or crossing an intentional newline. This is text formatting, not node identity.
.metro_numeric_orphans <- function(lines, fits) {
  if (length(lines) < 2L) return(lines)
  for (i in seq.int(2L, length(lines))) {
    if (!grepl("^[+-]?[[:digit:]]+([.][[:digit:]]+)?%?$", lines[i]) ||
        !grepl(" ", lines[i - 1L], fixed = TRUE)) next
    word <- sub(".* ", "", lines[i - 1L])
    candidate <- paste(word, lines[i])
    if (!fits(candidate)) next
    lines[i - 1L] <- sub(" [^ ]+$", "", lines[i - 1L])
    lines[i] <- candidate
  }
  lines
}

.GeomMetroDag <- ggplot2::ggproto("GeomMetroDag", ggplot2::Geom,
  required_aes = c("x", "y"),
  # Geometry lives in parameters. Censored training coordinates must not
  # suppress this layer, even when limits exclude every station: its drawing
  # method still needs to report the lost geometry.
  handle_na = function(data, params) data,
  draw_panel = function(data, panel_params, coord, nodes, edges, config, headers = NULL) {
    if (!coord$is_linear()) stop("MetroDAG requires linear coordinates.", call. = FALSE)
    # Geom parameters bypass ggplot's scale transformation of layer data.
    # Transform both endpoints explicitly before the coordinate system runs.
    for (axis in c("x", "y")) {
      panel_axis <- if (inherits(coord, "CoordFlip")) if (axis == "x") "y" else "x" else axis
      scale <- panel_params[[panel_axis]]$scale
      transformation <- if (is.function(scale$get_transformation)) scale$get_transformation() else scale$trans
      if (is.null(transformation$name) || !transformation$name %in% c("identity", "reverse"))
        stop("MetroDAG supports identity and reverse position scales only; use direction to change orientation.", call. = FALSE)
      nodes[[axis]] <- scale$transform(nodes[[axis]])
      if (!is.null(headers)) headers[[axis]] <- scale$transform(headers[[axis]])
      edges[[axis]] <- scale$transform(edges[[axis]])
      edges[[paste0(axis, "end")]] <- scale$transform(edges[[paste0(axis, "end")]])
    }
    config$clip <- coord$clip
    config$flipped <- inherits(coord, "CoordFlip")
    grid::gTree(nodes = coord$transform(nodes, panel_params),
      edges = coord$transform(edges, panel_params), config = config,
      headers = if (is.null(headers)) NULL else coord$transform(headers, panel_params),
      cl = "metrodag_grob")
  },
  draw_key = ggplot2::draw_key_blank
)

#' @export
plot.metrodag_layout <- function(x, ...) print(plot_metro_dag(x, ...))

#' @export
#' @importFrom ggplot2 autoplot
autoplot.metrodag_layout <- function(object, ...) plot_metro_dag(object, ...)
