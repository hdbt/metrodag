#' Compare analyses of one causal graph
#'
#' Computes one layout and displays analysis views in aligned panels with equal
#' physical panel sizes and a single role legend. Every panel retains the same
#' nodes, edges, coordinate limits and scale. Panel titles and subtitles are
#' wrapped on the drawing device, and their heights are synchronized.
#'
#' @param graph Any input accepted by [metro_dag()], or a `metrodag_layout`
#'   computed in advance.
#' @param views A non-empty list of named argument lists. Each view can set
#'   `exposure`, `outcome`, `adjusted`, `unobserved`, `selected`, `unadjusted`,
#'   and `estimand`.
#'   List names become panel titles when a view has no supplied or inherited
#'   estimand label. An explicit NULL estimand clears the title.
#' @param ncol Number of panel columns; default two.
#' @param direction Layout direction for graph input. Omit when reusing a layout.
#' @param labels Display labels shared by every panel.
#' @param node_groups,node_group_colours Shared display categories and optional
#'   station-outline colours; see [plot_metro_dag()]. Groups stay constant across
#'   analysis views, with one shared numbering and legend. NULL inherits a
#'   reused layout's groups; `list()` clears them. Put these arguments here,
#'   rather than inside individual `views`.
#' @param ... Layout arguments for graph input (`rank`, `layering`,
#'   `ordering`, `sweeps`, `align`, `bidirected`; see [layout_metro_dag()]) and
#'   shared appearance arguments for [plot_metro_dag()], including `style`,
#'   `family`, `label_size`, `label_nudge` and `rank_labels`. Analysis
#'   arguments belong to the views. The panels never draw their own legends;
#'   use `legend` for the shared one.
#' @param legend Draw one shared legend below the panels, for the roles,
#'   node groups and any `edge_legend` entries, as [plot_metro_dag()] does for
#'   a single plot. FALSE leaves out the legend row, and automatic export
#'   heights no longer allow for it.
#' @return A printable `metrodag_comparison`, supported by [save_metro_dag()]
#'   and `ggplot2::ggsave()`. Its `plots` component contains the individual
#'   ggplots, and `layout` contains the shared geometry.
#'   Creating or printing a comparison registers it as ggplot2's last plot,
#'   so a subsequent `ggsave()` without `plot` saves every panel.
#'   Failed construction preserves the previous last plot. Printing accepts
#'   `newpage` and `vp`, as for single MetroDAG plots, to compose comparisons
#'   in [grid::viewport()] regions.
#' @examples
#' edges <- data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y"))
#' views <- list(
#'   Unadjusted = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
#'   Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C")
#' )
#' compare_metro_dags(edges, views)
#' @export
compare_metro_dags <- function(graph, views, ncol = 2L, direction = "right",
  labels = TRUE, node_groups = NULL, node_group_colours = NULL, ..., legend = TRUE) {
  last <- ggplot2::last_plot()
  completed <- FALSE
  on.exit(if (!completed) ggplot2::set_last_plot(last), add = TRUE)
  .assert_number(ncol, "ncol", minimum = 1, integer = TRUE)
  .assert_flag(legend, "legend")
  if (!is.list(views) || !length(views) || !all(vapply(views, is.list, logical(1))))
    stop("`views` must be a non-empty list of analysis argument lists.", call. = FALSE)
  allowed <- c("exposure", "outcome", "adjusted", "unobserved", "selected", "unadjusted", "estimand")
  valid_names <- function(x) !length(x) || (!is.null(names(x)) && !anyNA(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x)))
  if (any(vapply(views, function(x) "legend" %in% names(x), logical(1))))
    stop("Views cannot set `legend`: the comparison draws one shared legend. Use compare_metro_dags(legend = FALSE) to leave it out.", call. = FALSE)
  if (!all(vapply(views, function(x) valid_names(x) && all(names(x) %in% allowed), logical(1))))
    stop("Each view must use named analysis arguments: exposure, outcome, adjusted, unobserved, selected, unadjusted, estimand.", call. = FALSE)
  .metro_check_arguments(sys.call(), names(formals(sys.function())), "compare_metro_dags()", parent.frame())
  appearance <- list(...)
  if (!valid_names(appearance))
    stop("Use `views` for analysis arguments; `...` is for layout and shared appearance arguments, given by name.", call. = FALSE)
  accepted <- c(setdiff(.metro_layout_arguments, "direction"), names(formals(plot_metro_dag)),
    "fontface", "lineheight", "colour", "color")
  unknown <- setdiff(names(appearance), c(accepted, allowed, "layout"))
  if (length(unknown)) stop(sprintf(
    "Unknown argument%s to compare_metro_dags(): %s. `...` takes layout arguments (%s) and arguments of plot_metro_dag().",
    if (length(unknown) == 1L) "" else "s", paste0("`", unknown, "`", collapse = ", "),
    paste(setdiff(.metro_layout_arguments, "direction"), collapse = ", ")), call. = FALSE)
  if (!valid_names(appearance) || any(names(appearance) %in% c(allowed, "layout")))
    stop("Use `views` for analysis arguments; `...` is for layout and shared appearance arguments.", call. = FALSE)
  layout_args <- appearance[intersect(names(appearance), setdiff(.metro_layout_arguments, "direction"))]
  appearance <- appearance[setdiff(names(appearance), names(layout_args))]
  if (inherits(graph, "metrodag_layout")) {
    if (!missing(direction) || length(layout_args))
      stop("When reusing a layout, omit `direction` and layout arguments such as `rank`.", call. = FALSE)
    fixed <- graph
  } else fixed <- attr(do.call(metro_dag, c(list(graph = graph, direction = direction,
    labels = FALSE, legend = FALSE), layout_args)), "metrodag_layout")
  fixed <- .annotate_node_groups(fixed, node_groups, node_group_colours)
  # The shared layout keeps the display labels, as each panel's layout does.
  fixed <- .metro_store_labels(fixed, labels)
  plots <- lapply(seq_along(views), function(i) {
    p <- do.call(plot_metro_dag, c(list(layout = fixed, labels = labels, legend = FALSE), views[[i]], appearance))
    if (!"estimand" %in% names(views[[i]]) && is.null(p$labels$title) && !is.null(names(views)) &&
        !is.na(names(views)[i]) && nzchar(names(views)[i])) p <- p + ggplot2::labs(title = names(views)[i])
    p
  })
  names(plots) <- names(views)
  # The shared legend explains the roles drawn in any view, and keeps the
  # "Observed, not adjusted" wording if any view specified an adjustment set.
  roles <- unique(unlist(lapply(plots, function(p)
    igraph::vertex_attr(attr(p, "metrodag_layout")$graph, "role")), use.names = FALSE))
  specified <- any(vapply(plots, function(p)
    isTRUE(attr(p, "metrodag_layout")$analysis$adjustment_specified), logical(1)))
  background <- if (is.null(appearance$background)) "white" else appearance$background
  style <- .as_metro_style(if (is.null(appearance$style)) "publication" else appearance$style)
  legend_plot <- NULL
  if (legend) {
    legend_plot <- .metro_role_legend(plots[[1L]], attr(plots[[1L]], "metrodag_layout"),
      background, style, roles = roles, specified = specified)
    legend_plot <- .metro_edge_legend(legend_plot, appearance$edge_legend, style,
      if (is.null(appearance$line_width)) 0.45 else appearance$line_width)
  }
  result <- structure(list(plots = plots, layout = fixed, ncol = min(ncol, length(plots)),
    legend_plot = legend_plot, background = background), class = "metrodag_comparison")
  ggplot2::set_last_plot(result)
  completed <- TRUE
  result
}

# Build the comparison at `width` mm. The result's height is the automatic
# figure height, at which the panel with the tallest aspect fills its cell.
# Given the figure `height` when the comparison is the whole figure, each
# panel also learns its cell and the room the figure gives it, so that size
# problems can name a figure size. Panels narrower than 10 mm cannot be
# drawn: `narrow` is then TRUE and nothing else is built.
.build_metro_comparison <- function(x, width, height = NULL) {
  measured <- .metro_measure_comparison(x, width)
  if (measured$narrow) return(measured)
  .metro_assemble_comparison(measured, height)
}

# The first step of .build_metro_comparison(): wrap and build the panels and
# the shared legend at `width` mm, and measure the automatic height. It does
# not depend on the figure height, so an export measures it once.
.metro_measure_comparison <- function(x, width) {
  columns <- x$ncol; rows <- ceiling(length(x$plots) / columns); gap <- 4
  cell_width <- (width - gap * (columns - 1L)) / columns
  if (cell_width <= 10) return(list(narrow = TRUE, height = NA_real_, rows = rows))
  prepared <- lapply(x$plots, .metro_prepare_plot, width = cell_width)
  plots <- lapply(prepared, `[[`, "plot")
  tables <- lapply(prepared, `[[`, "table")
  aspects <- vapply(seq_along(plots), function(i)
    .metro_panel_aspect(plots[[i]], tables[[i]]), numeric(1))
  widths <- do.call(grid::unit.pmax, lapply(tables, function(g) g$widths))
  heights <- do.call(grid::unit.pmax, lapply(tables, function(g) g$heights))
  legend_prepared <- if (is.null(x$legend_plot)) NULL else .metro_prepare_plot(x$legend_plot, width)
  legend <- if (is.null(legend_prepared)) NULL else .metro_legend_grob(legend_prepared$table)
  legend_height <- if (is.null(legend)) 0 else grid::convertHeight(grid::grobHeight(legend), "mm", valueOnly = TRUE) + 2
  fixed_width <- grid::convertWidth(sum(widths), "mm", valueOnly = TRUE)
  fixed_height <- grid::convertHeight(sum(heights), "mm", valueOnly = TRUE)
  cell_height <- max(1, cell_width - fixed_width) * max(aspects) + fixed_height
  list(narrow = FALSE, plots = plots, tables = tables, widths = widths, heights = heights,
    legend = legend, legend_height = legend_height, fixed = c(fixed_width, fixed_height),
    columns = columns, rows = rows, gap = gap, cell_width = cell_width,
    height = rows * cell_height + gap * (rows - 1L) + legend_height,
    relative = any(vapply(c(prepared, list(legend_prepared)), function(p) isTRUE(p$relative), logical(1))))
}

# The second step: arrange the measured panels, given the figure `height`
# when the comparison is the whole figure.
.metro_assemble_comparison <- function(measured, height = NULL) {
  columns <- measured$columns; rows <- measured$rows; gap <- measured$gap
  legend <- measured$legend; legend_height <- measured$legend_height
  widths <- measured$widths; heights <- measured$heights; plots <- measured$plots
  size <- if (!is.null(height)) c(measured$cell_width, (height - legend_height - gap * (rows - 1L)) / rows)
  arrangement <- list(columns = columns, rows = rows, gap = gap, legend = legend_height)
  tables <- lapply(seq_along(measured$tables), function(i) {
    g <- measured$tables[[i]]; g$widths <- widths; g$heights <- heights
    .metro_configure_gtable(g, plots[[i]], size, measured$fixed, arrangement)
  })
  interleave <- function(n) {
    value <- grid::unit(rep(gap, 2L * n - 1L), "mm")
    value[seq.int(1L, length(value), by = 2L)] <- grid::unit(1, "null")
    value
  }
  row_heights <- interleave(rows)
  if (!is.null(legend)) row_heights <- grid::unit.c(row_heights, grid::unit(legend_height, "mm"))
  table <- gtable::gtable(widths = interleave(columns), heights = row_heights)
  for (i in seq_along(tables)) table <- gtable::gtable_add_grob(table, tables[[i]],
    t = 2L * ((i - 1L) %/% columns) + 1L, l = 2L * ((i - 1L) %% columns) + 1L,
    name = paste0("view-", i), clip = "off")
  if (!is.null(legend)) table <- gtable::gtable_add_grob(table, legend,
    t = length(row_heights), l = 1L, r = length(table$widths), name = "shared-legend", clip = "off")
  list(grob = table, plots = plots, tables = tables, narrow = FALSE, rows = rows,
    height = measured$height)
}

.metro_narrow_comparison <- "Comparison panels are too narrow; increase width or reduce `ncol`."

#' @export
#' @importFrom grid makeContent
makeContent.metrodag_comparison_grob <- function(x) {
  if (.metro_take_prebuilt(x)) return(x)
  state <- .metro_collecting_state(x$comparison)
  .metro_capture_draw_error(state, x, {
    .metro_reset_text_metrics()
    width <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    height <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    # Size advice applies only when the comparison is the whole figure, not a
    # cell of a composition or a user viewport.
    device <- grDevices::dev.size("in") * 25.4
    whole <- all(abs(c(width, height) - device) < 0.5)
    measured <- .metro_prepared(x$comparison, width,
      function() .metro_measure_comparison(x$comparison, width))
    # Nothing can be drawn in panels this narrow. While problems are being
    # collected, record it (it is never downgraded by on_error = "warn").
    if (isTRUE(measured$narrow) && is.null(state)) stop(.metro_narrow_comparison, call. = FALSE)
    if (is.null(measured)) grid::setChildren(x, grid::gList()) else if (measured$narrow) {
      .metro_record_fatal(state, "comparison_narrow", .metro_narrow_comparison,
        "Increase the export width or reduce `ncol`.")
      grid::setChildren(x, grid::gList())
    } else grid::setChildren(x, grid::gList(grid::rectGrob(gp = grid::gpar(
      col = NA, fill = x$comparison$background)), .metro_assemble_comparison(measured, if (whole) height)$grob))
  })
}

# Drawn like a single plot: problems are raised after drawing returns.
#' @export
#' @importFrom grid grid.draw
grid.draw.metrodag_comparison <- function(x, recording = TRUE)
  grid::grid.draw(as_metro_grob(x), recording = recording)

#' @export
grid.draw.metrodag_comparison_grob <- function(x, recording = TRUE) {
  if (.metro_drawn_by_grid(x$comparison)) return(NextMethod())
  .metro_draw_collected(x$comparison, function(p) {
    x$comparison <- p
    .metro_draw_whole(x, .metro_collecting_state(p), recording)
  }, rethrow = TRUE)
}
