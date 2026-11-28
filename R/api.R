#' Draw a DAG with publication defaults in one call
#'
#' Accepts an igraph (including tidygraph), DAGitty or ggdag DAG, a two-column
#' edge matrix, a data frame with `from` and `to` columns, or a previously
#' computed layout. [metro_graph()] and [metro_graph_from_adjacency()] construct
#' graphs from explicit node/edge tables and named adjacency matrices.
#' The original graph and
#' geometry are not modified by analysis annotations. Roles and estimand labels
#' are supplied by the analyst, never inferred adjustment recommendations.
#'
#' Appearance arguments in `...` are passed on to [plot_metro_dag()], as are
#' its text options `fontface`, `lineheight` and `colour`. Unknown names are
#' rejected. Titles are set with `estimand` or `ggplot2::labs()`.
#'
#' @param graph A directed igraph (including `tidygraph::tbl_graph`), DAGitty DAG,
#'   `ggdag::tidy_dagitty`, edge matrix, `from`/`to` data frame, or `metrodag_layout`.
#'   ggdag tidy objects require the optional ggdag package. Imported identities,
#'   directed edges, display labels and explicit analysis roles are retained;
#'   source coordinates are replaced by MetroDAG's layout. Inconsistent ggdag
#'   data/graph components and unsupported edge kinds are rejected; DAGitty
#'   selection nodes import as `selected`, and bidirected arcs need
#'   `bidirected = "latent"`. Reuse a layout to keep node positions and routes fixed
#'   across analysis views; omit layout-computation arguments in that case.
#' @param direction Causal direction; defaults to `"right"`.
#' @param labels TRUE, FALSE, or display labels named by node; see
#'   [plot_metro_dag()]. A named vector overrides only the nodes it names.
#'   Supplied labels are stored in the returned layout, so
#'   `metro_dag(attr(p, "metrodag_layout"), ...)` and [compare_metro_dags()]
#'   draw them without passing `labels` again. `labels = FALSE` hides them in
#'   this plot only.
#' @inheritParams plot_metro_dag
#' @param rank,layering,ordering,sweeps,align,bidirected Arguments passed to
#'   [layout_metro_dag()]. Supplied `rank` values are kept with the layout, so
#'   named `rank_labels` can refer to them.
#' @inheritDotParams plot_metro_dag -layout -labels -exposure -outcome -adjusted -unobserved -unadjusted -estimand -legend -node_groups -node_group_colours -selected
#' @return A ggplot. Its `metrodag_layout` attribute contains the geometry and
#'   the display labels; `metrodag_analysis` records the selected roles,
#'   adjustment set and target label.
#' @examples
#' g <- metrodag_examples("causal")
#' p <- metro_dag(g, exposure = "X", outcome = "Y", unobserved = "U",
#'   unadjusted = TRUE, estimand = "Unadjusted X-Y association")
#' p
#' fixed <- attr(p, "metrodag_layout")
#' metro_dag(fixed, adjusted = c("C", "S"), estimand = "Total effect of X on Y")
#' @export
metro_dag <- function(graph, direction = "right", labels = TRUE,
  exposure = NULL, outcome = NULL, adjusted = NULL, unobserved = NULL,
  rank = NULL, layering = "longest", ordering = "barycenter", sweeps = 8L,
  unadjusted = FALSE, estimand = NULL, legend = TRUE, align = NULL,
  node_groups = NULL, node_group_colours = NULL, selected = NULL,
  bidirected = "error", ...) {
  .metro_check_arguments(sys.call(), names(formals(sys.function())), "metro_dag()", parent.frame(),
    titles = FALSE)
  # Check appearance arguments here, so a mistake is reported as an argument
  # of metro_dag(), the function the user called.
  appearance <- list(...)
  if (length(appearance) && (is.null(names(appearance)) || any(!nzchar(names(appearance)))))
    stop("Arguments in `...` must be named, such as style = \"metro\". Use `labels` for display text.", call. = FALSE)
  accepted <- c(setdiff(names(formals(plot_metro_dag)), c(names(formals(sys.function())), "layout", "...")),
    "fontface", "lineheight", "colour", "color")
  unknown <- setdiff(names(appearance), accepted)
  if (length(unknown)) .metro_unknown_arguments(unknown, "metro_dag()")
  if (inherits(graph, "metrodag_layout")) {
    if (!missing(direction) || !missing(rank) || !missing(layering) ||
        !missing(ordering) || !missing(sweeps) || !missing(align) || !missing(bidirected))
      stop("When reusing a layout, omit direction, rank, layering, ordering, sweeps, align and bidirected.", call. = FALSE)
    layout <- graph
  } else {
    # layout_metro_dag() checks the graph again, and warns about its names once.
    graph <- .as_metro_input(graph, bidirected, warn_spaces = FALSE)
    layout <- layout_metro_dag(graph, direction, rank, layering, ordering, sweeps, align = align)
  }
  args <- c(list(layout = layout, labels = labels, exposure = exposure, outcome = outcome,
    adjusted = adjusted, unobserved = unobserved, selected = selected, unadjusted = unadjusted,
    legend = legend, node_groups = node_groups, node_group_colours = node_group_colours), appearance)
  if (!missing(estimand)) args["estimand"] <- list(estimand)
  do.call(plot_metro_dag, args)
}

#' Export a DAG at publication dimensions
#'
#' PDF and SVG are vector formats; PNG and TIFF default to 600 dpi. Height is
#' chosen from the displayed panel proportions if omitted, accounting for
#' fixed coordinate ratios, theme aspect ratios and `coord_flip()` in single
#' plots and comparisons.
#' Text and arrow clearance are
#' recalculated on the export device. Titles, subtitles and captions are wrapped
#' using its font metrics, including labels added with `ggplot2::labs()`.
#' An export that cannot fit every station and edge within the visible figure,
#' or keep routes clear of unrelated stations, stops with an informative error,
#' which for single plots and comparisons estimates the size needed (a
#' comparison whose panels are too narrow to draw at all asks for more width
#' or fewer columns). The estimate is a first step; [diagnose_metro_dag()]
#' gives a size checked by drawing in its `min_size`.
#' Coordinate and scale limits must retain the complete graph.
#' Fanned attachments that enter their own source or destination station are
#' also rejected, with an `edge_endpoint` issue in [diagnose_metro_dag()].
#' A route consisting entirely of zero-length steps reports `edge_zero_length`;
#' recompute its geometry rather than increasing the output size.
#' Existing output files are replaced
#' only after the new figure has rendered successfully.
#' Internally converting a layout for export preserves ggplot2's last plot,
#' including when export fails.
#' Always inspect dense diagrams at their
#' final printed size. `ggplot2::ggsave()` also works with MetroDAG plots.
#'
#' @param filename Output path with a PDF, SVG, EPS/PS, PNG, or TIFF extension.
#'   File and directory names, including percent signs, are treated literally.
#' @param plot A MetroDAG ggplot, `metrodag_layout`, result of
#'   [compare_metro_dags()], or a patchwork composition containing MetroDAG
#'   plots. Compositions need an explicit `height`; every MetroDAG panel in
#'   them is checked as strictly as a single plot.
#' @param width Figure width, default 180 mm (approximately two journal
#'   columns). The presets `"single"` (85 mm), `"1.5"` (120 mm) and
#'   `"double"` (180 mm) name common journal column widths; they are converted
#'   to `units`.
#' @param height Optional height. Automatic height is constrained to 45--220 mm.
#'   When automatic height was limited to 220 mm and the figure does not fit,
#'   but a taller one would, the error says so and gives the height it needs;
#'   pass that height, or for a comparison use more columns.
#' @param units `"mm"`, `"cm"`, or `"in"`.
#' @param dpi Raster resolution, default 600.
#' @param path Optional existing directory in which to save `filename`.
#' @param on_error `"stop"` (the default) refuses to write a figure that cannot
#'   fit every arrow and station; `"warn"` writes what fits, marked in one
#'   corner (for example "Incomplete: 2 arrows not drawn"), and warns.
#' @param ... Extra device arguments passed to [ggplot2::ggsave()]. `scale`
#'   multiplies the requested width and any explicit height. Automatic height
#'   uses the scaled width and retains the 45--220 mm physical height limits.
#'   TIFF files use LZW compression unless `compression` is supplied or the
#'   bitmap type is `"quartz"`, which does not support it. PDF and
#'   EPS use Cairo devices when available; a warning reports the fallback to
#'   the base devices, which do not embed fonts and draw only the families
#'   listed by [grDevices::pdfFonts()] and [grDevices::postscriptFonts()],
#'   such as `"sans"`, `"serif"` and `"mono"`: text in another family then
#'   stops the export with an error naming it. SVG uses svglite when it is
#'   installed; otherwise [grDevices::svg()] draws text as outlines, which
#'   cannot be edited or searched, and a warning says so.
#' @return Invisibly, the output filename.
#' @examples
#' p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y")
#' out <- tempfile(fileext = ".pdf")
#' save_metro_dag(out, p)
#' save_metro_dag("dag.svg", p, width = "single", path = tempdir())
#' unlink(c(out, file.path(tempdir(), "dag.svg")))
#' @export
save_metro_dag <- function(filename, plot, width = 180, height = NULL,
  units = "mm", dpi = 600, ..., path = NULL, on_error = "stop") {
  if (inherits(filename, c("ggplot", "metrodag_layout", "metrodag_comparison")))
    stop("`filename` comes first: save_metro_dag(filename, plot).", call. = FALSE)
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) || !nzchar(filename))
    stop("`filename` must be a single output path.", call. = FALSE)
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
      stop("`path` must be a single output directory.", call. = FALSE)
    filename <- file.path(path, filename)
  }
  if (!dir.exists(dirname(filename))) stop("The output directory does not exist.", call. = FALSE)
  if (dir.exists(filename)) stop("`filename` must name a file, not a directory.", call. = FALSE)
  units <- .assert_scalar_choice(units, c("mm", "cm", "in"), "units")
  width <- .metro_width(width, units)
  .assert_number(dpi, "dpi", strict = TRUE)
  is_layout <- inherits(plot, "metrodag_layout")
  # Force the caller's plot expression before taking the snapshot: explicit
  # plot construction still registers normally, but our internal conversion
  # of a layout must not change a later ggsave() without a plot argument.
  last <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(last), add = TRUE)
  if (is_layout) plot <- plot_metro_dag(plot)
  on_error <- .assert_scalar_choice(on_error, c("stop", "warn"), "on_error")
  if (!is.null(height)) .assert_number(height, "height", strict = TRUE)
  kind <- .metro_export_kind(plot, height)
  # An export must not silently omit arrows because the preview tolerated it.
  # Size advice converts device sizes back to the arguments given here.
  # A single plot or comparison carries a token for the content prepared at
  # this width (see .metro_prepared()); compositions are drawn by patchwork.
  export_scale <- list(...)$scale
  export <- list(scale = if (is.null(export_scale)) 1 else export_scale, automatic_height = is.null(height))
  prepared <- NULL
  if (kind != "composition") {
    prepared <- new.env(parent = emptyenv())
    prepared$explicit <- !is.null(height)
    export$prepared <- prepared
  }
  plot <- .metro_with_config(plot, list(on_error = on_error, export = export))
  # A composition whose last plot is a MetroDAG plot is drawn by
  # grid.draw.metrodag_plot(), which resets the text metrics before patchwork
  # builds it; any other composition, wrap_elements() ones included, is drawn
  # the same way.
  if (kind == "composition" && !inherits(plot, "metrodag_plot"))
    class(plot) <- append(class(plot), "metrodag_composition", after = 1L)
  factor <- switch(units, mm = 1, cm = 10, `in` = 25.4)
  scale <- list(...)$scale
  if (is.null(scale)) scale <- 1
  .assert_number(scale, "scale", strict = TRUE)
  ext <- tolower(tools::file_ext(filename))
  device <- .metro_export_device(ext, plot)
  if (is.null(device)) stop("Use a .pdf, .svg, .eps, .ps, .png or .tiff filename.", call. = FALSE)
  extra <- list(...)
  # Quartz bitmaps (the macOS default type) do not support compression.
  bitmap <- if (is.null(extra$type)) getOption("bitmapType") else extra$type
  if (ext %in% c("tiff", "tif") && is.null(extra$compression) &&
      !identical(bitmap, "quartz")) extra$compression <- "lzw"
  preflight <- tempfile(fileext = paste0(".", ext))
  directory <- normalizePath(dirname(filename), winslash = "/", mustWork = TRUE)
  temporary <- tempfile(pattern = ".metrodag-", tmpdir = directory, fileext = paste0(".", ext))
  # Graphics devices interpret % formats even in directory names. Render
  # privately in that case, then copy beside the destination before publishing.
  rendered <- if (grepl("%", directory, fixed = TRUE))
    tempfile(fileext = paste0(".", ext)) else temporary
  on.exit(unlink(c(preflight, temporary, rendered)), add = TRUE)
  # An automatic height is measured on a blank preflight page of the same
  # device. Compositions have a given height; patchwork draws and wraps them
  # itself. The preflight measures the plot as it is drawn, so that the final
  # render can use the content it prepared.
  limited <- NULL
  measure <- function(plot) {
    state <- new.env(parent = emptyenv())
    probe <- grid::gTree(plot = plot, state = state, cl = "metrodag_measure_grob")
    do.call(ggplot2::ggsave, c(list(preflight, plot = probe, width = width,
      height = 100 / factor / scale, units = units, dpi = dpi, device = device), extra))
    if (!is.null(state$error)) stop(state$error)
    prepared$width <- state$width
    prepared$content <- state$content
    # Comparison panels too narrow to draw are reported by the render below.
    natural <- if (is.finite(state$height)) state$height else 100
    if (natural > 220.5) limited <<- list(height = natural, scale = scale,
      factor = factor, columns = kind == "comparison" && isTRUE(state$rows > 1))
    height <<- max(45, min(220, natural)) / factor / scale
  }
  # diagnose_metro_dag() collects the problems itself and reads the limit.
  collecting <- .metro_collecting_state(plot)
  render <- function(plot) {
    if (kind != "composition" && is.null(height)) measure(plot)
    if (!is.null(collecting)) collecting$limited <- limited
    result <- do.call(ggplot2::ggsave, c(list(rendered, plot = plot,
      width = width, height = height, units = units, dpi = dpi, device = device), extra))
    # An explicit height has no preflight; an error preparing the figure is
    # raised as the preflight would raise it.
    if (!is.null(prepared$error)) stop(prepared$error)
    result
  }
  .metro_draw_collected(plot, render, function() limited)
  if (!identical(rendered, temporary) &&
      !suppressWarnings(file.copy(rendered, temporary, overwrite = FALSE)))
    stop("Could not stage the completed figure in the output directory.", call. = FALSE)
  .metro_replace_file(temporary, filename)
  invisible(filename)
}
