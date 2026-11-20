# Every `type` a drawing issue can have, as reported by diagnose_metro_dag()
# and attached to drawing conditions. Each is documented in the "Issue types"
# section of ?diagnose_metro_dag, which a test checks.
.metro_issue_types <- c(
  "station_overlap", "station_bounds", "edge_zero_length", "edge_short", "edge_station",
  "edge_endpoint", "edge_bounds", "comparison_narrow", "render_error", "arrowhead_overlap",
  "label_overlap", "label_route", "label_station", "label_bounds", "label_association",
  "rank_header_overlap", "render_warning")

.empty_metro_issues <- function() data.frame(
  panel = character(), type = character(), severity = character(),
  node = character(), other_node = character(), from = character(), to = character(),
  message = character(), suggestion = character(), stringsAsFactors = FALSE)

.metro_render_report <- function(config, type, severity, message,
  node = NA_character_, other_node = NA_character_, from = NA_character_, to = NA_character_) {
  panel <- if (is.null(config$diagnostic_panel)) "1" else config$diagnostic_panel
  dimension <- switch(if (is.null(config$limiting)) "" else config$limiting,
    width = "the export width", height = "the export height", "width/height")
  suggestion <- if (type == "rank_header_overlap")
    "Shorten rank_labels, break them over lines with \"\\n\", or increase width." else
    if (type == "arrowhead_overlap")
    paste("Enlarge the station with node_size or metro_style(station_scaling = \"degree\");",
      "arrivals from one side may need a much larger station.") else
    if (type == "label_association")
    sprintf("Increase %s, shorten labels, or move the label with label_nudge.", dimension) else
    if (startsWith(type, "label_"))
    "Increase width/height, shorten labels, or adjust label_nudge." else
    if (type %in% c("station_bounds", "edge_bounds"))
    sprintf("Expand coordinate/scale limits, increase %s, or reduce node_size.", dimension) else
    if (type == "edge_zero_length")
    "Recompute the layout or provide a nonzero route between distinct endpoint positions." else
    sprintf("Increase %s or reduce node_size.", dimension)
  issues <- data.frame(panel = panel, type = type, severity = severity,
    node = node, other_node = other_node, from = from, to = to,
    message = message, suggestion = suggestion, stringsAsFactors = FALSE)
  state <- config$diagnostic_state
  if (!is.null(state)) {
    state$issues[[length(state$issues) + 1L]] <- issues
    return(invisible(NULL))
  }
  condition <- structure(list(message = message, call = NULL, issues = issues),
    class = c(paste0("metrodag_render_", severity), severity, "condition"))
  if (severity == "error" && !identical(config$on_error, "warn")) stop(condition)
  # In warn mode, collect drawing errors and report them once per render.
  if (severity == "error" && !is.null(config$warn_collector)) {
    collector <- config$warn_collector
    collector$issues[[length(collector$issues) + 1L]] <- issues
    return(invisible(NULL))
  }
  warning(condition)
}

# Detect intersections of the actual circular/square station outlines.
.station_overlap_pairs <- function(nodes, radius, outline = 0) {
  pairs <- matrix(integer(), ncol = 2L)
  if (nrow(nodes) < 2L) return(pairs)
  radius <- rep_len(radius, nrow(nodes))
  outline <- rep_len(outline, nrow(nodes))
  for (i in seq_len(nrow(nodes) - 1L)) {
    j <- seq.int(i + 1L, nrow(nodes))
    dx <- abs(nodes$x[j] - nodes$x[i]); dy <- abs(nodes$y[j] - nodes$y[i])
    a <- nodes$shape[i] == 22L; b <- nodes$shape[j] == 22L
    distance <- sqrt(dx^2 + dy^2) - radius[i] - radius[j]
    both <- a & b
    distance[both] <- .metro_rectangle_distance(
      dx[both] - radius[i] - radius[j[both]],
      dy[both] - radius[i] - radius[j[both]])
    mixed <- xor(a, b)
    square_radius <- if (a) rep(radius[i], length(j)) else radius[j]
    circle_radius <- if (a) radius[j] else rep(radius[i], length(j))
    distance[mixed] <- sqrt(pmax(0, dx[mixed] - square_radius[mixed])^2 +
      pmax(0, dy[mixed] - square_radius[mixed])^2) - circle_radius[mixed]
    distance <- distance - outline[i] - outline[j]
    if (any(distance < -1e-8)) pairs <- rbind(pairs, cbind(i, j[distance < -1e-8]))
  }
  pairs
}

# Compare padded label rectangles with the visible station footprints.
# Rounded square joins are the filled square plus the outline's half-width,
# rather than a larger sharp square. Circles expand by the same half-width.
.metro_label_station_hits <- function(nodes, box, radius, outline = 0) {
  radius <- rep_len(radius, nrow(nodes))
  outline <- rep_len(outline, nrow(nodes))
  dx <- pmax(0, box[1L] - nodes$x, nodes$x - box[2L])
  dy <- pmax(0, box[3L] - nodes$y, nodes$y - box[4L])
  distance <- sqrt(dx^2 + dy^2) - radius
  square <- nodes$shape == 22L
  distance[square] <- .metro_rectangle_distance(
    dx[square] - radius[square], dy[square] - radius[square])
  which(distance - outline < -1e-8)
}

# Copy each MetroDAG panel of a plot, comparison or patchwork composition
# (including nested compositions and grobs from as_metro_grob()) with
# `update(params, panel)` applied to its layer parameters. Panels are numbered
# in drawing order. Layers are ggproto environments: the caller's plot and
# later ordinary exports are never modified.
.metro_map_panels <- function(plot, update) {
  counter <- 0L
  visit <- function(x) {
    if (inherits(x, "metrodag_comparison")) {
      x$plots <- lapply(x$plots, visit)
      return(x)
    }
    # A composition draws its patches first and its own (last) plot after.
    if (inherits(x, "patchwork")) x$patches$plots <- lapply(x$patches$plots, visit)
    if (inherits(x, "wrapped_patch")) {
      # wrap_elements() keeps plots and compositions as they are, and grobs
      # from as_metro_grob() hold their plot.
      attr(x, "grobs") <- lapply(attr(x, "grobs"), function(g) {
        if (inherits(g, "metrodag_plot_grob")) g$plot <- visit(g$plot)
        if (inherits(g, "metrodag_comparison_grob")) g$comparison <- visit(g$comparison)
        if (inherits(g, c("ggplot", "patchwork"))) g <- visit(g)
        g
      })
      return(x)
    }
    if (inherits(x, "ggplot") && .metro_has_layer(x)) {
      counter <<- counter + 1L
      panel <- as.character(counter)
      x$layers <- lapply(x$layers, function(layer) {
        if (!inherits(layer$geom, "GeomMetroDag")) return(layer)
        result <- ggplot2::ggproto(NULL, layer)
        result$geom_params <- update(layer$geom_params, panel)
        result
      })
    }
    x
  }
  visit(plot)
}

.metro_has_layer <- function(plot)
  any(vapply(plot$layers, function(l) inherits(l$geom, "GeomMetroDag"), logical(1)))

.metro_panel_count <- function(plot) {
  count <- 0L
  .metro_map_panels(plot, function(params, panel) { count <<- count + 1L; params })
  count
}

# The state collecting drawing problems for a plot, comparison or composition,
# if any of its MetroDAG panels has one (see .metro_live_state()).
.metro_collecting_state <- function(plot) {
  state <- NULL
  .metro_map_panels(plot, function(params, panel) {
    if (is.null(state)) state <<- params$config$diagnostic_state
    params
  })
  .metro_live_state(state)
}

# The state that drawing problems go to. .metro_draw_collected() closes its
# state when drawing returns, but the plot copy that holds it stays on the
# device's display list, and replaying that list (a resized window,
# dev.copy(), dev.print(), replayPlot(), knitr saving a chunk's figure) draws
# the copy again. At the size it was drawn, its problems have been reported
# already, so they go to a scratch state. At another size there is no state,
# and problems are raised or warned as for an ordinary draw.
.metro_live_state <- function(state) {
  if (is.null(state) || !isTRUE(state$closed)) return(state)
  drawn <- state$drawn_size
  if (!is.null(drawn) && grDevices::dev.cur() > 1L &&
      all(abs(grDevices::dev.size("in") * 25.4 - drawn) < 0.5))
    return(new.env(parent = emptyenv()))
  NULL
}

# Evaluate `code`, which draws `grob`. While problems are being collected,
# patchwork would swallow an error raised here and leave a partial figure,
# and an error raised here leaves the device locked; record it instead and
# draw nothing for this grob. Such errors are never downgraded by
# on_error = "warn".
.metro_capture_draw_error <- function(state, grob, code) {
  state <- .metro_live_state(state)
  if (is.null(state)) return(code)
  if (is.null(state$drawn_size)) state$drawn_size <- grDevices::dev.size("in") * 25.4
  tryCatch(code, error = function(e) {
    .metro_record_fatal(state, "render_error", conditionMessage(e),
      "Check the plot, its layers and the figure size.", e)
    grid::setChildren(grob, grid::gList())
  })
}

# The size (mm) of the current viewport.
.metro_viewport_mm <- function() c(
  grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE),
  grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE))

# Whether a plot or comparison drawn now is drawn as grid draws any grob,
# without collecting its problems afresh: while a drawing that has not
# returned collects them (the composition it is part of), or when it is
# drawn again where it was drawn, on a device of that size, as grid.grab(),
# grid.edit() and grid.refresh() redraw the page. Its problems were reported
# then. Drawn anywhere else, it reports them again.
.metro_drawn_by_grid <- function(plot) {
  state <- NULL
  open <- FALSE
  .metro_map_panels(plot, function(params, panel) {
    current <- params$config$diagnostic_state
    if (!is.null(current) && !isTRUE(current$closed)) open <<- TRUE
    if (is.null(state)) state <<- current
    params
  })
  if (open) return(TRUE)
  if (is.null(state) || is.null(state$drawn_viewport) || is.null(state$drawn_size) ||
      grDevices::dev.cur() < 2L) return(FALSE)
  all(abs(grDevices::dev.size("in") * 25.4 - state$drawn_size) < 0.5) &&
    all(abs(.metro_viewport_mm() - state$drawn_viewport) < 0.5)
}

# Draw `grob`, the whole of a plot or comparison whose problems are being
# collected in `state`. Its content is built first, outside the display list:
# if that fails (an error from an added layer, panels too narrow to draw),
# nothing is drawn or recorded, so knitr saves no blank image, and the
# problem is raised once drawing returns. Raising it here instead, while the
# device records, would leave the device locked and no longer recording.
.metro_draw_whole <- function(grob, state, recording) {
  if (!is.null(state)) state$drawn_viewport <- .metro_viewport_mm()
  # A grob with its own viewport or graphical parameters is built as it is
  # drawn, inside them.
  if (is.null(grob$vp) && is.null(grob$gp)) {
    grob <- grid::makeContent(grob)
    if (length(state$fatal)) return(invisible(NULL))
    # Draw the content as built instead of building it again; replays of the
    # display list build it afresh for their device.
    grob$prebuilt <- new.env(parent = emptyenv())
    grob$prebuilt$fresh <- TRUE
  }
  # Grid's own method for a gTree: dispatching again would come back here.
  utils::getS3method("grid.draw", "gTree")(grob, recording)
}

# Whether `grob` holds content just built by .metro_draw_whole() for this
# drawing; it is used once.
.metro_take_prebuilt <- function(grob) {
  token <- grob$prebuilt
  if (is.null(token) || !isTRUE(token$fresh)) return(FALSE)
  token$fresh <- FALSE
  TRUE
}

# Record a problem that stops a whole plot or comparison from being drawn,
# with the error `condition` behind it, if any.
.metro_record_fatal <- function(state, type, message, suggestion, condition = NULL) {
  issue <- .empty_metro_issues()
  issue[1L, ] <- list(NA_character_, type, "error", NA_character_, NA_character_,
    NA_character_, NA_character_, message, suggestion)
  state$issues[[length(state$issues) + 1L]] <- issue
  if (!length(state$fatal)) state$fatal_condition <- condition
  state$fatal <- c(state$fatal, message)
  invisible(NULL)
}

# Set render-config entries on a copy of a plot, comparison or composition.
.metro_with_config <- function(plot, values)
  .metro_map_panels(plot, function(params, panel) {
    params$config[names(values)] <- values
    params
  })

.metro_diagnostic_plot <- function(plot, state)
  .metro_map_panels(plot, function(params, panel) {
    params$config$diagnostic_state <- state
    params$config$diagnostic_panel <- panel
    params
  })

# Draw `plot` with `draw` while collecting each MetroDAG panel's drawing
# problems, then report them: an error if a panel with on_error = "stop"
# could not be drawn in full, otherwise one warning. patchwork intercepts
# errors raised while it draws, which would leave a partial composition
# without a message, and an error raised mid-drawing leaves the device locked.
# Panels already collecting for diagnose_metro_dag() are left to it.
# `limited` describes an automatic export height limited to 220 mm (see
# save_metro_dag()), or is a function giving it once `draw` has returned;
# errors then say what height the figure needs.
# With `rethrow`, an error that stopped drawing (from an added layer, say) is
# raised as it was, as an ordinary print() or ggsave() would raise it.
.metro_draw_collected <- function(plot, draw, limited = NULL, rethrow = FALSE) {
  state <- new.env(parent = emptyenv())
  state$issues <- list(); state$strict <- character(); state$count <- 0L
  # Replays of the display list must not report to this state any more.
  on.exit(state$closed <- TRUE, add = TRUE)
  plot <- .metro_map_panels(plot, function(params, panel) {
    state$count <- state$count + 1L
    current <- params$config$diagnostic_state
    if (!is.null(current) && !isTRUE(current$closed)) return(params)
    if (!identical(params$config$on_error, "warn")) state$strict <- c(state$strict, panel)
    params$config$diagnostic_state <- state
    params$config$diagnostic_panel <- panel
    params
  })
  result <- draw(plot)
  if (is.function(limited)) limited <- limited()
  if (length(state$fatal)) {
    if (rethrow && !is.null(state$fatal_condition)) stop(state$fatal_condition)
    stop(structure(list(message = state$fatal[1L], call = NULL,
      issues = do.call(rbind, state$issues)), class = c("metrodag_render_error", "error", "condition")))
  }
  if (!length(state$issues)) return(invisible(result))
  issues <- unique(do.call(rbind, state$issues))
  summary <- function(rows, what, note = "") {
    if (nrow(rows) == 1L && !nzchar(note)) return(rows$message)
    where <- if (state$count > 1L) sprintf(" in panel%s %s",
      if (length(unique(rows$panel)) == 1L) "" else "s",
      paste(unique(rows$panel), collapse = ", ")) else " at this size"
    sprintf("%d %s%s%s%s. First: %s", nrow(rows), what, if (nrow(rows) == 1L) "" else "s",
      where, note, rows$message[1L])
  }
  warned <- issues[issues$severity != "error", , drop = FALSE]
  if (nrow(warned)) warning(structure(list(message = summary(warned, "drawing warning"),
    call = NULL, issues = warned), class = c("metrodag_render_warning", "warning", "condition")))
  errors <- issues[issues$severity == "error", , drop = FALSE]
  if (!nrow(errors)) return(invisible(result))
  strict <- any(errors$panel %in% state$strict)
  message <- summary(errors, "drawing problem", if (strict) "" else
    "; the figure was drawn without the affected elements (on_error = \"warn\")")
  # The note on a limited automatic height gives the whole figure's need.
  note <- !is.null(limited) && .metro_height_helps(errors, state)
  if (!note && !is.null(state$min_size) && nrow(errors) > 1L) message <- paste0(message,
    " Altogether the figure needs about ", .metro_size_text(state$min_size), " at these sizes.")
  if (note) message <- paste(message, .metro_height_note(limited, state$min_size))
  condition <- structure(list(message = message, call = NULL, issues = errors),
    class = c(if (strict) c("metrodag_render_error", "error") else
      c("metrodag_render_warning", "warning"), "condition"))
  if (strict) stop(condition) else warning(condition)
  invisible(result)
}

# Whether a figure whose automatic height was limited to 220 mm would fit if
# it were taller: its panels estimated a taller figure, or every error is one
# that more room fixes. Collapsed routes, stations drawn at one point, render
# errors and coordinate limits that cut the graph are not fixed by any size.
.metro_height_helps <- function(issues, state) {
  errors <- issues$type[issues$severity == "error"]
  "height" %in% names(state$min_size) || (length(errors) > 0L && !isTRUE(state$coincident) &&
    all(errors %in% c("edge_short", "edge_station", "edge_endpoint", "station_overlap")))
}

# The physical height (mm) a figure needs when its automatic height was
# limited to 220 mm: the height its panels ask for, if they do, otherwise the
# height it would have had. A tall, narrow figure would have had a height
# that fills the whole width, far more than its stations need.
# `min_size` is in the millimetres of save_metro_dag()'s arguments.
.metro_limited_height <- function(limited, min_size = NULL) {
  needed <- if ("height" %in% names(min_size)) min_size[["height"]] * limited$scale else limited$height
  ceiling(needed / 5) * 5
}

# What to do about it, with the `height` argument (and `width`, when the
# figure must also be wider) in the caller's units.
.metro_height_note <- function(limited, min_size = NULL) {
  needed <- .metro_limited_height(limited, min_size)
  argument <- function(mm) {
    value <- mm / limited$scale / limited$factor
    format(if (limited$factor == 1) ceiling(value) else ceiling(value * 10) / 10)
  }
  more <- if (isTRUE(limited$columns)) " or use more columns" else ""
  if (!"width" %in% names(min_size))
    return(sprintf("Automatic height was limited to 220 mm; this figure needs about %d mm. Pass `height = %s`%s.",
      as.integer(needed), argument(needed), more))
  width <- min_size[["width"]] * limited$scale
  sprintf("Automatic height was limited to 220 mm; this figure needs about %s. Pass `width = %s, height = %s`%s.",
    .metro_size_text(c(width = width, height = needed)), argument(width), argument(needed), more)
}

# "120 mm width", "90 mm height" or "120 x 90 mm (width x height)".
.metro_size_text <- function(size) {
  if (length(size) == 2L) return(sprintf("%d x %d mm (width x height)",
    as.integer(size[["width"]]), as.integer(size[["height"]])))
  sprintf("%d mm %s", as.integer(size), names(size))
}

# What save_metro_dag() and diagnose_metro_dag() accept: a MetroDAG plot, a
# comparison, or a patchwork composition with at least one MetroDAG panel.
# Compositions have no automatic height.
.metro_export_kind <- function(plot, height) {
  if (inherits(plot, "metrodag_comparison")) return("comparison")
  if (inherits(plot, c("patchwork", "wrapped_patch"))) {
    if (!.metro_panel_count(plot))
      stop("This patchwork composition contains no MetroDAG plot; use ggplot2::ggsave().", call. = FALSE)
    if (is.null(height))
      stop("Supply `height` for a patchwork composition; automatic height applies to single MetroDAG plots and comparisons.", call. = FALSE)
    return("composition")
  }
  if (!inherits(plot, "ggplot") || is.null(attr(plot, "metrodag_extent")))
    stop("`plot` must be a MetroDAG plot, layout, comparison or a patchwork composition containing one.", call. = FALSE)
  "plot"
}

#' Diagnose a DAG at its intended publication size
#'
#' Renders privately on the same device used by [save_metro_dag()] and collects
#' station, edge and label conflicts, including geometry outside the visible
#' figure or clipped panel. Label checks use the final trimmed attachments,
#' stroke widths and filled arrowheads, including after label nudges.
#' Collision checks distinguish circular and square station footprints,
#' including their visible outline widths and rounded square outline corners.
#' Labels nearer another station than their own, nearer another station
#' than that station's own label, or more than 7 mm from their station and
#' cut off from it by another arrow are reported as `label_association`,
#' since readers may attach them to the wrong variable; overlapping rank
#' headers as `rank_header_overlap`; and arrowheads that still overlap at a
#' crowded station as `arrowhead_overlap`.
#' Redundant zero-length steps do not alter attachments. An entirely collapsed
#' route produces an `edge_zero_length` error with its source and destination.
#' No user figure is saved or replaced.
#' Reports identify the affected nodes and edges, with panel numbers for
#' comparisons. This is a rendering check, not a causal identification check.
#' Font metrics depend on the device; use the intended output format.
#' Internal layout conversion preserves ggplot2's last plot, including when
#' rendering reports errors. A plot explicitly created in the `plot` argument
#' registers normally and remains the last plot for a later `ggsave()`.
#'
#' @section Issue types:
#' Each row of `issues` has one of these `type` values, given here with its
#' severity. Errors stop [save_metro_dag()] and set `ok` to FALSE; warnings
#' do neither. Sizes refer to the export width and height; the `suggestion`
#' column names the dimension that limits the figure when it is known.
#'
#' * `station_overlap` (error): two stations, `node` and `other_node`,
#'   overlap. Increase the export size or reduce `node_size`.
#' * `station_bounds` (error): the station `node` extends beyond the visible
#'   figure. Expand coordinate or scale limits, increase the export size, or
#'   reduce `node_size`.
#' * `edge_zero_length` (error): every step of the route `from` -> `to` has
#'   zero length. Recompute the layout or give the route a nonzero length; a
#'   larger figure does not help.
#' * `edge_short` (error): the arrow `from` -> `to` is too short to draw with
#'   station and arrowhead clearance. Increase the export size or reduce
#'   `node_size`.
#' * `edge_station` (error): the arrow `from` -> `to` passes through an
#'   unrelated station, `node`. Increase the export size or reduce
#'   `node_size`.
#' * `edge_endpoint` (error): after fanning, the arrow `from` -> `to` enters
#'   its own source or destination station, `node`. Increase the export size
#'   or reduce `node_size`.
#' * `edge_bounds` (error): the arrow `from` -> `to` extends beyond the
#'   visible figure. Expand coordinate or scale limits, or increase the export
#'   size.
#' * `comparison_narrow` (error): comparison panels would be 10 mm wide or
#'   less, so none is drawn. Increase the width or reduce `ncol` in
#'   [compare_metro_dags()].
#' * `render_error` (error): drawing stopped with an error, for example from
#'   an added layer or scale; `message` gives it. Check the plot, its layers
#'   and scales, the device options and the figure size.
#' * `arrowhead_overlap` (warning): the arrowheads of `from` -> `node` and
#'   `other_node` -> `node` overlap, outlines included. Arrivals are spread
#'   up to one arrowhead width apart around a station as far as their routes
#'   reach without an S-shaped bend; this reports a station with more
#'   arrivals from one side than its rim can part, or with wide routes.
#'   Enlarge the station with `node_size` or
#'   `metro_style(station_scaling = "degree")`. Several arrivals sharing one
#'   direction may need a much larger station: a six-parent star clears only
#'   from `node_size = 8`. Another export size can clear the warning or
#'   cause it, and thinner routes or shorter arrowheads help only
#'   occasionally.
#' * `label_overlap` (warning): the labels of `node` and `other_node`
#'   overlap. Increase the export size, shorten labels, or move one with
#'   `label_nudge`.
#' * `label_route` (warning): the label of `node` crosses the arrow `from` ->
#'   `to`. Increase the export size, shorten labels, or move it with
#'   `label_nudge`.
#' * `label_station` (warning): the label of `node` overlaps the station
#'   `other_node`, which may be its own. Increase the export size, shorten
#'   labels, or move it with `label_nudge`.
#' * `label_bounds` (warning): the label of `node` extends beyond the figure
#'   margin. Increase the export size, shorten labels, or move it with
#'   `label_nudge`.
#' * `label_association` (warning): the label of `node` is nearer the station
#'   `other_node` than its own, or nearer than that station's own label; or
#'   the label is more than 7 mm from its station and the arrow `from` ->
#'   `to`, which neither starts nor ends at `node`, runs between them and
#'   cuts it off (`other_node` is then missing). A label just beyond a route
#'   one lane from its station, such as a mediator's bypass, still reads as
#'   its own and is not reported. Increase the export size, shorten labels,
#'   or move it with `label_nudge`.
#' * `rank_header_overlap` (warning): neighbouring rank headers overlap.
#'   Shorten `rank_labels`, break them over lines with `"\n"`, or increase
#'   the width.
#' * `render_warning` (warning): the device or ggplot2 warned while drawing;
#'   `message` gives the warning. Check the plot scales, device options and
#'   dimensions.
#'
#' @inheritParams save_metro_dag
#' @param height Optional height. Automatic height is constrained to 45--220
#'   mm, as in [save_metro_dag()]. When automatic height was limited to 220 mm
#'   and a taller figure would fit, `min_size` includes the height it needs.
#' @param device Output format: "pdf", "svg", "eps", "png", or "tiff".
#' @param ... Extra device arguments, as for [save_metro_dag()].
#' @return A metrodag_diagnostics list. The ok field is FALSE if any error was
#'   found; warnings do not set it to FALSE. The issues data frame contains
#'   panel, type, severity, node, other_node, from, to, message and suggestion;
#'   the section "Issue types" lists every type. dimensions records the actual
#'   device width and height in millimetres.
#'   panels records physical panel dimensions. routes holds the drawn arrow
#'   shafts as points in millimetres (panel, from, to, point, x, y), after
#'   attachment fanning and up to each arrow's tip. device is the output
#'   format the figure was rendered in, as given by `device`. For a single
#'   plot or comparison that does not fit, min_size gives an export width
#'   and/or height in millimetres (named `width`, `height`) at which it does,
#'   keeping node_size and line widths and allowing for automatic height up
#'   to 220 mm. When automatic height was limited to 220 mm and a taller
#'   figure would fit, min_size includes the height it needs. The estimate is
#'   checked by rendering at that size and grown until the figure fits; it is
#'   NULL when no size up to that search fits or no estimate is available, as
#'   for patchwork compositions. An empty issues table means
#'   no conflicts were detected; it does not prove an optimal layout.
#' @examples
#' p <- metro_dag(metrodag_examples("chain"))
#' report <- diagnose_metro_dag(p, width = 85)
#' report$issues
#' @export
diagnose_metro_dag <- function(plot, width = 180, height = NULL, units = "mm",
  device = "pdf", dpi = 600, ...) {
  units <- .assert_scalar_choice(units, c("mm", "cm", "in"), "units")
  device <- .assert_scalar_choice(device, c("pdf", "svg", "eps", "png", "tiff"), "device")
  width <- .metro_width(width, units)
  .assert_number(dpi, "dpi", strict = TRUE)
  if (!is.null(height)) .assert_number(height, "height", strict = TRUE)
  if (any(names(list(...)) %in% c("path", "filename")))
    stop("Diagnostics do not accept an output path or filename.", call. = FALSE)
  # Evaluate the caller's expression before the snapshot, as for exports.
  # Only our internal conversion and rendering should restore the last plot.
  is_layout <- inherits(plot, "metrodag_layout")
  last <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(last), add = TRUE)
  if (is_layout) plot <- plot_metro_dag(plot)
  .metro_export_kind(plot, height)
  file <- tempfile(fileext = paste0(".", device))
  on.exit(unlink(file), add = TRUE)
  run <- function(width, height, units) {
    state <- new.env(parent = emptyenv())
    state$issues <- list(); state$panels <- list(); state$routes <- list()
    state$dimensions <- c(width = NA_real_, height = NA_real_)
    diagnostic_plot <- .metro_diagnostic_plot(plot, state)
    add_condition <- function(e, severity) {
      issue <- .empty_metro_issues()
      issue[1L, ] <- list(NA_character_, paste0("render_", severity), severity,
        NA_character_, NA_character_, NA_character_, NA_character_,
        conditionMessage(e), "Check the plot scales, device options and dimensions.")
      state$issues[[length(state$issues) + 1L]] <- issue
    }
    tryCatch(withCallingHandlers(save_metro_dag(file, diagnostic_plot,
      width = width, height = height, units = units, dpi = dpi, ...),
      warning = function(w) { add_condition(w, "warning"); invokeRestart("muffleWarning") }),
      error = function(e) add_condition(e, "error"))
    state$ok <- !any(vapply(state$issues, function(i) any(i$severity == "error"), logical(1)))
    state
  }
  state <- run(width, height, units)
  # A figure much too small hides problems that appear only as it grows, so
  # check the estimate at its own size and grow it until the figure fits.
  min_size <- if (state$ok) NULL else state$min_size
  # An automatic height limited to 220 mm needs an explicit height, when a
  # taller figure would fit.
  if (!state$ok && !is.null(state$limited) &&
      .metro_height_helps(do.call(rbind, state$issues), state))
    min_size <- .metro_larger_size(min_size, c(height =
      ceiling(.metro_limited_height(state$limited, min_size) / state$limited$scale / 5) * 5))
  factor <- switch(units, mm = 1, cm = 10, `in` = 25.4)
  for (attempt in seq_len(4L)) {
    if (is.null(min_size)) break
    trial <- run(if ("width" %in% names(min_size)) min_size[["width"]] else width * factor,
      if ("height" %in% names(min_size)) min_size[["height"]] else if (is.null(height)) NULL else height * factor,
      "mm")
    if (trial$ok) break
    if (is.null(trial$min_size)) { min_size <- NULL; break }
    grown <- min_size
    for (key in names(trial$min_size)) grown[[key]] <- max(c(grown[key], trial$min_size[[key]]), na.rm = TRUE)
    if (identical(grown, min_size)) { min_size <- NULL; break }
    min_size <- grown[intersect(c("width", "height"), names(grown))]
    if (attempt == 4L) min_size <- NULL
  }
  issues <- if (length(state$issues)) unique(do.call(rbind, state$issues)) else .empty_metro_issues()
  rownames(issues) <- NULL
  panels <- if (length(state$panels)) unique(do.call(rbind, state$panels)) else
    data.frame(panel = character(), width = numeric(), height = numeric())
  routes <- if (length(state$routes)) do.call(rbind, unname(state$routes)) else
    data.frame(panel = character(), from = character(), to = character(),
      point = integer(), x = numeric(), y = numeric())
  rownames(routes) <- NULL
  ok <- !any(issues$severity == "error")
  structure(list(ok = ok, issues = issues,
    dimensions = state$dimensions, panels = panels, routes = routes, device = device,
    min_size = if (ok) NULL else min_size),
    class = "metrodag_diagnostics")
}

#' @export
print.metrodag_diagnostics <- function(x, ...) {
  cat(sprintf("<metrodag_diagnostics> %s; %.1f x %.1f mm (%s)\n",
    if (!x$ok) "errors" else if (nrow(x$issues)) "warnings" else "clear",
    x$dimensions["width"], x$dimensions["height"], x$device))
  if (length(x$min_size)) cat("Needs about ", .metro_size_text(x$min_size),
    " at these node and line sizes.\n", sep = "")
  if (nrow(x$issues)) print(x$issues[, c("panel", "type", "severity", "node", "other_node", "from", "to")],
    row.names = FALSE)
  invisible(x)
}
