.metro_cairo_available <- function() {
  # Compile-time support can be TRUE when runtime libraries are absent (macOS
  # without XQuartz). A failed capability probe should select the PDF fallback.
  tryCatch(isTRUE(capabilities("cairo")) &&
      isTRUE(nzchar(grDevices::grSoftVersion()[["cairo"]])),
    warning = function(w) FALSE, error = function(e) FALSE)
}

# svglite writes text as <text> elements; grDevices::svg() draws every glyph
# as an outline, which cannot be edited or searched.
.metro_svglite_available <- function() requireNamespace("svglite", quietly = TRUE)

.metro_legend_grob <- function(table) {
  ids <- which(grepl("^guide-box", table$layout$name))
  for (id in ids) if (!inherits(table$grobs[[id]], "zeroGrob")) return(table$grobs[[id]])
  NULL
}

# `split_words = FALSE` breaks only at spaces: a word wider than `width` stays
# whole on its own line.
.metro_wrap_text <- function(text, width, gp, split_words = TRUE) {
  if (is.null(text) || !is.character(text) || length(text) != 1L) return(text)
  measure <- function(s) grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = gp)), "mm", valueOnly = TRUE)
  wrap_line <- function(line) {
    words <- strsplit(trimws(line), "[[:space:]]+")[[1L]]
    if (!length(words)) return("")
    lines <- character(); current <- ""
    for (word in words) {
      candidate <- if (nzchar(current)) paste(current, word) else word
      if (measure(candidate) <= width) { current <- candidate; next }
      if (nzchar(current)) { lines <- c(lines, current); current <- "" }
      if (!split_words) { current <- word; next }
      # Long identifiers still need to fit, even without spaces.
      for (letter in strsplit(word, "", fixed = TRUE)[[1L]]) {
        candidate <- paste0(current, letter)
        if (nzchar(current) && measure(candidate) > width) {
          lines <- c(lines, current); current <- letter
        } else current <- candidate
      }
    }
    paste(.metro_numeric_orphans(c(lines, current), function(s) measure(s) <= width), collapse = "\n")
  }
  paste(vapply(strsplit(text, "\n", fixed = TRUE)[[1L]], wrap_line, character(1)), collapse = "\n")
}

# Labels may extend into the plot margin beside the panel. A title, subtitle
# or caption placed directly against the panel would sit in that margin, so
# give it at least the same distance from the panel.
.metro_annotation_room <- function(plot, theme, table) {
  margin <- grid::convertHeight(ggplot2::calc_element("plot.margin", theme), "mm", valueOnly = TRUE)
  layer <- Find(function(l) inherits(l$geom, "GeomMetroDag"), plot$layers)
  headers <- if (is.null(layer)) NULL else layer$geom_params$config$rank_header
  flipped <- inherits(plot$coordinates, "CoordFlip")
  # The sides on which a legend is actually drawn. ggplot2 before 3.5 names
  # every legend box "guide-box"; its side is then the theme's position.
  boxes <- grepl("^guide-box", table$layout$name) &
    !vapply(table$grobs, inherits, logical(1), "zeroGrob")
  legend <- sub("^guide-box-?", "", table$layout$name[boxes])
  position <- ggplot2::calc_element("legend.position", theme)
  legend[!nzchar(legend)] <- if (is.character(position)) position else "inside"
  header_top <- !is.null(headers) && headers$side == "top" && !flipped
  # Headers keep at least the label room from the panel. The element styled
  # for the other orientation (kept so that coord_flip() shows the headers)
  # is blanked, so a scale added for the other axis draws no numbers.
  if (!is.null(headers)) {
    unused <- .metro_header_element(headers$side, !flipped)[1L]
    plot <- plot + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_blank()), unused))
    element <- .metro_header_element(headers$side, flipped)
    drawn <- ggplot2::calc_element(element[1L], theme)
    room <- margin[[c(b = 1L, l = 2L, t = 3L, r = 4L)[[element[2L]]]]] + 0.5
    if (inherits(drawn, "element_text") && !is.null(drawn$margin)) {
      current <- grid::convertHeight(drawn$margin, "mm", valueOnly = TRUE)
      index <- c(t = 1L, r = 2L, b = 3L, l = 4L)[[element[2L]]]
      if (current[index] < room) {
        current[index] <- room
        plot <- plot + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_text(
          margin = ggplot2::margin(current[1L], current[2L], current[3L], current[4L], unit = "mm"))),
          element[1L]))
      }
    }
  }
  present <- function(key) !is.null(plot$labels[[key]]) &&
    !inherits(ggplot2::calc_element(paste0("plot.", key), theme), "element_blank")
  widen <- function(plot, key, side, room) {
    element <- ggplot2::calc_element(paste0("plot.", key), theme)
    current <- grid::convertHeight(element$margin, "mm", valueOnly = TRUE)
    index <- c(t = 1L, b = 3L)[[side]]
    if (current[index] >= room) return(plot)
    current[index] <- room
    plot + do.call(ggplot2::theme, stats::setNames(list(ggplot2::element_text(
      margin = ggplot2::margin(current[1L], current[2L], current[3L], current[4L], unit = "mm"))),
      paste0("plot.", key)))
  }
  # A legend keeps the label room from the panel too: box spacing plus the
  # legend margin on the side facing the panel.
  spacing <- ggplot2::calc_element("legend.box.spacing", theme)
  spacing <- if (is.null(spacing)) 0 else grid::convertHeight(spacing, "mm", valueOnly = TRUE)
  # plot.margin side next to the legend, and the legend.margin side facing
  # the panel (both in top, right, bottom, left order).
  facing <- c(bottom = 3L, top = 1L, left = 4L, right = 2L)
  for (side in intersect(legend, names(facing))) {
    element <- ggplot2::calc_element("legend.margin", theme)
    current <- if (is.null(element)) rep(0, 4L) else grid::convertHeight(element, "mm", valueOnly = TRUE)
    index <- c(bottom = 1L, top = 3L, left = 2L, right = 4L)[[side]]
    needed <- margin[[facing[[side]]]] + 0.5 - spacing
    if (current[index] < needed) {
      current[index] <- needed
      plot <- plot + ggplot2::theme(legend.margin = ggplot2::margin(current[1L], current[2L],
        current[3L], current[4L], unit = "mm"))
    }
  }
  if (!header_top && !"top" %in% legend) {
    key <- if (present("subtitle")) "subtitle" else if (present("title")) "title" else NULL
    if (!is.null(key)) plot <- widen(plot, key, "b", margin[1L] + 0.5)
  }
  if (!"bottom" %in% legend && present("caption")) plot <- widen(plot, "caption", "t", margin[3L] + 0.5)
  plot
}

# Whether a unit converts to a length that depends on the size of the
# viewport it is converted in (npc, snpc, native and the like), rather than
# being absolute or relative to the font only.
.metro_relative_unit <- function(value) {
  if (!grid::is.unit(value)) return(FALSE)
  types <- unlist(grid::unitType(value, recurse = TRUE), use.names = FALSE)
  !all(types %in% c("mm", "cm", "inches", "points", "picas", "bigpts", "dida",
    "cicero", "scaledpts", "lines", "char", "null"))
}

# Whether any unit of a complete theme, or any element's margin, is such a
# unit. Preparing a plot converts theme units to fixed lengths on the page it
# is prepared on: .metro_annotation_room() converts margins and spacing, and
# ggplot2 converts legend key sizes, legend spacing and text margins while it
# builds the gtable. Elements without a margin of their own inherit one from
# an element that is scanned too.
.metro_theme_relative <- function(theme) {
  for (name in names(theme)) {
    element <- theme[[name]]
    if (.metro_relative_unit(element)) return(TRUE)
    margin <- tryCatch(element$margin, error = function(e) NULL)
    if (.metro_relative_unit(margin)) return(TRUE)
  }
  FALSE
}

# Rank headers are the breaks of one position scale. A scale added after
# metro_dag() (for example scale_x_reverse()) replaces them with numbers;
# put the headers back on a copy of that scale.
.metro_restore_headers <- function(plot) {
  layer <- Find(function(l) inherits(l$geom, "GeomMetroDag"), plot$layers)
  headers <- if (is.null(layer)) NULL else layer$geom_params$headers
  side <- if (is.null(layer)) NULL else layer$geom_params$config$rank_header
  if (is.null(headers) || is.null(side) || !nrow(headers)) return(plot)
  axis <- if (is.null(side$axis)) (if (side$side == "top") "x" else "y") else side$axis
  index <- which(vapply(plot$scales$scales, function(s) axis %in% s$aesthetics, logical(1)))
  if (!length(index)) return(plot)
  scale <- plot$scales$scales[[index[1L]]]
  if (is.numeric(scale$breaks) && isTRUE(all.equal(unname(scale$breaks), headers[[axis]])) &&
      identical(unname(scale$labels), headers$label)) return(plot)
  plot$scales <- plot$scales$clone()
  scale <- plot$scales$scales[[index[1L]]]
  scale$breaks <- headers[[axis]]
  scale$labels <- headers$label
  scale$position <- if (axis == "x") "top" else "left"
  plot
}

# Called on the actual output device: font metrics are device-specific.
.metro_wrap_annotations <- function(plot, width) .metro_prepare_plot(plot, width, keep_table = FALSE)$plot

# R's graphics engine remembers the metrics of the letter "M", from which it
# computes the height of every string, for the last device and font that
# asked for them, and recognises that device by its address. A device opened
# at the address of one closed earlier can then be given the closed device's
# metrics: after a 300 dpi PNG export, a PDF export in the same session could
# measure its subtitle several times too tall and so get a taller page, and
# text is placed with the same metrics. Measuring "M" at two different sizes
# replaces the remembered entry with one from the current device, whatever it
# was. Call this before measuring or drawing on a device, with a page open:
# grid starts a page on a device that has none.
# The font is the device's default, plain, whatever the enclosing viewport
# sets: measuring in a caller's family could load that font into the file or
# warn that the device does not know it.
# Returns the two heights measured (mm), invisibly.
.metro_reset_text_metrics <- function() {
  heights <- vapply(c(6, 7), function(size) grid::convertHeight(grid::grobHeight(grid::textGrob("M",
    gp = grid::gpar(fontsize = size, fontfamily = "", fontface = "plain"))), "mm", valueOnly = TRUE),
    numeric(1))
  invisible(heights)
}

# The plot with its annotations wrapped for `width` mm, as
# .metro_wrap_annotations() returns it, and its gtable. Fitting the legend
# builds that gtable already, which is then kept rather than built again.
.metro_prepare_plot <- function(plot, width, keep_table = TRUE) {
  last <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(last), add = TRUE)
  theme <- ggplot2::theme_get() + plot$theme
  table <- ggplot2::ggplotGrob(plot)
  fixed <- grid::convertWidth(sum(table$widths), "mm", valueOnly = TRUE)
  available <- max(1, width - fixed - 1)
  for (key in c("title", "subtitle", "caption")) {
    element <- ggplot2::calc_element(paste0("plot.", key), theme)
    if (inherits(element, "element_blank")) next
    gp <- grid::gpar(fontsize = element$size, fontfamily = element$family,
      fontface = element$face, lineheight = element$lineheight)
    value <- .metro_wrap_text(plot$labels[[key]], available, gp)
    plot <- plot + do.call(ggplot2::labs, stats::setNames(list(value), key))
  }
  plot <- .metro_restore_headers(plot)
  plot <- .metro_annotation_room(plot, theme, table)
  values <- attr(plot, "metrodag_legend_values")
  labels <- attr(plot, "metrodag_legend_labels")
  if (!is.null(labels)) {
    element <- ggplot2::calc_element("legend.text", theme)
    gp <- grid::gpar(fontsize = element$size, fontfamily = element$family,
      fontface = element$face, lineheight = element$lineheight)
    wrapped <- vapply(labels, .metro_wrap_text, character(1),
      width = max(1, available - 8), gp = gp)
    # Scale objects are mutable ggproto objects: never reflow the caller's copy.
    plot$scales <- plot$scales$clone()
    scale <- plot$scales$get_scales("shape")
    scale$labels <- unname(wrapped)
  }
  table <- NULL
  if (!is.null(values)) for (rows in seq_len(nrow(values))) {
    built <- ggplot2::ggplotGrob(plot)
    legend <- .metro_legend_grob(built)
    if (is.null(legend) || grid::convertWidth(grid::grobWidth(legend), "mm", valueOnly = TRUE) <= available) {
      table <- built
      break
    }
    plot <- plot + ggplot2::guides(shape = ggplot2::guide_legend(nrow = rows, order = 97L,
      byrow = TRUE, override.aes = as.list(values)))
  }
  if (keep_table && is.null(table)) table <- ggplot2::ggplotGrob(plot)
  list(plot = plot, table = if (keep_table) table, relative = .metro_theme_relative(theme))
}

# The gtable's null-unit ratio already includes coord_fixed(), transformed
# ranges and theme(aspect.ratio). Free coordinates use the routed extent as
# their preferred aspect, accounting for a displayed axis swap.
.metro_panel_aspect <- function(plot, table) {
  panel <- which(table$layout$name == "panel")
  if (length(panel) == 1L && any(table$respect)) {
    position <- table$layout[panel, ]
    width <- table$widths[position$l]; height <- table$heights[position$t]
    if (grid::unitType(width) == "null" && grid::unitType(height) == "null" &&
        as.numeric(width) > 0) return(as.numeric(height) / as.numeric(width))
  }
  extent <- attr(plot, "metrodag_extent")
  if (inherits(plot$coordinates, "CoordFlip")) extent <- rev(extent)
  extent[2L] / extent[1L]
}

.metro_plot_height <- function(plot, width, table = ggplot2::ggplotGrob(plot)) {
  fixed_width <- grid::convertWidth(sum(table$widths), "mm", valueOnly = TRUE)
  fixed_height <- grid::convertHeight(sum(table$heights), "mm", valueOnly = TRUE)
  max(1, width - fixed_width) * .metro_panel_aspect(plot, table) + fixed_height
}

# Carry the final theme into the device-space layer, including themes added
# after metro_dag(). A bare ggplotGrob() uses the conservative panel bounds.
# Given the figure size (mm) when the plot is the whole figure, also record
# which dimension limits the panel and the room available to it, so size
# errors can name the dimension that helps.
.metro_plot_gtable <- function(plot, size = NULL)
  .metro_configure_gtable(ggplot2::ggplotGrob(plot), plot, size)

# The configuration step of .metro_plot_gtable() for a gtable built already.
# A comparison passes each panel its cell as `size`, the fixed widths and
# heights shared by its panels as `fixed` (mm), and its grid of panels as
# `comparison` (columns, rows, gap, legend height), so that size advice can
# be given for the whole figure.
.metro_configure_gtable <- function(table, plot, size = NULL, fixed = NULL, comparison = NULL) {
  margin <- ggplot2::calc_element("plot.margin", ggplot2::theme_get() + plot$theme)
  limiting <- available <- NULL
  if (!is.null(size)) {
    if (is.null(fixed)) fixed <- c(grid::convertWidth(sum(table$widths), "mm", valueOnly = TRUE),
      grid::convertHeight(sum(table$heights), "mm", valueOnly = TRUE))
    available <- size - fixed
    # Automatic export heights fit the panel exactly; a tie means the width
    # limits it, since a wider export also grows the height.
    limiting <- if (available[1L] * .metro_panel_aspect(plot, table) <=
      available[2L] + 0.5) "width" else "height"
  }
  # The text that actually draws the rank headers, for the overlap check.
  final <- ggplot2::theme_get() + plot$theme
  header_text <- function(config) {
    if (is.null(config$rank_header)) return(NULL)
    element <- ggplot2::calc_element(.metro_header_element(config$rank_header$side,
      inherits(plot$coordinates, "CoordFlip"))[1L], final)
    if (!inherits(element, "element_text")) return(list(blank = TRUE))
    list(size = element$size, face = element$face, family = element$family,
      lineheight = element$lineheight, angle = if (is.null(element$angle)) 0 else element$angle)
  }
  set_margin <- function(grob) {
    if (inherits(grob, "metrodag_grob")) {
      grob$config$label_margin <- margin
      grob$config$limiting <- limiting
      grob$config$figure_size <- size
      grob$config$available <- available
      grob$config$comparison <- comparison
      grob$config$header_text <- header_text(grob$config)
    }
    if (inherits(grob, "gTree"))
      grob$children <- do.call(grid::gList, lapply(grob$children, set_margin))
    grob
  }
  table$grobs <- lapply(table$grobs, set_margin)
  table
}

# Wrap at draw time, not when the ggplot is constructed, so resizing, print(),
# viewports and ordinary ggsave() all use the actual device's font metrics.
#' @export
#' @importFrom grid makeContent
makeContent.metrodag_plot_grob <- function(x) {
  if (.metro_take_prebuilt(x)) return(x)
  .metro_capture_draw_error(.metro_collecting_state(x$plot), x, {
    .metro_reset_text_metrics()
    width <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    height <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    prepared <- .metro_prepared(x$plot, width, function() .metro_prepare_plot(x$plot, width))
    # Size advice applies only when this plot is the whole figure, not a
    # cell of a composition or a user viewport.
    device <- grDevices::dev.size("in") * 25.4
    whole <- all(abs(c(width, height) - device) < 0.5)
    grid::setChildren(x, if (is.null(prepared)) grid::gList() else grid::gList(
      .metro_configure_gtable(prepared$table, prepared$plot, if (whole) c(width, height))))
  })
}

# The export token of a plot or comparison drawn by save_metro_dag(): an
# environment shared by its MetroDAG panels, or NULL.
.metro_export_token <- function(plot) {
  token <- NULL
  .metro_map_panels(plot, function(params, panel) {
    if (is.null(token)) token <<- params$config$export$prepared
    params
  })
  token
}

# The content of a plot or comparison at `width` mm, from `prepare()`. When
# save_metro_dag() has measured the figure at this width on the same device
# type, its final render uses the content prepared then, once, instead of
# preparing it again, unless that content depends on the height of the
# preflight page (a theme unit in npc, say). An export with an explicit height has no preflight:
# its render prepares the content, and an error doing so is kept for
# save_metro_dag() to raise once the device is closed, as a preflight error
# is; nothing is drawn (NULL).
.metro_prepared <- function(plot, width, prepare) {
  token <- .metro_export_token(plot)
  if (is.null(token)) return(prepare())
  if (!is.null(token$content) && identical(token$width, width) && !isTRUE(token$content$relative)) {
    content <- token$content
    token$content <- NULL
    return(content)
  }
  if (!isTRUE(token$explicit)) return(prepare())
  tryCatch(prepare(), error = function(e) {
    token$error <- e
    NULL
  })
}

# Problems found while drawing are collected and raised once drawing has
# returned, as for exports: an error raised inside makeContent() leaves the
# device locked, so that the next dev.off() warns "Killing locked device".
# An error from ggplot2 or an added layer is raised as it was.
#' @export
#' @importFrom grid grid.draw
grid.draw.metrodag_plot <- function(x, recording = TRUE) {
  # patchwork copies the last plot's class onto the composition; let patchwork
  # draw every panel rather than only the last MetroDAG plot.
  if (inherits(x, "patchwork")) return(.metro_draw_composition(x))
  grid::grid.draw(as_metro_grob(x), recording = recording)
}

# save_metro_dag() marks a composition whose last plot is not a MetroDAG plot,
# and a wrap_elements() composition, with this class, so that it is drawn the
# same way.
#' @export
grid.draw.metrodag_composition <- function(x, recording = TRUE) .metro_draw_composition(x)

# patchwork draws a composition, every panel of it, once the text metrics are
# reset (see .metro_reset_text_metrics()) on the page that the reset starts.
.metro_draw_composition <- function(x) {
  # Drawing prints an internal copy; the caller's plot stays the last plot.
  on.exit(ggplot2::set_last_plot(x), add = TRUE)
  .metro_draw_collected(x, function(p) {
    .metro_reset_text_metrics()
    print(p, newpage = FALSE)
  })
}

# The grob from as_metro_grob() is drawn the same way, unless it is part of a
# figure whose problems are already being collected, such as a composition.
#' @export
grid.draw.metrodag_plot_grob <- function(x, recording = TRUE) {
  if (.metro_drawn_by_grid(x$plot)) return(NextMethod())
  .metro_draw_collected(x$plot, function(p) {
    x$plot <- p
    .metro_draw_whole(x, .metro_collecting_state(p), recording)
  }, rethrow = TRUE)
}

#' @export
print.metrodag_plot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  ggplot2::set_last_plot(x)
  if (newpage) grid::grid.newpage()
  # Match ggplot's recording boundary so knitr/evaluate capture consecutive
  # plots independently and replay can resolve our grid methods.
  grDevices::recordGraphics(requireNamespace("metrodag", quietly = TRUE), list(), getNamespace("metrodag"))
  if (!is.null(vp)) {
    if (is.character(vp)) {
      grid::seekViewport(vp)
      on.exit(grid::upViewport(), add = TRUE)
    } else {
      grid::pushViewport(vp)
      on.exit(grid::popViewport(), add = TRUE)
    }
  }
  grid::grid.draw(x)
  invisible(x)
}

# Use the S3 method directly so both classes retain its graphics recording
# behavior, including notebooks that draw and diagnose plots in later chunks.
#' @export
print.metrodag_comparison <- print.metrodag_plot

#' @export
plot.metrodag_plot <- function(x, ...) print(x, ...)

# Render beside the destination, then publish only a complete figure. POSIX
# rename replaces atomically; platforms that cannot replace an existing file
# retain a recoverable backup until the new file has been installed.
.metro_replace_file <- function(temporary, filename) {
  rename <- function(from, to) suppressWarnings(file.rename(from, to))
  if (rename(temporary, filename)) return(invisible(filename))
  if (!file.exists(filename) || dir.exists(filename))
    stop("Could not move the completed figure to `filename`.", call. = FALSE)
  backup <- tempfile(pattern = ".metrodag-backup-", tmpdir = dirname(filename))
  if (!rename(filename, backup))
    stop("Could not replace `filename`; the existing file is unchanged.", call. = FALSE)
  if (rename(temporary, filename)) {
    unlink(backup)
    return(invisible(filename))
  }
  if (!rename(backup, filename)) stop(sprintf(
    "Could not replace `filename` or restore it. The original file is preserved at %s.", backup), call. = FALSE)
  stop("Could not replace `filename`; the existing file has been restored.", call. = FALSE)
}

# A blank preflight page measures annotations with exactly the chosen device,
# including user-supplied fonts/device options. It never draws a reduced DAG.
# Errors are kept for save_metro_dag() to raise after the page is closed,
# rather than raised here, which would leave the device locked. The content
# prepared for the measurement is kept, with its width, for the final render.
#' @export
#' @importFrom grid makeContent
makeContent.metrodag_measure_grob <- function(x) {
  width <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  state <- x$state
  tryCatch({
    .metro_reset_text_metrics()
    if (inherits(x$plot, "metrodag_comparison")) {
      measured <- .metro_measure_comparison(x$plot, width)
      state$height <- measured$height
      state$rows <- measured$rows
      state$content <- measured
    } else {
      prepared <- .metro_prepare_plot(x$plot, width)
      state$height <- .metro_plot_height(prepared$plot, width, prepared$table)
      state$content <- prepared
    }
    state$width <- width
  }, error = function(e) state$error <- e)
  grid::setChildren(x, grid::gList())
}

# Journal column presets, converted to the caller's units.
.metro_width_presets <- c(single = 85, `1.5` = 120, double = 180)

.metro_width <- function(width, units) {
  if (is.character(width) && length(width) == 1L && !is.na(width)) {
    if (!width %in% names(.metro_width_presets))
      stop(sprintf("`width` must be a positive number or one of: %s.",
        paste0('"', names(.metro_width_presets), '"', collapse = ", ")), call. = FALSE)
    return(.metro_width_presets[[width]] / switch(units, mm = 1, cm = 10, `in` = 25.4))
  }
  .assert_number(width, "width", strict = TRUE)
  width
}

# The font families that the text of a figure asks for: the `family` of each
# MetroDAG panel, every family set in the themes of its plots, whether or not
# that element is drawn, and every family given to a layer, as in
# annotate("text", family = "serif"). The empty family is the device's
# default.
.metro_figure_families <- function(plot) {
  families <- character()
  add <- function(family) if (is.character(family)) families <<- c(families, family)
  add_theme <- function(theme) for (name in names(theme)) {
    element <- theme[[name]]
    if (inherits(element, "element_text")) add(element$family)
  }
  visit <- function(x) {
    if (inherits(x, "metrodag_comparison")) {
      for (panel in c(x$plots, list(x$legend_plot))) visit(panel)
      return(invisible(NULL))
    }
    if (inherits(x, "patchwork")) {
      for (patch in x$patches$plots) visit(patch)
      add_theme(x$patches$annotation$theme)
    }
    if (inherits(x, "wrapped_patch")) for (grob in attr(x, "grobs")) {
      if (inherits(grob, "metrodag_plot_grob")) visit(grob$plot)
      if (inherits(grob, "metrodag_comparison_grob")) visit(grob$comparison)
      if (inherits(grob, c("ggplot", "patchwork"))) visit(grob)
    }
    if (inherits(x, "ggplot")) {
      add_theme(ggplot2::theme_get() + x$theme)
      for (layer in x$layers) {
        add(layer$aes_params$family)
        if (inherits(layer$geom, "GeomMetroDag")) add(layer$geom_params$config$family)
      }
    }
    invisible(NULL)
  }
  visit(plot)
  unique(families[!is.na(families) & nzchar(families)])
}

# Vector devices prefer Cairo for font embedding; warn once per fallback so
# the user knows the PDF/EPS will reference fonts instead of embedding them.
# The base devices draw only the families in their font databases, and
# postscript() only those it is opened with, so it is opened with R's
# standard families and those of `plot`. Any other family stops the export:
# pdf() would fail while drawing it, and postscript() would draw it in its
# default font, measured with other metrics. Without svglite,
# grDevices::svg() draws text as outlines; that is warned once per export too.
.metro_export_device <- function(ext, plot = NULL) {
  cairo <- .metro_cairo_available()
  # Stop unless the base device `name` can draw every family of `plot`; then
  # warn that it does not embed fonts, and return those families.
  fallback <- function(name, database) {
    families <- .metro_figure_families(plot)
    unknown <- setdiff(families, names(database))
    if (length(unknown)) stop(sprintf(paste0(
      "Cairo is not available, and grDevices::%s() cannot draw font %s %s. Use R with Cairo ",
      "support, save as SVG (with svglite) or PNG, or choose a family listed by ",
      "grDevices::%sFonts(), such as \"sans\"."), name,
      if (length(unknown) == 1L) "family" else "families",
      paste0("\"", unknown, "\"", collapse = ", "), name), call. = FALSE)
    warning(sprintf("Cairo is not available; using grDevices::%s(), which does not embed fonts.", name), call. = FALSE)
    families
  }
  switch(ext,
    pdf = if (cairo) grDevices::cairo_pdf else { fallback("pdf", grDevices::pdfFonts()); grDevices::pdf },
    svg = if (.metro_svglite_available()) svglite::svglite else {
      warning("'svglite' is not installed; using grDevices::svg(), which draws text as outlines. ",
        "Install the optional 'svglite' package for editable, searchable text.", call. = FALSE)
      grDevices::svg
    },
    eps = , ps = if (cairo) function(filename, width, height, ...)
      grDevices::cairo_ps(filename, width = width, height = height, ..., onefile = FALSE, fallback_resolution = 600)
    else {
      required <- unique(c("sans", "serif", "mono", fallback("postscript", grDevices::postscriptFonts())))
      # Fonts given as a device argument are included too.
      function(filename, width, height, ..., fonts = NULL)
        grDevices::postscript(filename, width = width, height = height, ..., fonts = unique(c(required, fonts)),
          onefile = FALSE, horizontal = FALSE, paper = "special")
    },
    png = "png", tiff = "tiff", tif = "tiff", NULL)
}

#' Convert a MetroDAG figure to a grid grob for composition
#'
#' The returned grob wraps titles, places labels and checks clearances when it
#' is drawn, using the size it is given, exactly as printing the plot does.
#' Use it to combine MetroDAG figures with other plots, for example with
#' `patchwork::wrap_elements(full = as_metro_grob(p))`, or place it with
#' `grid::grid.draw()` inside a viewport.
#'
#' Plain `ggplot2::ggplotGrob()` and patchwork's default handling also work,
#' and labels then keep the plot's default margins; but titles are not
#' re-wrapped to the final width, so prefer this function for titled panels.
#' Drawn on its own, the grob raises its problems once drawing returns, as a
#' printed plot does. Drawn inside another grob (a patchwork composition,
#' `gridExtra::grid.arrange()` or a `gTree`), it raises them while that grob
#' draws: patchwork intercepts such errors, so a composition that is too
#' small for one of its panels can print without a message, and closing the
#' device afterwards warns "Killing locked device". Check it with
#' [diagnose_metro_dag()] or export it with [save_metro_dag()] (with an
#' explicit `height`), which report every panel's problems.
#'
#' @param plot A MetroDAG plot, layout or result of [compare_metro_dags()].
#' @return A grid `gTree` of class `metrodag_plot_grob`, or
#'   `metrodag_comparison_grob` for a comparison. It holds the figure and lays
#'   it out each time it is drawn, at the size of the viewport it is drawn in.
#' @examples
#' p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y")
#' g <- as_metro_grob(p)
#' grid::grid.newpage()
#' grid::grid.draw(g)
#' @export
as_metro_grob <- function(plot) {
  if (inherits(plot, "patchwork"))
    stop("`plot` is a patchwork composition; place it with patchwork::wrap_elements(full = <composition>) instead of as_metro_grob().", call. = FALSE)
  if (inherits(plot, "metrodag_layout")) plot <- plot_metro_dag(plot)
  if (inherits(plot, "metrodag_comparison"))
    return(grid::gTree(comparison = plot, cl = "metrodag_comparison_grob"))
  if (!inherits(plot, "metrodag_plot"))
    stop("`plot` must be a MetroDAG plot, layout or comparison.", call. = FALSE)
  grid::gTree(plot = plot, cl = "metrodag_plot_grob")
}
