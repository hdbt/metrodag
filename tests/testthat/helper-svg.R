# Readers for svglite output, which writes one element per line.

# Drawn elements of the given kinds, in drawing order.
.svg_primitives <- function(file, kinds = c("circle", "rect", "polyline", "polygon", "text")) {
  svg <- readLines(file, warn = FALSE)
  svg[grepl(sprintf("<(%s) ", paste(kinds, collapse = "|")), svg)]
}

# Number of elements of one kind, such as "polygon" or "circle". Arrowheads
# and dashed station outlines are both polygons; count them with
# .svg_arrowheads() and .svg_dashed_outlines().
.svg_count <- function(file, kind) {
  sum(grepl(paste0("<", kind), readLines(file, warn = FALSE), fixed = TRUE))
}

# Number of vertices of each polygon element in `lines`.
.svg_vertices <- function(lines) {
  lengths(strsplit(trimws(sub(".* points='([^']*)'.*", "\\1", lines)), " ", fixed = TRUE))
}

# Number of arrowheads: the triangles among the polygons.
.svg_arrowheads <- function(file) {
  svg <- readLines(file, warn = FALSE)
  sum(.svg_vertices(svg[grepl("<polygon ", svg, fixed = TRUE)]) == 3L)
}

# Number of dashed station outlines, of stations and legend keys alike. They
# are 72-sided polygons, not circles, which the Windows GDI device would draw
# solid (see .metro_circle_grob()).
.svg_dashed_outlines <- function(file) {
  svg <- readLines(file, warn = FALSE)
  dashed <- grepl("<polygon ", svg, fixed = TRUE) & grepl("stroke-dasharray", svg, fixed = TRUE)
  sum(.svg_vertices(svg[dashed]) == 72L)
}

# The sorted distinct alphabetic text strings drawn in the figure.
.svg_texts <- function(file) {
  lines <- readLines(file, warn = FALSE)
  sort(unique(regmatches(lines, regexpr("(?<=>)[[:alpha:]]+(?=</text>)", lines, perl = TRUE))))
}

# Horizontal extent of every text element, from its anchor and textLength.
.svg_text_bounds <- function(file) {
  text <- grep("<text", readLines(file, warn = FALSE), value = TRUE, fixed = TRUE)
  value <- function(attribute) as.numeric(sub("px$", "",
    sub(paste0(".* ", attribute, "='([^']+)'.*"), "\\1", text)))
  x <- value("x"); width <- value("textLength")
  left <- x - ifelse(grepl("text-anchor='middle'", text, fixed = TRUE), width / 2,
    ifelse(grepl("text-anchor='end'", text, fixed = TRUE), width, 0))
  data.frame(left = left, right = left + width, text = text)
}

# Export checks. save_metro_dag() renders to a staging file and replaces its
# target only once the new figure is complete. The dedicated test in
# test-device-safety.R checks five kinds of failure in every format, and
# test-diagnostics.R keeps one such check in the CRAN subset. `export` is a
# function of the output path; it must fail with `regexp`, the stand-in bytes
# written to `file` first must survive, and no staging file (a dot-file beside
# the target, or a file in the session's temporary directory) may be left.
.expect_failed_export_keeps_file <- function(file, export, regexp, ...) {
  original <- charToRaw("existing figure")
  writeBin(original, file)
  directories <- unique(normalizePath(c(dirname(file), tempdir())))
  contents <- function() unlist(lapply(directories, list.files,
    all.files = TRUE, no.. = TRUE, full.names = TRUE))
  before <- contents()
  expect_error(export(file), regexp, ...)
  kept <- if (file.exists(file)) readBin(file, "raw", n = file.info(file)$size + 1L) else raw()
  expect_identical(kept, original, ...)
  expect_identical(setdiff(contents(), before), character(), ...)
}
