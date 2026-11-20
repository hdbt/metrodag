.metro_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9")

.metro_legend_label_defaults <- c(exposure = "Exposure", outcome = "Outcome",
  adjusted = "Adjusted", selected = "Selected (conditioned on)", observed = "Observed",
  observed_unadjusted = "Observed, not adjusted", unobserved = "Unobserved")

.metro_style_presets <- function(preset) {
  base <- list(preset = preset, edge_colour = "#58636D", node_outline = "#34424D",
    text_colour = "#24292E", exposure_fill = "#0072B2", exposure_colour = NULL,
    outcome_fill = "#D55E00", outcome_colour = NULL, adjusted_fill = "#DDE2E6",
    unobserved_fill = NULL, selected_fill = NULL, outcome_marker = "filled",
    arrow_length = NULL, arrow_angle = 25,
    casing_width = 0.8, outline_width = NULL, station_scaling = "capped",
    legend_labels = .metro_legend_label_defaults, legend_position = "bottom",
    legend_columns = 3L, palette = .metro_palette)
  if (preset == "monochrome") {
    base$node_outline <- "#24292E"
    base$exposure_fill <- "#24292E"; base$exposure_colour <- "#24292E"
    base$outcome_fill <- "#24292E"; base$outcome_colour <- "#24292E"
    # Without colour, exposure and outcome differ by shape: filled vs ring.
    base$outcome_marker <- "ring"
  }
  base
}

#' Define the appearance of a MetroDAG figure
#'
#' A style object collects the colours, arrowhead geometry, station sizing
#' and legend text used by [plot_metro_dag()] and [metro_dag()]. Start from a
#' preset and override only what the figure needs. The `style` argument of the
#' plotting functions accepts either a preset name or a `metro_style()` object.
#'
#' The `"publication"` preset draws charcoal routes on white with Okabe-Ito
#' role colours. `"monochrome"` keeps every distinction in shapes, fills and
#' dashes. `"metro"` colours routes by visual path cover using `palette`; the
#' colours carry no causal meaning.
#'
#' @param preset `"publication"`, `"metro"`, or `"monochrome"`.
#' @param edge_colour Route colour in the publication and monochrome presets.
#' @param node_outline Station outline colour for observed, adjusted and
#'   unobserved variables.
#' @param text_colour Default label colour; the `colour` text option of
#'   [plot_metro_dag()] still overrides it.
#' @param exposure_fill,outcome_fill Station fills for the exposure and outcome.
#' @param exposure_colour,outcome_colour Their outline colours; `NULL` uses the
#'   fill in colour presets and `node_outline` in monochrome.
#' @param adjusted_fill Fill of the square adjusted-variable stations.
#' @param unobserved_fill Fill of dashed unobserved stations; `NULL` uses the
#'   figure background.
#' @param selected_fill Fill of selected (conditioned-on) stations, drawn as
#'   squares with a second inner outline; `NULL` uses the figure background.
#' @param outcome_marker `"filled"` draws the outcome as a filled circle;
#'   `"ring"` draws a hollow ring with a filled centre, so exposure and outcome
#'   differ by shape as well as colour. The monochrome preset uses `"ring"`.
#' @param arrow_length Arrowhead length in millimetres. `NULL` adapts the length
#'   to each destination's in-degree, between 1.3 and 2 mm.
#' @param arrow_angle Half-angle of the arrowhead in degrees.
#' @param casing_width Extra width, in millimetres, of the background-coloured
#'   casing painted under each route so crossings stay legible. `0` disables it.
#' @param outline_width Station outline width in millimetres; `NULL` derives it
#'   from `line_width`.
#' @param station_scaling How stations grow with their degree so fanned arrow
#'   tips remain distinct: `"capped"` (at most 1.5 times `node_size`),
#'   `"degree"` (unbounded, 0.55 mm per arrow), or `"none"`.
#' @param legend_labels Named character vector overriding legend text for any of
#'   `exposure`, `outcome`, `adjusted`, `selected`, `observed`,
#'   `observed_unadjusted` (used when an adjustment set was specified) and
#'   `unobserved`.
#' @param legend_position `"bottom"`, `"top"`, `"left"`, or `"right"`.
#' @param legend_columns Maximum number of legend keys per row.
#' @param palette Colours for path-cover lines in the metro preset.
#' @return A `metrodag_style` list.
#' @examples
#' journal <- metro_style(exposure_fill = "#1B9E77", outcome_fill = "#7570B3",
#'   legend_labels = c(exposure = "Treatment", outcome = "Response"),
#'   station_scaling = "none", arrow_length = 1.6)
#' journal
#' metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y",
#'   style = journal)
#' @export
metro_style <- function(preset = "publication", edge_colour = NULL,
  node_outline = NULL, text_colour = NULL, exposure_fill = NULL,
  exposure_colour = NULL, outcome_fill = NULL, outcome_colour = NULL,
  adjusted_fill = NULL, unobserved_fill = NULL, arrow_length = NULL,
  arrow_angle = NULL, casing_width = NULL, outline_width = NULL,
  selected_fill = NULL, outcome_marker = NULL,
  station_scaling = NULL, legend_labels = NULL, legend_position = NULL,
  legend_columns = NULL, palette = NULL) {
  preset <- .assert_scalar_choice(preset, c("publication", "metro", "monochrome"), "preset")
  style <- .metro_style_presets(preset)
  colour <- function(value, name, allow_null = FALSE) {
    if (is.null(value)) return(NULL)
    if (!is.character(value) || length(value) != 1L || is.na(value))
      stop(sprintf("`%s` must be a single colour.", name), call. = FALSE)
    tryCatch(grDevices::col2rgb(value), error = function(e)
      stop(sprintf("`%s` must be a valid colour.", name), call. = FALSE))
    value
  }
  for (key in c("edge_colour", "node_outline", "text_colour", "exposure_fill",
      "exposure_colour", "outcome_fill", "outcome_colour", "adjusted_fill",
      "unobserved_fill", "selected_fill")) {
    value <- colour(get(key), key)
    if (!is.null(value)) style[[key]] <- value
  }
  if (!is.null(arrow_length)) {
    .assert_number(arrow_length, "arrow_length", strict = TRUE)
    style$arrow_length <- arrow_length
  }
  if (!is.null(arrow_angle)) {
    .assert_number(arrow_angle, "arrow_angle", strict = TRUE)
    if (arrow_angle >= 90) stop("`arrow_angle` must be less than 90 degrees.", call. = FALSE)
    style$arrow_angle <- arrow_angle
  }
  if (!is.null(casing_width)) {
    .assert_number(casing_width, "casing_width")
    style$casing_width <- casing_width
  }
  if (!is.null(outline_width)) {
    .assert_number(outline_width, "outline_width", strict = TRUE)
    style$outline_width <- outline_width
  }
  if (!is.null(outcome_marker))
    style$outcome_marker <- .assert_scalar_choice(outcome_marker, c("filled", "ring"), "outcome_marker")
  if (!is.null(station_scaling))
    style$station_scaling <- .assert_scalar_choice(station_scaling,
      c("capped", "degree", "none"), "station_scaling")
  if (!is.null(legend_labels)) {
    known <- names(.metro_legend_label_defaults)
    if (!is.character(legend_labels) || !length(legend_labels) || anyNA(legend_labels) ||
        is.null(names(legend_labels)) || any(!names(legend_labels) %in% known) ||
        anyDuplicated(names(legend_labels)))
      stop(sprintf("`legend_labels` must be a named character vector using: %s.",
        paste(known, collapse = ", ")), call. = FALSE)
    style$legend_labels[names(legend_labels)] <- legend_labels
  }
  if (!is.null(legend_position))
    style$legend_position <- .assert_scalar_choice(legend_position,
      c("bottom", "top", "left", "right"), "legend_position")
  if (!is.null(legend_columns)) {
    .assert_number(legend_columns, "legend_columns", minimum = 1, integer = TRUE)
    style$legend_columns <- as.integer(legend_columns)
  }
  if (!is.null(palette)) {
    if (!is.character(palette) || !length(palette) || anyNA(palette))
      stop("`palette` must be a non-empty character vector.", call. = FALSE)
    tryCatch(grDevices::col2rgb(palette), error = function(e)
      stop("`palette` must contain valid colours.", call. = FALSE))
    style$palette <- palette
  }
  structure(style, class = "metrodag_style")
}

# Accept a preset name or a style object wherever `style` is supplied.
.as_metro_style <- function(style) {
  if (inherits(style, "metrodag_style")) return(style)
  if (is.character(style) && length(style) == 1L && !is.na(style))
    return(metro_style(.assert_scalar_choice(style, c("publication", "metro", "monochrome"), "style")))
  stop("`style` must be \"publication\", \"metro\", \"monochrome\" or a `metro_style()` object.", call. = FALSE)
}

#' @export
print.metrodag_style <- function(x, ...) {
  cat(sprintf("<metrodag_style> preset \"%s\"\n", x$preset))
  show <- function(key) {
    value <- x[[key]]
    if (is.null(value)) "automatic" else if (length(value) > 1L)
      paste0(paste(utils::head(value, 3L), collapse = ", "), if (length(value) > 3L) ", ..." else "")
    else as.character(value)
  }
  for (key in c("edge_colour", "node_outline", "exposure_fill", "outcome_fill",
      "adjusted_fill", "arrow_length", "arrow_angle", "casing_width",
      "station_scaling", "legend_position", "palette"))
    cat(sprintf("  %-16s %s\n", key, show(key)))
  invisible(x)
}
