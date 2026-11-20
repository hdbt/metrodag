# Comparisons that do not fit say what size would, as single plots do, and
# figures whose automatic height was limited to 220 mm say what height they
# need.

.test_size_error <- function(expr) tryCatch({
  suppressWarnings(expr)
  NA_character_
}, error = conditionMessage)

test_that("a comparison too small for its panels names the width it needs", {
  skip_on_cran()
  labels <- c(C = "Socioeconomic background", X = "Cigarette consumption", M = "Tar deposit in lung",
    Y = "Lung cancer risk", U = "Unmeasured inheritance", S = "Occupational hazards")
  views <- list(
    Total = list(exposure = "X", outcome = "Y", adjusted = "C", unobserved = "U"),
    Crude = list(exposure = "X", outcome = "Y", unadjusted = TRUE, unobserved = "U"))
  cmp <- compare_metro_dags(metrodag_examples("causal"), views, labels = labels)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  error <- .test_size_error(save_metro_dag(file, cmp, width = 85))
  expect_match(error, "drawing problems in panels 1, 2")
  expect_match(error, "Export at least about [0-9]+ mm wide")
  expect_false(grepl("Increase export width/height", error, fixed = TRUE))
  expect_false(grepl("Automatic height was limited", error, fixed = TRUE))
  expect_false(file.exists(file))
  report <- diagnose_metro_dag(cmp, width = 85)
  expect_false(report$ok)
  errors <- report$issues$severity == "error"
  expect_true(all(grepl("export width", report$issues$suggestion[errors], fixed = TRUE)))
  # The estimate is checked at its own size, and the export fits there.
  expect_named(report$min_size, "width")
  expect_gt(report$min_size[["width"]], 85)
  expect_lte(report$min_size[["width"]], 160)
  expect_true(diagnose_metro_dag(cmp, width = report$min_size[["width"]])$ok)
  expect_no_error(suppressWarnings(save_metro_dag(file, cmp, width = report$min_size[["width"]])))
  expect_true(file.exists(file))
  expect_null(diagnose_metro_dag(cmp, width = 180)$min_size)
})

test_that("comparison size estimates allow for columns, gaps and the legend", {
  config <- list(figure_size = c(60, 50), available = c(50, 30),
    export = list(automatic_height = FALSE, scale = 1))
  single <- .metro_needed_size(config, c(50, 30), 1.5)
  expect_equal(single, c(width = 90, height = 70))
  config$comparison <- list(columns = 3L, rows = 2L, gap = 4, legend = 12)
  # Three cells of 10 + 78.75 mm and two gaps; two cells of 20 + 47.25 mm,
  # one gap and the legend.
  expect_equal(.metro_needed_size(config, c(50, 30), 1.5), c(width = 275, height = 155))
  expect_equal(.metro_larger_size(c(width = 100), c(width = 90, height = 80)),
    c(width = 100, height = 80))
  expect_equal(.metro_larger_size(NULL, c(height = 80)), c(height = 80))
})

test_that("a single plot limited to 220 mm of automatic height says what it needs", {
  skip_on_cran()
  i <- seq_len(29L)
  edges <- rbind(cbind(paste0("A", i), paste0("A", i + 1L)), cbind(paste0("B", i), paste0("B", i + 1L)),
    cbind(paste0("A", i), paste0("B", i + 1L)))
  p <- metro_dag(edges, direction = "down", labels = FALSE, legend = FALSE)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  error <- .test_size_error(save_metro_dag(file, p, width = 85))
  expect_match(error, "Automatic height was limited to 220 mm; this figure needs about ([0-9]+) mm. Pass `height = \\1`.")
  expect_false(grepl("more columns", error, fixed = TRUE))
  needed <- as.numeric(sub(".*this figure needs about ([0-9]+) mm.*", "\\1", error))
  expect_gt(needed, 220)
  report <- diagnose_metro_dag(p, width = 85)
  expect_false(report$ok)
  expect_equal(unname(report$dimensions["height"]), 220, tolerance = 0.5)
  expect_true("height" %in% names(report$min_size))
  expect_gte(report$min_size[["height"]], needed)
  expect_no_error(save_metro_dag(file, p, width = 85, height = report$min_size[["height"]]))
  # In other units the height to pass is converted.
  error <- .test_size_error(save_metro_dag(file, p, width = 8.5, units = "cm"))
  expect_match(error, sprintf("this figure needs about %d mm. Pass `height = %s`.", needed,
    format(needed / 10)), fixed = TRUE)
  # An explicit height is never described as limited.
  error <- .test_size_error(save_metro_dag(file, p, width = 85, height = 200))
  expect_false(is.na(error))
  expect_false(grepl("Automatic height", error, fixed = TRUE))
})

test_that("a comparison limited to 220 mm of automatic height suggests more columns", {
  skip_on_cran()
  dense <- layout_metro_dag(metrodag_examples("dense"))
  cmp <- compare_metro_dags(dense, rep(list(list()), 4L), ncol = 1L, labels = FALSE)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  error <- .test_size_error(save_metro_dag(file, cmp, width = 120))
  expect_match(error, "drawing problems in panels 1, 2, 3, 4")
  expect_match(error, "Automatic height was limited to 220 mm; this figure needs about ([0-9]+) mm. Pass `height = \\1` or use more columns.")
  needed <- as.numeric(sub(".*this figure needs about ([0-9]+) mm.*", "\\1", error))
  report <- diagnose_metro_dag(cmp, width = 120)
  expect_false(report$ok)
  expect_true("height" %in% names(report$min_size))
  expect_gte(report$min_size[["height"]], needed)
  expect_true(diagnose_metro_dag(cmp, width = 120, height = report$min_size[["height"]])$ok)
  # With the views side by side, the automatic height is not limited.
  wide <- compare_metro_dags(dense, rep(list(list()), 4L), ncol = 4L, labels = FALSE)
  error <- .test_size_error(save_metro_dag(file, wide, width = 120))
  if (!is.na(error)) expect_false(grepl("Automatic height", error, fixed = TRUE))
})

test_that("a panel limited by automatic height is first advised to grow taller", {
  # A comparison cell of two rows limited to 220 mm of automatic height: the
  # panel is 125.9 x 75 mm in 169.9 x 75 mm of room, so a taller figure alone
  # lets it grow to fill the width (scale 1.35), at the height automatic
  # sizing would choose: two cells of 25.3 + 101.2 mm, a gap and the legend.
  config <- list(figure_size = c(179.9, 100.3), available = c(169.9, 75), limiting = "height",
    export = list(automatic_height = TRUE, scale = 1),
    comparison = list(columns = 1L, rows = 2L, gap = 4, legend = 20))
  expect_equal(.metro_needed_size(config, c(125.9, 75), 1.15), c(height = 260))
  # An estimate beyond that fill does not grow the width at once:
  # diagnose_metro_dag() checks the taller figure first.
  expect_equal(.metro_needed_size(config, c(125.9, 75), 1.488), c(height = 280))
  # With an explicit height, both dimensions grow as before.
  config$export$automatic_height <- FALSE
  expect_equal(.metro_needed_size(config, c(125.9, 75), 1.488), c(width = 210, height = 310))
})

test_that("the automatic-height note is given only when a taller figure can help", {
  issues <- function(...) data.frame(type = c(...), severity = "error", stringsAsFactors = FALSE)
  state <- new.env(parent = emptyenv())
  expect_true(.metro_height_helps(issues("edge_short", "edge_station", "edge_endpoint", "station_overlap"), state))
  expect_false(.metro_height_helps(issues("edge_short", "edge_zero_length"), state))
  expect_false(.metro_height_helps(issues("station_bounds", "edge_bounds"), state))
  expect_false(.metro_height_helps(issues("render_error"), state))
  # Warnings never count; a figure with no error needs no note.
  expect_false(.metro_height_helps(data.frame(type = "label_overlap", severity = "warning"), state))
  # A panel estimate of the height always helps.
  state$min_size <- c(height = 300)
  expect_true(.metro_height_helps(issues("edge_zero_length", "edge_short"), state))
  # Stations drawn at one point overlap at any size.
  state <- new.env(parent = emptyenv())
  state$coincident <- TRUE
  expect_false(.metro_height_helps(issues("station_overlap"), state))
  # A note whose figure must also be wider names both arguments.
  limited <- list(height = 300, scale = 1, factor = 10, columns = TRUE)
  expect_identical(.metro_height_note(limited, c(width = 210, height = 305)),
    "Automatic height was limited to 220 mm; this figure needs about 210 x 305 mm (width x height). Pass `width = 21, height = 30.5` or use more columns.")
})

test_that("problems a taller figure does not fix get no automatic-height note", {
  skip_on_cran()
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  edges <- cbind(paste0("N", 1:13), paste0("N", 2:14))
  layout <- layout_metro_dag(edges, direction = "down")
  tall <- metro_dag(layout, labels = FALSE, legend = FALSE)
  expect_true(diagnose_metro_dag(tall, width = 85)$ok)
  no_note <- function(plot, types) {
    error <- .test_size_error(save_metro_dag(file, plot, width = 85))
    expect_false(is.na(error))
    expect_false(grepl("Automatic height", error, fixed = TRUE))
    report <- diagnose_metro_dag(plot, width = 85)
    expect_false(report$ok)
    expect_true(all(types %in% report$issues$type))
    expect_false("height" %in% names(report$min_size))
  }
  # Coordinate limits that cut off half of the graph.
  ranges <- ggplot2::ggplot_build(tall)$layout$panel_params[[1L]]$y.range
  no_note(suppressMessages(tall + ggplot2::coord_cartesian(ylim = c(mean(ranges), ranges[2L]),
    expand = FALSE)), c("station_bounds", "edge_bounds"))
  # Move station `node` onto station `onto`, keeping routes continuous; with
  # `whole`, its outgoing routes collapse onto that point.
  collapse <- function(node, onto, whole = FALSE) {
    moved <- layout
    target <- moved$nodes[moved$nodes$node == onto, c("x", "y")]
    moved$nodes[moved$nodes$node == node, c("x", "y")] <- target
    first <- moved$edges$from == node & (whole | moved$edges$segment == 1L)
    last <- moved$edges$to == node
    if (any(last)) last <- last & moved$edges$segment == max(moved$edges$segment[last])
    moved$edges[first, c("x", "y")] <- target
    if (whole) moved$edges[first, c("xend", "yend")] <- target
    moved$edges[last, c("xend", "yend")] <- target
    metro_dag(moved, labels = FALSE, legend = FALSE)
  }
  # A collapsed edge, whose stations also coincide.
  collapsed <- expect_no_warning(collapse("N1", "N2", whole = TRUE))
  no_note(collapsed, c("edge_zero_length", "station_overlap"))
  # Routes through the centres of stations, and two stations at one point:
  # only size problems in name, but no size separates them.
  through <- expect_no_warning(collapse("N7", "N14"))
  no_note(through, c("edge_station", "station_overlap"))
})

test_that("a capped comparison gives one size, at the export width", {
  skip_on_cran()
  # The epidemiological example of the audit in one column: its panels'
  # estimate asks for more than a taller figure gives, yet that figure fits.
  edges <- data.frame(
    from = c("A", "A", "A", "A", "A", "G", "G", "S", "S", "S", "S", "K", "K", "L", "P", "B", "B",
      "H", "H", "D", "D", "X", "U", "U", "U", "X", "Y"),
    to = c("K", "H", "D", "X", "Y", "X", "Y", "K", "L", "X", "Y", "H", "Y", "B", "B", "H", "D",
      "X", "Y", "X", "Y", "Y", "X", "P", "Y", "R", "R"))
  labels <- c(A = "Age at baseline", G = "Sex", S = "Socioeconomic position",
    K = "Current smoking", L = "Alcohol consumption", P = "Systolic pressure",
    B = "Body mass index", H = "Hypertension", D = "Type 2 diabetes",
    X = "Statin prescription", Y = "Major cardiovascular event",
    U = "Health-seeking behaviour", R = "Retained in follow-up")
  fixed <- attr(metro_dag(edges, labels = labels, exposure = "X", outcome = "Y",
    adjusted = c("A", "G", "S", "H", "D"), unobserved = "U", selected = "R"), "metrodag_layout")
  views <- list(Crude = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = c("A", "G", "S", "H", "D")))
  cmp <- compare_metro_dags(fixed, labels = labels, ncol = 1, views)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  error <- .test_size_error(save_metro_dag(file, cmp, width = 180))
  expect_false(is.na(error))
  expect_false(grepl("width x height", error, fixed = TRUE))
  expect_false(grepl("mm wide", error, fixed = TRUE))
  advised <- as.numeric(sub(".*Export at least about ([0-9]+) mm tall.*", "\\1", error))
  expect_match(error, sprintf("this figure needs about %d mm. Pass `height = %d` or use more columns.",
    advised, advised), fixed = TRUE)
  # diagnose_metro_dag() checks the advice at the export width; a size it
  # reports is one at which the figure was found to fit.
  report <- diagnose_metro_dag(cmp, width = 180)
  expect_identical(names(report$min_size), "height")
  expect_gt(report$min_size[["height"]], 220)
  expect_lte(report$min_size[["height"]], advised)
})
