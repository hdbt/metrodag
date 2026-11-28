test_that("exports treat percent signs in paths as literal characters", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  root <- tempfile()
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE); ggplot2::set_last_plot(previous) }, add = TRUE)
  plot <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  # CRAN writes one vector format and the short TIFF extension.
  formats <- if (.test_on_cran()) c("svg", "tif") else c("svg", "pdf", "png", "tiff", "tif")
  for (name in c("figures%03d", "95% confidence")) {
    directory <- file.path(root, name)
    dir.create(directory)
    redirected <- file.path(root, "figures001")
    dir.create(redirected, showWarnings = FALSE)
    for (ext in formats) {
      filename <- paste0("figure%03d.", ext)
      file <- file.path(directory, filename)
      expect_no_warning(result <- save_metro_dag(filename, plot, path = directory,
        width = 120, height = 70, dpi = 72))
      expect_identical(result, file)
      expect_gt(file.info(file)$size, 100)
      expect_identical(ggplot2::last_plot(), plot)
      if (ext == "svg") {
        svg <- readLines(file)
        expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 3L)
        expect_equal(sum(grepl("<circle", svg, fixed = TRUE)), 4L)
      }
    }
    expect_setequal(list.files(directory, all.files = TRUE, no.. = TRUE),
      paste0("figure%03d.", formats))
    expect_length(list.files(redirected, all.files = TRUE, no.. = TRUE), 0L)
  }
})

test_that("automatic height measures scaled exports at a valid physical size", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- metrodag_examples("chain")
  plot <- metro_dag(graph, labels = FALSE, legend = FALSE)
  comparison <- compare_metro_dags(graph, list(First = list(), Second = list()), labels = FALSE)
  widths <- c(mm = 5, cm = 0.5, `in` = 5 / 25.4)
  # CRAN exports the single plot in one non-default unit.
  inputs <- if (.test_on_cran()) list(plot) else list(plot, comparison)
  if (.test_on_cran()) widths <- widths["cm"]
  for (input in inputs) for (units in names(widths)) {
    original <- input
    expect_no_warning(save_metro_dag(file, input, width = widths[[units]],
      units = units, scale = 20))
    svg <- readLines(file)
    expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)),
      if (inherits(input, "metrodag_comparison")) 6L else 3L)
    expect_no_warning(report <- diagnose_metro_dag(input, width = widths[[units]],
      units = units, scale = 20, device = "svg"))
    expect_true(report$ok)
    expect_equal(unname(report$dimensions["width"]), 100, tolerance = 0.01)
    expect_gte(report$dimensions[["height"]], 45 - 0.01)
    expect_lte(report$dimensions[["height"]], 220 + 0.01)
    expect_identical(input, original)
  }
})

test_that("relative exports work inside a percent-named working directory", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  original_directory <- getwd()
  directory <- paste0(tempfile(), "%03d")
  dir.create(directory)
  on.exit({
    setwd(original_directory)
    unlink(directory, recursive = TRUE)
    ggplot2::set_last_plot(previous)
  }, add = TRUE)
  plot <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  setwd(directory)
  here <- getwd()
  expect_no_warning(result <- save_metro_dag("relative%03d.svg", plot, width = 120))
  expect_identical(result, "relative%03d.svg")
  expect_identical(getwd(), here)
  expect_identical(ggplot2::last_plot(), plot)
  svg <- readLines(result)
  expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 3L)
  expect_equal(sum(grepl("<circle", svg, fixed = TRUE)), 4L)
  expect_identical(list.files(all.files = TRUE, no.. = TRUE), "relative%03d.svg")
})

test_that("failed percent-path exports retain existing files and clean staging", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  directory <- paste0(tempfile(), "%03d")
  dir.create(directory)
  on.exit({ unlink(directory, recursive = TRUE); ggplot2::set_last_plot(previous) }, add = TRUE)
  file <- file.path(directory, "saved.svg")
  original <- charToRaw("existing figure")
  writeBin(original, file)
  plot <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  warnings <- character()
  expect_error(withCallingHandlers(save_metro_dag(file, plot, width = 20, height = 55),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), "too small to draw edge")
  # Current ggplot2 can warn while closing a device after a deliberate draw error.
  expect_true(all(warnings == "Killing locked device"))
  expect_identical(readBin(file, "raw", n = file.info(file)$size), original)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "saved.svg")
  expect_identical(ggplot2::last_plot(), plot)
  testthat::local_mocked_bindings(.metro_replace_file = function(temporary, filename) {
    expect_true(file.exists(temporary))
    expect_gt(file.info(temporary)$size, 100)
    stop("Publication intentionally stopped.", call. = FALSE)
  }, .package = "metrodag")
  expect_error(save_metro_dag(file, plot, width = 120), "Publication intentionally stopped")
  expect_identical(readBin(file, "raw", n = file.info(file)$size), original)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "saved.svg")
  expect_identical(ggplot2::last_plot(), plot)
})

test_that("base PDF fallback and explicit scaled heights retain export behavior", {
  previous <- ggplot2::last_plot()
  directory <- paste0(tempfile(), "%03d")
  dir.create(directory)
  on.exit({ unlink(directory, recursive = TRUE); ggplot2::set_last_plot(previous) }, add = TRUE)
  testthat::local_mocked_bindings(.metro_cairo_available = function() FALSE, .package = "metrodag")
  plot <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  file <- file.path(directory, "base%03d.pdf")
  # The base device does not embed fonts; the export says so once and proceeds.
  expect_warning(result <- save_metro_dag(file, plot, width = 5, height = 4, scale = 20),
    "does not embed fonts")
  expect_identical(result, file)
  expect_gt(file.info(file)$size, 100)
  expect_no_warning(report <- diagnose_metro_dag(plot, width = 5, height = 4,
    scale = 20, device = "pdf"))
  expect_true(report$ok)
  expect_equal(unname(report$dimensions), c(100, 80), tolerance = 0.01)
  expect_identical(ggplot2::last_plot(), plot)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "base%03d.pdf")
})

# The messages of the warnings an expression raises.
.test_warnings <- function(expr) {
  warnings <- character()
  withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  warnings
}

test_that("a figure's font families include its themes, text layers and panels", {
  graph <- metrodag_examples("chain")
  plot <- metro_dag(graph, family = "serif") +
    ggplot2::theme(plot.title = ggplot2::element_text(family = "Palatino")) +
    ggplot2::annotate("text", x = 0, y = 0, label = "Note", family = "AvantGarde")
  expect_setequal(.metro_figure_families(plot), c("serif", "Palatino", "AvantGarde"))
  skip_on_cran()
  comparison <- compare_metro_dags(graph, list(A = list(exposure = "A"), B = list(outcome = "D")),
    family = "mono")
  expect_identical(.metro_figure_families(comparison), "mono")
  skip_if_not_installed("patchwork")
  # Every plot of a composition counts, wrapped ones included, and so does
  # its annotation theme.
  other <- ggplot2::ggplot() + ggplot2::theme_grey(base_family = "Bookman")
  composition <- patchwork::wrap_plots(patchwork::wrap_elements(full = as_metro_grob(comparison)),
    other) + patchwork::plot_annotation(title = "Views",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(family = "Times")))
  expect_setequal(.metro_figure_families(composition), c("mono", "Times", "Bookman"))
})

test_that("base PDF and EPS fallbacks draw labelled figures in the families they know", {
  previous <- ggplot2::last_plot()
  directory <- tempfile("fallback")
  dir.create(directory)
  on.exit({ unlink(directory, recursive = TRUE); ggplot2::set_last_plot(previous) }, add = TRUE)
  testthat::local_mocked_bindings(.metro_cairo_available = function() FALSE, .package = "metrodag")
  # Labels and legend are in the default family, "sans"; the title is in one
  # of R's font families outside the standard three.
  plot <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D",
    estimand = "Total effect of A on D") +
    ggplot2::theme(plot.title = ggplot2::element_text(family = "Palatino"))
  # Each export warns once that the base device does not embed fonts,
  # although an automatic height opens the device twice.
  export <- function(ext) {
    file <- file.path(directory, paste0("labelled.", ext))
    warnings <- .test_warnings(save_metro_dag(file, plot, width = 85, fonts = "Bookman"))
    expect_length(warnings, 1L)
    expect_match(warnings, "does not embed fonts", fixed = TRUE)
    file
  }
  # postscript() draws only the families it is opened with: R's standard
  # three, those of the figure and those given as a device argument.
  eps <- readLines(export("eps"), warn = FALSE)
  expect_match(eps[1L], "^%!PS.*EPSF")
  fonts <- sub("^.* font ", "", grep("^%%(DocumentNeededResources: |\\+ )font ", eps, value = TRUE))
  expect_true(all(c("Helvetica", "Times-Roman", "Courier", "Palatino-Roman", "Bookman-Light") %in% fonts))
  expect_identical(ggplot2::last_plot(), plot)
  skip_on_cran()
  # pdf() needs no declarations: it adds each font the figure uses (the
  # title is bold).
  pdf <- export("pdf")
  bytes <- readBin(pdf, "raw", file.size(pdf))
  text <- rawToChar(bytes[bytes != as.raw(0)])
  Encoding(text) <- "bytes"
  expect_match(text, "^%PDF", useBytes = TRUE)
  for (font in c("Helvetica", "Palatino-Bold"))
    expect_match(text, paste0("/BaseFont /", font), fixed = TRUE, useBytes = TRUE)
  expect_identical(ggplot2::last_plot(), plot)
  report <- diagnose_metro_dag(plot, width = 85, device = "eps")
  expect_true(report$ok)
  expect_identical(report$issues$type, "render_warning")
  expect_match(report$issues$message, "does not embed fonts", fixed = TRUE)
})

test_that("base fallbacks stop on font families they cannot draw, naming them", {
  previous <- ggplot2::last_plot()
  directory <- tempfile("fallback")
  dir.create(directory)
  on.exit({ unlink(directory, recursive = TRUE); ggplot2::set_last_plot(previous) }, add = TRUE)
  testthat::local_mocked_bindings(.metro_cairo_available = function() FALSE, .package = "metrodag")
  graph <- metrodag_examples("chain")
  # pdf() would fail while drawing a family outside its font database, and
  # postscript() would draw it in its default font, measured differently.
  unknown <- metro_dag(graph, family = "No Such Font")
  for (device in c("pdf", "postscript")) {
    file <- file.path(directory, paste0("unknown.", if (device == "pdf") "pdf" else "eps"))
    expect_no_warning(error <- expect_error(save_metro_dag(file, unknown, width = 85),
      sprintf("grDevices::%s() cannot draw font family \"No Such Font\"", device), fixed = TRUE))
    expect_match(conditionMessage(error), "Use R with Cairo support, save as SVG (with svglite) or PNG",
      fixed = TRUE)
    expect_match(conditionMessage(error), sprintf("grDevices::%sFonts()", device), fixed = TRUE)
  }
  # A family set in the theme counts too, whichever text it styles.
  titled <- unknown + ggplot2::theme(plot.title = ggplot2::element_text(family = "Another Missing Font"))
  error <- expect_error(save_metro_dag(file.path(directory, "titled.pdf"), titled, width = 85),
    "font families", fixed = TRUE)
  expect_match(conditionMessage(error), "\"No Such Font\", \"Another Missing Font\"", fixed = TRUE)
  report <- diagnose_metro_dag(unknown, width = 85, device = "pdf")
  expect_false(report$ok)
  expect_identical(report$issues$type, "render_error")
  expect_match(report$issues$message, "font family \"No Such Font\"", fixed = TRUE)
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)
  expect_identical(ggplot2::last_plot(), titled)
})

test_that("SVG export without svglite warns once that text is drawn as outlines", {
  skip_on_cran()
  skip_if_not(.metro_cairo_available(), "grDevices::svg() needs Cairo")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  testthat::local_mocked_bindings(.metro_svglite_available = function() FALSE, .package = "metrodag")
  plot <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D",
    estimand = "Total effect of A on D")
  # The automatic height opens the device twice; the export warns once.
  warnings <- .test_warnings(save_metro_dag(file, plot, width = 85))
  expect_length(warnings, 1L)
  expect_match(warnings, "using grDevices::svg(), which draws text as outlines", fixed = TRUE)
  expect_match(warnings, "Install the optional 'svglite' package", fixed = TRUE)
  # The labels and title are drawn, as glyph outlines.
  svg <- readLines(file, warn = FALSE)
  expect_false(any(grepl("<text", svg, fixed = TRUE)))
  expect_true(any(grepl("glyph", svg, fixed = TRUE)))
})
