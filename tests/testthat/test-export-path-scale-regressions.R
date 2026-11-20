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
