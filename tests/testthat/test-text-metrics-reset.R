# R's graphics engine remembers the metrics of "M" for the last device that
# measured text and recognises that device by its address, so a device opened
# where an earlier one was can measure, and place, text with the closed
# device's metrics. Which device gets the same address is a matter of chance:
# after a 300 dpi PNG export, a PDF export of the same figure in the same
# session was 19 bp taller in five of six fresh sessions. Every MetroDAG plot
# or comparison, every composition that save_metro_dag() or
# diagnose_metro_dag() draws, and one drawn with grid.draw() whose last plot is
# a MetroDAG plot, therefore replaces that entry before it measures anything.
# print() of a composition, and grid.draw() or ggsave() of one ending in
# another plot, are patchwork's own.

test_that("exports, diagnostics and printing reset the metrics before measuring", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("patchwork")
  # The order of resets and measurements in each drawing: "reset", "measure"
  # for preparing a plot or comparison at a width, or "layer" for the
  # MetroDAG layer placing its labels.
  events <- character()
  reset <- .metro_reset_text_metrics
  prepare <- .metro_prepare_plot
  measure <- .metro_measure_comparison
  content <- .metro_grob_content
  testthat::local_mocked_bindings(
    .metro_grob_content = function(...) {
      events <<- c(events, "layer")
      content(...)
    },
    .metro_reset_text_metrics = function() {
      events <<- c(events, "reset")
      reset()
    },
    .metro_prepare_plot = function(...) {
      events <<- c(events, "measure")
      prepare(...)
    },
    .metro_measure_comparison = function(...) {
      events <<- c(events, "measure")
      measure(...)
    }, .package = "metrodag")
  recorded <- function(code) {
    events <<- character()
    force(code)
    # A comparison prepares each panel while it is measured, and a figure
    # has a layer for each panel: count each run of events once.
    events[c(TRUE, utils::tail(events, -1L) != utils::head(events, -1L))]
  }
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y",
    estimand = "A subtitle")
  plain <- ggplot2::ggplot() + ggplot2::geom_blank()
  co <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  # The preflight page resets and measures; the final page resets and draws
  # what the preflight measured.
  automatic <- c("reset", "measure", "reset", "layer")
  expect_identical(recorded(save_metro_dag(file, p, width = 120)), automatic)
  expect_identical(recorded(save_metro_dag(file, co, width = 120)), automatic)
  expect_identical(recorded(save_metro_dag(file, p, width = 120, height = 80)),
    c("reset", "measure", "layer"))
  expect_identical(recorded(diagnose_metro_dag(p, width = 120, device = "svg")), automatic)
  # patchwork builds and draws a composition's panels itself, after one reset,
  # whichever plot comes last.
  composed <- c("reset", "layer")
  expect_identical(recorded(save_metro_dag(file, p | p, width = 180, height = 80)), composed)
  expect_identical(recorded(save_metro_dag(file, p | plain, width = 180, height = 80)), composed)
  expect_identical(recorded(save_metro_dag(file, patchwork::wrap_elements(full = p | plain),
    width = 180, height = 80)), composed)
  expect_identical(recorded(diagnose_metro_dag(p | plain, width = 180, height = 80, device = "svg")), composed)
  svglite::svglite(file, width = 5, height = 4)
  expect_identical(recorded(print(p)), c("reset", "measure", "layer"))
  expect_identical(recorded(print(co)), c("reset", "measure", "layer"))
  grDevices::dev.off()
})

test_that("resetting the metrics measures as before and starts no extra page", {
  skip_if_not_installed("patchwork")
  height <- function() grid::convertHeight(grid::grobHeight(grid::textGrob("Unadjusted",
    gp = grid::gpar(fontsize = 11))), "mm", valueOnly = TRUE)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  grDevices::pdf(file, width = 4, height = 3)
  grid::grid.newpage()
  expected <- height()
  # The reset measures "M" on the current device at two sizes, so that the
  # remembered metrics are this device's whatever they were before. Whether
  # an earlier device's are remembered depends on where devices are opened
  # in memory, which a test cannot arrange.
  measured <- .metro_reset_text_metrics()
  expect_length(measured, 2L)
  expect_true(all(measured > 0))
  expect_equal(measured[2L] / measured[1L], 7 / 6, tolerance = 1e-6)
  expect_identical(height(), expected)
  # The device's default font is measured, not the caller's: an unknown
  # family set by a viewport raises no warning.
  grid::pushViewport(grid::viewport(gp = grid::gpar(fontfamily = "NoSuchFamily", fontface = "bold")))
  expect_no_warning(inherited <- .metro_reset_text_metrics())
  expect_identical(inherited, measured)
  grid::popViewport()
  grDevices::dev.off()
  # A composition saved with ggsave() is drawn on a device without a page;
  # the reset before patchwork draws it must not add a blank one.
  pages <- function(file) {
    bytes <- readBin(file, "raw", file.size(file))
    text <- rawToChar(bytes[bytes != as.raw(0)])
    Encoding(text) <- "bytes"
    lengths(regmatches(text, gregexpr("/Type /Page\\b(?!s)", text, perl = TRUE, useBytes = TRUE)))
  }
  p <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  for (figure in list(p, p | p)) {
    ggplot2::ggsave(file, figure, width = 6, height = 3, device = grDevices::pdf)
    expect_identical(pages(file), 1L)
  }
})
