# save_metro_dag() measures an automatic height once and draws the figure
# with the content prepared for that measurement; an explicit height has no
# preflight. Neither may change the figure or the conditions an export raises.

.test_svg_bytes <- function(file) readBin(file, "raw", file.size(file))

test_that("an automatic height draws the same figure as that height given explicitly", {
  skip_if_not_installed("svglite")
  layout <- layout_metro_dag(metrodag_examples("causal"))
  p <- metro_dag(layout, exposure = "X", outcome = "Y", adjusted = "C", unobserved = "U",
    estimand = "A title long enough to be wrapped at one column")
  co <- compare_metro_dags(layout, list(Crude = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C")))
  # Theme units in npc or snpc depend on the page they are converted on: the
  # final page prepares such a figure again rather than use the preflight's
  # content. Margins converted for the annotations, legend sizes converted by
  # ggplot2, and the panels of a comparison.
  margins <- p + ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.05, 0.05, 0.05, unit = "npc"))
  # The room below the subtitle (the line nearest the panel) is widened, so
  # its margin is converted; the title's is not.
  title <- p + ggplot2::theme(plot.subtitle = ggplot2::element_text(
    margin = ggplot2::margin(t = 0.02, b = 0.001, unit = "snpc")))
  keys <- p + ggplot2::theme(legend.key.height = grid::unit(0.06, "npc"))
  panels <- co
  panels$plots <- lapply(co$plots, function(panel) panel + ggplot2::theme(
    plot.margin = ggplot2::margin(0.04, 0.04, 0.04, 0.04, unit = "npc")))
  for (figure in list(p, co, margins, title, keys, panels)) {
    # The automatic height, measured on the preflight's page.
    svglite::svglite(tempfile(fileext = ".svg"), width = 120 / 25.4, height = 100 / 25.4)
    width <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    natural <- if (inherits(figure, "metrodag_comparison")) .build_metro_comparison(figure, width)$height else
      .metro_plot_height(.metro_wrap_annotations(figure, width), width)
    grDevices::dev.off()
    automatic <- tempfile(fileext = ".svg"); explicit <- tempfile(fileext = ".svg")
    save_metro_dag(automatic, figure, width = 120)
    save_metro_dag(explicit, figure, width = 120, height = max(45, min(220, natural)))
    expect_identical(.test_svg_bytes(automatic), .test_svg_bytes(explicit))
    unlink(c(automatic, explicit))
  }
})

test_that("prepared content is used once, at the width it was prepared for", {
  p <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  token <- new.env(parent = emptyenv())
  marked <- .metro_with_config(p, list(export = list(prepared = token)))
  calls <- 0L
  prepare <- function() { calls <<- calls + 1L; "fresh" }
  measured <- list(value = "measured", relative = FALSE)
  token$width <- 120; token$content <- measured
  expect_identical(.metro_prepared(marked, 120, prepare), measured)
  expect_identical(.metro_prepared(marked, 120, prepare), "fresh")
  token$content <- measured
  expect_identical(.metro_prepared(marked, 85, prepare), "fresh")
  expect_identical(.metro_prepared(p, 120, prepare), "fresh")
  # Content that depends on the preflight page's height is prepared again.
  token$content <- list(value = "measured", relative = TRUE)
  expect_identical(.metro_prepared(marked, 120, prepare), "fresh")
  expect_identical(calls, 4L)
  # Without a preflight (an explicit height), a preparation error is kept.
  token$explicit <- TRUE
  expect_null(.metro_prepared(marked, 85, function() stop("no build")))
  expect_identical(conditionMessage(token$error), "no build")
})

test_that("exports and diagnostics raise an error building the plot as it was", {
  chain <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  bad <- chain + ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1))
  expected <- tryCatch(ggplot2::ggplot_build(bad), error = function(e) e)
  co <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  co$plots[[2L]] <- co$plots[[2L]] +
    ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1))
  directory <- tempfile("exports")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  file <- file.path(directory, "figure.pdf")
  for (figure in list(bad, co)) for (height in list(NULL, 60)) {
    error <- tryCatch(save_metro_dag(file, figure, width = 85, height = height), error = function(e) e)
    expect_identical(class(error), class(expected))
    expect_identical(conditionMessage(error), conditionMessage(expected))
    # Nothing is written, not even the private render.
    expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), character())
    report <- diagnose_metro_dag(figure, width = 85, height = height)
    expect_false(report$ok)
    expect_identical(report$issues$type, "render_error")
    expect_identical(report$issues$message, conditionMessage(expected))
    expect_identical(report$issues$suggestion, "Check the plot scales, device options and dimensions.")
  }
})
