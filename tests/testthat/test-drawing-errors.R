# Problems found while printing or drawing a plot or comparison are raised
# once drawing has returned, so the device is never left locked and closing
# it gives no "Killing locked device" warning.

# The dense example is laid out once for the file: the figures below differ
# only in how and where they are drawn.
.test_dense <- .test_once(function() layout_metro_dag(metrodag_examples("dense")))

.test_open_pdf <- function(width, height) {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file, width = width, height = height)
  list(file = file, device = grDevices::dev.cur())
}

.test_close_quietly <- function(opened) {
  expect_no_warning(grDevices::dev.off(opened$device))
  unlink(opened$file)
}

test_that("a strict plot that does not fit raises after drawing", {
  p <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  opened <- .test_open_pdf(0.6, 0.6)
  error <- tryCatch(print(p), error = function(e) e)
  expect_s3_class(error, "metrodag_render_error")
  expect_match(conditionMessage(error), "drawing problems at this size. First: .*(too small|beyond)")
  expect_true(all(error$issues$severity == "error"))
  .test_close_quietly(opened)
  opened <- .test_open_pdf(0.6, 0.6)
  expect_error(grid::grid.draw(p), "drawing problems")
  .test_close_quietly(opened)
  # ggsave() closes its own device on the way out.
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  warnings <- character()
  withCallingHandlers(expect_error(ggplot2::ggsave(file, p, width = 15, height = 15, units = "mm"),
    "drawing problems"),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("locked", warnings, fixed = TRUE)))
})

test_that("comparisons that do not fit raise after drawing", {
  co <- compare_metro_dags(.test_dense(), list(list(), list()), labels = FALSE)
  opened <- .test_open_pdf(1.2, 0.8)
  expect_error(print(co), "drawing problems in panels")
  .test_close_quietly(opened)
  # Panels too narrow to draw are a collected problem too.
  opened <- .test_open_pdf(0.8, 2)
  error <- tryCatch(print(co), error = function(e) e)
  expect_s3_class(error, "metrodag_render_error")
  expect_match(conditionMessage(error), "too narrow")
  expect_identical(error$issues$type, "comparison_narrow")
  expect_equal(grid::current.viewport()$name, "ROOT")
  .test_close_quietly(opened)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  warnings <- character()
  withCallingHandlers(expect_error(save_metro_dag(file, co, width = 20), "too narrow"),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("locked", warnings, fixed = TRUE)))
  report <- diagnose_metro_dag(co, width = 20)
  expect_false(report$ok)
  expect_true("comparison_narrow" %in% report$issues$type)
  # The tolerant setting does not draw panels that are too narrow either.
  tolerant <- compare_metro_dags(.test_dense(), list(list(), list()),
    labels = FALSE, on_error = "warn")
  opened <- .test_open_pdf(0.8, 2)
  expect_error(print(tolerant), "too narrow")
  .test_close_quietly(opened)
})

test_that("plots and comparisons can be drawn again on a new page", {
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y")
  co <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  dense <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  opened <- .test_open_pdf(7, 5)
  for (i in 1:2) {
    expect_no_warning(print(p))
    grid::grid.newpage()
    expect_no_warning(print(co))
    grid::grid.newpage()
    expect_no_warning(grid::grid.draw(p))
  }
  # A failed drawing does not stop the next one.
  expect_error(print(dense, vp = grid::viewport(width = 0.05, height = 0.05)), "drawing problems")
  expect_equal(grid::current.viewport()$name, "ROOT")
  grid::grid.newpage()
  expect_no_warning(print(p))
  expect_no_warning(print(co, newpage = TRUE))
  .test_close_quietly(opened)
  expect_false(opened$device %in% grDevices::dev.list())
})

test_that("tolerant plots warn once after drawing", {
  tolerant <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE, on_error = "warn")
  opened <- .test_open_pdf(0.6, 0.6)
  warnings <- list()
  withCallingHandlers(print(tolerant),
    warning = function(w) { warnings[[length(warnings) + 1L]] <<- w; invokeRestart("muffleWarning") })
  problems <- Filter(function(w) grepl("drawn without the affected elements", conditionMessage(w)), warnings)
  expect_length(problems, 1L)
  expect_s3_class(problems[[1L]], "metrodag_render_warning")
  expect_true(all(problems[[1L]]$issues$severity == "error"))
  .test_close_quietly(opened)
})

# Draw `plot` on a recording device of this size and return the recorded
# figure, whatever drawing reported.
.test_record <- function(plot, width, height) {
  grDevices::pdf(NULL, width = width, height = height)
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  try(suppressWarnings(print(plot)), silent = TRUE)
  grDevices::recordPlot()
}

# Replay `recorded` on a new device of this size and return the conditions it
# raised. An error raised while replaying leaves that device locked, as for
# any grid drawing, so its "Killing locked device" warning is dropped.
.test_replay <- function(recorded, width, height) {
  # Record first: recording opens and closes its own device, after which R
  # would make any other open device current instead of the replay device.
  force(recorded)
  grDevices::pdf(NULL, width = width, height = height)
  device <- grDevices::dev.cur()
  on.exit(withCallingHandlers(grDevices::dev.off(device), warning = function(w)
    if (grepl("locked", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")))
  conditions <- list()
  tryCatch(withCallingHandlers(grDevices::replayPlot(recorded), warning = function(w) {
    conditions[[length(conditions) + 1L]] <<- w
    invokeRestart("muffleWarning")
  }), error = function(e) conditions[[length(conditions) + 1L]] <<- e)
  conditions
}

test_that("a replayed figure reports problems at a new size, as a fresh draw does", {
  p <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  # Printed where it fits, then replayed where it does not (a resized window,
  # dev.copy(), dev.print() and recordPlot()/replayPlot() all replay).
  replayed <- .test_replay(.test_record(p, 7, 5), 0.6, 0.6)
  expect_length(replayed, 1L)
  expect_s3_class(replayed[[1L]], "error")
  expect_match(conditionMessage(replayed[[1L]]), "too small|beyond")
  co <- compare_metro_dags(.test_dense(), list(list(), list()), labels = FALSE)
  replayed <- .test_replay(.test_record(co, 7, 5), 1.2, 0.8)
  expect_true(any(vapply(replayed, inherits, logical(1), "error")))
  # Label warnings come back too.
  labels <- c(C = "Socioeconomic background", X = "Cigarette consumption", M = "Tar deposit in lung",
    Y = "Lung cancer risk", U = "Unmeasured inheritance", S = "Occupational hazards")
  q <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y", adjusted = "C",
    unobserved = "U", labels = labels)
  replayed <- .test_replay(.test_record(q, 7, 5), 3.3, 2.6)
  expect_gte(length(replayed), 1L)
  expect_true(all(vapply(replayed, inherits, logical(1), "warning")))
  # Which label problem comes first depends on the ggplot2 version.
  expect_true(any(vapply(replayed, function(w) inherits(w, "metrodag_render_warning") &&
    any(startsWith(w$issues$type, "label_")), logical(1))))
  # A failed print replays where it fits without a message.
  expect_length(.test_replay(.test_record(p, 0.6, 0.6), 7, 5), 0L)
})

test_that("a figure replayed at the size it was drawn does not report again", {
  # Its problems were reported when it was drawn; knitr replays each figure
  # at the chunk's size to save it, and must not fail or warn again.
  p <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  expect_length(.test_replay(.test_record(p, 0.6, 0.6), 0.6, 0.6), 0L)
  co <- compare_metro_dags(.test_dense(), list(list(), list()), labels = FALSE)
  expect_length(.test_replay(.test_record(co, 1.2, 0.8), 1.2, 0.8), 0L)
  tolerant <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE, on_error = "warn")
  expect_length(.test_replay(.test_record(tolerant, 0.6, 0.6), 0.6, 0.6), 0L)
})

test_that("knitr keeps a chunk's drawing error and saves its figure", {
  skip_if_not_installed("knitr")
  directory <- tempfile()
  dir.create(directory)
  options <- knitr::opts_chunk$get()
  previous <- ggplot2::last_plot()
  on.exit({
    knitr::opts_chunk$set(options)
    ggplot2::set_last_plot(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  knitr::opts_chunk$set(dev = "png", fig.path = paste0(directory, "/plot-"))
  p <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  warnings <- character()
  output <- withCallingHandlers(knitr::knit(text = c(
    "```{r small, error = TRUE, fig.width = 0.6, fig.height = 0.6}", "p", "```",
    "```{r after}", "1 + 1", "```"), envir = environment(), quiet = TRUE),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_match(paste(output, collapse = "\n"), "drawing problems at this size")
  expect_match(paste(output, collapse = "\n"), "## [1] 2", fixed = TRUE)
  expect_length(warnings, 0L)
  # The part of the figure that was drawn is saved and shown with the error.
  expect_length(list.files(directory, pattern = "^plot-small"), 1L)
  expect_match(paste(output, collapse = "\n"), "plot-small-1.png", fixed = TRUE)
})

test_that("knitr saves no blank image for a figure that an error stopped entirely", {
  skip_if_not_installed("knitr")
  directory <- tempfile()
  dir.create(directory)
  options <- knitr::opts_chunk$get()
  previous <- ggplot2::last_plot()
  on.exit({
    knitr::opts_chunk$set(options)
    ggplot2::set_last_plot(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  knitr::opts_chunk$set(dev = "png", fig.path = paste0(directory, "/plot-"))
  bad <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE) +
    ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1))
  narrow <- compare_metro_dags(.test_dense(), list(list(), list()), labels = FALSE)
  warnings <- character()
  output <- withCallingHandlers(knitr::knit(text = c(
    "```{r bad, error = TRUE}", "bad", "```",
    "```{r narrow, error = TRUE, fig.width = 0.8, fig.height = 2}", "narrow", "```",
    "```{r grob, error = TRUE}", "grid::grid.newpage()", "grid::grid.draw(as_metro_grob(bad))", "```"),
    envir = environment(), quiet = TRUE),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  output <- paste(output, collapse = "\n")
  expect_match(output, "missing_column")
  expect_match(output, "too narrow")
  expect_length(list.files(directory), 0L)
  expect_false(grepl("![", output, fixed = TRUE))
  expect_length(warnings, 0L)
})

test_that("a grob from as_metro_grob() raises its problems after drawing", {
  dense <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  opened <- .test_open_pdf(0.6, 0.6)
  error <- tryCatch(grid::grid.draw(as_metro_grob(dense)), error = function(e) e)
  expect_s3_class(error, "metrodag_render_error")
  expect_match(conditionMessage(error), "drawing problems at this size")
  .test_close_quietly(opened)
  co <- compare_metro_dags(.test_dense(), list(list(), list()), labels = FALSE)
  opened <- .test_open_pdf(1.2, 0.8)
  expect_error(grid::grid.draw(as_metro_grob(co)), "drawing problems in panels", class = "metrodag_render_error")
  .test_close_quietly(opened)
  # Where it fits, it draws without a message.
  opened <- .test_open_pdf(7, 5)
  expect_no_warning(grid::grid.draw(as_metro_grob(metro_dag(metrodag_examples("chain")))))
  .test_close_quietly(opened)
})

test_that("an error from an added layer is raised as it was", {
  chain <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  bad <- chain + ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1))
  # The condition ggplot2 raises for this plot when it is built.
  expected <- tryCatch(ggplot2::ggplot_build(bad), error = function(e) e)
  expect_s3_class(expected, "error")
  same <- function(error) {
    expect_identical(class(error), class(expected))
    expect_identical(conditionMessage(error), conditionMessage(expected))
    expect_identical(conditionCall(error), conditionCall(expected))
    expect_identical(class(error$parent), class(expected$parent))
  }
  opened <- .test_open_pdf(7, 5)
  same(tryCatch(print(bad), error = function(e) e))
  same(tryCatch(grid::grid.draw(bad), error = function(e) e))
  .test_close_quietly(opened)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  same(tryCatch(ggplot2::ggsave(file, bad, width = 85, height = 60, units = "mm"), error = function(e) e))
  co <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  co$plots[[2L]] <- co$plots[[2L]] +
    ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1))
  opened <- .test_open_pdf(7, 5)
  same(tryCatch(print(co), error = function(e) e))
  .test_close_quietly(opened)
})

test_that("a failed figure adds no page and draws nothing again", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  good <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  bad <- good + ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1))
  narrow <- compare_metro_dags(.test_dense(), list(list(), list()), labels = FALSE)
  # A device keeping a display list, as knitr's and interactive devices do.
  grDevices::png(file.path(directory, "page%03d.png"), width = 400, height = 300)
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE, after = FALSE)
  grDevices::dev.control("enable")
  print(good)
  expect_error(print(bad), "missing_column")
  expect_error(print(narrow, vp = grid::viewport(width = 0.08)), "too narrow")
  print(good)
  # Earlier content on the page is not drawn again, so its warnings stay its own.
  graphics::plot(1:3)
  suppressWarnings(graphics::arrows(2, 2, 2, 2))
  warnings <- character()
  withCallingHandlers(expect_error(print(bad, newpage = FALSE), "missing_column"),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_length(warnings, 0L)
  expect_no_warning(grDevices::dev.off(device))
  # One page per new page: good, the page that `bad` started (`narrow` was
  # placed on it), good again, and the base plot.
  expect_length(list.files(directory, pattern = "^page"), 4L)
})

test_that("plots and comparisons without a MetroDAG layer still draw", {
  p <- metro_dag(metrodag_examples("chain"))
  p$layers <- list()
  co <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  co$plots <- lapply(co$plots, function(q) { q$layers <- list(); q })
  opened <- .test_open_pdf(7, 5)
  expect_no_error(print(p))
  expect_no_error(grid::grid.draw(as_metro_grob(p)))
  expect_no_error(print(co))
  .test_close_quietly(opened)
})

test_that("a recorded grob drawn again where it does not fit reports its problems", {
  dense <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  opened <- .test_open_pdf(7, 5)
  g <- as_metro_grob(dense)
  g$name <- "dag"
  expect_no_warning(grid::grid.draw(g))
  copy <- grid::grid.get("dag")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = 0.05, height = 0.05))
  expect_error(grid::grid.draw(copy), "drawing problems", class = "metrodag_render_error")
  grid::upViewport()
  .test_close_quietly(opened)
})

test_that("grid redraws a figure in place without reporting its problems again", {
  dense <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE)
  tolerant <- metro_dag(.test_dense(), labels = FALSE, legend = FALSE, on_error = "warn")
  opened <- .test_open_pdf(2.4, 1.8)
  grDevices::dev.control("enable")
  expect_error(print(dense), class = "metrodag_render_error")
  grid::grid.text("note", name = "note", x = 0.1, y = 0.95)
  # grid.grab(), grid.edit() and grid.refresh() redraw the page in place.
  expect_s3_class(grid::grid.grab(), "gTree")
  expect_no_error(grid::grid.edit("note", label = "edited"))
  expect_no_error(grid::grid.refresh())
  grid::grid.newpage()
  warnings <- 0L
  withCallingHandlers({
    print(tolerant)
    grid::grid.grab()
  }, warning = function(w) { warnings <<- warnings + 1L; invokeRestart("muffleWarning") })
  expect_identical(warnings, 1L)
  .test_close_quietly(opened)
})

test_that("a grob with its own graphical parameters is laid out with them", {
  skip_if_not_installed("svglite")
  long <- metro_dag(metrodag_examples("chain"), legend = FALSE) +
    ggplot2::labs(title = "A rather long title that will need to be wrapped across several lines at small widths")
  # The first line of the title as drawn.
  first_line <- function(grob) {
    file <- tempfile(fileext = ".svg")
    on.exit(unlink(file))
    svglite::svglite(file, width = 160 / 25.4, height = 90 / 25.4)
    device <- grDevices::dev.cur()
    grid::grid.draw(grob)
    grDevices::dev.off(device)
    svg <- readLines(file)
    texts <- sub("^>(.*)</text>$", "\\1", regmatches(svg, regexpr(">[^<]*</text>", svg)))
    nchar(grep("^A rather", texts, value = TRUE)[1L])
  }
  plain <- first_line(as_metro_grob(long))
  large <- first_line(grid::editGrob(as_metro_grob(long), gp = grid::gpar(cex = 1.6)))
  # Larger text is wrapped to a shorter first line.
  expect_lt(large, plain)
})
