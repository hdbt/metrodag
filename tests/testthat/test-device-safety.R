test_that("rendered routes cannot hide behind unrelated stations", {
  skip_if_not_installed("svglite")
  g <- metrodag_examples("random", n = 12, probability = 0.3, seed = 5)
  p <- metro_dag(g, labels = FALSE)
  expect_equal(layout_metrics(attr(p, "metrodag_layout"))$node_edge_collisions, 0)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  report <- diagnose_metro_dag(p, width = 85, device = "svg")
  expect_true(any(with(report$issues, type == "edge_station" & from == "v7" & to == "v12" & node == "v4")))
  expect_error(save_metro_dag(file, p, width = 85), "station")
  expect_false(file.exists(file))
  expect_no_warning(save_metro_dag(file, p, width = 340, height = 220))
  expect_equal(.svg_count(file, "polygon"), igraph::ecount(g))
})

test_that("clearance includes station shape, stroke and arrowhead interiors", {
  node <- data.frame(x = 0, y = 0, shape = 21L)
  diagonal <- rbind(c(-2, 0), c(0, 2))
  expect_length(.route_station_collisions(diagonal, node, 1, integer(), 0), 0)
  node$shape <- 22L
  expect_identical(.route_station_collisions(diagonal, node, 1, integer(), 0), 1L)
  node <- data.frame(x = 0, y = 1.2, shape = 21L)
  route <- rbind(c(-5, 0), c(5, 0))
  expect_length(.route_station_collisions(route, node, 1, integer(), 0.1), 0)
  expect_identical(.route_station_collisions(route, node, 1, integer(), 0.5), 1L)
  route <- rbind(c(0, 0), c(10, 0))
  for (shape in c(21L, 22L)) {
    node <- data.frame(x = 8.3, y = 0.7, shape = shape)
    expect_length(.route_station_collisions(route, node, 0.02, integer(), 0.01), 0)
    expect_identical(.route_station_collisions(route, node, 0.02, integer(), 0.01, 2), 1L)
    expect_length(.route_station_collisions(route, node, 0.02, 1L, 0.01, 2), 0)
  }
})

test_that("path exports preserve existing files on failure and replace on success", {
  skip_if_not_installed("svglite")
  directory <- paste0(tempfile(), " with spaces")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  p <- metro_dag(cbind(1:17, 2:18), labels = FALSE)
  for (ext in c("svg", "png", "pdf")) {
    name <- paste0(basename(tempfile()), ".", ext)
    file <- file.path(directory, name)
    expect_false(file.exists(name))
    expect_identical(save_metro_dag(name, p, path = directory, width = 180, dpi = 72), file)
    expect_false(file.exists(name))
    expect_gt(file.info(file)$size, 100)
    original <- readBin(file, "raw", n = file.info(file)$size)
    expect_error(save_metro_dag(name, p, path = directory, width = 85, dpi = 72), "too small to draw edge")
    expect_identical(readBin(file, "raw", n = file.info(file)$size), original)
    expect_no_warning(save_metro_dag(name, metro_dag(metrodag_examples("chain")), path = directory, dpi = 72))
    expect_false(identical(readBin(file, "raw", n = file.info(file)$size), original))
  }
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 3L)
  expect_error(save_metro_dag("dag.svg", p, path = NA_character_), "output directory")
  expect_error(save_metro_dag("dag.svg", p, path = file.path(directory, "missing")), "does not exist")
})

test_that("a failed export of any kind leaves an existing figure untouched", {
  skip_if_not_installed("svglite")
  # Each kind of export failure, in every vector and raster format: a layout
  # rejected before drawing, a figure too small to draw, a comparison too
  # narrow for its panels, drawing problems in a composition (drawn without the
  # size preflight) and stations outside the visible figure. The tests of
  # these errors check the messages; this one checks the existing file and
  # that no staging file is left behind.
  chain <- layout_metro_dag(metrodag_examples("chain"))
  broken <- chain
  broken$edges <- broken$edges[broken$edges$segment != 1L, ]
  p <- metro_dag(chain, labels = FALSE, legend = FALSE)
  failures <- list(
    list(broken, list(width = 120), "Layout routes must be continuous"),
    list(metro_dag(cbind(1:17, 2:18), labels = FALSE), list(width = 85), "too small to draw edge"),
    list(compare_metro_dags(chain, list(list(), list()), labels = FALSE), list(width = 15),
      "too narrow"),
    list(suppressMessages(p + ggplot2::coord_cartesian(xlim = c(0, 1))),
      list(width = 85, height = 55), "visible figure bounds"))
  if (requireNamespace("patchwork", quietly = TRUE))
    failures <- c(failures, list(list(p | p, list(width = 30, height = 15), "drawing problems")))
  for (ext in c("svg", "pdf", "png")) for (failure in failures) {
    file <- tempfile(fileext = paste0(".", ext))
    .expect_failed_export_keeps_file(file, function(file) suppressWarnings(do.call(save_metro_dag,
      c(list(file, failure[[1L]], dpi = 72), failure[[2L]]))), failure[[3L]],
      info = paste(ext, failure[[3L]]))
    unlink(file)
  }
})

test_that("ordinary ggsave and viewport printing wrap annotations at draw time", {
  skip_if_not_installed("svglite")
  p <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D", adjusted = c("B", "C"),
    estimand = "Total effect of smoking on lung cancer risk") +
    ggplot2::labs(caption = "A long supplementary caption that also needs to fit within a single journal column.")
  original <- p$labels
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  expect_s3_class(p, "metrodag_plot")
  expect_no_warning(ggplot2::ggsave(file, p, width = 85, height = 85, units = "mm"))
  bounds <- .svg_text_bounds(file)
  expect_true(all(bounds$left >= -0.1))
  expect_true(all(bounds$right <= 85 / 25.4 * 72 + 0.1))
  expect_gt(sum(grepl("font-weight: bold", bounds$text, fixed = TRUE)), 1L)
  svglite::svglite(file, width = 170 / 25.4, height = 85 / 25.4)
  tryCatch({
    expect_no_warning(print(p, newpage = TRUE,
      vp = grid::viewport(x = 0.75, width = 0.5)))
    expect_true(identical(ggplot2::last_plot(), p))
  }, finally = grDevices::dev.off())
  bounds <- .svg_text_bounds(file)
  expect_true(all(bounds$left >= 85 / 25.4 * 72 - 0.1))
  expect_true(all(bounds$right <= 170 / 25.4 * 72 + 0.1))
  expect_identical(p$labels, original)
})

test_that("long labels fit zero and asymmetric margins, including ordinary ggsave", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  for (margin in list(c(0, 0, 0, 0), c(2, 0, 4, 12), c(2, 12, 4, 0))) {
    p <- metro_dag(metrodag_examples("chain"), labels = c(A = "Socioeconomic background"),
      label_wrap = NULL) + ggplot2::theme(plot.margin = ggplot2::margin(
        margin[1], margin[2], margin[3], margin[4], unit = "mm"))
    expect_no_warning(save_metro_dag(file, p, width = 85))
    bounds <- .svg_text_bounds(file)
    expect_true(all(bounds$left >= -0.1))
    expect_true(all(bounds$right <= 85 / 25.4 * 72 + 0.1))
    expect_no_warning(ggplot2::ggsave(file, p, width = 85, height = 55, units = "mm"))
    bounds <- .svg_text_bounds(file)
    expect_true(all(bounds$left >= -0.1))
    expect_true(all(bounds$right <= 85 / 25.4 * 72 + 0.1))
  }
  p <- metro_dag(metrodag_examples("chain"), label_nudge = list(A = c(-40, 0))) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  expect_warning(save_metro_dag(file, p, width = 85), "beyond the figure margin")
})

test_that("consecutive notebook plots are both recorded and replayed", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("svglite")
  directory <- tempfile()
  dir.create(directory)
  previous <- knitr::opts_chunk$get()
  on.exit({
    knitr::opts_chunk$set(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  knitr::opts_chunk$set(dev = "svglite", fig.path = paste0(directory, "/plot-"))
  p <- metro_dag(metrodag_examples("chain"))
  knitr::knit(text = c("```{r}", "p", "p + ggplot2::labs(title = 'Second plot')", "```"),
    envir = environment(), quiet = TRUE)
  files <- list.files(directory, pattern = "[.]svg$", full.names = TRUE)
  expect_length(files, 2L)
  for (file in files)
    expect_equal(.svg_count(file, "polygon"), 3L)
})
