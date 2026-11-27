test_that("layout exports preserve the plot used by the next default ggsave", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  graph <- metrodag_examples("chain")
  fixed <- layout_metro_dag(graph, direction = "right")
  co <- compare_metro_dags(graph, list(First = list(), Second = list()), labels = FALSE)
  expect_no_warning(save_metro_dag(files[1L], fixed, width = 180))
  expect_identical(ggplot2::last_plot(), co)
  expect_no_warning(ggplot2::ggsave(files[2L], width = 180, height = 90, units = "mm"))
  expect_equal(.svg_count(files[2L], "polygon"), 6L)
  expect_error(save_metro_dag(tempfile(fileext = ".html"), fixed), "Use a .pdf")
  expect_identical(ggplot2::last_plot(), co)
  expect_error(save_metro_dag(files[1L], fixed, width = 25, height = 55), "too small to draw edge")
  expect_identical(ggplot2::last_plot(), co)
  # Constructing a plot explicitly in the argument still registers that plot.
  created <- NULL
  expect_no_warning(save_metro_dag(files[1L], {
    created <- metro_dag(graph, labels = FALSE)
    created
  }))
  expect_identical(ggplot2::last_plot(), created)
})

test_that("failed comparisons do not register intermediate plots", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  graph <- metrodag_examples("chain")
  fixed <- layout_metro_dag(graph, direction = "right")
  original <- compare_metro_dags(graph, list(list(), list()), labels = FALSE)
  for (input in list(graph, fixed)) for (saved in list(original, NULL)) {
    ggplot2::set_last_plot(saved)
    expect_error(compare_metro_dags(input,
      list(Good = list(), Bad = list(exposure = "missing"))), "`exposure` names a node not in the graph")
    expect_identical(ggplot2::last_plot(), saved)
  }
  next_plot <- compare_metro_dags(fixed, list(list(), list()), labels = FALSE)
  expect_identical(ggplot2::last_plot(), next_plot)
})

test_that("comparison printing respects page and viewport arguments", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  previous <- ggplot2::last_plot()
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  co <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  svglite::svglite(file, width = 240 / 25.4, height = 100 / 25.4)
  tryCatch({
    grid::grid.newpage()
    grid::grid.text("Composition header", y = 0.97)
    for (x in c(0.25, 0.75)) expect_no_warning(print(co, newpage = FALSE,
      vp = grid::viewport(x = x, y = 0.4, width = 0.5, height = 0.8)))
    expect_equal(grid::current.viewport()$name, "ROOT")
  }, finally = grDevices::dev.off())
  svg <- readLines(file)
  expect_true(any(grepl("Composition header", svg, fixed = TRUE)))
  expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 12L)
  circles <- svg[grepl("<circle", svg, fixed = TRUE)]
  x <- as.numeric(sub(".* cx='([^']+)'.*", "\\1", circles))
  expect_length(x, 16L)
  midpoint <- 120 / 25.4 * 72
  expect_true(all(head(x, 8L) < midpoint))
  expect_true(all(tail(x, 8L) > midpoint))
  svglite::svglite(file, width = 240 / 25.4, height = 100 / 25.4)
  device <- grDevices::dev.cur()
  # Closed by the check below; this only covers a test that stops early.
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE, after = FALSE)
  grid::pushViewport(grid::viewport(name = "comparison-right", x = 0.75, width = 0.5))
  grid::upViewport()
  expect_no_warning(print(co, vp = "comparison-right"))
  expect_equal(grid::current.viewport()$name, "ROOT")
  expect_error(print(co, vp = grid::viewport(width = 0.08)), "too narrow")
  expect_equal(grid::current.viewport()$name, "ROOT")
  # The error was raised after drawing, so the device was not left locked.
  expect_no_warning(grDevices::dev.off(device))
  expect_identical(ggplot2::last_plot(), co)
})

test_that("blank display labels retain covariate identities in adjustment subtitles", {
  graph <- metrodag_examples("chain")
  p <- metro_dag(graph, adjusted = c("B", "C"), labels = c(B = "", C = "  \t"))
  expect_equal(p$labels$subtitle, "Adjusted for: B, C")
  nodes <- p$layers[[2L]]$geom_params$nodes
  expect_equal(nodes$label[nodes$node == "B"], "")
  expect_true(all(nodes$shape[nodes$node %in% c("B", "C")] == 22L))
  igraph::V(graph)$label <- c("Exposure", "", "  \t", "Outcome")
  for (labels in list(TRUE, FALSE)) {
    p <- metro_dag(graph, adjusted = c("B", "C"), labels = labels)
    expect_equal(p$labels$subtitle, "Adjusted for: B, C")
    expect_setequal(attr(p, "metrodag_analysis")$adjusted, c("B", "C"))
  }
  p <- metro_dag(graph, adjusted = c("B", "C"), labels = c(B = "Age", C = ""))
  expect_equal(p$labels$subtitle, "Adjusted for: Age, C")
})

test_that("consecutive comparison plots are recorded independently in notebooks", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("svglite")
  directory <- tempfile()
  dir.create(directory)
  options <- knitr::opts_chunk$get()
  previous <- ggplot2::last_plot()
  on.exit({
    knitr::opts_chunk$set(options)
    ggplot2::set_last_plot(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  knitr::opts_chunk$set(dev = "svglite", fig.path = paste0(directory, "/plot-"))
  a <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  b <- compare_metro_dags(metrodag_examples("diamond"), list(list(), list()), labels = FALSE)
  knitr::knit(text = c("```{r}", "a", "b", "```"), envir = environment(), quiet = TRUE)
  files <- list.files(directory, pattern = "[.]svg$", full.names = TRUE)
  expect_length(files, 2L)
  counts <- vapply(files, function(file) .svg_count(file, "polygon"), integer(1))
  expect_equal(unname(counts), c(6L, 8L))
})

test_that("the first plot created inside a notebook embeds a complete figure", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("svglite")
  skip_if_not(rmarkdown::pandoc_available())
  directory <- tempfile()
  dir.create(directory)
  previous <- ggplot2::last_plot()
  on.exit({ unlink(directory, recursive = TRUE); ggplot2::set_last_plot(previous) }, add = TRUE)
  input <- file.path(directory, "first-figure.Rmd")
  writeLines(c("---", "title: First figure", "output: rmarkdown::html_vignette",
    "vignette: >", "  %\\VignetteIndexEntry{First figure}", "  %\\VignetteEngine{knitr::rmarkdown}",
    "  %\\VignetteEncoding{UTF-8}", "---",
    "```{r, dev='svglite', fig.cap='First MetroDAG figure'}",
    "p <- metro_dag(metrodag_examples('causal'), exposure='X', outcome='Y', unobserved='U')",
    "p", "```", "```{r}", "diagnose_metro_dag(p, width=180, device='pdf')", "```"), input)
  output <- rmarkdown::render(input, envir = environment(), clean = FALSE, quiet = TRUE)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  sources <- regmatches(html, gregexpr('data:image/svg[+]xml;base64,[^"]*', html))[[1L]]
  expect_length(sources, 1L)
  expect_gt(nchar(sources[1L]), 100L)
  figures <- list.files(directory, pattern = "[.]svg$", recursive = TRUE, full.names = TRUE)
  expect_length(figures, 1L)
  expect_equal(.svg_arrowheads(figures[1L]), 8L)
})
