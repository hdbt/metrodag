test_that("default ggsave keeps every panel of the last comparison", {
  skip_if_not_installed("svglite")
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  previous <- ggplot2::last_plot()
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  co <- compare_metro_dags(metrodag_examples("chain"),
    list(First = list(), Second = list()), labels = FALSE)
  expect_identical(ggplot2::last_plot(), co)
  expect_no_warning(ggplot2::ggsave(files[1L], width = 180, height = 80, units = "mm"))
  # Printing must restore the comparison even after another plot was created.
  metro_dag(metrodag_examples("diamond"), labels = FALSE)
  svglite::svglite(files[2L], width = 180 / 25.4, height = 80 / 25.4)
  tryCatch(print(co), finally = grDevices::dev.off())
  expect_identical(ggplot2::last_plot(), co)
  expect_no_warning(ggplot2::ggsave(files[2L], width = 180, height = 80, units = "mm"))
  for (file in files) {
    svg <- readLines(file)
    expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 6L)
    expect_true(any(grepl(">First</text>", svg, fixed = TRUE)))
    expect_true(any(grepl(">Second</text>", svg, fixed = TRUE)))
  }
})

test_that("cropped graphs are errors even when every training row is censored", {
  p <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  changes <- list(
    ggplot2::coord_cartesian(xlim = c(0, 4)),
    ggplot2::coord_cartesian(xlim = c(0, 4), clip = "off"),
    ggplot2::scale_x_continuous(limits = c(0, 4)),
    ggplot2::scale_x_continuous(limits = c(100, 110)),
    ggplot2::scale_x_reverse(limits = c(4, 0)),
    list(ggplot2::coord_flip(), ggplot2::scale_x_reverse(limits = c(4, 0))))
  for (change in changes) {
    q <- suppressMessages(p + change)
    expect_no_warning(report <- diagnose_metro_dag(q, width = 85, height = 55))
    expect_false(report$ok)
    expect_true(any(report$issues$type == "station_bounds" & report$issues$node == "D"))
    expect_true(any(report$issues$type == "edge_bounds"))
  }
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  q <- suppressMessages(p + changes[[1L]])
  expect_error(save_metro_dag(file, q, width = 85, height = 55), "visible figure bounds")
  expect_false(file.exists(file))
})

test_that("station outlines must fit the page or active clipping region", {
  g <- igraph::make_empty_graph(1, directed = TRUE)
  p <- metro_dag(g, labels = FALSE, node_size = 40, legend = FALSE)
  report <- diagnose_metro_dag(p, width = 20, height = 20)
  expect_false(report$ok)
  expect_equal(report$issues$type, "station_bounds")
  expect_equal(report$issues$node, "1")
  expect_true(diagnose_metro_dag(p, width = 60, height = 60)$ok)
  # With clipping off, a station may use its reserved plot margin.
  p <- metro_dag(g, labels = FALSE, node_size = 15, legend = FALSE)
  expect_true(diagnose_metro_dag(p, width = 20, height = 20)$ok)
  q <- suppressMessages(p + ggplot2::coord_equal(clip = "on"))
  expect_false(diagnose_metro_dag(q, width = 20, height = 20)$ok)
})

test_that("route excursions are checked even when every station fits", {
  p <- metro_dag(metrodag_examples("dense"), labels = FALSE, legend = FALSE)
  q <- suppressMessages(p + ggplot2::coord_equal(xlim = c(-3, 17),
    ylim = c(-7, 7), expand = FALSE, clip = "on"))
  report <- diagnose_metro_dag(q, width = 180, height = 140)
  expect_false(report$ok)
  expect_false(any(report$issues$type == "station_bounds"))
  edges <- report$issues[report$issues$type == "edge_bounds", ]
  expect_gt(nrow(edges), 0L)
  expect_true(all(!is.na(edges$from) & !is.na(edges$to)))
})
