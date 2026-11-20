test_that("diagnostics retain a plot explicitly created in their argument", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  sentinel <- compare_metro_dags(metrodag_examples("chain"),
    list(list(), list()), labels = FALSE)
  graph <- metrodag_examples("diamond")
  created <- NULL; calls <- 0L
  expect_no_warning(report <- diagnose_metro_dag({
    calls <- calls + 1L
    created <- metro_dag(graph, labels = FALSE) +
      ggplot2::labs(title = "Newly checked graph")
    created
  }, width = 180, device = "svg"))
  expect_true(report$ok)
  expect_identical(calls, 1L)
  expect_identical(ggplot2::last_plot(), created)
  expect_no_warning(ggplot2::ggsave(file, width = 180, height = 90, units = "mm"))
  svg <- readLines(file)
  expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), igraph::ecount(graph))
  expect_true(any(grepl(">Newly checked graph</text>", svg, fixed = TRUE)))
  expect_identical(ggplot2::last_plot(), created)
})

test_that("inline comparisons remain the last plot after clear and failed checks", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  sentinel <- metro_dag(metrodag_examples("diamond"), labels = FALSE)
  graph <- metrodag_examples("chain")
  for (small in c(FALSE, TRUE)) {
    ggplot2::set_last_plot(sentinel)
    created <- NULL
    expect_no_warning(report <- diagnose_metro_dag({
      created <- compare_metro_dags(graph,
        list(First = list(), Second = list()), labels = FALSE)
      created
    }, width = if (small) 50 else 180, height = 80, device = "svg"))
    expect_identical(report$ok, !small)
    if (small) expect_true(any(report$issues$severity == "error"))
    expect_identical(ggplot2::last_plot(), created)
    expect_no_warning(ggplot2::ggsave(file, width = 180, height = 90, units = "mm"))
    svg <- readLines(file)
    expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 2L * igraph::ecount(graph))
    expect_true(any(grepl(">First</text>", svg, fixed = TRUE)))
    expect_true(any(grepl(">Second</text>", svg, fixed = TRUE)))
    expect_identical(ggplot2::last_plot(), created)
  }
})

test_that("diagnosing saved objects preserves the previous last plot", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  graph <- metrodag_examples("chain")
  single <- metro_dag(graph, labels = FALSE)
  comparison <- compare_metro_dags(graph, list(list(), list()), labels = FALSE)
  fixed <- layout_metro_dag(graph, direction = "right")
  original <- fixed
  for (saved in list(single, comparison, NULL)) {
    for (input in list(single, comparison, fixed)) {
      for (small in c(FALSE, TRUE)) {
        ggplot2::set_last_plot(saved)
        expect_no_warning(report <- diagnose_metro_dag(input,
          width = if (small) 20 else 180, height = 80))
        expect_identical(report$ok, !small)
        expect_identical(ggplot2::last_plot(), saved)
      }
    }
  }
  expect_identical(fixed, original)
  expect_null(single$layers[[2L]]$geom_params$config$diagnostic_state)
  expect_true(all(vapply(comparison$plots, function(p)
    is.null(p$layers[[2L]]$geom_params$config$diagnostic_state), logical(1))))
})

test_that("layout conversion retains plots constructed by the caller", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  sentinel <- compare_metro_dags(metrodag_examples("chain"),
    list(list(), list()), labels = FALSE)
  graph <- metrodag_examples("diamond")
  for (small in c(FALSE, TRUE)) {
    ggplot2::set_last_plot(sentinel)
    created <- NULL
    expect_no_warning(report <- diagnose_metro_dag({
      created <- metro_dag(graph, labels = FALSE)
      attr(created, "metrodag_layout")
    }, width = if (small) 20 else 180, height = 80, device = "svg"))
    expect_identical(report$ok, !small)
    expect_identical(ggplot2::last_plot(), created)
    expect_no_warning(ggplot2::ggsave(file, width = 180, height = 90, units = "mm"))
    expect_equal(.svg_count(file, "polygon"), igraph::ecount(graph))
  }
})

test_that("invalid diagnostic options do not evaluate a plot constructor", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  calls <- 0L
  expect_error(diagnose_metro_dag({ calls <- calls + 1L; metro_dag(metrodag_examples("chain")) },
    width = 0), "positive")
  expect_identical(calls, 0L)
  expect_identical(ggplot2::last_plot(), previous)
})
