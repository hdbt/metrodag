# compare_metro_dags(legend = FALSE) leaves out the shared legend row.

test_that("comparisons can leave out the shared legend", {
  edges <- data.frame(from = c("C", "C", "X", "M"), to = c("X", "Y", "M", "Y"))
  views <- list(
    Unadjusted = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C"))
  with <- compare_metro_dags(edges, views)
  without <- compare_metro_dags(edges, views, legend = FALSE)
  expect_false(is.null(with$legend_plot))
  expect_null(without$legend_plot)
  # The panels themselves are the same.
  expect_identical(lapply(without$plots, function(p) p$labels$title),
    lapply(with$plots, function(p) p$labels$title))
  grDevices::pdf(NULL, width = 180 / 25.4, height = 100 / 25.4)
  built <- tryCatch(list(with = .build_metro_comparison(with, 180),
    without = .build_metro_comparison(without, 180)), finally = grDevices::dev.off())
  expect_true("shared-legend" %in% built$with$grob$layout$name)
  expect_false("shared-legend" %in% built$without$grob$layout$name)
  # Only the row of panels remains.
  expect_length(built$without$grob$heights, 1L)
  expect_lt(built$without$height, built$with$height - 5)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  report <- diagnose_metro_dag(without, width = 180)
  expect_true(report$ok)
  expect_lt(unname(report$dimensions["height"]),
    unname(diagnose_metro_dag(with, width = 180)$dimensions["height"]) - 5)
  expect_no_warning(save_metro_dag(file, without, width = 180))
  expect_true(file.exists(file))
})

test_that("legend is a comparison argument, not a view argument", {
  edges <- data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y"))
  expect_error(compare_metro_dags(edges, list(A = list(exposure = "X", legend = FALSE))),
    "compare_metro_dags(legend = FALSE)", fixed = TRUE)
  expect_error(compare_metro_dags(edges, list(A = list(exposure = "X")), legend = NA),
    "`legend` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(compare_metro_dags(edges, list(A = list(exposure = "X")), legend = "none"),
    "`legend` must be TRUE or FALSE.", fixed = TRUE)
  # It must be named in full, as it follows `...`.
  expect_error(compare_metro_dags(edges, list(A = list(exposure = "X")), leg = FALSE), "`leg`")
  # A reused layout takes it too.
  fixed <- attr(metro_dag(edges), "metrodag_layout")
  expect_null(compare_metro_dags(fixed, list(A = list(exposure = "X")), legend = FALSE)$legend_plot)
})
