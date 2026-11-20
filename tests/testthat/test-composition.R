# Compositions only arrange drawn figures: each example is laid out once.
.composition_causal <- .test_once(function() layout_metro_dag(metrodag_examples("causal")))
.composition_dense <- .test_once(function() layout_metro_dag(metrodag_examples("dense")))

.draw_to_pdf <- function(draw, width = 6, height = 4) {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file, width = width, height = height)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  draw()
  invisible(TRUE)
}

test_that("panels drawn through ggplotGrob keep the plot's label margins", {
  p <- metro_dag(.composition_causal(), exposure = "X", outcome = "Y",
    labels = c(C = "Socioeconomic background", Y = "Lung cancer risk",
      S = "Occupational hazards", U = "Unmeasured inheritance"))
  config <- p$layers[[2L]]$geom_params$config
  expect_equal(grid::convertUnit(config$label_margin_default, "mm", valueOnly = TRUE), rep(5, 4))
  expect_no_warning(.draw_to_pdf(function() grid::grid.draw(ggplot2::ggplotGrob(p))))
  headers <- metro_dag(data.frame(from = c("A", "B"), to = c("B", "C")),
    rank = c(A = 0, B = 1, C = 2), rank_labels = c("One", "Two", "Three"))
  # Headers are axis labels outside the panel; label room is unchanged.
  margin <- grid::convertUnit(headers$layers[[2L]]$geom_params$config$label_margin_default, "mm", valueOnly = TRUE)
  expect_equal(margin, rep(5, 4))
  expect_no_warning(.draw_to_pdf(function() grid::grid.draw(ggplot2::ggplotGrob(headers))))
})

test_that("MetroDAG figures compose with patchwork and as_metro_grob()", {
  skip_if_not_installed("patchwork")
  layout <- .composition_causal()
  a <- metro_dag(layout, exposure = "X", outcome = "Y", legend = FALSE)
  b <- metro_dag(layout, exposure = "M", outcome = "Y", legend = FALSE)
  expect_no_warning(.draw_to_pdf(function() print(patchwork::wrap_plots(a, b)), width = 9))
  long <- a + ggplot2::labs(title = paste(rep("A long title that must wrap", 5), collapse = " "))
  composed <- patchwork::wrap_elements(full = as_metro_grob(long)) + b
  expect_no_warning(.draw_to_pdf(function() print(composed), width = 9))
  g <- as_metro_grob(a)
  expect_s3_class(g, "metrodag_plot_grob")
  expect_s3_class(as_metro_grob(layout), "metrodag_plot_grob")
  comparison <- compare_metro_dags(layout, list(A = list(exposure = "X", outcome = "Y")))
  expect_s3_class(as_metro_grob(comparison), "metrodag_comparison_grob")
  expect_error(as_metro_grob(ggplot2::ggplot()), "MetroDAG plot")
})

test_that("exported compositions keep every panel and stay strict", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("svglite")
  layout <- .composition_causal()
  a <- metro_dag(layout, exposure = "X", outcome = "Y", legend = FALSE)
  b <- metro_dag(metrodag_examples("chain"), legend = FALSE)
  scatter <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point() + ggplot2::labs(title = "Scatter")
  everything <- sort(c("A", "B", "C", "D", "M", "S", "U", "X", "Y"))
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  compositions <- list(a | b, b + a, patchwork::wrap_plots(a, b),
    patchwork::wrap_elements(full = as_metro_grob(a)) + b,
    b + patchwork::wrap_elements(full = as_metro_grob(a)))
  # CRAN checks one operator composition and one wrapped grob; the other
  # orders and constructors run in the full suite.
  if (.test_on_cran()) compositions <- compositions[c(1L, 4L)]
  for (composed in compositions) {
    # patchwork copies the last plot's class: every panel must still be drawn.
    ggplot2::ggsave(file, composed, width = 9, height = 4)
    expect_identical(.svg_texts(file), everything)
    save_metro_dag(file, composed, height = 100)
    expect_identical(.svg_texts(file), everything)
    report <- diagnose_metro_dag(composed, height = 100, device = "svg")
    expect_true(report$ok)
    expect_identical(sort(unique(report$panels$panel)), c("1", "2"))
  }
  save_metro_dag(file, scatter + a, height = 100)
  expect_true(all(c("Scatter", "X", "Y") %in% .svg_texts(file)))
  expect_error(save_metro_dag(file, a | b), "Supply `height`")
  expect_error(save_metro_dag(file, scatter + scatter, height = 100), "no MetroDAG plot")
  # patchwork intercepts drawing errors; exports report them after drawing.
  unlink(file)
  dense <- metro_dag(.composition_dense(), labels = FALSE, legend = FALSE)
  # Label warnings at this size are expected alongside the error.
  expect_error(suppressWarnings(save_metro_dag(file, a | dense, width = 30, height = 15)),
    "drawing problems in panels? [12].*(too small|beyond)")
  expect_false(file.exists(file))
  report <- diagnose_metro_dag(a | dense, width = 30, height = 15)
  expect_false(report$ok)
  expect_true(all(report$issues$panel[report$issues$severity == "error"] %in% c("1", "2")))
  messages <- character()
  withCallingHandlers(save_metro_dag(file, a | dense, width = 30, height = 15, on_error = "warn"),
    warning = function(w) { messages <<- c(messages, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("drawn without the affected elements", messages)))
  expect_true(file.exists(file))
})

test_that("on_error = 'warn' draws what fits while exports stay strict", {
  dense <- .composition_dense()
  p <- metro_dag(dense, labels = FALSE, legend = FALSE)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  # Problems are raised once drawing has returned, so the device is not left
  # locked and closing it gives no "Killing locked device" warning.
  grDevices::pdf(file, width = 0.6, height = 0.6)
  device <- grDevices::dev.cur()
  expect_error(print(p), "too small|beyond")
  expect_no_warning(grDevices::dev.off(device))
  tolerant <- metro_dag(dense, labels = FALSE, legend = FALSE, on_error = "warn")
  grDevices::pdf(file, width = 0.6, height = 0.6)
  device <- grDevices::dev.cur()
  expect_warning(print(tolerant), "drawn without the affected elements")
  expect_no_warning(grDevices::dev.off(device))
  unlink(file)
  expect_error(save_metro_dag(file, tolerant, width = 15, height = 15), "too small|beyond")
  expect_false(file.exists(file))
  expect_warning(save_metro_dag(file, tolerant, width = 15, height = 15, on_error = "warn"), "drawn without the affected elements")
  expect_true(file.exists(file))
  expect_false(diagnose_metro_dag(tolerant, width = 15, height = 15)$ok)
  expect_error(metro_dag(metrodag_examples("chain"), on_error = "ignore"), "on_error")
})
