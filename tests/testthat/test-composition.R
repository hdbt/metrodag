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

# The mark of an incomplete figure in an SVG file written by svglite, its
# lines joined; "" when there is none. Figures without labels, titles or a
# legend have no other text.
.composition_mark <- function(file) paste(gsub("&quot;|&#34;", "\"",
  sub("^<text .*>(.*)</text>$", "\\1", .svg_primitives(file, "text"))), collapse = " ")

# Export `plot` to `file` with on_error = "warn" and count what its warning
# reports: the arrows not drawn and the stations that overlap.
.composition_tolerant <- function(file, plot, width, height) {
  errors <- .empty_metro_issues()
  withCallingHandlers(save_metro_dag(file, plot, width = width, height = height, on_error = "warn"),
    warning = function(w) if (inherits(w, "metrodag_render_warning")) {
      errors <<- rbind(errors, w$issues[w$issues$severity == "error", , drop = FALSE])
      invokeRestart("muffleWarning")
    })
  overlap <- errors[errors$type == "station_overlap", , drop = FALSE]
  c(arrows = nrow(unique(errors[startsWith(errors$type, "edge_"), c("from", "to")])),
    stations = length(unique(c(overlap$node, overlap$other_node))))
}

test_that("an export drawn with on_error = 'warn' is marked as incomplete", {
  skip_if_not_installed("svglite")
  # Without labels, titles or a legend, neither the panel nor the arrows that
  # fit in it depend on font metrics.
  dense <- .composition_dense()
  p <- metro_dag(dense, labels = FALSE, legend = FALSE)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  # A missing arrow would read as "no direct effect": the mark counts the
  # arrows left out of the file.
  found <- .composition_tolerant(file, p, 40, 40)
  expect_gt(found[["arrows"]], 1L)
  expect_identical(found[["stations"]], 0L)
  expect_equal(.svg_count(file, "polygon"), igraph::ecount(dense$graph) - found[["arrows"]])
  expect_identical(.composition_mark(file),
    sprintf("Incomplete: %d arrows not drawn (on_error = \"warn\")", found[["arrows"]]))
  # Where everything fits, nothing is marked.
  expect_identical(.composition_tolerant(file, p, 120, 90), c(arrows = 0L, stations = 0L))
  expect_identical(.composition_mark(file), "")
  # Each kind of problem is counted, in the singular where it applies.
  expect_identical(.metro_incomplete_text(1L, 1L, 2L), c(
    "Incomplete: 1 arrow not drawn, 1 station cut off, 2 stations overlap", "(on_error = \"warn\")"))
  expect_null(.metro_incomplete_text(0L, 0L, 0L))
})

test_that("tolerant previews are marked as exports are, and the mark moves nothing", {
  skip_on_cran()
  skip_if_not_installed("svglite")
  dense <- .composition_dense()
  p <- metro_dag(dense, labels = FALSE, legend = FALSE)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  # Stations that overlap are counted beside the arrows left out.
  found <- .composition_tolerant(file, p, 30, 30)
  expect_gt(found[["stations"]], 1L)
  expect_identical(.composition_mark(file), sprintf(
    "Incomplete: %d arrows not drawn, %d stations overlap (on_error = \"warn\")",
    found[["arrows"]], found[["stations"]]))
  # Strict figures are never marked: one that fits is written as it is, and
  # one that does not fit raises an error after drawing what fits.
  save_metro_dag(file, p, width = 120, height = 90)
  expect_identical(.composition_mark(file), "")
  draw <- function(plot) {
    svglite::svglite(file, width = 40 / 25.4, height = 40 / 25.4)
    device <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(device))
    print(plot)
  }
  expect_error(draw(p), "drawing problems", class = "metrodag_render_error")
  strict <- .svg_primitives(file)
  expect_identical(.composition_mark(file), "")
  # A printed preview is saved as often as an export (by knitr, or from the
  # plot pane), so it is marked too. The mark is drawn over the same figure.
  tolerant <- metro_dag(dense, labels = FALSE, legend = FALSE, on_error = "warn")
  expect_warning(draw(tolerant), "drawn without the affected elements")
  drawn <- .svg_primitives(file)
  missing <- as.integer(igraph::ecount(dense$graph) - .svg_count(file, "polygon"))
  expect_gt(missing, 1L)
  expect_identical(.composition_mark(file),
    sprintf("Incomplete: %d arrows not drawn (on_error = \"warn\")", missing))
  expect_identical(drawn[drawn %in% strict], strict)
  expect_true(all(grepl("^<(rect|text) ", drawn[!drawn %in% strict])))
})
