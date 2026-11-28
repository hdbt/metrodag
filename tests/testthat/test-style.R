# Styles only decorate a layout: the causal example is laid out once for the file.
.style_causal <- .test_once(function() layout_metro_dag(metrodag_examples("causal")))

test_that("metro_style presets reproduce the named styles", {
  layout <- .style_causal()
  for (preset in c("publication", "metro", "monochrome")) {
    named <- metro_dag(layout, exposure = "X", outcome = "Y", adjusted = "C", style = preset)
    object <- metro_dag(layout, exposure = "X", outcome = "Y", adjusted = "C", style = metro_style(preset))
    for (key in c("nodes", "edges")) expect_identical(
      named$layers[[2L]]$geom_params[[key]], object$layers[[2L]]$geom_params[[key]])
    expect_s3_class(attr(named, "metrodag_style"), "metrodag_style")
    expect_identical(attr(named, "metrodag_style")$preset, preset)
  }
  expect_output(print(metro_style("monochrome")), "monochrome")
})

test_that("metro_style overrides reach stations, routes and the legend", {
  layout <- .style_causal()
  custom <- metro_style(exposure_fill = "#1B9E77", outcome_fill = "#7570B3",
    edge_colour = "#101010", node_outline = "#444444", adjusted_fill = "#EEEEEE",
    legend_labels = c(exposure = "Treatment", observed = "Measured"),
    legend_position = "right", station_scaling = "none", arrow_length = 1.6,
    arrow_angle = 30, casing_width = 0, text_colour = "#333333")
  p <- metro_dag(layout, exposure = "X", outcome = "Y", unobserved = "U", style = custom)
  nodes <- p$layers[[2L]]$geom_params$nodes
  edges <- p$layers[[2L]]$geom_params$edges
  expect_equal(nodes$fill[nodes$node == "X"], "#1B9E77")
  expect_equal(nodes$colour[nodes$node == "X"], "#1B9E77")
  expect_equal(nodes$fill[nodes$node == "Y"], "#7570B3")
  expect_equal(nodes$colour[nodes$node == "C"], "#444444")
  expect_true(all(edges$colour == "#101010"))
  expect_equal(p$layers[[2L]]$geom_params$config$text_options$colour, "#333333")
  expect_identical(p$layers[[2L]]$geom_params$config$style$station_scaling, "none")
  labels <- ggplot2::ggplot_build(p)$plot$scales$get_scales("shape")$get_labels()
  expect_setequal(labels, c("Treatment", "Outcome", "Measured", "Unobserved"))
  expect_equal(p$theme$legend.position, "right")
  expect_no_error(ggplot2::ggplotGrob(p))
  # Every scaling rule renders the gallery-sized causal example at 85 mm (on
  # CRAN, the unbounded rule only).
  for (scaling in if (.test_on_cran()) "degree" else c("none", "capped", "degree")) {
    q <- metro_dag(layout, exposure = "X", outcome = "Y",
      style = metro_style(station_scaling = scaling))
    expect_true(diagnose_metro_dag(q, width = 85)$ok)
  }
})

test_that("metro_style validates its inputs", {
  expect_error(metro_style("fancy"), "preset")
  expect_error(metro_style(exposure_fill = "not a colour"), "valid colour")
  expect_error(metro_style(arrow_length = -1), "arrow_length")
  expect_error(metro_style(arrow_angle = 90), "arrow_angle")
  expect_error(metro_style(casing_width = -0.1), "casing_width")
  expect_error(metro_style(station_scaling = "huge"), "station_scaling")
  expect_error(metro_style(legend_labels = c(mediator = "M")), "legend_labels")
  expect_error(metro_style(legend_labels = "Exposure"), "legend_labels")
  expect_error(metro_style(legend_position = "middle"), "legend_position")
  expect_error(metro_style(legend_columns = 0), "legend_columns")
  expect_error(metro_style(palette = character()), "palette")
  layout <- layout_metro_dag(metrodag_examples("chain"))
  expect_error(metro_dag(layout, style = list(preset = "publication")), "metro_style")
  expect_error(metro_dag(layout, style = "fancy"), "style")
})

test_that("comparisons share a custom style across panels and the legend", {
  layout <- .style_causal()
  custom <- metro_style(exposure_fill = "#1B9E77", legend_labels = c(exposure = "Treatment"))
  comparison <- compare_metro_dags(layout, list(
    A = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    B = list(exposure = "X", outcome = "Y", adjusted = "C")), style = custom)
  for (p in comparison$plots) {
    nodes <- p$layers[[2L]]$geom_params$nodes
    expect_equal(nodes$fill[nodes$node == "X"], "#1B9E77")
  }
  values <- attr(comparison$legend_plot, "metrodag_legend_values")
  expect_true("#1B9E77" %in% values$fill)
  labels <- ggplot2::ggplot_build(comparison$legend_plot)$plot$scales$get_scales("shape")$get_labels()
  expect_true("Treatment" %in% labels)
})

test_that("edge_style changes individual arrows only", {
  layout <- .style_causal()
  styled <- list("U->X" = list(linetype = "dotted"), "X->M" = list(colour = "#0072B2", width = 0.9),
    "C->Y" = list(color = "red"))
  p <- metro_dag(layout, exposure = "X", outcome = "Y", unobserved = "U", edge_style = styled)
  e <- p$layers[[2L]]$geom_params$edges
  key <- paste(e$from, e$to, sep = "->")
  expect_true(all(e$linetype[key == "U->X"] == "dotted"))
  expect_true(all(e$linetype[key == "U->C"] == "dashed"))
  expect_true(all(e$colour[key == "X->M"] == "#0072B2"))
  expect_true(all(e$width[key == "X->M"] == 0.9))
  expect_true(all(e$colour[key == "C->Y"] == "red"))
  expect_true(all(e$width[key != "X->M"] == 0.45))
  # The plot records the styles as one table, whichever form was supplied.
  recorded <- attr(p, "metrodag_edge_style")
  expect_identical(recorded, .metro_edge_style_table(styled))
  expect_equal(recorded$colour, c(NA, "#0072B2", "red"))
  plain <- metro_dag(layout, exposure = "X", outcome = "Y", unobserved = "U")
  expect_identical(p$layers[[2L]]$geom_params$nodes, plain$layers[[2L]]$geom_params$nodes)
  expect_identical(attr(p, "metrodag_layout")$edges, attr(plain, "metrodag_layout")$edges)
  expect_true(diagnose_metro_dag(p, width = 85)$ok)
  frame <- data.frame(from = c("U", "X"), to = c("X", "M"), linetype = c("dotted", NA),
    width = c(NA, 0.9))
  q <- metro_dag(layout, edge_style = frame)
  f <- q$layers[[2L]]$geom_params$edges
  expect_true(all(f$width[paste(f$from, f$to, sep = "->") == "X->M"] == 0.9))
  expect_true(all(f$linetype[paste(f$from, f$to, sep = "->") == "U->X"] == "dotted"))
  comparison <- compare_metro_dags(layout, list(
    A = list(exposure = "X", outcome = "Y"), B = list(exposure = "M", outcome = "Y")),
    edge_style = styled)
  for (panel in comparison$plots)
    expect_true(all(panel$layers[[2L]]$geom_params$edges$width[
      paste(panel$layers[[2L]]$geom_params$edges$from, panel$layers[[2L]]$geom_params$edges$to, sep = "->") == "X->M"] == 0.9))
})

test_that("edge_style rejects unknown arrows and malformed specifications", {
  layout <- .style_causal()
  expect_error(metro_dag(layout, edge_style = list("X->Q" = list(width = 1))), "not in the graph")
  expect_error(metro_dag(layout, edge_style = list("XY" = list(width = 1))), "from->to")
  expect_error(metro_dag(layout, edge_style = list("X->Y" = list(size = 1))), "linetype, colour and width")
  expect_error(metro_dag(layout, edge_style = list("X->Y" = list(width = -1))), "positive")
  expect_error(metro_dag(layout, edge_style = list("X->Y" = list(colour = "no such colour"))), "valid colours")
  expect_error(metro_dag(layout, edge_style = list("X->Y" = list(width = 1), "X->Y" = list(width = 2))), "at most once")
  expect_error(metro_dag(layout, edge_style = data.frame(linetype = "dotted")), "from")
  expect_identical(metro_dag(layout, edge_style = list())$layers[[2L]]$geom_params$edges,
    metro_dag(layout)$layers[[2L]]$geom_params$edges)
})

test_that("export width presets and EPS output work", {
  p <- metro_dag(.style_causal(), exposure = "X", outcome = "Y")
  expect_equal(.metro_width("single", "mm"), 85)
  expect_equal(.metro_width("double", "cm"), 18)
  expect_equal(.metro_width("1.5", "in"), 120 / 25.4)
  expect_error(.metro_width("triple", "mm"), "single")
  expect_error(save_metro_dag(tempfile(fileext = ".pdf"), p, width = "triple"), "single")
  directory <- tempfile("exports")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  svg <- file.path(directory, "single.svg")
  if (metrodag:::.metro_svglite_available()) {
    expect_no_warning(save_metro_dag(svg, p, width = "single"))
  } else {
    expect_warning(save_metro_dag(svg, p, width = "single"), "outlines")
  }
  expect_true(file.exists(svg))
  report <- diagnose_metro_dag(p, width = "single", device = "svg")
  expect_equal(unname(report$dimensions[["width"]]), 85, tolerance = 0.05)
  eps <- file.path(directory, "figure.eps")
  if (metrodag:::.metro_cairo_available()) {
    expect_no_warning(save_metro_dag(eps, p, width = "single"))
  } else {
    expect_warning(save_metro_dag(eps, p, width = "single"), "does not embed fonts")
  }
  expect_true(file.exists(eps))
  expect_true(any(grepl("^%!PS", readLines(eps, n = 2L, warn = FALSE))))
  tiff <- file.path(directory, "figure.tiff")
  expect_no_warning(save_metro_dag(tiff, p, width = "single", dpi = 72))
  expect_true(file.exists(tiff))
  skip_on_cran()
  expect_true(diagnose_metro_dag(p, width = 85, device = "eps")$ok)
  expect_no_warning(save_metro_dag(tiff, p, width = "single", dpi = 72, compression = "none"))
})
