.test_reordered_routes <- function(layout, id_type = identity, reverse = TRUE) {
  edges <- layout$edges
  rows <- if (reverse) rev(seq_len(nrow(edges))) else
    order(-edges$segment, edges$to, edges$from)
  edges <- edges[rows, , drop = FALSE]
  if (!identical(id_type, identity)) {
    ids <- paste0("route-", edges$edge_id)
    edges$edge_id <- if (identical(id_type, as.character)) ids else
      id_type(ids, levels = c("unused", rev(unique(ids)), "another unused"))
  }
  layout$edges <- edges
  layout
}

test_that("saved segment order governs attachments and render diagnostics", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  types <- list(identity, as.character, factor, ordered)
  directions <- c("right", "down", "left", "up")
  # The same geometry as a fresh layout in each direction (test-layout.R).
  layouts <- .test_layouts_by_direction(metrodag_examples("dense"), directions = directions)
  for (i in seq_along(directions)) for (arrows in c(TRUE, FALSE)) {
    fixed <- layouts[[directions[i]]]
    input <- .test_reordered_routes(fixed, types[[i]], reverse = arrows)
    before <- input
    p <- metro_dag(input, arrows = arrows, labels = FALSE, legend = FALSE)
    stored <- p$layers[[2L]]$geom_params$edges
    expect_no_warning(report <- diagnose_metro_dag(p, width = 260, height = 220,
      device = "svg"))
    reference <- diagnose_metro_dag(metro_dag(fixed, arrows = arrows,
      labels = FALSE, legend = FALSE), width = 260, height = 220, device = "svg")
    expect_true(report$ok, info = paste(directions[i], arrows))
    expect_equal(nrow(report$issues), 0L)
    expect_identical(report, reference)
    expect_equal(count_crossings(input$edges), count_crossings(fixed$edges))
    expect_identical(p$layers[[2L]]$geom_params$edges, stored)
    expect_identical(attr(p, "metrodag_layout")$edges, before$edges)
    expect_identical(input, before)
  }
})

test_that("checked exports retain routes, heads and labels after segment shuffles", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  layouts <- .test_layouts_by_direction(metrodag_examples("causal"), directions = c("right", "down"))
  for (direction in c("right", "down")) {
    fixed <- attr(metro_dag(layouts[[direction]],
      exposure = "X", outcome = "Y", adjusted = c("C", "S"), unobserved = "U",
      estimand = "Total effect of X on Y"), "metrodag_layout")
    for (style in c("publication", "metro", "monochrome")) {
      input <- .test_reordered_routes(fixed, factor, reverse = style == "publication")
      before <- input
      plots <- lapply(list(fixed, input), plot_metro_dag, style = style, legend = FALSE)
      for (i in seq_along(plots))
        expect_no_warning(save_metro_dag(files[i], plots[[i]], width = 260, height = 220))
      expect_identical(.svg_primitives(files[2L]), .svg_primitives(files[1L]),
        info = paste(direction, style))
      expect_equal(.svg_arrowheads(files[2L]), igraph::ecount(fixed$graph))
      # U's dashed outline is a polygon; X, M and Y are circles. Monochrome
      # draws the outcome as a ring: one extra circle for its centre.
      expect_equal(.svg_dashed_outlines(files[2L]), 1L)
      expect_equal(.svg_count(files[2L], "circle"), 3L + (style == "monochrome"))
      expect_identical(attr(plots[[2L]], "metrodag_layout")$edges, before$edges)
      expect_identical(input, before)
    }
  }
})

test_that("ggsave and comparisons preserve segment order with transformed coordinates", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  fixed <- layout_metro_dag(metrodag_examples("causal"), direction = "left")
  input <- .test_reordered_routes(fixed, ordered, reverse = FALSE)
  before <- input
  plots <- lapply(list(fixed, input), function(layout) suppressMessages(
    ggplot2::autoplot(layout, labels = FALSE, arrows = FALSE, legend = FALSE,
      style = "metro") + ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_x_reverse(expand = ggplot2::expansion(mult = 0, add = 5))))
  for (i in seq_along(plots))
    expect_no_warning(ggplot2::ggsave(files[i], plots[[i]], width = 260, height = 220,
      units = "mm"))
  expect_identical(.svg_primitives(files[2L]), .svg_primitives(files[1L]))
  expect_equal(.svg_count(files[2L], "polygon"), 0L)
  views <- list(Unadjusted = list(exposure = "X", outcome = "Y", unobserved = "U",
      unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", unobserved = "U", adjusted = c("C", "S")))
  plots <- lapply(list(fixed, input), compare_metro_dags, views = views)
  for (i in seq_along(plots))
    expect_no_warning(save_metro_dag(files[i], plots[[i]], width = 360, height = 220))
  expect_identical(.svg_primitives(files[2L]), .svg_primitives(files[1L]))
  expect_equal(.svg_arrowheads(files[2L]), 2L * igraph::ecount(fixed$graph))
  expect_no_warning(report <- diagnose_metro_dag(plots[[2L]], width = 360, height = 220,
    device = "svg"))
  expect_true(report$ok)
  expect_equal(nrow(report$issues), 0L)
  expect_identical(input, before)
})
