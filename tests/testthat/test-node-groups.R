.test_group_plot <- function(direction = "right", style = "publication", legend = TRUE) {
  metro_dag(metrodag_examples("chain"), direction = direction, exposure = "A",
    outcome = "D", adjusted = "B", unobserved = "C", style = style, legend = legend,
    node_groups = list(Selection = "C", Context = c("D", "B", "A")),
    node_group_colours = c(Selection = "purple", Context = "darkgreen"))
}

test_that("display groups retain every analysis annotation and all geometry", {
  base <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D",
    adjusted = "B", unobserved = "C", labels = c(C = "Unmeasured variable"))
  layout <- attr(base, "metrodag_layout"); original <- layout
  p <- metro_dag(layout, labels = c(C = "Unmeasured variable"),
    node_groups = list(Selection = "C", Context = c("D", "B", "A")))
  out <- attr(p, "metrodag_layout")
  expect_identical(out$nodes, layout$nodes)
  expect_identical(out$edges, layout$edges)
  expect_identical(out$parameters, layout$parameters)
  expect_identical(out$analysis, layout$analysis)
  expect_identical(igraph::vertex_attr(out$graph), igraph::vertex_attr(layout$graph))
  expect_identical(igraph::as_edgelist(out$graph), igraph::as_edgelist(layout$graph))
  expect_identical(layout, original)
  expect_identical(attr(p, "metrodag_analysis"), attr(base, "metrodag_analysis"))
  expect_identical(attr(p, "metrodag_node_groups"), list(Context = c("A", "B", "D"), Selection = "C"))
  a <- base$layers[[2L]]$geom_params; b <- p$layers[[2L]]$geom_params
  expect_identical(a$nodes, b$nodes[, names(a$nodes)])
  expect_identical(a$edges, b$edges)
  expect_equal(b$nodes$group_mark[match(c("A", "B", "C", "D"), b$nodes$node)],
    c("1", "1", "2", "1"))
  expect_identical(layout_metrics(out), layout_metrics(layout))
})

test_that("arbitrary category names never infer measurement or selection status", {
  l <- layout_metro_dag(direction = "down", metrodag_examples("chain"))
  p <- metro_dag(l, node_groups = list(Unobserved = "A", Selected = "B", Adjusted = "C", Outcome = "D"))
  expect_identical(p$layers[[2L]]$geom_params$nodes$role, rep("observed", 4))
  a <- attr(p, "metrodag_analysis")
  expect_length(a$unobserved, 0L)
  expect_length(a$adjusted, 0L)
  expect_length(a$outcome, 0L)
  expect_false(a$adjustment_specified)
  expect_null(p$labels$subtitle)
  expect_null(igraph::vertex_attr(attr(p, "metrodag_layout")$graph, "selected"))
  expect_null(igraph::vertex_attr(l$graph, "role"))
})

test_that("categories and outline colours inherit, replace and clear independently", {
  p <- .test_group_plot(); l <- attr(p, "metrodag_layout"); original <- l
  inherited <- metro_dag(l, unadjusted = TRUE)
  out <- attr(inherited, "metrodag_layout")
  expect_identical(out$node_groups, l$node_groups)
  expect_identical(out$node_group_colours, l$node_group_colours)
  expect_length(out$analysis$adjusted, 0L)
  replaced <- metro_dag(l, node_groups = list(New = "A", Context = "B"))
  z <- attr(replaced, "metrodag_layout")
  expect_identical(z$node_groups, list(Context = "B", New = "A"))
  expect_identical(z$node_group_colours, c(Context = "darkgreen"))
  no_colour <- metro_dag(l, node_group_colours = character())
  expect_identical(attr(no_colour, "metrodag_node_groups"), l$node_groups)
  expect_null(attr(no_colour, "metrodag_layout")$node_group_colours)
  cleared <- metro_dag(l, node_groups = list())
  expect_null(attr(cleared, "metrodag_node_groups"))
  expect_null(attr(cleared, "metrodag_layout")$node_groups)
  expect_null(attr(cleared, "metrodag_layout")$node_group_colours)
  expect_identical(attr(cleared, "metrodag_analysis"), attr(p, "metrodag_analysis"))
  expect_identical(l, original)
})

test_that("invalid group definitions fail before changing the last plot", {
  l <- layout_metro_dag(direction = "down", metrodag_examples("chain"))
  last <- ggplot2::ggplot(); ggplot2::set_last_plot(last)
  bad <- list("A", c(g = "A"), list("A"), list(g = character()), list(g = 1),
    list(g = NA_character_), list(g = "missing"), stats::setNames(list("A"), " "),
    stats::setNames(list("A"), NA_character_), stats::setNames(list("A", "B"), c("g", "g")))
  for (groups in bad) expect_error(metro_dag(l, node_groups = groups), "node_groups")
  expect_error(metro_dag(l, node_groups = list(a = c("A", "A"))), "category `a` lists `A` more than once")
  expect_error(metro_dag(l, node_groups = list(a = "A", b = "A")), "only one")
  for (colours in list("red", c(missing = "red"), c(g = "not a colour"), c(g = NA_character_),
      c(g = 1), stats::setNames(c("red", "blue"), c("g", "g"))))
    expect_error(metro_dag(l, node_groups = list(g = "A"), node_group_colours = colours), "node_group_colours")
  expect_error(metro_dag(l, node_group_colours = c(g = "red")), "`node_group_colours` names a group not in `node_groups`: `g`")
  expect_identical(ggplot2::last_plot(), last)
  invalid <- attr(.test_group_plot(), "metrodag_layout")
  invalid$node_groups[[1L]] <- "missing"
  expect_error(plot_metro_dag(invalid), "names a node not in the graph: `missing`")
  invalid <- attr(.test_group_plot(), "metrodag_layout")
  invalid$node_group_colours <- c(missing = "red")
  expect_error(plot_metro_dag(invalid), "names a group not in `node_groups`: `missing`")
})

test_that("numbering and styling are deterministic across equivalent reordered inputs", {
  graph <- metrodag_examples("chain")
  ends <- igraph::as_edgelist(graph)
  reverse <- igraph::graph_from_data_frame(ends[nrow(ends):1L, ],
    vertices = data.frame(name = rev(igraph::V(graph)$name)), directed = TRUE)
  groups <- list(Zulu = c("D", "C"), Alpha = c("B", "A"))
  canonical <- list(Alpha = c("A", "B"), Zulu = c("C", "D"))
  for (direction in c("right", "down", "left", "up")) {
    a <- metro_dag(graph, direction = direction, node_groups = groups)
    b <- metro_dag(reverse, direction = direction, node_groups = canonical)
    normalize <- function(p) {
      n <- p$layers[[2L]]$geom_params$nodes
      n <- n[order(n$node), c("node", "x", "y", "role", "shape", "fill", "colour", "group_mark", "label")]
      rownames(n) <- NULL; n
    }
    expect_identical(normalize(a), normalize(b))
    expect_identical(attr(a, "metrodag_node_groups"), attr(b, "metrodag_node_groups"))
    expect_equal(attr(a, "metrodag_legend_labels"), attr(b, "metrodag_legend_labels"))
    # Drawing every direction is part of the full suite; CRAN draws one.
    if (.test_on_cran() && direction != "right") next
    report <- diagnose_metro_dag(a, width = 120, device = "png", dpi = 100)
    expect_true(report$ok)
    expect_equal(nrow(report$issues), 0L)
  }
})

test_that("group styling preserves analysis symbols in every drawing style", {
  for (style in c("publication", "metro", "monochrome")) {
    p <- .test_group_plot(style = style)
    n <- p$layers[[2L]]$geom_params$nodes
    role <- stats::setNames(n$role, n$node)
    expect_equal(unname(role[c("A", "B", "C", "D")]), c("exposure", "adjusted", "unobserved", "outcome"))
    expect_equal(n$shape[n$node == "B"], 22L)
    expect_equal(n$fill[n$node == "B"], "#DDE2E6")
    expect_true(all(p$layers[[2L]]$geom_params$edges$linetype[
      p$layers[[2L]]$geom_params$edges$from == "C"] == "dashed"))
    if (style == "monochrome") expect_true(all(n$colour == "#24292E")) else {
      expect_equal(n$colour[n$node == "C"], "purple")
      expect_true(all(n$colour[n$node %in% c("A", "B", "D")] == "darkgreen"))
    }
    expect_true(all(nzchar(n$group_mark)))
  }
  expect_identical(.metro_group_ink("#0072B2", "white"), "#FFFFFF")
  expect_identical(.metro_group_ink("#D55E00", "white"), "#000000")
  expect_identical(.metro_group_ink("transparent", "white"), "#000000")
  serif <- metro_dag(metrodag_examples("chain"), family = "serif", node_groups = list(g = "B"))
  values <- attr(serif, "metrodag_legend_values")
  expect_identical(values$group_family, rep("serif", nrow(values)))
  key <- .draw_key_metro_node(values[nrow(values), ], list(), 4)
  expect_identical(key$children[[2L]]$gp$fontfamily, "serif")
})

test_that("grouped exports retain every arrow and a mark for each grouped station", {
  skip_if_not_installed("svglite")
  # CRAN exports the default style; the full suite exports every style.
  styles <- if (.test_on_cran()) "publication" else c("publication", "metro", "monochrome")
  for (style in styles) {
    p <- .test_group_plot(style = style)
    path <- tempfile(fileext = ".svg")
    on.exit(unlink(path), add = TRUE)
    expect_no_warning(save_metro_dag(path, p, width = 160))
    svg <- readLines(path)
    expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 3L)
    expect_equal(sum(grepl(">1</text>", svg, fixed = TRUE)), 4L)
    expect_equal(sum(grepl(">2</text>", svg, fixed = TRUE)), 2L)
    for (label in c("Context", "Selection"))
      expect_equal(sum(grepl(paste0(">", label, "</text>"), svg, fixed = TRUE)), 1L)
    expect_no_warning(save_metro_dag(sub("svg$", "png", path), p, width = 160, dpi = 100))
    unlink(sub("svg$", "png", path))
  }
  hidden <- .test_group_plot(legend = FALSE)
  path <- tempfile(fileext = ".svg"); on.exit(unlink(path), add = TRUE)
  save_metro_dag(path, hidden, width = 120)
  svg <- readLines(path)
  expect_equal(sum(grepl(">1</text>", svg, fixed = TRUE)), 3L)
  expect_equal(sum(grepl(">2</text>", svg, fixed = TRUE)), 1L)
  pdf <- tempfile(fileext = ".pdf"); on.exit(unlink(pdf), add = TRUE)
  expect_no_warning(save_metro_dag(pdf, hidden, width = 120))
  expect_gt(file.info(pdf)$size, 500)
})

test_that("comparison panels keep one shared group map and legend", {
  skip_if_not_installed("svglite")
  views <- list(Crude = list(exposure = "A", outcome = "D", unobserved = "C", unadjusted = TRUE),
    Adjusted = list(exposure = "A", outcome = "D", unobserved = "C", adjusted = "B"))
  groups <- list(Selection = "C", Context = c("A", "B", "D"))
  x <- compare_metro_dags(metrodag_examples("chain"), views, node_groups = groups)
  for (p in x$plots) {
    expect_identical(attr(p, "metrodag_node_groups"), x$layout$node_groups)
    expect_identical(attr(p, "metrodag_layout")$edges, x$layout$edges)
    expect_equal(p$layers[[2L]]$geom_params$nodes$group_mark, c("1", "1", "2", "1"))
  }
  path <- tempfile(fileext = ".svg"); on.exit(unlink(path), add = TRUE)
  expect_no_warning(save_metro_dag(path, x, width = 240))
  svg <- readLines(path)
  expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 6L)
  expect_equal(sum(grepl(">Context</text>", svg, fixed = TRUE)), 1L)
  expect_equal(sum(grepl(">Selection</text>", svg, fixed = TRUE)), 1L)
  expect_equal(sum(grepl(">1</text>", svg, fixed = TRUE)), 7L)
  expect_equal(sum(grepl(">2</text>", svg, fixed = TRUE)), 3L)
  inherited <- compare_metro_dags(attr(.test_group_plot(), "metrodag_layout"), views)
  expect_identical(inherited$layout$node_groups, x$layout$node_groups)
  expect_error(compare_metro_dags(x$layout, list(list(node_groups = groups))), "analysis arguments")
})

test_that("device reflow wraps custom legends without mutating reusable plots", {
  grDevices::pdf(NULL, width = 7, height = 5)
  on.exit(grDevices::dev.off(), add = TRUE)
  label <- "Unmeasured determinants of selection into the study and continued participation"
  p <- metro_dag(metrodag_examples("chain"), node_groups = stats::setNames(list("B"), label))
  original <- p$scales$get_scales("shape")$labels
  small <- .metro_wrap_annotations(p, 85)
  expect_true(any(grepl("\n", small$scales$get_scales("shape")$labels, fixed = TRUE)))
  expect_identical(p$scales$get_scales("shape")$labels, original)
  wide <- .metro_wrap_annotations(p, 400)
  expect_identical(wide$scales$get_scales("shape")$labels, original)
  expect_identical(attr(p, "metrodag_legend_labels"), attr(small, "metrodag_legend_labels"))
  mark <- .metro_group_mark_grob("123", grid::unit(0.5, "npc"), grid::unit(0.5, "npc"),
    1.5, "white", "white")
  expect_lte(grid::convertWidth(grid::grobWidth(mark), "mm", valueOnly = TRUE), 1.25 * sqrt(2) + 1e-8)
  expect_lte(grid::convertHeight(grid::grobHeight(mark), "mm", valueOnly = TRUE), 1.25 * sqrt(2) + 1e-8)
})
