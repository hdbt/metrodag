# Analysis views only annotate a layout: the causal example is laid out once.
.views_causal_down <- .test_once(function() layout_metro_dag(metrodag_examples("causal"),
  direction = "down"))
.views_causal <- function() .test_rotate_layout(.views_causal_down(), "right")

test_that("an explicit empty adjustment set clears inherited annotations", {
  g <- metrodag_examples("chain")
  igraph::V(g)$role <- c("exposure", "adjusted", "adjusted", "outcome")
  original <- g
  inherited <- metro_dag(g)
  expect_setequal(attr(inherited, "metrodag_analysis")$adjusted, c("B", "C"))
  for (p in list(metro_dag(g, unadjusted = TRUE), metro_dag(g, adjusted = character()))) {
    a <- attr(p, "metrodag_analysis")
    expect_length(a$adjusted, 0L)
    expect_true(a$unadjusted)
    expect_equal(a$exposure, "A")
    expect_equal(a$outcome, "D")
    expect_equal(p$labels$subtitle, "Unadjusted: no covariates")
    expect_false(any(igraph::V(attr(p, "metrodag_layout")$graph)$role == "adjusted"))
  }
  replacement <- metro_dag(g, adjusted = "B")
  expect_equal(attr(replacement, "metrodag_analysis")$adjusted, "B")
  expect_equal(igraph::vertex_attr(attr(replacement, "metrodag_layout")$graph, "role", "C"), "observed")
  expect_identical(g, original)
})

test_that("analysis views keep topology and stored geometry fixed", {
  g <- igraph::graph_from_edgelist(matrix(c("C", "X", "C", "Y", "X", "M",
    "M", "Y", "X", "Y"), ncol = 2L, byrow = TRUE), directed = TRUE)
  l <- layout_metro_dag(g, direction = "right")
  original <- l
  views <- list(
    metro_dag(l, exposure = "X", outcome = "Y", unadjusted = TRUE,
      estimand = "Unadjusted X-Y association"),
    metro_dag(l, exposure = "X", outcome = "Y", adjusted = "C",
      estimand = "Total effect of X on Y"),
    plot_metro_dag(l, exposure = "M", outcome = "Y", adjusted = c("C", "X"),
      estimand = "Total effect of M on Y"))
  for (p in views) {
    out <- attr(p, "metrodag_layout")
    expect_identical(out$nodes, l$nodes)
    expect_identical(out$edges, l$edges)
    expect_equal(igraph::as_edgelist(out$graph), igraph::as_edgelist(g))
    expect_equal(out$direction, "right")
  }
  expect_identical(l, original)
  expect_equal(views[[2L]]$labels$title, "Total effect of X on Y")
  expect_equal(views[[3L]]$labels$subtitle, "Adjusted for: C, X")
  expect_error(metro_dag(l, direction = "down"), "reusing a layout")
})

test_that("changing the target replaces previous exposure and adjustment sets", {
  p <- metro_dag(.views_causal(), exposure = "X", outcome = "Y", adjusted = "C")
  l <- attr(p, "metrodag_layout")
  q <- metro_dag(l, exposure = "M", adjusted = c("C", "X"))
  a <- attr(q, "metrodag_analysis")
  expect_equal(a$exposure, "M")
  expect_equal(a$outcome, "Y")
  expect_setequal(a$adjusted, c("C", "X"))
  expect_equal(attr(p, "metrodag_analysis")$exposure, "X")
})

test_that("contradictory or malformed annotations are rejected", {
  l <- .views_causal_down()
  expect_error(metro_dag(l, unadjusted = TRUE, adjusted = "C"), "non-empty")
  expect_error(metro_dag(l, adjusted = "missing"), "`adjusted` names a node not in the graph: `missing`")
  expect_error(metro_dag(l, adjusted = c("C", "C")), "`adjusted` names `C` more than once")
  expect_error(metro_dag(l, exposure = "X", adjusted = "X"), "disjoint")
  expect_error(metro_dag(l, unadjusted = NA), "unadjusted")
  expect_error(metro_dag(l, legend = "yes"), "legend")
  expect_error(metro_dag(l, estimand = c("a", "b")), "estimand")
  expect_error(metro_dag(l, estimand = "  "), "estimand")
  latent <- metro_dag(l, unobserved = "U")
  expect_error(metro_dag(attr(latent, "metrodag_layout"), adjusted = "U"), "Node `U` cannot be both adjusted and unobserved.*inherited")
})

test_that("adjustment labels and symbols explain the chosen view", {
  l <- .views_causal()
  for (style in c("publication", "monochrome", "metro")) {
    p <- metro_dag(l, exposure = "X", outcome = "Y", adjusted = "C", unobserved = "U",
      labels = c(C = "Socioeconomic background"), style = style)
    expect_equal(p$labels$subtitle, "Adjusted for: Socioeconomic background")
    nodes <- p$layers[[2L]]$geom_params$nodes
    expect_equal(nodes$shape[nodes$node == "C"], 22L)
    expect_equal(nodes$shape[nodes$node == "M"], 21L)
    expect_equal(nodes$fill[nodes$node == "C"], "#DDE2E6")
    expect_equal(nodes$role[nodes$node == "U"], "unobserved")
    expect_setequal(ggplot2::ggplot_build(p)$plot$scales$get_scales("shape")$get_labels(),
      c("Exposure", "Outcome", "Adjusted", "Observed, not adjusted", "Unobserved"))
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
  bare <- metro_dag(l)
  expect_null(bare$labels$subtitle)
  expect_null(bare$scales$get_scales("shape"))
  # Without an adjustment set the observed key must not claim "not adjusted".
  roles_only <- metro_dag(l, exposure = "X", outcome = "Y", unobserved = "U")
  expect_setequal(ggplot2::ggplot_build(roles_only)$plot$scales$get_scales("shape")$get_labels(),
    c("Exposure", "Outcome", "Observed", "Unobserved"))
  hidden <- metro_dag(l, unadjusted = TRUE, legend = FALSE)
  expect_null(hidden$scales$get_scales("shape"))
  expect_equal(hidden$labels$subtitle, "Unadjusted: no covariates")
})

test_that("annotated plots export with room for their title and legend", {
  p <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D",
    adjusted = "B", estimand = "Illustrative target")
  expect_gt(attr(p, "metrodag_annotation_height"), 0)
  for (ext in c("png", "pdf")) {
    file <- tempfile(fileext = paste0(".", ext))
    expect_no_warning(save_metro_dag(file, p, width = 120, dpi = 100))
    expect_gt(file.info(file)$size, 500)
    unlink(file)
  }
})
