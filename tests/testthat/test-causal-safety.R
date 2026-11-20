test_that("selection assumptions are never silently dropped on import", {
  skip_if_not_installed("dagitty")
  for (status in c("selected", "s")) {
    g <- dagitty::dagitty(paste0("dag { X [exposure]; Y [outcome]; S [", status,
      "]; X -> Y; X -> S; Y -> S }"))
    expect_false(dagitty::isAdjustmentSet(g, character()))
    # Selection is kept as a role and the adjustment check refuses to ignore it.
    expect_error(check_metro_adjustment(g, unadjusted = TRUE), "does not model selection")
    p <- metro_dag(g)
    expect_identical(attr(p, "metrodag_analysis")$selected, "S")
    layout <- layout_metro_dag(direction = "down", g)
    expect_identical(igraph::V(layout$graph)$role[igraph::V(layout$graph)$name == "S"], "selected")
  }
  for (syntax in c(
    'dag { selected -> Y }',
    'dag { "[selected]" -> Y }',
    'dag { X [label="a [selected] label"]; X -> Y }',
    'dag { X [label="escaped \\"quote\\" [selected]"]; X -> Y }',
    'dag { X [custom=selected]; X -> Y }')) {
    p <- metro_dag(dagitty::dagitty(syntax))
    expect_s3_class(p, "ggplot")
    expect_length(attr(p, "metrodag_analysis")$selected, 0L)
  }
})

test_that("reused estimands inherit, replace, clear and avoid stale targets", {
  p <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D",
    unadjusted = TRUE, estimand = "Total effect of A on D")
  l <- attr(p, "metrodag_layout")
  for (fun in list(metro_dag, plot_metro_dag)) {
    expect_equal(fun(l)$labels$title, "Total effect of A on D")
    expect_equal(fun(l, adjusted = "B")$labels$title, "Total effect of A on D")
    expect_equal(fun(l, estimand = "New label")$labels$title, "New label")
    expect_null(fun(l, estimand = NULL)$labels$title)
    expect_null(fun(l, exposure = "B")$labels$title)
    expect_null(fun(l, outcome = "C")$labels$title)
    expect_equal(fun(l, exposure = "A", outcome = "D")$labels$title, l$analysis$estimand)
  }
  comparison <- compare_metro_dags(l, list(Inherit = list(),
    Clear = list(estimand = NULL), Changed = list(exposure = "B")))
  expect_equal(comparison$plots[[1L]]$labels$title, l$analysis$estimand)
  expect_null(comparison$plots[[2L]]$labels$title)
  expect_equal(comparison$plots[[3L]]$labels$title, "Changed")
  expect_equal(attr(p, "metrodag_analysis")$estimand, "Total effect of A on D")
})

test_that("reverse scales transform stations and routed endpoints together", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  p <- metro_dag(metrodag_examples("diamond"), labels = FALSE, legend = FALSE)
  transforms <- list(
    ggplot2::scale_x_reverse(), ggplot2::scale_y_reverse(),
    list(ggplot2::scale_x_reverse(), ggplot2::scale_y_reverse()),
    list(ggplot2::coord_flip(), ggplot2::scale_x_reverse()))
  for (transformation in transforms) {
    q <- suppressMessages(p + transformation)
    expect_no_warning(save_metro_dag(file, q, width = 85, height = 65))
    svg <- readLines(file)
    circles <- svg[grepl("<circle", svg, fixed = TRUE)]
    value <- function(key) as.numeric(sub(paste0(".* ", key, "='([^']+)'.*"), "\\1", circles))
    expect_length(circles, 4L)
    expect_true(all(value("cx") - value("r") > 0))
    expect_true(all(value("cx") + value("r") < 85 / 25.4 * 72))
    expect_true(all(value("cy") - value("r") > 0))
    expect_true(all(value("cy") + value("r") < 65 / 25.4 * 72))
    expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 4L)
  }
  p <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  q <- suppressMessages(p + ggplot2::scale_x_reverse())
  save_metro_dag(file, q, width = 85)
  circles <- readLines(file)
  circles <- circles[grepl("<circle", circles, fixed = TRUE)]
  cx <- as.numeric(sub(".* cx='([^']+)'.*", "\\1", circles))
  expect_true(all(diff(cx) < 0))
  q <- suppressMessages(p + ggplot2::scale_x_log10())
  expect_error(suppressWarnings(ggplot2::ggplotGrob(q)), "identity and reverse")
})
