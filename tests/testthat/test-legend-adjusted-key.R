.legend_keys <- function(p) {
  scale <- ggplot2::ggplot_build(p)$plot$scales$get_scales("shape")
  if (is.null(scale)) character() else scale$get_labels()
}

test_that("the Adjusted key appears only when a station is drawn adjusted", {
  g <- metrodag_examples("causal")
  crude <- metro_dag(g, exposure = "X", outcome = "Y", unobserved = "U", unadjusted = TRUE)
  expect_identical(.legend_keys(crude),
    c("Exposure", "Outcome", "Observed, not adjusted", "Unobserved"))
  empty <- metro_dag(g, exposure = "X", outcome = "Y", unobserved = "U", adjusted = character(0))
  expect_identical(.legend_keys(empty), .legend_keys(crude))
  expect_identical(empty$labels$subtitle, "Unadjusted: no covariates")
  adjusted <- metro_dag(g, exposure = "X", outcome = "Y", unobserved = "U", adjusted = "C")
  expect_identical(.legend_keys(adjusted),
    c("Exposure", "Outcome", "Adjusted", "Observed, not adjusted", "Unobserved"))
  # A reused adjusted layout cleared with unadjusted = TRUE drops the key too.
  cleared <- metro_dag(attr(adjusted, "metrodag_layout"), unadjusted = TRUE)
  expect_false("Adjusted" %in% .legend_keys(cleared))
  expect_true("Observed, not adjusted" %in% .legend_keys(cleared))
  # Without an adjustment set the observed key does not claim "not adjusted".
  plain <- metro_dag(g, exposure = "X", outcome = "Y")
  expect_identical(.legend_keys(plain), c("Exposure", "Outcome", "Observed"))
  # The legend row count follows the shorter legend: with four keys a row,
  # its four keys fit on one row and the five adjusted keys need two.
  rows <- function(...) attr(metro_dag(g, exposure = "X", outcome = "Y", unobserved = "U",
    style = metro_style(legend_columns = 4L), ...), "metrodag_legend_rows")
  expect_equal(rows(unadjusted = TRUE), 1)
  expect_equal(rows(adjusted = "C"), 2)
})

test_that("an unadjusted view with only observed stations keeps its explanation", {
  p <- metro_dag(metrodag_examples("chain"), unadjusted = TRUE)
  expect_identical(.legend_keys(p), "Observed, not adjusted")
})

test_that("a comparison keeps the Adjusted key when any view adjusts", {
  g <- metrodag_examples("causal")
  views <- list(
    Unadjusted = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C"))
  comparison <- compare_metro_dags(g, views)
  expect_true(all(c("Adjusted", "Observed, not adjusted") %in% .legend_keys(comparison$legend_plot)))
  # The first view alone is unadjusted, yet the shared legend serves both.
  expect_false("Adjusted" %in% .legend_keys(metro_dag(g, exposure = "X", outcome = "Y", unadjusted = TRUE)))
  reversed <- compare_metro_dags(g, rev(views))
  expect_identical(.legend_keys(reversed$legend_plot), .legend_keys(comparison$legend_plot))
})

test_that("a shared legend shows the Adjusted key only for roles that are drawn", {
  # A legend shared by several views is given the roles drawn in any view and
  # whether any view specified an adjustment set.
  g <- metrodag_examples("causal")
  plain <- metro_dag(g, exposure = "X", outcome = "Y", legend = FALSE)
  layout <- attr(plain, "metrodag_layout")
  legend <- function(roles, specified)
    .legend_keys(metrodag:::.metro_role_legend(plain, layout, "white", "publication",
      roles = roles, specified = specified))
  expect_identical(legend(c("exposure", "outcome", "observed"), TRUE),
    c("Exposure", "Outcome", "Observed, not adjusted"))
  expect_identical(legend(c("exposure", "outcome", "observed", "adjusted"), TRUE),
    c("Exposure", "Outcome", "Adjusted", "Observed, not adjusted"))
  # The first view's layout specifies no adjustment set, but a later view does.
  expect_false(isTRUE(layout$analysis$adjustment_specified))
  expect_identical(legend(c("exposure", "outcome", "observed"), FALSE), c("Exposure", "Outcome", "Observed"))
  # Without `specified`, the layout's own setting applies, as for one plot.
  expect_identical(.legend_keys(metrodag:::.metro_role_legend(plain, layout, "white", "publication",
    roles = c("exposure", "outcome", "observed"))), c("Exposure", "Outcome", "Observed"))
})

test_that("a comparison drops the Adjusted key when no view adjusts", {
  g <- metrodag_examples("causal")
  none <- c("Exposure", "Outcome", "Observed, not adjusted")
  crude <- compare_metro_dags(g, list(Crude = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Empty = list(exposure = "X", outcome = "Y", adjusted = character(0))))
  expect_identical(.legend_keys(crude$legend_plot), none)
  # Only the second view specifies an (empty) adjustment set.
  later <- compare_metro_dags(g, list(Plain = list(exposure = "X", outcome = "Y"),
    Crude = list(exposure = "X", outcome = "Y", unadjusted = TRUE)))
  expect_identical(.legend_keys(later$legend_plot), none)
  fixed <- attr(metro_dag(g, exposure = "X", outcome = "Y", adjusted = "C"), "metrodag_layout")
  cleared <- compare_metro_dags(fixed, list(A = list(unadjusted = TRUE), B = list(adjusted = character(0))))
  expect_identical(.legend_keys(cleared$legend_plot), none)
  adjusted <- compare_metro_dags(g, list(Plain = list(exposure = "X", outcome = "Y"),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C")))
  expect_identical(.legend_keys(adjusted$legend_plot), c("Exposure", "Outcome", "Adjusted", "Observed, not adjusted"))
  plain <- compare_metro_dags(g, list(A = list(exposure = "X", outcome = "Y"), B = list(exposure = "X", outcome = "Y")))
  expect_identical(.legend_keys(plain$legend_plot), c("Exposure", "Outcome", "Observed"))
})

test_that("the observed key appears only when a station is drawn as observed", {
  # C causes X and Y: adjusting for C leaves no station drawn as observed.
  fixed <- layout_metro_dag(data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y")))
  adjusted <- metro_dag(fixed, exposure = "X", outcome = "Y", adjusted = "C",
    style = metro_style(legend_columns = 3L))
  expect_false("observed" %in% adjusted$layers[[2L]]$geom_params$nodes$role)
  expect_identical(.legend_keys(adjusted), c("Exposure", "Outcome", "Adjusted"))
  expect_identical(adjusted$labels$subtitle, "Adjusted for: C")
  # Legend rows count only the keys drawn: three fit one row of three.
  expect_equal(attr(adjusted, "metrodag_legend_rows"), 1)
  # An unadjusted view without observed stations has no observed key either.
  crude <- metro_dag(fixed, exposure = "X", outcome = "Y", unobserved = "C", unadjusted = TRUE)
  expect_identical(.legend_keys(crude), c("Exposure", "Outcome", "Unobserved"))
  expect_identical(crude$labels$subtitle, "Unadjusted: no covariates")
  # An observed variable left unadjusted brings the key back, with its wording.
  edges <- data.frame(from = c("C", "C", "X", "M"), to = c("X", "Y", "Y", "Y"))
  observed <- metro_dag(edges, exposure = "X", outcome = "Y", adjusted = "C")
  expect_identical(.legend_keys(observed), c("Exposure", "Outcome", "Adjusted", "Observed, not adjusted"))
})

test_that("a comparison keeps the observed key when any panel draws an observed station", {
  fixed <- layout_metro_dag(data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y")))
  # Only the crude panel draws C as observed, not adjusted.
  views <- list(
    Crude = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C"))
  keys <- c("Exposure", "Outcome", "Adjusted", "Observed, not adjusted")
  expect_identical(.legend_keys(compare_metro_dags(fixed, views)$legend_plot), keys)
  expect_identical(.legend_keys(compare_metro_dags(fixed, rev(views))$legend_plot), keys)
  # With C unobserved in the crude panel, no panel draws an observed station.
  views$Crude$unobserved <- "C"
  expect_identical(.legend_keys(compare_metro_dags(fixed, views)$legend_plot),
    c("Exposure", "Outcome", "Adjusted", "Unobserved"))
})

test_that("the role legend comes after a legend the user orders first, and before edge_legend", {
  # The text of each legend in the drawn guide box, in drawing order.
  legend_texts <- function(p) {
    table <- ggplot2::ggplotGrob(p)
    box <- table$grobs[grepl("^guide-box", table$layout$name) &
      !vapply(table$grobs, inherits, logical(1), "zeroGrob")]
    texts <- function(g) c(if (!is.null(g$label)) as.character(g$label),
      unlist(lapply(g$children, texts)), unlist(lapply(g$grobs, texts)))
    legends <- unlist(lapply(box, function(b) Filter(function(g) inherits(g, "gtable"), b$grobs)),
      recursive = FALSE)
    lapply(legends, texts)
  }
  g <- data.frame(from = c("C", "C", "X", "M", "X"), to = c("X", "Y", "M", "Y", "Y"))
  p <- metro_dag(g, exposure = "X", outcome = "Y", adjusted = "C",
    edge_style = list("C->Y" = list(linetype = "dotted")), edge_legend = c(dotted = "Assumed, uncertain"))
  first <- function(texts, key) which(vapply(texts, function(t) key %in% t, logical(1)))[1L]
  texts <- legend_texts(p)
  expect_lt(first(texts, "Exposure"), first(texts, "Assumed, uncertain"))
  user <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y, colour = group),
    data = data.frame(x = 0, y = 0, group = "Survey"), inherit.aes = FALSE) +
    ggplot2::scale_colour_manual(name = "Measured in", values = c(Survey = "grey40")) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1))
  texts <- legend_texts(user)
  expect_lt(first(texts, "Measured in"), first(texts, "Exposure"))
  expect_lt(first(texts, "Exposure"), first(texts, "Assumed, uncertain"))
})
