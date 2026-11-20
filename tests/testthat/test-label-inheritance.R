.drawn_labels <- function(p) {
  layer <- Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)
  nodes <- layer$geom_params$nodes
  stats::setNames(nodes$label, nodes$node)[sort(nodes$node)]
}

.readme_labels <- c(
  C = "Socioeconomic background", X = "Cigarette consumption",
  M = "Tar deposit in lung", Y = "Lung cancer risk",
  U = "Unmeasured inheritance", S = "Occupational hazards"
)

test_that("the README reuse block keeps the display labels of the first plot", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  # README.md, quick start and "One graph, several analyses", as printed.
  edges <- data.frame(
    from = c("C", "C", "X", "M", "U", "U", "S", "S"),
    to   = c("X", "Y", "M", "Y", "C", "X", "Y", "X")
  )
  p <- metro_dag(
    edges,
    exposure = "X", outcome = "Y", unobserved = "U",
    labels = c(
      C = "Socioeconomic background", X = "Cigarette consumption",
      M = "Tar deposit in lung", Y = "Lung cancer risk",
      U = "Unmeasured inheritance", S = "Occupational hazards"
    )
  )
  fixed <- attr(p, "metrodag_layout")
  crude <- metro_dag(fixed, exposure = "X", outcome = "Y", unadjusted = TRUE,
    estimand = "Unadjusted X-Y association")
  total <- metro_dag(fixed, exposure = "X", outcome = "Y", adjusted = "C",
    estimand = "Total effect of X on Y")

  comparison <- compare_metro_dags(fixed, list(
    Unadjusted = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C")
  ))
  expected <- .readme_labels[sort(names(.readme_labels))]
  for (plot in list(p, crude, total, comparison$plots[[1L]], comparison$plots[[2L]]))
    expect_identical(.drawn_labels(plot), expected)
  expect_identical(total$labels$subtitle, "Adjusted for: Socioeconomic background")
  expect_identical(comparison$plots[[2L]]$labels$subtitle, "Adjusted for: Socioeconomic background")
  expect_identical(crude$labels$subtitle, "Unadjusted: no covariates")
  # The labels are stored unwrapped, in graph vertex order.
  graph <- fixed$graph
  expect_identical(igraph::vertex_attr(graph, "label"), unname(.readme_labels[igraph::V(graph)$name]))
  # Reusing the layout leaves the geometry exactly as it was.
  for (plot in list(crude, total)) {
    expect_identical(attr(plot, "metrodag_layout")$nodes, fixed$nodes)
    expect_identical(attr(plot, "metrodag_layout")$edges, fixed$edges)
  }
  # The comparison, with these labels, exports cleanly at the README's width,
  # read from README.md when the source tree is at hand.
  width <- 190
  readme <- test_path("..", "..", "README.md")
  if (file.exists(readme)) {
    line <- grep('save_metro_dag("analyses.pdf", comparison, width = ', readLines(readme), fixed = TRUE, value = TRUE)
    expect_length(line, 1L)
    width <- as.numeric(sub(".*width = ([0-9.]+).*", "\\1", line))
  }
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  expect_no_warning(save_metro_dag(file, comparison, width = width))
  report <- diagnose_metro_dag(comparison, width = width)
  expect_true(report$ok)
  expect_identical(nrow(report$issues), 0L)
  stacked <- compare_metro_dags(fixed, list(
    Unadjusted = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C")
  ), ncol = 1)
  expect_identical(nrow(diagnose_metro_dag(stacked, width = 180)$issues), 0L)
})

test_that("labels = FALSE hides labels without clearing the stored ones", {
  g <- metrodag_examples("causal")
  p <- metro_dag(g, exposure = "X", outcome = "Y", labels = .readme_labels)
  fixed <- attr(p, "metrodag_layout")
  hidden <- metro_dag(fixed, adjusted = "C", labels = FALSE)
  expect_true(all(.drawn_labels(hidden) == ""))
  # The subtitle still names the covariate by its display label.
  expect_identical(hidden$labels$subtitle, "Adjusted for: Socioeconomic background")
  kept <- attr(hidden, "metrodag_layout")
  expect_identical(igraph::vertex_attr(kept$graph, "label"), igraph::vertex_attr(fixed$graph, "label"))
  again <- plot_metro_dag(kept)
  expect_identical(.drawn_labels(again), .readme_labels[sort(names(.readme_labels))])
  # labels = TRUE on a fresh graph adds no label attribute.
  expect_null(igraph::vertex_attr(attr(metro_dag(g), "metrodag_layout")$graph, "label"))
})

test_that("a new labels vector on a reused layout replaces only the nodes it names", {
  g <- metrodag_examples("causal")
  fixed <- attr(metro_dag(g, labels = .readme_labels), "metrodag_layout")
  p <- metro_dag(fixed, labels = c(X = "Cigarettes"), adjusted = "C")
  shown <- .drawn_labels(p)
  expect_identical(shown[["X"]], "Cigarettes")
  expect_identical(shown[["C"]], "Socioeconomic background")
  expect_identical(p$labels$subtitle, "Adjusted for: Socioeconomic background")
  expect_identical(.drawn_labels(plot_metro_dag(attr(p, "metrodag_layout")))[["X"]], "Cigarettes")
})

test_that("a named labels vector keeps imported ggdag labels for other nodes", {
  skip_if_not_installed("ggdag")
  dag <- ggdag::dagify(y ~ x + z, x ~ z,
    labels = c(x = "Exposure variable", z = "Confounder"))
  base <- .drawn_labels(metro_dag(dag))
  expect_identical(base[c("x", "z")], c(x = "Exposure variable", z = "Confounder"))
  p <- metro_dag(dag, labels = c(y = "Outcome variable"))
  expect_identical(.drawn_labels(p),
    c(x = "Exposure variable", y = "Outcome variable", z = "Confounder"))
  views <- list(V = list(exposure = "x", outcome = "y", adjusted = "z"))
  comparison <- compare_metro_dags(dag, views, labels = c(y = "Outcome variable"))
  expect_identical(.drawn_labels(comparison$plots[[1L]]),
    c(x = "Exposure variable", y = "Outcome variable", z = "Confounder"))
  expect_identical(comparison$plots[[1L]]$labels$subtitle, "Adjusted for: Confounder")
})

test_that("a named labels vector keeps igraph label attributes for other nodes", {
  g <- igraph::set_vertex_attr(metrodag_examples("causal"), "label",
    value = c("Confounder", "Exposure0", "Outcome", "Med", NA, "Safety"))
  names <- igraph::V(g)$name
  p <- metro_dag(g, labels = c(X = "Exposure"))
  expected <- stats::setNames(c("Confounder", "Exposure0", "Outcome", "Med", NA, "Safety"), names)
  expected[["X"]] <- "Exposure"
  # A missing label falls back to the node name, as with labels = TRUE.
  expected[is.na(expected)] <- names[is.na(expected)]
  expect_identical(.drawn_labels(p), expected[sort(names)])
  comparison <- compare_metro_dags(g, list(A = list(exposure = "X")), labels = c(X = "Exposure"))
  expect_identical(.drawn_labels(comparison$plots[[1L]]), expected[sort(names)])
})

test_that("stored labels are resolved, unwrapped and left alone by TRUE or FALSE", {
  layout <- layout_metro_dag(metrodag_examples("causal"))
  long <- "A rather long display label for the confounder"
  stored <- .metro_store_labels(layout, c(C = long))
  nm <- igraph::V(layout$graph)$name
  expect_identical(igraph::vertex_attr(stored$graph, "label"), ifelse(nm == "C", long, nm))
  expect_identical(stored$nodes, layout$nodes)
  expect_identical(stored$edges, layout$edges)
  expect_identical(.metro_store_labels(stored, TRUE), stored)
  expect_identical(.metro_store_labels(stored, FALSE), stored)
  expect_error(.metro_store_labels(layout, NA), "not NA", fixed = TRUE)
  expect_error(.metro_store_labels(layout, c(Q = "x")), "`labels` names a node not in the graph: `Q`.",
    fixed = TRUE)
})

test_that("a comparison's shared layout keeps its display labels", {
  g <- metrodag_examples("causal")
  cmp <- compare_metro_dags(g, list(A = list(exposure = "X", outcome = "Y", adjusted = "C")),
    labels = c(C = "Socioeconomic background"))
  expect_identical(igraph::vertex_attr(cmp$layout$graph, "label"),
    igraph::vertex_attr(attr(cmp$plots[[1L]], "metrodag_layout")$graph, "label"))
  expect_identical(metro_dag(cmp$layout, exposure = "X", outcome = "Y", adjusted = "C")$labels$subtitle,
    "Adjusted for: Socioeconomic background")
  expect_identical(cmp$layout$nodes, attr(cmp$plots[[1L]], "metrodag_layout")$nodes)
  expect_null(igraph::vertex_attr(compare_metro_dags(g, list(A = list(exposure = "X")))$layout$graph, "label"))
})

test_that("named factor labels imported from dagify() are used as text", {
  skip_if_not_installed("ggdag")
  labels <- factor(c(X = "Exposure", Y = "Outcome"))
  dag <- ggdag::dagify(Y ~ X, labels = labels)
  for (input in list(dag, ggdag::tidy_dagitty(dag))) {
    shown <- .drawn_labels(metro_dag(input))
    expect_identical(unname(shown[c("X", "Y")]), c("Exposure", "Outcome"))
  }
})
