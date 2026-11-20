test_that("numeric edge identifiers agree across input formats", {
  endpoints <- matrix(c(10, 20, 20, 30), ncol = 2, byrow = TRUE)
  a <- attr(metro_dag(endpoints), "metrodag_layout")
  b <- attr(metro_dag(data.frame(from = endpoints[, 1], to = endpoints[, 2])), "metrodag_layout")
  expect_identical(a$nodes, b$nodes)
  expect_setequal(a$nodes$node, c("10", "20", "30"))
  expect_equal(igraph::as_edgelist(a$graph), igraph::as_edgelist(b$graph))
})

test_that("an explicitly empty adjustment set survives layout reuse", {
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y", unadjusted = TRUE)
  q <- metro_dag(attr(p, "metrodag_layout"))
  expect_true(attr(q, "metrodag_analysis")$adjustment_specified)
  expect_true(attr(q, "metrodag_analysis")$unadjusted)
  expect_equal(q$labels$subtitle, "Unadjusted: no covariates")
  r <- metro_dag(attr(q, "metrodag_layout"), adjusted = "C")
  expect_false(attr(r, "metrodag_analysis")$unadjusted)
  expect_identical(attr(p, "metrodag_layout")$edges, attr(r, "metrodag_layout")$edges)
  expect_false(attr(metro_dag(metrodag_examples("causal")), "metrodag_analysis")$adjustment_specified)
})

test_that("simple source and sink fans do not cross", {
  for (kind in c("fork", "collider")) for (direction in c("up", "down", "left", "right")) {
    m <- layout_metrics(layout_metro_dag(metrodag_examples(kind), direction = direction))
    expect_equal(m$crossings, 0, info = paste(kind, direction))
    expect_equal(m$overlap_segments, 0)
    expect_equal(m$node_edge_collisions, 0)
    expect_lte(m$total_bends, 2)
  }
})

test_that("sink-aligned layering handles shortcuts and disconnected vertices", {
  g <- igraph::add_vertices(metrodag_examples("long_edge"), 1, name = "isolated")
  l <- layout_metro_dag(direction = "down", g, layering = "sinks")
  endpoints <- igraph::as_edgelist(g)
  ranks <- setNames(l$nodes$layer, l$nodes$node)
  expect_true(all(ranks[endpoints[, 2]] > ranks[endpoints[, 1]]))
  expect_equal(ranks["E"], ranks["isolated"], ignore_attr = TRUE)
  expect_warning(old <- layout_metro_dag(direction = "down", g, layering = "shortest"), "deprecated")
  expect_equal(old$nodes, l$nodes)
})

test_that("narrow exports never silently remove causal edges", {
  skip_if_not_installed("svglite")
  g <- igraph::graph_from_edgelist(cbind(1:17, 2:18), directed = TRUE)
  p <- metro_dag(g, labels = FALSE)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  expect_error(save_metro_dag(file, p, width = 85), "too small to draw edge")
  expect_no_warning(save_metro_dag(file, p, width = 180))
  expect_equal(.svg_count(file, "polygon"), 17L)
})

test_that("annotation wrapping fits physical width and includes custom captions", {
  grDevices::pdf(NULL, width = 85 / 25.4, height = 120 / 25.4)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D", adjusted = "B",
    estimand = "Total effect of smoking on lung cancer risk",
    labels = c(B = "Socioeconomic background and occupational hazards")) +
    ggplot2::labs(caption = "A long supplementary caption that also needs to fit within a single journal column.")
  wrapped <- .metro_wrap_annotations(p, 85)
  theme <- ggplot2::theme_get() + wrapped$theme
  for (key in c("title", "subtitle", "caption")) {
    expect_match(wrapped$labels[[key]], "\n")
    element <- ggplot2::calc_element(paste0("plot.", key), theme)
    gp <- grid::gpar(fontsize = element$size, fontfamily = element$family, fontface = element$face)
    for (line in strsplit(wrapped$labels[[key]], "\n", fixed = TRUE)[[1]])
      expect_lte(grid::convertWidth(grid::grobWidth(grid::textGrob(line, gp = gp)), "mm", valueOnly = TRUE), 74.1)
  }
  expect_equal(p$labels$title, "Total effect of smoking on lung cancer risk")
})

test_that("single-column SVG text stays inside the page", {
  skip_if_not_installed("svglite")
  p <- metro_dag(metrodag_examples("chain"), exposure = "A", outcome = "D", adjusted = c("B", "C"),
    estimand = "Total effect of smoking on lung cancer risk",
    labels = c(B = "Socioeconomic background", C = "Occupational hazards"))
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  expect_no_warning(save_metro_dag(file, p, width = 85))
  text <- grep("<text", readLines(file), value = TRUE, fixed = TRUE)
  value <- function(attribute) as.numeric(sub("px$", "",
    sub(paste0(".* ", attribute, "='([^']+)'.*"), "\\1", text)))
  x <- value("x"); width <- value("textLength")
  left <- x - ifelse(grepl("text-anchor='middle'", text, fixed = TRUE), width / 2,
    ifelse(grepl("text-anchor='end'", text, fixed = TRUE), width, 0))
  expect_true(all(left >= -0.1))
  expect_true(all(left + width <= 85 / 25.4 * 72 + 0.1))
})

test_that("argument errors name the argument and route layout options", {
  g <- metrodag_examples("causal")
  layout <- layout_metro_dag(g)
  expect_error(metro_dag(g, seed = 1), "Unknown argument .*`seed`")
  expect_error(metro_dag(g, exposures = "X"), "`exposures`")
  # Abbreviations would otherwise match another argument silently.
  expect_error(plot_metro_dag(layout, rank = c(U = 0)), "cannot take `rank`")
  expect_error(plot_metro_dag(layout, direction = "down"), "cannot take `direction`")
  expect_error(metro_dag(g, rank_lab = "R"), "`rank_lab`")
  expect_error(save_metro_dag(metro_dag(g), "figure.pdf"), "`filename` comes first")
  # Edge tables work wherever a graph does.
  expect_s3_class(layout_metro_dag(data.frame(from = "A", to = "B")), "metrodag_layout")
  expect_s3_class(layout_metro_dag(cbind("A", "B")), "metrodag_layout")
  views <- list(A = list(exposure = "X", outcome = "Y"))
  ranks <- c(U = 0, C = 1, S = 1, X = 2, M = 3, Y = 4)
  comparison <- compare_metro_dags(g, views, rank = ranks, rank_labels = paste("W", 1:5))
  expect_equal(comparison$layout$nodes$layer[match(names(ranks), comparison$layout$nodes$node)],
    unname(ranks))
  expect_error(compare_metro_dags(layout, views, rank = ranks), "omit `direction` and layout arguments")
  skip_if_not_installed("dagitty")
  d <- dagitty::dagitty("dag { X [exposure]; Y [outcome]; X -> Y; X <-> Y }")
  expect_s3_class(compare_metro_dags(d, list(A = list(unadjusted = TRUE)), bidirected = "latent"),
    "metrodag_comparison")
  check <- check_metro_adjustment(d, bidirected = "latent", unadjusted = TRUE)
  expect_false(check$valid)
})

test_that("edge_style rejects unknown linetypes when the plot is built", {
  edges <- data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y"))
  expect_error(metro_dag(edges, edge_style = list("X->Y" = list(linetype = "foo"))), "linetypes must be one of")
  expect_error(metro_dag(edges, edge_style = data.frame(from = "X", to = "Y", linetype = "12345")), "\"12345\"")
  expect_s3_class(metro_dag(edges, edge_style = list("X->Y" = list(linetype = "44"))), "metrodag_plot")
})
