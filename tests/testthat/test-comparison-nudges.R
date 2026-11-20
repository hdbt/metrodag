test_that("comparison panels share topology, geometry and one legend", {
  skip_if_not_installed("svglite")
  g <- metrodag_examples("causal")
  views <- list(Crude = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
    Adjusted = list(exposure = "X", outcome = "Y", adjusted = c("C", "S"),
      estimand = "Total effect of smoking on lung cancer risk"))
  x <- compare_metro_dags(g, views)
  expect_s3_class(x, "metrodag_comparison")
  for (p in x$plots) {
    expect_s3_class(p, "ggplot")
    expect_identical(attr(p, "metrodag_layout")$nodes, x$layout$nodes)
    expect_identical(attr(p, "metrodag_layout")$edges, x$layout$edges)
  }
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  expect_no_warning(save_metro_dag(file, x, width = 180))
  svg <- readLines(file)
  expect_equal(sum(grepl("<polygon", svg, fixed = TRUE)), 2L * igraph::ecount(g))
  expect_equal(sum(grepl(">Exposure</text>", svg, fixed = TRUE)), 1L)
  # The first two filled exposure/outcome circles belong to the actual panels;
  # a later circle belongs to the shared legend. Check physical scale/alignment.
  centers <- function(colour) {
    lines <- head(svg[grepl("<circle", svg, fixed = TRUE) & grepl(paste0("fill: ", colour), svg, fixed = TRUE)], 2)
    cbind(x = as.numeric(sub(".* cx='([^']+)'.*", "\\1", lines)),
      y = as.numeric(sub(".* cy='([^']+)'.*", "\\1", lines)))
  }
  exposure <- centers("#0072B2"); outcome <- centers("#D55E00")
  expect_equal(nrow(exposure), 2L)
  expect_equal(exposure[1, "y"], exposure[2, "y"], tolerance = 0.02)
  expect_equal(outcome[1, "y"], outcome[2, "y"], tolerance = 0.02)
  expect_equal(outcome[1, ] - exposure[1, ], outcome[2, ] - exposure[2, ], tolerance = 0.02)
  expect_error(compare_metro_dags(g, list(list(direction = "down"))), "analysis arguments")
  expect_error(compare_metro_dags(x$layout, views, direction = "down"), "omit")
  expect_error(compare_metro_dags(g, views, ncol = 0), "ncol")
})

test_that("label nudges move only the specified text in physical millimetres", {
  grDevices::pdf(NULL, width = 6, height = 4)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- metrodag_examples("chain")
  a <- metro_dag(g)
  b <- metro_dag(g, label_nudge = list(A = c(1, 2)))
  render <- function(p) {
    table <- ggplot2::ggplotGrob(p)
    panel <- table$grobs[[which(table$layout$name == "panel")]]
    grob <- panel$children[[which(vapply(panel$children, inherits, logical(1), "metrodag_grob"))]]
    grid::makeContent(grob)$diagnostics
  }
  before <- render(a); after <- render(b)
  i <- match("A", before$nodes$node)
  expect_equal(unname(after$label_centers[i, ] - before$label_centers[i, ]), c(1, 2))
  expect_equal(after$label_centers[-i, ], before$label_centers[-i, ])
  expect_identical(after$nodes, before$nodes)
  expect_identical(attr(a, "metrodag_layout")$edges, attr(b, "metrodag_layout")$edges)
  expect_equal(after$drawn_edges, igraph::ecount(g))
  expect_error(metro_dag(g, label_nudge = list(missing = c(0, 2))), "`label_nudge` names a node not in the graph: `missing`")
  expect_error(metro_dag(g, label_nudge = list(A = c(0, NA))), "finite")
})
