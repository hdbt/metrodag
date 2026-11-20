.test_incompatible_routes <- function(fixed) {
  variants <- stats::setNames(rep(list(fixed), 14L), c("missing_route", "empty_routes",
    "extra_route", "merged_ids", "split_ids", "retargeted", "reversed",
    "inconsistent", "duplicate_pair", "unknown_node", "missing_id",
    "missing_column", "graph_added", "graph_removed"))
  e <- fixed$edges
  ids <- unique(e$edge_id)
  route <- e$edge_id == ids[1L]
  variants$missing_route$edges <- e[!route, , drop = FALSE]
  variants$empty_routes$edges <- e[FALSE, , drop = FALSE]
  extra <- e[route, , drop = FALSE]
  extra$edge_id <- 99L
  variants$extra_route$edges <- rbind(e, extra)
  variants$merged_ids$edges$edge_id[e$edge_id == ids[2L]] <- ids[1L]
  variants$split_ids$edges$edge_id[which(route)[1L]] <- 99L
  variants$retargeted$edges$to[route] <- "D"
  variants$reversed$edges$from[route] <- e$to[route]
  variants$reversed$edges$to[route] <- e$from[route]
  variants$inconsistent$edges$to[which(route)[2L]] <- "D"
  variants$duplicate_pair$edges$from[e$edge_id == ids[2L]] <- e$from[which(route)[1L]]
  variants$duplicate_pair$edges$to[e$edge_id == ids[2L]] <- e$to[which(route)[1L]]
  variants$unknown_node$edges$from[1L] <- "Unknown"
  missing <- as.character(e$edge_id)
  missing[1L] <- NA_character_
  variants$missing_id$edges$edge_id <- factor(missing, exclude = NULL)
  variants$missing_column$edges$edge_id <- NULL
  variants$graph_added$graph <- igraph::add_edges(fixed$graph, c("A", "D"))
  variants$graph_removed$graph <- igraph::delete_edges(fixed$graph, 1L)
  variants
}

test_that("reused route tables must represent each graph edge exactly once", {
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".pdf")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  fixed <- layout_metro_dag(metrodag_examples("diamond"), direction = "right")
  sentinel <- ggplot2::ggplot()
  ggplot2::set_last_plot(sentinel)
  variants <- .test_incompatible_routes(fixed)
  for (name in names(variants)) {
    input <- variants[[name]]
    before <- input
    for (fun in list(metro_dag, plot_metro_dag, ggplot2::autoplot)) {
      ggplot2::set_last_plot(sentinel)
      expect_error(fun(input, labels = FALSE), "Layout edge rows", info = name)
      expect_identical(ggplot2::last_plot(), sentinel, info = name)
    }
    expect_error(compare_metro_dags(input, list(list()), labels = FALSE),
      "Layout edge rows", info = name)
    expect_identical(ggplot2::last_plot(), sentinel, info = name)
    expect_error(save_metro_dag(file, input, width = 180, height = 140),
      "Layout edge rows", info = name)
    expect_false(file.exists(file))
    expect_identical(ggplot2::last_plot(), sentinel, info = name)
    expect_error(diagnose_metro_dag(input, width = 180, height = 140),
      "Layout edge rows", info = name)
    expect_identical(ggplot2::last_plot(), sentinel, info = name)
    expect_error(layout_metrics(input), "Layout edge rows", info = name)
    expect_identical(input, before, info = name)
  }
})

test_that("valid route identities survive graph order and opaque ID changes", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit({ unlink(files); ggplot2::set_last_plot(previous) }, add = TRUE)
  fixed <- layout_metro_dag(metrodag_examples("diamond"), direction = "right")
  expect_no_warning(save_metro_dag(files[1L], metro_dag(fixed, labels = FALSE,
    legend = FALSE), width = 180, height = 140))
  primitives <- function(file) .svg_primitives(file, c("polyline", "polygon", "circle"))
  types <- list(character = function(x) c("A", "A B", "B C", "C")[x],
    factor = function(x) factor(x, levels = c("unused", rev(seq_len(4L)))),
    ordered = function(x) ordered(x, levels = c("unused", rev(seq_len(4L)))),
    large_numeric = function(x) 2^53 + 2 * x,
    empty_label = function(x) c("", "b", "c", "d")[x],
    empty_factor = function(x) factor(c("", "b", "c", "d")[x],
      levels = c("unused", "", "b", "c", "d")),
    nearby_numeric = function(x) 1 + x * 1e-15)
  for (name in names(types)) {
    input <- fixed
    input$edges <- input$edges[nrow(input$edges):1L, , drop = FALSE]
    input$edges$edge_id <- types[[name]](input$edges$edge_id)
    input$graph <- igraph::permute(input$graph, c(4L, 2L, 1L, 3L))
    before <- input
    expect_no_warning(plot <- metro_dag(input, labels = FALSE, legend = FALSE))
    expect_no_warning(report <- diagnose_metro_dag(plot, width = 180, height = 140,
      device = "svg"))
    expect_true(report$ok, info = name)
    expect_equal(nrow(report$issues), 0L, info = name)
    expect_no_warning(save_metro_dag(files[2L], plot, width = 180, height = 140))
    expect_identical(primitives(files[2L]), primitives(files[1L]), info = name)
    expect_equal(.svg_count(files[2L], "polygon"), 4L)
    expect_equal(layout_metrics(input), layout_metrics(fixed), info = name)
    expect_identical(attr(plot, "metrodag_layout")$edges, before$edges)
    expect_identical(input, before)
  }
})

test_that("route topology checks retain edgeless graphs and literal endpoint names", {
  previous <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(previous), add = TRUE)
  graphs <- list(igraph::make_empty_graph(1L, directed = TRUE),
    igraph::make_empty_graph(3L, directed = TRUE),
    igraph::graph_from_edgelist(matrix(c("A B", "C", "A", "B C",
      "A\nB", "C\nD"), ncol = 2L, byrow = TRUE)))
  for (graph in graphs) {
    fixed <- layout_metro_dag(graph, direction = "right")
    before <- fixed
    for (fun in list(metro_dag, plot_metro_dag, ggplot2::autoplot)) {
      expect_no_warning(plot <- fun(fixed, labels = FALSE, legend = FALSE))
      expect_identical(attr(plot, "metrodag_layout")$edges, before$edges)
    }
    expect_no_warning(report <- diagnose_metro_dag(plot, width = 180, height = 140))
    expect_true(report$ok)
    expect_equal(nrow(report$issues), 0L)
    expect_no_warning(layout_metrics(fixed))
    expect_identical(fixed, before)
  }
  # These two different endpoint pairs both paste to "A B C" with a space.
  # Replacing one with the other must not hide a missing edge and a duplicate.
  input <- fixed
  route <- input$edges$from == "A B" & input$edges$to == "C"
  input$edges$from[route] <- "A"
  input$edges$to[route] <- "B C"
  before <- input
  last <- ggplot2::last_plot()
  expect_error(metro_dag(input), "Layout edge rows")
  expect_error(compare_metro_dags(input, list(list())), "Layout edge rows")
  expect_error(layout_metrics(input), "Layout edge rows")
  expect_identical(ggplot2::last_plot(), last)
  expect_identical(input, before)
})
