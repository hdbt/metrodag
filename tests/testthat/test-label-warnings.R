# Label warnings name the first label and what it meets, and count the
# labels affected, as label_association warnings do.

.test_label_warning <- function(issues, type) {
  rows <- issues[issues$type == type, , drop = FALSE]
  expect_gt(nrow(rows), 0L)
  if (!nrow(rows)) return(invisible(NULL))
  first <- rows[1L, ]
  message <- first$message
  expect_false(grepl("Some MetroDAG labels", message, fixed = TRUE))
  expect_match(message, paste0("The label of ", first$node, " "), fixed = TRUE)
  if (!is.na(first$from))
    expect_match(message, paste0("arrow ", first$from, " -> ", first$to), fixed = TRUE)
  if (type == "label_overlap")
    expect_match(message, paste("the label of", first$other_node), fixed = TRUE)
  if (type == "label_station") expect_match(message, if (first$other_node == first$node)
    "its own station" else paste("station", first$other_node), fixed = TRUE)
  affected <- length(unique(c(rows$node, if (type == "label_overlap") rows$other_node)))
  if (affected > 1L) {
    expect_match(message, sprintf("; %d labels are affected.", affected), fixed = TRUE)
  } else expect_false(grepl("labels are affected", message, fixed = TRUE))
  invisible(first)
}

test_that("label warnings name the label, what it meets and how many are affected", {
  skip_on_cran()
  labels <- c(C = "Socioeconomic background", X = "Cigarette consumption", M = "Tar deposit in lung",
    Y = "Lung cancer risk", U = "Unmeasured inheritance", S = "Occupational hazards")
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y", labels = labels)
  report <- diagnose_metro_dag(p, width = 40)
  for (type in c("label_route", "label_bounds", "label_station")) .test_label_warning(report$issues, type)
  # The issue types are unchanged.
  expect_true(all(report$issues$type[report$issues$severity == "warning"] %in%
    c("label_route", "label_bounds", "label_station", "label_association", "label_overlap")))
  # A warning that is raised says the same, as does its summary.
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  warnings <- character()
  withCallingHandlers(save_metro_dag(file, p, width = 40, on_error = "warn"),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("drawing warnings at this size. First: (MetroDAG labels|The label of)", warnings)))
  expect_false(any(grepl("Some MetroDAG labels", warnings, fixed = TRUE)))
})

test_that("overlapping labels name both labels", {
  graph <- metrodag_examples("chain")
  p <- metro_dag(graph, legend = FALSE)
  nodes <- Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)$geom_params$nodes
  range <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]$x.range
  panel <- diagnose_metro_dag(p, width = 85)$panels$width[1L]
  # Move the labels of A and B to the point halfway between their stations.
  half <- diff(nodes$x[match(c("A", "B"), nodes$node)]) / diff(range) * panel / 2
  q <- metro_dag(graph, legend = FALSE, label_nudge = list(A = c(half, 0), B = c(-half, 0)))
  report <- diagnose_metro_dag(q, width = 85)
  first <- .test_label_warning(report$issues, "label_overlap")
  expect_setequal(c(first$node, first$other_node), c("A", "B"))
  expect_match(first$message, "MetroDAG labels overlap at this device size. The label of A overlaps the label of B; 2 labels are affected.",
    fixed = TRUE)
})

test_that("a label on its own station says so", {
  skip_if_not_installed("svglite")
  graph <- igraph::make_empty_graph(1L, directed = TRUE)
  igraph::V(graph)$name <- "A"
  p <- metro_dag(graph, legend = FALSE, node_size = 10)
  # Where the label is drawn relative to its station, in millimetres.
  file <- tempfile(fileext = ".svg")
  svglite::svglite(file, width = 85 / 25.4, height = 85 / 25.4)
  geometry <- tryCatch({
    state <- new.env(parent = emptyenv())
    state$issues <- state$panels <- list()
    table <- .metro_plot_gtable(.metro_diagnostic_plot(p, state))
    panel <- table$grobs[[which(table$layout$name == "panel")]]
    grob <- panel$children[[which(vapply(panel$children, inherits, logical(1), "metrodag_grob"))]]
    grid::makeContent(grob)$diagnostics
  }, finally = { grDevices::dev.off(); unlink(file) })
  offset <- unname(geometry$label_centers[1L, ] - unlist(geometry$nodes[1L, c("x", "y")]))
  q <- metro_dag(graph, legend = FALSE, node_size = 10, label_nudge = list(A = -offset))
  report <- diagnose_metro_dag(q, width = 85, height = 85, device = "svg")
  first <- .test_label_warning(report$issues, "label_station")
  expect_identical(first$other_node, "A")
  expect_match(first$message, "The label of A overlaps its own station.", fixed = TRUE)
  # The text matched by older checks remains.
  expect_match(first$message, "labels overlap stations", fixed = TRUE)
})
