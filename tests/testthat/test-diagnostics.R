test_that("diagnostics identify station collisions and protect output files", {
  g <- igraph::make_empty_graph(30, directed = TRUE)
  p <- metro_dag(g, direction = "down", rank = rep(0, 30), labels = FALSE)
  report <- diagnose_metro_dag(p, width = 85)
  expect_s3_class(report, "metrodag_diagnostics")
  expect_false(report$ok)
  expect_equal(sum(report$issues$type == "station_overlap"), 29L)
  expect_false(anyNA(report$issues$node))
  expect_false(anyNA(report$issues$other_node))
  expect_true(all(nzchar(report$issues$suggestion)))
  expect_true(diagnose_metro_dag(p, width = 180)$ok)
  expect_null(p$layers[[2L]]$geom_params$config$diagnostic_state)
  # The CRAN subset's check that a failed export keeps an existing figure.
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  .expect_failed_export_keeps_file(file, function(file) save_metro_dag(file, p, width = 85),
    "overlapping pair")
})

test_that("station clearance respects circles, squares and outlines", {
  n <- data.frame(x = c(0, 1.9), y = c(0, 1.9), shape = 21L)
  expect_equal(nrow(.station_overlap_pairs(n, c(1, 1))), 0L)
  n$shape <- 22L
  expect_equal(nrow(.station_overlap_pairs(n, c(1, 1))), 1L)
  n$shape <- c(21L, 22L)
  expect_equal(nrow(.station_overlap_pairs(n, c(1, 1))), 0L)
  expect_equal(nrow(.station_overlap_pairs(n, c(1.2, 1.2))), 1L)
  n$x[2L] <- 1.7; n$y[2L] <- 1.7
  expect_equal(nrow(.station_overlap_pairs(n, c(1, 1))), 1L)
})

test_that("diagnostics collect all short edges and comparison panel identities", {
  p <- metro_dag(cbind(1:17, 2:18), labels = FALSE)
  r <- diagnose_metro_dag(p, width = 85)
  expect_false(r$ok)
  edges <- r$issues[r$issues$type == "edge_short", ]
  expect_equal(nrow(edges), 17L)
  expect_setequal(paste(edges$from, edges$to), paste(1:17, 2:18))
  co <- compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE)
  expect_no_warning(r <- diagnose_metro_dag(co, width = 50, height = 45))
  expect_false(r$ok)
  expect_setequal(r$issues$panel, c("1", "2"))
  expect_setequal(r$panels$panel, c("1", "2"))
  expect_true(all(vapply(co$plots, function(p)
    is.null(p$layers[[2L]]$geom_params$config$diagnostic_state), logical(1))))
})

test_that("diagnostics report warning identities and leave last_plot unchanged", {
  p <- metro_dag(metrodag_examples("chain"), label_nudge = list(A = c(-40, 0)))
  previous <- ggplot2::last_plot()
  expect_no_warning(r <- diagnose_metro_dag(p, width = 85))
  expect_true(r$ok)
  expect_true(any(r$issues$type == "label_bounds" & r$issues$node == "A"))
  expect_identical(ggplot2::last_plot(), previous)
  expect_no_warning(diagnose_metro_dag(layout_metro_dag(direction = "down", metrodag_examples("chain")), width = 85))
  expect_identical(ggplot2::last_plot(), previous)
  expect_no_warning(r <- diagnose_metro_dag(metro_dag(metrodag_examples("chain")), width = 8.5, units = "cm", scale = 2))
  expect_equal(unname(r$dimensions["width"]), 170, tolerance = 0.5)
  expect_error(diagnose_metro_dag(p, width = 0), "positive")
  expect_error(diagnose_metro_dag(p, device = "unknown"), "device")
})
