test_that("fanned heads follow a complete straight terminal shaft", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  points <- function(line) matrix(as.numeric(strsplit(trimws(sub(
    ".* points='([^']+)'.*", "\\1", line)), "[, ]+")[[1L]]), ncol = 2L, byrow = TRUE)
  for (seed in c(58L, 77L)) {
    graph <- metrodag_examples("random", n = 12, probability = 0.3, seed = seed)
    # Laid out once; direction is a final rotation (test-layout.R).
    layouts <- .test_layouts_by_direction(graph, directions = c("right", "down"))
    for (direction in c("right", "down")) {
      p <- metro_dag(layouts[[direction]], labels = FALSE, legend = FALSE)
      original <- p
      size <- if (direction == "right") c(260, 220) else c(220, 260)
      # Crowded stations report their touching heads (arrowhead_overlap; the
      # narrow fan left them as close, unreported); nothing else may warn.
      reported <- character()
      expect_no_warning(withCallingHandlers(save_metro_dag(file, p, width = size[1L],
        height = size[2L]), metrodag_render_warning = function(w) {
          reported <<- c(reported, w$issues$type)
          invokeRestart("muffleWarning")
        }))
      expect_true(all(reported == "arrowhead_overlap"))
      svg <- readLines(file)
      heads <- lapply(svg[grepl("<polygon", svg, fixed = TRUE)], points)
      shafts <- lapply(svg[grepl("<polyline", svg, fixed = TRUE) &
        grepl("stroke: #58636D", svg, fixed = TRUE)], points)
      expect_length(heads, igraph::ecount(graph))
      expect_length(shafts, length(heads))
      cosine <- numeric(length(heads)); clearance <- numeric(length(heads))
      for (i in seq_along(heads)) {
        shaft <- shafts[[i]]; head <- heads[[i]]
        terminal <- shaft[nrow(shaft), ] - shaft[nrow(shaft) - 1L, ]
        forward <- head[1L, ] - colMeans(head[-1L, , drop = FALSE])
        cosine[i] <- sum(terminal * forward) / sqrt(sum(terminal^2) * sum(forward^2))
        clearance[i] <- sqrt(sum(terminal^2)) - sqrt(sum(forward^2))
        expect_equal(shaft[nrow(shaft), ], head[1L, ])
      }
      expect_true(all(cosine > 0.9999))
      expect_true(all(clearance > 0.1))
      expect_identical(p, original)
    }
  }
})

test_that("labels report arrowhead conflicts and retain edge and panel identities", {
  skip_if_not_installed("svglite")
  graph <- metrodag_examples("chain")
  labels <- c(A = "", B = "B", C = "", D = "")
  args <- list(graph = graph, labels = labels, legend = FALSE,
    label_nudge = list(B = c(-3.84, -2)))
  p <- do.call(metro_dag, args)
  original <- p
  report <- diagnose_metro_dag(p, width = 85, height = 55, device = "svg")
  expect_true(report$ok)
  conflict <- subset(report$issues, type == "label_route")
  expect_equal(conflict$node, "B")
  expect_equal(conflict$from, "A")
  expect_equal(conflict$to, "B")
  expect_equal(conflict$severity, "warning")
  expect_identical(p, original)
  args$arrows <- FALSE
  expect_equal(nrow(diagnose_metro_dag(do.call(metro_dag, args),
    width = 85, height = 55, device = "svg")$issues), 0L)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  expect_warning(save_metro_dag(file, p, width = 85, height = 55), "intersect routes")
  co <- compare_metro_dags(graph, list(First = list(), Second = list()),
    labels = labels, label_nudge = list(B = c(-3.84, -2)))
  report <- diagnose_metro_dag(co, width = 180, height = 80, device = "svg")
  conflict <- subset(report$issues, type == "label_route" & node == "B" & from == "A" & to == "B")
  expect_setequal(conflict$panel, c("1", "2"))
})

test_that("label clearance includes stroke widths and filled head interiors", {
  route <- data.frame(edge_id = 7L, x = -5, y = 0, xend = 8, yend = 0)
  box <- c(2, 2.2, 0.1, 0.2)
  expect_false(any(.segment_hits_box(route, box)))
  expect_identical(.metro_label_edge_hits(route, box, line_width = 0.4), 7L)
  expect_length(.metro_label_edge_hits(route, box), 0L)
  triangle <- rbind(c(8, 0), c(0, 4), c(0, -4))
  boundary <- data.frame(edge_id = 7L, x = triangle[, 1L], y = triangle[, 2L],
    xend = triangle[c(2L, 3L, 1L), 1L], yend = triangle[c(2L, 3L, 1L), 2L])
  edges <- rbind(route, boundary)
  box <- c(2, 2.2, 1, 1.2)
  expect_false(any(.segment_hits_box(edges, box)))
  expect_identical(.metro_label_edge_hits(edges, box,
    arrowheads = list(list(edge_id = 7L, triangle = triangle, bounds = c(0, 8, -4, 4)))), 7L)
  expect_length(.metro_label_edge_hits(edges, c(2, 2.2, 4.1, 4.2),
    arrowheads = list(list(edge_id = 7L, triangle = triangle, bounds = c(0, 8, -4, 4)))), 0L)
})

test_that("choice arguments reject non-character options before dispatch", {
  graph <- metrodag_examples("chain")
  expect_error(layout_metro_dag(graph, direction = factor("right")), "direction")
  expect_error(layout_metro_dag(direction = "down", graph, layering = factor("sinks")), "layering")
  expect_error(layout_metro_dag(direction = "down", graph, ordering = factor("none")), "ordering")
  expect_error(metrodag_examples(factor("dense")), "name")
  expect_error(metrodag_examples(list("dense")), "name")
  expect_error(metro_dag(graph, style = factor("metro")), "style")
  p <- metro_dag(graph)
  file <- tempfile(fileext = ".pdf")
  expect_error(save_metro_dag(file, p, width = 18, units = factor("cm")), "units")
  expect_false(file.exists(file))
  expect_error(diagnose_metro_dag(p, device = factor("pdf")), "device")
  expect_equal(igraph::vcount(metrodag_examples("dense")), 8L)
  expect_equal(igraph::ecount(metrodag_examples("dense")), 16L)
})
