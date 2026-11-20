test_that("arrowheads remain above all route casings in exported figures", {
  skip_if_not_installed("svglite")
  # The port-ordering case below is defined on the routes from before the
  # larger half-grid budget, which reroutes this graph with fewer crossings.
  old <- options(metrodag.control = list(budgets = c(half_grid = 12000)))
  on.exit(options(old), add = TRUE)
  p <- metro_dag(metrodag_examples("random", n = 12, probability = 0.3, seed = 11),
    labels = FALSE, legend = FALSE)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  # Station v10 reports its touching heads (arrowhead_overlap; the narrow fan
  # left them as close, unreported); nothing else may warn.
  reported <- character()
  expect_no_warning(withCallingHandlers(save_metro_dag(file, p, width = 260, height = 220),
    metrodag_render_warning = function(w) {
      reported <<- c(reported, w$issues$type)
      invokeRestart("muffleWarning")
    }))
  expect_true(all(reported == "arrowhead_overlap"))
  svg <- readLines(file)
  heads <- which(grepl("<polygon", svg, fixed = TRUE))
  casings <- which(grepl("<polyline", svg, fixed = TRUE) & grepl("stroke: #FFFFFF", svg, fixed = TRUE))
  expect_length(heads, igraph::ecount(attr(p, "metrodag_layout")$graph))
  expect_length(casings, length(heads))
  expect_lt(max(casings), min(heads))
  # Geometric port ordering separates these two arrows, whose terminal casing
  # previously met the earlier head. Retain that clearance as well as painting
  # every head above all casings, which protects other crowded intersections.
  points <- function(line) matrix(as.numeric(strsplit(
    trimws(sub(".* points='([^']+)'.*", "\\1", line)), "[, ]+")[[1L]]), ncol = 2L, byrow = TRUE)
  edges <- attr(p, "metrodag_layout")$edges
  edges <- edges[!duplicated(edges$edge_id), ]
  # Exported arrows follow endpoint order rather than input edge IDs.
  edges <- edges[order(edges$from, edges$to), ]
  i <- which(edges$from == "v5" & edges$to == "v10")
  j <- which(edges$from == "v6" & edges$to == "v10")
  triangle <- points(svg[heads[i]]); later <- points(svg[casings[j]])
  distance <- min(.point_segment_distance(triangle[1L, 1L], triangle[1L, 2L],
    head(later[, 1L], -1L), head(later[, 2L], -1L), tail(later[, 1L], -1L), tail(later[, 2L], -1L)))
  stroke <- function(line) as.numeric(sub(".*stroke-width: ([0-9.]+);.*", "\\1", line))
  expect_gt(distance, (stroke(svg[heads[i]]) + stroke(svg[casings[j]])) / 2)
  # Fanned heads should point into the destination, with their wings outside
  # its fill, even when the shaft's final segment approaches at another angle.
  circles <- svg[grepl("<circle", svg, fixed = TRUE)]
  id <- match("v10", attr(p, "metrodag_layout")$nodes$node)
  center <- c(as.numeric(sub(".* cx='([^']+)'.*", "\\1", circles[id])),
    as.numeric(sub(".* cy='([^']+)'.*", "\\1", circles[id])))
  for (edge in c(i, j)) {
    head <- points(svg[heads[edge]])
    direction <- head[1L, ] - colMeans(head[-1L, , drop = FALSE])
    target <- center - head[1L, ]
    expect_gt(sum(direction * target) / sqrt(sum(direction^2) * sum(target^2)), 0.999)
  }
  expect_true(diagnose_metro_dag(p, width = 260, height = 220, device = "svg")$ok)
  p <- metro_dag(metrodag_examples("chain"), labels = FALSE, arrows = FALSE)
  save_metro_dag(file, p)
  expect_false(any(grepl("<polygon", readLines(file), fixed = TRUE)))
})

test_that("attachments cannot enter their own station interiors", {
  # Station scaling does not move stations: lay the star out once.
  edges <- layout_metro_dag(cbind(paste0("P", seq_len(15L)), "Y"))
  # Unbounded degree scaling makes Y large enough that a fanned attachment
  # turns back through its interior; the capped default renders cleanly.
  p <- metro_dag(edges, labels = FALSE, legend = FALSE,
    style = metro_style(station_scaling = "degree"))
  report <- diagnose_metro_dag(p, width = 220, height = 200)
  expect_false(report$ok)
  capped <- metro_dag(edges, labels = FALSE, legend = FALSE)
  expect_true(diagnose_metro_dag(capped, width = 220, height = 200)$ok)
  conflict <- report$issues[report$issues$type == "edge_endpoint", ]
  expect_gt(nrow(conflict), 0L)
  expect_true(any(conflict$from == "P9" & conflict$to == "Y" & conflict$node == "Y"))
  expect_true(all(conflict$severity == "error"))
  expect_null(p$layers[[2L]]$geom_params$config$diagnostic_state)
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  expect_error(save_metro_dag(file, p, width = 220, height = 200), "enters endpoint station")
  expect_false(file.exists(file))
})

test_that("automatic sizing follows fixed and themed panel proportions", {
  skip_if_not_installed("svglite")
  p <- metro_dag(igraph::make_ring(12, directed = TRUE, circular = FALSE),
    labels = FALSE, legend = FALSE)
  for (change in list(ggplot2::coord_fixed(ratio = 3, clip = "off"),
      ggplot2::theme(aspect.ratio = 1))) {
    q <- suppressMessages(p + change)
    report <- diagnose_metro_dag(q, width = 180, device = "svg")
    expect_true(report$ok)
    expect_gt(report$dimensions[["height"]], 100)
    expect_equal(report$panels$width, 170, tolerance = 0.1)
    expect_gt(report$panels$height, 90)
    file <- tempfile(fileext = ".svg")
    expect_no_warning(save_metro_dag(file, q, width = 180))
    expect_equal(.svg_count(file, "polygon"), 11L)
    unlink(file)
  }
})

test_that("comparison sizing follows the displayed coordinates in every panel", {
  skip_if_not_installed("svglite")
  g <- igraph::make_ring(12, directed = TRUE, circular = FALSE)
  comparison <- compare_metro_dags(g, list(First = list(), Second = list()), labels = FALSE)
  comparison$plots <- lapply(comparison$plots, function(p)
    suppressMessages(p + ggplot2::coord_flip()))
  report <- diagnose_metro_dag(comparison, width = 180, device = "svg")
  expect_true(report$ok)
  expect_gt(report$dimensions[["height"]], 200)
  expect_gt(min(report$panels$height), 190)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  expect_no_warning(save_metro_dag(file, comparison, width = 180))
  expect_equal(.svg_count(file, "polygon"), 22L)
})

test_that("route, arrowhead and station strokes use their physical widths", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  for (width in c(0.45, 1)) {
    p <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE, line_width = width)
    save_metro_dag(file, p, width = 180)
    svg <- readLines(file)
    stroke <- function(lines) as.numeric(sub(".*stroke-width: ([0-9.]+);.*", "\\1", lines)) * 25.4 / 72
    shafts <- svg[grepl("<polyline", svg, fixed = TRUE) & grepl("stroke: #58636D", svg, fixed = TRUE)]
    heads <- svg[grepl("<polygon", svg, fixed = TRUE)]
    stations <- svg[grepl("<circle", svg, fixed = TRUE)]
    expect_length(shafts, 3L)
    expect_length(heads, 3L)
    expect_length(stations, 4L)
    expect_true(all(abs(stroke(shafts) - width) < 0.003))
    expect_true(all(abs(stroke(heads) - width) < 0.003))
    expect_true(all(abs(stroke(stations) - max(0.32, width * 0.8)) < 0.003))
  }
})
