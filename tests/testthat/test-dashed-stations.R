# The Windows GDI device draws a dashed circle solid, so dashed (unobserved)
# station outlines and their legend key are drawn as polygons on the same
# circle (.metro_circle_grob()).

# Dark pixels in an uncompressed 8-bit (palette) or 24-bit BMP, the formats
# that bmp(type = "windows") writes.
.test_bmp_dark_pixels <- function(file) {
  bytes <- readBin(file, "raw", file.info(file)$size)
  int <- function(at, size) readBin(bytes[at + seq_len(size)], "integer", size = size, endian = "little")
  width <- int(18L, 4L); height <- abs(int(22L, 4L)); bits <- int(28L, 2L)
  stopifnot(int(30L, 4L) == 0L, bits %in% c(8L, 24L))
  stride <- 4L * ((bits * width + 31L) %/% 32L)
  rows <- matrix(as.integer(bytes[int(10L, 4L) + seq_len(stride * height)]), nrow = stride)
  if (bits == 24L) return(sum(colMeans(matrix(rows[seq_len(3L * width), ], nrow = 3L)) < 128))
  colours <- int(46L, 4L)
  if (colours == 0L) colours <- 256L
  palette <- matrix(as.integer(bytes[14L + int(14L, 4L) + seq_len(4L * colours)]), nrow = 4L)
  sum(colMeans(palette[1:3, , drop = FALSE])[rows[seq_len(width), ] + 1L] < 128)
}

test_that("dashed station outlines are polygons on the station's circle", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  mm <- function(x) grid::unit(x, "mm")
  gp <- grid::gpar(col = "#24292E", fill = "white", lwd = 1.36, lty = "dashed", linejoin = "round")
  station <- .metro_station_grob(mm(10), mm(20), mm(1.5), 21L, "plain", gp, "white")
  expect_s3_class(station, "polygon")
  expect_identical(station$gp, gp)
  x <- grid::convertX(station$x, "mm", valueOnly = TRUE)
  y <- grid::convertY(station$y, "mm", valueOnly = TRUE)
  expect_length(x, 72L)
  expect_equal(sqrt((x - 10)^2 + (y - 20)^2), rep(1.5, 72L))
  # Clockwise from three o'clock, as Cairo and ragg draw a circle.
  expect_equal(c(x[1L], y[1L]), c(11.5, 20))
  expect_lt(y[2L], 20)
  # Solid outlines stay circles of the same radius.
  for (lty in list("solid", 1L, NULL)) {
    solid_gp <- gp
    solid_gp$lty <- lty
    solid <- .metro_station_grob(mm(10), mm(20), mm(1.5), 21L, "plain", solid_gp, "white")
    expect_s3_class(solid, "circle")
    expect_equal(grid::convertWidth(solid$r, "mm", valueOnly = TRUE), 1.5)
  }
  # A dashed ring keeps its filled centre, which has no outline. The GDI
  # device dashes rectangles, so dashed squares stay rectangles.
  ring <- .metro_station_grob(mm(10), mm(20), mm(1.5), 21L, "ring", gp, "white")
  expect_s3_class(ring$children[[1L]], "polygon")
  expect_s3_class(ring$children[[2L]], "circle")
  expect_s3_class(.metro_station_grob(mm(10), mm(20), mm(1.5), 22L, "plain", gp, "white"), "rect")
})

test_that("the Unobserved legend key is drawn as its stations are", {
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y", unobserved = "U")
  values <- attr(p, "metrodag_legend_values")
  dashed <- values$linetype == "dashed"
  expect_equal(sum(dashed), 1L)
  keys <- lapply(seq_len(nrow(values)), function(i) .draw_key_metro_node(values[i, ], list(), 4))
  expect_true(all(vapply(keys[dashed], inherits, logical(1), "polygon")))
  expect_true(all(vapply(keys[!dashed], inherits, logical(1), "circle")))
  expect_identical(keys[dashed][[1L]]$gp$lty, "dashed")
})

test_that("exports draw dashed stations and keys as polygons without moving stations", {
  skip_if_not_installed("svglite")
  files <- c(tempfile(fileext = ".svg"), tempfile(fileext = ".svg"))
  on.exit(unlink(files), add = TRUE)
  graph <- metrodag_examples("causal")
  # Roles never move stations: U is drawn at the same place, observed or not.
  plots <- list(metro_dag(graph, exposure = "X", outcome = "Y", unobserved = "U", legend = FALSE),
    metro_dag(graph, exposure = "X", outcome = "Y", legend = FALSE))
  for (i in 1:2) expect_no_warning(save_metro_dag(files[i], plots[[i]], width = 120))
  dashed <- readLines(files[1L]); solid <- readLines(files[2L])
  value <- function(lines, key) as.numeric(sub(paste0(".* ", key, "='([^']+)'.*"), "\\1", lines))
  outline <- dashed[grepl("<polygon ", dashed, fixed = TRUE) &
    grepl("stroke-dasharray", dashed, fixed = TRUE)]
  expect_length(outline, 1L)
  xy <- matrix(as.numeric(strsplit(trimws(sub(".* points='([^']+)'.*", "\\1", outline)),
    "[, ]+")[[1L]]), ncol = 2L, byrow = TRUE)
  expect_equal(nrow(xy), 72L)
  circles <- solid[grepl("<circle", solid, fixed = TRUE)]
  u <- which.min((value(circles, "cx") - mean(xy[, 1L]))^2 + (value(circles, "cy") - mean(xy[, 2L]))^2)
  distance <- sqrt((xy[, 1L] - value(circles[u], "cx"))^2 + (xy[, 2L] - value(circles[u], "cy"))^2)
  # svglite writes two decimals.
  expect_true(all(abs(distance - value(circles[u], "r")) <= 0.015))
  expect_equal(sum(grepl("<circle", dashed, fixed = TRUE)), length(circles) - 1L)
  expect_equal(.svg_arrowheads(files[1L]), igraph::ecount(graph))
  # With a legend, the Unobserved key is a dashed polygon too, and no dashed
  # circle is left for a device to draw solid.
  expect_no_warning(save_metro_dag(files[1L], metro_dag(graph, exposure = "X", outcome = "Y",
    unobserved = "U"), width = 120))
  svg <- readLines(files[1L])
  expect_equal(.svg_dashed_outlines(files[1L]), 2L)
  expect_false(any(grepl("<circle", svg, fixed = TRUE) &
    grepl("stroke-dasharray", svg, fixed = TRUE)))
})

test_that("the Windows GDI device draws dashed stations and legend keys dashed", {
  skip_on_cran()
  skip_if_not(identical(.Platform$OS.type, "windows"), "The GDI device is only on Windows.")
  file <- tempfile(fileext = ".bmp")
  on.exit(unlink(file), add = TRUE)
  dark <- function(grob, size) {
    grDevices::bmp(file, width = size, height = size, res = 300, type = "windows", bg = "white")
    device <- grDevices::dev.cur()
    tryCatch(grid::grid.draw(grob), finally = grDevices::dev.off(device))
    .test_bmp_dark_pixels(file)
  }
  centre <- grid::unit(0.5, "npc")
  ink <- vapply(c("solid", "dashed"), function(lty) {
    gp <- grid::gpar(col = "black", fill = "white", lwd = 1.36, lty = lty, linejoin = "round")
    station <- .metro_station_grob(centre, centre, grid::unit(5, "mm"), 21L, "plain", gp, "white")
    key <- data.frame(colour = "black", fill = "white", linetype = lty, shape = 21L)
    c(station = dark(station, 300L), key = dark(.draw_key_metro_node(key, list(), 4), 48L))
  }, numeric(2))
  # A dashed circle comes out solid here, with as many dark pixels as a solid
  # one; the polygon leaves gaps.
  expect_true(all(ink[, "solid"] > 0))
  expect_true(all(ink[, "dashed"] < 0.8 * ink[, "solid"]))
})
