# Package logo: the "causal" example drawn by metrodag itself in metro style,
# on a navy hexagon. From the package root: R CMD INSTALL . && Rscript data-raw/logo.R
# Needs the png package.
library(metrodag)
library(grid)

navy <- "#1F2A36"
border <- "#0F151B"

# Draw the map at a comfortable physical size on a transparent background,
# then scale the image into the hexagon: bold routes and stations stay in
# proportion to the network. Brighter line colours keep contrast on navy, and
# route casings are off so no white shows around the lines.
p <- metro_dag(metrodag_examples("causal"), labels = FALSE, legend = FALSE,
  style = metro_style("metro", arrow_length = 4.6, outline_width = 1.5,
    casing_width = 0, palette = c("#5AB4F0", "#F2884B", "#34C994")),
  node_size = 7, line_width = 2.2) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = NA, colour = NA),
    panel.background = ggplot2::element_rect(fill = NA, colour = NA))
map_file <- tempfile(fileext = ".png")
save_metro_dag(map_file, p, width = 120, dpi = 600, bg = "transparent")
map <- png::readPNG(map_file)
# Trim the transparent margin around the network.
ink <- map[, , 4] > 0.05
rows <- range(which(rowSums(ink) > 0)); cols <- range(which(colSums(ink) > 0))
map <- map[rows[1]:rows[2], cols[1]:cols[2], , drop = FALSE]

draw_logo <- function() {
  # Pointy-top hexagon filling the device (width:height = sqrt(3):2).
  angle <- seq(90, 390, by = 60) * pi / 180
  hex_x <- 0.5 + 0.5 * cos(angle)[-7] / cos(pi / 6) * 0.95
  hex_y <- 0.5 + 0.5 * sin(angle)[-7] * 0.95
  grid.polygon(hex_x, hex_y, gp = gpar(fill = navy, col = NA))
  grid.raster(map, x = 0.5, y = 0.585, width = 0.74)
  grid.polygon(hex_x, hex_y, gp = gpar(fill = NA, col = border, lwd = 7, linejoin = "mitre"))
  grid.text("metrodag", x = 0.5, y = 0.255,
    gp = gpar(col = "white", fontsize = 34, fontface = "bold"))
}

width_in <- 4 * sqrt(3) / 2   # 3.46 in wide, 4 in tall
png("man/figures/logo.png", width = width_in, height = 4, units = "in",
  res = 300, bg = "transparent", type = "cairo")
draw_logo()
invisible(dev.off())
