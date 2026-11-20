# Count labels that read as another station's (label_association), labels
# cut off from their station by another arrow (label_cut_off) and labels that
# touch a route (label_route) for the six gallery figures, five DAGitty
# examples and a mediator fan at 85, 120 and 180 mm. Every figure is checked
# with diagnose_metro_dag() on one device: PNG at 300 dpi.
#
# From the package root, after R CMD INSTALL . :
#   Rscript inst/benchmarks/label_association.R <label> [<library>]
# <library> loads metrodag from another library, for example a build of an
# older commit:
#   git worktree add --detach /tmp/before 6acb470^1
#   mkdir -p /tmp/before-lib && R CMD INSTALL -l /tmp/before-lib /tmp/before
#   Rscript inst/benchmarks/label_association.R before /tmp/before-lib
#   Rscript inst/benchmarks/label_association.R after
# Replaces the rows with the same label in inst/benchmarks/label_association.csv.
#
# diagnose_metro_dag() reports label_association only from PR #54 on, and
# cut-off labels only from round 2 on. So that every version is measured the
# same way, the script applies the detector in R/render.R of this checkout to
# the final label boxes and routes of each drawing, and records the counts
# that diagnose_metro_dag() itself reports where it can. Rows written before a
# column existed have NA in it.
args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args) >= 1L) args[[1L]] else "current"
library_path <- if (length(args) >= 2L) normalizePath(args[[2L]], mustWork = TRUE) else NULL
suppressMessages(library(metrodag, lib.loc = library_path))
source(file.path("tests", "testthat", "helper-gallery.R"))
widths <- c(85, 120, 180)
device <- "png"; dpi <- 300

# The detector and its helpers, read from this checkout.
detector <- new.env(parent = baseenv())
for (expr in parse(file.path("R", "render.R"), keep.source = FALSE))
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
    as.character(expr[[2L]]) %in% c(".metro_label_ambiguity", ".metro_nearer", ".metro_cut_off_rim",
      ".metro_rim_distance", ".metro_rim_distances", ".metro_rectangle_distance",
      ".label_route_index", ".label_route_terms", ".label_segment_pairs",
      ".segments_hit_boxes", ".segments_box_distance", ".segments_distance",
      ".point_segment_distance", ".points_in_triangle"))
    eval(expr, detector)

# Record the stations, station radii, final label boxes and drawn routes of
# the first drawing, which has the requested size. A figure that does not fit
# is drawn again while diagnose_metro_dag() checks a larger size.
ns <- asNamespace("metrodag")
drawing <- if (exists(".metro_grob_content", ns, inherits = FALSE))
  ".metro_grob_content" else "makeContent.metrodag_grob"
captured <- new.env()
invisible(suppressMessages(trace(drawing, where = ns, print = FALSE, exit = bquote(
  if (is.null(.(captured)$first) && exists("placement", inherits = FALSE) &&
    exists("radius", inherits = FALSE))
    assign("first", list(nodes = n, boxes = placement$boxes, radius = radius,
      edges = if (exists("label_edges", inherits = FALSE)) label_edges,
      line_width = cfg$line_width), envir = .(captured))))))
# A traced S3 method must be registered again to be dispatched.
if (drawing == "makeContent.metrodag_grob")
  registerS3method("makeContent", "metrodag_grob", get(drawing, ns), envir = asNamespace("grid"))

figures <- list()
for (example in .metro_gallery_examples) {
  arguments <- example[intersect(names(example),
    c("labels", "exposure", "outcome", "adjusted", "unobserved", "unadjusted", "rank", "align"))]
  figures[[paste0("gallery ", example$id)]] <- do.call(metro_dag, c(list(graph = example$edges), arguments))
}
for (name in c("confounding", "Schipf", "Shrier", "Thoemmes", "Kampen"))
  figures[[paste("dagitty", name)]] <- metro_dag(dagitty::getExample(name), bidirected = "latent")
fan <- data.frame(from = c("Pre", rep("Exposure", 5), paste("Mediator", 1:5)),
  to = c("Exposure", paste("Mediator", 1:5), rep("Outcome", 5)))
figures[["mediator fan"]] <- metro_dag(fan, exposure = "Exposure", outcome = "Outcome")

rows <- list()
for (name in names(figures)) for (width in widths) {
  captured$first <- NULL
  report <- diagnose_metro_dag(figures[[name]], width = width, device = device, dpi = dpi)
  final <- captured$first
  nodes <- attr(figures[[name]], "metrodag_layout")$nodes
  labelled <- if (is.null(final)) NA_integer_ else sum(!is.na(final$boxes[, 1L]))
  found <- if (is.null(final)) NULL else detector$.metro_label_ambiguity(final$nodes,
    final$boxes, final$radius, edges = final$edges, line_width = final$line_width)
  count <- function(rows) if (is.null(found)) NA_integer_ else length(unique(found$node[rows]))
  issues <- report$issues
  association <- issues$type == "label_association"
  rows[[length(rows) + 1L]] <- data.frame(label = label, figure = name, width = width,
    device = sprintf("%s %d dpi", device, dpi), ok = report$ok, nodes = nrow(nodes),
    labels = labelled, label_association = count(found$relation != "cut off"),
    reported_association = length(unique(issues$node[association & !is.na(issues$other_node)])),
    label_cut_off = count(found$relation == "cut off"),
    reported_cut_off = length(unique(issues$node[association & is.na(issues$other_node)])),
    label_route = length(unique(issues$node[issues$type == "label_route"])))
}
result <- do.call(rbind, rows)
print(result[, setdiff(names(result), c("label", "device"))], row.names = FALSE)
for (width in widths) {
  s <- result[result$width == width & result$ok, ]
  cat(sprintf("%s, %d mm (%d of %d figures without errors): label_association %d of %d labels, label_cut_off %d, label_route %d\n",
    label, width, nrow(s), length(figures), sum(s$label_association), sum(s$labels),
    sum(s$label_cut_off), sum(s$label_route)))
}
path <- file.path("inst", "benchmarks", "label_association.csv")
if (file.exists(path)) {
  previous <- utils::read.csv(path, stringsAsFactors = FALSE)
  previous <- previous[previous$label != label, , drop = FALSE]
  previous[setdiff(names(result), names(previous))] <- NA
  result <- rbind(previous[names(result)], result)
}
utils::write.csv(result, path, row.names = FALSE)
