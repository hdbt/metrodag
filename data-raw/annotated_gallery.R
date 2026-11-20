# Six annotated examples used by the README evaluation gallery.
# From the package root: R CMD INSTALL .
# Then: Rscript data-raw/annotated_gallery.R
# Source this file interactively to inspect gallery_plots and gallery_layouts.
# No coordinates, routes or label nudges are supplied. DAG 6 fixes wave ranks
# and aligns repeated measures in two rows using their semantic identities.
# These are illustrative causal structures with manually specified annotations.

library(metrodag)

source(file.path("tests", "testthat", "helper-gallery.R"))
examples <- .metro_gallery_examples

output_dir <- file.path("inst", "benchmarks", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
gallery_plots <- gallery_layouts <- gallery_diagnostics <- list()
metrics <- issues <- list()
for (example in examples) {
  parameters <- example[intersect(names(example),
    c("labels", "exposure", "outcome", "adjusted", "unobserved", "unadjusted", "rank", "align"))]
  plot <- do.call(metro_dag, c(list(graph = example$edges), parameters))
  layout <- attr(plot, "metrodag_layout")
  gallery_plots[[example$id]] <- plot
  gallery_layouts[[example$id]] <- layout
  reports <- lapply(c("svg", "png"), function(device)
    diagnose_metro_dag(plot, width = 240, device = device, dpi = 300))
  names(reports) <- c("svg", "png")
  gallery_diagnostics[[example$id]] <- reports
  metrics[[example$id]] <- cbind(id = example$id, title = example$title,
    layout_metrics(layout),
    label_warnings_svg = sum(reports$svg$issues$severity == "warning"),
    label_warnings_png = sum(reports$png$issues$severity == "warning"))
  for (device in names(reports)) {
    report <- reports[[device]]
    # Preserve and report label warnings so the gallery remains useful for
    # evaluating unresolved cases. Never publish a structurally invalid export.
    if (nrow(report$issues)) issues[[paste(example$id, device)]] <-
      cbind(id = example$id, device = device, report$issues)
    if (!report$ok) stop("Cannot render ", example$id, " on ", device,
      ": inspect gallery_diagnostics.")
    file <- file.path(output_dir, paste0("annotated_", example$id, ".", device))
    save_metro_dag(file, plot, width = 240, dpi = 300)
  }
  message(example$id, ": ", layout_metrics(layout)$crossings, " crossings; ",
    layout_metrics(layout)$total_bends, " bends; ",
    sum(reports$svg$issues$severity == "warning"), " SVG label warnings; ",
    sum(reports$png$issues$severity == "warning"), " PNG label warnings")
}
gallery_metrics <- do.call(rbind, metrics)
write.csv(gallery_metrics, file.path("inst", "benchmarks", "annotated_gallery_metrics.csv"),
  row.names = FALSE)
# Write even when empty, so a later clean regeneration removes stale warnings.
gallery_issues <- if (length(issues)) do.call(rbind, issues) else
  cbind(id = character(), device = character(), reports$svg$issues)
write.csv(gallery_issues, file.path("inst", "benchmarks", "annotated_gallery_diagnostics.csv"),
  row.names = FALSE)

# Also exercise single-column sizing without replacing the 240 mm gallery.
# SVG primitives provide a separate arrow-count check; PNG visibility is
# checked by rendering diagnostics and visual review, not a fabricated count.
check_gallery_rendering <- function(plots, diagnostics) {
  rows <- list()
  temporary_files <- character()
  on.exit(unlink(temporary_files), add = TRUE)
  for (id in names(plots)) for (width in c(180, 240)) for (device in c("svg", "png")) {
    plot <- plots[[id]]
    report <- if (width == 240) diagnostics[[id]][[device]] else
      diagnose_metro_dag(plot, width = width, device = device, dpi = 300)
    expected <- igraph::ecount(attr(plot, "metrodag_layout")$graph)
    heads <- NA_integer_
    if (device == "svg") {
      file <- if (width == 240) file.path("inst", "benchmarks", "figures",
        paste0("annotated_", id, ".svg")) else tempfile(fileext = ".svg")
      if (width != 240) {
        temporary_files <- c(temporary_files, file)
        save_metro_dag(file, plot, width = width, dpi = 300)
      }
      heads <- sum(grepl("<polygon", readLines(file), fixed = TRUE))
      stopifnot(heads == expected)
      if (width != 240) unlink(file)
    }
    stopifnot(report$ok)
    rows[[paste(id, width, device)]] <- data.frame(id = id, width_mm = width, device = device,
      expected_edges = expected, svg_arrowheads = heads,
      errors = sum(report$issues$severity == "error"),
      warnings = sum(report$issues$severity == "warning"))
  }
  do.call(rbind, rows)
}
write.csv(check_gallery_rendering(gallery_plots, gallery_diagnostics),
  file.path("inst", "benchmarks", "annotated_gallery_render_checks.csv"), row.names = FALSE)

# Interactive use after sourcing this file:
# gallery_plots[["01_alcohol_heart"]]
# gallery_metrics
# gallery_diagnostics[["05_obesity_admission"]]$svg$issues
