# Display categories for different kinds of unmeasured factors. These names
# are descriptive annotations, not causal types or selection assumptions.
# From the package root after R CMD INSTALL .:
# Rscript data-raw/node_groups.R
library(metrodag)

edges <- matrix(strsplit("Us X Us P Uo X Uo Y C X C Y X M M Y X Y Y P",
  " ", fixed = TRUE)[[1L]], ncol = 2L, byrow = TRUE)
groups <- list("Unmeasured selection factors" = "Us", "Other unmeasured factors" = "Uo")
colours <- c("Unmeasured selection factors" = "#7A5195", "Other unmeasured factors" = "#006D5B")
labels <- c(Us = "Selection influences", Uo = "Other influences", C = "Measured covariate",
  X = "Exposure", M = "Mediator", Y = "Outcome", P = "Study entry")
publication <- metro_dag(edges, exposure = "X", outcome = "Y", unobserved = c("Us", "Uo"),
  labels = labels, node_groups = groups, node_group_colours = colours)
fixed <- attr(publication, "metrodag_layout")
monochrome <- metro_dag(fixed, labels = labels, style = "monochrome")
comparison <- compare_metro_dags(fixed, list(
  Unadjusted = list(unadjusted = TRUE),
  `Covariate-conditioned` = list(adjusted = "C")), labels = labels)

plots <- list(node_groups = publication, node_groups_monochrome = monochrome,
  node_groups_comparison = comparison)
output <- file.path("inst", "benchmarks", "figures")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
checks <- list()
for (name in names(plots)) {
  widths <- if (name == "node_groups_comparison") 240 else c(120, 180)
  for (width in widths) for (device in c("svg", "png")) {
    report <- diagnose_metro_dag(plots[[name]], width = width, device = device, dpi = 300)
    if (!report$ok || nrow(report$issues)) stop("Inspect ", name, " at ", width, " mm: ",
      paste(report$issues$message, collapse = "; "))
    panels <- if (name == "node_groups_comparison") 2L else 1L
    expected <- nrow(edges) * panels
    file <- if (width == max(widths)) file.path(output, paste0(name, ".", device)) else
      tempfile(fileext = paste0(".", device))
    save_metro_dag(file, plots[[name]], width = width, dpi = 300)
    heads <- if (device == "svg") sum(grepl("<polygon", readLines(file), fixed = TRUE)) else NA_integer_
    if (device == "svg") stopifnot(heads == expected)
    if (width != max(widths)) unlink(file)
    checks[[length(checks) + 1L]] <- data.frame(example = name, width_mm = width,
      device = device, expected_edges = expected, svg_arrowheads = heads,
      errors = 0L, warnings = 0L)
  }
}
write.csv(do.call(rbind, checks), "inst/benchmarks/node_groups_render_checks.csv", row.names = FALSE)
# Both views are illustrations; the unmeasured common causes are retained.
# Marking Us as a display group does not encode conditioning on participation.
