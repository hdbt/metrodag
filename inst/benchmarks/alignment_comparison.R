# Rscript inst/benchmarks/alignment_comparison.R
# Preserve each gallery example's supplied ranks, labels, roles and topology;
# omit only its explicit align option for a separately named comparison image.
library(metrodag)
source(file.path("tests", "testthat", "helper-gallery.R"))
rows <- list()
for (example in .metro_gallery_examples) {
  if (!length(example$align)) next
  parameters <- example[intersect(names(example),
    c("labels", "exposure", "outcome", "adjusted", "unobserved", "unadjusted", "rank"))]
  for (aligned in c(FALSE, TRUE)) {
    params <- c(list(graph = example$edges), parameters)
    if (aligned) params$align <- example$align
    plot <- do.call(metro_dag, params)
    layout <- attr(plot, "metrodag_layout")
    reports <- lapply(c("svg", "png"), function(device)
      diagnose_metro_dag(plot, width = 240, device = device, dpi = 300))
    stopifnot(all(vapply(reports, function(report)
      report$ok && nrow(report$issues) == 0L, logical(1))))
    rows[[paste(example$id, aligned)]] <- cbind(id = example$id,
      align = aligned, layout_metrics(layout), label_warnings_svg = 0L, label_warnings_png = 0L)
    if (!aligned) for (device in c("svg", "png")) save_metro_dag(
      file.path("inst", "benchmarks", "figures", paste0("annotated_", example$id, "_rank_only.", device)),
      plot, width = 240, dpi = 300)
  }
}
write.csv(do.call(rbind, rows), "inst/benchmarks/alignment_comparison_metrics.csv", row.names = FALSE)
