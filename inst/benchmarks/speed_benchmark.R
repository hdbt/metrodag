# Layout and export timings for representative graphs.
# From the package root: R CMD INSTALL . && Rscript inst/benchmarks/speed_benchmark.R [label]
# Writes or updates inst/benchmarks/speed_metrics.csv (one row per graph and label).
suppressMessages(library(metrodag))
label <- commandArgs(trailingOnly = TRUE)
label <- if (length(label)) label[[1L]] else "current"
repeats <- 3L
graphs <- list(
  causal = metrodag_examples("causal"),
  dense = metrodag_examples("dense"),
  random_50 = metrodag_examples("random", n = 50, probability = 2 / 50, seed = 2026),
  random_100 = metrodag_examples("random", n = 100, probability = 2 / 100, seed = 2026))
median_time <- function(expr) {
  expr <- substitute(expr); env <- parent.frame()
  stats::median(vapply(seq_len(repeats), function(i)
    system.time(eval(expr, env))[["elapsed"]], numeric(1)))
}
rows <- lapply(names(graphs), function(name) {
  graph <- graphs[[name]]
  layout_seconds <- median_time(layout <- layout_metro_dag(graph))
  export_seconds <- if (name %in% c("causal", "dense")) {
    plot <- plot_metro_dag(layout)
    file <- tempfile(fileext = ".pdf")
    on.exit(unlink(file), add = TRUE)
    median_time(save_metro_dag(file, plot))
  } else NA_real_
  data.frame(label = label, graph = name, nodes = igraph::vcount(graph),
    edges = igraph::ecount(graph), layout_seconds = layout_seconds,
    export_seconds = export_seconds, r_version = paste(R.version$major, R.version$minor, sep = "."))
})
result <- do.call(rbind, rows)
print(result, row.names = FALSE)
path <- file.path("inst", "benchmarks", "speed_metrics.csv")
if (file.exists(path)) {
  previous <- utils::read.csv(path, stringsAsFactors = FALSE)
  result <- rbind(previous[previous$label != label, , drop = FALSE], result)
}
utils::write.csv(result, path, row.names = FALSE)
