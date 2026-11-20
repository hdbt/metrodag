# Count S-shaped hooks in drawn arrows for the gallery, node-groups and
# DAGitty example figures. From the package root, after R CMD INSTALL . :
#   Rscript inst/benchmarks/arrow_hooks.R <label>
# Appends one row per figure and width to inst/benchmarks/arrow_hooks.csv.
suppressMessages(library(metrodag))
source(file.path("tests", "testthat", "helper-gallery.R"))
source(file.path("tests", "testthat", "helper-hooks.R"))
label <- commandArgs(trailingOnly = TRUE)
label <- if (length(label)) label[[1L]] else "current"

figures <- list()
for (example in .metro_gallery_examples) {
  args <- example[intersect(names(example),
    c("labels", "exposure", "outcome", "adjusted", "unobserved", "unadjusted", "rank", "align"))]
  figures[[paste0("gallery ", example$id)]] <- do.call(metro_dag, c(list(graph = example$edges), args))
}
groups <- igraph::graph_from_edgelist(rbind(
  c("Us", "X"), c("Us", "P"), c("Uo", "X"), c("Uo", "Y"), c("C", "X"), c("C", "Y"),
  c("X", "M"), c("M", "Y"), c("X", "Y"), c("Y", "P")), directed = TRUE)
figures[["node groups"]] <- metro_dag(groups, exposure = "X", outcome = "Y",
  unobserved = c("Us", "Uo"), node_groups = list(Selection = "Us", Other = "Uo"))
if (requireNamespace("dagitty", quietly = TRUE))
  for (name in c("confounding", "Schipf", "Shrier", "Thoemmes", "Kampen"))
    figures[[paste("dagitty", name)]] <- metro_dag(dagitty::getExample(name), bidirected = "latent")

rows <- list()
for (name in names(figures)) for (width in c(180, 240)) {
  report <- diagnose_metro_dag(figures[[name]], width = width, device = "svg")
  score <- .metro_hook_score(report$routes)
  rows[[length(rows) + 1L]] <- data.frame(label = label, figure = name, width = width,
    arrows = nrow(score), hooks = sum(score$hook), ok = report$ok,
    worst = if (nrow(score)) round(max(pmin(score$positive, score$negative)), 1) else 0)
}
result <- do.call(rbind, rows)
print(result, row.names = FALSE)
cat(sprintf("\nTotal hooks (%s): %d of %d arrows\n", label, sum(result$hooks), sum(result$arrows)))
path <- file.path("inst", "benchmarks", "arrow_hooks.csv")
if (file.exists(path)) {
  previous <- utils::read.csv(path, stringsAsFactors = FALSE)
  result <- rbind(previous[previous$label != label, , drop = FALSE], result)
}
utils::write.csv(result, path, row.names = FALSE)
