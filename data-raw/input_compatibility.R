# Equivalent causal models from plain tables, adjacency, ggdag and tidygraph.
# Run from the package root after R CMD INSTALL .
# Roles illustrate an analysis view; the unmeasured common cause is retained.
library(metrodag)
if (!requireNamespace("ggdag", quietly = TRUE) || !requireNamespace("tidygraph", quietly = TRUE))
  stop("Install the optional ggdag and tidygraph packages to run all comparisons.")

nodes <- data.frame(name = c("C", "X", "Y", "U", "I"),
  label = c("Context", "Exposure", "Outcome", "Unmeasured factor", "Additional variable"),
  role = c("adjusted", "exposure", "outcome", "unobserved", "observed"))
edges <- data.frame(from = c("C", "C", "X", "U", "U"), to = c("X", "Y", "Y", "X", "Y"))
tables <- metro_graph(nodes, edges)
adjacency <- matrix(0L, 5, 5, dimnames = list(nodes$name, nodes$name))
adjacency[cbind(match(edges$from, nodes$name), match(edges$to, nodes$name))] <- 1L
adjacent <- metro_graph_from_adjacency(adjacency[5:1, c(2, 4, 1, 5, 3)])
dag <- dagitty::dagitty("dag { C [adjusted]; X [exposure]; Y [outcome]; U [latent]; I; C -> X; C -> Y; X -> Y; U -> X; U -> Y }")
dag <- ggdag::`label<-`(dag, stats::setNames(nodes$label, nodes$name))
inputs <- list(tables = tables, adjacency = adjacent,
  ggdag = ggdag::tidy_dagitty(dag, seed = 1), tidygraph = tidygraph::as_tbl_graph(tables))
plots <- lapply(names(inputs), function(name) {
  if (name == "adjacency") metro_dag(inputs[[name]], exposure = "X", outcome = "Y",
    adjusted = "C", unobserved = "U", labels = stats::setNames(nodes$label, nodes$name)) else
    metro_dag(inputs[[name]])
})
names(plots) <- names(inputs)
output <- file.path("inst", "benchmarks", "figures")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
checks <- list(); reference <- list()
svg_signature <- function(file) {
  lines <- readLines(file)
  # Disjoint stations and labels may be serialized in vertex-table order.
  # Keep route/casing/arrow and group order exact; compare every station/label.
  nodes <- grepl("^<(circle|rect|text)\\b", trimws(lines), perl = TRUE)
  list(ordered = lines[!nodes], nodes = sort(lines[nodes], method = "radix"))
}
for (name in names(plots)) for (device in c("png", "svg")) {
  p <- plots[[name]]
  report <- diagnose_metro_dag(p, width = 180, device = device, dpi = 300)
  stopifnot(report$ok, nrow(report$issues) == 0L)
  file <- if (name == "tables") file.path(output, paste0("input_compatibility.", device)) else
    tempfile(fileext = paste0(".", device))
  save_metro_dag(file, p, width = 180, dpi = 300)
  drawing <- if (device == "png") readBin(file, "raw", n = file.info(file)$size) else svg_signature(file)
  if (name == "tables") reference[[device]] <- drawing
  stopifnot(identical(drawing, reference[[device]]))
  heads <- if (device == "svg") sum(grepl("<polygon", readLines(file), fixed = TRUE)) else NA_integer_
  if (device == "svg") stopifnot(heads == nrow(edges))
  layout <- attr(p, "metrodag_layout")
  stopifnot(setequal(igraph::V(layout$graph)$name, nodes$name), igraph::degree(layout$graph, "I") == 0)
  checks[[length(checks) + 1L]] <- data.frame(input = name, device = device,
    nodes = nrow(layout$nodes), edges = igraph::ecount(layout$graph), isolated_nodes = 1L,
    same_render = TRUE, comparison = if (device == "png") "byte-identical" else "SVG primitives; disjoint node/text order ignored",
    svg_arrowheads = heads, errors = 0L, warnings = 0L)
  if (name != "tables") unlink(file)
}
write.csv(do.call(rbind, checks), "inst/benchmarks/input_compatibility_checks.csv", row.names = FALSE)
