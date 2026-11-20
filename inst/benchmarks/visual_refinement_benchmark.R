# Rscript inst/benchmarks/visual_refinement_benchmark.R /path/to/baseline-checkout
# Run from the package root after installing the current package. The recorded
# baseline is main 0ecfa6093131f9cf8ecd03397754c4d6289c8ae6 (merged PR #40).
library(metrodag)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(file.path(args[1L], "R")))
  stop("Supply one baseline checkout directory containing R/.")
baseline <- new.env(parent = asNamespace("metrodag"))
for (file in list.files(file.path(args[1L], "R"), pattern = "[.]R$", full.names = TRUE))
  sys.source(file, baseline)

kinds <- c("chain", "fork", "collider", "diamond", "long_edge", "causal", "dense")
graphs <- stats::setNames(lapply(kinds, metrodag_examples), kinds)
for (n in c(20L, 50L, 100L)) graphs[[paste0("random_", n)]] <-
  metrodag_examples("random", n = n, probability = 2 / n, seed = 2026)
for (seed in c(1L, 11L, 42L)) graphs[[paste0("random10_", seed)]] <-
  metrodag_examples("random", n = 10L, probability = 0.2, seed = seed)
for (seed in c(58L, 77L)) graphs[[paste0("attachment12_", seed)]] <-
  metrodag_examples("random", n = 12L, probability = 0.3, seed = seed)
graph <- igraph::disjoint_union(igraph::delete_vertex_attr(graphs$causal, "name"),
  igraph::delete_vertex_attr(graphs$diamond, "name"))
igraph::V(graph)$name <- paste0("node", seq_len(igraph::vcount(graph)))
graphs$disconnected <- graph

# Read the gallery's definitions without rendering or duplicating its inputs.
definitions <- new.env()
for (expr in parse("data-raw/annotated_gallery.R")) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      as.character(expr[[2L]]) %in% c("edge_list", "examples")) eval(expr, definitions)
}
constraints <- list()
for (example in definitions$examples) {
  graphs[[example$id]] <- igraph::graph_from_edgelist(example$edges)
  constraints[[example$id]] <- example[intersect(names(example), c("rank", "align"))]
}

fields <- c("crossings", "total_bends", "total_edge_length", "area",
  "overlap_segments", "shared_track_length", "node_edge_collisions")
rows <- list()
for (name in names(graphs)) {
  params <- c(list(graph = graphs[[name]]), constraints[[name]])
  t0 <- system.time(before <- do.call(baseline$layout_metro_dag, params))[["elapsed"]]
  t1 <- system.time(after <- do.call(layout_metro_dag, params))[["elapsed"]]
  a <- layout_metrics(before); b <- layout_metrics(after)
  first <- after$edges[!duplicated(after$edges$edge_id), ]
  ends <- igraph::as_edgelist(graphs[[name]])
  stopifnot(identical(before$graph, after$graph),
    identical(before$nodes$node, after$nodes$node), b$directional_consistency == 1,
    setequal(first$edge_id, seq_len(nrow(ends))),
    identical(first$from, ends[first$edge_id, 1L]),
    identical(first$to, ends[first$edge_id, 2L]),
    b$node_edge_collisions <= a$node_edge_collisions)
  # Final half-grid routing may change bend positions under explicit ranks;
  # stations, wave ties and alignment constraints must remain identical.
  if (length(constraints[[name]])) stopifnot(identical(before$nodes, after$nodes),
    identical(before$parameters, after$parameters))
  row <- data.frame(graph = name, seconds_before = t0, seconds_after = t1)
  for (field in fields) {
    row[[paste0(field, "_before")]] <- a[[field]]
    row[[paste0(field, "_after")]] <- b[[field]]
  }
  rows[[name]] <- row
  print(row, row.names = FALSE)
}
write.csv(do.call(rbind, rows), "inst/benchmarks/visual_refinement_metrics.csv", row.names = FALSE)
