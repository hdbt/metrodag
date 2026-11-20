# Rscript inst/benchmarks/arrangement_benchmark.R /path/to/baseline-checkout
# Run from the package root after installing the current package. The baseline
# for the checked-in CSV is main commit c48ae8b16f1f5d4379e20b028bd87117b87d79b1.
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
annotated <- c(
  alcohol_heart = "I X I Y H X H Y D X D S X M M Y X S S Y X Y",
  air_pollution = "I N I Y R N R X N X B N B Y X L L P X P P Y X T T Y",
  sugary_drinks = "W O W Y O Y A O S O C A C S C D D Y U O U Y H O H Y",
  statin_events = "X P P A A O O Y I X I R I Y R A T A S O U X U O G P G O G Y",
  obesity_admission = "E X E Y E I X I I Y X M M Y X Y X P Y P U X U Y",
  longitudinal = "S X1 S Y1 X1 X2 X2 X3 Y1 Y2 Y2 Y3 X1 Y2 Y1 X2 X2 Y3 Y2 X3 X1 M2 M2 Y3 C X2 C Y2")
for (name in names(annotated)) graphs[[name]] <- igraph::graph_from_edgelist(
  matrix(strsplit(annotated[[name]], " ")[[1L]], ncol = 2L, byrow = TRUE))
ranks <- list(longitudinal = c(S = 0, C = 0, X1 = 1, Y1 = 1,
  X2 = 2, Y2 = 2, M2 = 2, X3 = 3, Y3 = 3))
quality <- function(layout) metrodag:::.routed_geometry_quality(
  split(layout$edges, layout$edges$edge_id))
extent <- function(layout) c(
  width = diff(range(c(layout$nodes$x, layout$edges$x, layout$edges$xend))),
  depth = diff(range(c(layout$nodes$y, layout$edges$y, layout$edges$yend))))
rows <- lapply(names(graphs), function(name) {
  t0 <- system.time(before <- baseline$layout_metro_dag(direction = "down", graphs[[name]], rank = ranks[[name]]))["elapsed"]
  t1 <- system.time(after <- layout_metro_dag(direction = "down", graphs[[name]], rank = ranks[[name]]))["elapsed"]
  a <- quality(before); b <- quality(after)
  size_before <- extent(before); size_after <- extent(after)
  collisions <- layout_metrics(after)$node_edge_collisions
  stopifnot(identical(before$nodes[c("node", "layer", "order", "path")],
      after$nodes[c("node", "layer", "order", "path")]), all(b <= a + 1e-8),
    collisions <= layout_metrics(before)$node_edge_collisions,
    all(size_after <= size_before + 1e-8))
  row <- data.frame(graph = name, seconds_before = unname(t0), seconds_after = unname(t1),
    width_before = size_before["width"], width_after = size_after["width"],
    depth_before = size_before["depth"], depth_after = size_after["depth"],
    area_before = prod(size_before), area_after = prod(size_after),
    bends_before = a["bends"], bends_after = b["bends"],
    crossings_before = a["crossings"], crossings_after = b["crossings"],
    shared_before = a["shared"], shared_after = b["shared"],
    length_before = a["length"], length_after = b["length"], collisions_after = collisions)
  print(row, row.names = FALSE)
  row
})
write.csv(do.call(rbind, rows), "inst/benchmarks/arrangement_metrics.csv", row.names = FALSE)
