# Rscript inst/benchmarks/rank_refinement_baseline.R /path/to/baseline-checkout
# Reproduce DAG 4 before automatic-rank refinement, from merged PR #33.
# Both versions use the current renderer; the after figure is in the gallery.
library(metrodag)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(file.path(args[1L], "R")))
  stop("Supply one baseline checkout directory containing R/.")
baseline <- new.env(parent = asNamespace("metrodag"))
for (file in list.files(file.path(args[1L], "R"), pattern = "[.]R$", full.names = TRUE))
  sys.source(file, baseline)

graph <- igraph::graph_from_edgelist(matrix(strsplit(
  "X P P A A O O Y I X I R I Y R A T A S O U X U O G P G O G Y", " ")[[1L]],
  ncol = 2L, byrow = TRUE))
plot <- metro_dag(baseline$layout_metro_dag(graph, direction = "right"),
  labels = c(X = "Statin therapy given", P = "Pills collected", A = "Serum LDL cholesterol",
    O = "Arterial plaques", Y = "Vascular events", I = "Household income",
    R = "Comorbidity profile", T = "Dietary fat level", S = "Current smoking",
    U = "Health consciousness", G = "Age at baseline visit"),
  exposure = "X", outcome = "Y", adjusted = c("I", "R", "G"), unobserved = "U")
for (device in c("svg", "png")) save_metro_dag(
  file.path("inst", "benchmarks", "figures", paste0("rank_statin_events_before.", device)),
  plot, width = 240, dpi = 180)
