# Run from the package root after R CMD INSTALL .
# Annotated visual regressions for the final route-section cleanup.
# Optionally supply a baseline checkout to regenerate the before figures too.
library(metrodag)
args <- commandArgs(trailingOnly = TRUE)
baseline <- NULL
if (length(args)) {
  if (length(args) != 1L || !dir.exists(file.path(args[1L], "R")))
    stop("Supply at most one baseline checkout directory containing R/.")
  baseline <- new.env(parent = asNamespace("metrodag"))
  for (file in list.files(file.path(args[1L], "R"), pattern = "[.]R$", full.names = TRUE))
    sys.source(file, baseline)
}

edge_list <- function(text) {
  matrix(strsplit(text, " ")[[1L]], ncol = 2L, byrow = TRUE)
}
examples <- list(
  alcohol_heart = list(
    graph = edge_list("I X I Y H X H Y D X D S X M M Y X S S Y X Y"),
    labels = c(I = "Household income", H = "Baseline health",
      D = "Smoking at baseline", X = "Usual alcohol use",
      M = "Systolic BP rise", S = "Blood triglycerides", Y = "Coronary events"),
    exposure = "X", outcome = "Y", adjusted = c("I", "H", "D")),
  statin_events = list(
    graph = edge_list("X P P A A O O Y I X I R I Y R A T A S O U X U O G P G O G Y"),
    labels = c(X = "Statin therapy given", P = "Pills collected",
      A = "Serum LDL cholesterol", O = "Arterial plaques", Y = "Vascular events",
      I = "Household income", R = "Comorbidity profile", T = "Dietary fat level",
      S = "Current smoking", U = "Health consciousness", G = "Age at baseline visit"),
    exposure = "X", outcome = "Y", adjusted = c("I", "R", "G"), unobserved = "U")
)

dir.create("inst/benchmarks/figures", recursive = TRUE, showWarnings = FALSE)
for (name in names(examples)) for (version in c(if (!is.null(baseline)) "before", "after")) {
  parameters <- examples[[name]]
  if (version == "before") parameters$graph <- baseline$layout_metro_dag(
    igraph::graph_from_edgelist(parameters$graph), direction = "right")
  plot <- do.call(metro_dag, parameters)
  print(layout_metrics(attr(plot, "metrodag_layout")))
  # The larger review export fits long labels; no manual label or route nudges.
  for (device in c("pdf", "svg", "png")) {
    report <- diagnose_metro_dag(plot, width = 240, device = device)
    if (!report$ok || nrow(report$issues))
      stop("Inspect ", name, " ", version, " on ", device, " with diagnose_metro_dag().")
  }
  suffix <- if (version == "before") "_before" else ""
  for (extension in c("svg", "png")) {
    save_metro_dag(file.path("inst/benchmarks/figures",
      paste0("cleanup_", name, suffix, ".", extension)), plot, width = 240, dpi = 180)
  }
}
