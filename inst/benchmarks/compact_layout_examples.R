# Run from the package root after R CMD INSTALL .
# Annotated visual regressions for rank compaction and longer final approaches.
# Baseline: 70fb028a21b0cc29feb366efaacdbd6987125af6 (merged PR #31).
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

# Grid dispatches makeContent through its method table, even for a grob built
# by a baseline function. Temporarily register that version's renderer too so
# the before figures retain its label-placement behaviour; always restore it.
with_renderer <- function(version, code) {
  if (version == "before") {
    previous <- getS3method("makeContent", "metrodag_grob", envir = asNamespace("grid"))
    on.exit(registerS3method("makeContent", "metrodag_grob", previous,
      envir = asNamespace("grid")), add = TRUE)
    registerS3method("makeContent", "metrodag_grob", baseline$makeContent.metrodag_grob,
      envir = asNamespace("grid"))
  }
  force(code)
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
for (name in names(examples)) for (version in c(if (!is.null(baseline)) "before", "after")) with_renderer(version, {
  parameters <- examples[[name]]
  plot <- do.call(if (version == "before") baseline$metro_dag else metro_dag, parameters)
  print(layout_metrics(attr(plot, "metrodag_layout")))
  # Keep both sides at 240 mm to isolate geometry and default padding changes.
  # Current figures are also checked at the default 180 mm width. No nudges.
  for (width in if (version == "before") 240 else c(180, 240)) for (device in c("pdf", "svg", "png")) {
    report <- diagnose_metro_dag(plot, width = width, device = device, dpi = 180)
    if (!report$ok || nrow(report$issues))
      stop("Inspect ", name, " ", version, " on ", device, " with diagnose_metro_dag().")
  }
  suffix <- if (version == "before") "_before" else ""
  for (extension in c("svg", "png")) {
    save_metro_dag(file.path("inst/benchmarks/figures",
      paste0("compact_", name, suffix, ".", extension)), plot, width = 240, dpi = 180)
  }
})
