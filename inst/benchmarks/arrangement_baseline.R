# Rscript inst/benchmarks/arrangement_baseline.R /path/to/baseline-checkout
# Reproduce the two before figures at main c48ae8b16f1f5d4379e20b028bd87117b87d79b1.
# The after figures come from data-raw/annotated_gallery.R.
library(metrodag)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(file.path(args[1L], "R")))
  stop("Supply one baseline checkout directory containing R/.")
baseline <- new.env(parent = asNamespace("metrodag"))
for (file in list.files(file.path(args[1L], "R"), pattern = "[.]R$", full.names = TRUE))
  sys.source(file, baseline)

# Use that version's grid renderer as well as its layout and plot functions.
with_baseline_renderer <- function(code) {
  previous <- getS3method("makeContent", "metrodag_grob", envir = asNamespace("grid"))
  on.exit(registerS3method("makeContent", "metrodag_grob", previous,
    envir = asNamespace("grid")), add = TRUE)
  registerS3method("makeContent", "metrodag_grob", baseline$makeContent.metrodag_grob,
    envir = asNamespace("grid"))
  force(code)
}

examples <- list(
  alcohol_heart = list(
    graph = "I X I Y H X H Y D X D S X M M Y X S S Y X Y",
    labels = c(I = "Household income", H = "Baseline health",
      D = "Smoking at baseline", X = "Usual alcohol use",
      M = "Systolic BP rise", S = "Blood triglycerides", Y = "Coronary events"),
    exposure = "X", outcome = "Y", adjusted = c("I", "H", "D")),
  sugary_drinks = list(
    graph = "W O W Y O Y A O S O C A C S C D D Y U O U Y H O H Y",
    labels = c(W = "Sugary drinks", O = "Body fat mass", Y = "HbA1c level",
      A = "Food environment", S = "Fast-food consumption", C = "Household income",
      D = "Primary care", U = "Metabolic genotype", H = "Shift-work hours"),
    exposure = "W", outcome = "Y", unobserved = "U", unadjusted = TRUE))

with_baseline_renderer({
  for (name in names(examples)) {
    parameters <- examples[[name]]
    parameters$graph <- matrix(strsplit(parameters$graph, " ")[[1L]],
      ncol = 2L, byrow = TRUE)
    plot <- do.call(baseline$metro_dag, parameters)
    for (device in c("svg", "png")) baseline$save_metro_dag(
      file.path("inst", "benchmarks", "figures", paste0("arrangement_", name, "_before.", device)),
      plot, width = 240, dpi = 180)
  }
})
