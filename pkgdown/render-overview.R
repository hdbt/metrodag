# Run from the repository root after installing the current package.
# This is the labelled graph from the getting-started vignette.
library(metrodag)
edges <- data.frame(
  from = c("C", "C", "X", "X", "M"),
  to = c("X", "Y", "M", "Y", "Y")
)
p <- metro_dag(edges,
  labels = c(C = "Socioeconomic background", X = "Cigarettes per day",
    M = "Tar deposits", Y = "Lung cancer"),
  exposure = "X", outcome = "Y")
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
save_metro_dag("documentation-example.svg", p, path = "man/figures", width = 180)
