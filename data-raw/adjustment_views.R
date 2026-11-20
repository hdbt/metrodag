# Run from the repository root after R CMD INSTALL .
library(metrodag)

theory <- igraph::graph_from_edgelist(rbind(
  c("C", "X"), c("C", "Y"), c("X", "M"), c("M", "Y"), c("X", "Y")
), directed = TRUE)
fixed <- layout_metro_dag(theory, direction = "right")
views <- list(
  unadjusted = metro_dag(fixed, exposure = "X", outcome = "Y", unadjusted = TRUE,
    estimand = "Unadjusted X-Y association"),
  total_x = metro_dag(fixed, exposure = "X", outcome = "Y", adjusted = "C",
    estimand = "Total effect of X on Y"),
  total_m = metro_dag(fixed, exposure = "M", outcome = "Y", adjusted = c("C", "X"),
    estimand = "Total effect of M on Y")
)

out <- "inst/benchmarks/figures"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (name in names(views))
  save_metro_dag(file.path(out, paste0("adjustment_", name, ".svg")), views[[name]])

# One shared legend, synchronized headers and equal physical panel scales.
comparison <- compare_metro_dags(fixed, list(
  Unadjusted = list(exposure = "X", outcome = "Y", unadjusted = TRUE,
    estimand = "Unadjusted X-Y association"),
  Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C",
    estimand = "Total effect of X on Y")
))
save_metro_dag(file.path(out, "adjustment_comparison.svg"), comparison, width = 180)

# The README figure: its "One graph, several analyses" example, as printed.
edges <- data.frame(
  from = c("C", "C", "X", "M", "U", "U", "S", "S"),
  to   = c("X", "Y", "M", "Y", "C", "X", "Y", "X")
)
p <- metro_dag(
  edges,
  exposure = "X", outcome = "Y", unobserved = "U",
  labels = c(
    C = "Socioeconomic background", X = "Cigarette consumption",
    M = "Tar deposit in lung", Y = "Lung cancer risk",
    U = "Unmeasured inheritance", S = "Occupational hazards"
  )
)
readme <- compare_metro_dags(attr(p, "metrodag_layout"), list(
  Unadjusted = list(exposure = "X", outcome = "Y", unadjusted = TRUE),
  Adjusted = list(exposure = "X", outcome = "Y", adjusted = "C")
))
save_metro_dag(file.path(out, "adjustment_views.png"), readme, width = 190, dpi = 300)
