library(testthat)
library(metrodag)

if (isTRUE(as.logical(Sys.getenv("NOT_CRAN")))) {
  test_check("metrodag")
} else {
  # Cover the public workflows on CRAN: graph inputs, DAGitty, ggdag and
  # tidygraph objects, examples and their seeded random graphs, diagnostics,
  # analysis views, causal safety, components, styles, compositions, export
  # (paths, units, scale and keeping an existing figure when an export
  # fails) and the half-grid route pass. Their exhaustive loops, and the full
  # routing and rendering regression suite, run with NOT_CRAN=true in GitHub
  # CI.
  cran_tests <- c(
    "adjustment-views", "causal-safety", "comparison-nudges", "components",
    "composition", "conventions", "crossing-input-regressions", "dagitty",
    "diagnostics", "examples", "export-path-scale-regressions",
    "ggdag-compatibility", "graph-inputs", "layout", "node-groups",
    "route-resolution", "style", "tidygraph-compatibility",
    "waypoint-order-regressions", "wrap-generation-regressions"
  )
  test_check("metrodag", filter = paste0("^(", paste(cran_tests, collapse = "|"), ")$"))
}
