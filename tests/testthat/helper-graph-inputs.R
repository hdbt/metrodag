.compat_tables <- function() list(
  nodes = data.frame(name = c("C", "X", "Y", "U", "I"),
    label = c("Context", "Exposure", "Outcome", "Unmeasured\nfactor", "Isolated variable"),
    role = c("adjusted", "exposure", "outcome", "unobserved", "observed"),
    cohort = factor(c("a", "a", "a", "b", "b"))),
  edges = data.frame(from = c("C", "C", "X", "U", "U"),
    to = c("X", "Y", "Y", "X", "Y"), weight = c(1, 2, 3, 4, 5)))

.compat_reference <- function() {
  x <- .compat_tables()
  igraph::graph_from_data_frame(x$edges, vertices = x$nodes, directed = TRUE)
}

.compat_display <- function(p) {
  n <- p$layers[[2L]]$geom_params$nodes
  n <- n[order(n$node, method = "radix"), c("node", "x", "y", "label", "role", "shape", "fill", "colour")]
  rownames(n) <- NULL
  n
}

.compat_routes <- function(layout) {
  e <- layout$edges
  e <- e[order(e$from, e$to, e$segment, method = "radix"), setdiff(names(e), "edge_id")]
  rownames(e) <- NULL
  e
}
