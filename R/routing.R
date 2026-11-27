.path_cover <- function(graph, layer, order) {
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  names <- igraph::V(graph)$name
  used_in <- used_out <- rep(FALSE, igraph::vcount(graph))
  path <- seq_len(igraph::vcount(graph))
  if (nrow(ends)) {
    priority <- order(abs(order[ends[, 1L]] - order[ends[, 2L]]),
      -(layer[ends[, 2L]] - layer[ends[, 1L]]), names[ends[, 1L]], names[ends[, 2L]], method = "radix")
    for (i in priority) {
      u <- ends[i, 1L]; v <- ends[i, 2L]
      if (!used_out[u] && !used_in[v]) {
        used_out[u] <- TRUE; used_in[v] <- TRUE
        old <- path[v]; path[path == old] <- path[u]
      }
    }
  }
  # Number paths by their source names in C-locale (radix) order, independently
  # of input vertex order and of the session's collation locale.
  sources <- names[path]
  match(sources, sort(unique(sources), method = "radix"))
}

