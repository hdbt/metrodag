# Explicit semantic groups share a transverse coordinate. Never infer repeated
# measures from names: identifiers need not encode variable identity or time.
.alignment_groups <- function(align, nodes, layer) {
  if (is.null(align)) return(list())
  if (!is.list(align) || any(!vapply(align, function(z)
      is.character(z) && length(z) >= 2L && !anyNA(z) &&
        all(z %in% nodes), logical(1))) ||
      anyDuplicated(unlist(align, use.names = FALSE)))
    stop("`align` must be a list of disjoint character vectors, each naming at least two existing nodes.", call. = FALSE)
  for (group in align) if (anyDuplicated(layer[match(group, nodes)]))
    stop("Nodes in an `align` group must occupy different ranks; supply `rank` to distinguish them.", call. = FALSE)
  align
}

# Treat each alignment group as a single coordinate variable. Same-rank
# inequalities keep stations/waypoints four units apart. Use one global order
# of coordinate variables so shared groups cannot introduce an order cycle.
.aligned_positions <- function(x, layer, keys, align, spacing = 4) {
  group <- seq_along(x)
  for (j in seq_along(align)) group[match(align[[j]], keys)] <- length(x) + j
  group <- match(group, unique(group))
  count <- max(group)
  target <- vapply(seq_len(count), function(g) round(stats::median(x[group == g])), numeric(1))
  # The group's first name in the C-locale (radix) order used below; min()
  # would follow the session's collation locale.
  key <- vapply(seq_len(count), function(g) sort(keys[group == g], method = "radix")[1L],
    character(1))
  constraints <- list()
  for (rank in sort(unique(layer))) {
    ids <- which(layer == rank)
    ids <- ids[order(target[group[ids]], key[group[ids]], x[ids], method = "radix")]
    if (length(ids) > 1L) constraints[[length(constraints) + 1L]] <-
      cbind(head(group[ids], -1L), tail(group[ids], -1L))
  }
  if (length(constraints)) {
    constraints <- unique(do.call(rbind, constraints))
    for (pass in seq_len(count)) {
      previous <- target
      for (j in seq_len(nrow(constraints))) {
        a <- constraints[j, 1L]; b <- constraints[j, 2L]
        target[b] <- max(target[b], target[a] + spacing)
      }
      if (identical(previous, target)) break
    }
  }
  result <- target[group]
  result - round(mean(range(result)))
}
