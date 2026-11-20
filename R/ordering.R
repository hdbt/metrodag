# Ordering runs on the proper graph: every edge joins adjacent ranks.
.crossings_for_orders <- function(edges, layer, order) {
  total <- 0L
  for (l in sort(unique(layer))) {
    active <- which(layer[edges[, 1L]] == l)
    if (length(active) < 2L) next
    e <- edges[active, , drop = FALSE]
    a <- order[e[, 1L]]; b <- order[e[, 2L]]
    for (i in seq_len(nrow(e) - 1L)) {
      j <- seq.int(i + 1L, nrow(e))
      total <- total + sum((a[i] - a[j]) * (b[i] - b[j]) < 0)
    }
  }
  as.integer(total)
}

.order_layers <- function(graph, layer, method = "barycenter", sweeps = 8L, reverse = FALSE) {
  method <- .assert_scalar_choice(method, c("barycenter", "median", "none"), "ordering")
  n <- igraph::vcount(graph)
  ord <- integer(n)
  names <- igraph::V(graph)$name
  sources <- igraph::vertex_attr(graph, ".metrodag_order_source")
  targets <- igraph::vertex_attr(graph, ".metrodag_order_target")
  if (is.null(sources)) sources <- rep("", n)
  if (is.null(targets)) targets <- rep("", n)
  levels <- sort(unique(layer))
  for (l in levels) {
    ids <- which(layer == l)
    if (method != "none") ids <- ids[order(names[ids], sources[ids], targets[ids], method = "radix")]
    ord[ids] <- seq_along(ids)
    if (reverse && method != "none") ord[ids] <- rev(ord[ids])
  }
  if (method == "none" || !sweeps) return(ord)
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  incoming <- lapply(seq_len(n), function(v) as.integer(igraph::neighbors(graph, v, "in")))
  outgoing <- lapply(seq_len(n), function(v) as.integer(igraph::neighbors(graph, v, "out")))
  aggregate_position <- if (method == "median") stats::median else mean
  best <- ord
  best_score <- .crossings_for_orders(ends, layer, ord)
  for (s in seq_len(sweeps)) {
    nb <- if (s %% 2L) incoming else outgoing
    for (l in if (s %% 2L) levels else rev(levels)) {
      ids <- which(layer == l)
      score <- vapply(ids, function(v) {
        if (length(nb[[v]])) aggregate_position(ord[nb[[v]]]) else ord[v]
      }, numeric(1))
      ids <- ids[order(score, ord[ids], igraph::V(graph)$name[ids], method = "radix")]
      ord[ids] <- seq_along(ids)
    }
    score <- .crossings_for_orders(ends, layer, ord)
    if (score < best_score) { best <- ord; best_score <- score }
  }
  # Improve ties left by barycentres, without accepting a worse sweep.
  ord <- best
  for (pass in seq_len(2L)) {
    changed <- FALSE
    for (l in levels) {
      ids <- which(layer == l); ids <- ids[order(ord[ids], method = "radix")]
      if (length(ids) < 2L) next
      for (j in seq_len(length(ids) - 1L)) {
        u <- ids[j]; v <- ids[j + 1L]
        delta <- 0
        for (nb in list(incoming, outgoing)) {
          if (!length(nb[[u]]) || !length(nb[[v]])) next
          d <- outer(ord[nb[[u]]], ord[nb[[v]]], "-")
          delta <- delta + sum(d < 0) - sum(d > 0)
        }
        if (delta < 0) {
          ord[c(u, v)] <- ord[c(v, u)]
          ids[c(j, j + 1L)] <- c(v, u)
          changed <- TRUE
        }
      }
    }
    if (!changed) break
  }

  # sifting step: for ranks with >= 3 nodes, try inserting each node
  # at every position in its rank and keep the best crossing count.
  # Bounded to 8 sifting operations per rank to limit worst-case cost.
  MAX_SIFT_PER_RANK <- .metro_control()$sift_per_rank
  # Moving a node within rank l changes only crossings among the edges that
  # meet rank l, so sifting compares counts over those edges alone.
  near <- function(l) ends[layer[ends[, 1L]] == l | layer[ends[, 2L]] == l, , drop = FALSE]
  for (pass in seq_len(2L)) {
    changed <- FALSE
    for (l in levels) {
      ids <- which(layer == l)
      if (length(ids) < 3L) next
      local <- near(l)
  # sift in the current rank order, not vertex (input) order, and
      # insert v at `pos` so reordered equivalent inputs give the same layout.
      ids <- ids[order(ord[ids], method = "radix")]
      slots <- sort(ord[ids])
      sift_ops <- 0L
      for (k in seq_along(ids)) {
        if (sift_ops >= MAX_SIFT_PER_RANK) break
        v <- ids[k]
        current_score <- .crossings_for_orders(local, layer, ord)
        best_pos <- k; best_pos_score <- current_score
        for (pos in setdiff(seq_along(ids), k)) {
          trial_ord <- ord
          # Move node v to position pos; shift others accordingly.
          trial_ord[append(ids[-k], v, after = pos - 1L)] <- slots
          s <- .crossings_for_orders(local, layer, trial_ord)
          if (s < best_pos_score) { best_pos_score <- s; best_pos <- pos }
        }
        if (best_pos != k) {
          ids <- append(ids[-k], v, after = best_pos - 1L)
          ord[ids] <- slots
          changed <- TRUE
          sift_ops <- sift_ops + 1L
        }
      }
    }
    if (!changed) break
  }
  ord
}
