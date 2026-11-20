# Joint moves can escape a routing dead end: either arrow on its own would
# occupy the other's track, although moving both removes their crossing.
# All work is in the internal downward, unit-grid coordinate system.
.pair_route_candidates <- function(e, nodes, other, max_candidates, fixed = FALSE) {
  obstacles <- .prepare_route_obstacles(other)
  # Count occupied tracks with multiplicity: sharing a step with two arrows
  # contributes twice as much as sharing it with one.
  keys <- .route_grid_keys(other)
  unique_keys <- unique(keys)
  counts <- tabulate(match(keys, unique_keys))
  length_of <- function(z) sqrt((z$xend - z$x)^2 + (z$yend - z$y)^2)
  quality <- function(z, conflicts) {
    hits <- match(.route_grid_keys(z), unique_keys)
    shared <- sum(length_of(z)[!is.na(hits)] * counts[hits[!is.na(hits)]])
    c(crossings = unname(conflicts["crossings"]), shared = shared,
      bends = sum(diff(z$xend - z$x) != 0), length = sum(length_of(z)))
  }
  limit <- quality(e, .route_conflicts(e, obstacles))
  unrelated <- .unrelated_stations(nodes, e)
  candidates <- c(list(e), if (!fixed) .simple_route_candidates(e))
  stack <- .stack_routes(candidates)
  # A candidate repeating an earlier one's moves is skipped, as a string key
  # of its moves would find. The moves of unit-step routes are -1, 0 or 1, so
  # one byte per move is an exact key; other values use the string itself.
  if (all(stack$dx %in% c(-1, 0, 1))) {
    text <- rawToChar(as.raw(stack$dx + 2))
    seen <- substring(text, stack$offset + 1L, stack$offset + stack$size)
  } else seen <- vapply(candidates, function(z) paste(z$xend - z$x, collapse = ","), character(1))
  ids <- which(!duplicated(seen))
  ids <- ids[.stacked_clear(stack, ids, unrelated)]
  if (!length(ids)) return(list())
  conflicts <- .stacked_conflicts(stack, ids, obstacles)
  # quality() of every remaining candidate from the stack: the same step
  # values summed per candidate in step order.
  position <- match(stack$owner, ids)
  steps <- which(!is.na(position))
  owner <- position[steps]
  x <- stack$x[steps]; xend <- stack$xend[steps]
  step_length <- sqrt((xend - x)^2 + (stack$yend[steps] - stack$y[steps])^2)
  hits <- match(.grid_keys(x, stack$y[steps], xend), unique_keys)
  on_track <- !is.na(hits)
  k <- length(ids)
  crossings <- conflicts$crossings
  shared <- .candidate_sums(step_length[on_track] * counts[hits[on_track]], owner[on_track], k)
  bends <- stack$bends[ids]
  total <- .candidate_sums(step_length, owner, k)
  accepted <- which(!(crossings > limit[["crossings"]] + 1e-8 | shared > limit[["shared"]] + 1e-8 |
    total > limit[["length"]] + 1e-8 | bends > limit[["bends"]] + 1L))
  if (!length(accepted)) return(list())
  # Lengths are floating sums: rank them with the tolerance of the limits
  # above, so equal lengths tie on every platform and candidate order decides.
  priority <- order(crossings[accepted], .tolerance_rank(shared[accepted]), bends[accepted],
    .tolerance_rank(total[accepted]), seq_along(accepted), method = "radix")
  # Only the retained candidates become routes with their own obstacle tables.
  lapply(accepted[head(priority, max_candidates)], function(j) {
    z <- candidates[[ids[j]]]
    for (field in c("from", "to", "edge_id", "path_id")) z[[field]] <- e[[field]][1L]
    z$segment <- seq_len(nrow(z))
    z$bend <- c(FALSE, diff(z$xend - z$x) != 0)
    z <- z[, names(e)]
    list(edge = z, score = c(crossings = crossings[j], shared = shared[j], bends = bends[j],
      length = total[j]), obstacles = .prepare_route_obstacles(z))
  })
}

.reroute_crossing_pairs <- function(routes, nodes, max_pairs = 24L,
  max_candidates = 24L, fixed = integer()) {
  if (length(routes) < 2L) return(routes)
  route_order <- order(.route_from(routes),
    .route_to(routes), method = "radix")
  routes <- routes[route_order]
  fixed <- match(fixed, route_order)
  prepared <- lapply(routes, .prepare_route_obstacles)

  # pre-compute all crossing pairs and sort by descending crossing count
  # so the budget is spent on the most-impactful pairs first.
  n <- length(routes)
  cross_pairs <- list()
  # Skip pairs whose y-ranges are disjoint (cannot cross). Count the others'
  # crossings with route j for all earlier routes i at once.
  top <- vapply(routes, function(e) e$y[1L], numeric(1))
  bottom <- vapply(routes, function(e) e$yend[nrow(e)], numeric(1))
  stack <- .stack_routes(routes)
  crossing <- matrix(0, n, n)
  for (j in seq_len(n)[-1L]) {
    earlier <- which(pmin(top[seq_len(j - 1L)], top[j]) > pmax(bottom[seq_len(j - 1L)], bottom[j]))
    if (length(earlier)) crossing[earlier, j] <- .stacked_conflicts(stack, earlier, prepared[[j]])$crossings
  }
  for (i in seq_len(n - 1L)) {
    for (j in seq.int(i + 1L, n)) {
      pair_cross <- crossing[i, j]
      if (!pair_cross) next
      cross_pairs[[length(cross_pairs) + 1L]] <- list(i = i, j = j, n = pair_cross)
    }
  }
  if (!length(cross_pairs)) return(routes[order(route_order, method = "radix")])
  # Sort descending by crossing count so highest-impact pairs are tried first.
  cross_order <- order(-vapply(cross_pairs, `[[`, numeric(1), "n"), method = "radix")
  cross_pairs <- cross_pairs[cross_order]
  attempts <- 0L
  limit <- NULL
  for (pass in seq_len(2L)) {
    changed <- FALSE
    for (cp in cross_pairs) {
      if (attempts >= max_pairs) return(routes[order(route_order, method = "radix")])
      i <- cp$i; j <- cp$j
      a <- routes[[i]]; b <- routes[[j]]
      # Re-check crossings since earlier moves in this pass may have resolved them.
      pair_limit <- .route_conflicts(a, prepared[[j]])
      if (!pair_limit["crossings"]) next
      attempts <- attempts + 1L
      other <- .bind_route_columns(routes[-c(i, j)])
      if (is.null(other)) other <- .empty_routes()
      aa <- .pair_route_candidates(a, nodes, other, max_candidates, i %in% fixed)
      bb <- .pair_route_candidates(b, nodes, other, max_candidates, j %in% fixed)
      if (!length(aa) || !length(bb)) next
      bend_limit <- sum(a$bend) + sum(b$bend) + 1L
      best <- NULL
      best_score <- rep(Inf, 4L)
      # Conflicts of every first-arrow candidate with each second-arrow
      # candidate: column of `overlap`/`crossings` per second candidate.
      stack <- .stack_routes(lapply(aa, `[[`, "edge"))
      pairs <- lapply(bb, function(y) .stacked_conflicts(stack, seq_along(aa), y$obstacles))
      for (ix in seq_along(aa)) for (iy in seq_along(bb)) {
        x <- aa[[ix]]; y <- bb[[iy]]
        score <- x$score + y$score
        if (score["bends"] > bend_limit) next
        pair <- c(overlap = pairs[[iy]]$overlap[ix], crossings = pairs[[iy]]$crossings[ix])
        if (pair["overlap"] > pair_limit["overlap"] + 1e-8 ||
            pair["crossings"] >= pair_limit["crossings"]) next
        score["crossings"] <- score["crossings"] + pair["crossings"]
        score["shared"] <- score["shared"] + pair["overlap"]
        if (!.lexically_better(score, best_score)) next
        best <- list(x$edge, y$edge)
        best_score <- score
      }
      if (is.null(best)) next
      trial <- routes
      trial[c(i, j)] <- best
      # Verify the whole drawing with the public crossing definition, and
      # preserve pairwise shared-track length rather than just its union.
      if (is.null(limit)) limit <- .routed_geometry_quality(routes)
      quality <- .routed_geometry_quality(trial)
      if (quality["crossings"] >= limit["crossings"] ||
          any(quality[c("shared", "length")] > limit[c("shared", "length")] + 1e-8) ||
          quality["bends"] > limit["bends"] + 1L) next
  # keep the verified pair as is. Straightening it against only
      # itself ignored every other arrow, could undo the removed crossing and
      # bypassed this quality check (even a whole-drawing guard added bends).
      routes <- trial
      limit <- quality
      prepared[c(i, j)] <- lapply(best, .prepare_route_obstacles)
      changed <- TRUE
    }
    if (!changed) break
  }
  routes[order(route_order, method = "radix")]
}
