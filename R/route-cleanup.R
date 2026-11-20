# Shorten sections of a route when a simple whole-arrow replacement is blocked.
# Endpoint stubs and bend neighbours let a cleaner middle keep its connections.
.section_route_candidates <- function(e, max_windows = 24L, max_candidates = 128L) {
  x <- .subset2(e, "x"); xend <- .subset2(e, "xend")
  n <- length(x)
  moves <- xend - x
  bends <- sum(diff(moves) != 0)
  if (bends < 2L || max_windows <= 0L || max_candidates <= 0L) return(list())
  turns <- which(diff(moves) != 0)
  anchors <- sort(unique(c(0L, 1L, n - 1L, n, turns - 1L, turns, turns + 1L)))
  anchors <- anchors[anchors >= 0L & anchors <= n]
  windows <- expand.grid(start = head(anchors, max_windows),
    end = tail(anchors, max_windows))
  windows <- windows[windows$start < windows$end, , drop = FALSE]
  # Start with long sections that retain a short source or destination stub.
  priority <- order(-(windows$end - windows$start), windows$start, method = "radix")
  windows <- windows[head(priority, max_windows), , drop = FALSE]
  candidates <- list(); scores <- list(); seen <- character()
  ink <- sum(abs(moves))
  # On the integer grid every move is -1, 0 or 1 and sums of moves are exact:
  # score a section's candidates together and key a move vector by one byte
  # per move. Other coordinates take the general path, one candidate at a time.
  grid <- all(moves %in% c(-1, 0, 1)) && all(x == round(x)) && max(abs(x)) < 2^50
  for (j in seq_len(nrow(windows))) {
    ids <- seq.int(windows$start[j] + 1L, windows$end[j])
    if (sum(diff(moves[ids]) != 0) < 1L) next
    if (!grid) {
      for (z in .simple_route_candidates(e[ids, , drop = FALSE])) {
        trial <- moves
        trial[ids] <- z$xend - z$x
        count <- sum(diff(trial) != 0)
        diagonal_steps <- sum(abs(trial))
        if (count >= bends || diagonal_steps > ink) next
        key <- paste(trial, collapse = ",")
        if (key %in% seen) next
        seen <- c(seen, key)
        candidates[[length(candidates) + 1L]] <- trial
        scores[[length(scores) + 1L]] <- c(count, diagonal_steps)
      }
      next
    }
    section <- .simple_route_moves(x[ids], xend[ids])
    k <- length(section); h <- length(ids)
    if (!k) next
    # The moves of each candidate route as .route_from_moves() lays it out
    # from the section's start: differences of x0 + cumsum(moves).
    sums <- matrix(cumsum(unlist(section, use.names = FALSE)), h)
    if (k > 1L) sums <- sums - rep(c(0, sums[h, -k]), each = h)
    positions <- rbind(x[ids[1L]], x[ids[1L]] + sums)
    trial <- matrix(moves, n, k)
    trial[ids, ] <- positions[-1L, , drop = FALSE] - positions[-(h + 1L), , drop = FALSE]
    count <- colSums(trial[-1L, , drop = FALSE] - trial[-n, , drop = FALSE] != 0)
    diagonal_steps <- colSums(abs(trial))
    keep <- which(count < bends & diagonal_steps <= ink)
    if (!length(keep)) next
    key <- substring(rawToChar(as.raw(trial[, keep] + 2)),
      (seq_along(keep) - 1L) * n + 1L, seq_along(keep) * n)
    fresh <- !duplicated(c(seen, key))[length(seen) + seq_along(key)]
    seen <- c(seen, key[fresh])
    for (i in keep[fresh]) {
      candidates[[length(candidates) + 1L]] <- trial[, i]
      scores[[length(scores) + 1L]] <- c(count[[i]], diagonal_steps[[i]])
    }
  }
  if (!length(candidates)) return(list())
  scores <- do.call(rbind, scores)
  priority <- order(scores[, 1L], scores[, 2L], seq_along(candidates), method = "radix")
  lapply(candidates[head(priority, max_candidates)], function(z)
    .route_from_moves(e$x[1L], e$y[1L], z))
}

.simplify_route_sections <- function(routes, nodes, max_routes = 24L,
  max_windows = 24L, max_candidates = 128L) {
  if (!length(routes) || max_routes <= 0L || max_windows <= 0L || max_candidates <= 0L)
    return(routes)
  original <- routes
  prepared <- lapply(routes, .prepare_route_obstacles)
  index <- .obstacle_index(prepared)
  from <- .route_from(routes)
  to <- .route_to(routes)
  attempts <- 0L
  for (pass in seq_len(2L)) {
    changed <- FALSE
    bends <- .route_bends(routes)
    spans <- .route_steps(routes)   # number of unit steps = rank span
  # sort by bends * span descending so long multi-rank bypass arrows
    # with many bends are simplified before shorter arrows of equal bend count.
    # A 3-bend, 12-step arrow spanning 4 ranks is far more visually disruptive
    # than a 3-bend, 4-step short arrow.
    priority <- order(-bends * spans, -spans, from, to, method = "radix")
    for (id in priority[bends[priority] >= 2L]) {
      if (attempts >= max_routes) break
      attempts <- attempts + 1L
      e <- routes[[id]]
      obstacles <- .obstacles_except(index, id)
      # Unlike the union of occupied steps, this counts sharing with each arrow.
      keys <- unlist(lapply(routes[-id], .route_grid_keys), use.names = FALSE)
      unique_keys <- unique(keys)
      counts <- tabulate(match(keys, unique_keys))
      shared_length <- function(z) {
        hits <- match(.route_grid_keys(z), unique_keys)
        lengths <- sqrt((z$xend - z$x)^2 + (z$yend - z$y)^2)
        sum(lengths[!is.na(hits)] * counts[hits[!is.na(hits)]])
      }
      limit <- .route_conflicts(e, obstacles)
      shared <- shared_length(e)
      unrelated <- .unrelated_stations(nodes, e)
      # Retain the existing local whitespace up to one station-spacing unit.
      # Preserve tighter existing clearances rather than forcing all old routes
      # to satisfy a larger margin, but never allow less than the routing floor.
      runs <- .route_runs(e)
      clearance <- .route_clearances(runs, unrelated)
      candidates <- .section_route_candidates(e, max_windows, max_candidates)
      stack <- .stack_routes(candidates)
      # The first acceptable candidate wins: score them in blocks of 16.
      block <- (seq_len(stack$k) - 1L) %/% 16L
      for (b in unique(block)) {
        ids <- which(block == b)
        ids <- ids[.stacked_clear(stack, ids, unrelated, clearance)]
        if (!length(ids)) next
        conflicts <- .stacked_conflicts(stack, ids, obstacles)
        accepted <- NULL
        for (j in seq_along(ids)) {
          candidate <- candidates[[ids[j]]]
          if (any(c(conflicts$overlap[j], conflicts$crossings[j]) > limit + 1e-8) ||
              shared_length(candidate) > shared + 1e-8) next
          accepted <- candidate
          break
        }
        if (is.null(accepted)) next
        candidate <- accepted
        # Only geometry changes; endpoint, edge, path and segment identities stay.
        result <- e
        result[c("x", "y", "xend", "yend")] <- candidate
        result$bend <- c(FALSE, diff(candidate$xend - candidate$x) != 0)
        routes[[id]] <- result
        prepared[[id]] <- .prepare_route_obstacles(result)
        index <- .obstacle_index(prepared)
        changed <- TRUE
        break
      }
    }
    if (!changed || attempts >= max_routes) break
  }
  if (identical(routes, original)) return(routes)
  # Confirm the completed pass using the public crossing definition as well.
  before <- .routed_geometry_quality(original)
  after <- .routed_geometry_quality(routes)
  if (after["bends"] >= before["bends"] || any(after > before + 1e-8)) return(original)
  routes
}
