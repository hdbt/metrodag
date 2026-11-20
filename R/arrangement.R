# Compact the transverse spread after routing has exposed the actual branches.
# Whole ranks retain their order/ties; connected straight arrows constrain x
# offsets jointly, so shrinking a fan moves its branches together.
.compact_node_arrangement <- function(nodes, routes, spacing = 4, max_candidates = 8L) {
  finish <- function() list(nodes = nodes, routes = routes)
  if (length(routes) < 2L || max_candidates <= 0L) return(finish())
  # Rerouting compares each route step against other arrows. Bound that work
  # as well as the placement count; large components can skip this refinement.
  work <- sum(.route_steps(routes)) * length(routes)
  max_candidates <- .metro_stage_attempts("arrangement", work, max_candidates)
  if (max_candidates < 1L) return(finish())
  route_order <- order(.route_from(routes),
    .route_to(routes), method = "radix")
  routes <- routes[route_order]
  original <- routes
  finish <- function() list(nodes = nodes, routes = routes[order(route_order, method = "radix")])
  from <- match(.route_from(routes), nodes$node)
  to <- match(.route_to(routes), nodes$node)
  straight <- !.route_bent(routes)
  extent <- function(n, r) stats::setNames(.drawing_extent(n, r), c("width", "depth"))
  bounds <- extent(nodes, routes)
  if (bounds["width"] < spacing) return(finish())
  levels <- sort(unique(-nodes$y))
  rank <- match(-nodes$y, levels)
  gaps <- diff(levels)
  # Try a transverse move alone, then shorten gaps supporting straight fans.
  # Those gaps can reduce both dimensions without bending the fan's arrows.
  constrained <- vapply(seq_along(gaps), function(j)
    any(straight & nodes$x[from] != nodes$x[to] & rank[from] <= j & rank[to] > j), logical(1))
  depths <- list(nodes$y)
  for (j in order(!constrained, -gaps, seq_along(gaps), method = "radix")) if (gaps[j] > spacing) {
    slack <- gaps[j] - spacing
    for (amount in unique(c(ceiling(slack / 2), slack, 1L))) {
      g <- gaps; g[j] <- g[j] - amount
      depths[[length(depths) + 1L]] <- -c(0, cumsum(g))[rank]
    }
  }
  centre <- mean(range(nodes$x))
  targets <- lapply(c(0.75, 0.9), function(scale) round(centre + scale * (nodes$x - centre)))
  phases <- seq.int(0L, spacing - 1L)
  phases <- phases[order(-vapply(phases, function(p) sum(nodes$x %% spacing == p), integer(1)), phases, method = "radix")]
  targets <- c(targets, lapply(phases, function(p) round((nodes$x - p) / spacing) * spacing + p))
  seen <- character(); attempts <- 0L; limit <- NULL
  for (y in depths) for (target in targets) {
    x <- .arrangement_positions(nodes, routes, y, target, spacing)
    if (is.null(x) || diff(range(x)) >= diff(range(nodes$x))) next
    candidate <- nodes; candidate$x <- x; candidate$y <- y
    key <- paste(x[order(nodes$node, method = "radix")], y[order(nodes$node, method = "radix")], collapse = ",")
    if (key %in% seen) next
    seen <- c(seen, key)
    if (any(as.numeric(stats::dist(candidate[c("x", "y")])) < spacing - 1e-8)) next
    if (any(y[from] - y[to] < abs(x[to] - x[from]))) next
    if (attempts >= max_candidates) return(finish())
    attempts <- attempts + 1L
    for (mode in 0:1) {
      trial <- .route_arrangement(routes, candidate, nodes, mode)
      if (is.null(trial)) next
      trial <- .straighten_routes(trial, candidate, eligible = which(!straight))
      trial <- .lengthen_route_approaches(trial, candidate)
      if (any(.route_bent(trial[straight]))) next
      clear <- all(.routes_clear(trial, candidate))
      if (!clear) next
      size <- extent(candidate, trial)
      if (size["width"] >= bounds["width"] || any(size > bounds + 1e-8)) next
      if (is.null(limit)) limit <- .routed_geometry_quality(original)
      quality <- .routed_geometry_quality(trial)
      if (any(quality > limit + 1e-8)) next
      # Moving branches can expose longitudinal slack that was blocked before.
      compact <- .compact_route_gaps(candidate, trial)
      nodes <- compact$nodes
      routes <- .lengthen_route_approaches(compact$routes, nodes)
      return(finish())
    }
  }
  finish()
}

# Resolve relative x offsets for each connected component of straight arrows.
# Same-rank inequalities retain the existing left-to-right order and spacing.
.arrangement_positions <- function(nodes, routes, y, target, spacing = 4) {
  from <- match(.route_from(routes), nodes$node)
  to <- match(.route_to(routes), nodes$node)
  straight <- which(!.route_bent(routes))
  delta <- sign(nodes$x[to] - nodes$x[from]) * (y[from] - y[to])
  group <- rep(0L, nrow(nodes))
  relative <- numeric(nrow(nodes))
  groups <- 0L
  for (root in order(nodes$node, method = "radix")) if (group[root] == 0L) {
    groups <- groups + 1L
    group[root] <- groups
    todo <- root
    while (length(todo)) {
      i <- todo[1L]; todo <- todo[-1L]
      ids <- straight[from[straight] == i | to[straight] == i]
      for (id in ids) {
        other <- if (from[id] == i) to[id] else from[id]
        offset <- relative[i] + if (from[id] == i) delta[id] else -delta[id]
        if (group[other] != 0L) {
          if (abs(relative[other] - offset) > 1e-8) return(NULL)
        } else {
          group[other] <- groups
          relative[other] <- offset
          todo <- c(todo, other)
        }
      }
    }
  }
  translation <- vapply(seq_len(groups), function(g)
    round(stats::median(target[group == g] - relative[group == g])), numeric(1))
  constraints <- list()
  for (level in sort(unique(y), decreasing = TRUE)) {
    ids <- which(y == level)
    ids <- ids[order(nodes$x[ids], nodes$node[ids], method = "radix")]
    if (length(ids) > 1L) for (j in seq_len(length(ids) - 1L)) {
      a <- ids[j]; b <- ids[j + 1L]
      distance <- spacing + relative[a] - relative[b]
      if (group[a] == group[b]) {
        if (distance > 0) return(NULL)
      } else constraints[[length(constraints) + 1L]] <- c(group[a], group[b], distance)
    }
  }
  if (length(constraints)) {
    # Relax component translations; a positive cycle makes the order infeasible.
    for (pass in seq_len(groups)) {
      previous <- translation
      for (constraint in constraints) {
        a <- constraint[1L]; b <- constraint[2L]
        translation[b] <- max(translation[b], translation[a] + constraint[3L])
      }
      if (identical(previous, translation)) break
    }
    if (any(vapply(constraints, function(c)
      translation[c[2L]] < translation[c[1L]] + c[3L], logical(1)))) return(NULL)
  }
  x <- relative + translation[group]
  x - round(mean(range(x)) - mean(range(nodes$x)))
}

# Retain the original direction sequence when resizing a route to new endpoints.
# Allocate positive, negative and vertical steps across the existing runs.
.arrangement_route_shapes <- function(e, x0, y0, x1, y1) {
  height <- as.integer(y0 - y1)
  delta <- x1 - x0
  runs <- rle(e$xend - e$x)
  directions <- c(-1, 0, 1)
  count <- vapply(directions, function(d) sum(runs$values == d), integer(1))
  negative <- sum(runs$lengths[runs$values == -1])
  options <- unique(c(negative, round(negative * height / nrow(e)),
    max(count[1L], count[3L] - delta)))
  out <- list()
  for (n in options) {
    positive <- n + delta
    vertical <- height - positive - n
    total <- c(n, vertical, positive)
    if (any(total < count) || any(total[count == 0] != 0)) next
    for (style in c("proportion", "first", "last")) {
      sizes <- integer(length(runs$lengths))
      for (k in seq_along(total)) {
        ids <- which(runs$values == directions[k])
        if (!length(ids)) next
        extra <- total[k] - length(ids)
        part <- switch(style,
          proportion = extra * runs$lengths[ids] / sum(runs$lengths[ids]),
          first = c(extra, rep(0, length(ids) - 1L)),
          last = c(rep(0, length(ids) - 1L), extra))
        base <- floor(part)
        remaining <- extra - sum(base)
        if (remaining > 0) {
          selected <- order(-(part - base), seq_along(ids), method = "radix")[seq_len(remaining)]
          base[selected] <- base[selected] + 1L
        }
        sizes[ids] <- 1L + base
      }
      out[[length(out) + 1L]] <- .route_from_moves(x0, y0, rep(runs$values, sizes))
    }
  }
  out
}

# Route moved endpoints together, trying short-first and long-first orders.
# Existing lanes and displaced endpoint geometry provide deterministic ties.
.route_arrangement <- function(routes, nodes, oldnodes, order_mode = 0L) {
  from <- match(.route_from(routes), nodes$node)
  to <- match(.route_to(routes), nodes$node)
  moved <- which(nodes$x != oldnodes$x | nodes$y != oldnodes$y)
  affected <- which(from %in% moved | to %in% moved)
  if (!length(affected)) return(routes)
  occupied <- .bind_route_columns(routes[-affected])
  if (is.null(occupied)) occupied <- .empty_routes()
  height <- nodes$y[from] - nodes$y[to]
  straight <- !.route_bent(routes)
  priority <- affected[order(!straight[affected],
    if (order_mode == 0L) height[affected] else -height[affected],
    nodes$node[from[affected]], nodes$node[to[affected]], method = "radix")]
  trial <- routes
  # Obstacles grow by each rerouted arrow; earlier rows keep their order.
  obstacles <- .prepare_route_obstacles(occupied)
  for (id in priority) {
    e <- routes[[id]]
    keep <- !nodes$node %in% c(e$from[1L], e$to[1L])
    # Initial rerouting keeps up to two units of the old station clearance.
    # Subsequent straightening uses its usual 0.8-unit floor.
    margin <- pmin(2, .route_clearances(.route_runs(e), list(x = oldnodes$x[keep], y = oldnodes$y[keep])))
    skeleton <- .route_from_moves(nodes$x[from[id]], nodes$y[from[id]], rep(0, height[id]))
    skeleton$xend[nrow(skeleton)] <- nodes$x[to[id]]
    candidates <- c(.arrangement_route_shapes(e,
      nodes$x[from[id]], nodes$y[from[id]], nodes$x[to[id]], nodes$y[to[id]]),
      .simple_route_candidates(skeleton))
    best <- NULL; score <- rep(Inf, 5L)
    approach <- min(3L, tail(rle(e$xend - e$x)$lengths, 1L))
    stack <- .stack_routes(candidates)
    # Bends and the final approach (the last run of equal moves) of each
    # candidate, as rle() of its moves gives them.
    final <- stack$offset + stack$size + 1L -
      stack$run_start[!duplicated(stack$run_owner, fromLast = TRUE)]
    ids <- seq_len(stack$k)
    ids <- ids[!(straight[id] & stack$bends > 0) & final >= approach]
    ids <- ids[.stacked_clear(stack, ids, list(x = nodes$x[keep], y = nodes$y[keep]), margin)]
    conflicts <- .stacked_conflicts(stack, ids, obstacles)
    reference_rows <- NULL
    for (j in seq_along(ids)) {
      candidate <- candidates[[ids[j]]]
      moves <- candidate$xend - candidate$x
      bends <- sum(diff(moves) != 0)
      # The reference depends on the candidate only through its row count.
      if (!identical(reference_rows, nrow(candidate))) {
        reference_rows <- nrow(candidate)
        fraction <- seq(0, 1, length.out = nrow(candidate))
        reference <- stats::approx(seq(0, 1, length.out = nrow(e)), e$x, xout = fraction)$y
        if (order_mode == 1L) reference <- reference +
          (1 - fraction) * (nodes$x[from[id]] - e$x[1L]) +
          fraction * (nodes$x[to[id]] - tail(e$xend, 1L))
      }
      quality <- c(overlap = conflicts$overlap[j], crossings = conflicts$crossings[j], bends,
        sum(sqrt(moves^2 + 1)), sum(abs(candidate$x - reference)))
      if (!.lexically_better(quality, score)) next
      best <- candidate; score <- quality
    }
    if (is.null(best)) return(NULL)
    for (field in c("from", "to", "edge_id", "path_id")) best[[field]] <- e[[field]][1L]
    best$segment <- seq_len(nrow(best))
    best$bend <- c(FALSE, diff(best$xend - best$x) != 0)
    trial[[id]] <- best[, names(e)]
    obstacles <- .merge_route_obstacles(obstacles, .prepare_route_obstacles(trial[[id]]))
  }
  trial
}
