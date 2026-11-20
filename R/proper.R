# Insert private waypoints so a long edge participates in every crossed rank.
# They are routing aids only: never add them to the user's graph or node table.
.proper_graph <- function(graph, layer) {
  n <- igraph::vcount(graph)
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  chains <- vector("list", nrow(ends))
  keys <- igraph::V(graph)$name
  order_sources <- order_targets <- rep("", n)
  expanded_layer <- as.integer(layer)
  links <- list()
  for (i in seq_len(nrow(ends))) {
    u <- ends[i, 1L]; v <- ends[i, 2L]
    between <- seq.int(layer[u], layer[v])
    between <- between[between > layer[u] & between < layer[v]]
    dummy <- length(expanded_layer) + seq_along(between)
    expanded_layer <- c(expanded_layer, between)
    if (length(between)) {
      keys <- c(keys, paste0(keys[u], " -> ", keys[v], " @ ", between))
      order_sources <- c(order_sources, rep(keys[u], length(between)))
      order_targets <- c(order_targets, rep(keys[v], length(between)))
    }
    chains[[i]] <- c(u, dummy, v)
    links[[i]] <- cbind(head(chains[[i]], -1L), tail(chains[[i]], -1L))
  }
  g <- igraph::make_empty_graph(length(expanded_layer), directed = TRUE)
  if (length(links)) g <- igraph::add_edges(g, as.vector(t(do.call(rbind, links))))
  igraph::V(g)$name <- keys
  # Display-like waypoint names can collide. Endpoint keys disambiguate their
  # ordering without renaming user vertices or changing ordering = "none".
  g <- igraph::set_vertex_attr(g, ".metrodag_order_source", value = order_sources)
  g <- igraph::set_vertex_attr(g, ".metrodag_order_target", value = order_targets)
  list(graph = g, layer = expanded_layer, chains = chains, n = n)
}

# Centered, separated coordinates; align ranks with their neighbours while
# preserving the crossing-reduced order. Isotonic regression prevents swaps.
.compact_positions <- function(graph, layer, ord, spacing = 4, neighbour_mode = "all") {
  levels <- sort(unique(layer))
  x <- as.numeric(ord)
  for (l in levels) {
    ids <- which(layer == l)
    x[ids] <- (ord[ids] - (length(ids) + 1) / 2) * spacing
  }
  neighbours <- lapply(seq_along(x), function(i)
    as.integer(igraph::neighbors(graph, i, mode = neighbour_mode)))
  # fixed sweep count; scaling it with the rank count changed DAG 4's
  # refined ranks (+1 bend, +1 crossing) with no gallery improvement.
  for (s in seq_len(8L)) {
    for (l in if (s %% 2L) levels else rev(levels)) {
      ids <- which(layer == l)
      ids <- ids[order(ord[ids], method = "radix")]
      target <- vapply(ids, function(i) {
        nb <- neighbours[[i]]
        if (length(nb)) stats::median(x[nb]) else x[i]
      }, numeric(1))
      offset <- seq_along(ids) * spacing
      fit <- if (length(ids) > 1L) stats::isoreg(target - offset)$yf else target - offset
      x[ids] <- round(fit + offset)
    }
  }
  x - round(mean(range(x)))
}

.empty_routes <- function() data.frame(
  from = character(), to = character(), edge_id = integer(), segment = integer(),
  x = numeric(), y = numeric(), xend = numeric(), yend = numeric(),
  bend = logical(), path_id = integer(), stringsAsFactors = FALSE)

# A finite family of monotone octilinear routes through the ordered waypoints.
# Score occupied tracks and crossings; leave a rank diagonally when possible so
# incident edges do not all share the same trunk.
.route_span <- function(x0, y0, x1, y1, occupied, preference = 0L) {
  h <- as.integer(y0 - y1)
  delta <- x1 - x0
  make <- function(lane) {
    first <- abs(lane - x0); last <- abs(x1 - lane)
    middle <- h - first - last
    if (middle < 0) return(NULL)
    moves <- c(rep(sign(lane - x0), first), rep(0, middle),
      rep(sign(x1 - lane), last))
    xs <- c(x0, x0 + cumsum(moves)); ys <- y0 - 0:h
    .route_frame(x = head(xs, -1L), y = head(ys, -1L),
      xend = tail(xs, -1L), yend = tail(ys, -1L))
  }
  lanes <- unique(c(round((x0 + x1) / 2), x0, x1,
    seq.int(min(x0, x1) - 2L, max(x0, x1) + 2L)))
  candidates <- Filter(Negate(is.null), lapply(lanes, make))
  # Pre-index occupied rows once, rather than scanning the complete drawing
  # separately for every unit segment of every candidate.
  row <- match(occupied$y, y0 - seq.int(0L, h - 1L))
  active <- occupied[!is.na(row), , drop = FALSE]
  row <- row[!is.na(row)]
  key <- function(z) paste(z$x, z$y, z$xend, z$yend)
  occupied_keys <- if (nrow(active)) unique(key(active)) else character()
  score <- function(e) {
    dx <- e$xend - e$x
    cost <- sum(diff(dx) != 0) * 0.8 + sum(abs(dx)) * 0.08
    if (nrow(active)) {
  # flat cost per occupied track. Multiplicity weighting traded
      # extra crossings for shared track on wide fans with no gallery gain.
      # On this grid a diagonal usually crosses a vertical at a grid point,
      # which the convergence term (5) charges; the row term (12) sees two
      # diagonals swapping within a row. Also charging grid-point crossings
      # with the crossing-pair detector removed crossings on the densest
      # corpus graphs but added shared track, which the restart rule never
      # trades for crossings.
      cost <- cost + 80 * sum(key(e) %in% occupied_keys) +
        12 * sum((e$x[row] - active$x) * (e$xend[row] - active$xend) < 0) +
        5 * sum(row < nrow(e) & e$xend[row] == active$xend)
    }
    # Approach a shared sink late, or leave a shared source early. Otherwise an
    # early route can occupy the central lane and force a simple fan to cross.
    diagonal <- which(dx != 0)
    tie <- if (!length(diagonal)) 0 else if (preference > 0L)
      (h - mean(diagonal)) / h * 0.1 else if (preference < 0L)
      mean(diagonal) / h * 0.1 else abs(sum(e$x) / length(e$x) - (x0 + x1) / 2) * 0.01
    cost + tie
  }
  candidates[[which.min(vapply(candidates, score, numeric(1)))]]
}

# `ranks` is "automatic" when the layers may be refined, "supplied" for user
# ranks without alignment groups, or "fixed" for alignment groups and the
# trial rebuilds of rank refinement. Supplied ranks keep every node in its
# layer, but the placement passes may still move nodes within it and change
# the gaps between layers. Fixed ranks skip those passes as well.
# `trials` holds the drawings already made for this component (see
# .layout_trials()); the rebuilds of rank refinement, restart variants and
# layering starts pass it on, so a start they repeat is not drawn again.
.layout_component <- function(graph, layer, ordering, sweeps, ranks = "automatic",
  align = list(), variant = "default", trials = .layout_trials()) {
  initial_layer <- layer
  proper <- .proper_graph(graph, layer)
  ord <- .order_layers(proper$graph, proper$layer, ordering, sweeps,
    reverse = variant == "reverse")
  x <- .compact_positions(proper$graph, proper$layer, ord,
    neighbour_mode = if (variant == "children") "out" else "all")
  if (length(align)) {
    x <- .aligned_positions(x, proper$layer, igraph::V(proper$graph)$name, align)
    for (rank in unique(proper$layer)) {
      ids <- which(proper$layer == rank)
      ord[ids] <- rank(x[ids], ties.method = "first")
    }
  }
  # Fan straightening is the only pass before rank refinement that `ranks`
  # changes; without it (fixed ranks) every mode draws the "plain" drawing.
  mode <- if (ranks == "fixed" || !.metro_stage("fan_straighten")) "plain" else ranks
  entry <- .layout_trial(trials, list(layer = proper$layer, ord = ord, x = x,
    endpoint_lanes = variant == "endpoint_lanes", align = align))
  drawn <- entry$drawings[[mode]]
  if (is.null(drawn)) {
    if (is.null(entry$routed)) entry$routed <- .route_component(graph, layer, proper,
      ord, x, align, endpoint_lanes = variant == "endpoint_lanes")
    nodes <- entry$routed$nodes
    routes <- entry$routed$routes
    moved <- FALSE
    kept <- TRUE  # the ranks are this call's `layer`
    if (mode != "plain") {
      refined <- .straighten_fans(nodes, routes,
        layer = if (ranks == "supplied") initial_layer)
      moved <- !identical(refined$routes, routes)
      routes <- refined$routes
      if (!identical(refined$nodes, nodes)) {
        moved <- TRUE
        kept <- FALSE
        nodes <- refined$nodes
        levels <- sort(unique(-nodes$y))
        layer <- match(-nodes$y, levels) - 1L
        for (l in unique(layer)) {
          members <- which(layer == l)
          ord[members] <- rank(nodes$x[members], ties.method = "first")
        }
      }
    }
    # Unmoved nodes and routes leave the rest of the pipeline as in the plain
    # drawing, which an earlier fixed-rank rebuild may already have made.
    drawn <- if (!moved) entry$drawings$plain
    if (is.null(drawn)) {
      finished <- .finish_component(nodes, routes, align)
      drawn <- list(own_layer = kept, geometry = list(
        x = finished$nodes$x, y = finished$nodes$y, layer = layer,
        ord = ord[seq_len(proper$n)], paths = entry$routed$paths,
        edges = if (length(finished$routes)) do.call(rbind, finished$routes) else
          .empty_routes(), gaps = diff(sort(unique(-finished$nodes$y)))))
      if (!moved) entry$drawings$plain <- drawn
    }
    entry$drawings[[mode]] <- drawn
  }
  geometry <- drawn$geometry
  # Ranks that fan straightening kept are this call's own `layer` argument.
  if (drawn$own_layer) geometry$layer <- initial_layer
  if (ranks == "fixed") return(geometry)
  # A layering start is compared before rank refinement, which its winner
  # receives afterwards; refining every trial doubled the cost of the stage.
  if (ranks == "automatic" && variant != "layering" && .metro_stage("rank_refinement"))
    geometry <- .refine_rank_assignment(graph, geometry, ordering, sweeps, variant = variant,
      trials = trials)
  # Restarts, layering starts and relaxation run once, for the component's own
  # pipeline; with ordering = "none" or zero sweeps the supplied order is kept.
  if (variant != "default" || ordering == "none" || !sweeps) return(geometry)
  if (.metro_stage("restart_variants"))
    geometry <- .refine_layout_variants(graph, initial_layer, geometry, ordering, sweeps,
      ranks = ranks, trials = trials)
  # Supplied ranks admit only moves within a layer, and no other layering.
  if (ranks == "supplied") return(.relax_placements(graph, geometry, transverse = TRUE))
  .refine_layering_variants(graph, initial_layer, geometry, ordering, sweeps, trials = trials)
}

# The drawings of one component's pipeline runs, kept for the duration of its
# layout. Up to rank refinement a drawing is determined by the proper
# layering, the initial layer order and coordinates, the endpoint-lane option
# and the alignment groups, together with the mode of fan straightening
# ("plain" when it is skipped). Each entry holds the routed drawing before
# fan straightening and the finished drawing of each mode.
.layout_trials <- function() {
  trials <- new.env(parent = emptyenv())
  trials$entries <- list()
  trials
}

# The entry of `trials` for this start, created when it is new. Entries are
# environments, so the caller fills them in place.
.layout_trial <- function(trials, start) {
  for (entry in trials$entries) if (identical(entry$start, start)) return(entry)
  entry <- new.env(parent = emptyenv())
  entry$start <- start
  entry$drawings <- list()
  trials$entries[[length(trials$entries) + 1L]] <- entry
  entry
}

# Initial routes along the ordered layers, then whole-edge straightening, fan
# balancing and crossing-pair rerouting: every pass before fan straightening.
.route_component <- function(graph, layer, proper, ord, x, align, endpoint_lanes = FALSE) {
  ee <- igraph::ends(proper$graph, igraph::E(proper$graph), names = FALSE)
  levels <- sort(unique(proper$layer))
  gaps <- rep(4, max(0L, length(levels) - 1L))
  for (j in seq_along(gaps)) {
    active <- which(proper$layer[ee[, 1L]] == levels[j])
    if (length(active)) gaps[j] <- max(4, abs(x[ee[active, 2L]] - x[ee[active, 1L]]) + 2)
  }
  ys <- -c(0, cumsum(gaps))
  y <- ys[match(proper$layer, levels)]
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  paths <- .path_cover(graph, layer, ord[seq_len(proper$n)])
  routes <- vector("list", nrow(ends))
  occupied <- .empty_routes()
  incoming <- igraph::degree(proper$graph, mode = "in")
  outgoing <- igraph::degree(proper$graph, mode = "out")
  aligned <- integer(proper$n)
  for (j in seq_along(align)) aligned[match(align[[j]], igraph::V(graph)$name)] <- j
  backbone <- which(aligned[ends[, 1L]] > 0L &
    aligned[ends[, 1L]] == aligned[ends[, 2L]])
  # Short edges first; deterministic name ties also stabilize reordered inputs.
  # keep this order for fan targets too; routing in-degree >= 3 fans
  # last added bends/crossings in the gallery at every in-degree threshold.
  priority <- order(!seq_len(nrow(ends)) %in% backbone, lengths(proper$chains),
    igraph::V(graph)$name[ends[, 1L]], igraph::V(graph)$name[ends[, 2L]], method = "radix")

  for (i in priority) {
    chain <- proper$chains[[i]]
    parts <- lapply(seq_len(length(chain) - 1L), function(j) {
      u <- chain[j]; v <- chain[j + 1L]
      preference <- if (incoming[v] > 1L && outgoing[u] <= 1L) 1L else
        if (outgoing[u] > 1L && incoming[v] <= 1L) -1L else 0L
      .route_span(x[u], y[u], x[v], y[v], occupied, preference)
    })
    e <- do.call(rbind, parts)
    u <- ends[i, 1L]; v <- ends[i, 2L]
    if (i %in% backbone) {
      straight <- .route_from_moves(x[u], y[u], rep(0, y[u] - y[v]))
      unrelated <- setdiff(seq_len(proper$n), c(u, v))
      if (.route_clear(straight, data.frame(x = x[unrelated], y = y[unrelated]))) e <- straight
    }
    e$from <- igraph::V(graph)$name[u]; e$to <- igraph::V(graph)$name[v]
    e$edge_id <- i; e$segment <- seq_len(nrow(e))
    e$bend <- c(FALSE, diff(e$xend - e$x) != 0)
    e$path_id <- if (paths[u] == paths[v]) paths[u] else NA_integer_
    e <- e[, names(occupied)]
    routes[[i]] <- e
    occupied <- rbind(occupied, e)
  }
  ids <- seq_len(proper$n)
  backbone <- backbone[!.route_bent(routes[backbone])]
  if (.metro_stage("straighten")) routes <- .straighten_routes(routes,
    data.frame(node = igraph::V(graph)$name, x = x[ids], y = y[ids], stringsAsFactors = FALSE),
    eligible = setdiff(seq_along(routes), backbone),
    endpoint_lanes = endpoint_lanes)  # V-overshoot start
  balanced <- list(nodes = data.frame(node = igraph::V(graph)$name,
    x = x[ids], y = y[ids], stringsAsFactors = FALSE), routes = routes)
  if (!length(align) && .metro_stage("balance"))
    balanced <- .balance_geometry(graph, balanced$nodes, routes)
  # Double the crossing-pair budget for aligned layouts. Cross-lagged
  # arrows (connecting nodes from different alignment groups at adjacent ranks)
  # form C(groups x waves, 2) natural crossing pairs; the default budget of 24
  # is often exhausted before all of them are attempted.
  control <- .metro_control()
  routes <- if (.metro_stage("crossing_pairs")) .reroute_crossing_pairs(balanced$routes,
    balanced$nodes, fixed = backbone, max_pairs = if (length(align) > 0L)
      control$crossing_pairs_aligned else control$crossing_pairs) else balanced$routes
  list(nodes = balanced$nodes, routes = routes, paths = paths)
}

# The passes after fan straightening: section cleanup, gap compaction,
# approach lengthening, node arrangement and the singleton-rank slide.
.finish_component <- function(nodes, routes, align) {
  if (.metro_stage("section_cleanup")) routes <- .simplify_route_sections(routes, nodes)
  if (.metro_stage("gap_compaction")) {
    compact <- .compact_route_gaps(nodes, routes)
    nodes <- compact$nodes
    routes <- compact$routes
  }
  if (.metro_stage("approach_lengthening")) routes <- .lengthen_route_approaches(routes, nodes)
  arranged <- if (length(align) || !.metro_stage("arrangement")) list(nodes = nodes, routes = routes) else
    .compact_node_arrangement(nodes, routes)
  nodes <- arranged$nodes
  routes <- arranged$routes
  # Singleton-rank median pass: slide lone nodes to the median x of their
  # immediate neighbours, within the standard spacing/clearance guards, and
  # keep rerouted edges with the moved nodes. Alignment groups fix transverse
  # coordinates, so a lone aligned node must stay on its group's line.
  if (!length(align) && .metro_stage("singleton_slide")) {
    slid <- .slide_singleton_ranks(nodes, routes)
    nodes <- slid$nodes
    routes <- slid$routes
  }
  list(nodes = nodes, routes = routes)
}

# Final placement passes: sources, then low-degree intermediates. With
# `transverse = TRUE` (supplied ranks) every node keeps its layer.
.relax_placements <- function(graph, geometry, transverse = FALSE) {
  if (.metro_stage("source_relaxation")) {
    relaxed <- .relax_source_positions(data.frame(node = igraph::V(graph)$name,
      x = geometry$x, y = geometry$y), unname(split(geometry$edges, geometry$edges$edge_id)),
      transverse = transverse)
    if (!identical(geometry$x, relaxed$nodes$x) || !identical(geometry$y, relaxed$nodes$y)) {
      geometry$x <- relaxed$nodes$x; geometry$y <- relaxed$nodes$y
      geometry$edges <- if (length(relaxed$routes)) do.call(rbind, relaxed$routes) else .empty_routes()
      geometry <- .rerank_geometry(geometry)
    }
  }
  if (.metro_stage("intermediate_relaxation")) {
    placed <- .relax_intermediate_positions(data.frame(node = igraph::V(graph)$name,
      x = geometry$x, y = geometry$y), unname(split(geometry$edges, geometry$edges$edge_id)),
      transverse = transverse)
    if (!identical(geometry$x, placed$nodes$x) || !identical(geometry$y, placed$nodes$y)) {
      geometry$x <- placed$nodes$x; geometry$y <- placed$nodes$y
      geometry$edges <- do.call(rbind, placed$routes)
      geometry <- .rerank_geometry(geometry)
    }
  }
  geometry
}

# For each rank containing exactly one node, try sliding it to the
# median x-coordinate of its immediate parents and children. Accept the move
# only when spacing and route-clearance guards are satisfied and no metric
# worsens. O(edges) per node: targeted at singleton-rank wide-lane pathology.
# Return the rerouted edges together with the nodes; returning only the
# nodes left accepted moves with arrows still attached to the old position.
.slide_singleton_ranks <- function(nodes, routes, spacing = 4) {
  if (!length(routes)) return(list(nodes = nodes, routes = routes))
  levels <- sort(unique(-nodes$y))
  rank_of <- match(-nodes$y, levels)
  from <- match(.route_from(routes), nodes$node)
  to   <- match(.route_to(routes), nodes$node)
  limit <- NULL
  for (i in order(nodes$node, method = "radix")) {
    # Only consider nodes that are alone in their rank.
    members <- which(rank_of == rank_of[i])
    if (length(members) != 1L) next
    parents  <- unique(from[to   == i])
    children <- unique(to  [from == i])
    if (!length(parents) && !length(children)) next
    target <- round(stats::median(nodes$x[c(parents, children)]))
    if (target == nodes$x[i]) next
    candidate <- nodes
    candidate$x[i] <- target
    # Enforce spacing against all other nodes.
    if (any(as.numeric(stats::dist(candidate[c("x", "y")])) < spacing - 1e-8)) next
    # Enforce monotone edges (height >= |delta|).
    heights <- candidate$y[from] - candidate$y[to]
    delta   <- candidate$x[to]  - candidate$x[from]
    if (any(heights < abs(delta))) next
    # Re-route affected edges and check clearance.
    affected <- which(from == i | to == i)
    trial <- .reroute_moved_nodes(routes, candidate, affected, from, to)
    if (is.null(trial)) next
    trial <- .straighten_routes(trial, candidate, eligible = affected)
    clear <- all(.routes_clear(trial, candidate))
    if (!clear) next
    if (is.null(limit)) limit <- .routed_geometry_quality(routes)
    quality <- .routed_geometry_quality(trial)
    if (any(quality > limit + 1e-8)) next
    nodes <- candidate; routes <- trial; limit <- quality
  }
  list(nodes = nodes, routes = routes)
}
