# Lay out weakly connected components independently, then use deterministic
# shelves to pack their full routed extents. Translation preserves every edge
# direction, bend and endpoint. Explicit user ranks lay out all components
# together, so that common ranks stay aligned.
.layout_components <- function(graph, layer, ordering, sweeps) {
  membership <- igraph::components(graph, mode = "weak")$membership
  groups <- unname(split(seq_len(igraph::vcount(graph)), membership))
  indexed <- igraph::set_edge_attr(graph, ".metrodag_original_id",
    value = seq_len(igraph::ecount(graph)))
  parts <- lapply(groups, function(ids) {
    subgraph <- igraph::induced_subgraph(indexed, ids)
    local_ids <- match(igraph::V(subgraph)$name, igraph::V(graph)$name)
    geometry <- .layout_component(subgraph, layer[local_ids], ordering, sweeps)
    original_edges <- igraph::edge_attr(subgraph, ".metrodag_original_id")
    geometry$edges$edge_id <- original_edges[geometry$edges$edge_id]
    xr <- range(c(geometry$x, geometry$edges$x, geometry$edges$xend))
    yr <- range(c(geometry$y, geometry$edges$y, geometry$edges$yend))
    list(ids = local_ids, geometry = geometry, xr = xr, yr = yr,
      width = max(4, diff(xr)), height = max(4, diff(yr)),
      key = sort(igraph::V(subgraph)$name, method = "radix")[1L])
  })
  widths <- vapply(parts, "[[", numeric(1), "width")
  heights <- vapply(parts, "[[", numeric(1), "height")
  priority <- order(-heights, -widths, vapply(parts, "[[", character(1), "key"), method = "radix")
  gap <- 4
  # a new shelf moves a component downstream along the causal axis,
  # e.g. an isolated variable after the outcome. Also try shelves wide enough
  # to fit all parts within the deepest component's depth (capped at twice the
  # square target), and keep them only when the packed box is no larger.
  area <- sum((widths + gap) * (heights + gap))
  shelves <- function(target) {  # offsets for one shelf width
    dx <- dy <- numeric(length(parts))
    cursor_x <- cursor_y <- row_height <- 0
    for (i in priority) {
      part <- parts[[i]]
      if (cursor_x > 0 && cursor_x + part$width > target) {
        cursor_x <- 0
        cursor_y <- cursor_y + row_height + gap
        row_height <- 0
      }
      dx[i] <- cursor_x - part$xr[1L]  # no extra padding before narrow parts
      dy[i] <- -cursor_y - part$yr[2L]  # isolates share the shelf's first rank
      cursor_x <- cursor_x + part$width + gap
      row_height <- max(row_height, part$height)
    }
    box <- c(diff(range(unlist(lapply(seq_along(parts), function(i) parts[[i]]$xr + dx[i])))),
      diff(range(unlist(lapply(seq_along(parts), function(i) parts[[i]]$yr + dy[i])))))
    list(dx = dx, dy = dy, area = prod(box))
  }
  square <- shelves(max(widths, sqrt(area)))
  deep <- shelves(max(widths, sqrt(area), min(area / (max(heights) + gap), 2 * sqrt(area))))
  offsets <- if (deep$area <= square$area + 1e-8) deep else square
  n <- igraph::vcount(graph)
  x <- y <- ord <- paths <- numeric(n)
  refined_layer <- layer
  routes <- list(); gaps <- list()
  path_offset <- 0
  for (i in priority) {
    part <- parts[[i]]; geometry <- part$geometry
    dx <- offsets$dx[i]; dy <- offsets$dy[i]
    x[part$ids] <- geometry$x + dx
    y[part$ids] <- geometry$y + dy
    ord[part$ids] <- geometry$ord
    refined_layer[part$ids] <- geometry$layer
    paths[part$ids] <- geometry$paths + path_offset
    e <- geometry$edges
    e$x <- e$x + dx; e$xend <- e$xend + dx
    e$y <- e$y + dy; e$yend <- e$yend + dy
    e$path_id <- e$path_id + path_offset
    routes[[length(routes) + 1L]] <- e
    gaps[[part$key]] <- geometry$gaps
    path_offset <- path_offset + max(geometry$paths)
  }
  e <- do.call(rbind, routes)
  e <- e[order(e$edge_id, e$segment, method = "radix"), , drop = FALSE]
  rownames(e) <- NULL
  list(x = x, y = y, layer = refined_layer, ord = ord, paths = paths, edges = e, gaps = gaps)
}
