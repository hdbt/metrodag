# Held-out evaluation of metrodag against baseline layouts, plus an ablation.
# From the package root, after R CMD INSTALL . :
#   Rscript inst/benchmarks/evaluation.R baselines   # evaluation_baselines.csv, evaluation_stages.csv
#   Rscript inst/benchmarks/evaluation.R ablation    # writes evaluation_ablation.csv
# Requires dagitty, igraph and graphlayouts; Graphviz `dot` is used when found.
# The six gallery DAGs tuned the refinement passes and are excluded here.
suppressMessages(library(metrodag))
mode <- commandArgs(trailingOnly = TRUE)
mode <- if (length(mode)) mode[[1L]] else "baselines"
out_dir <- file.path("inst", "benchmarks")

# Corpus: DAGitty's bundled examples (published studies; see dagitty::getExample)
# and seeded random DAGs at two densities.
corpus <- function(random_sizes = c(10L, 20L, 40L), seeds = 1:5) {
  examples <- c("M-bias", "confounding", "mediator", "paths", "Sebastiani",
    "Polzer", "Schipf", "Shrier", "Thoemmes", "Kampen")
  items <- lapply(examples, function(name) {
    dag <- dagitty::getExample(name)
    list(name = name, family = "dagitty example", dag = dag,
      graph = metrodag:::.as_dag(dag, bidirected = "latent"))
  })
  for (n in random_sizes) for (density in c(1.5, 3)) for (seed in seeds) {
    g <- metrodag_examples("random", n = n, probability = density / n, seed = seed)
    items[[length(items) + 1L]] <- list(name = sprintf("random n=%d p=%.1f/n seed=%d", n, density, seed),
      family = sprintf("random n=%d, p=%.1f/n", n, density), dag = NULL, graph = g)
  }
  items
}

# Polylines (list of k x 2 matrices, one per edge) and node coordinates to a
# common metric set. Lengths and areas are scaled by the smallest distance
# between two stations, so methods with different units are comparable.
score <- function(nodes, polylines, seconds, method, item) {
  d <- stats::dist(nodes)
  unit <- if (length(d)) min(d[d > 1e-9]) else 1
  segments <- do.call(rbind, lapply(seq_along(polylines), function(i) {
    p <- polylines[[i]]
    data.frame(edge_id = i, segment = seq_len(nrow(p) - 1L),
      x = p[-nrow(p), 1L], y = p[-nrow(p), 2L], xend = p[-1L, 1L], yend = p[-1L, 2L])
  }))
  bends <- sum(vapply(polylines, function(p) {
    if (nrow(p) < 3L) return(0L)
    v <- diff(p); a <- atan2(v[, 2L], v[, 1L])
    sum(abs(diff(a)) > 1e-6)
  }, integer(1)))
  all <- rbind(nodes, do.call(rbind, polylines))
  area <- diff(range(all[, 1L])) * diff(range(all[, 2L]))
  data.frame(graph = item$name, family = item$family, method = method,
    nodes = nrow(nodes), edges = length(polylines),
    crossings = if (nrow(segments) > 1L) count_crossings(segments) else 0L,
    bends = bends,
    length = sum(sqrt((segments$xend - segments$x)^2 + (segments$yend - segments$y)^2)) / unit,
    area = area / unit^2, seconds = seconds)
}

edge_list <- function(g) igraph::as_edgelist(g, names = FALSE)

straight <- function(g, xy) {
  ends <- edge_list(g)
  lapply(seq_len(nrow(ends)), function(i) xy[ends[i, ], , drop = FALSE])
}

# Crossings left by layer ordering alone (on the proper graph with dummy
# nodes, before any routing), as the default layering and ordering give them.
ordering_crossings <- function(g) {
  layer <- metrodag:::.assign_layers(g, "longest", NULL)
  proper <- metrodag:::.proper_graph(g, layer)
  ord <- metrodag:::.order_layers(proper$graph, proper$layer, "barycenter", 8L)
  metrodag:::.crossings_for_orders(igraph::ends(proper$graph, igraph::E(proper$graph),
    names = FALSE), proper$layer, ord)
}

# Per graph: the layered crossing count and which refinement stages ran.
stage_rows <- list()

run_metrodag <- function(item, label = "metrodag") {
  t <- system.time(layout <- layout_metro_dag(item$graph))[["elapsed"]]
  if (mode == "baselines") stage_rows[[length(stage_rows) + 1L]] <<- data.frame(
    graph = item$name, ordering_crossings = ordering_crossings(item$graph),
    as.list(layout$parameters$stages), check.names = FALSE)
  nodes <- as.matrix(layout$nodes[match(igraph::V(item$graph)$name, layout$nodes$node), c("x", "y")])
  routes <- split(layout$edges, layout$edges$edge_id)
  polylines <- lapply(routes, function(e) {
    e <- e[order(e$segment), ]
    rbind(c(e$x[1L], e$y[1L]), cbind(e$xend, e$yend))
  })
  score(nodes, unname(polylines), t, label, item)
}

run_sugiyama <- function(item) {
  g <- item$graph
  t <- system.time(res <- igraph::layout_with_sugiyama(g))[["elapsed"]]
  xy <- rbind(res$layout, res$layout.dummy)
  e <- res$extd_graph; dummy <- igraph::V(e)$dummy
  ext <- edge_list(e)
  polylines <- list()
  for (k in which(!dummy[ext[, 1L]])) {
    chain <- ext[k, ]
    while (dummy[chain[length(chain)]]) chain <- c(chain, ext[ext[, 1L] == chain[length(chain)], 2L])
    polylines[[length(polylines) + 1L]] <- xy[chain, , drop = FALSE]
  }
  score(res$layout, polylines, t, "igraph Sugiyama", item)
}

run_dot <- function(item) {
  if (!nzchar(Sys.which("dot"))) return(NULL)
  g <- item$graph; nm <- igraph::V(g)$name
  ids <- paste0("n", seq_along(nm)); ends <- edge_list(g)
  text <- paste0("digraph { rankdir=LR; ", paste0(ids, ";", collapse = " "), " ",
    paste(ids[ends[, 1L]], "->", ids[ends[, 2L]], collapse = "; "), " }")
  file <- tempfile(fileext = ".dot"); writeLines(text, file); on.exit(unlink(file), add = TRUE)
  t <- system.time(plain <- system2("dot", c("-Tplain", file), stdout = TRUE))[["elapsed"]]
  rows <- strsplit(plain, " ", fixed = TRUE)
  node_rows <- rows[vapply(rows, function(r) r[1L] == "node", logical(1))]
  xy <- t(vapply(node_rows, function(r) as.numeric(r[3:4]), numeric(2)))
  xy <- xy[match(ids, vapply(node_rows, `[`, "", 2L)), , drop = FALSE]
  edge_rows <- rows[vapply(rows, function(r) r[1L] == "edge", logical(1))]
  # Spline control points approximate each curve as a polyline. Curves have no
  # straight-segment bends, so bends are reported as NA for this method.
  polylines <- lapply(edge_rows, function(r) {
    k <- as.integer(r[4L]); m <- matrix(as.numeric(r[4L + seq_len(2L * k)]), ncol = 2L, byrow = TRUE)
    rbind(xy[match(r[2L], ids), ], m, xy[match(r[3L], ids), ])
  })
  out <- score(xy, polylines, t, "Graphviz dot", item)
  out$bends <- NA
  out
}

run_straight <- function(item, method) {
  g <- igraph::as_undirected(item$graph, mode = "collapse")
  t <- system.time(xy <- switch(method,
    "stress (graphlayouts)" = graphlayouts::layout_with_stress(g),
    "Fruchterman-Reingold" = { set.seed(1); igraph::layout_with_fr(g) }))[["elapsed"]]
  score(xy, straight(item$graph, xy), t, method, item)
}

run_dagitty_coordinates <- function(item) {
  if (is.null(item$dag)) return(NULL)
  coords <- dagitty::coordinates(item$dag)
  nm <- igraph::V(item$graph)$name
  if (any(!nm %in% names(coords$x)) || anyNA(coords$x[nm])) return(NULL)
  xy <- cbind(coords$x[nm], -coords$y[nm])
  score(xy, straight(item$graph, xy), 0, "DAGitty author coordinates", item)
}

items <- corpus()
if (mode == "baselines") {
  rows <- list()
  for (item in items) {
    cat(item$name, "\n")
    rows <- c(rows, list(run_metrodag(item), run_sugiyama(item), run_dot(item),
      run_straight(item, "stress (graphlayouts)"), run_straight(item, "Fruchterman-Reingold"),
      run_dagitty_coordinates(item)))
  }
  result <- do.call(rbind, Filter(Negate(is.null), rows))
  utils::write.csv(result, file.path(out_dir, "evaluation_baselines.csv"), row.names = FALSE)
  utils::write.csv(do.call(rbind, stage_rows), file.path(out_dir, "evaluation_stages.csv"),
    row.names = FALSE)
} else if (mode == "ablation") {
  # Polzer (69 edges) takes several times longer than any other graph and is
  # left out of the ablation.
  subset <- Filter(function(item) item$name != "Polzer" &&
    (item$family == "dagitty example" || grepl("n=(10|20),", item$family) && grepl("seed=[1-3]$", item$name)), items)
  stages <- metrodag:::.metro_layout_stages
  path <- file.path(out_dir, "evaluation_ablation.csv")
  # Each configuration is appended as soon as it finishes, so an interrupted
  # run resumes from the first configuration that is not yet recorded.
  done <- if (file.exists(path)) unique(utils::read.csv(path)$disabled) else character()
  for (config in setdiff(c("all", stages), done)) {
    options(metrodag.control = if (config == "all") NULL else list(stages = stats::setNames(FALSE, config)))
    rows <- lapply(subset, function(item) cbind(run_metrodag(item, "metrodag"), disabled = config))
    utils::write.table(do.call(rbind, rows), path, sep = ",", row.names = FALSE,
      col.names = !file.exists(path), append = file.exists(path), qmethod = "double")
    cat(config, "\n")
  }
  options(metrodag.control = NULL)
}
