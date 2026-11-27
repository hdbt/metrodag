# Conventions annotate a layout: the causal example is laid out once for the file.
.conventions_causal <- .test_once(function() layout_metro_dag(metrodag_examples("causal")))

.layer_params <- function(p) {
  index <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomMetroDag"), logical(1)))
  p$layers[[index]]$geom_params
}

test_that("ring outcome markers distinguish exposure and outcome without colour", {
  layout <- .conventions_causal()
  mono <- metro_dag(layout, exposure = "X", outcome = "Y", style = "monochrome")
  nodes <- .layer_params(mono)$nodes
  expect_equal(nodes$marker[nodes$node == "Y"], "ring")
  expect_equal(nodes$marker[nodes$node == "X"], "plain")
  values <- attr(mono, "metrodag_legend_values")
  expect_equal(values$marker[match("ring", values$marker)], "ring")
  colour <- metro_dag(layout, exposure = "X", outcome = "Y",
    style = metro_style(outcome_marker = "ring"))
  expect_equal(.layer_params(colour)$nodes$marker[nodes$node == "Y"], "ring")
  expect_equal(.layer_params(metro_dag(layout, exposure = "X", outcome = "Y"))$nodes$marker,
    rep("plain", nrow(nodes)))
  expect_true(diagnose_metro_dag(mono, width = 85)$ok)
  expect_error(metro_style(outcome_marker = "star"), "outcome_marker")
})

test_that("selected nodes are drawn, imported from DAGitty and refused by adjustment checks", {
  edges <- data.frame(from = c("X", "X", "Y", "C", "C"), to = c("Y", "S", "S", "X", "Y"))
  p <- metro_dag(edges, exposure = "X", outcome = "Y", selected = "S", adjusted = "C")
  nodes <- .layer_params(p)$nodes
  expect_equal(nodes$role[nodes$node == "S"], "selected")
  expect_equal(nodes$shape[nodes$node == "S"], 22L)
  expect_equal(nodes$marker[nodes$node == "S"], "double")
  labels <- ggplot2::ggplot_build(p)$plot$scales$get_scales("shape")$get_labels()
  expect_true("Selected (conditioned on)" %in% labels)
  expect_true(diagnose_metro_dag(p, width = 120)$ok)
  expect_error(metro_dag(edges, adjusted = "S", selected = "S"), "disjoint")
  skip_if_not_installed("dagitty")
  expect_error(check_metro_adjustment(p), "does not model selection")
  g <- dagitty::dagitty('dag { X [exposure]; Y [outcome]; S [selected]; "my var" [selected];
    "tricky [selected]" -> X; X -> Y; X -> S; Y -> S; Y -> "my var" }')
  imported <- metro_dag(g)
  roles <- stats::setNames(.layer_params(imported)$nodes$role, .layer_params(imported)$nodes$node)
  expect_equal(unname(roles[c("S", "my var", "tricky [selected]")]), c("selected", "selected", "observed"))
  expect_error(check_metro_adjustment(g, adjusted = character(0)), "does not model selection")
})

test_that("bidirected arcs become unlabelled unobserved common causes on request", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { X [exposure]; Y [outcome]; X -> Y; X <-> Y; C -> X; C -> Y }")
  expect_error(metro_dag(g), "bidirected = \"latent\"")
  p <- metro_dag(g, bidirected = "latent")
  nodes <- .layer_params(p)$nodes
  hidden <- nodes[startsWith(nodes$node, ".latent_"), ]
  expect_equal(nrow(hidden), 1L)
  expect_equal(hidden$role, "unobserved")
  expect_equal(hidden$label, "")
  edges <- attr(p, "metrodag_layout")$edges
  first <- edges[!duplicated(edges$edge_id), ]
  expect_setequal(first$to[first$from == hidden$node], c("X", "Y"))
  expect_true(all(.layer_params(p)$edges$linetype[.layer_params(p)$edges$from == hidden$node] == "dashed"))
  # Other unobserved variables do not make the arc's latent cause observed,
  # and it cannot take another role.
  layout <- attr(p, "metrodag_layout")
  for (q in list(metro_dag(g, bidirected = "latent", unobserved = "C"),
      metro_dag(layout, unobserved = character(0)))) {
    drawn <- .layer_params(q)
    expect_equal(drawn$nodes$role[drawn$nodes$node == hidden$node], "unobserved")
    expect_true(all(drawn$edges$linetype[drawn$edges$from == hidden$node] == "dashed"))
  }
  expect_error(metro_dag(layout, adjusted = hidden$node), "always unobserved")
  expect_error(metro_dag(layout, exposure = hidden$node, outcome = "Y"), "always unobserved")
  expect_error(highlight_paths(p, from = hidden$node), "from")
  # Ranks are given for the visible variables only; the latent cause joins the
  # rank before both of its children and gets no header.
  ranked <- layout_metro_dag(g, bidirected = "latent", rank = c(C = 0, X = 1, Y = 2))
  layers <- stats::setNames(ranked$nodes$layer, ranked$nodes$node)
  expect_equal(unname(layers[hidden$node]), unname(layers["C"]))
  expect_equal(.layer_params(metro_dag(ranked, rank_labels = c("W0", "W1", "W2")))$headers$label,
    c("W0", "W1", "W2"))
  alone <- layout_metro_dag(dagitty::dagitty("dag { X -> Y; X <-> Y }"), bidirected = "latent",
    rank = c(X = 0, Y = 1))
  expect_equal(.layer_params(metro_dag(alone, rank_labels = c("A", "B")))$headers$label, c("A", "B"))
  expect_true(any(grepl("X <-> Y", attr(highlight_paths(p), "metrodag_paths")$path, fixed = TRUE)))
  # The latent node encodes the same assumption: C alone does not block X <-> Y.
  expect_false(check_metro_adjustment(p, adjusted = "C")$valid)
  layout <- layout_metro_dag(g, bidirected = "latent")
  expect_equal(igraph::vcount(layout$graph), 4L)
  expect_error(metro_dag(layout, bidirected = "latent"), "omit")
  expect_error(metro_dag(g, bidirected = "maybe"), "bidirected")
  skip_if_not_installed("ggdag")
  tidy <- ggdag::tidy_dagitty(g, seed = 1)
  expect_error(metro_dag(tidy), "bidirected")
  expect_equal(nrow(attr(metro_dag(tidy, bidirected = "latent"), "metrodag_layout")$nodes), 4L)
})

test_that("highlight_paths emphasises open paths only and keeps geometry", {
  skip_if_not_installed("dagitty")
  p <- metro_dag(.conventions_causal(), exposure = "X", outcome = "Y",
    unobserved = "U", adjusted = "C")
  before <- .layer_params(p)$edges
  h <- highlight_paths(p)
  after <- .layer_params(h)$edges
  paths <- attr(h, "metrodag_paths")
  expect_true(all(c("from", "to", "path", "open", "highlighted") %in% names(paths)))
  expect_true(any(paths$open))
  expect_true(all(paths$highlighted == paths$open))
  changed <- unique(paste(after$from, after$to, sep = "->")[after$colour != before$colour])
  open_arrows <- unique(unlist(lapply(strsplit(paths$path[paths$open], " ", fixed = TRUE), function(t) {
    k <- seq.int(2L, length(t) - 1L, by = 2L)
    ifelse(t[k] == "->", paste(t[k - 1L], t[k + 1L], sep = "->"), paste(t[k + 1L], t[k - 1L], sep = "->"))
  })))
  expect_setequal(changed, open_arrows)
  expect_identical(after[c("x", "y", "xend", "yend")], before[c("x", "y", "xend", "yend")])
  # The original plot is not modified by reference.
  expect_identical(.layer_params(p)$edges, before)
  expect_identical(attr(h, "metrodag_layout"), attr(p, "metrodag_layout"))
  causal <- highlight_paths(p, paths = "causal", colour = "#0072B2")
  causal_paths <- attr(causal, "metrodag_paths")
  expect_true(all(causal_paths$causal))
  expect_true(all(grepl("->", causal_paths$path)))
  # Biasing paths are the open ones that are not directed from X to Y.
  biasing <- attr(highlight_paths(p, paths = "biasing"), "metrodag_paths")
  expect_identical(biasing$highlighted, biasing$open & !biasing$causal)
  expect_true(any(biasing$open & biasing$causal & !biasing$highlighted))
  expect_false(attr(biasing, "truncated"))
  # A and B are joined only through the collider D, which blocks the path.
  expect_warning(highlight_paths(metro_dag(metrodag_examples("collider"), exposure = "A", outcome = "B")),
    "No open paths")
  expect_error(highlight_paths(p, from = "Q"), "from")
  expect_error(highlight_paths(attr(p, "metrodag_layout")), "MetroDAG plot")
  expect_error(highlight_paths(p, paths = "backdoor"), "paths")
})

test_that("highlight_paths never misses open paths because of the path limit", {
  skip_if_not_installed("dagitty")
  # 31 arrows and 5377 paths between v1 and v10, 8 of them open given {v2, v3}.
  g <- metrodag_examples("random", n = 10, probability = 0.7, seed = 1)
  # Paths depend on the graph, not on the drawing: skip refinement for speed.
  old <- options(metrodag.control = list(stages = stats::setNames(
    rep(FALSE, length(.metro_layout_stages)), .metro_layout_stages)))
  on.exit(options(old), add = TRUE)
  l <- layout_metro_dag(g)
  p <- plot_metro_dag(l, exposure = "v1", outcome = "v10", adjusted = c("v2", "v3"))
  full <- highlight_paths(p)
  paths <- attr(full, "metrodag_paths")
  expect_false(attr(paths, "truncated"))
  expect_equal(nrow(paths), 5377L)
  expect_equal(sum(paths$open), 8L)
  expect_true("v1 -> v6 -> v10" %in% paths$path[paths$open])
  # A truncated search says so instead of claiming there are no open paths.
  expect_warning(short <- highlight_paths(p, limit = 100), "stopped at `limit` = 100")
  expect_true(attr(attr(short, "metrodag_paths"), "truncated"))
  causal <- suppressWarnings(highlight_paths(p, limit = 100, paths = "causal"))
  expect_true(any(attr(causal, "metrodag_paths")$highlighted))
  # Where no open path exists, a truncated search stays silent about the limit.
  closed <- plot_metro_dag(l, exposure = "v1", outcome = "v10", adjusted = paste0("v", 2:9))
  messages <- character()
  withCallingHandlers(highlight_paths(closed, limit = 5), warning = function(w) {
    messages <<- c(messages, conditionMessage(w)); invokeRestart("muffleWarning")
  })
  expect_identical(messages, "No open paths found between v1 and v10.")
  # v1 has no parents, so the empty set meets the adjustment criterion and
  # leaves no biasing path open: a truncated biasing search need not warn.
  crude <- plot_metro_dag(l, exposure = "v1", outcome = "v10", unadjusted = TRUE)
  messages <- character()
  withCallingHandlers(biasing <- highlight_paths(crude, paths = "biasing", limit = 100), warning = function(w) {
    messages <<- c(messages, conditionMessage(w)); invokeRestart("muffleWarning")
  })
  expect_identical(messages, "No open biasing paths found between v1 and v10.")
  expect_false(attr(attr(biasing, "metrodag_paths"), "truncated"))
})

test_that("highlight_paths keeps data-frame edge styles", {
  skip_if_not_installed("dagitty")
  edges <- data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y"))
  for (style in list(data.frame(from = "C", to = "Y", linetype = "dotted"),
      data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y"), width = c(0.3, 0.3, 0.3)))) {
    p <- metro_dag(edges, exposure = "X", outcome = "Y", edge_style = style)
    h <- highlight_paths(p, paths = "biasing")
    recorded <- attr(h, "metrodag_edge_style")
    expect_s3_class(recorded, "data.frame")
    expect_named(recorded, c("from", "to", "linetype", "colour", "width"))
    expect_setequal(paste(recorded$from, recorded$to, sep = "->"),
      unique(c(paste(style$from, style$to, sep = "->"), "C->X", "C->Y")))
    drawn <- .layer_params(h)$edges
    if ("linetype" %in% names(style)) expect_true(all(drawn$linetype[drawn$from == "C" & drawn$to == "Y"] == "dotted"))
    expect_true(all(drawn$colour[drawn$from == "C"] == "#C0392B"))
  }
})

test_that("edge_legend adds line keys for arrow conventions", {
  layout <- .conventions_causal()
  p <- metro_dag(layout, exposure = "X", outcome = "Y",
    edge_style = list("C->Y" = list(linetype = "dotted")),
    edge_legend = c(dotted = "Assumed, uncertain"))
  scale <- ggplot2::ggplot_build(p)$plot$scales$get_scales("linetype")
  expect_equal(scale$get_labels(), "Assumed, uncertain")
  expect_true(attr(p, "metrodag_annotation_height") > attr(metro_dag(layout,
    exposure = "X", outcome = "Y"), "metrodag_annotation_height"))
  expect_true(diagnose_metro_dag(p, width = 120)$ok)
  expect_error(metro_dag(layout, edge_legend = c(wiggly = "No")), "edge_legend")
  expect_error(metro_dag(layout, edge_legend = "No names"), "edge_legend")
  comparison <- compare_metro_dags(layout, list(A = list(exposure = "X", outcome = "Y")),
    edge_legend = c(dotted = "Assumed"))
  expect_equal(ggplot2::ggplot_build(comparison$legend_plot)$plot$scales$get_scales("linetype")$get_labels(),
    "Assumed")
})

test_that("rank headers label each rank and keep node labels clear", {
  edges <- data.frame(from = c("C", "C", "X1", "Y1", "X1"), to = c("X1", "Y1", "X2", "Y2", "Y2"))
  ranks <- c(C = 0, X1 = 1, Y1 = 1, X2 = 2, Y2 = 2)
  for (direction in c("right", "down")) {
    p <- metro_dag(edges, rank = ranks, direction = direction,
      rank_labels = c("Baseline", "Wave 1", "Wave 2"))
    params <- .layer_params(p)
    expect_equal(params$headers$label, c("Baseline", "Wave 1", "Wave 2"))
    expect_equal(params$config$rank_header$side, if (direction == "right") "top" else "left")
    report <- diagnose_metro_dag(p, width = 120, device = "svg")
    expect_true(report$ok)
    expect_false(any(report$issues$type == "label_bounds"))
    skip_if_not_installed("svglite")
    file <- tempfile(fileext = ".svg")
    on.exit(unlink(file), add = TRUE)
    save_metro_dag(file, p, width = 120)
    svg <- paste(readLines(file), collapse = "\n")
    for (label in c("Baseline", "Wave 1", "Wave 2")) expect_true(grepl(label, svg, fixed = TRUE))
  }
  expect_error(metro_dag(edges, rank = ranks, rank_labels = c("One", "Two")), "one label per rank")
})

test_that("rank headers sit between the drawing and the title or legend", {
  layout <- .conventions_causal()
  for (position in c("bottom", "top")) {
    p <- plot_metro_dag(layout, exposure = "X", outcome = "Y", adjusted = "C",
      estimand = "Total effect of X on Y", rank_labels = paste("Wave", 1:5),
      style = metro_style(legend_position = position))
    table <- ggplot2::ggplotGrob(p)
    row <- function(name) table$layout$t[grepl(name, table$layout$name)][1L]
    expect_gt(row("^axis-t"), row("^subtitle"))
    expect_lt(row("^axis-t"), row("^panel"))
    if (position == "top") {
      # ggplot2 3.5 and later name the box "guide-box-top" and keep empty
      # boxes for the other sides; ggplot2 3.4 names the only box "guide-box".
      drawn <- grepl("^guide-box(-top)?$", table$layout$name) &
        !vapply(table$grobs, inherits, logical(1), "zeroGrob")
      expect_equal(sum(drawn), 1L)
      expect_gt(row("^axis-t"), table$layout$t[drawn])
    }
    expect_true(diagnose_metro_dag(p, width = 120)$ok)
  }
  # Headers follow the axis when the drawing is flipped, and clipping no
  # longer cuts their band out of the panel.
  p <- plot_metro_dag(layout, rank_labels = paste0("R", 1:5))
  flipped <- ggplot2::ggplotGrob(suppressMessages(p + ggplot2::coord_flip(clip = "off")))
  expect_true(any(grepl("^axis-[lr]", flipped$layout$name) &
    !vapply(flipped$grobs, inherits, logical(1), "zeroGrob")))
  expect_true(diagnose_metro_dag(suppressMessages(p + ggplot2::coord_equal(clip = "on")), width = 120)$ok)
  # Long headers that collide at this size are reported.
  chain <- plot_metro_dag(layout_metro_dag(metrodag_examples("chain")), rank_labels =
    c("Baseline enrolment", "First follow-up", "Second follow-up", "Final outcome visit"))
  report <- diagnose_metro_dag(chain, width = 85)
  expect_true(any(report$issues$type == "rank_header_overlap"))
  expect_false(any(diagnose_metro_dag(chain, width = 240)$issues$type == "rank_header_overlap"))
})

test_that("rank headers need ranks shared across packed components", {
  g <- metro_graph(data.frame(name = c(LETTERS[1:8], "I")),
    data.frame(from = c("A", "B", "D", "F", "G"), to = c("B", "C", "E", "G", "H")))
  expect_error(metro_dag(g, rank_labels = c("R0", "R1", "R2")), "Supply `rank`")
  ranks <- c(A = 0, B = 1, C = 2, D = 0, E = 1, F = 0, G = 1, H = 2, I = 0)
  p <- metro_dag(g, rank = ranks, rank_labels = c("R0", "R1", "R2"))
  expect_equal(.layer_params(p)$headers$label, c("R0", "R1", "R2"))
  # A component stacked below the others still shares the rank positions.
  stacked <- metro_dag(igraph::graph_from_literal(A-+B, B-+C, Z), rank_labels = c("R0", "R1", "R2"))
  expect_equal(.layer_params(stacked)$headers$label, c("R0", "R1", "R2"))
})
