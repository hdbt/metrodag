# Most messages below are raised after layout, when the plot is drawn: the
# causal example is laid out once for the file, plain and with labels.
.causal_plain <- .test_once(function() layout_metro_dag(metrodag_examples("causal")))
.causal_layout <- .test_once(function() attr(metro_dag(.causal_plain(),
  labels = c(X = "Cigarette consumption", Y = "Lung cancer risk")), "metrodag_layout"))

test_that("labels messages name the entries at fault", {
  fixed <- .causal_layout()
  nodes <- paste(igraph::V(fixed$graph)$name, collapse = ", ")
  hint <- sprintf(" Node names are: %s.", nodes)
  expect_error(metro_dag(fixed, labels = c(X = "Exposure", Q = "Nope", Z = "Not here")),
    paste0("`labels` names nodes not in the graph: `Q` and `Z`.", hint), fixed = TRUE)
  expect_error(metro_dag(fixed, labels = c(X = "Exposure", Y = NA)),
    "`labels` has a missing value for `Y`", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = c(X = "Exposure", X = "Treatment")),
    "`labels` names `X` more than once.", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = c("Exposure", "Outcome")),
    paste0("`labels` must be named by node, for example c(X = \"Exposure\").", hint), fixed = TRUE)
  expect_error(metro_dag(fixed, labels = stats::setNames(c("Exposure", "Outcome"), c("X", ""))),
    "`labels` has `Outcome` without a node name.", fixed = TRUE)
  # A named factor, as read from a node table, is used with its names.
  shown <- igraph::vertex_attr(attr(metro_dag(fixed, labels = factor(c(X = "Exposure"))),
    "metrodag_layout")$graph, "label")
  expect_identical(shown[igraph::V(fixed$graph)$name == "X"], "Exposure")
  expect_error(metro_dag(fixed, labels = factor("Exposure")), "`labels` must be named by node", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = list(X = "Exposure")), "not a list", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = c(X = 1)), "not a numeric vector", fixed = TRUE)
  # Every type reads as English: an article where one belongs, none for NULL.
  expect_error(metro_dag(fixed, labels = 1:2), "named by node, not a numeric vector.", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = NULL), "named by node, not NULL.", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = NA), "named by node, not NA.", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = c(TRUE, FALSE)), "not a logical vector.", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = as.raw(1)), "not a raw vector.", fixed = TRUE)
  expect_error(metro_dag(fixed, labels = Sys.Date()), "not an object of class `Date`.", fixed = TRUE)
  # A display label used as a name points to the node.
  expect_error(metro_dag(fixed, labels = c("Cigarette consumption" = "Cigarettes")),
    "`Cigarette consumption` is the display label of node `X`; use the node name.", fixed = TRUE)
})

test_that("role messages name unknown values and display labels used as names", {
  fixed <- .causal_layout()
  expect_error(metro_dag(fixed, exposure = "Q"),
    "`exposure` names a node not in the graph: `Q`. Node names are:", fixed = TRUE)
  expect_error(metro_dag(fixed, outcome = c("Q", "R")),
    "`outcome` names nodes not in the graph: `Q` and `R`.", fixed = TRUE)
  expect_error(metro_dag(fixed, exposure = "Cigarette consumption"),
    "`Cigarette consumption` is the display label of node `X`; use the node name.", fixed = TRUE)
  # Labels supplied in the same call count too, even with line breaks.
  expect_error(metro_dag(.causal_plain(), labels = c(C = "Socioeconomic\nbackground"),
    adjusted = "Socioeconomic background"),
    "`Socioeconomic background` is the display label of node `C`", fixed = TRUE)
  expect_error(metro_dag(fixed, adjusted = c("Cigarette consumption", "Lung cancer risk")),
    "`Cigarette consumption` and `Lung cancer risk` are display labels of nodes `X` and `Y`; use the node names.",
    fixed = TRUE)
  expect_error(metro_dag(fixed, selected = c("S", "S")), "`selected` names `S` more than once.", fixed = TRUE)
  expect_error(metro_dag(fixed, exposure = NA), "`exposure` contains a missing value", fixed = TRUE)
  expect_error(metro_dag(fixed, exposure = list("X")), "`exposure` must be a character vector of node names.",
    fixed = TRUE)
  expect_error(metro_dag(fixed, unadjusted = TRUE, adjusted = c("C", "S")),
    "non-empty `adjusted` set (`C` and `S`).", fixed = TRUE)
  skip_if_not_installed("dagitty")
  expect_error(check_metro_adjustment(fixed, exposure = "Cigarette consumption", outcome = "Y",
    unadjusted = TRUE), "display label of node `X`", fixed = TRUE)
})

test_that("role clashes name the node, both roles and an inherited role", {
  fixed <- .causal_layout()
  expect_error(metro_dag(fixed, exposure = "X", adjusted = "X"),
    "Node `X` cannot be both exposure and adjusted; roles must be disjoint.", fixed = TRUE)
  inherited <- attr(metro_dag(fixed, unobserved = "U"), "metrodag_layout")
  expect_error(metro_dag(inherited, adjusted = c("C", "U")), paste0(
    "Node `U` cannot be both adjusted and unobserved; roles must be disjoint. ",
    "Its unobserved role is inherited from the graph or a reused layout; clear it with ",
    "`unobserved = character(0)` or give `unobserved` without `U`."), fixed = TRUE)
  # Both clearing routes work.
  expect_s3_class(metro_dag(inherited, adjusted = c("C", "U"), unobserved = character(0)), "ggplot")
  expect_s3_class(metro_dag(inherited, adjusted = "U", unobserved = "S"), "ggplot")
  # Roles stored on the input graph are inherited in the same way.
  g <- igraph::set_vertex_attr(metrodag_examples("causal"), "role", index = "Y", value = "outcome")
  expect_error(metro_dag(g, exposure = "Y"), "Its outcome role is inherited", fixed = TRUE)
})

test_that("numeric and factor role values select nodes by name", {
  edges <- data.frame(from = 1:3, to = 2:4)
  p <- metro_dag(edges, exposure = 1, outcome = factor(4), adjusted = 2L)
  analysis <- attr(p, "metrodag_analysis")
  expect_identical(analysis$exposure, "1")
  expect_identical(analysis$outcome, "4")
  expect_identical(analysis$adjusted, "2")
  same <- metro_dag(edges, exposure = "1", outcome = "4", adjusted = "2")
  expect_identical(analysis, attr(same, "metrodag_analysis"))
  expect_error(metro_dag(edges, exposure = 9), "`exposure` names a node not in the graph: `9`.", fixed = TRUE)
  comparison <- compare_metro_dags(edges, list(A = list(exposure = 1, outcome = 4)))
  expect_identical(attr(comparison$plots[[1L]], "metrodag_analysis")$exposure, "1")
})

test_that("label_nudge, node_groups and node_group_colours name what is wrong", {
  fixed <- .causal_layout()
  expect_error(metro_dag(fixed, label_nudge = list(Q = c(0, 2))),
    "`label_nudge` names a node not in the graph: `Q`. Node names are:", fixed = TRUE)
  expect_error(metro_dag(fixed, label_nudge = list(X = c(0, 2), X = c(1, 1))),
    "`label_nudge` names `X` more than once.", fixed = TRUE)
  expect_error(metro_dag(fixed, label_nudge = list(X = c(0, NA), C = 1)),
    "`label_nudge` for `X` and `C` must be two finite numbers", fixed = TRUE)
  expect_error(metro_dag(fixed, label_nudge = list(c(0, 2))), "named list", fixed = TRUE)
  expect_error(metro_dag(fixed, node_groups = list(A = c("X", "Q"))),
    "`node_groups` category `A` names a node not in the graph: `Q`.", fixed = TRUE)
  expect_error(metro_dag(fixed, node_groups = list(A = "Cigarette consumption")),
    "display label of node `X`", fixed = TRUE)
  expect_error(metro_dag(fixed, node_groups = list(A = "X", B = c("C", "X"))),
    "Node `X` is in more than one `node_groups` category (`A` and `B`)", fixed = TRUE)
  expect_error(metro_dag(fixed, node_groups = list(A = "X", A = "C")),
    "`node_groups` names category `A` more than once.", fixed = TRUE)
  expect_error(metro_dag(fixed, node_groups = list(A = 1)),
    "`node_groups` category `A` must be a non-empty character vector", fixed = TRUE)
  expect_error(metro_dag(fixed, node_groups = list(A = "X", B = "C"), node_group_colours = c(Z = "red")),
    "`node_group_colours` names a group not in `node_groups`: `Z`. Groups are: `A` and `B`.", fixed = TRUE)
  expect_error(metro_dag(fixed, node_group_colours = c(Z = "red")),
    "Define the groups with `node_groups`.", fixed = TRUE)
  expect_error(metro_dag(fixed, node_groups = list(A = "X", B = "C"),
    node_group_colours = c(A = "red", B = "no such colour")),
    "`node_group_colours` has invalid colours for `B`: \"no such colour\".", fixed = TRUE)
})

test_that("edge_style keys tolerate spaces and name arrows the graph lacks", {
  g <- .causal_plain()
  spaced <- metro_dag(g, edge_style = list("X -> M" = list(linetype = "dotted"), " U->C " = list(width = 0.8)))
  tight <- metro_dag(g, edge_style = list("X->M" = list(linetype = "dotted"), "U->C" = list(width = 0.8)))
  layer <- function(p) Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)$geom_params$edges
  expect_identical(layer(spaced), layer(tight))
  expect_identical(attr(spaced, "metrodag_edge_style"), attr(tight, "metrodag_edge_style"))
  expect_error(metro_dag(g, edge_style = list("X -> M" = list(width = 1), "X->M" = list(width = 2))),
    "gives `X -> M` more than once", fixed = TRUE)
  expect_error(metro_dag(g, edge_style = list("M->X" = list(width = 1), "X->Q" = list(width = 1))),
    "`edge_style` names arrows not in the graph: `M -> X` and `X -> Q`. The graph has `X -> M`",
    fixed = TRUE)
  expect_error(metro_dag(g, edge_style = list("X <-> Y" = list(width = 1))),
    "names a bidirected arc", fixed = TRUE)
  expect_error(metro_dag(g, edge_style = list("X -> " = list(width = 1))), "from->to", fixed = TRUE)
})

test_that("edge_style keys keep node names that have spaces or a trailing '<'", {
  layer <- function(p) Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)$geom_params$edges
  spaced <- data.frame(from = c("C ", "C ", "X"), to = c("X", "Y", "Y"))
  # The key names node "C " exactly, as it did before spaces were trimmed.
  p <- metro_dag(spaced, edge_style = list("C ->X" = list(width = 1)))
  e <- layer(p)
  expect_true(all(e$width[e$from == "C " & e$to == "X"] == 1))
  expect_true(all(e$width[e$from != "C " | e$to != "X"] == 0.45))
  expect_identical(attr(p, "metrodag_edge_style")$from, "C ")
  # Spaces are still dropped when the key does not name an arrow as written.
  q <- metro_dag(spaced, edge_style = list("X -> Y" = list(width = 1)))
  expect_identical(attr(q, "metrodag_edge_style")[c("from", "to")], data.frame(from = "X", to = "Y"))
  expect_error(metro_dag(spaced, edge_style = list("C ->Q" = list(width = 1))),
    "`edge_style` names an arrow not in the graph: `C  -> Q`.", fixed = TRUE)
  angled <- data.frame(from = c("C<", "C<", "X"), to = c("X", "Y", "Y"))
  r <- metro_dag(angled, edge_style = list("C<->X" = list(linetype = "dotted")))
  e <- layer(r)
  expect_true(all(e$linetype[e$from == "C<" & e$to == "X"] == "dotted"))
  # A key that names no arrow and reads as a bidirected arc is still refused.
  expect_error(metro_dag(angled, edge_style = list("X<->Y" = list(width = 1))),
    "`edge_style` key \"X<->Y\" names a bidirected arc", fixed = TRUE)
  skip_if_not_installed("dagitty")
  for (edges in list(spaced, angled)) {
    source <- edges$from[[1L]]
    h <- highlight_paths(metro_dag(edges, exposure = "X", outcome = "Y"), paths = "biasing")
    e <- layer(h)
    expect_true(all(e$colour[e$from == source] == "#C0392B"))
    expect_true(all(e$colour[e$from == "X"] != "#C0392B"))
    expect_setequal(paste(attr(h, "metrodag_edge_style")$from, attr(h, "metrodag_edge_style")$to),
      paste(source, c("X", "Y")))
  }
})

test_that("node names containing \"->\" work in edge_style keys and highlight_paths()", {
  styled <- function(p) {
    layer <- Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)
    unique(layer$geom_params$edges[c("from", "to", "linetype", "colour")])
  }
  q <- metro_dag(data.frame(from = c("a b", "c->d"), to = c("c->d", "e")),
    edge_style = list("a b->c->d" = list(linetype = "dotted"), "c->d->e" = list(colour = "red")))
  edges <- styled(q)
  expect_identical(edges$linetype[edges$from == "a b"], "dotted")
  expect_identical(edges$colour[edges$from == "c->d"], "red")
  expect_error(metro_dag(data.frame(from = c("a b", "c->d"), to = c("c->d", "e")),
    edge_style = list("a->b->c" = list(linetype = "dotted"))),
    "`edge_style` key \"a->b->c\" must name one arrow as \"from->to\".", fixed = TRUE)
  # A key that splits into two arrows of the graph names both.
  two <- data.frame(from = c("a", "a->b"), to = c("b->c", "c"))
  expect_error(metro_dag(two, edge_style = list("a->b->c" = list(colour = "red"))),
    paste("`edge_style` key \"a->b->c\" names 2 arrows: from \"a\" to \"b->c\" and from \"a->b\" to \"c\".",
      "Write it with spaces around the arrow it means, as \"a -> b->c\", or give a data frame"), fixed = TRUE)
  edges <- styled(metro_dag(two, edge_style = list("a->b -> c" = list(colour = "red"))))
  expect_identical(edges$colour[edges$from == "a->b"], "red")
  expect_false(identical(edges$colour[edges$from == "a"], "red"))
  # Names with " -> " inside, and with any other characters, stay distinct.
  spaced <- data.frame(from = c("a", "a -> b"), to = c("b -> c", "c"))
  edges <- styled(metro_dag(spaced, edge_style = data.frame(from = c("a", "a -> b"), to = c("b -> c", "c"),
    colour = c("red", "blue"))))
  expect_identical(edges$colour[order(edges$from)], c("red", "blue"))
  returns <- data.frame(from = c("a\rb", "a"), to = c("c", "b\rc"))
  edges <- styled(metro_dag(returns, edge_style = data.frame(from = "a", to = "b\rc", colour = "red")))
  expect_identical(edges$colour[edges$from == "a"], "red")
  expect_false(identical(edges$colour[edges$from == "a\rb"], "red"))
  skip_if_not_installed("dagitty")
  p <- metro_dag(data.frame(from = c("Z", "Z", "X"), to = c("X", "Y->Y", "Y->Y")),
    exposure = "X", outcome = "Y->Y")
  highlighted <- attr(highlight_paths(p, paths = "biasing"), "metrodag_edge_style")
  expect_identical(paste(highlighted$from, highlighted$to, sep = " | "), c("Z | X", "Z | Y->Y"))
  q <- metro_dag(data.frame(from = c("x", "a", "b -> c", "a -> b", "a -> b"), to = c("a", "b -> c", "c", "c", "a")),
    exposure = "a", outcome = "c")
  highlighted <- attr(highlight_paths(q, paths = "all"), "metrodag_edge_style")
  expect_setequal(paste(highlighted$from, highlighted$to, sep = " | "), c("a | b -> c", "b -> c | c", "a -> b | a", "a -> b | c"))
})

test_that("highlight_paths() names its own colour and linetype arguments", {
  skip_if_not_installed("dagitty")
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y", unobserved = "U", adjusted = "C")
  expect_error(highlight_paths(p, colour = c("red", "blue")), "`colour` must be one valid colour", fixed = TRUE)
  expect_error(highlight_paths(p, colour = "notacolour"), "`colour` must be one valid colour", fixed = TRUE)
  expect_error(highlight_paths(p, colour = 3), "`colour` must be one valid colour", fixed = TRUE)
  expect_error(highlight_paths(p, linetype = "wavy"), "`linetype` must be NULL or one of", fixed = TRUE)
  expect_error(highlight_paths(p, linetype = c("dotted", "dashed")), "`linetype` must be NULL or one of", fixed = TRUE)
  expect_s3_class(highlight_paths(p, colour = "red", linetype = "dashed"), "metrodag_plot")
})

test_that("edge_style messages name keys that are not arrows", {
  g <- .causal_plain()
  for (key in c("C=>Y", "X<-M", "X - > M", "X->M->Y", "->M"))
    expect_error(metro_dag(g, edge_style = stats::setNames(list(list(width = 1), list(width = 1)),
      c("X->M", key))), sprintf("`edge_style` key \"%s\" must name one arrow as \"from->to\".", key),
      fixed = TRUE)
  expect_error(metro_dag(g, edge_style = list("X->M" = list(width = 1), "C->Y" = "dotted")),
    "`edge_style` for \"C->Y\" must be a list of linetype, colour and/or width", fixed = TRUE)
  expect_error(metro_dag(g, edge_style = stats::setNames(list(list(width = 1), list(width = 1)), c("X->M", ""))),
    "Every `edge_style` entry needs a key naming one arrow", fixed = TRUE)
  expect_error(metro_dag(g, edge_style = list(list(width = 1))),
    "`edge_style` must be a named list keyed \"from->to\"", fixed = TRUE)
})

test_that("metro_dag() reports unknown arguments as its own", {
  g <- metrodag_examples("causal")
  fixed <- .causal_plain()
  expect_error(metro_dag(g, seed = 1), "Unknown argument to metro_dag(): `seed`.", fixed = TRUE)
  expect_error(metro_dag(g, seed = 1, nodesize = 2),
    "Unknown arguments to metro_dag(): `seed`, `nodesize`.", fixed = TRUE)
  expect_error(metro_dag(g, layout = fixed), "Unknown argument to metro_dag(): `layout`.",
    fixed = TRUE)
  hint <- "Set the title with `estimand`, or add ggplot2::labs(title = ...) to the plot."
  expect_error(metro_dag(g, title = "My DAG"), paste("Unknown argument to metro_dag(): `title`.",
    "Text options in `...` are fontface, lineheight and colour.", hint), fixed = TRUE)
  expect_error(plot_metro_dag(fixed, title = "My DAG"), hint, fixed = TRUE)
  expect_error(plot(fixed, subtitle = "x"), "ggplot2::labs(subtitle = ...)", fixed = TRUE)
  expect_error(metro_dag(g, caption = "x"), "ggplot2::labs(caption = ...)", fixed = TRUE)
  # A title given with other unknown names is listed with them.
  expect_error(metro_dag(g, title = "x", seed = 1), "Unknown arguments to metro_dag(): `title`, `seed`.",
    fixed = TRUE)
  expect_error(plot_metro_dag(fixed, seed = 1, title = "x"),
    "Unknown arguments to plot_metro_dag(): `seed`, `title`.", fixed = TRUE)
  # Comparisons explain where their panel titles come from.
  views <- list(A = list(exposure = "X", outcome = "Y"))
  panel <- paste("Panel titles are the names of `views` or each view's `estimand`.",
    "To change one panel, add ggplot2::labs(title = ...) to it in the result's `plots`.")
  expect_error(compare_metro_dags(g, views, title = "My DAG"),
    paste("Unknown argument to compare_metro_dags(): `title`.", panel), fixed = TRUE)
  wrapper <- function(...) compare_metro_dags(g, views, ...)
  expect_error(wrapper(caption = "x"), "add ggplot2::labs(caption = ...) to it in the result's `plots`",
    fixed = TRUE)
  expect_error(compare_metro_dags(g, views, seed = 1), "Unknown argument to compare_metro_dags(): `seed`.",
    fixed = TRUE)
  # Appearance and text options still reach plot_metro_dag().
  p <- metro_dag(fixed, style = "monochrome", node_size = 4, fontface = "bold", color = "grey20")
  config <- Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)$geom_params$config
  expect_identical(config$node_size, 4)
  expect_identical(config$text_options$fontface, "bold")
  expect_identical(config$text_options$colour, "grey20")
})

test_that("an edge_style key with \"->\" in a node name reports an absent or reversed arrow", {
  g <- data.frame(from = c("a", "c->d"), to = c("c->d", "e"))
  expect_error(metro_dag(g, edge_style = list("c->d->a" = list(colour = "red"))),
    "`edge_style` names an arrow not in the graph: `c->d -> a`. The graph has `a -> c->d`", fixed = TRUE)
  expect_error(metro_dag(g, edge_style = list("e->c->d" = list(colour = "red"))),
    "`edge_style` names an arrow not in the graph: `e -> c->d`.", fixed = TRUE)
})

test_that("edge_style keys match names marked with another encoding, and alike arrows are told apart", {
  cafe <- "Café"
  latin <- iconv(cafe, "UTF-8", "latin1")
  expect_identical(Encoding(latin), "latin1")
  styled_colour <- function(p) {
    layer <- Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)
    edges <- layer$geom_params$edges
    unique(edges$colour[edges$from == cafe])
  }
  graph <- data.frame(from = c(latin, "Z"), to = c("X", "X"))
  expect_identical(styled_colour(metro_dag(graph, edge_style = stats::setNames(list(list(colour = "red")),
    paste0(cafe, "->X")))), "red")
  graph <- data.frame(from = c(cafe, "Z"), to = c("X", "X"))
  expect_identical(styled_colour(metro_dag(graph, edge_style = data.frame(from = latin, to = "X", colour = "red"))), "red")
  # Names holding " -> " make spaced keys ambiguous; only a data frame helps.
  spaced <- data.frame(from = c("a", "a -> b"), to = c("b -> c", "c"))
  expect_error(metro_dag(spaced, edge_style = list("a -> b -> c" = list(colour = "red"))),
    paste("`edge_style` key \"a -> b -> c\" names 2 arrows: from \"a\" to \"b -> c\" and from \"a -> b\" to \"c\".",
      "Give a data frame with `from` and `to` columns."), fixed = TRUE)
})

test_that("highlight_paths(colour = NULL) keeps each arrow's colour", {
  skip_if_not_installed("dagitty")
  p <- metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y", unobserved = "U",
    edge_style = list("C->X" = list(colour = "purple")))
  h <- highlight_paths(p, paths = "biasing", colour = NULL, width = 1.5)
  layer <- Find(function(l) inherits(l$geom, "GeomMetroDag"), h$layers)
  edges <- unique(layer$geom_params$edges[c("from", "to", "colour", "width")])
  expect_identical(edges$colour[edges$from == "C" & edges$to == "X"], "purple")
  expect_equal(edges$width[edges$from == "C" & edges$to == "X"], 1.5)
  expect_error(highlight_paths(p, colour = NA), "or NULL to keep each arrow's colour", fixed = TRUE)
})
