test_that("node and edge tables preserve isolated variables and all metadata", {
  x <- .compat_tables(); original <- x
  g <- metro_graph(x$nodes, x$edges)
  reference <- .compat_reference()
  expect_identical(igraph::vertex_attr(g), igraph::vertex_attr(reference))
  expect_identical(igraph::edge_attr(g), igraph::edge_attr(reference))
  expect_identical(igraph::as_edgelist(g), igraph::as_edgelist(reference))
  reordered <- metro_graph(x$nodes[, c("cohort", "role", "label", "name")],
    x$edges[, c("weight", "to", "from")])
  expect_identical(igraph::as_edgelist(reordered), igraph::as_edgelist(reference))
  expect_identical(igraph::vertex_attr(reordered)[names(igraph::vertex_attr(reference))],
    igraph::vertex_attr(reference))
  expect_identical(igraph::edge_attr(reordered), igraph::edge_attr(reference))
  expect_equal(igraph::degree(g, "I"), c(I = 0))
  expect_identical(x, original)
  expect_equal(igraph::ecount(metro_graph(x$nodes)), 0L)
  expect_equal(igraph::vcount(metro_graph(x$nodes)), nrow(x$nodes))
  for (missing in list(NA, NA_character_, NA_integer_, NA_real_)) {
    unspecified <- metro_graph(data.frame(name = "I", role = missing))
    p <- metro_dag(unspecified)
    expect_equal(p$layers[[2L]]$geom_params$nodes$role, "observed")
    expect_length(attr(p, "metrodag_analysis")$adjusted, 0L)
  }
  numeric <- metro_graph(data.frame(name = c(20, 10, 30)),
    data.frame(from = factor("10"), to = 30))
  expect_equal(igraph::V(numeric)$name, c("20", "10", "30"))
  expect_equal(unname(igraph::as_edgelist(numeric)), matrix(c("10", "30"), ncol = 2))
  literal <- c('quoted "node"', "node -> label", "Été\nvariable")
  g <- metro_graph(data.frame(name = literal), data.frame(from = literal[1L], to = literal[2L]))
  expect_identical(igraph::V(g)$name, literal)
  expect_null(igraph::vertex_attr(g, "role"))
})

test_that("table validation never invents nodes, collapses edges or changes last plot", {
  nodes <- data.frame(name = c("X", "Y")); edge <- data.frame(from = "X", to = "Y")
  previous <- ggplot2::ggplot(); ggplot2::set_last_plot(previous)
  for (bad in list(NULL, "X", data.frame(id = "X"), data.frame(name = character()),
      data.frame(name = c("X", "X")), data.frame(name = c("X", NA)), data.frame(name = ""),
      data.frame(name = I(list("X"))), data.frame(name = "X", role = "selection"),
      data.frame(name = "X", label = I(list("label"))),
      data.frame(name = "X", role = I(matrix(c("observed", "exposure"), nrow = 1)))))
    expect_error(metro_graph(bad, edge), "nodes")
  for (bad in list(data.frame(a = "X", to = "Y"), data.frame(from = NA, to = "Y"),
      data.frame(from = "", to = "Y"), data.frame(from = "Z", to = "Y"),
      data.frame(from = I(list("X")), to = "Y")))
    expect_error(metro_graph(nodes, bad), "edge")
  duplicated <- nodes; duplicated$extra <- 1; names(duplicated) <- c("name", "name")
  expect_error(metro_graph(duplicated, edge), "unique column")
  expect_error(metro_graph(nodes, rbind(edge, edge)), "repeats the arrow X -> Y")
  expect_error(metro_graph(nodes, data.frame(from = c("X", "Y"), to = c("Y", "X"))), "cycle")
  expect_error(metro_graph(nodes, data.frame(from = "X", to = "X")), "self-loop at `X`")
  expect_identical(ggplot2::last_plot(), previous)
})

test_that("igraph role attributes use the role names metro_graph() accepts", {
  g <- igraph::graph_from_data_frame(data.frame(from = c("C", "C", "X"), to = c("X", "Y", "Y")))
  with_roles <- function(graph, role) igraph::set_vertex_attr(graph, "role",
    value = unname(role[igraph::V(graph)$name]))
  bad <- with_roles(g, c(C = "observed", X = "Exposure", Y = "outcom"))
  original <- bad
  accepted <- "must use observed, exposure, outcome, adjusted, unobserved or selected (or NA)"
  expect_error(metro_graph(data.frame(name = c("C", "X", "Y"), role = c("observed", "Exposure", "outcom"))),
    accepted, fixed = TRUE)
  message <- paste0("The graph's `role` vertex attribute ", accepted,
    ", but node `X` has \"Exposure\" and node `Y` has \"outcom\". Role names are lower case.")
  expect_error(metro_dag(bad), message, fixed = TRUE)
  expect_identical(bad, original)
  # A layout ignores roles; drawing it reads them, in every view.
  expect_s3_class(layout <- layout_metro_dag(bad), "metrodag_layout")
  expect_error(plot_metro_dag(layout), message, fixed = TRUE)
  expect_error(compare_metro_dags(layout, list(A = list(exposure = "X"))), message, fixed = TRUE)
  draw <- function(role) {
    layout$graph <- with_roles(layout$graph, role)
    plot_metro_dag(layout)
  }
  # Factors are read by their labels; no value is taken for a role it resembles.
  expect_error(draw(factor(c(C = "observed", X = "Exposure", Y = "outcom"))), message, fixed = TRUE)
  expect_error(draw(c(C = "Outcome", X = "exposure", Y = "Outcome")),
    "but nodes `C` and `Y` have \"Outcome\". Role names are lower case.", fixed = TRUE)
  expect_error(draw(c(C = "latent", X = "exposure", Y = "outcome")),
    "but node `C` has \"latent\". Use \"unobserved\" for latent variables.", fixed = TRUE)
  # Up to six values are named; more are counted.
  expect_error(.metro_check_role_attribute(letters[1:6], LETTERS[1:6]),
    "node `E` has \"e\" and node `F` has \"f\".", fixed = TRUE)
  expect_error(.metro_check_role_attribute(letters[1:8], LETTERS[1:8]),
    "node `E` has \"e\" and 3 more values.", fixed = TRUE)
  # Supplying every role reads nothing from the attribute; one inherited role reads it.
  for (p in list(metro_dag(layout, exposure = "X", outcome = "Y", adjusted = character(0),
      unobserved = character(0), selected = character(0)),
    metro_dag(layout, exposure = "X", outcome = "Y", unadjusted = TRUE,
      unobserved = character(0), selected = character(0)))) {
    drawn <- p$layers[[2L]]$geom_params$nodes
    expect_identical(drawn$role[match(c("C", "X", "Y"), drawn$node)], c("observed", "exposure", "outcome"))
  }
  expect_error(metro_dag(layout, exposure = "X", outcome = "Y", unobserved = character(0),
    selected = character(0)), message, fixed = TRUE)
  # Valid roles are inherited, and NA is observed, in a list attribute too.
  p <- draw(c(C = NA, X = "exposure", Y = "outcome"))
  drawn <- p$layers[[2L]]$geom_params$nodes
  expect_identical(drawn$role[match(c("C", "X", "Y"), drawn$node)], c("observed", "exposure", "outcome"))
  expect_identical(attr(draw(list(C = NA, X = "exposure", Y = "outcome")), "metrodag_analysis"),
    attr(p, "metrodag_analysis"))
  skip_if_not_installed("dagitty")
  expect_error(check_metro_adjustment(bad, exposure = "X", outcome = "Y", unadjusted = TRUE),
    message, fixed = TRUE)
})

test_that("named adjacency matrices preserve direction under independent permutations", {
  a <- matrix(0L, 3, 3, dimnames = list(c("X", "Y", "I"), c("X", "Y", "I")))
  a["X", "Y"] <- 1L; original <- a
  for (matrix in list(a, a != 0, a[c(3, 1, 2), c(2, 3, 1)], a[, 3:1])) {
    g <- metro_graph_from_adjacency(matrix)
    expect_setequal(igraph::V(g)$name, c("X", "Y", "I"))
    expect_equal(unname(igraph::as_edgelist(g)), matrix(c("X", "Y"), ncol = 2))
    expect_equal(igraph::degree(g, "I"), c(I = 0))
  }
  expect_identical(a, original)
  expect_equal(igraph::vcount(metro_graph_from_adjacency(a * 0)), 3L)
  singleton <- matrix(0, 1, 1, dimnames = list("I", "I"))
  expect_equal(igraph::vcount(metro_graph_from_adjacency(singleton)), 1L)
  expect_equal(igraph::ecount(metro_graph_from_adjacency(singleton)), 0L)
  # Direct matrices retain their existing edge-list interpretation.
  edges <- rbind(c(1, 2), c(2, 3))
  expect_equal(unname(igraph::as_edgelist(.as_metro_input(edges))),
    matrix(c("1", "2", "2", "3"), ncol = 2, byrow = TRUE))
  expect_error(metro_graph_from_adjacency(edges), "only 0 and 1")
})

test_that("invalid adjacency inputs fail explicitly without inferring a representation", {
  a <- matrix(0, 2, 2, dimnames = list(c("X", "Y"), c("X", "Y")))
  for (bad in list(NULL, 1, data.frame(a), matrix(numeric(), 0, 0), matrix(0, 2, 3),
      a + 2, a + Inf, a + NA_real_, a + 1i, matrix("0", 2, 2)))
    expect_error(metro_graph_from_adjacency(bad), "square numeric or logical")
  expect_error(metro_graph_from_adjacency(unname(a)), "row and column names")
  for (names in list(c("X", "X"), c("X", "Z"), c("X", NA), c("X", ""))) {
    bad <- a; colnames(bad) <- names
    expect_error(metro_graph_from_adjacency(bad), "names")
  }
  cycle <- a; cycle[1, 2] <- cycle[2, 1] <- 1
  expect_error(metro_graph_from_adjacency(cycle), "cycle")
  diag(a) <- 1
  expect_error(metro_graph_from_adjacency(a), "self-loops at `X` and `Y`")
})

test_that("equivalent explicit inputs keep geometry, roles and labels in every direction", {
  x <- .compat_tables(); g <- metro_graph(x$nodes, x$edges)
  shuffled <- metro_graph(x$nodes[5:1, ], x$edges[5:1, ])
  a <- matrix(0, 5, 5, dimnames = list(x$nodes$name, x$nodes$name))
  a[cbind(match(x$edges$from, rownames(a)), match(x$edges$to, colnames(a)))] <- 1
  adjacent <- metro_graph_from_adjacency(a[5:1, c(2, 4, 1, 5, 3)])
  # Each input is laid out once and turned to every direction (test-layout.R
  # checks the rotation); stations and labels are placed per direction.
  layouts <- lapply(list(reference = .compat_reference(), g = g, shuffled = shuffled,
    adjacent = adjacent), .test_layouts_by_direction)
  for (direction in c("down", "up", "right", "left")) {
    reference <- metro_dag(layouts$reference[[direction]])
    for (input in layouts[c("g", "shuffled")]) {
      p <- metro_dag(input[[direction]])
      expect_identical(.compat_display(p), .compat_display(reference))
      expect_equal(.compat_routes(attr(p, "metrodag_layout")), .compat_routes(attr(reference, "metrodag_layout")))
    }
    p <- metro_dag(layouts$adjacent[[direction]], exposure = "X", outcome = "Y",
      adjusted = "C", unobserved = "U", labels = stats::setNames(x$nodes$label, x$nodes$name))
    expect_identical(.compat_display(p), .compat_display(reference))
    expect_equal(.compat_routes(attr(p, "metrodag_layout")), .compat_routes(attr(reference, "metrodag_layout")))
  }
})

test_that("table and adjacency exports keep every causal arrow and isolated station", {
  skip_if_not_installed("svglite")
  tables <- .compat_tables()
  graph <- metro_graph(tables$nodes, tables$edges)
  a <- matrix(0, 5, 5, dimnames = list(tables$nodes$name, tables$nodes$name))
  a[cbind(match(tables$edges$from, rownames(a)), match(tables$edges$to, colnames(a)))] <- 1
  for (input in list(graph, metro_graph_from_adjacency(a[, 5:1]))) {
    p <- metro_dag(input, exposure = "X", outcome = "Y", adjusted = "C", unobserved = "U",
      labels = stats::setNames(tables$nodes$label, tables$nodes$name))
    path <- tempfile(fileext = ".svg"); on.exit(unlink(path), add = TRUE)
    expect_no_warning(save_metro_dag(path, p, width = 180))
    svg <- readLines(path)
    expect_equal(.svg_arrowheads(path), 5L)
    expect_true(any(grepl("Isolated", svg, fixed = TRUE)))
    expect_true(any(grepl("variable", svg, fixed = TRUE)))
    expect_equal(igraph::degree(attr(p, "metrodag_layout")$graph, "I"), c(I = 0))
  }
})

test_that("whole-number identifiers name nodes in full in every input and role argument", {
  ids <- c("100000", "200000", "300000")
  edges <- data.frame(from = c(100000, 200000), to = c(200000, 300000))
  g <- igraph::make_ring(3, directed = TRUE, circular = FALSE)
  igraph::V(g)$name <- c(1e5, 2e5, 3e5)
  inputs <- list(edges, as.matrix(edges), unname(as.matrix(edges)), g,
    metro_graph(data.frame(name = c(1e5, 2e5, 3e5)), edges))
  for (input in inputs) expect_identical(igraph::V(.as_metro_input(input))$name, ids)
  p <- metro_dag(edges, exposure = 100000, outcome = 3e5, adjusted = "200000",
    edge_style = data.frame(from = 1e5, to = 2e5, colour = "red"))
  expect_identical(attr(p, "metrodag_analysis")[c("exposure", "outcome", "adjusted")],
    list(exposure = "100000", outcome = "300000", adjusted = "200000"))
  params <- Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)$geom_params
  expect_identical(params$nodes$label, ids)
  expect_identical(unique(params$edges$colour[params$edges$from == "100000"]), "red")
  if (requireNamespace("dagitty", quietly = TRUE))
    expect_identical(check_metro_adjustment(edges, exposure = 1e5, outcome = 300000,
      unadjusted = TRUE)$exposure, "100000")
  # Other values keep their as.character() text; -0 is "0", as it always was.
  expect_identical(.metro_node_names(c(1e5, -1e5, 123456, 0.5, -0, 2^53, NA)),
    c("100000", "-100000", "123456", "0.5", "0", as.character(2^53), NA))
  expect_identical(.metro_node_names(c(100000L, NA)), c("100000", NA))
  expect_identical(.metro_node_names(as.Date("2024-01-31")), "2024-01-31")
})

test_that("names that differ only by white space at their ends stay apart, with one warning", {
  warned <- function(expr) {
    found <- character()
    withCallingHandlers(expr, warning = function(w) {
      found <<- c(found, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    found
  }
  # One arrow spells Smoking with a trailing space, so Age seems not to confound it.
  edges <- data.frame(from = c("Age", "Age", "Smoking"), to = c("Smoking ", "Cancer", "Cancer"))
  message <- paste("Node names \"Smoking\" and \"Smoking \" differ only by white space at their ends,",
    "so they are separate nodes. Remove the white space if they name the same variable.")
  expect_identical(warned(layout <- layout_metro_dag(edges)), message)
  expect_identical(igraph::V(layout$graph)$name, c("Age", "Smoking", "Smoking ", "Cancer"))
  expect_identical(warned(metro_dag(edges, exposure = "Smoking", outcome = "Cancer")), message)
  expect_identical(warned(compare_metro_dags(edges, list(A = list(exposure = "Smoking")))), message)
  expect_identical(warned(metro_graph(data.frame(name = c("Smoking", "Smoking ")))), message)
  if (requireNamespace("dagitty", quietly = TRUE))
    expect_identical(warned(check_metro_adjustment(edges, exposure = "Smoking", outcome = "Cancer",
      unadjusted = TRUE)), message)
  # The wording of the warning does not need a layout.
  several <- data.frame(from = c("A ", " B", "C", "D\t", "E", "F "),
    to = c("A", "B", "C ", "D", "E\n", "F"))
  expect_identical(warned(.as_metro_input(several)), paste("Node names differ only by white space",
    "at their ends, so these are separate nodes: \"A \" and \"A\"; \" B\" and \"B\"; \"C\" and \"C \";",
    "\"D\\t\" and \"D\"; \"E\" and \"E\\n\"; and 1 more. Remove the white space where they name the",
    "same variable."))
  expect_identical(warned(.as_metro_input(data.frame(from = c("A ", " A"), to = "A"))), paste(
    "Node names \"A \", \" A\" and \"A\" differ only by white space at their ends, so they are",
    "separate nodes. Remove the white space if they name the same variable."))
  expect_no_warning(.as_metro_input(data.frame(from = c("smoking", "Smoking status"),
    to = c("Smoking", "Smoking  status"))))
})

test_that("a name declared in latin1 or in UTF-8 gives the same layout", {
  utf8 <- "\u00e9tude"
  latin <- iconv(utf8, "UTF-8", "latin1")
  expect_identical(Encoding(latin), "latin1")
  # Layout ties between the children of X are broken by name, in byte order.
  edges <- function(name) data.frame(from = c("X", "X", "X", name), to = c(name, "\u00fcber", "Y", "Z"))
  reference <- layout_metro_dag(edges(utf8))
  for (input in list(edges(latin), as.matrix(edges(latin)),
      igraph::graph_from_data_frame(edges(latin)))) {
    layout <- layout_metro_dag(input)
    expect_identical(layout$nodes, reference$nodes)
    expect_identical(layout$edges, reference$edges)
    expect_identical(Encoding(igraph::V(layout$graph)$name), Encoding(igraph::V(reference$graph)$name))
  }
})
