arrows <- function(graph) igraph::as_ids(igraph::E(graph))

test_that("edge matrices read `from` and `to` columns by name", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  reversed <- as.matrix(edges[, c("to", "from")])
  expect_identical(colnames(reversed), c("to", "from"))
  expect_identical(arrows(.as_metro_input(reversed)), c("A|B", "B|C"))
  expect_identical(arrows(layout_metro_dag(reversed)$graph), c("A|B", "B|C"))
  expect_identical(arrows(.as_metro_input(cbind(to = c(2, 3), from = c(1, 2)))), c("1|2", "2|3"))
  # Extra named columns are ignored, as they are in an edge data frame.
  expect_identical(arrows(.as_metro_input(as.matrix(cbind(weight = 1:2, edges[, 2:1])))),
    c("A|B", "B|C"))
  # Without both names the columns are positional, as before.
  expect_identical(arrows(.as_metro_input(cbind(c("A", "B"), c("B", "C")))), c("A|B", "B|C"))
  expect_identical(arrows(.as_metro_input(cbind(x = c("A", "B"), y = c("B", "C")))), c("A|B", "B|C"))
  # A lone `from` first or `to` second agrees with the positions.
  expect_identical(arrows(.as_metro_input(cbind(from = c("A", "B"), c("B", "C")))), c("A|B", "B|C"))
  expect_identical(arrows(layout_metro_dag(cbind(exposure = c("A", "B"), to = c("B", "C")))$graph),
    c("A|B", "B|C"))
  # A lone `to` first or `from` second would be read against its name.
  expect_error(.as_metro_input(cbind(to = c("B", "C"), x = c("A", "B"))),
    "is read by position, with the source of each arrow first, but this one names its first column `to`.",
    fixed = TRUE)
  expect_error(metro_dag(cbind(x = c("B", "C"), from = c("A", "B"))),
    "names its second column `from`. Name the columns `from` and `to`, or put the sources first.",
    fixed = TRUE)
  expect_error(.as_metro_input(cbind(a = "A", b = "B", c = "C")), "two columns")
  # Missing and empty endpoints name their rows, whatever the other columns.
  expect_error(metro_dag(cbind(from = c(1, NA), to = c(2, 3), weight = c(0.5, 0.2))),
    "Edge endpoints cannot be missing; see row 2 of `graph`.", fixed = TRUE)
  expect_error(metro_dag(cbind(c("A", NA), c("B", "C"))),
    "Edge endpoints cannot be missing; see row 2 of `graph`.", fixed = TRUE)
  expect_error(metro_dag(cbind(from = c("A", ""), to = c("B", "C"))),
    "Edge endpoints cannot be empty; see row 2 of `graph`.", fixed = TRUE)
  expect_error(metro_dag(data.frame(from = c("A", "B"), to = c("", "C"))),
    "Edge endpoints cannot be empty; see row 1 of `graph`.", fixed = TRUE)
  expect_error(metro_dag(data.frame(from = addNA(factor(c("A", "B", NA))), to = c("B", "C", "D"))),
    "Edge endpoints cannot be missing; see row 3 of `graph`.", fixed = TRUE)
  nodes <- data.frame(name = c("A", "B", "C"))
  expect_error(metro_graph(nodes, data.frame(from = c("A", NA), to = c("B", "C"))),
    "Edge endpoints cannot be missing; see row 2 of `edges`.", fixed = TRUE)
  expect_error(metro_graph(nodes, data.frame(from = c("A", "B"), to = c("B", ""))),
    "Edge endpoints cannot be empty; see row 2 of `edges`.", fixed = TRUE)
  expect_error(metro_dag(matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))),
    "For an adjacency matrix, use metro_graph_from_adjacency().", fixed = TRUE)
})

test_that("self-loops are named before repeated arrows and cycles", {
  loop <- data.frame(from = c("A", "B", "B", "C"), to = c("B", "B", "C", "A"))
  expected <- "`graph` has a self-loop at `B`; a DAG cannot contain one."
  expect_error(metro_dag(loop), expected, fixed = TRUE)
  expect_error(layout_metro_dag(loop), expected, fixed = TRUE)
  expect_error(compare_metro_dags(loop, list(A = list())), expected, fixed = TRUE)
  expect_error(metro_dag(as.matrix(loop)), expected, fixed = TRUE)
  expect_error(metro_dag(igraph::graph_from_data_frame(loop)), expected, fixed = TRUE)
  expect_error(metro_dag(data.frame(from = c("A", "B", "A"), to = c("A", "B", "C"))),
    "`graph` has self-loops at `A` and `B`; a DAG cannot contain them.", fixed = TRUE)
  # With a repeated arrow as well, the self-loop is named first.
  both <- rbind(loop, data.frame(from = "A", to = "B"))
  expect_true(anyDuplicated(both) > 0L)
  expect_error(metro_dag(both), expected, fixed = TRUE)
  expect_error(metro_graph(data.frame(name = c("A", "B", "C")), both), "`edges` has a self-loop at `B`",
    fixed = TRUE)
  expect_error(metro_graph(data.frame(name = c("A", "B")), data.frame(from = "B", to = "B")),
    "`edges` has a self-loop at `B`", fixed = TRUE)
  adjacency <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  adjacency["B", "B"] <- 1
  expect_error(metro_graph_from_adjacency(adjacency), "`adjacency` has a self-loop at `B`", fixed = TRUE)
  skip_if_not_installed("dagitty")
  expect_error(check_metro_adjustment(loop, exposure = "A", outcome = "C", unadjusted = TRUE),
    expected, fixed = TRUE)
})

test_that("repeated arrows are named before cycles", {
  repeated <- data.frame(from = c(LETTERS[1:11], "G"), to = c(LETTERS[2:12], "H"))
  expected <- "`graph` repeats the arrow G -> H; supply each arrow once."
  expect_error(metro_dag(repeated), expected, fixed = TRUE)
  expect_error(layout_metro_dag(as.matrix(repeated)), expected, fixed = TRUE)
  expect_error(compare_metro_dags(repeated, list(A = list())), expected, fixed = TRUE)
  expect_error(metro_dag(rbind(repeated, data.frame(from = c("L", "B"), to = c("A", "C")))),
    "`graph` repeats the arrows G -> H and B -> C;", fixed = TRUE)
  expect_error(metro_graph(data.frame(name = c("A", "B")), data.frame(from = c("A", "A"), to = c("B", "B"))),
    "`edges` repeats the arrow A -> B", fixed = TRUE)
  skip_if_not_installed("tidygraph")
  expect_error(metro_dag(tidygraph::as_tbl_graph(igraph::graph_from_data_frame(repeated))),
    expected, fixed = TRUE)
})

test_that("cycles are shown as a path with the arrows that break them", {
  cyclic <- data.frame(from = c(LETTERS[1:11], "K"), to = c(LETTERS[2:12], "C"))
  expected <- paste("`graph` contains the directed cycle C -> D -> E -> F -> G -> H -> I -> J -> K -> C;",
    "MetroDAG only lays out DAGs. Removing K -> C would make it acyclic.")
  expect_error(metro_dag(cyclic), expected, fixed = TRUE)
  expect_error(layout_metro_dag(cyclic), expected, fixed = TRUE)
  expect_error(compare_metro_dags(cyclic, list(A = list())), expected, fixed = TRUE)
  expect_error(metro_dag(as.matrix(cyclic)), expected, fixed = TRUE)
  expect_error(metro_graph(data.frame(name = LETTERS[1:12]), cyclic),
    "`edges` contains the directed cycle C -> D", fixed = TRUE)
  adjacency <- matrix(0, 2, 2, dimnames = list(c("X", "Y"), c("X", "Y")))
  adjacency["X", "Y"] <- adjacency["Y", "X"] <- 1
  expect_error(metro_graph_from_adjacency(adjacency),
    "`adjacency` contains the directed cycle X -> Y -> X;", fixed = TRUE)
  # A long cycle is shortened, and many breaking arrows are counted.
  ring <- data.frame(from = paste0("N", 1:30), to = paste0("N", c(2:30, 1)))
  expect_error(metro_dag(ring), "N1 -> N2 -> N3 -> N4 -> N5 -> ... -> N28 -> N29 -> N30 -> N1;",
    fixed = TRUE)
  pairs <- data.frame(from = c(letters[1:7], LETTERS[1:7]), to = c(LETTERS[1:7], letters[1:7]))
  message <- tryCatch(metro_dag(pairs), error = conditionMessage)
  expect_match(message, "Removing [^ ]+ -> [^ ]+, .* and 2 more would make it acyclic.")
  # Only arrows on a cycle are listed, and none that the others make redundant.
  joined <- data.frame(from = c("A", "C", "A", "E", "B", "C", "D"), to = c("E", "A", "B", "B", "E", "D", "C"))
  message <- tryCatch(metro_dag(joined), error = conditionMessage)
  listed <- regmatches(message, regexpr("Removing .* would", message))
  expect_false(grepl("C -> A", listed, fixed = TRUE))
  expect_match(listed, "B -> E|E -> B")
  expect_match(listed, "C -> D|D -> C")
  shared <- data.frame(from = c("X", "Z", "Z", "X", "M", "Y"), to = c("Y", "X", "Y", "M", "Y", "Z"))
  expect_error(metro_dag(shared), "Removing Y -> Z would make it acyclic.", fixed = TRUE)
  expect_error(metro_dag(as.matrix(shared)), "Removing Y -> Z would make it acyclic.", fixed = TRUE)
  # The heuristic's own set here also holds A -> B, which the others make redundant.
  six <- data.frame(from = c("A", "A", "B", "B", "C", "D", "E", "E", "F", "F"),
    to = c("B", "C", "C", "D", "F", "A", "A", "C", "D", "E"))
  expect_error(metro_dag(six), "Removing D -> A and F -> E would make it acyclic.", fixed = TRUE)
  # One arrow that breaks every cycle of a large part is named alone, though
  # the heuristic gives two (p1 -> v and p2 -> v).
  y <- paste0("y", 1:600)
  large <- rbind(data.frame(from = c("a", "a", "p1", "p2"), to = c("p1", "p2", "v", "v")),
    data.frame(from = "v", to = y), data.frame(from = y, to = "b"), data.frame(from = "b", to = "a"))
  expect_error(layout_metro_dag(large), "Removing b -> a would make it acyclic.", fixed = TRUE)
  # Redundant arrows are dropped however many the heuristic gives (120 here).
  copies <- do.call(rbind, lapply(1:40, function(i) data.frame(from = paste0(six$from, i), to = paste0(six$to, i))))
  expect_error(layout_metro_dag(copies),
    "Removing D1 -> A1, F1 -> E1, D2 -> A2, F2 -> E2, D3 -> A3 and 75 more would make it acyclic.", fixed = TRUE)
  # Different arrows that read alike are each listed.
  alike <- data.frame(from = c("B -> C", "A", "C", "A -> B"), to = c("A", "B -> C", "A -> B", "C"))
  expect_error(metro_dag(alike), "Removing A -> B -> C and A -> B -> C would make it acyclic.", fixed = TRUE)
  skip_if_not_installed("tidygraph")
  expect_error(metro_dag(tidygraph::as_tbl_graph(igraph::make_ring(3, directed = TRUE))),
    "`graph` contains the directed cycle 1 -> 2 -> 3 -> 1;", fixed = TRUE)
  skip_if_not_installed("dagitty")
  expect_error(metro_dag(dagitty::dagitty("dag { A -> B; B -> C; C -> A }")),
    "contains the directed cycle", fixed = TRUE)
  expect_error(check_metro_adjustment(cyclic, exposure = "A", outcome = "L", unadjusted = TRUE),
    expected, fixed = TRUE)
})

test_that("cycle messages ignore a `weight` edge attribute", {
  # Path coefficients kept as `weight` must not reach igraph's weighted
  # algorithms, which reject missing values and negative cycles.
  nodes <- data.frame(name = c("A", "B", "C"))
  arrows <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
  expected <- paste("contains the directed cycle A -> B -> C -> A; MetroDAG only lays out DAGs.",
    "Removing C -> A would make it acyclic.")
  for (weight in list(c(-0.3, -0.5, -0.2), c(1, NA, 2), c(1, Inf, -2), c(1, 5, 9),
                      c("a", "b", "c"))) {
    weighted <- transform(arrows, weight = weight)
    expect_error(metro_graph(nodes, weighted), paste("`edges`", expected), fixed = TRUE)
    expect_error(metro_dag(weighted), paste("`graph`", expected), fixed = TRUE)
    expect_error(layout_metro_dag(igraph::graph_from_data_frame(weighted)),
      paste("`graph`", expected), fixed = TRUE)
  }
  skip_if_not_installed("tidygraph")
  expect_error(metro_dag(tidygraph::tbl_graph(nodes,
    data.frame(from = 1:3, to = c(2, 3, 1), weight = c(0.4, -0.2, -0.5)))),
    paste("`graph`", expected), fixed = TRUE)
})

test_that("unsupported graph inputs list what each entry point accepts", {
  base <- paste("Supply an igraph or tidygraph graph, a dagitty or ggdag object,",
    "a two-column edge matrix, a data frame with `from` and `to` columns")
  expect_error(metro_dag(NULL), paste0("`graph` is NULL. ", base, ", or a metrodag_layout."), fixed = TRUE)
  expect_error(compare_metro_dags(NULL, list(A = list())),
    paste0("`graph` is NULL. ", base, ", or a metrodag_layout."), fixed = TRUE)
  expect_error(layout_metro_dag(NULL), paste("`graph` is NULL. Supply an igraph or tidygraph graph,",
    "a dagitty or ggdag object, a two-column edge matrix, or a data frame with `from` and `to` columns."),
    fixed = TRUE)
  dagitty_hint <- "It looks like DAGitty code: pass dagitty::dagitty(text) first."
  for (text in c("dag { A -> B }", "A -> B", "  DAG {}")) {
    expect_error(metro_dag(text), paste0("`graph` is a character string. ", base), fixed = TRUE)
    expect_error(metro_dag(text), dagitty_hint, fixed = TRUE)
    expect_error(layout_metro_dag(text), dagitty_hint, fixed = TRUE)
  }
  expect_false(grepl("DAGitty", tryCatch(metro_dag(c("A", "B")), error = conditionMessage)))
  list_hint <- "To use its `from` and `to` elements, pass data.frame(from = ..., to = ...)."
  expect_error(metro_dag(list(from = "A", to = "B")), "`graph` is a list.", fixed = TRUE)
  expect_error(metro_dag(list(from = "A", to = "B")), list_hint, fixed = TRUE)
  expect_error(metro_dag(y ~ x), "`graph` is a formula.", fixed = TRUE)
  expect_error(metro_dag(1:3), "`graph` is a numeric vector.", fixed = TRUE)
  p <- metro_dag(data.frame(from = "A", to = "B"), labels = FALSE, legend = FALSE)
  layout <- attr(p, "metrodag_layout")
  expect_error(metro_dag(p), "To reuse its layout, pass attr(plot, \"metrodag_layout\").", fixed = TRUE)
  expect_error(layout_metro_dag(p), "pass attr(plot, \"metrodag_layout\")$graph.", fixed = TRUE)
  expect_error(layout_metro_dag(layout), "`graph` is a metrodag_layout.", fixed = TRUE)
  expect_error(layout_metro_dag(layout), "pass its `graph` element.", fixed = TRUE)
  expect_identical(arrows(layout_metro_dag(layout$graph)$graph), "A|B")
  expect_error(metro_dag(data.frame(source = "A", target = "B")),
    "`graph` has columns `source` and `target`.", fixed = TRUE)
  expect_error(metro_dag(data.frame(from = c("A", NA, "C"), to = c("B", "C", NA))),
    "Edge endpoints cannot be missing; see rows 2 and 3 of `graph`.", fixed = TRUE)
  expect_error(metro_graph(data.frame(name = c("A", "B")), data.frame(from = c("A", "Z"), to = c("Q", "B"))),
    "`Z` and `Q` are not there.", fixed = TRUE)
  skip_if_not_installed("dagitty")
  checked <- paste0(base, ", a metrodag_layout, or a MetroDAG plot.")
  expect_error(check_metro_adjustment(NULL, exposure = "A", outcome = "B", unadjusted = TRUE),
    paste("`graph` is NULL.", checked), fixed = TRUE)
  expect_error(check_metro_adjustment("dag { A -> B }", exposure = "A", outcome = "B", unadjusted = TRUE),
    dagitty_hint, fixed = TRUE)
  expect_error(check_metro_adjustment(ggplot2::ggplot(), exposure = "A", outcome = "B", unadjusted = TRUE),
    paste("`graph` is a ggplot that MetroDAG did not draw.", checked), fixed = TRUE)
  expect_true(check_metro_adjustment(p, exposure = "A", outcome = "B", unadjusted = TRUE)$valid)
})

test_that("every empty graph input asks for at least one node", {
  expected <- "`graph` must contain at least one node."
  for (graph in list(data.frame(from = character(), to = character()),
      matrix(character(), ncol = 2), igraph::make_empty_graph(0))) {
    expect_error(metro_dag(graph), expected, fixed = TRUE)
    expect_error(layout_metro_dag(graph), expected, fixed = TRUE)
  }
  skip_if_not_installed("dagitty")
  empty <- dagitty::dagitty("dag { }")
  expect_error(metro_dag(empty), expected, fixed = TRUE)
  expect_error(layout_metro_dag(empty), expected, fixed = TRUE)
  expect_error(compare_metro_dags(empty, list(A = list())), expected, fixed = TRUE)
  expect_error(check_metro_adjustment(empty, exposure = "A", outcome = "B", unadjusted = TRUE),
    expected, fixed = TRUE)
  # A DAGitty graph with nodes but no arrows is still accepted.
  expect_identical(layout_metro_dag(dagitty::dagitty("dag { A }"))$nodes$node, "A")
  skip_if_not_installed("tidygraph")
  expect_error(metro_dag(tidygraph::tbl_graph()), expected, fixed = TRUE)
})

test_that("rank messages name the arrows and nodes at fault", {
  waves <- data.frame(from = c("C", "C", "X1", "X2", "Y1", "Y2", "X1", "Y1", "X2", "Y2"),
    to = c("X1", "Y1", "X2", "X3", "Y2", "Y3", "Y2", "X2", "Y3", "X3"))
  rank <- c(C = 0, X1 = 1, Y1 = 1, X2 = 2, Y2 = 2, X3 = 3, Y3 = 3)
  expect_identical(layout_metro_dag(waves, rank = rank)$nodes$layer, c(0L, 1L, 2L, 1L, 2L, 3L, 3L))
  same_wave <- rbind(waves, data.frame(from = "X2", to = "Y2"))
  expect_error(metro_dag(same_wave, rank = rank),
    "Ranks must strictly increase along every arrow, but X2 -> Y2 goes from rank 2 to rank 2.",
    fixed = TRUE)
  expect_error(compare_metro_dags(same_wave, list(A = list()), rank = rank),
    "X2 -> Y2 goes from rank 2 to rank 2.", fixed = TRUE)
  backwards <- rbind(waves, data.frame(from = c("X2", "X1", "X3", "C"), to = c("Y2", "Y1", "Y3", "Y3")))
  expect_error(layout_metro_dag(backwards, rank = c(rank[-1L], C = 3.5)), paste(
    "but C -> X1 goes from rank 3.5 to rank 1, C -> Y1 goes from rank 3.5 to rank 1,",
    "X2 -> Y2 goes from rank 2 to rank 2 and 3 more."), fixed = TRUE)
  expect_error(metro_dag(waves, rank = rank[-7L]),
    "Named `rank` must give every node a rank; it has none for `Y3`.", fixed = TRUE)
  expect_error(metro_dag(waves, rank = rank[-c(2L, 7L)]), "it has none for `X1` and `Y3`.", fixed = TRUE)
  expect_error(metro_dag(waves, rank = c(rank[-7L], y3 = 3)),
    "`rank` names `y3`, which is not in the graph. Node names are: C, X1, X2, Y1, Y2, X3, Y3.",
    fixed = TRUE)
  expect_error(metro_dag(waves, rank = c(rank, Z = 4)), "`rank` names `Z`, which is not in the graph.",
    fixed = TRUE)
  expect_error(metro_dag(waves, rank = replace(rank, c(3L, 7L), c(NA, Inf))),
    "`rank` must give every node a finite rank, but it gives `Y1` = NA and `Y3` = Inf.", fixed = TRUE)
  # Unnamed ranks follow vertex order, so non-finite values are named by node.
  expect_error(metro_dag(waves, rank = c(0, 1, NA, 1, 2, 3, 3)), "it gives `X2` = NA.", fixed = TRUE)
  expect_error(metro_dag(waves, rank = c(0, 1, 2)),
    "Unnamed `rank` has 3 values for 7 nodes; give one value per node", fixed = TRUE)
  expect_error(metro_dag(waves, rank = c(rank, C = 1)), "`rank` names `C` more than once", fixed = TRUE)
  expect_error(metro_dag(waves, rank = c(C = 0, 1, 1, 2, 2, 3, 3)), "`rank` must name every value or none.",
    fixed = TRUE)
  expect_error(metro_dag(waves, rank = as.character(rank)), "`rank` must be a numeric vector", fixed = TRUE)
})

test_that("a tidygraph node table without `name` says how its nodes are named", {
  skip_if_not_installed("tidygraph")
  graph <- tidygraph::tbl_graph(data.frame(id = c("a", "b", "c"), label = c("Alpha", "Beta", "Gamma")),
    data.frame(from = 1:2, to = 2:3))
  expect_message(layout <- layout_metro_dag(graph),
    "`graph` has no `name` node column, so its nodes are named 1 to 3 in row order.", fixed = TRUE)
  expect_identical(layout$nodes$node, c("1", "2", "3"))
  messages <- character()
  withCallingHandlers(metro_dag(graph, labels = FALSE, legend = FALSE),
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_length(messages, 1L)
  # Graphs without node attributes, or with names, stay silent.
  expect_no_message(layout_metro_dag(tidygraph::as_tbl_graph(igraph::make_ring(3, directed = TRUE, circular = FALSE))))
  expect_no_message(layout_metro_dag(tidygraph::as_tbl_graph(.compat_reference())))
})

test_that("cycle messages stay quick on large inputs", {
  skip_on_cran()
  quick <- function(edges) system.time(tryCatch(layout_metro_dag(edges), error = identity))[["elapsed"]]
  # A long cycle through the first arrow of its part: 8,000 waves of a panel study.
  waves <- 8000L
  at <- function(v, t) paste0(v, t)
  panel <- rbind(data.frame(from = at("X", 1:(waves - 1L)), to = at("X", 2:waves)),
    data.frame(from = at("Y", 1:(waves - 1L)), to = at("Y", 2:waves)),
    data.frame(from = at("X", 1:waves), to = at("Y", 1:waves)),
    data.frame(from = c(at("Y", waves), "Y1"), to = "X1"))
  expect_lt(quick(panel), 5)
  # Many separate cycles: 10,000 arrows entered in both directions.
  k <- seq_len(10000L)
  both <- rbind(data.frame(from = paste0("a", k), to = paste0("b", k)),
    data.frame(from = paste0("b", k), to = paste0("a", k)))
  expect_lt(quick(both), 5)
  expect_error(layout_metro_dag(both), "and 9995 more would make it acyclic.", fixed = TRUE)
})
