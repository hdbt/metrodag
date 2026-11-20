test_that("DAGitty imports retain directed edges, isolated nodes and roles", {
  skip_if_not_installed("dagitty")
  g <- dagitty::dagitty("dag { U [latent]; X [exposure]; Y [outcome]; C [adjusted]; I; U -> C; C -> X; C -> Y; X -> Y }")
  l <- attr(metro_dag(g), "metrodag_layout")
  expect_setequal(l$nodes$node, names(g))
  expect_equal(igraph::ecount(l$graph), nrow(dagitty::edges(g)))
  expect_equal(igraph::vertex_attr(l$graph, "role", "U"), "unobserved")
  expect_equal(igraph::vertex_attr(l$graph, "role", "X"), "exposure")
  expect_equal(igraph::vertex_attr(l$graph, "role", "Y"), "outcome")
  expect_equal(igraph::vertex_attr(l$graph, "role", "C"), "adjusted")
  expect_equal(igraph::degree(l$graph, "I"), c(I = 0))
  expect_error(metro_dag(dagitty::dagitty("dag { X <-> Y }")), "bidirected arcs")
  expect_error(metro_dag(dagitty::dagitty("pdag { X -- Y }")), "directed edges only")
})

test_that("total-effect checks distinguish confounding, mediation and collider adjustment", {
  skip_if_not_installed("dagitty")
  check <- function(syntax, adjusted) check_metro_adjustment(dagitty::dagitty(syntax),
    exposure = "X", outcome = "Y", adjusted = adjusted)$valid
  expect_true(check("dag { C -> X; C -> Y; X -> Y }", "C"))
  expect_false(check("dag { C -> X; C -> Y; X -> Y }", character()))
  expect_true(check("dag { X -> M -> Y }", character()))
  expect_false(check("dag { X -> M -> Y }", "M"))
  expect_false(check("dag { X -> M <- Y; X -> Y }", "M"))
  expect_false(check("dag { U [latent]; U -> X; U -> Y; X -> Y }", character()))
  expect_error(check_metro_adjustment(dagitty::dagitty("dag { X -> Y }"),
    exposure = "X", outcome = "Y"), "empty set")
  p <- metro_dag(dagitty::dagitty("dag { X [exposure]; Y [outcome]; X -> Y }"), unadjusted = TRUE)
  expect_true(check_metro_adjustment(p)$valid)
  expect_equal(check_metro_adjustment(p)$effect, "total")
})

test_that("adjustment checks support arbitrary igraph identifiers", {
  skip_if_not_installed("dagitty")
  g <- igraph::graph_from_edgelist(rbind(c('quoted "C"', "years of smoking"),
    c('quoted "C"', "lung-cancer"), c("years of smoking", "lung-cancer")), directed = TRUE)
  x <- check_metro_adjustment(g, exposure = "years of smoking", outcome = "lung-cancer", adjusted = 'quoted "C"')
  expect_true(x$valid)
  expect_equal(x$adjusted, 'quoted "C"')
  expect_error(check_metro_adjustment(g, exposure = "years of smoking", outcome = "lung-cancer", adjusted = "missing"), "`adjusted` names a node not in the graph: `missing`")
})
