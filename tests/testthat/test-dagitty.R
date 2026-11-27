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

test_that("checks of several exposures or outcomes print as joint total effects", {
  skip_if_not_installed("dagitty")
  # X2 mediates part of the effect of X1 on Y, and C confounds X1 and Y.
  g <- dagitty::dagitty("dag { C -> X1; C -> Y; X1 -> X2; X1 -> Y; X2 -> Y }")
  joint <- check_metro_adjustment(g, exposure = c("X1", "X2"), outcome = "Y", adjusted = "C")
  expect_true(joint$valid)
  expect_identical(utils::capture.output(print(joint)), c(
    "<metrodag_adjustment_check> joint total effect of {X1, X2} on {Y}",
    "Adjustment set: {C}", "Meets DAGitty's adjustment criterion: TRUE"))
  outcomes <- check_metro_adjustment(g, exposure = "X1", outcome = c("X2", "Y"), adjusted = "C")
  expect_identical(utils::capture.output(print(outcomes))[[1L]],
    "<metrodag_adjustment_check> joint total effect of {X1} on {X2, Y}")
  single <- utils::capture.output(print(check_metro_adjustment(g, exposure = "X1", outcome = "Y", adjusted = "C")))
  expect_identical(single[[1L]], "<metrodag_adjustment_check> total effect: X1 -> Y")
  expect_false(any(grepl("joint", single, fixed = TRUE)))
  # The joint check passes, yet in a model that also adjusts X1 for X2 the
  # coefficient of X1 is not its total effect.
  expect_false(check_metro_adjustment(g, exposure = "X1", outcome = "Y", adjusted = c("C", "X2"))$valid)
})

.dagitty_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, messages = messages)
}

# Paths depend on the graph, not on the drawing: skip refinement for speed.
.dagitty_layout <- function(graph) {
  old <- options(metrodag.control = list(stages = stats::setNames(
    rep(FALSE, length(.metro_layout_stages)), .metro_layout_stages)))
  on.exit(options(old))
  layout_metro_dag(graph)
}

test_that("highlight_paths() names over-adjustment that the biasing view cannot show", {
  skip_if_not_installed("dagitty")
  # C confounds X and Y, M mediates the effect of X on Y, and D and E descend
  # from the outcome and from the mediator.
  l <- .dagitty_layout(data.frame(from = c("C", "C", "X", "M", "Y", "M"), to = c("X", "Y", "M", "Y", "D", "E")))
  view <- function(...) plot_metro_dag(l, ..., labels = FALSE, legend = FALSE)
  advice <- "Biasing paths do not show this; check the adjustment set with check_metro_adjustment()."
  # Adjusting for the mediator leaves no biasing path open, yet the set fails
  # the adjustment criterion: the warning names the mediator and its path.
  mediator <- view(exposure = "X", outcome = "Y", adjusted = c("C", "M"))
  expect_false(check_metro_adjustment(mediator)$valid)
  result <- .dagitty_warnings(highlight_paths(mediator, paths = "biasing"))
  expect_false(any(attr(result$value, "metrodag_paths")$highlighted))
  expect_identical(result$messages, c("No open biasing paths found between X and Y.",
    paste("Conditioning on M can bias the total effect of X on Y: M lies on the causal path X -> M -> Y.", advice)))
  # Descendants of the outcome and of the mediator are named with the node on
  # the causal path that they descend from.
  descendants <- view(exposure = "X", outcome = "Y", adjusted = c("C", "D", "E"))
  expect_identical(.dagitty_warnings(highlight_paths(descendants, paths = "biasing"))$messages,
    c("No open biasing paths found between X and Y.",
      paste("Conditioning on D and E can bias the total effect of X on Y: D descends from Y, which lies on",
        "the causal path X -> M -> Y; E descends from M, which lies on the causal path X -> M -> Y.", advice)))
  # A search stopped at `limit` still finds every such node, from DAGitty's
  # proper back-door graph.
  cut <- .dagitty_warnings(highlight_paths(view(exposure = "X", outcome = "Y", adjusted = c("C", "M", "D")),
    paths = "biasing", limit = 1))
  expect_length(cut$messages, 2L)
  expect_match(cut$messages[2L], paste("^Conditioning on M and D can bias the total effect of X on Y:",
    "M lies on .+; D descends from .+check_metro_adjustment\\(\\)\\.$"))
  # A valid set gives no such warning, and the other views never do.
  valid <- view(exposure = "X", outcome = "Y", adjusted = "C")
  expect_true(check_metro_adjustment(valid)$valid)
  expect_identical(.dagitty_warnings(highlight_paths(valid, paths = "biasing"))$messages,
    "No open biasing paths found between X and Y.")
  for (kind in c("open", "causal", "all"))
    expect_false(any(startsWith(.dagitty_warnings(highlight_paths(mediator, paths = kind))$messages,
      "Conditioning on")))
  # Selection on the mediator biases the effect too, but check_metro_adjustment()
  # refuses selected variables, so the warning points to DAGitty instead.
  selection <- view(exposure = "X", outcome = "Y", adjusted = "C", selected = "M")
  expect_error(check_metro_adjustment(selection), "does not model selection")
  expect_identical(.dagitty_warnings(highlight_paths(selection, paths = "biasing"))$messages,
    c("No open biasing paths found between X and Y.",
      paste("Conditioning on M can bias the total effect of X on Y: M lies on the causal path X -> M -> Y.",
        "Biasing paths do not show this; check_metro_adjustment() does not model selection, so check the",
        "conditioning set with DAGitty directly.")))
  # With several exposures the warning concerns their joint effect, and a
  # path through another exposure is not a causal path to name.
  joint <- view(exposure = c("C", "X"), outcome = "Y", adjusted = "M")
  expect_identical(.dagitty_warnings(highlight_paths(joint, paths = "biasing"))$messages,
    c("No open biasing paths found between C, X and Y.",
      paste("Conditioning on M can bias the joint total effect of {C, X} on {Y}: M lies on the causal",
        "path X -> M -> Y.", advice)))
})

test_that("an empty biasing view without over-adjustment is DAGitty's adjustment criterion", {
  skip_if_not_installed("dagitty")
  skip_on_cran()
  cases <- list(
    # Confounding, mediation and a descendant of the outcome.
    list(dag = "dag { C -> X; C -> Y; X -> M; M -> Y; Y -> D }", exposure = "X", outcome = "Y"),
    # M-bias: adjusting for the collider B opens a path through unobserved U2.
    list(dag = "dag { U2 [latent]; U1 -> X; U1 -> B; U2 -> B; U2 -> Y; X -> Y }", exposure = "X", outcome = "Y"),
    # Two exposures, X2 on a causal path from X1, and a mediator of X1.
    list(dag = "dag { C -> X1; C -> Y; X1 -> X2; X2 -> Y; X1 -> M; M -> Y }",
      exposure = c("X1", "X2"), outcome = "Y"))
  for (case in cases) {
    dag <- dagitty::dagitty(case$dag)
    l <- .dagitty_layout(dag)
    candidates <- setdiff(names(dag), c(case$exposure, case$outcome, dagitty::latents(dag)))
    for (k in 0:length(candidates)) for (set in utils::combn(candidates, k, simplify = FALSE)) {
      p <- plot_metro_dag(l, exposure = case$exposure, outcome = case$outcome, adjusted = set,
        labels = FALSE, legend = FALSE)
      result <- .dagitty_warnings(highlight_paths(p, paths = "biasing"))
      rule <- !any(attr(result$value, "metrodag_paths")$highlighted) &&
        !any(startsWith(result$messages, "Conditioning on"))
      expect_identical(rule, dagitty::isAdjustmentSet(dag, Z = set, exposure = case$exposure,
        outcome = case$outcome), info = sprintf("%s, adjusted for {%s}", case$dag, paste(set, collapse = ", ")))
    }
  }
})
