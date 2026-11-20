.compat_dagitty <- function() {
  dagitty::dagitty("dag { C [adjusted]; X [exposure]; Y [outcome]; U [latent]; I; C -> X; C -> Y; X -> Y; U -> X; U -> Y }")
}

test_that("plain DAGitty input retains ggdag's named display labels without loading ggdag", {
  skip_if_not_installed("dagitty")
  dag <- .compat_dagitty()
  attr(dag, "labels") <- c(X = "Actual\nexposure", Y = "", I = NA_character_)
  original <- dag
  p <- metro_dag(dag)
  n <- p$layers[[2L]]$geom_params$nodes
  expect_equal(stats::setNames(n$label, n$node)[c("X", "Y", "I")],
    c(X = "Actual\nexposure", Y = "", I = "I"))
  expect_identical(dag, original)
  expect_equal(attr(p, "metrodag_analysis")$unobserved, "U")
  expect_equal(igraph::degree(attr(p, "metrodag_layout")$graph, "I"), c(I = 0))
  for (bad in list("text", c(X = 1), c(missing = "text"), c(X = "a", X = "b"))) {
    invalid <- dag; attr(invalid, "labels") <- bad
    expect_error(metro_dag(invalid), "Imported DAGitty labels")
  }
  overridden <- metro_dag(dag, labels = c(X = "Override"))
  expect_equal(overridden$layers[[2L]]$geom_params$nodes$label[
    overridden$layers[[2L]]$geom_params$nodes$node == "X"], "Override")
})

test_that("dagify and tidy_dagitty preserve the same model, display labels and analysis", {
  skip_if_not_installed("ggdag", "0.2.13")
  dag <- ggdag::dagify(Y ~ X + C + U, X ~ C + U, exposure = "X", outcome = "Y",
    latent = "U", labels = c(X = "Exposure", Y = "Outcome", U = "Unmeasured\nfactor"))
  original <- dag
  p <- metro_dag(dag)
  expect_equal(attr(p, "metrodag_analysis")$exposure, "X")
  expect_equal(attr(p, "metrodag_analysis")$outcome, "Y")
  expect_equal(attr(p, "metrodag_analysis")$unobserved, "U")
  expect_identical(dag, original)
  labels <- stats::setNames(.compat_tables()$nodes$label, .compat_tables()$nodes$name)
  dag <- ggdag::`label<-`(.compat_dagitty(), labels)
  tidy <- ggdag::tidy_dagitty(dag, seed = 1)
  tidy <- ggdag::node_status(tidy)
  original <- tidy
  # Each input is laid out once and turned to every direction (test-layout.R);
  # metro_dag() is also called on both inputs directly, facing down.
  dags <- .test_layouts_by_direction(dag)
  tidys <- .test_layouts_by_direction(tidy)
  a <- metro_dag(dag, direction = "down")
  b <- metro_dag(tidy, direction = "down")
  expect_identical(.compat_display(b), .compat_display(a))
  expect_equal(.compat_routes(attr(b, "metrodag_layout")), .compat_routes(attr(a, "metrodag_layout")))
  expect_identical(attr(b, "metrodag_analysis"), attr(a, "metrodag_analysis"))
  expect_identical(.compat_display(metro_dag(dags$down)), .compat_display(a))
  for (direction in c("down", "up", "right", "left")) {
    a <- metro_dag(dags[[direction]])
    b <- metro_dag(tidys[[direction]])
    expect_identical(.compat_display(b), .compat_display(a))
    expect_equal(.compat_routes(attr(b, "metrodag_layout")), .compat_routes(attr(a, "metrodag_layout")))
    expect_identical(attr(b, "metrodag_analysis"), attr(a, "metrodag_analysis"))
    expect_equal(igraph::degree(attr(b, "metrodag_layout")$graph, "I"), c(I = 0))
  }
  expect_identical(tidy, original)
  expect_true(check_metro_adjustment(tidy, unadjusted = TRUE)$effect == "total")
  compare <- compare_metro_dags(tidy, list(Inherit = list(), Crude = list(unadjusted = TRUE)))
  expect_equal(attr(compare$plots[[1L]], "metrodag_analysis")$adjusted, "C")
  expect_length(attr(compare$plots[[2L]], "metrodag_analysis")$adjusted, 0L)
  expect_equal(tidys$down$nodes$node, dags$down$nodes$node)
})

test_that("tidy labels are matched by identity and conflicting labels are rejected", {
  skip_if_not_installed("ggdag", "0.2.13")
  tidy <- ggdag::tidy_dagitty(.compat_dagitty(), seed = 1)
  d <- ggdag::pull_dag_data(tidy)
  d$label <- ifelse(d$name == "C", "Context\nlabel", NA_character_)
  changed <- ggdag::`update_dag_data<-`(tidy, d[rev(seq_len(nrow(d))), ])
  p <- metro_dag(changed)
  n <- p$layers[[2L]]$geom_params$nodes
  expect_equal(n$label[n$node == "C"], "Context\nlabel")
  expect_setequal(n$node, c("C", "X", "Y", "U", "I"))
  original <- changed
  d$label[which(d$name == "C")[1L]] <- "Conflicting label"
  bad <- ggdag::`update_dag_data<-`(tidy, d)
  expect_error(metro_dag(bad), "conflicting display labels")
  expect_identical(changed, original)
})

test_that("ggdag data cannot silently change the embedded model or analysis roles", {
  skip_if_not_installed("ggdag", "0.2.13")
  tidy <- ggdag::tidy_dagitty(.compat_dagitty(), seed = 1)
  previous <- ggplot2::ggplot(); ggplot2::set_last_plot(previous)
  update <- function(d) ggdag::`update_dag_data<-`(tidy, d)
  d <- ggdag::pull_dag_data(tidy)
  expect_error(metro_dag(update(d[-which(!is.na(d$to))[1L], ])), "disagree")
  bad <- d; bad$name[1L] <- "different node"
  expect_error(metro_dag(update(bad)), "disagree")
  bad <- d; bad$to[which(!is.na(bad$to))[1L]] <- "I"
  expect_error(metro_dag(update(bad)), "disagree")
  bad <- d; bad$status <- ifelse(bad$name == "X", "outcome", NA_character_)
  expect_error(metro_dag(update(bad)), "disagree")
  bad <- d; bad$adjusted <- ifelse(bad$name == "C", "adjusted", "unadjusted")
  expect_no_warning(metro_dag(update(bad)))
  bad$adjusted[which(bad$name == "C")[1L]] <- "unadjusted"
  ggplot2::set_last_plot(previous)
  expect_error(metro_dag(update(bad)), "disagree")
  expect_identical(ggplot2::last_plot(), previous)
  bidirected <- ggdag::tidy_dagitty(dagitty::dagitty("dag { X <-> Y }"), seed = 1)
  expect_error(metro_dag(bidirected), "bidirected arcs")
  selected <- ggdag::tidy_dagitty(dagitty::dagitty("dag { X [selected]; X -> Y }"), seed = 1)
  expect_identical(attr(metro_dag(selected), "metrodag_analysis")$selected, "X")
})
