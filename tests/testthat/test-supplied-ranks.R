# Supplied ranks fix every node's layer but not the gaps between layers or the
# positions within a layer. Re-running a layout with its own automatic layers
# as explicit ranks loses no rank information, so it should not draw worse.

test_that("explicit ranks keep the refinements that preserve every rank", {
  skip_on_cran()
  skip_if_not_installed("dagitty")
  # The final automatic layers of each graph (main 849f62b) and the crossings
  # and bends that main drew when these layers were supplied as `rank`.
  cases <- list(
    list(graph = "Kampen", main = c(6, 36), rank = c(AFF = 2, AIS = 1, ALN = 3,
      APA = 4, CDR = 3, DET = 6, EGC = 6, FTW = 5, HOS = 7, PER = 4, SAN = 0, SUS = 4)),
    list(graph = "Shrier", main = c(2, 16), rank = c(Coach = 1,
      ConnectiveTissueDisorder = 2, ContactSport = 0, FitnessLevel = 2, Genetics = 1,
      Injury = 6, IntraGameProprioception = 5, NeuromuscularFatigue = 3,
      PreGameProprioception = 3, PreviousInjury = 3, TeamMotivation = 2,
      TissueWeakness = 3, WarmUpExercises = 4)),
    list(graph = "Thoemmes", main = c(2, 12), rank = c(e0 = 0, e1 = 1, e2 = 0,
      e3 = 2, e4 = 6, s1 = 3, s2 = 4, s3 = 5, x = 1, y = 7, z = 4, z2 = 5, z3 = 8)),
    list(graph = "Schipf", main = c(5, 19), rank = c(A = 0, PA = 1, S = 3, T2DM = 4,
      TT = 4, U = 3, WC = 2)),
    list(graph = c(n = 10, density = 3, seed = 5), main = c(5, 18), rank = c(v1 = 0,
      v10 = 6, v2 = 1, v3 = 3, v4 = 2, v5 = 4, v6 = 5, v7 = 1, v8 = 4, v9 = 6)),
    list(graph = c(n = 20, density = 3, seed = 5), main = c(15, 31), rank = c(v1 = 1,
      v10 = 0, v11 = 2, v12 = 4, v13 = 4, v14 = 4, v15 = 0, v16 = 5, v17 = 6, v18 = 2,
      v19 = 2, v2 = 0, v20 = 5, v3 = 0, v4 = 2, v5 = 3, v6 = 0, v7 = 0, v8 = 3, v9 = 3)),
    list(graph = c(n = 20, density = 1.5, seed = 2), main = c(2, 12), rank = c(v1 = 3,
      v10 = 0, v11 = 3, v12 = 0, v13 = 3, v14 = 1, v15 = 0, v16 = 2, v17 = 4, v18 = 0,
      v19 = 3, v2 = 0, v20 = 1, v3 = 0, v4 = 2, v5 = 0, v6 = 0, v7 = 1, v8 = 0, v9 = 1)))
  drawn <- matrix(NA_real_, length(cases), 2L)
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    graph <- if (is.character(case$graph)) .as_dag(dagitty::getExample(case$graph),
      bidirected = "latent") else metrodag_examples("random", n = case$graph[["n"]],
        probability = case$graph[["density"]] / case$graph[["n"]], seed = case$graph[["seed"]])
    layout <- layout_metro_dag(graph, direction = "down", rank = case$rank)
    rank <- case$rank[layout$nodes$node]
    expect_equal(layout$nodes$layer, unname(rank))
    expect_true(.ranks_kept(layout$nodes$y, rank))
    expect_no_error(.assert_layout_geometry(layout))
    m <- layout_metrics(layout)
    drawn[i, ] <- c(m$crossings, m$total_bends)
    expect_lte(m$crossings, case$main[1L])
    expect_lte(m$total_bends, case$main[2L])
  }
  main <- do.call(rbind, lapply(cases, `[[`, "main"))
  expect_lt(sum(drawn), sum(main))
})

test_that("supplied ranks run rank-preserving refinements and skip rank reassignment", {
  graph <- metrodag_examples("causal")
  ranks <- c(U = 0, S = 0, C = 1, X = 2, M = 3, Y = 4)
  modes <- character()
  real <- .layout_component
  testthat::local_mocked_bindings(.layout_component = function(...) {
    modes <<- c(modes, list(...)$ranks)
    real(...)
  }, .refine_rank_assignment = function(...) stop("Supplied ranks are never reassigned"),
  .package = "metrodag")
  for (direction in c("down", "right")) {
    modes <- character()
    layout <- layout_metro_dag(graph, direction = direction, rank = ranks)
    expect_equal(layout$nodes$layer, unname(ranks[layout$nodes$node]))
    # The restart variants rebuild with the same supplied ranks.
    expect_gt(length(modes), 1L)
    expect_true(all(modes == "supplied"))
  }
  # Alignment groups keep the fixed pipeline without restarts.
  modes <- character()
  layout_metro_dag(graph, direction = "down", rank = ranks, align = list(c("C", "X")))
  expect_identical(modes, "fixed")
})
