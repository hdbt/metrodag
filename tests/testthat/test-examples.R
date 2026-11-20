test_that("random examples are reproducible DAGs", {
  a <- metrodag_examples("random", n = 50, seed = 42)
  b <- metrodag_examples("random", n = 50, seed = 42)
  expect_true(igraph::is_dag(a))
  expect_identical(igraph::as_edgelist(a), igraph::as_edgelist(b))
})

test_that("random examples preserve the caller's RNG, including an absent seed", {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed", previous, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(813)
  expected <- stats::runif(4)
  set.seed(813)
  before <- .Random.seed
  a <- metrodag_examples("random", seed = 3)
  expect_identical(.Random.seed, before)
  expect_identical(stats::runif(4), expected)
  b <- metrodag_examples("random", seed = 3)
  expect_identical(igraph::as_edgelist(a), igraph::as_edgelist(b))
  rm(".Random.seed", envir = .GlobalEnv)
  metrodag_examples("random", seed = 3)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  metrodag_examples("random", seed = NULL)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("random graph arguments are validated before changing RNG state", {
  for (n in list(0, -1, 3.5, NA, Inf, "4", TRUE, numeric(), c(2, 3)))
    expect_error(metrodag_examples("random", n = n), "`n`")
  for (p in list(-0.2, 1.5, NA, NaN, Inf, "x", TRUE, numeric(), c(0.1, 0.2)))
    expect_error(metrodag_examples("random", probability = p), "`probability`")
  for (seed in list(1.5, NA, Inf, "x", TRUE, numeric(), c(1, 2), 2^31))
    expect_error(metrodag_examples("random", seed = seed), "`seed`")
  expect_equal(igraph::ecount(metrodag_examples("random", n = 4, probability = 0)), 0L)
  expect_equal(igraph::ecount(metrodag_examples("random", n = 4, probability = 1)), 6L)
  expect_equal(igraph::vcount(metrodag_examples("random", n = 1, seed = -1)), 1L)
  before <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
  expect_error(metrodag_examples("random", probability = -0.2), "`probability`")
  after <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
  expect_identical(after, before)
})
