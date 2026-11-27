# testthat runs every test under C collation (local_reproducible_output()), so
# a layout that followed the session's collation locale would look
# deterministic here. These tests set another LC_COLLATE themselves.

# Evaluate `code` with LC_COLLATE set to `locale` and restore the previous
# setting afterwards; NULL when the locale cannot be set.
.with_collation <- function(locale, code) {
  previous <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", previous), add = TRUE)
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", locale)))) return(NULL)
  code
}

# The first of some common locales that can be set and that collate unlike C,
# which sorts capitals before lower case; NULL when there is none.
.other_collation <- function() {
  for (locale in c("en_US.UTF-8", "en_US.utf8", "English_United States.utf8",
      "English_United States.1252", "de_DE.UTF-8", "German_Germany.1252"))
    if (isTRUE(.with_collation(locale, identical(sort(c("Mango", "apple")), c("apple", "Mango")))))
      return(locale)
  NULL
}

test_that("aligned layouts and metro path colours do not depend on the collation locale", {
  locale <- .other_collation()
  if (is.null(locale)) skip("No locale that collates unlike C can be set.")
  cases <- list(
    # The first name of each alignment group depends on the collation: "F" or
    # "a", "B" or "b", "Zulu" or "zebra".
    list(graph = rbind(c("H", "a"), c("H", "e"), c("a", "e"), c("B", "e"), c("H", "F"),
      c("B", "F"), c("e", "F")), align = list(c("a", "F"))),
    list(graph = rbind(c("apple", "Mango"), c("apple", "b"), c("apple", "10"), c("B", "10")),
      rank = c(apple = 0, Mango = 1, b = 1, "10" = 1, B = 0), align = list(c("B", "b"))),
    list(graph = rbind(c("b", "Mango"), c("b", "zebra"), c("Mango", "Zulu"),
        c("zebra", "\u00e9clair"), c("Mango", "apple")),
      rank = c(b = 0, Mango = 1, zebra = 1, Zulu = 2, "\u00e9clair" = 2, apple = 2),
      align = list(c("zebra", "Zulu"))),
    # Metro paths are numbered by the names of their first stations.
    list(graph = rbind(c("apple", "zebra"), c("Mango", "zebra"), c("zebra", "kiwi"),
      c("\u00c9tape", "kiwi"), c("9", "apple"))))
  draw <- function() lapply(cases, function(case) {
    p <- do.call(metro_dag, c(case, list(style = "metro")))
    drawn <- Find(function(l) inherits(l$geom, "GeomMetroDag"), p$layers)$geom_params
    list(layout = attr(p, "metrodag_layout")[c("nodes", "edges", "parameters")],
      nodes = drawn$nodes, edges = drawn$edges)
  })
  expect_identical(.with_collation(locale, draw()), .with_collation("C", draw()))
})

test_that("metro paths are numbered in C-locale order of their first stations", {
  locale <- .other_collation()
  if (is.null(locale)) skip("No locale that collates unlike C can be set.")
  # "Banana" comes before "apple" in C-locale order, after it in en_US or de_DE.
  layout <- .with_collation(locale, layout_metro_dag(rbind(c("apple", "Mango"),
    c("Banana", "Mango"), c("Mango", "zebra"))))
  paths <- stats::setNames(layout$nodes$path, layout$nodes$node)
  expect_equal(paths[c("apple", "Banana", "Mango", "zebra")],
    c(apple = 2, Banana = 1, Mango = 1, zebra = 1))
})

test_that("an alignment group is ordered by its first name in C-locale order", {
  locale <- .other_collation()
  if (is.null(locale)) skip("No locale that collates unlike C can be set.")
  # The group {a, F} and the node G both target x = 0 in rank 1, so their first
  # names decide which comes first. In C-locale order "F" comes before "G";
  # min() took "a" under en_US or de_DE collation, and "a" comes after "G".
  placed <- lapply(c("C", locale), function(current) .with_collation(current,
    .aligned_positions(c(4, -4, 0), c(0, 1, 1), c("a", "F", "G"), list(c("a", "F")))))
  expect_identical(placed[[2L]], placed[[1L]])
  expect_equal(placed[[1L]][1L], placed[[1L]][2L])
  expect_lt(placed[[1L]][2L], placed[[1L]][3L])
})
