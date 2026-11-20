## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* Possibly misspelled words in DESCRIPTION: "DAGs" abbreviates directed
  acyclic graphs; Sugiyama, Tagawa, Toda and Textor are authors of the cited
  papers; "et al." belongs to a citation.

## Test environments

* Local: Ubuntu 24.04.5 LTS, R 4.6.2 (x86_64-pc-linux-gnu).
  `R CMD check --as-cran` on the built tarball, with the remote CRAN incoming
  checks; and `R CMD check` with only the packages in Depends and Imports
  (and testthat for the tests) available, as in the noSuggests checks.
* Local, the same machine with ggplot2 3.4.4 and patchwork 1.2.0 (older
  releases, for the lower bound in Imports): the CRAN test selection, all
  examples and all vignettes.

## Tests

On CRAN, `tests/testthat.R` runs a selection of 20 test files that covers
the public workflows (graph inputs, examples, analysis views, diagnostics,
styles, compositions and export) and the main routing checks. The complete
regression suite, which includes a geometry snapshot of the drawings, runs
when `NOT_CRAN=true` is set.
testthat runs the test files in parallel with its default of two worker
processes, so the tests use at most two cores.

The code, examples, tests and vignettes use the optional packages dagitty,
ggdag, tidygraph, svglite and patchwork only behind `requireNamespace()` or
`skip_if_not_installed()`. knitr and rmarkdown build the vignettes; the
tests that knit documents skip without them.

## Random numbers

`metrodag_examples("random")` generates a random DAG from its `seed`
argument and restores the caller's random-number state on exit.

## Downstream dependencies

There are currently no downstream dependencies for this package.
