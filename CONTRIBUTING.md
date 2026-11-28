# Contributing to metrodag

Thank you for helping improve metrodag. Bug reports, documentation fixes and
new example DAGs are all welcome.

## Reporting a problem

Open an issue at <https://github.com/hdbt/metrodag/issues> and include:

- a minimal graph (an edge data frame or `metrodag_examples()` call) and the
  `metro_dag()` or `save_metro_dag()` call that misbehaves;
- the output of `diagnose_metro_dag(p, width = <mm>)` when a figure does not
  fit or labels collide;
- `sessionInfo()` and, for export problems, the device (`pdf`, `svg`, `png`).

## Proposing a change

1. Fork the repository and create a branch from `main`.
2. Add or update tests under `tests/testthat/`. Layout tests should assert
   invariants (crossings, bends, clearances, identities) rather than exact
   coordinates where possible.
3. Run `devtools::document()`, then `R CMD build .` and
   `R CMD check --as-cran` on the tarball. Set `NOT_CRAN=true` to run the full
   regression suite.
4. If the change affects drawn geometry, regenerate the gallery with
   `Rscript data-raw/annotated_gallery.R` and review the PNGs.
5. Before merging, run the CRAN test selection against ggplot2 3.4.4 (see
   below).
6. Add a bullet to `NEWS.md` and open a pull request.

## Running the tests

The full suite takes about 8 minutes with testthat's two worker processes.
testthat runs the test files in parallel (`Config/testthat/parallel` in
`DESCRIPTION`), starting the slowest files first. It takes the number of
worker processes from the `Ncpus` option, then from the `TESTTHAT_CPUS`
environment variable, and uses two if neither is set. The option wins, so if
your `~/.Rprofile` sets `Ncpus`, testthat ignores `TESTTHAT_CPUS`. The command
below uses `--no-init-file` to skip that profile. `R CMD check` runs the tests
with `--vanilla`, which also skips it, so only `TESTTHAT_CPUS` counts there.
Use every core for a full run of the installed package:

```sh
R CMD INSTALL .
cd tests && TESTTHAT_CPUS=4 NOT_CRAN=true Rscript --no-init-file testthat.R
```

testthat prints "Starting 4 test processes" when the setting has taken effect.

Without `NOT_CRAN=true`, `tests/testthat.R` runs only the CRAN selection of
20 files (under a minute with two workers). The package supports ggplot2 3.4, which
names legend boxes and builds plots differently, so run that selection against
ggplot2 3.4.4 before merging. Keep the old versions in a separate library and
put it first in `R_LIBS`:

```sh
mkdir -p ~/R/ggplot2-3.4.4
Rscript -e 'v <- c(ggplot2 = "3.4.4", patchwork = "1.2.0")
  for (p in names(v)) remotes::install_version(p, v[[p]],
    lib = path.expand("~/R/ggplot2-3.4.4"), upgrade = "never",
    dependencies = FALSE)'
R CMD INSTALL .
cd tests && R_LIBS=~/R/ggplot2-3.4.4 Rscript testthat.R
```

The old-release GitHub job runs the full suite with the same versions.

## Continuous integration

GitHub Actions runs `R CMD check --as-cran` on every push to `main` and every
pull request: the full suite on R release (Ubuntu, Windows and macOS) and on
R oldrel-1 with ggplot2 3.4.4, and the CRAN selection with CRAN's incoming
checks on R-devel. A no-Suggests job runs the CRAN selection with only Depends
and Imports installed, plus testthat, as CRAN's noSuggests checks do, and
skips the vignettes. The Ubuntu release job also runs the README and home page
tests from the checkout, because `R CMD check` skips them (a local run from
the checkout, as above, includes them). A job at the declared floor, R 4.1 with
ggplot2 3.4.4, reports failures but does not block merging yet. A weekly run,
which you can also start from the Actions tab, adds a check against the
development versions of ggplot2 and patchwork (it never runs on pull requests,
and a failure fails the weekly run so that it is noticed), and writes test
coverage, broken URLs and possible misspellings to the run summary.

## Style

The package uses base R and `::`-qualified calls, two-space indentation, and
roxygen2 documentation with Markdown. Keep ASCII in source files.
