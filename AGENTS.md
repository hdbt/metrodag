# Notes for coding agents

metrodag is an R package that draws causal DAGs as publication figures in a
metro-map style. Human contributor guidance is in `CONTRIBUTING.md`; the
design history and release steps are in `DEVELOPMENT.md`. This file lists what
an automated agent needs most often.

## Commands

Run from the repository root.

```sh
Rscript -e 'devtools::document()'          # after editing roxygen comments
R CMD INSTALL .
cd tests && Rscript --no-init-file testthat.R                     # CRAN selection, < 1 min
cd tests && NOT_CRAN=true TESTTHAT_CPUS=4 Rscript --no-init-file testthat.R   # full suite, ~8 min
R CMD build . && R CMD check --as-cran metrodag_*.tar.gz
Rscript -e 'pkgdown::build_site(devel = FALSE, preview = FALSE)'  # site in _site/, incl. llms.txt
Rscript data-raw/annotated_gallery.R       # regenerate gallery after geometry changes
```

Tests run against the installed package, so reinstall before running them.
The CRAN test selection is the `cran_tests` list in `tests/testthat.R`; add a
new test file there only if it is fast and covers a public workflow.

## Invariants

- **Deterministic layout.** The same graph and arguments must give the same
  geometry on every platform. No unseeded randomness; `metrodag_examples()`
  must preserve the caller's RNG state.
- **Annotations never change geometry.** Roles, labels, styles, estimands and
  node groups are drawn on a fixed layout; they must not move nodes or routes.
- **Nothing causal is inferred.** Roles come only from the caller; plotting
  never runs dagitty checks implicitly.
- **Fail loudly on export.** `save_metro_dag()` refuses to write a figure that
  cannot fit every arrow and station unless `on_error = "warn"`.
- **ggplot2 >= 3.4.0 is supported.** Run the CRAN selection against ggplot2
  3.4.4 before merging (see `CONTRIBUTING.md`).
- **Layout tests assert invariants** (crossings, bends, clearances,
  identities) rather than exact coordinates where possible.

## Documentation

- The README, the `?metrodag` help page (`R/metrodag-package.R`) and the
  pkgdown home page (`pkgdown/index.md`) are the summary documentation. pkgdown
  builds the site's `llms.txt` from the home page plus the reference and
  article indexes in `_pkgdown.yml`.
- `tests/testthat/test-documentation-coverage.R` checks that the README and
  the help page name every exported function and no unexported one, and runs
  the README and home page code. Update these files when the API changes.
- README links must be absolute URLs: many repository files are excluded from
  the source package by `.Rbuildignore`.
- Add a bullet to `NEWS.md` for user-visible changes.
