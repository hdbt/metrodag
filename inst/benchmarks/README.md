# Reproducible examples and diagnostics

From the repository root:

```sh
R CMD INSTALL .
Rscript inst/benchmarks/run_benchmarks.R
Rscript data-raw/adjustment_views.R
Rscript data-raw/annotated_gallery.R
Rscript inst/benchmarks/route_cleanup_examples.R
Rscript inst/benchmarks/compact_layout_examples.R
```

The script regenerates:

- The seven canonical SVGs, using the same publication defaults as `metro_dag()`.
- A long-label SVG and PNG, plus monochrome and metro style examples.
- The README's six annotated evaluation DAGs, with stable numbering, PNG/SVG
  outputs, geometry metrics and device-specific diagnostics. Regenerate these
  with `data-raw/annotated_gallery.R` after changing the layout algorithm.
- `metrics.csv` and `REPORT.md`, including node–edge collisions.
- Layout-only timings for seeded sparse DAGs with 20, 50 and 100 nodes.
- An igraph Sugiyama coordinate/runtime table.

The adjustment-view script generates three SVGs and a side-by-side PNG using
one fixed layout. They show an unadjusted X-Y association, an adjusted analysis
targeting the total effect of X on Y, and a different target (M on Y). These
annotations do not check whether an adjustment set identifies the stated effect.

Runtimes exclude rendering and pairwise diagnostics. They are single-machine
smoke measurements. Crossing/overlap definitions changed in 0.2; see the report
before comparing with archived metrics. Dense graphs deliberately remain in
the gallery to expose shared tracks and crossings.

`svglite` is recommended for editable SVG text; the exporter falls back to base
R SVG otherwise. This benchmark does not establish perceptual superiority.

The [route-cleanup report](ROUTE_CLEANUP.md) compares the two annotated arrows
that motivated section simplification. Its review figures use 240 mm width so
the full labels fit. To reproduce the baseline comparison and the before figures,
supply a checkout of commit `32884467e422aeb643d7b0f390118249d417c115`:

```sh
Rscript inst/benchmarks/route_cleanup_benchmark.R /path/to/baseline
Rscript inst/benchmarks/route_cleanup_examples.R /path/to/baseline
```

The route-cleanup baseline scripts read the R sources from that checkout, while
the current package supplies the updated layout and renderer. The benchmark
checks the geometric safeguards on 22 canonical, random and annotated fixtures,
including explicit ranks, disconnected components and crowded arrow attachments.

The [compact-layout report](COMPACT_LAYOUTS.md) compares the following improvement
against merged PR #31, commit `70fb028a21b0cc29feb366efaacdbd6987125af6`:

```sh
Rscript inst/benchmarks/compact_layout_benchmark.R /path/to/baseline
Rscript inst/benchmarks/compact_layout_examples.R /path/to/baseline
```

These figures use each version's layout and plot defaults at the same 240 mm
width; current exports are also checked at 180 mm. The benchmark checks geometry
on the same 22 fixtures. Coordinates and route row counts can change during
compaction; causal edges, route identities, rank order and ties are preserved.

The [node-arrangement report](NODE_ARRANGEMENTS.md) compares the next compaction
against merged PR #32, commit `c48ae8b16f1f5d4379e20b028bd87117b87d79b1`:

```sh
Rscript inst/benchmarks/arrangement_benchmark.R /path/to/baseline
Rscript inst/benchmarks/arrangement_baseline.R /path/to/baseline
Rscript data-raw/annotated_gallery.R
```

The baseline script reproduces the two before figures. Current figures come
from the six-example gallery, so the comparison and README use the same output.
The 22-fixture benchmark additionally checks that neither routed extent grows.

The [automatic-rank report](RANK_REFINEMENT.md) compares the next refinement
against merged PR #33, commit `df9f782df7010b954932f1d6438efaf6a186e270`:

```sh
Rscript inst/benchmarks/rank_refinement_benchmark.R /path/to/baseline
Rscript inst/benchmarks/rank_refinement_baseline.R /path/to/baseline
Rscript data-raw/annotated_gallery.R
```

The before figure uses the baseline layout with the current, unchanged renderer.
The after figure is DAG 4 in the six-example gallery. The benchmark allows
automatic ranks and metro paths to change while checking topology, edge IDs,
forward ranks, routed extents and geometry quality. Explicitly ranked drawings
must retain identical nodes and routes.

`label_association.R` counts labels that read as another station's
(`label_association`), labels more than 7 mm from their station and cut off
from it by another arrow (`label_cut_off`), and labels that touch a route
(`label_route`) for the six gallery DAGs, five DAGitty examples and a mediator
fan at 85, 120 and 180 mm. Every figure is diagnosed as a PNG at 300 dpi,
because label placement depends on the device. The script applies the
detector of the checkout it runs in to the final labels of every version;
the `reported_association` and `reported_cut_off` columns hold what that
version's `diagnose_metro_dag()` itself reports. Cut-offs are reported from
round 2 on, and rows written before a column existed have NA in it. The
`round2 before` rows come from commit `849f62b`, the `round2 after` rows from
the round 2 label placement. To compare with the code before PR #54, install
commit `6acb470^1` into a separate library and pass it as the second argument:

```sh
git worktree add --detach /tmp/before 6acb470^1
mkdir -p /tmp/before-lib && R CMD INSTALL -l /tmp/before-lib /tmp/before
Rscript inst/benchmarks/label_association.R before /tmp/before-lib
Rscript inst/benchmarks/label_association.R after
```

The rows go to `label_association.csv`. Only `metrics.csv` from this directory
is included in the CRAN source package; the scripts and reports stay in the
repository.
