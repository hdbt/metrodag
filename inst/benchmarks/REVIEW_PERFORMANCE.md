# Routing performance review

Measured on the same Linux environment with R 4.3.3, igraph 1.6.0 and
ggplot2 3.4.4. These are individual runs, not a statistical performance study.
The baseline is main commit e728f28, before this review's changes.

The input has 100 vertices and 92 edges, generated with:

    metrodag_examples("random", n = 100, probability = 0.02, seed = 2026)

| Implementation | Layout seconds | Crossings | Bends | Shared runs | Node-edge collisions |
|---|---:|---:|---:|---:|---:|
| Baseline, without profiler | 77.768 | 260 | 122 | 14 | 0 |
| Cached obstacles and numeric keys, before packing | 25.902 | 260 | 122 | 14 | 0 |
| Cached refinement and independent component packing | 12.062 | 242 | 113 | 14 | 0 |

The intermediate optimization retained identical node coordinates and equal
routed edge data (ignoring data-frame row-name attributes). Packing translates
separate components and changes their local routing context, so its geometry
is intentionally different. It preserves all graph vertices, directed edges
and endpoints. Explicit ranks retain the common-rank layout.

A separate profiled baseline run took 79.242 seconds. Whole-edge refinement
accounted for about 98% of sampled runtime; repeated obstacle construction,
string keys and small data-frame operations were the main costs. Unchanged
obstacles are now cached per edge and refreshed only when that edge changes.

Run the normal benchmark suite to measure the current implementation on your
machine:

    R CMD INSTALL .
    Rscript inst/benchmarks/run_benchmarks.R

The generated metrics.csv and REPORT.md contain their own fresh timings.
Dense connected graphs still require substantial routing work; these changes
do not claim globally optimal routes or uniformly faster performance.
