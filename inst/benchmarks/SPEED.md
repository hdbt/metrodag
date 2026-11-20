# Layout and export speed

Regenerate with `Rscript inst/benchmarks/speed_benchmark.R <label>` after
`R CMD INSTALL .`. Each value is the median of three runs on one machine
(Ubuntu, R 4.6.2); compare labels from the same machine only.

| Graph | Nodes | Edges | Layout before (s) | Layout after (s) | Export before (s) | Export after (s) |
|:--|--:|--:|--:|--:|--:|--:|
| causal | 6 | 8 | 2.23 | 1.03 | 0.64 | 0.63 |
| dense | 8 | 16 | 8.90 | 3.86 | 0.88 | 0.87 |
| random_50 | 50 | 49 | 16.39 | 10.61 | | |
| random_100 | 100 | 92 | 42.07 | 31.74 | | |

"Before" is the package at the start of the speed work; "after" follows the
vectorised crossing count, single-pass route collapse, indexed obstacles and
vectorised clearance checks. Every drawing is unchanged: the geometry
snapshot test covers the canonical examples, the six gallery DAGs and six
seeded random DAGs, and a 3,016-input harness gave identical crossing counts.

## Router and rebuilds

| Graph | Nodes | Edges | Layout before (s) | Layout after (s) | Export before (s) | Export after (s) |
|:--|--:|--:|--:|--:|--:|--:|
| causal | 6 | 8 | 1.14 | 0.55 | 0.31 | 0.19 |
| dense | 8 | 16 | 3.85 | 1.00 | 0.66 | 0.41 |
| random_50 | 50 | 49 | 16.40 | 3.13 | | |
| random_100 | 100 | 92 | 28.12 | 6.06 | | |

"Before" (`router_before`) is the package before this work, "after"
(`router_after`) the package with it. The router scores all candidate routes
of an arrow together, skips stations and track segments out of reach and
reads route columns without data-frame dispatch; the crossings-first
source-placement pass runs only when it could choose a different route; the
rebuilds of one component share the drawings they make from the same start;
and an export builds its plot fewer times. Layouts are identical: 350 layout
cases and 943 replayed calls to the refinement stages gave the same nodes,
edges and parameters, and SVG and PNG exports are byte-identical except for
layers that draw random numbers, such as `geom_jitter()` without a seed.

Over the 40-graph evaluation corpus, with every layout in its own fresh R
session (medians of three, back to back), layout takes 117 s instead of
494 s. Graphs that took 5 s or more are 2.2 to 5.7 times as fast (median
4.2); graphs that took under half a second save at most 0.2 s, because a
fixed cost per layout remains. Timed in one session the previous version was
slower still: route cleanup kept candidate routes by name in an environment,
R never frees those names, and each large layout slowed down the ones after
it (Polzer took 136 to 143 s alone, but 169 and 178 s after a first Polzer
layout in the same session).

## Where the remaining time goes

Over the evaluation corpus, complete pipeline rebuilds from alternative
starting points (rank-refinement trials, restart variants and layering
starts) take 28% of layout time. (The first speed work found about two
thirds for restart variants on its benchmark graphs; restart variants are
what removed bends in gallery DAGs 1, 2 and 5.) Polzer, Sebastiani and the densest random
DAGs run the pipeline only one to five times, so most of their time is one
pass through the refinement stages. Two pruning rules were measured and not
adopted: stopping a rank search after repeated rejections, and screening rank
trials, restarts and layering starts by the crossings of their first routes.
Together they saved 8 to 14% of corpus time and changed no drawing on 72 test
graphs, but none reached the 15% set as the bar; screens with no safety margin
discarded trials that could have won, and a rule that skipped refining a
restart whose unrefined drawing was worse made a test graph worse. Memoising
whole-drawing quality was measured and rejected: 34 of 54 calls were distinct
on the causal example, saving about 2 % of layout time.

CI fails when the causal layout takes more than four times its most recent
recorded median, to catch accidental slowdowns rather than to compare machines.
