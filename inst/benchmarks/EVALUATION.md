# Held-out evaluation

Regenerate with `Rscript inst/benchmarks/evaluation.R baselines`, `Rscript inst/benchmarks/evaluation.R ablation` and `Rscript inst/benchmarks/evaluation_report.R` after `R CMD INSTALL .`.
Results below are from R version 4.6.2 (2026-10-29) on Intel(R) Xeon(R) Processor @ 2.10GHz.

## Corpus

The six gallery DAGs were used to tune the refinement passes, so they are excluded. The corpus is:

- the ten example DAGs bundled with dagitty (`dagitty::getExample()`), including published models from Schipf et al. (2011), Sebastiani et al. (2005), Polzer et al. (2012), Shrier and Platt (2008) and van Kampen (2014); bidirected arcs are drawn as latent common causes;
- thirty seeded random DAGs: 10, 20 and 40 nodes, forward-edge probability 1.5/n and 3/n, seeds 1 to 5.

## Methods and metrics

Every method is scored with the same functions: crossings with `count_crossings()` on the drawn polylines, counting the places where one arrow passes to the other side of another (two arrows that fork from or merge into one station are not a crossing); bends as direction changes along each polyline; total arrow length and bounding-box area, both scaled by the smallest distance between two stations so that methods with different units are comparable.
Graphviz `dot` draws splines; they are approximated by their control polylines for crossings and length, and bends are not reported for it. Stress majorisation (graphlayouts) and Fruchterman-Reingold use straight lines. DAGitty author coordinates are the layouts stored with the dagitty examples. Timings are single runs that shared the machine with other work, so treat them as indicative.

## Results

Median (interquartile range) per method.

### DAGitty examples

| Method | Graphs | Crossings | Bends | Length | Area | Seconds |
|:--|:--|:--|:--|:--|:--|:--|
| metrodag | 10 | 0.5 (0.0-5.0) | 10.0 (6.2-29.5) | 43.1 (18.5-104.0) | 18.9 (6.5-70.5) | 3.21 (0.44-3.72) |
| igraph Sugiyama | 10 | 5.0 (0.2-7.8) | 8.0 (3.5-15.5) | 49.3 (20.1-73.4) | 35.5 (12.0-51.4) | 0.00 (0.00-0.00) |
| Graphviz dot | 10 | 0.0 (0.0-3.8) | n/a | 43.7 (21.2-51.5) | 22.6 (8.2-43.5) | 0.01 (0.01-0.02) |
| stress (graphlayouts) | 10 | 0.5 (0.0-6.5) | 0.0 (0.0-0.0) | 22.7 (10.2-43.6) | 20.2 (4.5-32.7) | 0.00 (0.00-0.00) |
| Fruchterman-Reingold | 10 | 0.5 (0.0-5.8) | 0.0 (0.0-0.0) | 22.7 (10.7-52.0) | 27.9 (4.2-36.6) | 0.00 (0.00-0.00) |
| DAGitty author coordinates |  9 | 1.0 (0.0-1.0) | 0.0 (0.0-0.0) | 35.8 (27.1-58.6) | 26.9 (9.3-35.2) | 0.00 (0.00-0.00) |

### Random DAGs

| Method | Graphs | Crossings | Bends | Length | Area | Seconds |
|:--|:--|:--|:--|:--|:--|:--|
| metrodag | 30 | 3.5 (0.0-20.2) | 15.0 (6.2-37.5) | 51.4 (17.0-182.1) | 50.1 (24.1-228.0) | 1.95 (0.85-3.39) |
| igraph Sugiyama | 30 | 6.0 (0.0-39.0) | 6.0 (2.0-16.2) | 47.7 (18.8-128.9) | 38.2 (17.2-109.8) | 0.00 (0.00-0.00) |
| Graphviz dot | 30 | 1.0 (0.0-6.8) | n/a | 47.1 (23.2-85.7) | 53.7 (21.8-123.5) | 0.02 (0.01-0.02) |
| stress (graphlayouts) | 30 | 0.5 (0.0-4.5) | 0.0 (0.0-0.0) | 25.9 (13.0-68.5) | 58.8 (18.0-304.1) | 0.00 (0.00-0.01) |
| Fruchterman-Reingold | 30 | 1.0 (0.0-4.8) | 0.0 (0.0-0.0) | 29.9 (13.8-57.3) | 65.1 (26.3-150.6) | 0.00 (0.00-0.00) |

### Crossings: metrodag against each baseline

Number of graphs on which metrodag has fewer, equal or more crossings than the baseline.

| Baseline | Graphs | Fewer | Equal | More |
|:--|:--|:--|:--|:--|
| igraph Sugiyama | 40 | 23 | 13 |  4 |
| Graphviz dot | 40 |  0 | 18 | 22 |
| stress (graphlayouts) | 40 |  3 | 16 | 21 |
| Fruchterman-Reingold | 40 |  4 | 16 | 20 |
| DAGitty author coordinates |  9 |  2 |  4 |  3 |

### Crossings per DAGitty example

| Example | Nodes | Arrows | metrodag | igraph Sugiyama | Graphviz dot | stress (graphlayouts) | Fruchterman-Reingold | DAGitty author coordinates |
|:--|:--|:--|:--|:--|:--|:--|:--|:--|
| M-bias |  5 |  5 |   0 |   1 |   0 |   0 |   0 | - |
| confounding |  5 |  7 |   0 |   0 |   0 |   0 |   0 | 0 |
| mediator |  4 |  5 |   0 |   0 |   0 |   0 |   0 | 0 |
| paths | 17 | 19 |   0 |   0 |   0 |   0 |   0 | 0 |
| Sebastiani | 36 | 60 |  91 |  89 |  40 |  16 |  15 | 1 |
| Polzer | 14 | 69 | 397 | 294 | 228 | 282 | 284 | 383 |
| Schipf |  7 | 14 |   5 |   8 |   4 |   5 |   5 | 1 |
| Shrier | 13 | 19 |   0 |   6 |   0 |   1 |   1 | 1 |
| Thoemmes | 13 | 14 |   1 |   4 |   0 |   0 |   0 | 1 |
| Kampen | 12 | 24 |   5 |   7 |   3 |   7 |   6 | 10 |

### Where the crossings arise

Layer ordering counts the crossings between the layers of the proper graph (with a dummy node wherever an arrow spans several ranks) after the default longest-path layering and barycentre ordering, before any arrow is routed. Routing adds crossings where octilinear tracks cannot follow the ordering, and the later stages and other layerings remove some. Totals of crossings, and the number of graphs on which the final metrodag drawing has more or fewer crossings than its layer ordering:

| Graphs | Count | Layer ordering | metrodag | Graphviz dot | metrodag above ordering | metrodag below ordering |
|:--|:--|:--|:--|:--|:--|:--|
| DAGitty examples | 10 | 303 |  499 | 275 |  3 | 0 |
| Random DAGs | 30 | 622 |  730 | 415 | 12 | 6 |
| All | 40 | 925 | 1229 | 690 | 15 | 6 |

Over all 40 graphs, the layer ordering already has 235 crossings more than Graphviz `dot`'s drawings; routing and refinement add another 304. Polzer alone has 240 crossings after layer ordering, 397 in the final drawing and 228 with `dot`.

### Refinement stages that ran

`layout_metro_dag()` records in `parameters$stages` whether each refinement stage ran or was skipped because its work budget allowed no attempt. A stage is partly skipped when a graph has several components and only some got an attempt. Counts over the 40 graphs, and over the graphs of the ablation below:

| Stage | Ran | Partly skipped | Skipped by budget | Not applicable | Ran on ablation graphs |
|:--|:--|:--|:--|:--|:--|
| straighten | 40 | 0 |  0 | 0 | 21 of 21 |
| balance | 40 | 0 |  0 | 0 | 21 of 21 |
| crossing_pairs | 40 | 0 |  0 | 0 | 21 of 21 |
| fan_straighten | 40 | 0 |  0 | 0 | 21 of 21 |
| section_cleanup | 40 | 0 |  0 | 0 | 21 of 21 |
| gap_compaction | 40 | 0 |  0 | 0 | 21 of 21 |
| approach_lengthening | 40 | 0 |  0 | 0 | 21 of 21 |
| arrangement | 31 | 1 |  8 | 0 | 19 of 21 |
| singleton_slide | 40 | 0 |  0 | 0 | 21 of 21 |
| rank_refinement | 27 | 4 |  9 | 0 | 18 of 21 |
| restart_variants | 22 | 5 | 13 | 0 | 15 of 21 |
| layering_variants | 25 | 5 | 10 | 0 | 17 of 21 |
| source_relaxation | 35 | 0 |  5 | 0 | 20 of 21 |
| intermediate_relaxation | 33 | 0 |  7 | 0 | 20 of 21 |
| half_grid | 36 | 0 |  4 | 0 | 21 of 21 |

## Ablation

The ablation reruns metrodag on 21 graphs (the DAGitty examples except Polzer, and twelve random DAGs with 10 or 20 nodes) with one refinement stage switched off at a time. With every stage on, these graphs total 160 crossings and 340 bends. Each row gives the change in totals when one stage is off; positive numbers mean the drawings got worse without it. A graph is worse if it has more crossings, or equal crossings and more bends.

| Stage switched off | Crossings | Bends | Length | Area | Graphs worse | Graphs better | Time |
|:--|:--|:--|:--|:--|:--|:--|:--|
| straighten | -22 | +10 | +2.1% | +9.5% | 2 | 4 | +6.4% |
| balance | +9 | +12 | +3.1% | +2.6% | 5 | 0 | -4.5% |
| crossing_pairs | +2 | +10 | +0.6% | +2.5% | 5 | 1 | -5.6% |
| fan_straighten | +2 | +5 | +0.9% | +1.3% | 3 | 0 | +1.0% |
| section_cleanup | +1 | +22 | +0.4% | +7.6% | 3 | 1 | +6.4% |
| gap_compaction | +1 | -10 | +1.2% | +3.0% | 2 | 1 | +16.8% |
| approach_lengthening | +1 | +1 | +1.1% | +1.4% | 1 | 0 | +11.1% |
| arrangement | +5 | +6 | +0.4% | +1.1% | 4 | 3 | -12.3% |
| singleton_slide | +2 | +4 | +1.1% | +1.7% | 3 | 1 | +10.1% |
| rank_refinement | +1 | +2 | +0.6% | +0.7% | 1 | 0 | -7.3% |
| restart_variants | -1 | -3 | -0.1% | -0.3% | 0 | 1 | -12.9% |
| layering_variants | +3 | +2 | +0.6% | +0.5% | 2 | 0 | +8.4% |
| source_relaxation | +4 | +11 | +2.1% | +1.6% | 6 | 0 | +8.1% |
| intermediate_relaxation | +2 | +2 | +1.6% | +2.4% | 2 | 0 | +16.7% |
| half_grid | +1 | +11 | +0.5% | +6.6% | 3 | 0 | +22.9% |

Most stages help a few graphs each: 13 of 15 make more graphs better than worse, and 2 the reverse. Without fan balancing and source relaxation, at least five graphs are worse and none better. Without section cleanup the drawings have 22 more bends, the largest bend saving of any stage. The largest area savings come from whole-edge straightening, section cleanup and the half-grid pass (9.5%, 7.6% and 6.6% without them). The layering starts make 2 graphs better and none worse; without them the drawings have 3 more crossings and 2 more bends. Whole-edge straightening is mixed: without it the drawings have 22 fewer crossings, but 10 more bends and 9.5% more area. The restart variants, tuned on the gallery DAGs, do not improve this held-out corpus: without them the drawings have 1 fewer crossing and 3 fewer bends. Time changes are single runs, and switching a stage off can lengthen later stages, so treat the time column as indicative only.

## Limitations

- These are geometric measures. They do not show that metrodag drawings are easier to read; that needs a perceptual study, which has not been done.
- On crossings metrodag is behind most baselines: it has more crossings than igraph Sugiyama on 4 of 40 graphs, Graphviz dot on 22 of 40 graphs, stress (graphlayouts) on 21 of 40 graphs, Fruchterman-Reingold on 20 of 40 graphs, DAGitty author coordinates on 3 of 9 graphs. It has fewer crossings than igraph's Sugiyama layout on 23 graphs and almost never beats another baseline. Straight-line and polyline baselines also need fewer bends, because metrodag restricts arrows to octilinear tracks.
- Bidirected arcs in the DAGitty examples (for example M-bias) are expanded into latent nodes for every method, so node and arrow counts can exceed the original graph's.
- metrodag is much slower than every baseline. Dense graphs are hard: the 14-node, 69-arrow Polzer example takes 26 s and has many crossings with every method.
- The random DAGs are sparse and small; the corpus does not cover very large graphs.
