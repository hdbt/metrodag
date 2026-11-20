---
title: 'metrodag: publication-oriented causal diagrams in R'
tags:
  - R
  - causal inference
  - directed acyclic graphs
  - epidemiology
  - graph drawing
authors:
  - name: Hamid Bulut
    affiliation: 1
affiliations:
  - name: Independent researcher
    index: 1
date: 6 November 2026
bibliography: paper.bib
---

# Summary

Directed acyclic graphs (DAGs) are the standard way to state causal
assumptions in epidemiology and the health and social sciences
[@Greenland1999; @Shrier2008]. Reporting guidance increasingly asks authors to
publish the DAG behind an analysis [@Tennant2021], but drawing one that is
legible at journal column width usually means placing nodes by hand.
`metrodag` is an R package that lays out a causal DAG automatically and draws
it in a restrained schematic style borrowed from metro maps: arrows follow
vertical, horizontal and diagonal tracks, stop at station rims, and route
around intermediate variables. Labels, arrowheads and clearances are measured
on the output device, so a figure exported at 85 mm is checked at 85 mm.

The package keeps the causal model separate from its presentation. Analysis
roles (exposure, outcome, adjusted, unobserved and selected variables) and
estimand labels annotate a fixed layout, so several analyses of the same graph
can be compared panel by panel without anything moving. Optional integration
with `dagitty` [@Textor2016] imports DAGitty graphs, checks total-effect
adjustment sets and highlights the paths that remain open given an adjustment
set. Graphs can also come from `igraph` [@Csardi2006], `tidygraph`, `ggdag`
[@ggdag], edge tables or adjacency matrices.

# Statement of need

General graph-drawing tools optimise for graphs in general. Layered drawing in
the tradition of @Sugiyama1981, as implemented in Graphviz `dot`
[@Gansner1993], produces compact drawings with few crossings, but its curved
splines and free label placement are not tuned for causal-diagram figures in
manuscripts. Causal-inference packages such as `dagitty` and `ggdag` focus on
the analysis of the graph and leave publication layout to manual coordinates
or force-directed defaults.

`metrodag` targets the step between a correct DAG and a figure a journal will
accept. It provides device-aware labelling and diagnostics
(`diagnose_metro_dag()` reports every station, arrow and label conflict at a
given size), causal-diagram conventions (dashed unobserved variables, adjusted
squares, selection markers, bidirected arcs drawn as latent common causes,
role markers that remain distinct in greyscale), and export at journal column
widths to PDF, SVG, EPS, PNG and TIFF.

# Method

Layout starts from a layered drawing: ranks from longest-path layering or
user-supplied ranks (for example survey waves), long edges split into private
waypoints, and crossings reduced by barycentre or median sweeps with sifting.
Routes are then chosen from a finite family of monotone octilinear candidates
on an integer grid. A sequence of bounded, deterministic refinement passes
straightens arrows, balances fans, reroutes crossing pairs jointly, compacts
ranks and moves sources and low-degree intermediates, accepting a move only
when crossings, shared track, bends and length do not worsen. With automatic
ranks, other layerings (a minimum-span layering and the sink-aligned one) are
tried as starting points and kept only when their drawing is better. Ties are
broken by node name, so reordered but equivalent inputs give identical
drawings. The fifteen passes are named and can be switched off individually
for experiments. Rendering then trims arrows to station rims, spaces
converging arrowheads by their size and places labels with collision checks
on the actual device.

# Evaluation

`inst/benchmarks/evaluation.R` compares `metrodag` with igraph's Sugiyama
layout, Graphviz `dot`, stress majorisation (`graphlayouts`),
Fruchterman-Reingold and, where available, the DAGitty authors' own
coordinates. The corpus is held out from tuning: DAGitty's ten bundled
examples from published studies and thirty seeded random DAGs with 10 to 40
nodes at two densities.

Crossings are counted where one arrow passes to the other side of another;
two arrows that fork from or merge into one station do not cross. On this
measure `metrodag` had fewer crossings than igraph's Sugiyama layout on 23 of
40 graphs, the same on 13 and more on 4. It never had fewer crossings than
Graphviz `dot`, matching it on 18 graphs, and it had more crossings than the
straight-line stress and force-directed layouts on about half of the graphs.
Its octilinear routes have more bends than polyline baselines, its drawings
are of similar compactness, and it is much slower: a median of 3.2 s for the
DAGitty examples against milliseconds for every baseline. Dense graphs remain
hard; the 14-node, 69-arrow DAG of Polzer et al. took 26 s and produced 397
crossings. Of the 539 crossings more than `dot` over the
corpus, 235 already exist after layer ordering, before any arrow is routed;
routing adds the other 304, about half of them in the Polzer DAG.
An ablation that switches off one refinement pass at a time shows that fan
balancing, joint rerouting of crossing pairs and source relaxation help most
consistently, while the restart variants, tuned on the package's own gallery,
do not improve the held-out graphs. The evaluation report in
`inst/benchmarks/EVALUATION.md` gives all numbers and the limitations of these
metrics. We
make no claim that the drawings are easier to read; that requires a
perceptual study.

# Acknowledgements

The development of this package was assisted by AI coding tools.

# References
