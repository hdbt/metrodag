# Publication-default benchmark report

Generated on 2026-11-10 with R 4.6.2, igraph 2.3.4 and ggplot2 4.0.3.

Times are one run on this machine, not a performance study. Timings cover layout
only; they exclude rendering and geometric diagnostics. Canonical SVGs use the
rightward publication defaults. Random-graph rows test layout scaling only.

| Graph | Nodes | Edges | Crossings | Bends | Overlaps | Node–edge collisions | Seconds |
|:---|---:|---:|---:|---:|---:|---:|---:|
| chain | 4 | 3 | 0 | 0 | 0 | 0 | 0.048 |
| fork | 4 | 3 | 0 | 0 | 0 | 0 | 0.165 |
| collider | 4 | 3 | 0 | 0 | 0 | 0 | 0.128 |
| diamond | 4 | 4 | 0 | 4 | 0 | 0 | 0.114 |
| long_edge | 5 | 6 | 0 | 4 | 0 | 0 | 1.063 |
| causal | 6 | 8 | 0 | 4 | 0 | 0 | 1.345 |
| dense | 8 | 16 | 31 | 24 | 9 | 0 | 4.168 |
| random_20 | 20 | 26 | NA | NA | NA | NA | 9.546 |
| random_50 | 50 | 49 | NA | NA | NA | NA | 17.882 |
| random_100 | 100 | 92 | NA | NA | NA | NA | 31.686 |

All canonical examples have zero unrelated node–edge collisions. Long-edge and
causal examples have no overlapping straight runs. The dense graph still has
crossings and shared tracks; its station attachments separate the arrow tips.

Simple source and sink fans now have zero crossings. Whole-edge rerouting
can improve conflicts even when bend counts are unchanged, while retaining
necessary detours and never increasing crossings or shared track length.

A soft symmetry pass balances fans and their middle through-routes only when
spacing, station clearance, crossings, shared track length, bends and total
edge length remain safe. The annotated causal example now has aligned outer
bends and a straight exposure--mediator--outcome chain.

A bounded joint pass reroutes crossing arrow pairs without moving stations.
Each accepted move reduces crossings, permits at most one extra bend, and
does not increase pairwise shared-track length or total line length. The
causal example has zero crossings after this pass. Search
limits keep the extra work finite; remaining crossings may still be avoidable.

A final placement pass straightens balanced fans. It tries at most 24
placements per component, preserves station spacing, clearance and already
straight arrows, and reduces total bends without increasing crossings,
shared-track length or total line length. With explicit ranks a placement
must keep every node in its rank, so only complete ranks move; alignment
groups skip this step. The causal example now has four bends, straight
C-to-X and S-to-X arrows, and retains its aligned outer bends.

A bounded compaction pass removes excess distance between complete ranks
when route and station safeguards permit it. A second pass moves bends
away from short final arrow approaches without adding line length or bends.
See [the compact-layout comparison](COMPACT_LAYOUTS.md) for annotated
before/after examples and the broader 22-graph regression benchmark.

A final bounded placement search compacts branches together while retaining
straight arrows, rank ties, node order and station spacing. Every accepted
drawing is narrower without increasing its causal extent, crossings, shared
track length, bends or line length. See [node arrangements](NODE_ARRANGEMENTS.md)
for the six-example gallery and the 22-graph comparison.

Automatic ranks can then be reassigned to bring a variable nearer its
neighbours. A bounded search rebuilds up to three candidates. Strict
crossing reductions can spend limited line length and extent, with no extra
bends; the help page records the exact guards. Explicit ranks skip this
step. See [automatic ranks](RANK_REFINEMENT.md) for the earlier comparison.

Small components also compare child-directed coordinates and a
reversed initial order, within a separate two-alternative/work budget.
With automatic ranks, a minimum-span layering and the other of the source-
and sink-aligned layerings are tried as starting points too; one is kept
only when the same rule prefers its relaxed drawing.
The [current visual review](VISUAL_REFINEMENT.md) records all six annotated
DAGs, bounded trade-offs, 22-fixture evidence and the remaining limitations.

The metrics CSV includes pairwise shared-track length in layout units,
distinct from overlapping straight-run counts. Random rows omit pairwise
diagnostics from the scaling benchmark and record those fields as NA.

A final bounded pass moves sources towards their children, within their rank
when ranks are explicit, rerouting incident and obstructed arrows together.
It preserves straight arrows, station spacing and route clearance, and does
not increase crossings, bends, shared length, line length or either extent.
Extra transverse reversals are rejected even during subsequent compaction.
Those earlier gains are retained in the current comparison.

A subsequent bounded pass places low-degree intermediates nearer
their neighbours while protecting collinear straight through-chains. It may
exchange a bend between side arrows. Smaller area cannot buy more crossings,
shared length, ink, extents or transverse reversals. One extra bend needs at
least 15% less area and two need 25%, with a lower combined score. Explicit
ranks restrict it to moves within a rank and alignment groups skip it; guards
apply before and after half-grid routing.

After placement, a final half-grid routing pass keeps stations and straight
arrows fixed while shortening shared tracks and detours within strict
crossing, bend, length, extent and clearance guards. It also returns an
arrow between two stations on one straight track to that track when a
bending sibling had claimed it. Changed routes can use
half-unit steps. Tied station ports follow the routes' geometric divergence;
short monotone source transitions avoid returning to a shared track.
See VISUAL_REFINEMENT.md for the current 22-fixture comparison and limits.

A crossing is counted where one arrow passes from one side of another to the
other side (see `?count_crossings`). Arrows that fork from or merge into a
station, touch, or share a section and leave it on the sides they entered
are not crossings, so these counts are lower than the contact-point counts
of earlier reports. Layout refinement still compares contact points, so the
drawings are unchanged. Overlaps count pairs of merged straight runs.

The SVG gallery, including the deliberately dense example, is part of the
review. Publication styling and readable rendering are not evidence of a
globally optimal layout or demonstrated perceptual superiority.

The separate Sugiyama table records coordinate/runtime information only; no
routed-edge comparison is fabricated for that coordinate-only baseline.
