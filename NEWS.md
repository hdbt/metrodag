# metrodag 0.3.0

Initial CRAN release.

## Features

- `metro_dag()` draws a causal DAG in one call from an igraph, tidygraph,
  dagitty or ggdag object, an edge matrix or a `from`/`to` data frame;
  `metro_graph()` and `metro_graph_from_adjacency()` build graphs from tables
  and named adjacency matrices.
- Deterministic layered layout (`layout_metro_dag()`) with octilinear routes,
  bounded refinement passes, explicit `rank` and `align` constraints, and
  independent packing of disconnected components. Reorderings of the same
  graph produce identical drawings.
- Analysis annotations (`exposure`, `outcome`, `adjusted`, `unobserved`,
  `unadjusted`, `estimand`) and `node_groups` decorate a fixed layout;
  `compare_metro_dags()` draws aligned panels with one shared legend.
- `metro_style()` returns a style object with the `"publication"`, `"metro"`
  and `"monochrome"` presets and user-adjustable role colours, arrowhead
  geometry, casing width, station scaling and legend labels. `edge_style`
  sets linetype, colour and width for individual arrows.
- Labels, arrowheads and station clearances are measured on the output
  device. `diagnose_metro_dag()` reports fit problems at a given size;
  `save_metro_dag()` exports PDF, SVG, PNG, TIFF and EPS with column-width
  presets, automatic height and atomic file replacement.
- `layout_metrics()` and `count_crossings()` quantify crossings, bends,
  shared tracks, lengths and node/edge collisions.
- Optional `check_metro_adjustment()` tests a total-effect adjustment set with
  dagitty.

- Causal-diagram conventions: `selected` marks variables conditioned on by
  selection (double-outlined squares, imported from DAGitty `[selected]`);
  `bidirected = "latent"` draws DAGitty `A <-> B` arcs as unlabelled
  unobserved common causes; `highlight_paths()` emphasises the arrows on
  open, biasing (open, non-causal and not through another exposure) or causal
  paths given the adjustment and selection set;
  `edge_legend` explains arrow line styles; `rank_labels` adds headers such as
  survey waves; `metro_style(outcome_marker = "ring")` distinguishes the
  outcome by shape, the default in monochrome.

- `as_metro_grob()` returns a grob that wraps titles and places labels at
  draw time, for patchwork and grid layouts; plain `ggplotGrob()` panels now
  keep the plot's default label margins. `on_error = "warn"` draws what fits
  on a small device and reports the problems once; exports stay strict.

## Performance

- Layer ordering is about twice as fast with identical results: sifting
  counts only the crossings between the edges that meet the rank it changes.

- Layout is about twice as fast with identical drawings: the causal example
  takes 1.0 s instead of 2.2 s and a 100-node DAG 32 s instead of 42 s. Crossing
  counts, route merging and obstacle checks are vectorised; a geometry
  snapshot test guards every drawing. See
  <https://github.com/hdbt/metrodag/blob/main/inst/benchmarks/SPEED.md>.

- Layout is about four times as fast again, with identical layouts. Laid out
  one per fresh R session, the 40 graphs of the evaluation corpus take 117 s
  in total instead of 494 s (medians of three runs, each in its own session,
  measured back to back with the previous version): Polzer 25 s instead of
  140 s, Sebastiani 3.9 s instead of 21.9 s and Kampen 3.0 s instead of
  11.5 s. Graphs that took 5 s or more are 2.2 to 5.7 times as fast; graphs
  that took under half a second save at most 0.2 s. The router scores all candidate routes of
  an arrow together, skips stations and track segments out of its reach and
  avoids data-frame overhead in its inner loops, and the crossings-first
  source-placement pass runs only when it could choose a different route.
  Rank-refinement trials, restart variants and layering starts of one
  component reuse a drawing that another of them already made from the same
  start.
- A large layout no longer slows down later layouts in the same R session.
  Route cleanup kept track of candidate routes by name in an environment, and
  R never frees those names: one layout of Polzer left about 88,000 of them
  (50 MB) behind, after which Kampen took 18 to 21 s instead of about 12 s.
- `save_metro_dag()`, `diagnose_metro_dag()` and `print()` build the plot
  fewer times: per export, a single plot is built twice instead of six times
  (four without a legend) and a two-panel comparison six times instead of
  fourteen. An export with an explicit `height` no longer renders a blank
  preflight page, and one with an automatic height draws the content prepared
  while measuring it. Saving or diagnosing a single plot takes 32 to 60% less
  time (16 to 48% less for PNG at the default 600 dpi; the van Kampen figure
  at 180 mm saves as SVG in 0.53 s instead of 1.04 s), a comparison 30 to 50%
  less, and `print()` 8 to 31% less. The output is the same, with two
  exceptions: a layer that draws random numbers, such as `geom_jitter()`
  without a `seed`, draws different ones, and warnings and messages from
  added layers are repeated fewer times (twice per export instead of six
  times, for example).

## Layout

- With automatic ranks, `layout_metro_dag()` also starts from other
  layerings: a minimum-span layering (each node moves to the end of its legal
  rank range on the side with more arrows, until the total arrow span stops
  falling) and the other of the source- and sink-aligned layerings. Each start
  is screened like the existing restarts; the best is then rank-refined and
  relaxed, and kept only when the restart rule still prefers it to the relaxed
  default drawing. On the evaluation corpus this removes the crossing of
  DAGitty's Shrier example and two crossings of a random DAG. The comparison
  is made before the final half-grid pass, and a crossing reduction keeps the
  10% extent allowance measured from the unrelaxed start, so the result can
  have more shared track or a larger area than the default drawing would: a
  random 12-node DAG trades 1.2 units of shared track for one crossing, and a
  random 25-node DAG grows by half its area for five fewer crossings and four
  fewer bends. A DAG where a layering start wins can take up to three times as
  long to lay out.
- The final half-grid routing pass now runs on drawings up to
  `2 * route steps * arrows = 400000` (previously 12000). This removes shared
  track on mid-size graphs such as van Kampen's DAG and DAGitty's Sebastiani
  example (bends 100 to 91, shared track 117 to 48, area 12% smaller), without
  adding crossings or bends.
- The half-grid pass also returns an arrow whose two stations lie on one
  vertical or diagonal track to that track when a sibling claimed it first,
  and reroutes, crossings first, the bending arrows that share it. Five- and
  nine-mediator fans now draw their exposure-mediator-outcome chain straight on
  the station line: with five mediators, 0 crossings and 12 bends instead of 4
  and 14; with nine, 11 crossings instead of 21. An eleven-mediator fan is
  unchanged. Van Kampen's DAG goes from 35 to 33 bends; one random 15-node DAG
  keeps its crossings and bends but draws about 3% longer lines in a 5% larger
  area. Because this attempt roughly doubles the pass, it is made only up to
  `2 * route steps * arrows = 250000`.
- Explicit ranks (`rank` without `align`) no longer switch off fan
  straightening, the alternative starting points or the source and intermediate
  placement passes. These now run with moves that keep every node in its
  supplied rank; rank reassignment and the other layerings stay off, and
  `align` keeps the previous behaviour. Re-running seven test graphs with their
  own automatic layers as `rank` gives 34 crossings and 127 bends in total
  instead of 37 and 144, and no graph is worse on either. As for automatic
  ranks, a start may spend up to 5% more line length and 10% more of either
  extent to remove crossings. Ranked layouts take about twice as long as
  before on these graphs (up to about six times on small ones), and some now
  take longer than the automatic layout of the same graph. `?layout_metro_dag` documents what explicit ranks constrain.
- `layout$parameters$stages` records, for each refinement stage, whether it
  ran, was skipped or partly skipped by its work budget, was not applicable or
  was disabled; `print()` lists the stages a work budget skipped. The
  evaluation report,
  <https://github.com/hdbt/metrodag/blob/main/inst/benchmarks/EVALUATION.md>,
  now reports how often each stage ran on the evaluation corpus, and how many
  of the crossings already arise in the layer ordering, before any arrow is
  routed.
- Over the 40-graph evaluation corpus, crossings fall from 1238 to 1229 and
  bends from 1635 to 1611. These passes made layout 17% slower when they were
  added; the speed-ups under Performance more than make up for it.

## Rendering

- Labels stay beside their own stations. Placement now prefers touching a
  route (labels are drawn on a casing) over a slot nearer another station,
  and `diagnose_metro_dag()` reports any remaining `label_association`
  problems. Across the gallery, five DAGitty examples and a mediator fan,
  diagnosed as PNG at 300 dpi, labels that read as another station's fell
  from 25 to 7 of 76 at 85 mm, from 9 to 0 of 98 at 120 mm and from 2 to 0 of
  110 at 180 mm. Labels touching a route rose from 12 to 21 at 85 mm and from
  2 to 7 at 120 mm, and stayed at 0 at 180 mm. The counts cover the figures
  that fit at each width (9, 11 and 12 of 12, before and after);
  <https://github.com/hdbt/metrodag/blob/main/inst/benchmarks/label_association.R>
  reproduces them. Narrower rewrapping of colliding labels breaks only at
  spaces. With the later placement changes below, the same benchmark has 1
  such label of 76 at 85 mm, while labels touching a route stay at 21, 7 and
  0. With DejaVu Sans as the sans font it has 4 at 85 mm (previously 3).
- Labels are no longer placed on the far side of another arrow, cut off from
  their own station, when a slot on the station's side is clear. Where no
  such slot exists, a cut-off label is still preferred to one drawn across a
  route, and one within 7 mm of its station's rim, which still reads as its
  own, is preferred to one farther out. `diagnose_metro_dag()` reports a label
  left cut off more than 7 mm from its station as a `label_association`
  warning naming the separating arrow ("The label of R is cut off from its
  station by arrow U -> O").
- Labels keep clear of routes they do not touch. A slot within 1.5 mm of a
  route costs more the nearer it is, a third as much for the station's own
  routes, and slots more than 3 mm from the station rim cost more too, so this
  moves a label only a little. Over 414 labels (the gallery, five DAGitty
  examples, a mediator fan and two random graphs at 85, 120 and 180 mm, PNG
  at 150 dpi), labels within 0.6 mm of a route they do not touch fell from 51
  to 20, cut-off labels from 36 to 30 and labels across another station's
  arrow from 25 to 21.
- Labels stay beside their own stations on the more compact round 2 layouts.
  A label whose cheapest slot lies more than 3 mm from its station and nearer
  another station moves only to a slot that reads as its own and touches no
  other station or arrow, if one costs little more; otherwise it stays where
  it is. The fallback reflow of colliding labels now also counts labels that
  read as another station's, at the weight of four route crossings and less
  than covering a station. On the 414 labels, those nearer another station at
  85 mm fell from 7 to 5. In `compare_metro_dags()` panels of the gallery at
  120 mm (60 mm per panel), drawn from the same layouts by the previous
  renderer and this one, they fell from 61 to 57, while labels over another
  station rose from 18 to 22 rows.
- Labels without spaces, such as DAGitty's `IntraGameProprioception`, also
  wrap at camelCase boundaries. A number stays with the letters before it and
  with lowercase letters after it (`Covid19|Severity`,
  `Exposure3rd|Trimester`). Where two lines would put a label beside another
  station, the reflow tries it on one line. Over 16 figures with camelCase
  names at 85, 120 and 180 mm, labels on a route fell from 119 to 67 and labels
  nearer another station from 16 to 10. DAGitty's Shrier example has no label
  nearer another station from 80 to 120 mm on PNG, SVG or PDF; the previous
  version had 10 over those 28 drawings (with DejaVu Sans, 2 against 9).
- Label placement is faster: printing the six gallery figures to PDF takes
  5.4 s instead of 16.3 s at 85 mm and 3.9 s instead of 8.0 s at 180 mm.
- A title, subtitle or caption next to the drawing, and the legend, keep the
  5 mm margin into which labels may extend; labels no longer overprint the
  adjustment subtitle at single-column width.
- Size errors name the dimension that helps ("Increase the export width")
  instead of suggesting a taller figure that `coord_equal()` cannot use, and
  estimate the size needed ("Export at least about 180 mm wide"), allowing
  for the 220 mm automatic-height limit, an explicit height and `scale`.
  `diagnose_metro_dag()` returns a size that it has checked by rendering as
  `min_size`, for example 180 mm for van Kampen's DAG, which fits from
  155 mm. Panels of compositions get general advice.

- Arrows that fan into a busy station no longer form S-shaped hooks as often.
  Only attachments that are too close together are spread, so an arrow with
  room keeps its bearing; and a displaced arrival joins its port where its
  route has already moved to that side, turning one way only. With the
  arrowhead spacing below, hooked arrows across the gallery, node-groups and
  five DAGitty figures at 180 and 240 mm fall from 64 to 13 of 332 (gallery 32
  to 0); 11 of the 13 are in van Kampen's DAG.
  <https://github.com/hdbt/metrodag/blob/main/inst/benchmarks/arrow_hooks.R>
  reproduces the counts.
- Arrowheads converging on a station are spaced by their size. Neighbouring
  tips keep up to one arrowhead width (`2 * arrow_length * sin(arrow_angle)`)
  of arc, and each pair keeps clear of its own two route widths; arrivals
  already on different octilinear bearings, whose outlines clear at 45
  degrees, keep their ports when no other arrival at the station has to move.
  An arrival moves to a wider port only where its route still reaches it
  without an S-bend, a fold or lost clearance; each moved port is tried before
  it is used. Where such ports cannot keep the heads apart their outlines may
  touch, and only where filled heads would merge are arrows bent further. A
  straight arrival a few degrees off its port tilts its last run (at most 3
  degrees at the head base) instead of an S-bend. Across the gallery, five
  DAGitty examples and a mediator fan at 85 to 240 mm, tip pairs closer than 1
  mm fall from 62 to 0 and hooked arrows from 107 to 56. A few arrows at very
  crowded stations still gain a hook where their heads would otherwise merge:
  on a 93-figure test set, 21 arrows gained a hook and 256 lost one (576 hooks
  instead of 808).
- Large exports join arrows to a shared track up to three grid steps back (at
  least 20 mm, as before), so a 240 or 300 mm figure draws the joins a 180 mm
  one does. Routes wider than 0.7 mm, such as highlighted paths, are smoothed
  into their stations like thin ones.
- `diagnose_metro_dag()` reports a new warning, `arrowhead_overlap`, for each
  station whose arrowheads still overlap, outlines included: typically a
  small station with many arrivals from one side, or wide routes. Enlarge the
  station with `node_size` or `metro_style(station_scaling = "degree")`;
  arrivals from one side may need a much larger station (a six-parent star
  clears from `node_size = 8`). Another export size can clear the warning or
  cause it.
- `diagnose_metro_dag()` also returns `routes`, the drawn arrow shafts in
  millimetres.

## Clearer inputs, messages and diagnostics

- Display labels travel with the layout. Labels given to `metro_dag()`,
  `plot_metro_dag()` or `compare_metro_dags()` are stored in the returned
  layout, so `metro_dag(attr(p, "metrodag_layout"), ...)` and comparisons of
  a reused layout keep them, including in the "Adjusted for:" subtitle.
  `labels = FALSE` hides labels in one plot without clearing them. A named
  `labels` vector changes only the nodes it names; the others keep labels
  imported from ggdag, DAGitty or an igraph `label` attribute. A named factor,
  as read from a node table, works as a character vector does, also as
  `ggdag::dagify(labels =)`.
- Named `rank_labels` are matched to ranks by name (the `rank` values, or
  layer numbers for layouts computed without `rank`) instead of by position.
  The layout keeps the `rank` values named by node.
- The "Adjusted" legend key appears only when a station is drawn as adjusted,
  in single plots and in comparisons.
- Edge matrices with columns named `from` and `to` are read by name, as data
  frames are, so `as.matrix(df[, c("to", "from")])` no longer reverses every
  arrow. Other two-column matrices are still read by position, except that one
  naming only its first column `to`, or only its second `from`, is rejected.
- Errors name the inputs at fault: the self-loop, repeated arrow or cycle
  (shown as a path such as `A -> B -> C -> A`, with arrows whose removal
  makes the graph acyclic, each on a cycle and, unless the cyclic part is very
  large, none redundant; a single reversed arrow is named alone); missing or
  empty edge endpoints, by row; arrows whose
  ranks do not increase, and nodes missing from `rank` or named in it but
  absent from the graph (such names used to be ignored); unknown names in
  `labels`, roles, `label_nudge`,
  `node_groups`, `edge_style` and `rank_labels`, with a hint when a display
  label was used in place of a node name; and the node and roles of a role
  clash, including roles inherited from a reused layout. Unsupported or empty
  inputs say what was received and what is accepted, with hints for DAGitty
  text and lists. Numeric role values such as `exposure = 1` work, and
  `edge_style` keys may have spaces around the arrow. Node names that contain
  `->` work in `edge_style` keys and in `highlight_paths()`; a key that could
  name two arrows asks for spaces around the arrow or a data frame.
  `highlight_paths()` checks its own `colour` and `linetype`, and
  `colour = NULL` keeps each arrow's colour. A tidygraph
  graph whose node table has columns but no `name` column gets a message that
  its nodes are numbered in row order.
- `metro_dag()` reports unknown arguments as its own, and `title`, `subtitle`
  or `caption` get a hint to use `estimand` or `ggplot2::labs()`. `?metro_dag`
  documents the appearance arguments it passes to `plot_metro_dag()`.
- Label warnings (`label_overlap`, `label_route`, `label_bounds`,
  `label_station`) name the first label and the label, arrow or station it
  meets, and how many labels are affected.
- `compare_metro_dags()` gains `legend = FALSE`. Comparisons that do not fit
  give a figure size ("Export at least about 120 mm wide"), and
  `diagnose_metro_dag()$min_size` is estimated and checked for them, unless
  their panels are too narrow to draw at all. When the
  automatic height was limited to 220 mm, the error says so and gives the
  height to pass.
- `print()` and `grid.draw()`, including `grid.draw()` of a grob from
  `as_metro_grob()` drawn on its own, raise drawing problems after drawing
  returns, so closing the device no longer warns "Killing locked device". A
  knitr chunk keeps the part of a figure that was drawn, and a figure that an
  error stopped entirely leaves no blank image (unless it was printed with
  `vp`, or is a grob with its own viewport or graphical parameters). Drawing
  a figure again elsewhere, or replaying it at another size, reports its
  problems again; grid redrawing the page in place does not.
- The role legend is always drawn above an `edge_legend`, after any legend
  given a lower `order`; their order used to vary between R sessions.
- `?diagnose_metro_dag` lists every issue type with its severity, meaning and
  usual remedy.
- Test files run in parallel. The rank-header test passes on ggplot2 3.4, the
  algorithm vignette and the benchmark report
  (<https://github.com/hdbt/metrodag/blob/main/inst/benchmarks/REPORT.md>)
  state the current budgets and crossing definition, and the README
  (<https://github.com/hdbt/metrodag/blob/main/README.md>) and
  analysis-views vignette document the `selected` role.

## Tests

- The full suite (`NOT_CRAN=true`) takes 8 minutes instead of 26 with
  testthat's two worker processes, and the CRAN selection of 20 files 43 s
  instead of 95 s. Besides the faster layouts, tests that change only
  annotations, styles, labels or export sizes lay a graph out once and pass
  the layout to `metro_dag()`, and where direction is not what a test is
  about, one layout is turned to each direction; a rotation test checks that
  a turned layout equals a fresh one.
- Shared helpers replace duplicated scaffolding (`helper-layouts.R`,
  `helper-stages.R` and `helper-svg.R`), and one test checks that five kinds
  of failed export leave an existing SVG, PDF or PNG file untouched.

## Changes before release

- `layout_metro_dag()` now defaults to `direction = "right"`, matching
  `metro_dag()`. The unused `seed` argument was removed; ties are broken by
  node name.
- The legend reads "Observed" unless an adjustment set was specified, and
  high-degree stations grow at most 1.5 times the base diameter by default
  (`station_scaling = "capped"`).
- Alignment groups now also fix a node that is alone in its rank; previously
  it could be slid off its group's line towards its neighbours.
- Patchwork compositions export completely. `ggsave()` and `save_metro_dag()`
  previously drew only the last MetroDAG panel, because patchwork copies the
  last plot's class. `save_metro_dag()` and `diagnose_metro_dag()` now accept
  compositions (with an explicit `height`) and check every MetroDAG panel as
  strictly as a single figure, reporting problems that patchwork would
  otherwise swallow.
- `highlight_paths()` no longer misses open paths in dense graphs: the path
  search reports when it stops at the new `limit` argument (default 10,000
  instead of a silent 1,000), and whether any open path exists is decided
  exactly. `paths = "biasing"` highlights only open non-causal paths, the path
  table gains a `causal` column, and bidirected arcs read as `X <-> Y`. It
  also works on plots whose `edge_style` was a data frame.
- Nodes drawn for bidirected arcs stay unobserved when `unobserved` is given
  and cannot take another role. Explicit `rank` values are needed only for
  visible variables, and `rank_labels` counts only ranks with visible nodes.
- Rank headers are drawn as axis labels, between the drawing and any title,
  subtitle or legend, instead of over them. They follow `coord_flip()`, no
  longer cause false `station_bounds` errors with `clip = "on"`, and
  `diagnose_metro_dag()` reports headers that overlap. Layouts that pack
  disconnected components side by side now ask for explicit `rank` values
  rather than placing headers out of order.
- Automatic label wrapping keeps words whole ("Socioeconomic" no longer
  becomes "Socioeconomi" / "c") and breaks identifiers after `_`, `.`, `-` or
  `/`.
- `compare_metro_dags()` passes layout arguments such as `rank` and
  `bidirected` to the layout; `check_metro_adjustment()` accepts
  `bidirected`; `layout_metro_dag()` accepts edge tables. Unknown or
  abbreviated arguments are named in the error, and `rank` given to
  `plot_metro_dag()` is no longer taken as `rank_labels`. `edge_style`
  linetypes are validated when the plot is built.
- Works with ggplot2 3.4 again (`legend.key.spacing.x` needs 3.5). The
  geometry snapshot is included in the package build so that `R CMD check`
  compares it, and `citation("metrodag")` gives the release year rather than
  the current year.
- `count_crossings()` and `layout_metrics()` count a crossing only where one
  arrow passes to the other side of another. Two arrows that leave or enter
  the same station and then part, touch, or share a section and leave it on
  the sides they entered are no longer counted, so a star-shaped tree has no
  crossings. The count is the same in every direction. Layout refinement
  still compares contact points, so drawings do not change.
- Layouts no longer depend on the session's collation locale: ties between
  node names are broken in C-locale (radix) order everywhere.
- Compositions made with `wrap_elements()` are checked panel by panel too,
  errors inside them are raised rather than lost, and `as_metro_grob()`
  rejects compositions with a pointer to `wrap_elements()`. Highlighted path
  searches report truncation exactly, and arguments passed on through `...`
  (as by `plot()`, `autoplot()` and `compare_metro_dags()`) are checked by
  name.
- Exports and diagnoses no longer depend on what was exported earlier in the
  same session. R's graphics engine can, at random, give a newly opened device
  the text metrics of one closed earlier: after a 300 dpi PNG export, a PDF or
  EPS export of the same figure was sometimes measured with the PNG's metrics,
  so that its page came out up to 6.7 mm taller, and `diagnose_metro_dag()`
  reported sizes that varied the same way. MetroDAG plots, comparisons and the
  compositions that `save_metro_dag()` and `diagnose_metro_dag()` draw now
  refresh those metrics on their own device before measuring anything, and
  so does `grid.draw()` or `ggplot2::ggsave()` of a composition whose last
  plot is a MetroDAG plot. Not covered are `print()` of a composition,
  `grid.draw()` or `ggsave()` of one ending in another plot, which patchwork
  draws alone, and a plot converted with `ggplot2::ggplotGrob()` (as gridExtra
  and cowplot do); `as_metro_grob()` is covered.
- metrodag requires R 4.1.0 or later.
