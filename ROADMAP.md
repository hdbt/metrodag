# Roadmap

## Implemented through 0.3

- Structured device-specific rendering diagnostics and station-overlap errors.
- Safe rejection of unsupported DAGitty selection assumptions.
- Reversed ggplot scales and explicit estimand inheritance/clearing.
- Cached routing conflicts and independent disconnected-component packing.

- Private waypoints for long edges before crossing reduction.
- Best-sweep retention, local crossing improvements, separated rank compaction.
- Per-rank gaps and occupancy-aware monotone routing candidates.
- Whole-edge simplification to remove unnecessary waypoint staircases.
- Device-aware arrow clipping, separated station ports, wrapped label placement.
- Publication, monochrome and metro styles; one-call plotting and export.
- Node–edge collision diagnostics, grid-point crossing counts and regression checks.
- Aligned analysis comparisons, physical label nudges and device-measured export annotations.
- Directed DAGitty import and optional total-effect adjustment checks.
- ggdag/tidygraph input compatibility, retained display labels, explicit
  node/edge-table and named adjacency-matrix constructors.
- Source/sink fan routing and explicit errors for insufficient arrow clearance.
- Cross-platform package checks, including legacy ggplot2 and SVG export assertions.

## Next: difficult graph families

- Replace the finite candidate router with occupancy-aware search and selective
  rip-up/reroute, especially for dense layers and high-degree shared tracks.
- Add explicit role-aware alignment and refine component packing.
- Add global label/layout co-optimization beyond the existing manual label nudges.
- Evaluate ordered parallel lanes with explicit edge traceability.

## Evaluation

- Compare against igraph Sugiyama, Graphviz dot, and graphlayouts under a declared
  rendering policy; compare exact small-instance solutions where practical.
- Retain bad adversarial examples and test multiple fonts and export sizes.
- Use perceptual path-tracing tasks to evaluate readability separately from
  geometric costs. Make no superiority claim before that evidence exists.
