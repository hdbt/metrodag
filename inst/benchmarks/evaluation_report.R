# Write inst/benchmarks/EVALUATION.md from the evaluation CSVs.
# From the package root: Rscript inst/benchmarks/evaluation_report.R
dir <- file.path("inst", "benchmarks")
base <- utils::read.csv(file.path(dir, "evaluation_baselines.csv"), stringsAsFactors = FALSE)
abl_path <- file.path(dir, "evaluation_ablation.csv")
abl <- if (file.exists(abl_path)) utils::read.csv(abl_path, stringsAsFactors = FALSE) else NULL
stages_path <- file.path(dir, "evaluation_stages.csv")
stage_runs <- if (file.exists(stages_path))
  utils::read.csv(stages_path, stringsAsFactors = FALSE, check.names = FALSE) else NULL

iqr <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (!length(x)) return("n/a")
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  fmt <- paste0("%.", digits, "f")
  sprintf(paste0(fmt, " (", fmt, "-", fmt, ")"), q[2L], q[1L], q[3L])
}
table_md <- function(df) {
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  align <- paste0("|", paste(rep(":--", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1L, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(header, align, rows, "")
}

base$group <- ifelse(base$family == "dagitty example", "DAGitty examples", "Random DAGs")
methods <- unique(base$method)
summary_table <- function(group) {
  s <- base[base$group == group, ]
  do.call(rbind, lapply(methods, function(m) {
    z <- s[s$method == m, ]
    if (!nrow(z)) return(NULL)
    data.frame(Method = m, Graphs = nrow(z), Crossings = iqr(z$crossings), Bends = iqr(z$bends),
      Length = iqr(z$length), Area = iqr(z$area), Seconds = iqr(z$seconds, 2), check.names = FALSE)
  }))
}

wide <- stats::reshape(base[, c("graph", "method", "crossings")], idvar = "graph",
  timevar = "method", direction = "wide")
versus <- do.call(rbind, lapply(setdiff(methods, "metrodag"), function(m) {
  a <- wide[["crossings.metrodag"]]; b <- wide[[paste0("crossings.", m)]]
  keep <- !is.na(a) & !is.na(b)
  data.frame(Baseline = m, Graphs = sum(keep), Fewer = sum(a[keep] < b[keep]),
    Equal = sum(a[keep] == b[keep]), More = sum(a[keep] > b[keep]), check.names = FALSE)
}))

examples <- base[base$family == "dagitty example", ]
per_example <- stats::reshape(examples[, c("graph", "nodes", "edges", "method", "crossings")],
  idvar = c("graph", "nodes", "edges"), timevar = "method", direction = "wide")
names(per_example) <- sub("^crossings\\.", "", names(per_example))
names(per_example)[1:3] <- c("Example", "Nodes", "Arrows")
per_example[is.na(per_example)] <- "-"

# Crossings left by layer ordering, before routing, against the final drawings.
origin_lines <- character()
if (!is.null(stage_runs)) {
  crossings_of <- function(m) base$crossings[base$method == m][match(stage_runs$graph,
    base$graph[base$method == m])]
  routed <- crossings_of("metrodag"); dot <- crossings_of("Graphviz dot")
  ordered <- stage_runs$ordering_crossings
  group <- base$group[base$method == "metrodag"][match(stage_runs$graph, base$graph[base$method == "metrodag"])]
  origin <- do.call(rbind, lapply(c("DAGitty examples", "Random DAGs", "All"), function(g) {
    keep <- if (g == "All") rep(TRUE, length(group)) else group == g
    data.frame(Graphs = g, Count = sum(keep), `Layer ordering` = sum(ordered[keep]),
      metrodag = sum(routed[keep]), `Graphviz dot` = sum(dot[keep], na.rm = TRUE),
      `metrodag above ordering` = sum(routed[keep] > ordered[keep]),
      `metrodag below ordering` = sum(routed[keep] < ordered[keep]), check.names = FALSE)
  }))
  origin_lines <- c("### Where the crossings arise", "",
    "Layer ordering counts the crossings between the layers of the proper graph (with a dummy node wherever an arrow spans several ranks) after the default longest-path layering and barycentre ordering, before any arrow is routed. Routing adds crossings where octilinear tracks cannot follow the ordering, and the later stages and other layerings remove some. Totals of crossings, and the number of graphs on which the final metrodag drawing has more or fewer crossings than its layer ordering:", "",
    table_md(origin),
    sprintf("Over all 40 graphs, the layer ordering already has %d crossings more than Graphviz `dot`'s drawings; routing and refinement add another %d. Polzer alone has %d crossings after layer ordering, %d in the final drawing and %d with `dot`.",
      as.integer(sum(ordered) - sum(dot, na.rm = TRUE)), as.integer(sum(routed) - sum(ordered)),
      as.integer(ordered[stage_runs$graph == "Polzer"]), as.integer(routed[stage_runs$graph == "Polzer"]),
      as.integer(dot[stage_runs$graph == "Polzer"])), "")
}

# Which budgeted refinements each layout actually received.
stage_lines <- character()
if (!is.null(stage_runs)) {
  stage_names <- setdiff(names(stage_runs), c("graph", "ordering_crossings"))
  ablated <- if (!is.null(abl)) unique(abl$graph) else character()
  count <- function(stage, status, rows = rep(TRUE, nrow(stage_runs)))
    sum(stage_runs[[stage]][rows] == status)
  stage_table <- do.call(rbind, lapply(stage_names, function(stage) {
    in_ablation <- stage_runs$graph %in% ablated
    data.frame(Stage = stage, Ran = count(stage, "ran"),
      `Partly skipped` = count(stage, "partly skipped (budget)"),
      `Skipped by budget` = count(stage, "skipped (budget)"),
      `Not applicable` = count(stage, "not applicable"),
      `Ran on ablation graphs` = sprintf("%d of %d", count(stage, "ran", in_ablation),
        sum(in_ablation)), check.names = FALSE)
  }))
  stage_lines <- c("### Refinement stages that ran", "",
    "`layout_metro_dag()` records in `parameters$stages` whether each refinement stage ran or was skipped because its work budget allowed no attempt. A stage is partly skipped when a graph has several components and only some got an attempt. Counts over the 40 graphs, and over the graphs of the ablation below:", "",
    table_md(stage_table))
}

# The prose summary of the ablation table, with its numbers taken from it.
ablation_summary <- function(ablation) {
  labels <- c(straighten = "whole-edge straightening", balance = "fan balancing",
    crossing_pairs = "joint rerouting of crossing pairs", fan_straighten = "fan straightening",
    section_cleanup = "section cleanup", gap_compaction = "gap compaction",
    approach_lengthening = "approach lengthening", arrangement = "arrangement compaction",
    singleton_slide = "the singleton slide", rank_refinement = "rank refinement",
    restart_variants = "the restart variants", layering_variants = "the layering starts",
    source_relaxation = "source relaxation", intermediate_relaxation = "intermediate relaxation",
    half_grid = "the half-grid pass")
  stage <- ablation[["Stage switched off"]]
  name <- function(x) ifelse(x %in% names(labels), labels[x], gsub("_", " ", x, fixed = TRUE))
  join <- function(x) if (length(x) < 2L) x else
    paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
  number <- function(x) as.numeric(sub("%", "", x, fixed = TRUE))
  # "+3" -> "3 more crossings", "-22" -> "22 fewer crossings"
  change <- function(x, noun) {
    v <- number(x)
    if (v == 0) sprintf("no change in %s", noun) else
      sprintf("%s %s %s", format(abs(v)), if (v > 0) "more" else "fewer",
        if (abs(v) == 1) sub("s$", "", noun) else noun)
  }
  area <- function(x) {
    v <- number(x)
    sprintf("%.1f%% %s area", abs(v), if (v >= 0) "more" else "less")
  }
  row <- function(key) ablation[stage == key, , drop = FALSE]
  worse <- ablation[["Graphs worse"]]; better <- ablation[["Graphs better"]]
  helpful <- stage[worse >= 5 & better == 0]
  bends <- number(ablation$Bends); most_bends <- stage[which.max(bends)]
  areas <- number(ablation$Area); most_area <- stage[order(-areas)][1:3]
  straight <- row("straighten"); restarts <- row("restart_variants"); layerings <- row("layering_variants")
  graphs <- function(k) sprintf("%d %s", k, if (k == 1L) "graph" else "graphs")
  # Off-row signs: positive means the drawings are worse without the stage.
  signs <- function(r) sign(number(c(r$Crossings, r$Bends)))
  hurts <- sum(better > worse); helps <- sum(worse > better)
  paste0(sprintf("Most stages help a few graphs each: %d of %d make more graphs better than worse, and %d the reverse. ",
      helps, length(stage), hurts),
    if (length(helpful)) sprintf("Without %s, at least five graphs are worse and none better. ",
      join(name(helpful))),
    sprintf("Without %s the drawings have %s, the largest bend saving of any stage. ",
      name(most_bends), change(ablation$Bends[stage == most_bends], "bends")),
    sprintf("The largest area savings come from %s (%s without them). ",
      join(name(most_area)), join(sprintf("%.1f%%", areas[match(most_area, stage)]))),
    if (nrow(layerings)) sprintf("The layering starts make %s better and %s worse; without them the drawings have %s and %s. ",
      graphs(layerings[["Graphs worse"]]),
      if (layerings[["Graphs better"]] == 0L) "none" else graphs(layerings[["Graphs better"]]),
      change(layerings$Crossings, "crossings"), change(layerings$Bends, "bends")),
    if (nrow(straight)) {
      s <- signs(straight)
      sprintf("Whole-edge straightening %s: without it the drawings have %s%s%s and %s. ",
        if (length(unique(s[s != 0])) > 1L) "is mixed" else if (all(s >= 0)) "helps" else "does not help",
        change(straight$Crossings, "crossings"),
        if (length(unique(s[s != 0])) > 1L) ", but " else ", ", change(straight$Bends, "bends"),
        area(straight$Area))
    },
    if (nrow(restarts)) {
      s <- signs(restarts)
      sprintf("The restart variants, tuned on the gallery DAGs, %s this held-out corpus: without them the drawings have %s and %s. ",
        if (all(s <= 0)) "do not improve" else if (all(s >= 0)) "improve" else "have a mixed effect on",
        change(restarts$Crossings, "crossings"), change(restarts$Bends, "bends"))
    },
    "Time changes are single runs, and switching a stage off can lengthen later stages, so treat the time column as indicative only.")
}

ablation_lines <- "The ablation has not been run yet; see `evaluation.R ablation`."
if (!is.null(abl)) {
  reference <- abl[abl$disabled == "all", ]
  key <- function(d) d[order(d$graph), ]
  reference <- key(reference)
  stages <- setdiff(unique(abl$disabled), "all")
  ablation <- do.call(rbind, lapply(stages, function(stage) {
    z <- key(abl[abl$disabled == stage, ])
    z <- z[match(reference$graph, z$graph), ]
    total <- function(col) sprintf("%+d", as.integer(sum(z[[col]]) - sum(reference[[col]])))
    percent <- function(col) sprintf("%+.1f%%", 100 * (sum(z[[col]]) / sum(reference[[col]]) - 1))
    # Lexicographic: crossings first, then bends.
    worse <- sum(z$crossings > reference$crossings |
      (z$crossings == reference$crossings & z$bends > reference$bends))
    better <- sum(z$crossings < reference$crossings |
      (z$crossings == reference$crossings & z$bends < reference$bends))
    data.frame(`Stage switched off` = stage, Crossings = total("crossings"),
      Bends = total("bends"), Length = percent("length"), Area = percent("area"),
      `Graphs worse` = worse, `Graphs better` = better, Time = percent("seconds"),
      check.names = FALSE)
  }))
  ablation_lines <- c(
    sprintf("The ablation reruns metrodag on %d graphs (the DAGitty examples except Polzer, and twelve random DAGs with 10 or 20 nodes) with one refinement stage switched off at a time. With every stage on, these graphs total %d crossings and %d bends. Each row gives the change in totals when one stage is off; positive numbers mean the drawings got worse without it. A graph is worse if it has more crossings, or equal crossings and more bends.",
      nrow(reference), as.integer(sum(reference$crossings)), as.integer(sum(reference$bends))), "",
    table_md(ablation),
    ablation_summary(ablation), "")
}

machine <- tryCatch(sub("^model name\\s*:\\s*", "", grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1L]),
  error = function(e) "unknown CPU")
lines <- c(
  "# Held-out evaluation", "",
  "Regenerate with `Rscript inst/benchmarks/evaluation.R baselines`, `Rscript inst/benchmarks/evaluation.R ablation` and `Rscript inst/benchmarks/evaluation_report.R` after `R CMD INSTALL .`.",
  sprintf("Results below are from %s on %s.", R.version.string, machine), "",
  "## Corpus", "",
  "The six gallery DAGs were used to tune the refinement passes, so they are excluded. The corpus is:",
  "",
  "- the ten example DAGs bundled with dagitty (`dagitty::getExample()`), including published models from Schipf et al. (2011), Sebastiani et al. (2005), Polzer et al. (2012), Shrier and Platt (2008) and van Kampen (2014); bidirected arcs are drawn as latent common causes;",
  "- thirty seeded random DAGs: 10, 20 and 40 nodes, forward-edge probability 1.5/n and 3/n, seeds 1 to 5.", "",
  "## Methods and metrics", "",
  "Every method is scored with the same functions: crossings with `count_crossings()` on the drawn polylines, counting the places where one arrow passes to the other side of another (two arrows that fork from or merge into one station are not a crossing); bends as direction changes along each polyline; total arrow length and bounding-box area, both scaled by the smallest distance between two stations so that methods with different units are comparable.",
  "Graphviz `dot` draws splines; they are approximated by their control polylines for crossings and length, and bends are not reported for it. Stress majorisation (graphlayouts) and Fruchterman-Reingold use straight lines. DAGitty author coordinates are the layouts stored with the dagitty examples. Timings are single runs that shared the machine with other work, so treat them as indicative.", "",
  "## Results", "",
  "Median (interquartile range) per method.", "",
  "### DAGitty examples", "", table_md(summary_table("DAGitty examples")),
  "### Random DAGs", "", table_md(summary_table("Random DAGs")),
  "### Crossings: metrodag against each baseline", "",
  "Number of graphs on which metrodag has fewer, equal or more crossings than the baseline.", "",
  table_md(versus),
  "### Crossings per DAGitty example", "", table_md(per_example),
  origin_lines, stage_lines,
  "## Ablation", "", ablation_lines,
  "## Limitations", "",
  "- These are geometric measures. They do not show that metrodag drawings are easier to read; that needs a perceptual study, which has not been done.",
  paste0("- On crossings metrodag is behind most baselines: it has more crossings than ",
    paste(sprintf("%s on %d of %d graphs", versus$Baseline, versus$More, versus$Graphs), collapse = ", "),
    ". It has fewer crossings than igraph's Sugiyama layout on ", versus$Fewer[versus$Baseline == "igraph Sugiyama"],
    " graphs and almost never beats another baseline. Straight-line and polyline baselines also need fewer bends, because metrodag restricts arrows to octilinear tracks."),
  "- Bidirected arcs in the DAGitty examples (for example M-bias) are expanded into latent nodes for every method, so node and arrow counts can exceed the original graph's.",
  sprintf("- metrodag is much slower than every baseline. Dense graphs are hard: the 14-node, 69-arrow Polzer example takes %.0f s and has many crossings with every method.",
    base$seconds[base$graph == "Polzer" & base$method == "metrodag"]),
  "- The random DAGs are sparse and small; the corpus does not cover very large graphs.")
writeLines(lines, file.path(dir, "EVALUATION.md"))
cat("wrote", file.path(dir, "EVALUATION.md"), "\n")
