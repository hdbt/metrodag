# metrodag <img src="man/figures/logo.png" align="right" height="139" alt="metrodag logo" />

**Publication-oriented causal DAGs, without hand-placing every node.**

Turn a graph of assumed causal relationships into a readable diagram. Metrodag
places variables, routes arrows, and adapts labels to the size of your exported
figure. Use it to show one research question, compare analysis views of the
same graph, or prepare figures for a manuscript.

![A smoking and lung cancer DAG with readable labels and marked exposure and outcome.](man/figures/documentation-example.svg)

## Start with a small graph

```r
library(metrodag)

edges <- data.frame(
  from = c("C", "C", "X", "X", "M"),
  to   = c("X", "Y", "M", "Y", "Y")
)
p <- metro_dag(edges, exposure = "X", outcome = "Y", adjusted = "C")
p
save_metro_dag("dag.pdf", p, width = 180)
```

The [getting-started guide](articles/metrodag.html) develops this example into a
labelled smoking and lung cancer diagram, explains the inputs, and checks the
export. Install the development version with
`remotes::install_github("hdbt/metrodag")`.

## How metrodag differs from other DAG tools

- **Layout and drawing are separate.** `layout_metro_dag()` computes the
  geometry; `metro_dag()` and `plot_metro_dag()` draw it. To show several
  analyses of one graph, reuse `attr(p, "metrodag_layout")` instead of
  recomputing the layout.
- **Roles are annotations you supply.** `exposure`, `outcome`, `adjusted`,
  `unobserved` and `selected` are never inferred from the graph, and they never
  move nodes or arrows.
- **`estimand` is a label, not a check.** Call `check_metro_adjustment()`
  (requires dagitty) to test an adjustment set; plotting never runs it.
- **metrodag is not built on ggdag.** dagitty and ggdag objects are accepted as
  input, but metrodag draws its own layers: restyle with `metro_style()` and
  `edge_style` rather than ggdag geoms. Ordinary ggplot2 additions such as
  `ggplot2::labs()` still work.
- **Text is measured on the output device.** `ggplot2::ggsave()` works, but
  `save_metro_dag()` adds journal column widths and automatic height, and
  refuses to write a figure that cannot fit every arrow. Check the figure with
  `diagnose_metro_dag()` at its final width and format.

## Choose a guide

| What you want to do | Read |
|:--|:--|
| Create your first graph | [Getting started](articles/metrodag.html) |
| Show observed and unobserved variables, adjustment, and custom groups | [Variables and analysis views](articles/analysis-views.html) |
| Prepare a figure for a paper | [Publication figures](articles/publication.html) |
| Understand layout decisions and limitations | [Algorithm and constraints](articles/algorithm.html) |
| Look up an argument | [Function reference](reference/index.html) |

## Explore the six annotated examples

The [benchmark report](https://github.com/hdbt/metrodag/blob/main/inst/benchmarks/REPORT.md)
contains six larger diagrams, with PNG previews, SVGs, and runnable R code:

1. Alcohol and coronary heart disease.
2. Air pollution and asthma attacks.
3. Sugary drinks, body fat, and HbA1c.
4. Statin therapy and vascular events.
5. Obesity, hospital admission and heart failure.
6. Blood pressure and creatinine across three waves.

These teaching models also provide a stable gallery for evaluating the layout
algorithm. Their roles and adjustment annotations are illustrative.

## Cite metrodag

Use `citation("metrodag")` in R or copy the BibTeX entry from the
[authors and citation page](authors.html).

## From a graph to a defensible figure

The graph records your causal assumptions; annotations describe the selected
analysis. Drawing a DAG does not establish that an effect is identified.
Optional DAGitty checks assess a total-effect adjustment set under the assumed
graph. Device diagnostics separately check whether the drawing fits its output
size. The guides explain how to use both checks and interpret their limits.
