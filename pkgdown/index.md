# metrodag

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
