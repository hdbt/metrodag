<div class="md-hero">
<div class="md-hero-text">
<p class="md-hero-eyebrow">R package for causal diagrams</p>
<h1 class="md-hero-title">Causal DAGs that are ready for publication.</h1>
<p class="md-hero-lede">metrodag places variables, routes arrows like a metro map, and
sizes every label for the width your journal prints. You don't need to place
nodes by hand.</p>
<div class="md-hero-actions">
<a class="btn md-btn-primary" href="articles/metrodag.html">Get started</a>
<a class="btn md-btn-ghost" href="#gallery">See the gallery</a>
</div>
<pre class="md-hero-install"><code>remotes::install_github("hdbt/metrodag")</code></pre>
</div>
<div class="md-hero-figure">
<img src="gallery/annotated_04_statin_events.svg" alt="A twelve-variable DAG of statin therapy and vascular events, with adjusted, unobserved, exposure and outcome variables marked." />
</div>
</div>

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

## Gallery {#gallery}

Six annotated teaching models, all drawn with the default settings. Click a
figure to open the full-size SVG. Each one comes with runnable R code in the
[benchmark report](https://github.com/hdbt/metrodag/blob/main/inst/benchmarks/REPORT.md).
Their roles and adjustment annotations are illustrative.

<div class="md-gallery">
<div class="md-gallery-item">
<a href="gallery/annotated_01_alcohol_heart.svg"><img src="gallery/annotated_01_alcohol_heart.svg" alt="Alcohol and coronary heart disease" loading="lazy" /><span>Alcohol and coronary heart disease</span></a>
</div>
<div class="md-gallery-item">
<a href="gallery/annotated_02_air_pollution.svg"><img src="gallery/annotated_02_air_pollution.svg" alt="Air pollution and asthma attacks" loading="lazy" /><span>Air pollution and asthma attacks</span></a>
</div>
<div class="md-gallery-item">
<a href="gallery/annotated_03_sugary_drinks.svg"><img src="gallery/annotated_03_sugary_drinks.svg" alt="Sugary drinks, body fat and HbA1c" loading="lazy" /><span>Sugary drinks, body fat and HbA1c</span></a>
</div>
<div class="md-gallery-item">
<a href="gallery/annotated_04_statin_events.svg"><img src="gallery/annotated_04_statin_events.svg" alt="Statin therapy and vascular events" loading="lazy" /><span>Statin therapy and vascular events</span></a>
</div>
<div class="md-gallery-item">
<a href="gallery/annotated_05_obesity_admission.svg"><img src="gallery/annotated_05_obesity_admission.svg" alt="Obesity, hospital admission and heart failure" loading="lazy" /><span>Obesity, hospital admission and heart failure</span></a>
</div>
<div class="md-gallery-item">
<a href="gallery/annotated_06_longitudinal.svg"><img src="gallery/annotated_06_longitudinal.svg" alt="Blood pressure and creatinine across three waves" loading="lazy" /><span>Blood pressure and creatinine across three waves</span></a>
</div>
</div>

## Choose a guide

| What you want to do | Read |
|:--|:--|
| Create your first graph | [Getting started](articles/metrodag.html) |
| Show observed and unobserved variables, adjustment, and custom groups | [Variables and analysis views](articles/analysis-views.html) |
| Prepare a figure for a paper | [Publication figures](articles/publication.html) |
| Understand layout decisions and limitations | [Algorithm and constraints](articles/algorithm.html) |
| Look up an argument | [Function reference](reference/index.html) |

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

## Cite metrodag

Use `citation("metrodag")` in R or copy the BibTeX entry from the
[authors and citation page](authors.html).

## From a graph to a defensible figure

The graph records your causal assumptions; annotations describe the selected
analysis. Drawing a DAG does not establish that an effect is identified.
Optional DAGitty checks assess a total-effect adjustment set under the assumed
graph. Device diagnostics separately check whether the drawing fits its output
size. The guides explain how to use both checks and interpret their limits.
