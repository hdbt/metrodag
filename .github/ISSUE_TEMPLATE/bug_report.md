---
name: Bug report
about: A layout, rendering or export problem
labels: bug
---

**Minimal graph and call**

```r
library(metrodag)
edges <- data.frame(from = c("X"), to = c("Y"))
p <- metro_dag(edges)
```

**What happened / what you expected**

**Diagnostics** (if the figure does not fit or labels collide)

```r
diagnose_metro_dag(p, width = 85)$issues
```

**Session information**

```r
sessionInfo()
```
