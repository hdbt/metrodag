# Layout geometry must not change by accident. Speed and refactoring work is
# required to leave every snapshot identical; intentional geometry changes
# update the snapshot in the same commit, with the reason in NEWS.

.geometry_record <- function(layout) {
  n <- layout$nodes[order(layout$nodes$node, method = "radix"), c("node", "x", "y", "layer")]
  e <- layout$edges[order(layout$edges$from, layout$edges$to, layout$edges$segment, method = "radix"),
    c("from", "to", "segment", "x", "y", "xend", "yend", "bend")]
  for (key in c("x", "y")) n[[key]] <- round(n[[key]], 6)
  for (key in c("x", "y", "xend", "yend")) e[[key]] <- round(e[[key]], 6)
  rownames(n) <- NULL; rownames(e) <- NULL
  list(nodes = as.list(n), edges = as.list(e))
}

.geometry_cases <- function() {
  cases <- list()
  for (name in c("chain", "fork", "collider", "diamond", "long_edge", "causal", "dense"))
    cases[[name]] <- function(name) layout_metro_dag(metrodag_examples(name))
  for (example in .metro_gallery_examples) {
    args <- example[intersect(names(example), c("rank", "align"))]
    cases[[paste0("gallery_", example$id)]] <- local({
      graph <- metrodag:::.as_metro_input(example$edges); a <- args
      function(name) do.call(layout_metro_dag, c(list(graph), a))
    })
  }
  for (n in c(12L, 20L, 30L)) for (seed in c(3L, 11L))
    cases[[sprintf("random_%d_%d", n, seed)]] <- local({
      size <- n; s <- seed
      function(name) layout_metro_dag(metrodag_examples("random", n = size,
        probability = 2.5 / size, seed = s))
    })
  cases
}

for (case in names(.geometry_cases())) test_that(paste("layout geometry snapshot:", case), {
  skip_on_cran()
  layout <- .geometry_cases()[[case]](case)
  expect_snapshot_value(.geometry_record(layout), style = "json2")
})
