# Reuse of computed layouts across assertions.
#
# Direction is the last step of layout_metro_dag(): .rotate_geometry() turns
# the canonical "down" geometry into the requested direction. A test that needs
# the same graph in several directions lays it out once and rotates it; the
# rotation test in test-layout.R checks that the result is identical to a fresh
# layout in every direction. Annotations, styles, labels and export sizes do
# not change geometry either: lay out once and pass the layout to metro_dag().

.test_directions <- c("down", "up", "right", "left")

# A "down" layout turned to `direction`, exactly as layout_metro_dag() would
# have returned it.
.test_rotate_layout <- function(layout, direction) {
  stopifnot(inherits(layout, "metrodag_layout"), identical(layout$direction, "down"))
  rotated <- .rotate_geometry(layout$nodes, layout$edges, direction)
  layout$nodes <- rotated$nodes
  layout$edges <- rotated$edges
  layout$direction <- direction
  layout
}

# One layout per direction, named by direction, from a single layout run.
.test_layouts_by_direction <- function(graph, ..., directions = .test_directions) {
  down <- layout_metro_dag(graph, direction = "down", ...)
  stats::setNames(lapply(directions, function(d) .test_rotate_layout(down, d)), directions)
}

# A function that computes `make()` on its first call and then returns the
# stored value; `.test_memo(make)` does the same per key, as `make(key)`. Use
# them for fixtures that several tests in a file share, and call them only
# where no binding is mocked, so the stored value is the real one.
.test_once <- function(make) {
  value <- NULL
  made <- FALSE
  function() {
    if (!made) {
      value <<- make()
      made <<- TRUE
    }
    value
  }
}

.test_memo <- function(make) {
  cache <- new.env(parent = emptyenv())
  function(key) {
    name <- paste(key, collapse = "\r")
    if (!exists(name, envir = cache, inherits = FALSE)) assign(name, make(key), envir = cache)
    get(name, envir = cache, inherits = FALSE)
  }
}

# The annotated gallery graphs as igraph objects, in their stored edge order.
.test_gallery_graph <- function(k) {
  igraph::graph_from_edgelist(unname(.metro_gallery_examples[[k]]$edges))
}

.test_gallery_layout_arguments <- c("rank", "align")
.test_gallery_plot_arguments <- c("labels", "exposure", "outcome", "adjusted", "unobserved",
  "unadjusted")

# The layout of gallery example `k`, with its supplied ranks and alignment.
.test_gallery_layout <- function(k, direction = "right") {
  example <- .metro_gallery_examples[[k]]
  do.call(layout_metro_dag, c(list(example$edges, direction = direction),
    example[intersect(names(example), .test_gallery_layout_arguments)]))
}

# Gallery example `k` drawn with its annotations and any further metro_dag()
# arguments. Give `layout` (from .test_gallery_layout(k)) to reuse its
# geometry; the plot is the same as metro_dag() of the example's edges.
.test_gallery_plot <- function(k, ..., layout = NULL) {
  example <- .metro_gallery_examples[[k]]
  annotations <- example[intersect(names(example), .test_gallery_plot_arguments)]
  if (is.null(layout)) return(do.call(metro_dag, c(list(example$edges),
    example[intersect(names(example), .test_gallery_layout_arguments)], annotations, list(...))))
  do.call(metro_dag, c(list(layout), annotations, list(...)))
}

# Whether skip_on_cran() skips, by the rule of the installed testthat.
.test_on_cran <- function()
  inherits(tryCatch(testthat::skip_on_cran(), condition = identity), "skip")
