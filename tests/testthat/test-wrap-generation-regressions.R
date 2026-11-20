test_that("unbroken identifiers wrap without changing labels or graph identities", {
  graph <- metrodag_examples("chain")
  token <- paste(rep("W", 80), collapse = "")
  unicode <- paste(rep("\u00e9", 17), collapse = "")
  labels <- c(A = token, B = paste0(unicode, "\n\nTail"))
  shown <- .display_labels(graph, labels, 7)
  expect_equal(gsub("\n", "", shown[1L], fixed = TRUE), token)
  expect_true(all(nchar(strsplit(shown[1L], "\n", fixed = TRUE)[[1L]]) <= 7L))
  expect_equal(gsub("\n", "", shown[2L], fixed = TRUE), paste0(unicode, "Tail"))
  expect_match(shown[2L], "\n\nTail", fixed = TRUE)
  expect_equal(shown[3:4], c("C", "D"))
  expect_equal(.display_labels(graph, labels, NULL)[1:2], unname(labels))
  p <- metro_dag(graph, labels = labels)
  expect_identical(attr(p, "metrodag_layout")$nodes, layout_metro_dag(graph, direction = "right")$nodes)
  expect_identical(igraph::V(graph)$name, c("A", "B", "C", "D"))
})

test_that("long labels fit exports, large fonts, viewports and comparison panels", {
  skip_if_not_installed("svglite")
  previous <- ggplot2::last_plot()
  file <- tempfile(fileext = ".svg")
  on.exit({ unlink(file); ggplot2::set_last_plot(previous) }, add = TRUE)
  read_label <- function() {
    text <- readLines(file)
    text <- text[grepl("<text", text, fixed = TRUE)]
    x <- as.numeric(sub(".* x='([^']+)'.*", "\\1", text))
    width <- as.numeric(sub(".* textLength='([^']+)px'.*", "\\1", text))
    left <- x - ifelse(grepl("text-anchor='middle'", text, fixed = TRUE), width / 2,
      ifelse(grepl("text-anchor='end'", text, fixed = TRUE), width, 0))
    list(left = left, right = left + width,
      value = sub(".*>([^<]*)</text>.*", "\\1", text))
  }
  token <- paste(rep("W", 80), collapse = "")
  graph <- igraph::make_empty_graph(1, directed = TRUE)
  igraph::V(graph)$name <- "A"
  attribute_graph <- igraph::set_vertex_attr(graph, "label", value = token)
  identifier_graph <- igraph::set_vertex_attr(graph, "name", value = token)
  plots <- list(metro_dag(graph, labels = c(A = token)),
    metro_dag(attribute_graph), metro_dag(identifier_graph))
  for (p in plots) {
    original <- p$layers[[2L]]$geom_params
    report <- diagnose_metro_dag(p, width = 85, height = 100, device = "svg")
    expect_true(report$ok)
    expect_equal(nrow(report$issues), 0L)
    expect_no_warning(save_metro_dag(file, p, width = 85, height = 100))
    label <- read_label()
    expect_equal(paste(label$value, collapse = ""), token)
    expect_gt(length(label$value), 1L)
    expect_true(all(label$left >= -0.1))
    expect_true(all(label$right <= 85 / 25.4 * 72 + 0.1))
    expect_identical(p$layers[[2L]]$geom_params, original)
  }
  large <- metro_dag(graph, labels = c(A = "WWWWWWWWWW"), label_size = 12)
  expect_no_warning(save_metro_dag(file, large, width = 85, height = 100))
  label <- read_label()
  expect_equal(paste(label$value, collapse = ""), "WWWWWWWWWW")
  expect_gt(length(label$value), 1L)
  expect_true(all(label$left >= -0.1 & label$right <= 85 / 25.4 * 72 + 0.1))
  p <- plots[[1L]]
  expect_no_warning(ggplot2::ggsave(file, p, width = 85, height = 100, units = "mm"))
  expect_equal(paste(read_label()$value, collapse = ""), token)
  svglite::svglite(file, width = 170 / 25.4, height = 100 / 25.4)
  tryCatch(expect_no_warning(print(p, vp = grid::viewport(x = 0.75, width = 0.5))),
    finally = grDevices::dev.off())
  label <- read_label()
  expect_equal(paste(label$value, collapse = ""), token)
  expect_true(all(label$left >= 85 / 25.4 * 72 - 0.1 & label$right <= 170 / 25.4 * 72 + 0.1))
  comparison <- compare_metro_dags(graph, list(list(), list()), labels = c(A = token))
  expect_no_warning(save_metro_dag(file, comparison, width = 180, height = 100))
  label <- read_label()
  midpoint <- 90 / 25.4 * 72
  expect_equal(paste(label$value[label$right < midpoint], collapse = ""), token)
  expect_equal(paste(label$value[label$left > midpoint], collapse = ""), token)
  expect_true(all(label$left >= -0.1 & label$right <= 180 / 25.4 * 72 + 0.1))
  manual <- metro_dag(graph, labels = c(A = token), label_wrap = NULL)
  report <- diagnose_metro_dag(manual, width = 85, height = 100, device = "svg")
  expect_true(any(report$issues$type == "label_bounds"))
  expect_equal(manual$layers[[2L]]$geom_params$nodes$label, token)
})

test_that("random examples retain the seeded upper-triangle edge sequence", {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) before <- .Random.seed
  on.exit({
    if (had_seed) assign(".Random.seed", before, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  for (n in c(1L, 8L, 50L)) for (probability in c(0, 0.12, 1)) for (seed in c(-12L, 1L, 42L)) {
    # Reference the published seeded sequence before the memory optimization.
    set.seed(seed)
    candidates <- which(upper.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
    keep <- stats::runif(nrow(candidates)) < probability
    expected <- if (any(keep)) matrix(paste0("v", candidates[keep, , drop = FALSE]), ncol = 2L) else
      matrix(character(), ncol = 2L)
    caller_seed <- .Random.seed
    graph <- metrodag_examples("random", n = n, probability = probability, seed = seed)
    expect_equal(igraph::as_edgelist(graph), expected)
    expect_identical(.Random.seed, caller_seed)
  }
  graph <- metrodag_examples("random", n = 2000L, probability = 0)
  expect_equal(igraph::vcount(graph), 2000L)
  expect_equal(igraph::ecount(graph), 0L)
  expect_identical(igraph::V(graph)$name, paste0("v", seq_len(2000L)))
})

test_that("sparse random examples avoid quadratic temporary allocations", {
  skip_if_not(isTRUE(capabilities("profmem")))
  file <- tempfile()
  on.exit({ utils::Rprofmem(NULL); unlink(file) }, add = TRUE)
  for (probability in c(0, 0.001)) {
    utils::Rprofmem(file)
    graph <- metrodag_examples("random", n = 2000L, probability = probability, seed = 42L)
    utils::Rprofmem(NULL)
    bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(file))))
    expect_lt(max(bytes, na.rm = TRUE), 4 * 1024^2)
    expect_equal(igraph::vcount(graph), 2000L)
    expect_true(igraph::is_dag(graph))
  }
})
