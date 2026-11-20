# Regressions found by an adversarial review of compositions, highlighted
# paths, rank headers, labels and size advice.

# The examples are laid out once for the file; the figures below only
# annotate, compose, theme or export them.
.review_dense <- .test_once(function() layout_metro_dag(metrodag_examples("dense")))
.review_causal_down <- .test_once(function() layout_metro_dag(metrodag_examples("causal"),
  direction = "down"))
.review_causal <- .test_once(function() .test_rotate_layout(.review_causal_down(), "right"))

test_that("wrapped and nested compositions are checked panel by panel", {
  skip_on_cran()
  skip_if_not_installed("patchwork")
  a <- metro_dag(metrodag_examples("chain"), legend = FALSE)
  b <- metro_dag(.review_causal(), legend = FALSE)
  dense <- metro_dag(.review_dense(), legend = FALSE)
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  nested <- patchwork::wrap_elements(full = a | dense) + b
  expect_error(save_metro_dag(file, nested, width = 180, height = 60, dpi = 80), "drawing problems")
  expect_false(file.exists(file))
  report <- diagnose_metro_dag(nested, width = 180, height = 60)
  expect_false(report$ok)
  expect_true(diagnose_metro_dag(patchwork::wrap_elements(full = a | dense) + a,
    width = 600, height = 250)$ok)
  expect_equal(.metro_panel_count(nested), 3L)
  expect_silent(save_metro_dag(file, patchwork::wrap_elements(full = a), width = 180, height = 80))
  # Errors other than size problems are raised, not swallowed by patchwork.
  comparison <- compare_metro_dags(.review_causal(), list(A = list(exposure = "X", outcome = "Y"),
    B = list(exposure = "M", outcome = "Y")))
  narrow <- patchwork::wrap_elements(full = as_metro_grob(comparison)) + a +
    patchwork::plot_layout(widths = c(1, 6))
  expect_error(save_metro_dag(file, narrow, width = 100, height = 60), "too narrow")
  expect_false(diagnose_metro_dag(narrow, width = 100, height = 60)$ok)
  broken <- a + ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1))
  expect_error(save_metro_dag(file, patchwork::wrap_elements(full = as_metro_grob(broken)) + a,
    width = 180, height = 60), "missing_column")
  # as_metro_grob() takes single figures only.
  expect_error(as_metro_grob(a | b), "wrap_elements")
})

test_that("ggsave() of a composition leaves the caller's plot as the last plot", {
  skip_on_cran()
  skip_if_not_installed("patchwork")
  a <- metro_dag(metrodag_examples("chain"), legend = FALSE)
  dense <- metro_dag(.review_dense(), legend = FALSE)
  composed <- a | dense
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  suppressWarnings(ggplot2::ggsave(file, composed, width = 360, height = 160, units = "mm"))
  expect_identical(ggplot2::last_plot(), composed)
  expect_error(suppressWarnings(save_metro_dag(file, ggplot2::last_plot(), width = 30, height = 15)),
    "drawing problems")
})

test_that("size advice counts the whole figure, both dimensions and scale", {
  skip_on_cran()
  dense <- metro_dag(.review_dense(), legend = FALSE)
  # An explicit height that also limits gives both dimensions, and they fit.
  report <- diagnose_metro_dag(dense, width = 45, height = 45)
  expect_false(report$ok)
  expect_named(report$min_size, c("width", "height"))
  expect_true(diagnose_metro_dag(dense, width = report$min_size[["width"]],
    height = report$min_size[["height"]])$ok)
  # The estimate refers to the width argument even when `scale` is used, and
  # holds from a much smaller start.
  report <- suppressWarnings(diagnose_metro_dag(dense, width = 30, scale = 0.5))
  expect_true(diagnose_metro_dag(dense, width = report$min_size[["width"]], scale = 0.5)$ok)
  expect_gt(report$min_size[["width"]], diagnose_metro_dag(dense, width = 30)$min_size[["width"]])
  expect_match(tryCatch(suppressWarnings(save_metro_dag(tempfile(fileext = ".pdf"), dense,
    width = 30, scale = 0.5)), error = conditionMessage), "Export at least about [0-9]+ mm wide")
  # Large shortfalls are estimated, not capped.
  random <- metro_dag(metrodag_examples("random", n = 12, probability = 0.4, seed = 3))
  report <- diagnose_metro_dag(random, width = 60)
  expect_true(diagnose_metro_dag(random, width = report$min_size[["width"]])$ok)
  # Panels inside a composition get generic advice and no estimate.
  skip_if_not_installed("patchwork")
  p <- metro_dag(.review_causal(), legend = FALSE)
  cell <- function() patchwork::wrap_elements(full = as_metro_grob(p))
  report <- diagnose_metro_dag(cell() | cell() | cell(), width = 120, height = 100)
  expect_false(report$ok)
  expect_null(report$min_size)
  expect_false(any(grepl("Export at least", report$issues$message)))
})

test_that("only DAGitty bidirected arcs create hidden nodes", {
  g <- metro_graph(data.frame(name = c("X", "M", "Y", "C"), hidden = c(FALSE, TRUE, FALSE, FALSE)),
    data.frame(from = c("X", "M", "C", "C"), to = c("M", "Y", "X", "Y")))
  p <- metro_dag(g, exposure = "X", outcome = "Y", adjusted = "M")
  expect_equal(p$layers[[2L]]$geom_params$nodes$label, c("X", "M", "Y", "C"))
  expect_length(.metro_hidden_nodes(attr(p, "metrodag_layout")$graph), 0L)
  skip_if_not_installed("dagitty")
  expect_error(layout_metro_dag(dagitty::dagitty("dag { X -> Y; X <-> Y }"), bidirected = "latent",
    rank = c(X = 0, Y = 1, .latent_X_Y = NA)), "finite rank")
})

test_that("highlight_paths reports truncation, limits and proper paths exactly", {
  skip_if_not_installed("dagitty")
  edges <- data.frame(from = c("X", "C", "C"), to = c("Y", "X", "Y"))
  p <- metro_dag(edges, exposure = "X", outcome = "Y", adjusted = "C")
  # Exactly `limit` paths is a complete search.
  complete <- attr(highlight_paths(p, paths = "all", limit = 2), "metrodag_paths")
  expect_equal(nrow(complete), 2L)
  expect_false(attr(complete, "truncated"))
  expect_warning(cut <- highlight_paths(p, paths = "all", limit = 1), "stopped at `limit` = 1")
  expect_true(attr(attr(cut, "metrodag_paths"), "truncated"))
  # A truncated biasing search says what it did not check.
  expect_warning(highlight_paths(p, paths = "biasing", limit = 1), "found no open biasing paths among those listed")
  expect_error(highlight_paths(p, limit = 0.5), "limit")
  expect_error(highlight_paths(p, limit = 2.5), "limit")
  # With several exposures, a path through another exposure is not biasing.
  joint <- metro_dag(data.frame(from = c("X1", "X1", "X2"), to = c("X2", "Y", "Y")),
    exposure = c("X1", "X2"), outcome = "Y", unadjusted = TRUE)
  expect_warning(h <- highlight_paths(joint, paths = "biasing"), "No open biasing paths")
  expect_false(any(attr(h, "metrodag_paths")$highlighted))
})

test_that("hex linetypes with zero dash lengths are rejected up front", {
  expect_error(metro_dag(.review_causal(), edge_style = list("X->M" = list(linetype = "40"))),
    "digits 1-F")
  expect_error(metro_dag(.review_causal(), edge_style = list("X->M" = list(linetype = "44")),
    edge_legend = c("0F" = "Odd")), "edge_legend")
})

test_that("rank headers survive added scales and themes and follow the margin", {
  skip_if_not_installed("svglite")
  p <- metro_dag(.review_causal(), rank_labels = paste("Wave", 1:5))
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  texts <- function(q) {
    save_metro_dag(file, q, width = 120)
    lines <- readLines(file, warn = FALSE)
    c(headers = any(grepl(">Wave 3<", lines, fixed = TRUE)), numbers = any(grepl(">-?[0-9.]+<", lines)))
  }
  for (q in list(suppressMessages(p + ggplot2::scale_y_reverse()),
      suppressMessages(p + ggplot2::scale_x_reverse()),
      suppressMessages(p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 3))),
      suppressMessages(p + ggplot2::coord_flip()),
      metro_dag(.review_causal_down(), rank_labels = paste("Wave", 1:5))))
    expect_identical(texts(q), c(headers = TRUE, numbers = FALSE))
  # The overlap check measures the drawn headers, and skips hidden ones.
  big <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(size = 18))
  expect_true(any(diagnose_metro_dag(big, width = 85)$issues$type == "rank_header_overlap"))
  expect_false(any(diagnose_metro_dag(p + ggplot2::theme_void(), width = 85)$issues$type == "rank_header_overlap"))
  # Headers keep at least the plot margin from the panel.
  wide <- p + ggplot2::theme(plot.margin = ggplot2::margin(12, 12, 12, 12, "mm"))
  theme <- ggplot2::theme_get() + .metro_wrap_annotations(wide, 85)$theme
  expect_gte(grid::convertHeight(ggplot2::calc_element("axis.text.x.top", theme)$margin,
    "mm", valueOnly = TRUE)[3L], 12.5 - 1e-9)
  # A caption keeps the label room when the legend is switched off by guides().
  captioned <- metro_dag(.review_causal(), exposure = "X", outcome = "Y") +
    ggplot2::labs(caption = "Source") + ggplot2::guides(shape = "none")
  theme <- ggplot2::theme_get() + .metro_wrap_annotations(captioned, 85)$theme
  expect_gte(grid::convertHeight(ggplot2::calc_element("plot.caption", theme)$margin,
    "mm", valueOnly = TRUE)[1L], 5.5 - 1e-9)
})

test_that("labels: long identifier parts, square stations and suggestions", {
  expect_equal(.metro_wrap_label("ses_socioeconomicdeprivationindex", 12L),
    "ses_socioeco\nnomicdepriva\ntionindex")
  # A label on the diagonal of a square station is measured to its corner.
  nodes <- data.frame(node = c("A", "B"), x = c(0, 10), y = c(0, 9.4), shape = c(22L, 21L))
  corner <- 2.25 + 0.8 / sqrt(2)
  box <- c(corner, corner + 4, corner, corner + 2)
  expect_equal(nrow(.metro_label_ambiguity(nodes, rbind(box, c(NA, NA, NA, NA)), c(2.25, 1.5))), 0L)
  issue <- .metro_render_report(list(diagnostic_state = (state <- new.env()), limiting = "height"),
    "label_association", "warning", "message")
  expect_match(state$issues[[1L]]$suggestion, "Increase the export height, shorten labels")
})

test_that("arguments passed on through ... are checked by name", {
  layout <- .review_causal()
  expect_error(ggplot2::autoplot(layout, rank = paste0("W", 1:5)), "cannot take `rank`")
  expect_error(plot(layout, label_s = 4), "`label_s`")
  expect_s3_class(ggplot2::autoplot(layout, label_size = 4), "metrodag_plot")
  views <- list(A = list(exposure = "X", outcome = "Y"))
  expect_error(compare_metro_dags(metrodag_examples("causal"), views, sweep = 2),
    "Unknown argument to compare_metro_dags\\(\\): `sweep`")
  expect_error(lapply(list(metrodag_examples("chain")), metro_dag, lay = "coffman"), "`lay`")
})

test_that("header follow-ups: vertical scales, rotated headers, legend room, old legend names", {
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  vertical <- metro_dag(.review_causal_down(), exposure = "X", outcome = "Y",
    rank_labels = paste("Wave", 1:5))
  for (q in list(suppressMessages(vertical + ggplot2::scale_x_reverse()),
      suppressMessages(vertical + ggplot2::xlim(-5, 5)))) {
    save_metro_dag(file, q, width = 120)
    lines <- readLines(file, warn = FALSE)
    expect_true(any(grepl(">Wave 3<", lines, fixed = TRUE)))
    expect_false(any(grepl(">-?[0-9.]+<", lines)))
  }
  # Rotated headers are measured as drawn.
  p <- metro_dag(.review_causal(), rank_labels = paste("Wave", 1:5))
  rotated <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(size = 18, angle = 90))
  expect_false(any(diagnose_metro_dag(rotated, width = 120)$issues$type == "rank_header_overlap"))
  # A legend keeps a plot margin wider than 5 mm clear of the labels.
  wide <- metro_dag(.review_causal(), exposure = "X", outcome = "Y") +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 12, 12, 12, "mm"))
  theme <- ggplot2::theme_get() + .metro_wrap_annotations(wide, 85)$theme
  # ggplot2 3.4 leaves the box spacing unset in this theme (and draws its own
  # default); the package then counts 0, as test-label-association.R does.
  spacing <- ggplot2::calc_element("legend.box.spacing", theme)
  spacing <- if (is.null(spacing)) 0 else grid::convertHeight(spacing, "mm", valueOnly = TRUE)
  facing <- grid::convertHeight(ggplot2::calc_element("legend.margin", theme), "mm", valueOnly = TRUE)[1L]
  expect_gte(spacing + facing, 12.5 - 1e-9)
  # Before ggplot2 3.5 every legend box is named "guide-box".
  captioned <- metro_dag(.review_causal(), exposure = "X", outcome = "Y") +
    ggplot2::labs(caption = "Source")
  table <- ggplot2::ggplotGrob(captioned)
  table$layout$name <- sub("^guide-box-.*", "guide-box", table$layout$name)
  room <- .metro_annotation_room(captioned, ggplot2::theme_get() + captioned$theme, table)
  caption <- ggplot2::calc_element("plot.caption", ggplot2::theme_get() + room$theme)
  expect_lt(grid::convertHeight(caption$margin, "mm", valueOnly = TRUE)[1L], 5)
})

test_that("compare_metro_dags names unnamed and abbreviated arguments correctly", {
  g <- metrodag_examples("causal")
  views <- list(A = list(exposure = "X", outcome = "Y"))
  expect_error(compare_metro_dags(g, views, 2, "right", TRUE, NULL, NULL, "red", label_size = 2),
    "given by name")
  expect_error(compare_metro_dags(g, views, nc = 2), "Unknown argument to compare_metro_dags\\(\\): `nc`\\.$")
  # Paths whose `to` node is also a `from` node still show biasing paths.
  skip_if_not_installed("dagitty")
  p <- metro_dag(data.frame(from = c("K", "K", "X1", "X2"), to = c("X1", "X2", "Y", "Y")))
  h <- highlight_paths(p, from = "X1", to = c("X2", "Y"), paths = "biasing")
  expect_true(any(attr(h, "metrodag_paths")$highlighted))
})
