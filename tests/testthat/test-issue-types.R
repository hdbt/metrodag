# Every diagnostics issue type is documented in ?diagnose_metro_dag, and
# every type the package reports is in .metro_issue_types.

# The Rd source of one section, up to the next top-level section.
.test_rd_section <- function(rd, title) {
  text <- paste(as.character(rd), collapse = "")
  start <- regexpr(sprintf("\\section{%s}", title), text, fixed = TRUE)
  if (start < 0L) return(NULL)
  text <- substring(text, start + 1L)
  end <- regexpr("\\\\(section|examples|seealso|keyword)\\{", text)
  if (end > 0L) text <- substr(text, 1L, end)
  text
}

test_that("the help page documents every issue type", {
  rd <- tryCatch(tools::Rd_db("metrodag")[["diagnose_metro_dag.Rd"]], error = function(e) NULL)
  source <- test_path("..", "..", "man", "diagnose_metro_dag.Rd")
  if (is.null(rd) && file.exists(source)) rd <- tools::parse_Rd(source)
  skip_if(is.null(rd), "the help pages of metrodag are not installed")
  section <- .test_rd_section(rd, "Issue types")
  expect_false(is.null(section))
  types <- .metro_issue_types
  expect_false(anyDuplicated(types) > 0L)
  documented <- vapply(types, function(type)
    grepl(sprintf("\\code{%s}", type), section, fixed = TRUE) ||
      grepl(type, section, fixed = TRUE), logical(1))
  expect_true(all(documented), info = paste(types[!documented], collapse = ", "))
  # One list item per type, and nothing else documented as a type.
  rows <- regmatches(section, gregexpr("\\\\item \\\\code\\{[a-z_]+\\} \\((error|warning)\\)", section))[[1L]]
  listed <- sub("^\\\\item \\\\code\\{([a-z_]+)\\}.*$", "\\1", rows)
  expect_setequal(listed, types)
  expect_false(anyDuplicated(listed) > 0L)
  severity <- sub("^.*\\((error|warning)\\)$", "\\1", rows)
  expect_true(all(severity[startsWith(listed, "label_")] == "warning"))
  expect_true(all(severity[startsWith(listed, "edge_") | startsWith(listed, "station_")] == "error"))
  # diagnose_metro_dag() raises no size error: its `height` speaks of min_size.
  tagged <- function(x, tag) Filter(function(y) identical(attr(y, "Rd_tag"), tag), x)
  height <- Filter(function(item) identical(paste(unlist(item[[1L]]), collapse = ""), "height"),
    tagged(tagged(rd, "\\arguments")[[1L]], "\\item"))
  expect_length(height, 1L)
  height <- paste(unlist(height[[1L]][[2L]]), collapse = "")
  expect_match(height, "min_size", fixed = TRUE)
  expect_false(grepl("error", height, fixed = TRUE))
  # The list reads in an 80-column console; Rd2txt does not wrap tables.
  text <- utils::capture.output(tools::Rd2txt(rd, options = list(width = 80, underline_titles = FALSE)))
  expect_lte(max(nchar(text, type = "width")), 80L)
})

test_that("every type reported at small sizes is a documented type", {
  skip_on_cran()
  seen <- character()
  run <- function(plot, ...) {
    report <- diagnose_metro_dag(plot, ...)
    expect_true(all(report$issues$severity %in% c("error", "warning")))
    seen <<- c(seen, report$issues$type)
    invisible(report)
  }
  labels <- c(C = "Socioeconomic background", X = "Cigarette consumption", M = "Tar deposit in lung",
    Y = "Lung cancer risk", U = "Unmeasured inheritance", S = "Occupational hazards")
  run(metro_dag(metrodag_examples("causal"), exposure = "X", outcome = "Y", labels = labels), width = 40)
  run(metro_dag(metrodag_examples("dense"), legend = FALSE), width = 30, height = 30)
  run(metro_dag(igraph::make_empty_graph(12L, directed = TRUE), direction = "down",
    rank = rep(0, 12L), labels = FALSE), width = 40)
  chain <- metro_dag(metrodag_examples("chain"), labels = FALSE, legend = FALSE)
  run(suppressMessages(chain + ggplot2::coord_cartesian(xlim = c(0, 4))), width = 85, height = 55)
  run(plot_metro_dag(layout_metro_dag(metrodag_examples("chain")), rank_labels =
    c("Baseline enrolment", "First follow-up", "Second follow-up", "Final outcome visit")), width = 60)
  run(metro_dag(metrodag_examples("chain"), legend = FALSE,
    label_nudge = list(A = c(8, 0), B = c(-8, 0))), width = 85)
  star <- layout_metro_dag(cbind(paste0("P", 1:15), "Y"))
  run(metro_dag(star, labels = FALSE, legend = FALSE,
    style = metro_style(station_scaling = "degree")), width = 220, height = 200)
  run(metro_dag(star, labels = FALSE, legend = FALSE), width = 180)
  collapsed <- layout_metro_dag(metrodag_examples("chain"), direction = "right")
  b <- collapsed$nodes[collapsed$nodes$node == "B", ]
  collapsed$nodes[collapsed$nodes$node == "A", c("x", "y")] <- b[, c("x", "y")]
  rows <- collapsed$edges$from == "A"
  collapsed$edges$x[rows] <- collapsed$edges$xend[rows] <- b$x
  collapsed$edges$y[rows] <- collapsed$edges$yend[rows] <- b$y
  run(metro_dag(collapsed, labels = FALSE, legend = FALSE), width = 180, height = 120)
  run(compare_metro_dags(metrodag_examples("chain"), list(list(), list()), labels = FALSE), width = 15)
  run(chain + ggplot2::geom_point(ggplot2::aes(x = missing_column, y = 1), data = data.frame(z = 1)),
    width = 85)
  run(chain + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = data.frame(x = NA_real_, y = 1)),
    width = 85)
  seen <- unique(seen)
  expect_true(all(seen %in% .metro_issue_types),
    info = paste(setdiff(seen, .metro_issue_types), collapse = ", "))
  # The battery reaches most types, so a new type is unlikely to go unseen.
  expect_gte(length(intersect(seen, .metro_issue_types)), 12L)
  # The fifteen-parent star above is the case that reports overlapping heads.
  expect_true("arrowhead_overlap" %in% seen)
})
