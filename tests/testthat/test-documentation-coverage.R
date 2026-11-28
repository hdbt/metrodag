# The package help page, the README and the pkgdown home page are the summary
# documentation that people and AI assistants read first; pkgdown builds
# llms.txt from the home page. These tests keep their function names and code
# in step with the exported API.

.doc_functions <- function(text) {
  # Function names written as `name()`, excluding qualified calls such as
  # ggplot2::labs().
  found <- regmatches(text, gregexpr("(?<![:\\w.])[A-Za-z][\\w.]*(?=\\(\\))", text, perl = TRUE))
  unique(unlist(found))
}

.expect_documents_api <- function(text, source) {
  exports <- getNamespaceExports("metrodag")
  mentioned <- .doc_functions(text)
  expect_identical(setdiff(mentioned, exports), character(),
    label = paste("functions named in", source, "but not exported"))
  expect_identical(sort(setdiff(exports, mentioned)), character(),
    label = paste("exports missing from", source))
}

# .Rbuildignore drops README.md and pkgdown/ from the source package, so their
# tests skip under R CMD check. GitHub CI also runs this file from the
# repository checkout with METRODAG_SOURCE_TESTS=true, where a missing file is
# an error rather than a skip.
.source_file <- function(...) {
  path <- test_path("..", "..", ...)
  if (!file.exists(path)) {
    if (isTRUE(as.logical(Sys.getenv("METRODAG_SOURCE_TESTS"))))
      stop(file.path(...), " is missing, but METRODAG_SOURCE_TESTS=true ",
        "requires a source checkout", call. = FALSE)
    skip(paste(file.path(...), "is not in the installed package"))
  }
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

.r_chunks <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  starts <- which(lines == "```r")
  ends <- which(lines == "```")
  vapply(starts, function(s) {
    e <- ends[ends > s][1]
    paste(lines[seq(s + 1, e - 1)], collapse = "\n")
  }, character(1))
}

.run_chunks <- function(text) {
  chunks <- .r_chunks(text)
  # Installation and vignette-browsing commands are not part of the workflow.
  chunks <- chunks[!grepl("install|browseVignettes", chunks)]
  expect_gt(length(chunks), 0)
  dir <- tempfile("metrodag-doc-")
  dir.create(dir)
  old <- setwd(dir)
  on.exit({
    setwd(old)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)
  env <- new.env(parent = globalenv())
  for (chunk in chunks)
    expect_no_error(eval(parse(text = chunk), envir = env))
}

test_that("the package help page names every export and nothing else", {
  rd <- tools::Rd_db("metrodag")[["metrodag-package.Rd"]]
  skip_if(is.null(rd), "package help page is not installed")
  text <- paste(capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE))),
    collapse = "\n")
  .expect_documents_api(text, "?metrodag")
})

test_that("the README names every export and nothing else", {
  .expect_documents_api(.source_file("README.md"), "README.md")
})

test_that("the pkgdown home page names only exported functions", {
  text <- .source_file("pkgdown", "index.md")
  expect_identical(setdiff(.doc_functions(text), getNamespaceExports("metrodag")),
    character())
})

test_that("README code runs", {
  skip_on_cran()
  skip_if_not_installed("svglite")
  .run_chunks(.source_file("README.md"))
})

test_that("pkgdown home page code runs", {
  skip_on_cran()
  .run_chunks(.source_file("pkgdown", "index.md"))
})
