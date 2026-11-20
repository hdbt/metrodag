test_that("device reflow starts from original text and retains explicit breaks", {
  skip_if_not_installed("svglite")
  graph <- igraph::make_empty_graph(1L, directed = TRUE)
  igraph::V(graph)$name <- "A"
  label <- "Diuretic exposure: wave 2"
  p <- metro_dag(graph, labels = c(A = label), legend = FALSE)
  original <- p
  expect_identical(p$layers[[2L]]$geom_params$nodes$label, label)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  for (width in c(180, 240)) {
    expect_no_warning(save_metro_dag(file, p, width = width, height = 100))
    svg <- readLines(file)
    text <- svg[grepl("<text", svg, fixed = TRUE)]
    expect_true(any(grepl("wave 2</text>", text, fixed = TRUE)))
    expect_false(any(grepl(">2</text>", text, fixed = TRUE)))
  }
  explicit <- metro_dag(graph, labels = c(A = "Diuretic exposure: wave\n2"), legend = FALSE)
  expect_no_warning(save_metro_dag(file, explicit, width = 240, height = 100))
  expect_true(any(grepl(">2</text>", readLines(file), fixed = TRUE)))
  expect_identical(p, original)
})

test_that("automatic wrapping keeps a numeric orphan with its preceding word when it fits", {
  label <- "Diuretic exposure: wave 2"
  expect_equal(.metro_wrap_label(label, 15L), "Diuretic\nexposure:\nwave 2")
  expect_equal(.metro_wrap_label("Diuretic exposure: wave\n2", 15L), "Diuretic\nexposure: wave\n2")
  expect_equal(.metro_numeric_orphans(c("model number", "3"), function(s) nchar(s) <= 5L),
    c("model number", "3"))
  skip_if_not_installed("svglite")
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  svglite::svglite(file)
  tryCatch({
    gp <- grid::gpar(fontsize = 10)
    width <- grid::convertWidth(grid::grobWidth(grid::textGrob("exposure: wave", gp = gp)),
      "mm", valueOnly = TRUE) + 0.1
    wrapped <- .metro_wrap_text(label, width, gp)
    expect_match(wrapped, "wave 2", fixed = TRUE)
    expect_false(any(strsplit(wrapped, "\n", fixed = TRUE)[[1L]] == "2"))
    expect_match(.metro_wrap_text("Diuretic exposure: wave\n2", width, gp), "wave\n2", fixed = TRUE)
  }, finally = grDevices::dev.off())
  graph <- igraph::make_empty_graph(1L, directed = TRUE)
  igraph::V(graph)$name <- "node"
  p <- metro_dag(graph, labels = c(node = label), label_wrap = 15L, legend = FALSE)
  original <- p
  for (width in c(180, 240)) {
    expect_no_warning(save_metro_dag(file, p, width = width, height = 100))
    svg <- readLines(file)
    expect_true(any(grepl("wave 2</text>", svg, fixed = TRUE)))
    expect_false(any(grepl(">2</text>", svg, fixed = TRUE)))
  }
  expect_identical(p, original)
})

test_that("sliding label candidates clear the gallery's two exposure conflicts", {
  skip_if_not_installed("svglite")
  # Gallery DAGs 2 and 5 with their annotations (helper-gallery.R).
  for (k in c(2L, 5L)) {
    graph <- .metro_gallery_examples[[k]]$edges
    p <- .test_gallery_plot(k)
    for (device in c("svg", "png")) {
      report <- diagnose_metro_dag(p, width = 240, device = device, dpi = 180)
      expect_true(report$ok)
      expect_equal(nrow(report$issues), 0L)
      expect_equal(igraph::ecount(attr(p, "metrodag_layout")$graph), nrow(graph))
    }
  }
})

test_that("automatic wrapping keeps words whole and breaks identifiers at separators", {
  expect_equal(.metro_wrap_label("Socioeconomic background", 12L), "Socioeconomic\nbackground")
  expect_equal(.metro_wrap_label("Socioeconomic deprivation index", 12L),
    "Socioeconomic\ndeprivation\nindex")
  expect_equal(.metro_wrap_label("maternal_education_years", 12L), "maternal_\neducation_\nyears")
  # Only very long tokens without separators are cut by characters.
  expect_equal(.metro_wrap_label(strrep("W", 30L), 12L),
    paste(strrep("W", 12L), strrep("W", 12L), strrep("W", 6L), sep = "\n"))
  expect_equal(.metro_wrap_label(strrep("W", 20L), 12L), strrep("W", 20L))
})

test_that("labels without spaces also break at camelCase boundaries, keeping numbers with their letters", {
  expect_equal(.metro_wrap_label("IntraGameProprioception", 12L), "IntraGame\nProprioception")
  # The reflow's alternative keeps such a name whole.
  expect_equal(.metro_wrap_label("IntraGameProprioception", 12L, camel = FALSE),
    "IntraGameProprioception")
  expect_equal(.metro_wrap_label("maternal_education_years", 12L, camel = FALSE),
    "maternal_\neducation_\nyears")
  expect_equal(.metro_wrap_label("ConnectiveTissueDisorder", 12L), "Connective\nTissue\nDisorder")
  # A number stays with the letters before it, and with lowercase letters
  # after it, so ordinals, units and leading numbers keep their word.
  expect_equal(.metro_wrap_label("PreGameProprioception2", 12L), "PreGame\nProprioception2")
  expect_equal(.metro_wrap_label("Covid19Severity", 12L), "Covid19\nSeverity")
  expect_equal(.metro_wrap_label("smoking2010status", 12L), "smoking2010status")
  expect_equal(.metro_wrap_label("SecondWave2ndDoseVaccination", 12L), "Second\nWave2ndDose\nVaccination")
  expect_equal(.metro_wrap_label("Exposure3rdTrimester", 8L), "Exposure3rd\nTrimester")
  expect_equal(.metro_wrap_label("Age65plusFrailty", 8L), "Age65plus\nFrailty")
  expect_equal(.metro_wrap_label("Dose10mgDaily", 8L), "Dose10mg\nDaily")
  expect_equal(.metro_wrap_label("3DPrintedImplantFailure", 8L), "3DPrinted\nImplant\nFailure")
  expect_equal(.metro_wrap_label("H1N1VaccineUptake", 8L), "H1N1\nVaccine\nUptake")
  # One leading lowercase letter starts no word; boundaries one capital
  # apart are all found.
  expect_equal(.metro_wrap_label("iPhoneUsageMinutesDaily", 8L), "iPhone\nUsage\nMinutes\nDaily")
  expect_equal(.metro_camel_parts("abCdEfGh"), c("ab", "Cd", "Ef", "Gh"))
  # Separators and camelCase combine; identifiers that fit are left alone.
  expect_equal(.metro_wrap_label("tar_depositLevel", 12L), "tar_deposit\nLevel")
  expect_equal(.metro_wrap_label("FitnessLevel", 12L), "FitnessLevel")
  # Labels with spaces are prose: a capital inside a word is no boundary.
  expect_equal(.metro_wrap_label("Daily iPhoneUsageMinutes", 12L), "Daily\niPhoneUsageMinutes")
  skip_if_not_installed("svglite")
  graph <- igraph::make_empty_graph(1L, directed = TRUE)
  igraph::V(graph)$name <- "IntraGameProprioception"
  p <- metro_dag(graph, legend = FALSE)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  expect_no_warning(save_metro_dag(file, p, width = 85, height = 60))
  text <- readLines(file)
  expect_true(any(grepl(">IntraGame</text>", text, fixed = TRUE)))
  expect_true(any(grepl(">Proprioception</text>", text, fixed = TRUE)))
})
