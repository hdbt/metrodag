test_that("labels that read as another station's are found", {
  nodes <- data.frame(node = c("A", "B", "C"), x = c(0, 10, 20), y = 0)
  radius <- rep(1.5, 3L)
  # Each label sits just above its own station.
  boxes <- rbind(c(-2, 2, 2, 4), c(8, 12, 2, 4), c(18, 22, 2, 4))
  expect_equal(nrow(.metro_label_ambiguity(nodes, boxes, radius)), 0L)
  # A's label drifts over B: nearer B than A, and nearer B than B's own label.
  drifted <- boxes; drifted[1L, ] <- c(8, 12, -4, -2)
  found <- .metro_label_ambiguity(nodes, drifted, radius)
  expect_equal(found$node, "A")
  expect_equal(found$other_node, "B")
  expect_equal(found$relation, "nearer")
  # B's label is far above B while C's label sits beside B.
  far <- boxes; far[2L, ] <- c(8, 12, 12, 14); far[3L, ] <- c(12, 16, -1, 1)
  found <- .metro_label_ambiguity(nodes, far, radius)
  expect_true(any(found$node == "C" & found$other_node == "B"))
  # Unlabelled stations still count as stations a label can be nearer to.
  blank <- boxes; blank[2L, ] <- NA
  blank[1L, ] <- c(8, 12, -4, -2)
  expect_equal(.metro_label_ambiguity(nodes, blank, radius)$other_node, "B")
})

test_that("labels cut off from their station by another arrow are found", {
  nodes <- data.frame(node = c("A", "B", "C"), x = c(0, -10, 10), y = c(0, 2.5, 2.5))
  radius <- rep(1.5, 3L)
  boxes <- rbind(c(-3, 3, 9, 11), NA, NA)
  route <- function(from, to, x, xend) data.frame(edge_id = 1L, from = from, to = to,
    x = x, y = 2.5, xend = xend, yend = 2.5)
  # B -> C runs between station A and its label 7.5 mm above the rim.
  found <- .metro_label_ambiguity(nodes, boxes, radius, edges = route("B", "C", -10, 10),
    line_width = 0.45)
  expect_equal(found$node, "A")
  expect_equal(found$relation, "cut off")
  expect_true(is.na(found$other_node))
  expect_equal(c(found$from, found$to), c("B", "C"))
  # A's own arrow does not cut its label off, nor does one beside the line
  # of sight; without routes only nearness is checked.
  expect_equal(nrow(.metro_label_ambiguity(nodes, boxes, radius, edges = route("A", "C", -10, 10),
    line_width = 0.45)), 0L)
  expect_equal(nrow(.metro_label_ambiguity(nodes, boxes, radius, edges = route("B", "C", 4, 10),
    line_width = 0.45)), 0L)
  expect_equal(nrow(.metro_label_ambiguity(nodes, boxes, radius)), 0L)
  # 3.5 mm from the rim, just beyond an arrow one lane from its station,
  # the label still reads as A's.
  expect_equal(nrow(.metro_label_ambiguity(nodes, boxes - c(0, 0, 4, 4), radius,
    edges = route("B", "C", -10, 10), line_width = 0.45)), 0L)
})

test_that("a cut-off label stays where it is not reported rather than gain a little clearance", {
  # B -> C and H -> I cut off every slot above A (the bounds leave no other
  # room); the slots of the first two rings touch H -> I. In the third ring,
  # 5.6 mm from the rim, the label passes 1 mm from the short arrows F -> G
  # and J -> K; in the fourth, 9.2 mm out and reported cut off, it is clear
  # of them. A little clearance must not move a label to where it reads as
  # a stray.
  nodes <- data.frame(node = "A", label = "A label", x = 0, y = 0)
  route <- function(id, from, to, x, y, xend, yend) data.frame(edge_id = id, from = from, to = to,
    x = x, y = y, xend = xend, yend = yend)
  edges <- rbind(route(1L, "B", "C", -40, 2, 40, 2), route(2L, "H", "I", -40, 5, 40, 5),
    route(3L, "F", "G", 4.4, 5, 4.4, 9), route(4L, "J", "K", -4.4, 5, -4.4, 9))
  placed <- .place_labels(nodes, edges, rbind(c(6, 2)), 1.5, c(-3.8, 3.8, -0.5, 30), line_width = 0.45)
  rim <- .metro_rim_distance(placed$boxes[1L, ], nodes, 1.5)
  expect_gt(rim, 3)
  expect_lte(rim, .metro_cut_off_rim)
  others <- data.frame(node = c("A", "Z"), x = c(0, 30), y = 0)
  expect_equal(nrow(.metro_label_ambiguity(others, rbind(placed$boxes, NA), 1.5, edges = edges,
    line_width = 0.45)), 0L)
})

test_that("labels keep clear of routes they do not touch when a clear slot exists", {
  # An unrelated arrow passes just above the station: the slot above clears
  # its stroke by less than 0.1 mm, the slot below by far more.
  nodes <- data.frame(node = c("A", "U", "V"), label = c("A label", "", ""),
    x = c(0, -30, 30), y = c(0, 6.4, 6.4))
  edges <- data.frame(edge_id = 1L, from = "U", to = "V", x = -30, y = 6.4, xend = 30, yend = 6.4)
  sizes <- rbind(c(10, 3), c(0, 0), c(0, 0))
  placed <- .place_labels(nodes, edges, sizes, 1.5, c(-50, 50, -50, 50), line_width = 0.45)
  expect_lt(placed$centers[1L, 2L], 0)
  expect_gte(.segment_rectangle_distance(edges, placed$boxes[1L, ]) - 0.45 / 2, 1.5)
})

# Labels whose line of sight, from the station centre to the nearest point of
# the label, meets the stroke of an arrow that neither starts nor ends at the
# station, sampled along the segment independently of the renderer's
# detector. Their gaps to the station rim are attached as an attribute.
.cut_off_labels <- function(scene) {
  found <- character(); rim <- numeric()
  for (i in which(!is.na(scene$boxes[, 1L]))) {
    x0 <- scene$nodes$x[i]; y0 <- scene$nodes$y[i]; b <- scene$boxes[i, ]
    t <- seq(0, 1, length.out = 401L)
    px <- x0 + t * (min(max(x0, b[1L]), b[2L]) - x0)
    py <- y0 + t * (min(max(y0, b[3L]), b[4L]) - y0)
    e <- scene$edges[scene$edges$from != scene$nodes$node[i] & scene$edges$to != scene$nodes$node[i], ]
    gap <- vapply(seq_len(nrow(e)), function(k)
      min(.point_segment_distance(px, py, e$x[k], e$y[k], e$xend[k], e$yend[k])), numeric(1))
    if (!any(gap <= scene$line_width / 2)) next
    found <- c(found, scene$nodes$node[i])
    rim <- c(rim, .metro_rim_distance(b, scene$nodes[i, ], rep_len(scene$radius, nrow(scene$nodes))[i]))
  }
  structure(found, rim = rim)
}

# Diagnose `p` and keep the stations, label boxes and routes that the
# label_association check saw on the first drawing (the reflow checks
# nearness alone, without routes).
.diagnose_label_scene <- function(p, ...) {
  scenes <- list()
  detector <- .metro_label_ambiguity
  report <- with_mocked_bindings(diagnose_metro_dag(p, ...),
    .metro_label_ambiguity = function(nodes, boxes, radius, ...) {
      if (!is.null(list(...)$edges))
        scenes[[length(scenes) + 1L]] <<- list(nodes = nodes, boxes = boxes, radius = radius, ...)
      detector(nodes, boxes, radius, ...)
    })
  list(report = report, scene = scenes[[1L]])
}

test_that("a mediator label just beyond its bypass is not reported cut off", {
  skip_on_cran()
  # The vignettes' mediator. At 85 and 120 mm the wrapped label of M does
  # not fit between M and the X -> Y bypass one lane above, and sits just
  # beyond the bypass, in line with M.
  edges <- data.frame(from = c("C", "C", "X", "X", "M"), to = c("X", "Y", "M", "Y", "Y"))
  p <- metro_dag(edges, labels = c(C = "Socioeconomic background",
    X = "Cigarettes per day", M = "Tar deposits", Y = "Lung cancer"), exposure = "X", outcome = "Y")
  for (width in c(85, 120)) {
    checked <- .diagnose_label_scene(p, width = width)
    cut <- .cut_off_labels(checked$scene)
    expect_equal(as.vector(cut), "M", label = paste("cut-off labels at", width))
    expect_lte(attr(cut, "rim"), 7)
    expect_equal(nrow(checked$report$issues), 0L, label = paste("issues at", width))
  }
})

test_that("gallery labels are cut off only without a clear slot, and far ones are reported", {
  skip_on_cran()
  # Before round 2, Diabetes in 05 at 120 mm was also cut off although a slot
  # on its station's side was clear; the others have no slot that is clear
  # of routes, and a label across a route would be worse.
  most <- c("04_statin_events 120" = 3L, "04_statin_events 180" = 1L,
    "05_obesity_admission 120" = 1L, "05_obesity_admission 180" = 1L)
  for (example in .metro_gallery_examples[4:5]) {
    arguments <- example[intersect(names(example),
      c("labels", "exposure", "outcome", "adjusted", "unobserved"))]
    p <- do.call(metro_dag, c(list(graph = example$edges), arguments))
    for (width in c(120, 180)) {
      checked <- .diagnose_label_scene(p, width = width, device = "png", dpi = 150)
      case <- paste(example$id, width)
      cut <- .cut_off_labels(checked$scene)
      issues <- checked$report$issues
      reported <- issues[issues$type == "label_association" & is.na(issues$other_node), ]
      # Those more than 7 mm from their station's rim are reported.
      expect_setequal(unique(reported$node), cut[attr(cut, "rim") > 7])
      expect_lte(length(cut), most[[case]], label = case)
      expect_false(any(reported$from == reported$node | reported$to == reported$node))
      first <- issues[issues$type == "label_association", ][1L, ]
      if (nrow(reported) && is.na(first$other_node)) expect_match(first$message,
        sprintf("The label of %s is cut off from its station by arrow %s -> %s;",
          first$node, first$from, first$to), fixed = TRUE)
      # At the two-column width no label crosses another station's route.
      # Without a clear slot, R's label in 04 lies either beyond U -> O,
      # reported cut off, or beside R across its own arrow R -> A; the font's
      # metrics decide which. Either way only that one label is affected.
      if (width == 180) {
        route <- issues[issues$type == "label_route", ]
        expect_true(all(route$from == route$node | route$to == route$node), label = case)
        expect_lte(length(unique(c(cut, route$node))), most[[case]], label = case)
      }
    }
  }
})

test_that("camelCase labels are wrapped at spaces only where two lines would read as another station's", {
  skip_on_cran()
  skip_if_not_installed("dagitty")
  # In the compact layering of DAGitty's Shrier example, the two-line labels
  # of NeuromuscularFatigue, PreGameProprioception and WarmUpExercises found
  # slots only beside other stations from 85 to 100 mm; on one line each fits
  # beside its own station. Which of them need one line depends on the width
  # and the device, so they are tried one at a time as well as together.
  p <- metro_dag(dagitty::getExample("Shrier"), bidirected = "latent")
  cases <- list(c("png", 85), c("png", 90), c("png", 95), c("png", 100), c("pdf", 90), c("pdf", 95))
  for (case in cases) {
    width <- as.numeric(case[[2L]])
    issues <- if (case[[1L]] == "png") diagnose_metro_dag(p, width = width, device = "png", dpi = 150)$issues else
      diagnose_metro_dag(p, width = width, device = case[[1L]])$issues
    expect_false(any(issues$type == "label_association" & !is.na(issues$other_node)),
      label = paste(case, collapse = " "))
    expect_false(any(issues$type == "label_station"), label = paste(case, collapse = " "))
  }
})

test_that("a label that reads as another station's moves only to a clean slot", {
  # Slot 1 reads as another station's and is the cheapest; slot 2 crosses
  # another station's arrow; slot 3 is clean.
  z <- list(stray = c(TRUE, FALSE, FALSE), clean = c(FALSE, FALSE, TRUE))
  none <- numeric(3L)
  costs <- .label_detour(z, c(1000, 1600, 3000), none, none)
  expect_equal(which.min(costs), 3L)
  expect_true(attr(costs, "detour"))
  # The label stays put rather than cross the arrow when the clean slot costs
  # 2500 more, lies on another label, or is nearer another station than that
  # station's own label.
  expect_equal(which.min(.label_detour(z, c(1000, 1600, 3600), none, none)), 1L)
  expect_equal(which.min(.label_detour(z, c(1000, 1600, 3000), c(0, 0, 0.2), none)), 1L)
  expect_equal(which.min(.label_detour(z, c(1000, 1600, 1000), none, c(0, 0, 2000))), 1L)
  # A label whose cheapest slot reads as its own is left alone, unless the
  # placement passes made the detour and the pair search keeps it.
  costs <- .label_detour(z, c(2000, 1600, 3000), none, none)
  expect_equal(which.min(costs), 2L)
  expect_false(attr(costs, "detour"))
  expect_equal(which.min(.label_detour(z, c(2000, 1600, 3000), none, none, force = TRUE)), 3L)
  expect_false(attr(.label_detour(list(), c(1, 2), c(0, 0), c(0, 0)), "detour"))
})

test_that("a comparison panel keeps a label off its neighbours when its own station is free", {
  skip_on_cran()
  # In the 60 mm panels of gallery 01's comparison at 120 mm, the cheapest
  # slot for "Smoking at baseline" lies 9 mm out, beside H and S. The label
  # goes over its own square instead, as on main, and over no other station.
  example <- .metro_gallery_examples[[1L]]
  roles <- example[intersect(names(example), c("exposure", "outcome", "adjusted", "unobserved"))]
  p <- compare_metro_dags(example$edges, list(Adjusted = roles,
    Crude = list(exposure = example$exposure, outcome = example$outcome)), labels = example$labels)
  issues <- diagnose_metro_dag(p, width = 120, device = "png", dpi = 150)$issues
  expect_false(any(issues$type == "label_association" & issues$node == "D" &
    issues$other_node %in% c("H", "S")))
  station <- issues[issues$type == "label_station" & issues$node == "D", ]
  expect_true(all(station$other_node == "D"))
})

test_that("label placement keeps labels beside their own stations at single-column width", {
  skip_on_cran()
  fan <- data.frame(from = c("Pre", rep("Exposure", 5), paste("Mediator", 1:5)),
    to = c("Exposure", paste("Mediator", 1:5), rep("Outcome", 5)))
  p <- metro_dag(fan, exposure = "Exposure", outcome = "Outcome")
  for (width in c(85, 120)) {
    report <- diagnose_metro_dag(p, width = width, device = "png", dpi = 150)
    expect_true(report$ok)
    expect_false(any(report$issues$type == "label_association"), label = paste(width, "mm"))
  }
})

test_that("titles, captions and legends keep the label margin clear", {
  # Annotations do not move stations: lay the graph out once.
  causal <- layout_metro_dag(metrodag_examples("causal"))
  p <- metro_dag(causal, exposure = "X", outcome = "Y", adjusted = "C",
    estimand = "Total effect") + ggplot2::labs(caption = "Source: example")
  theme <- function(q) ggplot2::theme_get() + .metro_wrap_annotations(q, 85)$theme
  margin_mm <- function(q, key, side) round(grid::convertHeight(
    ggplot2::calc_element(key, theme(q))$margin, "mm", valueOnly = TRUE)[side], 6)
  # The subtitle is next to the panel, so it moves clear of the 5 mm label room;
  # the title above it keeps its own spacing.
  expect_gte(margin_mm(p, "plot.subtitle", 3L), 5.5)
  expect_lt(margin_mm(p, "plot.title", 3L), 5)
  untitled <- metro_dag(causal, estimand = "Total effect", legend = FALSE)
  expect_gte(margin_mm(untitled, "plot.title", 3L), 5.5)
  # A caption next to the panel moves down only when no legend lies between.
  expect_lt(margin_mm(p, "plot.caption", 1L), 5)
  bare <- metro_dag(causal, legend = FALSE) + ggplot2::labs(caption = "Source")
  expect_gte(margin_mm(bare, "plot.caption", 1L), 5.5)
  # Legends keep their spacing on the side that faces the panel.
  for (position in c("bottom", "top", "left", "right")) {
    q <- metro_dag(causal, exposure = "X", outcome = "Y",
      style = metro_style(legend_position = position))
    m <- grid::convertHeight(ggplot2::calc_element("legend.margin", theme(q)), "mm", valueOnly = TRUE)
    side <- c(bottom = 1L, top = 3L, left = 2L, right = 4L)[[position]]
    # With the legend box spacing, at least the 5 mm label margin plus 0.5 mm.
    # ggplot2 3.4 leaves it unset in this theme; the package then counts 0.
    spacing <- ggplot2::calc_element("legend.box.spacing", theme(q))
    spacing <- if (is.null(spacing)) 0 else grid::convertHeight(spacing, "mm", valueOnly = TRUE)
    expect_gte(m[side] + spacing, 5.5 - 1e-9)
  }
  # Headers already keep the label room: the title above them is unchanged.
  ranked <- metro_dag(metrodag_examples("chain"), estimand = "Total effect",
    rank_labels = paste("W", 0:3), legend = FALSE)
  expect_lt(margin_mm(ranked, "plot.title", 3L), 5)
})

test_that("size errors name the dimension that helps and estimate the width", {
  skip_on_cran()
  skip_if_not_installed("dagitty")
  p <- metro_dag(dagitty::getExample("Thoemmes"), bidirected = "latent")
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  error <- tryCatch(save_metro_dag(file, p, width = 85), error = function(e) conditionMessage(e))
  expect_match(error, "Export at least about [0-9]+ mm wide")
  expect_false(grepl("height", error, fixed = TRUE))
  report <- diagnose_metro_dag(p, width = 85)
  expect_false(report$ok)
  expect_named(report$min_size, "width")
  expect_true(all(grepl("export width", report$issues$suggestion[report$issues$severity == "error"])))
  # The estimate is enough: the figure fits at that width.
  expect_true(diagnose_metro_dag(p, width = report$min_size[["width"]])$ok)
  # A figure limited by an explicit height says so.
  tall <- diagnose_metro_dag(p, width = 400, height = 40)
  if (!tall$ok) expect_named(tall$min_size, "height")
  expect_null(diagnose_metro_dag(p, width = 240)$min_size)
})
