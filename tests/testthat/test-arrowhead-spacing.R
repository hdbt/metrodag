# Arrowheads converging on one station keep a physical separation: one head
# width of arc on the port circle where their routes reach it without a hook,
# outlines kept apart or touching otherwise; diagnostics report heads that
# still overlap.

.test_tip_gaps <- function(routes) {
  tips <- routes[!duplicated(paste(routes$panel, routes$from, routes$to), fromLast = TRUE), ]
  gaps <- lapply(split(tips, paste(tips$panel, tips$to)), function(at) {
    if (nrow(at) < 2L) return(numeric())
    d <- as.matrix(stats::dist(at[c("x", "y")]))
    d[upper.tri(d)]
  })
  unlist(gaps, use.names = FALSE)
}

# Per arrival, along its last 10 mm: the sharpest single turn, and the slant
# of its last shaft segment before the head off the nearest octilinear
# direction, when that segment is at least 4 mm long.
.test_arrival_turns <- function(routes) {
  key <- paste(routes$panel, routes$from, routes$to)
  do.call(rbind, lapply(split(routes, factor(key, levels = unique(key))), function(r) {
    r <- r[order(r$point), ]
    p <- cbind(r$x, r$y)
    step <- sqrt(rowSums(diff(p)^2))
    first <- max(1L, which(rev(cumsum(rev(step))) <= 10)[1L] - 1L)
    if (is.na(first)) first <- nrow(p) - 1L
    v <- diff(p[first:nrow(p), , drop = FALSE])
    v <- v[rowSums(v^2) > 1e-12, , drop = FALSE]
    a <- atan2(v[, 2L], v[, 1L])
    sharpest <- if (nrow(v) > 1L) max(abs(atan2(sin(diff(a)), cos(diff(a))))) * 180 / pi else 0
    d <- diff(p); k <- nrow(d) - 1L
    slant <- if (k >= 1L && sqrt(sum(d[k, ]^2)) >= 4) {
      angle <- atan2(d[k, 2L], d[k, 1L]) * 180 / pi
      abs(((angle + 22.5) %% 45) - 22.5)
    } else 0
    data.frame(from = r$from[1L], to = r$to[1L], sharpest = sharpest, slant = slant)
  }))
}

# Layouts shared by the drawing tests: each graph is laid out once, and the
# styles and widths that vary only decorate or export it.
.test_spacing_gallery <- .test_memo(function(k) .test_gallery_layout(as.integer(k)))
.test_spacing_plot <- function(k, ...) .test_gallery_plot(k, ..., layout = .test_spacing_gallery(k))
.test_fan_layout <- .test_once(function() layout_metro_dag(data.frame(
  from = c("Pre", rep("Exposure", 5), paste("Mediator", 1:5)),
  to = c("Exposure", paste("Mediator", 1:5), rep("Outcome", 5)))))
.test_mediator_fan <- function(...) {
  metro_dag(.test_fan_layout(), exposure = "Exposure", outcome = "Outcome", ...)
}
.test_star_layout <- .test_memo(function(k) layout_metro_dag(cbind(paste0("P", seq_len(k)), "Y")))

test_that("arrowheads converging on a station stay a head width apart at every width", {
  skip_on_cran()
  skip_if_not_installed("svglite")
  plots <- list(fan = .test_mediator_fan())
  for (i in c(1L, 3L)) plots[[.metro_gallery_examples[[i]]$id]] <- .test_spacing_plot(i)
  reports <- list()
  for (name in names(plots)) for (width in c(85, 120, 180)) {
    report <- diagnose_metro_dag(plots[[name]], width = width, device = "svg")
    reports[[paste(name, width)]] <- report
    # Five arrivals take 1.3 mm heads, 1.1 mm wide; the narrow fan used to
    # leave tips 0.69 mm apart at every width.
    expect_gte(min(.test_tip_gaps(report$routes)), 1.1, label = paste(name, width))
    expect_false(any(report$issues$type == "arrowhead_overlap"), info = paste(name, width))
  }
  # The five-mediator fan arrives without the hooks its narrow fan drew.
  hooks <- .metro_hook_score(reports[["fan 180"]]$routes)
  expect_false(any(hooks$hook[hooks$to == "Outcome"]))
})

test_that("a through arrow keeps its bearing while tied arrivals move to their own side", {
  rad <- pi / 180
  # A tied pair: the arrow that left the shared track below (turn < 0) can
  # reach any port below it, the through arrow only its room. The first moves
  # a full preferred separation; the through arrow stays.
  reach <- rbind(c(Inf, 0), c(0, 0))
  spread <- function(rel, through, reach, preferred, minimum, floor = minimum, room = 2 * rad,
    lower = -pi / 2, upper = pi / 2) .metro_spread_arrivals(rel, through,
    rep(room, length(rel)), reach, preferred, minimum, floor, rep(lower, length(rel)),
    rep(upper, length(rel)))
  expect_equal(spread(c(0, 0), c(FALSE, TRUE), reach, 0.5, 0.3), c(-0.5, 0), tolerance = 1e-5)
  # It reaches only 0.35: the through arrow takes the rest within its room.
  reach[1L, 1L] <- 0.35
  expect_equal(spread(c(0, 0), c(FALSE, TRUE), reach, 0.5, 0.3), c(-0.35, 2 * rad),
    tolerance = 1e-6)
  # Arrows alone on their bearings 45 degrees apart, with a branch between
  # them that cannot have the preferred 26.6 degrees on either side. Within
  # their room the three ports keep 24.5 degrees: the outlines touch, but no
  # arrow bends.
  rel <- c(-pi / 4, 0, 0)
  three <- function(floor) .metro_spread_arrivals(rel, c(TRUE, FALSE, TRUE), rep(2 * rad, 3L),
    rbind(c(0, 0), c(Inf, 0), c(0, 0)), rep(26.6 * rad, 2L), rep(25.3 * rad, 2L), floor,
    rel - pi / 4, rel + pi / 4)
  fit <- three(rep(20 * rad, 2L))
  expect_null(attr(fit, "bent"))
  expect_equal(fit[c(1L, 3L)] - rel[c(1L, 3L)], c(-2, 2) * rad, tolerance = 1e-6)
  expect_equal(diff(fit), rep(24.5 * rad, 2L), tolerance = 1e-6)
  # Where filled heads would still overlap, through arrows bend only as far
  # as parting their outlines needs, symmetrically.
  fit <- three(rep(25 * rad, 2L))
  expect_true(attr(fit, "bent"))
  expect_equal(diff(fit), rep(25.3 * rad, 2L), tolerance = 1e-6)
  expect_equal(fit[3L] - rel[3L], rel[1L] - fit[1L], tolerance = 1e-5)
  # A station that cannot fit them compresses within its bounds.
  fit <- spread(c(0, 0, 0), rep(TRUE, 3L), matrix(0, 3L, 2L), c(0.5, 0.5), c(0.4, 0.4),
    room = 0, lower = -0.1, upper = 0.1)
  expect_equal(as.numeric(fit), c(-0.1, 0, 0.1), tolerance = 1e-9)
  # Each pair of neighbours keeps its own gap: here the pair with wider
  # routes needs 0.5 and the other pair 0.3.
  fit <- spread(c(0, 0, 0), rep(TRUE, 3L), rbind(c(Inf, 0), c(0, 0), c(0, Inf)), c(0.3, 0.5),
    c(0.1, 0.1), room = 0, lower = -1, upper = 1)
  expect_equal(fit[2:3], c(0, 0.5), tolerance = 1e-9)
  expect_lte(fit[1L], -0.3 + 1e-9)
  # Weighted pooling, and equal weights agree with stats::isoreg().
  y <- c(3, 1, 2, 5, 4)
  expect_equal(.metro_isotonic(y, rep(1, 5L)), stats::isoreg(y)$yf)
  expect_equal(.metro_isotonic(c(1, 0), c(3, 1)), c(0.75, 0.75))
})

test_that("arrival ports take one head width of rim arc and keep clear of departures", {
  # A -> Y is straight; B -> Y shares its final track, having left it above.
  nodes <- data.frame(node = c("A", "B", "Y", "Z"), x = c(-20, -20, 0, -20),
    y = c(0, 10, 0, 20), shape = 21L)
  arrivals <- data.frame(edge_id = c(1L, 2L, 2L), from = c("A", "B", "B"), to = "Y",
    x = c(-20, -20, -10), y = c(0, 10, 0), xend = c(0, -10, 0), yend = c(0, 0, 0))
  radius <- rep(1.5, 4L); rim <- 2
  head <- 2 * 2 * sin(25 * pi / 180)
  between <- function(p, q) acos(sum(p * q) / sqrt(sum(p^2) * sum(q^2)))
  ports <- .station_ports(nodes, arrivals, radius, 0.5, arrow_length = 2, arrow_angle = 25)
  # The straight arrow keeps its bearing; its partner moves to its own side
  # by one head width of arc, where the narrow fan gave 0.48 radians.
  expect_equal(unname(ports$end["1", ]), c(-rim, 0), tolerance = 1e-5)
  expect_gt(ports$end["2", 2L], 0)
  expect_equal(between(ports$end["1", ], ports$end["2", ]), head / rim, tolerance = 1e-5)
  plain <- .station_ports(nodes, arrivals, radius, 0.5)
  expect_equal(between(plain$end["1", ], plain$end["2", ]), 0.48)
  # A route leaving towards the upper left bounds the widened fan: no head
  # lands on it, and the pair keeps the outlines of its heads apart.
  edges <- rbind(arrivals, data.frame(edge_id = 3L, from = "Y", to = "Z",
    x = 0, y = 0, xend = -20, yend = 20))
  ports <- .station_ports(nodes, edges, radius, 0.5, arrow_length = 2)
  for (id in c("1", "2")) expect_gt(between(ports$end[id, ], ports$start["3", ]), 0.2)
  separation <- between(ports$end["1", ], ports$end["2", ])
  expect_lt(separation, head / rim)
  expect_gt(separation, 2 * atan(2 * sin(25 * pi / 180) / (rim + 2 * cos(25 * pi / 180))))
})

test_that("an arrival spreads only as far as its route can reach without a hook", {
  # As above: A -> Y is straight, B -> Y left the shared track above. When
  # B would hook beyond 30 degrees from the bearing (and not at its narrow
  # port) and A beyond its 2-degree room, B stops there and A takes its room;
  # the heads still clear their outlines.
  nodes <- data.frame(node = c("A", "B", "Y"), x = c(-20, -20, 0), y = c(0, 10, 0), shape = 21L)
  edges <- data.frame(edge_id = c(1L, 2L, 2L), from = c("A", "B", "B"), to = "Y",
    x = c(-20, -20, -10), y = c(0, 10, 0), xend = c(-0, -10, 0), yend = c(0, 0, 0))
  bearing <- function(p) abs(atan2(p[2L], -p[1L])) * 180 / pi
  tried <- character()
  shape <- function(id, point) {
    tried <<- c(tried, as.character(id))
    c(back = if (bearing(point) <= c(2, 30)[id]) 0 else 20, sharp = 0, clear = TRUE)
  }
  ports <- .station_ports(nodes, edges, rep(1.5, 3L), 0.5, arrow_length = 2, shape = shape)
  expect_true("2" %in% tried)
  expect_gt(bearing(ports$end["2", ]), 29)
  expect_lte(bearing(ports$end["2", ]), 30)
  expect_lte(bearing(ports$end["1", ]), 2 + 1e-9)
  wing <- 2 * sin(25 * pi / 180); base <- 2 + 2 * cos(25 * pi / 180)
  minimum <- 2 * atan(wing / base) + 2 * asin(0.1 / (2 * sqrt(base^2 + wing^2)))
  expect_gte(bearing(ports$end["1", ]) + bearing(ports$end["2", ]), minimum * 180 / pi)
  # Without the test B takes a full head width of arc, as does the narrow
  # fan's 0.48 radians.
  free <- .station_ports(nodes, edges, rep(1.5, 3L), 0.5, arrow_length = 2)
  expect_equal(bearing(free$end["2", ]), 4 * sin(25 * pi / 180) / 2 * 180 / pi, tolerance = 1e-6)
  expect_equal(bearing(free$narrow["2", ]), 0.48 * 180 / pi, tolerance = 1e-6)
})

test_that("a crowded common approach keeps attachments outside the station with arrowheads", {
  nodes <- data.frame(node = c(paste0("P", 1:9), "Y"), x = c(rep(-20, 9), 0),
    y = c(rep(20, 9), 0), shape = 21L)
  edges <- data.frame(edge_id = 1:9, from = paste0("P", 1:9), to = "Y",
    x = -10, y = 10, xend = 0, yend = 0)
  ports <- .station_ports(nodes, edges, rep(5, 10), 0.5, arrow_length = 1.3)
  distance <- .point_segment_distance(0, 0, rep(-10, 9), rep(10, 9),
    ports$end[, 1L], ports$end[, 2L])
  expect_true(all(distance > 5))
  # Nine arrows on one track, all keeping its bearing, cannot part their
  # heads without bending: they bend within one octilinear direction of it,
  # far enough to part their heads.
  angle <- sort(atan2(ports$end[, 2L], ports$end[, 1L]))
  expect_true(all(abs(angle - 3 * pi / 4) <= pi / 4 + 1e-9))
  wing <- 1.3 * sin(25 * pi / 180)
  expect_true(all(diff(angle) >= 2 * atan(wing / (5.5 + 1.3 * cos(25 * pi / 180))) - 1e-9))
})

test_that("a port a few degrees off a straight arrival tilts the run instead of hooking", {
  rr <- 2.78; target <- c(0, 0)
  xy <- cbind(seq(-40, -rr, length.out = 9L), 0)
  delta <- 2 * pi / 180
  tip <- c(-rr * cos(delta), rr * sin(delta))
  tilted <- .metro_fan_end(xy, tip, target, straight = 1.3)
  expect_equal(unname(tilted), unname(rbind(xy[1L, ], tip)))
  drawn <- .metro_arrow_route(tilted, 1.3, target)
  routes <- data.frame(panel = "1", from = "A", to = "B", point = seq_len(nrow(drawn)),
    x = drawn[, 1L], y = drawn[, 2L])
  expect_false(.metro_hook_score(routes)$hook)
  # The tilt is bounded at the head base, not the tip: on a short run of
  # grid steps a port 2.5 degrees off keeps its tip within 3 degrees of the
  # run, but its head base would slant the drawn shaft by 5 degrees.
  short <- cbind(c(-6, -4.5, -rr), 0)
  port <- c(-rr * cos(2.5 * pi / 180), rr * sin(2.5 * pi / 180))
  expect_lt(atan2(port[2L], port[1L] + 6), 0.05)
  expect_false(isTRUE(all.equal(unname(.metro_fan_end(short, port, target, straight = 1.3)),
    unname(rbind(short[1L, ], port)))))
  # A final diagonal step shorter than the station clearance was trimmed
  # away, leaving a horizontal run that is not the arrow's last step. Its
  # port lies on the diagonal bearing: tilting the run there would slant the
  # whole run and kink it into the head, so it is joined or blended instead.
  lane <- cbind(seq(-40, -2.3, length.out = 9L), -0.3)
  corner <- c(-1, -1) * rr / sqrt(2)
  bent <- .metro_fan_end(lane, corner, target, straight = 1.3, bearing = c(-1, -1))
  expect_gt(nrow(bent), 3L)
  drawn <- .metro_arrow_route(bent, 1.3, target)
  step <- diff(drawn)[-(nrow(drawn) - 1L), , drop = FALSE]
  long <- sqrt(rowSums(step^2)) >= 4
  slope <- atan2(step[long, 2L], step[long, 1L]) * 180 / pi
  expect_true(all(abs(((slope + 22.5) %% 45) - 22.5) < 1))
  # Far from its bearing the port is still joined or blended.
  far <- .metro_fan_end(xy, c(-rr * cos(0.3), rr * sin(0.3)), target, straight = 1.3)
  expect_gt(nrow(far), 3L)
})

test_that("wide routes and long heads reach spread ports without folding back", {
  skip_on_cran()
  skip_if_not_installed("svglite")
  # A blend that lost its clearance used to leave the plain attachment to a
  # widened port, folding the shaft back before its head (S -> Y turned 143
  # degrees in gallery figure 01 with 1.2 mm routes). Routes wider than 0.7
  # mm also lost every blend to their own station's clearance.
  cases <- list(
    list(.test_spacing_plot(1L, line_width = 1.2), c(85, 180)),
    list(.test_mediator_fan(line_width = 1.2), 120),
    list(.test_spacing_plot(3L, style = metro_style(arrow_length = 4)), 85),
    list(.test_spacing_plot(1L, edge_style = list(`X->Y` = list(width = 1.5),
      `I->Y` = list(width = 1.5))), 85))
  reports <- list()
  for (i in seq_along(cases)) for (width in cases[[i]][[2L]]) {
    report <- diagnose_metro_dag(cases[[i]][[1L]], width = width, device = "svg")
    reports[[paste(i, width)]] <- report
    turns <- .test_arrival_turns(report$routes)
    expect_lte(max(turns$sharpest), 90, label = paste(width, "mm sharpest turn"))
  }
  # With 1.2 mm routes the arrivals into Y join their ports without hooks.
  hooks <- .metro_hook_score(reports[["1 180"]]$routes)
  expect_false(any(hooks$hook[hooks$to == "Y"]))
})

test_that("an arrival's last shaft before its head stays octilinear", {
  skip_on_cran()
  skip_if_not_installed("svglite")
  # Tilting a final run that is not the arrow's last step (the step was
  # trimmed inside the station clearance) slanted whole runs by up to 16
  # degrees and kinked them into the head.
  plots <- list(fan = .test_mediator_fan())
  if (requireNamespace("dagitty", quietly = TRUE)) for (name in c("Kampen", "Shrier"))
    plots[[name]] <- metro_dag(dagitty::getExample(name), bidirected = "latent")
  for (name in names(plots)) {
    report <- diagnose_metro_dag(plots[[name]], width = 85, device = "svg")
    turns <- .test_arrival_turns(report$routes)
    expect_lte(max(turns$slant), 3, label = paste(name, "slant"))
  }
})

test_that("triangle gaps measure overlapping and separate arrowheads", {
  a <- rbind(c(0, 0), c(1, 0), c(0, 1))
  expect_equal(.metro_triangle_gap(a, a + 3), sqrt(2 * 2.5^2))
  expect_equal(.metro_triangle_gap(a, a + 0.2), 0)
  # Edges that cross with no vertex inside the other triangle.
  star <- rbind(c(0.5, -0.5), c(1.2, 0.6), c(-0.2, 0.6))
  b <- rbind(c(-0.2, 0.1), c(1.2, 0.1), c(0.5, 1.2))
  expect_false(any(.points_in_triangle(star, b)) || any(.points_in_triangle(b, star)))
  expect_equal(.metro_triangle_gap(star, b), 0)
  heads <- list(list(to = "Y", from = "A", triangle = a, width = 0.45),
    list(to = "Y", from = "B", triangle = a + c(0, 1.3), width = 0.45),
    list(to = "Z", from = "C", triangle = a + 0.1, width = 0.45))
  overlaps <- .metro_arrowhead_overlaps(heads)
  expect_equal(overlaps, data.frame(node = "Y", from = "A", other_node = "B"))
})

test_that("diagnostics report arrowheads that still overlap at a crowded station", {
  skip_on_cran()
  star <- metro_dag(.test_star_layout(15L), labels = FALSE, legend = FALSE)
  report <- diagnose_metro_dag(star, width = 180)
  overlap <- report$issues[report$issues$type == "arrowhead_overlap", ]
  expect_gt(nrow(overlap), 0L)
  expect_true(report$ok)
  expect_true(all(overlap$severity == "warning" & overlap$node == "Y" & overlap$to == "Y"))
  expect_true(all(c(overlap$from, overlap$other_node) %in% paste0("P", 1:15)))
  expect_match(overlap$message[1L], "arrowheads of P[0-9]+ -> Y and P[0-9]+ -> Y overlap")
  expect_match(overlap$suggestion[1L], "station_scaling = \"degree\"", fixed = TRUE)
  # Drawn outside diagnostics, the same problem is a drawing warning.
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file), add = TRUE)
  expect_warning(save_metro_dag(file, star, width = 180), class = "metrodag_render_warning")
  # Larger stations give the heads more room, as the advice says. Arrivals
  # from one side may need a much larger station: a six-parent star clears
  # only from node_size = 8.
  # Stations and their sizes do not change the layout: lay each star out once.
  star <- function(k, ...) metro_dag(.test_star_layout(k), labels = FALSE, legend = FALSE, ...)
  count <- function(p, width = 180) sum(diagnose_metro_dag(p, width = width)$issues$type ==
    "arrowhead_overlap")
  crowded <- star(12)
  expect_gt(count(crowded), 0L)
  expect_identical(count(star(12, style = metro_style(station_scaling = "degree"))), 0L)
  expect_identical(count(star(12, node_size = 6)), 0L)
  issues <- diagnose_metro_dag(star(6), width = 180)$issues
  expect_gt(sum(issues$type == "arrowhead_overlap"), 0L)
  expect_gt(count(star(6, node_size = 6)), 0L)
  expect_identical(count(star(6, node_size = 8)), 0L)
  expect_match(issues$message[issues$type == "arrowhead_overlap"][1L], "much larger station",
    fixed = TRUE)
  # With two crowded stations, each row describes its own station.
  stars <- metro_dag(rbind(cbind(paste0("P", 1:12), "Y"), cbind(paste0("Q", 1:12), "Z")),
    labels = FALSE, legend = FALSE)
  overlap <- diagnose_metro_dag(stars, width = 180)$issues
  overlap <- overlap[overlap$type == "arrowhead_overlap", ]
  expect_setequal(unique(overlap$node), c("Y", "Z"))
  expect_true(all(startsWith(overlap$from, ifelse(overlap$node == "Y", "P", "Q"))))
  expect_true(all(mapply(grepl, paste0("-> ", overlap$node, " overlap at station ", overlap$node),
    overlap$message, fixed = TRUE)))
})

test_that("arrivals on different octilinear bearings keep their ports", {
  # A -> Y arrives horizontally and B -> Y diagonally, 45 degrees apart. One
  # head width of arc would need about 48 degrees here, but the outlines are
  # already clear at 45, so neither arrival moves or tilts.
  nodes <- data.frame(node = c("A", "B", "Y"), x = c(-20, -20, 0), y = c(0, 20, 0), shape = 21L)
  arrivals <- data.frame(edge_id = 1:2, from = c("A", "B"), to = "Y",
    x = c(-20, -20), y = c(0, 20), xend = c(0, 0), yend = c(0, 0))
  radius <- rep(1.5, 3L); rim <- 2
  expect_gt(2 * 2 * sin(25 * pi / 180) / rim, pi / 4)
  ports <- .station_ports(nodes, arrivals, radius, 0.5, arrow_length = 2, arrow_angle = 25)
  expect_equal(unname(ports$end["1", ]), c(-rim, 0), tolerance = 1e-9)
  expect_equal(unname(ports$end["2", ]), rim * c(-1, 1) / sqrt(2), tolerance = 1e-9)
})
