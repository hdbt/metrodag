test_that("tied incoming ports follow branch geometry before endpoint names", {
  nodes <- data.frame(node = c("A", "B", "T"), x = c(0, 0, 10),
    y = c(0, 5, 0), shape = 21L)
  edges <- data.frame(from = c("A", "B", "B"), to = "T", edge_id = c(1L, 2L, 2L),
    segment = c(1L, 1L, 2L), x = c(0, 0, 5), y = c(0, 5, 0),
    xend = c(10, 5, 10), yend = 0)
  original <- edges
  for (angle in c(0, pi / 2, pi, -pi / 2)) for (reflection in c(1, -1)) {
    rotation <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2L)
    rotation <- rotation %*% diag(c(1, reflection))
    n <- nodes; e <- edges
    n[c("x", "y")] <- as.matrix(n[c("x", "y")]) %*% t(rotation)
    for (keys in list(c("x", "y"), c("xend", "yend")))
      e[keys] <- as.matrix(e[keys]) %*% t(rotation)
    ports <- .station_ports(n, e, rep(1, 3L), 0.2)
    end <- ports$end %*% rotation
    expect_equal(unname(end["1", ]), c(8.8, 0), tolerance = 1e-8)
    expect_gt(end["2", 2L], 0.1)
    # Rename endpoints so alphabetical order now agrees with the branch.
    n$node[1:2] <- c("Z", "A")
    e$from <- c("Z", "A", "A")
    renamed <- .station_ports(n, e, rep(1, 3L), 0.2)
    expect_equal(renamed, ports)
  }
  noisy <- edges; noisy$y[3L] <- 1e-12
  ports <- .station_ports(nodes, noisy, rep(1, 3L), 0.2)
  expect_gt(ports$end["2", 2L], 0.1)
  expect_identical(edges, original)
})

test_that("source bridges leave shared tracks monotonically and retain every endpoint", {
  xy <- rbind(c(2, 0), c(4, 0), c(6, 2), c(8, 4), c(12, 4))
  port <- c(1.8, 0.5); source <- c(0, 0)
  smooth <- .metro_fan_start(xy, port, source, c(1, 0))
  expect_equal(unname(smooth[1L, ]), port)
  expect_equal(unname(tail(smooth, 2L)), unname(tail(xy, 2L)))
  expect_true(all(diff(smooth[, 1L]) > 0))
  expect_true(all(diff(smooth[, 2L]) >= -1e-8))
  expect_gt(nrow(smooth), nrow(xy))
  for (angle in c(pi / 2, pi, -pi / 2)) for (reflection in c(1, -1)) {
    rotation <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2L)
    rotation <- rotation %*% diag(c(1, reflection))
    actual <- .metro_fan_start(xy %*% t(rotation), drop(port %*% t(rotation)),
      drop(source %*% t(rotation)), drop(c(1, 0) %*% t(rotation)))
    expect_equal(actual, smooth %*% t(rotation))
  }
  expect_identical(.metro_fan_start(xy, xy[1L, ], source), xy)
  for (path in list(cbind(2:8, 0), sweep(xy, 2L, c(1, -1), "*"), xy * 10)) {
    fallback <- path; fallback[1L, ] <- port
    expect_identical(.metro_fan_start(path, port, source, c(1, 0)), fallback)
  }
})
