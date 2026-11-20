# Score S-shaped hooks in drawn arrows. `routes` is diagnose_metro_dag()$routes
# (points in millimetres, ending at each arrow's tip). Within the final
# `window` mm, a hook is a shaft that turns one way and then back the other way
# by more than `threshold` degrees each. Shared by tests and
# inst/benchmarks/arrow_hooks.R.
.metro_hook_score <- function(routes, window = 10, threshold = 8) {
  if (!nrow(routes)) return(data.frame(panel = character(), from = character(),
    to = character(), positive = numeric(), negative = numeric(), hook = logical()))
  key <- paste(routes$panel, routes$from, routes$to, sep = "\r")
  rows <- lapply(split(routes, factor(key, levels = unique(key))), function(r) {
    r <- r[order(r$point), ]
    p <- cbind(r$x, r$y)
    # Walk back from the tip until `window` mm of shaft is covered.
    step <- sqrt(rowSums(diff(p)^2))
    keep <- rev(cumsum(rev(step))) <= window + 1e-9
    first <- max(1L, which(keep)[1L] - 1L)
    if (is.na(first)) first <- nrow(p) - 1L
    q <- p[first:nrow(p), , drop = FALSE]
    v <- diff(q); v <- v[rowSums(v^2) > 1e-12, , drop = FALSE]
    turns <- if (nrow(v) > 1L) {
      a <- atan2(v[, 2L], v[, 1L])
      atan2(sin(diff(a)), cos(diff(a))) * 180 / pi
    } else numeric()
    positive <- sum(turns[turns > 0]); negative <- -sum(turns[turns < 0])
    data.frame(panel = r$panel[1L], from = r$from[1L], to = r$to[1L],
      positive = positive, negative = negative,
      hook = positive > threshold && negative > threshold)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
