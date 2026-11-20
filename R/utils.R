.assert_scalar_choice <- function(x, choices, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(sprintf("`%s` must be one of: %s.", name, paste(choices, collapse = ", ")), call. = FALSE)
  }
  x
}

.assert_number <- function(x, name, minimum = 0, integer = FALSE, strict = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < minimum || (strict && x == minimum) || (integer && x != floor(x))) {
    stop(sprintf("`%s` must be a %snumber%s.", name,
      if (strict && minimum == 0) "positive " else "finite ",
      if (integer) " (a non-negative integer)" else ""), call. = FALSE)
  }
  invisible(x)
}

.assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
}

.rotate_geometry <- function(nodes, edges, direction) {
  transform <- switch(direction,
    down = function(x, y) cbind(x, y),
    up = function(x, y) cbind(x, -y),
    right = function(x, y) cbind(-y, x),
    left = function(x, y) cbind(y, x)
  )
  xy <- transform(nodes$x, nodes$y)
  label_xy <- transform(nodes$label_x, nodes$label_y)
  nodes$x <- xy[, 1L]
  nodes$y <- xy[, 2L]
  nodes$label_x <- label_xy[, 1L]
  nodes$label_y <- label_xy[, 2L]
  if (nrow(edges)) {
    a <- transform(edges$x, edges$y)
    b <- transform(edges$xend, edges$yend)
    edges$x <- a[, 1L]; edges$y <- a[, 2L]
    edges$xend <- b[, 1L]; edges$yend <- b[, 2L]
  }
  list(nodes = nodes, edges = edges)
}


# Shared wording for messages that name the inputs at fault. List at most
# `max` items, each quoted, then say how many more there are.
# `distinct = FALSE` keeps repeated texts, when they stand for different things.
.metro_quote_list <- function(x, max = 5L, quote = "`", distinct = TRUE) {
  x <- as.character(x)
  if (distinct) x <- unique(x)
  shown <- paste0(quote, x[seq_len(min(length(x), max))], quote)
  more <- length(x) - length(shown)
  if (more > 0L) return(paste0(paste(shown, collapse = ", "), " and ", more, " more"))
  if (length(shown) <= 1L) return(paste(shown, collapse = ""))
  paste(paste(shown[-length(shown)], collapse = ", "), "and", shown[length(shown)])
}

# A sentence listing a graph's node names when there are few enough to read,
# so a user can spot a typo or a display label used in place of a name.
.metro_node_hint <- function(nodes, max = 20L) {
  if (!length(nodes) || length(nodes) > max) return("")
  sprintf(" Node names are: %s.", paste(nodes, collapse = ", "))
}

# Arrows as they are written in messages and `edge_style` keys.
.metro_arrow_text <- function(from, to, symbol = "->") paste(from, symbol, to)
