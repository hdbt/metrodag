# Per-arrow appearance overrides. Keys are "from->to"; a data frame with
# `from`/`to` columns is accepted too. Styles never change geometry or roles.
# `edges`, a data frame with `from` and `to` columns, holds the graph's arrows;
# with it, a key with spaces around its ends names the arrow it would name
# without them. A data frame names its ends exactly.
.metro_edge_style_table <- function(edge_style, edges = NULL) {
  if (is.null(edge_style) || (is.list(edge_style) && !is.data.frame(edge_style) && !length(edge_style)))
    return(NULL)
  fields <- c("linetype", "colour", "width")
  if (is.data.frame(edge_style)) {
    if (!all(c("from", "to") %in% names(edge_style)))
      stop("An `edge_style` data frame needs `from` and `to` columns.", call. = FALSE)
    table <- edge_style
    if ("color" %in% names(table) && !"colour" %in% names(table)) {
      table$colour <- table$color
    }
    table <- table[, intersect(c("from", "to", fields), names(table)), drop = FALSE]
    # Numeric ends name nodes as numeric edge tables do.
    table$from <- .metro_node_names(table$from); table$to <- .metro_node_names(table$to)
  } else {
    if (!is.list(edge_style) || is.null(names(edge_style)))
      stop("`edge_style` must be a named list keyed \"from->to\", each element a list of linetype, colour and/or width, or a data frame with from/to columns.", call. = FALSE)
    keys <- names(edge_style)
    if (anyNA(keys) || any(!nzchar(keys)))
      stop("Every `edge_style` entry needs a key naming one arrow, such as \"X->Y\".", call. = FALSE)
    bad <- keys[!vapply(edge_style, is.list, logical(1))]
    if (length(bad))
      stop(sprintf("`edge_style` for %s must be a list of linetype, colour and/or width, such as list(linetype = \"dotted\").",
        .metro_quote_list(bad, quote = "\"")), call. = FALSE)
    rows <- lapply(seq_along(keys), function(i) {
      key <- keys[[i]]
      ends <- .metro_edge_style_ends(key, edges)
      spec <- edge_style[[i]]
      if (!is.null(spec$color) && is.null(spec$colour)) spec$colour <- spec$color
      spec$color <- NULL
      unknown <- setdiff(names(spec), fields)
      if (is.null(names(spec)) && length(spec) || length(unknown))
        stop(sprintf("`edge_style` for \"%s\" accepts only linetype, colour and width.", key), call. = FALSE)
      row <- data.frame(from = ends[1L], to = ends[2L], linetype = NA_character_,
        colour = NA_character_, width = NA_real_, stringsAsFactors = FALSE)
      for (field in names(spec)) row[[field]] <- spec[[field]]
      row
    })
    table <- do.call(rbind, rows)
  }
  for (field in c("linetype", "colour")) if (field %in% names(table) &&
      !(is.character(table[[field]]) || all(is.na(table[[field]]))))
    stop(sprintf("`edge_style` %s values must be character.", field), call. = FALSE)
  if ("linetype" %in% names(table)) {
    bad <- table$linetype[!is.na(table$linetype) & !.metro_valid_linetype(table$linetype)]
    if (length(bad)) stop(sprintf(
      "`edge_style` linetypes must be one of solid, dashed, dotted, dotdash, longdash, twodash or a hex dash pattern such as \"44\" (2, 4, 6 or 8 digits 1-F); not %s.",
      paste0('"', unique(bad), '"', collapse = ", ")), call. = FALSE)
  }
  if ("width" %in% names(table) && (!is.numeric(table$width) ||
      any(!is.na(table$width) & (!is.finite(table$width) | table$width <= 0))))
    stop("`edge_style` width values must be positive numbers (millimetres).", call. = FALSE)
  if ("colour" %in% names(table)) {
    colours <- table$colour[!is.na(table$colour)]
    if (length(colours)) tryCatch(grDevices::col2rgb(colours), error = function(e)
      stop("`edge_style` colours must be valid colours.", call. = FALSE))
  }
  repeated <- duplicated(.metro_arrow_key(table$from, table$to))
  if (any(repeated))
    stop(sprintf("`edge_style` names each arrow at most once, but gives %s more than once.",
      .metro_quote_list(.metro_arrow_text(table$from[repeated], table$to[repeated]))), call. = FALSE)
  table
}

# The two ends of an `edge_style` key. Node names are used exactly as written
# when they name an arrow of the graph, so names with spaces, a trailing "<"
# or "->" inside them work; a key with several "->" names the arrow that one
# of its splits gives. Otherwise spaces around each end are dropped:
# "X -> M", as DAGitty writes arrows, names the arrow "X->M".
.metro_edge_style_ends <- function(key, edges = NULL) {
  at <- gregexpr("->", key, fixed = TRUE)[[1L]]
  wrong <- sprintf("`edge_style` key \"%s\" must name one arrow as \"from->to\".", key)
  if (at[[1L]] < 0L) stop(wrong, call. = FALSE)
  splits <- lapply(at, function(i) c(substr(key, 1L, i - 1L), substr(key, i + 2L, nchar(key))))
  if (is.null(edges)) {
    if (length(at) != 1L || any(!nzchar(splits[[1L]]))) stop(wrong, call. = FALSE)
    return(splits[[1L]])
  }
  is_arrow <- function(ends) all(nzchar(ends)) && any(edges$from == ends[1L] & edges$to == ends[2L])
  for (candidates in list(splits, lapply(splits, trimws))) {
    found <- Filter(is_arrow, candidates)
    if (length(found) == 1L) return(found[[1L]])
    # Node names containing "->" can make one key name two arrows. Spaces
    # around the arrow meant settle it, unless the names hold " -> " too.
    if (length(found) > 1L) stop(sprintf(paste("`edge_style` key \"%s\" names %d arrows: %s.",
      "%s a data frame with `from` and `to` columns."), key, length(found),
      paste(vapply(found, function(f) sprintf("from \"%s\" to \"%s\"", f[1L], f[2L]), character(1)),
        collapse = " and "),
      if (identical(candidates, splits)) sprintf("Write it with spaces around the arrow it means, as \"%s -> %s\", or give",
        found[[1L]][1L], found[[1L]][2L]) else "Give"), call. = FALSE)
  }
  if (grepl("<->", key, fixed = TRUE))
    stop(sprintf(paste0("`edge_style` key \"%s\" names a bidirected arc; keys name directed arrows ",
      "as \"from->to\"."), key), call. = FALSE)
  # Not an arrow of the graph: report it with each end as the node it names,
  # taking the split whose ends are both nodes when names contain "->".
  nodes <- c(edges$from, edges$to)
  for (candidates in list(splits, lapply(splits, trimws))) {
    named <- Filter(function(ends) all(ends %in% nodes), candidates)
    if (length(named)) return(named[[1L]])
  }
  raw <- splits[[1L]]
  ends <- trimws(raw)
  if (length(at) != 1L || any(!nzchar(ends))) stop(wrong, call. = FALSE)
  ifelse(raw %in% nodes, raw, ends)
}

# A key identifying an arrow by its two ends, for matching only. The length
# prefix keeps it unique whatever characters the names contain.
.metro_arrow_key <- function(from, to) {
  # In UTF-8, so that equal names marked with different encodings match.
  from <- enc2utf8(as.character(from)); to <- enc2utf8(as.character(to))
  paste0(nchar(from, type = "bytes"), ":", from, ":", to)
}

.metro_apply_edge_style <- function(e, edge_style) {
  table <- .metro_edge_style_table(edge_style, e)
  if (is.null(table)) return(e)
  # Match arrows by both ends: node names may themselves contain "->".
  keys <- .metro_arrow_key(e$from, e$to)
  wanted <- .metro_arrow_key(table$from, table$to)
  absent <- !wanted %in% keys
  if (any(absent)) {
    message <- sprintf("`edge_style` names %s not in the graph: %s.",
      if (sum(absent) == 1L) "an arrow" else "arrows",
      .metro_quote_list(.metro_arrow_text(table$from[absent], table$to[absent])))
    # A reversed key is the likeliest slip; point to the arrow that exists.
    reversed <- absent & .metro_arrow_key(table$to, table$from) %in% keys
    if (any(reversed)) message <- paste0(message, sprintf(" The graph has %s; keys run from cause to effect.",
      .metro_quote_list(.metro_arrow_text(table$to[reversed], table$from[reversed]))))
    stop(message, call. = FALSE)
  }
  hit <- match(keys, wanted)
  for (field in intersect(c("linetype", "colour", "width"), names(table))) {
    value <- table[[field]][hit]
    use <- !is.na(hit) & !is.na(value)
    e[[field]][use] <- value[use]
  }
  e
}

# Combine two edge_style specifications into one table; for an arrow named in
# both, values set in `new` replace those in `old`.
.metro_merge_edge_style <- function(old, new) {
  old <- .metro_edge_style_table(old); new <- .metro_edge_style_table(new)
  if (is.null(old)) return(new)
  if (is.null(new)) return(old)
  fields <- c("linetype", "colour", "width")
  blank <- list(linetype = NA_character_, colour = NA_character_, width = NA_real_)
  for (field in fields) {
    if (!field %in% names(old)) old[[field]] <- rep(blank[[field]], nrow(old))
    if (!field %in% names(new)) new[[field]] <- rep(blank[[field]], nrow(new))
  }
  old <- old[c("from", "to", fields)]; new <- new[c("from", "to", fields)]
  hit <- match(.metro_arrow_key(new$from, new$to), .metro_arrow_key(old$from, old$to))
  for (i in which(!is.na(hit))) for (field in fields)
    if (!is.na(new[[field]][i])) old[[field]][hit[i]] <- new[[field]][i]
  out <- rbind(old, new[is.na(hit), , drop = FALSE])
  rownames(out) <- NULL
  out
}
