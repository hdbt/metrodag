#' Canonical DAG examples
#'
#' @param name One of `chain`, `fork`, `collider`, `diamond`, `long_edge`,
#'   `causal`, `dense`, or `random`.
#' @param n Positive integer number of vertices for `random`.
#' @param probability Finite numeric forward-edge probability between 0 and 1
#'   (inclusive).
#' @param seed Integer random seed for `random` only, or NULL for a fresh seed.
#'   Example generation preserves the caller's random-number state.
#' @return A directed `igraph` object.
#' @examples
#' metrodag_examples("causal")
#' metro_dag(metrodag_examples("collider"))
#' metrodag_examples("random", n = 8, probability = 0.3, seed = 2)
#' @export
metrodag_examples <- function(name = "diamond", n = 20L, probability = 0.12, seed = 1L) {
  name <- .assert_scalar_choice(name, c("chain", "fork", "collider", "diamond", "long_edge", "causal", "dense", "random"), "name")
  edge_text <- switch(name,
    chain = "A B B C C D",
    fork = "A B A C A D",
    collider = "A D B D C D",
    diamond = "A B A C B D C D",
    long_edge = "A B B C C D D E A E A D",
    causal = "C X C Y X M M Y U C U X S Y S X",
    dense = paste(unlist(lapply(LETTERS[1:4], function(a) as.vector(rbind(a, LETTERS[5:8])))), collapse = " "),
    random = NULL)
  if (name != "random") return(igraph::graph_from_edgelist(matrix(strsplit(edge_text, " ")[[1L]], ncol = 2L, byrow = TRUE), directed = TRUE))
  .assert_number(n, "n", minimum = 1, integer = TRUE)
  if (n > .Machine$integer.max)
    stop("`n` must not exceed the maximum R integer.", call. = FALSE)
  if (!is.numeric(probability) || length(probability) != 1L ||
      !is.finite(probability) || probability < 0 || probability > 1)
    stop("`probability` must be a single finite number between 0 and 1.", call. = FALSE)
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L ||
      !is.finite(seed) || seed != floor(seed) || abs(seed) > .Machine$integer.max))
    stop("`seed` must be NULL or a single finite integer within R's integer range.", call. = FALSE)
  g <- igraph::make_empty_graph(n, directed = TRUE)
  igraph::V(g)$name <- paste0("v", seq_len(n))
  if (n == 1L || probability == 0) return(g)
  if (probability < 1) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (had_seed) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }
  # Visit the upper triangle in its original column order, retaining only
  # selected edges. Seeded results are unchanged without allocating n^2 cells.
  edges <- unlist(lapply(seq.int(2L, n), function(to) {
    from <- if (probability == 1) seq_len(to - 1L) else
      which(stats::runif(to - 1L) < probability)
    as.vector(rbind(from, rep.int(to, length(from))))
  }), use.names = FALSE)
  if (length(edges)) g <- igraph::add_edges(g, edges)
  g
}
