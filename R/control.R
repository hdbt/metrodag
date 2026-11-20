# Internal layout controls: named refinement stages and work budgets.
# Defaults reproduce the published geometry. Developers can disable stages or
# change budgets for experiments and ablations with
# options(metrodag.control = list(stages = c(restart_variants = FALSE))).
# This is not a user-facing tuning interface.

.metro_layout_stages <- c(
  "straighten",              # whole-edge straightening of initial routes
  "balance",                 # fan symmetry balancing
  "crossing_pairs",          # joint rerouting of crossing arrow pairs
  "fan_straighten",          # move fan nodes to straighten arrows (not with align)
  "section_cleanup",         # simplify route sections with fewer bends
  "gap_compaction",          # remove unnecessary straight steps between ranks
  "approach_lengthening",    # lengthen short final approaches
  "arrangement",             # transverse node-arrangement compaction
  "singleton_slide",         # slide lone nodes towards their neighbours
  "rank_refinement",         # rank reassignment with full rebuilds
  "restart_variants",        # alternative deterministic starting points
  "layering_variants",       # starts from other automatic layerings
  "source_relaxation",       # move sources towards their children
  "intermediate_relaxation", # move low-degree intermediates
  "half_grid"                # final half-grid route refinement
)

.metro_control_defaults <- function() list(
  stages = stats::setNames(rep(TRUE, length(.metro_layout_stages)), .metro_layout_stages),
  budgets = c(arrangement = 24000, rank_refinement = 12000, half_grid = 400000,
    restart_variants = 4000, layering_variants = 8000, intermediate_relaxation = 60000,
    source_relaxation = 120000),
  crossing_pairs = 24L, crossing_pairs_aligned = 48L, sift_per_rank = 8L)

.metro_control <- function() {
  control <- .metro_control_defaults()
  user <- getOption("metrodag.control")
  if (is.null(user)) return(control)
  if (!is.list(user) || is.null(names(user)) || any(!names(user) %in% names(control)))
    stop("`options(metrodag.control)` must be a named list using: ",
      paste(names(control), collapse = ", "), ".", call. = FALSE)
  for (key in c("stages", "budgets")) if (!is.null(user[[key]])) {
    value <- user[[key]]
    if (is.null(names(value)) || any(!names(value) %in% names(control[[key]])))
      stop(sprintf("Unknown metrodag.control %s: %s.", key,
        paste(setdiff(names(value), names(control[[key]])), collapse = ", ")), call. = FALSE)
    control[[key]][names(value)] <- value
    user[[key]] <- NULL
  }
  utils::modifyList(control, user)
}

# Stage gates double as the record of which refinements a drawing received.
# layout_metro_dag() opens a record; each gate passed in its own pipeline (not
# in the trial rebuilds of rank refinement or restart variants) is noted, and
# budgeted stages note whether their work budget left them an attempt. A
# component with nothing for such a stage to do (no arrows, or fewer than two
# routes) passes the gate but never reaches the budget, so it counts neither
# way: an isolated node must not turn a skipped stage into a partly run one.
.metro_stage_record <- new.env(parent = emptyenv())

.metro_stage <- function(name) {
  enabled <- isTRUE(.metro_control()$stages[[name]])
  if (enabled && .metro_recording()) .metro_stage_record$entered[[name]] <-
    .metro_stage_record$entered[[name]] + 1L
  enabled
}

.metro_budget <- function(name) .metro_control()$budgets[[name]]

# Attempts a budgeted stage may make on routes whose work is route steps times
# arrows: at most `max_candidates`, scaled down by the stage's work budget.
.metro_stage_attempts <- function(name, work, max_candidates) {
  attempts <- min(max_candidates, floor(.metro_budget(name) / work))
  .metro_note_budget(name, attempts >= 1L)
  attempts
}

.metro_note_budget <- function(name, attempted) {
  if (!.metro_recording()) return(invisible())
  counter <- if (attempted) "attempted" else "skipped"
  .metro_stage_record[[counter]][[name]] <- .metro_stage_record[[counter]][[name]] + 1L
}

.metro_recording <- function() isTRUE(.metro_stage_record$depth == 0L)

.metro_record_stages <- function() {
  record <- .metro_stage_record
  record$entered <- record$attempted <- record$skipped <-
    stats::setNames(integer(length(.metro_layout_stages)), .metro_layout_stages)
  record$depth <- 0L
}

.metro_clear_record <- function() rm(list = ls(.metro_stage_record), envir = .metro_stage_record)

# One status per stage: "ran", "skipped (budget)", "partly skipped (budget)"
# when the budget allowed it in some components with work for it but not in
# others, "disabled" by the internal controls, or "not applicable" (for
# example explicit ranks skip rank refinement, and a budgeted stage that no
# component had work for, such as any stage on a graph without arrows).
.metro_recorded_stages <- function() {
  record <- .metro_stage_record
  status <- ifelse(record$skipped == 0L, "ran",
    ifelse(record$attempted == 0L, "skipped (budget)", "partly skipped (budget)"))
  budgeted <- names(record$entered) %in% names(.metro_control_defaults()$budgets)
  idle <- budgeted & record$attempted == 0L & record$skipped == 0L
  status[record$entered == 0L | idle] <- "not applicable"
  status[!.metro_control()$stages[names(status)]] <- "disabled"
  stats::setNames(status, names(record$entered))
}

# Trial pipelines (rank refinement rebuilds, restart variants) and stages
# reused inside other stages do not decide which refinements were received.
.metro_unrecorded <- function(expr) {
  record <- .metro_stage_record
  if (!is.null(record$depth)) {
    record$depth <- record$depth + 1L
    on.exit(record$depth <- record$depth - 1L)
  }
  expr
}
