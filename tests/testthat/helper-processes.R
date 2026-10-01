bb_detach_collected_children <- function() {
  try(parallel:::cleanup(kill = FALSE, detach = TRUE), silent = TRUE)
  invisible(NULL)
}

bb_collect_child <- function(child, timeout = 1) {
  deadline <- Sys.time() + timeout
  repeat {
    collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
    if (!is.null(collected)) return(collected)
    if (Sys.time() >= deadline) return(NULL)
    Sys.sleep(0.01)
  }
}

bb_cleanup_child <- function(child) {
  collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
  if (is.null(collected)) {
    try(tools::pskill(child$pid, tools::SIGKILL), silent = TRUE)
    collected <- bb_collect_child(child, timeout = 1)
  }
  if (!is.null(collected) && length(parallel:::children()) <= 1L) {
    bb_detach_collected_children()
  }
  invisible(NULL)
}

bb_collect_children <- function(children, timeout = 1) {
  results <- lapply(children, function(child) {
    collected <- bb_collect_child(child, timeout = timeout)
    if (is.null(collected)) NULL else collected[[1L]]
  })
  if (all(vapply(results, Negate(is.null), logical(1L)))) {
    bb_detach_collected_children()
  }
  results
}
