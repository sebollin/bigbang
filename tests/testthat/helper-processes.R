bb_detach_collected_children <- function() {
  if (.Platform$OS.type == "windows") return(invisible(NULL))
  if (!identical(.Platform$OS.type, "unix")) return(invisible(NULL))
  try(parallel:::cleanup(kill = FALSE, detach = TRUE), silent = TRUE)
  invisible(NULL)
}

bb_collect_child <- function(child, timeout = 1) {
  if (!identical(.Platform$OS.type, "unix")) return(NULL)
  deadline <- Sys.time() + timeout
  repeat {
    collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
    if (!is.null(collected)) return(collected)
    if (Sys.time() >= deadline) return(NULL)
    Sys.sleep(0.01)
  }
}

bb_cleanup_child <- function(child) {
  if (!identical(.Platform$OS.type, "unix")) return(invisible(NULL))
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
  if (!identical(.Platform$OS.type, "unix")) return(vector("list", length(children)))
  results <- lapply(children, function(child) {
    collected <- bb_collect_child(child, timeout = timeout)
    if (is.null(collected)) NULL else collected[[1L]]
  })
  if (all(vapply(results, Negate(is.null), logical(1L)))) {
    bb_detach_collected_children()
  }
  results
}
