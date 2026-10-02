# Forked children and their registry exist only on Unix; on Windows the
# parallel namespace has no children() or cleanup().
if (.Platform$OS.type == "unix" &&
      requireNamespace("parallel", quietly = TRUE)) {
  registered <- parallel:::children()
  if (length(registered) > 0L) {
    live <- vapply(registered, function(child) {
      isTRUE(tryCatch(tools::pskill(child$pid, 0L),
                      error = function(e) FALSE))
    }, logical(1L))
    if (any(live)) {
      for (child in registered[live]) {
        try(tools::pskill(child$pid, tools::SIGKILL), silent = TRUE)
      }
    }
    deadline <- Sys.time() + 2
    repeat {
      remaining <- parallel:::children()
      if (length(remaining) == 0L || Sys.time() >= deadline) break
      suppressWarnings(parallel::mccollect(remaining, wait = FALSE))
      Sys.sleep(0.01)
    }
    suppressWarnings(parallel::mccollect(registered, wait = TRUE))
    remaining <- parallel:::children()
    if (length(remaining) > 0L) {
      # Every still-registered child was sent SIGKILL above.  Reap it with a
      # blocking collection so the parallel finalizer has no live registry
      # entry left to terminate during interpreter shutdown.
      suppressWarnings(parallel::mccollect(remaining, wait = TRUE))
    }
  }
  remaining <- parallel:::children()
  if (length(remaining) > 0L) {
    suppressWarnings(parallel::mccollect(remaining, wait = TRUE))
  }
  remaining <- parallel:::children()
  if (length(remaining) > 0L) {
    parallel:::cleanup(kill = TRUE, detach = FALSE, shutdown = TRUE)
  }
  remaining <- parallel:::children()
  if (length(remaining) > 0L) {
    parallel:::cleanup(kill = TRUE, detach = FALSE, shutdown = TRUE)
    remaining <- parallel:::children()
    if (length(remaining) > 0L) {
      suppressWarnings(parallel::mccollect(remaining, wait = TRUE))
    }
  }
}
