bb_test_children <- new.env(parent = emptyenv())

bb_mcparallel <- function(...) {
  if (identical(.Platform$OS.type, "unix")) {
    parallel:::cleanup(kill = FALSE, detach = FALSE)
  }
  child <- parallel::mcparallel(...)
  assign(as.character(child$pid), child, envir = bb_test_children)
  child
}

bb_reap_test_children <- function() {
  if (!identical(.Platform$OS.type, "unix")) return(invisible(NULL))
  ids <- ls(envir = bb_test_children, all.names = TRUE)
  for (id in ids) {
    child <- get(id, envir = bb_test_children, inherits = FALSE)
    suppressWarnings(parallel::mccollect(child, wait = FALSE))
  }
  parallel:::cleanup(kill = TRUE, detach = FALSE, shutdown = TRUE)
  invisible(NULL)
}

bb_finish_child <- function(child) {
  if (!identical(.Platform$OS.type, "unix")) return(invisible(NULL))
  suppressWarnings(parallel::mccollect(child, wait = FALSE))
  parallel:::cleanup(kill = TRUE, detach = FALSE, shutdown = TRUE)
  invisible(NULL)
}

bb_detach_collected_children <- function() {
  if (.Platform$OS.type == "windows") return(invisible(NULL))
  if (!identical(.Platform$OS.type, "unix")) return(invisible(NULL))
  registered <- parallel:::children()
  if (length(registered) > 0L) {
    suppressWarnings(parallel::mccollect(registered, wait = TRUE))
  }
  invisible(NULL)
}

bb_collect_child <- function(child, timeout = 1) {
  if (!identical(.Platform$OS.type, "unix")) return(NULL)
  deadline <- Sys.time() + timeout
  repeat {
    collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
    if (!is.null(collected)) {
      return(collected)
    }
    if (Sys.time() >= deadline) return(NULL)
    Sys.sleep(0.01)
  }
}

.bb_child_registered <- function(child) {
  registered <- parallel:::children()
  any(vapply(
    registered,
    function(item) identical(as.integer(item$pid), as.integer(child$pid)),
    logical(1L)
  ))
}

bb_cleanup_child <- function(child) {
  if (!identical(.Platform$OS.type, "unix")) return(invisible(NULL))
  if (.bb_child_registered(child)) {
    collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
    if (is.null(collected)) {
      try(tools::pskill(child$pid, tools::SIGKILL), silent = TRUE)
      collected <- bb_collect_child(child, timeout = 1)
    }
  }
  bb_finish_child(child)
  invisible(NULL)
}

bb_collect_children <- function(children, timeout = 1) {
  if (!identical(.Platform$OS.type, "unix")) return(vector("list", length(children)))
  results <- lapply(children, bb_collect_child, timeout = timeout)
  parallel:::cleanup(kill = TRUE, detach = FALSE, shutdown = TRUE)
  lapply(results, function(result) if (is.null(result)) NULL else result[[1L]])
}

bb_cleanup_all_children <- function() {
  if (!identical(.Platform$OS.type, "unix")) return(invisible(NULL))
  registered <- parallel:::children()
  if (length(registered) == 0L) return(invisible(NULL))
  live <- vapply(registered, function(child) {
    isTRUE(tryCatch(tools::pskill(child$pid, 0L),
                    error = function(e) FALSE))
  }, logical(1L))
  if (any(live)) {
    for (child in registered[live]) {
      try(tools::pskill(child$pid, tools::SIGKILL), silent = TRUE)
    }
  }
  suppressWarnings(parallel::mccollect(registered, wait = TRUE))
  remaining <- parallel:::children()
  if (length(remaining) > 0L) {
    suppressWarnings(parallel::mccollect(remaining, wait = TRUE))
  }
  invisible(NULL)
}
