round075_archive_template <- function() {
  round075_untraced_code(
    getFromNamespace(".archive_reader_template", "bigbang")()
  )
}

round075_check_status <- function(status, output) {
  covr_instrumentation_warning <- identical(Sys.getenv("R_COVR"), "true") &&
    any(grepl("covr:::", output, fixed = TRUE))
  grepl("ERROR", status) ||
    (grepl("WARNING", status) && !covr_instrumentation_warning)
}

round075_untraced_code <- function(code) {
  if (!identical(Sys.getenv("R_COVR"), "true") ||
        !requireNamespace("covr", quietly = TRUE)) {
    return(eval(substitute(code), parent.frame()))
  }
  replacements <- covr:::the$replacements
  names <- c(
    ".qualify_generated_runtime_calls", ".render_install_engine",
    "write_metapackage_files"
  )
  selected <- Filter(function(item) {
    if (!identical(item$env, asNamespace("bigbang"))) return(FALSE)
    item_name <- as.character(item$name)
    if (!(item_name %in% names || grepl("^\\.archive_", item_name))) {
      return(FALSE)
    }
    # A test may have mocked a package binding after covr installed its
    # instrumented replacement.  Keep that mock active; replacing it with
    # covr's original value would make the test exercise a different call.
    identical(get(item_name, envir = item$env, inherits = FALSE), item$new_value)
  }, replacements)
  saved <- lapply(selected, function(item) {
    item_name <- as.character(item$name)
    list(name = item_name, env = item$env,
         value = get(item_name, envir = item$env, inherits = FALSE))
  })
  restore <- function(items) {
    for (item in items) {
      unlockBinding(item$name, item$env)
      assign(item$name, item$value, envir = item$env)
      lockBinding(item$name, item$env)
    }
  }
  for (item in selected) {
    item_name <- as.character(item$name)
    unlockBinding(item_name, item$env)
    assign(item_name, item$orig_value, envir = item$env)
    lockBinding(item_name, item$env)
  }
  on.exit(restore(saved), add = TRUE)
  eval(substitute(code), parent.frame())
}

round075_create_metapackage <- function(...) {
  round075_untraced_code(create_metapackage(...))
}
