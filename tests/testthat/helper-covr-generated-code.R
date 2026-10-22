round075_archive_template <- function() {
  round075_untraced_code(.archive_reader_template())
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
    identical(item$env, asNamespace("bigbang")) &&
      (as.character(item$name) %in% names ||
         grepl("^\\.archive_", as.character(item$name)))
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
