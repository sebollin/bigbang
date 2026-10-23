capture_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    force(expr),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

expect_warning_set <- function(warnings, patterns, fixed = TRUE) {
  expect_length(warnings, length(patterns))
  expect_length(unique(warnings), length(warnings))
  matches <- vapply(
    warnings,
    function(message) {
      sum(vapply(
        patterns,
        function(pattern) grepl(pattern, message, fixed = fixed, perl = !fixed),
        logical(1L)
      )) == 1L
    },
    logical(1L)
  )
  expect_true(all(matches), info = paste(warnings, collapse = "\n"))
  expect_true(all(vapply(
    patterns,
    function(pattern) {
      sum(grepl(pattern, warnings, fixed = fixed, perl = !fixed)) == 1L
    },
    logical(1L)
  )), info = paste(patterns, collapse = "\n"))
}
