test_that("tests with forked children are skipped on CRAN", {
  files <- list.files(
    testthat::test_path(), pattern = "^test-.*\\.R$", full.names = TRUE
  )
  fork_token <- paste0("mc", "parallel")
  kill_token <- paste0("ps", "kill")
  signal_token <- paste0("SIG", "KILL")
  skip_token <- paste0("skip", "_on_cran")
  for (file in files) {
    expressions <- parse(file = file, keep.source = FALSE)
    for (expression in expressions) {
      if (!is.call(expression) ||
            !identical(as.character(expression[[1L]]), "test_that")) next
      source <- paste(deparse(expression), collapse = "\n")
      uses_signal <- grepl(fork_token, source, fixed = TRUE) ||
        grepl(kill_token, source, fixed = TRUE) ||
        grepl(signal_token, source, fixed = TRUE)
      if (uses_signal) {
        expect_true(grepl(skip_token, source, fixed = TRUE),
                    info = paste("Missing skip_on_cran in", file))
      }
    }
  }
})

test_that("tests never mutate standard-package namespaces", {
  files <- list.files(
    testthat::test_path(), pattern = "\\.R$", full.names = TRUE
  )
  namespace_call <- paste0("as", "Namespace")
  mutation <- paste0("(?:unlock", "Binding|assign)")
  packages <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk",
    "tools", "translations", "utils"
  )
  for (file in files) {
    source <- paste(readLines(file, warn = FALSE), collapse = "\n")
    has_mutation <- grepl(mutation, source, perl = TRUE)
    if (!has_mutation) next
    for (package in packages) {
      namespace <- paste0(
        namespace_call, "\\s*\\(\\s*['\"]", package, "['\"]"
      )
      expect_false(grepl(namespace, source, perl = TRUE),
                   info = paste("Standard namespace mutation in", file))
    }
  }
})
