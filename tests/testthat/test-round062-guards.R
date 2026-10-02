test_that("tests with forked children are skipped on CRAN", {
  files <- list.files(
    testthat::test_path(), pattern = "^test-.*\\.R$", full.names = TRUE
  )
  fork_token <- paste0("mc", "parallel")
  kill_token <- paste0("ps", "kill")
  signal_token <- paste0("SIG", "KILL")
  skip_token <- paste0("skip", "_on_cran")
  for (file in files) {
    if (identical(basename(file), "test-round062-guards.R")) next
    expressions <- parse(file = file, keep.source = FALSE)
    for (expression in expressions) {
      if (!is.call(expression) ||
            !identical(as.character(expression[[1L]]), "test_that")) next
      source <- paste(deparse(expression), collapse = "\n")
      uses_signal <- grepl(fork_token, source, fixed = TRUE) ||
        grepl(kill_token, source, fixed = TRUE) ||
        grepl(signal_token, source, fixed = TRUE) ||
        grepl("round056_interrupt_discard", source, fixed = TRUE)
      if (uses_signal) {
        expect_true(grepl(skip_token, source, fixed = TRUE),
                    info = paste("Missing skip_on_cran in", file))
      }
    }
  }
})

test_that("Unix-only process primitives are protected on Windows", {
  files <- list.files(
    testthat::test_path(), pattern = "\\.R$", full.names = TRUE
  )
  tokens <- c("mcparallel", "bb_mcparallel", "mccollect", "parallel:::")
  for (file in files) {
    if (identical(basename(file), "test-round062-guards.R")) next
    expressions <- parse(file = file, keep.source = FALSE)
    for (expression in expressions) {
      if (!is.call(expression) ||
            !identical(as.character(expression[[1L]]), "test_that")) next
      source <- paste(deparse(expression), collapse = "\\n")
      uses_signal <- any(vapply(tokens, grepl, logical(1L),
                                x = source, fixed = TRUE)) ||
        grepl("round056_interrupt_discard", source, fixed = TRUE)
      if (!uses_signal) next
      protected <- grepl('skip_on_os\\("windows"\\)', source, perl = TRUE) ||
        grepl("\\.Platform\\$OS.type[[:space:]]*==[[:space:]]*['\"]unix['\"]",
              source, perl = TRUE) ||
        grepl("identical\\(.Platform\\$OS.type,[[:space:]]*['\"]unix['\"]\\)",
              source, perl = TRUE)
      expect_true(protected, info = paste("Missing Unix guard in", file))
    }
  }
})

test_that("tests never mock base or recommended package namespaces", {
  skip_on_cran()
  files <- list.files(
    testthat::test_path(), pattern = "\\.R$", full.names = TRUE
  )
  packages <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk",
    "tools", "utils"
  )
  for (file in files) {
    if (identical(basename(file), "test-round062-guards.R")) next
    expressions <- parse(file = file, keep.source = FALSE)
    for (expression in expressions) {
      if (!is.call(expression) ||
            !identical(as.character(expression[[1L]]), "test_that")) next
      source <- paste(deparse(expression), collapse = "\\n")
      for (package in packages) {
        pattern <- paste0(
          "(?s:(?:local_mocked_bindings|with_mocked_bindings).*?\\.package\\s*=\\s*['\"])",
          package, "['\"]"
        )
        expect_false(grepl(pattern, source, perl = TRUE),
                     info = paste("Standard namespace mock in", file))
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

test_that("the process signal primitive has one guarded implementation seam", {
  r_root <- file.path(testthat::test_path(), "..", "..", "R")
  if (!dir.exists(r_root)) {
    probe <- paste(deparse(bigbang:::.update_signal_probe), collapse = " ")
    expect_match(probe, "tools::pskill[[:space:]]*\\(", perl = TRUE)
    return(invisible(NULL))
  }
  r_files <- list.files(
    r_root,
    pattern = "\\.R$", recursive = TRUE, full.names = TRUE
  )
  if (length(r_files) == 0L) {
    probe <- paste(deparse(bigbang:::.update_signal_probe), collapse = " ")
    expect_match(probe, "tools::pskill[[:space:]]*\\(", perl = TRUE)
    return(invisible(NULL))
  }
  hits <- unlist(lapply(r_files, function(file) {
    grep("tools::pskill[[:space:]]*\\(", readLines(file, warn = FALSE),
         value = TRUE, perl = TRUE)
  }), use.names = FALSE)
  expect_length(hits, 1L)
  expect_match(hits[[1L]],
               "\\.update_signal_probe[[:space:]]*<-[[:space:]]*function",
               perl = TRUE)
})
