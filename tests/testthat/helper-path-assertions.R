.canonical_test_path <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  normalized <- gsub("\\\\+", "/", normalized)
  if (startsWith(normalized, "//?/UNC/")) {
    return(paste0("//", substring(normalized, 9L)))
  }
  if (startsWith(normalized, "//?/")) return(substring(normalized, 5L))
  normalized
}

contains_path <- function(path, text) {
  needle <- .canonical_test_path(path)
  canonical_text <- gsub("\\\\+", "/", paste(text, collapse = "\n"))
  canonical_text <- gsub("//?/UNC/", "//", canonical_text, fixed = TRUE)
  canonical_text <- gsub("//?/", "", canonical_text, fixed = TRUE)
  grepl(needle, canonical_text, fixed = TRUE)
}

expect_path_absent <- function(path, text) {
  # Every negative assertion carries its own positive control. This prevents a
  # separator or path-prefix mismatch from making the absence check vacuous.
  expect_true(contains_path(path, c(text, .canonical_test_path(path))))
  expect_false(contains_path(path, text))
  invisible(NULL)
}
