# Minimal versioned negative control for the removed round-060 rule.
.round060_source_mutant <- function(package_root, package = NULL) {
  files <- sort(list.files(
    file.path(package_root, "R"),
    pattern = "\\.(R|r|S|s|q)$",
    full.names = TRUE,
    recursive = TRUE
  ))
  parsed <- lapply(files, parse, keep.source = TRUE)
  top_level <- sum(vapply(parsed, length, integer(1L)))
  if (top_level != 1L) {
    stop("length zero or length > 1", call. = FALSE)
  }
  list(package = package, parsed = parsed)
}
