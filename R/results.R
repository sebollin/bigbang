#' Print an artifact scan
#'
#' @param x A `bigbang_artifact_scan` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.bigbang_artifact_scan <- function(x, ...) {
  status <- if (isTRUE(x$vulnerable)) "VULNERABLE" else "no deletion signatures found"
  cat("<bigbang artifact scan>\n")
  cat("  Path: ", x$path, "\n", sep = "")
  cat("  Type: ", x$type, "\n", sep = "")
  cat("  Result: ", status, "\n", sep = "")
  if (length(x$signatures) > 0L) {
    cat("  Signatures: ", paste(x$signatures, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' Print a metapackage generation result
#'
#' @param x A `bigbang_result` object returned by [create_metapackage()].
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.bigbang_result <- function(x, ...) {
  cat("<bigbang metapackage>\n")
  cat("  Package: ", x$name, "\n", sep = "")
  cat("  Path: ", x$path, "\n", sep = "")
  cat("  Components: ", paste(x$packages, collapse = ", "), "\n", sep = "")
  if (length(x$cran_dependencies) > 0L) {
    cat("  Non-local dependencies: ",
        paste(x$cran_dependencies, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' Print a local package installation result
#'
#' @param x A `bigbang_install_result` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.bigbang_install_result <- function(x, ...) {
  cat("<bigbang local installation>\n")
  cat("  Installed: ", length(x$installed), "\n", sep = "")
  cat("  Failed: ", length(x$failed), "\n", sep = "")
  cat("  Skipped: ", length(x$skipped), "\n", sep = "")
  invisible(x)
}
