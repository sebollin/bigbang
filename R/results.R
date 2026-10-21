#' Print an artifact scan
#'
#' @param x A `bigbang_artifact_scan` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.bigbang_artifact_scan <- function(x, ...) {
  status <- if (isTRUE(x$vulnerable)) {
    .bb_tr("VULNERABLE")
  } else {
    .bb_tr("no deletion signatures found")
  }
  cat(.bb_tr("<bigbang artifact scan>"), "\n", sep = "")
  cat("  ", .bb_tr("Path"), ": ", x$path, "\n", sep = "")
  cat("  ", .bb_tr("Type"), ": ", x$type, "\n", sep = "")
  cat("  ", .bb_tr("Result"), ": ", status, "\n", sep = "")
  if (length(x$signatures) > 0L) {
    cat("  ", .bb_tr("Signatures"), ": ",
        paste(x$signatures, collapse = ", "), "\n", sep = "")
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
  cat(.bb_tr("<bigbang metapackage>"), "\n", sep = "")
  cat("  ", .bb_tr("Package"), ": ", x$name, "\n", sep = "")
  cat("  ", .bb_tr("Path"), ": ", x$path, "\n", sep = "")
  cat("  ", .bb_tr("Components"), ": ",
      paste(x$packages, collapse = ", "), "\n", sep = "")
  if (isTRUE(x$dry_run)) {
    cat("  ", .bb_tr("Mode: dry run (no files written)"), "\n", sep = "")
  }
  if (isTRUE(x$updated)) {
    cat("  ", .bb_tr("Mode: updated from generation manifest"), "\n", sep = "")
  }
  if (isTRUE(x$recovered)) {
    cat("  ", .bb_tr("Recovered: interrupted update"), "\n", sep = "")
  }
  if (length(x$added_files) > 0L) {
    label <- if (isTRUE(x$dry_run)) "Would add" else "Added"
    label <- paste0("  ", .bb_tr(label), ": ")
    cat(label, paste(x$added_files, collapse = ", "), "\n", sep = "")
  }
  if (length(x$removed_files) > 0L) {
    label <- if (isTRUE(x$dry_run)) "Would remove" else "Removed"
    label <- paste0("  ", .bb_tr(label), ": ")
    cat(label, paste(x$removed_files, collapse = ", "), "\n", sep = "")
  }
  if (is.data.frame(x$omitted) && nrow(x$omitted) > 0L) {
    cat("  ", .bb_tr("Omitted"), ": ",
        paste(x$omitted$component, collapse = ", "), "\n", sep = "")
  }
  if (is.data.frame(x$reexports) && nrow(x$reexports) > 0L) {
    summary <- paste0(
      x$reexports$symbol, " -> ", x$reexports$package,
      " (", x$reexports$resolution, ")"
    )
    cat("  ", .bb_tr("Re-exports"), ": ",
        paste(summary, collapse = ", "), "\n", sep = "")
  }
  if (length(x$reexport_excluded) > 0L) {
    cat("  ", .bb_tr("Re-exports excluded"), ": ",
        paste(x$reexport_excluded, collapse = ", "), "\n", sep = "")
  }
  if (length(x$cran_dependencies) > 0L) {
    cat("  ", .bb_tr("Non-local dependencies"), ": ",
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
  cat(.bb_tr("<bigbang local installation>"), "\n", sep = "")
  cat("  ", .bb_tr("Installed"), ": ", length(x$installed), "\n", sep = "")
  cat("  ", .bb_tr("Unchanged"), ": ", length(x$unchanged), "\n", sep = "")
  cat("  ", .bb_tr("Failed"), ": ", length(x$failed), "\n", sep = "")
  cat("  ", .bb_tr("Skipped"), ": ", length(x$skipped), "\n", sep = "")
  invisible(x)
}
