# Under `R CMD check --as-cran`, R puts placeholder copies of the recommended
# packages on R_LIBS (each holding a `dummy_for_check` file) so that an
# undeclared use fails. The meta-packages these tests generate are checked in
# child processes that inherit that R_LIBS, and their Rd cross-reference check
# stops on the MASS placeholder: R 4.1 reports it as a WARNING, later versions
# discard it, so the check verifies nothing there in any version. Skip it in
# the children only while the placeholders are present; outside
# `R CMD check` it still runs.
library_values <- c(Sys.getenv("R_LIBS"), Sys.getenv("R_LIBS_USER"))
libraries <- unlist(strsplit(
  library_values, .Platform$path.sep, fixed = TRUE
), use.names = FALSE)
libraries <- unique(libraries[nzchar(libraries)])
has_placeholders <- vapply(libraries, function(library) {
  package_dirs <- list.files(
    library, all.files = TRUE, no.. = TRUE, full.names = TRUE
  )
  any(file.exists(file.path(package_dirs, "dummy_for_check")))
}, logical(1L))
if (any(has_placeholders)) {
  cleanup_env <- getOption("bigbang.nested_check_cleanup_env")
  if (is.null(cleanup_env)) cleanup_env <- testthat::teardown_env()
  withr::local_envvar(
    "_R_CHECK_RD_XREFS_" = "false",
    .local_envir = cleanup_env
  )
}
