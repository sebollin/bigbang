# Under `R CMD check --as-cran`, R puts placeholder copies of the recommended
# packages on R_LIBS (each holding a `dummy_for_check` file) so that an
# undeclared use fails. The meta-packages these tests generate are checked in
# child processes that inherit that R_LIBS, and their Rd cross-reference check
# stops on the MASS placeholder: R 4.1 reports it as a WARNING, later versions
# discard it, so the check verifies nothing there in any version. Skip it in
# the children only while the placeholders are present; outside
# `R CMD check` it still runs.
local({
  libraries <- strsplit(Sys.getenv("R_LIBS"), .Platform$path.sep,
                        fixed = TRUE)[[1L]]
  has_placeholders <- vapply(libraries, function(library) {
    length(Sys.glob(file.path(library, "*", "dummy_for_check"))) > 0L
  }, logical(1L))
  if (any(has_placeholders)) {
    withr::local_envvar(
      "_R_CHECK_RD_XREFS_" = "false",
      .local_envir = testthat::teardown_env()
    )
  }
})
