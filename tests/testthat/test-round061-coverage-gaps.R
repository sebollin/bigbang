round061_make_coverage_archive <- function(root, name, imports = NULL,
                                           source = "value <- 1L",
                                           with_r = TRUE) {
  package_root <- file.path(root, name)
  dir.create(package_root, recursive = TRUE)
  if (isTRUE(with_r)) dir.create(file.path(package_root, "R"), recursive = TRUE)
  description <- c(
    paste0("Package: ", name),
    "Version: 0.1.0",
    "Title: Round 061 coverage fixture",
    "Description: Coverage fixture.",
    "License: MIT",
    "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  )
  if (!is.null(imports)) description <- c(description,
                                          paste0("Imports: ", imports))
  writeLines(description, file.path(package_root, "DESCRIPTION"),
             useBytes = TRUE)
  writeLines("export(value)", file.path(package_root, "NAMESPACE"),
             useBytes = TRUE)
  if (isTRUE(with_r)) {
    writeLines(source, file.path(package_root, "R", "value.R"),
               useBytes = TRUE)
  }
  archive <- file.path(root, paste0(name, "_0.1.0.tar.gz"))
  withr::with_dir(root, utils::tar(archive, name, compression = "gzip"))
  archive
}

test_that("install policy failures and offline dependency results are covered", {
  skip_on_cran()
  root <- tempfile("bigbang-round061-coverage-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- round061_make_coverage_archive(
    root, "coveragecomponent", imports = "missinground061dependency"
  )
  lib <- file.path(root, "library")

  invalid <- install_local_pkg(
    "not-an-archive", pkg_dir = root, lib = lib, verbose = FALSE
  )
  expect_true("not-an-archive" %in% names(invalid$failed))
  expect_error(
    install_local_pkg(archive, lib = "", verbose = FALSE),
    "one non-empty path"
  )
  expect_error(
    install_local_pkg(
      archive, lib = lib, force = TRUE, upgrade = "never", verbose = FALSE
    ),
    "conflicts"
  )

  skipped <- install_local_pkg(
    archive, lib = lib, cran_deps = "skip", verbose = FALSE
  )
  expect_true("coveragecomponent_0.1.0" %in% names(skipped$skipped))
  failed <- install_local_pkg(
    archive, lib = lib, cran_deps = "error", verbose = FALSE
  )
  expect_true("coveragecomponent_0.1.0" %in% names(failed$failed))
  no_repository <- install_local_pkg(
    archive, lib = lib, cran_deps = "install", repos = NULL,
    verbose = FALSE
  )
  expect_true("coveragecomponent_0.1.0" %in% names(no_repository$failed))

  installable <- round061_make_coverage_archive(root, "installedcomponent")
  installed <- install_local_pkg(
    installable, lib = lib, verbose = FALSE
  )
  expect_true("installedcomponent_0.1.0" %in% names(installed$installed))
  kept_never <- install_local_pkg(
    installable, lib = lib, upgrade = "never", verbose = FALSE
  )
  expect_true("installedcomponent_0.1.0" %in% names(kept_never$unchanged))
  kept_newer <- install_local_pkg(
    installable, lib = lib, upgrade = "newer", verbose = FALSE
  )
  expect_true("installedcomponent_0.1.0" %in% names(kept_newer$unchanged))
  reinstalled <- install_local_pkg(
    installable, lib = lib, upgrade = "always", verbose = FALSE
  )
  expect_true("installedcomponent_0.1.0" %in% names(reinstalled$installed))
})

test_that("dependency diagnosis scans both reference families", {
  root <- tempfile("bigbang-round061-diagnose-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- round061_make_coverage_archive(
    root, "diagnosecomponent", imports = "Matrix",
    source = c(
      "value <- Matrix::Matrix(1)",
      "class(value)"
    )
  )
  result <- diagnose_dependencies(archive)
  expect_true(length(result[[archive]]$matrix_refs) > 0L)
  expect_true(length(result[[archive]]$class_refs) > 0L)

  empty_archive <- round061_make_coverage_archive(
    root, "emptydiagnosecomponent", with_r = FALSE
  )
  empty_result <- diagnose_dependencies(empty_archive)
  expect_length(empty_result, 0L)
})
