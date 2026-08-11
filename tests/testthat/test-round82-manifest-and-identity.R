round82_make_archive <- function(source_root, archive_dir, name, version = "1.0.0") {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name),
    paste0("Version: ", version),
    paste0("Title: Round 82 fixture ", name),
    paste0("Description: Temporary fixture for manifest tests."),
    "License: MIT",
    "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(fixture_value)", file.path(package_dir, "NAMESPACE"),
             useBytes = TRUE)
  writeLines(
    "fixture_value <- function() 1L", file.path(package_dir, "R", "value.R"),
    useBytes = TRUE
  )
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(source_root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

test_that("manifest archive filenames fall back to supplied pkg_dir", {
  root <- tempfile("bigbang-round82-manifest-")
  manifest_dir <- file.path(root, "repository")
  archive_dir <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  source_root <- file.path(root, "sources")
  dir.create(manifest_dir, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(source_root)

  archive <- round82_make_archive(source_root, archive_dir, "ccc")
  manifest <- file.path(manifest_dir, "components.txt")
  writeLines("ccc_1.0.0.tar.gz", manifest, useBytes = TRUE)
  result <- create_metapackage(
    "manifestpkgdirverse", manifest, pkg_dir = archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
  expect_identical(result$packages, "ccc")
  expect_true(file.exists(file.path(
    result$path, "inst", "archives", basename(archive)
  )))
})

test_that("manifest entries can mix files beside the manifest and in pkg_dir", {
  root <- tempfile("bigbang-round82-mixed-")
  manifest_dir <- file.path(root, "repository")
  archive_dir <- file.path(root, "archives")
  source_root <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  dir.create(manifest_dir, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(source_root)
  dir.create(destination)
  beside <- round82_make_archive(source_root, manifest_dir, "aaa")
  external <- round82_make_archive(source_root, archive_dir, "bbb")
  manifest <- file.path(manifest_dir, "components.txt")
  writeLines(c(basename(beside), basename(external)), manifest, useBytes = TRUE)

  result <- create_metapackage(
    "mixedmanifestverse", manifest, pkg_dir = archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
  expect_setequal(result$packages, c("aaa", "bbb"))
})

test_that("missing manifest archive names report every search directory", {
  root <- tempfile("bigbang-round82-missing-")
  manifest_dir <- file.path(root, "repository")
  archive_dir <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(manifest_dir, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  manifest <- file.path(manifest_dir, "components.txt")
  writeLines("missing_1.0.0.tar.gz", manifest, useBytes = TRUE)

  error <- tryCatch(
    create_metapackage(
      "missingmanifestverse", manifest, pkg_dir = archive_dir,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character()
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  message <- conditionMessage(error)
  expect_true(grepl("missing_1.0.0.tar.gz", message, fixed = TRUE))
  expect_true(grepl(normalizePath(manifest_dir, winslash = "/"), message,
                    fixed = TRUE))
  expect_true(grepl(normalizePath(archive_dir, winslash = "/"), message,
                    fixed = TRUE))
})

test_that("bigbang installation messages name declared identity and archive stem", {
  root <- tempfile("bigbang-round82-identity-")
  source_root <- file.path(root, "sources")
  archive_dir <- file.path(root, "archives")
  library_dir <- file.path(root, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(library_dir)
  archive <- round82_make_archive(source_root, archive_dir, "bbb")
  renamed <- file.path(archive_dir, "wrong_9.9.tar.gz")
  file.rename(archive, renamed)

  withr::local_libpaths(c(library_dir, .libPaths()))
  messages <- suppressWarnings(capture.output(
    install_local_pkg(
      renamed, verbose = TRUE, upgrade = "always"
    ),
    type = "message"
  ))
  expect_true(any(grepl(
    "Installed local package: bbb from wrong_9.9", messages, fixed = TRUE
  )))
  expect_false(any(grepl(
    "Installed local package: wrong_9.9", messages, fixed = TRUE
  )))
})
