test_that("install_local_pkg removes its temporary directory after early exit", {
  sandbox <- tempfile("install-helper-")
  dir.create(sandbox)
  content <- file.path(sandbox, "content")
  dir.create(content)
  writeLines("missing DESCRIPTION", file.path(content, "file.txt"))
  withr::with_dir(content, utils::tar(
    file.path(sandbox, "roto_0.1.0.tar.gz"),
    files = "file.txt", compression = "gzip"
  ))

  before <- list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
  result <- install_local_pkg("roto_0.1.0", sandbox)
  expect_named(result$failed, "roto_0.1.0")
  expect_match(result$failed[[1L]], "one package root directory")
  after <- list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
  expect_setequal(after, before)
})

test_that("install_local_pkg reports a missing archive without side effects", {
  sandbox <- tempfile("install-helper-missing-")
  dir.create(sandbox)
  before <- list.files(sandbox, all.files = TRUE, no.. = TRUE)

  expect_message(
    result <- install_local_pkg(
      "absent_0.1.0", sandbox, cran_deps = "skip", verbose = TRUE
    ),
    "Packages that failed: absent_0.1.0"
  )

  expect_s3_class(result, "bigbang_install_result")
  expect_named(result$failed, "absent_0.1.0")
  expect_match(result$failed[[1L]], "Package archive does not exist")
  expect_identical(list.files(sandbox, all.files = TRUE, no.. = TRUE), before)
  expect_identical(.classify_local_archive("unused.tar.gz", ".tar.gz"), "source")
})

test_that("install_local_pkg recognizes a package version already installed", {
  sandbox <- tempfile("install-helper-present-")
  dir.create(sandbox)
  stem <- paste0("stats_", as.character(utils::packageVersion("stats")))
  archive <- file.path(sandbox, paste0(stem, ".tar.gz"))
  expect_true(file.create(archive))

  result <- install_local_pkg(stem, sandbox, verbose = FALSE)

  expect_s3_class(result, "bigbang_install_result")
  expect_identical(result$unchanged[[stem]], "Already installed")
  expect_length(result$installed, 0L)
  expect_length(result$failed, 0L)
  expect_length(result$skipped, 0L)
  expect_true(file.exists(archive))
})

test_that("force and upgrade policies control local reinstallation", {
  skip_on_cran()
  sandbox <- tempfile("install-helper-upgrade-")
  source_root <- file.path(sandbox, "source")
  archives <- file.path(sandbox, "archives")
  library <- file.path(sandbox, "library")
  package_dir <- file.path(source_root, "upgradepkg")
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  dir.create(archives)
  dir.create(library)
  withr::local_libpaths(c(library, .libPaths()))

  write_package <- function(version) {
    writeLines(c(
      "Package: upgradepkg",
      paste0("Version: ", version),
      "Title: Upgrade Policy Fixture",
      "Description: A temporary package for installation policy tests.",
      "Authors@R: person('T','A',email='t@example.org',role=c('aut','cre'))",
      "License: MIT"
    ), file.path(package_dir, "DESCRIPTION"))
    writeLines(character(), file.path(package_dir, "NAMESPACE"))
    writeLines("fixture_value <- 1L", file.path(package_dir, "R", "fixture.R"))
    archive <- file.path(archives, paste0("upgradepkg_", version, ".tar.gz"))
    withr::with_dir(source_root, utils::tar(
      archive, files = "upgradepkg", compression = "gzip"
    ))
    invisible(archive)
  }

  write_package("0.1.0")
  first <- install_local_pkg("upgradepkg_0.1.0", archives, verbose = FALSE)
  expect_named(first$installed, "upgradepkg_0.1.0")

  unchanged <- install_local_pkg(
    "upgradepkg_0.1.0", archives, verbose = FALSE
  )
  expect_named(unchanged$unchanged, "upgradepkg_0.1.0")

  forced <- install_local_pkg(
    "upgradepkg_0.1.0", archives, force = TRUE, verbose = FALSE
  )
  expect_named(forced$installed, "upgradepkg_0.1.0")

  write_package("0.2.0")
  never <- install_local_pkg(
    "upgradepkg_0.2.0", archives, upgrade = "never", verbose = FALSE
  )
  expect_named(never$unchanged, "upgradepkg_0.2.0")
  expect_identical(as.character(utils::packageVersion("upgradepkg")), "0.1.0")

  newer <- install_local_pkg(
    "upgradepkg_0.2.0", archives, upgrade = "newer", verbose = FALSE
  )
  expect_named(newer$installed, "upgradepkg_0.2.0")
  expect_identical(as.character(utils::packageVersion("upgradepkg")), "0.2.0")
})

test_that("force rejects an explicitly conflicting upgrade policy", {
  expect_error(
    install_local_pkg(
      "unused_0.1.0", tempdir(), force = TRUE, upgrade = "never"
    ),
    class = "bigbang_error_install_policy"
  )
  expect_error(
    install_local_pkg("unused_0.1.0", tempdir(), force = NA),
    class = "bigbang_error_install_policy"
  )
})

test_that("ZIP classification rejects archives without DESCRIPTION", {
  sandbox <- tempfile("install-helper-zip-")
  dir.create(sandbox)
  writeLines("not a package", file.path(sandbox, "payload.txt"))
  archive <- file.path(sandbox, "invalid.zip")
  invisible(capture.output(
    withr::with_dir(sandbox, utils::zip(archive, "payload.txt", flags = "-q"))
  ))

  expect_error(
    .classify_local_archive(archive, ".zip"),
    "does not contain a DESCRIPTION"
  )
  expect_true(file.exists(archive))
})
