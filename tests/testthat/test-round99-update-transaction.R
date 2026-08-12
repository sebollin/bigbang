round99_make_archive <- function(source_root, archive_dir, name,
                                 version = "0.1.0") {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name),
    paste0("Version: ", version),
    paste0("Title: Round 99 fixture ", name),
    "Description: Temporary component for transactional update tests.",
    "License: MIT",
    "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(value)", file.path(package_dir, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("value <- function() 1L", file.path(package_dir, "R", "value.R"),
             useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(source_root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

round99_snapshot <- function(project) {
  manifest_path <- file.path(project, .generation_manifest_name)
  manifest <- readRDS(manifest_path)
  relative <- unique(c(manifest$files, .generation_manifest_name))
  paths <- file.path(project, relative)
  list(
    relative = relative,
    exists = file.exists(paths),
    hashes = stats::setNames(
      unname(as.character(tools::md5sum(paths))), relative
    )
  )
}

round99_expect_unchanged <- function(project, snapshot) {
  paths <- file.path(project, snapshot$relative)
  expect_identical(file.exists(paths), snapshot$exists)
  expect_identical(
    stats::setNames(unname(as.character(tools::md5sum(paths))),
                    snapshot$relative),
    snapshot$hashes
  )
}

round99_fixture <- function(prefix) {
  root <- tempfile(prefix)
  sources <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(sources, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  keep <- round99_make_archive(sources, archives, "keepnine")
  drop <- round99_make_archive(sources, archives, "dropnine")
  initial <- create_metapackage(
    "roundnineverse", c(keep, drop), dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(),
    workflow = c("Keep" = "keepnine", "Drop" = "dropnine")
  )
  list(
    root = root, archives = archives, destination = destination,
    keep = keep, drop = drop, project = initial$path
  )
}

test_that("a failed update restores every pre-existing generated file", {
  fixture <- round99_fixture("bigbang-update-write-failure-")
  before <- round99_snapshot(fixture$project)
  shipped_drop <- file.path(
    fixture$project, "inst", "archives", basename(fixture$drop)
  )
  unlink(fixture$drop)
  expect_false(file.exists(fixture$drop))
  expect_true(file.exists(shipped_drop))

  local({
    testthat::local_mocked_bindings(
      write_basic_vignette = function(...) stop("forced update write failure"),
      .package = "bigbang"
    )
    expect_error(
      create_metapackage(
        "roundnineverse", fixture$keep, dest_dir = fixture$destination,
        document = FALSE, verbose = FALSE, import_deps = character(),
        force_deps = character(), update = TRUE
      ),
      "forced update write failure"
    )
  })

  round99_expect_unchanged(fixture$project, before)
  expect_true(file.exists(shipped_drop))

  retry <- create_metapackage(
    "roundnineverse", fixture$keep, dest_dir = fixture$destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), update = TRUE
  )
  expect_true(isTRUE(retry$updated))
  expect_true(file.path("inst", "archives", basename(fixture$drop)) %in%
                retry$removed_files)
  expect_false(file.exists(shipped_drop))
})

test_that("a partial stale-file removal is rolled back and remains retryable", {
  fixture <- round99_fixture("bigbang-update-removal-failure-")
  before <- round99_snapshot(fixture$project)
  calls <- 0L

  local({
    testthat::local_mocked_bindings(
      .stale_unlink = function(path) {
        calls <<- calls + 1L
        if (calls == 1L) {
          unlink(path, recursive = FALSE, force = TRUE)
        } else {
          1L
        }
      },
      .package = "bigbang"
    )
    expect_error(
      create_metapackage(
        "roundnineverse", fixture$keep, dest_dir = fixture$destination,
        document = FALSE, verbose = FALSE, import_deps = character(),
        force_deps = character(), update = TRUE
      ),
      "Could not remove completely"
    )
  })

  expect_gte(calls, 2L)
  round99_expect_unchanged(fixture$project, before)

  expect_message(
    retry <- create_metapackage(
      "roundnineverse", fixture$keep, dest_dir = fixture$destination,
      document = FALSE, verbose = TRUE, import_deps = character(),
      force_deps = character(), update = TRUE
    ),
    "Removing generated files no longer in the plan:"
  )
  expect_true(isTRUE(retry$updated))
  expect_true(file.path("inst", "archives", basename(fixture$drop)) %in%
                retry$removed_files)
})

test_that("an update dry run reports every generated file it would remove", {
  fixture <- round99_fixture("bigbang-update-dry-run-")
  before <- round99_snapshot(fixture$project)
  shipped_drop <- file.path("inst", "archives", basename(fixture$drop))
  workflow <- file.path("vignettes", "workflow-roundnineverse.Rmd")

  plan <- create_metapackage(
    "roundnineverse", fixture$keep, dest_dir = fixture$destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), update = TRUE, dry_run = TRUE
  )

  expect_true(isTRUE(plan$dry_run))
  expect_true(all(c(shipped_drop, workflow) %in% plan$removed_files))
  expect_output(print(plan), "Would remove:", fixed = TRUE)
  expect_output(print(plan), shipped_drop, fixed = TRUE)
  round99_expect_unchanged(fixture$project, before)
})

test_that("an incomplete restoration reports failure and preserves its backup", {
  fixture <- round99_fixture("bigbang-update-restore-failure-")
  manifest <- readRDS(file.path(
    fixture$project, .generation_manifest_name
  ))
  backup <- .create_update_backup(fixture$project, manifest)
  on.exit(.discard_update_backup(backup), add = TRUE)

  local({
    testthat::local_mocked_bindings(
      .atomic_copy = function(...) stop("forced restoration failure"),
      .package = "bigbang"
    )
    expect_warning(
      restored <- .restore_update_backup(fixture$project, backup),
      "Could not restore generated files after a failed update"
    )
    expect_false(restored)
  })

  expect_true(dir.exists(backup$path))
  expect_invisible(.discard_update_backup(NULL))
})

test_that("a failed backup is removed before the project can be changed", {
  fixture <- round99_fixture("bigbang-update-backup-failure-")
  pattern <- file.path(tempdir(), "bigbang-update-backup-*")
  before <- Sys.glob(pattern)

  expect_error(
    suppressWarnings(.create_update_backup(
      fixture$project, list(files = "missing-generated-file")
    )),
    "Could not back up generated file"
  )

  expect_setequal(Sys.glob(pattern), before)
})
