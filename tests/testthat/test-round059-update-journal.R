round059_make_archive <- function(root, name = "round059component") {
  source <- file.path(root, "source", name)
  archives <- file.path(root, "archives")
  dir.create(file.path(source, "R"), recursive = TRUE)
  dir.create(archives, recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name), "Version: 0.1.0",
    "Title: Round 059 fixture", "Description: Journal fixture.",
    "License: MIT", "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(source, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(value)", file.path(source, "NAMESPACE"), useBytes = TRUE)
  writeLines("value <- function() 1L", file.path(source, "R", "value.R"),
             useBytes = TRUE)
  withr::with_dir(dirname(source), utils::tar(
    file.path(archives, paste0(name, "_0.1.0.tar.gz")), basename(source),
    compression = "gzip"
  ))
  file.path(archives, paste0(name, "_0.1.0.tar.gz"))
}

round059_fixture <- function(prefix = "bigbang-round059-", version = "0.1.0") {
  root <- tempfile(prefix)
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  archive <- round059_make_archive(root)
  initial <- create_metapackage(
    "round059verse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    include_archives = TRUE, version = version,
    workflow = c(Stage = "round059component")
  )
  sentinel <- file.path(initial$path, "user-owned.bin")
  writeBin(charToRaw("round059 user bytes\n"), sentinel)
  list(root = root, destination = destination, archive = archive,
       project = initial$path, name = "round059verse", sentinel = sentinel)
}

round059_update <- function(fixture, version = NULL, ...) {
  args <- list(
    name = fixture$name, packages = fixture$archive,
    dest_dir = fixture$destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), include_archives = TRUE,
    workflow = c(Stage = "round059component"), update = TRUE, ...
  )
  if (!is.null(version)) args$version <- version
  do.call(create_metapackage, args)
}

round059_dead_update <- function(fixture, ...) {
  testthat::local_mocked_bindings(
    .update_owner_may_be_alive = function(state) FALSE,
    .package = "bigbang"
  )
  round059_update(fixture, ...)
}

round059_digest_snapshot <- function(path) {
  entries <- list.files(path, all.files = TRUE, recursive = TRUE,
                        no.. = TRUE, include.dirs = FALSE)
  if (length(entries) == 0L) return(character())
  hashes <- unname(tools::md5sum(file.path(path, entries)))
  stats::setNames(hashes, entries)
}

round059_apart <- function(fixture) {
  list.files(
    fixture$destination,
    pattern = paste0("^\\.", fixture$name, "\\.bigbang-apartado-"),
    full.names = TRUE, all.files = TRUE, no.. = TRUE
  )
}

round059_make_discarded <- function(fixture, suffix = "dead") {
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  marker <- readRDS(file.path(journal$path, "marker.rds"))
  .update_journal_tombstone(journal$path, fixture$name, marker)
  discarded <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.descartado-", suffix)
  )
  stopifnot(file.rename(journal$path, discarded))
  discarded
}

test_that("F1 and glm-02 set aside every non-empty unmarked preparation", {
  skip_on_cran()
  fixture <- round059_fixture("bigbang-round059-f1-")
  armando <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.armando-user")
  )
  dir.create(armando)
  user_file <- file.path(armando, ".marker.rds-abc")
  writeLines("user bytes", user_file, useBytes = TRUE)
  before <- unname(tools::md5sum(user_file))
  result <- round059_update(fixture)
  apart <- round059_apart(fixture)
  expect_true(result$updated)
  expect_true(any(vapply(result$recovery$reconciliation,
                         function(item) !is.null(item$apart_path),
                         logical(1L))))
  expect_length(apart, 1L)
  expect_identical(unname(tools::md5sum(file.path(apart, basename(user_file)))),
                   before)
  expect_identical(readBin(fixture$sentinel, "raw", 1000L),
                   charToRaw("round059 user bytes\n"))

  glm02 <- round059_fixture("bigbang-round059-glm02-")
  armando2 <- file.path(
    glm02$destination,
    paste0(".", glm02$name, ".bigbang-update.armando-two")
  )
  dir.create(file.path(armando2, "sub"), recursive = TRUE)
  writeLines("one", file.path(armando2, "one.txt"), useBytes = TRUE)
  writeLines("two", file.path(armando2, "sub", "two.txt"), useBytes = TRUE)
  result2 <- round059_update(glm02)
  apart2 <- round059_apart(glm02)
  expect_true(result2$updated)
  expect_length(apart2, 1L)
  expect_true(file.exists(file.path(apart2, "sub", "two.txt")))
  expect_true(any(vapply(result2$recovery$reconciliation,
                         function(item) !is.null(item$apart_path),
                         logical(1L))))
})

test_that("journal-shaped non-directories are set aside without blocking", {
  skip_on_cran()
  fixture <- round059_fixture("bigbang-round059-nondirectory-")
  armando <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.armando-file")
  )
  writeLines("user armando", armando, useBytes = TRUE)
  armando_hash <- unname(tools::md5sum(armando))
  result <- round059_update(fixture)
  expect_true(result$updated)
  apart <- round059_apart(fixture)
  expect_length(apart, 1L)
  expect_identical(unname(tools::md5sum(apart[[1L]])), armando_hash)

  fixture <- round059_fixture("bigbang-round059-nondirectory-discarded-")
  discarded <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.descartado-file")
  )
  writeLines("user descartado", discarded, useBytes = TRUE)
  discarded_hash <- unname(tools::md5sum(discarded))
  result <- round059_update(fixture)
  expect_true(result$updated)
  apart <- round059_apart(fixture)
  expect_length(apart, 1L)
  expect_identical(unname(tools::md5sum(apart[[1L]])), discarded_hash)
})

test_that("F2 recursively deletes only exact inventory entries and sets aside the rest", {
  skip_on_cran()
  fixture <- round059_fixture("bigbang-round059-f2-")
  discarded <- round059_make_discarded(fixture)
  user_file <- file.path(discarded, "backup", "user-notes.txt")
  user_nested <- file.path(discarded, "backup", "user-dir", "deep.txt")
  dir.create(dirname(user_nested), recursive = TRUE)
  writeLines("private notes", user_file, useBytes = TRUE)
  writeLines("nested private bytes", user_nested, useBytes = TRUE)
  link <- file.path(discarded, "backup", "user-link")
  if (.Platform$OS.type != "windows") {
    expect_true(file.symlink(user_nested, link))
  }
  tombstone <- readRDS(file.path(discarded, "tombstone.rds"))
  mismatched_relative <- names(tombstone$inventory)[[1L]]
  mismatched_path <- file.path(discarded, mismatched_relative)
  writeLines("user replacement", mismatched_path, useBytes = TRUE)
  before_mismatched <- unname(tools::md5sum(mismatched_path))
  before_file <- unname(tools::md5sum(user_file))
  before_nested <- unname(tools::md5sum(user_nested))
  result <- round059_dead_update(fixture)
  apart <- round059_apart(fixture)
  expect_true(result$updated)
  expect_true(any(vapply(result$recovery$reconciliation,
                         function(item) !is.null(item$apart_path),
                         logical(1L))))
  expect_length(apart, 1L)
  expect_identical(unname(tools::md5sum(file.path(
    apart, "backup", "user-notes.txt"
  ))), before_file)
  expect_identical(unname(tools::md5sum(file.path(
    apart, "backup", "user-dir", "deep.txt"
  ))), before_nested)
  expect_identical(unname(tools::md5sum(file.path(apart, mismatched_relative))),
                   before_mismatched)
  if (.Platform$OS.type != "windows") {
    expect_true(.path_is_symlink(file.path(apart, "backup", "user-link")))
  }
  expect_identical(readBin(fixture$sentinel, "raw", 1000L),
                   charToRaw("round059 user bytes\n"))
})

test_that("discarding a directory symlink never reaches its external target", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round059_fixture("bigbang-round059-directory-link-")
  discarded <- round059_make_discarded(fixture)
  backup <- file.path(discarded, "backup")
  original_backup <- file.path(fixture$root, "original-backup")
  external <- file.path(fixture$root, "external-user-directory")
  expect_true(file.rename(backup, original_backup))
  expect_true(dir.create(external))
  external_files <- file.path(external, sprintf("private-%02d.bin", 1:20))
  for (index in seq_along(external_files)) {
    writeBin(charToRaw(sprintf("external bytes %02d\n", index)),
             external_files[[index]])
  }
  before <- round059_digest_snapshot(external)
  expect_true(file.symlink(external, backup))

  result <- round059_dead_update(fixture)
  apart <- round059_apart(fixture)
  expect_true(result$updated)
  expect_length(apart, 1L)
  expect_true(.path_is_symlink(file.path(apart[[1L]], "backup")))
  expect_identical(round059_digest_snapshot(external), before)
  expect_true(all(file.exists(file.path(external, basename(external_files)))))
})

test_that("F3 keeps the exact-path and exact-MD5 boundary explicit", {
  skip_on_cran()
  fixture <- round059_fixture("bigbang-round059-f3-")
  discarded <- round059_make_discarded(fixture)
  tombstone <- readRDS(file.path(discarded, "tombstone.rds"))
  relative <- names(tombstone$inventory)[[1L]]
  different_relative <- names(tombstone$inventory)[[2L]]
  bytes <- readBin(file.path(discarded, relative), "raw", 100000L)
  expect_true(file.remove(file.path(discarded, relative)))
  writeBin(bytes, file.path(discarded, relative))
  different_path <- file.path(discarded, different_relative)
  writeBin(charToRaw("different md5 bytes\n"), different_path)
  different_md5 <- unname(tools::md5sum(different_path))
  result <- round059_dead_update(fixture)
  expect_true(result$updated)
  apart <- round059_apart(fixture)
  expect_length(apart, 1L)
  expect_false(file.exists(file.path(apart[[1L]], relative)))
  expect_identical(unname(tools::md5sum(
    file.path(apart[[1L]], different_relative)
  )), different_md5)
})

test_that("F4 stale generations are set aside and the next update runs", {
  skip_on_cran()
  first <- round059_fixture("bigbang-round059-f4-first-")
  second <- round059_fixture("bigbang-round059-f4-second-")
  expect_true(round059_update(first, version = "0.1.1")$updated)
  discarded <- round059_make_discarded(second, "stale")
  target <- file.path(first$destination, basename(discarded))
  expect_true(file.rename(discarded, target))
  before <- unname(tools::md5sum(first$sentinel))
  result <- round059_dead_update(first)
  expect_true(result$updated)
  expect_true(any(vapply(result$recovery$reconciliation,
                         function(item) !is.null(item$apart_path),
                         logical(1L))))
  expect_false(dir.exists(target))
  expect_length(round059_apart(first), 1L)
  expect_identical(unname(tools::md5sum(first$sentinel)), before)
})

test_that("F5 does not adopt a copied journal while its source project exists", {
  fixture <- round059_fixture("bigbang-round059-f5-")
  copy_project <- file.path(fixture$destination, "round059copy")
  expect_true(dir.create(copy_project))
  expect_true(all(file.copy(
    list.files(fixture$project, all.files = TRUE, no.. = TRUE, full.names = TRUE),
    copy_project, recursive = TRUE
  )))
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  copied_journal <- file.path(
    fixture$destination, ".round059copy.bigbang-update"
  )
  expect_true(file.rename(journal$path, copied_journal))
  before <- round059_digest_snapshot(copied_journal)
  result <- .recover_pending_update(
    copy_project, "round059copy", recover = TRUE
  )
  expect_identical(result$action, "ignored_copy")
  expect_identical(round059_digest_snapshot(copied_journal), before)
  expect_true(dir.exists(fixture$project))
})

test_that("F6 sets aside a partial tombstone and keeps the journal recoverable", {
  skip_on_cran()
  fixture <- round059_fixture("bigbang-round059-f6-")
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  temporary <- tempfile(pattern = ".tombstone-", tmpdir = journal$path)
  saveRDS(list(partial = TRUE), temporary)
  before <- unname(tools::md5sum(temporary))
  result <- round059_dead_update(fixture)
  apart <- round059_apart(fixture)
  expect_true(result$updated)
  expect_true(any(nzchar(result$recovery$apart)))
  expect_length(apart, 1L)
  expect_identical(unname(tools::md5sum(apart[[1L]])), before)
  expect_false(dir.exists(.update_journal_path(fixture$project)))
})

test_that("a changed tombstone digest is set aside instead of trusted", {
  fixture <- round059_fixture("bigbang-round059-tombstone-digest-")
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  .update_journal_tombstone(
    journal$path, fixture$name, readRDS(file.path(journal$path, "marker.rds"))
  )
  writeLines(strrep("0", 32L), .update_journal_digest_path(journal$path),
             useBytes = TRUE)
  result <- .recover_pending_update(
    fixture$project, fixture$name, handled = TRUE
  )
  expect_identical(result$action, "apart_unauthenticated_tombstone")
  expect_length(round059_apart(fixture), 1L)
  expect_true(dir.exists(fixture$project))
})

test_that("a real SIGKILL leaves a lock that a later update reclaims", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round059_fixture("bigbang-round059-lock-")
  mark <- file.path(fixture$root, "lock-ready")
  child <- bb_mcparallel({
    lock <- bigbang:::.acquire_update_lock(fixture$project)
    writeLines("ready", mark, useBytes = TRUE)
    Sys.sleep(600)
    bigbang:::.release_update_lock(lock)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(bb_cleanup_child(child), add = TRUE)
  deadline <- Sys.time() + 30
  while (!file.exists(mark) && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(mark))
  expect_true(tools::pskill(child$pid, tools::SIGKILL))
  expect_true(!is.null(bb_collect_child(child, timeout = 1)))
  result <- round059_update(fixture)
  expect_true(result$updated)
  expect_false(dir.exists(.update_lock_path(fixture$project)))
})

test_that("uncertain lock ownership gives the recover instruction", {
  fixture <- round059_fixture("bigbang-round059-lock-message-")
  lock_path <- .update_lock_path(fixture$project)
  dir.create(lock_path)
  saveRDS(.update_owner_record(), .update_lock_owner_path(lock_path))
  testthat::local_mocked_bindings(
    .update_process_start = function(pid = Sys.getpid()) NA_character_,
    .update_owner_may_be_alive = function(state) TRUE,
    .package = "bigbang"
  )
  expect_error(
    .acquire_update_lock(fixture$project),
    class = "bigbang_error_update_in_progress",
    regexp = "recover = TRUE"
  )
  unlink(lock_path, recursive = TRUE, force = TRUE)
})
