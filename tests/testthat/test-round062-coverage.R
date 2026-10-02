test_that("journal traversal handles empty and linked entries", {
  skip_on_os("windows")
  root <- tempfile("bigbang-round062-coverage-journal-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  empty <- file.path(root, "empty")
  dir.create(empty)
  expect_length(.update_journal_inventory(empty)$files, 0L)
  expect_false(.update_journal_digest_valid(empty))

  external <- file.path(root, "external")
  dir.create(external)
  writeLines("outside", file.path(external, "file"), useBytes = TRUE)
  linked <- file.path(root, "linked")
  if (!isTRUE(suppressWarnings(file.symlink(external, linked)))) {
    skip("symbolic links are unavailable")
  }
  expect_length(.update_journal_entries(linked), 0L)
  expect_true(.journal_has_link_ancestor(root, "linked/file"))
  expect_error(.update_journal_inventory(root), "Could not write")
})

test_that("journal diagnostics and set-aside helpers cover their guards", {
  expect_true(nzchar(.update_journal_kind_label("unarmed")))
  expect_true(nzchar(.update_journal_kind_label("discarded")))
  expect_true(nzchar(.update_journal_kind_label("other")))
  expect_error(
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal", "project", "journal",
      "unknown", "invalid", has_backup = FALSE, next_step = "inspect"
    )
  )
  expect_error(
    .update_journal_problem(
      "bigbang_error_update_in_progress", "project", "journal", "armed",
      "busy", has_backup = TRUE, next_step = "wait"
    )
  )
  expect_null(.update_tombstone_owner(list()))
  owner <- list(pid = 1L, host = "host", started_utc = "now",
                process_start = "start")
  expect_identical(.update_tombstone_owner(owner), owner)

  root <- tempfile("bigbang-round062-coverage-apart-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  journal <- file.path(root, ".project.bigbang-update")
  dir.create(journal)
  expect_message(
    apart <- .apart_update_journal(journal, "project"),
    "Set aside unverified"
  )
  expect_true(dir.exists(apart))
  lock <- file.path(root, "lock")
  dir.create(lock)
  expect_message(
    lock_apart <- .apart_update_lock(lock, "project"),
    "Set aside update-lock"
  )
  expect_true(dir.exists(lock_apart))
})

test_that("digest and discarded-journal validation reject unsafe states", {
  root <- tempfile("bigbang-round062-coverage-discard-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  expect_error(
    .finish_discarded_journal(root, "project"),
    "tombstone is missing or invalid"
  )

  tombstone <- list(
    name = "other", manifest_hash = paste(rep("0", 32L), collapse = ""),
    inventory = character(), inventory_directories = character()
  )
  expect_message(
    result <- .finish_discarded_journal(
      root, "project", tombstone, project_dir = root,
      verify_project = TRUE
    ),
    "Set aside unverified"
  )
  withr::defer(unlink(result$apart, recursive = TRUE, force = TRUE))
  expect_true(dir.exists(result$apart))

  directory_case <- tempfile("bigbang-round062-coverage-directory-")
  dir.create(directory_case)
  dir.create(file.path(directory_case, "sub"))
  withr::defer(unlink(directory_case, recursive = TRUE, force = TRUE))
  testthat::local_mocked_bindings(
    .discard_update_entry = function(...) invisible(NULL),
    .package = "bigbang"
  )
  expect_error(
    .finish_discarded_journal(
      directory_case, "project", list(
        name = "project", manifest_hash = "", inventory = character(),
        inventory_directories = "sub"
      )
    ),
    "Could not remove completely"
  )
})

test_that("sibling reconciliation plans empty and unarmed entries", {
  root <- tempfile("bigbang-round062-coverage-siblings-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  project <- file.path(root, "project")
  dir.create(project)
  empty <- file.path(root, ".project.bigbang-update.armando-empty")
  dir.create(empty)
  unarmed <- file.path(root, ".project.bigbang-update.armando-unarmed")
  dir.create(unarmed)
  writeLines("unknown", file.path(unarmed, "unknown"), useBytes = TRUE)
  discarded_empty <- file.path(root, ".project.bigbang-update.descartado-empty")
  dir.create(discarded_empty)
  discarded_nonempty <- file.path(
    root, ".project.bigbang-update.descartado-nonempty"
  )
  dir.create(discarded_nonempty)
  writeLines("unknown", file.path(discarded_nonempty, "unknown"),
             useBytes = TRUE)

  plan <- .reconcile_update_siblings(
    project, "project", dry_run = TRUE, recover = FALSE
  )
  actions <- vapply(plan, `[[`, character(1L), "action")
  expect_true(all(c("discard_empty", "apart_unarmed", "apart_discarded") %in%
                    actions))
})

test_that("intent helpers reject destinations outside the project", {
  root <- tempfile("bigbang-round062-coverage-intents-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  expect_identical(.project_relative_destination(
    file.path(root, "file"), root
  ), "file")
  expect_null(.project_relative_destination(tempfile(), root))
  expect_null(.project_relative_destination(file.path(root, "../file"), root))
  expect_null(.record_update_write(tempfile(), tempfile()))
  expect_null(.record_update_delete(tempfile()))
  expect_false(.staged_update_write_matches(root, "file", "hash"))
})

test_that("filesystem guards cover links, literals, and safe removal", {
  skip_on_os("windows")
  root <- tempfile("bigbang-round062-coverage-fs-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  target <- file.path(root, "target")
  dir.create(target)
  link <- file.path(root, "link")
  if (!isTRUE(suppressWarnings(file.symlink(target, link)))) {
    skip("symbolic links are unavailable")
  }
  expect_true(.path_is_symlink(link))
  expect_error(.validate_project_root_path(link), class = "bigbang_error_symlink_generated_path")
  expect_error(
    .validate_project_write_paths(root, "link/file"),
    class = "bigbang_error_symlink_generated_path"
  )
  expect_identical(.symlink_in_project_path(root, "../outside"), "")
  expect_identical(
    .symlink_in_project_path(root, link),
    gsub("\\\\", "/", link)
  )
  expect_identical(is_path_inside(file.path(root, "child"), root), TRUE)
  expect_identical(is_path_inside(file.path(root, "other"), dirname(root)), TRUE)

  physical <- file.path(root, "physical")
  dir.create(file.path(physical, "nuevo"), recursive = TRUE)
  outer <- file.path(root, "outer")
  dir.create(outer)
  link_outside <- file.path(outer, "sublink")
  expect_true(isTRUE(file.symlink(physical, link_outside)))
  expect_true(is_path_inside(outer, outer))
  expect_true(is_path_inside(tempdir(), tempdir()))
  expect_false(is_path_inside(file.path(outer, "sublink", ".."), outer))
  expect_false(is_path_inside(file.path(outer, "sublink", "nuevo"), outer))
  outer_with_parent <- file.path(root, "container", "..")
  dir.create(file.path(root, "container"))
  expect_true(is_path_inside(file.path(root, "child"), outer_with_parent))

  expect_match(.escape_non_ascii("A\n\u00e1\U0001f600"), "\\\\u000a")
  expect_match(.r_string_literal("quote\" slash\\"), "\\\\\"")
  expect_match(.r_string_literal("\u0001"), "\\\\u0001")
  expect_match(.r_ascii_literal(list(a = 1L, b = TRUE, c = NULL)),
               "structure")
  expect_match(.r_ascii_literal(character()), "character\\(\\)")
  expect_match(.r_ascii_literal(c("a", NA_character_)), "NA_character_")
  expect_match(.copyright_holders("person('A', 'B')"), "A B")
  expect_identical(.copyright_holders("not valid("), "Authors listed in Authors@R")
  expect_error(.r_string_literal(NA_character_), "one non-NA")
  expect_error(.validate_archive_members(c("ok", "../unsafe")), "unsafe")
  expect_identical(.validate_archive_members("ok/file"), "ok/file")

  sandbox <- tempfile("bigbang-round062-safe-root-")
  dir.create(sandbox)
  withr::defer(unlink(sandbox, recursive = TRUE, force = TRUE))
  safe_unlink_local <- safe_unlink
  safe_environment <- new.env(parent = environment(safe_unlink_local))
  safe_environment$tempdir <- function() sandbox
  environment(safe_unlink_local) <- safe_environment
  expect_false(safe_unlink_local(sandbox, recursive = TRUE, force = TRUE))
  outside <- tempfile("bigbang-round062-outside-package-")
  dir.create(file.path(outside, "R"), recursive = TRUE)
  writeLines("Package: outside", file.path(outside, "DESCRIPTION"),
             useBytes = TRUE)
  withr::defer(unlink(outside, recursive = TRUE, force = TRUE))
  expect_false(safe_unlink_local(outside, recursive = TRUE, force = TRUE))
  removable <- tempfile("bigbang-round062-removable-")
  dir.create(removable)
  expect_equal(safe_unlink(removable, recursive = TRUE, force = TRUE), 0L)
  expect_false(safe_unlink("/", verify = TRUE))
  expect_false(safe_unlink("R", verify = TRUE))
})

test_that("scanner validates artifact shapes and source boundaries", {
  root <- tempfile("bigbang-round062-coverage-scanner-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  expect_error(scan_bigbang_artifact(character()), "one non-empty")
  expect_error(scan_bigbang_artifact(NA_character_), "one non-empty")
  expect_error(scan_bigbang_artifact(file.path(root, "missing")), "does not exist")
  plain <- file.path(root, "plain.txt")
  writeLines("not an artifact", plain, useBytes = TRUE)
  expect_error(scan_bigbang_artifact(plain), "Unsupported artifact")
  expect_error(scan_bigbang_artifact(root, dry_run = FALSE), "read-only")

  expect_true(is.na(.read_provenance(file.path(root, "missing-DESCRIPTION"))$Package))
  expect_length(.scan_signature_text("value <- 1", "source:1")$signatures, 0L)
  expect_identical(.scan_code_tokens(c("value <- 1", "not valid (")),
                   c("value <- 1", "not valid ("))

  malformed <- file.path(root, "malformed")
  dir.create(file.path(malformed, "R"), recursive = TRUE)
  writeLines("Package: malformed", file.path(malformed, "DESCRIPTION"),
             useBytes = TRUE)
  expect_false(scan_bigbang_artifact(malformed)$vulnerable)
  link <- file.path(malformed, "R", "linked.R")
  source <- file.path(root, "source.R")
  writeLines("value <- 1", source, useBytes = TRUE)
  if (!isTRUE(suppressWarnings(file.symlink(source, link)))) {
    skip("symbolic links are unavailable")
  }
  expect_error(scan_bigbang_artifact(malformed), "symbolic links")

  package <- file.path(root, "zipmeta")
  dir.create(file.path(package, "R"), recursive = TRUE)
  writeLines(c(
    "Package: zipmeta", "Version: 0.1.0", "Title: ZIP fixture",
    "Description: ZIP scanner fixture.", "License: MIT"
  ), file.path(package, "DESCRIPTION"), useBytes = TRUE)
  writeLines("value <- 1", file.path(package, "R", "value.R"), useBytes = TRUE)
  zip <- file.path(root, "zipmeta_0.1.0.zip")
  withr::with_dir(root, utils::zip(zip, "zipmeta", flags = "-rq"))
  expect_identical(scan_bigbang_artifact(zip)$type, "archive")
})

test_that("filesystem fallbacks and local archive policies are covered", {
  root <- tempfile("bigbang-round062-coverage-fallbacks-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  source <- file.path(root, "source")
  destination <- file.path(root, "destination")
  external <- file.path(root, "external")
  writeLines("new", source, useBytes = TRUE)
  writeLines("old", external, useBytes = TRUE)
  if (!isTRUE(suppressWarnings(file.symlink(external, destination)))) {
    skip("symbolic links are unavailable")
  }
  # Remove the file link itself before the recursive cleanup of `root`.
  withr::defer(suppressWarnings(file.remove(destination)))
  calls <- 0L
  atomic_replace <- .atomic_replace
  atomic_environment <- new.env(parent = asNamespace("bigbang"))
  atomic_environment$file.rename <- function(from, to) {
    calls <<- calls + 1L
    if (calls == 1L) FALSE else base::file.rename(from, to)
  }
  environment(atomic_replace) <- atomic_environment
  expect_true(atomic_replace(source, destination))
  expect_identical(readLines(destination, warn = FALSE), "new")
  expect_identical(readLines(external, warn = FALSE), "old")

  expect_match(.r_string_literal("\u007f\u00e1\U0001f600"), "\\\\u007f")
  expect_match(.r_ascii_literal(setNames(1L, "one")), "names")
  expect_match(.r_ascii_literal(list()), "list\\(\\)")
  expect_match(.copyright_holders(
    "c(person(given = 'A', family = 'B'), person(role = 'aut'))"
  ), "A B")
  expect_identical(.copyright_holders("list(1, 2)"),
                   "Authors listed in Authors@R")
  expect_false(is_path_inside(
    file.path(dirname(root), paste0(basename(root), "-other")), root
  ))

  expect_false(safe_unlink(".", verify = TRUE))
  expect_false(safe_unlink("~", verify = TRUE))
  expect_false(safe_unlink("..", verify = TRUE))
  protected <- file.path(root, "R")
  dir.create(protected)
  expect_false(safe_unlink(protected, recursive = TRUE, force = TRUE))
  safe <- safe_unlink
  safe_environment <- new.env(parent = asNamespace("bigbang"))
  safe_environment$unlink <- function(...) 1L
  environment(safe) <- safe_environment
  expect_warning(
    safe(file.path(root, "incomplete"), verify = FALSE),
    "Could not remove completely"
  )

  expect_identical(.resolve_upgrade_policy(FALSE, "newer", FALSE), "newer")
  expect_identical(.resolve_upgrade_policy(TRUE, "newer", TRUE), "always")
  lib <- file.path(root, "library")
  dir.create(lib)
  expect_identical(.with_install_library_path(lib, "ok"), "ok")
  expect_identical(.classify_local_archive("unused.tar.gz", ".tar.gz"), "source")
})
