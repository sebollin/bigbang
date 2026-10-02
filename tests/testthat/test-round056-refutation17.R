round056_make_archive <- function(root, name = "round056component") {
  source <- file.path(root, "source", name)
  archives <- file.path(root, "archives")
  dir.create(file.path(source, "R"), recursive = TRUE)
  dir.create(archives, recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name), "Version: 0.1.0",
    "Title: Round 056 fixture",
    "Description: Fixture for update journal tests.", "License: MIT",
    "Author: Test Author", "Maintainer: Test Author <test@example.org>"
  ), file.path(source, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(value)", file.path(source, "NAMESPACE"), useBytes = TRUE)
  writeLines("value <- function() 1L", file.path(source, "R", "value.R"),
             useBytes = TRUE)
  archive <- file.path(archives, paste0(name, "_0.1.0.tar.gz"))
  withr::with_dir(dirname(source), utils::tar(
    archive, basename(source), compression = "gzip"
  ))
  archive
}

round056_fixture <- function(prefix = "bigbang-round056-",
                             name = "round056verse", version = "0.1.0") {
  root <- tempfile(prefix)
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  archive <- round056_make_archive(root)
  initial <- create_metapackage(
    name, archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    include_archives = TRUE, version = version,
    workflow = c(Stage = "round056component")
  )
  sentinel <- file.path(initial$path, "user-owned.bin")
  writeBin(charToRaw("user-owned bytes\n"), sentinel)
  list(root = root, destination = destination, archive = archive,
       project = initial$path, name = name, sentinel = sentinel)
}

round056_update <- function(fixture, ...) {
  create_metapackage(
    fixture$name, fixture$archive, dest_dir = fixture$destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), include_archives = TRUE,
    workflow = c(Stage = "round056component"), update = TRUE, ...
  )
}

round056_update_dead_owner <- function(fixture, ...) {
  testthat::local_mocked_bindings(
    .update_owner_may_be_alive = function(state) FALSE,
    .package = "bigbang"
  )
  round056_update(fixture, ...)
}

round056_snapshot <- function(path) {
  entries <- list.files(path, all.files = TRUE, recursive = TRUE, no.. = TRUE,
                        include.dirs = TRUE)
  full <- file.path(path, entries)
  info <- file.info(full)
  hashes <- rep(NA_character_, length(full))
  hashes[!info$isdir] <- unname(as.character(tools::md5sum(full[!info$isdir])))
  data.frame(path = entries, directory = info$isdir, hash = hashes,
             stringsAsFactors = FALSE)
}

round056_collect_child <- function(child, timeout = 1) {
  deadline <- Sys.time() + timeout
  repeat {
    collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
    if (!is.null(collected)) {
      bb_finish_child(child)
      return(invisible(TRUE))
    }
    if (Sys.time() >= deadline) return(invisible(FALSE))
    Sys.sleep(0.01)
  }
}

round056_cleanup_child <- function(child) {
  if (.bb_child_registered(child)) {
    collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
    if (is.null(collected)) {
      try(tools::pskill(child$pid, tools::SIGKILL), silent = TRUE)
      collected <- round056_collect_child(child, timeout = 1)
    }
  }
  bb_finish_child(child)
  invisible(NULL)
}

round056_kill <- function(child, mark) {
  on.exit(round056_cleanup_child(child), add = TRUE)
  deadline <- Sys.time() + 30
  while (!file.exists(mark) && Sys.time() < deadline) Sys.sleep(0.05)
  hit <- file.exists(mark)
  round056_cleanup_child(child)
  hit
}

round056_armando <- function(fixture) {
  list.files(
    fixture$destination,
    pattern = paste0("^\\.", fixture$name, "\\.bigbang-update\\.armando-"),
    full.names = TRUE, all.files = TRUE, no.. = TRUE
  )
}

round056_discarded <- function(fixture) {
  list.files(
    fixture$destination,
    pattern = paste0("^\\.", fixture$name, "\\.bigbang-update\\.descartado-"),
    full.names = TRUE, all.files = TRUE, no.. = TRUE
  )
}

round056_interrupt_discard <- function(fixture, mark) {
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  Sys.setenv(BB056_MARK = mark)
  child <- bb_mcparallel({
    trace(".discard_update_entry", where = asNamespace("bigbang"),
          tracer = quote({
            if (!file.exists(Sys.getenv("BB056_MARK"))) {
              writeLines("delete-started", Sys.getenv("BB056_MARK"),
                         useBytes = TRUE)
              Sys.sleep(600)
            }
          }), print = FALSE)
    bigbang:::.discard_update_journal(journal, fixture$project, fixture$name)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(round056_cleanup_child(child), add = TRUE)
  hit <- round056_kill(child, mark)
  Sys.unsetenv("BB056_MARK")
  expect_true(hit)
  paths <- round056_discarded(fixture)
  expect_length(paths, 1L)
  paths[[1L]]
}

test_that("J1 SIGKILL sets aside an unverified marker temporary", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round056_fixture("bigbang-round056-j1-")
  before <- round056_snapshot(fixture$project)
  mark <- file.path(fixture$root, "J1_MARK")
  Sys.setenv(BB056_MARK = mark)
  child <- bb_mcparallel({
    trace(".atomic_replace", where = asNamespace("bigbang"),
          tracer = quote({
            if (grepl("marker\\.rds$", destination) &&
                  !file.exists(Sys.getenv("BB056_MARK"))) {
              writeLines("marker-temporary", Sys.getenv("BB056_MARK"),
                         useBytes = TRUE)
              Sys.sleep(600)
            }
          }), print = FALSE)
    round056_update(fixture)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(round056_cleanup_child(child), add = TRUE)
  expect_true(round056_kill(child, mark))
  Sys.unsetenv("BB056_MARK")
  armando <- round056_armando(fixture)
  expect_length(armando, 1L)
  entries <- list.files(armando, all.files = TRUE, no.. = TRUE,
                        include.dirs = TRUE)
  expect_true(all(grepl("^\\.marker\\.rds-[[:alnum:]]+$", entries)))
  expect_identical(round056_snapshot(fixture$project), before)
  result <- round056_update(fixture)
  expect_true(result$updated)
  expect_false(dir.exists(armando))
  apart <- list.files(
    fixture$destination,
    pattern = paste0("^\\.", fixture$name, "\\.bigbang-apartado-"),
    full.names = TRUE, all.files = TRUE, no.. = TRUE
  )
  expect_length(apart, 1L)
  expect_true(file.exists(file.path(apart, entries[[1L]])))
  expect_identical(readBin(fixture$sentinel, "raw", 1000L),
                   charToRaw("user-owned bytes\n"))
})

test_that("J2 SIGKILL after the tombstone resumes the rename and discard", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round056_fixture("bigbang-round056-j2-")
  before <- round056_snapshot(fixture$project)
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  mark <- file.path(fixture$root, "J2_MARK")
  Sys.setenv(BB056_MARK = mark)
  child <- bb_mcparallel({
    trace(".update_journal_tombstone", where = asNamespace("bigbang"),
          exit = quote({
            if (!file.exists(Sys.getenv("BB056_MARK"))) {
              writeLines("tombstone-written", Sys.getenv("BB056_MARK"),
                         useBytes = TRUE)
              Sys.sleep(600)
            }
          }), print = FALSE)
    bigbang:::.discard_update_journal(journal, fixture$project, fixture$name)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(round056_cleanup_child(child), add = TRUE)
  expect_true(round056_kill(child, mark))
  Sys.unsetenv("BB056_MARK")
  expect_true(file.exists(file.path(
    .update_journal_path(fixture$project), "tombstone.rds"
  )))
  expect_identical(round056_snapshot(fixture$project), before)
  result <- round056_update(fixture)
  expect_true(result$updated)
  expect_false(dir.exists(.update_journal_path(fixture$project)))
  expect_length(round056_discarded(fixture), 0L)
  expect_identical(readBin(fixture$sentinel, "raw", 1000L),
                   charToRaw("user-owned bytes\n"))
})

test_that("J3 SIGKILL after the tombstone unlink removes the empty shell", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round056_fixture("bigbang-round056-j3-")
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  mark <- file.path(fixture$root, "J3_MARK")
  Sys.setenv(BB056_MARK = mark)
  child <- bb_mcparallel({
    trace(".remove_journal_tombstones", where = asNamespace("bigbang"),
          exit = quote({
            if (!file.exists(.update_journal_tombstone_path(path)) &&
                  !file.exists(Sys.getenv("BB056_MARK"))) {
              writeLines("tombstone-unlinked", Sys.getenv("BB056_MARK"),
                         useBytes = TRUE)
              Sys.sleep(600)
            }
          }), print = FALSE)
    bigbang:::.discard_update_journal(journal, fixture$project, fixture$name)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(round056_cleanup_child(child), add = TRUE)
  expect_true(round056_kill(child, mark))
  Sys.unsetenv("BB056_MARK")
  discarded <- round056_discarded(fixture)
  expect_length(discarded, 1L)
  expect_length(list.files(discarded[[1L]], all.files = TRUE, no.. = TRUE), 0L)
  result <- round056_update(fixture)
  expect_true(result$updated)
  expect_length(round056_discarded(fixture), 0L)
  expect_identical(readBin(fixture$sentinel, "raw", 1000L),
                   charToRaw("user-owned bytes\n"))
})

test_that("J4 preserves user bytes outside the tombstone inventory", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round056_fixture("bigbang-round056-j4-")
  discarded <- round056_interrupt_discard(
    fixture, file.path(fixture$root, "J4_MARK")
  )
  user_file <- file.path(discarded, "user-owned.txt")
  writeLines("do not delete", user_file, useBytes = TRUE)
  before <- readBin(user_file, "raw", 1000L)
  result <- round056_update_dead_owner(fixture)
  expect_true(result$updated)
  apart <- list.files(
    fixture$destination,
    pattern = paste0("^\\.", fixture$name, "\\.bigbang-apartado-"),
    full.names = TRUE, all.files = TRUE, no.. = TRUE
  )
  expect_length(apart, 1L)
  apart_file <- file.path(apart, "user-owned.txt")
  expect_true(file.exists(apart_file))
  expect_identical(readBin(apart_file, "raw", 1000L), before)
  expect_identical(readBin(fixture$sentinel, "raw", 1000L),
                   charToRaw("user-owned bytes\n"))
})

test_that("J5 preserves a discarded journal from another project generation", {
  skip_on_cran()
  skip_on_os("windows")
  first <- round056_fixture("bigbang-round056-j5-first-")
  second <- round056_fixture(
    "bigbang-round056-j5-second-", version = "0.1.1"
  )
  discarded <- round056_interrupt_discard(
    second, file.path(second$root, "J5_MARK")
  )
  target <- file.path(first$destination, basename(discarded))
  expect_true(file.rename(discarded, target))
  tombstone_path <- file.path(target, "tombstone.rds")
  tombstone <- readRDS(tombstone_path)
  tombstone$name <- "round056foreign"
  saveRDS(tombstone, tombstone_path)
  before <- readBin(first$sentinel, "raw", 1000L)
  result <- round056_update_dead_owner(first)
  expect_true(result$updated)
  expect_false(dir.exists(target))
  apart <- list.files(
    first$destination,
    pattern = paste0("^\\.", first$name, "\\.bigbang-apartado-"),
    full.names = TRUE, all.files = TRUE, no.. = TRUE
  )
  expect_length(apart, 1L)
  expect_identical(readBin(first$sentinel, "raw", 1000L), before)
  expect_true(dir.exists(second$project))
})

test_that("J6 recognizes a renamed project and reports an unrecognized owner", {
  skip_on_cran()
  fixture <- round056_fixture("bigbang-round056-j6-good-")
  manifest <- .read_generation_manifest(fixture$project)
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  renamed <- "round056verse2"
  new_project <- file.path(fixture$destination, renamed)
  new_journal <- file.path(
    fixture$destination, paste0(".", renamed, ".bigbang-update")
  )
  expect_true(file.rename(fixture$project, new_project))
  expect_true(file.rename(journal$path, new_journal))
  moved <- fixture
  moved$name <- renamed
  moved$project <- new_project
  moved$sentinel <- file.path(new_project, "user-owned.bin")
  result <- .recover_pending_update(
    moved$project, moved$name, recover = TRUE
  )
  expect_true(result$recovered)
  expect_identical(readBin(moved$sentinel, "raw", 1000L),
                   charToRaw("user-owned bytes\n"))

  bad <- round056_fixture("bigbang-round056-j6-bad-")
  bad_manifest <- .read_generation_manifest(bad$project)
  bad_journal <- .create_update_journal(bad$project, bad$name, bad_manifest)
  bad_new_name <- "round056verse3"
  bad_project <- file.path(bad$destination, bad_new_name)
  bad_path <- file.path(
    bad$destination, paste0(".", bad_new_name, ".bigbang-update")
  )
  expect_true(file.rename(bad$project, bad_project))
  expect_true(file.rename(bad_journal$path, bad_path))
  marker_path <- file.path(bad_path, "marker.rds")
  bad_marker <- readRDS(marker_path)
  bad_marker$name <- "round056owner"
  bad_marker$backup_hashes[[.generation_manifest_name]] <- strrep("0", 32L)
  saveRDS(bad_marker, marker_path)
  bad_moved <- bad
  bad_moved$name <- bad_new_name
  bad_moved$project <- bad_project
  error <- expect_error(
    .recover_pending_update(bad_moved$project, bad_moved$name, recover = TRUE),
    class = "bigbang_error_unrecognized_update_journal"
  )
  expect_match(conditionMessage(error), "round056owner|rename",
               ignore.case = TRUE)
  expect_true(dir.exists(bad_path))
})

round056_scan_fixture <- function(body) {
  root <- tempfile("bigbang-round056-scan-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(body, file.path(root, "R", "load.R"), useBytes = TRUE)
  evidence <- bigbang:::.reexport_source_evidence(root)
  child <- list(
    package = "round056child", exports = "f",
    imports = list(list("round056root", "f")),
    reexport_evidence = evidence
  )
  parent <- list(
    package = "round056root", exports = "f", imports = list(),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  list(
    evidence = evidence,
    child = child,
    parent = parent,
    probe = bigbang:::.reexport_probe(
      child, "f", list(child, parent), character()
    )
  )
}

test_that("G1 follows active string indirection and G2 records binding locks", {
  indirect <- c(
    do_call = ".onLoad <- function(lib, pkg) do.call('helper', list())",
    get = ".onLoad <- function(lib, pkg) get('helper')()",
    match_fun = ".onLoad <- function(lib, pkg) match.fun('helper')()",
    get_namespace = ".onLoad <- function(lib, pkg) getFromNamespace('helper', pkg)()",
    recall = ".onLoad <- function(lib, pkg) Recall('helper')"
  )
  for (call in indirect) {
    fixture <- round056_scan_fixture(c(
      "helper <- function() assign('f', 1, envir = asNamespace('round056child'))",
      call
    ))
    blockers <- bigbang:::.reexport_relevant_blockers(
      fixture$evidence, "f"
    )
    expect_true(length(blockers) > 0L, info = call)
    expect_false(fixture$probe$demonstrated, info = call)
  }
  nonliteral <- round056_scan_fixture(c(
    "helper <- function() assign('f', 1, envir = asNamespace('round056child'))",
    ".onLoad <- function(lib, pkg) do.call(target, list())"
  ))
  expect_true(length(bigbang:::.reexport_relevant_blockers(
    nonliteral$evidence, "f"
  )) > 0L)

  locked <- round056_scan_fixture(
    ".onLoad <- function(lib, pkg) unlockBinding('f', asNamespace(pkg))"
  )
  expect_true(any(vapply(locked$evidence$calls, function(item) {
    identical(item$name, "unlockBinding")
  }, logical(1L))))
  expect_true(length(bigbang:::.reexport_relevant_blockers(
    locked$evidence, "f"
  )) > 0L)
})

test_that("G3 ignores binder names used only as data", {
  fixture <- round056_scan_fixture("labels <- c('assign', 'get')")
  expect_length(bigbang:::.reexport_relevant_blockers(
    fixture$evidence, "f"
  ), 0L)
  expect_true(fixture$probe$demonstrated)
  expect_identical(fixture$probe$root, "round056root")
})

round056_static_text <- function(node) {
  if (is.character(node) && length(node) == 1L && !is.na(node)) {
    return(node)
  }
  if (!is.call(node) || !identical(as.character(node[[1L]]), "paste0")) {
    return(NULL)
  }
  parts <- lapply(as.list(node)[-1L], round056_static_text)
  if (any(vapply(parts, is.null, logical(1L)))) return(NULL)
  paste0(unlist(parts, use.names = FALSE), collapse = "")
}

round056_translation_calls <- function(node) {
  if (!is.language(node)) return(character())
  found <- character()
  if (is.call(node) &&
        (identical(as.character(node[[1L]]), ".bb_tr") ||
           identical(as.character(node[[1L]]), ".bb_trf"))) {
    message <- if (length(node) >= 2L) round056_static_text(node[[2L]]) else NULL
    if (!is.null(message)) found <- c(found, message)
  }
  if (is.call(node) && length(node) > 1L) {
    found <- c(found, unlist(lapply(as.list(node)[-1L],
                                    round056_translation_calls),
                             use.names = FALSE))
  } else if (is.expression(node)) {
    found <- c(found, unlist(lapply(node, round056_translation_calls),
                             use.names = FALSE))
  }
  found
}

test_that("every package translation call has a Spanish catalog entry", {
  source_root <- testthat::test_path("..", "..", "R")
  files <- list.files(source_root, pattern = "\\.(R|r|S|s|q)$",
                      full.names = TRUE, recursive = TRUE)
  messages <- unlist(lapply(files, function(path) {
    parsed <- parse(file = path, keep.source = TRUE)
    round056_translation_calls(parsed)
  }), use.names = FALSE)
  catalog <- names(bigbang:::.bigbang_spanish_catalog())
  expect_length(setdiff(unique(messages), catalog), 0L)
})
