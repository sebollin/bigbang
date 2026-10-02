round054_make_archive <- function(root, name = "round054component") {
  source <- file.path(root, "source", name)
  archives <- file.path(root, "archives")
  dir.create(file.path(source, "R"), recursive = TRUE)
  dir.create(archives, recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name), "Version: 0.1.0",
    "Title: Round 054 fixture",
    "Description: Fixture for interrupted update tests.", "License: MIT",
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

round054_fixture <- function(prefix = "bigbang-round054-") {
  root <- tempfile(prefix)
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  archive <- round054_make_archive(root)
  initial <- create_metapackage(
    "round054verse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    include_archives = TRUE, workflow = c(Stage = "round054component")
  )
  list(root = root, destination = destination, archive = archive,
       project = initial$path, name = "round054verse")
}

round054_update <- function(fixture, destination = fixture$destination, ...) {
  create_metapackage(
    fixture$name, fixture$archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    include_archives = TRUE, workflow = c(Stage = "round054component"),
    update = TRUE, ...
  )
}

round054_update_dead_owner <- function(fixture, destination = fixture$destination,
                                       ...) {
  testthat::local_mocked_bindings(
    .update_owner_may_be_alive = function(state) FALSE,
    .package = "bigbang"
  )
  round054_update(fixture, destination = destination, ...)
}

round054_collect_child <- function(child, timeout = 1) {
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

round054_cleanup_child <- function(child) {
  if (.bb_child_registered(child)) {
    collected <- suppressWarnings(parallel::mccollect(child, wait = FALSE))
    if (is.null(collected)) {
      try(tools::pskill(child$pid, tools::SIGKILL), silent = TRUE)
      collected <- round054_collect_child(child, timeout = 1)
    }
  }
  bb_finish_child(child)
  invisible(NULL)
}

round054_kill_child <- function(child, mark, timeout = 30) {
  on.exit(round054_cleanup_child(child), add = TRUE)
  deadline <- Sys.time() + timeout
  while (!file.exists(mark) && Sys.time() < deadline) Sys.sleep(0.05)
  hit <- file.exists(mark)
  round054_cleanup_child(child)
  hit
}

round054_snapshot <- function(path) {
  entries <- list.files(path, all.files = TRUE, recursive = TRUE, no.. = TRUE,
                        include.dirs = TRUE)
  full <- file.path(path, entries)
  info <- file.info(full)
  hashes <- rep(NA_character_, length(full))
  hashes[!info$isdir] <- unname(as.character(tools::md5sum(full[!info$isdir])))
  data.frame(path = entries, directory = info$isdir, hash = hashes,
             stringsAsFactors = FALSE)
}

test_that("the process cleanup helper enforces its timeout", {
  skip_on_cran()
  skip_on_os("windows")
  child <- bb_mcparallel(Sys.sleep(10), silent = TRUE,
                         mc.set.seed = FALSE)
  on.exit(round054_cleanup_child(child), add = TRUE)
  started <- Sys.time()
  hit <- round054_kill_child(child, tempfile("round054-missing-mark-"),
                             timeout = 0.05)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  expect_false(hit)
  expect_lt(elapsed, 2)
})

test_that("dry run reconciles sibling journals without mutating any sibling", {
  fixture <- round054_fixture("bigbang-round057-sibling-plan-")
  empty <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.armando-empty")
  )
  dir.create(empty)
  journal <- .create_update_journal(
    fixture$project, fixture$name,
    .read_generation_manifest(fixture$project)
  )
  armed <- paste0(journal$path, ".armando-marked")
  expect_true(file.rename(journal$path, armed))
  journal <- .create_update_journal(
    fixture$project, fixture$name,
    .read_generation_manifest(fixture$project)
  )
  .update_journal_tombstone(journal$path, fixture$name, journal$marker)
  discarded <- paste0(journal$path, ".descartado-marked")
  expect_true(file.rename(journal$path, discarded))
  before <- round054_snapshot(fixture$destination)

  result <- round054_update(fixture, dry_run = TRUE)

  expect_true(result$dry_run)
  expect_identical(round054_snapshot(fixture$destination), before)
  expect_length(result$recovery$reconciliation, 3L)
  expect_true(all(vapply(
    result$recovery$reconciliation,
    function(item) item$action %in% c("discard_empty", "blocked_live_owner"),
    logical(1L)
  )))
})

test_that("sibling matching escapes the name literally", {
  root <- tempfile("bigbang-round057-pattern-")
  dir.create(root)
  project <- file.path(root, "x.y")
  controls <- file.path(
    root,
    c(".x-y.bigbang-update.armando-user",
      ".x_y.bigbang-update.armando-user",
      ".xletters.bigbang-update.armando-user")
  )
  valid <- file.path(root, ".x.y.bigbang-update.armando-user")
  for (path in c(controls, valid)) dir.create(path)
  expect_identical(
    .update_journal_sibling_paths(project, "x.y", "armando"),
    normalizePath(valid, winslash = "/", mustWork = FALSE)
  )
})

test_that("H1 real SIGKILL leaves no final unmarked journal", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round054_fixture("bigbang-round054-h1-marker-")
  mark <- file.path(fixture$root, "H1_MARK")
  Sys.setenv(BB054_H1_MARK = mark)
  on.exit(Sys.unsetenv("BB054_H1_MARK"), add = TRUE)
  child <- bb_mcparallel({
    trace(".journal_backup_copy", where = asNamespace("bigbang"),
          tracer = quote({
            if (!file.exists(Sys.getenv("BB054_H1_MARK"))) {
              writeLines("backup-started", Sys.getenv("BB054_H1_MARK"),
                         useBytes = TRUE)
              Sys.sleep(600)
            }
          }), print = FALSE)
    round054_update(fixture)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(round054_cleanup_child(child), add = TRUE)
  expect_true(round054_kill_child(child, mark))

  armando <- list.files(fixture$destination,
                        pattern = paste0("^\\.", fixture$name,
                                         "\\.bigbang-update\\.armando-"),
                        full.names = TRUE, all.files = TRUE)
  expect_length(armando, 1L)
  expect_true(file.exists(file.path(armando, "marker.rds")))
  expect_false(dir.exists(.update_journal_path(fixture$project)))
  result <- round054_update(fixture)
  expect_true(result$updated)
  expect_false(dir.exists(armando))
})

test_that("H1 empty unmarked staging is discarded and non-empty is set aside", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round054_fixture("bigbang-round054-h1-empty-")
  mark <- file.path(fixture$root, "H1_EMPTY_MARK")
  Sys.setenv(BB054_H1_EMPTY_MARK = mark)
  on.exit(Sys.unsetenv("BB054_H1_EMPTY_MARK"), add = TRUE)
  child <- bb_mcparallel({
    trace(".atomic_save_rds", where = asNamespace("bigbang"),
          tracer = quote({
            if (grepl("marker\\.rds$", path) &&
                  !file.exists(Sys.getenv("BB054_H1_EMPTY_MARK"))) {
              writeLines("staging-created", Sys.getenv("BB054_H1_EMPTY_MARK"),
                         useBytes = TRUE)
              Sys.sleep(600)
            }
          }), print = FALSE)
    round054_update(fixture)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(round054_cleanup_child(child), add = TRUE)
  expect_true(round054_kill_child(child, mark))
  armando <- list.files(fixture$destination,
                        pattern = paste0("^\\.", fixture$name,
                                         "\\.bigbang-update\\.armando-"),
                        full.names = TRUE, all.files = TRUE)
  expect_length(armando, 1L)
  writeLines("user-owned", file.path(armando, "user-owned.txt"), useBytes = TRUE)
  before <- readBin(file.path(armando, "user-owned.txt"), "raw", 1000L)
  result <- round054_update(fixture)
  expect_true(result$updated)
  apart <- list.files(fixture$destination,
                      pattern = paste0("^\\.", fixture$name,
                                       "\\.bigbang-apartado-"),
                      full.names = TRUE, all.files = TRUE)
  expect_length(apart, 1L)
  expect_identical(readBin(file.path(apart, "user-owned.txt"), "raw", 1000L),
                   before)
})

test_that("H2 SIGKILL after rename and during deletion resumes from tombstone", {
  skip_on_cran()
  skip_on_os("windows")
  for (phase in c("after_rename", "during_delete")) {
    fixture <- round054_fixture(paste0("bigbang-round054-h2-", phase, "-"))
    manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
    journal <- .create_update_journal(fixture$project, fixture$name, manifest)
    mark <- file.path(fixture$root, "H2_MARK")
    Sys.setenv(BB054_H2_MARK = mark)
    Sys.setenv(BB054_H2_PHASE = phase)
    on.exit(Sys.unsetenv("BB054_H2_MARK"), add = TRUE)
    child <- bb_mcparallel({
      ns <- asNamespace("bigbang")
      if (identical(Sys.getenv("BB054_H2_PHASE"), "during_delete")) {
        trace(".discard_update_entry", where = ns, exit = quote({
          calls <- getOption("bb054.discard.calls", 0L) + 1L
          options(bb054.discard.calls = calls)
          if (calls == 1L && !file.exists(Sys.getenv("BB054_H2_MARK"))) {
            writeLines("delete-started", Sys.getenv("BB054_H2_MARK"),
                       useBytes = TRUE)
            Sys.sleep(600)
          }
        }), print = FALSE)
      } else {
        trace(".update_journal_after_rename", where = ns, tracer = quote({
          if (!file.exists(Sys.getenv("BB054_H2_MARK"))) {
            writeLines("renamed", Sys.getenv("BB054_H2_MARK"), useBytes = TRUE)
            Sys.sleep(600)
          }
        }), print = FALSE)
      }
      .discard_update_journal(journal, fixture$project, fixture$name)
    }, silent = TRUE, mc.set.seed = FALSE)
    on.exit(round054_cleanup_child(child), add = TRUE)
    expect_true(round054_kill_child(child, mark))
    Sys.unsetenv("BB054_H2_PHASE")
    discarded <- list.files(fixture$destination,
                            pattern = paste0("^\\.", fixture$name,
                                             "\\.bigbang-update\\.descartado-"),
                            full.names = TRUE, all.files = TRUE)
    expect_length(discarded, 1L)
    expect_true(file.exists(file.path(discarded, "tombstone.rds")))
    expect_false(dir.exists(.update_journal_path(fixture$project)))
    result <- round054_update_dead_owner(fixture)
    expect_true(result$updated)
    expect_false(dir.exists(discarded))
  }
})

test_that("H3 moved projects recognize the journal by name and manifest hash", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round054_fixture("bigbang-round054-h3-")
  reference_destination <- file.path(fixture$root, "reference-destination")
  dir.create(reference_destination)
  reference_initial <- create_metapackage(
    fixture$name, fixture$archive, dest_dir = reference_destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), include_archives = TRUE,
    workflow = c(Stage = "round054component")
  )
  reference <- list(
    root = fixture$root, destination = reference_destination,
    archive = fixture$archive, project = reference_initial$path,
    name = fixture$name
  )
  round054_update(reference, version = "0.2.0")
  expected <- round054_snapshot(reference$project)
  mark <- file.path(fixture$root, "H3_MARK")
  Sys.setenv(BB054_H3_MARK = mark)
  on.exit(Sys.unsetenv("BB054_H3_MARK"), add = TRUE)
  child <- bb_mcparallel({
    trace(".write_utf8", where = asNamespace("bigbang"), tracer = quote({
      writes <- getOption("bb054.writes", 0L) + 1L
      options(bb054.writes = writes)
      if (writes == 3L && !file.exists(Sys.getenv("BB054_H3_MARK"))) {
        writeLines("write-started", Sys.getenv("BB054_H3_MARK"), useBytes = TRUE)
        Sys.sleep(600)
      }
    }), print = FALSE)
    round054_update(fixture, version = "0.2.0")
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(round054_cleanup_child(child), add = TRUE)
  expect_true(round054_kill_child(child, mark))
  moved_parent <- file.path(fixture$root, "moved")
  expect_true(file.rename(fixture$destination, moved_parent))
  moved_project <- file.path(moved_parent, fixture$name)
  result <- round054_update(fixture, destination = moved_parent,
                            version = "0.2.0")
  expect_true(result$recovered)
  expect_identical(round054_snapshot(moved_project), expected)
  expect_false(dir.exists(.update_journal_path(moved_project)))
})

test_that("a journal left behind by a move reports where to look", {
  fixture <- round054_fixture("bigbang-round054-h3-left-")
  manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  moved <- file.path(fixture$root, "moved-project")
  expect_true(file.rename(fixture$project, moved))
  error <- expect_error(round054_update(fixture),
                        class = "bigbang_error_moved_update_journal")
  expect_match(conditionMessage(error), "journal|backup|moved|Next step",
               ignore.case = TRUE)
  expect_true(dir.exists(journal$path))
})

test_that("user folders with journal-looking names are set aside byte-for-byte", {
  skip_on_cran()
  fixture <- round054_fixture("bigbang-round054-control-")
  armando <- file.path(
    fixture$destination, paste0(".", fixture$name, ".bigbang-update.armando-user")
  )
  discarded <- file.path(
    fixture$destination, paste0(".", fixture$name, ".bigbang-update.descartado-user")
  )
  dir.create(armando)
  dir.create(discarded)
  writeLines("keep armando", file.path(armando, "owned.txt"), useBytes = TRUE)
  writeLines("keep descartado", file.path(discarded, "owned.txt"), useBytes = TRUE)
  before_armando <- readBin(file.path(armando, "owned.txt"), "raw", 1000L)
  before_discarded <- readBin(file.path(discarded, "owned.txt"), "raw", 1000L)
  result <- round054_update(fixture)
  expect_true(result$updated)
  apart <- list.files(fixture$destination,
                      pattern = paste0("^\\.", fixture$name,
                                       "\\.bigbang-apartado-"),
                      full.names = TRUE, all.files = TRUE)
  expect_length(apart, 2L)
  contents <- lapply(apart, function(path) {
    readBin(file.path(path, "owned.txt"), "raw", 1000L)
  })
  expect_true(any(vapply(contents, identical, logical(1), before_armando)))
  expect_true(any(vapply(contents, identical, logical(1), before_discarded)))
})

test_that("a discarded folder with an invalid tombstone is set aside", {
  skip_on_cran()
  fixture <- round054_fixture("bigbang-round054-bad-tombstone-")
  discarded <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.descartado-invalid")
  )
  dir.create(discarded)
  payload <- file.path(discarded, "payload.bin")
  writeBin(charToRaw("do not delete"), payload)
  writeLines("not an RDS", file.path(discarded, "tombstone.rds"),
             useBytes = TRUE)
  before <- readBin(payload, "raw", 1000L)

  result <- round054_update(fixture)
  expect_true(result$updated)
  apart <- list.files(fixture$destination,
                      pattern = paste0("^\\.", fixture$name,
                                       "\\.bigbang-apartado-"),
                      full.names = TRUE, all.files = TRUE)
  expect_length(apart, 1L)
  expect_identical(readBin(file.path(apart, "payload.bin"), "raw", 1000L),
                   before)
})

test_that("a moved journal rejects a state hash that does not match its backup", {
  fixture <- round054_fixture("bigbang-round054-state-hash-")
  manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  state_path <- file.path(journal$path, "state.rds")
  state <- readRDS(state_path)
  state$old_manifest_hash <- strrep("0", 32L)
  .atomic_save_rds(state, state_path)
  before <- round054_snapshot(fixture$project)

  error <- expect_error(
    .recover_pending_update(fixture$project, fixture$name, handled = TRUE),
    class = "bigbang_error_unrecognized_update_journal"
  )
  expect_match(conditionMessage(error), "armed|backup|state|Next step",
               ignore.case = TRUE)
  expect_identical(round054_snapshot(fixture$project), before)
  expect_true(dir.exists(journal$path))
})

test_that("missing projects and unarmed dry runs explain the next action", {
  missing <- file.path(tempdir(), paste0("bigbang-round054-missing-", Sys.getpid()))
  error <- expect_error(
    .recover_pending_update(missing, "round054verse"),
    class = "bigbang_error_missing_project"
  )
  expect_true(inherits(error, "bigbang_error_missing_manifest"))
  expect_match(conditionMessage(error), "moved|dest_dir|Next step",
               ignore.case = TRUE)

  fixture <- round054_fixture("bigbang-round054-unarmed-dry-run-")
  manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  unlink(file.path(journal$path, "state.rds"))
  dry <- .recover_pending_update(
    fixture$project, fixture$name, dry_run = TRUE
  )
  expect_true(dry$pending)
  expect_identical(dry$action, "discard_unarmed")
  expect_true(dir.exists(journal$path))
})

test_that("an armed journal with an out-of-plan intent is never deleted", {
  fixture <- round054_fixture("bigbang-round054-out-of-plan-")
  manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
  journal <- .create_update_journal(fixture$project, fixture$name, manifest)
  writeLines(
    paste("write", strrep("0", 32L), "not-planned.txt", sep = "\t"),
    file.path(journal$path, "intent.log"), useBytes = TRUE
  )
  error <- expect_error(
    .recover_pending_update(fixture$project, fixture$name, handled = TRUE),
    class = "bigbang_error_unrecognized_update_journal"
  )
  expect_match(conditionMessage(error), "armed|intention|backup|Next step",
               ignore.case = TRUE)
  expect_true(dir.exists(journal$path))
})

test_that("re-export proof ignores common functions and root self-mutation", {
  root <- tempfile("bigbang-round054-proof-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c(
    "common <- function() { df$col <- 2; load(f, envir = e); source(archivo, local = e) }"
  ), file.path(root, "R", "common.R"))
  evidence <- bigbang:::.reexport_source_evidence(root)
  reexporter <- list(
    package = "round054child", exports = "f",
    imports = list(list("round054root", "f")),
    reexport_evidence = evidence
  )
  parent <- list(
    package = "round054root", exports = "f", imports = list(),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  probe <- bigbang:::.reexport_probe(
    reexporter, "f", list(reexporter, parent), character()
  )
  expect_true(probe$demonstrated)
  expect_identical(probe$root, "round054root")

  root_hook <- tempfile("bigbang-round054-root-hook-")
  dir.create(file.path(root_hook, "R"), recursive = TRUE)
  writeLines(c(
    "f <- function() 1", ".onLoad <- function(lib, pkg) assign('other', 1,",
    "  envir = asNamespace(pkg))"
  ), file.path(root_hook, "R", "root.R"))
  root_evidence <- bigbang:::.reexport_source_evidence(root_hook)
  root_component <- list(
    package = "round054root2", exports = "f", imports = list(),
    reexport_evidence = root_evidence
  )
  root_probe <- bigbang:::.reexport_probe(
    root_component, "f", list(root_component), character()
  )
  expect_true(root_probe$demonstrated)
})

test_that("re-export proof follows load-time intra-package calls", {
  root <- tempfile("bigbang-round054-load-scope-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c(
    "helper <- function() assign('f', 1, envir = asNamespace('round054child'))",
    ".onLoad <- function(lib, pkg) helper()"
  ), file.path(root, "R", "load.R"))
  evidence <- bigbang:::.reexport_source_evidence(root)
  child <- list(
    package = "round054child", exports = "f",
    imports = list(list("round054root", "f")),
    reexport_evidence = evidence
  )
  parent <- list(
    package = "round054root", exports = "f", imports = list(),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )

  probe <- bigbang:::.reexport_probe(
    child, "f", list(child, parent), character()
  )
  expect_false(probe$demonstrated)
  expect_match(probe$reason, "assign|round054child", ignore.case = TRUE)
})

test_that("re-export blocker filtering distinguishes the requested symbol", {
  expect_identical(bigbang:::.update_journal_kind_label("discarded"),
                   "discarded")
  evidence <- list(
    mutations = list(
      list(name = "$", symbol = "col", active = TRUE),
      list(name = "$", symbol = "f", active = TRUE),
      list(name = "assign", symbol = NULL, active = TRUE,
           file = "R/load.R", line = 2L)
    ),
    calls = list(
      list(name = "assign", literal = TRUE, value = "other",
           file = "R/load.R", line = 2L),
      list(name = "load", literal = TRUE, value = "other",
           file = "R/common.R", line = 1L)
    ),
    dynamic = list(list(name = "eval", active = FALSE)),
    indirect = list(list(name = "other::f", active = FALSE))
  )
  blockers <- bigbang:::.reexport_relevant_blockers(evidence, "f")
  expect_true(length(blockers) >= 1L)
  expect_true(any(vapply(blockers, function(item) {
    identical(item$name, "$") && identical(item$symbol, "f")
  }, logical(1L))))
  expect_false(any(vapply(blockers, function(item) {
    identical(item$name, "$") && identical(item$symbol, "col")
  }, logical(1L))))

  unrelated <- bigbang:::.reexport_relevant_blockers(
    list(mutations = list(list(name = "custom", active = TRUE))), "f"
  )
  expect_identical(unrelated[[1L]]$name, "custom")

  non_binder <- bigbang:::.reexport_relevant_blockers(
    list(calls = list(list(name = "custom", active = TRUE))), "f"
  )
  expect_length(non_binder, 0L)

  sysdata_evidence <- bigbang:::.reexport_empty_evidence()
  sysdata_evidence$sysdata_error <- "corrupt sysdata"
  sysdata_component <- list(
    package = "round054child", exports = "f",
    imports = list(list("round054root", "f")),
    reexport_evidence = sysdata_evidence
  )
  sysdata_parent <- list(
    package = "round054root", exports = "f", imports = list(),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  sysdata_probe <- bigbang:::.reexport_probe(
    sysdata_component, "f", list(sysdata_component, sysdata_parent), character()
  )
  expect_false(sysdata_probe$demonstrated)
  expect_match(sysdata_probe$reason, "sysdata", ignore.case = TRUE)
})

test_that("two generations from one archive are byte-for-byte deterministic", {
  skip_on_cran()
  fixture <- round054_fixture("bigbang-round054-determinism-")
  second_destination <- file.path(fixture$root, "second-destination")
  dir.create(second_destination)
  second_initial <- create_metapackage(
    fixture$name, fixture$archive, dest_dir = second_destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), include_archives = TRUE,
    workflow = c(Stage = "round054component")
  )
  round054_update(fixture, version = "0.2.0")
  second <- list(
    root = fixture$root, destination = second_destination,
    archive = fixture$archive, project = second_initial$path,
    name = fixture$name
  )
  round054_update(second, version = "0.2.0")
  expect_identical(round054_snapshot(fixture$project),
                   round054_snapshot(second$project))
})
