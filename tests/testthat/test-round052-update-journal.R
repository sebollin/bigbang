round052_make_archive <- function(root, name = "journalcomp") {
  source <- file.path(root, "source", name)
  archive_dir <- file.path(root, "archives")
  dir.create(file.path(source, "R"), recursive = TRUE)
  dir.create(archive_dir, recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name), "Version: 0.1.0", "Title: Journal fixture",
    "Description: A package used to test durable update recovery.",
    "License: MIT", "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(source, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(value)", file.path(source, "NAMESPACE"), useBytes = TRUE)
  writeLines("value <- function() 1L", file.path(source, "R", "value.R"),
             useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_0.1.0.tar.gz"))
  withr::with_dir(dirname(source), utils::tar(
    archive, basename(source), compression = "gzip"
  ))
  archive
}

round052_fixture <- function(prefix = "bigbang-round052-", document = FALSE,
                             workflow = TRUE, include_archives = TRUE) {
  root <- tempfile(prefix)
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  archive <- round052_make_archive(root)
  initial <- create_metapackage(
    "journalverse", archive, dest_dir = destination,
    document = document, verbose = FALSE, import_deps = character(),
    force_deps = character(), include_archives = include_archives,
    workflow = if (workflow) c(Stage = "journalcomp") else NULL
  )
  list(root = root, destination = destination, archive = archive,
       project = initial$path)
}

round052_snapshot <- function(path) {
  relative <- list.files(path, all.files = TRUE, recursive = TRUE, no.. = TRUE,
                         include.dirs = TRUE)
  full <- file.path(path, relative)
  info <- file.info(full)
  hashes <- rep(NA_character_, length(full))
  hashes[!info$isdir] <- unname(as.character(tools::md5sum(full[!info$isdir])))
  data.frame(path = relative, directory = info$isdir, hash = hashes,
             stringsAsFactors = FALSE)
}

round052_arm <- function(fixture, extra_files = character()) {
  manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
  journal <- .create_update_journal(
    fixture$project, "journalverse", manifest, extra_files = extra_files
  )
  state_path <- file.path(journal$path, "state.rds")
  state <- readRDS(state_path)
  state$pid <- 99999999L
  state$process_start <- NA_character_
  .atomic_save_rds(state, state_path)
  marker_path <- file.path(journal$path, "marker.rds")
  marker <- readRDS(marker_path)
  marker[c("pid", "host", "started_utc", "process_start")] <-
    state[c("pid", "host", "started_utc", "process_start")]
  .atomic_save_rds(marker, marker_path)
  journal
}

round052_recover_dead_owner <- function(...) {
  testthat::local_mocked_bindings(
    .update_owner_may_be_alive = function(state) FALSE,
    .package = "bigbang"
  )
  .recover_pending_update(...)
}

round052_intended_write <- function(fixture, journal, relative, text) {
  source <- tempfile("round052-intended-")
  writeLines(text, source, useBytes = TRUE)
  .activate_update_journal(journal, fixture$project, "journalverse")
  on.exit(.deactivate_update_journal(), add = TRUE)
  .record_update_write(source, file.path(fixture$project, relative))
  invisible(source)
}

round052_update <- function(fixture, ..., document = FALSE,
                            include_archives = FALSE) {
  create_metapackage(
    "journalverse", fixture$archive, dest_dir = fixture$destination,
    document = document, verbose = FALSE, import_deps = character(),
    force_deps = character(), include_archives = include_archives,
    update = TRUE, ...
  )
}

round052_update_dead_owner <- function(fixture, ...) {
  testthat::local_mocked_bindings(
    .update_owner_may_be_alive = function(state) FALSE,
    .package = "bigbang"
  )
  round052_update(fixture, ...)
}

test_that("unknown user edits are refused and preserved with forced recovery", {
  skip_on_cran()
  fixture <- round052_fixture()
  relative <- "README.md"
  journal <- round052_arm(fixture)
  intended <- round052_intended_write(fixture, journal, relative,
                                      "content intended by update")
  .atomic_copy(intended, file.path(fixture$project, relative))
  user_bytes <- charToRaw("user edit after process death\n")
  writeBin(user_bytes, file.path(fixture$project, relative))

  error <- expect_error(
    round052_update_dead_owner(fixture),
    class = "bigbang_error_interrupted_update"
  )
  expect_true(relative %in% error$files)
  expect_identical(readBin(file.path(fixture$project, relative), "raw", 1000L),
                   user_bytes)

  result <- NULL
  expect_message(
    result <- round052_update_dead_owner(fixture, recover = TRUE),
    "Preserved unknown files"
  )
  expect_true(result$recovered)
  preserved <- file.path(result$recovery$preserved, relative)
  expect_true(file.exists(preserved))
  expect_identical(readBin(preserved, "raw", 1000L), user_bytes)
})

test_that("an absent file with intent but no matching staging bytes is unknown", {
  fixture <- round052_fixture("bigbang-round053-absent-user-delete-")
  relative <- "README.md"
  journal <- round052_arm(fixture)
  intended <- round052_intended_write(fixture, journal, relative,
                                      "content intended by update")
  unlink(file.path(fixture$project, relative))
  unlink(list.files(file.path(journal$path, "staging"), all.files = TRUE,
                    full.names = TRUE, no.. = TRUE), recursive = TRUE)

  plan <- round052_recover_dead_owner(
    fixture$project, "journalverse", recover = TRUE, dry_run = TRUE
  )
  expect_identical(plan$action, "preserve_and_recover")
  expect_identical(plan$unknown, relative)

  expect_message(
    recovered <- round052_recover_dead_owner(
      fixture$project, "journalverse", recover = TRUE
    ),
    "absent when recovery started"
  )
  expect_true(recovered$recovered)
  expect_identical(recovered$restored_absent, relative)
  expect_true(file.exists(file.path(fixture$project, relative)))
  unlink(intended)
})

test_that("a user file appearing at an intended new path is never guessed away", {
  fixture <- round052_fixture()
  relative <- .planned_documentation_files("journalverse")[[1L]]
  journal <- round052_arm(fixture, extra_files = relative)
  intended <- round052_intended_write(fixture, journal, relative,
                                      "intended generated documentation")
  dir.create(dirname(file.path(fixture$project, relative)), recursive = TRUE,
             showWarnings = FALSE)
  user_bytes <- charToRaw("user-created documentation\n")
  writeBin(user_bytes, file.path(fixture$project, relative))

  expect_error(
    round052_update_dead_owner(fixture, document = TRUE),
    class = "bigbang_error_interrupted_update"
  )
  expect_identical(readBin(file.path(fixture$project, relative), "raw", 1000L),
                   user_bytes)

  unlink(file.path(fixture$project, relative))
  .atomic_copy(intended, file.path(fixture$project, relative))
  expect_message(
    round052_recover_dead_owner(fixture$project, "journalverse"),
    "Recovered an interrupted update"
  )
  expect_false(file.exists(file.path(fixture$project, relative)))
})

test_that("unrecognized journal names are actionable and never touched", {
  skip_on_cran()
  for (kind in c("file", "directory")) {
    fixture <- round052_fixture(paste0("bigbang-round052-h8-", kind, "-"))
    journal <- .update_journal_path(fixture$project)
    if (identical(kind, "file")) {
      writeLines("user bytes", journal, useBytes = TRUE)
      before <- readBin(journal, "raw", file.info(journal)$size)
    } else {
      dir.create(journal)
      writeLines("user bytes", file.path(journal, "owned.txt"), useBytes = TRUE)
      before <- readBin(file.path(journal, "owned.txt"), "raw", 1000L)
    }
    expect_error(
      round052_update_dead_owner(fixture),
      class = "bigbang_error_unrecognized_update_journal"
    )
    target <- if (identical(kind, "file")) journal else file.path(journal, "owned.txt")
    expect_identical(readBin(target, "raw", 1000L), before)
  }

  fixture <- round052_fixture("bigbang-round052-h8-other-marker-")
  journal <- .update_journal_path(fixture$project)
  dir.create(journal)
  marker <- list(
    format = .update_journal_format, version = .update_journal_version,
    name = "anotherverse", project = fixture$project,
    backup_hashes = character(), planned_files = character()
  )
  saveRDS(marker, file.path(journal, "marker.rds"))
  before <- .file_digest(file.path(journal, "marker.rds"))
  expect_error(
    round052_update_dead_owner(fixture),
    class = "bigbang_error_unrecognized_update_journal"
  )
  expect_identical(.file_digest(file.path(journal, "marker.rds")), before)

  empty <- round052_fixture("bigbang-round052-h8-empty-")
  empty_journal <- .update_journal_path(empty$project)
  dir.create(empty_journal)
  expect_error(
    .discard_update_journal(empty_journal, empty$project, "journalverse"),
    class = "bigbang_error_unrecognized_update_journal"
  )
  expect_true(dir.exists(empty_journal))
})

test_that("recognized journals reject unexpected user entries", {
  fixture <- round052_fixture()
  journal <- round052_arm(fixture)
  user_file <- file.path(journal$path, "user-owned.txt")
  writeLines("do not delete", user_file, useBytes = TRUE)
  before <- .file_digest(user_file)
  expect_error(
    round052_recover_dead_owner(fixture$project, "journalverse", recover = TRUE),
    class = "bigbang_error_unrecognized_update_journal"
  )
  expect_identical(.file_digest(user_file), before)
  expect_true(dir.exists(journal$path))
  expect_error(
    .discard_update_journal(journal, fixture$project, "journalverse"),
    class = "bigbang_error_unrecognized_update_journal"
  )
})

test_that("journal defensive formats and partial arm states are explicit", {
  expect_invisible(.discard_update_journal(NULL, "unused", "unused"))
  expect_null(.project_relative_destination("/outside", "/project"))
  expect_invisible(.record_update_write("missing", "/outside"))
  expect_invisible(.record_update_delete("/outside"))
  source <- tempfile("round052-source-")
  blocked_parent <- tempfile("round052-parent-file-")
  writeLines("source", source, useBytes = TRUE)
  writeLines("parent", blocked_parent, useBytes = TRUE)
  expect_error(
    .journal_backup_copy(source, file.path(blocked_parent, "child")),
    "Could not create temporary directory"
  )

  fixture <- round052_fixture()
  manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
  expect_error(
    .create_update_journal(
      fixture$project, "journalverse", list(files = "missing")
    ),
    "Could not back up generated file"
  )
  journal <- round052_arm(fixture)
  .activate_update_journal(journal, fixture$project, "journalverse")
  expect_error(
    .record_update_write(
      "missing", file.path(fixture$project, "README.md")
    ),
    "update write has no digest"
  )
  .deactivate_update_journal()
  expect_error(
    .create_update_journal(fixture$project, "journalverse", manifest),
    "already present"
  )
  writeLines(character(), file.path(journal$path, "intent.log"))
  expect_equal(nrow(.read_update_intents(journal$path)), 0L)
  writeLines("invalid", file.path(journal$path, "intent.log"), useBytes = TRUE)
  expect_error(
    .read_update_intents(journal$path),
    class = "bigbang_error_unrecognized_update_journal"
  )

  partial <- round052_fixture("bigbang-round052-partial-arm-")
  partial_manifest <- readRDS(file.path(
    partial$project, .generation_manifest_name
  ))
  hashes <- vapply(file.path(
    partial$project,
    c(partial_manifest$files, .generation_manifest_name)
  ), .file_digest, character(1L))
  names(hashes) <- c(partial_manifest$files, .generation_manifest_name)
  partial_path <- .update_journal_path(partial$project)
  dir.create(partial_path)
  marker <- list(
    format = .update_journal_format, version = .update_journal_version,
    name = "journalverse", project = partial$project,
    backup_hashes = hashes, planned_files = names(hashes)
  )
  saveRDS(marker, file.path(partial_path, "marker.rds"))
  dir.create(file.path(partial_path, "backup"))
  writeLines("partial", file.path(partial_path, "backup", ".DESCRIPTION-abc"),
             useBytes = TRUE)
  writeLines("partial", file.path(partial_path, ".state.rds-abc"),
             useBytes = TRUE)
  expect_true(.unarmed_journal_is_expected(partial_path, marker))
  expect_message(
    dry <- round052_recover_dead_owner(
      partial$project, "journalverse", dry_run = TRUE
    ),
    NA
  )
  expect_identical(dry$action, "discard_unarmed")
  expect_true(dir.exists(partial_path))
  expect_message(
    round052_recover_dead_owner(partial$project, "journalverse"),
    "Discarded an unarmed update journal"
  )
  expect_false(dir.exists(partial_path))
})

test_that("journal checksum, state, host, and manifest guards reject ambiguity", {
  skip_on_cran()
  fixture <- round052_fixture()
  manifest_path <- file.path(fixture$project, .generation_manifest_name)
  manifest <- readRDS(manifest_path)
  .atomic_save_rds(
    list(schema = 2L, files = "README.md", hashes = character()),
    manifest_path
  )
  expect_false(.manifest_matches_project(fixture$project))
  .atomic_save_rds(manifest, manifest_path)

  corrupt <- round052_fixture("bigbang-round052-corrupt-backup-")
  corrupt_manifest <- readRDS(file.path(
    corrupt$project, .generation_manifest_name
  ))
  original_copy <- .journal_backup_copy
  copies <- 0L
  local({
    testthat::local_mocked_bindings(
      .journal_backup_copy = function(source, destination) {
        original_copy(source, destination)
        copies <<- copies + 1L
        if (copies == 1L) writeLines("corrupt", destination, useBytes = TRUE)
      },
      .package = "bigbang"
    )
    expect_error(
      .create_update_journal(
        corrupt$project, "journalverse", corrupt_manifest
      ),
      "Could not back up generated file"
    )
  })

  invalid <- round052_fixture("bigbang-round052-invalid-state-")
  invalid_journal <- round052_arm(invalid)
  invalid_state_path <- file.path(invalid_journal$path, "state.rds")
  invalid_state <- readRDS(invalid_state_path)
  expect_true(.update_owner_may_be_alive(
    utils::modifyList(invalid_state, list(host = NA_character_))
  ))
  expect_true(.update_owner_may_be_alive(
    utils::modifyList(invalid_state, list(host = "another-host"))
  ))
  invalid_state$pid <- NA_integer_
  .atomic_save_rds(invalid_state, invalid_state_path)
  expect_error(
    round052_recover_dead_owner(invalid$project, "journalverse", recover = TRUE),
    class = "bigbang_error_unrecognized_update_journal"
  )
})

test_that("forced recovery handles an unknown directory and missing parents", {
  fixture <- round052_fixture()
  relative <- .planned_documentation_files("journalverse")[[1L]]
  journal <- round052_arm(fixture, extra_files = relative)
  round052_intended_write(fixture, journal, relative, "planned")
  dir.create(file.path(fixture$project, relative), recursive = TRUE)
  writeLines("user child", file.path(fixture$project, relative, "child"),
             useBytes = TRUE)

  blocked <- round052_recover_dead_owner(
    fixture$project, "journalverse", dry_run = TRUE
  )
  expect_identical(blocked$action, "blocked_unknown")
  forced_plan <- round052_recover_dead_owner(
    fixture$project, "journalverse", recover = TRUE, dry_run = TRUE
  )
  expect_identical(forced_plan$action, "preserve_and_recover")
  expect_message(
    recovered <- round052_recover_dead_owner(
      fixture$project, "journalverse", recover = TRUE
    ),
    "Preserved unknown files"
  )
  expect_true(file.exists(file.path(recovered$preserved, relative, "child")))
  expect_false(dir.exists(file.path(fixture$project, relative)))

  missing_parent <- round052_fixture("bigbang-round052-missing-parent-")
  missing_journal <- round052_arm(missing_parent)
  r_files <- names(missing_journal$state$original_hashes)
  r_files <- r_files[startsWith(r_files, "R/")]
  .activate_update_journal(
    missing_journal, missing_parent$project, "journalverse"
  )
  for (path in r_files) {
    .record_update_delete(file.path(missing_parent$project, path))
  }
  .deactivate_update_journal()
  unlink(file.path(missing_parent$project, "R"), recursive = TRUE)
  expect_message(
    restored <- round052_recover_dead_owner(
      missing_parent$project, "journalverse", recover = TRUE
    ),
    "Recovered an interrupted update"
  )
  expect_true(restored$recovered)
  expect_true(.manifest_matches_project(missing_parent$project))
})

test_that("owner liveness distinguishes a live process from PID reuse", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(dir.exists("/proc"), "the process classifier requires /proc")
  fixture <- round052_fixture()
  journal <- round052_arm(fixture)
  sleeper <- bb_mcparallel(Sys.sleep(120), silent = TRUE)
  on.exit(bb_cleanup_child(sleeper), add = TRUE)
  state_path <- file.path(journal$path, "state.rds")
  state <- readRDS(state_path)
  state$pid <- sleeper$pid
  state$host <- .update_host()
  observed_start <- .update_process_start(sleeper$pid)
  state$process_start <- observed_start
  .atomic_save_rds(state, state_path)
  marker_path <- file.path(journal$path, "marker.rds")
  marker <- readRDS(marker_path)
  marker[c("pid", "host", "process_start")] <-
    state[c("pid", "host", "process_start")]
  .atomic_save_rds(marker, marker_path)

  expect_error(
    .recover_pending_update(fixture$project, "journalverse"),
    class = "bigbang_error_update_in_progress"
  )
  if (is.na(observed_start)) {
    expect_identical(.update_owner_liveness(state), "uncertain")
    return(invisible(NULL))
  }
  state$process_start <- if (is.na(observed_start)) {
    NA_character_
  } else {
    paste0(observed_start, "-reused")
  }
  .atomic_save_rds(state, state_path)
  marker[c("pid", "process_start")] <- state[c("pid", "process_start")]
  .atomic_save_rds(marker, marker_path)
  if (is.na(observed_start)) {
    expect_error(
      .recover_pending_update(fixture$project, "journalverse"),
      class = "bigbang_error_update_in_progress"
    )
  } else {
    expect_error(
      .recover_pending_update(fixture$project, "journalverse"),
      class = "bigbang_error_update_in_progress",
      regexp = paste0("pid ", sleeper$pid)
    )
  }

  second <- round052_fixture("bigbang-round052-force-live-")
  second_journal <- round052_arm(second)
  second_state_path <- file.path(second_journal$path, "state.rds")
  second_state <- readRDS(second_state_path)
  second_state$pid <- sleeper$pid
  second_state$host <- .update_host()
  second_state$process_start <- .update_process_start(sleeper$pid)
  .atomic_save_rds(second_state, second_state_path)
  second_marker_path <- file.path(second_journal$path, "marker.rds")
  second_marker <- readRDS(second_marker_path)
  second_marker[c("pid", "host", "process_start")] <-
    second_state[c("pid", "host", "process_start")]
  .atomic_save_rds(second_marker, second_marker_path)
  expect_error(
    .recover_pending_update(
      second$project, "journalverse", recover = TRUE
    ),
    class = "bigbang_error_update_in_progress",
    regexp = paste0("pid ", sleeper$pid)
  )
})

test_that("unknown process identity uses the conservative liveness policy", {
  fixture <- round052_fixture("bigbang-round052-no-proc-")
  journal <- round052_arm(fixture)
  state <- journal$state
  state$pid <- Sys.getpid()
  state$host <- .update_host()
  testthat::local_mocked_bindings(
    .update_process_start = function(pid = Sys.getpid()) NA_character_,
    .package = "bigbang"
  )
  expect_true(.update_owner_may_be_alive(state))
})

test_that("the update lock preserves a live owner and replaces a dead one", {
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(state) {
      if (identical(as.integer(state$pid), 99999999L)) "dead" else "alive"
    },
    .package = "bigbang"
  )
  fixture <- round052_fixture("bigbang-round057-lock-")
  lock <- .acquire_update_lock(fixture$project)
  on.exit(.release_update_lock(lock), add = TRUE)
  expect_error(
    .acquire_update_lock(fixture$project),
    class = "bigbang_error_update_in_progress"
  )
  expect_true(dir.exists(lock$path))
  .release_update_lock(lock)

  lock_path <- .update_lock_path(fixture$project)
  dir.create(lock_path)
  dead_owner <- .update_owner_record()
  dead_owner$pid <- 99999999L
  saveRDS(dead_owner, .update_lock_owner_path(lock_path))
  replacement <- .acquire_update_lock(fixture$project)
  expect_true(dir.exists(replacement$path))
  .release_update_lock(replacement)
  expect_false(dir.exists(lock_path))
  expect_null(.read_update_lock_owner(file.path(fixture$root, "absent-lock")))
  empty_lock <- file.path(fixture$root, "empty-lock")
  dir.create(empty_lock)
  expect_null(.read_update_lock_owner(empty_lock))
  saveRDS(list(pid = 1L), .update_lock_owner_path(empty_lock))
  expect_null(.read_update_lock_owner(empty_lock))
  expect_null(.update_tombstone_owner(list()))
  if (.Platform$OS.type != "windows") {
    dead <- list(pid = 99999999L, host = .update_host(),
                 started_utc = "now", process_start = NA_character_)
    expect_false(.update_owner_may_be_alive(dead))
  }
  .release_update_lock(NULL)
})

test_that("a live preparation is protected from a concurrent dry run", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round052_fixture("bigbang-round057-live-preparation-")
  marker <- file.path(fixture$root, "READY")
  release <- file.path(fixture$root, "RELEASE")
  Sys.setenv(BB057_READY = marker, BB057_RELEASE = release)
  on.exit(Sys.unsetenv(c("BB057_READY", "BB057_RELEASE")), add = TRUE)
  child <- bb_mcparallel({
    trace(".journal_backup_copy", where = asNamespace("bigbang"),
          tracer = quote({
            if (!file.exists(Sys.getenv("BB057_READY"))) {
              writeLines("ready", Sys.getenv("BB057_READY"), useBytes = TRUE)
              deadline <- Sys.time() + 10
              while (!file.exists(Sys.getenv("BB057_RELEASE")) &&
                       Sys.time() < deadline) Sys.sleep(0.02)
            }
          }), print = FALSE)
    round052_update(fixture, version = "0.2.0")
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit({
    bb_cleanup_child(child)
  }, add = TRUE)
  deadline <- Sys.time() + 10
  while (!file.exists(marker) && Sys.time() < deadline) Sys.sleep(0.02)
  expect_true(file.exists(marker))
  armando <- .update_journal_sibling_paths(
    fixture$project, "journalverse", "armando"
  )
  expect_length(armando, 1L)
  plan <- round052_update(fixture, dry_run = TRUE)
  expect_true(plan$dry_run)
  expect_true(dir.exists(armando))
  writeLines("release", release, useBytes = TRUE)
  result <- suppressMessages(bb_collect_child(child, timeout = 10)[[1L]])
  expect_s3_class(result, "bigbang_result")
  expect_true(isTRUE(result$updated))
  retry <- round052_update(fixture, version = "0.2.0")
  expect_s3_class(retry, "bigbang_result")
  expect_true(retry$updated)
  expect_true(.manifest_matches_project(fixture$project))
  expect_false(dir.exists(armando))
})

test_that("recovery is idempotent after interruption halfway through", {
  fixture <- round052_fixture()
  journal <- round052_arm(fixture)
  originals <- names(journal$state$original_hashes)[1:2]
  for (relative in originals) {
    .activate_update_journal(journal, fixture$project, "journalverse")
    .record_update_delete(file.path(fixture$project, relative))
    .deactivate_update_journal()
    unlink(file.path(fixture$project, relative))
  }
  copies <- 0L
  atomic_copy <- .atomic_copy
  local({
    testthat::local_mocked_bindings(
      .atomic_copy = function(...) {
        copies <<- copies + 1L
        if (copies == 2L) stop("forced recovery interruption")
        atomic_copy(...)
      },
      .package = "bigbang"
    )
    expect_error(
      round052_recover_dead_owner(fixture$project, "journalverse"),
      "forced recovery interruption"
    )
  })
  expect_true(dir.exists(journal$path))
  expect_message(
    result <- round052_recover_dead_owner(fixture$project, "journalverse"),
    "Recovered an interrupted update"
  )
  expect_true(result$recovered)
  expect_true(.manifest_matches_project(fixture$project))
})

test_that("real process death halfway through recovery converges", {
  skip_on_cran()
  skip_on_os("windows") # R has no safe fork/SIGKILL harness on Windows.
  fixture <- round052_fixture("bigbang-round052-recovery-kill-")
  journal <- round052_arm(fixture)
  originals <- names(journal$state$original_hashes)[1:3]
  .activate_update_journal(journal, fixture$project, "journalverse")
  for (relative in originals) {
    .record_update_delete(file.path(fixture$project, relative))
    unlink(file.path(fixture$project, relative))
  }
  .deactivate_update_journal()
  marker <- file.path(fixture$root, "RECOVERY_MARK")
  Sys.setenv(BB052_RECOVERY_MARK = marker, BB052_PROJECT = fixture$project)
  on.exit(Sys.unsetenv(c("BB052_RECOVERY_MARK", "BB052_PROJECT")), add = TRUE)
  child <- bb_mcparallel({
    trace(".atomic_copy", where = asNamespace("bigbang"), exit = quote({
      if (startsWith(
        normalizePath(destination, winslash = "/", mustWork = FALSE),
        paste0(Sys.getenv("BB052_PROJECT"), "/")
      ) && !file.exists(Sys.getenv("BB052_RECOVERY_MARK"))) {
        writeLines("restored-one", Sys.getenv("BB052_RECOVERY_MARK"),
                   useBytes = TRUE)
        Sys.sleep(600)
      }
    }), print = FALSE)
    bigbang:::.recover_pending_update(
      fixture$project, "journalverse", recover = TRUE
    )
  }, silent = TRUE, mc.set.seed = FALSE)
  deadline <- Sys.time() + 30
  while (!file.exists(marker) && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(file.exists(marker))
  expect_true(tools::pskill(child$pid, tools::SIGKILL))
  expect_true(!is.null(bb_collect_child(child, timeout = 1)))
  expect_true(dir.exists(journal$path))
  expect_message(
    result <- round052_recover_dead_owner(fixture$project, "journalverse"),
    "Recovered an interrupted update"
  )
  expect_true(result$recovered)
  expect_true(.manifest_matches_project(fixture$project))
  expect_false(dir.exists(journal$path))
})

test_that("dry run reports recovery without changing project or journal", {
  fixture <- round052_fixture()
  journal <- round052_arm(fixture)
  relative <- names(journal$state$original_hashes)[[1L]]
  .activate_update_journal(journal, fixture$project, "journalverse")
  .record_update_delete(file.path(fixture$project, relative))
  .deactivate_update_journal()
  unlink(file.path(fixture$project, relative))
  before_project <- round052_snapshot(fixture$project)
  before_journal <- round052_snapshot(journal$path)

  expect_message(
    result <- round052_update_dead_owner(fixture, dry_run = TRUE),
    "Dry run: pending update journal action"
  )
  expect_true(result$dry_run)
  expect_identical(round052_snapshot(fixture$project), before_project)
  expect_identical(round052_snapshot(journal$path), before_journal)
})

test_that("a Windows replacement window restores an absent old file from backup", {
  fixture <- round052_fixture("bigbang-round053-windows-window-")
  relative <- "README.md"
  destination <- file.path(fixture$project, relative)
  original <- readBin(destination, "raw", n = file.info(destination)$size)
  journal <- round052_arm(fixture)
  replacement <- file.path(journal$path, "staging", ".README.md-round053window")
  writeLines("replacement that never reached the destination", replacement,
             useBytes = TRUE)
  writeLines(
    paste("write", unname(as.character(tools::md5sum(replacement))), relative,
          sep = "\t"),
    file.path(journal$path, "intent.log"), useBytes = TRUE
  )
  .activate_update_journal(journal, fixture$project, "journalverse")
  on.exit(.deactivate_update_journal(), add = TRUE)

  failed <- testthat::with_mocked_bindings(
    .atomic_replace = function(source, target) {
      unlink(target)
      stop("simulated process death between remove and rename")
    },
    .package = "bigbang",
    tryCatch({
      .atomic_replace(replacement, destination)
      NULL
    }, error = identity)
  )
  expect_match(conditionMessage(failed), "simulated process death", fixed = TRUE)
  unlink(destination)
  expect_false(file.exists(destination))

  intents <- .read_update_intents(journal$path)
  unknown <- .unknown_update_paths(fixture$project, journal$state, intents)
  expect_identical(length(unknown), 0L)
  expect_message(
    recovered <- round052_recover_dead_owner(
      fixture$project, "journalverse", recover = TRUE
    ),
    "Recovered an interrupted update"
  )
  expect_true(recovered$recovered)
  expect_identical(readBin(destination, "raw", n = file.info(destination)$size),
                   original)
  expect_false(dir.exists(journal$path))
})

test_that("completed manifest wins over a stale journal", {
  fixture <- round052_fixture()
  old <- round052_snapshot(fixture$project)
  journal <- round052_arm(fixture)
  manifest_path <- file.path(fixture$project, .generation_manifest_name)
  manifest <- readRDS(manifest_path)
  readme <- file.path(fixture$project, "README.md")
  .activate_update_journal(journal, fixture$project, "journalverse")
  replacement <- tempfile("round052-new-readme-")
  writeLines("finished update", replacement, useBytes = TRUE)
  .atomic_copy(replacement, readme)
  manifest$hashes[["README.md"]] <- .file_digest(readme)
  .atomic_save_rds(manifest, manifest_path)
  .deactivate_update_journal()
  completed <- round052_snapshot(fixture$project)
  expect_false(identical(completed, old))

  plan <- round052_recover_dead_owner(
    fixture$project, "journalverse", dry_run = TRUE
  )
  expect_identical(plan$action, "discard_completed")
  expect_true(dir.exists(journal$path))

  expect_message(
    result <- round052_recover_dead_owner(fixture$project, "journalverse"),
    "previous update had completed"
  )
  expect_identical(round052_snapshot(fixture$project), completed)
  expect_true(result$recovered)
})

test_that("an unrecorded self-consistent manifest is not treated as completed", {
  fixture <- round052_fixture()
  round052_arm(fixture)
  manifest_path <- file.path(fixture$project, .generation_manifest_name)
  manifest <- readRDS(manifest_path)
  readme <- file.path(fixture$project, "README.md")
  writeLines("unrecorded user state", readme, useBytes = TRUE)
  manifest$hashes[["README.md"]] <- .file_digest(readme)
  saveRDS(manifest, manifest_path)

  plan <- round052_recover_dead_owner(
    fixture$project, "journalverse", recover = TRUE, dry_run = TRUE
  )
  expect_identical(plan$action, "preserve_and_recover")
  expect_contains(plan$unknown, c("README.md", .generation_manifest_name))
})

test_that("journal is external to artifacts and result file lists", {
  skip_on_cran()
  fixture <- round052_fixture()
  journal <- round052_arm(fixture)
  result <- NULL
  expect_message(
    result <- round052_update_dead_owner(fixture),
    "Recovered an interrupted update"
  )
  expect_false(any(grepl("bigbang-update", result$removed_files, fixed = TRUE)))
  manifest <- readRDS(file.path(fixture$project, .generation_manifest_name))
  expect_false(any(grepl("bigbang-update", manifest$files, fixed = TRUE)))
  journal <- round052_arm(fixture)
  built <- pkgbuild::build(fixture$project, dest_path = fixture$root,
                           quiet = TRUE)
  members <- utils::untar(built, list = TRUE)
  expect_false(any(grepl("bigbang-update", members, fixed = TRUE)))
  .discard_update_journal(journal, fixture$project, "journalverse")
})

round052_pause_expression <- function(label) {
  substitute({
    writeLines(LABEL, Sys.getenv("BB052_MARK"), useBytes = TRUE)
    Sys.sleep(600)
  }, list(LABEL = label))
}

round052_signal_case <- function(signal, phase) {
  fixture <- round052_fixture(
    paste0("bigbang-round052-signal-", phase, "-"),
    document = TRUE, workflow = TRUE, include_archives = TRUE
  )
  before <- round052_snapshot(fixture$project)
  marker <- file.path(fixture$root, "MARK")
  Sys.setenv(BB052_MARK = marker, BB052_PROJECT = fixture$project)
  on.exit(Sys.unsetenv(c("BB052_MARK", "BB052_PROJECT")), add = TRUE)
  child <- bb_mcparallel({
    ns <- asNamespace("bigbang")
    pause <- round052_pause_expression(phase)
    if (identical(phase, "during_backup")) {
      trace(".journal_backup_copy", where = ns, tracer = pause, print = FALSE)
    } else if (identical(phase, "after_backup_before_delete")) {
      trace(".remove_stale_generation_files", where = ns, tracer = pause,
            print = FALSE)
    } else if (identical(phase, "during_delete")) {
      trace(".stale_unlink", where = ns, exit = substitute(
        if (!file.exists(Sys.getenv("BB052_MARK"))) PAUSE,
        list(PAUSE = pause)
      ), print = FALSE)
    } else if (identical(phase, "during_archive_copy")) {
      trace(".atomic_copy", where = ns, tracer = substitute(
        if (startsWith(normalizePath(destination, winslash = "/", mustWork = FALSE),
                       paste0(Sys.getenv("BB052_PROJECT"), "/inst/archives/"))) PAUSE,
        list(PAUSE = pause)
      ), print = FALSE)
    } else if (identical(phase, "during_write")) {
      trace(".write_utf8", where = ns, tracer = substitute(
        if (startsWith(normalizePath(path, winslash = "/", mustWork = FALSE),
                       paste0(Sys.getenv("BB052_PROJECT"), "/"))) PAUSE,
        list(PAUSE = pause)
      ), print = FALSE)
    } else if (identical(phase, "during_roxygen")) {
      trace("document", where = asNamespace("devtools"), tracer = pause,
            print = FALSE)
    } else if (identical(phase, "after_write_before_manifest")) {
      trace(".manifest_records", where = ns, tracer = pause, print = FALSE)
    } else if (identical(phase, "during_manifest")) {
      trace(".atomic_save_rds", where = ns, tracer = substitute(
        if (identical(normalizePath(path, winslash = "/", mustWork = FALSE),
                      file.path(Sys.getenv("BB052_PROJECT"),
                                ".bigbang-manifest.rds"))) PAUSE,
        list(PAUSE = pause)
      ), print = FALSE)
    } else if (identical(phase, "after_manifest_before_discard")) {
      trace(".discard_update_journal", where = ns, tracer = pause,
            print = FALSE)
    }
    bigbang::create_metapackage(
      "journalverse", fixture$archive, dest_dir = fixture$destination,
      document = identical(phase, "during_roxygen"), verbose = FALSE,
      import_deps = character(), force_deps = character(),
      include_archives = identical(phase, "during_archive_copy"),
      update = TRUE
    )
  }, silent = TRUE, mc.set.seed = FALSE)
  deadline <- Sys.time() + 30
  while (!file.exists(marker) && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(file.exists(marker), info = phase)
  expect_true(tools::pskill(child$pid, signal), info = phase)
  expect_true(!is.null(bb_collect_child(child, timeout = 1)))
  after_kill <- round052_snapshot(fixture$project)
  if (identical(phase, "during_backup")) expect_identical(after_kill, before)
  if (identical(phase, "after_manifest_before_discard")) {
    journal_path <- .update_journal_path(fixture$project)
    intents <- .read_update_intents(journal_path)
    final_manifest <- readRDS(file.path(
      fixture$project, .generation_manifest_name
    ))
    written <- c(final_manifest$files, .generation_manifest_name)
    expect_true(all(vapply(written, function(relative) {
      hash <- .file_digest(file.path(fixture$project, relative))
      any(intents$operation == "write" & intents$path == relative &
            intents$hash == hash)
    }, logical(1L))))
    expect_true(any(
      intents$operation == "delete" &
        intents$path == file.path("vignettes", "workflow-journalverse.Rmd")
    ))
    expect_false(any(grepl("bigbang-update", intents$path, fixed = TRUE)))
  }
  recovered_snapshot <- file.path(fixture$root, "recovered.rds")
  Sys.setenv(BB052_RECOVERED = recovered_snapshot)
  on.exit(Sys.unsetenv("BB052_RECOVERED"), add = TRUE)
  trace(".validate_update_manifest", where = asNamespace("bigbang"),
        tracer = quote({
          relative <- list.files(
            project_dir, all.files = TRUE, recursive = TRUE, no.. = TRUE,
            include.dirs = TRUE
          )
          full <- file.path(project_dir, relative)
          info <- file.info(full)
          hashes <- rep(NA_character_, length(full))
          hashes[!info$isdir] <- unname(as.character(tools::md5sum(
            full[!info$isdir]
          )))
          saveRDS(data.frame(
            path = relative, directory = info$isdir, hash = hashes,
            stringsAsFactors = FALSE
          ), Sys.getenv("BB052_RECOVERED"))
        }), print = FALSE)
  on.exit(untrace(".validate_update_manifest", where = asNamespace("bigbang")),
          add = TRUE)
  result <- round052_update_dead_owner(
    fixture, document = identical(phase, "during_roxygen"),
    include_archives = identical(phase, "during_archive_copy")
  )
  expect_true(file.exists(recovered_snapshot), info = phase)
  observed_recovery <- readRDS(recovered_snapshot)
  expected_recovery <- if (identical(phase, "after_manifest_before_discard")) {
    after_kill
  } else {
    before
  }
  expect_identical(observed_recovery, expected_recovery, info = phase)
  expect_true(.manifest_matches_project(fixture$project), info = phase)
  expect_false(dir.exists(.update_journal_path(fixture$project)), info = phase)
  if (identical(phase, "during_backup")) {
    expect_false(result$recovered, info = phase)
  } else {
    expect_true(result$recovered, info = phase)
  }
  invisible(result)
}

test_that("real SIGKILL recovers every measured update phase", {
  skip_on_cran()
  skip_on_os("windows") # R has no safe fork/SIGKILL harness on Windows.
  phases <- c(
    "during_backup", "after_backup_before_delete", "during_delete",
    "during_archive_copy", "during_write", "during_roxygen",
    "after_write_before_manifest", "during_manifest",
    "after_manifest_before_discard"
  )
  for (phase in phases) round052_signal_case(tools::SIGKILL, phase)
})

test_that("real SIGTERM leaves the same recoverable journal", {
  skip_on_cran()
  skip_on_os("windows") # Windows TerminateProcess is not a POSIX SIGTERM test.
  round052_signal_case(tools::SIGTERM, "during_delete")
})
