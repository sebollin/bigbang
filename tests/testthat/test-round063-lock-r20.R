round063_lock_fixture <- function(prefix = "bigbang-round063-lock-") {
  root <- tempfile(prefix)
  dir.create(root, recursive = TRUE)
  project <- file.path(root, "project")
  dir.create(project)
  list(root = root, project = project, lock = .update_lock_path(project))
}

round063_uncertain_owner <- function() {
  owner <- .update_owner_record()
  owner$host <- "round063-unknown-host"
  owner
}

round063_snapshot <- function(path) {
  entries <- list.files(path, all.files = TRUE, no.. = TRUE,
                        recursive = TRUE, include.dirs = TRUE)
  if (length(entries) == 0L) return("empty")
  full <- file.path(path, entries)
  kind <- ifelse(dir.exists(full), "D", ifelse(.path_is_symlink(full), "L", "F"))
  value <- vapply(seq_along(full), function(index) {
    if (kind[[index]] == "D") return("")
    if (kind[[index]] == "L") return(Sys.readlink(full[[index]]))
    unname(tools::md5sum(full[[index]]))
  }, character(1L))
  digest_file <- tempfile("round063-snapshot-")
  on.exit(unlink(digest_file, force = TRUE), add = TRUE)
  writeLines(paste(entries, kind, value, sep = "|"), digest_file,
             useBytes = TRUE)
  unname(tools::md5sum(digest_file))
}

round063_journal_fixture <- function(prefix = "bigbang-round063-journal-") {
  root <- tempfile(prefix)
  project <- file.path(root, "project")
  dir.create(project, recursive = TRUE)
  value <- file.path(project, "value.txt")
  writeLines("old", value, useBytes = TRUE)
  manifest <- list(
    schema = 2L, files = "value.txt",
    hashes = stats::setNames(.file_digest(value), "value.txt")
  )
  saveRDS(manifest, file.path(project, .generation_manifest_name))
  list(root = root, project = project, name = "project", manifest = manifest)
}

test_that("discarded live owners survive a killed claimant", {
  skip_on_cran()
  skip_on_os("windows")
  testthat::local_mocked_bindings(
    .update_process_stat = function(...) NULL,
    .package = "bigbang"
  )
  fixture <- round063_lock_fixture("bigbang-round063-live-owner-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  owner_ready <- file.path(fixture$root, "owner-ready")
  owner <- bb_mcparallel({
    lock <- .acquire_update_lock(fixture$project)
    writeLines("ready", owner_ready, useBytes = TRUE)
    Sys.sleep(600)
    .release_update_lock(lock)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(bb_cleanup_child(owner), add = TRUE)
  deadline <- Sys.time() + 20
  while (!file.exists(owner_ready) && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(owner_ready))
  owner_record <- .read_update_lock_owner(fixture$lock)
  owner_hash <- unname(tools::md5sum(.update_lock_owner_path(fixture$lock)))
  discarded <- .update_lock_discard_path(fixture$lock, basename(fixture$project))
  expect_true(file.rename(fixture$lock, discarded))

  claim_ready <- file.path(fixture$root, "claim-ready")
  claimant <- bb_mcparallel({
    .atomic_save_rds(.update_owner_record(),
                     .update_lock_claim_path(discarded))
    writeLines("ready", claim_ready, useBytes = TRUE)
    Sys.sleep(600)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(bb_cleanup_child(claimant), add = TRUE)
  deadline <- Sys.time() + 20
  while (!file.exists(claim_ready) && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(claim_ready))
  expect_true(tools::pskill(claimant$pid, tools::SIGKILL))
  expect_true(!is.null(bb_collect_child(claimant, timeout = 2)))

  error <- expect_error(
    .acquire_update_lock(fixture$project),
    class = "bigbang_error_update_in_progress"
  )
  expect_match(conditionMessage(error), paste0("pid ", owner_record$pid))
  expect_true(dir.exists(fixture$lock))
  expect_identical(
    unname(tools::md5sum(.update_lock_owner_path(fixture$lock))), owner_hash
  )
})

test_that("a zombie is dead, while pid one is never classified as dead", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(dir.exists("/proc"), "the zombie classifier requires /proc")
  skip_if_not(
    identical(.update_process_stat(Sys.getpid()), "alive"),
    "the process-stat classifier is unavailable"
  )
  child <- bb_mcparallel(Sys.sleep(600), silent = TRUE,
                         mc.set.seed = FALSE)
  on.exit(bb_cleanup_child(child), add = TRUE)
  zombie <- .update_owner_record()
  zombie$pid <- as.integer(child$pid)
  zombie$process_start <- .update_process_start(child$pid)
  expect_true(tools::pskill(child$pid, tools::SIGKILL))
  state <- NA_character_
  deadline <- Sys.time() + 5
  while (Sys.time() < deadline) {
    state <- .update_process_stat(child$pid)
    if (identical(state, "dead")) break
    Sys.sleep(0.01)
  }
  expect_identical(state, "dead")
  expect_identical(.update_owner_liveness(zombie), "dead")

  if (file.exists("/proc/1/stat")) {
    init <- .update_owner_record()
    init$pid <- 1L
    init$process_start <- .update_process_start(1L)
    expect_false(identical(.update_owner_liveness(init), "dead"))
  }
})

test_that("dry run and execution agree for an uncertain lock preparation", {
  for (recover in c(FALSE, TRUE)) {
    fixture <- round063_lock_fixture(
      paste0("bigbang-round063-dry-", if (recover) "recover-" else "block-")
    )
    withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
    temporary <- .update_lock_temporary_path(fixture$project)
    dir.create(temporary)
    .atomic_save_rds(round063_uncertain_owner(),
                     .update_lock_owner_path(temporary))
    before <- round063_snapshot(fixture$root)
    dry <- .acquire_update_lock(fixture$project, recover = recover,
                                dry_run = TRUE)
    expect_identical(round063_snapshot(fixture$root), before)
    if (!recover) {
      expect_identical(dry$action, "blocked_uncertain_temporary")
      expect_error(
        .acquire_update_lock(fixture$project, recover = FALSE),
        class = "bigbang_error_update_in_progress"
      )
      expect_identical(round063_snapshot(fixture$root), before)
    } else {
      expect_identical(dry$action, "free")
      acquired <- .acquire_update_lock(fixture$project, recover = TRUE)
      expect_true(acquired$acquired)
      .release_update_lock(acquired)
    }
  }
})

test_that("recover sets aside both kinds of lock symlink without touching targets", {
  skip_on_os("windows")
  for (target_is_dir in c(FALSE, TRUE)) {
    fixture <- round063_lock_fixture(
      paste0("bigbang-round063-symlink-", if (target_is_dir) "dir-" else "file-")
    )
    withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
    target <- file.path(fixture$root, "target")
    if (target_is_dir) {
      dir.create(target)
      marker <- file.path(target, "marker")
      writeLines("target", marker, useBytes = TRUE)
      target_hash <- unname(tools::md5sum(marker))
    } else {
      marker <- NULL
      target_hash <- NA_character_
    }
    expect_true(file.symlink(target, fixture$lock))
    expect_match(
      conditionMessage(expect_error(
        .acquire_update_lock(fixture$project),
        class = "bigbang_error_update_in_progress"
      )),
      "symbolic link", ignore.case = TRUE
    )
    acquired <- .acquire_update_lock(fixture$project, recover = TRUE)
    withr::defer(.release_update_lock(acquired))
    expect_true(acquired$acquired)
    if (target_is_dir) {
      expect_identical(unname(tools::md5sum(marker)), target_hash)
    } else {
      expect_false(file.exists(target) || dir.exists(target))
    }
  }
})

test_that("inventory deletion is byte-exact and keeps mismatches apart", {
  fixture <- round063_lock_fixture("bigbang-round063-inventory-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  discarded <- file.path(
    fixture$root,
    ".project.bigbang-update.lock.descartado-inventory"
  )
  dir.create(discarded)
  evaluated <- .update_owner_record()
  replacement <- evaluated
  replacement$started_utc <- paste0(evaluated$started_utc, "-replacement")
  .atomic_save_rds(replacement, .update_lock_owner_path(discarded))
  .atomic_save_rds(evaluated, .update_lock_claim_path(discarded))
  .update_lock_inventory_discard(
    discarded, evaluated, "project", owner_digest = .update_lock_value_digest(evaluated),
    claim = evaluated, claim_digest = .update_lock_value_digest(evaluated)
  )
  apart <- list.files(fixture$root, pattern = "^\\.project\\.bigbang-apartado-",
                      full.names = TRUE, all.files = TRUE)
  expect_length(apart, 1L)
  expect_true(file.exists(file.path(apart, "owner.rds")))
  expect_false(file.exists(file.path(apart, "claim.rds")))
})

test_that("150 discarded locks are processed while progress continues", {
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(...) "dead",
    .package = "bigbang"
  )
  fixture <- round063_lock_fixture("bigbang-round063-many-discarded-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  for (index in seq_len(150L)) {
    discarded <- file.path(
      fixture$root,
      paste0(".project.bigbang-update.lock.descartado-old-", index)
    )
    dir.create(discarded)
    old <- .update_owner_record()
    old$pid <- 99999999L
    old$process_start <- "0"
    .atomic_save_rds(old, .update_lock_owner_path(discarded))
    .atomic_save_rds(old, .update_lock_claim_path(discarded))
  }
  acquired <- .acquire_update_lock(fixture$project)
  expect_true(acquired$acquired)
  expect_length(.update_lock_discarded_paths(fixture$project), 0L)
  .release_update_lock(acquired)
})

test_that("lock creation errors name a read-only parent", {
  fixture <- round063_lock_fixture("bigbang-round063-read-only-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  withr::defer(Sys.chmod(fixture$root, mode = "0755"))
  Sys.chmod(fixture$root, mode = "0555")
  if (file.access(fixture$root, 2L) == 0L) {
    Sys.chmod(fixture$root, mode = "0755")
    skip("the filesystem does not represent a read-only directory")
  }
  error <- expect_error(
    .acquire_update_lock(fixture$project),
    class = "bigbang_error_update_in_progress"
  )
  expect_match(conditionMessage(error), "writable|free space", ignore.case = TRUE)
  expect_false(grepl("recover = TRUE", conditionMessage(error), fixed = TRUE))
})

test_that("a lost published lock aborts before journal mutation", {
  fixture <- round063_lock_fixture("bigbang-round063-lost-lock-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  writeLines("old", file.path(fixture$project, "value.txt"), useBytes = TRUE)
  manifest <- list(
    schema = 2L, files = "value.txt",
    hashes = stats::setNames(.file_digest(file.path(fixture$project, "value.txt")),
                             "value.txt")
  )
  saveRDS(manifest, file.path(fixture$project, .generation_manifest_name))
  lock <- .acquire_update_lock(fixture$project)
  withr::defer(unlink(lock$path, recursive = TRUE, force = TRUE))
  forged <- lock$owner
  forged$started_utc <- paste0(forged$started_utc, "-forged")
  .atomic_save_rds(forged, .update_lock_owner_path(lock$path))
  before <- round063_snapshot(fixture$project)
  expect_error(
    .create_update_journal(fixture$project, "project", manifest, lock = lock),
    class = "bigbang_error_update_in_progress"
  )
  expect_identical(round063_snapshot(fixture$project), before)
})

test_that("a journal with mismatched marker and state owners is never rolled back", {
  fixture <- round063_journal_fixture("bigbang-round063-owner-mismatch-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  journal <- .create_update_journal(
    fixture$project, fixture$name, fixture$manifest
  )
  state_path <- file.path(journal$path, "state.rds")
  state <- readRDS(state_path)
  state$started_utc <- paste0(state$started_utc, "-forged")
  .atomic_save_rds(state, state_path)
  before <- round063_snapshot(fixture$project)
  result <- .recover_pending_update(
    fixture$project, fixture$name, recover = TRUE, handled = TRUE
  )
  expect_identical(result$action, "apart_owner_mismatch")
  expect_identical(round063_snapshot(fixture$project), before)
  expect_false(dir.exists(journal$path))
  expect_length(list.files(
    fixture$root, pattern = "^\\.project\\.bigbang-apartado-",
    all.files = TRUE, full.names = TRUE
  ), 1L)
})

test_that("Windows owners are never probed with a process signal", {
  # tools::pskill() terminates the target on Windows whatever the signal.
  probes <- 0L
  testthat::local_mocked_bindings(
    .update_is_windows = function() TRUE,
    .update_signal_probe = function(pid) {
      probes <<- probes + 1L
      TRUE
    },
    .package = "bigbang"
  )
  owner <- list(
    pid = Sys.getpid(),
    host = .update_host(),
    process_start = .update_process_start()
  )
  expect_identical(.update_owner_liveness(owner), "uncertain")
  expect_identical(.update_owner_status(owner), "uncertain")
  expect_identical(probes, 0L)
})
