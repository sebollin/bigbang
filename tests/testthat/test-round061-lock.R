round061_lock_snapshot <- function(path) {
  entries <- list.files(path, all.files = TRUE, no.. = TRUE,
                        recursive = TRUE, include.dirs = TRUE)
  if (length(entries) == 0L) return("empty")
  full <- file.path(path, entries)
  kind <- ifelse(
    dir.exists(full), "D", ifelse(.path_is_symlink(full), "L", "F")
  )
  value <- vapply(seq_along(full), function(index) {
    if (kind[[index]] == "D") return("")
    if (kind[[index]] == "L") return(Sys.readlink(full[[index]]))
    unname(tools::md5sum(full[[index]]))
  }, character(1L))
  digest_file <- tempfile("round061-lock-snapshot-")
  on.exit(unlink(digest_file, force = TRUE), add = TRUE)
  writeLines(paste(entries, kind, value, sep = "|"), digest_file,
             useBytes = TRUE)
  unname(tools::md5sum(digest_file))
}

round061_lock_fixture <- function(prefix = "bigbang-round061-lock-") {
  root <- tempfile(prefix)
  dir.create(root, recursive = TRUE)
  project <- file.path(root, "project")
  dir.create(project)
  list(root = root, project = project, lock = .update_lock_path(project))
}

test_that("published locks always contain a complete owner", {
  fixture <- round061_lock_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  lock <- .acquire_update_lock(fixture$project)
  expect_true(lock$acquired)
  expect_identical(.read_update_lock_owner(fixture$lock), lock$owner)
  expect_length(.update_lock_temporary_paths(fixture$project), 0L)
  .release_update_lock(lock)
  expect_false(file.exists(fixture$lock) || dir.exists(fixture$lock))
})

test_that("recover never steals a proven live owner or a live token conflict", {
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(state) {
      if (identical(state$process_start, "not-the-current-start-token")) {
        "live_token_conflict"
      } else {
        "alive"
      }
    },
    .package = "bigbang"
  )
  fixture <- round061_lock_fixture("bigbang-round061-live-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  owner <- .update_owner_record()
  dir.create(fixture$lock)
  .atomic_save_rds(owner, .update_lock_owner_path(fixture$lock))
  error <- expect_error(
    .acquire_update_lock(fixture$project, recover = TRUE),
    class = "bigbang_error_update_in_progress"
  )
  expect_match(conditionMessage(error), paste0("pid ", owner$pid))

  owner$process_start <- "not-the-current-start-token"
  .atomic_save_rds(owner, .update_lock_owner_path(fixture$lock))
  error <- expect_error(
    .acquire_update_lock(fixture$project, recover = TRUE),
    class = "bigbang_error_update_in_progress"
  )
  expect_match(conditionMessage(error), paste0("pid ", owner$pid))
  expect_true(dir.exists(fixture$lock))
})

test_that("dry-run lock inspection is byte-for-byte read-only", {
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(state) {
      if (identical(as.integer(state$pid), 99999999L)) "dead" else "alive"
    },
    .package = "bigbang"
  )
  states <- list(
    free = function(fixture) invisible(NULL),
    live = function(fixture) {
      dir.create(fixture$lock)
      .atomic_save_rds(.update_owner_record(),
                       .update_lock_owner_path(fixture$lock))
    },
    orphan = function(fixture) {
      dir.create(fixture$lock)
      owner <- .update_owner_record()
      owner$pid <- 99999999L
      owner$process_start <- "0"
      .atomic_save_rds(owner, .update_lock_owner_path(fixture$lock))
    },
    unreadable = function(fixture) {
      dir.create(fixture$lock)
      writeBin(charToRaw("incomplete"),
               .update_lock_owner_path(fixture$lock))
    },
    user_entry = function(fixture) {
      writeLines("user bytes", fixture$lock, useBytes = TRUE)
    }
  )
  for (state in names(states)) {
    fixture <- round061_lock_fixture(paste0("bigbang-round061-dry-", state, "-"))
    withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
    states[[state]](fixture)
    before <- round061_lock_snapshot(fixture$root)
    result <- .acquire_update_lock(
      fixture$project, recover = TRUE, dry_run = TRUE
    )
    after <- round061_lock_snapshot(fixture$root)
    expect_identical(after, before, info = state)
    expect_false(result$acquired)
  }
})

test_that("an interrupted orphan claim is inventoried before replacement", {
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(state) {
      if (identical(as.integer(state$pid), 99999999L)) "dead" else "alive"
    },
    .package = "bigbang"
  )
  fixture <- round061_lock_fixture("bigbang-round061-claim-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  discarded <- file.path(
    fixture$root,
    ".project.bigbang-update.lock.descartado-test"
  )
  dir.create(discarded)
  old_owner <- .update_owner_record()
  old_owner$pid <- 99999999L
  old_owner$process_start <- "0"
  .atomic_save_rds(old_owner, .update_lock_owner_path(discarded))
  claim <- old_owner
  .atomic_save_rds(claim, .update_lock_claim_path(discarded))
  writeLines("unverified", file.path(discarded, "extra"), useBytes = TRUE)

  state <- .update_lock_state(fixture$project, recover = TRUE)
  expect_identical(state$action, "claim_orphan_discard")
  acquired <- .acquire_update_lock(fixture$project, recover = TRUE)
  expect_true(acquired$acquired)
  apart <- list.files(
    fixture$root,
    pattern = "^\\.project\\.bigbang-apartado-",
    all.files = TRUE,
    full.names = TRUE
  )
  expect_length(apart, 1L)
  expect_identical(readLines(file.path(apart, "extra")), "unverified")
  .release_update_lock(acquired)
})

test_that("a live orphan claim remains blocking", {
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(state) {
      if (identical(as.integer(state$pid), 99999999L)) "dead" else "alive"
    },
    .package = "bigbang"
  )
  fixture <- round061_lock_fixture("bigbang-round061-live-claim-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  discarded <- file.path(
    fixture$root,
    ".project.bigbang-update.lock.descartado-test"
  )
  dir.create(discarded)
  old_owner <- .update_owner_record()
  old_owner$pid <- 99999999L
  old_owner$process_start <- "0"
  .atomic_save_rds(old_owner, .update_lock_owner_path(discarded))
  .atomic_save_rds(.update_owner_record(), .update_lock_claim_path(discarded))
  error <- expect_error(
    .acquire_update_lock(fixture$project, recover = TRUE),
    class = "bigbang_error_update_in_progress"
  )
  expect_match(conditionMessage(error), "pid")
  expect_true(dir.exists(discarded))
})

test_that("a killed lock preparation is set aside before the next lock", {
  skip_on_os("windows")
  skip_on_cran()
  fixture <- round061_lock_fixture("bigbang-round061-preparation-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  ready <- file.path(fixture$root, "ready")
  child <- bb_mcparallel({
    preparation <- .update_lock_temporary_path(fixture$project)
    dir.create(preparation)
    writeLines("partial bytes", file.path(preparation, "partial"),
               useBytes = TRUE)
    writeLines("ready", ready, useBytes = TRUE)
    Sys.sleep(600)
  }, silent = TRUE)
  on.exit(bb_cleanup_child(child), add = TRUE)
  deadline <- Sys.time() + 30
  while (!file.exists(ready) && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(ready))
  expect_true(tools::pskill(child$pid, tools::SIGKILL))
  expect_true(!is.null(bb_collect_child(child, timeout = 1)))

  acquired <- .acquire_update_lock(fixture$project)
  expect_true(acquired$acquired)
  expect_length(.update_lock_temporary_paths(fixture$project), 0L)
  apart <- list.files(fixture$root, pattern = "^\\.project\\.bigbang-apartado-",
                      all.files = TRUE, full.names = TRUE)
  expect_length(apart, 1L)
  expect_identical(readLines(file.path(apart, "partial")), "partial bytes")
  .release_update_lock(acquired)
})

test_that("two real processes have one orphan-recovery winner", {
  skip_on_os("windows")
  skip_on_cran()
  testthat::local_mocked_bindings(
    .update_process_stat = function(...) NULL,
    .package = "bigbang"
  )
  for (round in seq_len(1L)) {
    fixture <- round061_lock_fixture(
      paste0("bigbang-round061-race-", round, "-")
    )
    withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
    dir.create(fixture$lock)
    owner <- .update_owner_record()
    owner$pid <- 99999999L
    owner$process_start <- "0"
    .atomic_save_rds(owner, .update_lock_owner_path(fixture$lock))
    children <- lapply(seq_len(2L), function(index) {
      bb_mcparallel({
        writeLines("ready", file.path(fixture$root, paste0("ready-", index)),
                   useBytes = TRUE)
        trigger <- if (identical(index, 1L)) "go" else "go-second"
        while (!file.exists(file.path(fixture$root, trigger))) {
          Sys.sleep(0.005)
        }
        lock <- tryCatch(
          .acquire_update_lock(fixture$project, recover = TRUE),
          error = identity
        )
        outcome <- file.path(fixture$root, paste0("outcome-", index))
        if (inherits(lock, "error")) {
          writeLines(paste0("error: ", conditionMessage(lock)), outcome,
                     useBytes = TRUE)
          return(invisible(NULL))
        }
        writeLines("acquired", outcome, useBytes = TRUE)
        while (!file.exists(file.path(fixture$root, "release"))) {
          Sys.sleep(0.005)
        }
        .release_update_lock(lock)
        invisible(NULL)
      }, silent = TRUE, mc.set.seed = FALSE)
    })
    on.exit(lapply(children, bb_cleanup_child), add = TRUE)
    deadline <- Sys.time() + 10
    while (length(list.files(fixture$root, pattern = "^ready-",
                             all.files = TRUE)) < 2L &&
             Sys.time() < deadline) {
      Sys.sleep(0.005)
    }
    expect_length(
      list.files(fixture$root, pattern = "^ready-", all.files = TRUE), 2L
    )
    writeLines("go", file.path(fixture$root, "go"), useBytes = TRUE)
    deadline <- Sys.time() + 10
    while (!file.exists(file.path(fixture$root, "outcome-1")) &&
             Sys.time() < deadline) {
      Sys.sleep(0.005)
    }
    expect_true(file.exists(file.path(fixture$root, "outcome-1")))
    writeLines("go-second", file.path(fixture$root, "go-second"),
               useBytes = TRUE)
    deadline <- Sys.time() + 10
    while (length(list.files(fixture$root, pattern = "^outcome-",
                             all.files = TRUE)) < 2L &&
             Sys.time() < deadline) {
      Sys.sleep(0.005)
    }
    outcomes <- vapply(
      list.files(fixture$root, pattern = "^outcome-", all.files = TRUE,
                 full.names = TRUE),
      function(path) readLines(path, n = 1L), character(1L)
    )
    writeLines("release", file.path(fixture$root, "release"), useBytes = TRUE)
    expect_length(outcomes, 2L)
    expect_identical(
      readLines(file.path(fixture$root, "outcome-1"), n = 1L),
      "acquired"
    )
    expect_identical(sum(outcomes == "acquired"), 1L,
                     info = paste(outcomes, collapse = " | "))
    bb_collect_children(children, timeout = 10)
  }
})

test_that("lock helper states are conservative and preserve unknown bytes", {
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(...) "alive",
    .package = "bigbang"
  )
  fixture <- round061_lock_fixture("bigbang-round061-helper-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  expect_null(.read_update_lock_owner(fixture$lock))
  dir.create(fixture$lock)
  dir.create(.update_lock_owner_path(fixture$lock))
  expect_null(.read_update_lock_owner(fixture$lock))
  unlink(.update_lock_owner_path(fixture$lock), recursive = TRUE)
  writeBin(charToRaw("corrupt"), .update_lock_owner_path(fixture$lock))
  expect_null(.read_update_lock_owner(fixture$lock))
  expect_null(.read_update_lock_claim(fixture$lock))
  unlink(fixture$lock, recursive = TRUE, force = TRUE)

  temp <- .update_lock_temporary_path(fixture$project)
  expect_true(dir.create(temp))
  .atomic_save_rds(.update_owner_record(), .update_lock_owner_path(temp))
  plan <- .reconcile_lock_temps(
    fixture$project, recover = TRUE, dry_run = TRUE
  )
  expect_identical(plan[[1L]]$action, "live_lock_preparation")
  expect_true(dir.exists(temp))
  .reconcile_lock_temps(fixture$project, recover = TRUE)
  expect_true(dir.exists(temp))
  unlink(temp, recursive = TRUE, force = TRUE)

  discarded <- tempfile("round061-discarded-", tmpdir = fixture$root)
  dir.create(discarded)
  writeLines("unverified", file.path(discarded, "extra"), useBytes = TRUE)
  .update_lock_inventory_discard(discarded, NULL, basename(fixture$project))
  apart <- list.files(fixture$root, pattern = "^round061-discarded-",
                      all.files = TRUE, full.names = TRUE)
  expect_length(apart, 0L)
  apart <- list.files(
    fixture$root, pattern = "^\\.project\\.bigbang-apartado-",
    all.files = TRUE, full.names = TRUE
  )
  expect_length(apart, 1L)
  expect_identical(readLines(file.path(apart, "extra")), "unverified")

  first <- .update_lock_discard_path(fixture$lock, "project")
  file.create(first)
  second <- .update_lock_discard_path(fixture$lock, "project")
  expect_false(identical(first, second))
  unlink(first, force = TRUE)

  state <- .update_lock_state(fixture$project)
  expect_identical(state$status, "free")
  expect_identical(.update_lock_temporary_paths(fixture$project), character())
})

test_that("the process classifier distinguishes dead, live, and token-conflict owners", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(dir.exists("/proc"), "the process classifier requires /proc")
  owner <- .update_owner_record()
  expect_identical(.update_owner_liveness(list()), "uncertain")
  other <- owner
  other$host <- "other-host"
  expect_identical(.update_owner_liveness(other), "uncertain")
  expect_identical(.update_owner_liveness(owner), "alive")
  owner$process_start <- "wrong-token"
  expect_identical(.update_owner_liveness(owner), "live_token_conflict")
  owner$pid <- 99999999L
  expect_identical(.update_owner_liveness(owner), "dead")
})

test_that("release only removes the lock owned by the same record", {
  fixture <- round061_lock_fixture("bigbang-round061-release-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  owner <- .update_owner_record()
  dir.create(fixture$lock)
  .atomic_save_rds(owner, .update_lock_owner_path(fixture$lock))
  wrong <- owner
  wrong$started_utc <- paste0(owner$started_utc, "-different")
  .release_update_lock(list(path = fixture$lock, owner = wrong))
  expect_true(dir.exists(fixture$lock))
  .release_update_lock(list(path = fixture$lock, owner = owner))
  expect_false(dir.exists(fixture$lock))
  .release_update_lock(NULL)
})

test_that("ambiguous lock entries remain blocked until recovery is explicit", {
  fixture <- round061_lock_fixture("bigbang-round061-ambiguous-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  owner <- .update_owner_record()
  owner$process_start <- NA_character_
  expect_identical(.update_owner_liveness(owner), "uncertain")
  expect_error(
    .update_lock_problem(fixture$project, fixture$lock, status = "uncertain"),
    class = "bigbang_error_update_in_progress"
  )

  dir.create(fixture$lock)
  .atomic_save_rds(owner, .update_lock_owner_path(fixture$lock))
  expect_identical(
    .update_lock_state(fixture$project)$action, "blocked_uncertain_lock"
  )
  expect_identical(
    .update_lock_state(fixture$project, recover = TRUE)$action,
    "claim_uncertain_lock"
  )
  unlink(fixture$lock, recursive = TRUE, force = TRUE)

  discarded <- file.path(
    fixture$root, ".project.bigbang-update.lock.descartado-uncertain"
  )
  dir.create(discarded)
  uncertain <- .update_owner_record()
  uncertain$host <- "other-host"
  .atomic_save_rds(uncertain, .update_lock_claim_path(discarded))
  expect_identical(
    .update_lock_state(fixture$project)$action, "blocked_uncertain_discard"
  )
  expect_identical(
    .update_lock_state(fixture$project, recover = TRUE)$action,
    "claim_uncertain_discard"
  )
  unlink(discarded, recursive = TRUE, force = TRUE)

  temporary <- .update_lock_temporary_path(fixture$project)
  dir.create(temporary)
  .atomic_save_rds(uncertain, .update_lock_owner_path(temporary))
  plan <- .reconcile_lock_temps(fixture$project, recover = FALSE)
  expect_identical(plan[[1L]]$action, "uncertain_lock_preparation")
  expect_true(dir.exists(temporary))
  .reconcile_lock_temps(fixture$project, recover = TRUE)
  expect_false(dir.exists(temporary))

  empty <- tempfile("round061-empty-lock-", tmpdir = fixture$root)
  dir.create(empty)
  .atomic_save_rds(owner, .update_lock_owner_path(empty))
  .atomic_save_rds(owner, .update_lock_claim_path(empty))
  expect_null(.update_lock_inventory_discard(
    empty, owner, basename(fixture$project)
  ))
  expect_false(dir.exists(empty))
})

test_that("a lock symlink is aparted without touching its target", {
  skip_on_os("windows")
  fixture <- round061_lock_fixture("bigbang-round061-symlink-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  target <- file.path(fixture$root, "target")
  dir.create(target)
  marker <- file.path(target, "marker")
  writeLines("keep", marker)
  before <- unname(tools::md5sum(marker))
  expect_true(file.symlink(target, fixture$lock))
  expect_identical(.update_lock_state(fixture$project)$action, "blocked_lock")
  expect_match(
    conditionMessage(expect_error(
      .acquire_update_lock(fixture$project, recover = FALSE),
      class = "bigbang_error_update_in_progress"
    )), "symbolic link", ignore.case = TRUE
  )
  acquired <- .acquire_update_lock(fixture$project, recover = TRUE)
  withr::defer(.release_update_lock(acquired))
  expect_false(.path_is_symlink(fixture$lock))
  expect_true(dir.exists(fixture$lock))
  expect_identical(unname(tools::md5sum(marker)), before)
})
