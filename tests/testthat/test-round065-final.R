round065_lock_fixture <- function(prefix = "bigbang-round065-lock-") {
  root <- tempfile(prefix)
  dir.create(root, recursive = TRUE)
  project <- file.path(root, "project")
  dir.create(project)
  list(root = root, project = project, lock = .update_lock_path(project))
}

test_that("R symbol literals round-trip through namespace and suggestions", {
  symbols <- c("a`b", "a\\b", "space name", ".reexport_verify")
  for (symbol in symbols) {
    literal <- .r_symbol_literal(symbol)
    parsed <- parse(text = paste0("x <- ", literal))
    value <- parsed[[1L]][[3L]]
    value <- if (is.name(value)) as.character(value) else eval(value)
    expect_identical(value, symbol)
    expect_true(grepl(literal, .namespace_export_directive(symbol), fixed = TRUE))
  }
  suggestion <- .reexport_prefer_literal("a`b", "component")
  expect_identical(eval(parse(text = paste0("c(", suggestion, ")"))[[1L]]),
                   c("a`b" = "component"))
  backslash_suggestion <- .reexport_prefer_literal("a\\b", "component")
  expect_identical(
    eval(parse(text = paste0("c(", backslash_suggestion, ")"))[[1L]]),
    c("a\\b" = "component")
  )
})

test_that("an unreadable published lock is set aside without a retry hang", {
  fixture <- round065_lock_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  dir.create(fixture$lock)
  writeBin(charToRaw("corrupt"), .update_lock_owner_path(fixture$lock))
  writeLines("preserve", file.path(fixture$lock, "extra"), useBytes = TRUE)
  started <- Sys.time()
  acquired <- .acquire_update_lock(fixture$project, recover = TRUE)
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 5)
  expect_true(acquired$acquired)
  apart <- list.files(
    fixture$root, pattern = "^\\.project\\.bigbang-apartado-",
    full.names = TRUE, all.files = TRUE
  )
  expect_length(apart, 1L)
  expect_identical(readLines(file.path(apart, "extra")), "preserve")
  .release_update_lock(acquired)
})

test_that("unreadable discarded lock inventories are preserved", {
  fixture <- round065_lock_fixture("bigbang-round065-discard-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  for (with_extra in c(TRUE, FALSE)) {
    discarded <- file.path(
      fixture$root,
      paste0(".project.bigbang-update.lock.descartado-", with_extra)
    )
    dir.create(discarded)
    if (with_extra) {
      writeLines("preserve", file.path(discarded, "extra"), useBytes = TRUE)
    }
    testthat::local_mocked_bindings(
      .update_journal_entries = function(...) {
        structure(if (with_extra) "extra" else character(),
                  bigbang_unreadable = TRUE)
      },
      .package = "bigbang"
    )
    .update_lock_inventory_discard(discarded, NULL, "project")
    apart <- list.files(
      fixture$root, pattern = "^\\.project\\.bigbang-apartado-",
      full.names = TRUE, all.files = TRUE
    )
    expect_length(apart, 1L)
    if (with_extra) expect_identical(readLines(file.path(apart, "extra")),
                                     "preserve")
    unlink(apart, recursive = TRUE, force = TRUE)
  }
})

test_that("a live lock preparation blocks acquisition and dry-run", {
  fixture <- round065_lock_fixture("bigbang-round065-preparation-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  preparation <- .update_lock_temporary_path(fixture$project)
  dir.create(preparation)
  owner <- .update_owner_record()
  .atomic_save_rds(owner, .update_lock_owner_path(preparation))
  testthat::local_mocked_bindings(
    .update_owner_liveness = function(...) "alive",
    .package = "bigbang"
  )
  expect_message(
    dry <- .acquire_update_lock(fixture$project, dry_run = TRUE),
    "live lock preparation"
  )
  expect_identical(dry$action, "blocked_live_temporary")
  expect_error(
    .acquire_update_lock(fixture$project),
    class = "bigbang_error_update_in_progress",
    regexp = paste0("pid ", owner$pid)
  )
  expect_true(dir.exists(preparation))
})

test_that("dry-run and acquisition share the lock-creation diagnosis", {
  fixture <- round065_lock_fixture("bigbang-round065-read-only-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  testthat::local_mocked_bindings(
    .update_lock_parent_writable = function(...) FALSE,
    .update_lock_temporary_path = function(project_dir) {
      file.path(dirname(project_dir), "missing-parent", "armando")
    },
    .package = "bigbang"
  )
  expect_message(
    dry <- .acquire_update_lock(fixture$project, dry_run = TRUE),
    "lock parent is not writable"
  )
  expect_identical(dry$action, "lock_creation")
  expect_error(
    .acquire_update_lock(fixture$project),
    class = "bigbang_error_update_in_progress",
    regexp = "writable"
  )
})

test_that("a retry loop reports a no-progress lock state", {
  fixture <- round065_lock_fixture("bigbang-round065-progress-")
  withr::defer(unlink(fixture$root, recursive = TRUE, force = TRUE))
  testthat::local_mocked_bindings(
    .update_lock_state = function(...) {
      list(status = "free", path = fixture$lock, owner = NULL, action = "free")
    },
    .update_lock_make_temporary = function(...) NULL,
    .package = "bigbang"
  )
  expect_error(
    .acquire_update_lock(fixture$project),
    class = "bigbang_error_update_in_progress",
    regexp = "made no progress"
  )
})

test_that("the ps fallback stays quiet in every language", {
  # Without /proc a missing pid makes ps exit with status 1. The warning text
  # of system2() is translated, so it cannot be filtered by its wording.
  skip_on_os("windows")
  ps <- Sys.which("ps")[[1L]]
  skip_if_not(nzchar(ps), "ps is not available")
  testthat::local_mocked_bindings(
    .update_is_windows = function() FALSE,
    .update_process_stat = function(pid) NULL,
    .update_signal_probe = function(pid) FALSE,
    .package = "bigbang"
  )
  missing_pid <- 2147483646L
  owner <- list(pid = missing_pid, host = .update_host(),
                process_start = "1")
  for (language in c("en", "es", "fr")) {
    withr::with_envvar(c(LANGUAGE = language), {
      expect_no_warning(status <- .update_owner_liveness(owner))
      expect_identical(status, "dead")
    })
  }
})

test_that("the ps start token is stable outside /proc and under foreign locales", {
  skip_on_os("windows")
  ps <- Sys.which("ps")[[1L]]
  skip_if_not(nzchar(ps), "ps is not available")
  testthat::local_mocked_bindings(
    .update_is_windows = function() FALSE,
    .update_process_stat = function(pid) NULL,
    .update_signal_probe = function(pid) FALSE,
    .package = "bigbang"
  )
  withr::with_envvar(
    c(LANGUAGE = "es", LC_TIME = "fr_FR.UTF-8", LC_ALL = ""),
    {
      owner <- .update_owner_record()
      expect_identical(owner$process_start_source, "ps")
      expect_true(is.character(owner$process_start))
      expect_length(owner$process_start, 1L)
      expect_true(nzchar(owner$process_start))
      expect_identical(.update_owner_liveness(owner), "alive")
    }
  )
})

test_that("Windows link detection looks only at the last path component", {
  # GitHub's Windows tempdir is spelled with an 8.3 short name
  # (C:/Users/RUNNER~1/...) that normalizePath() expands; neither that nor a
  # linked ancestor may turn every path into a "link".
  links <- c(
    "C:/Users/runneradmin/Temp/x/junction" = "D:/elsewhere/target",
    "C:/Users/runneradmin/Temp/linked" = "E:/redirected"
  )
  normalize <- function(x) {
    x <- sub("RUNNER~1", "runneradmin", gsub("\\\\", "/", x), fixed = TRUE)
    for (link in names(links)) {
      if (startsWith(x, link)) {
        x <- paste0(links[[link]], substring(x, nchar(link) + 1L))
      }
    }
    x
  }
  missing <- "C:/Users/RUNNER~1/Temp/x/not-yet"
  detect <- function(path) {
    .path_is_windows_reparse_point(
      path, is_windows = TRUE, normalize = normalize,
      exists = function(x) !identical(x, missing)
    )
  }
  root <- "C:/Users/RUNNER~1/Temp"
  # A path that does not exist keeps its short spelling in normalizePath().
  expect_false(detect(missing))
  expect_identical(
    detect(c(file.path(root, "x", "file"), file.path(root, "linked"))),
    c(FALSE, TRUE)
  )
  expect_false(detect(file.path(root, "x", "file")))
  expect_false(detect(file.path(root, "x", "FILE")))
  expect_false(detect(file.path(root, "x", "file/")))
  expect_true(detect(file.path(root, "x", "junction")))
  expect_true(detect(file.path(root, "linked")))
  expect_false(detect(file.path(root, "linked", "inner")))
  expect_false(.path_is_windows_reparse_point(
    file.path(root, "linked"), is_windows = FALSE, normalize = normalize
  ))
})

test_that("writes through an aliased project path are still recorded", {
  # macOS spells tempdir() through /var, a link to /private/var: a new file
  # does not exist yet, so only its parent gets resolved.
  skip_on_os("windows")
  root <- withr::local_tempdir("bigbang-alias-")
  real <- file.path(root, "real")
  dir.create(file.path(real, "project"), recursive = TRUE)
  alias <- file.path(root, "alias")
  skip_if_not(isTRUE(file.symlink(real, alias)), "symbolic links unavailable")
  project <- file.path(alias, "project")
  expect_identical(
    .project_relative_destination(file.path(project, "new-file"), project),
    "new-file"
  )
  expect_identical(
    .project_relative_destination(
      file.path(project, "R", "new.R"), file.path(real, "project")
    ),
    "R/new.R"
  )
  expect_null(.project_relative_destination(file.path(root, "outside"), project))
  expect_identical(.path_is_symlink(c(alias, project)), c(TRUE, FALSE))
})
