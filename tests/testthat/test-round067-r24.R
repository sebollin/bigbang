round067_make_archive <- function(root, archive_dir, name, version = "0.1.0",
                                  depends = character()) {
  package_dir <- file.path(root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE,
             showWarnings = FALSE)
  description <- c(
    paste0("Package: ", name), paste0("Version: ", version),
    paste0("Title: Round 067 fixture ", name),
    "Description: Temporary component for the round 067 tests.",
    "License: MIT", "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  )
  if (length(depends) > 0L) description <- c(
    description, paste0("Depends: ", paste(depends, collapse = ", "))
  )
  writeLines(description, file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(value)", file.path(package_dir, "NAMESPACE"), useBytes = TRUE)
  writeLines("value <- function() 1L", file.path(package_dir, "R", "value.R"),
             useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

round067_fixture <- function(prefix = "bigbang-round067-") {
  root <- tempfile(prefix)
  sources <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(sources, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  list(
    root = root, sources = sources, archives = archives,
    destination = destination
  )
}

round067_generate <- function(name, packages, fixture, ...) {
  create_metapackage(
    name, packages, pkg_dir = fixture$archives, dest_dir = fixture$destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), include_archives = TRUE, ...
  )
}

test_that("update adds, re-adds, and previews new generated files", {
  skip_on_cran()
  fixture <- round067_fixture()
  first <- round067_make_archive(fixture$sources, fixture$archives, "first")
  second <- round067_make_archive(fixture$sources, fixture$archives, "second")
  third <- round067_make_archive(fixture$sources, fixture$archives, "third")
  initial <- round067_generate("r24verse", first, fixture)

  added <- round067_generate("r24verse", c(first, second), fixture, update = TRUE)
  expect_true(file.exists(file.path(
    initial$path, "inst", "archives", "second_0.1.0.tar.gz"
  )))
  expect_true("inst/archives/second_0.1.0.tar.gz" %in% added$added_files)

  dry <- round067_generate(
    "r24verse", c(first, second, third), fixture, update = TRUE, dry_run = TRUE
  )
  expect_true("inst/archives/third_0.1.0.tar.gz" %in% dry$added_files)
  expect_false(file.exists(file.path(
    initial$path, "inst", "archives", "third_0.1.0.tar.gz"
  )))

  removed <- round067_generate("r24verse", first, fixture, update = TRUE)
  expect_true("inst/archives/second_0.1.0.tar.gz" %in% removed$removed_files)
  readded <- round067_generate("r24verse", c(first, second), fixture, update = TRUE)
  expect_true(file.exists(file.path(
    initial$path, "inst", "archives", "second_0.1.0.tar.gz"
  )))
  expect_true("inst/archives/second_0.1.0.tar.gz" %in% readded$added_files)
})

test_that("update refuses an existing user file at a planned new path", {
  fixture <- round067_fixture()
  first <- round067_make_archive(fixture$sources, fixture$archives, "first")
  second <- round067_make_archive(fixture$sources, fixture$archives, "second")
  initial <- round067_generate("r24userverse", first, fixture)
  user_path <- file.path(initial$path, "inst", "archives", "second_0.1.0.tar.gz")
  writeLines("user-owned bytes", user_path, useBytes = TRUE)
  before <- readBin(user_path, "raw", n = file.info(user_path)$size)
  expect_error(
    round067_generate("r24userverse", c(first, second), fixture, update = TRUE),
    class = "bigbang_error_untracked_generated_file"
  )
  expect_identical(readBin(user_path, "raw", n = file.info(user_path)$size), before)
})

test_that("workflow addition follows the new-file update rule", {
  skip_on_cran()
  fixture <- round067_fixture()
  first <- round067_make_archive(fixture$sources, fixture$archives, "first")
  initial <- round067_generate("r24workflowverse", first, fixture)
  updated <- round067_generate(
    "r24workflowverse", first, fixture, update = TRUE,
    workflow = c(Stage = "first")
  )
  expect_true(file.exists(file.path(
    initial$path, "vignettes", "workflow-r24workflowverse.Rmd"
  )))
  expect_true("vignettes/workflow-r24workflowverse.Rmd" %in% updated$added_files)
})

test_that("a version bump adds the new archive and removes the old one", {
  skip_on_cran()
  fixture <- round067_fixture()
  old <- round067_make_archive(fixture$sources, fixture$archives, "versioned", "0.1.0")
  new <- round067_make_archive(fixture$sources, fixture$archives, "versioned", "0.2.0")
  initial <- round067_generate("r24versionverse", old, fixture)
  updated <- round067_generate("r24versionverse", new, fixture, update = TRUE)
  expect_true("inst/archives/versioned_0.2.0.tar.gz" %in% updated$added_files)
  expect_true("inst/archives/versioned_0.1.0.tar.gz" %in% updated$removed_files)
  expect_false(file.exists(file.path(
    initial$path, "inst", "archives", "versioned_0.1.0.tar.gz"
  )))
})

test_that("attachment uses loaded namespaces and preflights Depends", {
  fixture <- round067_fixture()
  loaded <- round067_make_archive(fixture$sources, fixture$archives, "loaded")
  missing <- round067_make_archive(
    fixture$sources, fixture$archives, "sky", depends = "wind"
  )
  generated <- round067_generate(
    "r24attachverse", c(loaded, missing), fixture
  )
  pkgload::load_all(file.path(fixture$sources, "loaded"), attach = FALSE,
                    quiet = TRUE)
  attach_env <- new.env(parent = asNamespace("bigbang"))
  sys.source(file.path(generated$path, "R", "attach.R"), attach_env)
  attached <- attach_env$attach_installed_packages(
    "loaded", warn_missing = FALSE, lib.loc = character()
  )
  expect_true("loaded" %in% attached$attached)
  if ("package:loaded" %in% search()) {
    detach("package:loaded", character.only = TRUE, unload = FALSE)
  }
  if (isNamespaceLoaded("loaded")) unloadNamespace("loaded")

  fake_library <- file.path(fixture$root, "fake-library")
  fake_package <- file.path(fake_library, "sky")
  dir.create(fake_package, recursive = TRUE)
  writeLines(c(
    "Package: sky", "Version: 0.1.0", "Depends: wind",
    "Title: fake", "Description: fake", "License: MIT"
  ), file.path(fake_package, "DESCRIPTION"), useBytes = TRUE)
  preflight <- attach_env$attach_installed_packages(
    "sky", warn_missing = FALSE, lib.loc = fake_library
  )
  expect_identical(preflight$attached, character())
  expect_identical(preflight$missing, "sky")
})

test_that("archive filename mismatch is warned once per install call", {
  skip_on_cran()
  fixture <- round067_fixture()
  original <- round067_make_archive(
    fixture$sources, fixture$archives, "mismatch", "0.1.0"
  )
  renamed <- file.path(fixture$archives, "mismatch_0.2.0.tar.gz")
  expect_true(file.copy(original, renamed))
  generated <- suppressWarnings(round067_generate(
    "r24warnverse", "mismatch_0.2.0", fixture,
    tolerate = "filename_mismatch"
  ))
  install_library <- file.path(fixture$root, "library")
  dir.create(install_library)
  install_env <- new.env(parent = baseenv())
  for (file in list.files(file.path(generated$path, "R"), full.names = TRUE)) {
    sys.source(file, install_env)
  }
  warnings <- character()
  withCallingHandlers(
    tryCatch(
      install_env$r24warnverse_install(
        pkg_dir = fixture$archives, lib = install_library, verbose = FALSE
      ),
      error = function(e) e
    ),
    warning = function(e) {
      warnings <<- c(warnings, conditionMessage(e))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_match(warnings[[1L]], "declares version 0.1.0", fixed = TRUE)
})

test_that("SIGKILL during a new archive write rolls back to the old manifest", {
  skip_on_cran()
  skip_on_os("windows")
  fixture <- round067_fixture("bigbang-round067-kill-")
  first <- round067_make_archive(fixture$sources, fixture$archives, "first")
  second <- round067_make_archive(fixture$sources, fixture$archives, "second")
  initial <- round067_generate("r24killverse", first, fixture)
  mark <- file.path(fixture$root, "new-write-started")
  Sys.setenv(BB067_PROJECT = initial$path, BB067_MARK = mark)
  on.exit(Sys.unsetenv(c("BB067_PROJECT", "BB067_MARK")), add = TRUE)
  child <- bb_mcparallel({
    trace(".atomic_replace", where = asNamespace("bigbang"),
          tracer = quote({
            if (grepl("second_0\\.1\\.0\\.tar\\.gz$", destination) &&
                  !file.exists(Sys.getenv("BB067_MARK"))) {
              writeLines("started", Sys.getenv("BB067_MARK"), useBytes = TRUE)
              Sys.sleep(600)
            }
          }), print = FALSE)
    round067_generate("r24killverse", c(first, second), fixture, update = TRUE)
  }, silent = TRUE, mc.set.seed = FALSE)
  on.exit(bb_cleanup_child(child), add = TRUE)
  deadline <- Sys.time() + 30
  while (!file.exists(mark) && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(file.exists(mark))
  expect_true(tools::pskill(child$pid, tools::SIGKILL))
  expect_true(!is.null(bb_collect_child(child, timeout = 1)))
  new_archive <- file.path(initial$path, "inst", "archives", "second_0.1.0.tar.gz")
  expect_false(file.exists(new_archive))
  expect_true(file.exists(.update_journal_path(initial$path)))
  retry <- round067_generate(
    "r24killverse", c(first, second), fixture, update = TRUE, recover = TRUE
  )
  expect_true(retry$updated)
  expect_true(file.exists(new_archive))
  expect_false(dir.exists(.update_journal_path(initial$path)))
})
