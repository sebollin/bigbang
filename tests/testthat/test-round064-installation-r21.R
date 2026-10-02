round064_make_archive <- function(source_root, archive_dir, name,
                                  exports = "value", body = "value <- 1L",
                                  imports = character(),
                                  namespace_extra = character()) {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name), "Type: Package", "Version: 0.1.0",
    paste0("Title: Round 064 fixture ", name),
    paste0("Description: Temporary fixture for ", name, "."),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    "License: MIT",
    if (length(imports) > 0L) paste0("Imports: ", paste(imports, collapse = ", "))
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(
    c(
      paste0("export(", paste(exports, collapse = ","), ")"),
      namespace_extra
    ),
    file.path(package_dir, "NAMESPACE"), useBytes = TRUE
  )
  writeLines(body, file.path(package_dir, "R", "fixture.R"), useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_0.1.0.tar.gz"))
  withr::with_dir(source_root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

round064_fixture <- function(prefix) {
  root <- tempfile(prefix)
  source_root <- file.path(root, "sources")
  archive_dir <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  archive <- round064_make_archive(
    source_root, archive_dir, "round064component"
  )
  generated <- create_metapackage(
    "round064verse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    include_archives = TRUE, workflow = c(Stage = "round064component")
  )
  list(
    root = root, destination = destination, project = generated$path,
    name = "round064verse", archive = archive
  )
}

round064_install <- function(archive, library) {
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  output <- system2(
    r_binary, c("CMD", "INSTALL", "-l", shQuote(library), shQuote(archive)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  testthat::expect_identical(status, 0L, info = paste(output, collapse = "\n"))
}

test_that("discard revalidates links after the private journal rename", {
  skip_on_cran()
  skip_on_os("windows")
  for (attempt in seq_len(5L)) {
    fixture <- round064_fixture(paste0("bigbang-round064-race-", attempt, "-"))
    try(untrace(".file_digest", where = asNamespace("bigbang")), silent = TRUE)
    journal <- .create_update_journal(
      fixture$project, fixture$name,
      .read_generation_manifest(fixture$project)
    )
    external <- file.path(fixture$root, "external")
    dir.create(external)
    dir.create(file.path(journal$path, "backup", "sub"), recursive = TRUE)
    values <- sprintf("backup/sub/v%02d.bin", 1:3)
    for (value in values) {
      writeBin(charToRaw(paste(rep("x", 1024L), collapse = "")),
               file.path(journal$path, value))
      file.copy(file.path(journal$path, value),
                file.path(external, basename(value)))
    }
    .update_journal_tombstone(journal$path, fixture$name, journal$marker)
    discarded <- file.path(
      fixture$destination,
      paste0(".", fixture$name, ".bigbang-update.descartado-r21")
    )
    expect_true(file.rename(journal$path, discarded))
    external_before <- sort(list.files(external, full.names = FALSE))
    mark <- file.path(fixture$root, "race-mark")
    Sys.setenv(BB064_MARK = mark)
    withr::defer(Sys.unsetenv("BB064_MARK"))
    ext_abs <- normalizePath(external, winslash = "/", mustWork = TRUE)
    sub <- file.path(discarded, "backup", "sub")
    sub_real <- file.path(discarded, "backup", "sub.real")
    child <- bb_mcparallel({
      deadline <- Sys.time() + 20
      while (!file.exists(Sys.getenv("BB064_MARK")) &&
               Sys.time() < deadline) Sys.sleep(0.002)
      if (file.exists(Sys.getenv("BB064_MARK"))) {
        repeat {
          moved <- suppressWarnings(file.rename(sub, sub_real))
          if (isTRUE(moved) || !dir.exists(sub)) break
          Sys.sleep(0.001)
        }
        if (!file.exists(sub) && dir.exists(sub_real)) file.symlink(ext_abs, sub)
      }
      invisible(NULL)
    }, silent = TRUE, mc.set.seed = FALSE)
    on.exit(bb_cleanup_child(child), add = TRUE)
    trace(
      .file_digest, where = asNamespace("bigbang"),
      tracer = quote({
        if (length(path) == 1L && grepl("/backup/", path, fixed = TRUE) &&
              !file.exists(Sys.getenv("BB064_MARK"))) {
          writeLines("go", Sys.getenv("BB064_MARK"), useBytes = TRUE)
        }
      }),
      print = FALSE
    )
    finish_result <- tryCatch(
      .finish_discarded_journal(discarded, fixture$name),
      error = identity
    )
    untrace(".file_digest", where = asNamespace("bigbang"))
    Sys.unsetenv("BB064_MARK")
    expect_false(inherits(finish_result, "error"),
                 info = if (inherits(finish_result, "error")) {
                   conditionMessage(finish_result)
                 } else {
                   ""
                 })
    bb_collect_child(child, timeout = 2)
    expect_identical(sort(list.files(external, full.names = FALSE)), external_before)
  }
})

test_that("unreadable discarded folders are set aside and never treated as empty", {
  fixture <- round064_fixture("bigbang-round064-permissions-")
  # Entries set aside keep their 0000 mode under a new name; restore every
  # directory under the fixture so tempdir() can be removed afterwards.
  withr::defer({
    for (pass in seq_len(20L)) {
      dirs <- list.dirs(fixture$destination, recursive = TRUE)
      locked <- dirs[file.access(dirs, 4L) != 0L]
      if (length(locked) == 0L) break
      Sys.chmod(locked, "0755")
    }
  })
  try(untrace(".file_digest", where = asNamespace("bigbang")), silent = TRUE)
  journal <- .create_update_journal(
    fixture$project, fixture$name,
    .read_generation_manifest(fixture$project)
  )
  locked <- file.path(journal$path, "backup", "locked")
  dir.create(locked, recursive = TRUE)
  writeLines("protected", file.path(locked, "value.txt"), useBytes = TRUE)
  .update_journal_tombstone(journal$path, fixture$name, journal$marker)
  discarded <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.descartado-permissions")
  )
  expect_true(file.rename(journal$path, discarded))
  locked <- file.path(discarded, "backup", "locked")
  Sys.chmod(locked, "0000")
  withr::defer(Sys.chmod(locked, "0755"))
  if (file.access(locked, 4L) == 0L) {
    Sys.chmod(locked, "0755")
    skip("The current user can list chmod 0000 directories.")
  }
  entries_before <- .update_journal_entries(discarded)
  expect_true(.journal_entries_unreadable(entries_before))
  result <- tryCatch(
    .finish_discarded_journal(discarded, fixture$name),
    error = identity
  )
  expect_false(inherits(result, "error"),
               info = if (inherits(result, "error")) {
                 conditionMessage(result)
               } else {
                 ""
               })
  expect_true(is.list(result))
  apart <- result$apart
  Sys.chmod(file.path(apart, "backup", "locked"), "0755")
  withr::defer(Sys.chmod(file.path(apart, "backup", "locked"), "0755"))
  expect_true(file.exists(file.path(apart, "backup", "locked", "value.txt")))

  # Some platforms (macOS) refuse to rename a directory whose own mode is
  # 0000; there the entry cannot be set aside and the reconciliation reports
  # it instead. Probe that capability before expecting a set-aside.
  probe <- file.path(fixture$root, "rename-probe")
  dir.create(probe)
  Sys.chmod(probe, "0000")
  renamed <- suppressWarnings(file.rename(probe, paste0(probe, "-moved")))
  Sys.chmod(c(probe, paste0(probe, "-moved"))[c(!renamed, renamed)], "0755")
  if (!isTRUE(renamed)) {
    skip("This platform cannot rename a directory with mode 0000.")
  }
  empty <- file.path(
    fixture$destination,
    paste0(".", fixture$name, ".bigbang-update.descartado-empty-permissions")
  )
  dir.create(empty)
  Sys.chmod(empty, "0000")
  withr::defer(Sys.chmod(empty, "0755"))
  if (file.access(empty, 4L) == 0L) {
    Sys.chmod(empty, "0755")
    skip("The current user can list chmod 0000 directories.")
  }
  plan <- expect_no_error(.reconcile_update_siblings(
    fixture$project, fixture$name, recover = TRUE
  ))
  Sys.chmod(empty, "0755")
  expect_true(any(vapply(plan, function(item) !is.null(item$apart_path),
                         logical(1L))))
})

test_that("round 064 journal guards cover failed and private transitions", {
  root <- tempfile("bigbang-round064-journal-guards-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  expect_false(.discard_update_entry(
    file.path(root, "missing"), root = root, relative = "missing",
    expected_digest = paste(rep("0", 32L), collapse = "")
  ))
  link_root <- file.path(root, "link-root")
  target <- file.path(root, "target")
  dir.create(target)
  if (!bb_dir_link(target, link_root)) {
    skip("directory links are unavailable")
  }
  expect_false(.discard_update_entry(
    file.path(link_root, "file"), root = link_root, relative = "file"
  ))
  expect_error(
    .rename_discarded_private(file.path(root, "missing"), "round064"),
    "Could not set aside"
  )
  private <- file.path(root, "journal")
  dir.create(private)
  renamed <- .rename_discarded_private(private, "round064")
  expect_true(dir.exists(renamed))
  expect_match(basename(renamed), "descartado-en-proceso", fixed = TRUE)
  expect_message(
    apart <- .apart_update_journal(
      renamed, "round064", deleted_count = 1L
    ),
    "only verified inventory files were deleted"
  )
  entries <- structure(character(), bigbang_unreadable = TRUE)
  tombstone <- list(
    name = "round064", manifest_hash = "", inventory = character(),
    inventory_directories = character()
  )
  dir.create(file.path(root, "unreadable"))
  testthat::local_mocked_bindings(
    .update_journal_entries = function(path) entries,
    .package = "bigbang"
  )
  expect_message(
    result <- .finish_discarded_journal(
      file.path(root, "unreadable"), "round064", tombstone
    ),
    "no bytes from the set-aside entry were deleted"
  )
  expect_true(dir.exists(result$apart))
})

test_that("journal deletion recognizes portable directory links", {
  root <- tempfile("bigbang-round064-reparse-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  target <- file.path(root, "target")
  dir.create(target)
  payload <- file.path(target, "payload")
  writeLines("keep", payload, useBytes = TRUE)
  link <- file.path(root, "link")
  made <- if (identical(.Platform$OS.type, "windows")) {
    skip_if_not(exists("Sys.junction", mode = "function"),
                "Sys.junction is unavailable")
    isTRUE(suppressWarnings(Sys.junction(target, link)))
  } else {
    isTRUE(suppressWarnings(file.symlink(target, link)))
  }
  if (!made) skip("the platform cannot create the directory link")
  withr::defer(unlink(link, recursive = TRUE, force = TRUE))
  expect_true(.path_is_symlink(link))
  expect_false(.discard_update_entry(
    file.path(link, "payload"), root = link, relative = "payload"
  ))
  expect_identical(readLines(payload, warn = FALSE), "keep")
})

test_that("recover sets aside an unrecognized armed journal owned by a dead process", {
  # Only a control character makes an entry name unverifiable here, and
  # Windows file names cannot contain one: the state cannot occur there.
  skip_on_os("windows")
  fixture <- round064_fixture("bigbang-round064-armed-")
  try(untrace(".file_digest", where = asNamespace("bigbang")), silent = TRUE)
  journal <- .create_update_journal(
    fixture$project, fixture$name,
    .read_generation_manifest(fixture$project)
  )
  bad_name <- "bad\nname"
  writeLines("newline-name", file.path(journal$path, bad_name),
             useBytes = TRUE)
  dead_owner <- list(
    pid = 4194303L, host = .update_host(), started_utc = "now",
    process_start = "0"
  )
  marker <- readRDS(file.path(journal$path, "marker.rds"))
  state <- readRDS(file.path(journal$path, "state.rds"))
  marker[names(dead_owner)] <- dead_owner
  state[names(dead_owner)] <- dead_owner
  saveRDS(marker, file.path(journal$path, "marker.rds"))
  saveRDS(state, file.path(journal$path, "state.rds"))
  result <- expect_no_error(.recover_pending_update(
    fixture$project, fixture$name, recover = TRUE
  ))
  expect_identical(result$action, "apart_unrecognized_armed")
  expect_true(file.exists(file.path(result$apart, bad_name)))
})

test_that("the verification accessor rejects attributes lost by subsetting", {
  root <- tempfile("bigbang-round064-accessor-")
  dir.create(root)
  bigbang:::write_metapackage_files(
    "round064", character(), character(), dest_dir = root,
    overwrite = TRUE, reexport = TRUE
  )
  generated <- new.env(parent = baseenv())
  generated$.meta_tr <- identity
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  sys.source(file.path(root, "reexports.R"), generated)
  sys.source(file.path(root, "attach.R"), generated)
  conflicts <- structure(
    list(reexport_verification = c("package:left", "package:right")),
    class = c("round064_conflicts", "list"),
    reexport_verification = structure(
      data.frame(identical = NA),
      class = c("round064_reexport_verification", "data.frame")
    )
  )
  accessor <- generated$round064_reexport_verification
  sliced <- conflicts["reexport_verification"]
  combined <- c(conflicts, list(other = character()))
  expect_null(accessor(sliced))
  expect_null(accessor(combined))
  expect_match(
    paste(capture.output(generated$print.round064_conflicts(sliced)),
          collapse = "\n"),
    "verification is not available",
    ignore.case = TRUE
  )
})

test_that("verification preparation failures return unverified rows", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round064-verification-tempdir-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)
  parent <- round064_make_archive(
    source_root, archive_dir, "tempverify064a", "s",
    body = "s <- function() 'ok'"
  )
  child <- round064_make_archive(
    source_root, archive_dir, "tempverify064b", "s",
    body = "# imported only", imports = "tempverify064a",
    namespace_extra = "importFrom(tempverify064a, s)"
  )
  generated <- create_metapackage(
    "tempverse064", c(parent, child), dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE, reexport_prefer = c(s = "tempverify064a")
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  expect_identical(
    system2(r_binary, c("CMD", "INSTALL", "-l", shQuote(meta_library),
                        shQuote(generated$path)), stdout = FALSE, stderr = FALSE),
    0L
  )
  round064_install(parent, component_library)
  round064_install(child, component_library)
  withr::local_libpaths(c(meta_library, component_library, .libPaths()))
  loadNamespace("tempverse064")
  loadNamespace("tempverify064a")
  loadNamespace("tempverify064b")
  messages <- character()
  withCallingHandlers(
    getExportedValue("tempverse064", "tempverse064_conflicts")(),
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("another library", messages, ignore.case = TRUE)))
  temp_root <- tempdir()
  old_mode <- file.info(temp_root)$mode
  withr::defer(Sys.chmod(temp_root, old_mode))
  Sys.chmod(temp_root, "0555")
  if (file.access(temp_root, 2L) == 0L) {
    Sys.chmod(temp_root, old_mode)
    skip("The current user can write the read-only tempdir.")
  }
  conflicts <- withCallingHandlers(
    getExportedValue("tempverse064", "tempverse064_conflicts")(),
    warning = function(condition) invokeRestart("muffleWarning")
  )
  Sys.chmod(temp_root, old_mode)
  expect_true(is.list(conflicts))
  verification <- getExportedValue(
    "tempverse064", "tempverse064_reexport_verification"
  )(conflicts)
  expect_true(is.na(verification$identical[[1L]]))
})
