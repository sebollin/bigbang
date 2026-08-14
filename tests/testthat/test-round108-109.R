round108_generate <- function(name, destination, document, ...) {
  create_metapackage(
    name,
    system.file(
      "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
    ),
    dest_dir = destination, document = document, verbose = FALSE,
    import_deps = character(), force_deps = character(), ...
  )
}

round108_expect_exact_manifest <- function(project) {
  manifest <- readRDS(file.path(project, .generation_manifest_name))
  actual <- list.files(
    project, recursive = TRUE, all.files = TRUE, no.. = TRUE,
    include.dirs = FALSE
  )
  expect_setequal(actual, c(manifest$files, .generation_manifest_name))
  expect_identical(
    unname(vapply(
      file.path(project, manifest$files), .file_digest, character(1L)
    )),
    unname(manifest$hashes[manifest$files])
  )
  invisible(manifest)
}

test_that("legacy migration adopts only archives in the current plan", {
  root <- tempfile("bigbang-legacy-archive-ownership-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  initial <- round108_generate(
    "legacyarchiveverse", destination, document = FALSE
  )
  shipped <- file.path(
    initial$path, "inst", "archives", "toycomponent_0.1.0.tar.gz"
  )
  user_archive <- file.path(
    initial$path, "inst", "archives", "mytool_0.9.tar.gz"
  )
  expect_true(file.copy(shipped, user_archive))

  current <- readRDS(file.path(initial$path, .generation_manifest_name))
  legacy <- .manifest_records(
    initial$path, c(current$files, "inst/archives/mytool_0.9.tar.gz")
  )
  legacy$schema <- 1L
  .atomic_save_rds(
    legacy, file.path(initial$path, .generation_manifest_name)
  )

  updated <- round108_generate(
    "legacyarchiveverse", destination, document = FALSE, update = TRUE
  )

  expect_true(file.exists(user_archive))
  expect_false("inst/archives/mytool_0.9.tar.gz" %in% updated$removed_files)
  migrated <- readRDS(file.path(initial$path, .generation_manifest_name))
  expect_identical(migrated$schema, 2L)
  expect_true("inst/archives/toycomponent_0.1.0.tar.gz" %in% migrated$files)
  expect_false("inst/archives/mytool_0.9.tar.gz" %in% migrated$files)

  retry <- round108_generate(
    "legacyarchiveverse", destination, document = FALSE, update = TRUE
  )
  expect_true(isTRUE(retry$updated))
  expect_true(file.exists(user_archive))
})

test_that("documentation can be disabled and enabled across updates", {
  skip_if_not_installed("devtools")
  root <- tempfile("bigbang-document-toggle-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  initial <- round108_generate(
    "doctoggleverse", destination, document = TRUE
  )
  documentation <- .planned_documentation_files("doctoggleverse")
  expect_true(all(file.exists(file.path(initial$path, documentation))))

  disabled <- round108_generate(
    "doctoggleverse", destination, document = FALSE, update = TRUE
  )
  expect_true(isTRUE(disabled$updated))
  expect_false(any(file.exists(file.path(initial$path, documentation))))
  round108_expect_exact_manifest(initial$path)

  failed <- NULL
  expect_warning(
    failed <- testthat::with_mocked_bindings(
      round108_generate(
        "doctoggleverse", destination, document = TRUE, update = TRUE
      ),
      document = function(pkg, ...) {
        partial <- file.path(pkg, documentation[[1L]])
        writeLines("partial documentation", partial)
        stop("forced documentation re-enable failure")
      },
      .package = "devtools"
    ),
    "Error generating documentation: forced documentation re-enable failure"
  )
  expect_true(isTRUE(failed$updated))
  expect_false(isTRUE(failed$documented))
  expect_false(any(file.exists(file.path(initial$path, documentation))))
  expect_true(documentation[[1L]] %in% failed$removed_files)
  round108_expect_exact_manifest(initial$path)

  enabled <- round108_generate(
    "doctoggleverse", destination, document = TRUE, update = TRUE
  )
  expect_true(isTRUE(enabled$updated))
  expect_true(isTRUE(enabled$documented))
  expect_true(all(file.exists(file.path(initial$path, documentation))))
  round108_expect_exact_manifest(initial$path)
})

test_that("documentation recovers after an initial generation failure", {
  skip_if_not_installed("devtools")
  root <- tempfile("bigbang-initial-document-failure-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  failed <- NULL
  expect_warning(
    failed <- testthat::with_mocked_bindings(
      round108_generate(
        "initialdocverse", destination, document = TRUE
      ),
      document = function(...) stop("forced initial documentation failure"),
      .package = "devtools"
    ),
    "Error generating documentation: forced initial documentation failure"
  )
  documentation <- .planned_documentation_files("initialdocverse")
  initial_manifest <- readRDS(file.path(
    failed$path, .generation_manifest_name
  ))
  expect_false(isTRUE(failed$documented))
  expect_false(any(file.exists(file.path(failed$path, documentation))))
  expect_false(any(documentation %in% initial_manifest$files))
  round108_expect_exact_manifest(failed$path)

  retry <- round108_generate(
    "initialdocverse", destination, document = TRUE, update = TRUE
  )
  retry_manifest <- round108_expect_exact_manifest(retry$path)
  expect_true(isTRUE(retry$updated))
  expect_true(isTRUE(retry$documented))
  expect_true(all(file.exists(file.path(retry$path, documentation))))
  expect_true(all(documentation %in% retry_manifest$files))
})

test_that("failed documentation keeps tracked Rd files and remains retryable", {
  skip_if_not_installed("devtools")
  root <- tempfile("bigbang-document-update-failure-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  initial <- round108_generate(
    "docretryverse", destination, document = TRUE
  )
  documentation <- .planned_documentation_files("docretryverse")
  documentation_paths <- file.path(initial$path, documentation)
  before <- unname(tools::md5sum(documentation_paths))

  failed <- NULL
  expect_warning(
    failed <- testthat::with_mocked_bindings(
      round108_generate(
        "docretryverse", destination, document = TRUE, update = TRUE
      ),
      document = function(pkg, ...) {
        writeLines("partial documentation", documentation_paths[[1L]])
        stop("forced documentation update failure")
      },
      .package = "devtools"
    ),
    "Error generating documentation: forced documentation update failure"
  )

  expect_true(isTRUE(failed$updated))
  expect_false(isTRUE(failed$documented))
  expect_identical(unname(tools::md5sum(documentation_paths)), before)
  manifest <- round108_expect_exact_manifest(initial$path)
  expect_true(all(documentation %in% manifest$files))

  retry <- round108_generate(
    "docretryverse", destination, document = TRUE, update = TRUE
  )
  expect_true(isTRUE(retry$updated))
  expect_true(isTRUE(retry$documented))
  round108_expect_exact_manifest(initial$path)
})

test_that("failed documentation preserves an untracked Rd file", {
  skip_if_not_installed("devtools")
  root <- tempfile("bigbang-untracked-documentation-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  initial <- round108_generate(
    "userdocverse", destination, document = FALSE
  )
  relative <- file.path("man", "userdocverse_install.Rd")
  user_file <- file.path(initial$path, relative)
  writeLines("user documentation", user_file, useBytes = TRUE)
  before <- .file_digest(user_file)

  failed <- NULL
  expect_warning(
    failed <- testthat::with_mocked_bindings(
      round108_generate(
        "userdocverse", destination, document = TRUE, update = TRUE
      ),
      document = function(...) stop("forced documentation failure"),
      .package = "devtools"
    ),
    "Error generating documentation: forced documentation failure"
  )

  expect_true(isTRUE(failed$updated))
  expect_false(isTRUE(failed$documented))
  expect_true(file.exists(user_file))
  expect_identical(.file_digest(user_file), before)
  expect_false(relative %in% failed$removed_files)
  manifest <- readRDS(file.path(initial$path, .generation_manifest_name))
  expect_false(relative %in% manifest$files)
})

test_that("failed documentation restores a partially overwritten user Rd", {
  skip_if_not_installed("devtools")
  root <- tempfile("bigbang-overwritten-documentation-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  initial <- round108_generate(
    "restoreuserdocverse", destination, document = FALSE
  )
  relative <- file.path("man", "restoreuserdocverse_install.Rd")
  user_file <- file.path(initial$path, relative)
  writeLines("original user documentation", user_file, useBytes = TRUE)
  before <- .file_digest(user_file)

  failed <- NULL
  expect_warning(
    failed <- testthat::with_mocked_bindings(
      round108_generate(
        "restoreuserdocverse", destination,
        document = TRUE, update = TRUE
      ),
      document = function(...) {
        writeLines("partial generated documentation", user_file, useBytes = TRUE)
        stop("forced failure after overwrite")
      },
      .package = "devtools"
    ),
    "Error generating documentation: forced failure after overwrite"
  )

  expect_true(isTRUE(failed$updated))
  expect_false(isTRUE(failed$documented))
  expect_true(file.exists(user_file))
  expect_identical(.file_digest(user_file), before)
  expect_false(relative %in% failed$removed_files)
  manifest <- readRDS(file.path(initial$path, .generation_manifest_name))
  expect_false(relative %in% manifest$files)
})
