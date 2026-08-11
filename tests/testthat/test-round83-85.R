round83_make_archive <- function(source_root, archive_dir, name,
                                 version = "1.0.0", imports = NULL) {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  fields <- c(
    paste0("Package: ", name),
    paste0("Version: ", version),
    paste0("Title: Round 83 fixture ", name),
    paste0("Description: Temporary fixture for round 83 tests."),
    "License: MIT",
    "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  )
  if (!is.null(imports)) fields <- c(fields, paste0("Imports: ", imports))
  writeLines(fields, file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(fixture_value)", file.path(package_dir, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("fixture_value <- function() 1L",
             file.path(package_dir, "R", "value.R"), useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(source_root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

round83_generate <- function(name, archive_dir, destination) {
  create_metapackage(
    name, "toycomponent_0.1.0", pkg_dir = archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
}

test_that("update rejects a symlink in a generated manifest file", {
  skip_on_os("windows")
  root <- tempfile("bigbang-round83-file-link-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  file.copy(toy, archives)
  initial <- round83_generate("filelinkverse", archives, destination)
  sentinel <- file.path(root, "outside.txt")
  writeLines("outside", sentinel)
  before <- unname(tools::md5sum(sentinel))
  description <- file.path(initial$path, "DESCRIPTION")
  unlink(description)
  expect_true(file.symlink(sentinel, description))
  expect_error(
    round83_generate("filelinkverse", archives, destination),
    class = "bigbang_error_nonempty_dest"
  )
  expect_error(
    create_metapackage(
      "filelinkverse", "toycomponent_0.1.0", pkg_dir = archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character(), update = TRUE
    ),
    class = "bigbang_error_symlink_generated_path"
  )
  expect_identical(unname(tools::md5sum(sentinel)), before)
})

test_that("update rejects a symlink in a generated path component", {
  skip_on_os("windows")
  root <- tempfile("bigbang-round83-dir-link-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  file.copy(toy, archives)
  initial <- round83_generate("dirlinkverse", archives, destination)
  outside <- file.path(root, "outside")
  dir.create(outside)
  sentinel <- file.path(outside, "attach.R")
  writeLines("outside", sentinel)
  unlink(file.path(initial$path, "R"), recursive = TRUE, force = TRUE)
  expect_true(file.symlink(outside, file.path(initial$path, "R")))
  before <- unname(tools::md5sum(sentinel))
  expect_error(
    create_metapackage(
      "dirlinkverse", "toycomponent_0.1.0", pkg_dir = archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character(), update = TRUE
    ),
    class = "bigbang_error_symlink_generated_path"
  )
  expect_identical(unname(tools::md5sum(sentinel)), before)
})

test_that("a symlink in the destination parent remains a valid path", {
  skip_on_os("windows")
  root <- tempfile("bigbang-round83-parent-link-")
  real_parent <- file.path(root, "real")
  linked_parent <- file.path(root, "linked")
  archives <- file.path(root, "archives")
  dir.create(real_parent, recursive = TRUE)
  dir.create(archives)
  expect_true(file.symlink(real_parent, linked_parent))
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  file.copy(toy, archives)
  destination <- file.path(linked_parent, "destination")
  initial <- round83_generate("parentlinkverse", archives, destination)
  updated <- create_metapackage(
    "parentlinkverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), update = TRUE,
    version = "0.9.9"
  )
  expect_true(dir.exists(initial$path))
  expect_true(isTRUE(updated$updated))
  expect_identical(
    unname(read.dcf(file.path(updated$path, "DESCRIPTION"))[1L, "Version"]),
    "0.9.9"
  )
})

test_that("manifests accept absolute and tilde archive paths", {
  root <- tempfile("bigbang-round84-absolute-")
  manifest_dir <- file.path(root, "repository")
  archive_dir <- file.path(root, "archives")
  source_root <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  dir.create(manifest_dir, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(source_root)
  dir.create(destination)
  withr::local_envvar(c(HOME = root, R_USER = root, USERPROFILE = root))
  expected_home <- normalizePath(root, winslash = "/", mustWork = TRUE)
  actual_home <- normalizePath(path.expand("~"), winslash = "/", mustWork = TRUE)
  if (!identical(actual_home, expected_home)) {
    skip("This platform does not honor a sandboxed HOME/R_USER for '~'.")
  }
  first <- round83_make_archive(source_root, archive_dir, "absoluteone")
  second <- round83_make_archive(source_root, archive_dir, "absolutetwo")
  manifest <- file.path(manifest_dir, "components.txt")
  writeLines(c(
    normalizePath(first, winslash = "/"),
    paste0("~/archives/", basename(second))
  ), manifest, useBytes = TRUE)
  result <- create_metapackage(
    "absoluteverse", manifest, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  expect_setequal(result$packages, c("absoluteone", "absolutetwo"))
})

test_that("manifests accept an absolute source directory", {
  skip_if_not_installed("pkgbuild")
  root <- tempfile("bigbang-round84-source-")
  source_root <- file.path(root, "source")
  manifest_dir <- file.path(root, "repository")
  destination <- file.path(root, "destination")
  dir.create(manifest_dir, recursive = TRUE)
  dir.create(destination)
  source_dir <- file.path(source_root, "sourcecomponent")
  dir.create(file.path(source_dir, "R"), recursive = TRUE)
  writeLines(c(
    "Package: sourcecomponent", "Version: 1.0.0",
    "Title: Absolute source fixture", "Description: Source fixture.",
    "License: MIT", "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(source_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(fixture_value)", file.path(source_dir, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("fixture_value <- function() 1L",
             file.path(source_dir, "R", "value.R"), useBytes = TRUE)
  manifest <- file.path(manifest_dir, "components.txt")
  writeLines(normalizePath(source_dir, winslash = "/"), manifest,
             useBytes = TRUE)
  result <- create_metapackage(
    "absolutesourceverse", manifest, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  expect_identical(result$packages, "sourcecomponent")
})

test_that("skip propagation uses declared identity when it is readable", {
  root <- tempfile("bigbang-round85-identity-")
  archive_dir <- file.path(root, "archives")
  source_root <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  dir.create(archive_dir, recursive = TRUE)
  dir.create(source_root)
  dir.create(destination)
  good <- round83_make_archive(source_root, archive_dir, "good")
  dependent <- round83_make_archive(
    source_root, archive_dir, "dependent", imports = "real"
  )
  bad_root <- file.path(source_root, "bad")
  dir.create(file.path(bad_root, "old", "R"), recursive = TRUE)
  dir.create(file.path(bad_root, "extra"))
  writeLines(c("Package: real", "Version: 1.0.0"),
             file.path(bad_root, "old", "DESCRIPTION"), useBytes = TRUE)
  writeLines("f <- function() 1L", file.path(bad_root, "old", "R", "f.R"),
             useBytes = TRUE)
  writeLines("extra", file.path(bad_root, "extra", "file.txt"), useBytes = TRUE)
  bad <- file.path(archive_dir, "old_1.0.0.tar.gz")
  withr::with_dir(bad_root, utils::tar(
    bad, files = c("old", "extra"), compression = "gzip"
  ))
  warnings <- character()
  result <- withCallingHandlers(
    create_metapackage(
      "identityskipverse", c(bad, dependent, good), dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character(), on_component_error = "skip"
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("Could not read archive", warnings, fixed = TRUE)))
  expect_identical(result$packages, "good")
  expect_setequal(result$omitted$component, c("real", "dependent"))
})

test_that("filesystem guards handle non-link and invalid relative paths", {
  root <- tempfile("bigbang-round83-fs-")
  project <- file.path(root, "project")
  dir.create(project, recursive = TRUE)
  outside <- file.path(root, "outside.txt")
  writeLines("outside", outside)
  expect_false(.path_is_symlink(file.path(root, "missing")))
  expect_identical(
    .symlink_in_project_path(project, normalizePath(outside, winslash = "/")),
    ""
  )
  expect_identical(.symlink_in_project_path(project, "../outside.txt"), "")
  expect_error(
    .atomic_copy(file.path(root, "missing"), file.path(root, "dest")),
    "Could not copy the component archive"
  )
})
