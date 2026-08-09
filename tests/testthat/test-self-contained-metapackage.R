toy_archive_dir <- function(destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  source <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(source)) {
    source <- testthat::test_path(
      "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
    )
  }
  file.copy(source, destination)
  invisible(destination)
}

generate_self_contained <- function(name, archives, destination, ...) {
  create_metapackage(
    name = name,
    packages = "toycomponent_0.1.0",
    pkg_dir = archives,
    dest_dir = destination,
    document = FALSE,
    verbose = FALSE,
    import_deps = character(),
    force_deps = character(),
    ...
  )
}

installer_signature <- function(project_dir) {
  sources <- list.files(file.path(project_dir, "R"), full.names = TRUE)
  lines <- unlist(lapply(sources, readLines, warn = FALSE), use.names = FALSE)
  grep("_install <- function", lines, value = TRUE)
}

test_that("component archives travel inside the meta-package by default", {
  sandbox <- tempfile("bigbang-self-contained-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("selfverse", archives, destination)

  expect_true(dir.exists(file.path(result$path, "inst", "archives")))
  expect_identical(
    list.files(file.path(result$path, "inst", "archives")),
    "toycomponent_0.1.0.tar.gz"
  )

  # The default is resolved when the installer runs, so it points at the
  # library of whoever installed the meta-package rather than at this machine.
  signature <- installer_signature(result$path)
  expect_true(any(grepl("system.file(\"archives\"", signature, fixed = TRUE)))
  expect_false(any(grepl(normalizePath(archives), signature, fixed = TRUE)))
  expect_false(any(grepl(normalizePath(sandbox), signature, fixed = TRUE)))
})

test_that("the build ignore rules keep shipped archives and drop stray ones", {
  sandbox <- tempfile("bigbang-buildignore-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("ignoreverse", archives, destination)
  patterns <- readLines(file.path(result$path, ".Rbuildignore"), warn = FALSE)

  matches_any <- function(path) {
    any(vapply(
      patterns, function(pattern) grepl(pattern, path, perl = TRUE), logical(1)
    ))
  }

  # Archives deliberately shipped with the meta-package must reach the tarball.
  expect_false(matches_any("inst/archives/toycomponent_0.1.0.tar.gz"))
  # Archives left at the project root must not.
  expect_true(matches_any("toycomponent_0.1.0.tar.gz"))
  expect_true(matches_any("something.tar.gz"))
  expect_true(matches_any("something.zip"))
})

test_that("include_archives = FALSE keeps pkg_dir mandatory", {
  sandbox <- tempfile("bigbang-external-archives-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained(
    "externalverse", archives, destination, include_archives = FALSE
  )

  expect_false(dir.exists(file.path(result$path, "inst", "archives")))
  signature <- installer_signature(result$path)
  expect_false(any(grepl("system.file(\"archives\"", signature, fixed = TRUE)))
  expect_true(any(grepl("function(pkg_dir,", signature, fixed = TRUE)))
})

test_that("include_archives rejects anything that is not one logical", {
  sandbox <- tempfile("bigbang-include-validation-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  for (bad in list("yes", NA, c(TRUE, FALSE), NULL, 1)) {
    expect_error(
      generate_self_contained(
        "badverse", archives, destination, include_archives = bad
      ),
      regexp = "'include_archives'"
    )
  }
})

test_that("a shipped meta-package installs its components with no arguments", {
  skip_on_cran()
  skip_if_not_installed("withr")

  sandbox <- tempfile("bigbang-no-paths-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("shippedverse", archives, destination)

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(result$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))

  tarball <- file.path(sandbox, "shippedverse_0.1.0.tar.gz")
  expect_true(file.exists(tarball))

  # Everything the recipient would not have is removed: the archive directory
  # and the generated source tree. Only the tarball survives.
  unlink(archives, recursive = TRUE)
  unlink(destination, recursive = TRUE)
  expect_false(dir.exists(archives))

  library_dir <- file.path(sandbox, "library")
  dir.create(library_dir)
  install_output <- system2(
    r_binary,
    c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(library_dir),
      shQuote(tarball)),
    stdout = TRUE, stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  expect_identical(
    install_status, 0L, info = paste(install_output, collapse = "\n")
  )
  expect_false(dir.exists(file.path(library_dir, "toycomponent")))

  script <- file.path(sandbox, "recipient.R")
  writeLines(c(
    sprintf(".libPaths(%s)", deparse(library_dir)),
    "suppressPackageStartupMessages(library(shippedverse))",
    "result <- shippedverse_install(verbose = FALSE)",
    "cat('FAILED:', length(result$failed), '\\n')",
    "cat('COMPONENT:', requireNamespace('toycomponent', quietly = TRUE), '\\n')"
  ), script)
  recipient_output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  )
  report <- paste(recipient_output, collapse = "\n")

  expect_true(any(grepl("FAILED: 0", recipient_output, fixed = TRUE)), info = report)
  expect_true(any(grepl("COMPONENT: TRUE", recipient_output, fixed = TRUE)), info = report)
  expect_true(dir.exists(file.path(library_dir, "toycomponent")), info = report)
})
