make_inert_vulnerable_source <- function(root, cleanup = TRUE) {
  pkg <- file.path(root, "toxicmeta")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c(
    "Package: toxicmeta", "Type: Package", "Title: Toxic Fixture",
    "Version: 0.1.0",
    "Authors@R: person('T','D',email='t@e.com',role=c('aut','cre'))",
    "Description: Inert fixture containing historical signatures.",
    "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(character(), file.path(pkg, "NAMESPACE"))
  writeLines(c(
    "clean_pkg_dirs <- function(...) NULL",
    paste0(
      "install_and_load_packages <- function(pkg) { ",
      "safe_unlink(pkg, recursive = TRUE, force = TRUE) }"
    ),
    paste0(
      ".onLoad <- function(...) { if (FALSE) { ",
      "x <- list.files(tempdir()); unlink(x, recursive = TRUE) } }"
    )
  ), file.path(pkg, "R", "toxic.R"))
  if (cleanup) writeLines("#!/bin/sh\nrm -rf aaa", file.path(pkg, "cleanup"))
  pkg
}

test_that("scanner detects V1/V2/V3/V7 in sources and tarballs without execution", {
  root <- tempfile("scanner-source-")
  dir.create(root)
  pkg <- make_inert_vulnerable_source(root)
  sentinel <- file.path(root, "sentinel.txt")
  writeLines("vive", sentinel)

  source_result <- scan_bigbang_artifact(pkg)
  expect_true(source_result$vulnerable)
  expect_setequal(
    source_result$signatures,
    c("V1_component_unlink", "V2_clean_pkg_dirs",
      "V3_tempdir_sweep", "V7_cleanup_hook")
  )

  archive <- file.path(root, "toxicmeta_0.1.0.tar.gz")
  withr::with_dir(root, utils::tar(
    archive, "toxicmeta", compression = "gzip"
  ))
  archive_result <- scan_bigbang_artifact(archive)
  expect_setequal(archive_result$signatures, source_result$signatures)
  expect_true(file.exists(sentinel))
  expect_error(scan_bigbang_artifact(pkg, dry_run = FALSE), "read-only")
})

test_that("installed lazy-load scanning neither loads nor attaches the package", {
  root <- tempfile("scanner-installed-")
  dir.create(root)
  pkg <- make_inert_vulnerable_source(root, cleanup = FALSE)
  lib <- file.path(root, "lib")
  dir.create(lib)
  r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  output <- withr::with_dir(root, system2(
    r_bin,
    c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), shQuote(pkg)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_equal(status, 0L, info = paste(output, collapse = "\n"))

  namespaces_antes <- loadedNamespaces()
  search_antes <- search()
  result <- scan_bigbang_artifact(file.path(lib, "toxicmeta"))
  expect_setequal(
    result$signatures,
    c("V1_component_unlink", "V2_clean_pkg_dirs", "V3_tempdir_sweep")
  )
  expect_identical(loadedNamespaces(), namespaces_antes)
  expect_identical(search(), search_antes)
  expect_false("toxicmeta" %in% loadedNamespaces())
  expect_identical(result$tested_r, "R 4.6.1")
})

test_that("artefactos nuevos quedan limpios y llevan procedencia", {
  skip_if_not_installed("whisker")
  root <- tempfile("scanner-clean-")
  dir.create(root)
  src <- file.path(root, "src")
  archives <- file.path(root, "archives")
  dest <- file.path(root, "dest")
  dir.create(src)
  dir.create(archives)
  dir.create(dest)
  pkg <- file.path(src, "aaa")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c(
    "Package: aaa", "Type: Package", "Title: Dummy aaa", "Version: 0.1.0",
    "Authors@R: person('T','D',email='t@e.com',role=c('aut','cre'))",
    "Description: Dummy package.", "License: MIT"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(character(), file.path(pkg, "NAMESPACE"))
  writeLines("x <- 1", file.path(pkg, "R", "x.R"))
  withr::with_dir(src, utils::tar(
    file.path(archives, "aaa_0.1.0.tar.gz"), "aaa", compression = "gzip"
  ))
  suppressMessages(create_metapackage(
    "scanverse", "aaa_0.1.0", archives,
    dest_dir = dest,
    document = FALSE,
    verbose = FALSE,
    force_deps = character()
  ))

  result <- scan_bigbang_artifact(file.path(dest, "scanverse"))
  expect_false(result$vulnerable)
  expect_length(result$signatures, 0L)
  expect_identical(
    result$provenance[["Config/bigbang/generator-version"]], "0.1.0"
  )
  expect_identical(
    result$provenance[["Config/bigbang/template-safety-schema"]], "2"
  )
  legacy_prefix <- paste0("Config/", "local", "verse")
  expect_true(is.na(result$provenance[[paste0(
    legacy_prefix, "/generator-version"
  )]]))
})

test_that("scanner reads current and legacy provenance families", {
  root <- tempfile("bigbang-provenance-")
  current <- file.path(root, "current")
  legacy <- file.path(root, "legacy")
  dir.create(file.path(current, "R"), recursive = TRUE)
  dir.create(file.path(legacy, "R"), recursive = TRUE)

  current_prefix <- "Config/bigbang"
  legacy_prefix <- paste0("Config/", "local", "verse")
  description <- function(package, prefix, version, schema) {
    c(
      paste0("Package: ", package),
      "Type: Package",
      "Title: Provenance Fixture",
      "Version: 0.1.0",
      "Description: A fixture for generator provenance fields.",
      "License: MIT",
      paste0(prefix, "/generator-version: ", version),
      paste0(prefix, "/template-safety-schema: ", schema)
    )
  }
  writeLines(
    description("currentmeta", current_prefix, "0.2.0", "3"),
    file.path(current, "DESCRIPTION")
  )
  writeLines(
    description("legacymeta", legacy_prefix, "0.1.0", "2"),
    file.path(legacy, "DESCRIPTION")
  )

  current_result <- scan_bigbang_artifact(current)$provenance
  legacy_result <- scan_bigbang_artifact(legacy)$provenance
  expect_identical(
    current_result[[paste0(current_prefix, "/generator-version")]], "0.2.0"
  )
  expect_identical(
    current_result[[paste0(current_prefix, "/template-safety-schema")]], "3"
  )
  expect_identical(
    legacy_result[[paste0(legacy_prefix, "/generator-version")]], "0.1.0"
  )
  expect_identical(
    legacy_result[[paste0(legacy_prefix, "/template-safety-schema")]], "2"
  )
})

test_that("scanner distinguishes hash characters in strings from R comments", {
  root <- tempfile("scanner-hash-string-")
  package <- file.path(root, "hashmeta")
  dir.create(file.path(package, "R"), recursive = TRUE)
  writeLines(c(
    "Package: hashmeta", "Version: 0.1.0", "Title: Hash Fixture",
    "Description: A scanner fixture containing hash characters in strings.",
    "License: MIT"
  ), file.path(package, "DESCRIPTION"))
  writeLines(c(
    "marker <- \"# not a comment\"",
    "text <- \"safe_unlink(pkg, recursive = TRUE, force = TRUE)\"",
    "# clean_pkg_dirs <- function() NULL"
  ), file.path(package, "R", "clean.R"))
  expect_false(scan_bigbang_artifact(package)$vulnerable)

  writeLines(
    "marker <- \"#\"; safe_unlink(pkg, recursive = TRUE, force = TRUE)",
    file.path(package, "R", "actual.R")
  )
  result <- scan_bigbang_artifact(package)
  expect_true(result$vulnerable)
  expect_true("V1_component_unlink" %in% result$signatures)
})
