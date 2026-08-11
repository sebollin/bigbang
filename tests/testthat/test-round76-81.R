round76_make_archive <- function(name, version, archive_path, source_root,
                                 imports = NULL) {
  package_root <- file.path(source_root, paste0(name, "-round76"))
  dir.create(file.path(package_root, "R"), recursive = TRUE)
  fields <- c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: Round 76 fixture ", name),
    paste0("Version: ", version),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    paste0("Description: Temporary fixture for ", name, "."),
    "License: MIT"
  )
  if (!is.null(imports)) fields <- c(fields, paste0("Imports: ", imports))
  writeLines(fields, file.path(package_root, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(round76_value)", file.path(package_root, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("round76_value <- function() 1L",
             file.path(package_root, "R", "value.R"), useBytes = TRUE)
  if (grepl("\\.zip$", archive_path, ignore.case = TRUE)) {
    withr::with_dir(source_root, utils::zip(archive_path, basename(package_root)))
  } else {
    withr::with_dir(source_root, utils::tar(
      archive_path, basename(package_root), compression = "gzip"
    ))
  }
  stopifnot(file.exists(archive_path))
  archive_path
}

round76_build_and_install <- function(project, sandbox, library_dir) {
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(project)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  stopifnot(build_status == 0L)
  tarball <- file.path(sandbox, paste0(basename(project), "_0.1.0.tar.gz"))
  install_output <- system2(
    r_binary,
    c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(library_dir),
      shQuote(tarball)),
    stdout = TRUE, stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  stopifnot(install_status == 0L)
  tarball
}

test_that("uppercase archive extensions are canonicalized end to end", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round76-case-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(library_dir)
  tar_archive <- round76_make_archive(
    "upperpkg", "1.0.0",
    file.path(archive_dir, "UPPERPKG_1.0.0.TAR.GZ"), source_root
  )
  zip_archive <- round76_make_archive(
    "zippkg", "1.0.0",
    file.path(archive_dir, "ZIPPKG_1.0.0.ZIP"), source_root
  )
  result <- suppressWarnings(create_metapackage(
    "caseverse", c(tar_archive, zip_archive), dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  ))
  expect_setequal(
    list.files(file.path(result$path, "inst", "archives")),
    c("UPPERPKG_1.0.0.tar.gz", "ZIPPKG_1.0.0.zip")
  )
  round76_build_and_install(result$path, sandbox, library_dir)
  script <- file.path(sandbox, "recipient-round76.R")
  writeLines(c(
    sprintf(".libPaths(%s)", deparse(library_dir)),
    "suppressPackageStartupMessages(library(caseverse))",
    "installed <- caseverse_install(verbose = FALSE)",
    "cat('FAILED:', length(installed$failed), '\\n')",
    "cat('UPPER:', requireNamespace('upperpkg', quietly = TRUE), '\\n')",
    "cat('ZIP:', requireNamespace('zippkg', quietly = TRUE), '\\n')"
  ), script)
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE
  )
  expect_true(any(grepl("FAILED: 0", output, fixed = TRUE)),
              info = paste(output, collapse = "\n"))
  expect_true(any(grepl("UPPER: TRUE", output, fixed = TRUE)),
              info = paste(output, collapse = "\n"))
  expect_true(any(grepl("ZIP: TRUE", output, fixed = TRUE)),
              info = paste(output, collapse = "\n"))
})

test_that("archive names that collide only by case are rejected", {
  sandbox <- tempfile("bigbang-round76-collision-")
  first_source <- file.path(sandbox, "first-source")
  second_source <- file.path(sandbox, "second-source")
  first_dir <- file.path(sandbox, "first")
  second_dir <- file.path(sandbox, "second")
  destination <- file.path(sandbox, "destination")
  dir.create(first_dir, recursive = TRUE)
  dir.create(second_dir)
  dir.create(destination)
  first <- round76_make_archive(
    "casefirst", "1.0.0", file.path(first_dir, "same_1.0.0.tar.gz"),
    first_source
  )
  second <- round76_make_archive(
    "casesecond", "1.0.0", file.path(second_dir, "SAME_1.0.0.TAR.GZ"),
    second_source
  )
  expect_error(
    suppressWarnings(create_metapackage(
      "casecollision", c(first, second), dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character()
    )),
    class = "bigbang_error_archive_basename_collision"
  )
})

test_that("unreadable unrelated archives are excluded with diagnostics", {
  sandbox <- tempfile("bigbang-round77-inventory-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(archive_dir, recursive = TRUE)
  dir.create(destination)
  valid <- round76_make_archive(
    "inventoryvalid", "1.0.0",
    file.path(archive_dir, "inventoryvalid_1.0.0.tar.gz"), source_root
  )
  noise <- round76_make_archive(
    "inventorynoise", "1.0.0",
    file.path(archive_dir, "inventorynoise_1.0.0.tar.gz"), source_root
  )
  bytes <- readBin(noise, "raw", n = file.info(noise)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], noise)
  warnings <- character()
  result <- withCallingHandlers(
    create_metapackage(
      "inventoryverse", valid, pkg_dir = archive_dir, dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character()
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(result$packages, "inventoryvalid")
  expect_true(any(grepl("Could not read archive", warnings, fixed = TRUE)))
  expect_true(any(grepl("inventorynoise_1.0.0.tar.gz", warnings, fixed = TRUE)))

  consumer <- round76_make_archive(
    "inventoryconsumer", "1.0.0",
    file.path(archive_dir, "inventoryconsumer_1.0.0.tar.gz"), source_root,
    imports = "inventorymissing"
  )
  missing <- round76_make_archive(
    "inventorymissing", "1.0.0",
    file.path(archive_dir, "inventorymissing_1.0.0.tar.gz"), source_root
  )
  bytes <- readBin(missing, "raw", n = file.info(missing)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], missing)
  warnings <- character()
  suppressWarnings(withCallingHandlers(
    create_metapackage(
      "inventorydependent", consumer, pkg_dir = archive_dir,
      dest_dir = file.path(sandbox, "dependent-destination"),
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character()
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  ))
  expect_true(any(grepl(
    "declares dependency inventorymissing", warnings, fixed = TRUE
  )))
  expect_true(any(grepl("could not be read", warnings, fixed = TRUE)))
})

test_that("vector resolution failures retain the requested names", {
  messages <- capture.output(
    result <- install_local_pkg(
      c("missing-round79-a", "missing-round79-b"), pkg_dir = tempdir(),
      verbose = TRUE
    ),
    type = "message"
  )
  expect_setequal(
    names(result$failed), c("missing-round79-a", "missing-round79-b")
  )
  expect_true(all(grepl(
    "missing-round79", names(result$failed), fixed = TRUE
  )))
  expect_true(any(grepl(
    "missing-round79-a, missing-round79-b", messages, fixed = TRUE
  )))
})

test_that("generated installation reports the declared package identity", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round81-identity-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(archive_dir, recursive = TRUE)
  dir.create(destination)
  dir.create(library_dir)
  archive <- round76_make_archive(
    "declaredround81", "1.0.0",
    file.path(archive_dir, "aliasround81_9.9.tar.gz"), source_root
  )
  result <- suppressWarnings(create_metapackage(
    "identityround81", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  ))
  environment <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), environment)
  sys.source(file.path(result$path, "R", "install_packages.R"), environment)
  withr::local_libpaths(c(library_dir, .libPaths()))
  messages <- suppressWarnings(capture.output(
    generated <- environment$install_packages_in_order(
      result$archives, archive_dir, NULL, verbose = FALSE, upgrade = "always",
      lib = library_dir
    ),
    type = "message"
  ))
  expect_length(generated$failed, 0L)
  expect_true(any(grepl(
    "Installed package declaredround81 from aliasround81_9.9 successfully.",
    messages, fixed = TRUE
  )))
  expect_false(any(grepl("Installed package aliasround81", messages, fixed = TRUE)))
  stem_messages <- suppressWarnings(capture.output(
    environment$install_local_archive(
      "aliasround81_9.9", archive_dir, NULL, upgrade = "always",
      lib = library_dir
    ),
    type = "message"
  ))
  expect_true(any(grepl(
    "Installed package declaredround81 from aliasround81_9.9 successfully.",
    stem_messages, fixed = TRUE
  )))
})
