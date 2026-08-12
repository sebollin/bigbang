round87_make_component <- function(source_root, archive_dir, name,
                                   version = "1.0.0") {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name), paste0("Version: ", version),
    paste0("Title: Round 87 fixture ", name),
    "Description: Temporary component for update and re-export tests.",
    "License: MIT", "Author: Test Author", "Maintainer: Test Author <test@example.org>"
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(paste0("export(", name, "_value)"),
             file.path(package_dir, "NAMESPACE"), useBytes = TRUE)
  writeLines(paste0(name, "_value <- function() 1L"),
             file.path(package_dir, "R", "value.R"), useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(source_root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

round87_install_component <- function(archive, library) {
  status <- system2(
    file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"),
    c("CMD", "INSTALL", "-l", shQuote(library), shQuote(archive)),
    stdout = FALSE, stderr = FALSE
  )
  expect_equal(status, 0L)
}

round87_build <- function(project, sandbox) {
  output <- withr::with_dir(sandbox, system2(
    file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"),
    c("CMD", "build", "--no-manual", "--no-resave-data", shQuote(project)),
    stdout = TRUE, stderr = TRUE
  ))
  tarball <- file.path(sandbox, paste0(basename(project), "_0.1.0.tar.gz"))
  expect_true(file.exists(tarball), paste(output, collapse = "\n"))
  tarball
}

round87_build_with_library <- function(project, sandbox, library) {
  library_paths <- unique(c(library, .libPaths()))
  withr::with_envvar(
    c(R_LIBS_USER = paste(library_paths, collapse = .Platform$path.sep)),
    round87_build(project, sandbox)
  )
}

test_that("update reconciles removed re-exports and component archives", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-round87-update-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  component_lib <- file.path(sandbox, "component-lib")
  recipient <- file.path(sandbox, "recipient")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dir.create(component_lib)
  dir.create(recipient)
  first <- round87_make_component(source_root, archives, "rounda")
  second <- round87_make_component(source_root, archives, "roundb")

  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  .libPaths(c(component_lib, old_libs))
  round87_install_component(first, component_lib)
  round87_install_component(second, component_lib)

  initial <- create_metapackage(
    "reconcileverse", c("rounda_1.0.0", "roundb_1.0.0"),
    pkg_dir = archives, dest_dir = destination, reexport = TRUE,
    document = TRUE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  expect_true(file.exists(file.path(initial$path, "R", "reexports.R")))
  expect_true(file.exists(file.path(
    initial$path, "inst", "archives", "roundb_1.0.0.tar.gz"
  )))

  updated <- create_metapackage(
    "reconcileverse", "rounda_1.0.0", pkg_dir = archives,
    dest_dir = destination, reexport = FALSE, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    update = TRUE
  )
  expect_false(file.exists(file.path(updated$path, "R", "reexports.R")))
  expect_false(file.exists(file.path(
    updated$path, "inst", "archives", "roundb_1.0.0.tar.gz"
  )))
  expect_false(grepl("roundb", paste(
    readLines(file.path(updated$path, "NAMESPACE"), warn = FALSE),
    collapse = "\n"
  ), fixed = TRUE))
  expect_false(any(grepl("importFrom\\(rounda", readLines(
    file.path(updated$path, "NAMESPACE"), warn = FALSE
  ))))

  tarball <- round87_build_with_library(updated$path, sandbox, component_lib)
  check_output <- withr::with_dir(sandbox, system2(
    file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"),
    c("CMD", "check", "--as-cran", shQuote(tarball)),
    stdout = TRUE, stderr = TRUE
  ))
  status_line <- check_output[grepl("^Status:", check_output)]
  expect_equal(length(status_line), 1L,
               info = paste(check_output, collapse = "\n"))
  expect_false(grepl("ERROR|WARNING", status_line),
               paste(check_output, collapse = "\n"))

  install_status <- system2(
    file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"),
    c("CMD", "INSTALL", "-l", shQuote(recipient), shQuote(tarball)),
    stdout = FALSE, stderr = FALSE
  )
  expect_equal(install_status, 0L)
  .libPaths(c(recipient, old_libs))
  loadNamespace("reconcileverse", lib.loc = recipient)
  install_function <- getExportedValue(
    "reconcileverse", "reconcileverse_install"
  )
  install_result <- install_function(verbose = FALSE)
  expect_true(any(grepl("^rounda", names(install_result$installed))))
  expect_true(dir.exists(file.path(recipient, "rounda")))
  expect_false(dir.exists(file.path(recipient, "roundb")))
})

test_that("re-exports install with or without documentation", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-round88-reexport-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  recipient <- file.path(sandbox, "recipient")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dir.create(recipient)
  first <- round87_make_component(source_root, archives, "nodoca")
  second <- round87_make_component(source_root, archives, "nodocb")

  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  .libPaths(c(recipient, old_libs))
  round87_install_component(first, recipient)
  round87_install_component(second, recipient)

  no_doc <- create_metapackage(
    "nodocverse", c("nodoca_1.0.0", "nodocb_1.0.0"), pkg_dir = archives,
    dest_dir = destination, reexport = TRUE, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  no_doc_namespace <- readLines(file.path(no_doc$path, "NAMESPACE"), warn = FALSE)
  expect_true(any(grepl("importFrom(nodoca, nodoca_value)", no_doc_namespace,
                        fixed = TRUE)))
  expect_true(any(grepl("importFrom(nodocb, nodocb_value)", no_doc_namespace,
                        fixed = TRUE)))
  no_doc_tar <- round87_build_with_library(no_doc$path, sandbox, recipient)
  no_doc_status <- system2(
    file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"),
    c("CMD", "INSTALL", "-l", shQuote(recipient), shQuote(no_doc_tar)),
    stdout = FALSE, stderr = FALSE
  )
  expect_equal(no_doc_status, 0L)

  documented <- create_metapackage(
    "docreexportverse", c("nodoca_1.0.0", "nodocb_1.0.0"), pkg_dir = archives,
    dest_dir = destination, reexport = TRUE, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  documented_namespace <- readLines(
    file.path(documented$path, "NAMESPACE"), warn = FALSE
  )
  import_lines <- documented_namespace[grepl("^importFrom\\(", documented_namespace)]
  expect_length(unique(gsub("[[:space:]]", "", import_lines)),
                length(import_lines))
  documented_tar <- round87_build_with_library(documented$path, sandbox, recipient)
  documented_status <- system2(
    file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"),
    c("CMD", "INSTALL", "-l", shQuote(recipient), shQuote(documented_tar)),
    stdout = FALSE, stderr = FALSE
  )
  expect_equal(documented_status, 0L)
})

test_that("atomic writer leftovers are excluded from generated tarballs", {
  sandbox <- tempfile("bigbang-round89-temporary-")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  file.copy(toy, archives)
  result <- create_metapackage(
    "temporaryverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
  root_temporary <- file.path(result$path, ".DESCRIPTION-tmp123")
  archive_temporary <- file.path(
    result$path, "inst", "archives", ".toycomponent_0.1.0.tar.gz-tmp123"
  )
  writeLines("interrupted write", root_temporary, useBytes = TRUE)
  writeLines("interrupted copy", archive_temporary, useBytes = TRUE)
  patterns <- readLines(file.path(result$path, ".Rbuildignore"), warn = FALSE)
  expect_true("^(.*/)?\\..*-[[:alnum:]]+$" %in% patterns)
  tarball <- round87_build(result$path, sandbox)
  members <- utils::untar(tarball, list = TRUE)
  expect_false(any(grepl("DESCRIPTION-tmp123|toycomponent.*-tmp123", members)))
  expect_true(any(grepl("inst/archives/toycomponent_0.1.0.tar.gz$", members)))
})
