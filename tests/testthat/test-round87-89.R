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

test_that("update reconciles legacy re-exports and removed component archives", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-round87-update-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  recipient <- file.path(sandbox, "recipient")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dir.create(recipient)
  first <- round87_make_component(source_root, archives, "rounda")
  second <- round87_make_component(source_root, archives, "roundb")

  initial <- create_metapackage(
    "reconcileverse", c("rounda_1.0.0", "roundb_1.0.0"),
    pkg_dir = archives, dest_dir = destination,
    document = TRUE, verbose = FALSE, import_deps = character(),
    force_deps = character(),
    workflow = c("First" = "rounda", "Second" = "roundb")
  )
  legacy_reexport <- file.path(initial$path, "R", "reexports.R")
  writeLines("legacy_value <- roundb::roundb_value", legacy_reexport)
  generated_manifest <- readRDS(file.path(
    initial$path, .generation_manifest_name
  ))
  legacy_manifest <- .manifest_records(
    initial$path, c(generated_manifest$files, "R/reexports.R")
  )
  .atomic_save_rds(
    legacy_manifest, file.path(initial$path, .generation_manifest_name)
  )
  expect_true(file.exists(legacy_reexport))
  expect_true(file.exists(file.path(
    initial$path, "inst", "archives", "roundb_1.0.0.tar.gz"
  )))

  updated <- create_metapackage(
    "reconcileverse", "rounda_1.0.0", pkg_dir = archives,
    dest_dir = destination, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    include_archives = FALSE, update = TRUE
  )
  expect_false(file.exists(file.path(updated$path, "R", "reexports.R")))
  expect_false(file.exists(file.path(
    updated$path, "inst", "archives", "roundb_1.0.0.tar.gz"
  )))
  expect_false(file.exists(file.path(
    updated$path, "inst", "archives", "rounda_1.0.0.tar.gz"
  )))
  expect_false(file.exists(file.path(
    updated$path, "vignettes", "workflow-reconcileverse.Rmd"
  )))
  expect_false(grepl("roundb", paste(
    readLines(file.path(updated$path, "NAMESPACE"), warn = FALSE),
    collapse = "\n"
  ), fixed = TRUE))
  expect_false(any(grepl("importFrom\\(rounda", readLines(
    file.path(updated$path, "NAMESPACE"), warn = FALSE
  ))))

  tarball <- round87_build(updated$path, sandbox)
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
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  .libPaths(c(recipient, old_libs))
  loadNamespace("reconcileverse", lib.loc = recipient)
  install_function <- getExportedValue(
    "reconcileverse", "reconcileverse_install"
  )
  install_result <- install_function(pkg_dir = archives, verbose = FALSE)
  expect_true(any(grepl("^rounda", names(install_result$installed))))
  expect_true(dir.exists(file.path(recipient, "rounda")))
  expect_false(dir.exists(file.path(recipient, "roundb")))
})

test_that("generated artifact option combinations pass R CMD check", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-generated-matrix-")
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(sources, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  round87_make_component(sources, archives, "matrixa")

  combinations <- expand.grid(
    include_archives = c(FALSE, TRUE),
    workflow = c(FALSE, TRUE),
    reexport = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  r <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  for (i in seq_len(nrow(combinations))) {
    name <- paste0("matrixverse", i)
    workflow <- if (combinations$workflow[[i]]) {
      c("Stage" = "matrixa")
    } else {
      NULL
    }
    generated <- create_metapackage(
      name, "matrixa_1.0.0", pkg_dir = archives, dest_dir = destination,
      document = TRUE, verbose = FALSE, import_deps = character(),
      force_deps = character(), workflow = workflow,
      include_archives = combinations$include_archives[[i]],
      reexport = combinations$reexport[[i]]
    )
    tarball <- round87_build(generated$path, sandbox)
    output <- withr::with_dir(sandbox, system2(
      r, c("CMD", "check", "--as-cran", "--no-manual", shQuote(tarball)),
      stdout = TRUE, stderr = TRUE
    ))
    status <- output[grepl("^Status:", output)]
    expect_equal(length(status), 1L, info = paste(output, collapse = "\n"))
    expect_false(
      grepl("ERROR|WARNING", status),
      info = paste(name, paste(output, collapse = "\n"), sep = "\n")
    )
  }
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
