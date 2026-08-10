build_safety_archive <- function(name, version, source_root, archive_dir,
                                 code = NULL, imports = NULL,
                                 filename_version = version) {
  source_dir <- file.path(source_root, paste0(name, "-", version, "-src"))
  dir.create(file.path(source_dir, "R"), recursive = TRUE)
  code <- code %||% paste0(name, "_value <- function() \"", name, "\"")
  description <- c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: Temporary ", name, " component"),
    paste0("Version: ", version),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    paste0("Description: Temporary component ", name, "."),
    "License: MIT",
    "Encoding: UTF-8",
    if (is.null(imports)) character() else paste0("Imports: ", imports)
  )
  writeLines(description, file.path(source_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(paste0("export(", name, "_value)"), file.path(source_dir, "NAMESPACE"), useBytes = TRUE)
  writeLines(code, file.path(source_dir, "R", "value.R"), useBytes = TRUE)

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(source_root, system2(
    r_binary, c("CMD", "build", shQuote(source_dir)), stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  stopifnot(build_status == 0L)
  built <- file.path(source_root, paste0(name, "_", version, ".tar.gz"))
  target <- file.path(archive_dir, paste0(name, "_", filename_version, ".tar.gz"))
  stopifnot(file.exists(built), file.rename(built, target))
  target
}

`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("default implicit scanning reports guesses without binding them", {
  sandbox <- tempfile("bigbang-implicit-default-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  build_safety_archive(
    "commentpkg", "1.0.0", source_root, archive_dir,
    code = c(
      "# filter select index over aes would be useful here",
      "commentpkg_text <- function() paste('filter select index over aes')",
      "commentpkg_value <- function() 'only code, no guessed dependency'"
    )
  )

  result <- create_metapackage(
    "implicitverse", "commentpkg_1.0.0", archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE
  )
  description <- read.dcf(file.path(result$path, "DESCRIPTION"))
  namespace <- readLines(file.path(result$path, "NAMESPACE"), warn = FALSE)

  expect_false(any(
    c("dplyr", "ggplot2", "sp", "zoo") %in% result$implicit_dependencies
  ))
  expect_false(any(grepl("dplyr|ggplot2|sp|zoo", description[, "Imports"], perl = TRUE)))
  expect_false(any(grepl("import\\((dplyr|ggplot2|sp|zoo)", namespace, perl = TRUE)))

  # A deliberate, qualified reference is still reported, but remains opt-in.
  build_safety_archive(
    "qualifiedpkg", "1.0.0", source_root, archive_dir,
    code = c(
      "qualifiedpkg_value <- function(x) dplyr::filter(x, TRUE)",
      "# select and aes in a comment do not count"
    )
  )
  detected <- detect_implicit_dependencies("qualifiedpkg_1.0.0", archive_dir)
  expect_true("dplyr" %in% detected)
  qualified <- create_metapackage(
    "qualifiedverse", "qualifiedpkg_1.0.0", archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE
  )
  qualified_description <- read.dcf(file.path(qualified$path, "DESCRIPTION"))
  expect_false(grepl("dplyr", qualified_description[, "Imports"], fixed = TRUE))

  opted_in <- create_metapackage(
    "optinverse", "qualifiedpkg_1.0.0", archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    additional_deps = "dplyr"
  )
  opted_description <- read.dcf(file.path(opted_in$path, "DESCRIPTION"))
  expect_true(grepl("dplyr", opted_description[, "Imports"], fixed = TRUE))
})

test_that("reserved component names cannot exclude generated package files", {
  sandbox <- tempfile("bigbang-reserved-components-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  reserved <- c("R", "inst", "data", "man", "tests", "vignettes", "po", "src", "Meta")
  stems <- vapply(
    reserved,
    function(name) build_safety_archive(name, "1.0.0", source_root, archive_dir),
    character(1L)
  )
  result <- create_metapackage(
    "reservedverse", sub("\\.tar\\.gz$", "", basename(stems)), archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    force_deps = character()
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(result$path)), stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))
  tarball <- file.path(sandbox, "reservedverse_0.1.0.tar.gz")
  entries <- utils::untar(tarball, list = TRUE)
  expect_true(any(grepl("reservedverse/inst/archives/", entries, fixed = TRUE)))
  expect_true(any(grepl("reservedverse/R/", entries, fixed = TRUE)))
  expect_true(all(vapply(
    paste0("reservedverse/inst/archives/", basename(stems)),
    function(entry) entry %in% entries,
    logical(1L)
  )))
})

test_that("a self-contained chain installs despite comment-only dependency words", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-comment-chain-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(library_dir)
  build_safety_archive("basepkg", "1.0.0", source_root, archive_dir)
  build_safety_archive(
    "midpkg", "1.0.0", source_root, archive_dir, imports = "basepkg"
  )
  build_safety_archive(
    "toppkg", "1.0.0", source_root, archive_dir,
    imports = "midpkg",
    code = c(
      "# filter select index over aes are ordinary words in this comment",
      "toppkg_value <- function() 'top'"
    )
  )
  destination_tree <- file.path(destination, "chainverse")
  result <- create_metapackage(
    "chainverse",
    paste0(c("basepkg", "midpkg", "toppkg"), "_1.0.0"),
    archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(result$path)), stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))
  tarball <- file.path(sandbox, "chainverse_0.1.0.tar.gz")
  expect_true(file.exists(tarball))
  unlink(archive_dir, recursive = TRUE)
  unlink(destination_tree, recursive = TRUE)

  install_output <- system2(
    r_binary,
    c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(library_dir), shQuote(tarball)),
    stdout = TRUE, stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  expect_identical(install_status, 0L, info = paste(install_output, collapse = "\n"))

  script <- file.path(sandbox, "recipient.R")
  writeLines(c(
    sprintf(".libPaths(%s)", deparse(library_dir)),
    "suppressPackageStartupMessages(library(chainverse))",
    "result <- chainverse_install(verbose = FALSE)",
    "cat('FAILED:', length(result$failed), '\\n')",
    "cat('ORDER:', paste(result$order, collapse = '|'), '\\n')",
    "cat('READY:', all(vapply(c('basepkg', 'midpkg', 'toppkg'), requireNamespace, logical(1), quietly = TRUE)), '\\n')"
  ), script)
  recipient_output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE
  )
  report <- paste(recipient_output, collapse = "\n")
  expect_true(any(grepl("FAILED: 0", recipient_output, fixed = TRUE)), info = report)
  expect_true(any(grepl(
    "ORDER: basepkg_1.0.0|midpkg_1.0.0|toppkg_1.0.0",
    recipient_output, fixed = TRUE
  )), info = report)
  expect_true(any(grepl("READY: TRUE", recipient_output, fixed = TRUE)), info = report)
})

test_that("archive metadata mismatches are rejected before generation", {
  sandbox <- tempfile("bigbang-archive-metadata-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  build_safety_archive(
    "mismatch", "2.0.0", source_root, archive_dir, filename_version = "1.0.0"
  )

  expect_error(
    create_metapackage(
      "mismatchverse", "mismatch_1.0.0", archive_dir,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    class = "bigbang_error_archive_metadata"
  )
  expect_length(list.files(destination, all.files = TRUE, no.. = TRUE), 0L)

  direct <- install_local_pkg("mismatch_1.0.0", archive_dir, verbose = FALSE)
  expect_named(direct$failed, "mismatch_1.0.0")
  expect_match(direct$failed[[1L]], "declares version 2.0.0")
})

test_that("duplicate components and cycles are rejected by the generator", {
  sandbox <- tempfile("bigbang-graph-validation-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  build_safety_archive("duplicate", "1.0.0", source_root, archive_dir)
  build_safety_archive("duplicate", "2.0.0", source_root, archive_dir)
  expect_error(
    create_metapackage(
      "duplicateverse", c("duplicate_1.0.0", "duplicate_2.0.0"), archive_dir,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    class = "bigbang_error_duplicate_component"
  )

  build_safety_archive("cyclea", "1.0.0", source_root, archive_dir, imports = "cycleb")
  build_safety_archive("cycleb", "1.0.0", source_root, archive_dir, imports = "cyclea")
  expect_error(
    create_metapackage(
      "cycleverse", c("cyclea_1.0.0", "cycleb_1.0.0"), archive_dir,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    class = "bigbang_error_cycle"
  )
  expect_length(list.files(destination, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("already installed messages report the installed version", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-installed-version-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(library_dir)
  build_safety_archive("versioned", "9.9.9", source_root, archive_dir)
  build_safety_archive("versioned", "1.0.0", source_root, archive_dir)
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  install_output <- system2(
    r_binary,
    c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(library_dir),
      shQuote(file.path(archive_dir, "versioned_9.9.9.tar.gz"))),
    stdout = TRUE, stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  expect_identical(install_status, 0L, info = paste(install_output, collapse = "\n"))
  withr::local_libpaths(c(library_dir, .libPaths()))

  result <- create_metapackage(
    "versionverse", "versioned_1.0.0", archive_dir,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    force_deps = character()
  )
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), runtime)
  sys.source(file.path(result$path, "R", "install_packages.R"), runtime)
  output <- capture.output(
    unchanged <- runtime$install_local_archive(
      "versioned_1.0.0", archive_dir, ".tar.gz", upgrade = "newer"
    ),
    type = "message"
  )
  expect_true(isTRUE(unchanged$unchanged))
  expect_match(paste(output, collapse = "\n"), "installed version 9.9.9")
  expect_false(grepl("version 1.0.0", paste(output, collapse = "\n"), fixed = TRUE))
})
