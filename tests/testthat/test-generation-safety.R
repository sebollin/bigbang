`%||%` <- function(x, y) if (is.null(x)) y else x

build_safety_archive <- function(name, version, source_root, archive_dir,
                                 code = NULL, imports = NULL,
                                 filename_version = version,
                                 root_name = NULL, nested_description = FALSE,
                                 depends = NULL) {
  root_name <- root_name %||% paste0(name, "-", version, "-src")
  source_dir <- file.path(source_root, root_name)
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
    if (is.null(depends)) character() else paste0("Depends: ", depends),
    "Encoding: UTF-8",
    if (is.null(imports)) character() else paste0("Imports: ", imports)
  )
  writeLines(description, file.path(source_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(paste0("export(", name, "_value)"), file.path(source_dir, "NAMESPACE"), useBytes = TRUE)
  writeLines(code, file.path(source_dir, "R", "value.R"), useBytes = TRUE)
  if (isTRUE(nested_description)) {
    dir.create(file.path(source_dir, "tests"), showWarnings = FALSE)
    writeLines("Nested: fixture", file.path(source_dir, "tests", "DESCRIPTION"), useBytes = TRUE)
  }

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

test_that("dependency constraints are validated and R requirements propagate", {
  sandbox <- tempfile("bigbang-constraints-")
  source_root <- file.path(sandbox, "sources")
  low_archives <- file.path(sandbox, "low")
  good_archives <- file.path(sandbox, "good")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(low_archives)
  dir.create(good_archives)
  dir.create(destination)
  dir.create(library_dir)
  build_safety_archive("aaa", "1.0.0", source_root, low_archives)
  build_safety_archive(
    "needv2", "1.0.0", source_root, low_archives,
    imports = "aaa (>= 2.0.0)", depends = "R (>= 4.3.0)"
  )
  expect_error(
    create_metapackage(
      "constraintverse", c("aaa_1.0.0", "needv2_1.0.0"), low_archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    class = "bigbang_error_dependency_version",
    regexp = "aaa.*>= 2.0.0.*1.0.0"
  )
  expect_length(list.files(destination, all.files = TRUE, no.. = TRUE), 0L)

  build_safety_archive("aaa", "2.0.0", source_root, good_archives)
  file.copy(
    file.path(low_archives, "needv2_1.0.0.tar.gz"), good_archives,
    overwrite = TRUE
  )
  result <- create_metapackage(
    "constraintverse", c("aaa_2.0.0", "needv2_1.0.0"), good_archives,
    dest_dir = destination, document = FALSE, verbose = FALSE
  )
  generated_description <- read.dcf(file.path(result$path, "DESCRIPTION"))
  expect_match(generated_description[, "Depends"], "R \\(>= 4.3.0\\)")

  skip_on_cran()
  withr::local_libpaths(c(library_dir, .libPaths()))
  installed <- install_local_pkg(
    "needv2_1.0.0", good_archives, verbose = FALSE, upgrade = "always"
  )
  expect_length(installed$failed, 0L)
  expect_true(requireNamespace("aaa", quietly = TRUE))
  expect_true(requireNamespace("needv2", quietly = TRUE))
})

test_that("local dependencies outside the component set are rejected", {
  sandbox <- tempfile("bigbang-orphan-dependency-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  build_safety_archive("zzz", "1.0.0", source_root, archives)
  build_safety_archive("orphan", "1.0.0", source_root, archives, imports = "zzz")
  expect_error(
    create_metapackage(
      "orphanverse", "orphan_1.0.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    class = "bigbang_error_unincluded_dependency",
    regexp = "orphan.*zzz.*zzz_1.0.0.tar.gz"
  )

  external <- file.path(sandbox, "external")
  dir.create(external)
  build_safety_archive(
    "externalpkg", "1.0.0", source_root, external,
    imports = "stats"
  )
  external_result <- create_metapackage(
    "externalverse", "externalpkg_1.0.0", external,
    dest_dir = destination, document = FALSE, verbose = FALSE
  )
  expect_true("stats" %in% external_result$cran_dependencies)
})

test_that("named tolerances are explicit, reported, and typo-safe", {
  sandbox <- tempfile("bigbang-tolerate-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)

  build_safety_archive(
    "mismatchtol", "2.0.0", source_root, archives,
    filename_version = "1.0.0"
  )
  expect_warning(
    mismatch <- create_metapackage(
      "mismatchtolverse", "mismatchtol_1.0.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      tolerate = "filename_mismatch"
    ),
    NA
  )
  expect_identical(
    names(mismatch$tolerated), c("relaxation", "component", "reason")
  )
  expect_identical(mismatch$tolerated$relaxation, "filename_mismatch")
  expect_identical(mismatch$tolerated$component, "mismatchtol")
  expect_match(mismatch$tolerated$reason, "declares version 2.0.0")

  build_safety_archive("includedtol", "1.0.0", source_root, archives)
  build_safety_archive(
    "orphantol", "1.0.0", source_root, archives, imports = "includedtol"
  )
  expect_warning(
    orphan <- create_metapackage(
      "orphantolverse", "orphantol_1.0.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      tolerate = "unincluded_local_dep"
    ),
    "will not ship"
  )
  expect_identical(orphan$tolerated$relaxation, "unincluded_local_dep")
  expect_identical(orphan$tolerated$component, "orphantol")
  expect_match(orphan$tolerated$reason, "includedtol")

  empty_destination <- file.path(sandbox, "unknown-destination")
  dir.create(empty_destination)
  expect_error(
    create_metapackage(
      "unknowntolverse", "mismatchtol_1.0.0", archives,
      dest_dir = empty_destination, document = FALSE, verbose = FALSE,
      tolerate = "filename_mismtach"
    ),
    class = "bigbang_error_tolerance",
    regexp = "filename_mismtach"
  )
  expect_length(
    list.files(empty_destination, all.files = TRUE, no.. = TRUE), 0L
  )
  expect_error(
    create_metapackage(
      "invalidtolverse", "mismatchtol_1.0.0", archives,
      dest_dir = empty_destination, document = FALSE, verbose = FALSE,
      tolerate = TRUE
    ),
    class = "bigbang_error_tolerance",
    regexp = "character vector"
  )
  expect_length(
    list.files(empty_destination, all.files = TRUE, no.. = TRUE), 0L
  )
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

test_that("archive metadata uses the package root, not every DESCRIPTION", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-root-metadata-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dir.create(library_dir)
  build_safety_archive(
    "twodesc", "1.0.0", source_root, archives,
    root_name = "unrelated-root", nested_description = TRUE
  )
  result <- create_metapackage(
    "rootverse", "twodesc_1.0.0", archives,
    dest_dir = destination, document = FALSE, verbose = FALSE
  )
  withr::local_libpaths(c(library_dir, .libPaths()))
  installed <- install_local_pkg("twodesc_1.0.0", archives, verbose = FALSE)
  expect_length(installed$failed, 0L)
  expect_true(requireNamespace("twodesc", quietly = TRUE))
  expect_true(file.exists(file.path(result$path, "DESCRIPTION")))
})

test_that("archives without a root DESCRIPTION and truncated archives fail clearly", {
  sandbox <- tempfile("bigbang-invalid-archives-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)

  invalid_root <- file.path(source_root, "notapkg-1.0.0")
  dir.create(file.path(invalid_root, "tests"), recursive = TRUE)
  writeLines("Nested: only", file.path(invalid_root, "tests", "DESCRIPTION"))
  withr::with_dir(source_root, utils::tar(
    file.path(archives, "noroot_1.0.0.tar.gz"), "notapkg-1.0.0", compression = "gzip"
  ))
  expect_error(
    create_metapackage(
      "norootverse", "noroot_1.0.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "no DESCRIPTION at the package root"
  )

  empty_root <- file.path(source_root, "empty-1.0.0")
  dir.create(empty_root, recursive = TRUE)
  file.create(file.path(empty_root, "DESCRIPTION"))
  withr::with_dir(source_root, utils::tar(
    file.path(archives, "empty_1.0.0.tar.gz"), "empty-1.0.0", compression = "gzip"
  ))
  expect_error(
    create_metapackage(
      "emptyverse", "empty_1.0.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "must declare non-empty Package and Version"
  )

  valid <- build_safety_archive("truncated", "1.0.0", source_root, archives)
  bytes <- readBin(valid, "raw", n = file.info(valid)$size)
  truncated <- file.path(archives, "truncated_bad_1.0.0.tar.gz")
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], truncated)
  unlink(valid)
  file.rename(truncated, file.path(archives, "truncated_1.0.0.tar.gz"))
  expect_error(
    create_metapackage(
      "truncatedverse", "truncated_1.0.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "status [0-9]|extract|archive"
  )
  expect_error(.validate_archive_members(c("root/../escape")), "unsafe")
})

test_that("malformed R source is reported during heuristic scanning", {
  sandbox <- tempfile("bigbang-parse-warning-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  build_safety_archive(
    "badparse", "1.0.0", source_root, archives,
    code = "badparse_value <- function(x) {{{"
  )
  warning_message <- NULL
  withCallingHandlers(
    create_metapackage(
      "parseverse", "badparse_1.0.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    warning = function(w) {
      if (grepl("Could not parse R source file", conditionMessage(w))) {
        warning_message <<- conditionMessage(w)
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_false(is.null(warning_message))
  # Name the component archive and the path inside it. The extraction directory
  # is a temporary that no longer exists when the reader sees the warning, so
  # reporting it would leave nothing to act on.
  expect_match(warning_message, "badparse_1.0.0.tar.gz", fixed = TRUE)
  expect_match(warning_message, "R/value.R", fixed = TRUE)
  expect_false(grepl(basename(tempdir()), warning_message, fixed = TRUE))
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

test_that("archive metadata mismatches warn but use DESCRIPTION identity", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-archive-metadata-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(library_dir)
  build_safety_archive(
    "mismatchfixture", "2.0.0", source_root, archive_dir, filename_version = "1.0.0"
  )

  expect_warning(
    result <- create_metapackage(
      "mismatchverse", "mismatchfixture_1.0.0", archive_dir,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "declares version 2.0.0"
  )
  expect_true(file.exists(file.path(result$path, "DESCRIPTION")))

  withr::local_libpaths(c(library_dir, .libPaths()))
  direct <- suppressWarnings(
    install_local_pkg("mismatchfixture_1.0.0", archive_dir, verbose = FALSE)
  )
  expect_length(direct$failed, 0L)
  expect_named(direct$installed, "mismatchfixture_1.0.0")
  expect_identical(as.character(utils::packageVersion("mismatchfixture")), "2.0.0")
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

test_that("generation tolerations cannot disable recipient protection invariants", {
  sandbox <- tempfile("bigbang-tolerance-invariant-")
  source_root <- file.path(sandbox, "sources")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(destination)
  all_tolerations <- .allowed_tolerations

  generate <- function(name, packages, pkg_dir) {
    create_metapackage(
      name, packages, pkg_dir,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      tolerate = all_tolerations
    )
  }
  archive_dir <- function(name) {
    path <- file.path(sandbox, name)
    dir.create(path)
    path
  }

  constraint_dir <- archive_dir("constraint")
  build_safety_archive("guarddep", "1.0.0", source_root, constraint_dir)
  build_safety_archive(
    "guardneed", "1.0.0", source_root, constraint_dir,
    imports = "guarddep (>= 2.0.0)"
  )

  cycle_dir <- archive_dir("cycle")
  build_safety_archive(
    "guardcyclea", "1.0.0", source_root, cycle_dir,
    imports = "guardcycleb"
  )
  build_safety_archive(
    "guardcycleb", "1.0.0", source_root, cycle_dir,
    imports = "guardcyclea"
  )

  duplicate_dir <- archive_dir("duplicate")
  build_safety_archive("guarddup", "1.0.0", source_root, duplicate_dir)
  build_safety_archive("guarddup", "2.0.0", source_root, duplicate_dir)

  valid_dir <- archive_dir("valid")
  valid <- build_safety_archive(
    "guardvalid", "1.0.0", source_root, valid_dir
  )

  truncated_dir <- archive_dir("truncated")
  truncated <- build_safety_archive(
    "guardtruncated", "1.0.0", source_root, truncated_dir,
    code = rep("guardtruncated_padding <- 1L", 1000L)
  )
  bytes <- readBin(truncated, "raw", n = file.info(truncated)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], truncated)

  traversal_dir <- archive_dir("traversal")
  traversal_work <- file.path(sandbox, "traversal-work")
  dir.create(traversal_work)
  traversal_payload <- file.path(sandbox, "traversal-payload.txt")
  writeLines("unsafe", traversal_payload)
  traversal <- file.path(traversal_dir, "guardtraversal.tar")
  withr::with_dir(traversal_work, utils::tar(
    traversal, "../traversal-payload.txt",
    compression = "none", tar = "internal"
  ))

  absolute_dir <- archive_dir("absolute")
  absolute <- file.path(absolute_dir, "guardabsolute.tar")
  utils::tar(
    absolute, traversal_payload, compression = "none", tar = "internal"
  )

  roots_dir <- archive_dir("roots")
  roots_work <- file.path(sandbox, "roots-work")
  dir.create(roots_work)
  for (root in c("root-one", "root-two")) {
    dir.create(file.path(roots_work, root))
    writeLines(c(
      paste0("Package: ", gsub("-", "", root)), "Version: 1.0.0"
    ), file.path(roots_work, root, "DESCRIPTION"))
  }
  multiple_roots <- file.path(roots_dir, "guardroots.tar.gz")
  withr::with_dir(roots_work, utils::tar(
    multiple_roots, c("root-one", "root-two"), compression = "gzip"
  ))

  no_description_dir <- archive_dir("no-description")
  no_description_work <- file.path(sandbox, "no-description-work")
  dir.create(file.path(no_description_work, "guardnodesc"), recursive = TRUE)
  writeLines(
    character(), file.path(no_description_work, "guardnodesc", "NAMESPACE")
  )
  no_description <- file.path(no_description_dir, "guardnodesc.tar.gz")
  withr::with_dir(no_description_work, utils::tar(
    no_description, "guardnodesc", compression = "gzip"
  ))

  empty_fields_dir <- archive_dir("empty-fields")
  empty_fields_work <- file.path(sandbox, "empty-fields-work")
  dir.create(file.path(empty_fields_work, "guardempty"), recursive = TRUE)
  writeLines(
    c("Package:", "Version:"),
    file.path(empty_fields_work, "guardempty", "DESCRIPTION")
  )
  empty_fields <- file.path(empty_fields_dir, "guardempty.tar.gz")
  withr::with_dir(empty_fields_work, utils::tar(
    empty_fields, "guardempty", compression = "gzip"
  ))

  checks <- list(
    unsatisfied_version = list(
      run = function() {
        generate(
          "guardconstraintverse",
          c("guarddep_1.0.0", "guardneed_1.0.0"), constraint_dir
        )
      },
      class = "bigbang_error_dependency_version"
    ),
    dependency_cycle = list(
      run = function() {
        generate(
          "guardcycleverse",
          c("guardcyclea_1.0.0", "guardcycleb_1.0.0"), cycle_dir
        )
      },
      class = "bigbang_error_cycle"
    ),
    truncated_archive = list(
      run = function() generate("guardtruncatedverse", truncated, NULL),
      class = NULL
    ),
    parent_traversal = list(
      run = function() generate("guardtraversalverse", traversal, NULL),
      class = NULL
    ),
    absolute_member = list(
      run = function() generate("guardabsoluteverse", absolute, NULL),
      class = NULL
    ),
    multiple_roots = list(
      run = function() generate("guardrootsverse", multiple_roots, NULL),
      class = NULL
    ),
    missing_root_description = list(
      run = function() generate("guardnodescverse", no_description, NULL),
      class = NULL
    ),
    empty_identity = list(
      run = function() generate("guardemptyverse", empty_fields, NULL),
      class = NULL
    ),
    invalid_metapackage_name = list(
      run = function() generate("guard_bad", valid, NULL),
      class = NULL
    ),
    reserved_metapackage_name = list(
      run = function() generate("stats", valid, NULL),
      class = NULL
    ),
    duplicate_component = list(
      run = function() {
        generate(
          "guarddupverse", c("guarddup_1.0.0", "guarddup_2.0.0"),
          duplicate_dir
        )
      },
      class = "bigbang_error_duplicate_component"
    )
  )

  if (.Platform$OS.type != "windows") {
    link_dir <- archive_dir("link")
    link_work <- file.path(sandbox, "link-work")
    link_root <- file.path(link_work, "guardlink")
    dir.create(link_root, recursive = TRUE)
    outside_description <- file.path(sandbox, "outside-DESCRIPTION")
    writeLines(c("Package: guardlink", "Version: 1.0.0"), outside_description)
    linked <- file.symlink(
      outside_description, file.path(link_root, "DESCRIPTION")
    )
    if (isTRUE(linked) && nzchar(Sys.which("tar"))) {
      link_archive <- file.path(link_dir, "guardlink.tar.gz")
      packed <- withr::with_dir(link_work, system2(
        "tar", c("czf", shQuote(link_archive), "guardlink"),
        stdout = FALSE, stderr = FALSE
      ))
      if (identical(packed, 0L)) {
        checks$symbolic_link <- list(
          run = function() generate("guardlinkverse", link_archive, NULL),
          class = NULL
        )
      }
    }
  }

  expect_true("symbolic_link" %in% names(checks) ||
                .Platform$OS.type == "windows")
  for (label in names(checks)) {
    condition <- tryCatch(
      suppressWarnings(checks[[label]]$run()),
      error = identity
    )
    expect_true(inherits(condition, "error"), info = label)
    expected_class <- checks[[label]]$class
    if (!is.null(expected_class)) {
      expect_true(inherits(condition, expected_class), info = label)
    }
  }
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
  expect_match(
    paste(output, collapse = "\n"),
    "installed version 9.9.9, newer than archive version 1.0.0"
  )
  direct_output <- capture.output(
    install_local_pkg(
      "versioned_1.0.0", archive_dir, verbose = TRUE, upgrade = "newer"
    ),
    type = "message"
  )
  expect_match(
    paste(direct_output, collapse = "\n"),
    "installed version 9.9.9, newer than archive version 1.0.0"
  )
})

test_that("reexport FALSE keeps the attach-only generation path", {
  sandbox <- tempfile("bigbang-reexport-")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  toy <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  file.copy(toy, archives)
  result <- NULL
  result <- create_metapackage(
    "reexportverse", "toycomponent_0.1.0", archives,
    dest_dir = destination, reexport = FALSE, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  expect_false(file.exists(file.path(result$path, "R", "reexports.R")))
  expect_false(any(grepl("importFrom\\(toycomponent", readLines(
    file.path(result$path, "NAMESPACE"), warn = FALSE
  ))))
})

test_that("an AppleDouble sibling does not hide the package root", {
  # Archiving a package directory on macOS with extended attributes emits a
  # "._<dir>" member beside it, and R installs such an archive, so rejecting it
  # would reject a working package. This exercises the root-counting rule
  # directly: building the archive with a tar that writes the sibling is not
  # reproducible across platforms, and the rule is what the fix changed.
  extracted <- tempfile("bigbang-appledouble-")
  root <- file.path(extracted, "apdouble")
  dir.create(root, recursive = TRUE)
  writeLines("Package: apdouble", file.path(root, "DESCRIPTION"))
  writeLines("apple double metadata", file.path(extracted, "._apdouble"))
  writeLines("finder metadata", file.path(extracted, ".DS_Store"))
  expect_identical(
    .find_archive_root(extracted, "apdouble_1.0.0.tar.gz"),
    file.path(extracted, "apdouble")
  )

  # Only that metadata convention is ignored. Any other extra entry still means
  # the archive is not a single package root.
  writeLines("stray", file.path(extracted, "stray.txt"))
  expect_error(
    .find_archive_root(extracted, "apdouble_1.0.0.tar.gz"),
    "one package root directory"
  )
})

test_that("diagnose_dependencies extracts through the guarded path", {
  # It is exported, and it used to call untar() directly. A truncated archive
  # shows the guard is in place on every platform: without it the extraction
  # status is discarded and only a warning is emitted.
  sandbox <- tempfile("bigbang-diagnose-guarded-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  build_safety_archive(
    "truncme", "1.0.0", source_root, archives,
    code = rep("padding_value <- 1", 4000)
  )
  archive <- file.path(archives, "truncme_1.0.0.tar.gz")
  bytes <- readBin(archive, "raw", file.size(archive))
  writeBin(bytes[seq_len(floor(length(bytes) * 0.4))], archive)
  expect_error(
    diagnose_dependencies("truncme_1.0.0", pkg_dir = archives),
    "Could not extract archive"
  )
})

test_that("diagnose_dependencies refuses an archive carrying symbolic links", {
  # Windows stores a link as a reparse point that untar does not reconstruct, so
  # there is no link left to reject and the case cannot be built there.
  skip_on_os("windows")
  sandbox <- tempfile("bigbang-diagnose-symlink-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  secret_dir <- file.path(sandbox, "secret")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(secret_dir)
  secret <- file.path(secret_dir, "credentials.txt")
  writeLines("class = sentinel_must_not_leak", secret)

  source_dir <- file.path(source_root, "spy")
  dir.create(file.path(source_dir, "R"), recursive = TRUE)
  writeLines(
    c("Package: spy", "Version: 1.0.0", "Title: Spy", "Description: Spy.",
      "License: GPL-3", "Author: A", "Maintainer: A <a@b.c>"),
    file.path(source_dir, "DESCRIPTION")
  )
  writeLines("export(f)", file.path(source_dir, "NAMESPACE"))
  linked <- file.symlink(secret, file.path(source_dir, "R", "f.R"))
  skip_if_not(isTRUE(linked), "This platform cannot create symbolic links.")

  # The system archiver stores the link as a link. utils::tar() reaches for a GNU
  # long-linkname extension on some platforms, which is not what we are testing.
  archive <- file.path(archives, "spy_1.0.0.tar.gz")
  packed <- withr::with_dir(source_root, system2(
    "tar", c("czf", shQuote(archive), "spy"), stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(identical(packed, 0L), "No usable system tar for this test.")

  extracted <- tempfile("bigbang-symlink-probe-")
  dir.create(extracted)
  suppressWarnings(utils::untar(archive, exdir = extracted))
  probe <- file.path(extracted, "spy", "R", "f.R")
  skip_if_not(
    nzchar(Sys.readlink(probe)),
    "This platform did not restore the member as a symbolic link."
  )

  # The scanner used to read through the link and return the contents of an
  # unrelated file in its result.
  expect_error(
    diagnose_dependencies("spy_1.0.0", pkg_dir = archives),
    "symbolic links"
  )
})
