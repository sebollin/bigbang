make_input_archive <- function(name, version, archive_path, source_root,
                               imports = NULL, filename_root = NULL,
                               r_code = NULL) {
  if (is.null(filename_root)) filename_root <- paste0(name, "-source")
  package_root <- file.path(source_root, filename_root)
  dir.create(file.path(package_root, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: Input fixture ", name),
    paste0("Version: ", version),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    paste0("Description: Temporary input fixture for ", name, "."),
    "License: MIT",
    if (is.null(imports)) character() else paste0("Imports: ", imports)
  ), file.path(package_root, "DESCRIPTION"), useBytes = TRUE)
  writeLines(paste0("export(", name, "_value)"),
             file.path(package_root, "NAMESPACE"), useBytes = TRUE)
  if (is.null(r_code)) r_code <- paste0(name, "_value <- function() 1L")
  writeLines(r_code,
             file.path(package_root, "R", "value.R"), useBytes = TRUE)
  if (grepl("\\.zip$", archive_path, ignore.case = TRUE)) {
    withr::with_dir(source_root, utils::zip(archive_path, filename_root))
  } else {
    withr::with_dir(source_root, utils::tar(
      archive_path, filename_root, compression = "gzip"
    ))
  }
  stopifnot(file.exists(archive_path))
  invisible(archive_path)
}

test_that("component paths can come from multiple directories and extensions", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-component-inputs-")
  source_root <- file.path(sandbox, "sources")
  first_dir <- file.path(sandbox, "first")
  second_dir <- file.path(sandbox, "second")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(first_dir)
  dir.create(second_dir)
  dir.create(destination)
  dir.create(library_dir)
  first <- make_input_archive(
    "firstpkg", "1.0.0", file.path(first_dir, "firstpkg_1.0.0.tar.gz"),
    source_root
  )
  second <- make_input_archive(
    "secondpkg", "1.0.0", file.path(second_dir, "secondpkg_1.0.0.zip"),
    source_root
  )

  result <- create_metapackage(
    "inputverse", c(first, second), dest_dir = destination,
    document = FALSE, verbose = FALSE, force_deps = character()
  )
  expect_identical(result$packages, c("firstpkg", "secondpkg"))
  expect_identical(result$archives, c("firstpkg_1.0.0", "secondpkg_1.0.0"))
  expect_setequal(
    list.files(file.path(result$path, "inst", "archives")),
    c("firstpkg_1.0.0.tar.gz", "secondpkg_1.0.0.zip")
  )
  origin <- normalizePath(sandbox, winslash = "/")
  generated_files <- list.files(
    result$path, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    no.. = TRUE
  )
  generated_text <- generated_files[
    !dir.exists(generated_files) & !grepl("\\.mo$|\\.(tar\\.gz|zip|tar)$",
                                          generated_files, ignore.case = TRUE)
  ]
  emitted <- paste(
    unlist(lapply(generated_text, readLines, warn = FALSE)), collapse = "\n"
  )
  expect_false(grepl(origin, emitted, fixed = TRUE))
  withr::local_libpaths(c(library_dir, .libPaths()))
  first_install <- install_local_pkg(first, verbose = FALSE, upgrade = "always")
  second_install <- install_local_pkg(second, verbose = FALSE, upgrade = "always")
  second_by_stem <- install_local_pkg(
    "secondpkg_1.0.0", c(first_dir, second_dir),
    verbose = FALSE, upgrade = "always"
  )
  expect_length(first_install$failed, 0L)
  expect_length(second_install$failed, 0L)
  expect_length(second_by_stem$failed, 0L)
  expect_true(requireNamespace("firstpkg", quietly = TRUE))
  expect_true(requireNamespace("secondpkg", quietly = TRUE))
})

test_that("the local installer reports policy, constraint, and install failures", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-installer-branches-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(library_dir)
  skipped <- make_input_archive(
    "skippedinput", "1.0.0", file.path(archives, "skippedinput_1.0.0.tar.gz"),
    source_root, imports = "not.installed.bigbang.input"
  )
  withr::local_libpaths(c(library_dir, .libPaths()))
  skipped_result <- install_local_pkg(
    skipped, cran_deps = "skip", verbose = TRUE, upgrade = "always"
  )
  expect_length(skipped_result$skipped, 1L)
  error_result <- install_local_pkg(
    skipped, cran_deps = "error", verbose = TRUE, upgrade = "always"
  )
  expect_length(error_result$failed, 1L)
  repo_result <- install_local_pkg(
    skipped, cran_deps = "install", repos = NULL, verbose = FALSE,
    upgrade = "always"
  )
  expect_length(repo_result$failed, 1L)

  dependency_source <- file.path(sandbox, "dependency-source")
  dependency_dir <- file.path(sandbox, "dependency-archives")
  dir.create(dependency_dir)
  make_input_archive(
    "constraintbase", "1.0.0",
    file.path(dependency_dir, "constraintbase_1.0.0.tar.gz"), dependency_source
  )
  constrained <- make_input_archive(
    "constraintconsumer", "1.0.0",
    file.path(archives, "constraintconsumer_1.0.0.tar.gz"), source_root,
    imports = "constraintbase (>= 2.0.0)"
  )
  constraint_result <- install_local_pkg(
    constrained, pkg_dir = c(archives, dependency_dir), verbose = FALSE,
    upgrade = "always"
  )
  expect_match(
    constraint_result$failed[[1L]], "requires constraintbase >= 2.0.0",
    fixed = TRUE
  )

  duplicate_one_source <- file.path(sandbox, "duplicate-one-source")
  duplicate_two_source <- file.path(sandbox, "duplicate-two-source")
  duplicate_one_dir <- file.path(sandbox, "duplicate-one")
  duplicate_two_dir <- file.path(sandbox, "duplicate-two")
  dir.create(duplicate_one_dir)
  dir.create(duplicate_two_dir)
  make_input_archive(
    "duplicatedep", "1.0.0", file.path(duplicate_one_dir, "first.tar.gz"),
    duplicate_one_source
  )
  make_input_archive(
    "duplicatedep", "1.0.0", file.path(duplicate_two_dir, "second.tar.gz"),
    duplicate_two_source
  )
  duplicate_consumer <- make_input_archive(
    "duplicateconsumer", "1.0.0",
    file.path(archives, "duplicateconsumer_1.0.0.tar.gz"), source_root,
    imports = "duplicatedep"
  )
  duplicate_result <- install_local_pkg(
    duplicate_consumer,
    pkg_dir = c(archives, duplicate_one_dir, duplicate_two_dir),
    verbose = FALSE, upgrade = "always"
  )
  expect_match(duplicate_result$failed[[1L]], "More than one local archive",
               fixed = TRUE)

  broken <- make_input_archive(
    "brokeninput", "1.0.0", file.path(archives, "brokeninput_1.0.0.tar.gz"),
    source_root, r_code = "brokeninput_value <- function("
  )
  broken_result <- suppressWarnings(
    install_local_pkg(broken, verbose = FALSE, upgrade = "always")
  )
  expect_length(broken_result$failed, 1L)
})

test_that("stems retain the old fallback while pkg_dir is optional for paths", {
  sandbox <- tempfile("bigbang-component-stems-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  archive <- make_input_archive(
    "stemcompat", "1.0.0", file.path(archives, "stemcompat_1.0.0.tar.gz"),
    source_root
  )

  result <- create_metapackage(
    "stemverse", "stemcompat_1.0.0", archives, ".tar.gz",
    dest_dir = destination, document = FALSE, verbose = FALSE,
    force_deps = character()
  )
  expect_identical(result$packages, "stemcompat")
  expect_error(
    create_metapackage(
      "missingverse", "missing_1.0.0", dest_dir = destination,
      document = FALSE, verbose = FALSE, force_deps = character()
    ),
    "no 'pkg_dir' was supplied"
  )
  expect_true(file.exists(archive))
})

test_that("generated metadata validation rejects empty DESCRIPTION fields", {
  sandbox <- tempfile("bigbang-generated-metadata-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(file.path(source_root, "empty-source", "R"), recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  file.create(file.path(source_root, "empty-source", "DESCRIPTION"))
  writeLines(character(), file.path(source_root, "empty-source", "NAMESPACE"))
  withr::with_dir(source_root, utils::tar(
    file.path(archives, "empty.tar.gz"), "empty-source", compression = "gzip"
  ))
  valid <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  if (!nzchar(valid)) valid <- testthat::test_path(
    "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
  )
  result <- create_metapackage(
    "generatedmetaverse", valid, dest_dir = destination,
    document = FALSE, verbose = FALSE, force_deps = character()
  )
  generated <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), generated)
  sys.source(file.path(result$path, "R", "install_packages.R"), generated)
  expect_error(
    generated$read_archive_metadata("empty", archives, ".tar.gz"),
    "must declare non-empty Package and Version"
  )
})

test_that("DESCRIPTION identity accepts unversioned and mismatched filenames", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-description-identity-")
  source_root <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  library_dir <- file.path(sandbox, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dir.create(library_dir)
  no_version <- make_input_archive(
    "noversionpkg", "1.2.3", file.path(archives, "noversionpkg.tar.gz"),
    source_root
  )
  mismatch <- make_input_archive(
    "declaredpkg", "2.0.0", file.path(archives, "alias_1.0.0.tar.gz"),
    source_root, filename_root = "declared-source"
  )

  result <- suppressWarnings(create_metapackage(
    "identityverse", c(no_version, mismatch), dest_dir = destination,
    document = FALSE, verbose = FALSE, force_deps = character()
  ))
  expect_identical(result$packages, c("noversionpkg", "declaredpkg"))
  withr::local_libpaths(c(library_dir, .libPaths()))
  direct <- install_local_pkg(no_version, verbose = FALSE, upgrade = "always")
  expect_length(direct$failed, 0L)
  expect_true(requireNamespace("noversionpkg", quietly = TRUE))
  mismatch_warnings <- character()
  direct_mismatch <- withCallingHandlers(
    install_local_pkg(mismatch, verbose = FALSE, upgrade = "always"),
    warning = function(condition) {
      mismatch_warnings <<- c(mismatch_warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("filename suggests version 1.0.0", mismatch_warnings,
                        fixed = TRUE)))
  expect_length(direct_mismatch$failed, 0L)
  expect_true(requireNamespace("declaredpkg", quietly = TRUE))

  generated <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), generated)
  sys.source(file.path(result$path, "R", "install_packages.R"), generated)
  sys.source(file.path(result$path, "R", "attach.R"), generated)
  generated_deps <- suppressWarnings(generated$identityverse_deps(
    file.path(result$path, "inst", "archives")
  ))
  expect_true("declaredpkg" %in% generated_deps)
  generated_result <- suppressWarnings(generated$install_packages_in_order(
    result$archives, file.path(result$path, "inst", "archives"), NULL,
    verbose = FALSE, upgrade = "always"
  ))
  expect_length(generated_result$failed, 0L)
  expect_true(requireNamespace("noversionpkg", quietly = TRUE))
})

test_that("an omitted dependency in a later source is rejected with its source", {
  sandbox <- tempfile("bigbang-omitted-source-")
  source_root <- file.path(sandbox, "sources")
  first_dir <- file.path(sandbox, "first")
  second_dir <- file.path(sandbox, "second")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(first_dir)
  dir.create(second_dir)
  dir.create(destination)
  make_input_archive(
    "orphaninput", "1.0.0", file.path(first_dir, "orphaninput_1.0.0.tar.gz"),
    source_root, imports = "laterinput"
  )
  later <- make_input_archive(
    "laterinput", "1.0.0", file.path(second_dir, "laterinput_1.0.0.tar.gz"),
    source_root
  )
  expect_error(
    create_metapackage(
      "omittedverse", file.path(first_dir, "orphaninput_1.0.0.tar.gz"),
      pkg_dir = c(first_dir, second_dir), dest_dir = destination,
      document = FALSE, verbose = FALSE, force_deps = character()
    ),
    regexp = paste0("orphaninput.*laterinput.*", normalizePath(later)),
    class = "bigbang_error_unincluded_dependency"
  )
})

test_that("archive basenames from different sources cannot overwrite", {
  sandbox <- tempfile("bigbang-basename-collision-")
  source_root <- file.path(sandbox, "sources")
  first_dir <- file.path(sandbox, "first")
  second_dir <- file.path(sandbox, "second")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(first_dir)
  dir.create(second_dir)
  dir.create(destination)
  first <- make_input_archive(
    "collisionone", "1.0.0", file.path(first_dir, "same.tar.gz"), source_root
  )
  second <- make_input_archive(
    "collisiontwo", "1.0.0", file.path(second_dir, "same.tar.gz"), source_root
  )
  expect_error(
    suppressWarnings(create_metapackage(
      "collisionverse", c(first, second), dest_dir = destination,
      document = FALSE, verbose = FALSE, force_deps = character()
    )),
    class = "bigbang_error_archive_basename_collision",
    regexp = paste0(normalizePath(first), ".*", normalizePath(second))
  )
})

test_that("multiple external sources are represented in the generated installer", {
  sandbox <- tempfile("bigbang-external-sources-")
  source_root <- file.path(sandbox, "sources")
  first_dir <- file.path(sandbox, "first")
  second_dir <- file.path(sandbox, "second")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(first_dir)
  dir.create(second_dir)
  dir.create(destination)
  first <- make_input_archive(
    "externalone", "1.0.0", file.path(first_dir, "externalone_1.0.0.tar.gz"),
    source_root
  )
  second <- make_input_archive(
    "externaltwo", "1.0.0", file.path(second_dir, "externaltwo_1.0.0.tar.gz"),
    source_root
  )
  expect_warning(
    result <- create_metapackage(
      "externalverse", c(first, second), dest_dir = destination,
      include_archives = FALSE, document = FALSE, verbose = FALSE,
      force_deps = character()
    ),
    "include_archives = TRUE"
  )
  install_file <- file.path(result$path, "R", "attach.R")
  install_text <- paste(readLines(install_file, warn = FALSE), collapse = "\n")
  expect_match(install_text, "pkg_dir", fixed = TRUE)
  expect_false(grepl(normalizePath(first_dir), install_text, fixed = TRUE))
  expect_false(grepl(normalizePath(second_dir), install_text, fixed = TRUE))

  generated <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), generated)
  sys.source(file.path(result$path, "R", "install_packages.R"), generated)
  resolved <- generated$resolve_component_archive(
    "externaltwo_1.0.0", c(first_dir, second_dir), NULL
  )
  expect_identical(normalizePath(resolved$path), normalizePath(second))
})
