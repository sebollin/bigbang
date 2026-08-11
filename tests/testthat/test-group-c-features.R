group_c_make_component <- function(root, name, version = "0.1.0",
                                   imports = NULL, body = NULL) {
  package_dir <- file.path(root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  fields <- c(
    paste0("Package: ", name),
    paste0("Version: ", version),
    paste0("Title: Group C fixture ", name),
    paste0("Description: Temporary fixture for the Group C tests."),
    "License: MIT",
    "Maintainer: Test Author <test@example.org>",
    "Author: Test Author"
  )
  if (!is.null(imports)) fields <- c(fields, paste0("Imports: ", imports))
  writeLines(fields, file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(fixture_value)", file.path(package_dir, "NAMESPACE"),
             useBytes = TRUE)
  writeLines(
    if (is.null(body)) "fixture_value <- function() 1L" else body,
    file.path(package_dir, "R", "value.R"), useBytes = TRUE
  )
  package_dir
}

group_c_archive <- function(source_root, archive_dir, name, version = "0.1.0",
                            imports = NULL, body = NULL) {
  group_c_make_component(
    source_root, name, version = version, imports = imports, body = body
  )
  withr::with_dir(source_root, utils::tar(
    file.path(archive_dir, paste0(name, "_", version, ".tar.gz")),
    name, compression = "gzip"
  ))
  file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
}

test_that("dry_run validates without creating its destination", {
  archives <- tempfile("bigbang-group-c-dry-")
  dir.create(archives)
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  destination <- file.path(tempdir(), "bigbang-dry-run-destination")
  unlink(destination, recursive = TRUE)
  result <- create_metapackage(
    "dryrunverse", toy, dest_dir = destination, document = FALSE,
    verbose = FALSE, dry_run = TRUE, import_deps = character(),
    force_deps = character()
  )
  expect_true(isTRUE(result$dry_run))
  expect_false(dir.exists(destination))
  expect_true(any(result$files == "DESCRIPTION"))
  expect_identical(result$order, "toycomponent")
})

test_that("dry_run reports optional planned files and internal helpers", {
  archives <- system.file("extdata", package = "bigbang")
  destination <- file.path(tempdir(), "bigbang-dry-run-options")
  unlink(destination, recursive = TRUE)
  result <- create_metapackage(
    "dryoptions", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), include_archives = FALSE,
    dry_run = TRUE, workflow = c(stage = "toycomponent")
  )
  expect_false(dir.exists(destination))
  expect_true("vignettes/workflow-dryoptions.Rmd" %in% result$files)
  expect_false(any(grepl("inst/archives", result$files, fixed = TRUE)))
  expect_true(nzchar(.bb_generator_version()))
  expect_true(is.na(.file_digest(file.path(tempdir(), "no-such-file"))))
  expect_true(any(grepl("reexports.R", .planned_generation_files(
    "xverse", list(), reexport = TRUE
  ), fixed = TRUE)))
})

test_that("a manifest file resolves relative component paths", {
  root <- tempfile("bigbang-group-c-manifest-")
  archives <- file.path(root, "archives")
  dir.create(archives, recursive = TRUE)
  group_c_archive(file.path(root, "sources"), archives, "manifestpkg")
  manifest <- file.path(root, "components.txt")
  writeLines(c(
    "# component list",
    "",
    "archives/manifestpkg_0.1.0.tar.gz"
  ), manifest, useBytes = TRUE)
  destination <- file.path(root, "destination")
  dir.create(destination)
  result <- create_metapackage(
    "manifestverse", manifest, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  expect_identical(result$packages, "manifestpkg")
})

test_that("component resolution guards report actionable errors", {
  manifest <- tempfile("bigbang-empty-manifest-")
  writeLines(c("# only comments", "", "  # another comment"), manifest,
             useBytes = TRUE)
  expect_error(.expand_package_manifest(manifest),
               "does not list any packages")
  expect_error(.normalize_archive_dirs(file.path(tempdir(), "missing-dir")),
               "does not exist")
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  expanded <- .expand_package_manifest(toy)
  expect_identical(expanded$packages, toy)
  expect_error(.resolve_archive_input("not-present", NULL),
               "no 'pkg_dir' was supplied")
  expect_error(.resolve_archive_input("not-present", tempdir()),
               "does not exist")
  expect_true(.version_satisfies("1.2.0", ">=", "1.0.0"))
  expect_false(.version_satisfies("not-a-version", ">=", "1.0.0"))
  expect_warning(
    .resolve_r_requirement(list(list(
      package = "fixture", constraints = list(
        list(package = "R", op = "<", version = "4.0.0")
      )
    ))),
    "only >= and >"
  )
})

test_that("component source directories are built in a temporary archive", {
  skip_if_not_installed("pkgbuild")
  root <- tempfile("bigbang-group-c-source-")
  source_root <- file.path(root, "sources", "directorypkg")
  destination <- file.path(root, "destination")
  dir.create(file.path(root, "sources"), recursive = TRUE)
  dir.create(destination, recursive = TRUE)
  group_c_make_component(file.path(root, "sources"), "directorypkg")
  result <- create_metapackage(
    "sourceverse", source_root, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  expect_identical(result$packages, "directorypkg")
  expect_true(file.exists(file.path(
    result$path, "inst", "archives", "directorypkg_0.1.0.tar.gz"
  )))
  expect_false(file.exists(file.path(source_root, "directorypkg_0.1.0.tar.gz")))
  expect_error(
    create_metapackage(
      "sourceexternal", source_root, dest_dir = file.path(root, "external"),
      include_archives = FALSE, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character()
    ),
    "include_archives = TRUE"
  )
})

test_that("component errors can be skipped transitively", {
  root <- tempfile("bigbang-group-c-skip-")
  good <- file.path(root, "good")
  bad <- file.path(root, "bad")
  dir.create(good, recursive = TRUE)
  dir.create(bad)
  bad_file <- file.path(bad, "broken_0.1.0.tar.gz")
  broken_source <- file.path(root, "broken-source")
  group_c_archive(broken_source, bad, "broken")
  bytes <- readBin(bad_file, "raw", n = file.info(bad_file)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], bad_file)
  group_c_archive(good, good, "dependent", imports = "broken")
  group_c_archive(good, good, "keep")
  warnings <- character()
  result <- withCallingHandlers(create_metapackage(
    "skipverse",
    c(bad_file, file.path(good, "dependent_0.1.0.tar.gz"),
      file.path(good, "keep_0.1.0.tar.gz")),
    dest_dir = file.path(root, "destination"), document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    on_component_error = "skip"
  ), warning = function(condition) {
    warnings <<- c(warnings, conditionMessage(condition))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("filename-derived name", warnings, fixed = TRUE)))
  expect_identical(result$packages, "keep")
  expect_setequal(result$omitted$component, c("broken", "dependent"))
})

test_that("update rewrites only an unmodified generated tree", {
  root <- tempfile("bigbang-group-c-update-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  file.copy(toy, archives)
  result <- create_metapackage(
    "updateverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
  unrelated <- file.path(result$path, "user-file.txt")
  writeLines("preserve", unrelated)
  updated <- create_metapackage(
    "updateverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), update = TRUE
  )
  expect_true(isTRUE(updated$updated))
  expect_identical(readLines(unrelated), "preserve")
  # Positive control: this assertion must fail if an update touches an
  # unrelated file; restore the sentinel before the next update attempt.
  writeLines("changed", unrelated)
  expect_failure(expect_identical(readLines(unrelated), "preserve"))
  writeLines("preserve", unrelated)
  writeLines("edited", file.path(result$path, "README.md"))
  expect_error(
    create_metapackage(
      "updateverse", "toycomponent_0.1.0", pkg_dir = archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character(), update = TRUE
    ),
    class = "bigbang_error_modified_generated_file"
  )
})

test_that("update refuses a destination without a generation manifest", {
  root <- tempfile("bigbang-group-c-no-manifest-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  file.copy(toy, archives)
  project <- file.path(destination, "nomani")
  dir.create(project)
  expect_error(
    create_metapackage(
      "nomani", "toycomponent_0.1.0", pkg_dir = archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character(), update = TRUE
    ),
    class = "bigbang_error_missing_manifest"
  )
})

test_that("generated install_upgrade, only, and lib are functional", {
  skip_on_cran()
  root <- tempfile("bigbang-group-c-install-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  library_dir <- file.path(root, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dir.create(library_dir)
  group_c_archive(source_root, archives, "basec")
  group_c_archive(source_root, archives, "topc", imports = "basec")
  result <- create_metapackage(
    "installverse", c("basec_0.1.0", "topc_0.1.0"), pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(),
    install_upgrade = "always"
  )
  environment <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), environment)
  sys.source(file.path(result$path, "R", "install_packages.R"), environment)
  sys.source(file.path(result$path, "R", "attach.R"), environment)
  expect_identical(formals(environment$installverse_install)$upgrade, "always")
  installed <- environment$installverse_install(
    pkg_dir = file.path(result$path, "inst", "archives"),
    only = "topc", lib = library_dir, verbose = FALSE
  )
  expect_true(dir.exists(file.path(library_dir, "basec")))
  expect_true(dir.exists(file.path(library_dir, "topc")))
  expect_identical(installed$pulled_in, "basec")
  expect_error(
    environment$installverse_install(
      pkg_dir = file.path(result$path, "inst", "archives"),
      only = "missing", lib = library_dir, verbose = FALSE
    ),
    class = "bigbang_error_only"
  )
})
