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

test_that("stale generated files can be removed safely", {
  project <- tempfile("bigbang-stale-files-")
  dir.create(project)
  stale <- file.path(project, "old-generated.R")
  writeLines("old", stale, useBytes = TRUE)
  .remove_stale_generation_files(project, "old-generated.R")
  expect_false(file.exists(stale))
})

test_that("duplicate namespace imports are removed", {
  namespace <- tempfile("bigbang-namespace-")
  writeLines(c(
    "importFrom(toy, value)",
    "importFrom(toy,value)",
    "export(other)"
  ), namespace, useBytes = TRUE)
  .deduplicate_namespace_imports(namespace)
  expect_identical(readLines(namespace, warn = FALSE), c(
    "importFrom(toy, value)",
    "export(other)"
  ))
})

test_that("verbose generation completes through every write stage", {
  archives <- system.file("extdata", package = "bigbang")
  destination <- tempfile("bigbang-verbose-generation-")
  result <- create_metapackage(
    "verboseverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = TRUE, debug = TRUE,
    import_deps = character(), force_deps = character()
  )
  expect_true(file.exists(file.path(result$path, "DESCRIPTION")))
  expect_identical(result$order, "toycomponent")
})

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
  expect_false(any(grepl(
    "reexports.R", .planned_generation_files("xverse", list()), fixed = TRUE
  )))
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

test_that("lib is the component destination but not the only dependency library", {
  skip_on_cran()
  # rcmdcheck runs R CMD check through callr, which points R_ENVIRON* and
  # R_PROFILE* at temporary files whose profile calls .libPaths() with the
  # check libraries. The Unix wrapper for R CMD BATCH --vanilla empties those
  # four variables, so descendants never read them; the Windows front-end
  # leaves them set, and every non-vanilla child R spawned here would have its
  # .libPaths() hijacked. Empty them exactly as the Unix wrapper does, so this
  # test asserts the same thing on every platform.
  withr::local_envvar(c(
    R_ENVIRON = "", R_ENVIRON_USER = "", R_PROFILE = "", R_PROFILE_USER = ""
  ))
  root <- tempfile("bigbang-lib-search-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  support_archives <- file.path(root, "support-archives")
  destination <- file.path(root, "destination")
  support_lib <- file.path(root, "support-library")
  direct_control_lib <- file.path(root, "direct-control-library")
  generated_control_lib <- file.path(root, "generated-control-library")
  direct_lib <- file.path(root, "direct-library")
  generated_lib <- file.path(root, "generated-library")
  dirs <- c(source_root, archives, support_archives, destination, support_lib,
            direct_control_lib, generated_control_lib, direct_lib, generated_lib)
  vapply(dirs, dir.create, logical(1L), recursive = TRUE)

  suffix <- as.character(Sys.getpid())
  dependency <- paste0("outside", suffix)
  component <- paste0("target", suffix)
  dependency_archive <- group_c_archive(
    source_root, support_archives, dependency
  )
  component_archive <- group_c_archive(
    source_root, archives, component, imports = dependency
  )
  r <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  install_into_support <- function(archive) {
    system2(
      r, c("CMD", "INSTALL", "-l", shQuote(support_lib), shQuote(archive)),
      stdout = FALSE, stderr = FALSE
    )
  }
  expect_equal(install_into_support(dependency_archive), 0L)
  expect_equal(install_into_support(component_archive), 0L)

  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)

  install_with_only_r_libs <- function(target_lib) {
    .libPaths(old_libs)
    warnings <- character()
    withr::with_envvar(
      c(R_LIBS = support_lib, R_LIBS_USER = NA_character_),
      withCallingHandlers(
        utils::install.packages(
          component_archive, repos = NULL, type = "source",
          dependencies = FALSE, lib = target_lib
        ),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    )
    expect_false(dir.exists(file.path(target_lib, component)))
    expect_true(any(grepl(basename(component_archive), warnings, fixed = TRUE)))
  }

  # R_LIBS alone is overwritten by install.packages(), so it cannot carry the
  # support library to the installation child.
  install_with_only_r_libs(direct_control_lib)

  original_wrapper <- bigbang:::.with_install_library_path
  direct_libraries <- NULL
  testthat::local_mocked_bindings(
    .with_install_library_path = function(libraries, code) {
      direct_libraries <<- libraries
      .libPaths(old_libs)
      original_wrapper(libraries, code)
    },
    .package = "bigbang"
  )
  .libPaths(c(support_lib, old_libs))
  direct <- install_local_pkg(
    component_archive, cran_deps = "skip", verbose = FALSE, lib = direct_lib
  )
  expect_true(normalizePath(support_lib, winslash = "/") %in% direct_libraries)
  expect_length(direct$failed, 0L)
  expect_length(direct$skipped, 0L)
  expect_true(dir.exists(file.path(direct_lib, component)))
  expect_true(dir.exists(file.path(support_lib, dependency)))
  expect_false(dir.exists(file.path(direct_lib, dependency)))

  generated <- create_metapackage(
    "libsearchverse", component_archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(generated$path, "R", "utils.R"), runtime)
  sys.source(file.path(generated$path, "R", "install_packages.R"), runtime)
  sys.source(file.path(generated$path, "R", "attach.R"), runtime)

  install_with_only_r_libs(generated_control_lib)
  emitted_wrapper <- runtime$with_install_library_path
  emitted_libraries <- NULL
  runtime$with_install_library_path <- function(libraries, code) {
    emitted_libraries <<- libraries
    .libPaths(old_libs)
    emitted_wrapper(libraries, code)
  }
  .libPaths(c(support_lib, old_libs))
  emitted <- runtime$libsearchverse_install(
    pkg_dir = file.path(generated$path, "inst", "archives"),
    cran_deps = "skip", verbose = FALSE, lib = generated_lib
  )
  expect_true(normalizePath(support_lib, winslash = "/") %in% emitted_libraries)
  expect_length(emitted$failed, 0L)
  expect_length(emitted$skipped, 0L)
  expect_true(dir.exists(file.path(generated_lib, component)))
  expect_false(dir.exists(file.path(generated_lib, dependency)))
})

test_that("installation subprocesses inherit the complete library path", {
  # rcmdcheck runs R CMD check through callr, which points R_ENVIRON* and
  # R_PROFILE* at temporary files whose profile calls .libPaths() with the
  # check libraries. The Unix wrapper for R CMD BATCH --vanilla empties those
  # four variables, so descendants never read them; the Windows front-end
  # leaves them set, and every non-vanilla child R spawned here would have its
  # .libPaths() hijacked. Empty them exactly as the Unix wrapper does, so this
  # test asserts the same thing on every platform.
  withr::local_envvar(c(
    R_ENVIRON = "", R_ENVIRON_USER = "", R_PROFILE = "", R_PROFILE_USER = ""
  ))
  root <- tempfile("bigbang-install-library-env-")
  first <- file.path(root, "first-library")
  second <- file.path(root, "second-library")
  dir.create(first, recursive = TRUE)
  dir.create(second, recursive = TRUE)

  variables <- c("R_LIBS", "R_LIBS_USER")
  before <- Sys.getenv(variables, unset = NA_character_)
  script <- file.path(root, "child.R")
  writeLines(
    "writeLines(.libPaths(), Sys.getenv('BIGBANG_CHILD_OUTPUT'))",
    script
  )
  output <- file.path(root, "libraries.txt")
  withr::local_envvar(c(BIGBANG_CHILD_OUTPUT = output))
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )

  environment_during_call <- NULL
  status <- bigbang:::.with_install_library_path(
    c(first, second),
    {
      environment_during_call <- Sys.getenv(variables, unset = NA_character_)
      system2(rscript, shQuote(script), stdout = FALSE, stderr = FALSE)
    }
  )

  expect_equal(status, 0L)
  child_libraries <- normalizePath(
    readLines(output, warn = FALSE), winslash = "/", mustWork = TRUE
  )
  expected <- normalizePath(
    c(first, second), winslash = "/", mustWork = TRUE
  )
  child_positions <- match(expected, child_libraries)
  expect_false(anyNA(child_positions))
  expect_true(all(diff(child_positions) > 0L))
  expect_identical(environment_during_call[["R_LIBS"]], before[["R_LIBS"]])
  expect_identical(
    environment_during_call[["R_LIBS_USER"]],
    paste(expected, collapse = .Platform$path.sep)
  )
  expect_identical(Sys.getenv(variables, unset = NA_character_), before)
})
