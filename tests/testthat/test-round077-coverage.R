.r077_make_archive <- function(root, name, version = "1.0.0", imports = NULL) {
  package_root <- file.path(root, paste0(name, "-source"))
  dir.create(file.path(package_root, "R"), recursive = TRUE)
  fields <- c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: Round 077 ", name),
    paste0("Version: ", version),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    paste0("Description: Round 077 fixture for ", name, "."),
    "License: MIT"
  )
  if (!is.null(imports)) fields <- c(fields, paste0("Imports: ", imports))
  writeLines(fields, file.path(package_root, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(r077_value)", file.path(package_root, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("r077_value <- function() 77L",
             file.path(package_root, "R", "value.R"), useBytes = TRUE)
  archive <- file.path(root, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(root, utils::tar(archive, basename(package_root),
                                   compression = "gzip"))
  archive
}

.r077_runtime <- function(project) {
  runtime <- new.env(parent = baseenv())
  for (file in list.files(file.path(project, "R"), pattern = "[.]R$",
                          full.names = TRUE)) {
    sys.source(file, runtime)
  }
  runtime
}

.r077_corrupt_gzip_crc <- function(archive) {
  bytes <- readBin(archive, "raw", file.info(archive)$size)
  position <- length(bytes) - 7L
  bytes[[position]] <- as.raw(bitwXor(as.integer(bytes[[position]]), 1L))
  writeBin(bytes, archive)
  archive
}

test_that("the plan marks repository installation as pending and unverified", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-repository-plan-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  library_dir <- file.path(root, "library")
  source_root <- file.path(root, "sources")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  dir.create(library_dir)
  dir.create(source_root)
  archive <- .r077_make_archive(
    source_root, "r077repository",
    imports = "r077_missing (>= 99.0.0)"
  )
  generated <- round075_create_metapackage(
    "r077repositoryverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  before <- list.files(library_dir, all.files = TRUE, recursive = TRUE)
  plan <- runtime$r077repositoryverse_deps(
    pkg_dir = dirname(archive), plan = TRUE, cran_deps = "install",
    repos = "file:///r077-repository-does-not-exist", lib = library_dir
  )
  expect_identical(plan$components$action, "install")
  expect_identical(plan$components$reason,
                   "external_dependency_install_pending")
  expect_identical(plan$external$action, "install_unverified")
  expect_true(plan$repositories_configured)
  expect_identical(before,
                   list.files(library_dir, all.files = TRUE, recursive = TRUE))

  installation_error <- NULL
  expect_warning(
    installation_error <- expect_error(
      withCallingHandlers(
        runtime$r077repositoryverse_install(
          pkg_dir = dirname(archive), cran_deps = "install",
          repos = "file:///r077-repository-does-not-exist", lib = library_dir,
          verbose = FALSE
        ),
        warning = function(condition) {
          if (!grepl("Installation failed", conditionMessage(condition),
                     fixed = TRUE)) invokeRestart("muffleWarning")
        }
      ),
      class = "bigbang_error_install"
    ),
    regexp = "Installation failed"
  )
  expect_match(conditionMessage(installation_error),
               "Missing non-local dependencies", fixed = TRUE)
  expect_false(dir.exists(file.path(library_dir, "r077repository")))
})

test_that("plan and generated installation reject a regular-file library equally", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-library-file-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  source_root <- file.path(root, "sources")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  dir.create(source_root)
  archive <- .r077_make_archive(source_root, "r077library")
  generated <- round075_create_metapackage(
    "r077libraryverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  library_file <- file.path(root, "library-file")
  writeLines("user data", library_file)
  expected <- paste0("Could not create installation library: ", library_file)

  planned <- expect_no_warning(tryCatch(runtime$r077libraryverse_deps(
    pkg_dir = dirname(archive), plan = TRUE, lib = library_file
  ), error = identity))
  installed <- expect_no_warning(tryCatch(runtime$r077libraryverse_install(
    pkg_dir = dirname(archive), lib = library_file, verbose = FALSE
  ), error = identity))
  direct <- expect_no_warning(tryCatch(
    install_local_pkg(archive, lib = library_file, verbose = FALSE),
    error = identity
  ))
  expect_s3_class(planned, "bigbang_error_install_policy")
  expect_s3_class(installed, "bigbang_error_install_policy")
  expect_s3_class(direct, "bigbang_error_install_policy")
  expect_identical(conditionMessage(planned), expected)
  expect_identical(conditionMessage(installed), expected)
  expect_identical(conditionMessage(direct), expected)
  expect_true(file.exists(library_file))
  expect_false(dir.exists(paste0(library_file, ".dir")))
})

test_that("corrupt gzip trailers have a private message and generated parity", {
  withr::local_envvar(c(LC_ALL = "", LANG = "C", LANGUAGE = "en"))
  withr::local_language("en")
  root <- tempfile("bigbang-r077-gzip-trailer-")
  source_root <- file.path(root, "sources")
  dir.create(source_root, recursive = TRUE)
  archive <- .r077_make_archive(source_root, "r077gzip")
  .r077_corrupt_gzip_crc(archive)

  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(
      c(list(message = message, call = NULL), list(...)),
      class = c(class, "bigbang_error", "error", "condition")
    ))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), generated)
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)

  package_error <- tryCatch(package_reader(archive, ".tar.gz"),
                            error = identity)
  generated_error <- tryCatch(generated_reader(archive, ".tar.gz"),
                              error = identity)
  expect_s3_class(package_error, "bigbang_error_archive_format")
  expect_identical(class(package_error), class(generated_error))
  expect_identical(conditionMessage(package_error),
                   conditionMessage(generated_error))
  expect_match(conditionMessage(package_error),
               "could not validate gzip member trailer", fixed = TRUE)
  expect_false(grepl("memDecompress|internal error|crc error",
                     conditionMessage(package_error), ignore.case = TRUE))
})

test_that("only rejects empty, missing, and duplicate selections", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-only-validation-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  library_dir <- file.path(root, "library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dir.create(library_dir)
  archive <- .r077_make_archive(source_root, "r077only")
  file.copy(archive, archives, overwrite = TRUE)
  generated <- round075_create_metapackage(
    "r077onlyverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  invalid <- list(
    empty = character(), missing = NA_character_,
    duplicate = c("r077only", "r077only")
  )
  expected <- "'only' must contain one or more unique component package names."
  for (value in invalid) {
    plan_error <- expect_error(
      runtime$r077onlyverse_deps(
        pkg_dir = archives, plan = TRUE, only = value, lib = library_dir
      ),
      class = "bigbang_error_only"
    )
    install_error <- expect_error(
      runtime$r077onlyverse_install(
        pkg_dir = archives, only = value, lib = library_dir, verbose = FALSE
      ),
      class = "bigbang_error_only"
    )
    expect_identical(conditionMessage(plan_error), expected)
    expect_identical(conditionMessage(install_error), expected)
  }
})

test_that("omitted lib has plan, generated, and direct-install parity", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-library-parity-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  lib1 <- file.path(root, "lib1")
  lib2 <- file.path(root, "lib2")
  explicit <- file.path(root, "explicit")
  for (path in c(source_root, archives, destination, lib1, lib2, explicit)) {
    dir.create(path, recursive = TRUE)
  }
  archive <- .r077_make_archive(source_root, "r077library")
  file.copy(archive, archives, overwrite = TRUE)
  seeded <- install_local_pkg(
    archive, lib = lib2, upgrade = "always", verbose = FALSE
  )
  expect_length(seeded$failed, 0L)
  generated <- round075_create_metapackage(
    "r077libraryverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  withr::local_libpaths(c(lib1, lib2, .libPaths()))

  direct <- install_local_pkg(
    archive, pkg_dir = archives, upgrade = "never", verbose = FALSE
  )
  expect_length(direct$unchanged, 1L)
  expect_length(direct$installed, 0L)

  plan <- runtime$r077libraryverse_deps(
    pkg_dir = archives, plan = TRUE, upgrade = "never"
  )
  expect_identical(plan$components$action, "keep")
  expect_identical(plan$components$reason, "upgrade_never")
  expect_identical(
    normalizePath(plan$components$installed_library, winslash = "/"),
    normalizePath(lib2, winslash = "/")
  )

  generated_result <- expect_no_warning(runtime$r077libraryverse_install(
    pkg_dir = archives, upgrade = "never", verbose = FALSE
  ))
  expect_length(generated_result$unchanged, 1L)
  expect_length(generated_result$installed, 0L)
  expect_false(dir.exists(file.path(lib1, "r077library")))

  explicit_result <- expect_no_warning(runtime$r077libraryverse_install(
    pkg_dir = archives, upgrade = "never", lib = explicit, verbose = FALSE
  ))
  expect_true("r077library_1.0.0" %in% names(explicit_result$installed))
  expect_true(dir.exists(file.path(explicit, "r077library")))
})

test_that("generated installation attaches retained components from lib2", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-library-attach-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  dependency <- .r077_make_archive(source_root, "r077dependency")
  component <- .r077_make_archive(
    source_root, "r077component", imports = "r077dependency"
  )
  file.copy(c(dependency, component), archives, overwrite = TRUE)
  generated <- round075_create_metapackage(
    "r077attachverse", c(dependency, component), dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  original_paths <- .libPaths()
  on.exit(.libPaths(original_paths), add = TRUE)

  for (policy in c("newer", "never")) {
    lib1 <- file.path(root, paste0("lib1-", policy))
    lib2 <- file.path(root, paste0("lib2-", policy))
    dir.create(lib1)
    dir.create(lib2)
    install_local_pkg(dependency, lib = lib2, upgrade = "always",
                      verbose = FALSE)
    withr::with_libpaths(c(lib1, lib2, original_paths), {
      result <- expect_no_warning(runtime$r077attachverse_install(
        pkg_dir = archives, only = "r077component", upgrade = policy,
        verbose = FALSE
      ))
      expect_true("r077dependency_1.0.0" %in% names(result$unchanged))
      expect_true("r077component_1.0.0" %in% names(result$installed))
      attached <- all(c("package:r077dependency", "package:r077component") %in%
                        search())
      expect_true(attached)
      expect_false(dir.exists(file.path(lib1, "r077dependency")))
      for (package in c("r077dependency", "r077component")) {
        entry <- paste0("package:", package)
        if (entry %in% search()) detach(entry, character.only = TRUE,
                                        unload = TRUE)
      }
    })
  }
})

test_that("a replaced archive is rejected by plan and generated installation", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-generated-identity-")
  source_root <- file.path(root, "sources")
  replacement_root <- file.path(root, "replacement")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  library_dir <- file.path(root, "library")
  for (path in c(source_root, replacement_root, archives, destination, library_dir)) {
    dir.create(path, recursive = TRUE)
  }
  original <- .r077_make_archive(source_root, "r077identity")
  file.copy(original, archives, overwrite = TRUE)
  generated <- round075_create_metapackage(
    "r077identityverse", original, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  replacement <- .r077_make_archive(replacement_root, "r077renamed")
  file.copy(
    replacement, file.path(archives, "r077identity_1.0.0.tar.gz"),
    overwrite = TRUE
  )

  plan_error <- expect_error(
    runtime$r077identityverse_deps(
      pkg_dir = archives, plan = TRUE, lib = library_dir
    ),
    class = "bigbang_error_generated_archive_changed"
  )
  expect_match(conditionMessage(plan_error), "changed after generation", fixed = TRUE)

  install_error <- expect_error(
    runtime$r077identityverse_install(
      pkg_dir = archives, lib = library_dir, verbose = FALSE
    ),
    class = "bigbang_error_generated_archive_changed"
  )
  expect_match(conditionMessage(install_error), "changed after generation", fixed = TRUE)
  expect_false(dir.exists(file.path(library_dir, "r077renamed")))
})

test_that("the plan deduplicates archive identity warnings", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-warning-state-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  library_dir <- file.path(root, "library")
  for (path in c(source_root, archives, destination, library_dir)) {
    dir.create(path, recursive = TRUE)
  }
  archive <- .r077_make_archive(source_root, "r077warning")
  alias <- file.path(archives, "alias_9.9.tar.gz")
  file.copy(archive, alias)
  first_warning <- TRUE
  generated <- NULL
  expect_warning(
    generated <- withCallingHandlers(
      round075_create_metapackage(
        "r077warningverse", alias, dest_dir = destination,
        document = FALSE, verbose = FALSE, import_deps = character(),
        force_deps = character()
      ),
      warning = function(condition) {
        if (first_warning) {
          first_warning <<- FALSE
        } else {
          invokeRestart("muffleWarning")
        }
      }
    ),
    regexp = "declares package"
  )
  runtime <- .r077_runtime(generated$path)
  messages <- character()
  plan <- withCallingHandlers(
    runtime$r077warningverse_deps(
      pkg_dir = archives, plan = TRUE, lib = library_dir
    ),
    warning = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(plan, "bigbang_dependency_plan")
  expect_length(messages, 2L)
  expect_length(unique(messages), 2L)
  expect_true(any(grepl("declares package", messages, fixed = TRUE)))
  expect_true(any(grepl("declares version", messages, fixed = TRUE)))
})
