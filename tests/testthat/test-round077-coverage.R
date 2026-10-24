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

  captured <- capture_warnings(expect_error(
    runtime$r077repositoryverse_install(
      pkg_dir = dirname(archive), cran_deps = "install",
      repos = "file:///r077-repository-does-not-exist", lib = library_dir,
      verbose = FALSE
    ),
    class = "bigbang_error_install"
  ))
  installation_error <- captured$value
  expect_length(captured$warnings, 1L)
  expect_match(
    captured$warnings[[1L]],
    "cannot open compressed file '.*r077-repository-does-not-exist/src/contrib/PACKAGES",
    fixed = FALSE
  )
  expect_match(captured$warnings[[1L]], "Installation failed for r077repository_1.0.0")
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

  planned <- expect_no_warning(tryCatch(
    runtime$r077libraryverse_deps(
      pkg_dir = dirname(archive), plan = TRUE, lib = library_file
    ), error = identity
  ))
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

test_that("an unwritable library ancestor is reported by a read-only plan", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if(identical(Sys.info()[["effective_user"]], "root"),
          "root can write through mode bits")
  root <- tempfile("bigbang-r077-library-unwritable-")
  dir.create(root)
  withr::defer({
    Sys.chmod(root, "0755")
    unlink(root, recursive = TRUE, force = TRUE)
  })
  source_root <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  dir.create(source_root)
  dir.create(destination)
  archive <- .r077_make_archive(source_root, "r077unwritable")
  generated <- round075_create_metapackage(
    "r077unwritableverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  lib <- file.path(root, "missing", "library")
  Sys.chmod(root, "0555")
  before <- list.files(root, all.files = TRUE, recursive = TRUE)
  warning_condition <- NULL
  plan <- withCallingHandlers(
    runtime$r077unwritableverse_deps(
      pkg_dir = dirname(archive), plan = TRUE, lib = lib
    ),
    warning = function(condition) {
      warning_condition <<- condition
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(plan, "bigbang_dependency_plan")
  expect_s3_class(warning_condition, "bigbang_warning_install_library")
  expect_identical(
    list.files(root, all.files = TRUE, recursive = TRUE), before
  )
})

test_that("a creatable library does not warn during planning", {
  skip_on_cran()
  root <- tempfile("bigbang-r077-library-creatable-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  source_root <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  dir.create(source_root)
  dir.create(destination)
  archive <- .r077_make_archive(source_root, "r077creatable")
  generated <- round075_create_metapackage(
    "r077creatableverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  lib <- file.path(root, "new", "library")
  plan <- expect_no_warning(runtime$r077creatableverse_deps(
    pkg_dir = dirname(archive), plan = TRUE, lib = lib
  ))
  expect_s3_class(plan, "bigbang_dependency_plan")
  expect_false(dir.exists(lib))
})

test_that("library feasibility warnings match both installers", {
  skip_on_os("windows")
  skip_if(identical(Sys.info()[["effective_user"]], "root"),
          "root can write through mode bits")
  root <- tempfile("bigbang-r080c-library-feasibility-")
  dir.create(root)
  withr::defer({
    if ("package:r080clibrary" %in% search()) {
      detach("package:r080clibrary", character.only = TRUE, unload = TRUE)
    }
    if (isNamespaceLoaded("r080clibrary")) {
      try(unloadNamespace("r080clibrary"), silent = TRUE)
    }
  })
  withr::defer({
    for (path in list.files(root, all.files = TRUE, full.names = TRUE,
                            recursive = FALSE)) {
      if (dir.exists(path)) Sys.chmod(path, "0755")
    }
    Sys.chmod(root, "0755")
    unlink(root, recursive = TRUE, force = TRUE)
  })
  source_root <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  dir.create(source_root)
  dir.create(destination)
  archive <- .r077_make_archive(source_root, "r080clibrary")
  generated <- round075_create_metapackage(
    "r080clibraryverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  deps <- runtime$r080clibraryverse_deps
  installer <- runtime$r080clibraryverse_install

  capture_plan <- function(lib, upgrade = "always") {
    condition <- NULL
    value <- withCallingHandlers(
      deps(pkg_dir = dirname(archive), plan = TRUE, lib = lib,
           upgrade = upgrade),
      warning = function(item) {
        condition <<- item
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warning = condition)
  }
  expect_error_class <- function(expression, class) {
    condition <- tryCatch(expression, error = identity)
    expect_s3_class(condition, class)
    condition
  }
  expect_invalid_install <- function(lib) {
    generated_error <- expect_error_class(
      installer(pkg_dir = dirname(archive), lib = lib, verbose = FALSE),
      "bigbang_error_install_policy"
    )
    direct_error <- expect_error_class(
      install_local_pkg(archive, lib = lib, verbose = FALSE),
      "bigbang_error_install_policy"
    )
    expect_identical(conditionMessage(generated_error),
                     conditionMessage(direct_error))
    invisible(NULL)
  }

  dangling <- file.path(root, "dangling-library")
  expect_true(file.symlink("missing-target", dangling))
  dangling_plan <- capture_plan(dangling)
  expect_s3_class(dangling_plan$warning, "bigbang_warning_install_library")
  expect_identical(dangling_plan$warning$reason, "not_creatable")
  expect_invalid_install(dangling)

  dangling_parent <- file.path(root, "dangling-parent")
  expect_true(file.symlink("missing-parent", dangling_parent))
  dangling_child <- file.path(dangling_parent, "library")
  parent_plan <- capture_plan(dangling_child)
  expect_s3_class(parent_plan$warning, "bigbang_warning_install_library")
  expect_identical(parent_plan$warning$reason, "not_creatable")
  expect_invalid_install(dangling_child)

  cycle_root <- file.path(root, "cycle")
  dir.create(cycle_root)
  expect_true(file.symlink("cycle-b", file.path(cycle_root, "cycle-a")))
  expect_true(file.symlink("cycle-a", file.path(cycle_root, "cycle-b")))
  cycle_lib <- file.path(cycle_root, "cycle-a", "library")
  cycle_plan <- capture_plan(cycle_lib)
  expect_s3_class(cycle_plan$warning, "bigbang_warning_install_library")
  expect_identical(cycle_plan$warning$reason, "not_creatable")
  expect_invalid_install(cycle_lib)

  regular <- file.path(root, "regular")
  writeLines("not a directory", regular)
  regular_slash <- paste0(regular, "/")
  regular_plan <- expect_error_class(
    deps(pkg_dir = dirname(archive), plan = TRUE, lib = regular_slash),
    "bigbang_error_install_policy"
  )
  expect_invalid_install(regular_slash)
  expect_identical(
    conditionMessage(regular_plan),
    paste0("Could not create installation library: ", regular_slash)
  )

  no_search <- file.path(root, "no-search")
  dir.create(no_search)
  Sys.chmod(no_search, "0200")
  no_search_lib <- file.path(no_search, "library")
  no_search_plan <- capture_plan(no_search_lib)
  expect_s3_class(no_search_plan$warning, "bigbang_warning_install_library")
  expect_identical(no_search_plan$warning$reason, "not_creatable")
  expect_invalid_install(no_search_lib)

  existing <- file.path(root, "existing")
  dir.create(existing)
  Sys.chmod(existing, "0555")
  existing_plan <- capture_plan(existing)
  expect_s3_class(existing_plan$warning, "bigbang_warning_install_library")
  expect_identical(existing_plan$warning$reason, "not_writable")
  generated_existing <- capture_warnings(tryCatch(
    installer(pkg_dir = dirname(archive), lib = existing, verbose = FALSE),
    error = identity
  ))
  direct_existing <- capture_warnings(tryCatch(
    install_local_pkg(archive, lib = existing, verbose = FALSE),
    error = identity
  ))
  expect_length(generated_existing$warnings, 1L)
  expect_length(direct_existing$warnings, 0L)
  generated_existing <- generated_existing$value
  direct_existing <- direct_existing$value
  expect_true(inherits(generated_existing, "bigbang_error_install"))
  expect_true(
    inherits(direct_existing, "bigbang_error_install") ||
      (is.list(direct_existing) && length(direct_existing$failed) > 0L)
  )

  kept <- file.path(root, "kept")
  dir.create(kept)
  install_local_pkg(archive, lib = kept, upgrade = "always", verbose = FALSE)
  Sys.chmod(kept, "0555")
  kept_plan <- capture_plan(kept, upgrade = "never")
  expect_null(kept_plan$warning)
  expect_identical(kept_plan$value$components$action, "keep")
  kept_generated <- expect_no_warning(installer(
    pkg_dir = dirname(archive), lib = kept, upgrade = "never", verbose = FALSE
  ))
  kept_direct <- expect_no_warning(install_local_pkg(
    archive, lib = kept, upgrade = "never", verbose = FALSE
  ))
  expect_true("r080clibrary_1.0.0" %in% names(kept_generated$unchanged))
  expect_true("r080clibrary_1.0.0" %in% names(kept_direct$unchanged))

  creatable <- file.path(root, "new", "library")
  creatable_plan <- capture_plan(creatable)
  expect_null(creatable_plan$warning)
  expect_no_warning(installer(
    pkg_dir = dirname(archive), lib = creatable, verbose = FALSE
  ))
})

test_that("a non-creatable library is warned about even when all components skip", {
  skip_on_os("windows")
  skip_if(identical(Sys.info()[["effective_user"]], "root"),
          "root can write through mode bits")
  root <- tempfile("bigbang-r080d-library-skip-")
  dir.create(root)
  withr::defer({
    Sys.chmod(root, "0755")
    unlink(root, recursive = TRUE, force = TRUE)
  })
  source_root <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  dir.create(source_root)
  dir.create(destination)
  archive <- .r077_make_archive(
    source_root, "r080dskip", imports = "r080d_missing (>= 99.0.0)"
  )
  generated <- round075_create_metapackage(
    "r080dskipverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  )
  runtime <- .r077_runtime(generated$path)
  dangling <- file.path(root, "dangling")
  expect_true(file.symlink("missing-target", dangling))
  restricted <- file.path(root, "restricted")
  dir.create(restricted)
  Sys.chmod(restricted, "0100")
  withr::defer(Sys.chmod(restricted, "0755"))

  for (lib in c(dangling, file.path(restricted, "library"))) {
    warning_condition <- NULL
    plan <- withCallingHandlers(
      runtime$r080dskipverse_deps(
        pkg_dir = dirname(archive), plan = TRUE, lib = lib,
        cran_deps = "skip"
      ),
      warning = function(condition) {
        warning_condition <<- condition
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(plan$components$action, "skip")
    expect_s3_class(warning_condition, "bigbang_warning_install_library")
    expect_identical(warning_condition$reason, "not_creatable")
    expect_error(
      runtime$r080dskipverse_install(
        pkg_dir = dirname(archive), lib = lib, cran_deps = "skip",
        verbose = FALSE
      ),
      class = "bigbang_error_install_policy"
    )
    expect_error(
      install_local_pkg(
        archive, lib = lib, cran_deps = "skip", verbose = FALSE
      ),
      class = "bigbang_error_install_policy"
    )
  }
})

test_that("inventory warnings survive successful reads in every public lookup", {
  skip_on_os("windows")
  root <- tempfile("bigbang-r080d-inventory-warning-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  dir.create(source_root)
  dir.create(archives)
  child <- .r077_make_archive(source_root, "r080dnonportable")
  child_root <- file.path(source_root, "r080dnonportable-source")
  writeLines("aux_value <- function() 80L",
             file.path(child_root, "R", "aux.R"))
  unlink(child)
  withr::with_dir(source_root, utils::tar(
    child, basename(child_root), compression = "gzip"
  ))
  parent <- .r077_make_archive(
    source_root, "r080dparent", imports = "r080dnonportable"
  )
  file.copy(c(child, parent), archives)
  child <- file.path(archives, basename(child))
  parent <- file.path(archives, basename(parent))
  withr::local_options(bigbang.archive.portable_names = FALSE)
  capture <- function(expr) {
    warnings <- list()
    value <- withCallingHandlers(
      force(expr),
      warning = function(condition) {
        warnings[[length(warnings) + 1L]] <<- condition
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warnings)
  }
  expect_nonportable <- function(result) {
    expect_length(result$warnings, 1L)
    expect_s3_class(
      result$warnings[[1L]], "bigbang_warning_archive_nonportable_name"
    )
  }

  metadata <- capture(bigbang:::extract_dependencies(
    child, pkg_dir = archives
  ))
  expect_nonportable(metadata)
  identity <- capture(bigbang:::.archives_for_package_identity(
    "r080dnonportable", archives
  ))
  expect_nonportable(identity)

  direct <- capture(install_local_pkg(
    child, pkg_dir = archives, lib = file.path(root, "direct"),
    verbose = FALSE, upgrade = "always"
  ))
  expect_nonportable(direct)
  parent_result <- capture(install_local_pkg(
    parent, pkg_dir = archives, lib = file.path(root, "parent"),
    verbose = FALSE, upgrade = "always"
  ))
  expect_nonportable(parent_result)
  by_name <- capture(install_local_pkg(
    "r080dnonportable", pkg_dir = archives,
    lib = file.path(root, "by-name"), verbose = FALSE, upgrade = "always"
  ))
  expect_nonportable(by_name)

  inventory <- capture(bigbang:::.archive_inventory(archives))
  expect_nonportable(inventory)
  diagnosed <- capture(diagnose_dependencies(
    "r080dnonportable", pkg_dir = archives
  ))
  expect_nonportable(diagnosed)
  generated <- capture(create_metapackage(
    "r080dnonportableverse", "r080dnonportable", pkg_dir = archives,
    dest_dir = file.path(root, "generated"), document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  ))
  expect_nonportable(generated)
})

test_that("unreadable archive inventory emits one typed warning", {
  skip_on_os("windows")
  skip_if(identical(Sys.info()[["effective_user"]], "root"),
          "root can read through mode bits")
  root <- tempfile("bigbang-r080c-inventory-permission-")
  dir.create(root)
  withr::defer({
    Sys.chmod(file.path(root, "unreadable_1.0.0.tar.gz"), "0644")
    unlink(root, recursive = TRUE, force = TRUE)
  })
  source_root <- file.path(root, "sources")
  dir.create(source_root)
  valid <- .r077_make_archive(source_root, "r080cvalid")
  unreadable <- file.path(root, "unreadable_1.0.0.tar.gz")
  file.copy(valid, unreadable)
  file.copy(valid, root)
  Sys.chmod(unreadable, "0000")
  capture <- function(expr) {
    warnings <- list()
    value <- withCallingHandlers(
      force(expr),
      warning = function(condition) {
        warnings[[length(warnings) + 1L]] <<- condition
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warnings)
  }
  expect_inventory_warning <- function(result) {
    expect_length(result$warnings, 1L)
    expect_s3_class(
      result$warnings[[1L]], "bigbang_warning_archive_inventory"
    )
    expect_match(
      result$warnings[[1L]]$reason,
      "cannot open file|Permission denied", ignore.case = TRUE
    )
  }
  expect_inventory_warning(capture(
    install_local_pkg(valid, pkg_dir = root, lib = file.path(root, "lib"),
                      verbose = FALSE)
  ))
  expect_inventory_warning(capture(
    install_local_pkg("r080cvalid", pkg_dir = root,
                      lib = file.path(root, "by-name-lib"), verbose = FALSE)
  ))
  expect_inventory_warning(capture(
    diagnose_dependencies("r080cvalid", pkg_dir = root)
  ))
  expect_inventory_warning(capture(
    create_metapackage(
      "r080cvalidverse", "r080cvalid", pkg_dir = root,
      dest_dir = file.path(root, "generated"), document = FALSE,
      verbose = FALSE, import_deps = character(), force_deps = character()
    )
  ))
})

test_that("local installer reports circular archive dependencies", {
  skip_on_cran()
  root <- tempfile("bigbang-r080c-cycle-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  .r077_make_archive(
    root, "r080ccyclea", imports = "r080ccycleb"
  )
  top <- .r077_make_archive(
    root, "r080ccycleb", imports = "r080ccyclea"
  )
  result <- install_local_pkg(
    top, pkg_dir = root, lib = file.path(root, "library"),
    upgrade = "always", verbose = FALSE
  )
  expect_true(any(grepl("Circular local dependency", result$failed,
                        fixed = TRUE)))
  expect_true(any(grepl("Circular local dependency", result$skipped,
                        fixed = TRUE)) || length(result$skipped) == 0L)
})

test_that("local installer rejects a Windows binary ZIP on POSIX", {
  skip_on_cran()
  skip_on_os("windows")
  root <- tempfile("bigbang-r080c-binary-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- file.path(root, "r080cbinary_1.0.0.zip")
  package_root <- file.path(root, "r080cbinary")
  dir.create(file.path(package_root, "Meta"), recursive = TRUE)
  writeLines(c(
    "Package: r080cbinary", "Version: 1.0.0", "Title: Binary fixture",
    "Description: Binary fixture.", "License: MIT"
  ), file.path(package_root, "DESCRIPTION"))
  writeBin(charToRaw("rds"), file.path(package_root, "Meta", "package.rds"))
  withr::with_dir(package_root, utils::zip(
    archive, files = c("DESCRIPTION", "Meta/package.rds"), flags = "-q"
  ))
  result <- install_local_pkg(
    archive, lib = file.path(root, "library"),
    upgrade = "always", verbose = FALSE
  )
  expect_match(result$failed[[1L]], "Windows binary ZIP", fixed = TRUE)
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
  archive <- .r077_make_archive(source_root, "r077libraryparity")
  file.copy(archive, archives, overwrite = TRUE)
  seeded <- install_local_pkg(
    archive, lib = lib2, upgrade = "always", verbose = FALSE
  )
  expect_length(seeded$failed, 0L)
  generated <- round075_create_metapackage(
    "r077libraryparityverse", archive, dest_dir = destination,
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

  plan <- runtime$r077libraryparityverse_deps(
    pkg_dir = archives, plan = TRUE, upgrade = "never"
  )
  expect_identical(plan$components$action, "keep")
  expect_identical(plan$components$reason, "upgrade_never")
  expect_identical(
    normalizePath(plan$components$installed_library, winslash = "/"),
    normalizePath(lib2, winslash = "/")
  )

  generated_result <- expect_no_warning(runtime$r077libraryparityverse_install(
    pkg_dir = archives, upgrade = "never", verbose = FALSE
  ))
  expect_length(generated_result$unchanged, 1L)
  expect_length(generated_result$installed, 0L)
  expect_false(dir.exists(file.path(lib1, "r077libraryparity")))

  explicit_result <- expect_no_warning(runtime$r077libraryparityverse_install(
    pkg_dir = archives, upgrade = "never", lib = explicit, verbose = FALSE
  ))
  expect_true("r077libraryparity_1.0.0" %in% names(explicit_result$installed))
  expect_true(dir.exists(file.path(explicit, "r077libraryparity")))
})

test_that("generated installation attaches retained components from lib2", {
  skip_on_cran()
  # Under R CMD check on Windows, callr's profiles would hijack .libPaths() in
  # the child installers; see test-group-c-features.R for the full reason.
  withr::local_envvar(c(
    R_ENVIRON = "", R_ENVIRON_USER = "", R_PROFILE = "", R_PROFILE_USER = ""
  ))
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
  captured <- capture_warnings(round075_create_metapackage(
    "r077warningverse", alias, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  ))
  generated <- captured$value
  expect_warning_set(captured$warnings, c(
    "declares package r077warning, but its filename suggests alias",
    "declares version 1.0.0, but its filename suggests version 9.9"
  ))
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
