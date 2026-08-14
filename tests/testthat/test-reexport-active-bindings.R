reexport_make_archive <- function(source_root, archive_dir, name,
                                  version = "0.1.0", exports,
                                  body, namespace_extra = character()) {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name),
    paste0("Version: ", version),
    paste0("Title: Re-export fixture ", name),
    paste0("Description: Temporary fixture for ", name, "."),
    "License: MIT",
    "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(c(
    paste0("export(", paste(exports, collapse = ","), ")"),
    namespace_extra
  ), file.path(package_dir, "NAMESPACE"), useBytes = TRUE)
  writeLines(body, file.path(package_dir, "R", "fixture.R"), useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(source_root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

test_that("reexport active bindings stay lazy and preserve NSE and S3", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-reexport-active-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  archive <- reexport_make_archive(
    source_root, archive_dir, "toya",
    exports = c(
      "toya_capture", "toya_data", "toya_generic", "toya_make", "toya_value"
    ),
    body = c(
      "toya_capture <- function(x) substitute(x)",
      "toya_data <- data.frame(value = 1L)",
      "toya_generic <- function(x) UseMethod('toya_generic')",
      "toya_generic.toya_class <- function(x) 'own S3 method'",
      "toya_make <- function() structure(list(), class = 'toya_class')",
      "print.toya_class <- function(x, ...) cat('foreign S3 method')",
      "toya_value <- function(x = 1) x + 1"
    ),
    namespace_extra = c(
      "S3method(toya_generic,toya_class)",
      "S3method(print,toya_class)"
    )
  )
  result <- bigbang::create_metapackage(
    "toyverse", archive, dest_dir = destination,
    document = TRUE, verbose = FALSE, import_deps = character(),
    force_deps = character(), reexport = TRUE
  )
  description <- readLines(file.path(result$path, "DESCRIPTION"), warn = FALSE)
  expect_false(any(grepl("^Imports:.*toya|^Depends:.*toya", description)))
  expect_true(file.exists(file.path(result$path, "R", "reexports.R")))
  namespace <- readLines(file.path(result$path, "NAMESPACE"), warn = FALSE)
  expect_true(any(grepl("export\\(toya_capture\\)", namespace)))

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  expect_identical(
    system2(r_binary, c("CMD", "INSTALL", "-l", shQuote(meta_library),
                        shQuote(result$path)), stdout = FALSE, stderr = FALSE),
    0L
  )
  withr::local_libpaths(c(meta_library, .libPaths()))
  library(toyverse, quietly = TRUE)
  withr::defer({
    if ("package:toya" %in% search()) {
      detach("package:toya", unload = TRUE, character.only = TRUE)
    }
    if ("package:toyverse" %in% search()) {
      detach("package:toyverse", unload = TRUE, character.only = TRUE)
    }
    if ("toyverse" %in% loadedNamespaces()) unloadNamespace("toyverse")
    if ("toya" %in% loadedNamespaces()) unloadNamespace("toya")
  })
  expect_error(toya_value(1), "Component package 'toya' is not installed")
  missing_data <- getExportedValue("toyverse", "toya_data")
  expect_true(is.function(missing_data))
  expect_error(missing_data(), "Component package 'toya' is not installed")

  toyverse_install(lib = component_library, verbose = FALSE)
  expect_identical(toya_value(1), 2)
  expect_s3_class(getExportedValue("toyverse", "toya_data"), "data.frame")
  expect_error(
    get(".make_reexport_binding", asNamespace("toyverse"))(
      "toya", "toya_value"
    )(1),
    "read-only"
  )
  expect_identical(toya_capture(alpha + beta), quote(alpha + beta))
  expect_identical(toya_generic(toya_make()), "own S3 method")
  expect_match(paste(capture.output(print(toya_make())), collapse = "\n"),
               "foreign S3 method")
})

test_that("reexport rejects export collisions and own generated symbols", {
  sandbox <- tempfile("bigbang-reexport-collision-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  first <- reexport_make_archive(
    source_root, archive_dir, "firstre", exports = "shared_value",
    body = "shared_value <- function() 1L"
  )
  second <- reexport_make_archive(
    source_root, archive_dir, "secondre", exports = "shared_value",
    body = "shared_value <- function() 2L"
  )
  expect_error(
    bigbang::create_metapackage(
      "collisionverse", c(first, second), dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character(), reexport = TRUE
    ),
    "shared_value.*firstre.*secondre"
  )

  own <- reexport_make_archive(
    source_root, archive_dir, "ownre", exports = "ownverse_install",
    body = "ownverse_install <- function() NULL"
  )
  expect_error(
    bigbang::create_metapackage(
      "ownverse", own, dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character(), reexport = TRUE
    ),
    "ownverse_install"
  )

  internal <- reexport_make_archive(
    source_root, archive_dir, "internalre",
    exports = ".reexport_component_value",
    body = ".reexport_component_value <- function() NULL"
  )
  expect_error(
    bigbang::create_metapackage(
      "internalverse", internal, dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character(), reexport = TRUE
    ),
    ".reexport_component_value",
    fixed = TRUE
  )
})

test_that("reexport requires a readable component NAMESPACE", {
  sandbox <- tempfile("bigbang-reexport-namespace-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  package_dir <- file.path(source_root, "nonamespace")
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c("Package: nonamespace", "Version: 0.1.0",
               "Title: Missing namespace", "Description: Fixture.",
               "License: MIT"), file.path(package_dir, "DESCRIPTION"))
  writeLines("value <- function() 1L", file.path(package_dir, "R", "value.R"))
  archive <- file.path(archive_dir, "nonamespace_0.1.0.tar.gz")
  withr::with_dir(source_root, utils::tar(
    archive, "nonamespace", compression = "gzip"
  ))
  expect_error(
    bigbang::create_metapackage(
      "namespaceverse", archive, dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character(), reexport = TRUE
    ),
    "NAMESPACE"
  )
  expect_error(
    bigbang::create_metapackage(
      "namespaceverse2", archive, dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character(), reexport = TRUE,
      on_component_error = "skip"
    ),
    "NAMESPACE"
  )
})

test_that("reexport validates explicit namespace exports and helper plans", {
  expect_true(is.character(bigbang:::.bb_generator_version()))
  testthat::local_mocked_bindings(
    packageVersion = function(...) stop("not available"),
    .package = "utils"
  )
  expect_identical(bigbang:::.bb_generator_version(), "unknown")
  expect_length(bigbang:::.planned_documentation_files("helperverse"), 20L)
  expect_error(
    bigbang::create_metapackage(
      "badreexport", system.file("extdata", "toycomponent_0.1.0.tar.gz",
                                 package = "bigbang"),
      dest_dir = tempfile("bad-reexport-dest-"), reexport = NA,
      document = FALSE, verbose = FALSE
    ),
    "reexport.*TRUE.*FALSE"
  )
  expect_error(
    bigbang::create_metapackage(
      "badreexport2", system.file("extdata", "toycomponent_0.1.0.tar.gz",
                                  package = "bigbang"),
      dest_dir = tempfile("bad-reexport-dest-"), reexport = "yes",
      document = FALSE, verbose = FALSE
    ),
    "reexport.*TRUE.*FALSE"
  )

  component <- list(exports = c("a", "b"))
  expect_true(
    ".reexport_state" %in%
      bigbang:::.generated_metapackage_symbols("helperverse")
  )

  sandbox <- tempfile("namespace-exports-")
  dir.create(sandbox)
  archive_dir <- file.path(sandbox, "archives")
  dir.create(archive_dir)
  export_archive <- reexport_make_archive(
    sandbox, archive_dir, "classre", exports = "classre_value",
    body = "classre_value <- function() structure(list(), class = 'classre')",
    namespace_extra = c("exportClasses(classre)", "exportMethods(classre_method)")
  )
  metadata <- bigbang:::.read_archive_metadata(
    export_archive, include_exports = TRUE
  )
  expect_identical(metadata$exports, "classre_value")

  namespace <- tempfile("namespace-")
  writeLines("export(existing)", namespace)
  bigbang:::.ensure_namespace_exports(namespace, c("existing", "added"))
  expect_true(any(grepl("export\\(added\\)", readLines(namespace))))
  expect_invisible(bigbang:::.ensure_namespace_exports(namespace, character()))
  expect_invisible(bigbang:::.ensure_namespace_exports(
    tempfile("missing-namespace-"), "added"
  ))

  project <- tempfile("reexport-docs-")
  dir.create(file.path(project, "man"), recursive = TRUE)
  bigbang:::.write_reexport_documentation(project, c("added"))
  expect_true(file.exists(file.path(project, "man", "reexports.Rd")))
  expect_invisible(bigbang:::.write_reexport_documentation(project, character()))

  duplicate_namespace <- tempfile("duplicate-namespace-")
  writeLines(c("importFrom(utils, head)", "importFrom(utils, head)"),
             duplicate_namespace)
  bigbang:::.deduplicate_namespace_imports(duplicate_namespace)
  expect_length(readLines(duplicate_namespace), 1L)
})

test_that("S4 namespace directives do not become active bindings", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-reexport-s4-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)
  archive <- reexport_make_archive(
    source_root, archive_dir, "s4re", exports = "s4_value",
    body = c(
      "methods::setClass('s4thing', slots = c(value = 'numeric'))",
      "s4_value <- function(x) methods::new('s4thing', value = x)"
    ),
    namespace_extra = c("import(methods)", "exportClasses(s4thing)")
  )
  generated <- create_metapackage(
    "s4verse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  expect_identical(system2(
    r_binary,
    c("CMD", "INSTALL", "-l", shQuote(meta_library), shQuote(generated$path)),
    stdout = FALSE, stderr = FALSE
  ), 0L)
  expect_identical(system2(
    r_binary,
    c("CMD", "INSTALL", "-l", shQuote(component_library), shQuote(archive)),
    stdout = FALSE, stderr = FALSE
  ), 0L)
  withr::local_libpaths(c(meta_library, component_library, .libPaths()))
  library("s4verse", character.only = TRUE, quietly = TRUE)
  withr::defer({
    if ("package:s4verse" %in% search()) {
      detach("package:s4verse", unload = TRUE, character.only = TRUE)
    }
    if ("s4verse" %in% loadedNamespaces()) unloadNamespace("s4verse")
    if ("s4re" %in% loadedNamespaces()) unloadNamespace("s4re")
  })
  namespace <- asNamespace("s4verse")
  expect_false("s4re" %in% loadedNamespaces())
  expect_s4_class(getExportedValue("s4verse", "s4_value")(1), "s4thing")
  expect_true("s4re" %in% loadedNamespaces())
  expect_false(exists("s4thing", envir = namespace, inherits = FALSE))
  expect_true(methods::isClass("s4thing", where = asNamespace("s4re")))
})

test_that("non-syntactic and Unicode exports remain installable bindings", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-reexport-symbols-")
  source_root <- file.path(sandbox, "sources")
  package_dir <- file.path(source_root, "oddexports")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)
  space_symbol <- "con espacios"
  unicode_symbol <- paste0("a", intToUtf8(0x00f1L), "o")
  writeLines(c(
    "Package: oddexports",
    "Version: 0.1.0",
    "Title: Non-Syntactic Export Fixture",
    "Description: Exercises quoted namespace exports.",
    "License: MIT",
    "Encoding: UTF-8",
    "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(c(
    paste0("export(\"", space_symbol, "\")"),
    paste0("export(\"", unicode_symbol, "\")")
  ), file.path(package_dir, "NAMESPACE"), useBytes = TRUE)
  writeLines(c(
    paste0("`", space_symbol, "` <- function() 11L"),
    paste0("`", unicode_symbol, "` <- function() 22L")
  ), file.path(package_dir, "R", "exports.R"), useBytes = TRUE)
  archive <- file.path(archive_dir, "oddexports_0.1.0.tar.gz")
  withr::with_dir(source_root, utils::tar(
    archive, "oddexports", compression = "gzip"
  ))

  generated <- create_metapackage(
    "symbolverse", archive, dest_dir = destination, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE
  )
  namespace <- readLines(file.path(generated$path, "NAMESPACE"), warn = FALSE)
  expect_true(paste0("export(`", space_symbol, "`)") %in% namespace)
  expect_true("export(\"a\\u00f1o\")" %in% namespace)
  specs <- readLines(file.path(generated$path, "R", "reexports.R"), warn = FALSE)
  expect_true(any(grepl(space_symbol, specs, fixed = TRUE)))
  aliases <- readLines(
    file.path(generated$path, "man", "reexports.Rd"), warn = FALSE
  )
  expect_true(paste0("\\alias{", space_symbol, "}") %in% aliases)
  expect_true(paste0("\\alias{", unicode_symbol, "}") %in% aliases)

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  withr::local_envvar(c(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false"))
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(
    build_status, 0L, info = paste(build_output, collapse = "\n")
  )
  tarball <- file.path(sandbox, "symbolverse_0.1.0.tar.gz")
  expect_true(file.exists(tarball))
  check_output <- withr::with_dir(sandbox, system2(
    r_binary,
    c("CMD", "check", "--as-cran", "--no-manual", shQuote(tarball)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- check_output[grepl("^Status:", check_output)]
  expect_equal(
    length(status), 1L, info = paste(check_output, collapse = "\n")
  )
  expect_false(
    grepl("ERROR|WARNING", status),
    info = paste(check_output, collapse = "\n")
  )
  expect_identical(system2(
    r_binary,
    c("CMD", "INSTALL", "-l", shQuote(meta_library), shQuote(generated$path)),
    stdout = FALSE, stderr = FALSE
  ), 0L)
  expect_identical(system2(
    r_binary,
    c("CMD", "INSTALL", "-l", shQuote(component_library), shQuote(archive)),
    stdout = FALSE, stderr = FALSE
  ), 0L)
  withr::local_libpaths(c(meta_library, component_library, .libPaths()))
  loadNamespace("symbolverse")
  withr::defer({
    if ("symbolverse" %in% loadedNamespaces()) unloadNamespace("symbolverse")
    if ("oddexports" %in% loadedNamespaces()) unloadNamespace("oddexports")
  })
  expect_identical(getExportedValue("symbolverse", space_symbol)(), 11L)
  expect_identical(getExportedValue("symbolverse", unicode_symbol)(), 22L)
})

test_that("reexport rejects namespace export patterns", {
  sandbox <- tempfile("bigbang-reexport-pattern-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(file.path(source_root, "patternre", "R"), recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  writeLines(c("Package: patternre", "Version: 0.1.0",
               "Title: Pattern fixture", "Description: Fixture.",
               "License: MIT"),
             file.path(source_root, "patternre", "DESCRIPTION"))
  writeLines("exportPattern('^[a-z]')",
             file.path(source_root, "patternre", "NAMESPACE"))
  writeLines("pattern_value <- function() 1L",
             file.path(source_root, "patternre", "R", "value.R"))
  archive <- file.path(archive_dir, "patternre_0.1.0.tar.gz")
  withr::with_dir(source_root, utils::tar(
    archive, "patternre", compression = "gzip"
  ))
  expect_error(
    bigbang::create_metapackage(
      "patternverse", archive, dest_dir = destination,
      document = FALSE, verbose = FALSE, import_deps = character(),
      force_deps = character(), reexport = TRUE
    ),
    "export patterns"
  )
})

test_that("reexport handles components with no explicit exports", {
  sandbox <- tempfile("bigbang-reexport-empty-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(file.path(source_root, "emptyre", "R"), recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  writeLines(c("Package: emptyre", "Version: 0.1.0",
               "Title: Empty export fixture", "Description: Fixture.",
               "License: MIT"),
             file.path(source_root, "emptyre", "DESCRIPTION"))
  writeLines(character(), file.path(source_root, "emptyre", "NAMESPACE"))
  writeLines("internal_value <- function() 1L",
             file.path(source_root, "emptyre", "R", "value.R"))
  archive <- file.path(archive_dir, "emptyre_0.1.0.tar.gz")
  withr::with_dir(source_root, utils::tar(
    archive, "emptyre", compression = "gzip"
  ))
  result <- bigbang::create_metapackage(
    "emptyverse", archive, dest_dir = destination,
    document = TRUE, verbose = FALSE, import_deps = character(),
    force_deps = character(), reexport = TRUE
  )
  expect_true(file.exists(file.path(result$path, "R", "reexports.R")))
  expect_match(
    paste(readLines(file.path(result$path, "R", "reexports.R")), collapse = "\n"),
    "component_reexport_specs <- list\\(\\)"
  )
})

test_that("update reconciles the reexport binding file", {
  sandbox <- tempfile("bigbang-reexport-update-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  archive <- reexport_make_archive(
    source_root, archive_dir, "togglere", exports = "toggle_value",
    body = "toggle_value <- function() 1L"
  )
  initial <- bigbang::create_metapackage(
    "toggleverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), reexport = FALSE
  )
  updated <- bigbang::create_metapackage(
    "toggleverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), reexport = TRUE, update = TRUE
  )
  expect_true(file.exists(file.path(updated$path, "R", "reexports.R")))
  expect_true("R/reexports.R" %in% readRDS(
    file.path(updated$path, ".bigbang-manifest.rds")
  )$files)
  reverted <- bigbang::create_metapackage(
    "toggleverse", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), reexport = FALSE, update = TRUE
  )
  expect_false(file.exists(file.path(reverted$path, "R", "reexports.R")))
})

test_that("reexport toggles reconcile code, documentation, and manifests", {
  testthat::skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-reexport-toggle-matrix-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  archive <- reexport_make_archive(
    source_root, archive_dir, "matrixre", exports = "matrix_value",
    body = "matrix_value <- function() 1L"
  )
  for (document in c(FALSE, TRUE)) {
    for (initial_reexport in c(FALSE, TRUE)) {
      name <- paste0(
        "toggle", if (document) "doc" else "nodoc",
        if (initial_reexport) "on" else "off"
      )
      destination <- file.path(sandbox, name)
      dir.create(destination)
      initial <- create_metapackage(
        name, archive, dest_dir = destination, document = document,
        verbose = FALSE, import_deps = character(), force_deps = character(),
        reexport = initial_reexport
      )
      updated <- create_metapackage(
        name, archive, dest_dir = destination, document = document,
        verbose = FALSE, import_deps = character(), force_deps = character(),
        reexport = !initial_reexport, update = TRUE
      )
      expected_code <- !initial_reexport
      expect_identical(
        file.exists(file.path(updated$path, "R", "reexports.R")),
        expected_code
      )
      expect_identical(
        file.exists(file.path(updated$path, "man", "reexports.Rd")),
        document && expected_code
      )
      manifest <- readRDS(file.path(updated$path, .generation_manifest_name))
      actual <- list.files(
        updated$path, recursive = TRUE, all.files = TRUE, no.. = TRUE,
        include.dirs = FALSE
      )
      expect_setequal(actual, c(manifest$files, .generation_manifest_name))
      expect_true(isTRUE(updated$updated), info = initial$path)
    }
  }
})

test_that("reexport updates never overwrite untracked user files", {
  testthat::skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-reexport-untracked-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  archive <- reexport_make_archive(
    source_root, archive_dir, "userre", exports = "user_value",
    body = "user_value <- function() 1L"
  )
  for (relative in c(file.path("R", "reexports.R"),
                     file.path("man", "reexports.Rd"))) {
    name <- if (startsWith(relative, "R")) "usercodeverse" else "userdocverse"
    destination <- file.path(sandbox, name)
    dir.create(destination)
    generated <- create_metapackage(
      name, archive, dest_dir = destination, document = FALSE,
      verbose = FALSE, import_deps = character(), force_deps = character(),
      reexport = FALSE
    )
    user_file <- file.path(generated$path, relative)
    dir.create(dirname(user_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("user-owned content", user_file, useBytes = TRUE)
    before <- readBin(user_file, "raw", n = file.info(user_file)$size)
    expect_error(
      create_metapackage(
        name, archive, dest_dir = destination,
        document = startsWith(relative, "man"),
        verbose = FALSE, import_deps = character(), force_deps = character(),
        reexport = TRUE, update = TRUE
      ),
      class = "bigbang_error_modified_generated_file"
    )
    expect_identical(
      readBin(user_file, "raw", n = file.info(user_file)$size), before
    )
  }
})

test_that("failed documentation leaves a clean reexport toggle retry", {
  testthat::skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-reexport-doc-failure-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  archive <- reexport_make_archive(
    source_root, archive_dir, "retryre", exports = "retry_value",
    body = "retry_value <- function() 1L"
  )
  generated <- create_metapackage(
    "retryverse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = FALSE
  )
  failed <- NULL
  expect_warning(
    failed <- testthat::with_mocked_bindings(
      create_metapackage(
        "retryverse", archive, dest_dir = destination, document = TRUE,
        verbose = FALSE, import_deps = character(), force_deps = character(),
        reexport = TRUE, update = TRUE
      ),
      document = function(...) stop("forced reexport documentation failure"),
      .package = "devtools"
    ),
    "forced reexport documentation failure"
  )
  expect_false(isTRUE(failed$documented))
  expect_false(file.exists(file.path(generated$path, "man", "reexports.Rd")))
  manifest <- readRDS(file.path(generated$path, .generation_manifest_name))
  actual <- list.files(
    generated$path, recursive = TRUE, all.files = TRUE, no.. = TRUE,
    include.dirs = FALSE
  )
  expect_setequal(actual, c(manifest$files, .generation_manifest_name))

  retry <- create_metapackage(
    "retryverse", archive, dest_dir = destination, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE, update = TRUE
  )
  expect_true(isTRUE(retry$documented))
  expect_true(file.exists(file.path(retry$path, "man", "reexports.Rd")))
})

test_that("every generated top-level symbol is reserved from reexport", {
  sandbox <- tempfile("bigbang-reexport-reserved-")
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)
  archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  generated <- create_metapackage(
    "reservedverse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE
  )
  expressions <- unlist(lapply(
    list.files(file.path(generated$path, "R"), pattern = "\\.R$", full.names = TRUE),
    function(path) as.list(parse(path))
  ), recursive = FALSE)
  assignments <- Filter(function(expression) {
    is.call(expression) && identical(as.character(expression[[1L]])[[1L]], "<-") &&
      is.symbol(expression[[2L]])
  }, expressions)
  defined <- unique(vapply(
    assignments, function(expression) as.character(expression[[2L]]), character(1L)
  ))
  expect_setequal(
    intersect(defined, .generated_metapackage_symbols("reservedverse")),
    defined
  )
})

test_that("generated installers create a new lib in both startup modes", {
  testthat::skip_on_cran()
  withr::defer({
    if ("package:toycomponent" %in% search()) {
      detach("package:toycomponent", unload = TRUE, character.only = TRUE)
    }
    if ("toycomponent" %in% loadedNamespaces()) unloadNamespace("toycomponent")
  })
  sandbox <- tempfile("bigbang-reexport-new-lib-")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  dir.create(destination, recursive = TRUE)
  dir.create(meta_library)
  archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  for (reexport in c(FALSE, TRUE)) {
    name <- if (reexport) "newlibreexport" else "newlibattach"
    generated <- create_metapackage(
      name, archive, dest_dir = destination, document = FALSE,
      verbose = FALSE, import_deps = character(), force_deps = character(),
      reexport = reexport
    )
    expect_identical(system2(
      r_binary,
      c("CMD", "INSTALL", "-l", shQuote(meta_library), shQuote(generated$path)),
      stdout = FALSE, stderr = FALSE
    ), 0L)
    component_library <- file.path(sandbox, paste0(name, "-components"))
    withr::local_libpaths(c(meta_library, .libPaths()))
    loadNamespace(name)
    installer <- getExportedValue(name, paste0(name, "_install"))
    result <- installer(lib = component_library, verbose = FALSE)
    expect_length(result$failed, 0L)
    expect_true(dir.exists(file.path(component_library, "toycomponent")))
    unloadNamespace(name)
  }
})
