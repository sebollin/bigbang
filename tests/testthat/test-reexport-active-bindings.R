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
  dir.create(component_library)
  archive <- reexport_make_archive(
    source_root, archive_dir, "toya",
    exports = c("toya_capture", "toya_generic", "toya_make", "toya_value"),
    body = c(
      "toya_capture <- function(x) substitute(x)",
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
  expect_error(toya_value(1), "Component package 'toya' is not installed")

  toyverse_install(lib = component_library, verbose = FALSE)
  expect_identical(toya_value(1), 2)
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
  expect_true(all(c("classre", "classre_method") %in% metadata$exports))

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
