test_that("archive work is reused within one source call", {
  testthat::skip_on_cran()
  archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(archive)) {
    archive <- normalizePath(
      testthat::test_path("..", "..", "inst", "extdata",
                          "toycomponent_0.1.0.tar.gz"),
      winslash = "/", mustWork = TRUE
    )
  }
  calls <- new.env(parent = emptyenv())
  calls$listing <- 0L
  calls$extract <- 0L
  original <- bigbang:::.untar_quiet
  testthat::local_mocked_bindings(
    .untar_quiet = function(...) {
      arguments <- list(...)
      if (isTRUE(arguments$list)) calls$listing <- calls$listing + 1L
      if (!is.null(arguments$exdir)) calls$extract <- calls$extract + 1L
      original(...)
    },
    .package = "bigbang"
  )

  cache <- bigbang:::.archive_cache_new()
  on.exit(bigbang:::.archive_cache_close(cache), add = TRUE)
  bigbang:::.read_archive_identity(archive, ".tar.gz", cache = cache)
  bigbang:::.read_archive_metadata(archive, ext = ".tar.gz", cache = cache)
  bigbang:::.extract_archive_checked(
    archive, ".tar.gz", NULL, cache = cache
  )

  expect_identical(calls$listing, 1L)
  expect_identical(calls$extract, 1L)
})

test_that("runtime call qualification keeps a real template byte-identical", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-round068-qualification-")
  dir.create(sandbox)
  withr::defer(unlink(sandbox, recursive = TRUE, force = TRUE))
  qualify <- bigbang:::.qualify_generated_runtime_calls
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .qualify_generated_runtime_calls = function(content) {
      if (grepl("install_packages_in_order <- function", content, fixed = TRUE)) {
        captured$content <- content
      }
      content
    },
    .package = "bigbang"
  )
  archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  generated <- create_metapackage(
    "fixtureverse068", archive, dest_dir = sandbox, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  expect_true(exists("content", envir = captured, inherits = FALSE))
  rendered <- strsplit(captured$content, "\n", fixed = TRUE)[[1L]]
  rendered <- paste(rendered[seq_len(30L)], collapse = "\n")
  reference <- paste(readLines(
    testthat::test_path("fixtures", "round068-qualified-runtime.txt"),
    warn = FALSE, encoding = "UTF-8"
  ), collapse = "\n")
  expect_identical(
    qualify(rendered),
    reference
  )
  expect_true(file.exists(file.path(generated$path, "R", "install_packages.R")))
})

test_that("install and attachment keep re-export bindings unevaluated", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-round068-lazy-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  marker <- file.path(sandbox, "read-marker")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)
  withr::defer(unlink(sandbox, recursive = TRUE, force = TRUE))

  package_dir <- file.path(source_root, "guardcomponent068")
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c(
    "Package: guardcomponent068",
    "Version: 0.1.0",
    "Title: Lazy binding guard",
    "Description: A package used to guard lazy re-exports.",
    "License: GPL (>= 3)"
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(guard_value)", file.path(package_dir, "NAMESPACE"),
             useBytes = TRUE)
  writeLines(c(
    "guard_value <- NULL",
    ".onLoad <- function(libname, pkgname) {",
    "  if (identical(Sys.getenv('BIGBANG_GUARD_ARMED'), 'yes')) {",
    "    marker <- Sys.getenv('BIGBANG_GUARD_MARKER')",
    "    namespace <- asNamespace(pkgname)",
    "    unlockBinding('guard_value', namespace)",
    "    rm('guard_value', envir = namespace)",
    "    makeActiveBinding('guard_value', function(value) {",
    "      if (!missing(value)) stop('read-only')",
    "      write('read', marker, append = TRUE)",
    "      42L",
    "    }, namespace)",
    "  }",
    "}"
  ), file.path(package_dir, "R", "guard.R"), useBytes = TRUE)
  archive <- file.path(archive_dir, "guardcomponent068_0.1.0.tar.gz")
  withr::with_dir(source_root, utils::tar(
    archive, "guardcomponent068", compression = "gzip"
  ))

  generated <- create_metapackage(
    "guardverse068", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), reexport = TRUE
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  status <- system2(
    r_binary,
    c("CMD", "INSTALL", "-l", shQuote(meta_library),
      shQuote(generated$path)),
    stdout = FALSE, stderr = FALSE
  )
  expect_identical(status, 0L)
  expect_false(
    file.exists(marker),
    info = if (file.exists(marker)) paste(readLines(marker), collapse = " | ") else ""
  )

  withr::local_envvar(
    BIGBANG_GUARD_MARKER = marker, BIGBANG_GUARD_ARMED = "no"
  )
  withr::local_libpaths(c(meta_library, component_library, .libPaths()))
  suppressPackageStartupMessages(library(guardverse068, quietly = TRUE))
  expect_true(exists(
    ".conflicts.OK", envir = as.environment("package:guardverse068"),
    inherits = FALSE
  ))
  withr::defer({
    if ("package:guardcomponent068" %in% search()) {
      detach("package:guardcomponent068", unload = TRUE,
             character.only = TRUE)
    }
    if ("package:guardverse068" %in% search()) {
      detach("package:guardverse068", unload = TRUE, character.only = TRUE)
    }
    if ("guardcomponent068" %in% loadedNamespaces()) {
      try(unloadNamespace("guardcomponent068"), silent = TRUE)
    }
    if ("guardverse068" %in% loadedNamespaces()) {
      try(unloadNamespace("guardverse068"), silent = TRUE)
    }
  })
  expect_false(file.exists(marker))
  suppressWarnings(guardverse068_conflicts())
  expect_false(file.exists(marker))
  suppressWarnings(guardverse068_attach())
  expect_false(file.exists(marker))
  expect_identical(guardverse068_deps(), "guardcomponent068")
  expect_false(file.exists(marker))
  guardverse068_install(lib = component_library, verbose = FALSE)
  expect_false(file.exists(marker))
  expect_true("package:guardcomponent068" %in% search())
  guardverse068_detach()
  if ("guardcomponent068" %in% loadedNamespaces()) {
    unloadNamespace("guardcomponent068")
  }
  withr::local_envvar(BIGBANG_GUARD_ARMED = "yes")
  guardverse068_install(lib = component_library, verbose = FALSE)
  expect_false(file.exists(marker))
  expect_true("package:guardcomponent068" %in% search())
  guardverse068_detach()
  expect_false("package:guardcomponent068" %in% search())
  expect_false(file.exists(marker))
  if ("package:guardverse068" %in% search()) {
    detach("package:guardverse068", unload = TRUE, character.only = TRUE)
  }
  expect_false(file.exists(marker))
  if ("guardverse068" %in% loadedNamespaces()) {
    unloadNamespace("guardverse068")
  }
  expect_false(file.exists(marker))
  suppressPackageStartupMessages(library(guardverse068))
  # As in 0.5.0, a re-export metapackage stays lazy on attach: components are
  # attached only on request.
  expect_false("package:guardcomponent068" %in% search())
  expect_false(file.exists(marker))
  # Conflicts describe the search path, so unattached components report none.
  expect_length(suppressWarnings(guardverse068_conflicts()), 0L)
  expect_false(file.exists(marker))
  attach_messages <- capture.output(
    suppressWarnings(guardverse068_attach()), type = "message"
  )
  expect_false(
    file.exists(marker),
    info = if (file.exists(marker)) paste(readLines(marker), collapse = " | ") else ""
  )
  expect_no_match(
    paste(attach_messages, collapse = "\n"),
    "The following objects are masked",
    fixed = TRUE
  )
  expect_true("package:guardcomponent068" %in% search())
  conflicts <- suppressWarnings(guardverse068_conflicts())
  expect_true("guard_value" %in% names(conflicts))
  expect_false(file.exists(marker))
  guardverse068_detach()
  guardverse068_attach()
  expect_false(file.exists(marker))
  expect_identical(guardverse068_deps(), "guardcomponent068")
  expect_false(file.exists(marker))

  expect_identical(getExportedValue("guardverse068", "guard_value"), 42L)
  expect_true(file.exists(marker))
})
