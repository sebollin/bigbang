copy_toy_archive <- function(destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  source <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(source)) {
    source <- testthat::test_path(
      "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
    )
  }
  file.copy(source, destination)
  invisible(destination)
}

generate_toy_metapackage <- function(name, archives, destination,
                                     document = FALSE) {
  create_metapackage(
    name = name,
    packages = "toycomponent_0.1.0",
    pkg_dir = archives,
    dest_dir = destination,
    document = document,
    verbose = FALSE,
    import_deps = character(),
    force_deps = character()
  )
}

test_that("relative destination and archive paths generate a complete project", {
  sandbox <- tempfile("bigbang-relative-generation-")
  dir.create(sandbox)
  copy_toy_archive(file.path(sandbox, "archives"))

  result <- withr::with_dir(sandbox, {
    dir.create("output")
    cwd_before <- getwd()
    generated <- generate_toy_metapackage(
      "relverse", "archives", "output", document = FALSE
    )
    expect_identical(getwd(), cwd_before)
    generated
  })

  expect_identical(
    result$path,
    normalizePath(file.path(sandbox, "output", "relverse"), winslash = "/")
  )
  expect_true(file.exists(file.path(result$path, "DESCRIPTION")))
  expect_true(file.exists(file.path(result$path, "NAMESPACE")))
  expect_setequal(
    list.files(file.path(result$path, "R")),
    c("attach.R", "install_packages.R", "utils.R", "zzz.R")
  )

  text_files <- list.files(
    result$path,
    pattern = "(\\.(R|Rd|Rmd|po|pot)$|DESCRIPTION$|NAMESPACE$)",
    recursive = TRUE,
    full.names = TRUE
  )
  generated_text <- unlist(lapply(
    text_files, readLines, warn = FALSE, encoding = "UTF-8"
  ), use.names = FALSE)
  helper_identifiers <- regmatches(
    generated_text,
    gregexpr(
      "\\b[A-Za-z][A-Za-z0-9.]*verse_(load_all|detach|packages|attach_all)\\b",
      generated_text,
      perl = TRUE
    )
  )
  helper_identifiers <- unique(unlist(helper_identifiers, use.names = FALSE))
  expect_true(length(helper_identifiers) > 0L)
  expect_true(all(startsWith(helper_identifiers, "relverse_")))
})

test_that("failed generation restores cwd and rolls back only its own project", {
  sandbox <- tempfile("bigbang-generation-rollback-")
  dir.create(sandbox)
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "output")
  dir.create(archives)
  dir.create(destination)
  cwd_before <- getwd()

  expect_error(
    create_metapackage(
      "retryverse", "missing_0.1.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "archives were not found"
  )
  expect_identical(getwd(), cwd_before)
  expect_false(dir.exists(file.path(destination, "retryverse")))

  copy_toy_archive(archives)
  result <- generate_toy_metapackage(
    "retryverse", archives, destination, document = FALSE
  )
  expect_true(file.exists(file.path(result$path, "R", "attach.R")))
  expect_identical(getwd(), cwd_before)

  preexisting <- file.path(destination, "preexistingverse")
  dir.create(preexisting)
  expect_error(
    create_metapackage(
      "preexistingverse", "missing_0.1.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "archives were not found"
  )
  expect_true(dir.exists(preexisting))
  expect_true(dir.exists(file.path(preexisting, "R")))
  expect_identical(getwd(), cwd_before)
})

test_that("successful documentation restores the caller session", {
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-document-session-")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "output")
  dir.create(destination, recursive = TRUE)
  copy_toy_archive(archives)

  search_before <- search()
  namespaces_before <- loadedNamespaces()
  cwd_before <- getwd()
  result <- suppressWarnings(generate_toy_metapackage(
    "docverse", archives, destination, document = TRUE
  ))

  expect_true(result$documented)
  expect_identical(search(), search_before)
  expect_setequal(loadedNamespaces(), namespaces_before)
  expect_identical(getwd(), cwd_before)
  expect_false("docverse" %in% loadedNamespaces())
  expect_false("package:docverse" %in% search())
  if (!"devtools_shims" %in% search_before) {
    expect_false("devtools_shims" %in% search())
  }
})

test_that("failed documentation is reported and restores the caller session", {
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-document-failure-")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "output")
  dir.create(destination, recursive = TRUE)
  copy_toy_archive(archives)

  search_before <- search()
  namespaces_before <- loadedNamespaces()
  cwd_before <- getwd()
  result <- NULL
  expect_warning(
    result <- testthat::with_mocked_bindings(
      generate_toy_metapackage(
        "faileddocverse", archives, destination, document = TRUE
      ),
      document = function(pkg, ...) {
        suppressPackageStartupMessages(devtools::load_all(pkg, quiet = TRUE))
        stop("forced documentation failure")
      },
      .package = "devtools"
    ),
    "Error generating documentation: forced documentation failure"
  )

  expect_false(result$documented)
  expect_identical(search(), search_before)
  expect_setequal(loadedNamespaces(), namespaces_before)
  expect_identical(getwd(), cwd_before)
  expect_false("faileddocverse" %in% loadedNamespaces())
  expect_false("package:faileddocverse" %in% search())
  if (!"devtools_shims" %in% search_before) {
    expect_false("devtools_shims" %in% search())
  }
})
