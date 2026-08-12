test_that("the public API uses English snake_case names", {
  expect_true(is.function(create_metapackage))
  expect_true(is.function(install_local_pkg))
  expect_true(is.function(diagnose_dependencies))
  expect_true(is.function(scan_bigbang_artifact))
  previous_scanner <- paste0("scan_", "local", "verse_artifact")
  expect_false(previous_scanner %in% getNamespaceExports("bigbang"))
  expect_named(
    formals(create_metapackage),
    c(
      "name", "packages", "pkg_dir", "ext", "version", "dest_dir",
      "reexport", "document", "verbose", "authors", "description",
      "license", "additional_deps", "ignore_deps", "import_deps",
      "force_deps", "debug", "workflow", "include_archives", "tolerate",
      "dry_run", "on_component_error", "update", "install_upgrade"
    )
  )
  expect_named(
    formals(install_local_pkg),
    c(
      "package", "pkg_dir", "ext", "repos", "cran_deps", "verbose",
      "force", "upgrade", "lib"
    )
  )
  expect_named(formals(diagnose_dependencies), c("packages", "pkg_dir", "ext"))
})

test_that("generated provenance uses the installed package version", {
  expect_identical(
    .bb_generator_version(),
    as.character(utils::packageVersion("bigbang"))
  )
})

test_that("the Spanish transition aliases are gone", {
  removed <- c(
    "crear_meta_paquete_local", "diagnosticar_dependencias",
    "install_loc_pkg_w_dep"
  )
  if ("bigbang" %in% loadedNamespaces()) {
    expect_false(any(removed %in% getNamespaceExports("bigbang")))
  }
  # The source NAMESPACE is only reachable when the tests run from the source
  # tree; under R CMD check the installed namespace above is the authority.
  namespace_file <- file.path(testthat::test_path(), "..", "..", "NAMESPACE")
  if (file.exists(namespace_file)) {
    namespace <- readLines(namespace_file, warn = FALSE)
    for (name in removed) {
      expect_false(
        any(grepl(paste0("export(", name, ")"), namespace, fixed = TRUE)),
        info = name
      )
    }
  }
  expect_false(any(vapply(
    removed, exists, logical(1), where = asNamespace("bigbang"), inherits = FALSE
  )))
})

test_that("unrelated utility leftovers are internal", {
  if ("bigbang" %in% loadedNamespaces()) {
    exports <- getNamespaceExports("bigbang")
    expect_false(any(c("to_unicode", "count_scripts_lines") %in% exports))
  } else {
    namespace <- readLines(file.path(testthat::test_path(), "..", "..", "NAMESPACE"))
    expect_false(any(grepl("export\\((to_unicode|count_scripts_lines)\\)", namespace)))
  }
})
