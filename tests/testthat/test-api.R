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
      "force_deps", "debug"
    )
  )
  expect_named(
    formals(install_local_pkg),
    c("package", "pkg_dir", "ext", "repos", "cran_deps", "verbose")
  )
  expect_named(formals(diagnose_dependencies), c("packages", "pkg_dir", "ext"))
})

test_that("Spanish compatibility aliases issue standard deprecation warnings", {
  withr::local_options(bigbang.deprecation_warnings = TRUE)
  expect_warning(
    diagnosticar_dependencias(character(), tempdir()),
    "deprecated"
  )
  expect_warning(
    install_loc_pkg_w_dep("doesnotexist_0.0.0", tempdir()),
    "deprecated"
  )
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
