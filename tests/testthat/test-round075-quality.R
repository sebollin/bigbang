test_that("the Spanish catalog does not retain the 44 retired keys", {
  retired <- c(
    "The directory specified by 'pkg_dir' does not exist",
    "Unsupported archive extension: %s",
    paste0(
      "Component %s declares dependency %s, available as %s in pkg_dir but ",
      "not included. Add it to packages or remove the dependency."
    ),
    "Expected one DESCRIPTION in %s; found %d.",
    "Archive %s declares package %s, but its filename names %s.",
    "Archive %s declares version %s, but its filename names version %s.",
    "No DESCRIPTION file found in package %s",
    "Package archive not found: %s",
    "Cannot re-export symbol(s) %s because components collide: %s.",
    "Cannot re-export symbol(s) %s because they belong to generated metapackage code.",
    "The following package archives were not found: %s",
    "No DESCRIPTION found in source archive.",
    "Source archive has multiple candidate package roots.",
    paste0(
      "Cannot update %s because %s is not a recognized bigbang update journal. ",
      "It has no recognized backup. Next step: leave it intact, preserve its ",
      "contents, and inspect or move it aside before retrying."
    ),
    "Set aside unverified update-journal content at %s; no bytes were deleted and the update continues.",
    paste0(
      "Its tombstone belongs to project '%s' with manifest hash %s, not project ",
      "'%s' with manifest hash %s; nothing was deleted."
    ),
    "leave the folder intact, preserve the backup, and move it back beside its project before retrying.",
    "It contains entries outside its discard inventory; nothing was deleted. Preserved: %s.",
    "leave the folder intact, preserve the backup, remove only the reported user entries, and retry.",
    "It is not a directory with a valid marker.",
    "leave it intact, inspect it, and remove or move it only after preserving its contents.",
    "It has no marker but is not empty; nothing was deleted.",
    "leave it intact, preserve its contents, and inspect the failed preparation before retrying.",
    "It is not a directory with a valid tombstone.",
    "leave it intact, preserve its contents, and inspect it before retrying.",
    "It has no valid tombstone; nothing was deleted.",
    "leave the folder intact, preserve its contents, and inspect it before retrying.",
    paste0(
      "Could not acquire the update lock at %s because its state made no ",
      "progress after %s retries; inspect the lock entry before retrying."
    ),
    "The update journal at %s has an invalid intention log.",
    "The unarmed update journal at %s contains unexpected files.",
    "The update journal at %s has invalid armed state.",
    "The armed update journal at %s contains unexpected files.",
    "The update journal at %s contains unexpected files.",
    paste0(
      "An update may still be running for %s (journal: %s). If no other update ",
      "is running, call again with recover = TRUE."
    ),
    paste0(
      "The interrupted update left files in an unknown state: %s. Journal: %s. ",
      "Use recover = TRUE to preserve them and recover."
    ),
    "Component package '%s' is not installed.",
    "Assignment to re-export symbol '%s' in %s:%d.",
    "Call %s targets re-export symbol '%s' in %s:%d.",
    "Call %s has a non-literal first argument in %s:%d.",
    "Dynamic namespace mutation via %s in %s:%d.",
    "NAMESPACE does not declare a unique import source for '%s'.",
    "NAMESPACE imports '%s' from external package '%s' with import().",
    "NAMESPACE imports '%s' from component '%s', which was skipped.",
    paste0(
      "Installed owners for re-export symbol '%s' differ: %s. Choose a provider ",
      "with reexport_prefer or omit it with reexport_exclude."
    )
  )
  expect_length(retired, 44L)
  expect_false(any(retired %in% names(bigbang:::.bigbang_spanish_catalog())))
  expect_false(any(retired %in% names(
    bigbang:::.metapackage_spanish_catalog("qualityverse")
  )))
})

test_that("generated packages declare the runtime floor that was exercised", {
  skip_on_cran()
  archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(archive)) {
    archive <- testthat::test_path(
      "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
    )
  }
  root <- tempfile("bigbang-round075-r-floor-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  generated <- create_metapackage(
    "floorverse075", archive, pkg_dir = dirname(archive), dest_dir = root,
    document = FALSE, verbose = FALSE, include_archives = TRUE,
    import_deps = character(), force_deps = character()
  )
  description <- readLines(
    file.path(generated$path, "DESCRIPTION"), warn = FALSE
  )
  expect_true(any(grepl("^    R \\(>= 3\\.5\\.0\\)$", description)))
})
