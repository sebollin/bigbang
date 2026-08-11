toy_archive_dir <- function(destination) {
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

generate_self_contained <- function(name, archives, destination, ...) {
  create_metapackage(
    name = name,
    packages = "toycomponent_0.1.0",
    pkg_dir = archives,
    dest_dir = destination,
    document = FALSE,
    verbose = FALSE,
    import_deps = character(),
    force_deps = character(),
    ...
  )
}

installer_signature <- function(project_dir) {
  sources <- list.files(file.path(project_dir, "R"), full.names = TRUE)
  lines <- unlist(lapply(sources, readLines, warn = FALSE), use.names = FALSE)
  grep("_install <- function", lines, value = TRUE)
}

test_that("component archives travel inside the meta-package by default", {
  sandbox <- tempfile("bigbang-self-contained-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("selfverse", archives, destination)

  expect_true(dir.exists(file.path(result$path, "inst", "archives")))
  expect_identical(
    list.files(file.path(result$path, "inst", "archives")),
    "toycomponent_0.1.0.tar.gz"
  )

  # The default is resolved when the installer runs, so it points at the
  # library of whoever installed the meta-package rather than at this machine.
  signature <- installer_signature(result$path)
  expect_true(any(grepl("system.file(\"archives\"", signature, fixed = TRUE)))
  expect_path_absent(archives, signature)
  expect_path_absent(sandbox, signature)
})

test_that("shipped archives do not leak any source directory", {
  sandbox <- tempfile("bigbang-source-paths-")
  first_dir <- file.path(sandbox, "origin-alpha-distinct")
  second_dir <- file.path(sandbox, "origin-beta-distinct")
  source_root <- file.path(sandbox, "sources")
  destination <- file.path(sandbox, "destination")
  dir.create(first_dir, recursive = TRUE)
  dir.create(second_dir)
  dir.create(source_root)
  dir.create(destination)

  first <- file.path(first_dir, "toycomponent_0.1.0.tar.gz")
  toy <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  if (!nzchar(toy)) toy <- testthat::test_path(
    "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
  )
  file.copy(toy, first)
  second_source <- file.path(source_root, "secondpkg")
  dir.create(file.path(second_source, "R"), recursive = TRUE)
  writeLines(c(
    "Package: secondpkg", "Version: 1.0.0", "Title: Second fixture",
    "Description: A temporary second fixture.", "License: MIT"
  ), file.path(second_source, "DESCRIPTION"))
  writeLines("secondpkg_value <- function() 1L",
             file.path(second_source, "R", "value.R"))
  writeLines("export(secondpkg_value)", file.path(second_source, "NAMESPACE"))
  second <- file.path(second_dir, "secondpkg_1.0.0.tar.gz")
  withr::with_dir(source_root, utils::tar(
    second, "secondpkg", compression = "gzip"
  ))
  result <- create_metapackage(
    "pathverse", c(first, second), dest_dir = destination,
    document = FALSE, verbose = FALSE, force_deps = character()
  )
  generated_text <- function() {
    files <- list.files(
      result$path, recursive = TRUE, full.names = TRUE, all.files = TRUE,
      no.. = TRUE
    )
    text_files <- files[
      !dir.exists(files) & !grepl("\\.(mo|tar\\.gz|zip|tar)$", files,
                                  ignore.case = TRUE)
    ]
    paste(
      unlist(lapply(text_files, readLines, warn = FALSE), use.names = FALSE),
      collapse = "\n"
    )
  }
  text <- generated_text()
  expect_path_absent(first_dir, text)
  expect_path_absent(second_dir, text)

  # Prove that the tree scan detects a leak instead of passing vacuously.
  probe <- file.path(result$path, "source-path-probe.txt")
  on.exit(unlink(probe), add = TRUE)
  writeLines(normalizePath(first_dir, winslash = "/"), probe)
  expect_true(contains_path(first_dir, generated_text()))
  unlink(probe)
})

test_that("the build ignore rules keep shipped archives and drop stray ones", {
  sandbox <- tempfile("bigbang-buildignore-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("ignoreverse", archives, destination)
  patterns <- readLines(file.path(result$path, ".Rbuildignore"), warn = FALSE)

  matches_any <- function(path) {
    any(vapply(
      patterns, function(pattern) grepl(pattern, path, perl = TRUE), logical(1)
    ))
  }

  # Archives deliberately shipped with the meta-package must reach the tarball.
  expect_false(matches_any("inst/archives/toycomponent_0.1.0.tar.gz"))
  # Archives left at the project root must not.
  expect_true(matches_any("toycomponent_0.1.0.tar.gz"))
  expect_true(matches_any("something.tar.gz"))
  expect_true(matches_any("something.zip"))
})

test_that("include_archives = FALSE keeps pkg_dir mandatory", {
  sandbox <- tempfile("bigbang-external-archives-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained(
    "externalverse", archives, destination, include_archives = FALSE
  )

  expect_false(dir.exists(file.path(result$path, "inst", "archives")))
  signature <- installer_signature(result$path)
  expect_false(any(grepl("system.file(\"archives\"", signature, fixed = TRUE)))
  expect_true(any(grepl("function(pkg_dir,", signature, fixed = TRUE)))
})

test_that("include_archives rejects anything that is not one logical", {
  sandbox <- tempfile("bigbang-include-validation-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  for (bad in list("yes", NA, c(TRUE, FALSE), NULL, 1)) {
    expect_error(
      generate_self_contained(
        "badverse", archives, destination, include_archives = bad
      ),
      regexp = "'include_archives'"
    )
  }
})

build_component <- function(name, root, archives, imports = NULL) {
  pkg <- file.path(root, name)
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name), "Type: Package",
    paste0("Title: Component ", name), "Version: 0.1.0",
    "Authors@R: person('Test', 'Author', email='test@example.org', role=c('aut','cre'))",
    paste0("Description: Component used only in temporary tests: ", name, "."),
    "License: MIT", "Encoding: UTF-8",
    if (!is.null(imports)) paste0("Imports: ", imports)
  ), file.path(pkg, "DESCRIPTION"))
  writeLines(paste0("export(", name, "_value)"), file.path(pkg, "NAMESPACE"))
  writeLines(
    paste0(name, "_value <- function() \"", name, "\""),
    file.path(pkg, "R", "value.R")
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  withr::with_dir(root, system2(
    r_binary, c("CMD", "build", shQuote(pkg)), stdout = TRUE, stderr = TRUE
  ))
  tarball <- paste0(name, "_0.1.0.tar.gz")
  file.rename(file.path(root, tarball), file.path(archives, tarball))
  invisible(file.path(archives, tarball))
}

test_that("a shipped meta-package installs its components with no arguments", {
  skip_on_cran()
  skip_if_not_installed("withr")

  sandbox <- tempfile("bigbang-no-paths-")
  archives <- file.path(sandbox, "archives")
  dir.create(archives, recursive = TRUE)
  sources <- file.path(sandbox, "sources")
  dir.create(sources, recursive = TRUE)
  # The second component depends on the first, so the recipient also exercises
  # the topological order rather than a single trivial install.
  build_component("compalpha", sources, archives)
  build_component("compbeta", sources, archives, imports = "compalpha")
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- create_metapackage(
    name = "shippedverse",
    packages = c("compalpha_0.1.0", "compbeta_0.1.0"),
    pkg_dir = archives,
    dest_dir = destination,
    document = FALSE,
    verbose = FALSE,
    import_deps = character(),
    force_deps = character()
  )

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(result$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))

  tarball <- file.path(sandbox, "shippedverse_0.1.0.tar.gz")
  expect_true(file.exists(tarball))

  # Everything the recipient would not have is removed: the archive directory
  # and the generated source tree. Only the tarball survives.
  unlink(archives, recursive = TRUE)
  unlink(destination, recursive = TRUE)
  expect_false(dir.exists(archives))

  library_dir <- file.path(sandbox, "library")
  dir.create(library_dir)
  install_output <- system2(
    r_binary,
    c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(library_dir),
      shQuote(tarball)),
    stdout = TRUE, stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  expect_identical(
    install_status, 0L, info = paste(install_output, collapse = "\n")
  )
  expect_false(dir.exists(file.path(library_dir, "compalpha")))
  expect_false(dir.exists(file.path(library_dir, "compbeta")))

  script <- file.path(sandbox, "recipient.R")
  writeLines(c(
    sprintf(".libPaths(%s)", deparse(library_dir)),
    "suppressPackageStartupMessages(library(shippedverse))",
    "result <- shippedverse_install(verbose = FALSE)",
    "cat('FAILED:', length(result$failed), '\\n')",
    "cat('ORDER:', paste(result$order, collapse = '|'), '\\n')",
    "cat('ALPHA:', requireNamespace('compalpha', quietly = TRUE), '\\n')",
    "cat('BETA:', requireNamespace('compbeta', quietly = TRUE), '\\n')",
    "cat('VALUES:', compalpha::compalpha_value(), compbeta::compbeta_value(), '\\n')"
  ), script)
  recipient_output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  )
  report <- paste(recipient_output, collapse = "\n")

  expect_true(any(grepl("FAILED: 0", recipient_output, fixed = TRUE)), info = report)
  expect_true(any(grepl("ALPHA: TRUE", recipient_output, fixed = TRUE)), info = report)
  expect_true(any(grepl("BETA: TRUE", recipient_output, fixed = TRUE)), info = report)
  # The dependency must be installed before the package that imports it.
  expect_true(
    any(grepl("ORDER: compalpha_0.1.0|compbeta_0.1.0", recipient_output, fixed = TRUE)),
    info = report
  )
  expect_true(
    any(grepl("VALUES: compalpha compbeta", recipient_output, fixed = TRUE)),
    info = report
  )
  expect_true(dir.exists(file.path(library_dir, "compalpha")), info = report)
  expect_true(dir.exists(file.path(library_dir, "compbeta")), info = report)
})

test_that("the generated README documents the installation call that applies", {
  sandbox <- tempfile("bigbang-readme-modes-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  shipped <- generate_self_contained("shippedreadme", archives, destination)
  shipped_readme <- readLines(
    file.path(shipped$path, "README.md"), warn = FALSE
  )
  expect_true(any(grepl("shippedreadme_install()", shipped_readme, fixed = TRUE)))
  expect_false(any(grepl("pkg_dir =", shipped_readme, fixed = TRUE)))
  expect_true(any(grepl("ship inside this package", shipped_readme, fixed = TRUE)))

  external <- generate_self_contained(
    "externalreadme", archives, destination, include_archives = FALSE
  )
  external_readme <- readLines(
    file.path(external$path, "README.md"), warn = FALSE
  )
  expect_true(any(grepl(
    "externalreadme_install(pkg_dir =", external_readme, fixed = TRUE
  )))
  expect_true(any(grepl("live outside this package", external_readme, fixed = TRUE)))

  # The inclusion request is the only governance guidance the generator emits.
  for (readme in list(shipped_readme, external_readme)) {
    expect_true(any(grepl("## Adding a component", readme, fixed = TRUE)))
    expect_true(any(grepl(
      "maintainer named in DESCRIPTION", readme, fixed = TRUE
    )))
  }
})

test_that("the built tarball carries the shipped archives and no stray ones", {
  skip_on_cran()
  skip_if_not_installed("withr")

  sandbox <- tempfile("bigbang-tarball-contents-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("tarballverse", archives, destination)

  # Archives a user might leave lying around, at the root and nested.
  file.copy(
    file.path(archives, "toycomponent_0.1.0.tar.gz"),
    file.path(result$path, "root-stray.tar.gz")
  )
  dir.create(file.path(result$path, "vendor"))
  file.copy(
    file.path(archives, "toycomponent_0.1.0.tar.gz"),
    file.path(result$path, "vendor", "nested-stray.tar.gz")
  )

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(result$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))

  tarball <- file.path(sandbox, "tarballverse_0.1.0.tar.gz")
  entries <- untar(tarball, list = TRUE)

  # The assertion is on the tarball itself, not on the patterns in isolation.
  expect_true(any(grepl(
    "tarballverse/inst/archives/toycomponent_0.1.0.tar.gz", entries, fixed = TRUE
  )))
  expect_false(any(grepl("root-stray.tar.gz", entries, fixed = TRUE)))
  expect_false(any(grepl("nested-stray.tar.gz", entries, fixed = TRUE)))
})

test_that("reserved R package names are refused", {
  sandbox <- tempfile("bigbang-reserved-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  for (reserved in c("base", "stats", "utils", "methods", "tools")) {
    expect_error(
      generate_self_contained(reserved, archives, destination),
      regexp = "belongs to R itself",
      info = reserved
    )
  }
  expect_length(list.files(destination, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("the startup hint names a call the reader can actually make", {
  sandbox <- tempfile("bigbang-hints-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  shipped <- generate_self_contained("hintshipped", archives, destination)
  shipped_code <- unlist(lapply(
    list.files(file.path(shipped$path, "R"), full.names = TRUE),
    readLines, warn = FALSE
  ), use.names = FALSE)
  hints <- grep("to install them", shipped_code, value = TRUE)
  expect_length(hints, 2L)
  expect_true(all(grepl("hintshipped_install()", hints, fixed = TRUE)))
  expect_false(any(grepl("pkg_dir", hints, fixed = TRUE)))

  external <- generate_self_contained(
    "hintexternal", archives, destination, include_archives = FALSE
  )
  external_code <- unlist(lapply(
    list.files(file.path(external$path, "R"), full.names = TRUE),
    readLines, warn = FALSE
  ), use.names = FALSE)
  external_hints <- grep("to install them", external_code, value = TRUE)
  expect_length(external_hints, 2L)
  # Suggesting a bare call here would send the reader straight into an error.
  expect_true(all(grepl(
    "hintexternal_install(pkg_dir = PATH)", external_hints, fixed = TRUE
  )))
})

test_that("the arguments added after 0.1.0 do not displace the earlier ones", {
  # A positional call written against 0.1.0 must keep binding to the same
  # parameters, or it silently starts meaning something else.
  released_create <- c(
    "name", "packages", "pkg_dir", "ext", "version", "dest_dir", "reexport",
    "document", "verbose", "authors", "description", "license",
    "additional_deps", "ignore_deps", "import_deps", "force_deps", "debug"
  )
  expect_identical(
    names(formals(create_metapackage))[seq_along(released_create)],
    released_create
  )

  released_install <- c(
    "package", "pkg_dir", "ext", "repos", "cran_deps", "verbose"
  )
  expect_identical(
    names(formals(install_local_pkg))[seq_along(released_install)],
    released_install
  )
})

test_that("only the real inst/archives is exempt from the ignore rules", {
  sandbox <- tempfile("bigbang-ignore-case-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("caseverse", archives, destination)
  patterns <- readLines(file.path(result$path, ".Rbuildignore"), warn = FALSE)
  # R applies these with perl = TRUE and ignore.case = TRUE.
  excluded <- function(path) {
    any(vapply(patterns, function(pattern) {
      grepl(pattern, path, perl = TRUE, ignore.case = TRUE)
    }, logical(1)))
  }

  expect_false(excluded("inst/archives/toycomponent_0.1.0.tar.gz"))
  expect_true(excluded("INST/ARCHIVES/upper.TAR.GZ"))
  expect_true(excluded("Inst/Archives/mixed.tar.gz"))
  expect_true(excluded("inst.archives/x.tar.gz"))
  expect_true(excluded("vendor/nested.tar.gz"))
  expect_true(excluded("root.tar.gz"))
})

test_that("the generated vignette names the installation call of its mode", {
  sandbox <- tempfile("bigbang-vignette-mode-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  shipped <- generate_self_contained("vshipped", archives, destination)
  shipped_lines <- readLines(
    file.path(shipped$path, "vignettes", "introduction-vshipped.Rmd"),
    warn = FALSE
  )
  expect_true(any(grepl("vshipped_install()`", shipped_lines, fixed = TRUE)))
  expect_false(any(grepl("pkg_dir = ...", shipped_lines, fixed = TRUE)))

  external <- generate_self_contained(
    "vexternal", archives, destination, include_archives = FALSE
  )
  external_lines <- readLines(
    file.path(external$path, "vignettes", "introduction-vexternal.Rmd"),
    warn = FALSE
  )
  expect_true(any(grepl(
    "vexternal_install(pkg_dir = ...)", external_lines, fixed = TRUE
  )))
})

test_that("the stamped generator version tracks the package version", {
  sandbox <- tempfile("bigbang-generator-version-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("stampverse", archives, destination)
  description <- read.dcf(file.path(result$path, "DESCRIPTION"))
  expect_identical(
    unname(description[1, "Config/bigbang/generator-version"]),
    as.character(utils::packageVersion("bigbang"))
  )
})

test_that("returned paths use one separator convention", {
  sandbox <- tempfile("bigbang-separators-")
  archives <- file.path(sandbox, "archives")
  toy_archive_dir(archives)
  destination <- file.path(sandbox, "destination")
  dir.create(destination, recursive = TRUE)

  result <- generate_self_contained("sepverse", archives, destination)

  # The package normalises with winslash = "/" throughout. A result path in the
  # platform default would not compare equal to a path the caller built with
  # file.path(), on Windows only.
  expect_identical(
    result$path,
    normalizePath(
      file.path(destination, "sepverse"), winslash = "/", mustWork = TRUE
    )
  )
  expect_false(grepl("\\\\", result$path))

  scan <- scan_bigbang_artifact(result$path)
  expect_identical(
    scan$path, normalizePath(result$path, winslash = "/", mustWork = TRUE)
  )
  expect_false(grepl("\\\\", scan$path))
})
