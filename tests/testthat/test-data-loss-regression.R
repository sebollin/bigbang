# Regression test for the historical data-loss defect (see NEWS.md).
#
# It covers two levels:
#  A) STATIC: emitted startup hooks contain no installation or removal code.
#  B) EMPIRICAL: generation from a cwd containing decoys preserves every file.
#
# The full source-install and library() scenario is covered by the integration
# script in the tests/integration directory.

if (!exists("create_metapackage", mode = "function")) {
  project_root <- file.path(testthat::test_path(), "..", "..")
  source_files <- c(
    "fs-utils.R", "i18n-tools.R", "translations.R", "dependencies.R",
    "scaffold.R", "templates-engine.R", "results.R", "create_metapackage.R",
    "install_local_pkg.R"
  )
  for (source_file in source_files) {
    sys.source(file.path(project_root, "R", source_file), envir = globalenv())
  }
}

make_dummy_component <- function(name, source_dir, with_r = TRUE, imports = NULL) {
  path <- file.path(source_dir, name)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    paste0("Package: ", name), "Type: Package",
    paste0("Title: Dummy ", name), "Version: 0.1.0",
    "Authors@R: person('T', 'D', email = 't@e.com', role = c('aut', 'cre'))",
    "Description: Temporary component package.", "License: GPL (>= 3)", "Encoding: UTF-8",
    if (!is.null(imports)) paste0("Imports: ", paste(imports, collapse = ", "))
  ), file.path(path, "DESCRIPTION"))
  writeLines(
    if (is.null(imports)) character() else paste0("import(", imports, ")"),
    file.path(path, "NAMESPACE")
  )
  if (with_r) {
    dir.create(file.path(path, "R"), showWarnings = FALSE)
    writeLines(paste0("f_", name, " <- function() '", name, "'"),
               file.path(path, "R", "f.R"))
    writeLines(c(
      readLines(file.path(path, "NAMESPACE"), warn = FALSE),
      paste0("export(f_", name, ")")
    ), file.path(path, "NAMESPACE"))
  } else {
    dir.create(file.path(path, "data"), showWarnings = FALSE)
    writeLines("data", file.path(path, "data", "data.txt"))
  }
  path
}

generate_in_sandbox <- function() {
  skip_if_not(exists("create_metapackage", mode = "function"),
              "generator unavailable")
  skip_if_not_installed("whisker")
  skip_if_not_installed("withr")

  # Do not use local_tempdir(): it would remove the fixture before assertions run.
  sandbox <- tempfile("data-loss-regression-")
  dir.create(sandbox)
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  cwd <- file.path(sandbox, "cwd")
  dir.create(sources)
  dir.create(archives)
  dir.create(cwd)

  for (pkg in c("aaa", "bbb")) {
    make_dummy_component(pkg, sources)
    withr::with_dir(sources, utils::tar(
      file.path(archives, paste0(pkg, "_0.1.0.tar.gz")),
      files = pkg, compression = "gzip"
    ))
  }

  # Decoys include the historical data-only and tmp-prefixed edge cases.
  decoys <- c("aaa", "bbb", "aaa_extra", "tmp_analysis", "doc", "file2024", "Meta", "libs")
  make_dummy_component("aaa", cwd)
  dir.create(file.path(cwd, "bbb", "data"), recursive = TRUE)
  writeLines(c("Package: bbb", "Version: 0.1.0"), file.path(cwd, "bbb", "DESCRIPTION"))
  writeLines("valuable", file.path(cwd, "bbb", "data", "x.txt"))
  for (d in c("aaa_extra", "tmp_analysis", "doc", "file2024", "Meta", "libs")) {
    dir.create(file.path(cwd, d))
    writeLines("valuable", file.path(cwd, d, "x.txt"))
  }

  withr::with_dir(cwd, {
    suppressMessages(create_metapackage(
      name = "testverse",
      packages = c("aaa_0.1.0", "bbb_0.1.0"),
      pkg_dir = archives,
      dest_dir = file.path(sandbox, "project"),
      document = FALSE,
      verbose = FALSE
    ))
  })

  list(cwd = cwd, decoys = decoys,
       project = file.path(sandbox, "project", "testverse"))
}

test_that("metapackage generation preserves every cwd directory", {
  s <- generate_in_sandbox()
  files <- file.path(s$cwd, s$decoys, "x.txt")
  files[match(c("aaa", "bbb"), s$decoys)] <- c(
    file.path(s$cwd, "aaa", "R", "f.R"),
    file.path(s$cwd, "bbb", "data", "x.txt")
  )
  before <- unname(tools::md5sum(files))
  for (d in s$decoys) {
    expect_true(dir.exists(file.path(s$cwd, d)),
                label = paste0("decoy '", d, "' survives generation"))
  }
  expect_true(file.exists(file.path(s$cwd, "bbb", "data", "x.txt")))
  expect_true(file.exists(file.path(s$cwd, "doc", "x.txt")))
  expect_identical(unname(tools::md5sum(files)), before)
})

make_functional_sandbox <- function() {
  skip_if_not_installed("whisker")
  skip_if_not_installed("withr")
  sandbox <- tempfile("functional-regression-")
  dir.create(sandbox)
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  cwd <- file.path(sandbox, "cwd")
  lib <- file.path(sandbox, "lib")
  for (d in c(sources, archives, destination, cwd, lib)) dir.create(d)

  make_dummy_component("bbblv", sources, with_r = FALSE)
  make_dummy_component("aaalv", sources, imports = "bbblv")
  make_dummy_component("tmpcomponent", sources)
  archive_stems <- c("aaalv_0.1.0", "bbblv_0.1.0", "tmpcomponent_0.1.0")
  for (pkg in sub("_.*", "", archive_stems)) {
    withr::with_dir(sources, utils::tar(
      file.path(archives, paste0(pkg, "_0.1.0.tar.gz")),
      files = pkg, compression = "gzip"
    ))
  }

  decoys <- c("aaalv", "bbblv", "tmpcomponent", "doc", "Meta", "libs")
  for (d in decoys) {
    dir.create(file.path(cwd, d))
    writeLines(paste("valuable content", d), file.path(cwd, d, "sentinel.txt"))
  }
  files <- file.path(cwd, decoys, "sentinel.txt")
  hashes <- unname(tools::md5sum(files))

  withr::with_dir(cwd, suppressWarnings(suppressMessages(
    create_metapackage(
      name = "functionalverse",
      packages = archive_stems,
      pkg_dir = archives,
      dest_dir = destination,
      document = FALSE,
      verbose = FALSE,
      force_deps = character()
    )
  )))

  list(
    sandbox = sandbox, archives = archives, cwd = cwd, lib = lib,
    project = file.path(destination, "functionalverse"), packages = sub("_.*", "", archive_stems),
    files = files, hashes = hashes
  )
}

test_that("_install installs each component once in dependency order and attaches it", {
  s <- make_functional_sandbox()
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  .libPaths(c(s$lib, old_libs))

  suppressWarnings(utils::install.packages(
    s$project, repos = NULL, type = "source", lib = s$lib, quiet = TRUE
  ))
  suppressWarnings(suppressMessages(
    library("functionalverse", character.only = TRUE, lib.loc = s$lib)
  ))
  expect_false(any(paste0("package:", s$packages) %in% search()))

  ns <- asNamespace("functionalverse")
  assign(".bigbang_install_count", 0L, envir = .GlobalEnv)
  on.exit(rm(".bigbang_install_count", envir = .GlobalEnv), add = TRUE)
  trace(
    "install_local_archive",
    tracer = quote(
      assign(
        ".bigbang_install_count",
        get(".bigbang_install_count", envir = .GlobalEnv) + 1L,
        envir = .GlobalEnv
      )
    ),
    where = ns,
    print = FALSE
  )
  on.exit(untrace("install_local_archive", where = ns), add = TRUE)

  result <- getExportedValue("functionalverse", "functionalverse_install")(
    s$archives, ".tar.gz"
  )
  expect_equal(get(".bigbang_install_count", envir = .GlobalEnv), 3L)
  expect_identical(
    result$order,
    c("bbblv_0.1.0", "aaalv_0.1.0", "tmpcomponent_0.1.0")
  )
  expect_true(all(vapply(s$packages, requireNamespace, logical(1), quietly = TRUE)))
  expect_true(all(paste0("package:", s$packages) %in% search()))
  expect_identical(unname(tools::md5sum(s$files)), s$hashes)
  expect_true(dir.exists(tempdir()))

  detach("package:aaalv", character.only = TRUE, unload = FALSE)
  expect_true("aaalv" %in% loadedNamespaces())
  expect_false("package:aaalv" %in% search())
  getExportedValue("functionalverse", "functionalverse_attach")("aaalv")
  expect_true("package:aaalv" %in% search())
})

test_that("R CMD INSTALL preserves decoys inside the source tree", {
  s <- generate_in_sandbox()
  internal_decoys <- c("aaa", "bbb", "tmpcomponent", "doc", "Meta", "libs")
  for (d in internal_decoys) {
    dir.create(file.path(s$project, d), showWarnings = FALSE)
    writeLines(paste("internal sentinel", d), file.path(s$project, d, "sentinel.txt"))
  }
  files <- file.path(s$project, internal_decoys, "sentinel.txt")
  before <- unname(tools::md5sum(files))
  lib <- tempfile("lib-r-cmd-install-")
  dir.create(lib)
  r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  output <- system2(
    r_bin,
    c("CMD", "INSTALL", "--no-multiarch", paste0("--library=", shQuote(lib)),
      shQuote(s$project)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_equal(status, 0L, info = paste(output, collapse = "\n"))
  expect_true(all(file.exists(files)))
  expect_identical(unname(tools::md5sum(files)), before)
})

test_that("a vulnerable legacy source is rejected without changing files", {
  s <- generate_in_sandbox()
  legacy_dir <- file.path(dirname(s$project), "legacy")
  dir.create(file.path(legacy_dir, "testverse", "R"), recursive = TRUE)
  root <- file.path(legacy_dir, "testverse")
  writeLines(
    ".onLoad <- function(...) unlink('aaa', recursive = TRUE, force = TRUE)",
    file.path(root, "R", "zzz.R")
  )
  writeLines("rm -rf aaa", file.path(root, "cleanup"))
  writeLines("valuable content", file.path(root, "sentinel.txt"))
  files <- list.files(root, recursive = TRUE, full.names = TRUE)
  before <- unname(tools::md5sum(files))
  temp_sentinel <- tempfile("tempdir-alive-")
  writeLines("alive", temp_sentinel)

  expect_error(
    create_metapackage(
      "testverse", c("aaa_0.1.0", "bbb_0.1.0"),
      file.path(dirname(dirname(s$project)), "archives"),
      dest_dir = legacy_dir,
      document = TRUE,
      verbose = FALSE
    ),
    "destination must be new or empty"
  )
  expect_identical(unname(tools::md5sum(files)), before)
  expect_true(file.exists(temp_sentinel))
})

test_that("dependency graph detects cycles without installing", {
  s <- make_functional_sandbox()
  sources <- file.path(s$sandbox, "cycle-sources")
  cycle_archives <- file.path(s$sandbox, "cycle-archives")
  dir.create(sources)
  dir.create(cycle_archives)
  make_dummy_component("cyclea", sources, imports = "cycleb")
  make_dummy_component("cycleb", sources, imports = "cyclea")
  for (pkg in c("cyclea", "cycleb")) {
    withr::with_dir(sources, utils::tar(
      file.path(cycle_archives, paste0(pkg, "_0.1.0.tar.gz")),
      files = pkg, compression = "gzip"
    ))
  }

  env <- new.env(parent = baseenv())
  sys.source(file.path(s$project, "R", "utils.R"), envir = env)
  sys.source(file.path(s$project, "R", "install_packages.R"), envir = env)
  installer_calls <- 0L
  env$install_local_archive <- function(...) {
    installer_calls <<- installer_calls + 1L
    stop("graph construction must not install")
  }
  expect_error(
    env$build_dependency_graph(
      c("cyclea_0.1.0", "cycleb_0.1.0"), cycle_archives, ".tar.gz"
    ),
    "Circular dependencies"
  )
  expect_equal(installer_calls, 0L)
})

test_that("_install turns engine failures into a visible error", {
  s <- make_functional_sandbox()
  env <- new.env(parent = baseenv())
  sys.source(file.path(s$project, "R", "attach.R"), envir = env)
  env$install_packages_in_order <- function(...) {
    list(installed = list(), failed = list(
      "aaalv_0.1.0" = "deliberate failure"
    ))
  }
  expect_error(
    env$functionalverse_install(s$archives),
    "aaalv_0.1.0: deliberate failure"
  )
})

test_that("generated metapackage contains no cleanup scripts", {
  s <- generate_in_sandbox()
  expect_false(file.exists(file.path(s$project, "cleanup")))
  expect_false(file.exists(file.path(s$project, "cleanup.win")))
})

test_that("generated startup hooks neither install nor remove files", {
  s <- generate_in_sandbox()
  files <- list.files(file.path(s$project, "R"), full.names = TRUE)
  code <- unlist(lapply(files, readLines, warn = FALSE))

  # Isolate .onLoad and .onAttach through the start of .onUnload.
  start <- grep("\\.onLoad <- function|\\.onAttach <- function", code)
  end <- grep("\\.onUnload <- function", code)
  expect_true(length(start) > 0)
  through <- if (length(end) > 0) max(end) else length(code)
  startup <- code[min(start):through]
  # Remove comments so safety notes do not match forbidden patterns.
  startup_cod <- sub("#.*$", "", startup)

  forbidden <- c("install\\.packages", "clean_pkg_dirs", "safe_unlink",
                 "unlink\\(", "\\brm -rf\\b", "rmdir", "list\\.files\\(tempdir",
                 "\\bsystem\\(")
  for (pattern in forbidden) {
    expect_false(any(grepl(pattern, startup_cod)),
                 label = paste0("startup is free of '", pattern, "'"))
  }
})
