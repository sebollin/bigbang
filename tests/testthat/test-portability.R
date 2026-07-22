make_portability_package <- function(name, root, imports = NULL) {
  pkg <- file.path(root, name)
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: Portability test package ", name),
    "Version: 0.1.0",
    "Authors@R: person('Test', 'Author', email='test@example.org', role=c('aut','cre'))",
    "Description: A package used only in temporary portability tests.",
    "License: MIT",
    "Encoding: UTF-8",
    if (!is.null(imports)) paste0("Imports: ", imports)
  ), file.path(pkg, "DESCRIPTION"), useBytes = TRUE)
  writeLines(paste0("export(value_", gsub("\\.", "_", name), ")"),
             file.path(pkg, "NAMESPACE"), useBytes = TRUE)
  writeLines(
    paste0("value_", gsub("\\.", "_", name), " <- function() 'ok'"),
    file.path(pkg, "R", "value.R"), useBytes = TRUE
  )
  pkg
}

test_that("all generated text files are valid UTF-8 in the C locale", {
  skip_if_not_installed("brio")
  skip_if_not_installed("whisker")
  skip_if_not_installed("withr")
  sandbox <- tempfile("bigbang-utf8-")
  sources <- file.path(sandbox, "sources")
  archive_name <- if (.Platform$OS.type == "windows") {
    file.path("files with spaces", "archives")
  } else {
    "C:\\Users\\Sebastian\\archives"
  }
  archives <- file.path(sandbox, archive_name)
  destination <- file.path(sandbox, "destination")
  dir.create(sources, recursive = TRUE)
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  make_portability_package("mi.pkg", sources)
  withr::with_dir(sources, utils::tar(
    file.path(archives, "mi.pkg_0.1.0.tar.gz"),
    files = "mi.pkg", compression = "gzip"
  ))

  withr::with_locale(c(LC_CTYPE = "C"), suppressMessages(
    create_metapackage(
      "portablemeta", "mi.pkg_0.1.0", archives,
      dest_dir = destination,
      document = FALSE,
      verbose = FALSE,
      description = "Metapackage with UTF-8 text: café, jalapeño, naïve."
    )
  ))
  project <- file.path(destination, "portablemeta")
  text_files <- list.files(
    project, recursive = TRUE, all.files = TRUE, full.names = TRUE
  )
  text_files <- text_files[!dir.exists(text_files)]
  # Compiled gettext catalogs are binary, not emitted text files.
  text_files <- text_files[!grepl("\\.mo$", text_files)]
  expect_gt(length(text_files), 5L)
  for (path in text_files) {
    bytes <- readBin(path, what = "raw", n = file.info(path)$size)
    value <- rawToChar(bytes)
    expect_true(validUTF8(value), info = path)
  }

  patterns <- readLines(file.path(project, ".Rbuildignore"), encoding = "UTF-8")
  exact_pattern <- patterns[patterns == "^mi\\.pkg$"]
  expect_length(exact_pattern, 1L)
  expect_true(grepl(exact_pattern, "mi.pkg"))
  expect_false(grepl(exact_pattern, "miXpkg"))

  attach_env <- new.env(parent = baseenv())
  sys.source(file.path(project, "R", "attach.R"), envir = attach_env)
  expect_named(
    formals(attach_env$portablemeta_install),
    c("pkg_dir", "ext", "cran_deps", "repos", "verbose")
  )
  install_default <- formals(attach_env$portablemeta_install)$pkg_dir
  expect_identical(eval(install_default), archives)
})

test_that("ZIP content distinguishes source archives from Windows binaries", {
  skip_if_not_installed("withr")
  skip_if(Sys.which("zip") == "", "the zip utility is unavailable")
  sandbox <- tempfile("bigbang-zip-")
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  dir.create(sources, recursive = TRUE)
  dir.create(archives)
  make_portability_package("ziplv", sources)
  withr::with_dir(sources, utils::zip(
    file.path(archives, "ziplv_0.1.0.zip"), files = "ziplv", flags = "-rq"
  ))
  expect_identical(
    .classify_local_archive(file.path(archives, "ziplv_0.1.0.zip"), ".zip"),
    "source.zip"
  )
  test_lib <- file.path(sandbox, "library")
  dir.create(test_lib)
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  .libPaths(c(test_lib, old_libs))
  source_result <- suppressMessages(install_local_pkg(
    "ziplv_0.1.0", archives, ext = ".zip", cran_deps = "skip"
  ))
  expect_named(source_result$installed, "ziplv_0.1.0")
  expect_true(requireNamespace("ziplv", quietly = TRUE))

  binary_root <- file.path(sandbox, "binary")
  dir.create(file.path(binary_root, "binlv", "Meta"), recursive = TRUE)
  writeLines(c("Package: binlv", "Version: 0.1.0"),
             file.path(binary_root, "binlv", "DESCRIPTION"))
  saveRDS(list(Package = "binlv"), file.path(binary_root, "binlv", "Meta", "package.rds"))
  withr::with_dir(binary_root, utils::zip(
    file.path(archives, "binlv_0.1.0.zip"), files = "binlv", flags = "-rq"
  ))
  expect_identical(
    .classify_local_archive(file.path(archives, "binlv_0.1.0.zip"), ".zip"),
    "win.binary"
  )
})

test_that("offline skip never calls install.packages for missing CRAN dependencies", {
  sandbox <- tempfile("bigbang-offline-")
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  dir.create(sources, recursive = TRUE)
  dir.create(archives)
  make_portability_package("offlinelv", sources, imports = "definitely.not.installed.lv")
  withr::with_dir(sources, utils::tar(
    file.path(archives, "offlinelv_0.1.0.tar.gz"),
    files = "offlinelv", compression = "gzip"
  ))

  result <- install_local_pkg(
    "offlinelv_0.1.0", archives, cran_deps = "skip", repos = NULL
  )
  expect_length(result$failed, 0L)
  expect_named(result$skipped, "offlinelv_0.1.0")
  expect_false(requireNamespace("offlinelv", quietly = TRUE))
})
