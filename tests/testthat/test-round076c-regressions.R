.r076c_make_archive <- function(name, version, archive_path, root,
                                imports = NULL) {
  package_root <- file.path(root, paste0(name, "-r076c"))
  dir.create(file.path(package_root, "R"), recursive = TRUE,
             showWarnings = FALSE)
  fields <- c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: R076c fixture ", name),
    paste0("Version: ", version),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    paste0("Description: Temporary R076c fixture for ", name, "."),
    "License: MIT"
  )
  if (!is.null(imports)) fields <- c(fields, paste0("Imports: ", imports))
  writeLines(fields, file.path(package_root, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(r076c_value)", file.path(package_root, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("r076c_value <- function() 76L",
             file.path(package_root, "R", "value.R"), useBytes = TRUE)
  withr::with_dir(root, utils::tar(
    archive_path, basename(package_root), compression = "gzip"
  ))
  archive_path
}

.r076c_runtime <- function(project) {
  runtime <- new.env(parent = baseenv())
  for (file in list.files(file.path(project, "R"), full.names = TRUE,
                          pattern = "[.]R$")) {
    sys.source(file, runtime)
  }
  runtime
}

test_that("gzip ISIZE lies are rejected under timeout and memory limits", {
  skip_on_os("windows")
  root <- tempfile("bigbang-r076c-isize-")
  dir.create(root)
  probe <- file.path(root, "probe.R")
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "pkg <- args[[1L]]; d <- args[[2L]]",
    "source_files <- list.files(file.path(pkg, 'R'), pattern = '[.]R$')",
    "if (file.exists(file.path(pkg, 'DESCRIPTION')) &&",
    "    length(source_files) > 0) {",
    "  suppressMessages(pkgload::load_all(pkg, quiet = TRUE))",
    "} else {",
    "  suppressMessages(library(bigbang))",
    "}",
    "ns <- asNamespace('bigbang')",
    "archive_inspect <- get('.archive_inspect', envir = ns)",
    "archive_trailer <- get('.archive_gzip_member_trailer', envir = ns)",
    "archive_le32 <- get('.archive_le32', envir = ns)",
    "src <- file.path(d, 'toy'); dir.create(file.path(src, 'R'), recursive = TRUE)",
    "fields <- c('Package: toyisize', 'Version: 0.1.0', 'Title: T',",
    "  'Description: T.', 'License: MIT')",
    "writeLines(fields, file.path(src, 'DESCRIPTION'))",
    "writeLines(strrep('x <- 1\\n', 20000), file.path(src, 'R', 'a.R'))",
    "old <- setwd(d); utils::tar('toyisize_0.1.0.tar.gz', 'toy', compression = 'gzip'); setwd(old)",
    "f <- file.path(d, 'toyisize_0.1.0.tar.gz'); b <- readBin(f, 'raw', file.info(f)$size)",
    "n <- length(b); trailer <- b[(n - 7L):n]",
    "real_size <- archive_trailer(f)$size",
    "cases <- list(",
    "  isize0 = archive_le32(0), isize1 = archive_le32(1),",
    "  isize_minus = archive_le32(real_size - 1),",
    "  isize_plus = archive_le32(real_size + 1)",
    ")",
    "for (label in names(cases)) {",
    "  x <- b; x[(n - 3L):n] <- cases[[label]]",
    "  path <- file.path(d, paste0(label, '.tar.gz')); writeBin(x, path)",
    "  elapsed <- system.time(result <- tryCatch(",
    "    archive_inspect(path, '.tar.gz'), error = identity",
    "  ))['elapsed']",
    "  if (!inherits(result, 'error') || elapsed >= 1) stop(label)",
    "}",
    "x <- b; x[n - 7L] <- as.raw(bitwXor(as.integer(x[n - 7L]), 255L))",
    "path <- file.path(d, 'r33.tar.gz'); writeBin(c(x, trailer), path)",
    "elapsed <- system.time(result <- tryCatch(",
    "  archive_inspect(path, '.tar.gz'), error = identity",
    "))['elapsed']",
    "if (!inherits(result, 'error') || elapsed >= 1) stop('r33')",
    "cat('ok\\n')"
  ), probe)
  pkg <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)
  command <- paste(
    "ulimit -v 2000000;",
    "timeout 15s", shQuote(file.path(R.home("bin"), "Rscript")),
    "--vanilla",
    shQuote(probe), shQuote(pkg), shQuote(root)
  )
  output <- system2("sh", c("-c", shQuote(command)),
                    stdout = TRUE, stderr = TRUE)
  expect_match(paste(output, collapse = "\n"), "ok", fixed = TRUE)
})

test_that("partial plans and installs agree on component external constraints", {
  root <- tempfile("bigbang-r076c-diamond-")
  archives <- file.path(root, "archives")
  external_archives <- file.path(root, "external-archives")
  sources <- file.path(root, "sources")
  destination <- file.path(root, "destination")
  lib <- file.path(root, "lib")
  direct_lib <- file.path(root, "direct-lib")
  dir.create(archives, recursive = TRUE)
  dir.create(external_archives)
  dir.create(sources)
  dir.create(destination)
  dir.create(lib)
  dir.create(direct_lib)

  ext <- .r076c_make_archive(
    "extpkg", "1.5.0", file.path(external_archives, "extpkg_1.5.0.tar.gz"), sources
  )
  b <- .r076c_make_archive(
    "diaB", "1.0.0", file.path(archives, "diaB_1.0.0.tar.gz"), sources,
    imports = "extpkg (>= 2.0.0)"
  )
  c <- .r076c_make_archive(
    "diaC", "1.0.0", file.path(archives, "diaC_1.0.0.tar.gz"), sources,
    imports = "extpkg (>= 1.0.0)"
  )
  a <- .r076c_make_archive(
    "diaA", "1.0.0", file.path(archives, "diaA_1.0.0.tar.gz"), sources,
    imports = "diaB, diaC"
  )
  expect_length(expect_no_warning(bigbang::install_local_pkg(
    ext, lib = lib, upgrade = "always", verbose = FALSE
  ))$failed, 0L)
  generated <- round075_create_metapackage(
    "diamondverse", c(a, b, c), dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  runtime <- .r076c_runtime(generated$path)
  plan <- runtime$diamondverse_deps(
    pkg_dir = archives, plan = TRUE, only = "diaA",
    cran_deps = "skip", upgrade = "always", lib = lib
  )
  plan_rows <- plan$components
  rownames(plan_rows) <- plan_rows$component
  expect_identical(plan_rows["diaB", "action"], "skip")
  expect_identical(plan_rows["diaC", "action"], "install")
  expect_identical(plan_rows["diaC", "reason"], "not_installed")
  expect_identical(plan_rows["diaA", "reason"], "local_dependency_failed")
  expect_setequal(plan$components$component, c("diaA", "diaB", "diaC"))

  captured <- capture_warnings(runtime$diamondverse_install(
    pkg_dir = archives, only = "diaA", cran_deps = "skip",
    upgrade = "always", verbose = FALSE, lib = lib
  ))
  installed <- captured$value
  expect_warning_set(captured$warnings, c(
    "Skipped diaB_1.0.0: Skipped because non-local dependencies are missing",
    "Skipped diaA_1.0.0: Skipped because local dependency diaB was not installed",
    "Some components were skipped because non-local dependencies are missing",
    "Some components were skipped because local dependencies were skipped"
  ))
  expect_true("diaC_1.0.0" %in% names(installed$installed))
  expect_true("diaB_1.0.0" %in% names(installed$skipped))
  expect_true("diaA_1.0.0" %in% names(installed$skipped))
  expect_setequal(
    installed$selected,
    c("diaA_1.0.0", "diaB_1.0.0", "diaC_1.0.0")
  )

  after <- runtime$diamondverse_deps(
    pkg_dir = archives, plan = TRUE, only = "diaA",
    cran_deps = "skip", upgrade = "always", lib = lib
  )
  after_c <- after$components[after$components$component == "diaC", , drop = FALSE]
  expect_identical(after_c$reason, "forced_reinstall")

  expect_length(expect_no_warning(bigbang::install_local_pkg(
    ext, lib = direct_lib, upgrade = "always", verbose = FALSE
  ))$failed, 0L)
  direct <- expect_no_warning(bigbang::install_local_pkg(
    a, pkg_dir = archives, cran_deps = "skip", upgrade = "always",
    verbose = FALSE, lib = direct_lib
  ))
  expect_true("diaC_1.0.0" %in% names(direct$installed))
  expect_true("diaB_1.0.0" %in% names(direct$skipped))
  expect_true("diaA_1.0.0" %in% names(direct$skipped))
})

test_that("forced reinstall has an exact plan reason", {
  expect_identical(
    bigbang:::.component_upgrade_action(
      base::package_version("2.0.0"), "1.0.0", "always"
    )$reason_code,
    "forced_reinstall"
  )
})
