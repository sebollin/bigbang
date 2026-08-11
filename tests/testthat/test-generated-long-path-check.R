test_that("generated documentation keeps long archive paths out of usage", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  latex_tools <- Sys.which(c("pdflatex", "makeindex"))
  skip_if(any(!nzchar(latex_tools)), "LaTeX tools required for the PDF manual")

  sandbox <- tempfile("bigbang-long-path-check-")
  long_component <- paste(rep("local-package-archives", 7L), collapse = "-")
  archives <- file.path(sandbox, long_component)
  destination <- file.path(sandbox, "generated")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  normalized_archives <- normalizePath(archives, winslash = "/")
  expect_gt(nchar(normalized_archives), 120L)

  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(fixture)) {
    fixture <- testthat::test_path(
      "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
    )
  }
  file.copy(fixture, archives)

  result <- suppressWarnings(create_metapackage(
    name = "longpathverse",
    packages = "toycomponent_0.1.0",
    pkg_dir = archives,
    dest_dir = destination,
    document = TRUE,
    verbose = FALSE,
    description = "Long Path Meta-Package",
    import_deps = character(),
    force_deps = character()
  ))
  expect_true(result$documented)

  generated_r <- unlist(lapply(
    list.files(file.path(result$path, "R"), full.names = TRUE),
    readLines,
    warn = FALSE
  ), use.names = FALSE)
  expect_path_absent(archives, generated_r)
  install_rd <- readLines(
    file.path(result$path, "man", "longpathverse_install.Rd"), warn = FALSE
  )
  expect_path_absent(archives, install_rd)
  # The usage shows the portable default instead of this machine's archive
  # path, which is what keeps the Rd line widths within the limit.
  expect_true(any(grepl(
    'system.file("archives", package = "longpathverse")',
    install_rd, fixed = TRUE
  )))

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  withr::local_envvar(c(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false"))
  build_output <- withr::with_dir(sandbox, system2(
    r_binary,
    c("CMD", "build", shQuote(result$path)),
    stdout = TRUE,
    stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))

  archive <- file.path(sandbox, "longpathverse_0.1.0.tar.gz")
  expect_true(file.exists(archive))
  check_output <- withr::with_dir(sandbox, system2(
    r_binary,
    c("CMD", "check", "--as-cran", shQuote(archive)),
    stdout = TRUE,
    stderr = TRUE
  ))
  check_status <- attr(check_output, "status")
  if (is.null(check_status)) check_status <- 0L
  expect_identical(check_status, 0L, info = paste(check_output, collapse = "\n"))

  check_log <- readLines(
    file.path(sandbox, "longpathverse.Rcheck", "00check.log"), warn = FALSE
  )
  expect_true(any(grepl("checking Rd line widths ... OK", check_log, fixed = TRUE)))
  expect_false(any(grepl("These lines will be truncated", check_log, fixed = TRUE)))
  expect_true(any(grepl("checking PDF version of manual ... OK", check_log, fixed = TRUE)))
})
