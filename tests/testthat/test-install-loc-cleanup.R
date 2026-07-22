test_that("install_loc_pkg_w_dep limpia su temporal aun si sale antes", {
  if (!exists("install_loc_pkg_w_dep", mode = "function")) {
    sys.source(
      file.path(testthat::test_path(), "..", "..", "R", "crear_meta_paquete_local.R"),
      envir = globalenv()
    )
    sys.source(
      file.path(testthat::test_path(), "..", "..", "R", "install_loc_pkg_w_dep.R"),
      envir = globalenv()
    )
  }

  sandbox <- tempfile("install-helper-")
  dir.create(sandbox)
  contenido <- file.path(sandbox, "contenido")
  dir.create(contenido)
  writeLines("sin DESCRIPTION", file.path(contenido, "archivo.txt"))
  withr::with_dir(contenido, utils::tar(
    file.path(sandbox, "roto_0.1.0.tar.gz"),
    files = "archivo.txt", compression = "gzip"
  ))

  antes <- list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
  result <- install_loc_pkg_w_dep("roto_0.1.0", sandbox)
  expect_named(result$fallidos, "roto_0.1.0")
  expect_match(result$fallidos[[1L]], "Expected one DESCRIPTION")
  despues <- list.dirs(tempdir(), recursive = FALSE, full.names = TRUE)
  expect_setequal(despues, antes)
})

test_that("install_local_pkg reports a missing archive without side effects", {
  sandbox <- tempfile("install-helper-missing-")
  dir.create(sandbox)
  before <- list.files(sandbox, all.files = TRUE, no.. = TRUE)

  expect_message(
    result <- install_local_pkg(
      "absent_0.1.0", sandbox, cran_deps = "skip", verbose = TRUE
    ),
    "Packages that failed: absent_0.1.0"
  )

  expect_s3_class(result, "bigbang_install_result")
  expect_named(result$failed, "absent_0.1.0")
  expect_match(result$failed[[1L]], "Package archive does not exist")
  expect_identical(list.files(sandbox, all.files = TRUE, no.. = TRUE), before)
  expect_identical(.classify_local_archive("unused.tar.gz", ".tar.gz"), "source")
})

test_that("install_local_pkg recognizes a package version already installed", {
  sandbox <- tempfile("install-helper-present-")
  dir.create(sandbox)
  stem <- paste0("stats_", as.character(utils::packageVersion("stats")))
  archive <- file.path(sandbox, paste0(stem, ".tar.gz"))
  expect_true(file.create(archive))

  result <- install_local_pkg(stem, sandbox, verbose = FALSE)

  expect_s3_class(result, "bigbang_install_result")
  expect_identical(result$installed[[stem]], "Already installed")
  expect_length(result$failed, 0L)
  expect_length(result$skipped, 0L)
  expect_true(file.exists(archive))
})

test_that("ZIP classification rejects archives without DESCRIPTION", {
  sandbox <- tempfile("install-helper-zip-")
  dir.create(sandbox)
  writeLines("not a package", file.path(sandbox, "payload.txt"))
  archive <- file.path(sandbox, "invalid.zip")
  invisible(capture.output(
    withr::with_dir(sandbox, utils::zip(archive, "payload.txt", flags = "-q"))
  ))

  expect_error(
    .classify_local_archive(archive, ".zip"),
    "does not contain a DESCRIPTION"
  )
  expect_true(file.exists(archive))
})
