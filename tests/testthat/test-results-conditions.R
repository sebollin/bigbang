test_that("generation returns a classified result and non-empty destinations are typed", {
  sandbox <- tempfile("bigbang-result-sandbox-")
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- tempfile("bigbang-result-")
  dir.create(sources, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  for (name in c("aaa", "bbb")) {
    pkg <- file.path(sources, name)
    dir.create(file.path(pkg, "R"), recursive = TRUE)
    writeLines(c(
      paste0("Package: ", name), "Version: 0.1.0",
      paste0("Title: Test ", name),
      "Description: A temporary component package.",
      "Authors@R: person('T','A',email='t@example.org',role=c('aut','cre'))",
      "License: MIT"
    ), file.path(pkg, "DESCRIPTION"))
    writeLines(character(), file.path(pkg, "NAMESPACE"))
    writeLines("value <- 1L", file.path(pkg, "R", "value.R"))
    withr::with_dir(sources, utils::tar(
      file.path(archives, paste0(name, "_0.1.0.tar.gz")),
      files = name, compression = "gzip"
    ))
  }
  result <- suppressMessages(create_metapackage(
    name = "resultverse",
    packages = c("aaa_0.1.0", "bbb_0.1.0"),
    pkg_dir = archives,
    dest_dir = destination,
    document = FALSE,
    verbose = FALSE,
    force_deps = character()
  ))
  expect_s3_class(result, "bigbang_result")
  expect_identical(result$name, "resultverse")
  expect_setequal(result$packages, c("aaa", "bbb"))
  expect_true(dir.exists(result$path))

  emitted <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), envir = emitted)
  sys.source(file.path(result$path, "R", "install_packages.R"), envir = emitted)
  emitted$read_archive_dependencies <- function(nombre_paquete, ...) {
    if (startsWith(nombre_paquete, "aaa_")) "bbb" else "aaa"
  }
  expect_error(
    emitted$build_dependency_graph(
      c("aaa_0.1.0", "bbb_0.1.0"), archives, ".tar.gz"
    ),
    class = "bigbang_error_cycle"
  )

  expect_error(
    create_metapackage(
      "resultverse", c("aaa_0.1.0", "bbb_0.1.0"), archives,
      dest_dir = destination,
      document = FALSE,
      verbose = FALSE
    ),
    class = "bigbang_error_nonempty_dest"
  )
})

test_that("artifact scans have a concise print method", {
  sandbox <- tempfile("bigbang-print-scan-")
  dir.create(sandbox)
  writeLines(c(
    "Package: cleanmeta", "Version: 0.1.0",
    "Title: Clean Meta", "Description: A clean temporary artifact.",
    "License: MIT"
  ), file.path(sandbox, "DESCRIPTION"))
  dir.create(file.path(sandbox, "R"))
  writeLines("value <- 1L", file.path(sandbox, "R", "value.R"))
  result <- scan_bigbang_artifact(sandbox)
  output <- capture.output(returned <- print(result))
  expect_identical(returned, result)
  expect_match(paste(output, collapse = "\n"), "no deletion signatures found")

  writeLines(
    "safe_unlink(pkg, recursive = TRUE, force = TRUE)",
    file.path(sandbox, "R", "legacy.R")
  )
  vulnerable <- scan_bigbang_artifact(sandbox)
  vulnerable_output <- capture.output(vulnerable_returned <- print(vulnerable))
  expect_identical(vulnerable_returned, vulnerable)
  expect_true(vulnerable$vulnerable)
  expect_match(paste(vulnerable_output, collapse = "\n"), "VULNERABLE")
  expect_match(
    paste(vulnerable_output, collapse = "\n"),
    "V1_component_unlink"
  )
})

test_that("generation and installation results print all summary branches", {
  generation <- structure(
    list(
      name = "printverse",
      path = file.path(tempdir(), "printverse"),
      packages = c("aaa", "bbb"),
      cran_dependencies = c("cli", "rlang")
    ),
    class = "bigbang_result"
  )
  generation_output <- capture.output(generation_returned <- print(generation))
  expect_identical(generation_returned, generation)
  expect_match(paste(generation_output, collapse = "\n"), "Package: printverse")
  expect_match(paste(generation_output, collapse = "\n"), "Components: aaa, bbb")
  expect_match(
    paste(generation_output, collapse = "\n"),
    "Non-local dependencies: cli, rlang"
  )

  installation <- structure(
    list(
      installed = list(aaa = "Installed successfully"),
      failed = list(bbb = "Installation failed"),
      skipped = list(ccc = "Offline policy")
    ),
    class = "bigbang_install_result"
  )
  installation_output <- capture.output(
    installation_returned <- print(installation)
  )
  expect_identical(installation_returned, installation)
  expect_match(paste(installation_output, collapse = "\n"), "Installed: 1")
  expect_match(paste(installation_output, collapse = "\n"), "Failed: 1")
  expect_match(paste(installation_output, collapse = "\n"), "Skipped: 1")
})
