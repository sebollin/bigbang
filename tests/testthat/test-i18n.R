translate_from_catalog <- function(domain, bind_dir, message) {
  code <- paste0(
    "candidates <- c('es_ES.UTF-8', 'es_ES.utf8', ",
    "'Spanish_Spain.utf8', 'Spanish_Spain.1252', ",
    "'en_US.UTF-8', 'English_United States.utf8'); ",
    "for (candidate in candidates) { ",
    "changed <- suppressWarnings(Sys.setlocale('LC_MESSAGES', candidate)); ",
    "if (!is.na(changed) && !grepl('^(C|POSIX)', changed)) break }; ",
    "Sys.setenv(LANGUAGE = 'es'); ",
    "probe <- tryCatch(get('definitely_missing_bigbang_probe'), ",
    "error = conditionMessage); ",
    "writeLines(paste0('GETTEXT_PROBE|', probe)); ",
    "invisible(bindtextdomain(", deparse(domain), ", ", deparse(bind_dir), ")); ",
    "writeLines(gettext(", deparse(message), ", domain = ", deparse(domain), "))"
  )
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  testthat::expect_equal(status, 0L, info = paste(output, collapse = "\n"))
  probe_line <- grep("^GETTEXT_PROBE\\|", output, value = TRUE)
  testthat::expect_length(probe_line, 1L)
  probe <- sub("^GETTEXT_PROBE\\|", "", probe_line)
  if (!grepl("objeto", tolower(probe), fixed = TRUE)) {
    testthat::skip(
      "This platform cannot activate Spanish gettext catalogs in a child R process."
    )
  }
  output[!startsWith(output, "GETTEXT_PROBE|")]
}

test_that("the bigbang runtime catalog translates messages to Spanish", {
  installed_catalog <- system.file(
    "po", "es", "LC_MESSAGES", "R-bigbang.mo", package = "bigbang"
  )
  bind_dir <- if (nzchar(installed_catalog)) {
    dirname(dirname(dirname(installed_catalog)))
  } else {
    normalizePath(file.path(testthat::test_path(), "..", "..", "inst", "po"))
  }
  output <- translate_from_catalog(
    "R-bigbang", bind_dir, "The directory specified by 'pkg_dir' does not exist"
  )
  expect_identical(
    tail(output, 1L),
    "El directorio indicado por 'pkg_dir' no existe"
  )
})

test_that("generated metapackages include their own Spanish runtime catalog", {
  skip_if_not(exists("create_metapackage", mode = "function"))
  sandbox <- tempfile("bigbang-i18n-meta-")
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(file.path(sources, "i18npkg", "R"), recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  writeLines(c(
    "Package: i18npkg", "Version: 0.1.0", "Title: I18n Component",
    "Description: A temporary package used to test translations.",
    "Authors@R: person('T','A',email='t@example.org',role=c('aut','cre'))",
    "License: MIT"
  ), file.path(sources, "i18npkg", "DESCRIPTION"))
  writeLines(character(), file.path(sources, "i18npkg", "NAMESPACE"))
  writeLines("value <- 1L", file.path(sources, "i18npkg", "R", "value.R"))
  withr::with_dir(sources, utils::tar(
    file.path(archives, "i18npkg_0.1.0.tar.gz"),
    files = "i18npkg", compression = "gzip"
  ))
  result <- suppressMessages(create_metapackage(
    "i18nverse", "i18npkg_0.1.0", archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  ))
  po <- file.path(result$path, "po", "R-es.po")
  mo <- file.path(
    result$path, "inst", "po", "es", "LC_MESSAGES", "R-i18nverse.mo"
  )
  expect_true(file.exists(po))
  expect_true(file.exists(mo))
  emitted_code <- unlist(lapply(
    list.files(file.path(result$path, "R"), full.names = TRUE),
    readLines, warn = FALSE
  ))
  expect_false(any(grepl("[áéíóúÁÉÍÓÚñÑ¿¡]", emitted_code)))
  previous_name <- paste0("local", "verse")
  expect_false(any(grepl(previous_name, emitted_code, fixed = TRUE)))
  expect_match(paste(readLines(po, encoding = "UTF-8"), collapse = "\n"),
               "Instalación completa")
  output <- translate_from_catalog(
    "R-i18nverse", file.path(result$path, "inst", "po"),
    "Installation complete."
  )
  expect_identical(tail(output, 1L), "Instalación completa.")
})

test_that("Spanish catalogs are complete and preserve format placeholders", {
  local_catalog <- .bigbang_spanish_catalog()
  meta_catalog <- .metapackage_spanish_catalog("catalogverse")

  expect_gt(length(local_catalog), 50L)
  expect_gt(length(meta_catalog), 30L)
  expect_identical(anyDuplicated(names(local_catalog)), 0L)
  expect_identical(anyDuplicated(names(meta_catalog)), 0L)
  expect_true(all(nzchar(names(local_catalog))))
  expect_true(all(nzchar(unname(local_catalog))))
  expect_true(all(nzchar(names(meta_catalog))))
  expect_true(all(nzchar(unname(meta_catalog))))

  placeholders <- function(text) {
    matches <- gregexpr("%(?:[0-9]+\\$)?[a-zA-Z]", text, perl = TRUE)
    lapply(regmatches(text, matches), sort)
  }
  expect_identical(
    placeholders(names(local_catalog)),
    placeholders(unname(local_catalog))
  )
  expect_identical(
    placeholders(names(meta_catalog)),
    placeholders(unname(meta_catalog))
  )
  install_messages <- grep("Run catalogverse_install", names(meta_catalog))
  expect_true(all(grepl(
    "catalogverse_install", unname(meta_catalog[install_messages]), fixed = TRUE
  )))
})
