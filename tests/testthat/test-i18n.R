run_spanish_child <- function(domain, bind_dir, code) {
  child_code <- paste0(
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
    code
  )
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(child_code)),
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

translate_from_catalog <- function(domain, bind_dir, message) {
  run_spanish_child(
    domain,
    bind_dir,
    paste0(
      "writeLines(gettext(", deparse(message),
      ", domain = ", deparse(domain), "))"
    )
  )
}

bigbang_catalog_dir <- function() {
  source_catalog <- file.path(
    testthat::test_path(), "..", "..", "inst", "po", "es", "LC_MESSAGES",
    "R-bigbang.mo"
  )
  installed_catalog <- system.file(
    "po", "es", "LC_MESSAGES", "R-bigbang.mo", package = "bigbang"
  )
  catalog <- if (file.exists(source_catalog)) source_catalog else installed_catalog
  if (nzchar(catalog)) {
    dirname(dirname(dirname(normalizePath(catalog, winslash = "/"))))
  } else {
    normalizePath(
      file.path(testthat::test_path(), "..", "..", "inst", "po"),
      winslash = "/"
    )
  }
}

bigbang_child_load_code <- function() {
  source_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/", mustWork = FALSE
  )
  if (file.exists(file.path(source_root, "R", "create_metapackage.R"))) {
    paste0(
      "devtools::load_all(",
      deparse(source_root), ", quiet = TRUE); "
    )
  } else {
    "library(bigbang); "
  }
}

test_that("the bigbang runtime catalog translates messages to Spanish", {
  bind_dir <- bigbang_catalog_dir()
  output <- translate_from_catalog(
    "R-bigbang", bind_dir, "The directory specified by 'pkg_dir' does not exist"
  )
  expect_identical(
    tail(output, 1L),
    "El directorio indicado por 'pkg_dir' no existe"
  )

  destination_message <- paste0(
    "'dest_dir' must be supplied as one non-empty path: the meta-package is ",
    "written inside it. Use tempdir() for disposable output."
  )
  destination_output <- translate_from_catalog(
    "R-bigbang", bind_dir, destination_message
  )
  expect_identical(
    tail(destination_output, 1L),
    paste0(
      "'dest_dir' debe proporcionarse como una ruta no vac\u00eda: el metapaquete ",
      "se escribe dentro de ella. Use tempdir() para una salida descartable."
    )
  )

  collision_message <- paste0(
    "Cannot re-export symbol(s) %s because components collide: %s."
  )
  collision_output <- translate_from_catalog(
    "R-bigbang", bind_dir, collision_message
  )
  expect_identical(
    tail(collision_output, 1L),
    paste0(
      "No se pueden reexportar los s\u00edmbolos %s porque hay colisiones ",
      "entre componentes: %s."
    )
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
  expect_false(any(grepl(
    "[\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00f1\u00d1\u00bf\u00a1]",
    emitted_code
  )))
  previous_name <- paste0("local", "verse")
  expect_false(any(grepl(previous_name, emitted_code, fixed = TRUE)))
  expect_match(paste(readLines(po, encoding = "UTF-8"), collapse = "\n"),
               "Instalaci\u00f3n completa")
  output <- translate_from_catalog(
    "R-i18nverse", file.path(result$path, "inst", "po"),
    "Installation complete."
  )
  expect_identical(tail(output, 1L), "Instalaci\u00f3n completa.")
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
  expect_identical(names(local_catalog), trimws(names(local_catalog)))
  expect_identical(unname(local_catalog), trimws(unname(local_catalog)))
  expect_identical(names(meta_catalog), trimws(names(meta_catalog)))
  expect_identical(unname(meta_catalog), trimws(unname(meta_catalog)))

  reexport_messages <- c(
    "'reexport' must be TRUE or FALSE",
    "Could not read NAMESPACE from archive %s: the file is missing.",
    "Could not read NAMESPACE from archive %s: %s",
    paste0(
      "Could not determine explicit exports in NAMESPACE from archive %s: ",
      "export patterns are not supported for reexport."
    ),
    "Could not read explicit exports from NAMESPACE in archive %s.",
    "Cannot re-export symbol(s) %s because components collide: %s.",
    paste0(
      "Cannot re-export symbol(s) %s because they belong to generated ",
      "metapackage code."
    )
  )
  expect_true(all(reexport_messages %in% names(local_catalog)))
  expect_false(any(grepl(
    "reexport' is deprecated and ignored", names(local_catalog), fixed = TRUE
  )))

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
  stem_collision <- "More than one archive was found for component stem '%s': %s."
  expect_identical(
    unname(meta_catalog[stem_collision]),
    "Se encontr\u00f3 m\u00e1s de un archivo para el stem componente '%s': %s."
  )
})

test_that("messages formerly keyed with edge whitespace translate at runtime", {
  skip_on_cran()
  skip_if(Sys.which("zip") == "", "the zip utility is unavailable")
  sandbox <- tempfile("bigbang-i18n-runtime-")
  dir.create(sandbox)
  empty_zip_root <- file.path(sandbox, "empty-zip")
  dir.create(empty_zip_root)
  writeLines("not a package", file.path(empty_zip_root, "empty.txt"))
  empty_zip <- file.path(sandbox, "empty.zip")
  withr::with_dir(empty_zip_root, utils::zip(
    empty_zip, files = "empty.txt", flags = "-q"
  ))

  bad_archives <- file.path(sandbox, "bad-archives")
  dir.create(bad_archives)
  writeLines("not an archive", file.path(bad_archives, "badpkg_0.1.0.bad"))

  skipped_source <- file.path(sandbox, "skipped-source")
  skipped_pkg <- file.path(skipped_source, "skippkg")
  dir.create(file.path(skipped_pkg, "R"), recursive = TRUE)
  writeLines(c(
    "Package: skippkg", "Version: 0.1.0", "Title: Skipped Package",
    "Description: Temporary package for translation tests.",
    "Authors@R: person('T','A',email='t@example.org',role=c('aut','cre'))",
    "License: MIT", "Imports: definitely.not.installed.bigbang"
  ), file.path(skipped_pkg, "DESCRIPTION"))
  writeLines(character(), file.path(skipped_pkg, "NAMESPACE"))
  writeLines("value <- 1L", file.path(skipped_pkg, "R", "value.R"))
  skipped_archives <- file.path(sandbox, "skipped-archives")
  dir.create(skipped_archives)
  withr::with_dir(skipped_source, utils::tar(
    file.path(skipped_archives, "skippkg_0.1.0.tar.gz"),
    files = "skippkg", compression = "gzip"
  ))

  no_description <- file.path(sandbox, "no-description")
  dir.create(no_description)
  unsupported_artifact <- file.path(sandbox, "artifact.txt")
  writeLines("artifact", unsupported_artifact)
  test_library <- file.path(sandbox, "library")
  dir.create(test_library)
  toy_archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(toy_archive)) {
    toy_archive <- normalizePath(
      testthat::test_path(
        "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
      ),
      winslash = "/"
    )
  }
  toy_archives <- dirname(toy_archive)

  code <- paste0(
    bigbang_child_load_code(),
    ".libPaths(c(", deparse(test_library), ", .Library)); ",
    "capture_error <- function(expr) tryCatch(expr, error = conditionMessage); ",
    "capture_messages <- function(expr) paste(capture.output(expr, type = 'message'), collapse = '\\n'); ",
    "classify <- get('.classify_local_archive', envir = asNamespace('bigbang')); ",
    "validate <- get('.validate_archive_members', envir = asNamespace('bigbang')); ",
    "writeLines(paste0('ZIP|', capture_error(classify(", deparse(empty_zip), ", '.zip')))); ",
    "bad <- install_local_pkg('badpkg_0.1.0', ", deparse(bad_archives), ", ext = '.bad'); ",
    "writeLines(paste0('FORMAT|', bad$failed[['badpkg_0.1.0']])); ",
    "writeLines(paste0('INSTALLED|', capture_messages(install_local_pkg('toycomponent_0.1.0', ",
    deparse(toy_archives), ", verbose = TRUE)))); ",
    "writeLines(paste0('FAILED|', capture_messages(install_local_pkg('absent_0.1.0', ",
    deparse(bad_archives), ", verbose = TRUE)))); ",
    "writeLines(paste0('SKIPPED|', capture_messages(install_local_pkg('skippkg_0.1.0', ",
    deparse(skipped_archives), ", verbose = TRUE)))); ",
    "writeLines(paste0('MISSING|', capture_error(scan_bigbang_artifact(",
    deparse(file.path(sandbox, "missing")), ")))); ",
    "writeLines(paste0('TYPE|', capture_error(scan_bigbang_artifact(",
    deparse(unsupported_artifact), ")))); ",
    "writeLines(paste0('DESCRIPTION|', capture_error(scan_bigbang_artifact(",
    deparse(no_description), ")))); ",
    "writeLines(paste0('UNSAFE|', capture_error(validate('../escape')))); ",
    "writeLines(paste0('TOLERATE|', capture_error(create_metapackage(",
    "'tolverse', 'missing', dest_dir = tempdir(), document = FALSE, ",
    "tolerate = 'unknown'))))"
  )
  output <- run_spanish_child("R-bigbang", bigbang_catalog_dir(), code)
  translated <- paste(output, collapse = "\n")

  expect_match(translated, "El archivo ZIP no contiene DESCRIPTION", fixed = TRUE)
  expect_match(translated, "Formato de archivo no compatible", fixed = TRUE)
  expect_match(translated, "Paquete local instalado", fixed = TRUE)
  expect_match(translated, "Paquetes que fallaron", fixed = TRUE)
  expect_match(
    translated, "Paquetes omitidos por la pol\u00edtica offline", fixed = TRUE
  )
  expect_match(translated, "El artefacto no existe", fixed = TRUE)
  expect_match(translated, "Tipo de artefacto no compatible", fixed = TRUE)
  expect_match(
    translated, "No se encontr\u00f3 DESCRIPTION en el directorio fuente", fixed = TRUE
  )
  expect_match(translated, "rutas absolutas", fixed = TRUE)
  expect_match(translated, "Tolerancias desconocidas", fixed = TRUE)
})

test_that("template diagnostics translate without edge whitespace", {
  sandbox <- tempfile("bigbang-i18n-template-")
  dir.create(sandbox)
  code <- paste0(
    bigbang_child_load_code(),
    "whisker_ns <- asNamespace('whisker'); ",
    "unlockBinding('whisker.render', whisker_ns); ",
    "assign('whisker.render', function(...) stop('forced render failure'), envir = whisker_ns); ",
    "lockBinding('whisker.render', whisker_ns); ",
    "writer <- get('write_metapackage_files', envir = asNamespace('bigbang')); ",
    "messages <- capture.output(writer(name = 'diagverse', packages = 'toycomponent', ",
    "archive_stems = 'toycomponent_0.1.0', dest_dir = ",
    deparse(file.path(sandbox, "R")), ", verbose = TRUE), type = 'message'); ",
    "writeLines(messages)"
  )
  output <- run_spanish_child("R-bigbang", bigbang_catalog_dir(), code)
  translated <- paste(output, collapse = "\n")
  expect_match(translated, "Plantilla original:", fixed = TRUE)
  expect_match(translated, "Datos de la plantilla:", fixed = TRUE)
})

test_that("generated cli startup message translates to Spanish", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-i18n-startup-")
  destination <- file.path(sandbox, "output")
  library <- file.path(sandbox, "library")
  dir.create(destination, recursive = TRUE)
  dir.create(library)
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  result <- create_metapackage(
    "spanishverse", "toycomponent_0.1.0", dirname(fixture),
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
  code <- paste0(
    ".libPaths(c(", deparse(library), ", .libPaths())); ",
    "utils::install.packages(", deparse(result$path),
    ", repos = NULL, type = 'source', lib = ", deparse(library), "); ",
    "invisible(bindtextdomain('R-spanishverse', file.path(",
    deparse(library), ", 'spanishverse', 'po'))); ",
    "library(spanishverse, lib.loc = ", deparse(library), ")"
  )
  output <- run_spanish_child("R-bigbang", bigbang_catalog_dir(), code)
  translated <- paste(output, collapse = "\n")
  expect_match(translated, "Adjuntando paquetes", fixed = TRUE)
  expect_match(translated, "Faltan componentes por instalar", fixed = TRUE)
})
