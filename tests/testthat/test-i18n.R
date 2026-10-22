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

ast_call_name <- function(node) {
  if (!is.call(node)) return("")
  head <- node[[1L]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) {
    return(as.character(head[[3L]]))
  }
  ""
}

ast_literal_text <- function(node) {
  if (is.character(node) && length(node) == 1L) return(node)
  if (!is.call(node) || !ast_call_name(node) %in% c("paste", "paste0")) {
    return(NULL)
  }
  tags <- names(node)
  if (is.null(tags)) tags <- rep("", length(node))
  argument_positions <- if (length(node) > 1L) {
    seq.int(2L, length(node))
  } else {
    integer()
  }
  argument_positions <- argument_positions[
    !tags[argument_positions] %in% c("sep", "collapse")
  ]
  values <- lapply(as.list(node)[argument_positions], ast_literal_text)
  if (!all(vapply(values, Negate(is.null), logical(1L)))) return(NULL)
  if (identical(ast_call_name(node), "paste0")) {
    return(paste0(unlist(values, use.names = FALSE), collapse = ""))
  }
  separator <- if ("sep" %in% tags) {
    ast_literal_text(node[[which(tags == "sep")[[1L]]]])
  } else {
    " "
  }
  if (is.null(separator)) return(NULL)
  paste(unlist(values, use.names = FALSE), collapse = separator)
}

ast_unknown_value <- function() {
  list(raw = character(), translated = FALSE)
}

ast_safe_value <- function() {
  list(raw = character(), translated = TRUE)
}

ast_raw_value <- function(text) {
  if (!is.character(text) || length(text) != 1L ||
        !nzchar(trimws(text)) || !grepl("[[:alpha:]]", text) ||
        startsWith(text, "Internal error:")) {
    return(ast_unknown_value())
  }
  list(raw = text, translated = FALSE)
}

ast_merge_values <- function(values) {
  raw <- unique(unlist(lapply(values, function(value) value[["raw"]]),
                       use.names = FALSE))
  list(raw = raw, translated = length(raw) == 0L)
}

ast_message_value <- function(node, bindings) {
  if (is.symbol(node)) {
    value <- bindings[[as.character(node)]]
    return(if (is.null(value)) ast_unknown_value() else value)
  }
  if (is.character(node) && length(node) == 1L) {
    return(ast_raw_value(node))
  }
  if (!is.call(node)) return(ast_unknown_value())

  name <- ast_call_name(node)
  if (name %in% c(".bb_tr", ".bb_trf", ".meta_tr", ".meta_trf")) {
    return(ast_safe_value())
  }
  if (name == "(" && length(node) >= 2L) {
    return(ast_message_value(node[[2L]], bindings))
  }
  if (name == "sprintf") {
    tags <- names(node)
    if (is.null(tags)) tags <- rep("", length(node))
    format_position <- which(tags == "fmt")
    format_position <- if (length(format_position) > 0L) {
      format_position[[1L]]
    } else {
      2L
    }
    if (length(node) < format_position) return(ast_unknown_value())
    return(ast_message_value(node[[format_position]], bindings))
  }
  if (name %in% c("paste", "paste0")) {
    literal <- ast_literal_text(node)
    if (!is.null(literal)) return(ast_raw_value(literal))
    tags <- names(node)
    if (is.null(tags)) tags <- rep("", length(node))
    positions <- if (length(node) > 1L) {
      seq.int(2L, length(node))
    } else {
      integer()
    }
    positions <- positions[!tags[positions] %in% c("sep", "collapse")]
    return(ast_merge_values(lapply(
      as.list(node)[positions], ast_message_value, bindings = bindings
    )))
  }
  ast_unknown_value()
}

ast_untranslated_messages <- function(expressions) {
  found <- character()
  is_missing_call_child <- function(child) {
    if (missing(child)) return(TRUE)
    is.symbol(child) && identical(as.character(child), "")
  }
  walk <- function(node, bindings = list()) {
    if (is.expression(node)) {
      children <- as.list(node)
      for (index in seq_along(children)) {
        child <- children[[index]]
        if (!is_missing_call_child(child)) bindings <- walk(child, bindings)
      }
      return(bindings)
    }
    if (!is.call(node)) return(bindings)
    name <- ast_call_name(node)

    if (name %in% c("<-", "=") && length(node) >= 3L &&
          is.symbol(node[[2L]])) {
      bindings[[as.character(node[[2L]])]] <-
        ast_message_value(node[[3L]], bindings)
      return(walk(node[[3L]], bindings))
    }
    if (name == "function" && length(node) >= 3L) {
      walk(node[[3L]], list())
      return(bindings)
    }

    if (name %in% c("stop", "warning", "message")) {
      arguments <- if (length(node) > 1L) as.list(node)[-1L] else list()
      tags <- names(node)
      if (is.null(tags)) tags <- rep("", length(node))
      tags <- if (length(node) > 1L) tags[-1L] else character()
      arguments <- arguments[!tags %in% c("call.", "immediate.", "appendLF")]
      values <- lapply(arguments, ast_message_value, bindings = bindings)
      found <<- c(
        found,
        unlist(lapply(values, function(value) value[["raw"]]), use.names = FALSE)
      )
    }

    children <- if (length(node) > 1L) as.list(node)[-1L] else list()
    for (index in seq_along(children)) {
      child <- children[[index]]
      if (!is_missing_call_child(child)) bindings <- walk(child, bindings)
    }
    bindings
  }
  bindings <- list()
  for (expression in expressions) {
    bindings <- walk(expression, bindings)
  }
  unique(found)
}

ast_project_expressions <- function(project) {
  files <- list.files(file.path(project, "R"), pattern = "\\.R$",
                      recursive = TRUE, full.names = TRUE)
  lapply(files, parse, keep.source = FALSE)
}

meta_untranslated_sentences <- function(project) {
  ast_untranslated_messages(ast_project_expressions(project))
}

ast_catalog_literals <- function(project, helpers) {
  literals <- character()
  is_missing_call_child <- function(child) {
    if (missing(child)) return(TRUE)
    is.symbol(child) && identical(as.character(child), "")
  }
  visit <- function(node) {
    if (is.expression(node)) {
      children <- as.list(node)
      for (index in seq_along(children)) {
        child <- children[[index]]
        if (!is_missing_call_child(child)) visit(child)
      }
      return(invisible(NULL))
    }
    if (!is.call(node)) return(invisible(NULL))
    if (ast_call_name(node) %in% helpers && length(node) >= 2L) {
      literal <- ast_literal_text(node[[2L]])
      if (!is.null(literal)) literals <<- c(literals, literal)
    }
    children <- if (length(node) > 1L) as.list(node)[-1L] else list()
    for (index in seq_along(children)) {
      child <- children[[index]]
      if (!is_missing_call_child(child)) visit(child)
    }
    invisible(NULL)
  }
  for (expression in ast_project_expressions(project)) visit(expression)
  unique(literals)
}

meta_translation_literals <- function(project) {
  ast_catalog_literals(project, c(".meta_tr", ".meta_trf"))
}

test_that("user-facing R diagnostics use the translation helpers", {
  source_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
  found <- meta_untranslated_sentences(source_root)
  expect_true(length(found) == 0L, info = paste(found, collapse = "\n"))
})

test_that("every .bb_tr literal in R is present in the bigbang catalog", {
  source_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
  literals <- ast_catalog_literals(
    source_root, c(".bb_tr", ".bb_trf")
  )
  missing <- setdiff(literals, names(.bigbang_spanish_catalog()))
  expect_true(length(missing) == 0L, info = paste(missing, collapse = "\n"))
})

test_that("the AST diagnostic guard covers message data flow without false positives", {
  bad <- parse(text = c(
    "stop('Failed')",
    "base::warning(sprintf(",
    "  fmt = '%s is missing', value",
    "))",
    "format <- '(failed: %s)'",
    "base::message(sprintf(fmt = format, value))",
    "quoted <- sprintf(\"'%s' must be TRUE\", value)",
    "stop(quoted)",
    "plain <- sprintf('plain words %d', count)",
    "message(plain)",
    "message(paste0('Missing ', value))",
    "base::stop('FailedAgain')",
    "base::message(paste('A', 'failure', sep = ' '))",
    "message(paste0('Another', ' failure'))"
  ), keep.source = FALSE)
  found <- ast_untranslated_messages(list(bad))
  expect_true(all(c(
    "Failed", "%s is missing", "(failed: %s)", "'%s' must be TRUE",
    "plain words %d", "Missing ", "FailedAgain", "A failure",
    "Another failure"
  ) %in% found))

  good <- parse(text = c(
    "internal <- sprintf('internal %s', value)",
    "invisible(internal)",
    "stop(.bb_tr('Already translated'))",
    "warning(base::message(.meta_trf('Already %s', value)))",
    "message(paste0(.bb_tr('Translated'), ' ', value))"
  ), keep.source = FALSE)
  expect_length(ast_untranslated_messages(list(good)), 0L)
})

test_that("the bigbang runtime catalog translates messages to Spanish", {
  bind_dir <- bigbang_catalog_dir()
  output <- translate_from_catalog(
    "R-bigbang", bind_dir, "The archive directory does not exist: %s"
  )
  expect_identical(
    tail(output, 1L),
    "El directorio de archivos no existe: %s"
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
    "Cannot resolve re-export collision(s): %s. Candidates, diagnoses, and ",
    "proof details: %s. %s."
  )
  collision_output <- translate_from_catalog(
    "R-bigbang", bind_dir, collision_message
  )
  expect_identical(
    tail(collision_output, 1L),
    paste0(
      "No se pueden resolver las colisiones de reexportaci\u00f3n: %s. ",
      "Candidatos, diagn\u00f3sticos y detalles de la prueba: %s. %s."
    )
  )
})

test_that("bigbang print methods translate in a Spanish child without changing English", {
  result <- structure(
    list(
      name = "demo", path = "/tmp/demo", packages = c("a", "b"),
      dry_run = TRUE, updated = TRUE, recovered = TRUE,
      added_files = "new.R", removed_files = "old.R",
      omitted = data.frame(component = "skip", stringsAsFactors = FALSE),
      reexports = data.frame(
        symbol = "f", package = "a", resolution = "chosen",
        stringsAsFactors = FALSE
      ),
      reexport_excluded = "g", cran_dependencies = "h"
    ),
    class = "bigbang_result"
  )
  normal <- result
  normal$dry_run <- FALSE
  normal$updated <- FALSE
  normal$recovered <- FALSE
  install <- structure(
    list(installed = list(a = TRUE), unchanged = list(b = TRUE),
         failed = list(), skipped = list()),
    class = "bigbang_install_result"
  )
  artifact <- structure(
    list(path = "/tmp/artifact", type = "source", vulnerable = FALSE,
         signatures = "one"),
    class = "bigbang_artifact_scan"
  )
  vulnerable <- artifact
  vulnerable$vulnerable <- TRUE
  withr::local_envvar(LANGUAGE = "C")
  expect_identical(
    capture.output(print(artifact)),
    c(
      "<bigbang artifact scan>", "  Path: /tmp/artifact",
      "  Type: source", "  Result: no deletion signatures found",
      "  Signatures: one"
    )
  )
  expect_identical(
    capture.output(print(result)),
    c(
      "<bigbang metapackage>", "  Package: demo", "  Path: /tmp/demo",
      "  Components: a, b", "  Mode: dry run (no files written)",
      "  Mode: updated from generation manifest", "  Recovered: interrupted update",
      "  Would add: new.R", "  Would remove: old.R", "  Omitted: skip",
      "  Re-exports: f -> a (chosen)", "  Re-exports excluded: g",
      "  Non-local dependencies: h"
    )
  )
  expect_identical(
    capture.output(print(normal)),
    c(
      "<bigbang metapackage>", "  Package: demo", "  Path: /tmp/demo",
      "  Components: a, b", "  Added: new.R", "  Removed: old.R",
      "  Omitted: skip", "  Re-exports: f -> a (chosen)",
      "  Re-exports excluded: g", "  Non-local dependencies: h"
    )
  )
  expect_identical(
    capture.output(print(install)),
    c("<bigbang local installation>", "  Installed: 1", "  Unchanged: 1",
      "  Failed: 0", "  Skipped: 0")
  )

  code <- paste0(
    bigbang_child_load_code(),
    "invisible(bindtextdomain('R-bigbang', ",
    deparse(bigbang_catalog_dir()), ")); ",
    "result <- structure(list(name='demo', path='/tmp/demo', packages=c('a','b'), ",
    "dry_run=TRUE, updated=TRUE, recovered=TRUE, added_files='new.R', ",
    "removed_files='old.R', omitted=data.frame(component='skip'), ",
    "reexports=data.frame(symbol='f', package='a', resolution='chosen'), ",
    "reexport_excluded='g', cran_dependencies='h'), class='bigbang_result'); ",
    "normal <- result; normal$dry_run <- FALSE; normal$updated <- FALSE; ",
    "normal$recovered <- FALSE; ",
    "install <- structure(list(installed=list(a=TRUE), unchanged=list(b=TRUE), ",
    "failed=list(), skipped=list()), class='bigbang_install_result'); ",
    "artifact <- structure(list(path='/tmp/artifact', type='source', ",
    "vulnerable=FALSE, signatures='one'), class='bigbang_artifact_scan'); ",
    "vulnerable <- artifact; vulnerable$vulnerable <- TRUE; ",
    "artifact_print <- get('print.bigbang_artifact_scan', envir=asNamespace('bigbang')); ",
    "result_print <- get('print.bigbang_result', envir=asNamespace('bigbang')); ",
    "install_print <- get('print.bigbang_install_result', envir=asNamespace('bigbang')); ",
    "writeLines(c(capture.output(artifact_print(artifact)), ",
    "capture.output(artifact_print(vulnerable)), ",
    "capture.output(result_print(result)), ",
    "capture.output(result_print(normal)), ",
    "capture.output(install_print(install))))"
  )
  output <- run_spanish_child("R-bigbang", bigbang_catalog_dir(), code)
  translated <- paste(output, collapse = "\n")
  expect_match(translated, "<escaneo de artefacto bigbang>", fixed = TRUE)
  expect_match(translated, "  Ruta: /tmp/artifact", fixed = TRUE)
  expect_match(translated, "  Resultado: no se encontraron firmas de eliminaci\u00f3n", fixed = TRUE)
  expect_match(translated, "  Resultado: VULNERABLE", fixed = TRUE)
  expect_match(translated, "<metapaquete bigbang>", fixed = TRUE)
  expect_match(translated, "  Paquete: demo", fixed = TRUE)
  expect_match(translated, "  Componentes: a, b", fixed = TRUE)
  expect_match(translated, "  Modo: simulaci\u00f3n (no se escribieron archivos)", fixed = TRUE)
  expect_match(translated, "  Recuperado: actualizaci\u00f3n interrumpida", fixed = TRUE)
  expect_match(translated, "  Se agregar\u00edan: new.R", fixed = TRUE)
  expect_match(translated, "  Se agregaron: new.R", fixed = TRUE)
  expect_match(translated, "  Se quitaron: old.R", fixed = TRUE)
  expect_match(translated, "  Reexportaciones: f -> a (chosen)", fixed = TRUE)
  expect_match(translated, "  Reexportaciones excluidas: g", fixed = TRUE)
  expect_match(translated, "<instalaci\u00f3n local bigbang>", fixed = TRUE)
  expect_match(translated, "  Instalados: 1", fixed = TRUE)
  expect_match(translated, "  Sin cambios: 1", fixed = TRUE)
  expect_match(translated, "  Fallidos: 0", fixed = TRUE)
  expect_match(translated, "  Omitidos: 0", fixed = TRUE)
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
  output <- translate_from_catalog(
    "R-i18nverse", file.path(result$path, "inst", "po"),
    "Components available"
  )
  expect_identical(tail(output, 1L), "Componentes disponibles")
})

test_that("every generated .meta_tr literal is in its generated catalog", {
  skip_if_not(exists("create_metapackage", mode = "function"))
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(fixture)) {
    fixture <- testthat::test_path(
      "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
    )
  }
  cases <- expand.grid(
    reexport = c(FALSE, TRUE), include_archives = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  missing <- vector("list", nrow(cases))
  for (index in seq_len(nrow(cases))) {
    root <- tempfile("bigbang-i18n-generated-catalog-")
    destination <- file.path(root, "output")
    dir.create(destination, recursive = TRUE)
    name <- paste0("catalogguard", index, "verse")
    result <- create_metapackage(
      name, "toycomponent_0.1.0", dirname(fixture), dest_dir = destination,
      document = FALSE, verbose = FALSE, reexport = cases$reexport[[index]],
      include_archives = cases$include_archives[[index]],
      import_deps = character(), force_deps = character()
    )
    generated <- meta_translation_literals(result$path)
    catalog <- names(.metapackage_spanish_catalog(
      name, include_archives = cases$include_archives[[index]]
    ))
    missing[[index]] <- c(
      setdiff(generated, catalog),
      meta_untranslated_sentences(result$path)
    )
  }
  expect_true(
    all(lengths(missing) == 0L),
    info = paste(
      vapply(seq_along(missing), function(index) {
        paste0(
          "reexport=", cases$reexport[[index]],
          ", include_archives=", cases$include_archives[[index]], ": ",
          paste(missing[[index]], collapse = " | ")
        )
      }, character(1L)),
      collapse = "\n"
    )
  )
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
    "Cannot resolve re-export collision(s): %s. Candidates, diagnoses, and proof details: %s. %s.",
    "Generated metapackage symbol(s) %s can only be resolved with reexport_exclude: %s."
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
  expect_identical(
    unname(local_catalog[[
      "For distinct_definitions and undetermined collisions, decide among these candidates: %s"
    ]]),
    "Para las colisiones distinct_definitions y undetermined, elija entre estos candidatos: %s"
  )
  expect_false("'pkg_dir' must be one character string" %in% names(local_catalog))
  expect_false(
    "Cannot update %s because requested generated files are not in its manifest: %s." %in%
      names(local_catalog)
  )
})

test_that("the source Spanish catalog and compiled MO catalog have the same keys", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                        winslash = "/", mustWork = TRUE)
  read_po <- function(path) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    parse_field <- function(line, field) {
      eval(parse(text = sub(paste0("^", field, " "), "", line)))
    }
    ids <- vapply(grep("^msgid ", lines, value = TRUE), parse_field,
                  character(1L), field = "msgid")
    values <- vapply(grep("^msgstr ", lines, value = TRUE), parse_field,
                     character(1L), field = "msgstr")
    stats::setNames(values[ids != ""], ids[ids != ""])
  }
  read_mo <- function(path) {
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    word <- function(offset) {
      sum(as.integer(bytes[(offset + 1L):(offset + 4L)]) * 256^(0:3))
    }
    text <- function(offset, length) {
      if (length == 0L) return("")
      rawToChar(bytes[(offset + 1L):(offset + length)])
    }
    count <- word(8L)
    originals <- word(12L)
    translations <- word(16L)
    ids <- values <- character(count)
    for (index in seq_len(count)) {
      original_entry <- originals + (index - 1L) * 8L
      translation_entry <- translations + (index - 1L) * 8L
      id_length <- word(original_entry)
      id_offset <- word(original_entry + 4L)
      value_length <- word(translation_entry)
      value_offset <- word(translation_entry + 4L)
      ids[[index]] <- text(id_offset, id_length)
      values[[index]] <- text(value_offset, value_length)
    }
    stats::setNames(values[ids != ""], ids[ids != ""])
  }
  pot_path <- file.path(root, "po", "R-bigbang.pot")
  po_path <- file.path(root, "po", "R-es.po")
  mo_path <- file.path(root, "inst", "po", "es", "LC_MESSAGES",
                       "R-bigbang.mo")
  if (!file.exists(mo_path)) {
    mo_path <- system.file(
      "po", "es", "LC_MESSAGES", "R-bigbang.mo", package = "bigbang"
    )
  }
  mo <- read_mo(mo_path)
  if (file.exists(pot_path) && file.exists(po_path)) {
    pot <- read_po(pot_path)
    po <- read_po(po_path)
    expect_setequal(names(pot), names(po))
    expect_setequal(names(pot), names(mo))
    ids <- sort(names(po))
    expect_identical(po[ids], mo[ids])
    expect_true(all(nzchar(unname(po))))
  } else {
    expect_setequal(names(.bigbang_spanish_catalog()), names(mo))
  }
  expect_true(all(nzchar(unname(mo))))
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
  skip_on_cran()
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
  user_like_library <- file.path(sandbox, "user-like-library")
  dir.create(destination, recursive = TRUE)
  dir.create(library)
  dir.create(user_like_library)
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  install_status <- system2(
    file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"),
    c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(user_like_library),
      shQuote(fixture)),
    stdout = FALSE, stderr = FALSE
  )
  expect_identical(install_status, 0L)
  result <- create_metapackage(
    "spanishverse", "toycomponent_0.1.0", dirname(fixture),
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
  code <- paste0(
    ".libPaths(c(", deparse(library), ", .Library)); ",
    "stopifnot(!requireNamespace('toycomponent', quietly = TRUE)); ",
    "utils::install.packages(", deparse(result$path),
    ", repos = NULL, type = 'source', lib = ", deparse(library), "); ",
    "invisible(bindtextdomain('R-spanishverse', file.path(",
    deparse(library), ", 'spanishverse', 'po'))); ",
    "library(spanishverse, lib.loc = ", deparse(library), ")"
  )
  output <- withr::with_envvar(
    c(R_LIBS_USER = user_like_library),
    run_spanish_child("R-bigbang", bigbang_catalog_dir(), code)
  )
  translated <- paste(output, collapse = "\n")
  expect_match(translated, "Faltan componentes por instalar", fixed = TRUE)
})

test_that("nested check placeholder detection handles both library variables", {
  sandbox <- tempfile("bigbang-nested-check-")
  user_library <- file.path(sandbox, "user [library]*?")
  package_dir <- file.path(user_library, "dummy[package]*?")
  dir.create(package_dir, recursive = TRUE)
  file.create(file.path(package_dir, "dummy_for_check"))

  marker <- "_R_CHECK_RD_XREFS_"
  before <- Sys.getenv(marker, unset = NA_character_)
  before_r_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  before_r_libs_user <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  cleanup_env <- new.env(parent = emptyenv())
  before_cleanup_env <- getOption("bigbang.nested_check_cleanup_env")
  options(bigbang.nested_check_cleanup_env = cleanup_env)
  on.exit({
    restore <- function(name, value) {
      if (is.na(value)) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(list(value), name))
      }
    }
    restore(marker, before)
    restore("R_LIBS", before_r_libs)
    restore("R_LIBS_USER", before_r_libs_user)
    withr::deferred_run(cleanup_env)
    if (is.null(before_cleanup_env)) {
      options(bigbang.nested_check_cleanup_env = NULL)
    } else {
      options(bigbang.nested_check_cleanup_env = before_cleanup_env)
    }
  }, add = TRUE)
  on.exit(
    expect_identical(Sys.getenv(marker, unset = NA_character_), before),
    add = TRUE
  )
  Sys.setenv(
    R_LIBS = paste(
      c("", file.path(sandbox, "missing*"), ""),
      collapse = .Platform$path.sep
    ),
    R_LIBS_USER = user_library
  )
  sys.source(testthat::test_path("setup-nested-check.R"), envir = environment())
  expect_identical(Sys.getenv(marker), "false")
})
