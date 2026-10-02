round051_export_directive <- function(symbol) {
  paste0("export(", bigbang:::.r_symbol_literal(symbol), ")")
}

round051_make_archive <- function(source_root, archive_dir, name,
                                  exports, body = character(),
                                  namespace_extra = character(),
                                  imports = character(), files = list(),
                                  sysdata = NULL, version = "0.1.0") {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  description <- c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: Round 051 fixture ", name),
    paste0("Version: ", version),
    paste0("Description: Temporary fixture for ", name, "."),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    "License: MIT"
  )
  if (length(imports) > 0L) {
    description <- c(description, paste0("Imports: ", paste(imports, collapse = ", ")))
  }
  writeLines(description, file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(c(
    vapply(exports, round051_export_directive, character(1L)),
    namespace_extra
  ), file.path(package_dir, "NAMESPACE"), useBytes = TRUE)
  if (length(body) > 0L) {
    writeLines(body, file.path(package_dir, "R", "fixture.R"), useBytes = TRUE)
  }
  if (length(files) > 0L) {
    for (relative in names(files)) {
      destination <- file.path(package_dir, relative)
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      writeLines(files[[relative]], destination, useBytes = TRUE)
    }
  }
  if (!is.null(sysdata)) {
    environment <- new.env(parent = emptyenv())
    for (symbol in names(sysdata)) assign(symbol, sysdata[[symbol]], environment)
    save(list = names(sysdata), file = file.path(package_dir, "R", "sysdata.rda"),
         envir = environment)
  }
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(source_root, utils::tar(archive, name, compression = "gzip"))
  archive
}

round051_create <- function(name, archives, destination, ...) {
  bigbang::create_metapackage(
    name, archives, dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), ...
  )
}

round051_install <- function(archive, library) {
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  status <- system2(
    r_binary,
    c("CMD", "INSTALL", "-l", shQuote(library), shQuote(archive)),
    stdout = FALSE, stderr = FALSE
  )
  expect_identical(status, 0L, info = archive)
}

round051_collision_condition <- function(expr) {
  tryCatch(expr, error = identity)
}

test_that("round 051 requires an explicit choice for every collision", {
  sandbox <- tempfile("bigbang-round051-origins-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  origin_a <- round051_make_archive(
    source_root, archive_dir, "originra", "shared",
    body = "shared <- function() 'origin'"
  )
  origin_b <- round051_make_archive(
    source_root, archive_dir, "originrb", "shared",
    body = "# imported only",
    namespace_extra = "importFrom(originra, shared)", imports = "originra"
  )
  origin_c <- round051_make_archive(
    source_root, archive_dir, "originrc", "shared",
    body = "# imported only",
    namespace_extra = "importFrom(originrb, shared)", imports = "originrb"
  )
  origin_full <- round051_make_archive(
    source_root, archive_dir, "originrf", "shared",
    body = "# imported only", namespace_extra = "import(originra)",
    imports = "originra"
  )
  collision <- round051_collision_condition(round051_create(
    "originverse", c(origin_c, origin_full, origin_a, origin_b), destination,
    reexport = TRUE
  ))
  expect_s3_class(collision, "bigbang_error_reexport_collision")
  expect_match(collision$message, "reexport_prefer = c\\(shared = \\\"originra\\\"\\)", perl = TRUE)

  result <- round051_create(
    "originverse", c(origin_c, origin_full, origin_a, origin_b), destination,
    reexport = TRUE, reexport_prefer = c(shared = "originra")
  )
  expect_identical(result$reexports$symbol, "shared")
  expect_identical(result$reexports$package, "originra")
  expect_identical(result$reexports$resolution, "preferred")
  expect_identical(result$reexports$diagnosis, "probable_same_object")
  expect_identical(result$reexport_excluded, character())
  expect_identical(result$reexports$package, "originra")

  testthat::skip_on_cran()
  library <- file.path(sandbox, "library")
  dir.create(library)
  round051_install(origin_a, library)
  round051_install(origin_b, library)
  round051_install(origin_c, library)
  round051_install(origin_full, library)
  withr::local_libpaths(c(library, .libPaths()))
  expect_true(identical(
    getExportedValue("originra", "shared"),
    getExportedValue("originrb", "shared")
  ))
  expect_true(identical(
    getExportedValue("originra", "shared"),
    getExportedValue("originrc", "shared")
  ))
  expect_true(identical(
    getExportedValue("originra", "shared"),
    getExportedValue("originrf", "shared")
  ))
})

test_that("re-export verification does not occupy a conflict symbol slot", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round062-conflict-slot-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  component_library <- file.path(sandbox, "component-library")
  meta_library <- file.path(sandbox, "meta-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(component_library)
  dir.create(meta_library)
  left <- round051_make_archive(
    source_root, archive_dir, "h6left", "reexport_verification",
    body = "reexport_verification <- function() 'left'"
  )
  right <- round051_make_archive(
    source_root, archive_dir, "h6right", "reexport_verification",
    body = "reexport_verification <- function() 'right'"
  )
  generated <- round051_create(
    "h6verse", c(left, right), destination, reexport = TRUE,
    reexport_prefer = c(reexport_verification = "h6left")
  )
  round051_install(left, component_library)
  round051_install(right, component_library)
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L,
                   info = paste(build_output, collapse = "\n"))
  meta_archive <- file.path(sandbox, "h6verse_0.1.0.tar.gz")
  round051_install(meta_archive, meta_library)
  withr::with_libpaths(c(meta_library, component_library), {
    loadNamespace("h6verse")
    getExportedValue("h6verse", "h6verse_attach")()
    on.exit(getExportedValue("h6verse", "h6verse_detach")(), add = TRUE)
    conflicts <- suppressWarnings(
      getExportedValue("h6verse", "h6verse_conflicts")()
    )
    expect_named(conflicts, "reexport_verification")
    expect_setequal(
      conflicts[["reexport_verification"]],
      c("package:h6left", "package:h6right")
    )
    verification <- getExportedValue(
      "h6verse", "h6verse_reexport_verification"
    )(conflicts)
    expect_s3_class(verification, "h6verse_reexport_verification")
    expect_identical(verification$symbol, "reexport_verification")
  })
})

test_that("round 051 reports genuine collisions and every selected proof blocker", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-round051-blockers-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)

  expect_collision <- function(stem, body = character(), namespace_extra = character(),
                               files = list(), sysdata = NULL, imports = character(),
                               reason) {
    root <- round051_make_archive(
      source_root, archive_dir, paste0(stem, "a"), "s",
      body = "s <- function() 'a'"
    )
    child <- round051_make_archive(
      source_root, archive_dir, paste0(stem, "b"), "s", body = body,
      namespace_extra = c(
        paste0("importFrom(", stem, "a, s)"), namespace_extra
      ), imports = unique(c(paste0(stem, "a"), imports)), files = files,
      sysdata = sysdata
    )
    condition <- round051_collision_condition(round051_create(
      paste0(stem, "verse"), c(root, child), destination, reexport = TRUE
    ))
    expect_s3_class(condition, "bigbang_error_reexport_collision")
    expect_true(is.data.frame(condition$data))
    expect_identical(condition$data$symbol, "s")
    expect_match(condition$data$reason, reason, perl = TRUE)
    condition
  }

  local <- expect_collision("local", body = "s <- function() 'b'", reason = "Local definition")
  expect_match(local$message, "locala.*localb", perl = TRUE)
  expect_collision("onload", body = c(
    ".onLoad <- function(lib, pkg) assign('s', function() 'b', envir = asNamespace(pkg))"
  ), reason = "Binder or namespace mutation via assign")
  expect_collision(
    "sysdat", body = "# imported only", sysdata = list(s = function() "b"),
    reason = "sysdata.rda contains"
  )
  expect_collision(
    "generic", body = "setGeneric('s')", reason = "Binder or namespace mutation via setGeneric"
  )
  expect_collision(
    "dynamic", body = "list2env(list(s = function() 'b'), envir = .GlobalEnv)",
    reason = "Binder or namespace mutation via list2env"
  )
  expect_collision(
    "evaldyn", body = "eval(parse(text = 's <- 2'))", reason = "Binder or namespace mutation via eval"
  )
  expect_collision(
    "rlangdyn", body = "rlang::env_bind(.GlobalEnv, s = function() 'b')",
    reason = "Binder or namespace mutation via env_bind"
  )
  expect_collision(
    "utilsdyn", body = "utils::assignInMyNamespace('s', function() 'b')",
    reason = "Binder or namespace mutation via assignInMyNamespace"
  )
  expect_collision(
    "badparse", body = "# imported only",
    files = list("R/broken.R" = "s <- function("), reason = "Could not parse R file"
  )
  expect_collision(
    "dupe", body = "# imported only",
    namespace_extra = c("importFrom(dupea, s)", "importFrom(dupea, s)"),
    reason = "root component dupea"
  )

  literal_calls <- c(
    assign = "assign('s', 1)", delayedAssign = "delayedAssign('s', 1)",
    makeActiveBinding = "makeActiveBinding('s', function() 1, .GlobalEnv)",
    setGeneric = "setGeneric('s')", setClass = "setClass('s')",
    setRefClass = "setRefClass('s')", setValidity = "setValidity('s', NULL)",
    setGroupGeneric = "setGroupGeneric('s', 's')", setMethod = "setMethod('s', 'x', function(x) x)"
  )
  for (call_name in names(literal_calls)) {
    expect_collision(
      paste0("call", call_name), body = unname(literal_calls[[call_name]]),
      reason = paste0("Binder or namespace mutation via ", call_name)
    )
  }
  expect_collision(
    "nonliteral", body = "setClass(x)", reason = "Binder or namespace mutation via setClass"
  )

  root_one <- round051_make_archive(
    source_root, archive_dir, "distincta", "s", body = "s <- function() 1L"
  )
  root_two <- round051_make_archive(
    source_root, archive_dir, "distinctb", "s", body = "s <- function() 2L"
  )
  distinct <- round051_collision_condition(round051_create(
    "distinctverse", c(root_one, root_two), destination, reexport = TRUE
  ))
  expect_s3_class(distinct, "bigbang_error_reexport_collision")
  expect_match(distinct$data$reason, "root component distinct", perl = TRUE)

  external_root <- round051_make_archive(
    source_root, archive_dir, "externalroot", "head", body = "head <- function(x) x"
  )
  external_import <- round051_make_archive(
    source_root, archive_dir, "externalimport", "head", body = "# imported only",
    namespace_extra = "import(utils)", imports = "utils"
  )
  external <- round051_collision_condition(round051_create(
    "externalverse", c(external_root, external_import), destination, reexport = TRUE
  ))
  expect_s3_class(external, "bigbang_error_reexport_collision")
  expect_match(external$data$reason, "imports complete 'utils', which could provide 'head'", fixed = TRUE)
})

test_that("round 051 validates options, names, exclusions, preference, and order", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round051-options-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  archive <- round051_make_archive(
    source_root, archive_dir, "optiona", c("s", "other"),
    body = c("s <- function() 1L", "other <- function() 2L")
  )
  expect_error(
    round051_create("falseverse", archive, destination,
                    reexport_prefer = c(s = "optiona")),
    class = "bigbang_error_reexport_options"
  )
  expect_error(
    round051_create("unnamedverse", archive, destination,
                    reexport = TRUE, reexport_prefer = "optiona"),
    class = "bigbang_error_reexport_prefer"
  )
  expect_error(
    round051_create("emptyverse", archive, destination,
                    reexport = TRUE,
                    reexport_prefer = stats::setNames("optiona", "")),
    class = "bigbang_error_reexport_prefer"
  )
  expect_error(
    round051_create("dupenameverse", archive, destination,
                    reexport = TRUE, reexport_prefer = c(s = "optiona", s = "optiona")),
    class = "bigbang_error_reexport_prefer"
  )
  expect_error(
    round051_create("badvalueverse", archive, destination,
                    reexport = TRUE, reexport_prefer = structure(c("optiona", "optiona"), names = "s")),
    class = "bigbang_error_reexport_prefer"
  )
  expect_error(
    round051_create("unknownverse", archive, destination,
                    reexport = TRUE, reexport_exclude = "typo"),
    class = "bigbang_error_reexport_unknown"
  )
  expect_error(
    round051_create("badtargetverse", archive, destination,
                    reexport = TRUE, reexport_prefer = c(s = "missing")),
    class = "bigbang_error_reexport_prefer_component"
  )
  expect_error(
    round051_create("overlapverse", archive, destination,
                    reexport = TRUE, reexport_prefer = c(s = "optiona"),
                    reexport_exclude = "s"),
    class = "bigbang_error_reexport_overlap"
  )
  expect_error(
    round051_create("badexcludeverse", archive, destination,
                    reexport = TRUE, reexport_exclude = ""),
    class = "bigbang_error_reexport_exclude"
  )

  left <- round051_make_archive(
    source_root, archive_dir, "orderleft", "shared", body = "shared <- function() 'left'"
  )
  right <- round051_make_archive(
    source_root, archive_dir, "orderright", "shared", body = "shared <- function() 'right'"
  )
  first <- round051_create(
    "ordervers", c(left, right), destination, reexport = TRUE,
    reexport_prefer = c(shared = "orderleft")
  )
  second <- round051_create(
    "ordervers2", c(right, left), destination, reexport = TRUE,
    reexport_prefer = c(shared = "orderleft")
  )
  expect_identical(first$reexports, second$reexports)
  expect_identical(first$reexports$package, "orderleft")

  excluded <- round051_create(
    "excludeverse", c(left, right), destination, reexport = TRUE,
    reexport_exclude = "shared"
  )
  expect_length(excluded$reexports$symbol, 0L)
  expect_identical(excluded$reexport_excluded, "shared")
})

test_that("round 051 dry runs and updates re-export plans without stale files", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round051-update-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  left <- round051_make_archive(
    source_root, archive_dir, "updateleft", "shared", body = "shared <- function() 'left'"
  )
  right <- round051_make_archive(
    source_root, archive_dir, "updateright", "shared", body = "shared <- function() 'right'"
  )
  dry_destination <- file.path(sandbox, "dry")
  dry <- round051_create(
    "dryverse", c(left, right), dry_destination, reexport = TRUE,
    reexport_prefer = c(shared = "updateleft"), dry_run = TRUE
  )
  expect_true(isTRUE(dry$dry_run))
  expect_false(dir.exists(dry_destination))
  expect_match(paste(capture.output(print(dry)), collapse = "\n"), "Re-exports", fixed = TRUE)

  initial <- round051_create(
    "updateverse", c(left, right), destination, reexport = TRUE,
    reexport_prefer = c(shared = "updateleft")
  )
  updated <- bigbang::create_metapackage(
    "updateverse", c(left, right), dest_dir = destination, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE, reexport_prefer = c(shared = "updateright"), update = TRUE
  )
  expect_true(isTRUE(updated$updated))
  expect_identical(updated$reexports$package, "updateright")
  expect_match(
    paste(readLines(file.path(updated$path, "R", "reexports.R")), collapse = "\n"),
    "updateright", fixed = TRUE
  )
  expect_true(any(grepl("export\\(shared\\)", readLines(
    file.path(updated$path, "NAMESPACE"), warn = FALSE
  ))))
  expect_true(file.exists(file.path(updated$path, "man", "reexports.Rd")))
  excluded <- bigbang::create_metapackage(
    "updateverse", c(left, right), dest_dir = destination, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE, reexport_exclude = "shared", update = TRUE
  )
  expect_false(any(grepl("export\\(shared\\)", readLines(
    file.path(excluded$path, "NAMESPACE"), warn = FALSE
  ))))
  expect_false(file.exists(file.path(excluded$path, "man", "reexports.Rd")))
  expect_true(isTRUE(initial$reexports$package == "updateleft"))
})

test_that("round 051 uses an explicit hard rule for skipped re-export owners", {
  sandbox <- tempfile("bigbang-round051-skip-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  origin <- round051_make_archive(
    source_root, archive_dir, "skiporigin", "s", body = "s <- function() 1L"
  )
  child <- round051_make_archive(
    source_root, archive_dir, "skipchild", "s", body = "# imported only",
    namespace_extra = "importFrom(skiporigin, s)", imports = "skiporigin"
  )
  bytes <- readBin(origin, "raw", n = file.info(origin)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], origin)
  expect_warning(
    condition <- round051_collision_condition(bigbang::create_metapackage(
      "skipverse", c(origin, child), dest_dir = destination, document = FALSE,
      verbose = FALSE, import_deps = character(), force_deps = character(),
      reexport = TRUE, on_component_error = "skip"
    )),
    "skiporigin"
  )
  expect_s3_class(condition, "bigbang_error_reexport_skipped")
  expect_match(condition$message, "skiporigin", fixed = TRUE)
  expect_match(condition$message, "omitted|skip", ignore.case = TRUE)
  expect_false(dir.exists(file.path(destination, "skipverse")))

  child_prefer <- round051_make_archive(
    source_root, archive_dir, "skipchildprefer", "s", body = "# imported only",
    namespace_extra = "importFrom(skiporigin, s)"
  )
  expect_warning(
    expect_warning(
      preferred_condition <- round051_collision_condition(bigbang::create_metapackage(
        "skippreferverse", c(origin, child_prefer), dest_dir = destination,
        document = FALSE, verbose = FALSE, import_deps = character(),
        force_deps = character(), reexport = TRUE,
        reexport_prefer = c(s = "skiporigin"), on_component_error = "skip"
      )),
      "skiporigin"
    ),
    "Could not read archive"
  )
  expect_s3_class(preferred_condition, "bigbang_error_reexport_skipped")
  expect_match(preferred_condition$message, "omitted|skip", ignore.case = TRUE)
})

test_that("round 051 exercises parser and proof guard edge cases", {
  expect_identical(bigbang:::.reexport_parse_target("s"), "s")
  expect_identical(bigbang:::.reexport_parse_target("'s'"), "s")
  expect_null(bigbang:::.reexport_parse_target("s + 1"))

  root <- tempfile("bigbang-round051-evidence-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines("not an rda file", file.path(root, "R", "sysdata.rda"))
  evidence <- suppressWarnings(bigbang:::.reexport_source_evidence(root))
  expect_true(is.character(evidence$sysdata_error))

  cycle_a <- list(
    package = "cyclea", exports = "s", imports = list(list("cycleb", "s")),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  cycle_b <- list(
    package = "cycleb", exports = "s", imports = list(list("cyclea", "s")),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  cycle <- bigbang:::.reexport_probe(
    cycle_a, "s", list(cycle_a, cycle_b), character()
  )
  expect_false(cycle$demonstrated)
  expect_match(cycle$reason, "cycle", ignore.case = TRUE)
})

test_that("round 053 covers the remaining evidence and proof branches", {
  root <- tempfile("bigbang-round053-evidence-branches-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c(
    "x@s <- 1", "x[[name]] <- 1", "evalq(str2expression('s <- 1'))",
    "lapply('s', delayedAssign, value = 1)",
    "do.call('assign', list('s', 1))", "get('assign')('s', 1)",
    ".onAttach <- function(lib, pkg) otherpkg::mutate()"
  ), file.path(root, "R", "evidence.R"))
  writeLines("s <- 1", file.path(root, "R", "extra.s"))
  writeLines("s <- 1", file.path(root, "R", "extra.q"))
  evidence <- bigbang:::.reexport_source_evidence(root)
  expect_true(any(vapply(evidence$mutations, function(item) {
    identical(item$name, "@") && identical(item$symbol, "s")
  }, logical(1L))))
  expect_true(any(vapply(evidence$mutations, function(item) {
    identical(item$name, "[[") && !identical(item$symbol, "s")
  }, logical(1L))))
  expect_length(evidence$dynamic, 1L)
  expect_true(any(vapply(evidence$indirect, function(item) {
    identical(item$name, "otherpkg::mutate")
  }, logical(1L))))

  parse_evidence <- bigbang:::.reexport_empty_evidence()
  parse_evidence$parse_errors <- list(
    list(file = "R/broken.R", error = "bad")
  )
  no_import_parse <- list(
    package = "root053parse", exports = "s", imports = list(),
    reexport_evidence = parse_evidence
  )
  parse_probe <- bigbang:::.reexport_probe(
    no_import_parse, "s", list(no_import_parse), character()
  )
  expect_false(parse_probe$demonstrated)
  expect_match(parse_probe$reason, "Could not parse")

  sysdata_evidence <- bigbang:::.reexport_empty_evidence()
  sysdata_evidence$sysdata_names <- "s"
  no_import_sysdata <- list(
    package = "root053sysdata", exports = "s", imports = list(),
    reexport_evidence = sysdata_evidence
  )
  sysdata_probe <- bigbang:::.reexport_probe(
    no_import_sysdata, "s", list(no_import_sysdata), character()
  )
  expect_false(sysdata_probe$demonstrated)

  external <- list(
    package = "external053", exports = "s",
    imports = list(list("utils", "s")),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  external_probe <- bigbang:::.reexport_probe(
    external, "s", list(external), character()
  )
  expect_true(external_probe$demonstrated)
  expect_identical(external_probe$root_type, "external")

  parent <- list(
    package = "parent053", exports = character(), imports = list(),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  child <- list(
    package = "child053", exports = "s",
    imports = list(list("parent053", "s")),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  missing_export <- bigbang:::.reexport_probe(
    child, "s", list(child, parent), character()
  )
  expect_false(missing_export$demonstrated)
  expect_false(missing_export$skipped)
  missing_component <- child
  missing_component$imports <- list(list("absent053", "s"))
  absent <- bigbang:::.reexport_probe(
    missing_component, "s", list(missing_component),
    structure("absent053", names = "archive failed")
  )
  expect_true(absent$skipped)

  multi <- child
  multi$imports <- list(list("parent053", "s"), list("other053", "s"))
  multi_sources <- bigbang:::.reexport_probe(
    multi, "s", list(multi, parent), character()
  )
  expect_false(multi_sources$demonstrated)
})

test_that("round 053 labels every measured static-analysis escape as undetermined", {
  sandbox <- tempfile("bigbang-round053-vectors-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)

  cases <- list(
    dollar = list(body = "ns$s <- function() 'child'"),
    extract = list(body = "ns[[\"s\"]] <- function() 'child'"),
    do_call = list(body = "do.call(\"assign\", list(\"s\", function() 'child'))"),
    get_assign = list(body = "get(\"assign\")(\"s\", function() 'child')"),
    environment_extract = list(body = c(
      "fn <- function() NULL", "environment(fn)$s <- function() 'child'"
    )),
    delayed_object = list(body = c(
      "lapply(\"s\", delayedAssign, value = function() 'child',",
      "       assign.env = environment())"
    )),
    dynamic_eval = list(body = "eval(str2lang(\"s <- function() 'child'\"))"),
    source_connection = list(body = "source(textConnection(\"s <- 1\"))"),
    s_file = list(expected = "distinct_definitions", body = "# imported only", files = list(
      "R/extra.S" = "s <- function() 'child'"
    )),
    load_indirection = list(body = c(
      ".onLoad <- function(lib, pkg) helper053::sneaky(\"s\", asNamespace(pkg))"
    )),
    rcpp_native = list(body = "# imported only", namespace_extra = "useDynLib(helper053)")
  )

  for (label in names(cases)) {
    stem <- paste0("v053", gsub("_", "", label, fixed = TRUE))
    parent <- round051_make_archive(
      source_root, archive_dir, paste0(stem, "a"), "s",
      body = "s <- function() 'parent'"
    )
    spec <- cases[[label]]
    child <- round051_make_archive(
      source_root, archive_dir, paste0(stem, "b"), "s",
      body = spec$body,
      namespace_extra = c(
        paste0("importFrom(", stem, "a, s)"),
        spec$namespace_extra
      ),
      imports = paste0(stem, "a"), files = spec$files
    )
    condition <- round051_collision_condition(round051_create(
      paste0(stem, "verse"), c(parent, child), destination,
      reexport = TRUE
    ))
    expect_true(inherits(condition, "bigbang_error_reexport_collision"), info = label)
    expect_identical(condition$data$diagnosis,
                     if (is.null(spec$expected)) "undetermined" else spec$expected,
                     info = label)
  }
})

test_that("round 051 prefers a non-syntactic export and installs its binding", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-round051-install-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)
  left <- round051_make_archive(
    source_root, archive_dir, "preferleft", c("%>%", "with spaces"),
    body = c("`%>%` <- function() 'left pipe'", "`with spaces` <- function() 'left space'")
  )
  right <- round051_make_archive(
    source_root, archive_dir, "preferright", c("%>%", "with spaces"),
    body = c("`%>%` <- function() 'right pipe'", "`with spaces` <- function() 'right space'")
  )
  generated <- bigbang::create_metapackage(
    "preferverse", c(left, right), dest_dir = destination, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE, reexport_prefer = stats::setNames("preferright", "%>%"),
    reexport_exclude = "with spaces"
  )
  expect_identical(generated$reexports$package, "preferright")
  namespace <- readLines(file.path(generated$path, "NAMESPACE"), warn = FALSE)
  expect_true(any(grepl("%>%", namespace, fixed = TRUE)))
  expect_false(any(grepl("with spaces", namespace, fixed = TRUE)))
  expect_true(file.exists(file.path(generated$path, "man", "reexports.Rd")))

  round051_install(left, component_library)
  round051_install(right, component_library)
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))
  tarball <- file.path(sandbox, "preferverse_0.1.0.tar.gz")
  expect_true(file.exists(tarball))
  round051_install(tarball, meta_library)
  withr::local_libpaths(c(meta_library, component_library, .libPaths()))
  loadNamespace("preferverse")
  expect_identical(getExportedValue("preferverse", "%>%")(), "right pipe")
  conflicts <- NULL
  expect_warning(
    conflicts <- getExportedValue("preferverse", "preferverse_conflicts")(),
    "differ as distinct objects"
  )
  expect_s3_class(conflicts, "preferverse_conflicts")
  expect_s3_class(
    getExportedValue("preferverse", "preferverse_reexport_verification")(
      conflicts
    ),
    "preferverse_reexport_verification"
  )
  expect_identical(getExportedValue(
    "preferverse", "preferverse_reexport_verification"
  )(conflicts)$resolution, "preferred")
})

test_that("round 053 verifies probable choices after install and keeps the binding", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-round053-verify-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)

  helper <- round051_make_archive(
    source_root, archive_dir, "helper053", "sneaky",
    body = "sneaky <- function(name, value, envir) assign(name, value, envir = envir)"
  )
  parent <- round051_make_archive(
    source_root, archive_dir, "verifyparent", "s",
    body = "s <- function() 'parent'"
  )
  child <- round051_make_archive(
    source_root, archive_dir, "verifychild", "s",
    body = c(
      ".onLoad <- function(lib, pkg) {",
      "  helper <- base::getExportedValue('helper053', 'sneaky')",
      "  helper('s', function() 'child', base::asNamespace(pkg))",
      "}"
    ),
    namespace_extra = "importFrom(verifyparent, s)",
    imports = "verifyparent"
  )
  round051_install(helper, component_library)
  round051_install(parent, component_library)
  round051_install(child, component_library)

  generated <- bigbang::create_metapackage(
    "verifyverse", c(parent, child), dest_dir = destination,
    document = TRUE, verbose = FALSE, import_deps = character(),
    force_deps = character(), reexport = TRUE,
    reexport_prefer = c(s = "verifyparent")
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))
  tarball <- file.path(sandbox, "verifyverse_0.1.0.tar.gz")
  expect_true(file.exists(tarball))
  round051_install(tarball, meta_library)

  withr::with_libpaths(meta_library, {
    loadNamespace("verifyverse")
    before_warning <- NULL
    before_install <- withCallingHandlers(
      getExportedValue("verifyverse", "verifyverse_conflicts")(),
      warning = function(condition) {
        before_warning <<- condition
        invokeRestart("muffleWarning")
      }
    )
    expect_s3_class(before_warning, "bigbang_warning_reexport_verification")
    expect_identical(
      getExportedValue("verifyverse", "verifyverse_reexport_verification")(
        before_install
      )$missing,
      "verifyparent, verifychild"
    )
    expect_true(is.na(getExportedValue(
      "verifyverse", "verifyverse_reexport_verification"
    )(before_install)$identical))
  })

  withr::local_libpaths(c(meta_library, component_library, .libPaths()))
  loadNamespace("verifyverse")
  install_function <- getExportedValue("verifyverse", "verifyverse_install")
  warning_condition <- NULL
  result <- withCallingHandlers(
    install_function(lib = component_library, verbose = FALSE),
    warning = function(condition) {
      if (inherits(condition, "bigbang_warning_reexport_verification")) {
        warning_condition <<- condition
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_s3_class(warning_condition, "bigbang_warning_reexport_verification")
  expect_match(conditionMessage(warning_condition), "s", fixed = TRUE)
  expect_match(conditionMessage(warning_condition), "distinct objects", fixed = TRUE)
  expect_identical(getExportedValue(
    "verifyverse", "verifyverse_reexport_verification"
  )(result)$identical, FALSE)
  expect_identical(getExportedValue("verifyverse", "s")(), "parent")

  conflicts_warning <- NULL
  conflicts <- withCallingHandlers(
    getExportedValue("verifyverse", "verifyverse_conflicts")(),
    warning = function(condition) {
      conflicts_warning <<- condition
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(conflicts_warning, "bigbang_warning_reexport_verification")
  expect_s3_class(conflicts, "verifyverse_conflicts")
  expect_identical(getExportedValue(
    "verifyverse", "verifyverse_reexport_verification"
  )(conflicts)$identical, FALSE)
  expect_identical(getExportedValue(
    "verifyverse", "verifyverse_reexport_verification"
  )(conflicts)$missing, "")
})

test_that("clean re-export verification follows the runtime library order", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-round063-second-library-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  second_library <- file.path(sandbox, "second-library")
  target_library <- file.path(sandbox, "target-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(second_library)
  dir.create(target_library)
  parent <- round051_make_archive(
    source_root, archive_dir, "secondparent", "s",
    body = "s <- function() 'second'"
  )
  child <- round051_make_archive(
    source_root, archive_dir, "secondchild", "s",
    body = "# imported only",
    namespace_extra = "importFrom(secondparent, s)",
    imports = "secondparent"
  )
  generated <- round051_create(
    "secondverse", c(parent, child), destination, reexport = TRUE,
    reexport_prefer = c(s = "secondparent")
  )
  round051_install(parent, second_library)
  round051_install(child, second_library)
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L,
                   info = paste(build_output, collapse = "\n"))
  meta_archive <- file.path(sandbox, "secondverse_0.1.0.tar.gz")
  round051_install(meta_archive, meta_library)

  withr::with_libpaths(c(meta_library, second_library), {
    base::loadNamespace("secondverse")
    namespace <- base::getNamespace("secondverse")
    base::get(".set_reexport_library", envir = namespace)(target_library)
    warning_condition <- NULL
    verification <- withCallingHandlers(
      base::get(".reexport_verify", envir = namespace)(warn = TRUE),
      warning = function(condition) {
        if (inherits(condition, "bigbang_warning_reexport_verification")) {
          warning_condition <<- condition
          invokeRestart("muffleWarning")
        }
      }
    )
    expect_null(warning_condition)
    expect_identical(verification$missing, "")
    expect_identical(verification$not_installed, "")
    expect_identical(verification$not_exported, "")
    expect_identical(verification$loaded_from_other_library, "")
    expect_identical(verification$installed, "secondparent, secondchild")
  })
})

test_that("round 055 verification warns when an installed owner lost its export", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round055-missing-export-")
  source_root <- file.path(sandbox, "sources")
  bad_source_root <- file.path(sandbox, "bad-sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(bad_source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)

  parent <- round051_make_archive(
    source_root, archive_dir, "missingparent", "s",
    body = "s <- function() 'parent'"
  )
  child <- round051_make_archive(
    source_root, archive_dir, "missingchild", "s",
    body = "# imported only",
    namespace_extra = "importFrom(missingparent, s)",
    imports = "missingparent"
  )
  generated <- round051_create(
    "missingverse", c(parent, child), destination, reexport = TRUE,
    reexport_prefer = c(s = "missingparent")
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L,
                   info = paste(build_output, collapse = "\n"))
  meta_archive <- file.path(sandbox, "missingverse_0.1.0.tar.gz")
  expect_true(file.exists(meta_archive))
  round051_install(meta_archive, meta_library)

  bad_parent <- round051_make_archive(
    bad_source_root, archive_dir, "missingparent", character(),
    body = "invisible(NULL)", version = "0.2.0"
  )
  round051_install(bad_parent, component_library)
  withr::with_libpaths(c(meta_library, component_library), {
    loadNamespace("missingverse")
    warning_condition <- NULL
    result <- withCallingHandlers(
      getExportedValue("missingverse", "missingverse_conflicts")(),
      warning = function(condition) {
        if (inherits(condition, "bigbang_warning_reexport_verification")) {
          warning_condition <<- condition
          invokeRestart("muffleWarning")
        }
      }
    )
    expect_s3_class(warning_condition, "bigbang_warning_reexport_verification")
    expect_match(conditionMessage(warning_condition), "missingparent", fixed = TRUE)
    expect_identical(
      getExportedValue("missingverse", "missingverse_reexport_verification")(
        result
      )$missing,
      "missingparent, missingchild"
    )
    expect_identical(
      getExportedValue("missingverse", "missingverse_reexport_verification")(
        result
      )$installed,
      "missingparent"
    )
    expect_true(is.na(getExportedValue(
      "missingverse", "missingverse_reexport_verification"
    )(result)$identical))
  })
})

test_that("round 055 names equivalent copies separately from distinct objects", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-round055-equivalent-copies-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)

  helper <- round051_make_archive(
    source_root, archive_dir, "helper055", "sneaky",
    body = "sneaky <- function(name, value, envir) assign(name, value, envir = envir)"
  )
  parent <- round051_make_archive(
    source_root, archive_dir, "equivparent", "s",
    body = "s <- function() 'same'"
  )
  child <- round051_make_archive(
    source_root, archive_dir, "equivchild", "s",
    body = c(
      ".onLoad <- function(lib, pkg) {",
      "  helper <- base::getExportedValue('helper055', 'sneaky')",
      "  helper('s', function() 'same', base::asNamespace(pkg))",
      "}"
    ),
    namespace_extra = "importFrom(equivparent, s)",
    imports = c("equivparent", "helper055")
  )
  round051_install(helper, component_library)
  round051_install(parent, component_library)
  round051_install(child, component_library)
  generated <- round051_create(
    "equivverse", c(parent, child, helper), destination, reexport = TRUE,
    reexport_prefer = c(s = "equivparent")
  )
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L,
                   info = paste(build_output, collapse = "\n"))
  meta_archive <- file.path(sandbox, "equivverse_0.1.0.tar.gz")
  expect_true(file.exists(meta_archive))
  round051_install(meta_archive, meta_library)
  withr::with_libpaths(c(meta_library, component_library), {
    loadNamespace("equivverse")
    warning_condition <- NULL
    result <- withCallingHandlers(
      getExportedValue("equivverse", "equivverse_install")(
        lib = component_library, verbose = FALSE
      ),
      warning = function(condition) {
        if (inherits(condition, "bigbang_warning_reexport_verification")) {
          warning_condition <<- condition
          invokeRestart("muffleWarning")
        }
      }
    )
    expect_s3_class(warning_condition, "bigbang_warning_reexport_verification")
    expect_match(conditionMessage(warning_condition), "equivalent copies", fixed = TRUE)
    expect_identical(getExportedValue(
      "equivverse", "equivverse_reexport_verification"
    )(result)$identical, FALSE)
  })
})

test_that("round 053 poison component cannot mask generated runtime calls", {
  testthat::skip_on_cran()
  sandbox <- tempfile("bigbang-round053-poison-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  meta_library <- file.path(sandbox, "meta-library")
  component_library <- file.path(sandbox, "component-library")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  dir.create(meta_library)
  dir.create(component_library)

  probe_dir <- file.path(sandbox, "probe")
  dir.create(probe_dir)
  bigbang:::write_metapackage_files(
    "poisonverse", character(), character(), dest_dir = probe_dir,
    overwrite = TRUE, reexport = TRUE
  )
  writeLines(c(
    "Package: poisonverse", "Version: 0.1.0",
    "Title: Poison probe", "Description: Poison probe.",
    "License: MIT"
  ), file.path(probe_dir, "DESCRIPTION"), useBytes = TRUE)
  bigbang:::write_consistency_test("poisonverse", probe_dir)
  bigbang:::write_basic_vignette(
    "poisonverse", character(), probe_dir, verbose = FALSE
  )
  function_names <- character()
  special_names <- character()
  token_name <- function(value) {
    ifelse(grepl("^`.*`$", value), substring(value, 2L, nchar(value) - 1L), value)
  }
  probe_files <- c(
    list.files(probe_dir, pattern = "\\.R$", full.names = TRUE),
    list.files(file.path(probe_dir, "tests"), pattern = "\\.R$",
               full.names = TRUE)
  )
  for (path in probe_files) {
    parsed <- parse(file = path, keep.source = TRUE)
    data <- utils::getParseData(parsed, includeText = TRUE)
    function_names <- c(
      function_names,
      token_name(data$text[data$token == "SYMBOL_FUNCTION_CALL"])
    )
    special_names <- c(special_names, token_name(data$text[data$token == "SPECIAL"]))
  }
  vignette_code <- readLines(
    file.path(probe_dir, "vignettes", "introduction-poisonverse.Rmd"),
    warn = FALSE
  )
  evaluated <- character()
  in_chunk <- FALSE
  include_chunk <- FALSE
  chunk <- character()
  for (line in vignette_code) {
    if (!in_chunk && grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      include_chunk <- !grepl("eval[[:space:]]*=[[:space:]]*FALSE",
                              line, ignore.case = TRUE)
      chunk <- character()
    } else if (in_chunk && identical(line, "```")) {
      if (include_chunk) evaluated <- c(evaluated, chunk)
      in_chunk <- FALSE
    } else if (in_chunk) {
      chunk <- c(chunk, line)
    }
  }
  if (length(evaluated) > 0L) {
    data <- utils::getParseData(parse(text = evaluated), includeText = TRUE)
    function_names <- c(
      function_names,
      token_name(data$text[data$token == "SYMBOL_FUNCTION_CALL"])
    )
    special_names <- c(special_names, token_name(data$text[data$token == "SPECIAL"]))
  }
  install_engine <- bigbang:::.render_install_engine(
    "poisonverse",
    list(list(
      package = "poisoncomponent", stem = "poisoncomponent_0.1.0",
      ext = ".tar.gz"
    ))
  )
  engine_data <- utils::getParseData(
    parse(text = install_engine), includeText = TRUE
  )
  function_names <- c(
    function_names,
    token_name(engine_data$text[engine_data$token == "SYMBOL_FUNCTION_CALL"])
  )
  special_names <- c(
    special_names, token_name(engine_data$text[engine_data$token == "SPECIAL"])
  )
  function_names <- sort(unique(function_names))
  special_names <- sort(unique(special_names))
  own_symbols <- bigbang:::.generated_metapackage_symbols("poisonverse")
  poison_exports <- setdiff(sort(unique(c(
    function_names, special_names, own_symbols,
    "identical", "requireNamespace", "getExportedValue", "readRDS",
    "saveRDS"
  ))), bigbang:::.r_syntax_symbols())
  excluded <- intersect(poison_exports, own_symbols)
  poison_body <- vapply(
    poison_exports,
    function(symbol) {
      lhs <- if (symbol %in% bigbang:::.r_syntax_symbols()) {
        paste0("`", symbol, "`")
      } else if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", symbol)) {
        symbol
      } else {
        paste0("`", symbol, "`")
      }
      paste0(lhs, " <- function(...) 'poison'")
    },
    character(1L)
  )
  poison_archive <- round051_make_archive(
    source_root, archive_dir, "poisoncomponent", poison_exports,
    body = poison_body
  )
  generated <- bigbang::create_metapackage(
    "poisonverse", poison_archive, dest_dir = destination, document = TRUE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE, reexport_exclude = excluded
  )
  generated_files <- list.files(file.path(generated$path, "R"),
                                pattern = "\\.R$", full.names = TRUE)
  generated_files <- c(
    generated_files,
    list.files(file.path(generated$path, "tests"),
               pattern = "\\.R$", full.names = TRUE)
  )
  reexports <- file.path(generated$path, "R", "reexports.R")
  for (path in generated_files) expect_silent(parse(file = path))

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  build_output <- withr::with_dir(sandbox, system2(
    r_binary, c("CMD", "build", shQuote(generated$path)),
    stdout = TRUE, stderr = TRUE
  ))
  build_status <- attr(build_output, "status")
  if (is.null(build_status)) build_status <- 0L
  expect_identical(build_status, 0L, info = paste(build_output, collapse = "\n"))
  tarball <- file.path(sandbox, "poisonverse_0.1.0.tar.gz")
  expect_true(file.exists(tarball))
  check_libs <- paste(c(component_library, Sys.getenv("R_LIBS_USER")),
                      collapse = .Platform$path.sep)
  check_output <- withr::with_envvar(
    c(R_LIBS_USER = check_libs),
    withr::with_dir(sandbox, system2(
      r_binary,
      c("CMD", "check", "--as-cran", "--no-manual", "--no-install",
        "--no-examples",
        paste0("--library=", component_library), shQuote(tarball)),
      stdout = TRUE, stderr = TRUE
    ))
  )
  check_status <- check_output[grepl("^Status:", check_output)]
  expect_true(length(check_status) == 1L,
              info = paste(check_output, collapse = "\n"))
  expect_false(
    grepl("ERROR|WARNING", check_status),
    info = paste(check_output, collapse = "\n")
  )
  round051_install(tarball, meta_library)
  withr::local_libpaths(c(meta_library, component_library, .libPaths()))
  on.exit({
    if (base::`%in%`("package:poisoncomponent", base::search())) {
      base::detach("package:poisoncomponent", unload = TRUE,
                   character.only = TRUE)
    }
  }, add = TRUE)
  loadNamespace("poisonverse")
  attach <- getExportedValue("poisonverse", "poisonverse_attach")
  missing_before <- NULL
  withCallingHandlers(attach(), warning = function(condition) {
    missing_before <<- condition
    invokeRestart("muffleWarning")
  })
  expect_match(conditionMessage(missing_before), "Not installed", fixed = TRUE)
  round051_install(poison_archive, component_library)
  missing_after <- NULL
  withCallingHandlers(attach(), warning = function(condition) {
    missing_after <<- condition
    invokeRestart("muffleWarning")
  })
  expect_null(missing_after)
  install_function <- base::getExportedValue("poisonverse", "poisonverse_install")
  result <- install_function(lib = component_library, verbose = FALSE)
  expect_true(base::is.data.frame(result$reexport_verification))
  conflicts <- base::getExportedValue("poisonverse", "poisonverse_conflicts")()
  expect_s3_class(conflicts, "poisonverse_conflicts")
  expect_s3_class(
    base::getExportedValue("poisonverse", "poisonverse_reexport_verification")(
      conflicts
    ),
    "poisonverse_reexport_verification"
  )

  unqualified <- base::tempfile("bigbang-round053-unqualified-")
  base::writeLines(
    base::gsub("base::", "", base::readLines(reexports), fixed = TRUE),
    unqualified
  )
  poisoned_env <- base::new.env(parent = base::baseenv())
  base::assign(
    "character", function(...) base::stop("unqualified poison"),
    envir = poisoned_env
  )
  expect_error(base::sys.source(unqualified, poisoned_env), "unqualified poison")
})

test_that("generated call qualification respects parse tokens", {
  content <- paste(
    'message <- "symbol nchar( is invalid"',
    "# comment with paste( and nchar(",
    "value <- nchar(file.exists(path))",
    "already <- base::c(nchar(value), as.character.factor(value))",
    sep = "\n"
  )
  qualified <- bigbang:::.qualify_generated_runtime_calls(content)
  expect_match(qualified, '"symbol nchar( is invalid"', fixed = TRUE)
  expect_match(qualified, "# comment with paste( and nchar(", fixed = TRUE)
  expect_match(qualified, "base::nchar(base::file.exists(path))", fixed = TRUE)
  expect_match(
    qualified,
    "base::c(base::nchar(value), base::as.character.factor(value))",
    fixed = TRUE
  )
  expect_no_match(qualified, "base::base::")
})

test_that("control-byte re-export suggestions round-trip as R literals", {
  sandbox <- tempfile("bigbang-round055-control-literal-")
  source_root <- file.path(sandbox, "sources")
  archive_dir <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archive_dir)
  dir.create(destination)
  symbol <- paste0("a", rawToChar(as.raw(8L)), "b")
  parent <- round051_make_archive(
    source_root, archive_dir, "controlparent", symbol,
    body = paste0("`", symbol, "` <- function() 'parent'")
  )
  child <- round051_make_archive(
    source_root, archive_dir, "controlchild", symbol,
    body = "# imported only",
    namespace_extra = paste0("importFrom(controlparent, \"", symbol, "\")")
  )
  condition <- round051_collision_condition(round051_create(
    "controlverse", c(parent, child), destination, reexport = TRUE
  ))
  expect_s3_class(condition, "bigbang_error_reexport_collision")
  suggestion <- bigbang:::.reexport_prefer_literal(symbol, "controlparent")
  mapping <- eval(parse(text = paste0("c(", suggestion, ")")))
  expect_identical(names(mapping), symbol)
  generated <- round051_create(
    "controlverse", c(parent, child), destination, reexport = TRUE,
    reexport_prefer = mapping
  )
  namespace <- paste(readLines(file.path(generated$path, "NAMESPACE")),
                     collapse = "\n")
  expect_true(grepl('export("a\\u0008b")', namespace, fixed = TRUE))
})

test_that("skipped multi-owner diagnostics keep each owner prefix", {
  empty <- bigbang:::.reexport_empty_evidence()
  skipped_parent <- list(
    package = "skippedparent", exports = "s", imports = list(),
    reexport_evidence = empty
  )
  child <- list(
    package = "skippedchild", exports = "s",
    imports = list(list("skippedparent", "s")),
    reexport_evidence = empty
  )
  live <- list(
    package = "liveowner", exports = "s", imports = list(),
    reexport_evidence = empty
  )
  condition <- tryCatch(
    bigbang:::.resolve_reexport_plan(
      list(child, live),
      omitted = data.frame(
        component = "skippedparent", input = "broken.tar.gz",
        reason = "broken archive", stringsAsFactors = FALSE
      )
    ),
    error = identity
  )
  expect_s3_class(condition, "bigbang_error_reexport_skipped")
  expect_match(condition$message, "skippedchild:", fixed = TRUE)
  expect_match(condition$message, "liveowner:", fixed = TRUE)
  expect_match(condition$message, "broken archive", fixed = TRUE)
})
