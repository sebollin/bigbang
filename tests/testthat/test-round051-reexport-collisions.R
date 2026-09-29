round051_export_directive <- function(symbol) {
  if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", symbol)) {
    paste0("export(", symbol, ")")
  } else {
    paste0("export(\"", symbol, "\")")
  }
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

test_that("round 051 resolves simple, chain, and complete-import origins", {
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
  result <- round051_create(
    "originverse", c(origin_c, origin_full, origin_a, origin_b), destination,
    reexport = TRUE
  )
  expect_identical(result$reexports$symbol, "shared")
  expect_identical(result$reexports$package, "originra")
  expect_identical(result$reexports$resolution, "same_origin")
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

  local <- expect_collision("local", body = "s <- function() 'b'", reason = "Assignment")
  expect_match(local$message, "locala.*localb", perl = TRUE)
  expect_collision("onload", body = c(
    ".onLoad <- function(lib, pkg) assign('s', function() 'b', envir = asNamespace(pkg))"
  ), reason = "Call assign targets")
  expect_collision(
    "sysdat", body = "# imported only", sysdata = list(s = function() "b"),
    reason = "sysdata.rda contains"
  )
  expect_collision(
    "generic", body = "setGeneric('s')", reason = "Call setGeneric targets"
  )
  expect_collision(
    "dynamic", body = "list2env(list(s = function() 'b'), envir = .GlobalEnv)",
    reason = "Dynamic namespace mutation"
  )
  expect_collision(
    "evaldyn", body = "eval(parse(text = 's <- 2'))", reason = "Dynamic namespace mutation"
  )
  expect_collision(
    "rlangdyn", body = "rlang::env_bind(.GlobalEnv, s = function() 'b')",
    reason = "Dynamic namespace mutation"
  )
  expect_collision(
    "utilsdyn", body = "utils::assignInMyNamespace('s', function() 'b')",
    reason = "Dynamic namespace mutation"
  )
  expect_collision(
    "badparse", body = "# imported only",
    files = list("R/broken.R" = "s <- function("), reason = "Could not parse R file"
  )
  expect_collision(
    "dupe", body = "# imported only",
    namespace_extra = c("importFrom(dupea, s)", "importFrom(dupea, s)"),
    reason = "unique import source"
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
      reason = paste0("Call ", call_name, " targets")
    )
  }
  expect_collision(
    "nonliteral", body = "setClass(x)", reason = "non-literal first argument"
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
  expect_match(external$data$reason, "external package 'utils'", fixed = TRUE)
})

test_that("round 051 validates options, names, exclusions, preference, and order", {
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
    namespace_extra = "importFrom(skiporigin, s)"
  )
  bytes <- readBin(origin, "raw", n = file.info(origin)$size)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], origin)
  condition <- round051_collision_condition(bigbang::create_metapackage(
    "skipverse", c(origin, child), dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character(),
    reexport = TRUE, on_component_error = "skip"
  ))
  expect_s3_class(condition, "bigbang_error_reexport_skipped")
  expect_match(condition$message, "skiporigin", fixed = TRUE)
  expect_false(dir.exists(file.path(destination, "skipverse")))
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
  conflicts <- getExportedValue("preferverse", "preferverse_conflicts")()
  expect_identical(conflicts$resolution, "preferred")
})
