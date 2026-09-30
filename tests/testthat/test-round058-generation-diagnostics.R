round058_scope_fixture <- function(files, package = "round058child") {
  root <- tempfile("bigbang-round058-scope-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  for (name in names(files)) {
    writeLines(files[[name]], file.path(root, "R", name), useBytes = TRUE)
  }
  evidence <- bigbang:::.reexport_source_evidence(root, package = package)
  child <- list(
    package = package, exports = "f", imports = list(list("round058root", "f")),
    reexport_evidence = evidence
  )
  parent <- list(
    package = "round058root", exports = "f", imports = list(),
    reexport_evidence = bigbang:::.reexport_empty_evidence()
  )
  list(
    evidence = evidence,
    probe = bigbang:::.reexport_probe(
      child, "f", list(child, parent), character()
    )
  )
}

test_that("load-time helpers are indexed across files", {
  called <- round058_scope_fixture(list(
    helper.R = "helper <- function() assign('f', 1, envir = asNamespace('round058child'))",
    load.R = ".onLoad <- function(lib, pkg) helper()"
  ))
  blockers <- bigbang:::.reexport_relevant_blockers(called$evidence, "f")
  expect_true(length(blockers) > 0L)
  expect_false(called$probe$demonstrated)

  unused <- round058_scope_fixture(list(
    helper.R = "helper <- function() assign('f', 1, envir = asNamespace('round058child'))",
    load.R = ".onLoad <- function(lib, pkg) invisible(NULL)"
  ))
  expect_length(
    bigbang:::.reexport_relevant_blockers(unused$evidence, "f"), 0L
  )
  expect_true(unused$probe$demonstrated)

  indirect <- round058_scope_fixture(list(
    helper.R = "helper <- function() assign('f', 1, envir = asNamespace('round058child'))",
    load.R = ".onLoad <- function(lib, pkg) do.call('helper', list())"
  ))
  expect_true(length(
    bigbang:::.reexport_relevant_blockers(indirect$evidence, "f")
  ) > 0L)
  expect_false(indirect$probe$demonstrated)
})

test_that("calculated load-time destinations are conservative blockers", {
  calls <- c(
    indexed = "fns[['helper']]()",
    member = "environment(x)$f()",
    nested = "f()()"
  )
  for (call in calls) {
    fixture <- round058_scope_fixture(list(
      helper.R = "helper <- function() invisible(NULL)",
      load.R = paste0(".onLoad <- function(lib, pkg) { fns <- list(helper = helper); ",
                      call, " }")
    ))
    expect_true(length(
      bigbang:::.reexport_relevant_blockers(fixture$evidence, "f")
    ) > 0L, info = names(call))
    expect_false(fixture$probe$demonstrated, info = names(call))
  }

  foreign <- round058_scope_fixture(list(
    helper.R = "helper <- function() invisible(NULL)",
    load.R = ".onLoad <- function(lib, pkg) getFromNamespace('helper', 'other')()"
  ))
  expect_true(length(
    bigbang:::.reexport_relevant_blockers(foreign$evidence, "f")
  ) > 0L)
  expect_false(foreign$probe$demonstrated)

  own <- round058_scope_fixture(list(
    helper.R = "helper <- function() invisible(NULL)",
    load.R = ".onLoad <- function(lib, pkg) getFromNamespace('helper', pkg)()"
  ))
  expect_length(
    bigbang:::.reexport_relevant_blockers(own$evidence, "f"), 0L
  )
  expect_true(own$probe$demonstrated)
})

test_that("the package parser handles literal arguments and parse failures", {
  parsed <- utils::getParseData(
    parse(text = c(
      "do.call('helper', list())",
      "getFromNamespace('helper', namespace = 'round058child')",
      "empty()"
    ), keep.source = TRUE),
    includeText = TRUE
  )
  do_call <- parsed$id[parsed$token == "SYMBOL_FUNCTION_CALL" &
                         parsed$text == "do.call"]
  do_call_expr <- bigbang:::.reexport_call_expression(parsed, do_call)
  expect_identical(
    bigbang:::.reexport_literal_arg(parsed, do_call_expr, 1L), "helper"
  )
  expect_null(bigbang:::.reexport_literal_arg(parsed, do_call_expr, 3L))

  namespace_call <- parsed$id[
    parsed$token == "SYMBOL_FUNCTION_CALL" &
      parsed$text == "getFromNamespace"
  ]
  namespace_expr <- bigbang:::.reexport_call_expression(parsed, namespace_call)
  expect_identical(
    bigbang:::.reexport_literal_arg(parsed, namespace_expr, 2L),
    "round058child"
  )
  empty_call <- parsed$id[parsed$token == "SYMBOL_FUNCTION_CALL" &
                            parsed$text == "empty"]
  empty_expr <- bigbang:::.reexport_call_expression(parsed, empty_call)
  expect_null(bigbang:::.reexport_literal_arg(parsed, empty_expr, 1L))
  expect_null(bigbang:::.reexport_literal_arg(parsed, 0L, 1L))
  expect_identical(
    bigbang:::.reexport_call_target_root("fns[['helper']]"), "[["
  )

  root <- tempfile("bigbang-round058-parse-error-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines("broken <- function(", file.path(root, "R", "broken.R"))
  evidence <- bigbang:::.reexport_source_evidence(root)
  expect_length(evidence$parse_errors, 1L)
})

test_that("re-export evidence can be loaded lazily from an archive", {
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  component <- list(
    path = fixture, ext = ".tar.gz", package = "toycomponent",
    reexport_native = FALSE
  )
  evidence <- bigbang:::.read_reexport_evidence(component)
  expect_type(evidence, "list")
  expect_false(evidence$native)
})
