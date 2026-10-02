round060_fixture_evidence <- function(package = "round060fixture") {
  root <- tempfile("bigbang-round060-corpus-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  invisible(file.copy(
    testthat::test_path("fixtures", "round060-syntax-corpus.R"),
    file.path(root, "R", "syntax-corpus.R")
  ))
  list(
    evidence = bigbang:::.reexport_source_evidence(root, package = package)
  )
}

test_that("the syntax corpus classifies parsed R without falling", {
  fixture_path <- testthat::test_path(
    "fixtures", "round060-syntax-corpus.R"
  )
  expect_gte(length(readLines(fixture_path, warn = FALSE)), 300L)
  fixture <- round060_fixture_evidence()
  evidence <- fixture$evidence

  expect_length(evidence$parse_errors, 0L)
  expect_length(evidence$non_simple, 5L)
  expected_dynamic <- c(
    "x[[\"f\"]]", "x$f", "environment(x)$f", "(expr)", "f()"
  )
  actual_dynamic <- vapply(evidence$non_simple, `[[`, character(1L), "name")
  expect_identical(actual_dynamic, expected_dynamic)
  expect_true(all(vapply(
    evidence$non_simple,
    function(item) {
      is.character(item$reason) && length(item$reason) == 1L &&
        nzchar(item$reason)
    },
    logical(1L)
  )))

  blockers <- bigbang:::.reexport_relevant_blockers(evidence, "f")
  expect_identical(
    vapply(blockers, `[[`, character(1L), "name"), expected_dynamic
  )
  expect_false(any(grepl(
    "^(base|utils|dplyr|data\\.table)::", actual_dynamic
  )))

  safe_targets <- c(
    simple = "helper",
    backticked_operator = "`+`",
    qualified = "pkg::f",
    qualified_internal = "pkg:::f",
    calculated_index = "x[[\"f\"]]",
    calculated_member = "x$f",
    calculated_environment = "environment(x)$f",
    calculated_parenthesized = "(expr)",
    calculated_nested = "f()"
  )
  expected_kinds <- c(
    simple = "simple", backticked_operator = "simple",
    qualified = "qualified", qualified_internal = "qualified",
    calculated_index = "calculated", calculated_member = "calculated",
    calculated_environment = "calculated",
    calculated_parenthesized = "calculated",
    calculated_nested = "calculated"
  )
  actual_kinds <- vapply(
    safe_targets,
    function(target) bigbang:::.reexport_call_target_kind(target)$kind,
    character(1L)
  )
  expect_identical(actual_kinds, expected_kinds)
  expect_false(any(vapply(evidence$indirect, function(item) {
    identical(item$name, "data.table::.") && isTRUE(item$active)
  }, logical(1L))))
})

test_that("the modern syntax fixture is checked only on supported R", {
  skip_if(getRversion() < "4.1.0")
  fixture_path <- testthat::test_path(
    "fixtures", "round060-modern-syntax-corpus.R"
  )
  expect_length(parse(file = fixture_path, keep.source = TRUE), 2L)
})

test_that("the source classifier fails closed on empty and unusual nodes", {
  empty <- data.frame(
    id = integer(), parent = integer(), token = character(),
    text = character(), line1 = integer(), col1 = integer()
  )
  expect_null(bigbang:::.reexport_call_expression(empty, integer()))
  expect_null(bigbang:::.reexport_call_expression(empty, 1L))
  expect_false(bigbang:::.reexport_position_before(empty, empty))
  one <- data.frame(line1 = 1L, col1 = 1L)
  two <- data.frame(line1 = 1L, col1 = 2L)
  expect_true(bigbang:::.reexport_position_before(one, two))
  expect_false(bigbang:::.reexport_position_before(two, one))
  expect_identical(
    bigbang:::.reexport_call_children(empty, integer()), empty
  )
  expect_null(bigbang:::.reexport_call_target(empty, empty))
  expect_null(bigbang:::.reexport_call_first_argument(empty, 1L))
  expect_null(bigbang:::.reexport_call_arg_id(empty, 1L))
  expect_null(bigbang:::.reexport_literal_arg(empty, 1L, 1L))
  expect_identical(
    bigbang:::.reexport_qualify_function(empty, integer()),
    list(package = NULL, function_name = NULL)
  )
  expect_false(bigbang:::.reexport_is_descendant(empty, integer(), 1L))
  expect_identical(bigbang:::.reexport_parse_descendants(empty, integer()),
                   integer())
  expect_identical(bigbang:::.reexport_parse_definitions(empty),
                   list(definitions = list(), roots = integer()))
  expect_null(bigbang:::.reexport_definition_name(empty, 1L))
  expect_null(bigbang:::.reexport_parse_target(NA_character_))
  expect_null(bigbang:::.reexport_call_target_root(character()))
  expect_null(bigbang:::.reexport_call_target_root(NA_character_))
  expect_null(bigbang:::.reexport_call_target_root("base"))
  expect_identical(bigbang:::.reexport_call_target_root("pkg::f(x)"), "::")
  expect_false(bigbang:::.reexport_simple_symbol("(x)"))
  expect_identical(
    bigbang:::.reexport_call_target_kind(character())$kind, "undetermined"
  )
  expect_identical(
    bigbang:::.reexport_call_target_kind(NA_character_)$kind, "undetermined"
  )
  expect_identical(bigbang:::.reexport_call_target_kind("")$kind,
                   "undetermined")
  expect_identical(bigbang:::.reexport_call_target_kind("a + b")$kind,
                   "calculated")
  expect_identical(bigbang:::.reexport_call_target_kind("1")$kind,
                   "undetermined")
  expect_identical(
    bigbang:::.reexport_call_target_kind("function(x) x")$kind,
    "calculated"
  )
  expect_identical(bigbang:::.reexport_call_target_kind("(")$kind,
                   "undetermined")
  cycle <- data.frame(
    id = c(1L, 2L), parent = c(2L, 1L), token = c("expr", "expr"),
    text = c("a", "b"), line1 = c(1L, 1L), col1 = c(1L, 2L)
  )
  expect_false(bigbang:::.reexport_is_descendant(cycle, 1L, 3L))

  root <- tempfile("bigbang-round060-parse-data-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  writeLines("value <- function() NULL", file.path(root, "R", "value.R"))
  testthat::local_mocked_bindings(
    .reexport_get_parse_data = function(...) stop("parse data probe"),
    .package = "bigbang"
  )
  evidence <- bigbang:::.reexport_source_evidence(root)
  expect_match(evidence$parse_errors[[1L]]$error, "parse data probe")
})

test_that("the source tree is itself a valid diagnostic corpus", {
  skip_on_cran()
  package_root <- normalizePath(testthat::test_path("..", ".."),
                                winslash = "/", mustWork = TRUE)
  r_dir <- file.path(package_root, "R")
  if (!dir.exists(r_dir)) {
    testthat::skip("The installed check copy has no package source tree.")
  }
  evidence <- bigbang:::.reexport_source_evidence(
    package_root, package = "bigbang"
  )
  expect_length(evidence$parse_errors, 0L)
  expect_length(evidence$non_simple, 0L)
  expect_false(any(vapply(evidence$indirect, function(item) {
    isTRUE(item$active) && grepl("^(base|utils)::", item$name)
  }, logical(1L))))
})

test_that("source evidence preserves the saved round061 reference", {
  reference <- testthat::test_path(
    "fixtures", "round061-source-reference.R"
  )
  source(reference, local = TRUE)
  root <- tempfile("bigbang-round061-reference-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  invisible(file.copy(
    testthat::test_path("fixtures", "round060-syntax-corpus.R"),
    file.path(root, "R", "syntax-corpus.R")
  ))
  evidence <- bigbang:::.reexport_source_evidence(
    root, package = "round060fixture"
  )
  counts <- vapply(
    evidence[names(round061_source_reference$counts)], length, integer(1L)
  )
  expect_identical(counts, round061_source_reference$counts)
})

test_that("qualified calls activate own helpers and preserve the collision proof", {
  root <- tempfile("bigbang-round060-own-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c(
    "helper <- function() assign('f', 1, envir = asNamespace('round060own'))",
    ".onLoad <- function(lib, pkg) round060own::helper()"
  ), file.path(root, "R", "own.R"), useBytes = TRUE)
  evidence <- bigbang:::.reexport_source_evidence(
    root, package = "round060own"
  )
  expect_length(evidence$non_simple, 0L)
  expect_true(length(
    bigbang:::.reexport_relevant_blockers(evidence, "f")
  ) > 0L)

  control_root <- tempfile("bigbang-round060-control-")
  dir.create(file.path(control_root, "R"), recursive = TRUE)
  writeLines(c(
    "helper <- function(x) {",
    "  dplyr::filter(x, TRUE)",
    "  x$col <- 1L",
    "  x",
    "}",
    "helper(NULL)"
  ), file.path(control_root, "R", "control.R"), useBytes = TRUE)
  b_evidence <- bigbang:::.reexport_source_evidence(
    control_root, package = "round060B"
  )
  a_evidence <- bigbang:::.reexport_empty_evidence()
  component_a <- list(
    package = "round060A", exports = "f", dependencies = character(),
    imports = list(), reexport_evidence = a_evidence,
    reexport_evidence_loaded = TRUE
  )
  component_b <- list(
    package = "round060B", exports = "f", dependencies = "round060A",
    imports = list(list("round060A", "f")),
    reexport_evidence = b_evidence, reexport_evidence_loaded = TRUE
  )
  omitted <- data.frame(
    component = character(), reason = character(),
    stringsAsFactors = FALSE
  )
  plan <- bigbang:::.resolve_reexport_plan(
    list(component_a, component_b), omitted = omitted,
    prefer = c(f = "round060A")
  )
  expect_identical(plan$table$diagnosis[[1L]], "probable_same_object")
  expect_length(
    bigbang:::.reexport_relevant_blockers(b_evidence, "f"), 0L
  )
})

round060_mutant_environment <- function() {
  environment <- new.env(parent = asNamespace("bigbang"))
  clone <- function(name) {
    original <- get(name, envir = asNamespace("bigbang"))
    assign(
      name,
      eval(call("function", formals(original), body(original)), environment),
      environment
    )
  }
  for (name in c(
    ".reexport_call_target_kind", ".reexport_package_scope",
    ".reexport_source_evidence", ".reexport_relevant_blockers",
    ".reexport_probe", ".resolve_reexport_plan"
  )) clone(name)
  environment$.reexport_call_target_kind <- function(text) {
    result <- bigbang:::.reexport_call_target_kind(text)
    if (identical(result$kind, "qualified")) {
      return(list(
        kind = "calculated", root = "::",
        reason = "Qualified destinations were treated as calculated."
      ))
    }
    result
  }
  environment
}

test_that("negative controls fail under the two removed rules", {
  mutant_path <- testthat::test_path(
    "fixtures", "round060-source-mutant.R"
  )
  mutant_environment <- new.env(parent = baseenv())
  source(mutant_path, local = mutant_environment)
  corpus_root <- tempfile("bigbang-round060-reverted-")
  dir.create(file.path(corpus_root, "R"), recursive = TRUE)
  withr::defer(unlink(corpus_root, recursive = TRUE, force = TRUE))
  invisible(file.copy(
    testthat::test_path("fixtures", "round060-syntax-corpus.R"),
    file.path(corpus_root, "R", "syntax-corpus.R")
  ))
  reverted <- tryCatch(
    mutant_environment$.round060_source_mutant(
      corpus_root, package = "round060fixture"
    ),
    error = identity
  )
  if (inherits(reverted, "error")) {
    expect_match(conditionMessage(reverted), "length zero|length > 1")
  } else {
    expect_length(reverted$non_simple, 5L)
  }

  control_root <- tempfile("bigbang-round060-mutant-")
  dir.create(file.path(control_root, "R"), recursive = TRUE)
  withr::defer(unlink(control_root, recursive = TRUE, force = TRUE))
  writeLines(c(
    "helper <- function(x) {",
    "  dplyr::filter(x, TRUE)",
    "  x$col <- 1L",
    "  x",
    "}",
    "helper(NULL)"
  ), file.path(control_root, "R", "control.R"), useBytes = TRUE)
  mutant <- round060_mutant_environment()
  current_evidence <- bigbang:::.reexport_source_evidence(
    control_root, package = "round060B"
  )
  mutant_evidence <- mutant$.reexport_source_evidence(
    control_root, package = "round060B"
  )
  expect_length(
    bigbang:::.reexport_relevant_blockers(current_evidence, "f"), 0L
  )
  expect_true(length(
    mutant$.reexport_relevant_blockers(mutant_evidence, "f")
  ) > 0L)

  component_a <- list(
    package = "round060A", exports = "f", dependencies = character(),
    imports = list(), reexport_evidence = bigbang:::.reexport_empty_evidence(),
    reexport_evidence_loaded = TRUE
  )
  component_b <- function(evidence) {
    list(
      package = "round060B", exports = "f", dependencies = "round060A",
      imports = list(list("round060A", "f")),
      reexport_evidence = evidence, reexport_evidence_loaded = TRUE
    )
  }
  omitted <- data.frame(
    component = character(), reason = character(),
    stringsAsFactors = FALSE
  )
  current_plan <- bigbang:::.resolve_reexport_plan(
    list(component_a, component_b(current_evidence)), omitted = omitted,
    prefer = c(f = "round060A")
  )
  mutant_plan <- mutant$.resolve_reexport_plan(
    list(component_a, component_b(mutant_evidence)), omitted = omitted,
    prefer = c(f = "round060A")
  )
  expect_identical(current_plan$table$diagnosis[[1L]], "probable_same_object")
  expect_identical(mutant_plan$table$diagnosis[[1L]], "undetermined")
})
