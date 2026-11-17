counter_covr4 <- function(expr) {
  call("if", TRUE, call("{", quote(.Call(covr:::covr_count, "key")), expr))
}

counter_covr3 <- function(expr) {
  call("if", TRUE, call("{", quote(covr:::count("key")), expr))
}

instrument_pick <- function(counter) {
  call(
    "{",
    counter(call("<-", quote(x), call(
      "if", counter(quote(!is.null(a))), counter(quote(a)), counter(quote(b))
    ))),
    counter(quote(x))
  )
}

test_that("coverage counters are removed from code embedded in meta-packages", {
  original <- .language_without_srcrefs(quote({
    x <- if (!is.null(a)) a else b
    x
  }))
  for (counter in list(counter_covr4, counter_covr3)) {
    instrumented <- instrument_pick(counter)
    expect_identical(.strip_coverage_counters(instrumented), original)
  }

  recorded <- call(
    "if", TRUE, call("{", quote(covr:::count_and_record("key")), quote(a))
  )
  expect_identical(.strip_coverage_counters(recorded), quote(a))
  dot_call <- call(
    "if", TRUE, call("{", quote(base::.Call(covr:::covr_count, "key")), NULL)
  )
  expect_identical(.strip_coverage_counters(call("f", dot_call)), quote(f(NULL)))

  # An `if (TRUE)` block that is not a counter is kept.
  plain <- quote(if (TRUE) {
    message("a")
    a
  })
  expect_identical(
    .strip_coverage_counters(.language_without_srcrefs(plain)),
    .language_without_srcrefs(plain)
  )
})

test_that("closures lose their counters and keep their environment", {
  pick <- function(a, b = if (is.null(a)) 1 else 2) {
    x <- if (!is.null(a)) a else b
    x
  }
  traced <- pick
  body(traced) <- instrument_pick(counter_covr4)
  formals(traced)$b <- call(
    "if", counter_covr4(quote(is.null(a))), counter_covr4(1), counter_covr4(2)
  )

  cleaned <- .without_coverage_counters(traced)
  expect_identical(
    .language_without_srcrefs(body(cleaned)),
    .language_without_srcrefs(body(pick))
  )
  expect_identical(formals(cleaned)$b, quote(if (is.null(a)) 1 else 2))
  expect_identical(environment(cleaned), environment(pick))
  expect_identical(cleaned(NULL, "b"), "b")

  expect_identical(.without_coverage_counters(pick), pick)
  expect_identical(.without_coverage_counters(sum), sum)
  expect_identical(.without_coverage_counters(1:3), 1:3)
  no_default <- function(a, b) NULL
  expect_identical(.without_coverage_counters(no_default), no_default)
})

test_that("deparsed code that parses back differently is detected", {
  # covr 4.0.0 builds this structure. R deparses it as
  # `if (!is.null(a)) if (TRUE) {...} else b`, so the `else` moves to the
  # inner `if` when the text is parsed again.
  ambiguous <- function(a, b) NULL
  body(ambiguous) <- call(
    "if", quote(!is.null(a)), counter_covr4(quote(a)), quote(b)
  )
  expect_identical(ambiguous(NULL, "b"), "b")
  source <- c("ambiguous <- ", deparse(ambiguous, width.cutoff = 500L))
  expect_false(.deparsed_code_matches(source, list(ambiguous = ambiguous)))

  cleaned <- .without_coverage_counters(ambiguous)
  source <- c("cleaned <- ", deparse(cleaned, width.cutoff = 500L))
  expect_true(.deparsed_code_matches(source, list(cleaned = cleaned)))

  expect_true(.deparsed_code_matches(
    c("extensions <- ", deparse(c(".tar", ".zip"))),
    list(extensions = c(".tar", ".zip"))
  ))
  expect_false(.deparsed_code_matches(
    c("extensions <- ", deparse(c(".tar", ".zip"))),
    list(other = c(".tar", ".zip"))
  ))
  expect_false(.deparsed_code_matches("extensions <- c(", list(extensions = 1)))
})

test_that("the embedded archive reader carries no coverage counters", {
  template <- .archive_reader_template()
  expect_false(grepl("covr", template, fixed = TRUE))
  expect_true(is.expression(parse(text = template, keep.source = FALSE)))

  local_mocked_bindings(.deparsed_code_matches = function(...) FALSE)
  expect_error(.archive_reader_template(), class = "bigbang_error_internal")
})
