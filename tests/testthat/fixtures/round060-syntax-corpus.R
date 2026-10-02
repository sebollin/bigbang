# Round 060 source-analysis corpus. Parentheses in this comment are inert.
corpus_top_001 <- base::identity("text (not a call)")
corpus_top_002 <- utils::head(c(1L, 2L), 1L)
corpus_top_003 <- data.table::.(value = 1L)
corpus_top_004 <- 1:3 %>% identity() %>% identity() # nolint: pipe_consistency_linter
corpus_top_005 <- `+`(1L, 2L)
corpus_top_006 <- identity(function(value) value)
corpus_top_007 <- identity(~ value + 1)
corpus_top_008 <- identity(base::identity)
corpus_top_009 <- lapply(1:2, function(value) value)
corpus_top_010 <- tryCatch(identity(1L), error = function(error) NULL)
corpus_top_011 <- local({
  on.exit(invisible(NULL))
  identity(1L)
})
corpus_top_012 <- switch(1L, identity(1L), identity(2L))
corpus_top_013 <- if (TRUE) identity(1L) else identity(2L)
for (corpus_index in 1:2) identity(corpus_index)
while (FALSE) identity(NULL)
repeat {
  break
}
corpus_top_014 <- identity("quoted (parentheses) and # not a comment")
corpus_top_015 <- identity("a string with // and /* markers */")
corpus_top_016 <- identity("a string with a comma, a colon ::, and a pipe |>")
corpus_top_017 <- identity(
  base::identity(
    utils::head(c(1L, 2L), 1L)
  )
)
corpus_top_018 <- identity(list(argument = function(value) value))
corpus_top_019 <- identity(list(formula = ~ value + 1))
corpus_top_020 <- identity(list(namespace = base::identity))
corpus_top_021 <- UseMethod("corpus_top")
corpus_top_022 <- NextMethod()
corpus_top_024 <- on.exit(invisible(NULL))
corpus_top_025 <- .Call("corpus_call")
corpus_top_026 <- .External("corpus_external")
corpus_top_027 <- .C("corpus_c")
corpus_top_028 <- identity(list(a = 1L, b = 2L, c = 3L))
corpus_top_029 <- identity(c(
  "line one (text)",
  "line two (text)",
  "line three (text)"
))
corpus_top_030 <- identity(
  list(
    nested = list(
      call = base::identity,
      formula = ~ value,
      lambda = function(value) value
    )
  )
)

# Common functions are indexed but their bodies are not load-time active.
corpus_plain_helper <- function(value) value

corpus_common <- function(value) {
  common_001 <- base::identity(value)
  common_002 <- utils::head(value)
  common_003 <- dplyr::filter(value, TRUE)
  common_004 <- round060fixture::corpus_plain_helper(value)
  common_005 <- value %>% identity() %>% identity() # nolint: pipe_consistency_linter
  common_006 <- identity(function(argument) argument)
  common_007 <- identity(~ argument + 1)
  common_008 <- if (length(value)) identity(value) else identity(NULL)
  for (index in seq_along(value)) identity(index)
  while (FALSE) identity(value)
  repeat {
    break
  }
  common_009 <- switch(1L, identity(value), identity(NULL))
  common_010 <- tryCatch(
    identity(value),
    error = function(error) identity(error)
  )
  common_011 <- local({
    on.exit(invisible(NULL))
    identity(value)
  })
  common_012 <- UseMethod("corpus_common")
  common_013 <- NextMethod()
  common_014 <- Recall()
  common_015 <- on.exit(invisible(NULL))
  common_016 <- .Call("corpus_call", value)
  common_017 <- .External("corpus_external", value)
  common_018 <- .C("corpus_c", value)
  common_019 <- `+`(1L, 2L)
  common_020 <- identity(base::identity)
  common_021 <- identity(utils::head)
  value[["col"]] <- 1L
  value$col <- 1L
  environment(value)$field <- 1L
  common_022 <- identity("common string (parentheses)")
  common_023 <- identity(
    list(
      lambda = function(argument) argument,
      function_value = function(argument) argument,
      formula = ~ argument
    )
  )
  common_024 <- identity(
    base::identity(
      utils::head(value)
    )
  )
  common_025 <- tryCatch(
    local({
      on.exit(invisible(NULL))
      identity(value)
    }),
    error = function(error) identity(error),
    finally = invisible(NULL)
  )
  common_026 <- switch(
    1L,
    identity(value),
    identity(NULL)
  )
  common_027 <- if (TRUE) {
    identity(value)
  } else {
    identity(NULL)
  }
  common_028 <- identity("# parentheses in a string: ( )")
  common_029 <- identity("quotes: 'single' and \"double\"")
  common_030 <- identity(list(namespace = round060fixture::corpus_plain_helper))
  common_031 <- identity(value %>% identity()) # nolint: pipe_consistency_linter
  common_032 <- identity(identity(value))
  common_033 <- identity(data.table::.(value = value))
  common_034 <- identity(.Call("corpus_call", value))
  common_035 <- identity(.External("corpus_external", value))
  common_036 <- identity(.C("corpus_c", value))
  common_037 <- identity(UseMethod("corpus_common"))
  common_038 <- identity(NextMethod())
  common_039 <- identity(Recall())
  common_040 <- identity(on.exit(invisible(NULL)))
  common_041 <- identity(function(argument) argument)
  common_042 <- identity(function(argument) argument)
  common_043 <- identity(~ value + 1)
  common_044 <- identity(base::identity(value))
  common_045 <- identity(utils::head(value))
  common_046 <- identity(
    tryCatch(identity(value), error = function(error) identity(error))
  )
  common_047 <- identity(local({
    identity(value)
  }))
  common_048 <- identity(switch(1L, value, NULL))
  common_049 <- identity(if (TRUE) value else NULL)
  common_050 <- identity(for (index in 1L) index)
  invisible(common_001)
}

corpus_common_002 <- function(value) {
  second_001 <- base::identity(value)
  second_002 <- utils::head(value)
  second_003 <- dplyr::filter(value, TRUE)
  second_004 <- value %>% identity() %>% identity() # nolint: pipe_consistency_linter
  second_005 <- identity(function(argument) argument)
  second_006 <- identity(~ value)
  second_007 <- tryCatch(value, error = function(error) error)
  second_008 <- local({
    on.exit(invisible(NULL))
    value
  })
  second_009 <- UseMethod("corpus_common_002")
  second_010 <- NextMethod()
  second_011 <- Recall()
  second_012 <- on.exit(invisible(NULL))
  second_013 <- .Call("corpus_call", value)
  second_014 <- .External("corpus_external", value)
  second_015 <- .C("corpus_c", value)
  second_016 <- `+`(1L, 2L)
  second_017 <- identity(round060fixture::corpus_plain_helper(value))
  second_018 <- identity(data.table::.(value = value))
  second_019 <- identity("second string (parentheses)")
  second_020 <- identity(
    list(
      lambda = function(argument) argument,
      function_value = function(argument) argument,
      formula = ~ value
    )
  )
  value[["second_col"]] <- 2L
  value$second_col <- 2L
  environment(value)$second_field <- 2L
  invisible(second_001)
}

# The load hook keeps the safe forms active and puts the five calculated
# destinations below under explicit test labels.
.onLoad <- function(libname, pkgname) {
  load_001 <- base::identity(NULL)
  load_002 <- utils::head(NULL)
  load_003 <- data.table::.(value = 1L)
  load_004 <- identity(function(argument) argument)
  load_005 <- identity(function(argument) argument)
  load_006 <- identity(~ argument + 1)
  load_007 <- tryCatch(
    identity(NULL),
    error = function(error) identity(error)
  )
  load_008 <- local({
    on.exit(invisible(NULL))
    identity(NULL)
  })
  load_009 <- UseMethod("corpus")
  load_010 <- NextMethod()
  load_011 <- on.exit(invisible(NULL))
  load_012 <- .Call("corpus_call")
  load_013 <- .External("corpus_external")
  load_014 <- .C("corpus_c")
  load_015 <- identity(base::identity)
  load_016 <- identity(utils::head)
  load_017 <- identity("load string (parentheses)")
  load_018 <- identity("a comment marker # inside a string")
  load_019 <- identity("a pipe marker |> inside a string")
  load_020 <- identity(
    base::identity(
      utils::head(c(1L, 2L), 1L)
    )
  )
  load_021 <- identity(
    list(
      lambda = function(argument) argument,
      function_value = function(argument) argument,
      formula = ~ argument
    )
  )
  load_022 <- if (TRUE) identity(NULL) else identity(NULL)
  for (index in 1L) identity(index)
  while (FALSE) identity(NULL)
  repeat {
    break
  }
  load_023 <- switch(1L, identity(NULL), identity(NULL))
  load_024 <- identity(round060fixture::corpus_plain_helper)
  load_025 <- getFromNamespace("corpus_plain_helper", "round060fixture")
  load_026 <- identity(
    list(argument = base::identity, nested = list(value = 1L))
  )
  load_dynamic_001 <- x[["f"]]()
  load_dynamic_002 <- x$f()
  load_dynamic_003 <- environment(x)$f()
  load_dynamic_004 <- (expr)()
  load_dynamic_005 <- f()()
  load_027 <- identity(
    "multiline text with (parentheses) and a comma, safely quoted"
  )
  load_028 <- `+`(1L, 2L)
  load_029 <- identity(1L)
  load_030 <- identity(2L)
  load_031 <- identity(3L)
  load_032 <- identity(4L)
  load_033 <- identity(5L)
  load_034 <- identity(6L)
  load_035 <- identity(7L)
  load_036 <- identity(8L)
  load_037 <- identity(9L)
  load_038 <- identity(10L)
  load_039 <- identity(11L)
  load_040 <- identity(12L)
  load_041 <- identity(13L)
  load_042 <- identity(14L)
  load_043 <- identity(15L)
  load_044 <- identity(16L)
  load_045 <- identity(17L)
  load_046 <- identity(18L)
  load_047 <- identity(19L)
  load_048 <- identity(20L)
  load_049 <- identity(21L)
  load_050 <- identity(22L)
  load_051 <- identity(23L)
  load_052 <- identity(24L)
  load_053 <- identity(25L)
  load_054 <- identity(26L)
  load_055 <- identity(27L)
  load_056 <- identity(28L)
  load_057 <- identity(29L)
  load_058 <- identity(30L)
  load_059 <- identity(31L)
  load_060 <- identity(32L)
  load_061 <- identity(33L)
  load_062 <- identity(34L)
  load_063 <- identity(35L)
  load_064 <- identity(36L)
  load_065 <- identity(37L)
  load_066 <- identity(38L)
  load_067 <- identity(39L)
  load_068 <- identity(40L)
  load_069 <- identity(41L)
  load_070 <- identity(42L)
  load_071 <- identity(43L)
  load_072 <- identity(44L)
  load_073 <- identity(45L)
  load_074 <- identity(46L)
  load_075 <- identity(47L)
  load_076 <- identity(48L)
  load_077 <- identity(49L)
  load_078 <- identity(50L)
  load_079 <- identity(51L)
  load_080 <- identity(52L)
  load_081 <- identity(53L)
  load_082 <- identity(54L)
  load_083 <- identity(55L)
  load_084 <- identity(56L)
  load_085 <- identity(57L)
  load_086 <- identity(58L)
  load_087 <- identity(59L)
  load_088 <- identity(60L)
  load_089 <- identity(61L)
  load_090 <- identity(62L)
  load_091 <- identity(63L)
  load_092 <- identity(64L)
  load_093 <- identity(65L)
  load_094 <- identity(66L)
  load_095 <- identity(67L)
  load_096 <- identity(68L)
  load_097 <- identity(69L)
  load_098 <- identity(70L)
  load_099 <- identity(71L)
  load_100 <- identity(72L)
  invisible()
}
