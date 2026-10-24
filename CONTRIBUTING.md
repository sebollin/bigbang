# Contributing to bigbang

## Tests and lint

Run the complete suite before opening a pull request. `NOT_CRAN=true` includes
the tests that install packages and check generated metapackages:

```sh
NOT_CRAN=true Rscript --vanilla -e 'testthat::test_local(reporter = "summary", stop_on_failure = TRUE)'
Rscript --vanilla tests/integration/data-loss-verification.R
Rscript --vanilla -e 'print(lintr::lint_package())'
```

A pull request should leave the suite without failures and `lintr` without new
findings. New behavior comes with a test, and a fix with a test that fails
without it. The complete release gates, including `R CMD check --as-cran`, are
in `RELEASE.md`.

Tests must pass on Linux, Windows and macOS, and under root. r-universe builds
the Linux binaries as root, which ignores file permissions. A test that relies
on `Sys.chmod()` checks the restriction with `file.access()` and skips when it
is not enforced. A test that relies on symbolic links, case-sensitive file
names or POSIX tools skips where they are not available.

## Reproducible spelling checks

The package declares `Language: en`, while
`vignettes/bigbang-es.Rmd` is intentionally written in Spanish. The
high-level `spelling::spell_check_package()` function has no per-file exclusion
argument, so run the English checks explicitly:

```r
wordlist <- spelling::get_wordlist(".")

# DESCRIPTION and generated Rd files.
spelling::spell_check_package(".", vignettes = FALSE)

# English prose that is not covered by the preceding call.
spelling::spell_check_files(
  c("README.md", "vignettes/getting-started.Rmd"),
  ignore = wordlist,
  lang = "en_US"
)
```

Check the Spanish guide separately when the `es_ES` Hunspell dictionary is
installed:

```r
if ("es_ES" %in% hunspell::list_dictionaries()) {
  spelling::spell_check_files(
    "vignettes/bigbang-es.Rmd",
    ignore = c(
      spelling::get_wordlist("."),
      readLines("tools/WORDLIST-es", encoding = "UTF-8")
    ),
    lang = "es_ES"
  )
}
```

The split is deliberate: adding every Spanish word to the English WORDLIST
would hide real spelling errors in the English documentation.
