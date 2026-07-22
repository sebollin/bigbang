# Contributing to bigbang

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
