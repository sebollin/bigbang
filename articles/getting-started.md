# Getting started with bigbang

## A complete disposable example

This guide creates a component archive and a metapackage source tree
entirely under [`tempdir()`](https://rdrr.io/r/base/tempfile.html). It
does not write to the working directory, install anything, or contact a
repository.

``` r

root <- tempfile("bigbang-vignette-")
source_root <- file.path(root, "sources")
archive_root <- file.path(root, "archives")
destination <- file.path(root, "generated")
dir.create(file.path(source_root, "toycomponent", "R"), recursive = TRUE)
dir.create(archive_root)
dir.create(destination)

writeLines(c(
  "Package: toycomponent",
  "Type: Package",
  "Title: Toy Component",
  "Version: 0.1.0",
  "Authors@R: person('Test', 'Author', email='test@example.org', role=c('aut','cre'))",
  "Description: A disposable component used by the bigbang vignette.",
  "License: MIT"
), file.path(source_root, "toycomponent", "DESCRIPTION"), useBytes = TRUE)
writeLines(
  "export(toy_value)",
  file.path(source_root, "toycomponent", "NAMESPACE"),
  useBytes = TRUE
)
writeLines(
  "toy_value <- function() 'hello from the component'",
  file.path(source_root, "toycomponent", "R", "toy.R"),
  useBytes = TRUE
)

withr::with_dir(source_root, utils::tar(
  file.path(archive_root, "toycomponent_0.1.0.tar.gz"),
  files = "toycomponent", compression = "gzip"
))
```

The archive stem includes the version; `ext` is supplied separately.

``` r

result <- create_metapackage(
  name = "toyverse",
  packages = "toycomponent_0.1.0",
  pkg_dir = archive_root,
  dest_dir = destination,
  document = FALSE,
  verbose = FALSE,
  import_deps = character(),
  force_deps = character()
)
result
#> <bigbang metapackage>
#>   Package: toyverse
#>   Path: /tmp/RtmphOzBPC/bigbang-vignette-1c7f2b985973/generated/toyverse
#>   Components: toycomponent
list.files(result$path)
#> [1] "DESCRIPTION"    "inst"           "LICENSE"        "man"           
#> [5] "NAMESPACE"      "po"             "R"              "toyverse.Rproj"
#> [9] "vignettes"
```

The generated tree can be scanned without loading it:

``` r

scan <- scan_bigbang_artifact(result$path)
scan
#> <bigbang artifact scan>
#>   Path: /tmp/RtmphOzBPC/bigbang-vignette-1c7f2b985973/generated/toyverse
#>   Type: source
#>   Result: no deletion signatures found
stopifnot(!scan$vulnerable)
```

## Build, install, and use

In a real project, build and install the generated package using the
standard R workflow. Loading the metapackage never installs components:

``` r

system2(file.path(R.home("bin"), "R"), c("CMD", "build", result$path))
install.packages("toyverse_0.1.0.tar.gz", repos = NULL, type = "source")
library(toyverse)
toyverse_install(cran_deps = "skip")
```

`cran_deps = "skip"` is the offline default. Choose `"error"` for strict
offline validation or `"install"` only when an explicit repository is
available.
