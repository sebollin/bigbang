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
#>   Path: /tmp/RtmpXH7w1B/bigbang-vignette-1bde37bdf46a/generated/toyverse
#>   Components: toycomponent
list.files(result$path)
#>  [1] "DESCRIPTION"    "inst"           "LICENSE"        "man"           
#>  [5] "NAMESPACE"      "po"             "R"              "README.md"     
#>  [9] "tests"          "toyverse.Rproj" "vignettes"
```

The generated tree can be scanned without loading it:

``` r

scan <- scan_bigbang_artifact(result$path)
scan
#> <bigbang artifact scan>
#>   Path: /tmp/RtmpXH7w1B/bigbang-vignette-1bde37bdf46a/generated/toyverse
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
toyverse_install()
```

Those four lines are also the whole procedure for someone else. The
component archives were copied into the generated package, so
`toyverse_0.1.0.tar.gz` is the only file that has to travel: whoever
receives it installs it and calls `toyverse_install()`, with no archive
directory beside it and no path agreed on in advance. The default
`pkg_dir` is `system.file("archives", package = "toyverse")`, which is
resolved when the installer runs and therefore points at the library it
was installed into.

Generate with `include_archives = FALSE` when the archives should stay
in a location every recipient can already reach; then
`toyverse_install()` requires an explicit `pkg_dir`.

`cran_deps = "skip"` is the offline default. Choose `"error"` for strict
offline validation or `"install"` only when an explicit repository is
available. Use `upgrade = "always"` or `force = TRUE` for an explicit
reinstall. Generated metapackages provide `<meta>_conflicts()` and honor
`options(<meta>.quiet = TRUE)` for startup output. Their optional `cli`
display falls back to a dependency-free ASCII banner.
