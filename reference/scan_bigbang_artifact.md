# Scan a generated metapackage for historical deletion signatures

Inspects a generated metapackage source directory, source archive, or
installed package without loading it. The scanner looks for the
historical V1, V2, V3, and V7 deletion signatures from the pre-release
security investigation. Provenance is read from both the current
generator fields and the legacy pre-rename fields so development
artifacts remain classifiable.

## Usage

``` r
scan_bigbang_artifact(path, dry_run = TRUE)
```

## Arguments

- path:

  Character scalar. Source directory, .tar.gz/.tar/.zip source archive,
  or installed package directory.

- dry_run:

  Logical. Must be TRUE, the default. Automatic mutation or remediation
  is deliberately not implemented.

## Value

A list with the artifact type, vulnerability flag, detected signatures,
evidence locations, provenance fields, and R version used for the scan.

## Details

Installed packages are inspected through R's internal lazy-load database
API. That code is isolated in .scan_installed_lazydb() and has been
exercised with R 4.6.1. Because this is an internal R format, callers
should re-run the scanner tests when adopting a new R minor release.

## Examples

``` r
archives <- system.file("extdata", package = "bigbang")
destination <- tempfile("bigbang-scan-example-")
dir.create(destination)

result <- create_metapackage(
  name = "toyverse",
  packages = "toycomponent_0.1.0",
  pkg_dir = archives,
  dest_dir = destination,
  document = FALSE,
  verbose = FALSE,
  import_deps = character(),
  force_deps = character()
)
scan_bigbang_artifact(result$path)
#> <bigbang artifact scan>
#>   Path: /tmp/RtmpO38oPF/bigbang-scan-example-19a933ee1ee4/toyverse
#>   Type: source
#>   Result: no deletion signatures found

unlink(destination, recursive = TRUE)
```
