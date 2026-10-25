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

## Archive resource limits

Archive walks use [`options()`](https://rdrr.io/r/base/options.html)
values named `bigbang.archive.max_entries` (default 100,000),
`bigbang.archive.max_total_bytes` (default 2 GiB),
`bigbang.archive.max_metadata_bytes` (default 64 MiB), and
`bigbang.archive.max_path_bytes` (default 4,096 bytes). Limits apply
during both inspection and extraction; the byte limit includes archive
metadata and data discarded during inspection. Metadata includes ZIP
local and central records, data descriptors, the EOCD comment, every TAR
header, and PAX/GNU metadata payload. Implicit directories and PAX/GNU
metadata headers count toward `bigbang.archive.max_entries`. The default
metadata cap leaves room for the documented maximum of 65,534 entries in
a classic ZIP (ZIP64 is not supported). Entry names are compared without
locale dependence, but they are not Unicode-normalized: NFC and NFD
spellings remain distinct; NFD names are accepted because macOS can
produce them.

The `bigbang.archive.portable_names` option defaults to `TRUE` and
applies the strict Windows-compatible name policy. Set it to `FALSE`
only when all recipients use Linux or macOS. Then Windows device names,
Windows-invalid characters, and trailing dots or spaces may be accepted
on non-Windows systems, while case-only collisions are accepted only
after a real probe confirms that the extraction filesystem distinguishes
case; an inspection without an extraction destination defers that
decision until extraction. Components made only of dots and spaces are
always rejected. Links, special entries, absolute or parent-traversal
paths, empty components, backslashes, control characters, invalid UTF-8,
and size limits are never relaxed. Windows remains strict regardless of
the option. The result reports accepted relaxed names in
`portable_name_warnings`.

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
#>   Path: /tmp/RtmpYP33am/bigbang-scan-example-1af37d66ae21/toyverse
#>   Type: source
#>   Result: no deletion signatures found

unlink(destination, recursive = TRUE)
```
