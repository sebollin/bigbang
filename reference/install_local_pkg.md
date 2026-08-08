# Install a local package together with its dependencies

Installs a package from a local archive. Dependencies available as local
archives are installed recursively; missing non-local dependencies
follow the explicit `cran_deps` policy. ZIP archives containing
`Meta/package.rds` are treated as Windows binaries, while other ZIP
archives are unpacked and installed as source packages.

## Usage

``` r
install_local_pkg(
  package,
  pkg_dir,
  ext = ".tar.gz",
  repos = getOption("repos"),
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive())
)
```

## Arguments

- package:

  Character. Package file name without extension (for example,
  `"uspr_0.8.5"`).

- pkg_dir:

  Character. Directory containing local archives.

- ext:

  Character. Archive extension.

- repos:

  Character. Repositories used only when `cran_deps = "install"`.

- cran_deps:

  Character. Policy for missing non-local dependencies: `"skip"` (the
  default) never accesses the network, `"error"` fails without accessing
  it, and `"install"` attempts installation from `repos`.

- verbose:

  Logical. Whether to emit progress and summary messages. The default
  follows `getOption("bigbang.verbose", interactive())`.

## Value

Invisibly, a list describing installed, failed, and skipped packages.

## Installation

This function installs packages into the user's active R library.
Installation occurs only when the user calls the function; loading
`bigbang` never installs packages. With the default
`cran_deps = "skip"`, it does not access the network.

## See also

[`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)
for generating a meta-package with an explicit component installer.
