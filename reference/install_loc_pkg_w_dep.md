# Deprecated Spanish alias for `install_local_pkg()`

Deprecated Spanish alias for
[`install_local_pkg()`](https://sebollin.github.io/bigbang/reference/install_local_pkg.md)

## Usage

``` r
install_loc_pkg_w_dep(
  nombre_paquete,
  ruta_instalables,
  ext = ".tar.gz",
  repos = getOption("repos"),
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive())
)
```

## Arguments

- nombre_paquete:

  Character package archive stem.

- ruta_instalables:

  Directory containing local archives.

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

The result of
[`install_local_pkg()`](https://sebollin.github.io/bigbang/reference/install_local_pkg.md).

## Installation

This function installs packages into the user's active R library.
Installation occurs only when the user calls the function; loading
`bigbang` never installs packages. With the default
`cran_deps = "skip"`, it does not access the network.
