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
  pkg_dir = NULL,
  ext = ".tar.gz",
  repos = getOption("repos"),
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive()),
  force = FALSE,
  upgrade = c("newer", "always", "never")
)
```

## Arguments

- package:

  Character. An existing archive path, or a package stem such as
  `"uspr_0.8.5"` to resolve in `pkg_dir`. An existing file is always
  treated as a path; only a non-existing element is resolved as a stem.

- pkg_dir:

  Character. Optional directory or directories containing local
  archives. It is not needed when `package` is an existing path.

- ext:

  Character. Fallback archive extension for stems; existing paths keep
  their own extension.

- repos:

  Character. Repositories used only when `cran_deps = "install"`.

- cran_deps:

  Character. Policy for missing non-local dependencies: `"skip"` (the
  default) never accesses the network, `"error"` fails without accessing
  it, and `"install"` attempts installation from `repos`.

- verbose:

  Logical. Whether to emit progress and summary messages. The default
  follows `getOption("bigbang.verbose", interactive())`.

- force:

  Logical. Reinstall every local archive. This is a convenience alias
  for `upgrade = "always"`.

- upgrade:

  Character. Installed-version policy: `"newer"` installs only when the
  archive is newer, `"always"` reinstalls, and `"never"` keeps any
  installed version. Explicitly combining `force = TRUE` with a value
  other than `"always"` is an error.

## Value

Invisibly, a list describing installed, unchanged, failed, and skipped
packages. Components that an upgrade policy left in place are reported
in `unchanged`, not in `installed`.

## Installation

This function installs packages into the user's active R library.
Installation occurs only when the user calls the function; loading
`bigbang` never installs packages. With the default
`cran_deps = "skip"`, it does not access the network. An installed
package can be kept without reading its archive when
`upgrade = "never"`. Under the default policy, bigbang reads only the
archive `DESCRIPTION` first; if that metadata cannot be verified for an
already installed package, the installed package is kept and the reason
is reported. With `upgrade = "never"`, the shortcut takes the component
identity from the archive filename because the archive is not read. Use
`upgrade = "newer"` when the declared `Package` field must be checked
against the installed package.

## See also

[`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)
for generating a meta-package with an explicit component installer.
