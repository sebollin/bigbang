# Build a local meta-package

Creates the full structure and files of a meta-package that installs,
manages and loads a set of locally stored R packages, resolving the
dependencies between them with a graph-based (topologically ordered)
approach.

## Usage

``` r
create_metapackage(
  name,
  packages,
  pkg_dir,
  ext = ".tar.gz",
  version = "0.1.0",
  dest_dir,
  reexport = FALSE,
  document = TRUE,
  verbose = getOption("bigbang.verbose", interactive()),
  authors =
    "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
  description = "Local Package Metapackage",
  license = "MIT + file LICENSE",
  additional_deps = NULL,
  ignore_deps = NULL,
  import_deps = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts",
    "zoo"),
  force_deps = NULL,
  debug = FALSE
)
```

## Arguments

- name:

  Character. Name of the meta-package to create (must not contain
  underscores `_`).

- packages:

  Character vector. Names (with version) of the local packages to
  include, e.g. `"myPackage_1.0.0"`.

- pkg_dir:

  Character. Directory containing the local archive files (`.tar.gz`,
  `.zip`, etc.).

- ext:

  Character. Archive extension. Defaults to `".tar.gz"`.

- version:

  Character. Version of the meta-package. Defaults to `"0.1.0"`.

- dest_dir:

  Character. Required destination directory. The function writes the
  generated meta-package exclusively inside this directory; there is no
  default path. Use [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  for disposable output.

- reexport:

  Logical. If `TRUE`, re-exports the component packages' functions so
  they are reachable directly through the meta-package (tidyverse
  style). Defaults to `FALSE`.

- document:

  Logical. If `TRUE`, runs
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  automatically. Defaults to `TRUE`.

- verbose:

  Logical. If `TRUE`, shows verbose messages. The default follows
  `getOption("bigbang.verbose", interactive())`.

- authors:

  Character. Content for the `Authors@R` field of DESCRIPTION.

- description:

  Character. Description of the meta-package.

- license:

  Character. License of the meta-package.

- additional_deps:

  Character vector. Extra dependencies to add on top of the ones
  detected automatically.

- ignore_deps:

  Character vector. Dependencies to ignore even if detected.

- import_deps:

  Character vector. Packages that should go in the `Imports` field of
  DESCRIPTION rather than `Depends`. Imports are not attached when the
  user calls [`library()`](https://rdrr.io/r/base/library.html) on the
  meta-package, but remain available via `::` (e.g. `dplyr::filter()`),
  reducing name clashes in the user's workspace.

- force_deps:

  Character vector. Exact package names to use as dependencies,
  bypassing automatic detection. If supplied, only these are used as the
  meta-package's implicit dependencies.

- debug:

  Logical. If `TRUE`, emits detailed debugging messages. Defaults to
  `FALSE`.

## Value

Invisibly, a `bigbang_result` containing the generated path, component
archives, dependency classification, and documentation status.

## Details

The function performs the following steps:

1.  Creates the basic R package structure (`R`, `man`, `vignettes`,
    etc.).

2.  Detects dependencies between packages, both explicit (from
    DESCRIPTION) and implicit (found by scanning the source code).

3.  Generates DESCRIPTION and NAMESPACE with the appropriate
    dependencies.

4.  Creates a basic vignette documenting the meta-package.

5.  Generates R files with functions to install and load the component
    packages:

    - `<name>_install()`: installs the component packages from the local
      archives.

    - `<name>_attach()`: attaches the components that are already
      installed.

    - `<name>_detach()`: detaches all the meta-package's components.

    - `<name>_packages()`: lists the included packages.

Installation is **explicit**: calling `library(<meta>)` attaches the
components that are already installed and reports which ones are
missing, but does not install anything or delete any files. To install
the components from the local archives, the user calls
`<meta>_install()`. Installation resolves dependencies with a
graph-based topological ordering that also detects circular
dependencies.

## Component installation

The generated meta-package installs component packages only when the
user explicitly calls `<meta>_install()`. Loading it with
[`library()`](https://rdrr.io/r/base/library.html) never installs
packages. By default, the generated installer does not access a
repository.

If `reexport = TRUE`, a `reexports.R` file is generated so users can
reach the component functions directly through the meta-package
(`meta::fun()` instead of `component::fun()`), tidyverse style.

## Requirements

- The local packages must exist in `pkg_dir` with the given extension.

- Automatic documentation (`document = TRUE`) requires the `devtools`
  package.

## Examples

``` r
archives <- system.file("extdata", package = "bigbang")
destination <- tempfile("bigbang-example-")
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
list.files(result$path)
#> [1] "DESCRIPTION"    "LICENSE"        "NAMESPACE"      "R"             
#> [5] "inst"           "man"            "po"             "toyverse.Rproj"
#> [9] "vignettes"     

unlink(destination, recursive = TRUE)
```
