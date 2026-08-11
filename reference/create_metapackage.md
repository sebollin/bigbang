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
  pkg_dir = NULL,
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
  debug = FALSE,
  workflow = NULL,
  include_archives = TRUE
)
```

## Arguments

- name:

  Character. Name of the meta-package to create (must not contain
  underscores `_`).

- packages:

  Character vector. Archive paths or stems of the local packages to
  include. Existing paths may come from different directories; stems are
  resolved in `pkg_dir`, e.g. `"myPackage_1.0.0"`.

- pkg_dir:

  Character. Optional directory or directories containing local archives
  used to resolve stems. It is not needed when every `packages` element
  is an existing archive path.

- ext:

  Character. Fallback archive extension for stems. Defaults to
  `".tar.gz"`; each existing archive path keeps its own extension.

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
  declared by components. Source-code guesses are diagnostic by default;
  use this argument when a guessed dependency should bind in the
  generated package.

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

- workflow:

  Optional named character vector mapping ordered stage labels to
  component package names. When supplied, every component must appear
  once and a pipeline vignette skeleton is generated.

- include_archives:

  Logical. If `TRUE`, the default, the component archives are copied
  into `inst/archives/` of the generated meta-package, so that the
  meta-package is the only artifact that has to be distributed and
  `<meta>_install()` works with no arguments, without any path being
  agreed on beforehand. Components still install only where they can: a
  Windows binary archive is refused on other platforms. Shipping the
  archives also means redistributing them, so their licenses have to
  allow it, and it makes the generated tarball as large as its
  components: CRAN prefers source tarballs under 10 MB and does not
  accept binary executables in them, which matters only if a generated
  meta-package is ever submitted there. Set it to `FALSE` when the
  archives stay in a shared location that recipients can reach; then
  `<meta>_install()` requires an explicit `pkg_dir`.

## Value

Invisibly, a `bigbang_result` containing the generated path, component
archives, dependency classification, and documentation status.

## Details

The function performs the following steps:

1.  Creates the basic R package structure (`R`, `man`, `vignettes`,
    etc.).

2.  Detects dependencies between packages, both explicit (from
    DESCRIPTION) and possible implicit uses (found by scanning
    executable source tokens). The latter are reported for diagnosis and
    are not hard dependencies unless explicitly supplied through
    `additional_deps` or `force_deps`.

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

Generation validates every supplied component and its dependency graph
eagerly before writing the metapackage. This hard validation protects an
artifact that will be distributed to another machine. The installer is
more tolerant: when an already installed component does not need to be
changed, it can retain that installation without reading an archive that
will not be used.

## Component installation

The generated meta-package installs component packages only when the
user explicitly calls `<meta>_install()`. Loading it with
[`library()`](https://rdrr.io/r/base/library.html) never installs
packages. By default, the generated installer does not access a
repository.

With `include_archives = TRUE`, the default, the component archives
travel inside the generated meta-package and `pkg_dir` defaults to
`system.file("archives", package = "<meta>")`. That default is resolved
when the installer is called, so it points at the library of whoever
installed the meta-package: recipients need nothing beyond the
meta-package itself, and no path has to be agreed on between machines.
Network access is needed only when a component depends on a package that
must come from a repository, which happens exclusively under
`cran_deps = "install"`.

If `reexport = TRUE`, a `reexports.R` file is generated so users can
reach the component functions directly through the meta-package
(`meta::fun()` instead of `component::fun()`), tidyverse style.

## Requirements

- Each component must be an existing archive path or a stem resolvable
  in one of the optional `pkg_dir` directories; `ext` is only a fallback
  for stems.

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
#>  [1] "DESCRIPTION"    "LICENSE"        "NAMESPACE"      "R"             
#>  [5] "README.md"      "inst"           "man"            "po"            
#>  [9] "tests"          "toyverse.Rproj" "vignettes"     

unlink(destination, recursive = TRUE)
```
