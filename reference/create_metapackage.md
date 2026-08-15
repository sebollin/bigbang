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
  include_archives = TRUE,
  tolerate = character(),
  dry_run = FALSE,
  on_component_error = c("abort", "skip"),
  update = FALSE,
  install_upgrade = c("newer", "always", "never")
)
```

## Arguments

- name:

  Character. Name of the meta-package to create (must not contain
  underscores `_`).

- packages:

  Character vector. Archive paths or stems of the local packages to
  include. An existing file is always used as a path; otherwise the
  element is resolved as a stem in `pkg_dir`, e.g. `"myPackage_1.0.0"`.
  A bare package name such as `"myPackage"` resolves when exactly one
  archive in those directories declares that `Package` identity. Zero
  matches use the usual unresolved-archive error; multiple matches are
  an ambiguity error. Supported archives that cannot be read during this
  identity search are excluded with a warning that names the archive.
  Existing paths may come from different directories. A single existing
  text file without a recognized archive extension is treated as a
  manifest, with one component per line; relative paths in that file are
  resolved relative to the manifest directory, absolute paths and `~`
  paths are used as written, and bare archive filenames may also be
  found in `pkg_dir`.

- pkg_dir:

  Character. Optional directory or directories containing local archives
  used to resolve stems and bare package names. It is not needed when
  every `packages` element is an existing archive path.

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

  Logical flag retained in its original position for positional-call
  compatibility. The default `FALSE` attaches installed components as
  usual. With `TRUE`, explicit exports read from each component's
  NAMESPACE are exposed through read-only active bindings. Components
  are never added to `Imports` or `Depends`, so the generated package
  still installs offline without them. Before installation, reading a
  binding returns a placeholder function whose clear missing-component
  error appears only when that function is called. For non-function
  exports, access therefore returns the placeholder instead of the
  object until the component is installed. The same binding then works
  without reloading the metapackage. Only explicit `export()` directives
  become bindings. S4 classes and methods remain available by loading
  their component package. Non-syntactic explicit export names are
  quoted in the generated NAMESPACE. An object restored with
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html) does not load a
  component by itself, so base R cannot dispatch that component's S3
  method until it is loaded.

- document:

  Logical. If `TRUE`, runs
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  automatically. Defaults to `TRUE`. The planned `man/<name>_*.Rd` and
  internal-helper Rd filenames are reserved for generated documentation;
  custom Rd files should use different names. A successful documentation
  run may adopt a reserved filename into the generation manifest, after
  which a later update with `document = FALSE` removes it as generated
  output.

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

- tolerate:

  Character vector of explicitly named validation relaxations. Use
  `"filename_mismatch"` to silence filename-versus-DESCRIPTION mismatch
  warnings, or `"unincluded_local_dep"` to turn an
  available-but-unincluded local dependency error into a warning. With
  the latter relaxation, the generated metapackage does not ship that
  dependency: the recipient must provide it through `pkg_dir` or a
  repository with `cran_deps = "install"`. Unknown names are errors.
  Each applied relaxation is recorded in the returned `tolerated` table.

- dry_run:

  Logical. If TRUE, resolves and validates components and returns the
  planned generation without creating dest_dir or writing a project.

- on_component_error:

  Character policy for component-level failures: "abort" (default) stops
  generation, while "skip" omits the failed component and transitively
  omits components that depend on it. When a failed archive still
  exposes its DESCRIPTION, propagation uses its declared `Package`;
  otherwise the filename-derived name is used and the limitation is
  reported. If that fallback name differs from `Package`, a dependent
  may fail on the recipient. During an update, omitted inputs never
  authorize deletion of a previously shipped archive. When the old
  component cannot be identified unambiguously, archive reconciliation
  is deferred until a clean update rather than risking the only
  surviving copy.

- update:

  Logical. If TRUE, update a previously generated project only when its
  bigbang manifest is present and all generated files are unchanged.
  Files outside that manifest are never touched. Updates are refused
  when the generated project root, a manifest file, or any path
  component inside the project is a symbolic link, so writes cannot
  escape the project tree. Generated files no longer in the plan are
  reported in `removed_files`. Removing a component also removes its
  shipped archive, which may be the last available copy. The same field
  includes partial documentation outputs created and cleaned up after a
  failed documentation run. A dry run cannot predict those
  failure-dependent cleanups and reports only planned removals. Before
  changing the project, an update backs up every generated file and its
  manifest. A failed update restores that state so the same update can
  be retried. Documentation files requested by `document = TRUE` can
  always be regenerated: they can be restored after documentation was
  disabled, and an update that cannot regenerate them retains the
  previously tracked Rd files. See `document` for the reserved
  generated-documentation filenames.

- install_upgrade:

  Character default upgrade policy emitted in the generated installer
  function: "newer", "always", or "never". This controls whether a
  generated installer keeps newer installed versions, reinstalls every
  component, or skips archive inspection.

## Value

Invisibly, a `bigbang_result` containing the generated path, component
archives, dependency classification, applied tolerations, files removed
by the call, and documentation status.

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

## Validation strictness

During generation, validations that protect the recipient cannot be
disabled: malformed or unsafe archives, invalid component metadata,
duplicate components, cycles, and unsatisfied local version constraints
remain hard errors. Checks about project tidiness can be relaxed
individually through `tolerate`; there is no switch that disables
validation as a whole. bigbang does not run `R CMD check` on component
packages, so component warnings and notes do not prevent generation.
Component source directories are built in a temporary directory with the
optional pkgbuild package; passing an already built archive avoids that
optional dependency.

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

Loading the generated meta-package attaches installed components, so
their exported functions can be called directly or through
`component::function()`. With `reexport = TRUE`, explicit component
exports are instead exposed through read-only active bindings in the
meta-package namespace. This does not add components to `Imports` or
`Depends`: loading remains possible without them, and a binding resolves
the component on every access. Only explicit `export()` directives are
rebound; S4 classes and methods are used through the loaded component
namespace. An object restored with
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) cannot load a
component by itself, so base R cannot dispatch that component's S3
method until the component has been loaded.

## Requirements

- Each component must be an existing archive path or a stem resolvable
  in one of the optional `pkg_dir` directories; `ext` is only a fallback
  for stems.

- Files in the supplied archive directories that cannot be read are
  excluded from the inventory with a warning. A requested component
  still fails validation, while an unreadable file matching a declared
  dependency is reported as an unavailable local archive.

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
