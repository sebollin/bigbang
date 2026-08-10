# bigbang 0.2.0

Released on 2026-08-09.

## Breaking changes

- The deprecated Spanish aliases `crear_meta_paquete_local()`,
  `diagnosticar_dependencias()` and `install_loc_pkg_w_dep()` were removed. They
  existed to ease a rename inside the organisation the package grew in, while it
  was still unpublished. Use `create_metapackage()`, `diagnose_dependencies()`
  and `install_local_pkg()`.
- In a generated meta-package built with `include_archives = FALSE`,
  `<meta>_install()` requires `pkg_dir`. In 0.1.0 that argument defaulted to the
  absolute archive path of the machine where the meta-package had been
  generated, which exists nowhere else. With the new default,
  `include_archives = TRUE`, the argument is optional again because the archives
  travel inside the package.
- Components that the installation policy left untouched are reported in a new
  `unchanged` element of the installation result instead of appearing in
  `installed` labelled "Already installed". Reporting a package as installed
  when nothing was installed was misleading.
- Non-local dependencies are installed with `dependencies = NA` instead of
  `TRUE`, so their `Suggests` are no longer installed. Everything a component
  needs in order to run is still installed; what is no longer pulled in is the
  tooling those dependencies use for their own examples, tests and vignettes.
- The arguments added in this release come last in every signature, so a
  positional call written against 0.1.0 keeps binding to the same parameters.
  Positional calls are still a fragile way to call these functions: name the
  arguments.

## Generated meta-packages are self-contained

- `create_metapackage()` gained `include_archives`, `TRUE` by default, which
  copies the component archives into `inst/archives/` of the generated
  meta-package. Its installer then defaults `pkg_dir` to
  `system.file("archives", package = "<meta>")`, resolved when the installer is
  called, so the meta-package is the only artifact that has to be distributed
  and its components install with no arguments, with no path agreed on
  beforehand. Network access remains necessary only for components that depend
  on a package coming from a repository. Pass `include_archives = FALSE` to keep
  the archives in a shared location the recipient can reach.
- The build ignore rules of a generated meta-package exempt `inst/archives/`, so
  shipped components reach the tarball while every other archive anywhere in the
  tree is still excluded.

## New features

- Generated meta-packages provide an optional `cli` two-column startup message
  that falls back to the ASCII banner when `cli` is absent, a
  `<meta>.quiet` option, `<meta>_conflicts()` for masking conflicts, explicit
  reinstall and upgrade policies (`force`, and
  `upgrade = "newer" | "always" | "never"`), and an optional ordered workflow
  vignette through the `workflow` argument.
- Generated metadata records the component list in `Config/bigbang/packages`,
  and the generated package tests that the list agrees with what it exports.

## Bug fixes

- Generation no longer changes the process working directory; every scaffold
  writer receives an explicit absolute path.
- Fixed generation with relative `dest_dir` and `pkg_dir` paths, and roll back
  incomplete project trees created by a failed invocation.
- `create_metapackage()` validates `name` before writing anything: against the R
  package name grammar, and against the names R itself ships. A name carrying a
  path separator or a parent reference previously placed the generated tree
  outside `dest_dir` and still reported success.
- Non-local dependencies are installed with `dependencies = NA`, which covers
  Depends, Imports and LinkingTo, instead of `TRUE`, which also covered
  Suggests. Asking for one small dependency used to pull development tooling and
  its whole tree into the library.
- Restored the caller's attached packages and loaded namespaces after automatic
  documentation, and report documentation success accurately.
- Fixed eleven Spanish runtime messages whose catalog keys had leading or
  trailing whitespace.
- Generated helper examples use the requested meta-package name instead of an
  internal historical name.
- When a component is skipped because a non-local dependency is missing, the
  warning names the call that resolves it and the attachment step no longer
  follows it with a vaguer hint suggesting a bare re-run, which would have
  skipped the component again for the same reason.

## Documentation

- The maintainer's given name is spelled Sebastián, and the author entry now
  carries an ORCID identifier.

- Documented that distributing the component archives inside a meta-package is a
  redistribution, so their licenses have to allow it, and that a component still
  installs only where its format can.
- Documented related meta-package projects and design precedents.
- Added the r-universe version badge and the one-line install from the universe
  binaries.

# bigbang 0.1.0

First release on CRAN, accepted on 2026-08-08.

**bigbang — create custom R metapackages from local packages**

## Data-safety architecture

- Removed all generated startup installation and cwd-relative cleanup. Generated
  `.onLoad()` hooks are side-effect free; component installation is explicit via
  `<meta>_install()`.
- Added destructive regression tests in disposable directories, including real
  source installation, attachment, data-only components, names beginning with
  `tmp`, and content hashes for decoy directories.
- Added `scan_bigbang_artifact()` to identify the historical V1/V2/V3/V7
  signatures in source trees, archives, and installed packages without loading
  them.
- Generated projects must use a new or empty destination, preventing unsafe
  in-place regeneration of unclassified historical sources.

## Installation and portability

- Component archives are installed once in topological dependency order; cycles
  raise a typed `bigbang_error_cycle` condition.
- Added explicit offline dependency policies (`skip`, `error`, `install`) and
  content-based distinction between source ZIPs and Windows binary ZIPs.
- All generated text is written as UTF-8, R literals and paths are emitted safely,
  and CI covers Linux, Windows, and macOS configurations.

## API, language, and documentation

- Added the English snake_case API: `create_metapackage()`,
  `install_local_pkg()`, and `diagnose_dependencies()`. Spanish aliases remain
  available as deprecated transition wrappers.
- Added typed results and conditions with print methods.
- English is the source language. Spanish runtime translations are supplied
  through gettext for both `bigbang` and generated metapackages.
- Added English and Spanish guides, release documentation, and a pkgdown
  configuration.
