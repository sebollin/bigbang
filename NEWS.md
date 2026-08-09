# bigbang (development version)

- Removed the deprecated Spanish aliases `crear_meta_paquete_local()`,
  `diagnosticar_dependencias()` and `install_loc_pkg_w_dep()`. They existed to
  ease a transition inside the organisation the package grew in, before it was
  published. Use `create_metapackage()`, `diagnose_dependencies()` and
  `install_local_pkg()`.

- Generated meta-packages are now self-contained. `create_metapackage()` gained
  `include_archives`, `TRUE` by default, which copies the component archives
  into `inst/archives/` of the generated meta-package. The generated
  `<meta>_install()` then defaults `pkg_dir` to
  `system.file("archives", package = "<meta>")`, resolved when the installer is
  called, so the meta-package is the only artifact that has to be distributed
  and its components install with no arguments on any machine, with no path
  agreed on beforehand. Network access remains necessary only for components
  that depend on a package coming from a repository. Pass
  `include_archives = FALSE` to keep the previous behaviour, where the archives
  live in a location the recipient can reach and `pkg_dir` is required.
- The build ignore rules of a generated meta-package no longer discard archives
  below the project root, so shipped components reach the tarball while
  archives left at the root are still excluded.

- Generated metapackages now provide optional `cli` startup formatting, a
  package-specific quiet option, conflict reporting, explicit reinstall and
  upgrade policies, and optional ordered workflow vignettes.
- Component archive directories are now required by generated installers, and
  generated metadata includes a consistency test for its component list.
- Generation no longer changes the process working directory; every scaffold
  writer receives an explicit absolute path.
- Fixed generation with relative `dest_dir` and `pkg_dir` paths, and roll back
  incomplete project trees created by a failed invocation.
- `create_metapackage()` now validates `name` against the R package name
  grammar before writing anything. A name carrying a path separator or a parent
  reference previously placed the generated tree outside `dest_dir` and still
  reported success.
- Components that were left untouched by the installation policy are now
  reported in the new `unchanged` element of the install result instead of
  `installed`.
- Restored the caller's attached packages and loaded namespaces after automatic
  documentation, and report documentation success accurately.
- Fixed eleven Spanish runtime messages whose catalog keys had leading or
  trailing whitespace.
- Documented related metapackage projects and design precedents.
- Fixed generated helper examples so they use the requested metapackage name
  instead of an internal historical name.

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
