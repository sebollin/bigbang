# bigbang 0.1.0

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
