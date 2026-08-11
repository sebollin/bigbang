# Changelog

## bigbang (development version)

### New capabilities

- The generator supports read-only dry-run reports, explicit
  component-error policies, package source directories, component
  manifests, and guarded update regeneration through a generation
  manifest.
- Generated installers accept component subsets and an explicit
  installation library, and the generator can set their default upgrade
  policy.
- Source-directory components must use include_archives = TRUE because
  their temporary build archive cannot be reused as an external
  installation source.

### Component input and resolution

- Component archives may be supplied as existing paths from multiple
  directories, with mixed `.tar.gz`, `.tar`, and `.zip` extensions.
  Stems remain supported through the optional `pkg_dir` fallback.
- Package identity and version now come from the archive `DESCRIPTION`.
  Filename mismatches are warned about, while empty metadata, duplicate
  components, invalid archives, cycles, and unsatisfied local
  constraints remain hard errors.
- Generated installers accept a vector of archive directories when
  archives are not shipped; archives shipped inside a generated
  meta-package remain portable and contain no source-machine paths.
- [`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)
  gains a final `tolerate` argument for explicitly named relaxations.
  Filename mismatches can be silenced, and an available local dependency
  omitted from `packages` can be downgraded from an error to a warning;
  unknown relaxation names are errors.
- Generation results now include a `tolerated` table identifying every
  applied relaxation, affected component, and reason.
- The validation documentation now distinguishes non-negotiable
  recipient protections from optional project-tidiness checks, and
  states explicitly that bigbang does not run `R CMD check` on component
  packages.

### Bug fixes

- Component manifests now resolve bare archive filenames from the
  supplied `pkg_dir` directories when the files are not beside the
  manifest, and missing entries report every directory that was
  searched.
- Installation messages now name both the declared package identity and
  the archive stem when those differ.
- Installing an already present package no longer requires reading an
  archive when `upgrade = "never"`; under the default policy, only its
  DESCRIPTION is read before deciding whether the archive needs to be
  used. An unreadable archive is reported as unchanged when the
  installed package can be retained.
- Rollback paths after a post-scaffold generation error are covered,
  including destinations reached through a symbolic link and
  unsuccessful removal.
- Lazy archive-version inspection now rejects extracted symbolic links
  before reading metadata, matching the full archive validation path.
- The installation help now explains that `upgrade = "never"` identifies
  a component from its filename because the archive is intentionally not
  read.
- Component archives whose extensions use upper-case letters are copied
  under canonical names so generated installers find them on
  case-sensitive systems; archive names that collide only by case are
  rejected.
- Unreadable unrelated archives in a supplied source directory are
  excluded from the inventory with a warning, including a specific
  diagnostic when a declared dependency appears to match one.
- Vector resolution failures retain the requested component names in the
  installation result, and generated installers report the package
  identity declared by DESCRIPTION after installation.

## bigbang 0.2.0

Released on 2026-08-09.

### Breaking changes

- The deprecated Spanish aliases `crear_meta_paquete_local()`,
  `diagnosticar_dependencias()` and `install_loc_pkg_w_dep()` were
  removed. They existed to ease a rename inside the organisation the
  package grew in, while it was still unpublished. Use
  [`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md),
  [`diagnose_dependencies()`](https://sebollin.github.io/bigbang/reference/diagnose_dependencies.md)
  and
  [`install_local_pkg()`](https://sebollin.github.io/bigbang/reference/install_local_pkg.md).
- In a generated meta-package built with `include_archives = FALSE`,
  `<meta>_install()` requires `pkg_dir`. In 0.1.0 that argument
  defaulted to the absolute archive path of the machine where the
  meta-package had been generated, which exists nowhere else. With the
  new default, `include_archives = TRUE`, the argument is optional again
  because the archives travel inside the package.
- Components that the installation policy left untouched are reported in
  a new `unchanged` element of the installation result instead of
  appearing in `installed` labelled “Already installed”. Reporting a
  package as installed when nothing was installed was misleading.
- Non-local dependencies are installed with `dependencies = NA` instead
  of `TRUE`, so their `Suggests` are no longer installed. Everything a
  component needs in order to run is still installed; what is no longer
  pulled in is the tooling those dependencies use for their own
  examples, tests and vignettes.
- Source-code guesses from `detect_implicit_dependencies()` are now
  diagnostic only. Dependencies declared by a component, or supplied
  explicitly through `additional_deps`/`force_deps`, remain binding;
  guessed packages must be opted in explicitly. This prevents comments
  and common function names from making a distributed meta-package
  depend on unrelated packages.
- Generation now rejects a local dependency constraint that the included
  archive cannot satisfy, and rejects a local archive that is present in
  `pkg_dir` but was not included in `packages`. Component R version
  requirements are propagated to the generated DESCRIPTION for
  enforcement on the recipient’s R version.
- The arguments added in this release come last in every signature, so a
  positional call written against 0.1.0 keeps binding to the same
  parameters. Positional calls are still a fragile way to call these
  functions: name the arguments.

### Generated meta-packages are self-contained

- [`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)
  gained `include_archives`, `TRUE` by default, which copies the
  component archives into `inst/archives/` of the generated
  meta-package. Its installer then defaults `pkg_dir` to
  `system.file("archives", package = "<meta>")`, resolved when the
  installer is called, so the meta-package is the only artifact that has
  to be distributed and its components install with no arguments, with
  no path agreed on beforehand. Network access remains necessary only
  for components that depend on a package coming from a repository. Pass
  `include_archives = FALSE` to keep the archives in a shared location
  the recipient can reach.
- The build ignore rules of a generated meta-package exempt
  `inst/archives/`, so shipped components reach the tarball while every
  other archive anywhere in the tree is still excluded.

### New features

- Generated meta-packages provide an optional `cli` two-column startup
  message that falls back to the ASCII banner when `cli` is absent, a
  `<meta>.quiet` option, `<meta>_conflicts()` for masking conflicts,
  explicit reinstall and upgrade policies (`force`, and
  `upgrade = "newer" | "always" | "never"`), and an optional ordered
  workflow vignette through the `workflow` argument.
- Generated metadata records the component list in
  `Config/bigbang/packages`, and the generated package tests that the
  list agrees with what it exports.

### Bug fixes

- Rollback of a failed generation works when a component of the
  destination path is a symbolic link. The project path was normalised
  before the directory existed and therefore left unresolved, while the
  path it was compared against was normalised after creation and
  resolved, so the two never matched and the rollback declined silently.
  That is the situation on macOS, where
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) sits under `/var`,
  itself a link to `/private/var`. Both sides are now normalised at the
  same moment.
- `create_metapackage(reexport = TRUE)` works. It previously failed for
  every input: a block that ran before the re-export writer iterated the
  versioned archive stems and called
  [`asNamespace()`](https://rdrr.io/r/base/ns-internal.html) on them,
  which can never resolve, so the call aborted with “there is no package
  called ‘pkg_1.0’” before reaching the writer that does the work. That
  block also declared `S3method(<name>, default)` for every export
  whether or not it was a generic. It was removed;
  `write_reexports_file()` already resolves component namespaces and
  distinguishes S3 generics from ordinary functions.
- An archive whose root directory is accompanied by an AppleDouble
  `._<dir>` member is accepted. Archiving a package directory on macOS
  with extended attributes emits one, and R installs such an archive, so
  rejecting it rejected a working package.
- [`diagnose_dependencies()`](https://sebollin.github.io/bigbang/reference/diagnose_dependencies.md)
  extracts through the same guarded path as generation. A component
  carrying a symbolic link made the scanner read a file outside the
  archive and return its contents in the result.
- Generation no longer changes the process working directory; every
  scaffold writer receives an explicit absolute path.
- Fixed generation with relative `dest_dir` and `pkg_dir` paths, and
  roll back incomplete project trees created by a failed invocation.
- [`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)
  validates `name` before writing anything: against the R package name
  grammar, and against the names R itself ships. A name carrying a path
  separator or a parent reference previously placed the generated tree
  outside `dest_dir` and still reported success.
- Non-local dependencies are installed with `dependencies = NA`, which
  covers Depends, Imports and LinkingTo, instead of `TRUE`, which also
  covered Suggests. Asking for one small dependency used to pull
  development tooling and its whole tree into the library.
- Restored the caller’s attached packages and loaded namespaces after
  automatic documentation, and report documentation success accurately.
- Fixed eleven Spanish runtime messages whose catalog keys had leading
  or trailing whitespace.
- Generated helper examples use the requested meta-package name instead
  of an internal historical name.
- Paths returned in results use forward slashes on every platform, like
  the rest of the package. On Windows `result$path` came back in the
  platform convention and therefore did not compare equal to a path the
  caller had built with
  [`file.path()`](https://rdrr.io/r/base/file.path.html). The
  containment checks normalise both sides the same way, so a destination
  that does not exist yet cannot be compared against one that does in a
  different convention.
- When a component is skipped because a non-local dependency is missing,
  the warning names the call that resolves it and the attachment step no
  longer follows it with a vaguer hint suggesting a bare re-run, which
  would have skipped the component again for the same reason.
- Component archive `Package` and `Version` fields are validated against
  their filenames, duplicate component versions and archive basenames
  are rejected while generating, and generated installers verify the
  declared version.
- Already-installed messages now report the installed version rather
  than the version named by the archive.
- Component names matching standard R package paths no longer generate
  `.Rbuildignore` rules that could exclude the generated package’s own
  files or shipped archives.
- Already-installed messages show both the installed and archive
  versions, and explicitly flag when the installed version is newer. The
  `unchanged` element of an installation result now carries the same two
  versions instead of the label “Already installed”, which is untrue
  when the installed package only shares a component’s name; the result
  is the only thing a calling script can inspect.
- Archive inspection now validates extraction status, member paths,
  symbolic links, and the single package root before accepting an
  archive. Nested `tests/DESCRIPTION` files are allowed, while a missing
  root DESCRIPTION is reported clearly.
- Malformed component R files produce a warning naming the component
  archive and the path inside it, while generation continues, so a
  possible encoding issue is diagnosable without turning generation into
  an unconditional rejection. The warning no longer names the extraction
  directory, a temporary that no longer exists by the time the warning
  is read.
- Rollback removes an empty destination directory created by the failed
  call, and `safe_unlink()` uses temporary-directory containment rather
  than a broad basename heuristic.
- The source-code patterns require a namespace qualifier or a call for
  every package they guess at, so ordinary S4 code is no longer reported
  as needing `Matrix`, and an identifier beginning with `st_` no longer
  suggests `sf`.

### Documentation

- The maintainer’s given name is spelled Sebastián, and the author entry
  now carries an ORCID identifier.

- Documented that distributing the component archives inside a
  meta-package is a redistribution, so their licenses have to allow it,
  and that a component still installs only where its format can.

- Documented related meta-package projects and design precedents.

- Added the r-universe version badge and the one-line install from the
  universe binaries.

## bigbang 0.1.0

CRAN release: 2026-08-08

First release on CRAN, accepted on 2026-08-08.

**bigbang — create custom R metapackages from local packages**

### Data-safety architecture

- Removed all generated startup installation and cwd-relative cleanup.
  Generated `.onLoad()` hooks are side-effect free; component
  installation is explicit via `<meta>_install()`.
- Added destructive regression tests in disposable directories,
  including real source installation, attachment, data-only components,
  names beginning with `tmp`, and content hashes for decoy directories.
- Added
  [`scan_bigbang_artifact()`](https://sebollin.github.io/bigbang/reference/scan_bigbang_artifact.md)
  to identify the historical V1/V2/V3/V7 signatures in source trees,
  archives, and installed packages without loading them.
- Generated projects must use a new or empty destination, preventing
  unsafe in-place regeneration of unclassified historical sources.

### Installation and portability

- Component archives are installed once in topological dependency order;
  cycles raise a typed `bigbang_error_cycle` condition.
- Added explicit offline dependency policies (`skip`, `error`,
  `install`) and content-based distinction between source ZIPs and
  Windows binary ZIPs.
- All generated text is written as UTF-8, R literals and paths are
  emitted safely, and CI covers Linux, Windows, and macOS
  configurations.

### API, language, and documentation

- Added the English snake_case API:
  [`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md),
  [`install_local_pkg()`](https://sebollin.github.io/bigbang/reference/install_local_pkg.md),
  and
  [`diagnose_dependencies()`](https://sebollin.github.io/bigbang/reference/diagnose_dependencies.md).
  Spanish aliases remain available as deprecated transition wrappers.
- Added typed results and conditions with print methods.
- English is the source language. Spanish runtime translations are
  supplied through gettext for both `bigbang` and generated
  metapackages.
- Added English and Spanish guides, release documentation, and a pkgdown
  configuration.
