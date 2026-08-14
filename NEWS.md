# bigbang 0.4.0

## New features

- The project lifecycle is now documented as stable: its public API has remained
  stable since 0.1.0 and is protected by an extensive regression suite and
  repeated independent adversarial reviews.

- `create_metapackage(reexport = TRUE)` now exposes explicit component exports
  through lazy, read-only active bindings without making components installation
  dependencies. Generated metapackages can load offline before their components
  are installed, and the bindings resolve after a later installation.

- Runtime re-exports now exclude S4 class and method directives, reserve every
  generated helper symbol, preserve untracked `reexports.R` and `reexports.Rd`
  files during updates, avoid loading components for startup reporting, and
  support installation into a new library directory.

- Component inputs now accept a bare package name when exactly one archive in
  the supplied directories declares that `Package` identity. Ambiguous versions
  or sources are rejected with the full candidate list.

## Documentation

- The acknowledgments now use Richard Detomasi's current contact and profile,
  retain the precise credit for the initial metapackage-builder suggestion, and
  identify `pegeler/metapackage` as a related online declarative metapackage.

## Bug fixes

- Bare package-name discovery now warns when an unreadable archive is excluded
  and suppresses uncontrolled tar diagnostics while retaining readable matches.

- Runtime re-exports quote non-syntactic names in generated NAMESPACE files, so
  legal exports containing spaces or Unicode characters produce installable
  metapackages and working active bindings.

- `removed_files` now includes partial documentation outputs that a generation
  call created and cleaned up after roxygen failed. Dry-run plans continue to
  report only removals that are knowable before generation starts.

# bigbang 0.3.0

This release makes the package accept component packages as an organisation
actually keeps them, instead of requiring one directory, one archive extension,
and a version in every filename. Package identity now comes from each archive's
`DESCRIPTION` rather than its filename, generation can report a plan without
writing anything, and the checks that concern project tidiness can be relaxed
individually. The checks that protect whoever installs the generated
meta-package cannot: they remain hard errors during generation.

It also supersedes 0.2.0, which was prepared and verified but never submitted.
Everything listed under 0.2.0 below reaches CRAN for the first time here, so a
user updating from 0.1.0 should read both sections.

## Breaking changes

- `reexport` is deprecated and ignored. Generated metapackages attach installed
  components, so their exports remain available directly or through each
  component namespace, but they are not copied into the metapackage namespace.
  Re-exporting would require declaring components as installation-time
  dependencies, which conflicts with explicit offline component installation;
  the option did not produce a valid package in the published 0.1.0 release.

## Component archives can come from anywhere

- Any element of `packages` that is an existing file is used as a path, so
  components can come from **several directories** in one call and mix
  `.tar.gz`, `.tar` and `.zip`. Stems still work, resolved against `pkg_dir`,
  which now accepts more than one directory.
- **Package identity and version come from each archive's `DESCRIPTION`**, not
  from its filename, so a filename without a version is valid. A filename that
  disagrees with the `DESCRIPTION` produces a warning and the `DESCRIPTION`
  wins.
- A component may be a **source directory** with a `DESCRIPTION` at its root,
  built for you with the optional `pkgbuild` package. It requires
  `include_archives = TRUE`, because the archive built for it lives in a
  temporary directory that does not outlive the call.
- `packages` may be the path to a **manifest**: one component per line, `#` for
  comments. Relative paths resolve against the manifest's own directory,
  absolute and `~` paths are used as given, and bare filenames are also looked
  up in `pkg_dir`, so the list can live under version control while the archives
  do not. A missing entry reports every directory that was searched.

## Seeing what a call would do before it does it

- `create_metapackage(dry_run = TRUE)` resolves and validates everything and
  returns the plan — components, installation order, files that would be
  written, and every validation finding — **without creating `dest_dir` or
  writing anything**.

## Strictness you choose, within limits you do not

- `create_metapackage()` gains a final `tolerate` argument taking a closed list
  of named relaxations: `"filename_mismatch"` silences the filename-versus-
  `DESCRIPTION` warning, and `"unincluded_local_dep"` turns the error for a
  local dependency available in the supplied sources but omitted from `packages`
  into a warning. That second relaxation means the dependency does not travel,
  so the recipient has to supply it. Unknown names are errors, not silent
  no-ops.
- Generation results include a `tolerated` table naming every applied
  relaxation, the component it affected, and why.
- `on_component_error = "skip"` generates from the components that are valid
  instead of aborting, and reports the ones it left out. The exclusion is
  transitive: a component that depends on an excluded one is excluded too, and
  the chain is reported. When the omitted archive can be read, its declared
  `Package` drives the exclusion; when it cannot, the filename fallback is
  reported explicitly. Excluding every component is an error.
- The documentation now separates the two classes of check: validations that
  protect whoever installs the generated meta-package cannot be disabled by any
  value of `tolerate`, and there is no switch that turns validation off as a
  whole. It also states explicitly that bigbang does **not** run `R CMD check`
  on component packages, so a component with check warnings or notes can be
  included.

## Regenerating in place

- `create_metapackage(update = TRUE)` regenerates an existing project, which
  relaxes the rule that a destination must be new or empty. Generation records a
  manifest of the files it wrote together with their content hashes; `update`
  rewrites only those, and refuses to run if the manifest is missing or if a
  generated file was modified or removed by hand.
- Files bigbang did not write are never touched. `update` refuses when a
  generated file, or any parent directory on the way to it, is a symbolic link,
  and it replaces generated files and shipped archives atomically, so a
  regeneration cannot write outside the project tree.
- When components or options are dropped, `update` reconciles: generated files
  and shipped component archives that the new plan no longer includes are
  removed transactionally, so the regenerated meta-package cannot end up
  importing from a component it no longer declares. Before changing an existing
  project, bigbang backs up all generated files and its manifest. If generation
  or reconciliation fails, that state is restored and the update remains
  retryable. Dry runs and completed updates report the affected paths in
  `removed_files`; removing a component may remove the last available copy of
  its shipped archive.

## Generated installers

- `<meta>_install(only = ...)` installs a subset; local dependencies of the
  selection are added automatically, and an unknown component name is an error.
- `<meta>_install(lib = ...)` chooses the library in which components must be
  installed and verified. Non-local dependencies may already be available in
  that library or anywhere on `.libPaths()`; they no longer need to be
  duplicated into the component destination.
- `create_metapackage(install_upgrade = ...)` fixes the default upgrade policy
  of the installer that gets emitted, so the person generating decides whether
  recipients stay pinned to the versions being shipped or keep anything newer
  they already have. The default is unchanged.
- The installer accepts a vector of archive directories when the archives are
  not shipped, and archives that do travel inside a generated meta-package
  contain no path from the machine that produced them.
- Real generation results now include the same topological `order` field as
  dry-run plans. The planned `files` field remains specific to dry runs.
- When installing a source component fails, `install_local_pkg()` and generated
  installers now report the error lines of the installation subprocess itself
  -- for example, which dependency it could not find -- instead of only a
  generic verification failure.

## Bug fixes

- A failed documentation run during `update = TRUE` no longer deletes or leaves
  partially overwritten an untracked Rd file whose name is reserved for
  generated documentation. Such files are backed up before roxygen runs and
  restored without being adopted into the generation manifest.
- Migrating a development-era schema 1 generation manifest now adopts only the
  shipped component archives that belong to the current plan. An unrelated
  archive placed by the user under `inst/archives/` remains untracked and is
  never removed by a later update.
- Documentation outputs can be disabled and re-enabled across updates. If
  roxygen fails during an update, previously tracked Rd files are restored and
  retained in the manifest, while partial newly generated Rd files are removed,
  so a later documentation update remains possible.
- Update manifests are now built from the explicit generation plan instead of
  scanning the project tree. Repeated updates no longer absorb and later delete
  user files, including files under `.git/`.
- With `on_component_error = "skip"`, a failed update input no longer authorizes
  deletion of a previously shipped component archive. If the old component
  cannot be identified safely, archive reconciliation is deferred until a clean
  update.
- Updates now reject a generated project whose root entry is itself a symbolic
  link, in addition to links below the project root.
- Successful source-install transcripts now honor `verbose = FALSE` in both
  `install_local_pkg()` and generated installers; failure details still retain
  the child process's `ERROR` lines.
- `install_local_pkg(lib = ...)` and generated installers now distinguish the
  component destination from the dependency search path: a component present
  only elsewhere is installed into `lib`, while a non-local dependency already
  available elsewhere on `.libPaths()` is reused. This also works under the
  dependency isolation used by `R CMD check` on Windows.
- A component archive with an upper-case extension — `PKG_1.0.TAR.GZ`, as they
  often arrive from Windows — was shipped under its original name while the
  generated installer looked for the normalised one. The component was never
  installed on a case-sensitive filesystem, with no error anywhere. Archives are
  now copied under canonical names, and names that collide only by case are
  rejected.
- An unreadable archive unrelated to the requested components, left in a
  supplied source directory, aborted generation. Such archives are now excluded
  from the inventory with a warning, and there is a specific diagnostic when a
  declared dependency appears to match one.
- Installing a package that is already present no longer requires reading its
  archive when `upgrade = "never"`; under the default policy only the archive's
  `DESCRIPTION` is read before deciding whether the archive is needed at all. An
  unreadable archive is reported as unchanged when the installed package can be
  kept, instead of failing the call.
- A failed resolution of several components kept only a placeholder name in the
  installation result; the requested names are retained.
- Installation messages name both the package declared by the `DESCRIPTION` and
  the archive it came from when the two differ, instead of reporting the
  filename as though it were the package.
- Reading an archive's version for the installation shortcut now rejects
  extracted symbolic links before reading metadata, matching the guarantees of
  the full archive validation path.
- Rolling back a generation that failed after the project tree existed is now
  exercised, including destinations reached through a symbolic link and a
  removal that does not succeed.

## Documentation

- The installation help explains that under `upgrade = "never"` the component is
  identified from its filename, because the archive is deliberately not read;
  use the default policy when the declared `Package` has to be checked against
  what is installed.
- The README, in both languages, documents where components can come from and
  every generation option. It previously described the input model the package
  had before this release.

# bigbang 0.2.0

Prepared and verified but never submitted: CRAN submissions were closed between
2026-08-05 and 2026-08-19. These changes are released as part of 0.3.0.

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
- Source-code guesses from `detect_implicit_dependencies()` are now diagnostic
  only. Dependencies declared by a component, or supplied explicitly through
  `additional_deps`/`force_deps`, remain binding; guessed packages must be opted
  in explicitly. This prevents comments and common function names from making
  a distributed meta-package depend on unrelated packages.
- Generation now rejects a local dependency constraint that the included archive
  cannot satisfy, and rejects a local archive that is present in `pkg_dir` but was
  not included in `packages`. Component R version requirements are propagated to
  the generated DESCRIPTION for enforcement on the recipient's R version.
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

- Rollback of a failed generation works when a component of the destination path
  is a symbolic link. The project path was normalised before the directory
  existed and therefore left unresolved, while the path it was compared against
  was normalised after creation and resolved, so the two never matched and the
  rollback declined silently. That is the situation on macOS, where `tempdir()`
  sits under `/var`, itself a link to `/private/var`. Both sides are now
  normalised at the same moment.
- An archive whose root directory is accompanied by an AppleDouble `._<dir>`
  member is accepted. Archiving a package directory on macOS with extended
  attributes emits one, and R installs such an archive, so rejecting it rejected
  a working package.
- `diagnose_dependencies()` extracts through the same guarded path as generation.
  A component carrying a symbolic link made the scanner read a file outside the
  archive and return its contents in the result.
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
- Paths returned in results use forward slashes on every platform, like the rest
  of the package. On Windows `result$path` came back in the platform convention
  and therefore did not compare equal to a path the caller had built with
  `file.path()`. The containment checks normalise both sides the same way, so a
  destination that does not exist yet cannot be compared against one that does
  in a different convention.
- When a component is skipped because a non-local dependency is missing, the
  warning names the call that resolves it and the attachment step no longer
  follows it with a vaguer hint suggesting a bare re-run, which would have
  skipped the component again for the same reason.
- Component archive `Package` and `Version` fields are validated against their
  filenames, duplicate component versions and archive basenames are rejected
  while generating, and generated installers verify the declared version.
- Already-installed messages now report the installed version rather than the
  version named by the archive.
- Component names matching standard R package paths no longer generate
  `.Rbuildignore` rules that could exclude the generated package's own files or
  shipped archives.
- Already-installed messages show both the installed and archive versions, and
  explicitly flag when the installed version is newer. The `unchanged` element of
  an installation result now carries the same two versions instead of the label
  "Already installed", which is untrue when the installed package only shares a
  component's name; the result is the only thing a calling script can inspect.
- Archive inspection now validates extraction status, member paths, symbolic
  links, and the single package root before accepting an archive. Nested
  `tests/DESCRIPTION` files are allowed, while a missing root DESCRIPTION is
  reported clearly.
- Malformed component R files produce a warning naming the component archive and
  the path inside it, while generation continues, so a possible encoding issue is
  diagnosable without turning generation into an unconditional rejection. The
  warning no longer names the extraction directory, a temporary that no longer
  exists by the time the warning is read.
- Rollback removes an empty destination directory created by the failed call,
  and `safe_unlink()` uses temporary-directory containment rather than a broad
  basename heuristic.
- The source-code patterns require a namespace qualifier or a call for every
  package they guess at, so ordinary S4 code is no longer reported as needing
  `Matrix`, and an identifier beginning with `st_` no longer suggests `sf`.

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
