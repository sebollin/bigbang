## Update

This is an update from 0.1.0, published on 2026-08-08. It supersedes 0.2.0
and 0.3.0, which were prepared and verified but never submitted because CRAN
submissions were closed from 2026-08-05 through 2026-08-19. The version gap
reflects that closure rather than a pattern of frequent releases. `NEWS.md`
documents 0.2.0, 0.3.0 and 0.4.0 separately so that users updating from 0.1.0
can distinguish the changes from each development cycle.

## What it does and what is new

`bigbang` generates self-contained meta-packages from local R package archives.
By default the component archives travel inside the generated meta-package;
the recipient installs that one artifact and can then install its components
in dependency order without knowing any path from the generating machine.
Repositories are contacted only when the user explicitly requests installation
of a component's non-local dependencies.

The changes prepared in 0.2.0 and 0.3.0 made that distribution model portable
and data-safe. Component identity and version come from `DESCRIPTION`; inputs
may mix archive formats and source directories from several locations; manifests,
dry runs, selective installation, explicit destination libraries and controlled
in-place updates are supported. Generated projects track only files written by
bigbang and preserve untracked user files.

In 0.4.0, `reexport = TRUE` exposes explicit component exports through lazy,
read-only active bindings. Components remain outside `Imports` and `Depends`, so
the generated meta-package still installs and loads offline before its components
exist. Bare package names now resolve by the declared `Package` identity when
exactly one readable archive matches. The lifecycle is documented as stable,
and `removed_files` now includes partial documentation output created and cleaned
up by the same failed documentation attempt.

Generation and installation deliberately have different strictness. Generation
validates every archive and rejects conditions that could produce an artifact
which fails on the recipient's machine. Installation is tolerant of an unreadable
archive only when the selected policy leaves an already installed component
untouched, because that archive will not be used.

## Files on disk

`create_metapackage(update = TRUE)` rewrites only files recorded in its generated
manifest and reconciles generated files and shipped archives removed from the new
plan. It refuses modified or missing tracked files, untracked paths it would need
to overwrite, and symbolic links within the generated project. Before writing,
it backs up every tracked file and the manifest; a failed update restores the
previous state and remains retryable. Files bigbang did not write are never
touched. Dry runs report predictable removals, and completed calls report both
planned removals and partial outputs created and cleaned during the call.

## Test environments

Every result below refers to the source of this submission.

- Local Linux (Pop!_OS 22.04), R 4.6.1:
  `NOT_CRAN=true R CMD check --as-cran bigbang_0.4.0.tar.gz`, including the PDF
  manual and installation tests: 0 errors, 0 warnings, 2 notes. One is the
  days-since-last-update note caused by the submission closure described above;
  the other is environmental because HTML Tidy is not installed locally.
- win-builder, R-devel (2026-08-14 r90407 ucrt): Status OK — no errors,
  warnings or notes.
- R-hub v2, R-devel on Linux, Windows and macOS: all three passed
  (GitHub Actions run 31859081215).
- GitHub Actions matrix (Ubuntu release/devel/oldrel-1, Windows release and
  macOS release): all five passed (run 31859076052).

The suite contains 179 `test_that` blocks and 964 assertions. With
`NOT_CRAN=true`, all installation tests run and the result is 964 passed,
0 failed, 0 warnings and 0 skipped. The destructive data-loss verification also
runs on Ubuntu release. Generated meta-packages are built and checked across the
shipping, workflow, re-export and update combinations that alter their contents.

## Reverse dependencies

There are none. Checked against the 24732 packages available from CRAN: no
package declares `bigbang` in Depends, Imports, Suggests, LinkingTo or Enhances.
