## Update

This is an update from 0.1.0, published on 2026-08-08. It supersedes 0.2.0, which
was prepared and verified on 2026-08-09 but never submitted, because submissions
were closed between 2026-08-05 and 2026-08-19. The calendar gap is that closure
rather than a habit of frequent releases; the work in both versions was done in
the days following the 0.1.0 acceptance, and we will keep to the usual pace from
here. `NEWS.md` documents 0.2.0 and 0.3.0 separately, because a user updating
from 0.1.0 receives both.

The release changes what the package accepts as input, and it stops the package
from producing artifacts that fail on someone else's machine.

## What it does

`bigbang` builds a meta-package from a set of local package archives. Until now
those archives had to sit in one directory, share one extension, and carry the
version in the filename, because a single expression reconstructed each path from
a stem. They can now be supplied as paths from several directories, with
`.tar.gz`, `.tar` and `.zip` mixed, and with any filename: package identity and
version are read from each archive's `DESCRIPTION`, which is the authority. A
component may also be a source directory, which is built with the optional
`pkgbuild` package.

`create_metapackage(dry_run = TRUE)` resolves and validates everything and
returns the plan — components, installation order, files that would be written,
and every finding — without creating the destination or writing anything.

Three defects fixed here produced a meta-package that installed on the machine
that generated it and failed on the recipient's: an archive whose extension was
upper-case was shipped under its original name while the generated installer
looked for the normalised one, so the component was never installed on a
case-sensitive filesystem; a component manifest could not find archives supplied
through `pkg_dir`; and an unreadable unrelated archive left in a source directory
aborted generation.

## Validation strictness

The release adds a `tolerate` argument, and we want to be precise about its
scope, because "relaxable validation" is easy to misread.

Validations that protect whoever installs the generated meta-package **cannot be
disabled**, and there is no argument that switches validation off as a whole.
During generation these remain hard errors: an unsatisfiable version constraint
between components, a dependency cycle, an archive whose extraction fails,
archive members with absolute or parent-traversal paths, symbolic links pointing
outside an archive, an archive without a single package root, a missing or empty
`Package`/`Version`, an invalid meta-package name, and two archives for the same
component. A test enumerates that list and asserts that no value of `tolerate`
reaches any of it.

`tolerate` takes a closed list of two named relaxations, both about the tidiness
of the person generating rather than the correctness of what is distributed: a
filename that disagrees with the archive's `DESCRIPTION`, and a declared local
dependency that is present in a supplied source directory but deliberately not
included. An unknown name is an error, not a silent no-op, and every applied
relaxation is recorded in the result. The help states that the second relaxation
means the dependency does not travel, so the recipient must supply it.

The installer is deliberately more tolerant than generation, and the
documentation now says so explicitly: when a component is already installed and
the policy would leave it alone, its archive is not read at all. Generation
validates hard because it produces an artifact for another machine; installation
does not need to read archives it will not use.

## Files on disk

`create_metapackage(update = TRUE)` regenerates in place, which relaxes the rule
that a destination must be new or empty. It is constrained: generation records a
manifest of the files it wrote together with their content hashes, `update`
rewrites only those files and reconciles generated files and shipped archives
removed from the new plan. It refuses to proceed if the manifest is absent or
if any recorded file was modified or removed. Files the package did not write are
never touched; updates also refuse to write through symbolic links in the
generated project. Before changing an existing project, bigbang backs up every
generated file and its manifest; a failed update restores that state and remains
retryable. Dry runs and completed updates report generated files that would be or
were removed. Nothing is installed or removed unless the user calls a function
explicitly, and no repository is contacted unless the user selects
`cran_deps = "install"`.

## Behaviour changes that are not backwards compatible

All in the direction of failing early rather than misleading, and all listed in
`NEWS.md`. Arguments added in this release come last in every signature, so a
positional call written against 0.1.0 still binds to the same parameters.

The ones a 0.1.0 user would notice: source-code dependency guesses are
diagnostic rather than binding; components left untouched by the installation
policy are reported in a new `unchanged` element instead of appearing in
`installed`; the deprecated Spanish aliases were removed; `reexport` is now a
deprecated, ignored positional placeholder because importing components would
make them installation-time dependencies; and generation rejects inputs it used
to accept, in every case because the resulting meta-package could not have been
installed.

## Included component archives

`include_archives = TRUE`, the default, copies the user's own package archives
into the meta-package that `bigbang` generates on the user's machine. It does not
ship third-party content inside `bigbang`; the archives are supplied by whoever
calls the function. The help states that distributing them this way is a
redistribution, so their licenses have to allow it, that it makes the generated
tarball as large as its components, and that CRAN prefers source tarballs under
10 MB and does not accept binary executables in them — which matters only if a
generated meta-package is ever submitted there.

## Test environments

Every result below was obtained on the source of this submission.

- Local Linux (Pop!_OS 22.04), R 4.6.1:
  `NOT_CRAN=true R CMD check --as-cran bigbang_0.3.0.tar.gz`, including the PDF
  manual and installation tests: 0 errors, 0 warnings, 2 notes. One is the
  days-since-update note; the other is the absence of HTML Tidy, which is not
  installed here.
- win-builder, R-devel: <PENDING>
- R-hub v2, R-devel on the Linux, Windows and macOS containers: <PENDING>
- GitHub Actions: Ubuntu (release, devel, oldrel-1), Windows (release) and macOS
  (release), with the PDF manual enabled: <PENDING>
- The test suite is 137 `test_that` blocks. Tests that install packages are
  skipped on CRAN itself and exercised by setting `NOT_CRAN=true` in every CI
  configuration; the destructive data-loss verification runs on Ubuntu release.
- Generated meta-packages were built and checked with `R CMD check --as-cran`
  in all four combinations of shipped/external archives and workflow/no
  workflow. A project was also updated while changing those options and removing
  a component, then built, checked, installed and exercised without the removed
  component in the recipient library.
- A generated meta-package carrying its components was built, its source tree and
  the original archive directory were deleted, and it was installed into an empty
  library on its own. `<meta>_install()` with no arguments installed its
  components in dependency order.

## Reverse dependencies

There are none. Checked against the 24705 packages available from CRAN: no
package declares `bigbang` in Depends, Imports, Suggests, LinkingTo or Enhances.
