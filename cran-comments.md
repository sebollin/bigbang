<!-- PENDIENTE ANTES DE ENVIAR: reemplazar los dos RUN-PENDIENTE de la sección
     "Test environments" por los identificadores reales de las corridas de R-hub
     y GitHub Actions sobre el commit exacto que se envía, y borrar este
     comentario. Las corridas anteriores se hicieron sobre un commit previo y no
     valen: la lección del ciclo pasado es que las verificaciones externas tienen
     que correr sobre el SHA definitivo. -->

## Update

This is an update from 0.1.0, which was published on 2026-08-08. The interval is
short and we do not intend to make a habit of it; what follows is why we think it
is worth your time now rather than in two months. Subsequent updates will keep to
the usual pace.

The release completes the feature the package is built around, and it stops the
package from producing artifacts that fail on someone else's machine.

A generated meta-package now carries its component archives, so installing that
one file is enough to install the components. Previously the recipient also
needed the directory of archives and had to be told its path, which assumes both
machines agree on one; in the environments this package is meant for they often
share neither a path nor a network. Two smaller fixes are of the same kind:
generation left the destination unusable when `dest_dir` was relative, and the
generated installer took the absolute archive path of the machine that produced
it as its default.

The second group of fixes concerns validation. `bigbang` reads the DESCRIPTION of
every component it is given, so it can tell before writing anything whether the
set it was handed can be installed at all. In 0.1.0 it did not check, and several
inputs produced a meta-package that was distributed and then failed on the
recipient's machine, which is the worst place for it to fail: a version
constraint between components that the included archives could not satisfy, a
declared dependency that was present in the source directory but had not been
included, a truncated archive copied inside and shipped, and a component R
version requirement that was silently discarded instead of being recorded. All
four are now detected while generating, with a message that names what is wrong
and what to do about it. One input that 0.1.0 rejected is now accepted: a
component whose tarball contains a nested `DESCRIPTION`, which many CRAN packages
ship as an example or test fixture.

One documented argument never worked. `create_metapackage(reexport = TRUE)`
failed for every input in 0.1.0, because a block that ran before the re-export
writer called `asNamespace()` on versioned archive stems, which can never
resolve. The block was redundant as well as wrong and has been removed. There is
now a test that calls the argument; the previous suite only referenced it by
name.

Mechanically: the archives are copied into `inst/archives/` of the generated
meta-package, and its installer defaults the archive directory to
`system.file("archives", package = "<meta>")`, which is resolved when the
installer runs and so points at the library of whoever installed it.

Nothing in `bigbang` itself installs anything unless the user calls an
installation function explicitly, and no repository is contacted unless the user
selects `cran_deps = "install"`. Loading either `bigbang` or a generated
meta-package never installs packages.

## Behaviour changes that are not backwards compatible

All in the direction of failing early rather than misleading. They are listed in
NEWS.md.

1. In a generated meta-package built with `include_archives = FALSE`,
   `<meta>_install()` requires `pkg_dir`. In 0.1.0 that argument defaulted to the
   absolute archive path of the machine where the meta-package had been
   generated, which does not exist anywhere else and produced a confusing
   failure. With the new default, `include_archives = TRUE`, the argument is
   optional again because the archives travel inside the package.
2. Components that the installation policy left untouched are reported in a new
   `unchanged` element of the installation result instead of appearing in
   `installed`, where they were labelled "Already installed". Reporting a package
   as installed when nothing was installed was misleading, and the label itself
   is untrue when the installed package merely shares a component's name, so the
   reported reason now names the installed version and the archive version.
3. The deprecated Spanish aliases `crear_meta_paquete_local()`,
   `diagnosticar_dependencias()` and `install_loc_pkg_w_dep()` were removed. They
   existed to ease a rename inside the organisation the package grew in, while it
   was still unpublished. The English names have been the documented API since
   0.1.0.
4. Source-code dependency guesses are diagnostic by default rather than hard
   dependencies of generated packages. Dependencies declared by a component, or
   supplied explicitly through `additional_deps`/`force_deps`, still bind. This
   prevents comments and ambiguous common function names from making a
   distributed meta-package require unrelated packages.
5. `create_metapackage()` now rejects inputs it used to accept, in every case
   because the resulting meta-package could not have been installed: an
   unsatisfiable version constraint between components, a declared dependency
   available in the archive directory but absent from the component list, an
   archive whose extraction fails, an archive containing absolute or
   parent-traversal member paths, and an archive without a single package root
   directory. It also validates the meta-package name against the R package name
   grammar and against the names R itself ships.
6. A component R version requirement is aggregated into the `Depends` field of
   the generated meta-package, so R enforces it on the recipient. Validating it
   against the generating machine would be wrong, since the meta-package is built
   to be installed elsewhere.

Arguments added in this release come last in every signature, so a positional
call written against 0.1.0 still binds to the same parameters.

## Test environments

Every result below was obtained on the source of this submission.

- Local Linux (Pop!_OS 22.04), R 4.6.1: `R CMD check --as-cran`, including the
  PDF manual: 0 errors, 0 warnings.
- win-builder, R-devel: Status 1 NOTE, the days-since-update one. No technical
  observation.
- R-hub v2, R-devel on the Linux, Windows, and macOS containers: Status OK on all
  three (run RUN-PENDIENTE).
- GitHub Actions: Ubuntu (release, devel, oldrel-1), Windows (release), and macOS
  (release), with the PDF manual enabled: all five configurations pass (run
  RUN-PENDIENTE).
- The tests that install packages are skipped under `--as-cran` and run in a
  dedicated step on every configuration; the destructive data-loss verification
  runs on Ubuntu release.
- A meta-package generated by this source was built and checked with
  `R CMD check --as-cran`, including its PDF manual, in both shipping modes and
  with a destination path longer than 120 characters: 0 errors, 0 warnings.
- A generated meta-package carrying its components was built, its source tree and
  the original archive directory were deleted, and it was installed into an empty
  library on its own. `<meta>_install()` with no arguments installed all three
  components in dependency order.

The only NOTE on this machine beyond the days-since-update one is the absence of
HTML Tidy, which is not installed here.

## Included component archives

`include_archives = TRUE` copies the user's own package archives into the
meta-package that `bigbang` generates on the user's machine. It does not ship any
third-party content inside `bigbang`; the archives are supplied by whoever calls
the function. The help states that distributing them this way is a
redistribution, so their licenses have to allow it.

## Reverse dependencies

There are none. Checked against the 24705 packages available from CRAN: no
package declares `bigbang` in Depends, Imports, Suggests, LinkingTo or Enhances.
