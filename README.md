# bigbang — create custom R metapackages from local packages <img src="man/figures/logo.png" align="right" height="139" alt="bigbang logo" />

**bigbang** builds tidyverse-style metapackages from local package archives.
Every metapackage ends in *-verse*—`tidyverse`, `teamverse`, yours. This package
creates them: one function call, and a new *-verse* exists.

It is designed for teams that exchange local R package archives (`.tar.gz` or
`.zip`) behind an
institutional firewall and do not operate a package repository.

The generated package has two separate jobs:

- `library(<meta>)` attaches components that are already installed and reports
  missing ones.
- `<meta>_install()` explicitly installs local archives once, in topological
  dependency order, and then attaches them.

Package startup hooks never install packages or remove files.

## Installation

During development, install the source checkout with:

```r
install.packages("path/to/bigbang", repos = NULL, type = "source")
```

## Quick start

Suppose `archives/` contains:

```text
archives/
├── datahelpers_1.2.0.tar.gz
└── reports_0.9.1.tar.gz
```

Create the metapackage in a new directory:

```r
library(bigbang)

result <- create_metapackage(
  name = "teamverse",
  packages = c("datahelpers_1.2.0", "reports_0.9.1"),
  pkg_dir = "archives",
  dest_dir = tempdir(),
  document = TRUE
)
result
```

Build and install `teamverse` by the usual R package workflow. Component
installation remains explicit:

```r
library(teamverse)                 # attaches what is already installed
teamverse_install(cran_deps = "skip")
```

`cran_deps = "skip"` is the default and never accesses the network. Use
`"error"` to fail immediately when a non-local dependency is missing, or
`"install"` with an explicitly configured `repos` value to allow repository
installation.

See `vignette("getting-started", package = "bigbang")` for a reproducible
toy project created entirely under `tempdir()`.

## Main API

- `create_metapackage()` creates a complete metapackage source tree.
- `install_local_pkg()` installs one local archive and its dependencies.
- `diagnose_dependencies()` reports possible implicit dependencies.
- `scan_bigbang_artifact()` scans old source trees, archives, or installed
  packages for historical deletion signatures without loading them.

The former Spanish function names remain as deprecated transition aliases.

## ZIP files and portability

ZIP archives are classified by content. A ZIP containing `Meta/package.rds` is
a Windows binary and is installed with `type = "win.binary"` on Windows only.
Other ZIPs containing DESCRIPTION are unpacked into an owned temporary
directory and installed as source packages.

All generated text is written explicitly as UTF-8. CI is prepared for R release
on Windows and macOS and for release, devel, and oldrel on Ubuntu. The declared
minimum is R 3.6.0, following the minimum of the imported `brio` release.

## English and Spanish

English is the source language for code, help, and runtime messages. A complete
Spanish runtime catalog is included through R's gettext mechanism:

```r
Sys.setLanguage("es")  # R >= 4.2
```

On earlier R versions, set `LANGUAGE=es` before starting R. A complete Spanish
guide is available in `vignette("bigbang-es", package = "bigbang")` and
as [README.es.md](README.es.md). Rd help remains English because R has no stable
native mechanism for translated help; a separate `bigbang.es` module can be
considered if `rhelpi18n` becomes production-ready and reaches CRAN.

## Choosing the right tool

| Need | Best fit |
|---|---|
| One metapackage over a fixed set of local archives, with explicit offline installation | `bigbang` |
| A conventional local repository with indexes, multiple packages, and repository semantics | `miniCRAN` or `drat` |
| A small metapackage around packages already available from repositories | `pkgverse` |

`bigbang` deliberately does not replace a repository manager. If a team needs
version retention, repository indexes, or dependency distribution to many
projects, `miniCRAN`/`drat` is the stronger abstraction. `bigbang` is useful
when the distributed unit is a curated metapackage plus a directory of archives.

## Data-safety history and old artifacts

An unreleased predecessor generated cleanup code that could remove directories
named after components from the user's working directory. The startup installer
and all cwd-relative cleanup paths were removed before this CRAN submission and
are covered by destructive regression tests that run only in disposable trees.

Do not load or document an old generated artifact before classifying it:

```r
scan <- scan_bigbang_artifact("path/to/artifact", dry_run = TRUE)
scan
```

If `scan$vulnerable` is true, quarantine the artifact and generate a new version
in a new, empty destination. Never regenerate an unclassified source tree in
place. The full remediation procedure is documented in the Spanish guide and in
`RELEASE.md`.

## Development status

The complete test suite includes unit, portability, i18n, scanner, installation,
and data-loss regression tests. `R CMD check --as-cran` is run with the PDF manual
enabled for both `bigbang` and a generated metapackage. Platform CI will become
authoritative once the repository is published; win-builder and `rhub::rhub_check()`
remain release gates before CRAN submission.
