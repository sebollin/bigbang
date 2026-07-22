# bigbang release runbook

Run every gate from a clean checkout. Record the command, R version, operating
system, and complete result in `cran-comments.md`; do not summarize an
environmental failure as a successful check.

## 0. Remediate pre-release metapackages

Complete this before announcing or distributing `bigbang`.

1. Inventory every generated source tree, source archive, and installed
   metapackage without loading it.
2. Scan each artifact in read-only mode:

   ```r
   bigbang::scan_bigbang_artifact(path, dry_run = TRUE)
   ```

3. Do not call `library()`, `load_all()`, `document()`, or an old generated
   installer on an unclassified artifact.
4. For every vulnerable or unversioned artifact, generate a higher-version
   replacement in a new, empty directory. Never regenerate in place.
5. Scan the new source and built archive, install it in an isolated library,
   scan the installation, and only then exercise its explicit `<name>_install()`
   function.
6. Replace deployed copies only after the clean replacement passes those gates.
   Quarantine or remove the old copies through the organization's normal backup
   and change-control process.

## 1. Local gates

Make TinyTeX visible and run the complete suite, destructive sandbox integration,
lint, spelling, and coverage checks:

```sh
export PATH="$HOME/.TinyTeX/bin/x86_64-linux:$PATH"
Rscript --vanilla -e 'devtools::test(reporter = "summary")'
Rscript --vanilla tests/regresion/verificar_fix_borrado.R
Rscript --vanilla -e 'print(lintr::lint_package())'
Rscript --vanilla -e 'spelling::spell_check_package(".", vignettes = FALSE)'
Rscript --vanilla -e 'print(covr::package_coverage(quiet = FALSE))'
R CMD build .
R CMD check --as-cran bigbang_*.tar.gz
```

Run the English and Spanish prose checks exactly as documented in
`CONTRIBUTING.md`. Confirm in each check log that the manual PDF and vignettes
were actually built. The manual is enabled by default; `--manual` is not an
`R CMD check` option (and `--no-manual` must not be used for this gate). Generate
a representative metapackage in `tempdir()`, build it, scan its source and
archive, and run the same `R CMD check --as-cran` gate on it.

## 2. Hosted CI and coverage

1. Confirm the release commit is published at
   `https://github.com/sebollin/bigbang`.
2. Confirm that `.github/workflows/R-CMD-check.yaml` is green for
   `ubuntu-release`, `ubuntu-devel`, `ubuntu-oldrel`, `windows-release`, and
   `macos-release`.
3. Record the CI commit SHA and the covr percentage produced from that exact SHA
   in `cran-comments.md`.
4. Review the complete `lintr::lint_package()` result. Every remaining item must
   be either fixed or explicitly justified in the release notes.

## 3. External CRAN-like gates

Submit the built tarball to both services and wait for all platforms to finish:

```r
devtools::check_win_devel()
rhub::rhub_check()
```

Record service URLs, platform/R versions, and ERROR/WARNING/NOTE counts in
`cran-comments.md`. Investigate every difference from the local and GitHub CI
results; do not waive failures merely because another platform passed.

## 4. Submission

1. Update `NEWS.md`, package version and release date.
2. Update `cran-comments.md` with local, generated-metapackage, GitHub Actions,
   win-builder, R-hub, coverage, spelling, lint, PDF, and vignette results.
3. Rebuild once from the final commit and verify the tarball contents.
4. Submit only that tarball:

   ```r
   devtools::submit_cran()
   ```

5. Tag the submitted commit. Announce the package only after the internal
   remediation in step 0 is complete and the CRAN result is known.
