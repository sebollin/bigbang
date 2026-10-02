# Local metapackage generator
#
# `create_metapackage()` creates a package project whose installation remains
# side-effect free. Component installation is an explicit `<name>_install()` call.
# Generated code reads archive DESCRIPTION files, builds the local dependency
# graph, rejects cycles, and installs each component once in topological order.
# Startup hooks may attach installed components but never install or remove files.

# Read from the installed DESCRIPTION rather than kept as a literal, so the
# field stamped into a generated meta-package can never fall behind the version
# that actually produced it.
.bb_package_version <- function(package, lib_loc = NULL) {
  if (is.null(lib_loc)) {
    utils::packageVersion(package)
  } else {
    utils::packageVersion(package, lib.loc = lib_loc)
  }
}

.bb_generator_version <- function() {
  version <- tryCatch(
    as.character(.bb_package_version("bigbang")),
    error = function(e) NA_character_
  )
  if (is.na(version)) "unknown" else version
}

# Names R ships with. Kept as a literal so the check works without inspecting
# the library, which may differ from the machine the meta-package runs on.
.r_standard_packages <- c(
  "base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
  "parallel", "splines", "stats", "stats4", "tcltk", "tools", "translations",
  "utils"
)
# Names that R CMD build needs for the generated package itself. A component
# with one of these names cannot be excluded by a component-specific pattern:
# doing so would also exclude the generated DESCRIPTION, R, or inst tree.
.r_build_reserved_paths <- c(
  "r", "src", "data", "demo", "exec", "inst", "tests", "vignettes",
  "man", "po", "tools", "meta", "configure", "cleanup", "cleanup.win",
  "description", "namespace", "license", "licence", "readme",
  ".rbuildignore", ".gitignore"
)
.template_safety_schema <- "2"
.allowed_tolerations <- c("filename_mismatch", "unincluded_local_dep")
.generation_manifest_name <- ".bigbang-manifest.rds"

.planned_documentation_files <- function(name, reexport = FALSE) {
  static <- c(
    "build_dependency_graph", "classify_package_archive", "detect_cycles",
    "format_cli_startup", "generate_ascii_banner", "install_local_archive",
    "install_packages_in_order", "is_path_inside", "read_archive_metadata",
    "safe_unlink", "style_startup_text", "topological_order"
  )
  public <- paste0(name, c(
    "_attach_all", "_attach", "_conflicts", "_deps", "_detach", "_install",
    "_load_all", "_packages"
  ))
  public <- if (isTRUE(reexport)) {
    c(public, paste0(name, "_reexport_verification"))
  } else {
    public
  }
  file.path("man", paste0(c(static, public), ".Rd"))
}

.planned_generation_files <- function(name, components, workflow = NULL,
                                      include_archives = TRUE,
                                      license = "MIT + file LICENSE",
                                      document = FALSE,
                                      reexport = FALSE,
                                      reexport_symbols = NULL) {
  files <- c(
    "DESCRIPTION", "NAMESPACE", "README.md", ".Rbuildignore", ".gitignore",
    ".BBSoptions", paste0(name, ".Rproj"),
    file.path("R", c("attach.R", "utils.R", "zzz.R", "install_packages.R")),
    file.path("vignettes", paste0("introduction-", name, ".Rmd")),
    file.path("vignettes", ".gitignore"),
    file.path("tests", "component-consistency.R"),
    file.path("po", paste0("R-", name, ".pot")),
    file.path("po", "R-es.po"),
    file.path("inst", "po", "es", "LC_MESSAGES", paste0("R-", name, ".mo")),
    .generation_manifest_name
  )
  if (isTRUE(reexport)) {
    files <- c(files, file.path("R", "reexports.R"))
  }
  if (grepl("file[[:space:]]+LICENSE", license, ignore.case = TRUE)) {
    files <- c(files, "LICENSE")
  }
  if (!is.null(workflow)) {
    files <- c(files, file.path("vignettes", paste0("workflow-", name, ".Rmd")))
  }
  if (isTRUE(document)) {
    files <- c(files, .planned_documentation_files(name, reexport = reexport))
    if (isTRUE(reexport)) {
      exports <- if (is.null(reexport_symbols)) {
        unique(unlist(lapply(components, function(component) {
          .or_null(component$exports, character())
        }), use.names = FALSE))
      } else {
        unique(reexport_symbols)
      }
      if (length(exports) > 0L) files <- c(files, file.path("man", "reexports.Rd"))
    }
  }
  if (isTRUE(include_archives) && length(components) > 0L) {
    files <- c(files, file.path(
      "inst", .archive_subdir,
      vapply(components, .canonical_archive_name, character(1L))
    ))
  }
  unique(files)
}

.file_digest <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)
  unname(as.character(tools::md5sum(path)))
}

.manifest_records <- function(project_dir, files) {
  files <- unique(setdiff(files, .generation_manifest_name))
  paths <- file.path(project_dir, files)
  missing <- files[!file.exists(paths) | dir.exists(paths)]
  if (length(missing) > 0L) {
    stop(.bb_trf(
      "Generated files were not written as planned: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  root <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  rel <- substring(paths, nchar(root) + 2L)
  hashes <- vapply(paths, .file_digest, character(1L))
  list(schema = 2L, files = rel, hashes = stats::setNames(hashes, rel))
}

.legacy_owned_generation_files <- function(project_dir, files,
                                           requested_files = character()) {
  name <- basename(normalizePath(
    project_dir, winslash = "/", mustWork = TRUE
  ))
  known <- setdiff(
    .planned_generation_files(
      name, list(), workflow = stats::setNames(name, "stage"),
      include_archives = FALSE, license = "MIT + file LICENSE",
      document = TRUE
    ),
    .generation_manifest_name
  )
  known <- c(known, file.path("R", "reexports.R"))
  requested_archives <- requested_files[grepl(
    "^inst/archives/[^/]+\\.(tar\\.gz|tar|zip)$",
    requested_files, ignore.case = TRUE, perl = TRUE
  )]
  # Schema 1 scanned the whole tree and could therefore claim archives placed
  # there by the user. During migration, only canonical archive paths in the
  # current plan are adopted. An archive for a component removed in this same
  # legacy update remains untracked and must be cleaned up manually; retaining
  # it is safer than guessing ownership and deleting the only surviving copy.
  files[files %in% known | files %in% requested_archives]
}

.preserve_omitted_archives <- function(stale_files, omitted) {
  archives <- stale_files[startsWith(stale_files, "inst/archives/")]
  if (length(archives) == 0L || nrow(omitted) == 0L) return(character())

  archive_packages <- sub(
    "_.*", "", vapply(archives, function(path) {
      extension <- tryCatch(.archive_extension(path), error = function(e) "")
      if (nzchar(extension)) .archive_stem(path, extension) else basename(path)
    }, character(1L))
  )
  omitted_packages <- unique(omitted$component[nzchar(omitted$component)])
  matched <- archives[archive_packages %in% omitted_packages]

  # An unreadable or misspelled input may not expose the identity of the old
  # component it was meant to replace. In that ambiguous case deletion is
  # deferred for every stale shipped archive; a later clean update reconciles
  # them. Guessing would risk deleting the only surviving copy.
  if (any(!omitted_packages %in% archive_packages)) archives else matched
}

.read_generation_manifest <- function(project_dir) {
  path <- file.path(project_dir, .generation_manifest_name)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

.validate_update_manifest <- function(project_dir,
                                      requested_files = character()) {
  manifest_path <- file.path(project_dir, .generation_manifest_name)
  if (.path_is_symlink(manifest_path)) {
    .bigbang_abort(
      "bigbang_error_symlink_generated_path",
      .bb_trf(
        paste0(
          "Cannot update %s because generated path components are symbolic links: ",
          "%s. Refusing to write outside the project."
        ),
        project_dir, manifest_path
      ),
      path = project_dir, links = manifest_path
    )
  }
  manifest <- .read_generation_manifest(project_dir)
  if (is.null(manifest) || !is.character(manifest$files) ||
        !is.character(manifest$hashes) || anyNA(manifest$files) ||
        any(!nzchar(manifest$files))) {
    .bigbang_abort(
      "bigbang_error_missing_manifest",
      .bb_trf(
        "Cannot update %s because it has no valid bigbang generation manifest.",
        project_dir
      ),
      path = project_dir
    )
  }
  invalid <- manifest$files[grepl(
    "(^/|^[A-Za-z]:[/\\\\]|^~|(^|[/\\\\])\\.\\.([/\\\\]|$))",
    manifest$files, perl = TRUE
  )]
  if (length(invalid) > 0L) {
    .bigbang_abort(
      "bigbang_error_modified_generated_file",
      .bb_trf(
        "Cannot update %s because its manifest contains invalid paths: %s.",
        project_dir, paste(invalid, collapse = ", ")
      ),
      path = project_dir, files = invalid
    )
  }
  if (is.null(manifest$schema) || identical(manifest$schema, 1L)) {
    manifest$files <- .legacy_owned_generation_files(
      project_dir, manifest$files, requested_files
    )
    manifest$hashes <- manifest$hashes[manifest$files]
  }
  .validate_project_write_paths(
    project_dir, c(manifest$files, .generation_manifest_name)
  )
  paths <- file.path(project_dir, manifest$files)
  changed <- manifest$files[
    !file.exists(paths) | vapply(seq_along(paths), function(i) {
      !identical(.file_digest(paths[[i]]), unname(manifest$hashes[[manifest$files[[i]]]]))
    }, logical(1L))
  ]
  if (length(changed) > 0L) {
    .bigbang_abort(
      "bigbang_error_modified_generated_file",
      .bb_trf(
        "Cannot update %s because generated files were modified or removed: %s.",
        project_dir, paste(changed, collapse = ", ")
      ),
      path = project_dir, files = changed
    )
  }
  manifest
}

.stale_unlink <- function(path) {
  unlink(path, recursive = FALSE, force = TRUE)
}

.remove_stale_generation_files <- function(project_dir, files) {
  files <- setdiff(files, .generation_manifest_name)
  if (length(files) == 0L) return(invisible(NULL))

  # The manifest was validated immediately before this call. Recheck the
  # paths so a stale entry can never turn into a write-through symlink during
  # reconciliation. unlink() removes a replaced symlink entry itself rather
  # than following its target.
  .validate_project_write_paths(project_dir, files)
  for (relative in files) {
    path <- file.path(project_dir, relative)
    if (dir.exists(path)) {
      .bigbang_abort(
        "bigbang_error_modified_generated_file",
        .bb_trf(
          "Cannot update %s because generated files were modified or removed: %s.",
          project_dir, relative
        ),
        path = project_dir, files = relative
      )
    }
    .record_update_delete(path)
    if (.stale_unlink(path) != 0L) {
      stop(.bb_trf("Could not remove completely: %s", path), call. = FALSE)
    }
  }
  invisible(NULL)
}

.generation_metadata_findings <- function(components, tolerate = character()) {
  rows <- lapply(components, function(component) {
    stem <- component$stem
    expected_name <- sub("_.*", "", stem)
    has_version <- grepl("_", stem, fixed = TRUE)
    expected_version <- if (has_version) {
      sub("^[^_]+_", "", stem)
    } else {
      NA_character_
    }
    mismatch <- !identical(component$package, expected_name) ||
      (has_version && !.version_matches(component$version, expected_version))
    if (!mismatch) return(NULL)
    data.frame(
      relaxation = "filename_mismatch",
      component = component$package,
      tolerated = "filename_mismatch" %in% tolerate,
      reason = .bb_trf(
        "Archive %s does not match its DESCRIPTION identity (%s %s).",
        component$path, component$package, component$version
      ),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame(
      relaxation = character(), component = character(),
      tolerated = logical(), reason = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

.bigbang_condition <- function(class, message, ..., call = NULL) {
  structure(
    c(list(message = message, call = call), list(...)),
    class = c(class, "bigbang_condition", "condition")
  )
}

.bigbang_abort <- function(class, message, ..., call = NULL) {
  condition <- .bigbang_condition(class, message, ..., call = call)
  class(condition) <- c(class, "bigbang_error", "error", "condition")
  stop(condition)
}

.validate_tolerate <- function(tolerate) {
  if (!is.character(tolerate) || anyNA(tolerate) || any(!nzchar(tolerate))) {
    .bigbang_abort(
      "bigbang_error_tolerance",
      .bb_tr("'tolerate' must be a character vector of named relaxations")
    )
  }
  tolerate <- unique(tolerate)
  unknown <- setdiff(tolerate, .allowed_tolerations)
  if (length(unknown) > 0L) {
    .bigbang_abort(
      "bigbang_error_tolerance",
      .bb_trf(
        "Unknown tolerance(s): %s. Supported values are: %s.",
        paste(unknown, collapse = ", "),
        paste(.allowed_tolerations, collapse = ", ")
      ),
      unknown = unknown
    )
  }
  tolerate
}

.validate_reexport_options <- function(reexport, prefer, exclude) {
  if (!is.character(exclude) || anyNA(exclude) || any(!nzchar(exclude))) {
    .bigbang_abort(
      "bigbang_error_reexport_exclude",
      .bb_tr("'reexport_exclude' must be a character vector of non-empty symbols")
    )
  }
  if (!is.character(prefer) || anyNA(prefer) || any(!nzchar(prefer))) {
    .bigbang_abort(
      "bigbang_error_reexport_prefer",
      .bb_tr("'reexport_prefer' must be a named character vector with one component per symbol")
    )
  }
  if (length(prefer) > 0L) {
    prefer_names <- names(prefer)
    if (is.null(prefer_names) || length(prefer_names) != length(prefer) ||
          anyNA(prefer_names) || any(!nzchar(prefer_names)) ||
          anyDuplicated(prefer_names)) {
      .bigbang_abort(
        "bigbang_error_reexport_prefer",
        .bb_tr("'reexport_prefer' must have non-empty, unique names")
      )
    }
  }
  if (!isTRUE(reexport) && (length(prefer) > 0L || length(exclude) > 0L)) {
    .bigbang_abort(
      "bigbang_error_reexport_options",
      .bb_tr(
        "'reexport_prefer' and 'reexport_exclude' require reexport = TRUE"
      )
    )
  }
  invisible(list(prefer = prefer, exclude = exclude))
}

# Keep rollback's filesystem operation behind a package binding so its
# defensive failure path can be tested without replacing base::unlink globally.
.rollback_unlink <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
}

.restore_documentation_session <- function(search_before, namespaces_before,
                                           generated_name) {
  new_search_entries <- setdiff(search(), search_before)
  for (entry in rev(new_search_entries)) {
    try(detach(entry, character.only = TRUE, unload = FALSE), silent = TRUE)
  }

  if (generated_name %in% setdiff(loadedNamespaces(), namespaces_before) &&
        "devtools" %in% loadedNamespaces()) {
    try(devtools::unload(generated_name, quiet = TRUE), silent = TRUE)
  }

  repeat {
    new_namespaces <- setdiff(loadedNamespaces(), namespaces_before)
    if (length(new_namespaces) == 0L) break

    count_before <- length(new_namespaces)
    for (namespace in rev(new_namespaces)) {
      try(unloadNamespace(namespace), silent = TRUE)
    }
    if (length(setdiff(loadedNamespaces(), namespaces_before)) >= count_before) {
      break
    }
  }

  remaining <- setdiff(loadedNamespaces(), namespaces_before)
  if (length(remaining) > 0L) {
    importers <- unique(unlist(lapply(remaining, function(namespace) {
      packages <- setdiff(loadedNamespaces(), namespaces_before)
      packages[vapply(packages, function(package) {
        imports <- tryCatch(
          getNamespaceInfo(asNamespace(package), "imports"),
          error = function(e) list()
        )
        namespace %in% names(imports)
      }, logical(1L))]
    }), use.names = FALSE))
    if (length(importers) > 0L) {
      .bigbang_abort(
        "bigbang_error_unload_namespace",
        .bb_trf(
          "Could not unload component namespace: %s. Restart R before retrying; another package imports it.",
          paste0(remaining, " (imported by ", paste(importers, collapse = ", "), ")",
                 collapse = "; ")
        ),
        namespaces = remaining, importers = importers
      )
    }
  }

  invisible(NULL)
}

#' Build a local meta-package
#'
#' @description
#' Creates the full structure and files of a meta-package that installs, manages
#' and loads a set of locally stored R packages, resolving the dependencies between
#' them with a graph-based (topologically ordered) approach.
#'
#' @param name Character. Name of the meta-package to create (must not contain
#'   underscores `_`).
#' @param packages Character vector. Archive paths or stems of the local
#'   packages to include. An existing file is always used as a path; otherwise
#'   the element is resolved as a stem in `pkg_dir`, e.g. `"myPackage_1.0.0"`.
#'   A bare package name such as `"myPackage"` resolves when exactly one archive
#'   in those directories declares that `Package` identity. Zero matches use the
#'   usual unresolved-archive error; multiple matches are an ambiguity error.
#'   Supported archives that cannot be read during this identity search are
#'   excluded with a warning that names the archive.
#'   Existing paths may come from different directories. A single existing text
#'   file without a recognized archive extension is treated as a manifest, with
#'   one component per line; relative paths in that file are resolved relative
#'   to the manifest directory, absolute paths and `~` paths are used as written,
#'   and bare archive filenames may also be found in `pkg_dir`.
#' @param pkg_dir Character. Optional directory or directories containing local
#'   archives used to resolve stems and bare package names. It is not needed
#'   when every `packages` element is an existing archive path.
#' @param ext Character. Fallback archive extension for stems. Defaults to
#'   `".tar.gz"`; each existing archive path keeps its own extension.
#' @param version Character. Version of the meta-package. Defaults to `"0.1.0"`.
#' @param dest_dir Character. Required destination directory. The function writes
#'   the generated meta-package exclusively inside this directory; there is no
#'   default path. Use `tempdir()` for disposable output.
#' @param reexport Logical flag retained in its original position for
#'   positional-call compatibility. The default `FALSE` attaches installed
#'   components as usual. With `TRUE`, explicit exports read from each
#'   component's NAMESPACE are exposed through read-only active bindings.
#'   Components are never added to `Imports` or `Depends`, so the generated
#'   package still installs offline without them. Evaluating a binding never
#'   throws: if a component is absent, cannot be loaded, or is an older
#'   installation that no longer exports the symbol, it returns a callable
#'   placeholder. Calling it reports the component, installed version, missing
#'   export, and the `<name>_install()` call that repairs the installation.
#'   Namespace inspection is safe for the same reason. For non-function
#'   exports, access therefore returns the placeholder instead of the object
#'   until the component is installed. The same binding then works without
#'   reloading the metapackage. Only explicit `export()` directives become
#'   bindings. S4
#'   classes and methods remain available by loading their component package.
#'   Non-syntactic explicit export names are quoted in the generated NAMESPACE.
#'   An object restored with `readRDS()` does not load a
#'   component by itself, so base R cannot dispatch that component's S3 method
#'   until it is loaded.
#' @param document Logical. If `TRUE`, runs `devtools::document()`
#'   automatically. Defaults to `TRUE`. The planned `man/<name>_*.Rd` and
#'   internal-helper Rd filenames are reserved for generated documentation;
#'   custom Rd files should use different names. A successful documentation run
#'   may adopt a reserved filename into the generation manifest, after which a
#'   later update with `document = FALSE` removes it as generated output.
#' @param verbose Logical. If `TRUE`, shows verbose messages. The default follows
#'   `getOption("bigbang.verbose", interactive())`.
#' @param authors Character. Content for the `Authors@R` field of DESCRIPTION.
#' @param description Character. Description of the meta-package.
#' @param license Character. License of the meta-package.
#' @param additional_deps Character vector. Extra dependencies to add on top of the
#'   ones declared by components. Source-code guesses are diagnostic by default;
#'   use this argument when a guessed dependency should bind in the generated
#'   package.
#' @param ignore_deps Character vector. Dependencies to ignore even if detected.
#' @param import_deps Character vector. Packages that should go in the `Imports`
#'   field of DESCRIPTION rather than `Depends`. Imports are not attached when the
#'   user calls `library()` on the meta-package, but remain available via `::`
#'   (e.g. `dplyr::filter()`), reducing name clashes in the user's workspace.
#' @param force_deps Character vector. Exact package names to use as dependencies,
#'   bypassing automatic detection. If supplied, only these are used as the
#'   meta-package's implicit dependencies.
#' @param workflow Optional named character vector mapping ordered stage labels
#'   to component package names. When supplied, every component must appear once
#'   and a pipeline vignette skeleton is generated.
#' @param include_archives Logical. If `TRUE`, the default, the component
#'   archives are copied into `inst/archives/` of the generated meta-package, so
#'   that the meta-package is the only artifact that has to be distributed and
#'   `<meta>_install()` works with no arguments, without any path being agreed
#'   on beforehand. Components still install only where they can: a Windows
#'   binary archive is refused on other platforms. Shipping the archives also
#'   means redistributing them, so their licenses have to allow it, and it makes
#'   the generated tarball as large as its components: CRAN prefers source
#'   tarballs under 10 MB and does not accept binary executables in them, which
#'   matters only if a generated meta-package is ever submitted there. Set it to
#'   `FALSE` when the archives stay in a shared location that recipients can
#'   reach; then `<meta>_install()` requires an explicit `pkg_dir`.
#' @param tolerate Character vector of explicitly named validation relaxations.
#'   Use `"filename_mismatch"` to silence filename-versus-DESCRIPTION mismatch
#'   warnings, or `"unincluded_local_dep"` to turn an available-but-unincluded
#'   local dependency error into a warning. With the latter relaxation, the
#'   generated metapackage does not ship that dependency: the recipient must
#'   provide it through `pkg_dir` or a repository with `cran_deps = "install"`.
#'   Unknown names are errors. Each applied relaxation is recorded in the
#'   returned `tolerated` table.
#' @param dry_run Logical. If TRUE, resolves and validates components and
#'   returns the planned generation without creating dest_dir or writing a
#'   project. For an update, sibling journal reconciliation is also planned
#'   and reported with its paths and actions without changing those folders.
#' @param on_component_error Character policy for component-level failures:
#'   "abort" (default) stops generation, while "skip" omits the failed
#'   component and transitively omits components that depend on it. When a
#'   failed archive still exposes its DESCRIPTION, propagation uses its declared
#'   `Package`; otherwise the filename-derived name is used and the limitation is
#'   reported. If that fallback name differs from `Package`, a dependent may
#'   fail on the recipient. During an update, omitted inputs never authorize
#'   deletion of a previously shipped archive. When the old component cannot be
#'   identified unambiguously, archive reconciliation is deferred until a clean
#'   update rather than risking the only surviving copy.
#' @param update Logical. If TRUE, update a previously generated project only
#'   when its bigbang manifest is present and all generated files are unchanged.
#'   A planned file absent from both the manifest and the project is a new
#'   generated file and is added. A planned file already present outside the
#'   manifest is treated as user content and makes the update fail without
#'   touching it. Files outside that manifest are never touched. Updates are refused when the
#'   generated project root, a manifest file, or any path component inside the
#'   project is a symbolic link, so writes cannot escape the project tree.
#'   Generated files
#'   no longer in the plan are reported in `removed_files`. Removing a component
#'   also removes its shipped archive, which may be the last available copy.
#'   Before changing the project, an update backs up every generated file and
#'   its manifest. A failed update restores that state so the same update can be
#'   retried. Updates take an exclusive project lock across preparation,
#'   reconciliation, generation, rollback, and journal publication. A second
#'   session preserves a live preparation and reports `recover = TRUE` as the
#'   next action until the owner is proven to have finished. Documentation files
#'   requested by `document = TRUE` follow the same rule: absent planned files
#'   are added, while existing untracked files are refused. See `document` for
#'   the reserved generated-documentation filenames.
#' @param install_upgrade Character default upgrade policy emitted in the
#'   generated installer function: "newer", "always", or "never".
#'   This controls whether a generated installer keeps newer installed
#'   versions, reinstalls every component, or skips archive inspection.
#' @param reexport_prefer Named character vector mapping symbols to the one
#'   component that should provide them when `reexport = TRUE`, for example
#'   `c(filter = "componentb")`. Names and values must be non-empty and each
#'   named component must be included and export the mapped symbol.
#' @param reexport_exclude Character vector of symbols that must not be
#'   re-exported. Symbols are validated against the explicit exports of the
#'   included components and cannot also appear in `reexport_prefer`.
#' @param recover Logical. With `update = TRUE`, request recovery after the
#'   durable journal owner has been confirmed finished, or when a file has user
#'   content that is neither the original nor an intended update value. Unknown
#'   content is copied byte for byte to a new preserved directory beside the
#'   project before recovery; that directory is reported and is never removed
#'   automatically. A proven-live owner still blocks mutation; an uncertain
#'   owner can be reclaimed only with `recover = TRUE`. Defaults to `FALSE`.
#' @param debug Logical. If `TRUE`, emits detailed debugging messages. Defaults
#'   to `FALSE`.
#'
#' @return Invisibly, a `bigbang_result` containing the generated path,
#'   component archives, dependency classification, applied tolerations,
#'   files removed by the call, documentation status, the `reexports` table,
#'   `reexport_excluded` symbols, and whether an interrupted update was
#'   recovered. Recovery details include any directory used to preserve unknown
#'   user content and the sibling-journal reconciliation plan.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Creates the basic R package structure (`R`, `man`, `vignettes`, etc.).
#' 2. Detects dependencies between packages, both explicit (from DESCRIPTION) and
#'    possible implicit uses (found by scanning executable source tokens). The
#'    latter are reported for diagnosis and are not hard dependencies unless
#'    explicitly supplied through `additional_deps` or `force_deps`.
#' 3. Generates DESCRIPTION and NAMESPACE with the appropriate dependencies.
#' 4. Creates a basic vignette documenting the meta-package.
#' 5. Generates R files with functions to install and load the component packages:
#'    - `<name>_install()`: installs the component packages from the local archives.
#'    - `<name>_attach()`: attaches the components that are already installed.
#'    - `<name>_detach()`: detaches all the meta-package's components.
#'    - `<name>_packages()`: lists the included packages.
#'
#' Installation is **explicit**: calling `library(<meta>)` attaches the components
#' that are already installed and reports which ones are missing, but does not
#' install anything or delete any files. To install the components from the local
#' archives, the user calls `<meta>_install()`. Installation resolves dependencies
#' with a graph-based topological ordering that also detects circular dependencies.
#'
#' Generation validates every supplied component and its dependency graph eagerly
#' before writing the metapackage. This hard validation protects an artifact that
#' will be distributed to another machine. The installer is more tolerant: when
#' an already installed component does not need to be changed, it can retain that
#' installation without reading an archive that will not be used.
#'
#' @section Validation strictness:
#' During generation, validations that protect the recipient cannot be disabled:
#' malformed or
#' unsafe archives, invalid component metadata, duplicate components, cycles,
#' and unsatisfied local version constraints remain hard errors. Checks about
#' project tidiness can be relaxed individually through `tolerate`; there is no
#' switch that disables validation as a whole. bigbang does not run
#' `R CMD check` on component packages, so component warnings and notes do not
#' prevent generation.
#' Component source directories are built in a temporary directory with the
#' optional pkgbuild package; passing an already built archive avoids that
#' optional dependency.
#'
#' @section Component installation:
#' The generated meta-package installs component packages only when the user
#' explicitly calls `<meta>_install()`. Loading it with `library()` never installs
#' packages. By default, the generated installer does not access a repository.
#'
#' With `include_archives = TRUE`, the default, the component archives travel
#' inside the generated meta-package and `pkg_dir` defaults to
#' `system.file("archives", package = "<meta>")`. That default is resolved when
#' the installer is called, so it points at the library of whoever installed the
#' meta-package: recipients need nothing beyond the meta-package itself, and no
#' path has to be agreed on between machines. Network access is needed only when
#' a component depends on a package that must come from a repository, which
#' happens exclusively under `cran_deps = "install"`.
#'
#' Loading the generated meta-package attaches installed components, so their
#' exported functions can be called directly or through `component::function()`.
#' With `reexport = TRUE`, explicit component exports are instead exposed through
#' read-only active bindings in the meta-package namespace. This does not add
#' components to `Imports` or `Depends`: loading remains possible without them,
#' and a binding resolves the component on every access. Evaluating a binding
#' never throws: if a component is absent, cannot be loaded, or is an older
#' installation that no longer exports the symbol, it returns a callable
#' placeholder. Calling it reports the component, installed version, missing
#' export, and the `<name>_install()` call that repairs the installation. This
#' also keeps namespace inspection safe. Only explicit `export()` directives
#' are rebound; S4 classes and methods are used through
#' the loaded component namespace. An object restored with `readRDS()` cannot
#' load a component by itself, so base R cannot dispatch that component's S3
#' method until the component has been loaded.
#'
#' @section Re-export collisions:
#' When more than one component exports a symbol, `reexport_prefer` chooses its
#' provider explicitly and `reexport_exclude` removes it from the generated
#' namespace. Every collision requires one of those options because static
#' source analysis cannot prove that two exported objects are the same at
#' runtime. The analysis remains as a diagnostic with
#' `probable_same_object`, `distinct_definitions`, or `undetermined`, including
#' ordered file, line, import, and parse reasons. For a preferred
#' `probable_same_object`, `<name>_install()` verifies the installed owners in
#' a clean R subprocess whose destination library is first in `.libPaths()`.
#' If that subprocess cannot run, the result is explicitly unverified and never
#' reports a false identity. A namespace already loaded from another library is
#' reported before the clean verification starts. Missing owners remain
#' unverified and the verification is retained in the returned result. The
#' diagnostic is a help, not the guarantee: the guarantee is the explicit
#' `reexport_prefer` or `reexport_exclude` decision plus that verification.
#' Calling `library(<meta>)` alone does not verify installed owners. The scanner
#' is deliberately conservative and can count a never-forced `delayedAssign`,
#' an `if (FALSE)` branch, or a `reg.finalizer()` body; this overcount does not
#' weaken the explicit decision and installation-verification guarantee.
#' `<name>_conflicts()` repeats that check on request. Its masking-conflict
#' names remain ordinary symbols; use
#' `<name>_reexport_verification(conflicts)` to access the verification
#' attribute without a name collision. If `on_component_error = "skip"` omits a component required by a preferred
#' binding or an import source, generation errors with an actionable skipped
#' condition instead of creating a binding to a component that will not travel
#' with the metapackage.
#' With `reexport = TRUE`, `<name>_conflicts()` retains the masking-conflict
#' list from earlier releases and stores its installed-owner table as an
#' attribute. The accessor keeps the same `<name>_reexport_verification` class
#' when it has zero rows.
#'
#' @section Interrupted updates:
#' Before an in-place update mutates the project, bigbang assembles a durable
#' journal beside it in a private `.<name>.bigbang-update.armando-*` folder.
#' The marker is written before the backup, and the complete folder is renamed
#' to `.<name>.bigbang-update` only after every hash has been verified.
#' Every later file write or removal records its intention first. Generated
#' files, shipped component archives, catalogs, `.Rbuildignore`, and the final
#' manifest are replaced atomically. On Windows the guarantee is that a file is
#' old, new, or temporarily absent with a journal backup. Roxygen runs in a staging copy and only its
#' known outputs are promoted atomically to the project.
#'
#' Updates also publish `.<name>.bigbang-update.lock` atomically from a sibling
#' temporary folder that already contains a complete `owner.rds`. A published
#' lock therefore always has an owner. Reclaiming an orphan first atomically
#' renames it to a unique discarded name; only the process that wins that
#' rename may publish a replacement, and it rechecks the owner before doing so.
#' Lock disposition is owner-first. For the published lock, a proven live owner
#' blocks every caller; an uncertain owner blocks without `recover = TRUE` and
#' is reclaimable only with `recover = TRUE`; a proven dead owner is reclaimable.
#' For a discarded lock, a proven live `owner.rds` is restored when the lock
#' name is free or blocks on its PID when it is occupied. It is never deleted.
#' An uncertain discarded owner follows the uncertain-lock rule. Only after the
#' discarded owner is proven dead does the claimant decide the outcome: a live
#' claimant blocks, an uncertain claimant needs `recover = TRUE`, and a dead
#' claimant may be discarded. `owner.rds` and `claim.rds` are removed only when
#' their bytes still have the digest observed for that decision; mismatches are
#' preserved by setting the entry aside. No claim is written before the owner
#' has been re-read immediately before publication. The update also revalidates
#' its published owner before creating the journal, recording each intent, and
#' completing an irreversible step. If the owner changed, it aborts before the
#' next mutation. A discarded entry can therefore contain a claimant record,
#' but that record is never allowed to override a live owner.
#' For a `.lock.armando-*` entry, a live owner stays in place and blocks;
#' an uncertain owner stays in place without `recover = TRUE` and is set aside
#' with `recover = TRUE`; a dead or missing owner is set aside. A symbolic link
#' at the published lock name is reported as a link without an update-running
#' claim; with `recover = TRUE` the link itself is renamed aside and its target
#' is not followed.
#' A regular file or other user entry at the lock name is atomically set aside
#' as `.<name>.bigbang-apartado-*`. Lock preparations left by an interruption
#' are recognized on the next call and set aside without deleting their bytes.
#' These names are reserved bigbang siblings: `.<name>.bigbang-update`,
#' `.<name>.bigbang-update.armando-*`, `.<name>.bigbang-update.lock`,
#' `.<name>.bigbang-update.lock.armando-*`,
#' `.<name>.bigbang-update.lock.descartado-*`,
#' `.<name>.bigbang-update.descartado-*`, and `.<name>.bigbang-apartado-*`.
#'
#' If a process dies while preparing the journal, an empty unmarked
#' `armando-*` folder is removed; any non-empty unmarked folder is atomically
#' set aside as `.<name>.bigbang-apartado-*` without copying or deleting bytes.
#' The initial marker records the owner PID, host, process start token,
#' and start time before the first backup copy. On Linux, liveness reads
#' `/proc/<pid>` and treats a missing process as dead, `Z` or `X` in
#' `/proc/<pid>/stat` as dead, and any other readable state as existing; the
#' process-start token still decides identity. Without `/proc`, `kill(pid, 0)`
#' proves existence only when it succeeds; if that probe is unavailable,
#' `LC_ALL=C ps -p <pid>` establishes whether the PID is present or absent, and
#' `ps -o lstart= -p <pid>` supplies the portable start token. The token source
#' is stored (`proc` or `ps`) and mismatched sources never compare equal. A
#' failure is dead only when `ps -p` also proves that the PID is absent;
#' permission errors, an unavailable `ps`, and an unreadable token are uncertain.
#' The exact policy is:
#' dead means the process does not exist or is `Z`/`X`; alive means it exists,
#' is not terminal, and its start token matches; live-token-conflict means it
#' exists but the token differs; uncertain means existence or identity cannot be
#' proved. `recover = TRUE` may claim or set aside uncertain entries, but never
#' overrides a proven live owner. A process of another user is therefore never
#' inferred dead from `EPERM`. Journal disposal first writes an atomic tombstone with
#' the exact relative-path and MD5 inventory of the entries bigbang wrote, then
#' renames the folder to `.<name>.bigbang-update.descartado-*`; cleanup can
#' therefore resume after another interruption. Cleanup checks every file
#' recursively and removes it only when its relative path and MD5 match the
#' inventory; it removes an inventory directory only after it is empty. Before
#' destructive cleanup the journal is renamed to an unpredictable private
#' sibling after verifying it is not a link, and each deletion revalidates its
#' ancestors and MD5 immediately before `unlink()`. Any
#' file, directory, or symbolic link that cannot be proved to be in the
#' inventory causes the whole discarded folder to be set aside atomically and
#' reported, so the update continues without deleting user bytes. The tombstone
#' has a digest recorded beside it before the rename; a missing or changed
#' digest is set aside rather than trusted. A discarded folder without a valid
#' tombstone is set aside when non-empty; an empty one is removed as an
#' interrupted cleanup shell. A matching name and manifest are required before
#' a discarded folder is cleaned. A stale generation or another project is
#' therefore set aside beside the current project and never blocks a later
#' update. A file with the same path and MD5 as the inventory is an unavoidable
#' limit: its bytes are identical, so deleting it loses no content, but the
#' journal cannot prove who created it. The tombstone and its digest are local
#' journal state, not a cryptographic signature; treat the journal as bigbang's
#' private territory. A process of the same user with write permission can forge
#' `owner.rds`, `marker.rds`, or `state.rds`; that is outside this integrity
#' model. R has no `unlinkat()`/`O_NOFOLLOW`, so a same-user process that actively
#' replaces journal directories during discard remains an integrity boundary;
#' the remaining race is the interval between the last revalidation and
#' `unlink()`. As a cheap consistency check, an armed journal is recoverable only when
#' the owner fields in `state.rds` match those in `marker.rds`; otherwise the
#' journal is set aside and is never used for rollback.
#'
#' The journal is designed to survive process interruptions such as SIGKILL, an
#' R error, or Ctrl-C. It does not promise fsync durability against an OS or
#' power shutdown. The next
#' `create_metapackage(update = TRUE)` call examines it before validating the
#' generation manifest. The marker identifies the metapackage and old-manifest
#' hash rather than an absolute path, so moving the project together with its
#' journal remains recoverable. Renaming a project is not supported: generated
#' file names contain the metapackage name. Rename the project and its journal
#' back to `<name>` before updating. A byte-for-byte copy placed at the same
#' path and name as the moved original is indistinguishable from that original;
#' the journal consequently treats it as the project. If the original project
#' still exists beside a copied journal, the journal is not adopted or changed.
#' A partial tombstone temporary is set aside after the owner is confirmed dead,
#' and recovery continues. An already completed update is recognized by its new
#' manifest;
#' otherwise a dead owner's changes are rolled back and the requested update
#' continues. On POSIX systems liveness uses the PID and, where Linux `/proc`
#' exposes it, the process start time. Windows is never probed with
#' the process-termination helper because that operation terminates a process. A dry run
#' evaluates and reports the lock as free, live, orphaned, or uncertain without
#' acquiring, reclaiming, renaming, or deleting any lock entry.
#'
#' Automatic recovery proceeds only when every affected path contains its
#' original bytes, intended bytes, or an expected absence. Other content raises
#' `bigbang_error_interrupted_update`; `recover = TRUE` preserves it outside the
#' project before rollback. Recovery is idempotent, so another interruption can
#' be recovered by a later call. `dry_run = TRUE` reports the pending action and
#' leaves the project and every sibling journal folder untouched. A handled
#' error uses this same journal for immediate rollback and retains it if
#' verification cannot finish. Documentation generation failures in the staging
#' copy are warnings; a failure while promoting any documentation output aborts
#' the update and rolls the complete project back through the journal.
#'
#' @section Requirements:
#' - Each component must be an existing archive path or a stem resolvable in
#'   one of the optional `pkg_dir` directories; `ext` is only a fallback for
#'   stems.
#' - Files in the supplied archive directories that cannot be read are excluded
#'   from the inventory with a warning. A requested component still fails
#'   validation, while an unreadable file matching a declared dependency is
#'   reported as an unavailable local archive.
#' - Automatic documentation (`document = TRUE`) requires the
#'   `devtools` package.
#'
#' @examples
#' archives <- system.file("extdata", package = "bigbang")
#' destination <- tempfile("bigbang-example-")
#' dir.create(destination)
#'
#' result <- create_metapackage(
#'   name = "toyverse",
#'   packages = "toycomponent_0.1.0",
#'   pkg_dir = archives,
#'   dest_dir = destination,
#'   document = FALSE,
#'   verbose = FALSE,
#'   import_deps = character(),
#'   force_deps = character()
#' )
#' list.files(result$path)
#'
#' unlink(destination, recursive = TRUE)
#' @export

create_metapackage <- function(
  name,
  packages,
  pkg_dir = NULL,
  ext = ".tar.gz",
  version = "0.1.0",
  dest_dir,
  reexport = FALSE,
  document = TRUE,
  verbose = getOption("bigbang.verbose", interactive()),
  authors = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
  description = "Local Package Metapackage",
  license = "MIT + file LICENSE",
  additional_deps = NULL,
  ignore_deps = NULL,
  import_deps = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts", "zoo"),
  force_deps = NULL,
  debug = FALSE,
  # Arguments added after 0.1.0 go last, so that a positional call written
  # against 0.1.0 keeps binding to the same parameters.
  workflow = NULL,
  include_archives = TRUE,
  tolerate = character(),
  dry_run = FALSE,
  on_component_error = c("abort", "skip"),
  update = FALSE,
  install_upgrade = c("newer", "always", "never"),
  reexport_prefer = character(),
  reexport_exclude = character(),
  recover = FALSE
) {
  verbose <- isTRUE(verbose)
  debug <- isTRUE(debug)
  on_component_error <- match.arg(on_component_error)
  install_upgrade <- match.arg(install_upgrade)
  if (!is.logical(reexport) || length(reexport) != 1L || is.na(reexport)) {
    stop(.bb_tr("'reexport' must be TRUE or FALSE"), call. = FALSE)
  }
  .validate_reexport_options(reexport, reexport_prefer, reexport_exclude)

  # Validate public arguments before touching the filesystem.
  if (missing(dest_dir) || is.null(dest_dir) ||
        !is.character(dest_dir) || length(dest_dir) != 1L ||
        is.na(dest_dir) || !nzchar(dest_dir)) {
    stop(.bb_tr(paste0(
      "'dest_dir' must be supplied as one non-empty path: the meta-package is ",
      "written inside it. Use tempdir() for disposable output."
    )), call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1) {
    stop(.bb_tr("'name' must be one character string"), call. = FALSE)
  }
  if (!is.character(packages) || length(packages) < 1) {
    stop(.bb_tr("'packages' must be a non-empty character vector"), call. = FALSE)
  }
  if (!is.null(pkg_dir) &&
        (!is.character(pkg_dir) || anyNA(pkg_dir) || any(!nzchar(pkg_dir)))) {
    stop(.bb_tr("'pkg_dir' must contain one or more non-empty paths"), call. = FALSE)
  }
  if (!is.logical(include_archives) || length(include_archives) != 1L ||
        is.na(include_archives)) {
    stop(.bb_tr("'include_archives' must be TRUE or FALSE"), call. = FALSE)
  }
  tolerate <- .validate_tolerate(tolerate)
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop(.bb_tr("'dry_run' must be TRUE or FALSE"), call. = FALSE)
  }
  if (!is.logical(update) || length(update) != 1L || is.na(update)) {
    stop(.bb_tr("'update' must be TRUE or FALSE"), call. = FALSE)
  }
  if (!is.logical(recover) || length(recover) != 1L || is.na(recover)) {
    stop(.bb_tr("'recover' must be TRUE or FALSE"), call. = FALSE)
  }
  if (isTRUE(recover) && !isTRUE(update)) {
    stop(.bb_tr("'recover' requires update = TRUE"), call. = FALSE)
  }

  # Resolve caller-supplied paths before any generated files are written. A
  # component may be an existing archive path or a stem resolved in pkg_dir.
  dest_dir <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)

  # Validate the package name.
  if (grepl("_", name)) {
    suggested_name <- gsub("_", ".", name)
    stop(.bb_trf(
      "Package name '%s' contains underscores, which R package names do not allow. Use '%s' instead.",
      name, suggested_name
    ), call. = FALSE)
  }
  # The name becomes a directory under 'dest_dir', so anything that is not a
  # legal package name is rejected before touching the filesystem. Otherwise a
  # name carrying path separators or a parent reference would place the
  # generated tree outside the requested destination.
  # R ships these names, so a meta-package cannot take one: R CMD build would
  # reject it later, with a message that does not point back here.
  if (name %in% .r_standard_packages) {
    stop(.bb_trf(
      "Package name '%s' belongs to R itself and cannot be reused.", name
    ), call. = FALSE)
  }
  if (!grepl("^[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$", name)) {
    stop(.bb_trf(paste0(
      "Package name '%s' is not a valid R package name: use at least two ",
      "characters, start with a letter, continue with letters, digits or dots, ",
      "and do not end with a dot."
    ), name), call. = FALSE)
  }

  project_path <- file.path(dest_dir, name)
  if (isTRUE(update)) .validate_project_root_path(project_path)
  project_dir <- normalizePath(project_path, winslash = "/", mustWork = FALSE)
  update_lock <- NULL
  if (isTRUE(update) && dir.exists(project_dir)) {
    update_lock <- .acquire_update_lock(
      project_dir, recover = recover, dry_run = dry_run
    )
    if (!isTRUE(dry_run)) {
      on.exit(.release_update_lock(update_lock), add = TRUE)
    }
  }
  recovery <- list(pending = FALSE, recovered = FALSE, preserved = NULL,
                   restored_absent = character(), lock = update_lock)
  if (isTRUE(update)) {
    sibling_reconciliation <- .reconcile_update_siblings(
      project_dir, name, dry_run = dry_run, recover = recover
    )
    recovery <- .recover_pending_update(
      project_dir, name, recover = recover, dry_run = dry_run
    )
    recovery$reconciliation <- sibling_reconciliation
    if (isTRUE(dry_run) && isTRUE(recovery$pending)) {
      message(.bb_trf("Dry run: pending update journal action is %s at %s.",
                      recovery$action, .update_journal_path(project_dir)))
      return(invisible(structure(list(
        path = project_dir, name = name, packages = character(),
        archives = character(), components = list(), reexports = data.frame(),
        reexport_excluded = character(), order = character(),
        files = character(), added_files = character(),
        removed_files = character(), findings = list(),
        local_dependencies = character(), cran_dependencies = character(),
        implicit_dependencies = character(), tolerated = character(),
        omitted = data.frame(), workflow = workflow, documented = FALSE,
        dry_run = TRUE, updated = FALSE, recovered = FALSE,
        recovery = recovery
      ), class = "bigbang_result")))
    }
  }

  resolved_components <- .resolve_components(
    packages, pkg_dir, ext, on_component_error = on_component_error,
    reexport = isTRUE(reexport)
  )
  validated <- .validate_generation(
    resolved_components, tolerate = tolerate,
    on_component_error = on_component_error,
    reexport = isTRUE(reexport), metapackage_name = name,
    reexport_prefer = reexport_prefer,
    reexport_exclude = reexport_exclude
  )
  resolved_components <- validated$resolved
  validation <- validated$validation
  omitted <- validated$omitted
  components <- validation$components
  tolerated <- validation$tolerated
  reexport_plan <- validated$reexport
  component_packages <- vapply(components, `[[`, character(1L), "package")
  archive_stems <- vapply(components, `[[`, character(1L), "stem")
  archive_paths <- vapply(components, `[[`, character(1L), "path")
  archive_names <- vapply(components, .canonical_archive_name, character(1L))
  source_components <- vapply(
    components, function(x) !is.null(x$source_dir), logical(1L)
  )
  if (!isTRUE(include_archives) && any(source_components)) {
    stop(.bb_tr(paste0(
      "Source directory components require include_archives = TRUE because their ",
      "temporary build archive cannot be reused."
    )), call. = FALSE)
  }
  if (!is.null(workflow)) {
    valid_workflow <- is.character(workflow) && length(workflow) > 0L &&
      !is.null(names(workflow)) && all(nzchar(names(workflow))) &&
      !any(grepl("\\r|\\n", names(workflow), perl = TRUE)) &&
      !anyDuplicated(names(workflow)) && !anyDuplicated(unname(workflow)) &&
      setequal(unname(workflow), component_packages)
    if (!isTRUE(valid_workflow)) {
      .bigbang_abort(
        "bigbang_error_workflow",
        .bb_tr(
          "'workflow' must map unique non-empty stage names to every component package exactly once"
        )
      )
    }
  }
  r_requirement <- .resolve_r_requirement(components)
  if (!isTRUE(include_archives) && length(resolved_components$source_dirs) > 1L) {
    warning(.bb_trf(
      paste0(
        "This meta-package needs the following archive directories on the ",
        "recipient: %s. Set include_archives = TRUE to avoid this requirement."
      ),
      paste(resolved_components$source_dirs, collapse = ", ")
    ), call. = FALSE)
  }

  # Resolve dependency diagnostics before creating the destination. This keeps
  # dry_run genuinely read-only and ensures all preflight failures happen before
  # the generated project exists.
  if (!is.null(force_deps)) {
    detected_implicit_deps <- character()
  } else {
    detected_implicit_deps <- detect_implicit_dependencies(
      resolved_components$packages, resolved_components$pkg_dir, ext,
      components = components
    )
  }
  hard_implicit_deps <- unique(c(
    if (is.null(force_deps)) character() else force_deps,
    if (is.null(additional_deps)) character() else additional_deps
  ))
  if (!is.null(ignore_deps) && length(ignore_deps) > 0L) {
    hard_implicit_deps <- setdiff(hard_implicit_deps, ignore_deps)
    if (is.null(force_deps)) {
      detected_implicit_deps <- setdiff(detected_implicit_deps, ignore_deps)
    }
  }
  dependencies <- unlist(lapply(components, function(x) x$dependencies),
                         use.names = FALSE)
  classified_deps <- classify_dependencies(
    dependencies, included_packages = component_packages
  )
  cran_deps <- unique(setdiff(classified_deps$cran, "utils"))
  local_deps <- classified_deps$local

  if (verbose) {
    if (!is.null(force_deps)) {
      message(.bb_trf(
        "Using explicitly supplied dependencies: %s",
        paste(force_deps, collapse = ", ")
      ))
    } else {
      message(.bb_tr("Scanning local packages for implicit dependencies..."))
      message(.bb_trf(
        "Detected implicit dependencies: %s",
        paste(detected_implicit_deps, collapse = ", ")
      ))
    }
  }

  update_manifest <- NULL
  added_files <- character()
  stale_files <- character()
  preserved_files <- character()
  requested_files <- setdiff(
    .planned_generation_files(
      name, resolved_components$components, workflow,
      include_archives, license = license, document = document,
      reexport = isTRUE(reexport),
      reexport_symbols = reexport_plan$table$symbol
    ),
    .generation_manifest_name
  )
  if (isTRUE(update)) {
    if (!dir.exists(project_dir)) {
      .bigbang_abort(
        "bigbang_error_missing_manifest",
        .bb_tr(
          "Cannot update a project that does not exist or has no bigbang generation manifest."
        ),
        path = project_dir
      )
    }
    update_manifest <- .validate_update_manifest(
      project_dir, requested_files
    )
    outside_manifest <- setdiff(requested_files, update_manifest$files)
    .validate_project_write_paths(project_dir, outside_manifest)
    outside_paths <- file.path(project_dir, outside_manifest)
    already_present <- vapply(
      outside_paths,
      function(path) {
        file.exists(path) || dir.exists(path) || .path_is_symlink(path)
      },
      logical(1L)
    )
    untracked <- outside_manifest[already_present]
    added_files <- outside_manifest[!already_present]
    if (length(untracked) > 0L) {
      .bigbang_abort(
        c("bigbang_error_untracked_generated_file",
          "bigbang_error_modified_generated_file"),
        .bb_trf(
          paste0(
            "Cannot update %s because planned generated files already exist ",
            "outside its manifest and may belong to the user: %s. Refusing ",
            "to overwrite them."
          ),
          project_dir, paste(untracked, collapse = ", ")
        ),
        path = project_dir, files = untracked
      )
    }
    stale_files <- setdiff(update_manifest$files, requested_files)
    preserved_files <- .preserve_omitted_archives(stale_files, omitted)
    stale_files <- setdiff(stale_files, preserved_files)
  }
  generation_findings <- list(
    metadata = .generation_metadata_findings(components, tolerate),
    tolerated = tolerated,
    omitted = omitted
  )
  if (isTRUE(dry_run)) {
    result <- structure(list(
      path = project_dir,
      name = name,
      packages = component_packages,
      archives = archive_stems,
      components = components,
      reexports = reexport_plan$table,
      reexport_excluded = reexport_plan$excluded,
      order = .component_topological_order(components),
      files = .planned_generation_files(
        name, components, workflow, include_archives,
        license = license, document = document, reexport = isTRUE(reexport),
        reexport_symbols = reexport_plan$table$symbol
      ),
      added_files = added_files,
      removed_files = stale_files,
      findings = generation_findings,
      local_dependencies = local_deps,
      cran_dependencies = cran_deps,
      implicit_dependencies = detected_implicit_deps,
      tolerated = tolerated,
      omitted = omitted,
      workflow = workflow,
      documented = FALSE,
      dry_run = TRUE,
      updated = FALSE,
      recovered = isTRUE(recovery$recovered),
      recovery = recovery
    ), class = "bigbang_result")
    return(invisible(result))
  }

  # Debug logger.
  log_debug <- function(debug_message) {
    if (debug) message(.bb_trf("DEBUG: %s", debug_message))
  }

  log_debug("Starting create_metapackage()")


  project_created <- FALSE
  destination_created <- !dir.exists(dest_dir)
  generation_complete <- FALSE
  if (isTRUE(update)) {
    .assert_update_lock_owner(update_lock, project_dir, "update")
  }
  update_journal <- if (isTRUE(update)) {
    .create_update_journal(
      project_dir, name, update_manifest,
      extra_files = setdiff(requested_files, update_manifest$files),
      lock = update_lock
    )
  } else {
    NULL
  }
  if (!is.null(update_journal)) {
    .activate_update_journal(
      update_journal, project_dir, name, lock = update_lock
    )
  }
  documentation_search <- NULL
  documentation_namespaces <- NULL
  documentation_files <- if (isTRUE(document)) {
    c(
      .planned_documentation_files(name, reexport = reexport),
      if (isTRUE(reexport) && nrow(reexport_plan$table) > 0L) {
        file.path("man", "reexports.Rd")
      } else {
        character()
      }
    )
  } else {
    character()
  }
  on.exit({
    if (!is.null(documentation_search)) {
      .restore_documentation_session(
        documentation_search, documentation_namespaces, name
      )
    }
    .deactivate_update_journal()

    # Roll back only a project directory created by this exact invocation.
    # Pre-existing directories, including empty ones, are never removed.
    #
    # Both sides of the comparison are normalised here, at the same moment, and
    # never against a value captured earlier. normalizePath() returns a path that
    # does not exist unchanged and resolves one that does, so a value normalised
    # before creation cannot be compared with one normalised after it: as soon as
    # any component of the path is a symbolic link the two differ and the
    # rollback silently declines. That is the situation on macOS, where tempdir()
    # sits under /var, itself a link to /private/var.
    actual_project <- normalizePath(
      project_dir, winslash = "/", mustWork = FALSE
    )
    expected_project <- normalizePath(
      file.path(dest_dir, name), winslash = "/", mustWork = FALSE
    )
    owned_project <- project_created &&
      identical(actual_project, expected_project) &&
      is_path_inside(actual_project, dest_dir)
    if (!generation_complete && owned_project && dir.exists(actual_project)) {
      # unlink removes the directory entry itself and does not follow a
      # symlink replaced during this call's short TOCTOU window.
      removal_status <- .rollback_unlink(actual_project)
      if (removal_status != 0L) {
        warning(.bb_trf("Could not remove completely: %s", actual_project),
                call. = FALSE)
      }
    }
    if (!generation_complete && destination_created && dir.exists(dest_dir) &&
          length(list.files(dest_dir, all.files = TRUE, no.. = TRUE)) == 0L) {
      # The directory is known to be empty; recursive=TRUE is required by
      # unlink() to remove an empty directory on all supported platforms.
      .rollback_unlink(dest_dir)
    }
    if (!generation_complete && !is.null(update_journal) &&
          (is.null(update_lock) || .update_lock_is_owner(update_lock))) {
      restored <- tryCatch({
        .recover_pending_update(
          project_dir, name, recover = TRUE, handled = TRUE
        )
        TRUE
      }, error = function(e) FALSE)
      if (!restored) {
        warning(.bb_trf(
          paste0(
            "The update journal was retained for recovery at: %s. It contains",
            " a backup at %s. Next step: confirm that no other update is running",
            " and call update = TRUE, recover = TRUE."
          ),
          update_journal$path, file.path(update_journal$path, "backup")
        ), call. = FALSE)
      }
    }
  }, add = TRUE, after = FALSE)

  # Reconcile before documentation so stale generated R code cannot be loaded
  # by roxygen. The update backup and rollback above are already active, so a
  # failure here or later restores every pre-existing generated file.
  if (length(stale_files) > 0L) {
    if (isTRUE(update)) {
      .assert_update_lock_owner(update_lock, project_dir,
                                "generated-file reconciliation")
    }
    if (verbose) {
      message(.bb_trf(
        "Removing generated files no longer in the plan: %s",
        paste(stale_files, collapse = ", ")
      ))
    }
    .remove_stale_generation_files(project_dir, stale_files)
  }

  log_debug(glue::glue("New project path: {project_dir}"))

  # In-place regeneration is allowed only when a matching manifest was
  # validated above. Without update=TRUE, the historical safety rule remains.
  if (isTRUE(update)) {
    project_created <- FALSE
  } else if (dir.exists(project_dir)) {
    existing_entries <- list.files(
      project_dir, all.files = TRUE, no.. = TRUE
    )
    if (length(existing_entries) > 0L) {
      .bigbang_abort(
        "bigbang_error_nonempty_dest",
        .bb_trf(
          paste0(
            "For safety, the destination must be new or empty: %s. ",
            "Generate into a new empty path; never regenerate an existing source in place."
          ),
          project_dir
        ),
        path = project_dir
      )
    }
  } else {
    if (verbose) {
      message(.bb_trf("Creating package structure at: %s", project_dir))
    }
    if (!dir.create(project_dir, showWarnings = TRUE, recursive = TRUE)) {
      stop(.bb_trf("Could not create project directory: %s", project_dir),
           call. = FALSE)
    }
    project_created <- TRUE
  }

  for (subdir in c("R", "man", "vignettes")) {
    subdir <- file.path(project_dir, subdir)
    if (!dir.create(subdir, showWarnings = FALSE) && !dir.exists(subdir)) {
      stop(.bb_trf("Could not create directory: %s", subdir), call. = FALSE)
    }
  }
  log_debug("Basic directory structure created")

  # Ship the component archives inside the meta-package so that installing it
  # is enough to install the components wherever it is installed.
  if (isTRUE(include_archives)) {
    archive_dir <- file.path(project_dir, "inst", .archive_subdir)
    if (!dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE) &&
          !dir.exists(archive_dir)) {
      stop(.bb_trf("Could not create directory: %s", archive_dir), call. = FALSE)
    }
    copied <- vapply(seq_along(archive_paths), function(index) {
      tryCatch({
        .atomic_copy(
          archive_paths[[index]],
          file.path(archive_dir, archive_names[[index]])
        )
        TRUE
      }, error = function(e) FALSE)
    }, logical(1L))
    if (!all(copied)) {
      stop(.bb_trf(
        "Could not copy the component archives into the meta-package: %s",
        paste(archive_names[!copied], collapse = ", ")
      ), call. = FALSE)
    }
    total_bytes <- sum(file.size(archive_paths), na.rm = TRUE)
    if (verbose) {
      message(.bb_trf(
        "Component archives copied into the meta-package: %s (%.1f MB).",
        archive_dir, total_bytes / 1024^2
      ))
    }
    log_debug(paste("Component archives copied into", archive_dir))
  }

  # Report verbose when requested.
  if (verbose) {
    message(.bb_trf(
      "Creating metapackage '%s' for %d local packages...",
      name, length(packages)
    ))
    if (length(packages) > 5) {
      message(.bb_trf(
        "Packages: %s... and %d more",
        paste(utils::head(packages, 5), collapse = ", "),
        length(packages) - 5
      ))
    } else {
      message(.bb_trf("Packages: %s", paste(packages, collapse = ", ")))
    }
  }

  # Write DESCRIPTION with the configured dependencies.
  if (verbose) {
    message(.bb_tr("Generating DESCRIPTION and NAMESPACE..."))
  }

  write_description_file(
    name = name,
    version = version,
    implicit_deps = hard_implicit_deps,
    import_deps = import_deps,
    authors = authors,
    description = description,
    license = license,
    component_packages = component_packages,
    description_path = file.path(project_dir, "DESCRIPTION"),
    verbose = debug,
    r_requirement = r_requirement
  )


  # Create the basic vignette after DESCRIPTION exists.
  write_basic_vignette(name, component_packages, project_dir,
                       include_archives = include_archives,
                       reexport = isTRUE(reexport), verbose = debug)
  if (!is.null(workflow)) {
    write_workflow_vignette(name, workflow, project_dir)
  }
  write_metapackage_readme(
    name, project_dir, include_archives, reexport = isTRUE(reexport)
  )
  write_consistency_test(name, project_dir)
  if (debug) {
    log_debug("Basic vignette created for R CMD check")
  }

  # Write NAMESPACE with explicitly selected additional dependencies.
  write_namespace_file(
    name = name,
    namespace_path = file.path(project_dir, "NAMESPACE"),
    implicit_deps = hard_implicit_deps,
    import_deps = import_deps,
    verbose = debug,
    reexport = isTRUE(reexport),
    reexport_symbols = if (isTRUE(reexport)) {
      reexport_plan$table$symbol
    } else {
      character()
    }
  )

  log_debug("NAMESPACE file created")

  # Generate the component installation engine.
  install_packages_content <- .render_install_engine(
    name, components, .archive_dir_default(name, include_archives),
    install_upgrade = install_upgrade
  )

  install_packages_content <- .drop_regular_comment_lines(install_packages_content)
  install_packages_content <- .qualify_generated_runtime_calls(install_packages_content)
  .write_utf8(install_packages_content, file.path(project_dir, "R", "install_packages.R"))
  log_debug("install_packages.R created")

  # Write LICENSE when the declared license requires it.
  if (grepl("file[[:space:]]+LICENSE", license, ignore.case = TRUE)) {
    license_content <- c(
      paste0("YEAR: ", format(Sys.Date(), "%Y")),
      paste0("COPYRIGHT HOLDER: ", .copyright_holders(authors))
    )
    .write_utf8(license_content, file.path(project_dir, "LICENSE"))
    log_debug("LICENSE file created")
  }

  # Runtime translations belong to the generated metapackage, so it receives
  # its own source catalog and a precompiled catalog for environments without
  # gettext build tools. The conditional keeps standalone sourcing of this file
  # useful in the security regression script.
  if (exists(".metapackage_spanish_catalog", mode = "function")) {
    spanish_catalog <- .metapackage_spanish_catalog(name, include_archives)
    .write_po_catalog(
      names(spanish_catalog), NULL,
      file.path(project_dir, "po", paste0("R-", name, ".pot")),
      project = paste(name, version)
    )
    .write_po_catalog(
      names(spanish_catalog), spanish_catalog,
      file.path(project_dir, "po", "R-es.po"),
      project = paste(name, version)
    )
    .write_mo_catalog(
      names(spanish_catalog), spanish_catalog,
      file.path(
        project_dir, "inst", "po", "es", "LC_MESSAGES",
        paste0("R-", name, ".mo")
      )
    )
  }

  # Build the project ignore list, including component archives and sources.
  rbuildignore_content <- c(
    # Basic project patterns
    "^.*\\.Rproj$",        # Any R project file
    "^\\.Rproj\\.user$",   # RStudio state directory
    paste0("^", name, "\\.Rproj$"),

    # Installation and check directories
    "^00LOCK-.*$",
    "^00_pkg_src$",
    "^libs$",
    "^doc$",
    "^Meta$",
    "^tmp$",
    "^temp$",
    "^check$",
    "\\.Rcheck$",

    # Temporary files left by atomic writers after an interrupted generation.
    # The optional directory prefix also covers atomic copies in inst/archives.
    "^(.*/)?\\..*-[[:alnum:]]+$",

    # CI, version-control, and pkgdown files
    "^\\.github$",
    "^_pkgdown\\.yml$",
    "^pkgdown$",
    "^\\.travis\\.yml$",
    "^codecov\\.yml$",
    "^\\.gitignore$",
    "^\\.git$",

    # Package archives anywhere in the tree, except the component archives
    # shipped under inst/archives/, which are part of the meta-package and must
    # reach the tarball. R applies these patterns with perl = TRUE, so the
    # negative lookahead is honoured; (?-i:) keeps the exemption case-sensitive,
    # because R also applies them with ignore.case = TRUE and only the real
    # inst/archives/ is ours.
    "^(?!(?-i:inst/archives/)).*\\.tar\\.gz$",
    "^(?!(?-i:inst/archives/)).*\\.zip$",
    "^(?!(?-i:inst/archives/)).*\\.tar$",

    # Local component patterns
    unlist(lapply(component_packages, function(pkg) {
      pkg_pattern <- .escape_regex_literal(pkg)
      if (tolower(pkg) %in% .r_build_reserved_paths) return(character())
      c(sprintf("^%s$", pkg_pattern),         # Exact component directory
        sprintf("^%s(/.*)?$", pkg_pattern),  # Directory and descendants
        sprintf("^%s[._-].*$", pkg_pattern)  # Files prefixed by component name
      )
    }), use.names = FALSE)

  )

  # Keep patterns deterministic and unique.
  rbuildignore_content <- unique(unlist(rbuildignore_content))

  # Write project metadata files.
  .write_utf8(".Rproj.user", file.path(project_dir, ".gitignore"))
  log_debug(".Rbuildignore and .gitignore created")

  # Accept non-standard directories in the generated source package.
  bbsoptions_content <- "UnsupportedPlatforms: \nAcceptNonstandardNonTestDirectories: TRUE"
  .write_utf8(bbsoptions_content, file.path(project_dir, ".BBSoptions"))
  log_debug(".BBSoptions created")

  # Exclude the build-service configuration from the source tarball.
  rbuildignore_content <- c(rbuildignore_content, "^\\.BBSoptions$")
  rbuildignore_content <- c(rbuildignore_content,
                            "^\\.bigbang-manifest\\.rds$")

  # Persist the complete build ignore list.
  .write_utf8(rbuildignore_content, file.path(project_dir, ".Rbuildignore"))

  # Write the RStudio project file.
  rproj_content <-
    "Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX

AutoAppendNewline: Yes
StripTrailingWhitespace: Yes"

  .write_utf8(rproj_content, file.path(project_dir, paste0(name, ".Rproj")))
  log_debug(glue::glue("{name}.Rproj created"))

  # Render the remaining metapackage source files.
  if (verbose) {
    message(.bb_tr("Generating metapackage R files..."))
  }

  write_metapackage_files(
    name = name,
    packages = component_packages,
    archive_stems = archive_stems,
    dest_dir = file.path(project_dir, "R"),
    implicit_deps = hard_implicit_deps,
    include_archives = include_archives,
    verbose = debug,
    overwrite = update,
    install_upgrade = install_upgrade,
    reexport = isTRUE(reexport),
    reexport_specs = if (isTRUE(reexport)) {
      unname(reexport_plan$specs)
    } else {
      list()
    }
  )
  log_debug("Additional metapackage files created")


  if (verbose) {
    message(.bb_trf("Metapackage %s created successfully at %s", name, project_dir))
  }


  # Safety invariant: generation never removes pre-existing content. Historical
  # cwd-relative cleanup hooks and scripts are intentionally absent.


  # Generate documentation only when explicitly requested.
  doc_ok <- FALSE
  devtools_available <- FALSE
  if (isTRUE(document)) {
    documentation_search <- search()
    documentation_namespaces <- loadedNamespaces()
    devtools_available <- requireNamespace("devtools", quietly = TRUE)
  }
  if (isTRUE(document) && devtools_available) {
    if (verbose) {
      message(.bb_trf("Generating documentation for %s...", name))
    }

    # Run roxygen in a staging copy. Only known outputs are promoted through
    # the atomic writer, after their intentions have been recorded.
    documentation_generated <- tryCatch({
      staging_parent <- tempfile("bigbang-document-staging-")
      dir.create(staging_parent)
      on.exit(unlink(staging_parent, recursive = TRUE, force = TRUE), add = TRUE)
      if (!file.copy(project_dir, staging_parent, recursive = TRUE)) {
        stop(.bb_trf("Could not create temporary directory for %s", project_dir),
             call. = FALSE)
      }
      staging_project <- file.path(staging_parent, basename(project_dir))
      if (isTRUE(reexport)) {
        .write_reexport_documentation(staging_project, reexport_plan$table)
      }
      if (verbose) {
        devtools::document(pkg = staging_project, quiet = TRUE)
      } else {
        suppressPackageStartupMessages(
          devtools::document(pkg = staging_project, quiet = TRUE)
        )
      }
      staged_outputs <- unique(c(documentation_files, "NAMESPACE", "DESCRIPTION"))
      staged_outputs <- staged_outputs[file.exists(file.path(
        staging_project, staged_outputs
      )) & !dir.exists(file.path(staging_project, staged_outputs))]
      TRUE
    }, error = function(e) {
      warning(.bb_trf("Error generating documentation: %s", e$message),
              call. = FALSE)
      FALSE
    })
    if (isTRUE(documentation_generated)) {
      for (relative in staged_outputs) {
        .atomic_copy(file.path(staging_project, relative),
                     file.path(project_dir, relative))
      }
      if (verbose) {
        message(.bb_tr("Documentation generated successfully."))
      }
      doc_ok <- TRUE
    }
  } else if (isTRUE(document) && verbose) {
    message(.bb_tr("Install package 'devtools' to generate documentation automatically."))
  }

  retained_documentation <- if (isTRUE(document) && !doc_ok &&
                                  !is.null(update_manifest)) {
    intersect(documentation_files, update_manifest$files)
  } else {
    character()
  }
  reverted_documentation <- character()

  # Keep the emitted NAMESPACE deterministic when documentation rewrites it.
  .deduplicate_namespace_imports(file.path(project_dir, "NAMESPACE"))
  .ensure_namespace_exports(
    file.path(project_dir, "NAMESPACE"),
    if (isTRUE(reexport)) {
      reexport_plan$table$symbol
    } else {
      character()
    }
  )

  manifest_files <- union(
    setdiff(
      .planned_generation_files(
        name, components, workflow, include_archives,
        license = license, document = doc_ok, reexport = isTRUE(reexport),
        reexport_symbols = reexport_plan$table$symbol
      ),
      .generation_manifest_name
    ),
    union(preserved_files, retained_documentation)
  )
  manifest <- .manifest_records(project_dir, manifest_files)
  .atomic_save_rds(manifest, file.path(project_dir, .generation_manifest_name))

  if (!is.null(update_journal)) {
    .assert_update_lock_owner(update_lock, project_dir, "update completion")
    .deactivate_update_journal()
    .discard_update_journal(update_journal, project_dir, name)
    update_journal <- NULL
  }

  result <- structure(
    list(
      path = normalizePath(project_dir, winslash = "/", mustWork = TRUE),
      name = name,
      packages = component_packages,
      archives = archive_stems,
      reexports = reexport_plan$table,
      reexport_excluded = reexport_plan$excluded,
      order = .component_topological_order(components),
      added_files = added_files,
      removed_files = unique(c(stale_files, reverted_documentation)),
      local_dependencies = local_deps,
      cran_dependencies = cran_deps,
      implicit_dependencies = detected_implicit_deps,
      tolerated = tolerated,
      omitted = omitted,
      workflow = workflow,
      documented = doc_ok,
      dry_run = FALSE,
      updated = isTRUE(update),
      recovered = isTRUE(recovery$recovered),
      recovery = recovery,
      findings = generation_findings
    ),
    class = "bigbang_result"
  )
  generation_complete <- TRUE
  invisible(result)
}
