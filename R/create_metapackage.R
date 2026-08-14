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
.bb_generator_version <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("bigbang")),
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

.planned_documentation_files <- function(name) {
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
  file.path("man", paste0(c(static, public), ".Rd"))
}

.planned_generation_files <- function(name, components, workflow = NULL,
                                      include_archives = TRUE,
                                      license = "MIT + file LICENSE",
                                      document = FALSE,
                                      reexport = FALSE) {
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
    files <- c(files, .planned_documentation_files(name))
    if (isTRUE(reexport)) {
      exports <- unique(unlist(lapply(components, function(component) {
        .or_null(component$exports, character())
      }), use.names = FALSE))
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

.snapshot_untracked_docs <- function(project_dir, documentation_files,
                                     update_manifest = NULL,
                                     update_backup = NULL) {
  tracked <- if (is.null(update_manifest)) {
    character()
  } else {
    intersect(documentation_files, update_manifest$files)
  }
  candidates <- setdiff(documentation_files, tracked)
  .validate_project_write_paths(project_dir, candidates)
  paths <- file.path(project_dir, candidates)
  existing <- candidates[file.exists(paths) & !dir.exists(paths)]
  if (length(existing) == 0L) {
    return(list(
      path = NULL, files = character(), hashes = character(),
      candidates = candidates
    ))
  }
  if (is.null(update_backup)) {
    stop("Internal error: documentation backup is unavailable", call. = FALSE)
  }

  backup_dir <- file.path(update_backup$path, ".untracked-documentation")
  if (!dir.create(backup_dir)) {
    stop(.bb_trf("Could not create temporary directory for %s", project_dir),
         call. = FALSE)
  }
  complete <- FALSE
  on.exit({
    if (!complete) unlink(backup_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  for (relative in existing) {
    source <- file.path(project_dir, relative)
    destination <- file.path(backup_dir, relative)
    parent <- dirname(destination)
    if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) {
      stop(.bb_trf("Could not create temporary directory for %s", relative),
           call. = FALSE)
    }
    if (!file.copy(source, destination, overwrite = FALSE)) {
      stop(.bb_trf("Could not back up generated file: %s", source),
           call. = FALSE)
    }
  }
  hashes <- vapply(
    file.path(project_dir, existing), .file_digest, character(1L)
  )
  names(hashes) <- existing
  complete <- TRUE
  list(
    path = backup_dir, files = existing, hashes = hashes,
    candidates = candidates
  )
}

.restore_untracked_docs <- function(project_dir, snapshot) {
  if (is.null(snapshot)) return(invisible(character()))
  .validate_project_write_paths(project_dir, snapshot$candidates)
  current <- snapshot$candidates[file.exists(file.path(
    project_dir, snapshot$candidates
  ))]
  appeared <- setdiff(current, snapshot$files)
  .remove_stale_generation_files(project_dir, appeared)

  for (relative in snapshot$files) {
    .atomic_copy(
      file.path(snapshot$path, relative),
      file.path(project_dir, relative)
    )
  }
  restored <- vapply(
    file.path(project_dir, snapshot$files), .file_digest, character(1L)
  )
  if (!identical(unname(restored), unname(snapshot$hashes[snapshot$files]))) {
    stop(.bb_trf(
      "Could not restore generated files after a failed update: %s",
      paste(snapshot$files, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(appeared)
}

.reconcile_failed_docs <- function(project_dir, documentation_files,
                                   documentation_snapshot = NULL,
                                   update_manifest = NULL,
                                   update_backup = NULL) {
  tracked <- if (is.null(update_manifest)) {
    character()
  } else {
    intersect(documentation_files, update_manifest$files)
  }
  # A failed roxygen run may have written only part of its output. Files that
  # appeared during this call are removed, while pre-existing untracked files
  # are restored without becoming manifest-owned. Tracked documentation is
  # restored byte-for-byte from the main update backup.
  removed <- .restore_untracked_docs(project_dir, documentation_snapshot)
  if (length(tracked) > 0L) {
    if (is.null(update_backup)) {
      stop("Internal error: documentation backup is unavailable", call. = FALSE)
    }
    for (relative in tracked) {
      .atomic_copy(
        file.path(update_backup$path, relative),
        file.path(project_dir, relative)
      )
    }
  }
  list(retained = tracked, removed = removed)
}

.create_update_backup <- function(project_dir, manifest) {
  files <- unique(c(manifest$files, .generation_manifest_name))
  backup_dir <- tempfile("bigbang-update-backup-")
  if (!dir.create(backup_dir)) {
    stop(.bb_trf("Could not create temporary directory for %s", project_dir),
         call. = FALSE)
  }
  complete <- FALSE
  on.exit({
    if (!complete) unlink(backup_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  for (relative in files) {
    source <- file.path(project_dir, relative)
    destination <- file.path(backup_dir, relative)
    parent <- dirname(destination)
    if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) {
      stop(.bb_trf("Could not create temporary directory for %s", relative),
           call. = FALSE)
    }
    if (!file.copy(source, destination, overwrite = FALSE)) {
      stop(.bb_trf("Could not back up generated file: %s", source),
           call. = FALSE)
    }
  }
  complete <- TRUE
  list(path = backup_dir, files = files)
}

.restore_update_backup <- function(project_dir, backup) {
  failures <- character()
  for (relative in backup$files) {
    source <- file.path(backup$path, relative)
    destination <- file.path(project_dir, relative)
    restored <- tryCatch({
      .atomic_copy(source, destination)
      TRUE
    }, error = function(e) FALSE)
    if (!restored) failures <- c(failures, relative)
  }
  if (length(failures) > 0L) {
    warning(.bb_trf(
      "Could not restore generated files after a failed update: %s",
      paste(failures, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(length(failures) == 0L)
}

.discard_update_backup <- function(backup) {
  if (is.null(backup)) return(invisible(NULL))
  unlink(backup$path, recursive = TRUE, force = TRUE)
  invisible(NULL)
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
#'   package still installs offline without them. Before installation, reading
#'   a binding returns a placeholder function whose clear missing-component
#'   error appears only when that function is called. For non-function exports,
#'   access therefore returns the placeholder instead of the object until the
#'   component is installed. The same binding then works without reloading the
#'   metapackage. Only explicit `export()` directives become bindings. S4
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
#'   project.
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
#'   Files outside that manifest are never touched. Updates are refused when the
#'   generated project root, a manifest file, or any path component inside the
#'   project is a symbolic link, so writes cannot escape the project tree.
#'   Generated files
#'   no longer in the plan are reported in `removed_files`. Removing a component
#'   also removes its shipped archive, which may be the last available copy.
#'   The same field includes partial documentation outputs created and cleaned
#'   up after a failed documentation run. A dry run cannot predict those
#'   failure-dependent cleanups and reports only planned removals.
#'   Before changing the project, an update backs up every generated file and
#'   its manifest. A failed update restores that state so the same update can be
#'   retried. Documentation files requested by `document = TRUE` can always be
#'   regenerated: they can be restored after documentation was disabled, and an
#'   update that cannot regenerate them retains the previously tracked Rd files.
#'   See `document` for the reserved generated-documentation filenames.
#' @param install_upgrade Character default upgrade policy emitted in the
#'   generated installer function: "newer", "always", or "never".
#'   This controls whether a generated installer keeps newer installed
#'   versions, reinstalls every component, or skips archive inspection.
#' @param debug Logical. If `TRUE`, emits detailed debugging messages. Defaults
#'   to `FALSE`.
#'
#' @return Invisibly, a `bigbang_result` containing the generated path,
#'   component archives, dependency classification, applied tolerations,
#'   files removed by the call, and documentation status.
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
#' and a binding resolves the component on every access. Only explicit
#' `export()` directives are rebound; S4 classes and methods are used through
#' the loaded component namespace. An object restored with `readRDS()` cannot
#' load a component by itself, so base R cannot dispatch that component's S3
#' method until the component has been loaded.
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
  install_upgrade = c("newer", "always", "never")
) {
  verbose <- isTRUE(verbose)
  debug <- isTRUE(debug)
  on_component_error <- match.arg(on_component_error)
  install_upgrade <- match.arg(install_upgrade)
  if (!is.logical(reexport) || length(reexport) != 1L || is.na(reexport)) {
    stop(.bb_tr("'reexport' must be TRUE or FALSE"), call. = FALSE)
  }

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

  resolved_components <- .resolve_components(
    packages, pkg_dir, ext, on_component_error = on_component_error,
    reexport = isTRUE(reexport)
  )
  validated <- .validate_generation(
    resolved_components, tolerate = tolerate,
    on_component_error = on_component_error,
    reexport = isTRUE(reexport), metapackage_name = name
  )
  resolved_components <- validated$resolved
  validation <- validated$validation
  omitted <- validated$omitted
  components <- validation$components
  tolerated <- validation$tolerated
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

  project_path <- file.path(dest_dir, name)
  if (isTRUE(update)) .validate_project_root_path(project_path)
  project_dir <- normalizePath(
    project_path, winslash = "/", mustWork = FALSE
  )
  update_manifest <- NULL
  stale_files <- character()
  preserved_files <- character()
  requested_files <- setdiff(
    .planned_generation_files(
      name, resolved_components$components, workflow,
      include_archives, license = license, document = document,
      reexport = isTRUE(reexport)
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
    regenerable <- if (isTRUE(document)) {
      .planned_documentation_files(name)
    } else {
      character()
    }
    if (isTRUE(reexport)) {
      reexport_files <- c(
        file.path("R", "reexports.R"),
        if (isTRUE(document)) file.path("man", "reexports.Rd") else character()
      )
      reexport_files <- reexport_files[!file.exists(file.path(
        project_dir, reexport_files
      ))]
      regenerable <- c(regenerable, reexport_files)
    }
    untracked <- setdiff(
      requested_files, union(update_manifest$files, regenerable)
    )
    if (length(untracked) > 0L) {
      .bigbang_abort(
        "bigbang_error_modified_generated_file",
        .bb_trf(
          "Cannot update %s because requested generated files are not in its manifest: %s.",
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
      order = .component_topological_order(components),
      files = .planned_generation_files(
        name, components, workflow, include_archives,
        license = license, document = document, reexport = isTRUE(reexport)
      ),
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
      updated = FALSE
    ), class = "bigbang_result")
    return(invisible(result))
  }

  # Debug logger.
  log_debug <- function(debug_message) {
    if (debug) message(paste0("DEBUG: ", debug_message))
  }

  log_debug("Starting create_metapackage()")


  project_created <- FALSE
  destination_created <- !dir.exists(dest_dir)
  generation_complete <- FALSE
  update_backup <- if (isTRUE(update)) {
    .create_update_backup(project_dir, update_manifest)
  } else {
    NULL
  }
  documentation_search <- NULL
  documentation_namespaces <- NULL
  documentation_snapshot <- NULL
  documentation_files <- if (isTRUE(document)) {
    c(
      .planned_documentation_files(name),
      if (isTRUE(reexport)) file.path("man", "reexports.Rd") else character()
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
    documentation_restored <- TRUE
    if (!generation_complete && !is.null(documentation_snapshot)) {
      documentation_restored <- tryCatch({
        .restore_untracked_docs(project_dir, documentation_snapshot)
        TRUE
      }, error = function(e) FALSE)
    }
    backup_restored <- TRUE
    if (!generation_complete && !is.null(update_backup)) {
      backup_restored <- .restore_update_backup(project_dir, update_backup)
    }
    recovery_complete <- backup_restored && documentation_restored
    if (generation_complete || recovery_complete) {
      .discard_update_backup(update_backup)
    } else if (!is.null(update_backup)) {
      warning(.bb_trf(
        "The update backup was retained for manual recovery at: %s",
        update_backup$path
      ), call. = FALSE)
    }
  }, add = TRUE)

  # Reconcile before documentation so stale generated R code cannot be loaded
  # by roxygen. The update backup and rollback above are already active, so a
  # failure here or later restores every pre-existing generated file.
  if (length(stale_files) > 0L) {
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
                       include_archives = include_archives, verbose = debug)
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
    reexport_symbols = if (isTRUE(reexport)) {
      unique(unlist(lapply(components, function(component) {
        .or_null(component$exports, character())
      }), use.names = FALSE))
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
      specs <- lapply(components, function(component) {
        exports <- unique(.or_null(component$exports, character()))
        lapply(exports, function(symbol) {
          list(package = component$package, symbol = symbol)
        })
      })
      unname(unlist(specs, recursive = FALSE))
    } else {
      list()
    }
  )
  if (isTRUE(document)) {
    documentation_snapshot <- .snapshot_untracked_docs(
      project_dir, documentation_files, update_manifest, update_backup
    )
  }
  if (isTRUE(reexport) && isTRUE(document)) {
    .write_reexport_documentation(
      project_dir,
      unique(unlist(lapply(components, function(component) {
        .or_null(component$exports, character())
      }), use.names = FALSE))
    )
  }
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

    # Run roxygen without loading unclassified legacy source.
    tryCatch({
      if (verbose) {
        devtools::document(pkg = project_dir, quiet = TRUE)
      } else {
        suppressPackageStartupMessages(
          devtools::document(pkg = project_dir, quiet = TRUE)
        )
      }
      if (verbose) {
        message(.bb_tr("Documentation generated successfully."))
      }
      doc_ok <- TRUE
    }, error = function(e) {
      warning(.bb_trf("Error generating documentation: %s", e$message),
              call. = FALSE)
    })
  } else if (isTRUE(document) && verbose) {
    message(.bb_tr("Install package 'devtools' to generate documentation automatically."))
  }

  retained_documentation <- character()
  reverted_documentation <- character()
  if (isTRUE(document) && !doc_ok) {
    reconciliation <- .reconcile_failed_docs(
      project_dir, documentation_files,
      documentation_snapshot, update_manifest, update_backup
    )
    retained_documentation <- reconciliation$retained
    reverted_documentation <- reconciliation$removed
  }

  # Keep the emitted NAMESPACE deterministic when documentation rewrites it.
  .deduplicate_namespace_imports(file.path(project_dir, "NAMESPACE"))
  .ensure_namespace_exports(
    file.path(project_dir, "NAMESPACE"),
    if (isTRUE(reexport)) {
      unique(unlist(lapply(components, function(component) {
        .or_null(component$exports, character())
      }), use.names = FALSE))
    } else {
      character()
    }
  )

  manifest_files <- union(
    setdiff(
      .planned_generation_files(
        name, components, workflow, include_archives,
        license = license, document = doc_ok, reexport = isTRUE(reexport)
      ),
      .generation_manifest_name
    ),
    union(preserved_files, retained_documentation)
  )
  manifest <- .manifest_records(project_dir, manifest_files)
  .atomic_save_rds(manifest, file.path(project_dir, .generation_manifest_name))

  result <- structure(
    list(
      path = normalizePath(project_dir, winslash = "/", mustWork = TRUE),
      name = name,
      packages = component_packages,
      archives = archive_stems,
      order = .component_topological_order(components),
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
      findings = generation_findings
    ),
    class = "bigbang_result"
  )
  generation_complete <- TRUE
  invisible(result)
}
