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
#'   packages to include. Existing paths may come from different directories;
#'   stems are resolved in `pkg_dir`, e.g. `"myPackage_1.0.0"`.
#' @param pkg_dir Character. Optional directory or directories containing local
#'   archives used to resolve stems. It is not needed when every `packages`
#'   element is an existing archive path.
#' @param ext Character. Fallback archive extension for stems. Defaults to
#'   `".tar.gz"`; each existing archive path keeps its own extension.
#' @param version Character. Version of the meta-package. Defaults to `"0.1.0"`.
#' @param dest_dir Character. Required destination directory. The function writes
#'   the generated meta-package exclusively inside this directory; there is no
#'   default path. Use `tempdir()` for disposable output.
#' @param reexport Logical. If `TRUE`, re-exports the component packages'
#'   functions so they are reachable directly through the meta-package (tidyverse
#'   style). Defaults to `FALSE`.
#' @param document Logical. If `TRUE`, runs `devtools::document()`
#'   automatically. Defaults to `TRUE`.
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
#'   local dependency error into a warning. Unknown names are errors. Each
#'   applied relaxation is recorded in the returned `tolerated` table.
#' @param debug Logical. If `TRUE`, emits detailed debugging messages. Defaults
#'   to `FALSE`.
#'
#' @return Invisibly, a `bigbang_result` containing the generated path,
#'   component archives, dependency classification, applied tolerations, and
#'   documentation status.
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
#' Validations that protect the recipient cannot be disabled: malformed or
#' unsafe archives, invalid component metadata, duplicate components, cycles,
#' and unsatisfied local version constraints remain hard errors. Checks about
#' project tidiness can be relaxed individually through `tolerate`; there is no
#' switch that disables validation as a whole. bigbang does not run
#' `R CMD check` on component packages, so component warnings and notes do not
#' prevent generation.
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
#' If `reexport = TRUE`, a `reexports.R` file is generated so users can
#' reach the component functions directly through the meta-package
#' (`meta::fun()` instead of `component::fun()`), tidyverse style.
#'
#' @section Requirements:
#' - Each component must be an existing archive path or a stem resolvable in
#'   one of the optional `pkg_dir` directories; `ext` is only a fallback for
#'   stems.
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
  tolerate = character()
) {
  verbose <- isTRUE(verbose)
  debug <- isTRUE(debug)

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

  resolved_components <- .resolve_components(packages, pkg_dir, ext)
  validation <- .validate_component_archives(
    resolved_components, tolerate = tolerate
  )
  components <- validation$components
  tolerated <- validation$tolerated
  component_packages <- vapply(components, `[[`, character(1L), "package")
  archive_stems <- vapply(components, `[[`, character(1L), "stem")
  archive_paths <- vapply(components, `[[`, character(1L), "path")
  archive_basenames <- basename(archive_paths)
  if (anyDuplicated(archive_basenames)) {
    duplicate_names <- unique(archive_basenames[duplicated(archive_basenames)])
    duplicate_paths <- archive_paths[archive_basenames %in% duplicate_names]
    .bigbang_abort(
      "bigbang_error_archive_basename_collision",
      .bb_trf(
        "Cannot use component archives with the same basename (%s): %s.",
        paste(duplicate_names, collapse = ", "),
        paste(duplicate_paths, collapse = "; ")
      ),
      paths = duplicate_paths
    )
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
  # Debug logger.
  log_debug <- function(debug_message) {
    if (debug) message(paste0("DEBUG: ", debug_message))
  }

  log_debug("Starting create_metapackage()")


  # Resolve the generated project path.
  project_dir <- normalizePath(
    file.path(dest_dir, name), winslash = "/", mustWork = FALSE
  )
  project_created <- FALSE
  destination_created <- !dir.exists(dest_dir)
  generation_complete <- FALSE
  documentation_search <- NULL
  documentation_namespaces <- NULL
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
  }, add = TRUE)

  log_debug(glue::glue("New project path: {project_dir}"))

  # In-place regeneration could preserve unsafe historical hooks or overwrite
  # user content. Only a new or completely empty destination is accepted.
  if (dir.exists(project_dir)) {
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
    copied <- file.copy(archive_paths, archive_dir, overwrite = TRUE)
    if (!all(copied)) {
      stop(.bb_trf(
        "Could not copy the component archives into the meta-package: %s",
        paste(archive_basenames[!copied], collapse = ", ")
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

  # Source-code guesses are diagnostic by default. Only explicitly supplied
  # dependencies (`force_deps` or `additional_deps`) become hard dependencies
  # in the generated DESCRIPTION/NAMESPACE. Supplying even an empty
  # `force_deps` intentionally disables the heuristic for reproducible builds.
  if (!is.null(force_deps)) {
    detected_implicit_deps <- character()

    if (verbose) {
      message(.bb_trf(
        "Using explicitly supplied dependencies: %s",
        paste(force_deps, collapse = ", ")
      ))
    }
  } else {
    # Detect implicit dependencies.
    if (verbose) {
      message(.bb_tr("Scanning local packages for implicit dependencies..."))
    }

    detected_implicit_deps <- detect_implicit_dependencies(
      packages, pkg_dir, ext, components = components
    )

    if (verbose) {
      message(.bb_trf(
        "Detected implicit dependencies: %s",
        paste(detected_implicit_deps, collapse = ", ")
      ))
    }

  }

  hard_implicit_deps <- unique(c(
    if (is.null(force_deps)) character() else force_deps,
    if (is.null(additional_deps)) character() else additional_deps
  ))
  if (!is.null(ignore_deps) && length(ignore_deps) > 0L) {
    hard_implicit_deps <- setdiff(hard_implicit_deps, ignore_deps)
  }
  if (is.null(force_deps) && !is.null(ignore_deps) && length(ignore_deps) > 0L) {
    detected_implicit_deps <- setdiff(detected_implicit_deps, ignore_deps)
  }

  # Extract explicit dependencies from local archives.
  dependencies <- unlist(lapply(components, `[[`, "dependencies"), use.names = FALSE)

  # Classify dependencies as local or repository-provided.
  classified_deps <- classify_dependencies(
    dependencies, included_packages = component_packages
  )
  cran_deps <- classified_deps$cran
  local_deps <- classified_deps$local

  # Remove utils because the generated package already imports it.
  cran_deps <- setdiff(cran_deps, "utils")

  # Deduplicate dependencies before writing DESCRIPTION.
  cran_deps <- unique(cran_deps)

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
  write_metapackage_readme(name, project_dir, include_archives)
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
    verbose = debug
  )

  # Re-exports are written by write_reexports_file(), reached through
  # write_metapackage_files() below. It receives component names, resolves each
  # namespace, and distinguishes S3 generics from ordinary functions. An earlier
  # block here duplicated that work incorrectly: it iterated the versioned
  # archive stems, so asNamespace() could never resolve them and every
  # reexport = TRUE call failed before reaching the working path, and it declared
  # S3method(<name>, default) for every export whether or not it was a generic.

  log_debug("NAMESPACE file created")

  # Generate the component installation engine.
  install_packages_content <- .render_install_engine(
    name, components, .archive_dir_default(name, include_archives)
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
    reexport = reexport,
    include_archives = include_archives,
    verbose = debug
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


  result <- structure(
    list(
      path = normalizePath(project_dir, winslash = "/", mustWork = TRUE),
      name = name,
      packages = component_packages,
      archives = archive_stems,
      local_dependencies = local_deps,
      cran_dependencies = cran_deps,
      implicit_dependencies = detected_implicit_deps,
      tolerated = tolerated,
      workflow = workflow,
      documented = doc_ok
    ),
    class = "bigbang_result"
  )
  generation_complete <- TRUE
  invisible(result)
}
