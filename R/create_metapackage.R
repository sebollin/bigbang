# Local metapackage generator
#
# `create_metapackage()` creates a package project whose installation remains
# side-effect free. Component installation is an explicit `<name>_install()` call.
# Generated code reads archive DESCRIPTION files, builds the local dependency
# graph, rejects cycles, and installs each component once in topological order.
# Startup hooks may attach installed components but never install or remove files.

.generator_version <- "0.1.0"
.template_safety_schema <- "2"

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

#' Build a local meta-package
#'
#' @description
#' Creates the full structure and files of a meta-package that installs, manages
#' and loads a set of locally stored R packages, resolving the dependencies between
#' them with a graph-based (topologically ordered) approach.
#'
#' @param name Character. Name of the meta-package to create (must not contain
#'   underscores `_`).
#' @param packages Character vector. Names (with version) of the local
#'   packages to include, e.g. `"myPackage_1.0.0"`.
#' @param pkg_dir Character. Directory containing the local archive files
#'   (`.tar.gz`, `.zip`, etc.).
#' @param ext Character. Archive extension. Defaults to `".tar.gz"`.
#' @param version Character. Version of the meta-package. Defaults to `"0.1.0"`.
#' @param dest_dir Character. Directory in which to create the meta-package.
#'   If `NULL`, it is created in the current directory under the meta-package name.
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
#'   ones detected automatically.
#' @param ignore_deps Character vector. Dependencies to ignore even if detected.
#' @param import_deps Character vector. Packages that should go in the `Imports`
#'   field of DESCRIPTION rather than `Depends`. Imports are not attached when the
#'   user calls `library()` on the meta-package, but remain available via `::`
#'   (e.g. `dplyr::filter()`), reducing name clashes in the user's workspace.
#' @param force_deps Character vector. Exact package names to use as dependencies,
#'   bypassing automatic detection. If supplied, only these are used as the
#'   meta-package's implicit dependencies.
#' @param debug Logical. If `TRUE`, emits detailed debugging messages. Defaults
#'   to `FALSE`.
#'
#' @return Invisibly, a `bigbang_result` containing the generated path,
#'   component archives, dependency classification, and documentation status.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Creates the basic R package structure (`R`, `man`, `vignettes`, etc.).
#' 2. Detects dependencies between packages, both explicit (from DESCRIPTION) and
#'    implicit (found by scanning the source code).
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
#' If `reexport = TRUE`, a `reexports.R` file is generated so users can
#' reach the component functions directly through the meta-package
#' (`meta::fun()` instead of `component::fun()`), tidyverse style.
#'
#' @section Requirements:
#' - The local packages must exist in `pkg_dir` with the given extension.
#' - Automatic documentation (`document = TRUE`) requires the
#'   `devtools` package.
#'
#' @examples
#' \dontrun{
#' # Basic: a meta-package with two local packages
#' create_metapackage(
#'   name = "MyMeta",
#'   packages = c("pkg1_1.0.0", "pkg2_0.8.3"),
#'   pkg_dir = "path/to/archives"
#' )
#'
#' # Advanced: with re-exports and custom dependencies
#' create_metapackage(
#'   name = "AnalyticsMeta",
#'   packages = c("myStats_1.2.0", "myPlots_0.9.1"),
#'   pkg_dir = "path/to/archives",
#'   reexport = TRUE,
#'   additional_deps = c("ggplot2", "dplyr"),
#'   import_deps = c("data.table", "purrr", "tibble")
#' )
#' }
#' @export

create_metapackage <- function(
  name,
  packages,
  pkg_dir,
  ext = ".tar.gz",
  version = "0.1.0",
  dest_dir = NULL,
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
  debug = FALSE
) {
  verbose <- isTRUE(verbose)
  debug <- isTRUE(debug)

  # Validate public arguments before touching the filesystem.
  if (!is.character(name) || length(name) != 1) {
    stop(.bb_tr("'name' must be one character string"), call. = FALSE)
  }
  if (!is.character(packages) || length(packages) < 1) {
    stop(.bb_tr("'packages' must be a non-empty character vector"), call. = FALSE)
  }
  if (!is.character(pkg_dir) || length(pkg_dir) != 1) {
    stop(.bb_tr("'pkg_dir' must be one character string"), call. = FALSE)
  }
  if (!dir.exists(pkg_dir)) {
    stop(.bb_tr("The directory specified by 'pkg_dir' does not exist"), call. = FALSE)
  }

  # Validate the package name.
  if (grepl("_", name)) {
    suggested_name <- gsub("_", ".", name)
    stop(.bb_trf(
      "Package name '%s' contains underscores, which R package names do not allow. Use '%s' instead.",
      name, suggested_name
    ), call. = FALSE)
  }
  # Debug logger.
  log_debug <- function(debug_message) {
    if (debug) message(paste0("DEBUG: ", debug_message))
  }

  log_debug("Starting create_metapackage()")


  # Resolve the generated project path.
  dir_original <- getwd()
  on.exit(setwd(dir_original), add = TRUE)
  project_dir <- if (is.null(dest_dir)) file.path(dir_original, name) else file.path(dest_dir, name)

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
  }

  for (subdir in c("R", "man", "vignettes")) {
    subdir <- file.path(project_dir, subdir)
    if (!dir.create(subdir, showWarnings = FALSE) && !dir.exists(subdir)) {
      stop(.bb_trf("Could not create directory: %s", subdir), call. = FALSE)
    }
  }
  log_debug("Basic directory structure created")

  setwd(project_dir)
  log_debug(glue::glue("Changed to project directory: {getwd()}"))

  # Require every requested local archive to exist.
  missing_archives <- packages[!file.exists(file.path(pkg_dir, paste0(packages, ext)))]
  if (length(missing_archives) > 0) {
    stop(.bb_trf(
      "The following package archives were not found: %s",
      paste(missing_archives, collapse = ", ")
    ), call. = FALSE)
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

  # Explicit forced dependencies bypass automatic detection.
  if (!is.null(force_deps) && length(force_deps) > 0) {
    implicit_deps <- force_deps

    if (verbose) {
      message(.bb_trf(
        "Using explicitly supplied dependencies: %s",
        paste(implicit_deps, collapse = ", ")
      ))
    }
  } else {
    # Detect implicit dependencies.
    if (verbose) {
      message(.bb_tr("Scanning local packages for implicit dependencies..."))
    }

    implicit_deps <- detect_implicit_dependencies(packages, pkg_dir, ext)

    # Add user-supplied dependencies.
    if (!is.null(additional_deps) && length(additional_deps) > 0) {
      implicit_deps <- unique(c(implicit_deps, additional_deps))
    }

    # Remove explicitly ignored dependencies.
    if (!is.null(ignore_deps) && length(ignore_deps) > 0) {
      implicit_deps <- setdiff(implicit_deps, ignore_deps)
    }

    if (verbose) {
      message(.bb_trf(
        "Detected implicit dependencies: %s",
        paste(implicit_deps, collapse = ", ")
      ))
    }

  }

  # Extract explicit dependencies from local archives.
  dependencies <- unlist(lapply(packages, extract_dependencies, pkg_dir, ext))

  # Classify dependencies as local or repository-provided.
  classified_deps <- classify_dependencies(dependencies, pkg_dir, ext)
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
    implicit_deps = implicit_deps,
    import_deps = import_deps,
    authors = authors,
    description = description,
    license = license,
    verbose = debug
  )


  # Create the basic vignette after DESCRIPTION exists.
  write_basic_vignette(name, packages, project_dir, verbose = debug)
  if (debug) {
    log_debug("Basic vignette created for R CMD check")
  }

  # Write NAMESPACE with implicit dependencies.
  write_namespace_file(
    name = name,
    cran_packages = cran_deps,
    namespace_path = "NAMESPACE",
    implicit_deps = implicit_deps,
    import_deps = import_deps,
    verbose = debug
  )

  if (reexport) {
    namespace_additions <- character()
    for (pkg in packages) {
      exports <- getNamespaceExports(asNamespace(pkg))
      for (func in exports) {
        namespace_additions <- c(
          namespace_additions,
          paste0("S3method(", func, ", default)")
        )
      }
    }
  }

  # Append generated S3 method directives when present.
  if (exists("namespace_additions") && length(namespace_additions) > 0) {
    # Read the current NAMESPACE.
    namespace_content <- readLines("NAMESPACE")

    # Append S3 method directives.
    namespace_content <- c(namespace_content, "", "# S3 methods from reexports", namespace_additions)

    # Write the updated NAMESPACE.
    .write_utf8(namespace_content, "NAMESPACE")

    if (debug) {
      log_debug(paste("Added", length(namespace_additions), "S3 methods to NAMESPACE"))
    }
  }

  log_debug("NAMESPACE file created")

  # Generate the component installation engine.
  install_packages_content <- .render_install_engine(name, packages, pkg_dir, ext)

  install_packages_content <- .drop_regular_comment_lines(install_packages_content)
  .write_utf8(install_packages_content, file.path(project_dir, "R", "install_packages.R"))
  log_debug("install_packages.R created")

  # Write LICENSE when the declared license requires it.
  if (grepl("file[[:space:]]+LICENSE", license, ignore.case = TRUE)) {
    license_content <- c(
      paste0("YEAR: ", format(Sys.Date(), "%Y")),
      paste0("COPYRIGHT HOLDER: ", .copyright_holders(authors))
    )
    .write_utf8(license_content, "LICENSE")
    log_debug("LICENSE file created")
  }

  # Runtime translations belong to the generated metapackage, so it receives
  # its own source catalog and a precompiled catalog for environments without
  # gettext build tools. The conditional keeps standalone sourcing of this file
  # useful in the security regression script.
  if (exists(".metapackage_spanish_catalog", mode = "function")) {
    spanish_catalog <- .metapackage_spanish_catalog(name)
    .write_po_catalog(
      names(spanish_catalog), NULL, file.path("po", paste0("R-", name, ".pot")),
      project = paste(name, version)
    )
    .write_po_catalog(
      names(spanish_catalog), spanish_catalog, file.path("po", "R-es.po"),
      project = paste(name, version)
    )
    .write_mo_catalog(
      names(spanish_catalog), spanish_catalog,
      file.path("inst", "po", "es", "LC_MESSAGES", paste0("R-", name, ".mo"))
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

    # Generic package archives
    "^.*\\.tar\\.gz$",
    "^.*\\.zip$",
    "^.*\\.tar$",

    # Local component patterns
    vapply(sub("_.*", "", packages), function(pkg) {
      pkg_pattern <- .escape_regex_literal(pkg)
      c(sprintf("^%s$", pkg_pattern),         # Exact component directory
        sprintf("^%s(/.*)?$", pkg_pattern),  # Directory and descendants
        sprintf("^%s[._-].*$", pkg_pattern)  # Files prefixed by component name
      )
    }, character(3))

  )

  # Keep patterns deterministic and unique.
  rbuildignore_content <- unique(unlist(rbuildignore_content))

  # Write project metadata files.
  .write_utf8(".Rproj.user", ".gitignore")
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

  .write_utf8(rproj_content, paste0(name, ".Rproj"))
  log_debug(glue::glue("{name}.Rproj created"))

  # Render the remaining metapackage source files.
  if (verbose) {
    message(.bb_tr("Generating metapackage R files..."))
  }

  write_metapackage_files(
    name = name,
    packages = sub("_.*", "", packages),
    pkg_dir = pkg_dir,
    archive_stems = packages,
    ext = ext,
    dest_dir = "R",
    implicit_deps = implicit_deps,
    reexport = reexport,
    verbose = debug
  )
  log_debug("Additional metapackage files created")


  if (verbose) {
    message(.bb_trf("Metapackage %s created successfully at %s", name, project_dir))
  }


  # Safety invariant: generation never removes pre-existing content. Historical
  # cwd-relative cleanup hooks and scripts are intentionally absent.


  # Generate documentation only when explicitly requested.
  if (isTRUE(document) && requireNamespace("devtools", quietly = TRUE)) {
    if (verbose) {
      message(.bb_trf("Generating documentation for %s...", name))
    }

    # Restore the caller's working directory after documentation.
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)

    # Document the generated package from its source root.
    setwd(project_dir)

    # Run roxygen without loading unclassified legacy source.
    tryCatch({
      if (verbose) {
        devtools::document(quiet = TRUE)
      } else {
        suppressPackageStartupMessages(devtools::document(quiet = TRUE))
      }
      if (verbose) {
        message(.bb_tr("Documentation generated successfully."))
      }
    }, error = function(e) {
      warning(.bb_trf("Error generating documentation: %s", e$message),
              call. = FALSE)
    })
  } else if (isTRUE(document) && verbose) {
    message(.bb_tr("Install package 'devtools' to generate documentation automatically."))
  }


  invisible(structure(
    list(
      path = normalizePath(project_dir, mustWork = TRUE),
      name = name,
      packages = sub("_.*", "", packages),
      archives = packages,
      local_dependencies = local_deps,
      cran_dependencies = cran_deps,
      implicit_dependencies = implicit_deps,
      documented = isTRUE(document) &&
        requireNamespace("devtools", quietly = TRUE)
    ),
    class = "bigbang_result"
  ))
}


#' Deprecated Spanish alias for `create_metapackage()`
#'
#' @inheritParams create_metapackage
#' @param nombre,paquetes_locales,ruta_instalables Deprecated Spanish arguments.
#' @param ruta_destino,reexportar_funciones,generar_documentacion Deprecated Spanish arguments.
#' @param mostrar_progreso,autores,descripcion,licencia Deprecated Spanish arguments.
#' @param deps_adicionales,deps_ignorar,deps_imports,deps_forzar,verbose Deprecated Spanish arguments.
#' @return A `bigbang_result`, invisibly.
#' @keywords internal
#' @export
crear_meta_paquete_local <- function(
  nombre,
  paquetes_locales,
  ruta_instalables,
  ext = ".tar.gz",
  version = "0.1.0",
  ruta_destino = NULL,
  reexportar_funciones = FALSE,
  generar_documentacion = TRUE,
  mostrar_progreso = TRUE,
  autores = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
  descripcion = "Local Package Metapackage",
  licencia = "MIT + file LICENSE",
  deps_adicionales = NULL,
  deps_ignorar = NULL,
  deps_imports = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts", "zoo"),
  deps_forzar = NULL,
  verbose = FALSE
) {
  if (isTRUE(getOption("bigbang.deprecation_warnings", interactive()))) {
    .Deprecated("create_metapackage", package = "bigbang")
  }
  create_metapackage(
    name = nombre,
    packages = paquetes_locales,
    pkg_dir = ruta_instalables,
    ext = ext,
    version = version,
    dest_dir = ruta_destino,
    reexport = reexportar_funciones,
    document = generar_documentacion,
    verbose = mostrar_progreso,
    authors = autores,
    description = descripcion,
    license = licencia,
    additional_deps = deps_adicionales,
    ignore_deps = deps_ignorar,
    import_deps = deps_imports,
    force_deps = deps_forzar,
    debug = verbose
  )
}
