#' Build the default clause for the generated 'pkg_dir' argument
#'
#' Component archives shipped inside the meta-package are resolved with
#' `system.file()`, which is evaluated when the installer is called and
#' therefore points at the library of whoever installed it. When the archives
#' are not shipped there is no portable location to guess, so the argument
#' stays mandatory.
#'
#' @param name Character meta-package name.
#' @param include_archives Logical, whether archives travel inside the package.
#' @return Character default clause, empty when the argument is mandatory.
#' @noRd
.archive_dir_default <- function(name, include_archives) {
  if (!isTRUE(include_archives)) return("")
  paste0(' = system.file("', .archive_subdir, '", package = "', name, '")')
}

#' Subdirectory holding component archives inside a generated meta-package
#' @noRd
.archive_subdir <- "archives"

.render_install_engine <- function(name, components, pkg_dir_default = "",
                                   install_upgrade = "newer") {
  packages <- vapply(components, `[[`, character(1L), "package")
  archive_stems <- vapply(components, `[[`, character(1L), "stem")
  component_specs <- stats::setNames(lapply(components, function(component) {
    list(
      package = component$package,
      stem = component$stem,
      ext = component$ext
    )
  }), packages)
  component_specs_literal <- .r_literal(component_specs)
  package_list_literal <- .r_literal(packages)
  glue::glue('

.bigbang_abort <- function(class, message, ...) {{
  condition <- structure(
    c(list(message = message, call = NULL), list(...)),
    class = c(class, "bigbang_error", "error", "condition")
  )
  stop(condition)
}}

resolve_upgrade_policy <- function(force, upgrade, upgrade_missing) {{
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {{
    .bigbang_abort(
      "bigbang_error_install_policy",
      .meta_tr("\'force\' must be TRUE or FALSE")
    )
  }}
    upgrade <- match.arg(upgrade, c("newer", "always", "never"))
  if (isTRUE(force)) {{
    if (!isTRUE(upgrade_missing) && !identical(upgrade, "always")) {{
      .bigbang_abort(
        "bigbang_error_install_policy",
        .meta_tr(
          "\'force = TRUE\' conflicts with an explicit upgrade policy other than \'always\'"
        )
      )
    }}
    upgrade <- "always"
  }}
  upgrade
}}

with_install_library_path <- function(libraries, code) {{
  libraries <- unique(normalizePath(
    libraries[dir.exists(libraries)], winslash = "/", mustWork = TRUE
  ))
  library_path <- paste(libraries, collapse = .Platform$path.sep)
  previous <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  on.exit({{
    if (is.na(previous)) {{
      Sys.unsetenv("R_LIBS_USER")
    }} else {{
      Sys.setenv(R_LIBS_USER = previous)
    }}
  }}, add = TRUE)
  # install.packages() always rebuilds R_LIBS from the current .libPaths(), so
  # R_LIBS_USER is the channel that preserves additional libraries for its child.
  Sys.setenv(R_LIBS_USER = library_path)
  force(code)
}}

install_source_component <- function(target, lib, verbose = TRUE) {{
  # utils::install.packages() discards the output of the child installer for
  # local source packages, so a failure only reports a non-zero exit status.
  # Run the same R CMD INSTALL directly and keep the output: the ERROR lines
  # of the child become the failure message instead of a generic one.
  log_file <- tempfile("bigbang-install-log-")
  on.exit(unlink(log_file, force = TRUE), add = TRUE)
  target <- normalizePath(path.expand(target), winslash = "/", mustWork = FALSE)
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  status <- system2(
    r_binary, c("CMD", "INSTALL", "-l", shQuote(lib), shQuote(target)),
    stdout = log_file, stderr = log_file
  )
  output <- if (file.exists(log_file)) {{
    readLines(log_file, warn = FALSE)
  }} else {{
    character()
  }}
  if (isTRUE(verbose) && length(output) > 0L) cat(output, sep = "\\n")
  if (!identical(status, 0L)) {{
    detail <- grep("ERROR", output, value = TRUE, fixed = TRUE)
    if (length(detail) == 0L) detail <- utils::tail(output, 5L)
    detail <- paste(detail, collapse = " | ")
    if (!nzchar(detail)) {{
      detail <- sprintf("R CMD INSTALL exited with status %d", status)
    }}
    stop(detail, call. = FALSE)
  }}
  invisible(TRUE)
}}

.component_specs <- {component_specs_literal}
.component_names <- {package_list_literal}

resolve_component_spec <- function(package, ext = NULL) {{
  if (package %in% names(.component_specs)) return(.component_specs[[package]])
  by_stem <- vapply(.component_specs, function(spec) identical(spec$stem, package),
                    logical(1L))
  if (sum(by_stem) == 1L) return(.component_specs[[which(by_stem)]])
  fallback_ext <- if (is.null(ext)) ".tar.gz" else ext
  list(
    package = sub("_.*", "", package),
    stem = package,
    ext = fallback_ext
  )
}}

resolve_component_archive <- function(package, pkg_dir, ext = NULL) {{
  spec <- resolve_component_spec(package, ext)
  if (!is.character(pkg_dir) || length(pkg_dir) < 1L || anyNA(pkg_dir) ||
      any(!nzchar(pkg_dir))) {{
    stop(.meta_tr(
      "The archive directory must be one or more non-empty paths."
    ), call. = FALSE)
  }}
  dirs <- normalizePath(pkg_dir, winslash = "/", mustWork = FALSE)
  candidates <- file.path(dirs, paste0(spec$stem, spec$ext))
  found <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (length(found) == 0L) {{
    expected <- tolower(basename(candidates))
    found <- unlist(lapply(dirs, function(dir) {{
      files <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      files[tolower(basename(files)) %in% expected & !dir.exists(files)]
    }}), use.names = FALSE)
  }}
  if (length(found) == 0L) {{
    stop(.meta_trf(
      "Could not resolve component \'%s\' in the supplied archive directories.",
      package
    ), call. = FALSE)
  }}
  list(path = normalizePath(found[[1L]], winslash = "/", mustWork = TRUE),
       ext = spec$ext, stem = spec$stem)
}}

#\' Read dependencies from a local package archive
#\'
#\' Extracts into an owned temporary directory and reads Depends, Imports, and
#\' LinkingTo from DESCRIPTION. It does not install or load the package.
#\'
#\' @param package Character archive stem or generated component identifier.
#\' @param pkg_dir Character vector of directories containing local archives.
#\' @param ext Character fallback archive extension.
#\'
#\' @return A list with declared package metadata and dependencies.
#\' @keywords internal
read_archive_metadata <- function(package, pkg_dir, ext = NULL) {{
  resolved <- resolve_component_archive(package, pkg_dir, ext)
  archive <- resolved$path
  ext <- resolved$ext

  temp_dir <- tempfile("bigbang-deps-")
  if (!dir.create(temp_dir)) stop(.meta_trf(
    "Could not extract archive %s: could not create temporary directory.",
    archive
  ), call. = FALSE)
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  if (!identical(tolower(ext), ".zip") && !ext %in% c(".tar.gz", ".tar")) {{
    stop(.meta_trf("Unsupported archive format: %s", ext), call. = FALSE)
  }}
  listing <- tryCatch(suppressWarnings({{
    if (identical(tolower(ext), ".zip")) utils::unzip(archive, list = TRUE)
    else utils::untar(archive, list = TRUE)
  }}), error = identity)
  if (inherits(listing, "error")) {{
    stop(.meta_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(listing)
    ), call. = FALSE)
  }}
  listing_status <- attr(listing, "status")
  if (is.numeric(listing_status) && length(listing_status) == 1L &&
      listing_status != 0) {{
    stop(.meta_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, listing_status
    ), call. = FALSE)
  }}
  members <- if (identical(tolower(ext), ".zip")) listing$Name else listing
  members <- gsub("\\\\", "/", members, fixed = TRUE)
  unsafe <- startsWith(members, "/") |
    grepl("^[A-Za-z]:", members) |
    grepl("(^|/)\\\\.\\\\.(/|$)", members, perl = TRUE)
  if (any(unsafe)) {{
    stop(.meta_trf(
      "Archive contains unsafe absolute or parent-traversal paths: %s",
      paste(utils::head(members[unsafe], 3L), collapse = ", ")
    ), call. = FALSE)
  }}
  extraction <- tryCatch(suppressWarnings({{
    if (identical(tolower(ext), ".zip")) utils::unzip(archive, exdir = temp_dir)
    else utils::untar(archive, exdir = temp_dir)
  }}), error = identity)
  if (inherits(extraction, "error")) {{
    stop(.meta_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(extraction)
    ), call. = FALSE)
  }}
  if (is.numeric(extraction) && length(extraction) == 1L && extraction != 0) {{
    stop(.meta_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, extraction
    ), call. = FALSE)
  }}
  extracted <- list.files(
    temp_dir, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, include.dirs = TRUE, no.. = TRUE
  )
  if (length(extracted) > 0L && any(nzchar(Sys.readlink(extracted)))) {{
    stop(.meta_trf(
      "Archive %s contains symbolic links, which are not supported.", archive
    ), call. = FALSE)
  }}
  roots <- list.files(temp_dir, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
  flat_binary <- identical(tolower(ext), ".zip") &&
    file.exists(file.path(temp_dir, "Meta", "package.rds"))
  if (isTRUE(flat_binary) && file.exists(file.path(temp_dir, "DESCRIPTION"))) {{
    package_root <- temp_dir
  }} else if (length(roots) != 1L || !dir.exists(file.path(temp_dir, roots[[1L]]))) {{
    stop(.meta_trf("Archive %s must contain one package root directory.", archive), call. = FALSE)
  }} else {{
    package_root <- file.path(temp_dir, roots[[1L]])
  }}
  description_file <- file.path(package_root, "DESCRIPTION")
  if (!file.exists(description_file)) {{
    stop(.meta_trf("Archive %s has no DESCRIPTION at the package root.", archive), call. = FALSE)
  }}

  desc <- read.dcf(
    description_file,
    fields = c("Package", "Version", "Depends", "Imports", "LinkingTo")
  )
  if (nrow(desc) == 0L) {{
    stop(.meta_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }}
  field <- function(name) {{
    if (!name %in% colnames(desc)) return(NA_character_)
    value <- unname(desc[1L, name])
    if (is.na(value)) NA_character_ else trimws(value)
  }}
  declared_package <- field("Package")
  declared_version <- field("Version")
  if (is.na(declared_package) || !nzchar(declared_package) ||
      is.na(declared_version) || !nzchar(declared_version)) {{
    stop(.meta_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }}
  spec <- resolve_component_spec(package, ext)
  expected_package <- sub("_.*", "", spec$stem)
  has_version <- grepl("_", spec$stem, fixed = TRUE)
  expected_version <- if (has_version) sub("^[^_]+_", "", spec$stem) else NA_character_
  if (!identical(declared_package, expected_package)) {{
    warning(.meta_trf(
      "Archive %s declares package %s, but its filename suggests %s.",
      archive, declared_package, expected_package
    ), call. = FALSE)
  }}
  if (has_version && !tryCatch(
      isTRUE(base::package_version(declared_version) ==
             base::package_version(expected_version)),
      error = function(e) FALSE
  )) {{
    warning(.meta_trf(
      "Archive %s declares version %s, but its filename suggests version %s.",
      archive, declared_version, expected_version
    ), call. = FALSE)
  }}
  dependencies <- character()
  constraints <- list()
  for (value in vapply(c("Depends", "Imports", "LinkingTo"), field, character(1L))) {{
    if (is.na(value) || !nzchar(value)) next
    for (piece in strsplit(value, ",", fixed = TRUE)[[1L]]) {{
      piece <- trimws(piece)
      match <- regexec(
        "^([A-Za-z][A-Za-z0-9.]*)[[:space:]]*\\\\(([<>=]+)[[:space:]]*([^)]*)\\\\)$",
        piece, perl = TRUE
      )
      captures <- regmatches(piece, match)[[1L]]
      if (length(captures) == 4L) {{
        dependency <- captures[[2L]]
        constraints[[length(constraints) + 1L]] <- list(
          package = dependency, op = captures[[3L]], version = trimws(captures[[4L]])
        )
      }} else {{
        dependency <- sub("[[:space:]].*$", "", piece)
      }}
      dependencies <- c(dependencies, dependency)
    }}
  }}
  list(
    path = archive,
    ext = ext,
    stem = spec$stem,
    package = declared_package,
    version = declared_version,
    dependencies = unique(setdiff(dependencies[nzchar(dependencies)], "R")),
    constraints = constraints
  )
}}

read_archive_dependencies <- function(package, pkg_dir, ext = NULL) {{
  read_archive_metadata(package, pkg_dir, ext)$dependencies
}}

version_satisfies <- function(actual, op, required) {{
  tryCatch({{
    actual <- base::package_version(actual)
    required <- base::package_version(required)
    switch(op, ">=" = actual >= required, ">" = actual > required,
           "<=" = actual <= required, "<" = actual < required,
           "==" = actual == required, FALSE)
  }}, error = function(e) FALSE)
}}

validate_local_constraints <- function(packages, pkg_dir, ext = NULL) {{
  metadata <- lapply(packages, read_archive_metadata, pkg_dir = pkg_dir, ext = ext)
  package_names <- vapply(metadata, function(item) item$package, character(1L))
  versions <- vapply(metadata, function(item) item$version, character(1L))
  names(versions) <- package_names
  for (index in seq_along(metadata)) {{
    constraints <- metadata[[index]]$constraints
    local <- constraints[vapply(constraints, function(item) {{
      item$package %in% package_names
    }}, logical(1L))]
    for (constraint in local) {{
      actual <- unname(versions[[constraint$package]])
      if (!version_satisfies(actual, constraint$op, constraint$version)) {{
        .bigbang_abort(
          "bigbang_error_dependency_version",
          .meta_trf(
            "Component %s requires %s %s %s, but the included archive provides version %s.",
            metadata[[index]]$package, constraint$package, constraint$op,
            constraint$version, actual
          ),
          component = metadata[[index]]$package,
          dependency = constraint$package,
          required = constraint,
          actual = actual
        )
      }
    }
  }
  invisible(metadata)
}


#\' Classify a local package archive
#\'
#\' A ZIP containing `Meta/package.rds` is a Windows binary package. Other ZIP
#\' archives are treated as source archives and are unpacked before installation.
#\'
#\' @return One of `"source"`, `"source.zip"`, or `"win.binary"`.
#\' @keywords internal
classify_package_archive <- function(archive, ext) {{
  if (!identical(tolower(ext), ".zip")) return("source")

  members <- utils::unzip(archive, list = TRUE)$Name
  members <- gsub("\\\\", "/", members, fixed = TRUE)
  has_description <- any(grepl("(^|/)DESCRIPTION$", members))
  if (!has_description) {{
    stop(.meta_trf(
      "The ZIP archive does not contain a DESCRIPTION file: %s", archive
    ), call. = FALSE)
  }}
  if (any(grepl("(^|/)Meta/package\\\\.rds$", members))) {{
    return("win.binary")
  }}
  "source.zip"
}}


#\' Install a local package with its dependencies
#\'
#\' Checks the installed version, resolves non-local dependencies according to
#\' policy, and installs the local archive. Local dependencies are installed by
#\' the outer topological loop, so this helper is not recursive.
#\'
#\' @param package Character archive stem or generated component identifier.
#\' @param pkg_dir Character vector of directories containing local archives.
#\' @param ext Character fallback archive extension.
#\' @param repos Character repositories for non-local dependencies.
#\' @param cran_deps Character missing-dependency policy: `"skip"` and `"error"`
#\'   never access the network; `"install"` uses `repos`.
#\'
#\' @return A list with installation status and detected dependencies.
#\' @param lib Character library in which to install and verify the package.
#\' @param verbose Logical; print the installation subprocess transcript.
#\' @keywords internal
install_local_archive <- function(package, pkg_dir, ext = NULL,
                                   repos = getOption("repos"),
                                   cran_deps = c("skip", "error", "install"),
                                   upgrade = c("newer", "always", "never"),
                                   lib = .libPaths()[[1L]], verbose = TRUE) {{
  cran_deps <- match.arg(cran_deps)
  upgrade <- match.arg(upgrade)
  dependency_libraries <- unique(c(lib, .libPaths()))
  resolved <- tryCatch(
    resolve_component_archive(package, pkg_dir, ext),
    error = function(e) e
  )
  if (inherits(resolved, "error")) {{
    return(list(success = FALSE, message = conditionMessage(resolved)))
  }}
  archive <- resolved$path
  ext <- resolved$ext
  metadata <- tryCatch(
    read_archive_metadata(package, pkg_dir, ext),
    error = function(e) e
  )
  if (inherits(metadata, "error")) {{
    return(list(success = FALSE, message = conditionMessage(metadata)))
  }}
  base_name <- metadata$package
  version <- metadata$version
  dependencies <- metadata$dependencies

  installed_version <- tryCatch(
    utils::packageVersion(base_name, lib.loc = lib), error = function(e) NULL
  )
  keep_installed <- !is.null(installed_version) && (
    identical(upgrade, "never") ||
      (identical(upgrade, "newer") &&
         installed_version >= base::package_version(version))
  )
  if (keep_installed) {{
    newer <- installed_version > base::package_version(version)
    message(if (newer) {{
      .meta_trf(
        "Package %s has installed version %s, newer than archive version %s; keeping the installed version.",
        base_name, as.character(installed_version), version
      )
    }} else {{
      .meta_trf(
        "Package %s has installed version %s and archive version %s; keeping the installed version.",
        base_name, as.character(installed_version), version
      )
    }})
    # The reported reason has to carry the versions: a bare already-installed
    # label is false when the installed package only shares the component name.
    unchanged_message <- if (identical(upgrade, "never")) {{
      .meta_trf(
        "Kept installed version %s because upgrade = \'never\'; the archive names version %s",
        as.character(installed_version), version
      )
    }} else if (newer) {{
      .meta_trf(
        "Kept installed version %s, newer than archive version %s",
        as.character(installed_version), version
      )
    }} else {{
      .meta_trf(
        "Kept installed version %s, matching archive version %s",
        as.character(installed_version), version
      )
    }}
    return(list(
      success = TRUE,
      unchanged = TRUE,
      message = unchanged_message,
      dependencies = dependencies
    ))
  }}

  local_names <- .component_names

  # Local dependencies are installed once by the outer topological loop. This
  # branch resolves only dependencies not provided by local archives.
  missing_nonlocal <- setdiff(dependencies, local_names)
  missing_nonlocal <- missing_nonlocal[!vapply(
    missing_nonlocal, requireNamespace, logical(1), quietly = TRUE,
    lib.loc = dependency_libraries
  )]
  if (length(missing_nonlocal) > 0L && cran_deps != "install") {{
    detail <- paste(missing_nonlocal, collapse = ", ")
    if (cran_deps == "skip") {{
      return(list(
        success = FALSE,
        skipped = TRUE,
        message = .meta_trf("Skipped because non-local dependencies are missing: %s", detail),
        missing_dependencies = missing_nonlocal,
        dependencies = dependencies
      ))
    }}
    return(list(
      success = FALSE,
      message = .meta_trf("Missing non-local dependencies: %s", detail),
      missing_dependencies = missing_nonlocal,
      dependencies = dependencies
    ))
  }}

  if (length(missing_nonlocal) > 0L && cran_deps == "install") {{
    invalid_repos <- is.null(repos) || length(repos) == 0L ||
      all(is.na(repos) | !nzchar(repos) | repos == "@CRAN@")
    if (invalid_repos) {{
      detail <- paste(missing_nonlocal, collapse = ", ")
      return(list(
        success = FALSE,
        message = .meta_trf(
          "Cannot install non-local dependencies without a configured repository: %s",
          detail
        ),
        missing_dependencies = missing_nonlocal,
        dependencies = dependencies
      ))
    }}
    for (dep in missing_nonlocal) {{
      message(.meta_trf("Installing non-local dependency: %s", dep))
      tryCatch(with_install_library_path(
        dependency_libraries,
        # NA is what is needed to use the package: Depends, Imports and
        # LinkingTo. TRUE would add Suggests, pulling development tooling
        # into environments that asked for one dependency.
        utils::install.packages(dep, dependencies = NA, repos = repos, lib = lib)
      ),
        error = function(e) warning(conditionMessage(e), call. = FALSE)
      )
    }}
  }}

  missing <- dependencies[!vapply(
    dependencies, requireNamespace, logical(1), quietly = TRUE,
    lib.loc = dependency_libraries
  )]
  if (length(missing) > 0L) {{
    return(list(
      success = FALSE,
      message = .meta_trf(
        "Dependencies are not installed: %s", paste(missing, collapse = ", ")
      ),
      missing_dependencies = missing,
      dependencies = dependencies
    ))
  }}

  archive_type <- tryCatch(
    classify_package_archive(archive, ext),
    error = function(e) e
  )
  if (inherits(archive_type, "error")) {{
    return(list(success = FALSE, message = conditionMessage(archive_type)))
  }}
  if (identical(archive_type, "win.binary") && .Platform$OS.type != "windows") {{
    return(list(
      success = FALSE,
      message = .meta_tr("Windows binary ZIP packages can only be installed on Windows.")
    ))
  }}

  install_target <- archive
  install_type <- if (identical(archive_type, "win.binary")) "win.binary" else "source"
  if (identical(archive_type, "source.zip")) {{
    source_dir <- tempfile("bigbang-source-zip-")
    dir.create(source_dir)
    on.exit(safe_unlink(source_dir, recursive = TRUE), add = TRUE)
    utils::unzip(archive, exdir = source_dir)
    roots <- list.files(source_dir, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
    if (length(roots) != 1L || !dir.exists(file.path(source_dir, roots[[1L]]))) {{
      return(list(success = FALSE,
                  message = .meta_trf("Archive %s must contain one package root directory.", archive)))
    }}
    package_root <- file.path(source_dir, roots[[1L]])
    if (!file.exists(file.path(package_root, "DESCRIPTION"))) {{
      return(list(success = FALSE,
                  message = .meta_trf("Archive %s has no DESCRIPTION at the package root.", archive)))
    }}
    install_target <- package_root
  }}

  install_error <- NULL
  tryCatch(
    if (identical(install_type, "win.binary")) {{
      with_install_library_path(
        dependency_libraries,
        utils::install.packages(
          install_target, repos = NULL, type = install_type,
          dependencies = FALSE, lib = lib
        )
      )
    }} else {{
      with_install_library_path(
        dependency_libraries,
        install_source_component(install_target, lib, verbose = verbose)
      )
    }},
    error = function(e) install_error <<- conditionMessage(e)
  )

  installed <- is.null(install_error) && tryCatch(
    utils::packageVersion(base_name, lib.loc = lib) ==
      base::package_version(version),
    error = function(e) FALSE
  )
  if (!installed) {{
    detail <- if (is.null(install_error)) {{
      .meta_tr("Installation could not be verified")
    }} else {{
      install_error
    }}
    return(list(
      success = FALSE,
      message = detail,
      failed = stats::setNames(list(detail), package),
      dependencies = dependencies
    ))
  }}

  if (identical(base_name, package)) {{
    message(.meta_trf("Installed package %s successfully.", base_name))
  }} else {{
    message(.meta_trf(
      "Installed package %s from %s successfully.", base_name, package
    ))
  }}
  list(
    success = TRUE,
    message = .meta_tr("Installed successfully"),
    installed = stats::setNames(list(.meta_tr("Installed successfully")), package),
    dependencies = dependencies
  )
}}


#\' Detect cycles in a dependency graph
#\'
#\' Analyzes an adjacency matrix and returns circular package dependencies.
#\'
#\' @param adjacency Matrix. A value of 1 means the row package depends on the
#\'   column package.
#\'
#\' @return A list of integer vectors, one per cycle.
#\'
#\' @details
#\' Uses depth-first search (DFS).
#\'
#\' @examples
#\' \\dontrun{{
#\'   # Create an adjacency matrix containing a cycle
#\'   mat <- matrix(c(0,1,0, 0,0,1, 1,0,0), nrow=3, byrow=TRUE)
#\'   rownames(mat) <- colnames(mat) <- c("pkg1", "pkg2", "pkg3")
#\'
#\'   # Detect cycles
#\'   cycles <- detect_cycles(mat)
#\'   print(cycles)
#\' }}
#\'
#\' @keywords internal
detect_cycles <- function(adjacency) {{
  package_count <- nrow(adjacency)
  visited <- rep(FALSE, package_count)
  rec_stack <- rep(FALSE, package_count)
  cycles <- list()

  dfs <- function(v, path = integer(0)) {{
    if (rec_stack[v]) {{
      # Cycle found
      cycle_start <- match(v, path)
      if (!is.na(cycle_start)) {{
        cycles <<- c(cycles, list(path[cycle_start:length(path)]))
      }}
      return(TRUE)
    }}

    if (visited[v]) return(FALSE)

    visited[v] <<- TRUE
    rec_stack[v] <<- TRUE
    path <- c(path, v)

    for (u in which(adjacency[v, ] == 1)) {{
      if (dfs(u, path)) return(TRUE)
    }}

    rec_stack[v] <<- FALSE
    return(FALSE)
  }}

  for (i in 1:package_count) {{
    if (!visited[i]) dfs(i)
  }}

  return(cycles)
}}



#\' Build a dependency graph from local packages
#\'
#\' Reads each archive DESCRIPTION without installing or loading packages and
#\' builds the adjacency matrix used for installation ordering.
#\'
#\' @param packages Character archive stems or generated component identifiers.
#\' @param pkg_dir Character vector of archive directories.
#\' @param ext Character archive extension.
#\'
#\' @return An adjacency matrix.
#\'
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   adj <- build_dependency_graph(
#\'     packages = c("uspr_0.8.6", "conexiones_0.8.3"),
#\'     pkg_dir = "X:/path",
#\'     ext = ".tar.gz"
#\'   )
#\'   print(adj)
#\' }}

build_dependency_graph <- function(packages, pkg_dir, ext = NULL) {{
  package_count <- length(packages)
  adjacency <- base::matrix(0, nrow = package_count, ncol = package_count)
  rownames(adjacency) <- colnames(adjacency) <- packages
  package_names <- vapply(
    packages,
    function(package) resolve_component_spec(package, ext)$package,
    character(1L)
  )
  validate_local_constraints(packages, pkg_dir, ext)

  for (package in packages) {{
    deps <- read_archive_dependencies(package, pkg_dir, ext)
    local_deps <- intersect(deps, package_names)
    for (dep in local_deps) {{
      dependency_index <- which(package_names == dep)
      if (length(dependency_index) > 0) {{
        package_index <- which(packages == package)
        adjacency[package_index, dependency_index[1]] <- 1
      }}
    }}
  }}


  # Check cycles
  cycles <- detect_cycles(adjacency)
  if (length(cycles) > 0) {{
    # Convert indices to package names
    named_cycles <- lapply(cycles, function(cycle) {{
      packages[cycle]
    }})

    cycle_text <- paste(
      vapply(named_cycles, paste, character(1), collapse = " -> "),
      collapse = "; "
    )
    .bigbang_abort(
      "bigbang_error_cycle",
      .meta_trf(
        "Circular dependencies detected: %s. A clean installation has no valid topological order.",
        cycle_text
      ),
      cycles = named_cycles
    )
  }}

  return(adjacency)
}}

#\' Topologically sort a dependency graph
#\'
#\' Uses DFS on the adjacency matrix to find an installation order.
#\'
#\' @param adjacency Matrix where 1 means the row depends on the column.
#\'
#\' @return An integer vector containing the topological order.
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   mat <- base::matrix(c(0,1,0,0), nrow=2, byrow=TRUE)
#\'   rownames(mat) <- colnames(mat) <- c("conexiones_0.8.3", "uspr_0.8.6")
#\'   ord <- topological_order(mat)
#\'   print(ord)
#\' }}

topological_order <- function(adjacency) {{
  package_count <- nrow(adjacency)
  visited <- rep(FALSE, package_count)
  order <- integer(0)

  dfs <- function(v) {{
    visited[v] <<- TRUE
    for (u in which(adjacency[v, ] == 1)) {{
      if (!visited[u]) {{
        dfs(u)
      }}
    }}
    order <<- c(order, v)
  }}

  for (i in seq_len(package_count)) {{
    if (!visited[i]) {{
      dfs(i)
    }}
  }}

  return(order)

}}

#\' Install local packages in dependency order
#\'
#\' Builds the graph, computes its topological order, and installs each package
#\' exactly once.
#\'
#\' @param packages Character archive stems or generated component identifiers.
#\' @param pkg_dir Character vector of archive directories.
#\' @param ext Character archive extension.
#\' @param verbose Logical progress toggle.
#\' @return Invisibly, installation, failure, skip, and order information.
#\' @param only Optional component names; local dependencies are added.
#\' @param lib Character library in which components are installed and verified.
#\'   A component found only in another library is installed into `lib`, while
#\'   non-local dependencies may be available in `lib` or any `.libPaths()` entry.
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   install_packages_in_order(
#\'     packages = c("uspr_1.0.0", "conexiones_0.8.3"),
#\'     pkg_dir = "X:/path"
#\'   )
#\' }}


install_packages_in_order <- function(packages, pkg_dir, ext = NULL,
                                      verbose = TRUE,
                                      repos = getOption("repos"),
                                      cran_deps = c("skip", "error", "install"),
                                      upgrade = c("newer", "always", "never"),
                                      only = NULL,
                                      lib = .libPaths()[[1L]]) {{
  cran_deps <- match.arg(cran_deps)
  upgrade <- match.arg(upgrade)
  if (!is.character(lib) || length(lib) != 1L || is.na(lib) || !nzchar(lib)) {{
    stop(.meta_tr("The installation library must be one non-empty path."),
         call. = FALSE)
  }}
  if (!dir.exists(lib) && !dir.create(lib, recursive = TRUE)) {{
    stop(.meta_trf("Could not create installation library: %s", lib),
         call. = FALSE)
  }}
  lib <- normalizePath(lib, winslash = "/", mustWork = TRUE)
  if (!is.null(only)) {{
    if (!is.character(only) || anyNA(only) || any(!nzchar(only))) {{
      stop(.meta_tr("\'only\' must contain component package names."),
           call. = FALSE)
    }}
    unknown <- setdiff(only, .component_names)
    if (length(unknown) > 0L) {{
      condition <- structure(
        list(message = .meta_trf("Unknown component(s) in \'only\': %s.",
                                 paste(unknown, collapse = ", ")),
             call = NULL, unknown = unknown),
        class = c("bigbang_error_only", "bigbang_error", "error", "condition")
      )
      stop(condition)
    }}
    selected <- unique(only)
    repeat {{
      dependencies <- unlist(lapply(selected, read_archive_dependencies,
                                    pkg_dir = pkg_dir, ext = ext),
                             use.names = FALSE)
      pulled <- setdiff(intersect(dependencies, .component_names), selected)
      if (length(pulled) == 0L) break
      selected <- c(selected, pulled)
    }}
    packages <- selected
  }}
  adjacency <- build_dependency_graph(packages, pkg_dir, ext)
  install_order <- topological_order(adjacency)

  installed_packages <- list()
  unchanged_packages <- list()
  failed_packages <- list()
  skipped_packages <- list()
  pb <- NULL

  total_pkgs <- length(packages)
  if (verbose && interactive() && total_pkgs > 1) {{
    message(.meta_trf("Starting installation of %d packages", total_pkgs))
    pb <- utils::txtProgressBar(min = 0, max = total_pkgs, style = 3)
    on.exit(close(pb), add = TRUE)
  }}

  for (i in seq_along(install_order)) {{
    idx <- install_order[i]
    package <- packages[idx]

    result <- tryCatch(
      install_local_archive(
        package, pkg_dir, ext, repos = repos, cran_deps = cran_deps,
        upgrade = upgrade, lib = lib, verbose = verbose
      ),
      error = function(e) list(success = FALSE, message = conditionMessage(e))
    )

    if (isTRUE(result$success) && isTRUE(result$unchanged)) {{
      unchanged_packages[[package]] <- result$message
    }} else if (isTRUE(result$success)) {{
      installed_packages[[package]] <- result$message
    }} else if (isTRUE(result$skipped)) {{
      skipped_packages[[package]] <- result$message
      warning(.meta_trf("Skipped %s: %s", package, result$message), call. = FALSE)
    }} else {{
      failed_packages[[package]] <- result$message
      warning(.meta_trf("Installation failed for %s: %s", package, result$message),
              call. = FALSE, immediate. = TRUE)
    }}

    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  }}

  if (!is.null(pb)) message(.meta_tr("Installation complete."))

  # SAFETY (2026-07): no cleanup is performed relative to the current directory.

  invisible(list(
    installed = installed_packages,
    unchanged = unchanged_packages,
    failed = failed_packages,
    skipped = skipped_packages,
    order = packages[install_order],
    selected = packages,
    pulled_in = if (is.null(only)) character() else setdiff(packages, only)
  ))
}}

# The historical duplicate load-all definition was removed.

#\' List all metapackage dependencies
#\'
#\' Returns component names and dependencies read from their archives.
#\'
#\' @param pkg_dir Character vector of archive directories.
#\' @param ext Character archive extension.
#\' @return A sorted character vector of dependency names.
#\' @export
{name}_deps <- function(
    pkg_dir{pkg_dir_default},
    ext = NULL) {{
    packages <- {.r_literal(packages)}
  deps <- unlist(lapply(
    packages, read_archive_dependencies,
    pkg_dir = pkg_dir, ext = ext
  ), use.names = FALSE)
  # DESCRIPTION is the authority for component identity.  The archive stem
  # can be unversioned or can deliberately differ from the declared package
  # name, so deriving names with sub("_.*", ...) would report a filename
  # rather than the component users actually receive.
  sort(unique(c(.component_names, deps)))
}}
')
}
#' Render generated metapackage R files
#'
#' @param name Character metapackage name.
#' @param packages Character component names without versions.
#' @param archive_stems Character archive stems including versions.
#' @param ext Character archive extension.
#' @param dest_dir Character R output directory.
#' @param implicit_deps Character implicit dependencies.
#' @param authors Character Authors@R expression.
#' @param description Character metapackage description.
#' @param license Character license declaration.
#' @param verbose Logical debug toggle.
#'
#' @return Invisible character vector of created paths.
#' @noRd
write_metapackage_files <- function(
    name,
    packages,
    archive_stems,
    ext = ".tar.gz",
    dest_dir = "R",
    implicit_deps = NULL,
    authors = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
    description = "Local Package Metapackage",
    license = "MIT + file LICENSE",
    include_archives = FALSE,
    verbose = FALSE,
    overwrite = FALSE,
    install_upgrade = "newer",
    reexport = FALSE,
    reexport_specs = list()
) {

  log_debug <- function(debug_message) {
    if (verbose) message(paste0("DEBUG: ", debug_message))
  }

  log_debug("Preparing template data")

  # Prepare shared template data once.
  template_data <- list(
    name = name,
    package_list = .r_literal(packages),
    local_packages = .r_literal(archive_stems),
    extension = "NULL",
    pkg_dir_default = .archive_dir_default(name, include_archives),
    install_upgrade = install_upgrade,
    reexport = isTRUE(reexport),
    reexport_on_load = if (isTRUE(reexport)) {
      paste0(".install_reexport_bindings(pkgname)")
    } else {
      "invisible()"
    },
    reexport_library_setup = if (isTRUE(reexport)) {
      ".set_reexport_library(lib)"
    } else {
      "invisible()"
    },
    reexport_specs = .r_ascii_literal(reexport_specs),
    install_call = if (isTRUE(include_archives)) {
      paste0(name, "_install()")
    } else {
      paste0(name, "_install(pkg_dir = PATH)")
    },
    # Single quotes inside the emitted message, which is itself a double-quoted
    # R string.
    install_call_repo = if (isTRUE(include_archives)) {
      paste0(name, "_install(cran_deps = 'install')")
    } else {
      paste0(name, "_install(pkg_dir = PATH, cran_deps = 'install')")
    },
    implicit_deps = if (!is.null(implicit_deps)) paste(implicit_deps, collapse = ", ") else ""
  )

  if (verbose) {
    log_debug("Template values:")
    log_debug(paste("name:", template_data$name))
    log_debug(paste("package_list:", template_data$package_list))
    log_debug(paste("local_packages:", template_data$local_packages))
    log_debug(paste("extension:", template_data$extension))
  }


  # Templates for the generated runtime files.
  templates <- list(
    reexports = '\n.component_reexport_specs <- {{{ reexport_specs }}}\n.reexport_state <- new.env(parent = emptyenv())\n.reexport_state$library <- character()\n.reexport_library_paths <- function() {\n  unique(c(.reexport_state$library[dir.exists(.reexport_state$library)], .libPaths()))\n}\n.set_reexport_library <- function(lib) {\n  .reexport_state$library <- normalizePath(lib, winslash = "/", mustWork = FALSE)\n  invisible(NULL)\n}\n\n.reexport_component_value <- function(package, symbol) {\n  if (!requireNamespace(package, quietly = TRUE,\n                        lib.loc = .reexport_library_paths())) {\n    return(function(...) {\n      stop(.meta_trf("Component package \'%s\' is not installed.", package), call. = FALSE)\n    })\n  }\n  getExportedValue(package, symbol)\n}\n\n.make_reexport_binding <- function(package, symbol) {\n  force(package)\n  force(symbol)\n  function(value) {\n    if (!missing(value)) {\n      stop(.meta_tr("Runtime re-export bindings are read-only."), call. = FALSE)\n    }\n    .reexport_component_value(package, symbol)\n  }\n}\n\n.install_reexport_bindings <- function(pkgname) {\n  namespace <- asNamespace(pkgname)\n  for (spec in .component_reexport_specs) {\n    makeActiveBinding(\n      spec$symbol,\n      .make_reexport_binding(spec$package, spec$symbol),\n      namespace\n    )\n  }\n  invisible(NULL)\n}\n',
    attach = '
utils::globalVariables(".pkgs")
.pkgs <- {{{ package_list }}}
.component_names <- .pkgs

attach_installed_packages <- function(pkgs, warn_missing = TRUE,
                                      lib.loc = .libPaths()) {
  already_attached <- gsub("^package:", "", search())
  to_load <- setdiff(pkgs, already_attached)
  missing <- to_load[!vapply(to_load, requireNamespace, logical(1),
                             quietly = TRUE, lib.loc = lib.loc)]
  if (warn_missing && length(missing) > 0) {
    warning(gettextf(
      "Not installed: %s. Run {{{ install_call }}} to install them.",
      paste(missing, collapse = ", "), domain = "R-{{ name }}"
    ), call. = FALSE)
  }
  to_load <- setdiff(to_load, missing)
  if (length(to_load) > 0) {
    suppressPackageStartupMessages(
      lapply(to_load, library, character.only = TRUE, lib.loc = lib.loc)
    )
  }
  invisible(list(attached = to_load, missing = missing))
}

#\' Attach installed local packages
#\'
#\' Attaches installed components with `library()`. It never installs packages;
#\' use `{{ name }}_install()` explicitly for installation.
#\'
#\' @param pkgs Character vector. Packages to attach; defaults to `.pkgs`.
#\'
#\' @return Invisibly, attachment information.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   {{ name }}_attach()
#\' }
{{ name }}_attach <- function(pkgs = .pkgs) {
  attach_installed_packages(pkgs, warn_missing = TRUE)
}

#\' Install local metapackage components
#\'
#\' Installs local archives in topological order and then attaches them. Installation
#\' is explicit and never occurs from a startup hook.
#\'
#\' @param pkg_dir Character vector of archive directories.
#\' @param ext Character archive extension.
#\' @param cran_deps Character missing non-local dependency policy.
#\' @param repos Character repositories used only by `cran_deps = "install"`.
#\' @param force Logical. Reinstall every component; equivalent to
#\'   `upgrade = "always"`.
#\' @param upgrade Character installed-version policy: `"newer"`, `"always"`,
#\'   or `"never"`. Combining `force = TRUE` with an explicit value other than
#\'   `"always"` is an error.
#\' @param verbose Logical progress toggle.
#\'
#\' @param only Optional component names; local dependencies are added automatically.
#\' @param lib Character library in which to install and verify components.
#\' @return Invisibly, structured installation results.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   {{ name }}_install(pkg_dir = "/path/to/local/archives")
#\' }
{{ name }}_install <- function(pkg_dir{{{ pkg_dir_default }}},
                               ext = NULL,
                               cran_deps = c("skip", "error", "install"),
                               repos = getOption("repos"),
                               verbose = getOption("bigbang.verbose", interactive()),
                               force = FALSE,
                               upgrade = "{{ install_upgrade }}",
                               only = NULL,
                               lib = .libPaths()[[1L]]) {
  cran_deps <- match.arg(cran_deps)
  upgrade <- resolve_upgrade_policy(force, upgrade, missing(upgrade))
  # An empty pkg_dir means the shipped archive directory was not found, which
  # produces a misleading path further down. Say what is wrong instead.
  if (!is.character(pkg_dir) || length(pkg_dir) < 1L || anyNA(pkg_dir) ||
      any(!nzchar(pkg_dir))) {
    stop(.meta_tr(
      "The component archives that ship with this package are not available. Reinstall it, or pass pkg_dir pointing at a directory holding the component archives."
    ), call. = FALSE)
  }
  missing_dirs <- pkg_dir[!dir.exists(pkg_dir)]
  if (length(missing_dirs) > 0L) {
    stop(.meta_trf("The archive directory does not exist: %s",
                   paste(missing_dirs, collapse = ", ")),
         call. = FALSE)
  }
  packages <- {{{ local_packages }}}
  {{{ reexport_library_setup }}}
  result <- install_packages_in_order(
    packages, pkg_dir, ext, verbose = verbose,
    repos = repos, cran_deps = cran_deps, upgrade = upgrade,
    only = only, lib = lib
  )
  if (length(result$failed) > 0) {
    details <- paste0(
      names(result$failed), ": ", unlist(result$failed, use.names = FALSE)
    )
    condition <- structure(
      list(
        message = gettextf(
          "Could not install all components: %s",
          paste(details, collapse = "; "), domain = "R-{{ name }}"
        ),
        call = NULL,
        failures = result$failed
      ),
      class = c("bigbang_error_install", "bigbang_error", "error", "condition")
    )
    stop(condition)
  }
  if (length(result$skipped) > 0L) {
    warning(gettextf(
      "Some components were skipped because non-local dependencies are missing: %s. Install those dependencies, or call {{{ install_call_repo }}} to obtain them from a repository.",
      paste(names(result$skipped), collapse = ", "), domain = "R-{{ name }}"
    ), call. = FALSE)
  }
  if (isTRUE(verbose) && length(result$pulled_in) > 0L) {
    message(.meta_trf(
      "Added local dependencies of selected components: %s",
      paste(result$pulled_in, collapse = ", ")
    ))
  }
  if (isTRUE(verbose) && interactive() && length(result$unchanged) > 0L) {
    message(.meta_trf(
      "Use force = TRUE or upgrade = \'always\' to reinstall unchanged packages: %s",
      paste(names(result$unchanged), collapse = ", ")
    ))
  }
  # A skipped component was just reported with its reason and the call that
  # fixes it, so attaching must not follow it with a vaguer hint.
  attach_names <- vapply(
    result$selected,
    function(item) resolve_component_spec(item, ext)$package,
    character(1L)
  )
  attach_installed_packages(
    attach_names,
    warn_missing = length(result$skipped) == 0L,
    lib.loc = lib
  )
  invisible(result)
}

#\' Deprecated alias for `{{ name }}_attach()`
#\'
#\' @return The result of `{{ name }}_attach()`.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   {{ name }}_load_all()
#\' }
{{ name }}_load_all <- function() {
  .Deprecated("{{ name }}_attach", package = "{{ name }}")
  {{ name }}_attach()
}

#\' Detach all metapackage components
#\'
#\' Detaches packages declared in `.pkgs` when present on the search path.
#\'
#\' @return Invisibly, `NULL`.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   {{ name }}_detach()
#\' }

{{ name }}_detach <- function() {
  search_entries <- paste0("package:", .pkgs)
  lapply(search_entries[search_entries %in% search()], detach, character.only = TRUE)
  invisible()
}

#\' List metapackage components
#\'
#\' @return A character vector of package names.
#\' @export
#\'
#\' @examples
#\' {{ name }}_packages()

{{ name }}_packages <- function() {
  .pkgs
}

#\' Report masking conflicts involving metapackage components
#\'
#\' Examines attached package environments and reports names exported by more
#\' than one package when at least one owner is a metapackage component.
#\'
#\' @return A named list of conflicting package search entries.
#\' @export
{{ name }}_conflicts <- function() {
  package_entries <- grep("^package:", search(), value = TRUE)
  component_entries <- intersect(paste0("package:", .pkgs), package_entries)
  if (length(component_entries) == 0L) {
    return(structure(list(), class = c("{{ name }}_conflicts", "list")))
  }

  objects <- lapply(package_entries, function(entry) {
    ls(envir = as.environment(entry), all.names = TRUE)
  })
  names(objects) <- package_entries
  candidates <- unique(unlist(objects[component_entries], use.names = FALSE))
  conflicts <- lapply(candidates, function(object) {
    package_entries[vapply(objects, function(exports) {
      object %in% exports
    }, logical(1))]
  })
  names(conflicts) <- candidates
  conflicts <- conflicts[vapply(conflicts, length, integer(1)) > 1L]
  structure(conflicts, class = c("{{ name }}_conflicts", "list"))
}

#\' @export
print.{{ name }}_conflicts <- function(x, ...) {
  if (length(x) == 0L) {
    cat(.meta_tr("No conflicts found."), "\\n")
    return(invisible(x))
  }
  cat(.meta_tr("Conflicts:"), "\\n")
  for (object in names(x)) {
    owners <- sub("^package:", "", x[[object]])
    cat("  ", object, ": ", paste(owners, collapse = ", "), "\\n", sep = "")
  }
  invisible(x)
}

#\' Attach all components without a preflight check
#\'
#\' Calls `library()` for every package in `.pkgs` and errors if one is missing.
#\'
#\' @return Invisibly, `NULL`.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   {{ name }}_attach_all()
#\' }
{{ name }}_attach_all <- function() {
  lapply(.pkgs, library, character.only = TRUE)
  invisible()
}

',
utils = '
# SAFETY NOTE (2026-07): this metapackage does not define `clean_pkg_dirs`.
# That historical helper deleted cwd-relative directories and was removed.

.meta_tr <- function(message) {
  gettext(message, domain = "R-{{ name }}")
}

.meta_trf <- function(format, ...) {
  gettextf(format, ..., domain = "R-{{ name }}")
}


#\' Utilities for {{{ name }}}
#\'
#\' This script contains utility functions for the {{{ name }}} metapackage.
#\'
#\' @keywords internal
style_startup_text <- function(x) {
  if (requireNamespace("cli", quietly = TRUE)) cli::style_bold(x) else x
}

package_version <- function(x) {
  version <- base::unclass(utils::packageVersion(x))[[1]]
  if (length(version) > 3 && requireNamespace("cli", quietly = TRUE)) {
    version[4:length(version)] <- cli::col_red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

startup_message <- function(...) {
  packageStartupMessage(style_startup_text(...))
}

#\' Generate an ASCII package banner
#\'
#\' @param name Character metapackage name.
#\' @param packages Character component names.
#\' @return The generated banner.
#\' @keywords internal
generate_ascii_banner <- function(name, packages = NULL) {
  width <- 60
  border <- paste0(rep("=", width), collapse = "")

  # Centered title
  title <- paste0(" ", name, " ")
  padding_length <- floor((width - nchar(title)) / 2)
  left_padding <- paste0(rep("-", padding_length), collapse = "")
  right_padding <- paste0(rep("-", width - padding_length - nchar(title)), collapse = "")
  title_line <- paste0(left_padding, title, right_padding)

  # Build the banner.
  banner <- c(
    border,
    title_line,
    border
  )

  # Add component information
  if (!is.null(packages) && length(packages) > 0) {
    banner <- c(banner, "")
    banner <- c(banner, .meta_tr("Included packages:"))

    for (pkg in packages) {
      # Read the version when available
      version_text <- ""
      if (requireNamespace(pkg, quietly = TRUE)) {
        tryCatch({
          version <- utils::packageVersion(pkg)
          version_text <- paste0(" (v", version, ")")
        }, error = function(e) {})
      }

      banner <- c(banner, paste0("  * ", pkg, version_text))
    }

    banner <- c(banner, "", border)
  }

  paste(banner, collapse = "\\n")
}

#\' Format the modern startup message
#\'
#\' Uses cli when it is available and returns `NULL` otherwise, allowing the
#\' caller to fall back to the dependency-free ASCII banner.
#\'
#\' @param name Character metapackage name.
#\' @param packages Character installed component names.
#\' @return A character scalar or `NULL`.
#\' @keywords internal
format_cli_startup <- function(name, packages) {
  if (!requireNamespace("cli", quietly = TRUE)) return(NULL)

  meta_version <- tryCatch(package_version(name), error = function(e) "")
  right <- trimws(paste(name, meta_version))
  heading <- cli::rule(
    left = .meta_tr("Attaching packages"),
    right = right
  )
  if (length(packages) == 0L) return(heading)

  versions <- vapply(packages, function(package) {
    tryCatch(as.character(utils::packageVersion(package)), error = function(e) "")
  }, character(1))
  name_width <- max(cli::ansi_nchar(packages))
  version_width <- max(cli::ansi_nchar(versions))
  rows <- paste(
    cli::col_green(cli::symbol$tick),
    cli::ansi_align(packages, width = name_width, align = "left"),
    cli::ansi_align(versions, width = version_width, align = "right")
  )
  paste(c(heading, rows), collapse = "\\n")
}

#\' Remove an owned temporary path safely
#\'
#\' @description
#\' Wraps `unlink()` with conservative checks. Generated code uses it only for
#\' temporary paths created by the same operation.
#\'
#\' @param path Character path vector.
#\' @param recursive Logical recursive-removal flag.
#\' @param force Logical force flag.
#\' @param verify Logical safety-check flag.
#\'
#\' @return The `unlink()` status or invisible `FALSE` when blocked.
#\'
#\' @details
#\' Checks short paths, roots, UNC paths, protected directories, and non-temporary
#\' R package sources before permitting removal.
#\' \\itemize{
#\'   \\item Rejects suspiciously short paths and filesystem roots.
#\'   \\item Rejects system and development directories.
#\'   \\item Rejects non-temporary R package source directories.
#\' }
#\'
#\' @examples
#\' \\dontrun{
#\' # Remove an owned temporary file
#\' safe_unlink("temporary-file.txt")
#\' # Attempt safe directory removal
#\' safe_unlink("temporary-directory", recursive = TRUE, force = TRUE)
#\' }
#\'
#\' @keywords internal

safe_unlink <- function(path, recursive = FALSE, force = FALSE, verify = TRUE) {

  # Safety configuration
  MIN_PATH_LENGTH <- 3  # Very short paths are suspicious

  # System and development directories that must never be removed.
  PROTECTED_DIRS <- c(
    # Operating-system directories
    "bin", "boot", "dev", "etc", "home", "lib", "mnt", "opt", "proc", "root",
    "run", "sbin", "srv", "sys", "tmp", "usr", "var", "Program Files",
    "Windows", "Users", "System32", "AppData", "ProgramData",

    # R and development directories
    "library", "include", "share", "R", "Rtools", "Git", "src",

    # Version-control and configuration directories
    ".git", ".svn", ".hg", "node_modules"
  )

  # Potentially dangerous path patterns
  DANGEROUS_PATTERNS <- c(
    "^[A-Za-z]:\\\\\\\\$",  # C:\\, D:\\, etc.
    "^/$",             # Unix filesystem root
    "^\\\\\\\\\\\\\\\\",       # UNC paths such as \\\\server\\
    "^~$",             # Home directory
    "^\\\\.$",           # Current directory
    "^\\\\.\\\\.$"         # Parent directory
  )

  # Run conservative validation unless explicitly disabled.
  if (verify) {
    if (is.character(path) && length(path) > 0) {
      for (p in path) {
        # Reject suspiciously short paths such as roots.
        if (nchar(p) < MIN_PATH_LENGTH) {
          message(.meta_trf("SAFETY: Path is too short and may be dangerous: %s", p))
          return(invisible(FALSE))
        }

        # Reject known dangerous patterns.
        if (any(sapply(DANGEROUS_PATTERNS, function(pattern) grepl(pattern, p)))) {
          message(.meta_trf("SAFETY: Potentially dangerous path pattern: %s", p))
          return(invisible(FALSE))
        }

        # Apply directory-specific checks.
        if (dir.exists(p)) {
          # Never remove protected directories.
          if (basename(p) %in% PROTECTED_DIRS) {
            message(.meta_trf("SAFETY: Potentially important directory: %s", p))
            return(invisible(FALSE))
          }

          # Forced recursive removal requires additional source-tree checks.
          if (recursive && force) {
            # Detect an R package source tree.
            has_desc <- file.exists(file.path(p, "DESCRIPTION"))
            has_r_dir <- dir.exists(file.path(p, "R"))
            has_man_dir <- dir.exists(file.path(p, "man"))

            if (has_desc && (has_r_dir || has_man_dir)) {
              # Only known temporary package directories may pass.
              is_temp_pkg <- grepl("^00LOCK-|^\\\\.Rcheck$|^tmp|^temp", basename(p))

              if (!is_temp_pkg) {
                message(.meta_trf("SAFETY: Possible non-temporary R package directory: %s", p))
                return(invisible(FALSE))
              }
            }
          }
        }
      }
    }
  }

  # Delegate only after every check passes
  result <- unlink(path, recursive = recursive, force = force)

  # Report incomplete removal
  if (result != 0) {
    warning(.meta_trf("Could not remove completely: %s", paste(path, collapse = ", ")),
            call. = FALSE)
  }

  result
}

#\' Check whether one path is inside another
#\'
#\' @description
#\' Normalizes and compares paths without relying on partial prefix matches.
#\'
#\' @param inner_path Character candidate child path.
#\' @param outer_path Character candidate parent path.
#\'
#\' @return `TRUE` when `inner_path` is contained by `outer_path`.
#\'
#\' @examples
#\' \\dontrun{
#\' # Check a project file
#\' is_path_inside("R/file.R", getwd())
#\' # Check an owned temporary directory
#\' is_path_inside(file.path(tempdir(), "subdir"), tempdir())
#\' }
#\'
#\' @keywords internal

is_path_inside <- function(inner_path, outer_path) {
  # Normalize paths before comparing components.
  # Both sides use the same separator convention as the rest of the package. A
  # path that does not exist comes back from normalizePath() unchanged, so
  # mixing conventions would make the comparison fail on Windows.
  inner <- normalizePath(inner_path, winslash = "/", mustWork = FALSE)
  outer <- normalizePath(outer_path, winslash = "/", mustWork = FALSE)

  # Use one separator representation on Windows.
  if (.Platform$OS.type == "windows") {
    inner <- gsub("\\\\\\\\", "/", inner)
    outer <- gsub("\\\\\\\\", "/", outer)
  }

  # Add a separator to prevent partial-prefix matches.
  if (!endsWith(outer, "/")) {
    outer <- paste0(outer, "/")
  }

  startsWith(inner, outer)
}

',
zzz = '
#\' Package namespace initialization
#\'
#\' Side-effect-free namespace load hook.
#\'
#\' @details
#\' Component installation is exclusively explicit through `{{ name }}_install()`.
#\'
#\' @param libname Character library path.
#\' @param pkgname Character package name.
#\'
#\' @return Invisibly, `NULL`.
#\' @noRd

.onLoad <- function(libname, pkgname) {
  # Safety fix 2026-07: .onLoad never installs packages or deletes files.
  {{{ reexport_on_load }}}
  invisible()
}


#\' Package attachment hook
#\'
#\' Attaches components that are already installed and reports missing ones.
#\'
#\' @param libname Character library path.
#\' @param pkgname Character package name.
#\'
#\' @return Invisibly, `NULL`.
#\' @noRd

.onAttach <- function(libname, pkgname) {
  # Safety fix 2026-07: no deletion and no installation from startup.
  pkg_base_names <- .component_names

{{^reexport}}  # Tidyverse-style startup hook: delegate search-path changes to a helper.
  result <- attach_installed_packages(pkg_base_names, warn_missing = FALSE)
  missing <- result$missing
  installed <- setdiff(pkg_base_names, missing)
{{/reexport}}{{#reexport}}  # Runtime re-exports stay lazy; do not attach components here.
  missing <- pkg_base_names[!vapply(pkg_base_names, function(package) {
    length(find.package(
      package, lib.loc = .reexport_library_paths(), quiet = TRUE
    )) > 0L
  }, logical(1))]
  installed <- setdiff(pkg_base_names, missing)
{{/reexport}}

  if (isTRUE(getOption("{{ name }}.quiet", FALSE))) return(invisible())

  formatted <- format_cli_startup("{{{ name }}}", installed)
  if (!is.null(formatted)) {
    startup_message(paste0("\\n", formatted, "\\n"))
  } else {
    banner <- generate_ascii_banner("{{{ name }}}", pkg_base_names)
    startup_message(paste0("\\n", banner, "\\n"))
    if (length(installed) > 0) {
      startup_message(.meta_trf(
        "Attached packages: %s", paste(installed, collapse = ", ")
      ))
    }
  }
  if (length(missing) > 0) {
    packageStartupMessage(.meta_trf(
      "Components still need installation: %s\\nRun {{{ install_call }}} to install them from local archives.",
      paste(missing, collapse = ", ")
    ))
  }
  invisible()
}


#\' Safe package unload hook
#\'
#\' Detaches attached components without deleting any files or directories.
#\'
#\' @param libpath Character library path.
#\'
#\' @return Invisibly, `NULL`.
#\' @noRd
.onUnload <- function(libpath) {
  # Detach only; never delete.
  tryCatch({
    if (exists(".pkgs")) {
      for (pkg in .pkgs) {
        # Detach without unloading component namespaces.
        try(detach(paste0("package:", pkg), character.only = TRUE, unload = FALSE),
            silent = TRUE)
      }
    }
  }, error = function(e) {
    # Report unload errors without turning them into destructive recovery.
    message(.meta_trf("Note: Error during safe unload: %s", e$message))
  })

  # No cleanup or deletion operation is allowed here.
  invisible()
    }
'
  )

  if (!isTRUE(reexport)) templates$reexports <- NULL

  # Ensure the destination directory exists.
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Render and write every runtime template.
  created_files <- character(0)
  for (file_name in names(templates)) {
    file_path <- file.path(dest_dir, paste0(file_name, ".R"))

    if (isTRUE(overwrite) || !file.exists(file_path)) {
      tryCatch({
        content <- whisker::whisker.render(
          template = templates[[file_name]],
          data = template_data,
          partials = list()
        )
        content <- .drop_regular_comment_lines(content)

        # Reject empty rendered output
        if (nchar(content) == 0) {
          stop(.bb_tr("The template rendered empty content."), call. = FALSE)
        }

        .write_utf8(content, file_path)
        if (verbose) {
          message(.bb_trf("Created %s.R successfully.", file_name))
        }
        created_files <- c(created_files, file_path)
      }, error = function(e) {
        warning(.bb_trf("Error creating %s.R: %s", file_name, e$message),
                call. = FALSE)
        # Emit diagnostic context in verbose mode
        if (verbose) {
          message()
          message(.bb_tr("Original template:"))
          message(templates[[file_name]])
          message()
          message(.bb_tr("Template data:"))
          utils::str(template_data)
        }
      })
    } else {
      if (verbose) {
        message(.bb_trf("%s.R already exists and will not be overwritten.", file_name))
      }
    }
  }

  invisible(created_files)
}
