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
  paste0(' = base::system.file("', .archive_subdir, '", package = "', name, '")')
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
  condition <- base::structure(
    base::c(base::list(message = message, call = NULL), base::list(...)),
    class = c(class, "bigbang_error", "error", "condition")
  )
  base::stop(condition)
}}

resolve_upgrade_policy <- function(force, upgrade, upgrade_missing) {{
  if (!base::is.logical(force) || base::length(force) != 1L || base::is.na(force)) {{
    .bigbang_abort(
      "bigbang_error_install_policy",
      .meta_tr("\'force\' must be TRUE or FALSE")
    )
  }}
    upgrade <- base::match.arg(upgrade, base::c("newer", "always", "never"))
  if (base::isTRUE(force)) {{
    if (!base::isTRUE(upgrade_missing) && !base::identical(upgrade, "always")) {{
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
  libraries <- base::unique(base::normalizePath(
    libraries[base::dir.exists(libraries)], winslash = "/", mustWork = TRUE
  ))
  library_path <- base::paste(libraries, collapse = .Platform$path.sep)
  previous <- base::Sys.getenv("R_LIBS_USER", unset = NA_character_)
  on.exit({{
    if (base::is.na(previous)) {{
      base::Sys.unsetenv("R_LIBS_USER")
    }} else {{
      base::Sys.setenv(R_LIBS_USER = previous)
    }}
  }}, add = TRUE)
  # install.packages() always rebuilds R_LIBS from the current .libPaths(), so
  # R_LIBS_USER is the channel that preserves additional libraries for its child.
  base::Sys.setenv(R_LIBS_USER = library_path)
  base::force(code)
}}

install_source_component <- function(target, lib, verbose = TRUE) {{
  # utils::install.packages() discards the output of the child installer for
  # local source packages, so a failure only reports a non-zero exit status.
  # Run the same R CMD INSTALL directly and keep the output: the ERROR lines
  # of the child become the failure message instead of a generic one.
  log_file <- base::tempfile("bigbang-install-log-")
  on.exit(base::unlink(log_file, force = TRUE), add = TRUE)
  target <- base::normalizePath(base::path.expand(target), winslash = "/", mustWork = FALSE)
  r_binary <- base::file.path(
    base::R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  status <- base::system2(
    r_binary, base::c("CMD", "INSTALL", "-l", base::shQuote(lib), base::shQuote(target)),
    stdout = log_file, stderr = log_file
  )
  output <- if (base::file.exists(log_file)) {{
    base::readLines(log_file, warn = FALSE)
  }} else {{
    base::character()
  }}
  if (base::isTRUE(verbose) && base::length(output) > 0L) base::cat(output, sep = "\\n")
  if (!identical(status, 0L)) {{
    detail <- base::grep("ERROR", output, value = TRUE, fixed = TRUE)
    if (base::length(detail) == 0L) detail <- utils::tail(output, 5L)
    detail <- base::paste(detail, collapse = " | ")
    if (!base::nzchar(detail)) {{
      detail <- base::sprintf("R CMD INSTALL exited with status %d", status)
    }}
    stop(detail, call. = FALSE)
  }}
  base::invisible(TRUE)
}}

.component_specs <- {component_specs_literal}
.component_names <- {package_list_literal}

.archive_cache_new <- function() {{
  cache <- base::new.env(parent = base::emptyenv())
  cache$entries <- base::new.env(hash = TRUE, parent = base::emptyenv())
  cache$extracted <- base::character()
  cache
}}

.archive_cache_entry <- function(cache, archive, ext) {{
  info <- base::file.info(archive)
  key <- base::paste(
    base::normalizePath(archive, winslash = "/", mustWork = TRUE),
    base::as.character(info$size), base::as.numeric(info$mtime),
    base::tolower(ext), sep = "|"
  )
  if (base::exists(key, envir = cache$entries, inherits = FALSE)) {{
    return(base::get(key, envir = cache$entries, inherits = FALSE))
  }}
  entry <- base::new.env(parent = base::emptyenv())
  entry$archive <- base::normalizePath(archive, winslash = "/", mustWork = TRUE)
  entry$listing <- NULL
  entry$extract_dir <- NULL
  base::assign(key, entry, envir = cache$entries)
  entry
}}

.archive_cache_listing <- function(cache, archive, ext) {{
  entry <- .archive_cache_entry(cache, archive, ext)
  if (base::is.null(entry$listing)) {{
    entry$listing <- base::suppressWarnings({{
      if (base::identical(base::tolower(ext), ".zip")) utils::unzip(archive, list = TRUE)
      else utils::untar(archive, list = TRUE)
    }})
  }}
  entry$listing
}}

.archive_cache_extract <- function(cache, archive, ext) {{
  entry <- .archive_cache_entry(cache, archive, ext)
  if (!base::is.null(entry$extract_dir)) return(entry$extract_dir)
  listing <- .archive_cache_listing(cache, archive, ext)
  members <- if (base::identical(base::tolower(ext), ".zip")) listing$Name else listing
  members <- base::gsub("\\\\", "/", members, fixed = TRUE)
  unsafe <- base::startsWith(members, "/") |
    base::grepl("^[A-Za-z]:", members) |
    base::grepl("(^|/)\\\\.\\\\.(/|$)", members, perl = TRUE)
  if (base::any(unsafe)) {{
    base::stop(.meta_trf(
      "Archive contains unsafe absolute or parent-traversal paths: %s",
      base::paste(utils::head(members[unsafe], 3L), collapse = ", ")
    ), call. = FALSE)
  }}
  extract_dir <- base::tempfile("bigbang-archive-cache-")
  if (!base::dir.create(extract_dir)) base::stop(.meta_tr(
    "Could not create a temporary archive directory."), call. = FALSE)
  extraction <- base::tryCatch(base::suppressWarnings({{
    if (base::identical(base::tolower(ext), ".zip")) utils::unzip(archive, exdir = extract_dir)
    else utils::untar(archive, exdir = extract_dir)
  }}), error = base::identity)
  if (base::inherits(extraction, "error")) {{
    safe_unlink(extract_dir, recursive = TRUE)
    base::stop(.meta_trf(
      "Could not extract archive %s: %s", archive, base::conditionMessage(extraction)
    ), call. = FALSE)
  }}
  if (base::is.numeric(extraction) && base::length(extraction) == 1L && extraction != 0) {{
    safe_unlink(extract_dir, recursive = TRUE)
    base::stop(.meta_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, extraction
    ), call. = FALSE)
  }}
  extracted <- base::list.files(
    extract_dir, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, include.dirs = TRUE, no.. = TRUE
  )
  if (base::length(extracted) > 0L && base::any(base::nzchar(base::Sys.readlink(extracted)))) {{
    safe_unlink(extract_dir, recursive = TRUE)
    base::stop(.meta_trf(
      "Archive %s contains symbolic links, which are not supported.", archive
    ), call. = FALSE)
  }}
  entry$extract_dir <- extract_dir
  cache$extracted <- base::c(cache$extracted, extract_dir)
  extract_dir
}}

resolve_component_spec <- function(package, ext = NULL) {{
  if (base::`%in%`(package, base::names(.component_specs))) return(.component_specs[[package]])
  by_stem <- base::vapply(.component_specs, function(spec) base::identical(spec$stem, package),
                    base::logical(1L))
  if (base::sum(by_stem) == 1L) return(.component_specs[[base::which(by_stem)]])
  fallback_ext <- if (base::is.null(ext)) ".tar.gz" else ext
  list(
    package = base::sub("_.*", "", package),
    stem = package,
    ext = fallback_ext
  )
}}

resolve_component_archive <- function(package, pkg_dir, ext = NULL) {{
  spec <- resolve_component_spec(package, ext)
  if (!base::is.character(pkg_dir) || base::length(pkg_dir) < 1L || base::anyNA(pkg_dir) ||
      base::any(!base::nzchar(pkg_dir))) {{
    stop(.meta_tr(
      "The archive directory must be one or more non-empty paths."
    ), call. = FALSE)
  }}
  dirs <- base::normalizePath(pkg_dir, winslash = "/", mustWork = FALSE)
  candidates <- base::file.path(dirs, base::paste0(spec$stem, spec$ext))
  found <- candidates[base::file.exists(candidates) & !base::dir.exists(candidates)]
  if (length(found) == 0L) {{
    expected <- base::tolower(base::basename(candidates))
    found <- base::unlist(base::lapply(dirs, function(dir) {{
      files <- base::list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      files[base::`%in%`(base::tolower(base::basename(files)), expected) & !base::dir.exists(files)]
    }}), use.names = FALSE)
  }}
  if (base::length(found) == 0L) {{
    stop(.meta_trf(
      "Could not resolve component \'%s\' in the supplied archive directories.",
      package
    ), call. = FALSE)
  }}
  list(path = base::normalizePath(found[[1L]], winslash = "/", mustWork = TRUE),
       ext = spec$ext, stem = spec$stem)
}}

# .archive_warning_state belongs to the generated runtime, not to the
# roxygen block for read_archive_metadata.
.archive_warning_state <- base::new.env(parent = base::emptyenv())
.archive_warning_state$active <- FALSE
.archive_warning_state$seen <- base::character()
.warn_archive_filename_mismatch <- function(archive, message) {{
  key <- base::normalizePath(archive, winslash = "/", mustWork = FALSE)
  if (!base::isTRUE(.archive_warning_state$active)) {{
    base::warning(message, call. = FALSE)
    return(base::invisible(NULL))
  }}
  if (!base::`%in%`(key, .archive_warning_state$seen)) {{
    .archive_warning_state$seen <- base::c(.archive_warning_state$seen, key)
    base::warning(message, call. = FALSE)
  }}
  base::invisible(NULL)
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
read_archive_metadata <- function(package, pkg_dir, ext = NULL, cache = NULL) {{
  resolved <- resolve_component_archive(package, pkg_dir, ext)
  archive <- resolved$path
  ext <- resolved$ext

  if (base::is.null(cache)) {{
    temp_dir <- tempfile("bigbang-deps-")
    if (!dir.create(temp_dir)) stop(.meta_trf(
      "Could not extract archive %s: could not create temporary directory.",
      archive
    ), call. = FALSE)
    on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)
  }}

  if (!identical(tolower(ext), ".zip") && !base::`%in%`(ext, base::c(".tar.gz", ".tar"))) {{
    stop(.meta_trf("Unsupported archive format: %s", ext), call. = FALSE)
  }}
  listing <- base::tryCatch(
    if (base::is.null(cache)) base::suppressWarnings({{
      if (base::identical(base::tolower(ext), ".zip")) utils::unzip(archive, list = TRUE)
      else utils::untar(archive, list = TRUE)
    }}) else .archive_cache_listing(cache, archive, ext),
    error = base::identity
  )
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
  if (!base::is.null(cache)) temp_dir <- .archive_cache_extract(cache, archive, ext)
  extraction <- if (base::is.null(cache)) base::tryCatch(base::suppressWarnings({{
    if (base::identical(base::tolower(ext), ".zip")) utils::unzip(archive, exdir = temp_dir)
    else utils::untar(archive, exdir = temp_dir)
  }}), error = base::identity) else 0L
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
    if (!base::`%in%`(name, colnames(desc))) return(NA_character_)
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
    .warn_archive_filename_mismatch(archive, .meta_trf(
      "Archive %s declares package %s, but its filename suggests %s.",
      archive, declared_package, expected_package
    ))
  }}
  if (has_version && !tryCatch(
      isTRUE(base::package_version(declared_version) ==
             base::package_version(expected_version)),
      error = function(e) FALSE
  )) {{
    .warn_archive_filename_mismatch(archive, .meta_trf(
      "Archive %s declares version %s, but its filename suggests version %s.",
      archive, declared_version, expected_version
    ))
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

read_archive_dependencies <- function(package, pkg_dir, ext = NULL, cache = NULL) {{
  read_archive_metadata(package, pkg_dir, ext, cache = cache)$dependencies
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

validate_local_constraints <- function(packages, pkg_dir, ext = NULL, cache = NULL) {{
  metadata <- lapply(
    packages, read_archive_metadata, pkg_dir = pkg_dir, ext = ext, cache = cache
  )
  package_names <- vapply(metadata, function(item) item$package, character(1L))
  versions <- vapply(metadata, function(item) item$version, character(1L))
  names(versions) <- package_names
  for (index in seq_along(metadata)) {{
    constraints <- metadata[[index]]$constraints
    local <- constraints[vapply(constraints, function(item) {{
      base::`%in%`(item$package, package_names)
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
classify_package_archive <- function(archive, ext, cache = NULL) {{
  if (!identical(tolower(ext), ".zip")) return("source")

  members <- if (is.null(cache)) utils::unzip(archive, list = TRUE)$Name else {{
    .archive_cache_listing(cache, archive, ext)$Name
  }}
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
                                   lib = .libPaths()[[1L]], verbose = TRUE,
                                   cache = NULL) {{
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
    read_archive_metadata(package, pkg_dir, ext, cache = cache),
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
    if (base::isTRUE(verbose)) message(if (newer) {{
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
    classify_package_archive(archive, ext, cache = cache),
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
    source_dir <- if (is.null(cache)) tempfile("bigbang-source-zip-") else
      .archive_cache_extract(cache, archive, ext)
    if (is.null(cache)) {{
      dir.create(source_dir)
      on.exit(safe_unlink(source_dir, recursive = TRUE), add = TRUE)
      utils::unzip(archive, exdir = source_dir)
    }}
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

  if (base::isTRUE(verbose) && identical(base_name, package)) {{
    message(.meta_trf("Installed package %s successfully.", base_name))
  }} else if (base::isTRUE(verbose)) {{
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

build_dependency_graph <- function(packages, pkg_dir, ext = NULL, cache = NULL) {{
  package_count <- length(packages)
  adjacency <- base::matrix(0, nrow = package_count, ncol = package_count)
  rownames(adjacency) <- colnames(adjacency) <- packages
  package_names <- vapply(
    packages,
    function(package) resolve_component_spec(package, ext)$package,
    character(1L)
  )
  validate_local_constraints(packages, pkg_dir, ext, cache = cache)

  for (package in packages) {{
    deps <- read_archive_dependencies(package, pkg_dir, ext, cache = cache)
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
  archive_cache <- .archive_cache_new()
  on.exit({{
    if (base::length(archive_cache$extracted) > 0L) {{
      safe_unlink(base::unique(archive_cache$extracted), recursive = TRUE)
    }}
  }}, add = TRUE)
  .archive_warning_state$seen <- base::character()
  .archive_warning_state$active <- TRUE
  base::on.exit({{
    .archive_warning_state$active <- FALSE
    .archive_warning_state$seen <- base::character()
  }}, add = TRUE)
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
                                    pkg_dir = pkg_dir, ext = ext,
                                    cache = archive_cache),
                             use.names = FALSE)
      pulled <- setdiff(intersect(dependencies, .component_names), selected)
      if (length(pulled) == 0L) break
      selected <- c(selected, pulled)
    }}
    packages <- selected
  }}
  adjacency <- build_dependency_graph(packages, pkg_dir, ext, cache = archive_cache)
  install_order <- topological_order(adjacency)
  package_names <- base::vapply(
    packages,
    function(package) resolve_component_spec(package, ext)$package,
    character(1L)
  )

  installed_packages <- list()
  unchanged_packages <- list()
  failed_packages <- list()
  skipped_packages <- list()
  skipped_local_packages <- list()
  skipped_nonlocal_packages <- list()
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

    dependencies <- tryCatch(
      read_archive_dependencies(
        package, pkg_dir, ext, cache = archive_cache
      ),
      error = function(e) character()
    )
    local_dependencies <- intersect(dependencies, package_names)
    skipped_dependencies <- local_dependencies[vapply(
      local_dependencies,
      function(dependency) {{
        dependency_index <- which(package_names == dependency)
        length(dependency_index) == 1L &&
          !is.null(skipped_packages[[packages[[dependency_index]]]])
      }},
      logical(1L)
    )]
    result <- if (length(skipped_dependencies) > 0L) {{
      dependency <- skipped_dependencies[[1L]]
      dependency_index <- which(package_names == dependency)[[1L]]
      list(
        success = FALSE,
        skipped = TRUE,
        skip_kind = "local",
        message = .meta_trf(
          "Skipped because local dependency %s was skipped: %s",
          dependency, skipped_packages[[packages[[dependency_index]]]]
        )
      )
    }} else {{
      tryCatch(
        install_local_archive(
          package, pkg_dir, ext, repos = repos, cran_deps = cran_deps,
          upgrade = upgrade, lib = lib, verbose = verbose,
          cache = archive_cache
        ),
        error = function(e) list(success = FALSE, message = conditionMessage(e))
      )
    }}

    if (isTRUE(result$success) && isTRUE(result$unchanged)) {{
      unchanged_packages[[package]] <- result$message
    }} else if (isTRUE(result$success)) {{
      installed_packages[[package]] <- result$message
    }} else if (isTRUE(result$skipped)) {{
      skipped_packages[[package]] <- result$message
      if (identical(result$skip_kind, "local")) {{
        skipped_local_packages[[package]] <- result$message
      }} else {{
        skipped_nonlocal_packages[[package]] <- result$message
      }}
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
    skipped_local = skipped_local_packages,
    skipped_nonlocal = skipped_nonlocal_packages,
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
.qualify_generated_runtime_calls <- function(content) {
  packages <- c("base", "utils", "tools", "methods")
  package_functions <- lapply(packages, function(package) {
    exports <- getNamespaceExports(package)
    exports <- exports[grepl("^[A-Za-z.][A-Za-z0-9._]*$", exports)]
    setdiff(exports, c("break", "else", "for", "function", "if", "next", "repeat", "while"))
  })
  owners <- character()
  for (index in seq_along(packages)) {
    names <- setdiff(package_functions[[index]], names(owners))
    owners[names] <- packages[[index]]
  }
  parsed <- tryCatch(parse(text = content, keep.source = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed)) return(content)
  # Text is needed only for terminal tokens and a few assignment targets;
  # computing it for every node of a large generated file is very slow.
  data <- utils::getParseData(parsed, includeText = NA)
  if (is.null(data) || nrow(data) == 0L) return(content)
  node_text <- function(id) {
    needed <- data[data$id == id, , drop = FALSE]
    attr(needed, "srcfile") <- attr(data, "srcfile")
    tryCatch(utils::getParseText(needed, id), error = function(e) "")
  }
  defined_functions <- character()
  function_ids <- data$id[data$token == "FUNCTION"]
  function_nodes <- data$parent[data$id %in% function_ids]
  for (function_node in function_nodes) {
    root <- data$parent[data$id == function_node]
    if (length(root) != 1L) next
    assignment <- data[data$parent == root & data$token %in%
                          c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "RIGHT_ASSIGN2"),
                        , drop = FALSE]
    if (nrow(assignment) != 1L) next
    lhs <- data[data$parent == root & data$token %in%
                  c("expr", "expr_or_assign_or_help") &
                  data$col1 < assignment$col1[[1L]], , drop = FALSE]
    if (nrow(lhs) == 0L) next
    lhs <- lhs[order(lhs$col1, lhs$id), , drop = FALSE][1L, , drop = FALSE]
    name <- lhs$text[[1L]]
    if (is.na(name) || !nzchar(name)) name <- node_text(lhs$id[[1L]])
    name <- trimws(name)
    if (grepl("^`?[A-Za-z.][A-Za-z0-9._]*`?$", name, perl = TRUE)) {
      defined_functions <- c(defined_functions, sub("^`|`$", "", name))
    }
  }
  defined_functions <- unique(defined_functions)
  calls <- data[data$token == "SYMBOL_FUNCTION_CALL" &
                  data$text %in% setdiff(names(owners), defined_functions),
                , drop = FALSE]
  if (nrow(calls) == 0L) return(content)
  lines <- strsplit(content, "\n", fixed = TRUE)[[1L]]
  edits <- lapply(seq_len(nrow(calls)), function(index) {
    row <- calls[index, , drop = FALSE]
    line <- lines[[row$line1[[1L]]]]
    prefix <- if (row$col1[[1L]] <= 1L) "" else {
      substr(line, 1L, row$col1[[1L]] - 1L)
    }
    if (grepl("::[[:space:]]*$", prefix, perl = TRUE)) return(NULL)
    list(
      line = row$line1[[1L]], col = row$col1[[1L]],
      text = paste0(owners[[row$text[[1L]]]], "::")
    )
  })
  edits <- Filter(Negate(is.null), edits)
  if (length(edits) == 0L) return(content)
  order_index <- order(
    vapply(edits, `[[`, integer(1L), "line"),
    vapply(edits, `[[`, integer(1L), "col"),
    decreasing = TRUE
  )
  for (index in order_index) {
    edit <- edits[[index]]
    line <- lines[[edit$line]]
    lines[[edit$line]] <- paste0(
      substr(line, 1L, edit$col - 1L), edit$text,
      substr(line, edit$col, nchar(line))
    )
  }
  paste(lines, collapse = "\n")
}

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

  qualify_runtime_calls <- .qualify_generated_runtime_calls

  log_debug <- function(debug_message) {
    if (verbose) message(.bb_trf("DEBUG: %s", debug_message))
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
    attach_warn_conflicts = if (isTRUE(reexport)) "FALSE" else "TRUE",
    attach_body = paste0(
      "  attach_installed_packages(pkgs, warn_missing = TRUE, ",
      "warn_conflicts = ",
      if (isTRUE(reexport)) "FALSE" else "TRUE",
      ")"
    ),
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
    reexport_verification_call = if (isTRUE(reexport)) {
      ".reexport_verify(warn = TRUE)"
    } else {
      "base::data.frame()"
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

  masking_conflicts_body <- c(
    "  package_entries <- grep(\"^package:\", search(), value = TRUE)",
    "  component_entries <- intersect(paste0(\"package:\", .pkgs), package_entries)",
    "  if (length(component_entries) == 0L) {",
    paste0("    return(structure(list(), class = c(\"", name, "_conflicts\", \"list\")))"),
    "  }",
    "  objects <- lapply(package_entries, function(entry) {",
    "    package <- sub(\"^package:\", \"\", entry)",
    "    tryCatch(base::getNamespaceExports(package), error = function(e) character())",
    "  })",
    "  names(objects) <- package_entries",
    "  candidates <- unique(unlist(objects[component_entries], use.names = FALSE))",
    "  conflicts <- lapply(candidates, function(object) package_entries[vapply(objects, function(exports) object %in% exports, logical(1))])",
    "  names(conflicts) <- candidates",
    "  conflicts <- conflicts[vapply(conflicts, length, integer(1)) > 1L]",
    paste0("  structure(conflicts, class = c(\"", name, "_conflicts\", \"list\"))")
  )

  if (isTRUE(reexport)) {
    template_data$conflicts_function <- paste(c(
      "  .reexport_verify_subprocess <- function(specs, library) {",
      "    input <- output <- script <- stdout <- stderr <- NA_character_",
      "    base::on.exit(base::unlink(base::Filter(function(path) !base::is.na(path), base::c(input, output, script, stdout, stderr)), force = TRUE), add = TRUE)",
      "    prepared <- base::tryCatch({",
      "      input <- base::tempfile(\"bigbang-reexport-verify-input-\")",
      "      output <- base::tempfile(\"bigbang-reexport-verify-output-\")",
      "      script <- base::tempfile(\"bigbang-reexport-verify-script-\", fileext = \".R\")",
      "      stdout <- base::tempfile(\"bigbang-reexport-verify-stdout-\")",
      "      stderr <- base::tempfile(\"bigbang-reexport-verify-stderr-\")",
      "      base::saveRDS(base::list(specs = specs, library = library, paths = base::.libPaths()), input)",
      "      input_literal <- base::deparse(input)",
      "      output_literal <- base::deparse(output)",
      "      script_lines <- base::c(",
      "        base::paste0(\"payload <- base::readRDS(\", input_literal, \")\"),",
      "      \"ordered_paths <- base::unique(base::c(payload$library[base::dir.exists(payload$library)], payload$paths[base::dir.exists(payload$paths)]))\",",
      "      \"base::.libPaths(base::unique(base::c(ordered_paths, base::.libPaths())))\",",
      "      \"target <- function(package) {\",",
      "        \"candidate <- base::find.package(package, lib.loc = ordered_paths, quiet = TRUE)\",",
      "        \"if (base::length(candidate) == 0L) return(NA_character_)\",",
      "        \"base::normalizePath(candidate[[1L]], winslash = '/', mustWork = FALSE)\",",
      "      \"}\",",
      "      \"load_target <- function(package) {\",",
      "        \"target_path <- target(package)\",",
      "        \"if (base::is.na(target_path)) return(base::list(status = 'not_installed', ok = FALSE, path = NA_character_, target = target_path))\",",
      "        \"loaded <- base::tryCatch(base::requireNamespace(package, quietly = TRUE, lib.loc = base::.libPaths()), error = function(e) FALSE)\",",
      "        \"if (!base::isTRUE(loaded)) return(base::list(status = 'not_loadable', ok = FALSE, path = NA_character_, target = target_path))\",",
      "        \"namespace <- base::getNamespace(package)\",",
      "        \"path <- base::normalizePath(base::getNamespaceInfo(namespace, 'path'), winslash = '/', mustWork = FALSE)\",",
      "        \"same_path <- base::identical(path, target_path)\",",
      "        \"base::list(status = if (same_path) 'installed' else 'foreign', ok = same_path, path = path, target = target_path)\",",
      "      \"}\",",
      "      \"rows <- base::lapply(payload$specs, function(spec) {\",",
      "        \"loaded <- base::lapply(spec$candidates, load_target)\",",
      "        \"installed <- base::vapply(loaded, function(item) item$status %in% base::c('installed', 'foreign'), base::logical(1))\",",
      "        \"values <- base::lapply(base::seq_along(spec$candidates), function(index) {\",",
      "          \"if (!installed[[index]]) return(base::list(ok = FALSE))\",",
      "          \"base::tryCatch(base::list(ok = TRUE, value = base::getExportedValue(spec$candidates[[index]], spec$symbol)), error = function(e) base::list(ok = FALSE))\",",
      "        \"})\",",
      "        \"available <- base::vapply(values, function(value) base::isTRUE(value$ok), base::logical(1))\",",
      "        \"not_installed <- spec$candidates[base::vapply(loaded, function(item) base::identical(item$status, 'not_installed'), base::logical(1))]\",",
      "        \"not_loadable <- spec$candidates[base::vapply(loaded, function(item) base::identical(item$status, 'not_loadable'), base::logical(1))]\",",
      "        \"not_exported <- spec$candidates[installed & !available]\",",
      "        \"foreign <- spec$candidates[base::vapply(loaded, function(item) base::identical(item$status, 'foreign'), base::logical(1))]\",",
      "        \"missing <- spec$candidates[!available]\",",
      "        \"same <- NA\",",
      "        \"equivalent <- FALSE\",",
      "        \"if (base::all(available)) {\",",
      "          \"objects <- base::lapply(values, function(value) value[['value']])\",",
      "          \"same <- base::all(base::vapply(objects[-1L], base::identical, base::logical(1), y = objects[[1L]]))\",",
      "          \"if (base::length(objects) > 1L && base::identical(same, FALSE)) equivalent <- base::all(base::vapply(objects[-1L], function(value) base::is.function(value) && base::is.function(objects[[1L]]) && base::identical(base::body(value), base::body(objects[[1L]])) && base::identical(base::formals(value), base::formals(objects[[1L]])), base::logical(1)))\",",
      "        \"}\",",
      "        \"base::data.frame(symbol = spec$symbol, package = spec$package, resolution = spec$resolution, diagnosis = spec$diagnosis, candidates = base::paste(spec$candidates, collapse = ', '), installed = base::paste(spec$candidates[installed], collapse = ', '), missing = base::paste(missing, collapse = ', '), not_installed = base::paste(not_installed, collapse = ', '), not_loadable = base::paste(not_loadable, collapse = ', '), not_exported = base::paste(not_exported, collapse = ', '), loaded_from_other_library = base::paste(foreign, collapse = ', '), identical = same, equivalent = equivalent, stringsAsFactors = FALSE)\",",
      "      \"})\",",
      "      base::paste0(\"base::saveRDS(rows, \", output_literal, \")\")",
      "    )",
      "      base::writeLines(script_lines, script, useBytes = TRUE)",
      "      TRUE",
      "    }, error = function(e) FALSE)",
      "    if (!base::isTRUE(prepared)) return(NULL)",
      "    r_binary <- base::file.path(base::R.home(\"bin\"), if (base::.Platform$OS.type == \"windows\") \"R.exe\" else \"R\")",
      "    status <- base::tryCatch(base::system2(r_binary, base::c(\"--vanilla\", \"-f\", base::shQuote(script)), stdout = stdout, stderr = stderr), error = base::identity)",
      "    if (base::inherits(status, \"error\") || !base::identical(status, 0L) || !base::file.exists(output)) return(NULL)",
      "    base::tryCatch(base::readRDS(output), error = function(e) NULL)",
      "  }",
      "",
      "  .reexport_verify_subprocess <- function(specs, library) {",
      "    input <- output <- script <- stdout <- stderr <- NA_character_",
      "    base::on.exit(base::unlink(base::Filter(function(path) !base::is.na(path), base::c(input, output, script, stdout, stderr)), force = TRUE), add = TRUE)",
      "    prepared <- base::tryCatch({",
      "      input <- base::tempfile('bigbang-reexport-verify-input-')",
      "      output <- base::tempfile('bigbang-reexport-verify-output-')",
      "      script <- base::tempfile('bigbang-reexport-verify-script-', fileext = '.R')",
      "      stdout <- base::tempfile('bigbang-reexport-verify-stdout-')",
      "      stderr <- base::tempfile('bigbang-reexport-verify-stderr-')",
      "      base::saveRDS(base::list(specs = specs, library = library, paths = base::.libPaths()), input)",
      "      input_literal <- base::deparse(input)",
      "      output_literal <- base::deparse(output)",
      "      script_lines <- base::c(",
      "        base::paste0('payload <- base::readRDS(', input_literal, ')'),",
      "        'ordered_paths <- base::unique(base::c(payload$library[base::dir.exists(payload$library)], payload$paths[base::dir.exists(payload$paths)]))',",
      "        'base::.libPaths(base::unique(base::c(ordered_paths, base::.libPaths())))',",
      "        'target <- function(package) {',",
      "          'candidate <- base::find.package(package, lib.loc = ordered_paths, quiet = TRUE)',",
      "          'if (base::length(candidate) == 0L) return(NA_character_)',",
      "          \"base::normalizePath(candidate[[1L]], winslash = '/', mustWork = FALSE)\",",
      "        '}',",
      "        'load_target <- function(package) {',",
      "          'target_path <- target(package)',",
      "          \"if (base::is.na(target_path)) return(base::list(status = 'not_installed', ok = FALSE, path = NA_character_, target = target_path))\",",
      "          \"loaded <- base::tryCatch(base::loadNamespace(package, lib.loc = ordered_paths), error = base::identity)\",",
      "          \"if (base::inherits(loaded, 'error')) return(base::list(status = 'not_loadable', ok = FALSE, path = NA_character_, target = target_path, detail = base::conditionMessage(loaded)))\",",
      "          \"namespace <- base::getNamespace(package)\",",
      "          \"path <- base::normalizePath(base::getNamespaceInfo(namespace, 'path'), winslash = '/', mustWork = FALSE)\",",
      "          \"same_path <- base::identical(path, target_path)\",",
      "          \"base::list(status = if (same_path) 'installed' else 'foreign', ok = same_path, path = path, target = target_path)\",",
      "        '}',",
      "        'rows <- base::lapply(payload$specs, function(spec) {',",
      "          'loaded <- base::lapply(spec$candidates, load_target)',",
      "          \"installed <- base::vapply(loaded, function(item) item$status %in% base::c('installed', 'foreign'), base::logical(1))\",",
      "          'values <- base::lapply(base::seq_along(spec$candidates), function(index) {',",
      "            'if (!installed[[index]]) return(base::list(ok = FALSE))',",
      "            'base::tryCatch(base::list(ok = TRUE, value = base::getExportedValue(spec$candidates[[index]], spec$symbol)), error = function(e) base::list(ok = FALSE))',",
      "          '})',",
      "          \"available <- base::vapply(values, function(value) base::isTRUE(value$ok), base::logical(1))\",",
      "          \"not_installed <- spec$candidates[base::vapply(loaded, function(item) base::identical(item$status, 'not_installed'), base::logical(1))]\",",
      "          \"not_loadable <- base::vapply(base::seq_along(loaded), function(index) if (base::identical(loaded[[index]]$status, 'not_loadable')) base::paste0(spec$candidates[[index]], ': ', loaded[[index]]$detail) else '', base::character(1))\",",
      "          'not_loadable <- not_loadable[base::nzchar(not_loadable)]',",
      "          'not_exported <- spec$candidates[installed & !available]',",
      "          \"foreign <- spec$candidates[base::vapply(loaded, function(item) base::identical(item$status, 'foreign'), base::logical(1))]\",",
      "          'missing <- spec$candidates[!available]',",
      "          'same <- NA',",
      "          'equivalent <- FALSE',",
      "          'if (base::all(available)) {',",
      "            \"objects <- base::lapply(values, function(value) value[['value']])\",",
      "            'same <- base::all(base::vapply(objects[-1L], base::identical, base::logical(1), y = objects[[1L]]))',",
      "            'if (base::length(objects) > 1L && base::identical(same, FALSE)) equivalent <- base::all(base::vapply(objects[-1L], function(value) base::is.function(value) && base::is.function(objects[[1L]]) && base::identical(base::body(value), base::body(objects[[1L]])) && base::identical(base::formals(value), base::formals(objects[[1L]])), base::logical(1)))',",
      "          '}',",
      "          \"base::data.frame(symbol = spec$symbol, package = spec$package, resolution = spec$resolution, diagnosis = spec$diagnosis, candidates = base::paste(spec$candidates, collapse = ', '), installed = base::paste(spec$candidates[installed], collapse = ', '), missing = base::paste(missing, collapse = ', '), not_installed = base::paste(not_installed, collapse = ', '), not_loadable = base::paste(not_loadable, collapse = ', '), not_exported = base::paste(not_exported, collapse = ', '), loaded_from_other_library = base::paste(foreign, collapse = ', '), identical = same, equivalent = equivalent, stringsAsFactors = FALSE)\",",
      "        '})',",
      "        base::paste0('base::saveRDS(rows, ', output_literal, ')')",
      "      )",
      "      base::writeLines(script_lines, script, useBytes = TRUE)",
      "      TRUE",
      "    }, error = function(e) FALSE)",
      "    if (!base::isTRUE(prepared)) return(NULL)",
      "    r_binary <- base::file.path(base::R.home('bin'), if (base::.Platform$OS.type == 'windows') 'R.exe' else 'R')",
      "    status <- base::tryCatch(base::system2(r_binary, base::c('--vanilla', '-f', base::shQuote(script)), stdout = stdout, stderr = stderr), error = base::identity)",
      "    if (base::inherits(status, 'error') || !base::identical(status, 0L) || !base::file.exists(output)) return(NULL)",
      "    base::tryCatch(base::readRDS(output), error = function(e) NULL)",
      "  }",
      "",
      "  .reexport_verify <- function(warn = FALSE) {",
      "    specs <- base::Filter(function(spec) base::identical(spec$resolution, \"preferred\"), .component_reexport_specs)",
      paste0("    empty <- base::structure(base::data.frame(symbol = base::character(), package = base::character(), resolution = base::character(), diagnosis = base::character(), candidates = base::character(), installed = base::character(), missing = base::character(), not_installed = base::character(), not_loadable = base::character(), not_exported = base::character(), loaded_from_other_library = base::character(), identical = base::logical(), stringsAsFactors = FALSE), class = base::c(\"", name, "_reexport_verification\", \"data.frame\"))"),
      "    if (base::length(specs) == 0L) return(empty)",
      "    foreign <- unique(unlist(base::lapply(specs, function(spec) spec$candidates[base::vapply(spec$candidates, function(package) {",
      "      if (!base::isNamespaceLoaded(package)) return(FALSE)",
      "      loaded_path <- base::normalizePath(base::getNamespaceInfo(base::getNamespace(package), 'path'), winslash = '/', mustWork = FALSE)",
      "      target_path <- base::find.package(package, lib.loc = .reexport_library_paths(), quiet = TRUE)",
      "      if (base::length(target_path) == 0L) return(TRUE)",
      "      target_path <- base::normalizePath(target_path[[1L]], winslash = '/', mustWork = FALSE)",
      "      !base::identical(loaded_path, target_path)",
      "    }, base::logical(1L))]), use.names = FALSE))",
      "    if (base::length(foreign) > 0L) base::message(.meta_trf(\"A namespace was already loaded from another library (%s); verification is running in a clean R process.\", base::paste(foreign, collapse = ', ')))",
      "    clean_rows <- .reexport_verify_subprocess(specs, .reexport_state$library)",
      "    if (base::is.null(clean_rows)) clean_rows <- base::lapply(specs, function(spec) base::data.frame(symbol = spec$symbol, package = spec$package, resolution = spec$resolution, diagnosis = spec$diagnosis, candidates = base::paste(spec$candidates, collapse = ', '), installed = '', missing = '', not_installed = '', not_loadable = '', not_exported = '', loaded_from_other_library = '', identical = NA, equivalent = FALSE, stringsAsFactors = FALSE))",
      "    clean_rows <- base::lapply(clean_rows, function(row) {",
      "      candidates <- base::strsplit(row$candidates[[1L]], ', ', fixed = TRUE)[[1L]]",
      "      foreign_here <- base::intersect(candidates, foreign)",
      "      if (base::length(foreign_here) > 0L) row$loaded_from_other_library <- base::paste(base::Filter(base::nzchar, base::unique(base::c(row$loaded_from_other_library[[1L]], foreign_here))), collapse = ', ')",
      "      same <- row$identical[[1L]]",
      "      equivalent <- base::isTRUE(row$equivalent[[1L]])",
      "      row$equivalent <- NULL",
      "      if (base::isTRUE(warn) && (base::nzchar(row$missing[[1L]]) || base::nzchar(row$loaded_from_other_library[[1L]]) || base::identical(same, FALSE) || base::is.na(same))) {",
      "        message <- if (base::nzchar(row$not_exported[[1L]]) && base::nzchar(row$not_installed[[1L]])) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' could not be verified: it is not exported by %s; %s is not installed in the library search path. Install it before verifying.\", row$symbol[[1L]], row$not_exported[[1L]], row$not_installed[[1L]])",
      "        } else if (base::nzchar(row$not_installed[[1L]])) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' could not be verified: %s is not installed in the library search path. Install it before verifying.\", row$symbol[[1L]], row$not_installed[[1L]])",
      "        } else if (base::nzchar(row$not_loadable[[1L]])) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' could not be loaded from the library search path: %s. Check the package installation before verifying.\", row$symbol[[1L]], row$not_loadable[[1L]])",
      "        } else if (base::nzchar(row$not_exported[[1L]])) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' could not be verified: it is not exported by %s. Choose a provider with reexport_prefer or omit it with reexport_exclude.\", row$symbol[[1L]], row$not_exported[[1L]])",
      "        } else if (base::nzchar(row$loaded_from_other_library[[1L]])) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' was loaded from another library: %s. Restart R or use the ordered library path before verifying.\", row$symbol[[1L]], row$loaded_from_other_library[[1L]])",
      "        } else if (base::nzchar(row$missing[[1L]])) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' could not be verified: it is not exported by %s. Choose a provider with reexport_prefer or omit it with reexport_exclude.\", row$symbol[[1L]], row$missing[[1L]])",
      "        } else if (base::is.na(same)) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' could not be verified in a clean R process.\", row$symbol[[1L]])",
      "        } else if (base::isTRUE(equivalent)) {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' are distinct objects with equivalent copies (same body and formals): %s. Choose a provider with reexport_prefer or omit it with reexport_exclude.\", row$symbol[[1L]], row$candidates[[1L]])",
      "        } else {",
      "          .meta_trf(\"Installed owners for re-export symbol '%s' differ as distinct objects: %s. Choose a provider with reexport_prefer or omit it with reexport_exclude.\", row$symbol[[1L]], row$candidates[[1L]])",
      "        }",
      "        condition <- base::structure(base::list(message = message, call = NULL, data = row), class = base::c(\"bigbang_warning_reexport_verification\", \"warning\", \"condition\"))",
      "        base::warning(condition)",
      "      }",
      "      row",
      "    })",
      paste0("    return(base::structure(base::do.call(base::rbind, clean_rows), class = base::c(\"", name, "_reexport_verification\", \"data.frame\")))"),
      "  }",
      "",
      "#' Return installed-owner verification for a conflicts object",
      "#'",
      "#' @param x A conflicts object returned by <name>_conflicts().",
      "#' @return The installed-owner verification data frame, or NULL when absent.",
      "#' @export",
      paste0(name, "_reexport_verification <- function(x) {"),
      "  value <- base::attr(x, \"reexport_verification\", exact = TRUE)",
      "  if (base::is.null(value) && base::is.list(x) &&",
      "      base::`%in%`(\"reexport_verification\", base::names(x))) {",
      "    value <- x[[\"reexport_verification\"]]",
      "  }",
      "  if (!base::is.data.frame(value)) return(NULL)",
      paste0("  if (!base::inherits(value, \"", name, "_reexport_verification\")) return(NULL)"),
      "  value",
      "}",
      "",
      "#' Report masking conflicts and re-export verification",
      "#'",
      "#' The returned list keeps masking conflicts and stores an installed-owner",
      "#' verification data frame as an attribute.",
      "#' @return A named list of masking conflicts with an attribute containing verification.",
      "#' @export",
      paste0(name, "_conflicts <- function() {"),
      "  package_entries <- base::grep(\"^package:\", base::search(), value = TRUE)",
      "  component_entries <- base::intersect(base::paste0(\"package:\", .pkgs), package_entries)",
      "  conflicts <- if (base::length(component_entries) == 0L) {",
      "    base::list()",
      "  } else {",
      "    objects <- base::lapply(package_entries, function(entry) {",
      "      package <- base::sub(\"^package:\", \"\", entry)",
      "      base::tryCatch(base::getNamespaceExports(package), error = function(e) base::character())",
      "    })",
      "    base::names(objects) <- package_entries",
      "    candidates <- base::unique(base::unlist(objects[component_entries], use.names = FALSE))",
      "    conflicts <- base::lapply(candidates, function(object) package_entries[base::vapply(objects, function(exports) base::`%in%`(object, exports), base::logical(1))])",
      "    base::names(conflicts) <- candidates",
      "    conflicts[base::vapply(conflicts, base::length, base::integer(1)) > 1L]",
      "  }",
      paste0("  base::structure(conflicts, class = base::c(\"", name, "_conflicts\", \"list\"), reexport_verification = .reexport_verify(warn = TRUE))"),
      "}",
      "",
      "#' @export",
      paste0("print.", name, "_conflicts <- function(x, ...) {"),
      "  masking_names <- base::names(x)",
      "  if (base::length(masking_names) == 0L) {",
      "    base::cat(.meta_tr(\"No conflicts found.\"), \"\\n\")",
      "  } else {",
      "    base::cat(.meta_tr(\"Conflicts:\"), \"\\n\")",
      "    for (object in masking_names) {",
      "      owners <- base::sub(\"^package:\", \"\", x[[object]])",
      "      base::cat(\"  \", object, \": \", base::paste(owners, collapse = \", \"), \"\\n\", sep = \"\")",
      "    }",
      "  }",
      "  verification <- base::attr(x, \"reexport_verification\", exact = TRUE)",
      paste0("  if (!base::inherits(verification, \"", name, "_reexport_verification\")) {"),
      "    base::cat(.meta_tr(\"Re-export verification is not available.\"), \"\\n\")",
      "  } else if (base::nrow(verification) > 0L) {",
      "    base::cat(.meta_tr(\"Re-export resolutions:\"), \"\\n\")",
      "    for (index in base::seq_len(base::nrow(verification))) {",
      "      status <- if (base::is.na(verification$identical[[index]])) .meta_tr(\"not verified\") else base::as.character(verification$identical[[index]])",
      "      missing <- if (base::nzchar(verification$missing[[index]])) base::paste0(\"; \", .meta_trf(\"missing: %s\", verification$missing[[index]])) else \"\"",
      "      base::cat(\"  \", verification$symbol[[index]], \": \", verification$package[[index]], \" [\", verification$resolution[[index]], \"; \", verification$diagnosis[[index]], \"]; identical=\", status, missing, \"\\n\", sep = \"\")",
      "    }",
      "  }",
      "  base::invisible(x)",
      "}"
    ), collapse = "\n")
  } else {
    template_data$conflicts_function <- paste(c(
      "#' Report masking conflicts involving metapackage components",
      "#'",
      "#' Examines attached package environments and reports names exported by more",
      "#' than one package when at least one owner is a metapackage component.",
      "#'",
      "#' @return A named list of conflicting package search entries.",
      "#' @export",
      paste0(name, "_conflicts <- function() {"),
      masking_conflicts_body,
      "}",
      "",
      "#' @export",
      paste0("print.", name, "_conflicts <- function(x, ...) {"),
      "  if (length(x) == 0L) {",
      "    cat(.meta_tr(\"No conflicts found.\"), \"\\n\")",
      "    return(invisible(x))",
      "  }",
      "  cat(.meta_tr(\"Conflicts:\"), \"\\n\")",
      "  for (object in names(x)) {",
      "    owners <- sub(\"^package:\", \"\", x[[object]])",
      "    cat(\"  \", object, \": \", paste(owners, collapse = \", \"), \"\\n\", sep = \"\")",
      "  }",
      "  invisible(x)",
      "}"
    ), collapse = "\n")
  }


  # Templates for the generated runtime files.
  templates <- list(
    reexports = '\n.component_reexport_specs <- {{{ reexport_specs }}}\n.reexport_state <- base::new.env(parent = base::emptyenv())\n.reexport_state$library <- base::character()\n.reexport_library_paths <- function() {\n  base::unique(base::c(.reexport_state$library[base::dir.exists(.reexport_state$library)], base::.libPaths()))\n}\n.set_reexport_library <- function(lib) {\n  .reexport_state$library <- base::normalizePath(lib, winslash = "/", mustWork = FALSE)\n  base::invisible(NULL)\n}\n\n.make_reexport_binding <- function(package, symbol) {\n  base::force(package)\n  base::force(symbol)\n  function(value) {\n    if (!base::missing(value)) {\n      base::stop(.meta_tr("Runtime re-export bindings are read-only."), call. = FALSE)\n    }\n    .reexport_component_value(package, symbol)\n  }\n}\n\n.install_reexport_bindings <- function(pkgname) {\n  namespace <- base::asNamespace(pkgname)\n  for (spec in .component_reexport_specs) {\n    base::makeActiveBinding(\n      spec$symbol,\n      .make_reexport_binding(spec$package, spec$symbol),\n      namespace\n    )\n  }\n  base::invisible(NULL)\n}\n',
    attach = '
utils::globalVariables(".pkgs")
.pkgs <- {{{ package_list }}}
.component_names <- .pkgs

attach_installed_packages <- function(pkgs, warn_missing = TRUE,
                                      lib.loc = base::.libPaths(),
                                      attach_components = TRUE,
                                      warn_conflicts = TRUE) {
  already_attached <- base::gsub("^package:", "", base::search())
  to_load <- base::setdiff(pkgs, already_attached)
  package_available <- function(package, seen = base::character()) {
    if (base::`%in%`(package, seen)) return(TRUE)
    if (base::isNamespaceLoaded(package)) return(TRUE)
    target <- base::find.package(package, lib.loc = lib.loc, quiet = TRUE)
    if (base::length(target) == 0L) return(FALSE)
    description <- base::tryCatch(
      utils::packageDescription(package, lib.loc = lib.loc),
      error = function(e) NULL
    )
    depends <- if (base::is.null(description)) {
      base::character()
    } else {
      value <- description[["Depends"]]
      if (base::is.null(value) || base::is.na(value)) base::character() else
        base::trimws(base::strsplit(value, ",", fixed = TRUE)[[1L]])
    }
    depends <- base::sub("[[:space:]]*\\\\(.*$", "", depends)
    depends <- base::setdiff(depends[base::nzchar(depends)], "R")
    base::all(base::vapply(
      depends, package_available, base::logical(1),
      seen = base::c(seen, package)
    ))
  }
  available <- base::vapply(to_load, package_available, base::logical(1))
  missing <- to_load[!available]
  attached <- character()
  to_attach <- if (base::isTRUE(attach_components)) {
    base::setdiff(to_load, missing)
  } else {
    base::character()
  }
  for (package in to_attach) {
    loaded <- base::tryCatch({
      if (base::isNamespaceLoaded(package)) {
        base::attachNamespace(package)
      } else {
        base::suppressPackageStartupMessages(
          base::library(
            package,
            character.only = TRUE,
            lib.loc = lib.loc,
            warn.conflicts = warn_conflicts
          )
        )
      }
      TRUE
    }, error = function(e) FALSE)
    if (base::isTRUE(loaded)) {
      attached <- base::c(attached, package)
    } else {
      missing <- base::c(missing, package)
    }
  }
  missing <- base::unique(missing)
  if (warn_missing && base::length(missing) > 0) {
    base::warning(.meta_trf(
      "Not installed: %s. Run {{{ install_call }}} to install them.",
      base::paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  base::invisible(base::list(attached = attached, missing = missing))
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
{{{ attach_body }}}
}

#\' Install local metapackage components
#\'
#\' Installs local archives in topological order and then attaches them.
#\' Installation is explicit and never occurs from a startup hook.
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
                               cran_deps = base::c("skip", "error", "install"),
                               repos = base::getOption("repos"),
                               verbose = base::getOption("bigbang.verbose", base::interactive()),
                               force = FALSE,
                               upgrade = "{{ install_upgrade }}",
                               only = NULL,
                               lib = base::.libPaths()[[1L]]) {
  cran_deps <- base::match.arg(cran_deps)
  upgrade <- resolve_upgrade_policy(force, upgrade, base::missing(upgrade))
  # An empty pkg_dir means the shipped archive directory was not found, which
  # produces a misleading path further down. Say what is wrong instead.
  if (!base::is.character(pkg_dir) || base::length(pkg_dir) < 1L || base::anyNA(pkg_dir) ||
      base::any(!base::nzchar(pkg_dir))) {
    base::stop(.meta_tr(
      "The component archives that ship with this package are not available. Reinstall it, or pass pkg_dir pointing at a directory holding the component archives."
    ), call. = FALSE)
  }
  missing_dirs <- pkg_dir[!base::dir.exists(pkg_dir)]
  if (base::length(missing_dirs) > 0L) {
    stop(.meta_trf("The archive directory does not exist: %s",
                   base::paste(missing_dirs, collapse = ", ")),
         call. = FALSE)
  }
  packages <- {{{ local_packages }}}
  {{{ reexport_library_setup }}}
  result <- install_packages_in_order(
    packages, pkg_dir, ext, verbose = verbose,
    repos = repos, cran_deps = cran_deps, upgrade = upgrade,
    only = only, lib = lib
  )
  if (base::length(result$skipped_nonlocal) > 0L) {
    base::warning(.meta_trf(
      "Some components were skipped because non-local dependencies are missing: %s. Install those dependencies, or call {{{ install_call_repo }}} to obtain them from a repository.",
      base::paste(base::names(result$skipped_nonlocal), collapse = ", ")
    ), call. = FALSE)
  }
  if (base::length(result$skipped_local) > 0L) {
    base::warning(.meta_trf(
      "Some components were skipped because local dependencies were skipped: %s.",
      base::paste(base::names(result$skipped_local), collapse = ", ")
    ), call. = FALSE)
  }
  if (base::length(result$failed) > 0) {
    details <- base::paste0(
      base::names(result$failed), ": ", base::unlist(result$failed, use.names = FALSE)
    )
    condition <- base::structure(
      list(
        message = .meta_trf(
          "Could not install all components: %s",
          base::paste(details, collapse = "; ")
        ),
        call = NULL,
        failures = result$failed
      ),
      class = c("bigbang_error_install", "bigbang_error", "error", "condition")
    )
    base::stop(condition)
  }
  result$reexport_verification <- {{{ reexport_verification_call }}}
  if (base::isTRUE(verbose) && base::length(result$pulled_in) > 0L) {
      base::message(.meta_trf(
      "Added local dependencies of selected components: %s",
      base::paste(result$pulled_in, collapse = ", ")
    ))
  }
  if (base::isTRUE(verbose) && base::interactive() && base::length(result$unchanged) > 0L) {
      base::message(.meta_trf(
      "Use force = TRUE or upgrade = \'always\' to reinstall unchanged packages: %s",
      base::paste(base::names(result$unchanged), collapse = ", ")
    ))
  }
  # A skipped component was just reported with its reason and the call that
  # fixes it, so attaching must not follow it with a vaguer hint.
  attach_names <- base::vapply(
    result$selected,
    function(item) resolve_component_spec(item, ext)$package,
    character(1L)
  )
  attach_installed_packages(
    attach_names,
    warn_missing = base::length(result$skipped) == 0L,
    lib.loc = lib,
    warn_conflicts = {{{ attach_warn_conflicts }}}
  )
  base::invisible(result)
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
  component_entries <- base::paste0("package:", .pkgs)
  search_entries <- base::search()
  attached <- search_entries[
    base::startsWith(search_entries, "package:") &
      base::`%in%`(search_entries, component_entries)
  ]
  for (entry in attached) {
    base::try(base::detach(entry, character.only = TRUE), silent = TRUE)
  }
  base::invisible()
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

{{{ conflicts_function }}}

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
  base::lapply(
    .pkgs,
    base::library,
    character.only = TRUE,
    warn.conflicts = {{{ attach_warn_conflicts }}}
  )
  base::invisible()
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

.meta_package_version <- function(x) {
  version <- base::unclass(utils::packageVersion(x))[[1]]
  if (base::length(version) > 3 && base::requireNamespace("cli", quietly = TRUE)) {
    version[4:base::length(version)] <- cli::col_red(base::as.character(version[4:base::length(version)]))
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

  meta_version <- tryCatch(.meta_package_version(name), error = function(e) "")
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

        temp_root <- base::normalizePath(
          base::tempdir(), winslash = "/", mustWork = TRUE
        )
        candidate <- base::normalizePath(
          p, winslash = "/", mustWork = FALSE
        )
        if (base::identical(candidate, temp_root)) {
          message(.meta_trf("SAFETY: Potentially important directory: %s", p))
          return(invisible(FALSE))
        }

        # Apply directory-specific checks.
        if (dir.exists(p)) {
          # Never remove protected directories.
          if (base::`%in%`(basename(p), PROTECTED_DIRS)) {
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
              if (!is_path_inside(p, temp_root)) {
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
  # Resolve the existing ancestor first. This preserves the child suffix when
  # a temporary path does not exist yet and its parent is an aliased path.
  resolve_path <- function(path) {
    current <- normalizePath(path, winslash = "/", mustWork = FALSE)
    suffix <- character()
    repeat {
      if (file.exists(current) || dir.exists(current)) break
      parent <- dirname(current)
      if (identical(parent, current)) break
      suffix <- c(basename(current), suffix)
      current <- parent
    }
    resolved <- normalizePath(current, winslash = "/", mustWork = FALSE)
    if (length(suffix) == 0L) return(resolved)
    do.call(file.path, c(list(resolved), as.list(suffix)))
  }
  inner <- resolve_path(inner_path)
  outer <- resolve_path(outer_path)

  # Use one separator representation on Windows.
  if (.Platform$OS.type == "windows") {
    inner <- gsub("\\\\\\\\", "/", inner)
    outer <- gsub("\\\\\\\\", "/", outer)
  }

  if (identical(inner, outer)) return(TRUE)

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
{{#reexport}}  # Intentional re-export conflicts are reported by *_conflicts().
  base::assign(
    ".conflicts.OK", TRUE,
    envir = base::as.environment(base::paste0("package:", pkgname))
  )
{{/reexport}}

{{^reexport}}  # Tidyverse-style startup hook: delegate search-path changes to a helper.
  result <- attach_installed_packages(
    pkg_base_names,
    warn_missing = FALSE,
    warn_conflicts = {{{ attach_warn_conflicts }}}
  )
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
      attached <- search()[startsWith(search(), "package:") &
        paste0("package:", sub("^package:", "", search())) %in% paste0("package:", .pkgs)]
      for (entry in attached) {
        # Detach without unloading component namespaces.
        try(detach(entry, character.only = TRUE, unload = FALSE),
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

  if (isTRUE(reexport)) {
    templates$reexports <- paste0(
      templates$reexports,
      paste(c(
        "",
        "# The installed version is read only to explain a failure: reading it",
        "# on every access made each use of a re-exported symbol ~50 times slower.",
        ".reexport_installed_version <- function(package, libraries) {",
        "  base::tryCatch(",
        "    base::as.character(utils::packageVersion(package, lib.loc = libraries)),",
        "    error = function(e) .meta_tr(\"component is not installed\")",
        "  )",
        "}",
        "",
        "if (FALSE) { .reexport_component_value <- function(package, symbol) {",
        "  # Fast path for the common case: the component is loaded and exports it.",
        "  if (base::isNamespaceLoaded(package) && base::exists(",
        "    symbol, envir = base::getNamespaceInfo(package, \"exports\"),",
        "    inherits = FALSE",
        "  )) {",
        "    value <- base::tryCatch(",
        "      base::getExportedValue(package, symbol),",
        "      error = base::identity",
        "    )",
        "    if (!base::inherits(value, \"error\")) return(value)",
        "  }",
        "  libraries <- .reexport_library_paths()",
        "  loaded <- base::tryCatch(",
        "    base::requireNamespace(package, quietly = TRUE, lib.loc = libraries),",
        "    error = base::identity",
        "  )",
        "  if (!base::isTRUE(loaded)) {",
        "    reason <- if (base::inherits(loaded, \"error\")) {",
        "      .meta_trf(\"component could not be loaded: %s\", base::conditionMessage(loaded))",
        "    } else {",
        "      .meta_trf(\"Component package '%s' is not installed\", package)",
        "    }",
        "    message <- .meta_trf(",
        "      \"Re-exported symbol '%s' from component package '%s' (installed version: %s) is unavailable: %s. Run %s to install the required component version.\",",
        "      symbol, package, .reexport_installed_version(package, libraries), reason, \"{{ name }}_install()\"",
        "    )",
        "    return(function(...) base::stop(message, call. = FALSE))",
        "  }",
        "  value <- base::tryCatch(",
        "    base::getExportedValue(package, symbol),",
        "    error = base::identity",
        "  )",
        "  if (base::inherits(value, \"error\")) {",
        "    reason <- .meta_trf(",
        "      \"component does not export '%s': %s\", symbol,",
        "      base::conditionMessage(value)",
        "    )",
        "    message <- .meta_trf(",
        "      \"Re-exported symbol '%s' from component package '%s' (installed version: %s) is unavailable: %s. Run %s to install the required component version.\",",
        "      symbol, package, .reexport_installed_version(package, libraries), reason, \"{{ name }}_install()\"",
        "    )",
        "    return(function(...) base::stop(message, call. = FALSE))",
        "  }",
        "  value",
        "}",
        "}",
        ""
      ), collapse = "\n")
    )
    templates$reexports <- paste0(
      templates$reexports,
      paste(c(
        "",
        ".reexport_loaded_version <- function(package) {",
        "  if (!base::isNamespaceLoaded(package)) return(NA_character_)",
        "  namespace <- base::getNamespace(package)",
        "  path <- base::getNamespaceInfo(namespace, 'path')",
        "  base::tryCatch(base::as.character(utils::packageVersion(package, lib.loc = base::dirname(path))), error = function(e) NA_character_)",
        "}",
        "",
        ".reexport_installed_version <- function(package, libraries) {",
        "  candidate <- base::find.package(package, lib.loc = libraries, quiet = TRUE)",
        "  if (base::length(candidate) == 0L) return(.meta_tr('component is not installed'))",
        "  base::tryCatch(base::as.character(utils::packageVersion(package, lib.loc = base::dirname(candidate[[1L]]))), error = function(e) .meta_tr('component version could not be read'))",
        "}",
        "",
        ".reexport_version_text <- function(package, libraries) {",
        "  loaded <- .reexport_loaded_version(package)",
        "  installed <- .reexport_installed_version(package, libraries)",
        "  if (!base::is.na(loaded) && !base::identical(installed, .meta_tr('component is not installed')) && !base::identical(loaded, installed)) return(.meta_trf('loaded version: %s; installed version: %s. Restart R to use the installed version.', loaded, installed))",
        "  if (!base::is.na(loaded)) return(.meta_trf('loaded version: %s', loaded))",
        "  .meta_trf('installed version: %s', installed)",
        "}",
        "",
        ".reexport_component_value <- function(package, symbol) {",
        "  libraries <- .reexport_library_paths()",
        "  target <- base::find.package(package, lib.loc = libraries, quiet = TRUE)",
        "  loaded <- if (base::length(target) == 0L) {",
        "    .meta_tr('component is not installed')",
        "  } else {",
        "    base::tryCatch(base::loadNamespace(package, lib.loc = libraries), error = base::identity)",
        "  }",
        "  if (base::inherits(loaded, 'error') || base::is.character(loaded)) {",
        "    reason <- if (base::inherits(loaded, 'error')) .meta_trf('component could not be loaded: %s', base::conditionMessage(loaded)) else .meta_trf(\"Component package '%s' is not installed\", package)",
        "    message <- .meta_trf(\"Re-exported symbol '%s' from component package '%s' (%s) is unavailable: %s. Run %s to install the required component version.\", symbol, package, .reexport_version_text(package, libraries), reason, \"{{ name }}_install()\")",
        "    return(function(...) base::stop(message, call. = FALSE))",
        "  }",
        "  value <- base::tryCatch(base::getExportedValue(package, symbol), error = base::identity)",
        "  if (base::inherits(value, 'error')) {",
        "    reason <- .meta_trf(\"component does not export '%s': %s\", symbol, base::conditionMessage(value))",
        "    message <- .meta_trf(\"Re-exported symbol '%s' from component package '%s' (%s) is unavailable: %s. Run %s to install the required component version.\", symbol, package, .reexport_version_text(package, libraries), reason, \"{{ name }}_install()\")",
        "    return(function(...) base::stop(message, call. = FALSE))",
        "  }",
        "  value",
        "}",
        ""
      ), collapse = "\n")
    )
  }

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
        content <- qualify_runtime_calls(content)

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
