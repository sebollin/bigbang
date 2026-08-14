#' @param package Character archive stem or archive path.
#' @param pkg_dir Character archive directory or directories.
#' @param ext Character archive extension, or `NULL` to infer it.
#'
#' Extract one archive and read its DESCRIPTION metadata.
#'
#' The returned metadata is deliberately limited to facts declared by the
#' component itself. Source-code heuristics belong to
#' `detect_implicit_dependencies()` and must never silently become hard
#' dependencies of a generated package.
#'
#' @param package Character archive stem or archive path.
#' @param pkg_dir Character archive directory or directories.
#' @param ext Character archive extension, or `NULL` to infer it.
#' @return A list containing `package`, `version`, and declared `dependencies`.
#' @noRd
.archive_extensions <- c(".tar.gz", ".zip", ".tar")

.or_null <- function(x, y) if (is.null(x)) y else x

.untar_quiet <- function(...) utils::untar(..., tar = "internal")

.archive_extension <- function(path) {
  path_lower <- tolower(path)
  match <- .archive_extensions[vapply(
    .archive_extensions, function(value) endsWith(path_lower, value), logical(1L)
  )]
  if (length(match) == 0L) {
    stop(.bb_trf("Unsupported archive format: %s", path), call. = FALSE)
  }
  match[[1L]]
}

.archive_stem <- function(path, ext = NULL) {
  ext <- .or_null(ext, .archive_extension(path))
  basename <- basename(path)
  substr(basename, 1L, nchar(basename) - nchar(ext))
}

.canonical_archive_name <- function(component) {
  paste0(component$stem, component$ext)
}

.expand_package_manifest <- function(packages, pkg_dir = NULL) {
  if (!is.character(packages) || length(packages) != 1L ||
        !file.exists(packages) || dir.exists(packages)) {
    return(list(packages = packages, pkg_dir = pkg_dir))
  }
  manifest <- normalizePath(packages, winslash = "/", mustWork = TRUE)
  if (!is.null(tryCatch(.archive_extension(manifest), error = function(e) NULL))) {
    return(list(packages = packages, pkg_dir = pkg_dir))
  }
  lines <- readLines(manifest, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) == 0L) {
    stop(.bb_tr("The component manifest does not list any packages."),
         call. = FALSE)
  }
  manifest_dir <- dirname(manifest)
  entries <- vapply(lines, function(line) {
    absolute <- startsWith(line, "~") || startsWith(line, "/") ||
      grepl("^[A-Za-z]:[/\\\\]", line, perl = TRUE) ||
      grepl("^\\\\\\\\", line, perl = TRUE)
    candidate <- if (startsWith(line, "~")) {
      path.expand(line)
    } else if (absolute) {
      line
    } else {
      file.path(manifest_dir, line)
    }
    is_archive <- !is.null(
      tryCatch(.archive_extension(line), error = function(e) NULL)
    )
    # A bare archive filename may live beside the manifest or in one of the
    # supplied archive directories. Keep it as a filename so the resolver can
    # search all sources and detect duplicate basenames.
    # Explicit paths remain paths and therefore fail at their stated location.
    if (absolute || grepl("[/\\\\]", line)) {
      candidate
    } else if (is_archive) {
      line
    } else if (file.exists(candidate)) {
      candidate
    } else {
      line
    }
  }, character(1L))
  list(
    packages = entries,
    pkg_dir = unique(c(manifest_dir, pkg_dir))
  )
}

.build_source_component <- function(source_dir) {
  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    stop(.bb_tr(
      "Component directories require the 'pkgbuild' package. Install 'pkgbuild' or pass a built archive instead."
    ), call. = FALSE)
  }
  build_dir <- tempfile("bigbang-source-build-")
  if (!dir.create(build_dir, recursive = TRUE)) {
    stop(.bb_trf("Could not create temporary directory for %s", source_dir),
         call. = FALSE)
  }
  built <- tryCatch(
    pkgbuild::build(
      path = source_dir, dest_path = build_dir, binary = FALSE,
      vignettes = FALSE, manual = FALSE, quiet = TRUE
    ),
    error = identity
  )
  if (inherits(built, "error")) {
    stop(.bb_trf(
      "Could not build component source directory %s: %s",
      source_dir, conditionMessage(built)
    ), call. = FALSE)
  }
  built <- as.character(built)[1L]
  if (!nzchar(built) || !file.exists(built)) {
    candidates <- list.files(
      build_dir, pattern = "\\.(tar\\.gz|zip)$", full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(candidates) != 1L) {
      stop(.bb_trf(
        "Could not find the archive built from component directory %s.",
        source_dir
      ), call. = FALSE)
    }
    built <- candidates[[1L]]
  }
  normalizePath(built, winslash = "/", mustWork = TRUE)
}

.normalize_archive_dirs <- function(pkg_dir) {
  if (is.null(pkg_dir) || length(pkg_dir) == 0L) return(character())
  if (!is.character(pkg_dir) || anyNA(pkg_dir) || any(!nzchar(pkg_dir))) {
    stop(.bb_tr("'pkg_dir' must contain one or more non-empty paths"), call. = FALSE)
  }
  if (any(!dir.exists(pkg_dir))) {
    missing <- pkg_dir[!dir.exists(pkg_dir)]
    stop(.bb_trf("The archive directory does not exist: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  normalizePath(pkg_dir, winslash = "/", mustWork = TRUE)
}

.archives_for_package_identity <- function(package, dirs) {
  archives <- unique(unlist(lapply(dirs, function(dir) {
    files <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    files <- files[file.exists(files) & !dir.exists(files)]
    files[vapply(files, function(path) {
      !is.null(tryCatch(.archive_extension(path), error = function(e) NULL))
    }, logical(1L))]
  }), use.names = FALSE))
  identities <- lapply(archives, function(path) {
    tryCatch(
      .read_archive_identity(path, .archive_extension(path)),
      error = function(error) {
        warning(.bb_trf(
          "Could not read archive %s; excluding it from the archive inventory: %s",
          path, conditionMessage(error)
        ), call. = FALSE)
        NULL
      }
    )
  })
  matches <- vapply(identities, function(identity) {
    !is.null(identity) && identical(identity$package, package)
  }, logical(1L))
  normalizePath(archives[matches], winslash = "/", mustWork = TRUE)
}

.resolve_archive_input <- function(input, pkg_dir = NULL, ext = ".tar.gz") {
  if (!is.character(input) || length(input) != 1L || is.na(input) || !nzchar(input)) {
    stop(.bb_tr("Each component must be one non-empty archive path or stem"), call. = FALSE)
  }
  if (dir.exists(input) && file.exists(file.path(input, "DESCRIPTION"))) {
    return(.build_source_component(normalizePath(
      input, winslash = "/", mustWork = TRUE
    )))
  }
  if (file.exists(input) && !dir.exists(input)) {
    return(normalizePath(input, winslash = "/", mustWork = TRUE))
  }
  dirs <- .normalize_archive_dirs(pkg_dir)
  if (length(dirs) == 0L) {
    stop(.bb_trf(
      "Could not resolve component '%s': it is not an existing file and no 'pkg_dir' was supplied.",
      input
    ), call. = FALSE)
  }
  has_separator <- grepl("[/\\\\]", input)
  input_extension <- tryCatch(.archive_extension(input), error = function(e) NULL)
  discovered <- character()
  if (!is.null(input_extension) && !has_separator) {
    # A manifest can name an archive without placing it beside the manifest.
    # Search that basename in every supplied source, rather than treating the
    # already-suffixed value as a stem and appending the extension again.
    expected_names <- tolower(basename(input))
    discovered <- unlist(lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      files[tolower(basename(files)) == expected_names & !dir.exists(files)]
    }), use.names = FALSE)
    found <- discovered
  } else if (has_separator) {
    # A path containing a separator is explicit.  Do not reinterpret it as a
    # stem and search unrelated directories when the path is missing.
    found <- character()
  } else if (!grepl("_", input, fixed = TRUE)) {
    # A bare package name is resolved by the Package field, never by a string
    # prefix. This also supports archives whose filenames are build labels or
    # omit their version while keeping traditional name_version stems intact.
    discovered <- .archives_for_package_identity(input, dirs)
    found <- discovered
  } else {
    candidates <- file.path(dirs, paste0(input, ext))
    found <- candidates[file.exists(candidates) & !dir.exists(candidates)]
    # `ext` is a fallback, not a restriction: a stem may resolve to a source
    # archive with any supported extension. Discover all matches even when the
    # fallback exists, so a second format cannot be silently ignored.
    expected_names <- tolower(paste0(input, .archive_extensions))
    discovered <- unlist(lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      files[tolower(basename(files)) %in% expected_names & !dir.exists(files)]
    }), use.names = FALSE)
    found <- c(found, discovered)
  }
  found <- unique(c(found, discovered))
  if (length(found) > 1L) {
    .bigbang_abort(
      "bigbang_error_duplicate_component",
      .bb_trf(
        "More than one archive was found for component stem '%s': %s.",
        input, paste(found, collapse = "; ")
      ),
      packages = found
    )
  }
  if (length(found) == 0L) {
    stop(.bb_trf(
      "Package archive does not exist: %s; archives were not found in the supplied archive directories: %s.",
      input, paste(dirs, collapse = "; ")
    ), call. = FALSE)
  }
  normalizePath(found[[1L]], winslash = "/", mustWork = TRUE)
}

.resolve_components <- function(packages, pkg_dir = NULL, ext = ".tar.gz",
                                on_component_error = "abort",
                                reexport = FALSE) {
  if (!is.character(packages) || length(packages) < 1L) {
    stop(.bb_tr("'packages' must be a non-empty character vector"), call. = FALSE)
  }
  if (!is.character(ext) || length(ext) != 1L || is.na(ext) || !nzchar(ext)) {
    stop(.bb_tr("'ext' must be one non-empty archive extension"), call. = FALSE)
  }
  on_component_error <- match.arg(on_component_error, c("abort", "skip"))
  expanded <- .expand_package_manifest(packages, pkg_dir)
  packages <- expanded$packages
  pkg_dir <- expanded$pkg_dir
  dirs <- .normalize_archive_dirs(pkg_dir)
  omitted <- data.frame(
    component = character(), input = character(), reason = character(),
    stringsAsFactors = FALSE
  )
  components <- list()
  for (input in packages) {
    source_dir <- if (dir.exists(input) &&
                        file.exists(file.path(input, "DESCRIPTION"))) {
      normalizePath(input, winslash = "/", mustWork = TRUE)
    } else {
      NULL
    }
    path_result <- tryCatch(
      .resolve_archive_input(input, dirs, ext), error = identity
    )
    declared_identity <- NULL
    if (!inherits(path_result, "error")) {
      actual_ext <- .archive_extension(path_result)
      declared_identity <- tryCatch(
        .read_archive_identity(path_result, actual_ext),
        error = function(e) NULL
      )
      resolved <- tryCatch({
        metadata <- .read_archive_metadata(
          path_result, ext = actual_ext, include_exports = isTRUE(reexport)
        )
        list(
          path = path_result,
          ext = actual_ext,
          stem = .archive_stem(path_result, actual_ext),
          input = input,
          source_dir = source_dir,
          package = metadata$package,
          version = metadata$version,
          dependencies = metadata$dependencies,
          constraints = metadata$constraints,
          exports = .or_null(metadata$exports, character())
        )
      }, error = identity)
    } else {
      resolved <- path_result
    }
    if (inherits(resolved, "error")) {
      if (identical(on_component_error, "abort") ||
            inherits(resolved, "bigbang_error_duplicate_component") ||
            inherits(resolved, "bigbang_error_reexport_namespace")) {
        stop(resolved)
      }
      component_name <- if (!is.null(declared_identity)) {
        declared_identity$package
      } else {
        sub("_.*", "", basename(input))
      }
      if (!is.null(path_result) && !inherits(path_result, "error") &&
            is.null(declared_identity)) {
        warning(.bb_trf(
          paste0(
            "Component archive %s could not be read; skip propagation uses ",
            "filename-derived name '%s'; dependents may fail on the recipient ",
            "if that name differs from Package."
          ),
          path_result, component_name
        ), call. = FALSE)
      }
      omitted <- rbind(
        omitted,
        data.frame(
          component = component_name, input = input,
          reason = conditionMessage(resolved), stringsAsFactors = FALSE
        )
      )
    } else {
      components[[length(components) + 1L]] <- resolved
    }
  }
  if (length(components) == 0L) {
    stop(.bb_tr("No valid component archives remain after applying the component error policy."),
         call. = FALSE)
  }
  component_packages <- vapply(components, `[[`, character(1L), "package")
  repeat {
    omitted_names <- unique(omitted$component)
    dependent <- vapply(components, function(component) {
      any(component$dependencies %in% omitted_names)
    }, logical(1L))
    if (!any(dependent)) break
    newly_omitted <- components[dependent]
    for (component in newly_omitted) {
      omitted <- rbind(
        omitted,
        data.frame(
          component = component$package, input = component$input,
          reason = .bb_trf(
            "Omitted because it depends on omitted component %s.",
            paste(intersect(component$dependencies, omitted_names), collapse = ", ")
          ), stringsAsFactors = FALSE
        )
      )
    }
    components <- components[!dependent]
    component_packages <- vapply(components, `[[`, character(1L), "package")
    if (length(components) == 0L) {
      stop(.bb_tr("No valid component archives remain after propagating omitted dependencies."),
           call. = FALSE)
    }
  }
  source_dirs <- unique(c(dirs, dirname(vapply(components, `[[`, character(1L), "path"))))
  inventory <- .archive_inventory(source_dirs, known = components)
  list(
    components = components, inventory = inventory, source_dirs = source_dirs,
    omitted = omitted, packages = packages, pkg_dir = pkg_dir
  )
}

.read_archive_metadata <- function(package, pkg_dir = NULL, ext = NULL,
                                   include_exports = FALSE) {
  archive <- if (file.exists(package) && !dir.exists(package)) {
    normalizePath(package, winslash = "/", mustWork = TRUE)
  } else {
    .resolve_archive_input(package, pkg_dir, .or_null(ext, ".tar.gz"))
  }
  ext <- .or_null(ext, .archive_extension(archive))
  if (!file.exists(archive)) {
    stop(.bb_trf("Package archive does not exist: %s", archive), call. = FALSE)
  }

  temp_dir <- tempfile("bigbang-metadata-")
  if (!dir.create(temp_dir)) {
    stop(.bb_trf("Could not create temporary directory for %s", archive), call. = FALSE)
  }
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  .extract_archive_checked(archive, ext, temp_dir)
  allow_flat <- identical(tolower(ext), ".zip") &&
    file.exists(file.path(temp_dir, "Meta", "package.rds"))
  package_root <- .find_archive_root(temp_dir, archive, allow_flat = allow_flat)
  desc_file <- file.path(package_root, "DESCRIPTION")

  desc <- read.dcf(
    desc_file,
    fields = c("Package", "Version", "Depends", "Imports", "LinkingTo")
  )
  if (nrow(desc) == 0L) {
    stop(.bb_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }
  field <- function(name) {
    if (!name %in% colnames(desc)) return(NA_character_)
    value <- unname(desc[1L, name])
    if (is.na(value)) NA_character_ else trimws(value)
  }
  declared_package <- field("Package")
  declared_version <- field("Version")
  if (is.na(declared_package) || !nzchar(declared_package) ||
        is.na(declared_version) || !nzchar(declared_version)) {
    stop(.bb_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }

  parsed_dependencies <- .parse_dependency_constraints(
    vapply(c("Depends", "Imports", "LinkingTo"), field, character(1L))
  )

  exports <- character()
  if (isTRUE(include_exports)) {
    namespace_path <- file.path(package_root, "NAMESPACE")
    if (!file.exists(namespace_path) || dir.exists(namespace_path)) {
      .bigbang_abort(
        "bigbang_error_reexport_namespace",
        .bb_trf(
          "Could not read NAMESPACE from archive %s: the file is missing.",
          archive
        ),
        archive = archive
      )
    }
    namespace <- tryCatch(
      base::parseNamespaceFile(
        basename(package_root), dirname(package_root), mustExist = TRUE
      ),
      error = identity
    )
    if (inherits(namespace, "error")) {
      .bigbang_abort(
        "bigbang_error_reexport_namespace",
        .bb_trf(
          "Could not read NAMESPACE from archive %s: %s",
          archive, conditionMessage(namespace)
        ),
        archive = archive
      )
    }
    if (length(namespace$exportPatterns) > 0L) {
      .bigbang_abort(
        "bigbang_error_reexport_namespace",
        .bb_trf(
          paste0(
            "Could not determine explicit exports in NAMESPACE from archive %s: ",
            "export patterns are not supported for reexport."
          ),
          archive
        ),
        archive = archive
      )
    }
    explicit <- namespace$exports
    if (length(explicit) > 0L && any(nzchar(names(explicit)))) {
      exported_names <- names(explicit)
      exported_names[!nzchar(exported_names)] <- unname(
        explicit[!nzchar(exported_names)]
      )
      exports <- exported_names
    } else {
      exports <- unname(explicit)
    }
    exports <- unique(exports)
    if (any(!nzchar(exports))) {
      .bigbang_abort(
        "bigbang_error_reexport_namespace",
        .bb_trf(
          "Could not read explicit exports from NAMESPACE in archive %s.",
          archive
        ),
        archive = archive
      )
    }
  }

  list(
    path = normalizePath(archive, winslash = "/", mustWork = TRUE),
    ext = ext,
    stem = .archive_stem(archive, ext),
    package = declared_package,
    version = declared_version,
    dependencies = parsed_dependencies$dependencies,
    constraints = parsed_dependencies$constraints,
    exports = exports
  )
}

# Read only the identity needed to propagate an omitted component through the
# dependency graph. This deliberately does not replace full generation-time
# validation: an archive can expose Package and Version while still being
# rejected for another invariant (for example, multiple package roots).
.read_archive_identity <- function(archive, ext) {
  temp_dir <- tempfile("bigbang-identity-")
  if (!dir.create(temp_dir)) {
    stop(.bb_trf("Could not create temporary directory for %s", archive),
         call. = FALSE)
  }
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  listing <- tryCatch(suppressWarnings({
    if (identical(tolower(ext), ".zip")) {
      utils::unzip(archive, list = TRUE)
    } else {
      .untar_quiet(archive, list = TRUE)
    }
  }), error = identity)
  if (inherits(listing, "error")) {
    stop(.bb_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(listing)
    ), call. = FALSE)
  }
  listing_status <- attr(listing, "status")
  if (is.numeric(listing_status) && length(listing_status) == 1L &&
        listing_status != 0) {
    stop(.bb_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, listing_status
    ), call. = FALSE)
  }
  members <- if (identical(tolower(ext), ".zip")) listing$Name else listing
  members <- as.character(members)
  if (length(members) == 0L) {
    stop(.bb_trf("Archive %s must contain one package root directory.", archive),
         call. = FALSE)
  }
  .validate_archive_members(members)
  normalized <- sub("^\\./", "", gsub("\\\\", "/", members))
  candidates <- which(
    normalized == "DESCRIPTION" |
      grepl("^[^/]+/DESCRIPTION$", normalized)
  )
  if (length(candidates) == 0L) {
    stop(.bb_trf("Archive %s has no DESCRIPTION at the package root.", archive),
         call. = FALSE)
  }
  if (length(candidates) != 1L) {
    stop(.bb_trf("Archive %s must contain one package root directory.", archive),
         call. = FALSE)
  }
  member <- members[[candidates[[1L]]]]
  extraction <- tryCatch(suppressWarnings({
    if (identical(tolower(ext), ".zip")) {
      utils::unzip(archive, files = member, exdir = temp_dir)
    } else {
      .untar_quiet(archive, files = member, exdir = temp_dir)
    }
  }), error = identity)
  if (inherits(extraction, "error")) {
    stop(.bb_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(extraction)
    ), call. = FALSE)
  }
  if (is.numeric(extraction) && length(extraction) == 1L && extraction != 0) {
    stop(.bb_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, extraction
    ), call. = FALSE)
  }
  .validate_extracted_links(temp_dir, archive)
  description_files <- list.files(
    temp_dir, pattern = "^DESCRIPTION$", recursive = TRUE,
    full.names = TRUE, all.files = TRUE
  )
  if (length(description_files) != 1L) {
    stop(.bb_trf("Archive %s has no DESCRIPTION at the package root.", archive),
         call. = FALSE)
  }
  description <- tryCatch(
    read.dcf(description_files[[1L]], fields = c("Package", "Version")),
    error = identity
  )
  if (inherits(description, "error")) stop(description)
  if (nrow(description) == 0L) {
    stop(.bb_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }
  field <- function(name) {
    if (!name %in% colnames(description)) return(NA_character_)
    value <- unname(description[1L, name])
    if (is.na(value)) NA_character_ else trimws(value)
  }
  package <- field("Package")
  version <- field("Version")
  if (is.na(package) || !nzchar(package)) {
    stop(.bb_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }
  list(package = package, version = version)
}

.read_archive_version <- function(archive, ext) {
  temp_dir <- tempfile("bigbang-version-")
  if (!dir.create(temp_dir)) {
    stop(.bb_trf("Could not create temporary directory for %s", archive),
         call. = FALSE)
  }
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  listing <- tryCatch(suppressWarnings({
    if (identical(tolower(ext), ".zip")) {
      utils::unzip(archive, list = TRUE)
    } else {
      .untar_quiet(archive, list = TRUE)
    }
  }), error = identity)
  if (inherits(listing, "error")) {
    stop(.bb_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(listing)
    ), call. = FALSE)
  }
  listing_status <- attr(listing, "status")
  if (is.numeric(listing_status) && length(listing_status) == 1L &&
        listing_status != 0) {
    stop(.bb_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, listing_status
    ), call. = FALSE)
  }
  members <- if (identical(tolower(ext), ".zip")) listing$Name else listing
  members <- as.character(members)
  .validate_archive_members(members)
  normalized_members <- sub("^\\./", "", gsub("\\\\", "/", members))
  candidate <- which(
    normalized_members == "DESCRIPTION" |
      grepl("^[^/]+/DESCRIPTION$", normalized_members)
  )
  if (length(candidate) != 1L) {
    stop(.bb_trf(
      "Archive %s has no DESCRIPTION at the package root.", archive
    ), call. = FALSE)
  }
  member <- members[[candidate[[1L]]]]
  extraction <- tryCatch(suppressWarnings({
    if (identical(tolower(ext), ".zip")) {
      utils::unzip(archive, files = member, exdir = temp_dir)
    } else {
      .untar_quiet(archive, files = member, exdir = temp_dir)
    }
  }), error = identity)
  if (inherits(extraction, "error")) {
    stop(.bb_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(extraction)
    ), call. = FALSE)
  }
  if (is.numeric(extraction) && length(extraction) == 1L && extraction != 0) {
    stop(.bb_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, extraction
    ), call. = FALSE)
  }
  .validate_extracted_links(temp_dir, archive)
  description_files <- list.files(
    temp_dir, pattern = "^DESCRIPTION$", recursive = TRUE,
    full.names = TRUE, all.files = TRUE
  )
  if (length(description_files) != 1L) {
    stop(.bb_trf(
      "Archive %s has no DESCRIPTION at the package root.", archive
    ), call. = FALSE)
  }
  description <- read.dcf(
    description_files[[1L]], fields = c("Package", "Version")
  )
  if (nrow(description) == 0L) {
    stop(.bb_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }
  field <- function(name) {
    if (!name %in% colnames(description)) return(NA_character_)
    value <- unname(description[1L, name])
    if (is.na(value)) NA_character_ else trimws(value)
  }
  declared_package <- field("Package")
  declared_version <- field("Version")
  if (is.na(declared_package) || !nzchar(declared_package) ||
        is.na(declared_version) || !nzchar(declared_version)) {
    stop(.bb_trf(
      "Archive %s must declare non-empty Package and Version fields.", archive
    ), call. = FALSE)
  }
  list(package = declared_package, version = declared_version)
}

.parse_dependency_constraints <- function(values) {
  dependencies <- character()
  constraints <- list()
  for (value in values) {
    if (is.na(value) || !nzchar(value)) next
    pieces <- strsplit(value, ",", fixed = TRUE)[[1L]]
    for (piece in pieces) {
      piece <- trimws(piece)
      if (!nzchar(piece)) next
      match <- regexec(
        "^([A-Za-z][A-Za-z0-9.]*)[[:space:]]*\\(([<>=]+)[[:space:]]*([^)]*)\\)$",
        piece, perl = TRUE
      )
      captures <- regmatches(piece, match)[[1L]]
      if (length(captures) == 4L) {
        dependency <- captures[[2L]]
        constraints[[length(constraints) + 1L]] <- list(
          package = dependency,
          op = captures[[3L]],
          version = trimws(captures[[4L]])
        )
      } else {
        dependency <- sub("[[:space:]].*$", "", piece)
      }
      dependencies <- c(dependencies, dependency)
    }
  }
  list(
    dependencies = unique(setdiff(dependencies[nzchar(dependencies)], "R")),
    constraints = constraints
  )
}

.version_satisfies <- function(actual, op, required) {
  tryCatch({
    actual <- base::package_version(actual)
    required <- base::package_version(required)
    switch(
      op,
      ">=" = actual >= required,
      ">" = actual > required,
      "<=" = actual <= required,
      "<" = actual < required,
      "==" = actual == required,
      FALSE
    )
  }, error = function(e) FALSE)
}

.find_archive_root <- function(extract_dir, archive, allow_flat = FALSE) {
  entries <- list.files(
    extract_dir, all.files = TRUE, no.. = TRUE, include.dirs = TRUE
  )
  if (isTRUE(allow_flat) && file.exists(file.path(extract_dir, "DESCRIPTION"))) {
    return(extract_dir)
  }
  # Ignore AppleDouble siblings. Archiving a package directory on macOS with
  # extended attributes emits a "._<dir>" member next to it, and R installs such
  # an archive without complaint, so rejecting it would reject a working package.
  # Only this specific metadata convention is ignored: any other extra entry
  # still means the archive is not a single package root.
  entries <- entries[!startsWith(entries, "._") & entries != ".DS_Store"]
  if (length(entries) != 1L || !dir.exists(file.path(extract_dir, entries[[1L]]))) {
    stop(.bb_trf(
      "Archive %s must contain one package root directory.", archive
    ), call. = FALSE)
  }
  root <- file.path(extract_dir, entries[[1L]])
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop(.bb_trf(
      "Archive %s has no DESCRIPTION at the package root.", archive
    ), call. = FALSE)
  }
  root
}

.validate_extracted_links <- function(extract_dir, archive) {
  entries <- list.files(
    extract_dir, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, include.dirs = TRUE, no.. = TRUE
  )
  if (length(entries) > 0L && any(nzchar(Sys.readlink(entries)))) {
    stop(.bb_trf(
      "Archive %s contains symbolic links, which are not supported.", archive
    ), call. = FALSE)
  }
  invisible(entries)
}

.extract_archive_checked <- function(archive, ext, extract_dir) {
  if (!identical(tolower(ext), ".zip") && !ext %in% c(".tar.gz", ".tar")) {
    stop(.bb_trf("Unsupported archive format: %s", ext), call. = FALSE)
  }
  listing <- tryCatch(suppressWarnings({
    if (identical(tolower(ext), ".zip")) {
      utils::unzip(archive, list = TRUE)
    } else if (ext %in% c(".tar.gz", ".tar")) {
      .untar_quiet(archive, list = TRUE)
    }
  }), error = identity)
  if (inherits(listing, "error")) {
    stop(.bb_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(listing)
    ), call. = FALSE)
  }
  listing_status <- attr(listing, "status")
  if (is.numeric(listing_status) && length(listing_status) == 1L &&
        listing_status != 0) {
    stop(.bb_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, listing_status
    ), call. = FALSE)
  }
  members <- if (identical(tolower(ext), ".zip")) listing$Name else listing
  .validate_archive_members(members)
  extraction <- tryCatch(suppressWarnings({
    if (identical(tolower(ext), ".zip")) {
      utils::unzip(archive, exdir = extract_dir)
    } else {
      .untar_quiet(archive, exdir = extract_dir)
    }
  }), error = identity)
  if (inherits(extraction, "error")) {
    stop(.bb_trf(
      "Could not extract archive %s: %s", archive, conditionMessage(extraction)
    ), call. = FALSE)
  }
  if (is.numeric(extraction) && length(extraction) == 1L && extraction != 0) {
    stop(.bb_trf(
      "Could not extract archive %s: extraction returned status %d.",
      archive, extraction
    ), call. = FALSE)
  }
  .validate_extracted_links(extract_dir, archive)
  invisible(extraction)
}

.version_matches <- function(left, right) {
  tryCatch(
    isTRUE(base::package_version(left) == base::package_version(right)),
    error = function(e) FALSE
  )
}

.empty_tolerated <- function() {
  data.frame(
    relaxation = character(),
    component = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}

.tolerated_entry <- function(relaxation, component, reason) {
  data.frame(
    relaxation = relaxation,
    component = component,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

.combine_tolerated <- function(...) {
  entries <- list(...)
  entries <- entries[vapply(entries, nrow, integer(1L)) > 0L]
  if (length(entries) == 0L) return(.empty_tolerated())
  result <- do.call(rbind, entries)
  rownames(result) <- NULL
  result
}

.validate_archive_metadata <- function(component, tolerate = character()) {
  stem <- component$stem
  expected_name <- sub("_.*", "", stem)
  has_version <- grepl("_", stem, fixed = TRUE)
  expected_version <- if (has_version) {
    sub("^[^_]+_", "", stem)
  } else {
    NA_character_
  }
  tolerated <- .empty_tolerated()
  report_mismatch <- function(reason) {
    if ("filename_mismatch" %in% tolerate) {
      tolerated <<- .combine_tolerated(
        tolerated,
        .tolerated_entry("filename_mismatch", component$package, reason)
      )
    } else {
      warning(reason, call. = FALSE)
    }
  }
  if (!identical(component$package, expected_name)) {
    report_mismatch(.bb_trf(
      "Archive %s declares package %s, but its filename suggests %s.",
      component$path, component$package, expected_name
    ))
  }
  if (has_version && !.version_matches(component$version, expected_version)) {
    report_mismatch(.bb_trf(
      "Archive %s declares version %s, but its filename suggests version %s.",
      component$path, component$version, expected_version
    ))
  }
  tolerated
}

.component_dependency_cycle <- function(components) {
  names_only <- vapply(components, `[[`, character(1L), "package")
  adjacency <- lapply(components, function(item) {
    intersect(item$dependencies, names_only)
  })
  names(adjacency) <- names_only
  context <- new.env(parent = emptyenv())
  context$state <- stats::setNames(rep(0L, length(names_only)), names_only)
  context$path <- character()

  visit <- function(node) {
    if (identical(context$state[[node]], 1L)) {
      start <- match(node, context$path)
      return(c(context$path[start:length(context$path)], node))
    }
    if (identical(context$state[[node]], 2L)) return(character())
    context$state[[node]] <- 1L
    context$path <- c(context$path, node)
    for (dependency in adjacency[[node]]) {
      cycle <- visit(dependency)
      if (length(cycle) > 0L) return(cycle)
    }
    context$path <- context$path[-length(context$path)]
    context$state[[node]] <- 2L
    character()
  }

  for (node in names_only) {
    cycle <- visit(node)
    if (length(cycle) > 0L) return(cycle)
  }
  character()
}

.validate_constraints <- function(components) {
  included <- vapply(components, `[[`, character(1L), "package")
  versions <- vapply(components, `[[`, character(1L), "version")
  names(versions) <- included
  for (index in seq_along(components)) {
    constraints <- components[[index]]$constraints
    local_constraints <- constraints[vapply(
      constraints,
      function(x) x$package %in% included,
      logical(1L)
    )]
    for (constraint in local_constraints) {
      actual <- unname(versions[[constraint$package]])
      if (!.version_satisfies(actual, constraint$op, constraint$version)) {
        .bigbang_abort(
          "bigbang_error_dependency_version",
          .bb_trf(
            "Component %s requires %s %s %s, but the included archive provides version %s.",
            components[[index]]$package, constraint$package, constraint$op,
            constraint$version, actual
          ),
          component = components[[index]]$package,
          dependency = constraint$package,
          required = constraint,
          actual = actual
        )
      }
    }
  }
  invisible(components)
}

.archive_inventory <- function(pkg_dir, ext = NULL, known = list()) {
  dirs <- .normalize_archive_dirs(pkg_dir)
  paths <- unique(unlist(lapply(dirs, function(dir) {
    files <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    files[file.exists(files) & !dir.exists(files)]
  }), use.names = FALSE))
  paths <- paths[vapply(paths, function(path) {
    supported <- tryCatch(.archive_extension(path), error = function(e) NULL)
    !is.null(supported) && (is.null(ext) || identical(tolower(supported), tolower(ext)))
  }, logical(1L))]
  known_paths <- if (length(known) == 0L) {
    character()
  } else {
    vapply(known, `[[`, character(1L), "path")
  }
  unreadable <- list()
  entries <- lapply(paths, function(path) {
    known_index <- match(path, known_paths)
    if (!is.na(known_index)) {
      known[[known_index]]
    } else {
      actual_ext <- .archive_extension(path)
      metadata <- tryCatch(
        .read_archive_metadata(path, ext = actual_ext),
        error = identity
      )
      if (inherits(metadata, "error")) {
        reason <- conditionMessage(metadata)
        guessed_package <- sub("_.*", "", .archive_stem(path, actual_ext))
        warning(.bb_trf(
          "Could not read archive %s; excluding it from the archive inventory: %s",
          path, reason
        ), call. = FALSE)
        unreadable[[length(unreadable) + 1L]] <<- list(
          path = path, package = guessed_package, reason = reason
        )
        return(NULL)
      }
      list(
        path = path,
        ext = actual_ext,
        stem = .archive_stem(path, actual_ext),
        package = metadata$package,
        version = metadata$version,
        dependencies = metadata$dependencies,
        constraints = metadata$constraints
      )
    }
  })
  entries <- Filter(Negate(is.null), entries)
  stems <- if (length(entries) == 0L) {
    character()
  } else {
    vapply(entries, `[[`, character(1L), "stem")
  }
  packages <- if (length(entries) == 0L) {
    character()
  } else {
    vapply(entries, `[[`, character(1L), "package")
  }
  list(
    entries = entries, files = paths, stems = stems, packages = packages,
    unreadable = unreadable
  )
}

.validate_unincluded_deps <- function(components, inventory,
                                      tolerate = character()) {
  included <- vapply(components, `[[`, character(1L), "package")
  tolerated <- .empty_tolerated()
  for (item in components) {
    candidates <- setdiff(item$dependencies, included)
    for (dependency in candidates) {
      match <- which(inventory$packages == dependency)
      if (length(match) > 0L) {
        archive <- inventory$entries[[match[[1L]]]]$path
        reason <- .bb_trf(
          paste0(
            "Component %s declares dependency %s, available at %s but not ",
            "included. Add it to packages or remove the dependency."
          ),
          item$package, dependency, archive
        )
        if ("unincluded_local_dep" %in% tolerate) {
          consequence <- .bb_trf(
            paste0(
              "The generated meta-package will not ship %s; the recipient ",
              "must provide it through pkg_dir or a repository with ",
              "cran_deps = 'install'."
            ),
            dependency
          )
          warning(paste0(
            reason, " ", consequence
          ), call. = FALSE)
          tolerated <- .combine_tolerated(
            tolerated,
            .tolerated_entry(
              "unincluded_local_dep", item$package, reason
            )
          )
        } else {
          .bigbang_abort(
            "bigbang_error_unincluded_dependency",
            reason,
            component = item$package,
            dependency = dependency,
            archive = archive
          )
        }
      } else if (length(inventory$unreadable) > 0L) {
        unreadable <- Filter(
          function(item) identical(item$package, dependency),
          inventory$unreadable
        )
        for (archive in unreadable) {
          warning(.bb_trf(
            paste0(
              "Component %s declares dependency %s, but archive %s could not ",
              "be read and was excluded from the inventory: %s"
            ),
            item$package, dependency, archive$path, archive$reason
          ), call. = FALSE)
        }
      }
    }
  }
  tolerated
}

classify_dependencies <- function(dependencies, pkg_dir = NULL, ext = ".tar.gz",
                                  included_packages = NULL) {
  local_names <- if (is.null(included_packages)) {
    # Classification is also useful as a light-weight diagnostic on a folder
    # that may contain placeholders or archives not meant to be opened. Keep
    # this path filename-based; generation passes `included_packages` and uses
    # the fully validated component table instead.
    dirs <- .normalize_archive_dirs(pkg_dir)
    paths <- unique(unlist(lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = FALSE, all.files = TRUE, no.. = TRUE)
      files[!dir.exists(file.path(dir, files))]
    }), use.names = FALSE))
    paths <- paths[vapply(paths, function(path) {
      tryCatch({
        actual <- .archive_extension(path)
        is.null(ext) || identical(tolower(actual), tolower(ext))
      }, error = function(e) FALSE)
    }, logical(1L))]
    stems <- vapply(paths, .archive_stem, character(1L))
    unique(c(stems, sub("_.*", "", stems)))
  } else {
    included_packages
  }
  is_local <- dependencies %in% unique(local_names)

  list(
    local = unique(dependencies[is_local]),
    cran = unique(dependencies[!is_local])
  )
}

.resolve_r_requirement <- function(metadata, floor = "3.5.0") {
  candidates <- list(list(op = ">=", version = floor))
  for (item in metadata) {
    constraints <- item$constraints[
      vapply(item$constraints, function(x) identical(x$package, "R"), logical(1L))
    ]
    for (constraint in constraints) {
      if (!constraint$op %in% c(">=", ">")) {
        warning(.bb_trf(
          "Component %s declares R constraint %s %s; only >= and > constraints are propagated.",
          item$package, constraint$op, constraint$version
        ), call. = FALSE)
      } else if (.version_satisfies(constraint$version, ">=", "0.0.0")) {
        candidates[[length(candidates) + 1L]] <- constraint
      } else {
        warning(.bb_trf(
          "Component %s declares an invalid R version constraint: %s %s.",
          item$package, constraint$op, constraint$version
        ), call. = FALSE)
      }
    }
  }
  best <- candidates[[1L]]
  for (candidate in candidates[-1L]) {
    newer <- .version_satisfies(candidate$version, ">", best$version)
    same_stricter <- .version_satisfies(candidate$version, "==", best$version) &&
      identical(candidate$op, ">") && identical(best$op, ">=")
    if (newer || same_stricter) best <- candidate
  }
  best
}

.validate_component_archives <- function(resolved, tolerate = character()) {
  components <- resolved$components
  inventory <- resolved$inventory
  names_only <- vapply(components, `[[`, character(1L), "package")
  duplicates <- unique(names_only[duplicated(names_only)])
  if (length(duplicates) > 0L) {
    .bigbang_abort(
      "bigbang_error_duplicate_component",
      .bb_trf(
        "More than one archive was supplied for component package(s): %s.",
        paste(duplicates, collapse = ", ")
      ),
      packages = duplicates
    )
  }

  metadata_tolerated <- lapply(
    components, .validate_archive_metadata, tolerate = tolerate
  )
  .validate_constraints(components)
  dependency_tolerated <- .validate_unincluded_deps(
    components, inventory, tolerate = tolerate
  )
  cycle <- .component_dependency_cycle(components)
  if (length(cycle) > 0L) {
    .bigbang_abort(
      "bigbang_error_cycle",
      .bb_trf(
        "Circular dependencies detected: %s. A clean installation has no valid topological order.",
        paste(cycle, collapse = " -> ")
      ),
      cycles = list(cycle)
    )
  }
  list(
    components = components,
    tolerated = do.call(
      .combine_tolerated,
      c(metadata_tolerated, list(dependency_tolerated))
    )
  )
}

.component_topological_order <- function(components) {
  names_only <- vapply(components, function(x) x[["package"]], character(1L))
  adjacency <- lapply(components, function(component) {
    intersect(component$dependencies, names_only)
  })
  names(adjacency) <- names_only
  state <- new.env(parent = emptyenv())
  state$visited <- character()
  state$ordered <- character()
  visit <- function(node) {
    if (node %in% state$visited) return(invisible(NULL))
    state$visited <- c(state$visited, node)
    for (dependency in adjacency[[node]]) visit(dependency)
    state$ordered <- c(state$ordered, node)
    invisible(NULL)
  }
  for (node in names_only) visit(node)
  state$ordered
}

.validate_generation <- function(
  resolved, tolerate = character(), on_component_error = "abort",
  reexport = FALSE, metapackage_name = NULL
) {
  omitted <- resolved$omitted
  validation <- .validate_component_archives(resolved, tolerate = tolerate)
  archive_paths <- vapply(
    resolved$components, function(x) x[["path"]], character(1L)
  )
  archive_names <- vapply(
    resolved$components, .canonical_archive_name, character(1L)
  )
  archive_keys <- tolower(archive_names)
  if (anyDuplicated(archive_keys)) {
    duplicate_names <- unique(archive_names[duplicated(archive_keys)])
    duplicate_paths <- archive_paths[archive_keys %in% tolower(duplicate_names)]
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
  if (isTRUE(reexport)) {
    component_exports <- lapply(resolved$components, function(component) {
      exports <- .or_null(component$exports, character())
      unique(exports[nzchar(exports)])
    })
    exported_symbols <- unlist(component_exports, use.names = FALSE)
    if (anyDuplicated(exported_symbols)) {
      duplicated_symbols <- unique(exported_symbols[duplicated(exported_symbols)])
      owners <- vapply(duplicated_symbols, function(symbol) {
        packages <- vapply(seq_along(component_exports), function(index) {
          if (symbol %in% component_exports[[index]]) {
            resolved$components[[index]]$package
          } else {
            NA_character_
          }
        }, character(1L))
        paste(stats::na.omit(packages), collapse = ", ")
      }, character(1L))
      .bigbang_abort(
        "bigbang_error_reexport_collision",
        .bb_trf(
          "Cannot re-export symbol(s) %s because components collide: %s.",
          paste(duplicated_symbols, collapse = ", "),
          paste(
            paste0(duplicated_symbols, " (", owners, ")"),
            collapse = "; "
          )
        ),
        symbols = duplicated_symbols,
        components = owners
      )
    }
    if (!is.null(metapackage_name)) {
      own_symbols <- .generated_metapackage_symbols(metapackage_name)
      conflicting <- intersect(unique(exported_symbols), own_symbols)
      if (length(conflicting) > 0L) {
        .bigbang_abort(
          "bigbang_error_reexport_collision",
          .bb_trf(
            "Cannot re-export symbol(s) %s because they belong to generated metapackage code.",
            paste(conflicting, collapse = ", ")
          ),
          symbols = conflicting,
          components = metapackage_name
        )
      }
    }
  }
  list(resolved = resolved, validation = validation, omitted = omitted)
}

#' @return A character vector of dependency names declared in DESCRIPTION.
#' @noRd
extract_dependencies <- function(package, pkg_dir = NULL, ext = ".tar.gz") {
  .read_archive_metadata(package, pkg_dir, ext)$dependencies
}

.strip_r_comments_and_strings <- function(lines, source_file = NULL) {
  parsed <- try(parse(text = lines, keep.source = TRUE), silent = TRUE)
  if (!inherits(parsed, "try-error")) {
    parse_data <- utils::getParseData(parsed)
    if (is.null(parse_data) || nrow(parse_data) == 0L) return("")
    parse_data$text[parse_data$token %in% c("COMMENT", "STR_CONST")] <- ""
    return(paste(parse_data$text, collapse = " "))
  }

  # A malformed source file should still be diagnosable. This fallback is
  # intentionally conservative: it removes ordinary comments and quoted
  # strings without changing the generator's control flow.
  if (!is.null(source_file)) {
    warning(.bb_trf("Could not parse R source file: %s", source_file), call. = FALSE)
  }
  content <- paste(lines, collapse = "\n")
  content <- gsub("(?m)#[^\\n]*$", "", content, perl = TRUE)
  gsub("'(?:\\\\.|[^'\\\\])*'|\"(?:\\\\.|[^\"\\\\])*\"", "", content, perl = TRUE)
}

#' Diagnose implicit dependencies of local packages
#'
#' Scans local packages for references to the recommended packages 'Matrix' and
#' 'class', which can cause `R CMD check` failures when they are used implicitly
#' but not declared as dependencies.
#'
#' @param packages Character vector. Archive paths or stems to examine, e.g.
#'   `"conexiones_0.8.3"`.
#' @param pkg_dir Character. Directory or directories containing local archives
#'   (`.tar.gz`, `.zip`, etc.).
#' @param ext Character. Archive extension. Defaults to `".tar.gz"`.
#'
#' @return A named list with one entry per local package, each a list with two
#'   elements:
#'   \describe{
#'     \item{matrix_refs}{Character vector of references to 'Matrix', with file and line.}
#'     \item{class_refs}{Character vector of references to 'class', with file and line.}
#'   }
#'
#' @details
#' Extracts and scans the R source of each package for patterns that suggest
#' implicit use of 'Matrix' or 'class'. Useful for debugging `R CMD check` errors
#' such as "there is no package called 'Matrix'" even when the package does not
#' appear to use it directly.
#'
#' @examples
#' archives <- system.file("extdata", package = "bigbang")
#' res <- diagnose_dependencies(
#'   packages = "toycomponent_0.1.0",
#'   pkg_dir = archives
#' )
#' res[["toycomponent_0.1.0"]]
#' lapply(res, function(x) x$matrix_refs)
#' @export
diagnose_dependencies <- function(packages, pkg_dir = NULL, ext = ".tar.gz") {
  results <- list()
  resolved <- .resolve_components(packages, pkg_dir, ext)

  for (component in resolved$components) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

    archive <- component$path
    package <- component$input

    # This is an exported entry point, so it gets the same extraction guards as
    # generation. Without them a component carrying a symbolic link made the
    # scanner read a file outside the archive and return its contents in the
    # result, which is how an unrelated file ends up in a diagnostic report.
    .extract_archive_checked(archive, component$ext, temp_dir)

    # Locate references to Matrix and class APIs from the archive root rather
    # than assuming that the root directory has the package name.
    package_root <- .find_archive_root(
      temp_dir, archive,
      allow_flat = identical(tolower(component$ext), ".zip") &&
        file.exists(file.path(temp_dir, "Meta", "package.rds"))
    )
    r_dir <- file.path(package_root, "R")

    if (!dir.exists(r_dir)) {
      message(.bb_trf("No R directory found for package: %s", package))
      next
    }

    r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

    matrix_refs <- character(0)
    class_refs <- character(0)

    for (file in r_files) {
      content <- readLines(file, warn = FALSE)

      matrix_lines <- grep("Matrix|sparseMatrix|[dstz][gsd]Matrix|Sparse", content)
      if (length(matrix_lines) > 0) {
        for (line_number in matrix_lines) {
          matrix_refs <- c(matrix_refs,
                           paste0(basename(file), ":", line_number, " - ",
                                  trimws(content[line_number])))
        }
      }

      class_lines <- grep("\\bclass\\b|\\bknn\\b|\\bLDA\\b|\\bQDA\\b", content)
      if (length(class_lines) > 0) {
        for (line_number in class_lines) {
          class_refs <- c(class_refs,
                          paste0(basename(file), ":", line_number, " - ",
                                 trimws(content[line_number])))
        }
      }
    }

    results[[package]] <- list(
      matrix_refs = matrix_refs,
      class_refs = class_refs
    )
  }

  results
}


#' Detect possible implicit dependencies in local package sources
#'
#' Extracts each archive into an owned temporary directory and scans its R code
#' for conservative package-specific patterns. This supplements, but does not
#' replace, dependencies declared in DESCRIPTION.
#'
#' @param packages Character archive paths or stems.
#' @param pkg_dir Character archive directory or directories.
#' @param ext Character archive extension.
#' @return A sorted character vector of possible dependency names.
#' @noRd
detect_implicit_dependencies <- function(
  packages, pkg_dir = NULL, ext = ".tar.gz", components = NULL
) {
  possible_deps <- character(0)
  if (is.null(components)) {
    components <- .resolve_components(packages, pkg_dir, ext)$components
  }

  # Conservative patterns for common implicit dependencies.
  package_patterns <- list(
    # Special matrix handling
    # Evidence has to point at Matrix itself. The S4 helpers that used to be
    # listed here (setClass, new, representation) belong to `methods`, so any
    # S4 code was reported as needing Matrix.
    "Matrix" = paste0(
      "\\bMatrix\\s*::|\\bMatrix\\s*\\(|\\bsparseMatrix\\s*\\(|",
      "[dstz][gsd]Matrix"
    ),

    # Statistical analysis
    "class" = "\\bclass\\s*::|\\b(?:knn|naiveBayes)\\s*\\(",
    "MASS" = "\\bMASS\\s*::|\\b(?:lda|qda|ridgeReg|boxcox)\\s*\\(",
    "cluster" = "\\bcluster\\s*::|\\b(?:pam|clara|fanny|silhouette)\\s*\\(",

    # Graphics
    "lattice" = "\\bxyplot\\b|\\bbwplot\\b|\\bcontourplot\\b|\\blevelplot\\b|\\bwireframe\\b",
    "grid" = "\\bgrid\\.arrange\\b|\\bgpar\\b|\\bgrobTree\\b|\\bviewport\\b|\\bgrid\\.layout\\b",

    # Data manipulation. Common base names such as `filter` and `select` are
    # not sufficient evidence: require a namespace qualifier or a pipe.
    "data.table" = "\\bdata\\.table\\s*\\(|\\bdt\\[|\\bsetkey\\s*\\(|\\bfread\\s*\\(|\\bfwrite\\s*\\(",
    "dplyr" = "(?:\\bdplyr\\s*::\\s*|%>%\\s*|\\|>\\s*)\\b(?:filter|arrange|select|mutate|group_by|summarise)\\s*\\(",
    "tidyr" = paste0(
      "\\btidyr\\s*::|",
      "\\b(?:gather|spread|separate|unite|pivot_longer|pivot_wider)\\s*\\("
    ),

    # Time series
    "zoo" = "\\bzoo\\s*\\(|\\bzoo\\s*::|\\bcoredata\\s*\\(|\\brollapply\\s*\\(",
    "xts" = "\\bxts\\s*::|\\bxts\\s*\\(|\\b(?:indexClass|periodicity)\\s*\\(",

    # Spatial statistics
    "sp" = "\\b(?:sp\\s*::\\s*)?(?:SpatialPoints|SpatialPolygons|spplot)\\s*\\(|\\bsp\\s*::\\s*over\\s*\\(",
    "sf" = "\\bsf\\s*::|\\bst_\\w+\\s*\\(",

    # Other commonly used packages
    "tibble" = "\\btibble\\s*::|\\btibble\\s*\\(|\\bas_tibble\\s*\\(",
    "readr" = "\\bread_csv\\b|\\bwrite_csv\\b|\\bread_delim\\b|readr::",
    "jsonlite" = "\\bfromJSON\\b|\\btoJSON\\b|jsonlite::",
    "ggplot2" = "\\bggplot2\\s*::|\\bggplot\\s*\\(|\\bgeom_\\w+\\s*\\(|\\bfacet_\\w+\\s*\\(",
    "shiny" = "\\bshinyApp\\b|\\brenderUI\\b|\\bobserveEvent\\b|\\breactiveVal\\b"

  )

  for (component in components) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

    archive <- component$path
    package <- component$input

    tryCatch({
      # Generation validates every component archive before reaching this
      # scanner, so a hostile archive cannot get here today. Extract through the
      # guarded path anyway: this is a helper that could be called from
      # somewhere else later, and the guard costs nothing.
      .extract_archive_checked(archive, component$ext, temp_dir)

      package_root <- .find_archive_root(
        temp_dir, archive,
        allow_flat = identical(tolower(component$ext), ".zip") &&
          file.exists(file.path(temp_dir, "Meta", "package.rds"))
      )
      r_dir <- file.path(package_root, "R")

      if (!dir.exists(r_dir)) {
        warning(.bb_trf("No R directory found for package: %s", package), call. = FALSE)
        next
      }

      r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

      # Scan executable R tokens only. Comments and string literals are not
      # evidence that a component uses a package: a prose sentence containing
      # `filter` must not turn dplyr into a hard dependency of the generated
      # meta-package.
      content <- paste(vapply(
        r_files,
        function(file) {
          # Name the component archive and the path inside it. The extraction
          # directory is a temporary that no longer exists when the reader sees
          # the warning, so reporting it would be unactionable.
          .strip_r_comments_and_strings(
            readLines(file, warn = FALSE),
            source_file = paste0(basename(archive), ": R/", basename(file))
          )
        },
        character(1L)
      ), collapse = " ")

      for (pkg_name in names(package_patterns)) {
        pattern <- package_patterns[[pkg_name]]
        if (grepl(pattern, content, perl = TRUE)) {
          possible_deps <- c(possible_deps, pkg_name)
        }
      }
    }, error = function(e) {
      warning(.bb_trf("Error processing package %s: %s", package, e$message), call. = FALSE)
    })
  }

  sort(unique(possible_deps))
}
