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
          path_result, ext = actual_ext, include_exports = isTRUE(reexport),
          include_reexport_evidence = FALSE
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
          exports = .or_null(metadata$exports, character()),
          imports = .or_null(metadata$imports, list()),
          reexport_evidence = .or_null(
            metadata$reexport_evidence, .reexport_empty_evidence()
          ),
          reexport_evidence_loaded = isTRUE(metadata$reexport_evidence_loaded),
          reexport_native = isTRUE(metadata$reexport_native)
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
    if (isTRUE(reexport) && nrow(omitted) > 0L) {
      .bigbang_abort(
        "bigbang_error_reexport_skipped",
        .bb_trf(
          "No re-export components remain after skip: %s. Repair the omitted archive or choose a complete generation.",
          paste(paste0(omitted$component, " (", omitted$reason, ")"),
                collapse = "; ")
        ),
        symbols = character(), components = omitted$component,
        skipped = omitted
      )
    }
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
      if (isTRUE(reexport) && nrow(omitted) > 0L) {
        .bigbang_abort(
          "bigbang_error_reexport_skipped",
          .bb_trf(
            paste0(
              "All re-export components were omitted by skip: %s. Repair the ",
              "omitted archive or remove dependent components from the generation."
            ),
            paste(paste0(omitted$component, " (", omitted$reason, ")"),
                  collapse = "; ")
          ),
          symbols = character(), components = omitted$component,
          skipped = omitted
        )
      }
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
                                   include_exports = FALSE,
                                   include_reexport_evidence = include_exports) {
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
  imports <- list()
  reexport_evidence <- .reexport_empty_evidence()
  reexport_native <- FALSE
  reexport_evidence_loaded <- FALSE
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
    imports <- .or_null(namespace$imports, list())
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
    reexport_native <- length(.or_null(namespace$dynlibs, list())) > 0L ||
      length(.or_null(namespace$nativeRoutines, list())) > 0L
    if (isTRUE(include_reexport_evidence)) {
      reexport_evidence <- .reexport_source_evidence(
        package_root, package = declared_package
      )
      reexport_evidence$native <- reexport_native
      reexport_evidence_loaded <- TRUE
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
    exports = exports,
    imports = imports,
    reexport_evidence = reexport_evidence,
    reexport_evidence_loaded = reexport_evidence_loaded,
    reexport_native = reexport_native
  )
}

.read_reexport_evidence <- function(component) {
  temp_dir <- tempfile("bigbang-reexport-")
  if (!dir.create(temp_dir)) {
    stop(.bb_trf("Could not create temporary directory for %s", component$path),
         call. = FALSE)
  }
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)
  .extract_archive_checked(component$path, component$ext, temp_dir)
  package_root <- .find_archive_root(temp_dir, component$path)
  evidence <- .reexport_source_evidence(
    package_root, package = component$package
  )
  evidence$native <- isTRUE(component$reexport_native)
  evidence
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
  if (length(entries) > 0L &&
        any(vapply(entries, .path_is_symlink, logical(1L)))) {
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

.reexport_empty_evidence <- function() {
  list(
    assignments = list(), calls = list(), dynamic = list(), mutations = list(),
    indirect = list(), non_simple = list(), native = FALSE,
    parse_errors = list(), sysdata_names = character(),
    sysdata_error = NULL
  )
}

.reexport_source_label <- function(path, package_root) {
  root <- normalizePath(package_root, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  substring(path, nchar(root) + 2L)
}

.reexport_parse_index <- function(data) {
  rows <- seq_len(nrow(data))
  open_rows <- split(rows, data$token)[["'('"]]
  open_parents <- if (is.null(open_rows)) integer() else data$parent[open_rows]
  max_id <- if (length(data$id) == 0L) 0L else max(data$id)
  row_by_id <- integer(max_id)
  parent_by_id <- integer(max_id)
  token_by_id <- character(max_id)
  text_by_id <- character(max_id)
  call_open_parent <- logical(max_id)
  if (max_id > 0L) {
    row_by_id[data$id] <- rows
    parent_by_id[data$id] <- data$parent
    token_by_id[data$id] <- data$token
    text_by_id[data$id] <- data$text
    if (length(open_parents) > 0L) call_open_parent[open_parents] <- TRUE
  }
  list(
    row_by_id = row_by_id,
    id_by_row = data$id,
    children_by_parent = split(rows, as.character(data$parent)),
    rows_by_token = split(rows, data$token),
    token_by_row = data$token,
    token_by_id = token_by_id,
    parent_by_id = parent_by_id,
    text_by_id = text_by_id,
    call_open_parent = call_open_parent,
    call_expression_cache = new.env(hash = TRUE, parent = emptyenv()),
    qualification_cache = new.env(hash = TRUE, parent = emptyenv())
  )
}

.reexport_index_rows <- function(data, index, ids) {
  ids <- as.integer(ids)
  rows <- if (length(ids) == 0L) integer() else
    index$row_by_id[ids[ids > 0L & ids <= length(index$row_by_id)]]
  rows <- rows[!is.na(rows) & rows > 0L]
  if (length(rows) == 0L) data[0, , drop = FALSE] else data[rows, , drop = FALSE]
}

.reexport_index_child_rows <- function(data, index, parent, tokens = NULL) {
  rows <- index$children_by_parent[[as.character(parent)]]
  if (is.null(rows)) rows <- integer()
  rows <- rows[!is.na(rows)]
  if (!is.null(tokens) && length(rows) > 0L) {
    rows <- rows[index$token_by_row[rows] %in% tokens]
  }
  rows
}

.reexport_index_children <- function(data, index, parent, tokens = NULL) {
  rows <- .reexport_index_child_rows(data, index, parent, tokens)
  if (length(rows) == 0L) data[0, , drop = FALSE] else data[rows, , drop = FALSE]
}

.reexport_parse_target <- function(text) {
  expression <- tryCatch(
    parse(text = paste0("function() ", text))[[1L]][[3L]],
    error = function(e) NULL
  )
  if (is.symbol(expression) && length(expression) == 1L) {
    return(as.character(expression))
  }
  if (is.character(expression) && length(expression) == 1L &&
        !is.na(expression)) return(expression)
  NULL
}

.reexport_call_expr_raw <- function(data, function_id, index) {
  if (length(function_id) != 1L || is.na(function_id)) return(NULL)
  if (is.null(index)) index <- .reexport_parse_index(data)
  current <- as.integer(function_id)
  repeat {
    if (current < 1L || current > length(index$row_by_id)) return(NULL)
    row <- index$row_by_id[[current]]
    if (row == 0L) return(NULL)
    if (index$token_by_id[[current]] %in% c("expr", "expr_or_assign_or_help") &&
          isTRUE(index$call_open_parent[[current]])) {
      return(index$id_by_row[[row]])
    }
    parent <- index$parent_by_id[[current]]
    if (identical(parent, 0L)) return(NULL)
    current <- parent
  }
}

.reexport_call_expression <- function(data, function_id, index = NULL) {
  if (length(function_id) != 1L || is.na(function_id)) return(NULL)
  if (is.null(index)) index <- .reexport_parse_index(data)
  key <- as.character(function_id)
  if (exists(key, envir = index$call_expression_cache, inherits = FALSE)) {
    return(get(key, envir = index$call_expression_cache, inherits = FALSE))
  }
  value <- .reexport_call_expr_raw(data, function_id, index)
  assign(key, value, envir = index$call_expression_cache)
  value
}

.reexport_position_before <- function(left, right) {
  if (nrow(left) != 1L || nrow(right) != 1L) return(FALSE)
  left$line1[[1L]] < right$line1[[1L]] ||
    (identical(left$line1[[1L]], right$line1[[1L]]) &&
       left$col1[[1L]] < right$col1[[1L]])
}

.reexport_call_children <- function(data, call_id, open = NULL, index = NULL) {
  if (length(call_id) != 1L || is.na(call_id)) return(data[0, , drop = FALSE])
  if (is.null(index)) index <- .reexport_parse_index(data)
  if (is.null(open)) {
    open <- .reexport_index_children(data, index, call_id, "'('")
  }
  if (nrow(open) != 1L) return(data[0, , drop = FALSE])
  children <- .reexport_index_children(
    data, index, call_id, c("expr", "expr_or_assign_or_help")
  )
  if (nrow(children) == 0L) return(children)
  after_open <- !(
    children$line1 < open$line1[[1L]] |
      (children$line1 == open$line1[[1L]] &
         children$col1 <= open$col1[[1L]])
  ) & (
    children$line1 > open$line1[[1L]] |
      (children$line1 == open$line1[[1L]] &
         children$col1 > open$col1[[1L]])
  )
  children[after_open, , drop = FALSE][order(
    children$line1[after_open], children$col1[after_open],
    children$id[after_open]
  ), , drop = FALSE]
}

.reexport_call_target <- function(data, open, index = NULL) {
  if (nrow(open) != 1L) return(NULL)
  if (is.null(index)) index <- .reexport_parse_index(data)
  call_id <- open$parent[[1L]]
  children <- .reexport_index_children(
    data, index, call_id, c("expr", "expr_or_assign_or_help")
  )
  if (nrow(children) == 0L) return(NULL)
  before_open <- children$line1 < open$line1[[1L]] |
    (children$line1 == open$line1[[1L]] &
       children$col1 < open$col1[[1L]])
  children <- children[before_open, , drop = FALSE]
  if (nrow(children) == 0L) return(NULL)
  children <- children[order(
    children$line1, children$col1, children$id
  ), , drop = FALSE]
  children[nrow(children), , drop = FALSE]
}

.reexport_call_first_argument <- function(data, call_id, index = NULL) {
  if (is.null(index)) index <- .reexport_parse_index(data)
  open <- .reexport_index_children(data, index, call_id, "'('")
  if (nrow(open) != 1L) return(NULL)
  arguments <- .reexport_call_children(data, call_id, open, index)
  if (nrow(arguments) == 0L) return(NULL)
  first <- arguments[1L, , drop = FALSE]
  parsed <- tryCatch(
    parse(text = paste0("f(", first$text[[1L]], ")"))[[1L]][[2L]],
    error = function(e) NULL
  )
  if (is.call(parsed) && identical(as.character(parsed[[1L]]), "=")) {
    parsed <- parsed[[3L]]
  }
  if (is.character(parsed) && length(parsed) == 1L && !is.na(parsed)) {
    return(list(literal = TRUE, value = parsed))
  }
  list(literal = FALSE, value = NULL)
}

.reexport_qualify_fn_raw <- function(data, function_id, index) {
  if (length(function_id) != 1L || is.na(function_id)) {
    return(list(package = NULL, function_name = NULL))
  }
  if (is.null(index)) index <- .reexport_parse_index(data)
  function_id <- as.integer(function_id)
  if (function_id < 1L || function_id > length(index$row_by_id)) {
    return(list(package = NULL, function_name = NULL))
  }
  row <- index$row_by_id[[function_id]]
  if (row == 0L) return(list(package = NULL, function_name = NULL))
  parent <- index$parent_by_id[[function_id]]
  parent_row <- if (parent > 0L && parent <= length(index$row_by_id)) {
    index$row_by_id[[parent]]
  } else {
    0L
  }
  if (parent_row > 0L && parent_row <= length(index$token_by_row) &&
        index$token_by_row[[parent_row]] == "NS_GET") {
    child_rows <- .reexport_index_child_rows(
      data, index, parent, "SYMBOL_PACKAGE"
    )
    package <- index$text_by_id[index$id_by_row[child_rows]]
    return(list(
      package = if (length(package) == 1L) package else NULL,
      function_name = index$text_by_id[[function_id]]
    ))
  }
  if (parent_row > 0L && parent_row <= length(index$token_by_row) &&
        data$token[[parent_row]] %in% c("expr", "expr_or_assign_or_help")) {
    ns_get <- .reexport_index_child_rows(data, index, parent, "NS_GET")
    if (length(ns_get) == 1L) {
      package_rows <- .reexport_index_child_rows(
        data, index, parent, "SYMBOL_PACKAGE"
      )
      package <- index$text_by_id[index$id_by_row[package_rows]]
      return(list(
        package = if (length(package) == 1L) package else NULL,
        function_name = index$text_by_id[[function_id]]
      ))
    }
  }
  list(package = NULL,
       function_name = index$text_by_id[[function_id]])
}

.reexport_qualify_function <- function(data, function_id, index = NULL) {
  if (length(function_id) != 1L || is.na(function_id)) {
    return(list(package = NULL, function_name = NULL))
  }
  if (is.null(index)) index <- .reexport_parse_index(data)
  key <- as.character(function_id)
  if (exists(key, envir = index$qualification_cache, inherits = FALSE)) {
    return(get(key, envir = index$qualification_cache, inherits = FALSE))
  }
  value <- .reexport_qualify_fn_raw(data, function_id, index)
  assign(key, value, envir = index$qualification_cache)
  value
}

.reexport_parse_descendants <- function(data, roots, index = NULL) {
  if (is.null(index)) index <- .reexport_parse_index(data)
  roots <- unique(as.integer(roots))
  found <- roots
  seen <- new.env(hash = TRUE, parent = emptyenv())
  for (root in roots) assign(as.character(root), TRUE, envir = seen)
  repeat {
    rows <- unlist(lapply(found, function(parent) {
      index$children_by_parent[[as.character(parent)]]
    }), use.names = FALSE)
    children <- unique(data$id[rows])
    children <- children[!vapply(
      children, function(child) {
        exists(as.character(child), envir = seen, inherits = FALSE)
      }, logical(1L)
    )]
    if (length(children) == 0L) break
    for (child in children) assign(as.character(child), TRUE, envir = seen)
    found <- c(found, children)
  }
  found
}

.reexport_is_descendant <- function(data, node, ancestor, index = NULL) {
  current <- as.integer(node)
  ancestor <- as.integer(ancestor)
  if (length(current) != 1L || length(ancestor) != 1L ||
        is.na(current) || is.na(ancestor)) return(FALSE)
  if (is.null(index)) index <- .reexport_parse_index(data)
  seen <- integer()
  repeat {
    if (identical(current, ancestor)) return(TRUE)
    if (current %in% seen) return(FALSE)
    seen <- c(seen, current)
    row <- .reexport_index_rows(data, index, current)
    if (nrow(row) == 0L || identical(row$parent[[1L]], 0L)) return(FALSE)
    current <- row$parent[[1L]]
  }
}

.reexport_call_arg_id <- function(data, call_id, index = NULL) {
  if (is.null(index)) index <- .reexport_parse_index(data)
  open <- .reexport_index_children(data, index, call_id, "'('")
  if (nrow(open) != 1L) return(NULL)
  arguments <- .reexport_call_children(data, call_id, open, index)
  if (nrow(arguments) == 0L) return(NULL)
  arguments$id[[1L]]
}

.reexport_definition_name <- function(data, root, index = NULL) {
  if (is.null(index)) index <- .reexport_parse_index(data)
  assignment <- .reexport_index_children(
    data, index, root,
    c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "RIGHT_ASSIGN2")
  )
  if (nrow(assignment) != 1L) return(NULL)
  lhs <- .reexport_index_children(
    data, index, root, c("expr", "expr_or_assign_or_help")
  )
  lhs <- lhs[lhs$line1 < assignment$line1[[1L]] |
               (lhs$line1 == assignment$line1[[1L]] &
                  lhs$col1 < assignment$col1[[1L]]), , drop = FALSE]
  if (nrow(lhs) == 0L) return(NULL)
  lhs <- lhs[order(lhs$line1, lhs$col1, lhs$id), , drop = FALSE]
  lhs <- lhs[nrow(lhs), , drop = FALSE]
  .reexport_parse_target(lhs$text[[1L]])
}

.reexport_parse_definitions <- function(data, index = NULL) {
  if (is.null(index)) index <- .reexport_parse_index(data)
  function_rows <- index$rows_by_token[["FUNCTION"]]
  function_exprs <- if (is.null(function_rows)) integer() else data$id[function_rows]
  function_nodes <- data$parent[match(function_exprs, data$id)]
  definitions <- list()
  roots <- integer()
  for (function_node in function_nodes) {
    node <- .reexport_index_rows(data, index, function_node)
    root <- if (nrow(node) == 1L) node$parent[[1L]] else integer()
    if (length(root) != 1L) next
    name <- .reexport_definition_name(data, root, index)
    if (is.null(name)) next
    body <- .reexport_index_children(data, index, function_node, "expr")$id
    if (length(body) == 0L) next
    definitions[[name]] <- list(node = function_node, body = body[[1L]], root = root)
    roots <- c(roots, root)
  }
  list(definitions = definitions, roots = roots)
}

.reexport_literal_arg <- function(data, call_id, position, index = NULL) {
  if (is.null(index)) index <- .reexport_parse_index(data)
  open <- .reexport_index_children(data, index, call_id, "'('")
  if (nrow(open) != 1L) return(NULL)
  arguments <- .reexport_call_children(data, call_id, open, index)
  if (nrow(arguments) == 0L) return(NULL)
  if (length(position) != 1L || is.na(position) || nrow(arguments) < position) {
    return(NULL)
  }
  parsed <- tryCatch(
    parse(text = paste0("f(", arguments$text[[position]], ")"))[[1L]][[2L]],
    error = function(e) NULL
  )
  if (is.call(parsed) && identical(as.character(parsed[[1L]]), "=")) {
    parsed <- parsed[[3L]]
  }
  if (is.character(parsed) && length(parsed) == 1L && !is.na(parsed)) {
    parsed
  } else {
    NULL
  }
}

.reexport_simple_symbol <- function(text) {
  text <- trimws(text)
  grepl("^`?[A-Za-z.][A-Za-z0-9._]*`?$", text, perl = TRUE) ||
    grepl("^`[^`]+`$", text, perl = TRUE)
}

.reexport_call_target_root <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    return(NULL)
  }
  expression <- tryCatch(
    parse(text = paste0("(", text, ")"))[[1L]][[2L]],
    error = function(e) NULL
  )
  if (!is.call(expression) || length(expression) == 0L) return(NULL)
  head <- expression[[1L]]
  head_text <- tryCatch(as.character(head), error = function(e) character())
  if (length(head_text) > 0L && !is.na(head_text[[1L]])) {
    head_text[[1L]]
  } else {
    NULL
  }
}

.reexport_call_target_kind <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    return(list(kind = "undetermined", reason = .bb_tr(
      "The call target is not a scalar string."
    )))
  }
  trimmed <- trimws(text)
  if (!nzchar(trimmed)) {
    return(list(kind = "undetermined", reason = .bb_tr(
      "The call target is empty."
    )))
  }
  if (.reexport_simple_symbol(trimmed)) {
    return(list(kind = "simple", name = trimmed))
  }
  expression <- tryCatch(
    parse(text = paste0("(", trimmed, ")"))[[1L]][[2L]],
    error = function(e) NULL
  )
  if (is.null(expression)) {
    return(list(
      kind = "undetermined",
      reason = .bb_tr("The call target could not be parsed independently.")
    ))
  }
  if (is.symbol(expression) && length(expression) == 1L &&
        identical(trimmed, as.character(expression))) {
    return(list(kind = "simple", name = as.character(expression)))
  }
  head_text <- if (is.call(expression) && length(expression) > 0L) {
    as.character(expression[[1L]])
  } else {
    character()
  }
  if (is.call(expression) && length(expression) == 3L &&
        length(head_text) == 1L && head_text %in% c("::", ":::") &&
        is.symbol(expression[[2L]]) && is.symbol(expression[[3L]])) {
    return(list(
      kind = "qualified", package = as.character(expression[[2L]]),
      name = as.character(expression[[3L]])
    ))
  }
  root <- .reexport_call_target_root(trimmed)
  if (length(root) == 1L) {
    return(list(
      kind = "calculated", root = root,
      reason = .bb_trf("The call target is calculated through '%s'.", root)
    ))
  }
  list(
    kind = "undetermined",
    reason = .bb_tr("The call target has no classifiable root expression.")
  )
}

.reexport_package_scope <- function(parsed, package = NULL) {
  key <- function(file_index, node) paste(file_index, node, sep = "::")
  indices <- lapply(parsed, function(item) {
    if (is.null(item$index)) .reexport_parse_index(item$data) else item$index
  })
  definitions <- list()
  definition_roots <- vector("list", length(parsed))
  for (index in seq_along(parsed)) {
    info <- .reexport_parse_definitions(
      parsed[[index]]$data, indices[[index]]
    )
    definition_roots[[index]] <- info$roots
    for (name in names(info$definitions)) {
      entry <- info$definitions[[name]]
      entry$file_index <- index
      definitions[[name]] <- entry
    }
  }

  definition_names <- new.env(hash = TRUE, parent = emptyenv())
  definition_by_name <- new.env(hash = TRUE, parent = emptyenv())
  definition_root_sets <- lapply(definition_roots, function(roots) {
    stats::setNames(rep(TRUE, length(roots)), as.character(roots))
  })
  for (name in names(definitions)) {
    definition <- definitions[[name]]
    assign(key(definition$file_index, definition$root), name,
           envir = definition_names)
    assign(name, TRUE, envir = definition_by_name)
  }

  active_state <- new.env(parent = emptyenv())
  active_state$parts <- list()
  active_state$count <- 0L
  active_keys <- new.env(hash = TRUE, parent = emptyenv())
  activate <- function(keys) {
    if (length(keys) == 0L) return(invisible(NULL))
    active_state$parts[[length(active_state$parts) + 1L]] <- keys
    active_state$count <- active_state$count + length(keys)
    for (item in keys) assign(item, TRUE, envir = active_keys)
    invisible(NULL)
  }
  for (index in seq_along(parsed)) {
    data <- parsed[[index]]$data
    roots <- data$id[data$parent == 0L & data$token == "expr"]
    for (root in roots) {
      if (isTRUE(definition_root_sets[[index]][as.character(root)])) {
        root_key <- key(index, root)
        name <- if (exists(root_key, envir = definition_names,
                           inherits = FALSE)) {
          get(root_key, envir = definition_names, inherits = FALSE)
        } else {
          character()
        }
        if (length(name) == 1L && name %in% c(".onLoad", ".onAttach")) {
          definition <- definitions[[name]]
          activate(key(index, .reexport_parse_descendants(
            data, definition$body, indices[[index]]
          )))
        } else {
          activate(key(index, root))
        }
      } else {
        activate(key(index, .reexport_parse_descendants(
          data, root, indices[[index]]
        )))
      }
    }
  }

  indirect_names <- c(
    "do.call", "get", "getFromNamespace", "match.fun", "Recall", "mget",
    "setLoadAction"
  )
  calls <- lapply(seq_along(parsed), function(index) {
    data <- parsed[[index]]$data
    rows <- indices[[index]]$rows_by_token[["SYMBOL_FUNCTION_CALL"]]
    if (is.null(rows)) return(data.frame())
    rows <- rows[data$text[rows] %in% c(names(definitions), indirect_names)]
    rows <- data[rows, , drop = FALSE]
    if (nrow(rows) == 0L) return(data.frame())
    rows$file_index <- index
    rows
  })
  calls <- Filter(function(rows) nrow(rows) > 0L, calls)
  calls <- if (length(calls) == 0L) data.frame() else do.call(rbind, calls)
  indirect <- vector("list", length(parsed))
  non_simple <- vector("list", length(parsed))
  indirect_seen <- new.env(hash = TRUE, parent = emptyenv())
  followed <- new.env(hash = TRUE, parent = emptyenv())
  repeat {
    before <- active_state$count
    if (nrow(calls) > 0L) for (row_index in seq_len(nrow(calls))) {
      call <- calls[row_index, , drop = FALSE]
      call_key <- key(call$file_index[[1L]], call$id[[1L]])
      if (!exists(call_key, envir = active_keys, inherits = FALSE)) next
      function_name <- call$text[[1L]]
      if (!exists(function_name, envir = definition_by_name,
                  inherits = FALSE) ||
            exists(function_name, envir = followed, inherits = FALSE)) next
      qualification <- .reexport_qualify_function(
        parsed[[call$file_index[[1L]]]]$data, call$id[[1L]],
        indices[[call$file_index[[1L]]]]
      )
      if (!is.null(qualification$package) && !is.null(package) &&
            !identical(qualification$package, package)) next
      assign(function_name, TRUE, envir = followed)
      definition <- definitions[[function_name]]
      activate(key(definition$file_index, .reexport_parse_descendants(
        parsed[[definition$file_index]]$data,
        definition$body,
        indices[[definition$file_index]]
      )))
    }
    if (nrow(calls) > 0L) for (row_index in seq_len(nrow(calls))) {
      call <- calls[row_index, , drop = FALSE]
      call_key <- key(call$file_index[[1L]], call$id[[1L]])
      if (!exists(call_key, envir = active_keys, inherits = FALSE) ||
            !call$text[[1L]] %in% indirect_names) next
      seen_key <- paste(call$file_index[[1L]], call$id[[1L]], sep = "::")
      if (exists(seen_key, envir = indirect_seen, inherits = FALSE)) next
      assign(seen_key, TRUE, envir = indirect_seen)
      data <- parsed[[call$file_index[[1L]]]]$data
      index_data <- indices[[call$file_index[[1L]]]]
      call_id <- .reexport_call_expression(data, call$id[[1L]], index_data)
      first <- if (is.null(call_id)) NULL else
        .reexport_call_first_argument(data, call_id, index_data)
      namespace <- if (identical(call$text[[1L]], "getFromNamespace") &&
                         !is.null(call_id)) {
        .reexport_literal_arg(data, call_id, 2L, index_data)
      } else {
        NULL
      }
      foreign_namespace <- identical(call$text[[1L]], "getFromNamespace") &&
        (!is.null(namespace) &&
           (is.null(package) || !identical(namespace, package)))
      item <- list(
        name = call$text[[1L]], file = parsed[[call$file_index[[1L]]]]$label,
        line = call$line1[[1L]],
        literal = if (is.null(first)) FALSE else isTRUE(first$literal),
        value = if (is.null(first)) NULL else first$value,
        namespace = namespace, foreign_namespace = foreign_namespace,
        active = TRUE, followed = FALSE
      )
      if (!is.null(first) && isTRUE(first$literal) &&
            exists(first$value, envir = definition_by_name,
                   inherits = FALSE) && !foreign_namespace) {
        item$followed <- TRUE
        if (!exists(first$value, envir = followed, inherits = FALSE)) {
          assign(first$value, TRUE, envir = followed)
          definition <- definitions[[first$value]]
          activate(key(definition$file_index, .reexport_parse_descendants(
            parsed[[definition$file_index]]$data,
            definition$body,
            indices[[definition$file_index]]
          )))
        }
      }
      index <- call$file_index[[1L]]
      indirect[[index]][[length(indirect[[index]]) + 1L]] <- item
    }
    if (active_state$count == before) break
  }

  active <- if (length(active_state$parts) == 0L) {
    character()
  } else {
    unlist(active_state$parts, use.names = FALSE)
  }

  for (index in seq_along(parsed)) {
    data <- parsed[[index]]$data
    parse_index <- indices[[index]]
    open_rows <- parse_index$rows_by_token[["'('"]]
    open_rows <- open_rows[data$parent[open_rows] %in% data$id[
      unlist(parse_index$rows_by_token[
        c("expr", "expr_or_assign_or_help")
      ], use.names = FALSE)
    ]]
    if (length(open_rows) == 0L) next
    active_keys <- stats::setNames(rep(TRUE, length(active)), active)
    for (open_row in open_rows) {
      call_id <- data$parent[[open_row]]
      call_key <- key(index, call_id)
      if (!isTRUE(active_keys[call_key])) next
      child_rows <- .reexport_index_child_rows(
        data, parse_index, call_id, c("expr", "expr_or_assign_or_help")
      )
      if (length(child_rows) == 0L) next
      before <- data$line1[child_rows] < data$line1[[open_row]] |
        (data$line1[child_rows] == data$line1[[open_row]] &
           data$col1[child_rows] < data$col1[[open_row]])
      child_rows <- child_rows[before]
      if (length(child_rows) == 0L) next
      child_rows <- child_rows[order(
        data$line1[child_rows], data$col1[child_rows], data$id[child_rows]
      )]
      target_text <- trimws(data$text[[child_rows[[length(child_rows)]]]])
      target_kind <- .reexport_call_target_kind(target_text)
      if (target_kind$kind %in% c("simple", "qualified")) next
      root <- .reexport_call_target_root(target_text)
      if (length(root) == 1L && root %in% indirect_names) next
      call_id <- as.integer(call_id)
      call_row <- if (call_id > 0L && call_id <= length(parse_index$row_by_id)) {
        parse_index$row_by_id[[call_id]]
      } else {
        0L
      }
      line <- if (call_row > 0L) data$line1[[call_row]] else NA_integer_
      non_simple[[index]][[length(non_simple[[index]]) + 1L]] <- list(
        name = target_text, file = parsed[[index]]$label,
        line = line, active = TRUE, non_simple = TRUE,
        reason = target_kind$reason
      )
    }
  }
  list(
    active = lapply(seq_along(parsed), function(index) {
      ids <- sub(paste0("^", index, "::"), "", active)
      as.integer(ids[grepl(paste0("^", index, "::"), active)])
    }),
    indirect = indirect, non_simple = non_simple
  )
}

.reexport_parse_source <- function(path, package_root, data,
                                   active_ids = NULL, scope_indirect = list(),
                                   scope_non_simple = list(), package = NULL,
                                   index = NULL) {
  label <- .reexport_source_label(path, package_root)
  evidence <- list(
    assignments = list(), calls = list(), dynamic = list(), mutations = list(),
    indirect = list(), non_simple = list(), parse_errors = list()
  )
  if (is.null(data) || nrow(data) == 0L) return(evidence)
  parse_index <- if (is.null(index)) .reexport_parse_index(data) else index
  scope_indirect <- lapply(scope_indirect, function(item) {
    item$file <- label
    item
  })
  evidence$indirect <- c(evidence$indirect, scope_indirect)
  evidence$non_simple <- scope_non_simple

  binder_names <- c(
    "assign", "delayedAssign", "makeActiveBinding", "list2env", "sys.source",
    "assignInMyNamespace", "assignInNamespace", "source", "load", "attach",
    "setGeneric", "setClass", "setRefClass", "setValidity", "setGroupGeneric",
    "setMethod", "lockBinding", "unlockBinding"
  )

  assignment_tokens <- c(
    "LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "RIGHT_ASSIGN2"
  )
  assignment_rows <- sort(unlist(
    parse_index$rows_by_token[assignment_tokens], use.names = FALSE
  ))
  for (row_index in assignment_rows) {
    parent <- data$parent[[row_index]]
    candidate_rows <- .reexport_index_child_rows(
      data, parse_index, parent, c("expr", "expr_or_assign_or_help")
    )
    if (length(candidate_rows) == 0L) next
    candidate_rows <- candidate_rows[order(
      data$line1[candidate_rows], data$col1[candidate_rows],
      data$id[candidate_rows]
    )]
    before <- data$line1[candidate_rows] < data$line1[[row_index]] |
      (data$line1[candidate_rows] == data$line1[[row_index]] &
         data$col1[candidate_rows] < data$col1[[row_index]])
    candidate_index <- if (data$token[[row_index]] %in%
                             c("RIGHT_ASSIGN", "RIGHT_ASSIGN2")) {
      which(!before)[1L]
    } else {
      which(before)[sum(before)]
    }
    if (length(candidate_index) == 0L || is.na(candidate_index)) next
    target_text <- data$text[[candidate_rows[[candidate_index]]]]
    target <- .reexport_parse_target(target_text)
    if (!is.null(target)) {
      evidence$assignments[[length(evidence$assignments) + 1L]] <- list(
        symbol = target, file = label, line = data$line1[[row_index]]
      )
    } else {
      target_expression <- tryCatch(
        parse(text = paste0("function() ", target_text))[[1L]][[3L]],
        error = function(e) NULL
      )
      if (is.call(target_expression) &&
            as.character(target_expression[[1L]])[[1L]] %in%
              c("$", "[[", "@")) {
        extracted <- if (length(target_expression) >= 3L) {
          target_expression[[3L]]
        } else {
          NULL
        }
        literal <- if (is.symbol(extracted)) {
          as.character(extracted)
        } else if (is.character(extracted) && length(extracted) == 1L) {
          extracted
        } else {
          NULL
        }
        evidence$mutations[[length(evidence$mutations) + 1L]] <- list(
          symbol = literal, name = as.character(target_expression[[1L]])[[1L]],
          file = label, line = data$line1[[row_index]],
          active = data$id[[row_index]] %in% active_ids
        )
      }
    }
  }

  target_calls <- binder_names
  function_rows <- parse_index$rows_by_token[["SYMBOL_FUNCTION_CALL"]]
  function_rows <- if (is.null(function_rows)) integer() else function_rows
  has_load_hook <- any(data$text %in% c(".onLoad", ".onAttach"))
  if (!has_load_hook) {
    function_rows <- function_rows[data$text[function_rows] %in% c(
      binder_names, "eval", "evalq", "env_bind"
    )]
  }
  for (row_index in function_rows) {
    function_id <- data$id[[row_index]]
    call_id <- .reexport_call_expression(data, function_id, parse_index)
    if (is.null(call_id)) next
    qualification <- .reexport_qualify_function(
      data, function_id, parse_index
    )
    if (length(qualification$function_name) != 1L ||
          is.na(qualification$function_name)) next
    first <- .reexport_call_first_argument(data, call_id, parse_index)
    record <- list(
      name = qualification$function_name,
      package = qualification$package,
      file = label,
      line = data$line1[[row_index]],
      literal = if (is.null(first)) FALSE else isTRUE(first$literal),
      value = if (is.null(first)) NULL else first$value,
      active = function_id %in% active_ids
    )
    function_name <- qualification$function_name
    is_env_bind <- identical(qualification$package, "rlang") &&
      startsWith(function_name, "env_bind")
    is_namespace_assign <- identical(qualification$package, "utils") &&
      function_name %in% c("assignInMyNamespace", "assignInNamespace")
    call_id <- as.integer(call_id)
    call_row <- if (call_id > 0L && call_id <= length(parse_index$row_by_id)) {
      parse_index$row_by_id[[call_id]]
    } else {
      0L
    }
    call_text <- if (call_row > 0L) data$text[[call_row]] else ""
    is_dynamic <- function_name %in% c(
      "list2env", "sys.source", "source", "load", "attach"
    ) ||
      is_env_bind || is_namespace_assign ||
      (function_name %in% c("eval", "evalq") && !is.null(first) &&
         grepl("(^|[[(,[:space:]])(parse|str2lang|str2expression)[[:space:]]*\\(",
               call_text, perl = TRUE))
    if (is_dynamic) {
      evidence$dynamic[[length(evidence$dynamic) + 1L]] <- record
    }
    if (function_name %in% target_calls) {
      evidence$calls[[length(evidence$calls) + 1L]] <- record
    }
    if (length(qualification$package) == 1L &&
          !is.null(qualification$package) &&
          !identical(qualification$package, "base") &&
          !identical(qualification$package, "utils") &&
          !identical(function_name, ".") &&
          (is.null(package) || !identical(qualification$package, package)) &&
          has_load_hook) {
      evidence$indirect[[length(evidence$indirect) + 1L]] <- list(
        name = paste0(qualification$package, "::", function_name),
        file = label, line = data$line1[[row_index]],
        active = function_id %in% active_ids
      )
    }
  }

  # A binder can be passed as a value or by name (for example to lapply(),
  # do.call(), get(), or match.fun()). Parse data retains both forms.
  binder_row_indices <- sort(unlist(parse_index$rows_by_token[
    c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST")
  ], use.names = FALSE))
  binder_rows <- data[binder_row_indices, , drop = FALSE]
  if (nrow(binder_rows) > 0L) {
    string_rows <- binder_rows$token == "STR_CONST"
    if (any(string_rows)) {
      binder_rows$text[string_rows] <- vapply(
        binder_rows$text[string_rows], function(value) {
          parsed <- tryCatch(parse(text = value)[[1L]], error = function(e) NULL)
          if (is.character(parsed) && length(parsed) == 1L) parsed else value
        }, character(1L)
      )
    }
    binder_rows <- binder_rows[binder_rows$text %in% binder_names, , drop = FALSE]
  }
  if (nrow(binder_rows) > 0L) {
    indirect_names <- c(
      "do.call", "get", "getFromNamespace", "match.fun", "Recall", "mget",
      "setLoadAction"
    )
    function_rows <- parse_index$rows_by_token[["SYMBOL_FUNCTION_CALL"]]
    function_rows <- if (is.null(function_rows)) integer() else function_rows
    function_rows <- function_rows[data$text[function_rows] %in%
                                     c(binder_names, indirect_names)]
    children_by_parent <- parse_index$children_by_parent
    call_contexts <- lapply(function_rows, function(function_index) {
      function_id <- data$id[[function_index]]
      call_id <- .reexport_call_expression(
        data, function_id, parse_index
      )
      if (is.null(call_id)) return(NULL)
      first_id <- .reexport_call_arg_id(data, call_id, parse_index)
      if (is.null(first_id)) return(NULL)
      qualification <- .reexport_qualify_function(
        data, function_id, parse_index
      )
      list(
        first_id = first_id, name = qualification$function_name,
        package = qualification$package
      )
    })
    call_contexts <- Filter(Negate(is.null), call_contexts)
    context_by_row <- new.env(hash = TRUE, parent = emptyenv())
    for (context in call_contexts) {
      found <- as.integer(context$first_id)
      pending <- found
      while (length(pending) > 0L) {
        children <- unlist(
          children_by_parent[as.character(pending)],
          use.names = FALSE
        )
        children <- unique(as.integer(data$id[children]))
        children <- children[!is.na(children) & !children %in% found]
        if (length(children) == 0L) break
        found <- c(found, children)
        pending <- children
      }
      for (row_id in found) {
        key <- as.character(row_id)
        if (!exists(key, envir = context_by_row, inherits = FALSE)) {
          assign(key, context, envir = context_by_row)
        }
      }
    }
    call_context <- function(row_id) {
      key <- as.character(row_id)
      if (!exists(key, envir = context_by_row, inherits = FALSE)) return(NULL)
      context <- get(key, envir = context_by_row, inherits = FALSE)
      list(name = context$name, package = context$package)
    }
    for (index in seq_len(nrow(binder_rows))) {
      row <- binder_rows[index, , drop = FALSE]
      if (identical(row$token[[1L]], "STR_CONST")) {
        context <- call_context(row$id[[1L]])
        if (is.null(context) || !context$name %in% c(
          binder_names, indirect_names
        )) next
        is_indirect <- context$name %in% indirect_names
        evidence$mutations[[length(evidence$mutations) + 1L]] <- list(
          symbol = if (is_indirect) NULL else row$text[[1L]],
          name = if (is_indirect) row$text[[1L]] else context$name,
          file = label, line = row$line1[[1L]],
          active = row$id[[1L]] %in% active_ids
        )
        next
      }
      evidence$mutations[[length(evidence$mutations) + 1L]] <- list(
        symbol = NULL, name = row$text[[1L]], file = label,
        line = row$line1[[1L]], active = row$id[[1L]] %in% active_ids
      )
    }
  }
  evidence
}

.reexport_get_parse_data <- function(...) {
  utils::getParseData(...)
}

.reexport_source_evidence <- function(package_root, package = NULL) {
  evidence <- .reexport_empty_evidence()
  r_dir <- file.path(package_root, "R")
  if (dir.exists(r_dir)) {
    files <- sort(list.files(
      r_dir, pattern = "\\.(R|r|S|s|q)$", full.names = TRUE, recursive = TRUE
    ))
    empty_data <- data.frame(
      id = integer(), parent = integer(), token = character(),
      text = character(), line1 = integer(), col1 = integer()
    )
    parsed <- lapply(files, function(path) {
      label <- .reexport_source_label(path, package_root)
      source <- tryCatch(parse(file = path, keep.source = TRUE), error = identity)
      if (inherits(source, "error")) {
        return(list(
          path = path, label = label, data = empty_data,
          parse_error = conditionMessage(source)
        ))
      }
      data <- tryCatch(
        .reexport_get_parse_data(source, includeText = TRUE),
        error = identity
      )
      if (inherits(data, "error")) {
        return(list(
          path = path, label = label, data = empty_data,
          parse_error = paste0("Could not inspect parse data: ",
                               conditionMessage(data))
        ))
      }
      if (is.null(data)) data <- empty_data
      list(path = path, label = label, data = data,
           index = .reexport_parse_index(data), parse_error = NULL)
    })
    scope <- .reexport_package_scope(parsed, package = package)
    parsed_evidence <- lapply(seq_along(parsed), function(index) {
      .reexport_parse_source(
        parsed[[index]]$path, package_root, data = parsed[[index]]$data,
        active_ids = scope$active[[index]],
        scope_indirect = scope$indirect[[index]],
        scope_non_simple = scope$non_simple[[index]], package = package,
        index = parsed[[index]]$index
      )
    })
    parsed_evidence <- lapply(seq_along(parsed_evidence), function(index) {
      if (!is.null(parsed[[index]]$parse_error)) {
        parsed_evidence[[index]]$parse_errors <- list(list(
          file = parsed[[index]]$label, error = parsed[[index]]$parse_error
        ))
      }
      parsed_evidence[[index]]
    })
    evidence$assignments <- unlist(lapply(parsed_evidence, `[[`, "assignments"),
                                   recursive = FALSE)
    evidence$calls <- unlist(lapply(parsed_evidence, `[[`, "calls"), recursive = FALSE)
    evidence$dynamic <- unlist(lapply(parsed_evidence, `[[`, "dynamic"), recursive = FALSE)
    evidence$mutations <- unlist(lapply(parsed_evidence, `[[`, "mutations"), recursive = FALSE)
    evidence$indirect <- unlist(lapply(parsed_evidence, `[[`, "indirect"), recursive = FALSE)
    evidence$non_simple <- unlist(lapply(parsed_evidence, `[[`, "non_simple"), recursive = FALSE)
    evidence$parse_errors <- unlist(lapply(parsed_evidence, `[[`, "parse_errors"),
                                    recursive = FALSE)
  }
  sysdata <- file.path(r_dir, "sysdata.rda")
  if (file.exists(sysdata) && !dir.exists(sysdata)) {
    environment <- new.env(parent = emptyenv())
    loaded <- tryCatch(load(sysdata, envir = environment), error = identity)
    if (inherits(loaded, "error")) {
      evidence$sysdata_error <- conditionMessage(loaded)
    } else {
      evidence$sysdata_names <- sort(unique(as.character(loaded)))
    }
  }
  evidence
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

.reexport_empty_table <- function() {
  data.frame(
    symbol = character(), package = character(), resolution = character(),
    candidates = character(), diagnosis = character(),
    stringsAsFactors = FALSE
  )
}

.reexport_prefer_literal <- function(symbol, package) {
  paste0(.r_symbol_literal(symbol), " = ", .r_string_literal(package))
}

.reexport_import_sources <- function(component, symbol, components) {
  imports <- .or_null(component$imports, list())
  sources <- list()
  if (length(imports) == 0L) return(sources)
  for (entry in imports) {
    if (is.character(entry) && length(entry) == 1L) {
      package <- entry[[1L]]
      if (package %in% vapply(components, `[[`, character(1L), "package")) {
        owner <- components[[match(
          package, vapply(components, `[[`, character(1L), "package")
        )]]
        if (symbol %in% .or_null(owner$exports, character())) {
          sources[[length(sources) + 1L]] <- list(
            package = package, kind = "full", external = FALSE
          )
        }
      } else {
        sources[[length(sources) + 1L]] <- list(
          package = package, kind = "full", external = TRUE
        )
      }
      next
    }
    if (is.list(entry) && length(entry) >= 2L) {
      package <- as.character(entry[[1L]])[[1L]]
      symbols <- as.character(entry[[2L]])
      if (symbol %in% symbols) {
        sources[[length(sources) + 1L]] <- list(
          package = package, kind = "from", external = !package %in%
            vapply(components, `[[`, character(1L), "package")
        )
      }
    }
  }
  if (length(sources) == 0L) return(sources)
  keys <- vapply(
    sources, function(source) {
      paste(source$package, source$kind, source$external, sep = "\u001f")
    }, character(1L)
  )
  sources[!duplicated(keys)]
}

.reexport_probe_failure <- function(reason, skipped = FALSE) {
  list(demonstrated = FALSE, skipped = isTRUE(skipped), reason = reason)
}

.reexport_relevant_blockers <- function(evidence, symbol) {
  mutations <- .or_null(evidence$mutations, list())
  calls <- .or_null(evidence$calls, list())
  dynamic <- .or_null(evidence$dynamic, list())
  indirect <- .or_null(evidence$indirect, list())
  non_simple <- .or_null(evidence$non_simple, list())
  binder_names <- c(
    "assign", "delayedAssign", "makeActiveBinding", "list2env", "sys.source",
    "assignInMyNamespace", "assignInNamespace", "source", "load", "attach",
    "setGeneric", "setClass", "setRefClass", "setValidity", "setGroupGeneric",
    "setMethod", "lockBinding", "unlockBinding"
  )
  relevant <- list()
  add <- function(items, predicate = function(item) TRUE) {
    if (length(items) == 0L) return(invisible(NULL))
    for (item in items) {
      active <- is.null(item$active) || isTRUE(item$active)
      if (active && predicate(item)) relevant[[length(relevant) + 1L]] <<- item
    }
    invisible(NULL)
  }
  add(mutations, function(item) {
    if (item$name %in% c("$", "[[", "@")) {
      return(is.null(item$symbol) || identical(item$symbol, symbol))
    }
    if (!item$name %in% binder_names) return(TRUE)
    matching <- Filter(function(call) {
      identical(call$name, item$name) &&
        identical(call$file, item$file) && identical(call$line, item$line)
    }, calls)
    if (length(matching) == 0L) return(TRUE)
    call <- matching[[1L]]
    !isTRUE(call$literal) || is.null(call$value) ||
      identical(call$value, symbol)
  })
  add(calls, function(item) {
    if (!item$name %in% binder_names) return(FALSE)
    if (item$name %in% c(
      "list2env", "sys.source", "source", "load", "attach", "lockBinding",
      "unlockBinding"
    )) return(TRUE)
    !isTRUE(item$literal) || is.null(item$value) || identical(item$value, symbol)
  })
  add(dynamic)
  add(indirect, function(item) {
    !isTRUE(item$followed) || isTRUE(item$foreign_namespace)
  })
  add(non_simple)
  if (length(relevant) == 0L) return(relevant)
  keys <- vapply(
    relevant,
    function(item) {
      paste(
        .or_null(item$name, "mutation"), .or_null(item$file, ""),
        .or_null(item$line, ""), sep = "\u001f"
      )
    },
    character(1L)
  )
  relevant[!duplicated(keys)]
}

.reexport_probe_reasons <- function(component, symbol, sources) {
  evidence <- .or_null(
    component$reexport_evidence, .reexport_empty_evidence()
  )
  reasons <- character()
  assignments <- Filter(
    function(item) identical(item$symbol, symbol),
    .or_null(evidence$assignments, list())
  )
  if (length(assignments) > 0L) {
    reasons <- c(reasons, vapply(assignments, function(item) {
      .bb_trf(
        "Local definition of re-export symbol '%s' in %s:%d.",
        symbol, item$file, item$line
      )
    }, character(1L)))
  }

  if (!is.null(evidence$sysdata_error)) {
    reasons <- c(reasons, .bb_trf(
      "Could not inspect R/sysdata.rda: %s", evidence$sysdata_error
    ))
  } else if (symbol %in% .or_null(evidence$sysdata_names, character())) {
    reasons <- c(reasons, .bb_trf(
      "R/sysdata.rda contains re-export symbol '%s'.", symbol
    ))
  }

  mutation_items <- if (length(sources) > 0L) {
    .reexport_relevant_blockers(evidence, symbol)
  } else {
    list()
  }
  if (length(mutation_items) > 0L) {
    keys <- vapply(mutation_items, function(item) {
      paste(
        .or_null(item$name, "mutation"), .or_null(item$file, ""),
        .or_null(item$line, ""), sep = "\u001f"
      )
    }, character(1L))
    mutation_items <- mutation_items[!duplicated(keys)]
    reasons <- c(reasons, vapply(mutation_items, function(item) {
      if (!is.null(item$reason) && length(item$reason) == 1L &&
            !is.na(item$reason)) return(item$reason)
      .bb_trf(
        "Binder or namespace mutation via %s in %s:%d; it is not known whether it changes '%s'.",
        .or_null(item$name, "mutation"), item$file, item$line, symbol
      )
    }, character(1L)))
  }

  if (isTRUE(evidence$native)) {
    reasons <- c(reasons, .bb_tr(
      "NAMESPACE declares useDynLib(), so the exported object's origin is undetermined."
    ))
  }

  if (length(sources) == 0L) {
    reasons <- c(reasons, .bb_trf(
      "NAMESPACE does not declare an import source for '%s'.", symbol
    ))
  } else {
    reasons <- c(reasons, vapply(sources, function(source) {
      if (identical(source$kind, "full")) {
        .bb_trf(
          "NAMESPACE imports complete '%s', which could provide '%s'.",
          source$package, symbol
        )
      } else if (isTRUE(source$external)) {
        .bb_trf(
          "NAMESPACE imports '%s' from external package '%s'.",
          symbol, source$package
        )
      } else {
        .bb_trf(
          "NAMESPACE imports '%s' from component '%s'.",
          symbol, source$package
        )
      }
    }, character(1L)))
  }

  parse_errors <- .or_null(evidence$parse_errors, list())
  if (length(parse_errors) > 0L) {
    reasons <- c(reasons, vapply(parse_errors, function(item) {
      .bb_trf("Could not parse R file %s: %s", item$file, item$error)
    }, character(1L)))
  }
  unique(reasons)
}

.reexport_probe <- function(component, symbol, components, omitted_names,
                            trail = character()) {
  omitted_components <- if (is.data.frame(omitted_names) &&
                              "component" %in% names(omitted_names)) {
    unique(omitted_names$component[nzchar(omitted_names$component)])
  } else {
    omitted_names
  }
  package <- component$package
  key <- paste(package, symbol, sep = "::")
  if (key %in% trail) {
    return(.reexport_probe_failure(
      .bb_trf("Re-export proof cycle while following '%s'.", symbol)
    ))
  }

  sources <- .reexport_import_sources(component, symbol, components)
  evidence <- .or_null(
    component$reexport_evidence, .reexport_empty_evidence()
  )
  reasons <- .reexport_probe_reasons(component, symbol, sources)
  root_component <- length(sources) == 0L
  local_definition <- any(vapply(
    .or_null(evidence$assignments, list()),
    function(item) identical(item$symbol, symbol), logical(1L)
  ))
  has_blocker <- isTRUE(evidence$native) ||
    length(.or_null(evidence$parse_errors, list())) > 0L ||
    !is.null(evidence$sysdata_error) ||
    symbol %in% .or_null(evidence$sysdata_names, character()) ||
    (!root_component && length(.reexport_relevant_blockers(evidence, symbol)) > 0L) ||
    (length(sources) > 0L && local_definition)
  if (has_blocker) {
    return(.reexport_probe_failure(paste(reasons, collapse = "; ")))
  }
  if (length(sources) == 0L) {
    return(list(
      demonstrated = TRUE, skipped = FALSE, root_type = "component",
      root = package, reason = paste(reasons, collapse = "; "),
      local_definition = local_definition
    ))
  }
  if (length(sources) != 1L) {
    return(.reexport_probe_failure(paste(reasons, collapse = "; ")))
  }
  source <- sources[[1L]]
  if (identical(source$kind, "full") && isTRUE(source$external)) {
    return(.reexport_probe_failure(paste(reasons, collapse = "; ")))
  }
  if (source$package %in% omitted_components) {
    reason <- if (is.data.frame(omitted_names) &&
                    "reason" %in% names(omitted_names)) {
      omitted_names$reason[match(source$package, omitted_names$component)]
    } else {
      .or_null(names(omitted_names), source$package)
    }
    return(.reexport_probe_failure(.bb_trf(
      "NAMESPACE imports '%s' from component '%s', which was skipped: %s",
      symbol, source$package,
      .or_null(reason, source$package)
    ), skipped = TRUE))
  }
  external_source <- identical(source$kind, "from") && isTRUE(source$external)
  if (!external_source) {
    index <- match(
      source$package, vapply(components, `[[`, character(1L), "package")
    )
    if (is.na(index)) {
      return(.reexport_probe_failure(.bb_trf(
        "NAMESPACE imports '%s' from component '%s', which is not in this generation.",
        symbol, source$package
      ), skipped = TRUE))
    }
    parent <- components[[index]]
    if (!symbol %in% .or_null(parent$exports, character())) {
      return(.reexport_probe_failure(.bb_trf(
        "NAMESPACE imports '%s' from component '%s', but that component does not export it.",
        symbol, source$package
      )))
    }
  }

  if (external_source) {
    return(list(
      demonstrated = TRUE, skipped = FALSE, root_type = "external",
      root = source$package, reason = NULL
    ))
  }

  parent_probe <- .reexport_probe(
    parent, symbol, components, omitted_names, c(trail, key)
  )
  if (!isTRUE(parent_probe$demonstrated)) {
    parent_reason <- .or_null(
      parent_probe$reason,
      .bb_trf("Proof for component '%s' is inconclusive.", source$package)
    )
    return(.reexport_probe_failure(.bb_trf(
      "NAMESPACE imports '%s' from component '%s'; proof for that component is inconclusive: %s",
      symbol, source$package, parent_reason
    ), skipped = isTRUE(parent_probe$skipped)))
  }
  parent_probe
}

.reexport_collision_choices <- function(symbol, candidates) {
  preferred <- vapply(
    candidates, function(package) .reexport_prefer_literal(symbol, package),
    character(1L)
  )
  paste0(
    "reexport_prefer = ", paste(preferred, collapse = " or "),
    "; reexport_exclude = ", .r_symbol_literal(symbol)
  )
}

.resolve_reexport_plan <- function(components, prefer = character(),
                                   exclude = character(), omitted = NULL,
                                   metapackage_name = NULL) {
  component_names <- vapply(components, `[[`, character(1L), "package")
  exports_by_component <- lapply(components, function(component) {
    unique(.or_null(component$exports, character()))
  })
  names(exports_by_component) <- component_names
  exported_symbols <- sort(unique(unlist(exports_by_component, use.names = FALSE)))
  prefer_symbols <- names(prefer)
  omitted_names <- if (is.null(omitted)) {
    character()
  } else {
    unique(omitted$component[nzchar(omitted$component)])
  }
  omitted_prefer <- prefer_symbols[unname(prefer[prefer_symbols]) %in% omitted_names]
  if (length(omitted_prefer) > 0L) {
    skipped_table <- data.frame(
      symbol = omitted_prefer,
      components = unname(prefer[omitted_prefer]),
      reason = vapply(unname(prefer[omitted_prefer]), function(package) {
        reasons <- omitted$reason[omitted$component == package]
        if (length(reasons) == 0L) {
          .bb_trf("Component '%s' was omitted by skip.", package)
        } else {
          paste(reasons, collapse = "; ")
        }
      }, character(1L)), stringsAsFactors = FALSE
    )
    .bigbang_abort(
      "bigbang_error_reexport_skipped",
      .bb_trf(
        paste0(
          "Cannot use reexport_prefer for omitted component(s): %s. The ",
          "component was omitted by skip; repair it or choose an installed ",
          "candidate."
        ),
        paste(paste0(skipped_table$symbol, " = ", skipped_table$components,
                     " (", skipped_table$reason, ")"), collapse = "; ")
      ),
      symbols = skipped_table$symbol, components = skipped_table$components,
      skipped = skipped_table
    )
  }
  requested <- unique(c(prefer_symbols, exclude))
  unknown <- setdiff(requested, exported_symbols)
  if (length(unknown) > 0L) {
    .bigbang_abort(
      "bigbang_error_reexport_unknown",
      .bb_trf(
        "Unknown re-export symbol(s): %s. No component exports them.",
        paste(unknown, collapse = ", ")
      ),
      unknown = unknown
    )
  }
  overlap <- intersect(prefer_symbols, exclude)
  if (length(overlap) > 0L) {
    .bigbang_abort(
      "bigbang_error_reexport_overlap",
      .bb_trf(
        "Re-export symbol(s) cannot be both preferred and excluded: %s.",
        paste(overlap, collapse = ", ")
      ),
      symbols = overlap
    )
  }
  invalid_prefer <- prefer_symbols[
    vapply(prefer_symbols, function(symbol) {
      !unname(prefer[[symbol]]) %in% component_names ||
        !(symbol %in% exports_by_component[[unname(prefer[[symbol]])]])
    }, logical(1L))
  ]
  if (length(invalid_prefer) > 0L) {
    details <- vapply(invalid_prefer, function(symbol) {
      paste0(symbol, " = ", unname(prefer[[symbol]]))
    }, character(1L))
    .bigbang_abort(
      "bigbang_error_reexport_prefer_component",
      .bb_trf(
        paste0(
          "Preferred re-export target(s) are invalid: %s. Each component ",
          "must be in the generation and export its symbol."
        ),
        paste(details, collapse = ", ")
      ),
      symbols = invalid_prefer,
      components = unname(prefer[invalid_prefer])
    )
  }

  active_symbols <- setdiff(exported_symbols, exclude)
  own_symbols <- if (is.null(metapackage_name)) {
    character()
  } else {
    intersect(active_symbols, .generated_metapackage_symbols(metapackage_name))
  }
  if (length(own_symbols) > 0L) {
    own_candidates <- vapply(own_symbols, function(symbol) {
      paste(component_names[vapply(
        exports_by_component, function(exports) symbol %in% exports, logical(1L)
      )], collapse = ", ")
    }, character(1L))
    own_table <- data.frame(
      symbol = own_symbols, candidates = own_candidates,
      reason = "generated metapackage symbol", stringsAsFactors = FALSE
    )
    syntax_symbols <- intersect(own_symbols, .r_syntax_symbols())
    if (length(syntax_symbols) > 0L) {
      .bigbang_abort(
        "bigbang_error_reexport_collision",
        .bb_trf(
          paste0(
            "Cannot re-export R syntax symbol(s) %s: generated code must retain ",
            "R syntax and cannot be shadowed. Use reexport_exclude = %s."
          ),
          paste(syntax_symbols, collapse = ", "),
          paste(vapply(syntax_symbols, .r_symbol_literal, character(1L)),
                collapse = ", ")
        ),
        symbols = syntax_symbols, components = metapackage_name,
        collisions = own_table, data = own_table
      )
    }
    generated_symbols <- setdiff(own_symbols, syntax_symbols)
    .bigbang_abort(
      "bigbang_error_reexport_collision",
      .bb_trf(
        "Generated metapackage symbol(s) %s can only be resolved with reexport_exclude: %s.",
        paste(generated_symbols, collapse = ", "),
        paste(vapply(generated_symbols, .r_symbol_literal, character(1L)),
              collapse = ", ")
      ),
      symbols = generated_symbols, components = metapackage_name,
      collisions = own_table, data = own_table
    )
  }

  sorted_components <- components[order(component_names)]
  installation_order <- .component_topological_order(sorted_components)
  owner_rank <- match(component_names, installation_order)

  # Export inventory and omitted-component validation are independent from
  # source proof. Parse only owners that participate in a collision, plus the
  # import chain needed to establish their roots.
  collision_symbols <- exported_symbols[vapply(exported_symbols, function(symbol) {
    sum(vapply(exports_by_component, function(exports) symbol %in% exports,
               logical(1L))) > 1L
  }, logical(1L))]
  needed <- character()
  if (length(collision_symbols) > 0L) {
    needed <- unique(unlist(lapply(collision_symbols, function(symbol) {
      component_names[vapply(
        exports_by_component, function(exports) symbol %in% exports, logical(1L)
      )]
    }), use.names = FALSE))
    repeat {
      sources <- unlist(lapply(components[component_names %in% needed], function(component) {
        unlist(lapply(collision_symbols, function(symbol) {
          .reexport_import_sources(component, symbol, components)
        }), recursive = FALSE)
      }), recursive = FALSE)
      parents <- if (length(sources) == 0L) character() else vapply(
        sources, `[[`, character(1L), "package"
      )
      new_needed <- setdiff(parents, needed)
      if (length(new_needed) == 0L) break
      needed <- c(needed, new_needed)
    }
  }
  if (length(needed) > 0L) {
    for (index in which(component_names %in% needed)) {
      evidence_loaded <- components[[index]]$reexport_evidence_loaded
      if (is.null(evidence_loaded)) {
        # Test and extension callers may provide an evidence object directly
        # without the archive metadata fields used by the normal resolver.
        components[[index]]$reexport_evidence_loaded <- TRUE
      } else if (!isTRUE(evidence_loaded)) {
        components[[index]]$reexport_evidence <-
          .read_reexport_evidence(components[[index]])
        components[[index]]$reexport_evidence_loaded <- TRUE
      }
    }
  }
  rows <- list()
  specs <- list()
  collisions <- list()
  skipped <- list()
  for (symbol in active_symbols) {
    owners <- component_names[vapply(
      exports_by_component, function(exports) symbol %in% exports, logical(1L)
    )]
    owners <- owners[order(owner_rank[match(owners, component_names)])]
    if (length(owners) == 1L) {
      unique_probe <- if (nrow(omitted) > 0L) {
        .reexport_probe(
          components[[match(owners[[1L]], component_names)]], symbol,
          components, omitted
        )
      } else {
        list(demonstrated = TRUE, skipped = FALSE)
      }
      if (isTRUE(unique_probe$skipped)) {
        skipped[[length(skipped) + 1L]] <- data.frame(
          symbol = symbol, components = owners[[1L]],
          reason = unique_probe$reason, stringsAsFactors = FALSE
        )
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(
        symbol = symbol, package = owners[[1L]], resolution = "unique",
        candidates = owners[[1L]], diagnosis = NA_character_,
        stringsAsFactors = FALSE
      )
      specs[[length(specs) + 1L]] <- list(
        symbol = symbol, package = owners[[1L]], resolution = "unique",
        candidates = owners, diagnosis = NA_character_
      )
      next
    }
    probes <- lapply(owners, function(owner) {
      .reexport_probe(
        components[[match(owner, component_names)]], symbol, components,
        omitted
      )
    })
    if (any(vapply(probes, `[[`, logical(1L), "skipped"))) {
      skipped[[length(skipped) + 1L]] <- data.frame(
        symbol = symbol, components = paste(owners, collapse = ", "),
        reason = paste(vapply(seq_along(probes), function(index) {
          paste0(owners[[index]], ": ", probes[[index]]$reason)
        }, character(1L)), collapse = "; "), stringsAsFactors = FALSE
      )
      next
    }
    demonstrated <- vapply(probes, `[[`, logical(1L), "demonstrated")
    roots <- if (all(demonstrated)) {
      paste(vapply(probes, `[[`, character(1L), "root_type"),
            vapply(probes, `[[`, character(1L), "root"), sep = ":")
    } else {
      character()
    }
    local_definition <- vapply(owners, function(owner) {
      evidence <- .or_null(
        components[[match(owner, component_names)]]$reexport_evidence,
        .reexport_empty_evidence()
      )
      any(vapply(.or_null(evidence$assignments, list()),
                 function(item) identical(item$symbol, symbol), logical(1L)))
    }, logical(1L))
    diagnosis <- if (all(demonstrated) && length(unique(roots)) == 1L) {
      "probable_same_object"
    } else if (sum(local_definition) >= 2L) {
      "distinct_definitions"
    } else {
      "undetermined"
    }
    reason <- vapply(seq_along(probes), function(index) {
      probe <- probes[[index]]
      detail <- if (isTRUE(probe$demonstrated)) {
        .bb_trf("root %s %s", probe$root_type, probe$root)
      } else {
        probe$reason
      }
      paste0(owners[[index]], ": ", detail)
    }, character(1L))
    if (symbol %in% prefer_symbols) {
      selected <- unname(prefer[[symbol]])
      rows[[length(rows) + 1L]] <- data.frame(
        symbol = symbol, package = selected, resolution = "preferred",
        candidates = paste(owners, collapse = ", "), diagnosis = diagnosis,
        stringsAsFactors = FALSE
      )
      specs[[length(specs) + 1L]] <- list(
        symbol = symbol, package = selected, resolution = "preferred",
        candidates = owners, diagnosis = diagnosis
      )
    } else {
      collisions[[length(collisions) + 1L]] <- data.frame(
        symbol = symbol, candidates = paste(owners, collapse = ", "),
        reason = paste(reason, collapse = "; "), diagnosis = diagnosis,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(skipped) > 0L) {
    skipped_table <- do.call(rbind, skipped)
    .bigbang_abort(
      "bigbang_error_reexport_skipped",
      .bb_trf(
        "Cannot resolve re-export symbol(s) because skipped components are required: %s.",
        paste(paste0(skipped_table$symbol, " (", skipped_table$reason, ")"),
              collapse = "; ")
      ),
      symbols = skipped_table$symbol, components = skipped_table$components,
      skipped = skipped_table
    )
  }
  if (length(collisions) > 0L) {
    collision_table <- do.call(rbind, collisions)
    probable <- collision_table$diagnosis == "probable_same_object"
    probable_choices <- if (any(probable)) {
      paste(vapply(which(probable), function(index) {
        candidates <- strsplit(collision_table$candidates[[index]], ", ", fixed = TRUE)[[1L]]
        .reexport_prefer_literal(collision_table$symbol[[index]], candidates[[1L]])
      }, character(1L)), collapse = ", ")
    } else {
      ""
    }
    other_choices <- if (any(!probable)) {
      paste(vapply(which(!probable), function(index) {
        paste0(collision_table$symbol[[index]], " (",
               collision_table$diagnosis[[index]], "): ",
               collision_table$candidates[[index]])
      }, character(1L)), collapse = "; ")
    } else {
      ""
    }
    resolution_text <- paste(c(
      if (nzchar(probable_choices)) .bb_trf(
        "For probable_same_object collisions, copy this: reexport_prefer = c(%s)",
        probable_choices
      ) else NULL,
      if (nzchar(other_choices)) .bb_trf(
        "For distinct_definitions and undetermined collisions, decide among these candidates: %s",
        other_choices
      ) else NULL
    ), collapse = "; ")
    .bigbang_abort(
      "bigbang_error_reexport_collision",
      .bb_trf(
        "Cannot resolve re-export collision(s): %s. Candidates, diagnoses, and proof details: %s. %s.",
        paste(collision_table$symbol, collapse = ", "),
        paste(paste0(collision_table$symbol, " (", collision_table$candidates,
                     "; ", collision_table$diagnosis, "): ", collision_table$reason),
              collapse = "; "),
        resolution_text
      ),
      symbols = collision_table$symbol,
      components = collision_table$candidates,
      collisions = collision_table, data = collision_table
    )
  }
  table <- if (length(rows) == 0L) {
    .reexport_empty_table()
  } else {
    result <- do.call(rbind, rows)
    rownames(result) <- NULL
    result
  }
  names(specs) <- vapply(specs, `[[`, character(1L), "symbol")
  list(
    table = table, specs = specs, excluded = sort(unique(exclude)),
    installation_order = installation_order
  )
}

.validate_generation <- function(
  resolved, tolerate = character(), on_component_error = "abort",
  reexport = FALSE, metapackage_name = NULL,
  reexport_prefer = character(), reexport_exclude = character()
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
  reexport_plan <- if (isTRUE(reexport)) {
    .resolve_reexport_plan(
      resolved$components, prefer = reexport_prefer,
      exclude = reexport_exclude, omitted = omitted,
      metapackage_name = metapackage_name
    )
  } else {
    list(
      table = .reexport_empty_table(), specs = list(), excluded = character(),
      installation_order = .component_topological_order(resolved$components)
    )
  }
  list(
    resolved = resolved, validation = validation, omitted = omitted,
    reexport = reexport_plan
  )
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

    r_files <- list.files(r_dir, pattern = "\\.(R|r|S|s|q)$", full.names = TRUE)

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

      r_files <- list.files(r_dir, pattern = "\\.(R|r|S|s|q)$", full.names = TRUE)

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
