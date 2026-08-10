#' @param package Character archive stem.
#' @param pkg_dir Character archive directory.
#' @param ext Character archive extension.
#'
#' Extract one archive and read its DESCRIPTION metadata.
#'
#' The returned metadata is deliberately limited to facts declared by the
#' component itself. Source-code heuristics belong to
#' [detect_implicit_dependencies()] and must never silently become hard
#' dependencies of a generated package.
#'
#' @param package Character archive stem.
#' @param pkg_dir Character archive directory.
#' @param ext Character archive extension.
#' @return A list containing `package`, `version`, and declared `dependencies`.
#' @noRd
.read_archive_metadata <- function(package, pkg_dir, ext = ".tar.gz") {
  archive <- file.path(pkg_dir, paste0(package, ext))
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

  list(
    package = declared_package,
    version = declared_version,
    dependencies = parsed_dependencies$dependencies,
    constraints = parsed_dependencies$constraints
  )
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
      utils::untar(archive, list = TRUE)
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
      utils::untar(archive, exdir = extract_dir)
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

.validate_archive_metadata <- function(archive_stem, metadata, ext) {
  expected_name <- sub("_.*", "", archive_stem)
  expected_version <- sub("^[^_]+_", "", archive_stem)
  archive <- paste0(archive_stem, ext)
  if (!identical(metadata$package, expected_name)) {
    .bigbang_abort(
      "bigbang_error_archive_metadata",
      .bb_trf(
        "Archive %s declares package %s, but its filename names %s.",
        archive, metadata$package, expected_name
      ),
      archive = archive,
      declared_package = metadata$package,
      filename_package = expected_name
    )
  }
  if (!.version_matches(metadata$version, expected_version)) {
    .bigbang_abort(
      "bigbang_error_archive_metadata",
      .bb_trf(
        "Archive %s declares version %s, but its filename names version %s.",
        archive, metadata$version, expected_version
      ),
      archive = archive,
      declared_version = metadata$version,
      filename_version = expected_version
    )
  }
  invisible(metadata)
}

.component_dependency_cycle <- function(archive_stems, metadata) {
  names_only <- sub("_.*", "", archive_stems)
  adjacency <- lapply(metadata, function(item) {
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

.validate_constraints <- function(archive_stems, metadata) {
  included <- sub("_.*", "", archive_stems)
  versions <- vapply(metadata, `[[`, character(1L), "version")
  names(versions) <- included
  for (index in seq_along(metadata)) {
    constraints <- metadata[[index]]$constraints
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

.archive_inventory <- function(pkg_dir, ext) {
  archives <- list.files(pkg_dir)
  archives <- archives[endsWith(tolower(archives), tolower(ext))]
  stems <- substr(archives, 1L, nchar(archives) - nchar(ext))
  list(files = archives, stems = stems, packages = sub("_.*", "", stems))
}

.validate_unincluded_deps <- function(archive_stems, metadata, pkg_dir, ext) {
  included <- sub("_.*", "", archive_stems)
  inventory <- .archive_inventory(pkg_dir, ext)
  for (item in metadata) {
    candidates <- setdiff(item$dependencies, included)
    for (dependency in candidates) {
      match <- which(inventory$packages == dependency)
      if (length(match) > 0L) {
        .bigbang_abort(
          "bigbang_error_unincluded_dependency",
          .bb_trf(
            paste0(
              "Component %s declares dependency %s, available as %s in ",
              "pkg_dir but not included. Add it to packages or remove the ",
              "dependency."
            ),
            item$package, dependency, inventory$files[[match[[1L]]]]
          ),
          component = item$package,
          dependency = dependency,
          archive = inventory$files[[match[[1L]]]]
        )
      }
    }
  }
  invisible(metadata)
}

classify_dependencies <- function(dependencies, pkg_dir, ext = ".tar.gz",
                                  included_packages = NULL) {
  local_names <- if (is.null(included_packages)) {
    inventory <- .archive_inventory(pkg_dir, ext)
    unique(c(inventory$stems, inventory$packages))
  } else {
    sub("_.*", "", included_packages)
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

.validate_component_archives <- function(archive_stems, pkg_dir, ext) {
  names_only <- sub("_.*", "", archive_stems)
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

  metadata <- lapply(archive_stems, .read_archive_metadata, pkg_dir, ext)
  for (index in seq_along(archive_stems)) {
    .validate_archive_metadata(archive_stems[[index]], metadata[[index]], ext)
  }
  .validate_constraints(archive_stems, metadata)
  .validate_unincluded_deps(archive_stems, metadata, pkg_dir, ext)
  cycle <- .component_dependency_cycle(archive_stems, metadata)
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
  metadata
}

#' @return A character vector of dependency names declared in DESCRIPTION.
#' @noRd
extract_dependencies <- function(package, pkg_dir, ext = ".tar.gz") {
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
#' @param packages Character vector. Names (with version) of the local
#'   packages to examine, e.g. `"conexiones_0.8.3"`.
#' @param pkg_dir Character. Directory containing the local archive files
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
diagnose_dependencies <- function(packages, pkg_dir, ext = ".tar.gz") {
  results <- list()

  for (package in packages) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

    archive <- file.path(pkg_dir, paste0(package, ext))
    if (!file.exists(archive)) {
      message(.bb_trf("Package archive not found: %s", archive))
      next
    }

    # This is an exported entry point, so it gets the same extraction guards as
    # generation. Without them a component carrying a symbolic link made the
    # scanner read a file outside the archive and return its contents in the
    # result, which is how an unrelated file ends up in a diagnostic report.
    .extract_archive_checked(archive, ext, temp_dir)

    # Locate references to Matrix and class APIs from the archive root rather
    # than assuming that the root directory has the package name.
    package_root <- .find_archive_root(temp_dir, archive)
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
#' @param packages Character archive stems including versions.
#' @param pkg_dir Character archive directory.
#' @param ext Character archive extension.
#' @return A sorted character vector of possible dependency names.
#' @noRd
detect_implicit_dependencies <- function(packages, pkg_dir, ext = ".tar.gz") {
  possible_deps <- character(0)

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

  for (package in packages) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

    archive <- file.path(pkg_dir, paste0(package, ext))
    if (!file.exists(archive)) {
      warning(.bb_trf("Package archive not found: %s", archive), call. = FALSE)
      next
    }

    tryCatch({
      # Generation validates every component archive before reaching this
      # scanner, so a hostile archive cannot get here today. Extract through the
      # guarded path anyway: this is a helper that could be called from
      # somewhere else later, and the guard costs nothing.
      .extract_archive_checked(archive, ext, temp_dir)

      package_root <- .find_archive_root(temp_dir, archive)
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
