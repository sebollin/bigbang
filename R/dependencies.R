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
  dir.create(temp_dir)
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  if (ext %in% c(".tar.gz", ".tar")) {
    utils::untar(archive, exdir = temp_dir)
  } else if (identical(tolower(ext), ".zip")) {
    utils::unzip(archive, exdir = temp_dir)
  } else {
    stop(.bb_trf("Unsupported archive extension: %s", ext), call. = FALSE)
  }

  desc_file <- list.files(
    temp_dir, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE
  )
  if (length(desc_file) != 1L) {
    stop(.bb_trf(
      "Expected one DESCRIPTION in %s; found %d.", archive, length(desc_file)
    ), call. = FALSE)
  }

  desc <- read.dcf(
    desc_file,
    fields = c("Package", "Version", "Depends", "Imports", "LinkingTo")
  )
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

  dependencies <- unlist(lapply(c("Depends", "Imports", "LinkingTo"), function(name) {
    value <- field(name)
    if (is.na(value) || !nzchar(value)) return(character())
    trimws(gsub("\\s*\\([^)]*\\)", "", strsplit(value, ",", fixed = TRUE)[[1L]]))
  }), use.names = FALSE)

  list(
    package = declared_package,
    version = declared_version,
    dependencies = unique(dependencies[nzchar(dependencies) & dependencies != "R"])
  )
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

.strip_r_comments_and_strings <- function(lines) {
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
  content <- paste(lines, collapse = "\n")
  content <- gsub("(?m)#[^\\n]*$", "", content, perl = TRUE)
  gsub("'(?:\\\\.|[^'\\\\])*'|\"(?:\\\\.|[^\"\\\\])*\"", "", content, perl = TRUE)
}

#' Classify local and repository dependencies
#'
#' @param dependencies Character dependency names.
#' @param pkg_dir Character archive directory.
#' @param ext Character archive extension.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{local}{Dependencies available as local archives.}
#'   \item{cran}{Dependencies expected from a configured repository.}
#' }
#' @noRd
classify_dependencies <- function(dependencies, pkg_dir, ext = ".tar.gz") {
  archives <- list.files(pkg_dir)
  archives <- archives[endsWith(tolower(archives), tolower(ext))]
  stems <- substr(archives, 1L, nchar(archives) - nchar(ext))
  local_names <- unique(c(stems, sub("_.*", "", stems)))
  is_local <- dependencies %in% local_names

  list(
    local = unique(dependencies[is_local]),
    cran = unique(dependencies[!is_local])
  )
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

    if (ext == ".tar.gz" || ext == ".tar") {
      utils::untar(archive, exdir = temp_dir)
    } else if (ext == ".zip") {
      utils::unzip(archive, exdir = temp_dir)
    }

    # Locate references to Matrix and class APIs.
    base_name <- sub("_.*", "", package)
    r_dir <- file.path(temp_dir, base_name, "R")

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
    "Matrix" = paste0(
      "sparse|[dstz][gsd]Matrix|Matrix\\.|setClass|setGeneric|setMethod|",
      "setValidity|representation|prototype|new\\("
    ),

    # Statistical analysis
    "class" = "\\bknn\\b|\\bLDA\\b|\\bQDA\\b|\\bnaiveBayes\\b",
    "MASS" = "\\blda\\b|\\bqda\\b|\\bridgeReg\\b|\\blogistic\\b|\\bboxcox\\b|\\bVIF\\b",
    "cluster" = "\\bkmeans\\b|\\bpam\\b|\\bclara\\b|\\bfanny\\b|\\bsilhouette\\b",

    # Graphics
    "lattice" = "\\bxyplot\\b|\\bbwplot\\b|\\bcontourplot\\b|\\blevelplot\\b|\\bwireframe\\b",
    "grid" = "\\bgrid\\.arrange\\b|\\bgpar\\b|\\bgrobTree\\b|\\bviewport\\b|\\bgrid\\.layout\\b",

    # Data manipulation. Common base names such as `filter` and `select` are
    # not sufficient evidence: require a namespace qualifier or a pipe.
    "data.table" = "\\bdata\\.table\\s*\\(|\\bdt\\[|\\bsetkey\\s*\\(|\\bfread\\s*\\(|\\bfwrite\\s*\\(",
    "dplyr" = "(?:\\bdplyr\\s*::\\s*|%>%\\s*|\\|>\\s*)\\b(?:filter|arrange|select|mutate|group_by|summarise)\\s*\\(",
    "tidyr" = "\\bgather\\b|\\bspread\\b|\\bseparate\\b|\\bunite\\b|\\bpivot_longer\\b|\\bpivot_wider\\b",

    # Time series
    "zoo" = "\\bzoo\\s*\\(|\\bzoo\\s*::|\\bcoredata\\s*\\(|\\brollapply\\s*\\(",
    "xts" = "\\bxts\\b|\\bindexClass\\b|\\bperiodicity\\b",

    # Spatial statistics
    "sp" = "\\b(?:sp\\s*::\\s*)?(?:SpatialPoints|SpatialPolygons|spplot)\\s*\\(|\\bsp\\s*::\\s*over\\s*\\(",
    "sf" = "\\bst_\\b|\\bsf::st_\\b|\\bsf_\\b",

    # Other commonly used packages
    "tibble" = "\\btibble\\b|as_tibble|\\btbl_|tibble::",
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
      if (ext == ".tar.gz" || ext == ".tar") {
        utils::untar(archive, exdir = temp_dir)
      } else if (ext == ".zip") {
        utils::unzip(archive, exdir = temp_dir)
      }

      base_name <- sub("_.*", "", package)
      r_dir <- file.path(temp_dir, base_name, "R")

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
        function(file) .strip_r_comments_and_strings(readLines(file, warn = FALSE)),
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
