#' @param package Character archive stem.
#' @param pkg_dir Character archive directory.
#' @param ext Character archive extension.
#'
#' @return A character vector of dependency names.
#' @noRd

extract_dependencies <- function(package, pkg_dir, ext = ".tar.gz") {
  archive <- file.path(pkg_dir, paste0(package, ext))

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Extract the archive into a directory owned by this invocation.
  if (ext == ".tar.gz" || ext == ".tar") {
    utils::untar(archive, exdir = temp_dir)
  } else if (ext == ".zip") {
    utils::unzip(archive, exdir = temp_dir)
  } else {
    stop(.bb_trf("Unsupported archive extension: %s", ext), call. = FALSE)
  }

  desc_file <- list.files(temp_dir, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE)

  if (length(desc_file) == 0) {
    stop(.bb_trf("No DESCRIPTION file found in package %s", package), call. = FALSE)
  }

  desc <- read.dcf(desc_file)

  # Read the dependency-bearing DESCRIPTION fields.
  fields <- c("Depends", "Imports", "LinkingTo")
  dependencies <- unlist(lapply(fields, function(field) {
    if (field %in% colnames(desc)) {
      unlist(strsplit(desc[, field], split = ","))
    } else {
      NULL
    }
  }))

  dependencies <- gsub("\\s*\\(.*\\)", "", dependencies)
  dependencies <- gsub("\\s+", "", dependencies)

  # Add recommended packages when their APIs appear in source code.
  r_files <- list.files(file.path(temp_dir, sub("_.*", "", package), "R"),
                        pattern = "\\.[Rr]$", full.names = TRUE)

  for (file in r_files) {
    content <- readLines(file, warn = FALSE)
    if (any(grepl("setClass|setGeneric|setMethod|setValidity|representation|prototype", content)) ||
          any(grepl("sparseMatrix|dgCMatrix|dsCMatrix|Matrix\\(", content))) {
      dependencies <- c(dependencies, "Matrix")
    }
    if (any(grepl("knn|LDA|QDA|class::", content))) {
      dependencies <- c(dependencies, "class")
    }
  }

  unique(dependencies[dependencies != "R"])
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
#' \dontrun{
#' res <- diagnose_dependencies(
#'   packages = c("conexiones_0.8.3", "utiles_1.4"),
#'   pkg_dir = "path/to/local/archives"
#' )
#' res[["conexiones_0.8.3"]]
#' lapply(res, function(x) x$matrix_refs)
#' }
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

#' Deprecated Spanish alias for `diagnose_dependencies()`
#'
#' @param paquetes_locales Character vector of package archive stems.
#' @param ruta_instalables Directory containing the archives.
#' @param ext Archive extension.
#' @return The result of [diagnose_dependencies()].
#' @keywords internal
#' @export
diagnosticar_dependencias <- function(paquetes_locales, ruta_instalables,
                                      ext = ".tar.gz") {
  if (isTRUE(getOption("bigbang.deprecation_warnings", interactive()))) {
    .Deprecated("diagnose_dependencies", package = "bigbang")
  }
  diagnose_dependencies(paquetes_locales, ruta_instalables, ext)
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

    # Data manipulation
    "data.table" = "\\bdata\\.table\\b|\\bdt\\[|\\bsetkey\\b|\\bfread\\b|\\bfwrite\\b",
    "dplyr" = "\\bfilter\\b|\\barrange\\b|\\bselect\\b|\\bmutate\\b|\\bgroup_by\\b|\\bsummarise\\b",
    "tidyr" = "\\bgather\\b|\\bspread\\b|\\bseparate\\b|\\bunite\\b|\\bpivot_longer\\b|\\bpivot_wider\\b",

    # Time series
    "zoo" = "\\bzoo\\b|\\bindex\\b|\\bcoredata\\b|\\brollapply\\b",
    "xts" = "\\bxts\\b|\\bindexClass\\b|\\bperiodicity\\b",

    # Spatial statistics
    "sp" = "\\bSpatialPoints\\b|\\bSpatialPolygons\\b|\\bover\\b|\\bspplot\\b",
    "sf" = "\\bst_\\b|\\bsf::st_\\b|\\bsf_\\b",

    # Other commonly used packages
    "tibble" = "\\btibble\\b|as_tibble|\\btbl_|tibble::",
    "readr" = "\\bread_csv\\b|\\bwrite_csv\\b|\\bread_delim\\b|readr::",
    "jsonlite" = "\\bfromJSON\\b|\\btoJSON\\b|jsonlite::",
    "data.table" = "\\bdata\\.table\\b|\\bsetkey\\b|\\bfread\\b|:=",
    "ggplot2" = "\\bggplot\\b|\\baes\\b|\\bgeom_\\w+\\b|\\bfacet_\\w+\\b",
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

      # Scan all R source as one text value.
      content <- paste(unlist(lapply(r_files, readLines, warn = FALSE)), collapse = " ")

      for (pkg_name in names(package_patterns)) {
        pattern <- package_patterns[[pkg_name]]
        if (grepl(pattern, content)) {
          possible_deps <- c(possible_deps, pkg_name)
        }
      }
    }, error = function(e) {
      warning(.bb_trf("Error processing package %s: %s", package, e$message), call. = FALSE)
    })
  }

  sort(unique(possible_deps))
}
