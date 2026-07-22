#' Scan a generated metapackage for historical deletion signatures
#'
#' Inspects a generated metapackage source directory, source archive, or installed
#' package without loading it. The scanner looks for the historical V1, V2, V3,
#' and V7 deletion signatures from the pre-release security investigation.
#' Provenance is read from both the current generator fields and the legacy
#' pre-rename fields so development artifacts remain classifiable.
#'
#' Installed packages are inspected through R's internal lazy-load database API.
#' That code is isolated in .scan_installed_lazydb() and has been exercised with
#' R 4.6.1. Because this is an internal R format, callers should re-run the scanner
#' tests when adopting a new R minor release.
#'
#' @param path Character scalar. Source directory, .tar.gz/.tar/.zip
#'   source archive, or installed package directory.
#' @param dry_run Logical. Must be TRUE, the default. Automatic mutation or
#'   remediation is deliberately not implemented.
#'
#' @return A list with the artifact type, vulnerability flag, detected signatures,
#'   evidence locations, provenance fields, and R version used for the scan.
#'
#' @examples
#' \dontrun{
#' scan_bigbang_artifact("path/to/generated/metapackage")
#' scan_bigbang_artifact("metapackage_0.1.0.tar.gz")
#' }
#' @export
scan_bigbang_artifact <- function(path, dry_run = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("'path' must be one non-empty character string.", call. = FALSE,
         domain = "R-bigbang")
  }
  if (!isTRUE(dry_run)) {
    stop(
      "Only read-only scanning is supported; 'dry_run' must remain TRUE.",
      call. = FALSE, domain = "R-bigbang"
    )
  }
  if (!file.exists(path) && !dir.exists(path)) {
    stop("Artifact does not exist: ", path, call. = FALSE, domain = "R-bigbang")
  }

  original_path <- path
  path <- normalizePath(path, mustWork = TRUE)

  if (dir.exists(path)) {
    db <- list.files(
      file.path(path, "R"), pattern = "\\.rdx$", full.names = TRUE
    )
    if (length(db) == 1L &&
          file.exists(sub("\\.rdx$", ".rdb", db, ignore.case = TRUE))) {
      result <- .scan_installed_lazydb(path)
      type <- "installed"
    } else {
      result <- .scan_source_tree(path)
      type <- "source"
    }
  } else if (grepl("\\.(tar\\.gz|tgz|tar|zip)$", path, ignore.case = TRUE)) {
    result <- .scan_source_archive(path)
    type <- "archive"
  } else {
    stop("Unsupported artifact type: ", original_path, call. = FALSE,
         domain = "R-bigbang")
  }

  structure(
    list(
      path = path,
      type = type,
      vulnerable = length(result$signatures) > 0L,
      signatures = result$signatures,
      evidence = result$evidence,
      provenance = result$provenance,
      dry_run = TRUE,
      tested_r = "R 4.6.1",
      running_r = R.version.string
    ),
    class = "bigbang_artifact_scan"
  )
}

.scan_signature_text <- function(code, labels) {
  stopifnot(length(code) == length(labels))
  collapsed <- paste(code, collapse = "\n")
  definitions <- list(
    V1_component_unlink = list(
      combined = "(safe_)?unlink\\s*\\(\\s*pkg\\s*,",
      line = "(safe_)?unlink\\s*\\("
    ),
    V2_clean_pkg_dirs = list(
      combined = "\\bclean_pkg_dirs\\s*(<-|=|\\()",
      line = "\\bclean_pkg_dirs\\b"
    ),
    V3_tempdir_sweep = list(
      combined = "list\\.files\\s*\\(\\s*tempdir\\s*\\(",
      line = "list\\.files|tempdir"
    )
  )

  signatures <- character()
  evidence <- list()
  for (name in names(definitions)) {
    definition <- definitions[[name]]
    if (grepl(definition$combined, collapsed, perl = TRUE)) {
      signatures <- c(signatures, name)
      hits <- grep(definition$line, code, perl = TRUE)
      evidence[[name]] <- unique(labels[hits])
    }
  }
  list(signatures = signatures, evidence = evidence)
}

.read_provenance <- function(description) {
  current_prefix <- "Config/bigbang"
  legacy_prefix <- paste0("Config/", "local", "verse")
  fields <- c(
    "Package",
    "Version",
    paste0(current_prefix, "/generator-version"),
    paste0(current_prefix, "/template-safety-schema"),
    paste0(legacy_prefix, "/generator-version"),
    paste0(legacy_prefix, "/template-safety-schema")
  )
  if (!file.exists(description)) {
    return(stats::setNames(as.list(rep(NA_character_, length(fields))), fields))
  }
  desc <- tryCatch(read.dcf(description), error = function(e) NULL)
  values <- stats::setNames(as.list(rep(NA_character_, length(fields))), fields)
  if (!is.null(desc)) {
    present <- intersect(fields, colnames(desc))
    for (field in present) values[[field]] <- unname(desc[1L, field])
  }
  values
}

.scan_code_tokens <- function(lines) {
  parsed <- tryCatch(
    parse(text = lines, keep.source = TRUE),
    error = function(e) NULL
  )
  # Invalid historical sources are kept verbatim. This deliberately prefers a
  # conservative false positive over hiding a destructive signature.
  if (is.null(parsed)) return(lines)

  tokens <- utils::getParseData(parsed)
  tokens <- tokens[tokens$token %in% c("COMMENT", "STR_CONST"), , drop = FALSE]
  if (nrow(tokens) == 0L) return(lines)

  result <- lines
  for (index in rev(seq_len(nrow(tokens)))) {
    token <- tokens[index, ]
    for (line_number in token$line1:token$line2) {
      start <- if (line_number == token$line1) token$col1 else 1L
      end <- if (line_number == token$line2) token$col2 else nchar(result[[line_number]])
      before <- if (start > 1L) substr(result[[line_number]], 1L, start - 1L) else ""
      after <- if (end < nchar(result[[line_number]])) {
        substr(result[[line_number]], end + 1L, nchar(result[[line_number]]))
      } else {
        ""
      }
      result[[line_number]] <- paste0(before, strrep(" ", end - start + 1L), after)
    }
  }
  result
}

.scan_source_tree <- function(root) {
  description <- file.path(root, "DESCRIPTION")
  if (!file.exists(description)) {
    stop("No DESCRIPTION found in source directory: ", root, call. = FALSE,
         domain = "R-bigbang")
  }
  r_dir <- file.path(root, "R")
  r_files <- if (dir.exists(r_dir)) {
    list.files(
      r_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE
    )
  } else {
    character()
  }
  if (length(r_files) > 0L && any(nzchar(Sys.readlink(r_files)))) {
    stop("Refusing to scan symbolic links in the source R directory.", call. = FALSE,
         domain = "R-bigbang")
  }

  code <- character()
  labels <- character()
  for (file in r_files) {
    lines <- readLines(file, warn = FALSE)
    lines <- .scan_code_tokens(lines)
    keep <- nzchar(trimws(lines))
    code <- c(code, lines[keep])
    relative <- substring(file, nchar(root) + 2L)
    labels <- c(labels, paste0(relative, ":", which(keep)))
  }
  result <- .scan_signature_text(code, labels)

  cleanup_files <- file.path(root, c("cleanup", "cleanup.win"))
  cleanup_files <- cleanup_files[file.exists(cleanup_files)]
  if (length(cleanup_files) > 0L) {
    cleanup_lines <- unlist(lapply(cleanup_files, readLines, warn = FALSE))
    destructive <- grepl(
      "\\brm\\s+-rf\\b|\\brmdir\\b.*(/s|/q)|Remove-Item.*-Recurse",
      cleanup_lines, ignore.case = TRUE, perl = TRUE
    )
    if (any(destructive)) {
      result$signatures <- c(result$signatures, "V7_cleanup_hook")
      result$evidence$V7_cleanup_hook <- basename(cleanup_files)
    }
  }

  result$signatures <- unique(result$signatures)
  result$provenance <- .read_provenance(description)
  result
}

.validate_archive_members <- function(members) {
  members <- gsub("\\\\", "/", members)
  unsafe <- startsWith(members, "/") |
    grepl("^[A-Za-z]:", members) |
    grepl("(^|/)\\.\\.(/|$)", members, perl = TRUE)
  if (any(unsafe)) {
    stop(
      "Archive contains unsafe absolute or parent-traversal paths: ",
      paste(utils::head(members[unsafe], 3L), collapse = ", "),
      call. = FALSE, domain = "R-bigbang"
    )
  }
  invisible(members)
}

.scan_source_archive <- function(archive) {
  is_zip <- grepl("\\.zip$", archive, ignore.case = TRUE)
  members <- if (is_zip) {
    utils::unzip(archive, list = TRUE)$Name
  } else {
    utils::untar(archive, list = TRUE)
  }
  .validate_archive_members(members)

  extract_dir <- tempfile("bigbang-scan-archive-")
  dir.create(extract_dir)
  on.exit(safe_unlink(extract_dir, recursive = TRUE), add = TRUE)
  if (is_zip) {
    utils::unzip(archive, exdir = extract_dir)
  } else {
    utils::untar(archive, exdir = extract_dir)
  }

  extracted <- list.files(
    extract_dir, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, include.dirs = TRUE, no.. = TRUE
  )
  if (length(extracted) > 0L && any(nzchar(Sys.readlink(extracted)))) {
    stop("Refusing to scan an archive containing symbolic links.", call. = FALSE,
         domain = "R-bigbang")
  }
  descriptions <- list.files(
    extract_dir, pattern = "^DESCRIPTION$", recursive = TRUE, full.names = TRUE
  )
  if (length(descriptions) == 0L) {
    stop("No DESCRIPTION found in source archive.", call. = FALSE,
         domain = "R-bigbang")
  }
  depths <- lengths(strsplit(
    substring(descriptions, nchar(extract_dir) + 2L), .Platform$file.sep,
    fixed = TRUE
  ))
  descriptions <- descriptions[depths == min(depths)]
  if (length(descriptions) != 1L) {
    stop("Source archive has multiple candidate package roots.", call. = FALSE,
         domain = "R-bigbang")
  }
  .scan_source_tree(dirname(descriptions))
}

.scan_installed_lazydb <- function(root) {
  description <- file.path(root, "DESCRIPTION")
  provenance <- .read_provenance(description)
  package <- provenance$Package
  if (is.na(package) || !nzchar(package)) {
    stop("Installed package has no valid Package field.", call. = FALSE,
         domain = "R-bigbang")
  }

  filebase <- file.path(root, "R", package)
  if (!file.exists(paste0(filebase, ".rdx"))) {
    candidates <- list.files(
      file.path(root, "R"), pattern = "\\.rdx$", full.names = TRUE
    )
    if (length(candidates) != 1L) {
      stop("Could not identify the installed lazy-load database.", call. = FALSE,
           domain = "R-bigbang")
    }
    filebase <- sub("\\.rdx$", "", candidates)
  }

  targets <- c(
    ".onLoad", ".onAttach", ".onUnload", "clean_pkg_dirs",
    "install_and_load_packages", "install_packages_in_order"
  )
  objects <- base::lazyLoadDBexec(
    filebase,
    function(db_env) {
      names <- get("vars", envir = db_env, inherits = FALSE)
      keys <- get("vals", envir = db_env, inherits = FALSE)
      datafile <- get("datafile", envir = db_env, inherits = FALSE)
      compressed <- get("compressed", envir = db_env, inherits = FALSE)
      envhook <- get("envhook", envir = db_env, inherits = FALSE)
      values <- lapply(
        keys, base::lazyLoadDBfetch,
        file = datafile, compressed = compressed, hook = envhook
      )
      stats::setNames(values, names)
    },
    function(names) names %in% targets
  )

  code <- character()
  labels <- character()
  for (name in names(objects)) {
    lines <- deparse(objects[[name]], width.cutoff = 500L)
    lines[1L] <- paste0(name, " <- ", lines[1L])
    code <- c(code, lines)
    labels <- c(labels, paste0("lazydb:", name, ":", seq_along(lines)))
  }
  result <- .scan_signature_text(code, labels)
  result$provenance <- provenance
  result
}
