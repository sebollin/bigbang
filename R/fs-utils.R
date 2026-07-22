.write_utf8 <- function(text, path) {
  brio::write_lines(text, path)
}

.r_literal <- function(x) {
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

.copyright_holders <- function(authors) {
  expression <- tryCatch(parse(text = authors)[[1L]], error = function(e) NULL)
  if (is.null(expression)) return("Authors listed in Authors@R")

  literal <- function(value) {
    if (is.character(value) && length(value) == 1L) value else NULL
  }
  collect <- function(value) {
    if (!is.call(value)) return(character())
    call_name <- as.character(value[[1L]])
    if (identical(call_name, "person")) {
      arguments <- as.list(value)[-1L]
      argument_names <- names(arguments)
      if (is.null(argument_names)) argument_names <- rep("", length(arguments))
      named <- function(name) {
        index <- match(name, argument_names, nomatch = 0L)
        if (index > 0L) literal(arguments[[index]]) else NULL
      }
      positional <- arguments[argument_names == ""]
      given <- named("given")
      family <- named("family")
      if (is.null(given) && length(positional) >= 1L) given <- literal(positional[[1L]])
      if (is.null(family) && length(positional) >= 2L) family <- literal(positional[[2L]])
      holder <- trimws(paste(c(given, family), collapse = " "))
      if (nzchar(holder)) return(holder)
      return(character())
    }
    unlist(lapply(as.list(value)[-1L], collect), use.names = FALSE)
  }

  holders <- unique(collect(expression))
  if (length(holders) == 0L) "Authors listed in Authors@R" else paste(holders, collapse = ", ")
}

.escape_regex_literal <- function(x) {
  gsub(".", "\\.", x, fixed = TRUE)
}

#' Remove owned files with defensive path checks
#'
#' Rejects roots, protected directories, suspiciously short paths, and
#' non-temporary R package sources before delegating to [unlink()].
#'
#' @param path Character paths to remove.
#' @param recursive Logical recursive-removal flag.
#' @param force Logical force-removal flag.
#' @param verify Logical safety-check flag.
#' @return The result returned by [unlink()], or `FALSE` when blocked.
#' @noRd
safe_unlink <- function(path, recursive = FALSE, force = FALSE, verify = TRUE) {
  # Safety configuration
  min_path_length <- 3  # Very short paths are suspicious

  # System and development directories that must never be removed.
  protected_dirs <- c(
    # Operating-system directories
    "bin", "boot", "dev", "etc", "home", "lib", "mnt", "opt", "proc", "root",
    "run", "sbin", "srv", "sys", "tmp", "usr", "var", "Program Files",
    "Windows", "Users", "System32", "AppData", "ProgramData",

    # R and development directories
    "library", "include", "share", "R", "Rtools", "Git", "src",

    # Version-control and dependency directories
    ".git", ".svn", ".hg", "node_modules"
  )

  # Potentially dangerous path patterns.
  dangerous_patterns <- c(
    "^[A-Za-z]:\\\\$",  # C:\, D:\, etc.
    "^/$",             # Unix filesystem root
    "^\\\\\\\\",       # UNC paths
    "^~$",             # Home directory
    "^\\.$",           # Current directory
    "^\\.\\.$"         # Parent directory
  )

  # Run conservative validation unless explicitly disabled.
  if (verify) {
    if (is.character(path) && length(path) > 0) {
      for (p in path) {
        # Reject suspiciously short paths such as roots.
        if (nchar(p) < min_path_length) {
          message(.bb_trf("SAFETY: Path is too short and may be dangerous: %s", p))
          return(invisible(FALSE))
        }

        # Reject known dangerous patterns.
        if (any(sapply(dangerous_patterns, function(pattern) grepl(pattern, p)))) {
          message(.bb_trf("SAFETY: Potentially dangerous path pattern: %s", p))
          return(invisible(FALSE))
        }

        # Apply directory-specific checks.
        if (dir.exists(p)) {
          # Never remove protected directories.
          if (basename(p) %in% protected_dirs) {
            message(.bb_trf("SAFETY: Potentially important directory: %s", p))
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
              is_temp_pkg <- grepl("^00LOCK-|^\\.Rcheck$|^tmp|^temp", basename(p))

              if (!is_temp_pkg) {
                message(.bb_trf("SAFETY: Possible non-temporary R package directory: %s", p))
                return(invisible(FALSE))
              }
            }
          }
        }
      }
    }
  }

  # Delegate only after every check passes.
  result <- unlink(path, recursive = recursive, force = force)

  # Report incomplete removal.
  if (result != 0) {
    warning(.bb_trf("Could not remove completely: %s", paste(path, collapse = ", ")),
            call. = FALSE)
  }

  result
}



#' Check whether one path is contained by another
#'
#' Normalizes both paths and compares path components without unsafe partial
#' prefix matches.
#'
#' @param inner_path Character candidate child path.
#' @param outer_path Character candidate parent path.
#' @return `TRUE` when `inner_path` is inside `outer_path`.
#' @noRd
is_path_inside <- function(inner_path, outer_path) {
  # Normalize paths before comparing components.
  inner <- normalizePath(inner_path, mustWork = FALSE)
  outer <- normalizePath(outer_path, mustWork = FALSE)

  # Use one separator representation on Windows.
  if (.Platform$OS.type == "windows") {
    inner <- gsub("\\\\", "/", inner)
    outer <- gsub("\\\\", "/", outer)
  }

  # Add a separator to prevent partial-prefix matches.
  if (!endsWith(outer, "/")) {
    outer <- paste0(outer, "/")
  }

  startsWith(inner, outer)
}
