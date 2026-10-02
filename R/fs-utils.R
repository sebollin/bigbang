# Resolve the deepest existing ancestor of a path and keep the rest as a
# suffix. normalizePath() leaves a path that does not exist as it was
# written, while it canonicalizes an existing parent (a symbolic link such as
# macOS /var -> /private/var, or a Windows 8.3 short name such as
# C:/Users/RUNNER~1). Resolving the ancestor keeps both spellings comparable.
.resolve_physical_path <- function(path) {
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

# Windows has no readlink: Sys.readlink() returns "" there, also for
# junctions and symbolic links. normalizePath() does resolve them, so the last
# component of an existing path is a link exactly when resolving the whole
# path differs from resolving its parent and appending the component.
# Comparing the full path instead would flag every 8.3 short name
# (C:/Users/RUNNER~1/...) and every linked ancestor. A path that does not
# exist is not resolved by normalizePath() at all, so it is never a link
# here. No external program is run: fsutil needs administrator rights and
# would start one process per journal entry.
.path_is_windows_reparse_point <- function(path, is_windows = NULL,
                                           normalize = NULL,
                                           exists = NULL) {
  if (is.null(is_windows)) {
    is_windows <- identical(.Platform$OS.type, "windows")
  }
  if (!isTRUE(is_windows)) return(rep(FALSE, length(path)))
  if (is.null(normalize)) {
    normalize <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  }
  if (is.null(exists)) {
    exists <- function(x) file.exists(x) || dir.exists(x)
  }
  canonical <- function(x) {
    value <- tryCatch(normalize(x), error = function(e) NA_character_)
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      return(NA_character_)
    }
    tolower(sub("/+$", "", gsub("\\\\", "/", value)))
  }
  one <- function(entry) {
    if (is.na(entry) || !nzchar(entry)) return(FALSE)
    entry <- sub("[/\\\\]+$", "", entry)
    component <- basename(entry)
    if (!nzchar(component) || component %in% c(".", "..")) return(FALSE)
    if (!isTRUE(exists(entry))) return(FALSE)
    resolved <- canonical(entry)
    parent <- canonical(dirname(entry))
    if (is.na(resolved) || is.na(parent)) return(FALSE)
    !identical(resolved, paste(parent, tolower(component), sep = "/"))
  }
  vapply(as.character(path), one, logical(1L), USE.NAMES = FALSE)
}

.path_is_symlink <- function(path) {
  readlink <- function(entry) {
    target <- tryCatch(Sys.readlink(entry), error = function(e) "")
    is.character(target) && length(target) == 1L && !is.na(target) &&
      nzchar(target)
  }
  linked <- vapply(as.character(path), readlink, logical(1L),
                   USE.NAMES = FALSE)
  linked | .path_is_windows_reparse_point(path)
}

.validate_project_root_path <- function(project_dir) {
  if (!.path_is_symlink(project_dir)) return(invisible(NULL))
  link <- gsub("\\\\", "/", project_dir)
  .bigbang_abort(
    "bigbang_error_symlink_generated_path",
    .bb_trf(
      paste0(
        "Cannot update %s because generated path components are symbolic links: ",
        "%s. Refusing to write outside the project."
      ),
      project_dir, link
    ),
    path = project_dir, links = link
  )
}

.symlink_in_project_path <- function(project_dir, relative_path) {
  root <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  relative_path <- gsub("\\\\", "/", relative_path)
  if (grepl("^/", relative_path) || grepl("^[A-Za-z]:/", relative_path)) {
    return(if (.path_is_symlink(relative_path)) relative_path else "")
  }
  parts <- strsplit(relative_path, "/", fixed = TRUE)[[1L]]
  current <- root
  parts <- parts[nzchar(parts) & parts != "."]
  for (part in parts) {
    if (identical(part, "..")) return("")
    current <- file.path(current, part)
    if (.path_is_symlink(current)) {
      return(gsub("\\\\", "/", current))
    }
  }
  ""
}

.validate_project_write_paths <- function(project_dir, relative_paths) {
  links <- unique(Filter(nzchar, vapply(
    relative_paths,
    function(path) .symlink_in_project_path(project_dir, path),
    character(1L)
  )))
  if (length(links) > 0L) {
    .bigbang_abort(
      "bigbang_error_symlink_generated_path",
      .bb_trf(
        paste0(
          "Cannot update %s because generated path components are symbolic links: ",
          "%s. Refusing to write outside the project."
        ),
        project_dir, paste(links, collapse = ", ")
      ),
      path = project_dir, links = links
    )
  }
  invisible(NULL)
}

.atomic_replace <- function(source, destination) {
  .record_update_write(source, destination)
  if (file.rename(source, destination)) return(invisible(TRUE))

  # Windows cannot replace an existing file with rename(). Remove only the
  # destination entry. `file.remove()` removes a file reparse point itself on
  # Windows; using `unlink()` there can try to delete the link target.
  if (.path_is_symlink(destination)) {
    removed <- if (identical(.Platform$OS.type, "windows")) {
      suppressWarnings(file.remove(destination))
    } else {
      unlink(destination, recursive = FALSE, force = TRUE)
    }
    if (!isTRUE(removed) && (.path_is_symlink(destination) ||
                               file.exists(destination) || dir.exists(destination))) {
      stop(.bb_trf("Could not replace generated file: %s", destination),
           call. = FALSE)
    }
  } else if (file.exists(destination) && !file.remove(destination)) {
    stop(.bb_trf("Could not replace generated file: %s", destination),
         call. = FALSE)
  }
  if (!file.rename(source, destination)) {
    stop(.bb_trf("Could not replace generated file: %s", destination),
         call. = FALSE)
  }
  invisible(TRUE)
}

.atomic_temporary <- function(path) {
  context <- .update_journal_runtime$current
  tmpdir <- dirname(path)
  if (!is.null(context) &&
        (!is.null(.project_relative_destination(path, context$project)) ||
           startsWith(normalizePath(path, winslash = "/", mustWork = FALSE),
                      paste0(context$path, "/")))) {
    tmpdir <- file.path(context$path, "staging")
  }
  tempfile(pattern = paste0(".", basename(path), "-"), tmpdir = tmpdir)
}

.write_utf8 <- function(text, path) {
  temporary <- .atomic_temporary(path)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  brio::write_lines(text, temporary)
  .atomic_replace(temporary, path)
}

.atomic_copy <- function(source, destination) {
  temporary <- .atomic_temporary(destination)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  if (!file.copy(source, temporary, overwrite = FALSE)) {
    stop(.bb_trf("Could not copy the component archive: %s", source),
         call. = FALSE)
  }
  .atomic_replace(temporary, destination)
}

.atomic_save_rds <- function(object, path) {
  temporary <- .atomic_temporary(path)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(object, temporary)
  .atomic_replace(temporary, path)
}

.r_literal <- function(x) {
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

.escape_non_ascii <- function(text) {
  codepoints <- utf8ToInt(enc2utf8(text))
  paste(vapply(codepoints, function(codepoint) {
    if (codepoint < 0x20L || codepoint == 0x7fL) {
      return(sprintf("\\u%04x", codepoint))
    }
    if (codepoint <= 0x7fL) return(intToUtf8(codepoint))
    if (codepoint <= 0xffffL) return(sprintf("\\u%04x", codepoint))
    sprintf("\\U%08x", codepoint)
  }, character(1L)), collapse = "")
}

.r_string_literal <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(.bb_tr("value must be one non-NA character string"), call. = FALSE)
  }
  codepoints <- utf8ToInt(enc2utf8(value))
  escaped <- vapply(codepoints, function(codepoint) {
    if (codepoint == 0x22L) return("\\\"")
    if (codepoint == 0x5cL) return("\\\\")
    if (codepoint < 0x20L || codepoint == 0x7fL) {
      return(sprintf("\\u%04x", codepoint))
    }
    if (codepoint <= 0x7fL) return(intToUtf8(codepoint))
    if (codepoint <= 0xffffL) return(sprintf("\\u%04x", codepoint))
    sprintf("\\U%08x", codepoint)
  }, character(1L))
  paste0('"', paste(escaped, collapse = ""), '"')
}

.r_symbol_literal <- function(symbol) {
  if (!is.character(symbol) || length(symbol) != 1L || is.na(symbol)) {
    stop(.bb_tr("symbol must be one non-NA character string"), call. = FALSE)
  }
  codepoints <- utf8ToInt(enc2utf8(symbol))
  ascii <- all(codepoints < 0x80L)
  control <- any(codepoints < 0x20L | codepoints == 0x7fL)
  syntactic <- identical(make.names(symbol), symbol) &&
    !grepl("^[0-9]", symbol) && ascii && !control &&
    !symbol %in% .r_syntax_symbols()
  if (isTRUE(syntactic)) symbol else .r_string_literal(symbol)
}

.r_ascii_literal <- function(x) {
  render <- function(value) {
    if (is.null(value)) return("NULL")
    if (is.character(value)) {
      if (length(value) == 0L) return("character()")
      values <- paste(vapply(value, function(item) {
        if (is.na(item)) "NA_character_" else .r_string_literal(item)
      }, character(1L)),
      collapse = ", ")
      rendered <- if (length(value) == 1L) {
        if (is.na(value[[1L]])) "NA_character_" else .r_string_literal(value[[1L]])
      } else {
        paste0("c(", values, ")")
      }
      if (!is.null(names(value))) {
        rendered <- paste0(
          "structure(", rendered, ", names = ",
          render(unname(names(value))), ")"
        )
      }
      return(rendered)
    }
    if (is.list(value)) {
      rendered <- if (length(value) == 0L) {
        "list()"
      } else {
        paste0("list(", paste(vapply(value, render, character(1L)),
                              collapse = ", "), ")")
      }
      if (!is.null(names(value))) {
        rendered <- paste0(
          "structure(", rendered, ", names = ",
          render(unname(names(value))), ")"
        )
      }
      return(rendered)
    }
    if (is.logical(value) || is.numeric(value) || is.integer(value)) {
      rendered <- paste(deparse(value, width.cutoff = 500L), collapse = "")
      if (!is.null(names(value))) {
        rendered <- paste0(
          "structure(", rendered, ", names = ",
          render(unname(names(value))), ")"
        )
      }
      return(rendered)
    }
    .escape_non_ascii(.r_literal(value))
  }
  render(x)
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

.validate_archive_members <- function(members) {
  members <- gsub("\\\\", "/", members)
  unsafe <- startsWith(members, "/") |
    grepl("^[A-Za-z]:", members) |
    grepl("(^|/)\\.\\.(/|$)", members, perl = TRUE)
  if (any(unsafe)) {
    stop(.bb_trf(
      "Archive contains unsafe absolute or parent-traversal paths: %s",
      paste(utils::head(members[unsafe], 3L), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(members)
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

        temp_root <- normalizePath(
          tempdir(), winslash = "/", mustWork = TRUE
        )
        candidate <- normalizePath(
          p, winslash = "/", mustWork = FALSE
        )
        if (identical(candidate, temp_root)) {
          message(.bb_trf("SAFETY: Potentially important directory: %s", p))
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
              # Location, not a basename, establishes that this is temporary.
              # A name such as 'templates' is not evidence of ownership.
              if (!is_path_inside(p, temp_root)) {
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
  # Resolve the existing ancestor first. This preserves the child suffix when
  # a temporary path does not exist yet and its parent is an aliased path.
  inner <- .resolve_physical_path(inner_path)
  outer <- .resolve_physical_path(outer_path)

  # Use one separator representation on Windows.
  if (.Platform$OS.type == "windows") {
    inner <- gsub("\\\\", "/", inner)
    outer <- gsub("\\\\", "/", outer)
  }

  # Equality is containment: callers that need a strict descendant must reject
  # equality separately. Paths are compared after resolving the existing
  # ancestor, so a suffix that traverses a link is judged by its physical path.
  if (identical(inner, outer)) return(TRUE)

  # Add a separator to prevent partial-prefix matches.
  if (!endsWith(outer, "/")) {
    outer <- paste0(outer, "/")
  }

  startsWith(inner, outer)
}
