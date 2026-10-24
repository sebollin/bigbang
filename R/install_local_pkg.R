.classify_local_archive <- function(archive, ext) {
  if (!identical(tolower(ext), ".zip")) return("source")
  members <- gsub(
    "\\\\", "/", .archive_listing(archive, ext)$Name, fixed = TRUE
  )
  if (!any(grepl("(^|/)DESCRIPTION$", members))) {
    stop(.bb_trf(
      "The ZIP archive does not contain a DESCRIPTION file: %s", archive
    ), call. = FALSE)
  }
  if (any(grepl("(^|/)Meta/package\\.rds$", members))) {
    "win.binary"
  } else {
    "source.zip"
  }
}

.resolve_upgrade_policy <- function(force, upgrade, upgrade_missing) {
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    .bigbang_abort(
      "bigbang_error_install_policy",
      .bb_tr("'force' must be TRUE or FALSE")
    )
  }
  upgrade <- match.arg(upgrade, c("newer", "always", "never"))
  if (isTRUE(force)) {
    if (!isTRUE(upgrade_missing) && !identical(upgrade, "always")) {
      .bigbang_abort(
        "bigbang_error_install_policy",
        .bb_tr("'force = TRUE' conflicts with an explicit upgrade policy other than 'always'")
      )
    }
    upgrade <- "always"
  }
  upgrade
}

.component_upgrade_action <- function(installed_version, archive_version,
                                      upgrade) {
  if (is.null(installed_version)) {
    return(list(action = "install", reason_code = "not_installed"))
  }
  if (identical(upgrade, "never")) {
    return(list(action = "keep", reason_code = "upgrade_never"))
  }
  if (identical(upgrade, "newer") && tryCatch(
    installed_version >= base::package_version(archive_version),
    error = function(error) FALSE
  )) {
    return(list(action = "keep", reason_code = "installed_newer_or_equal"))
  }
  list(
    action = "install",
    reason_code = if (identical(upgrade, "always")) {
      "forced_reinstall"
    } else {
      "archive_newer"
    }
  )
}

.repositories_configured <- function(repos) {
  !is.null(repos) && length(repos) > 0L && !all(
    is.na(repos) | !nzchar(trimws(repos)) | trimws(repos) == "@CRAN@"
  )
}

.validate_install_library <- function(lib) {
  if (!is.character(lib) || length(lib) != 1L ||
        is.na(lib) || !nzchar(lib)) {
    .bigbang_abort(
      "bigbang_error_install_policy",
      .bb_tr("The installation library must be one non-empty path.")
    )
  }
  existing <- sub("[/\\\\]+$", "", lib)
  if (!nzchar(existing)) existing <- "/"
  if (file.exists(existing) && !dir.exists(existing)) {
    .bigbang_abort(
      "bigbang_error_install_policy",
      .bb_trf("Could not create installation library: %s", lib)
    )
  }
  invisible(lib)
}

# Explain, in the result itself, why a component was left alone. A bare
# "Already installed" is false whenever the installed package merely shares the
# component's name, and the result is the only thing a calling script can read.
.unchanged_reason <- function(installed_version, version_text, upgrade, newer) {
  installed_text <- as.character(installed_version)
  if (identical(upgrade, "never")) {
    .bb_trf(
      "Kept installed version %s because upgrade = 'never'; the archive names version %s",
      installed_text, version_text
    )
  } else if (isTRUE(newer)) {
    .bb_trf(
      "Kept installed version %s, newer than archive version %s",
      installed_text, version_text
    )
  } else {
    .bb_trf(
      "Kept installed version %s, matching archive version %s",
      installed_text, version_text
    )
  }
}

.unchanged_without_archive <- function(installed_version, reason) {
  .bb_trf(
    "Kept installed version %s; %s",
    as.character(installed_version), reason
  )
}

.with_install_library_path <- function(libraries, code) {
  libraries <- unique(normalizePath(
    libraries[dir.exists(libraries)], winslash = "/", mustWork = TRUE
  ))
  library_path <- paste(libraries, collapse = .Platform$path.sep)
  previous <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  on.exit({
    if (is.na(previous)) {
      Sys.unsetenv("R_LIBS_USER")
    } else {
      Sys.setenv(R_LIBS_USER = previous)
    }
  }, add = TRUE)
  # install.packages() always rebuilds R_LIBS from the current .libPaths(), so
  # R_LIBS_USER is the channel that preserves additional libraries for its child.
  Sys.setenv(R_LIBS_USER = library_path)
  force(code)
}

.installed_package_info <- function(package, libraries) {
  path <- tryCatch(
    find.package(package, lib.loc = libraries, quiet = TRUE),
    error = function(error) character()
  )
  if (length(path) == 0L || !dir.exists(path[[1L]])) {
    return(list(version = NULL, path = NULL, library = NULL))
  }
  path <- normalizePath(path[[1L]], winslash = "/", mustWork = TRUE)
  version <- tryCatch(
    utils::packageVersion(package, lib.loc = dirname(path)),
    error = function(error) NULL
  )
  list(
    version = version,
    path = path,
    library = dirname(path)
  )
}

.read_install_log <- function(log_file) {
  size <- file.info(log_file)$size
  if (is.na(size) || size <= 0) return(character())
  bytes <- readBin(log_file, "raw", n = as.integer(size))
  bytes <- bytes[bytes != as.raw(0L)]
  text <- tryCatch(
    iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = "byte"),
    error = function(error) ""
  )
  if (length(text) == 0L || !nzchar(text)) return(character())
  if (endsWith(text, "\n")) text <- substr(text, 1L, nchar(text) - 1L)
  strsplit(text, "\n", fixed = TRUE)[[1L]]
}

.installation_error_excerpt <- function(output, max_lines = 18L,
                                        max_chars = 4000L) {
  if (length(output) == 0L) return(.bb_tr("No installer output was captured"))
  output <- tryCatch(
    iconv(as.character(output), from = "UTF-8", to = "UTF-8", sub = "byte"),
    error = function(error) rep("<invalid installer output>", length(output))
  )
  output[is.na(output)] <- "<invalid installer output>"
  real_markers <- grep(
    "(^|[[:space:]])(ERROR:|Error:|Error in|fatal error:|error:|Execution halted$)",
    output, ignore.case = TRUE, perl = TRUE
  )
  if (length(real_markers) > 0L) {
    anchor <- real_markers[[length(real_markers)]]
    end <- min(length(output), anchor + 3L)
    start <- max(1L, end - max_lines + 1L)
  } else {
    start <- max(1L, length(output) - max_lines + 1L)
    end <- min(length(output), start + max_lines - 1L)
  }
  excerpt <- output[start:end]
  anchor_line <- if (length(real_markers) > 0L) anchor - start + 1L else NA_integer_
  line_count <- length(excerpt)
  line_limit <- max(
    1L,
    floor((max_chars - max(0L, line_count - 1L)) / line_count)
  )
  truncate_line <- function(line, limit, anchor = FALSE) {
    length <- nchar(line, type = "chars")
    if (length <= limit) return(line)
    if (limit <= 3L || !isTRUE(anchor)) {
      return(substr(line, 1L, limit))
    }
    marker <- regexpr(
      "(ERROR:|Error:|Error in|fatal error:|error:|Execution halted$)",
      line, ignore.case = TRUE, perl = TRUE
    )
    marker_start <- marker[[1L]]
    marker_length <- attr(marker, "match.length")[[1L]]
    if (marker_start < 1L || marker_length < 1L) {
      return(substr(line, 1L, limit))
    }
    if (marker_start + marker_length - 1L <= limit - 3L) {
      return(paste0(substr(line, 1L, limit - 3L), "..."))
    }
    paste0("...", substr(line, marker_start, marker_start + limit - 4L))
  }
  excerpt <- Map(
    function(line, index) truncate_line(line, line_limit, index == anchor_line),
    excerpt, seq_along(excerpt)
  )
  paste(unlist(excerpt, use.names = FALSE), collapse = "\n")
}

.collect_install_components <- function(component, inventory) {
  state <- new.env(parent = emptyenv())
  state$collected <- list()
  state$seen <- character()
  collect <- function(item) {
    if (item$package %in% state$seen) return(invisible(NULL))
    state$seen <- c(state$seen, item$package)
    state$collected[[length(state$collected) + 1L]] <- item
    for (dependency in intersect(item$dependencies, inventory$packages)) {
      matches <- which(inventory$packages == dependency)
      if (length(matches) == 1L) collect(inventory$entries[[matches[[1L]]]])
    }
    invisible(NULL)
  }
  collect(component)
  state$collected
}

.external_requirements <- function(components, local_names) {
  dependencies <- unique(unlist(lapply(
    components, function(item) item$dependencies
  ), use.names = FALSE))
  external <- setdiff(dependencies, local_names)
  requirements <- stats::setNames(vector("list", length(external)), external)
  for (dependency in external) {
    requirements[[dependency]] <- unlist(lapply(components, function(item) {
      Filter(
        function(constraint) identical(constraint$package, dependency),
        item$constraints
      )
    }), recursive = FALSE)
  }
  requirements
}

.dependency_constraint_text <- function(constraints) {
  if (length(constraints) == 0L) return(character())
  vapply(
    constraints,
    function(constraint) {
      paste(constraint$package, constraint$op, constraint$version)
    },
    character(1L)
  )
}

.external_dependency_status <- function(dependency, constraints, libraries) {
  installed_info <- .installed_package_info(dependency, libraries)
  installed <- installed_info$version
  available <- !is.null(installed)
  requirement_text <- paste(
    .dependency_constraint_text(constraints), collapse = ", "
  )
  satisfied <- isTRUE(available) && !is.null(installed) && (
    length(constraints) == 0L || all(vapply(
      constraints,
      function(constraint) {
        .version_satisfies(installed, constraint$op, constraint$version)
      },
      logical(1L)
    ))
  )
  detail <- if (!isTRUE(available) || is.null(installed)) {
    if (nzchar(requirement_text)) {
      .bb_trf(
        "Dependency %s is not installed; required constraint(s): %s",
        dependency, requirement_text
      )
    } else {
      dependency
    }
  } else if (!isTRUE(satisfied)) {
    .bb_trf(
      "Dependency %s has installed version %s; required constraint(s): %s",
      dependency, as.character(installed), requirement_text
    )
  } else {
    character()
  }
  list(
    available = available, installed = installed, constraints = constraints,
    satisfied = satisfied, detail = detail,
    library = installed_info$library, path = installed_info$path
  )
}

.install_external_dependency <- function(dependency, libraries, lib, repos,
                                         type = NULL) {
  diagnostics <- character()
  install <- function() {
    args <- list(
      pkgs = dependency, dependencies = NA, repos = repos, lib = lib
    )
    if (!is.null(type)) args$type <- type
    do.call(utils::install.packages, args)
  }
  tryCatch(
    withCallingHandlers(
      .with_install_library_path(libraries, install()),
      warning = function(condition) {
        diagnostics <<- c(diagnostics, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) {
      diagnostics <<- c(diagnostics, conditionMessage(condition))
      NULL
    }
  )
  list(diagnostics = unique(diagnostics))
}

.prepare_external_dependencies <- function(requirements, libraries, lib,
                                           cran_deps, repos) {
  status <- lapply(
    names(requirements),
    function(dependency) {
      .external_dependency_status(
        dependency, requirements[[dependency]], libraries
      )
    }
  )
  names(status) <- names(requirements)
  unsatisfied <- names(status)[!vapply(status, `[[`, logical(1L), "satisfied")]
  no_repository <- FALSE
  if (length(unsatisfied) > 0L && identical(cran_deps, "install")) {
    if (!.repositories_configured(repos)) {
      no_repository <- TRUE
    } else {
      for (dependency in unsatisfied) {
        attempts <- character()
        install_attempt <- function(type, label) {
          outcome <- .install_external_dependency(
            dependency, libraries, lib, repos, type = type
          )
          current <- .external_dependency_status(
            dependency, requirements[[dependency]], libraries
          )
          if (!isTRUE(current$satisfied)) {
            reason <- if (length(outcome$diagnostics) > 0L) {
              paste(outcome$diagnostics, collapse = "; ")
            } else {
              "the dependency remains unsatisfied"
            }
            attempts <<- c(attempts, paste0(label, ": ", reason))
          }
          current
        }
        current <- install_attempt(NULL, "default package type")
        if (!isTRUE(current$satisfied) &&
              !identical(getOption("pkgType"), "source")) {
          current <- install_attempt("source", "source package type")
        }
        status[[dependency]] <- current
        if (!isTRUE(current$satisfied) && length(attempts) > 0L) {
          status[[dependency]]$detail <- paste(
            status[[dependency]]$detail,
            paste("Installation attempts:", paste(attempts, collapse = " | ")),
            sep = "; "
          )
        }
      }
    }
  }
  list(status = status, no_repository = no_repository)
}

.install_source_component <- function(target, lib, verbose = TRUE) {
  # utils::install.packages() discards the child installer's output for local
  # source packages, so a failure only reports a non-zero exit status. Run the
  # same R CMD INSTALL directly and keep the output: the child's own ERROR
  # lines become the failure message instead of a generic verification one.
  log_file <- tempfile("bigbang-install-log-")
  keep_log <- FALSE
  on.exit(if (!keep_log) unlink(log_file, force = TRUE), add = TRUE)
  target <- normalizePath(path.expand(target), winslash = "/", mustWork = FALSE)
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  status <- system2(
    r_binary, c("CMD", "INSTALL", "-l", shQuote(lib), shQuote(target)),
    stdout = log_file, stderr = log_file
  )
  output <- if (file.exists(log_file)) .read_install_log(log_file) else character()
  if (isTRUE(verbose) && length(output) > 0L) cat(output, sep = "\n")
  if (!identical(status, 0L)) {
    keep_log <- TRUE
    detail <- .installation_error_excerpt(output)
    if (!nzchar(detail)) {
      detail <- .bb_trf("R CMD INSTALL exited with status %d", status)
    } else if (length(output) == 0L) {
      detail <- paste(
        detail,
        .bb_trf("R CMD INSTALL exited with status %d", status),
        sep = "; "
      )
    }
    stop(.bb_trf(
      "Installation failed; error details: %s; full installation log: %s",
      detail, normalizePath(log_file, winslash = "/", mustWork = FALSE)
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Install a local package together with its dependencies
#'
#' Installs a package from a local archive. Dependencies available as local
#' archives are installed recursively; missing non-local dependencies follow the
#' explicit `cran_deps` policy. ZIP archives containing `Meta/package.rds` are
#' treated as Windows binaries, while other ZIP archives are unpacked and
#' installed as source packages.
#'
#' Archive entry names follow the `bigbang.archive.portable_names` option
#' described in [scan_bigbang_artifact()]. It is `TRUE` by default; set it to
#' `FALSE` only when every recipient uses Linux or macOS. Relaxed names warn
#' during installation and remain unavailable on Windows or on filesystems that
#' do not distinguish case. Case-only collisions are decided when the archive
#' is extracted; an earlier inspection without a destination defers that
#' decision. Components made only of dots and spaces, Windows-specific rules,
#' and security-critical path rules are never relaxed.
#'
#' @param package Character. An existing archive path, or a package stem such
#'   as `"uspr_0.8.5"` to resolve in `pkg_dir`. An existing file is always
#'   treated as a path; only a non-existing element is resolved as a stem. A
#'   bare package name such as `"uspr"` resolves when exactly one archive in
#'   those directories declares that `Package` identity; multiple matches are
#'   an ambiguity error. Supported archives that cannot be read during this
#'   identity search are excluded with a warning that names the archive.
#' @param pkg_dir Character. Optional directory or directories containing local
#'   archives. It is not needed when `package` is an existing path.
#' @param ext Character. Fallback archive extension for stems; existing paths
#'   keep their own extension.
#' @param repos Character. Repositories used only when `cran_deps = "install"`.
#' @param cran_deps Character. Policy for missing non-local dependencies:
#'   `"skip"` (the default) never accesses the network, `"error"` fails without
#'   accessing it, and `"install"` attempts installation from `repos`.
#' @param force Logical. Reinstall every local archive. This is a convenience
#'   alias for `upgrade = "always"`.
#' @param upgrade Character. Installed-version policy: `"newer"` installs only
#'   when the archive is newer, `"always"` reinstalls, and `"never"` keeps any
#'   installed version. Explicitly combining `force = TRUE` with a value other
#'   than `"always"` is an error.
#' @param verbose Logical. Whether to emit progress and summary messages. The
#'   default follows `getOption("bigbang.verbose", interactive())`.
#' @param lib Character. Library in which the local component must be installed
#'   and verified when supplied explicitly. When omitted, the legacy lookup
#'   considers all of `.libPaths()` and installs into its first entry. Non-local
#'   dependencies may be available either in `lib` or anywhere on `.libPaths()`.
#'
#' @section Installation:
#' This function installs packages into `lib`, which defaults to the user's
#' active R library. When `lib` is supplied explicitly, a component found only
#' in another library is still installed into `lib`; non-local dependencies are
#' resolved from `lib` plus `.libPaths()`. When `lib` is omitted, an installed
#' component found anywhere on `.libPaths()` retains the pre-0.3.0 behavior.
#' Installation
#' occurs only when the user calls the function; loading `bigbang` never installs
#' packages. With the default `cran_deps = "skip"`, it does not access the network.
#' An installed package can be kept without reading its archive when
#' `upgrade = "never"`. Under the default policy, bigbang reads only the archive
#' `DESCRIPTION` first; if that metadata cannot be verified for an already
#' installed package, the installed package is kept and the reason is reported.
#' With `upgrade = "never"`, the shortcut takes the component identity from the
#' archive filename because the archive is not read. Use `upgrade = "newer"` when
#' the declared `Package` field must be checked against the installed package.
#'
#' @return Invisibly, a list describing installed, unchanged, failed, and
#'   skipped packages. Components that an upgrade policy left in place are
#'   reported in `unchanged`, not in `installed`.
#' @examples
#' \donttest{
#' archive <- system.file(
#'   "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
#' )
#' example_library <- tempfile("bigbang-example-library-")
#' dir.create(example_library)
#' on.exit(unlink(example_library, recursive = TRUE), add = TRUE)
#' install_local_pkg(archive, lib = example_library, verbose = FALSE)
#' }
#' @seealso [create_metapackage()] for generating a meta-package with an explicit
#'   component installer; [scan_bigbang_artifact()] for the archive-name policy.
#' @export
install_local_pkg <- function(
  package,
  pkg_dir = NULL,
  ext = ".tar.gz",
  repos = NULL,
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive()),
  # Added after 0.1.0, so they go last: a positional call written against
  # 0.1.0 passed verbose sixth, and must keep meaning verbose.
  force = FALSE,
  upgrade = c("newer", "always", "never"),
  lib = .libPaths()[[1L]]
) {
  .archive_portable_names()
  .archive_warning_state$seen <- character()
  .archive_warning_state$active <- TRUE
  on.exit({
    .archive_warning_state$active <- FALSE
    .archive_warning_state$seen <- character()
  }, add = TRUE)
  source_build_context <- .source_build_context_new()
  on.exit(.source_build_context_close(source_build_context), add = TRUE)
  lib_was_missing <- missing(lib)
  cran_deps <- match.arg(cran_deps)
  upgrade <- .resolve_upgrade_policy(force, upgrade, missing(upgrade))
  .validate_install_library(lib)
  if (!dir.exists(lib) && !.install_library_creatable(lib)) {
    .bigbang_abort(
      "bigbang_error_install_policy",
      .bb_trf("Could not create installation library: %s", lib)
    )
  }
  if (!dir.exists(lib) && !dir.create(lib, recursive = TRUE,
                                      showWarnings = FALSE)) {
    .bigbang_abort(
      "bigbang_error_install_policy",
      .bb_trf("Could not create installation library: %s", lib)
    )
  }
  lib <- normalizePath(lib, winslash = "/", mustWork = TRUE)
  dependency_libraries <- unique(c(lib, .libPaths()))
  component_libraries <- if (lib_was_missing) dependency_libraries else lib
  failure_names <- if (is.character(package) && length(package) > 0L) {
    package
  } else {
    "package"
  }
  resolution_failure <- function(error) {
    if (isTRUE(verbose)) {
      message(.bb_trf(
        "Packages that failed: %s", paste(failure_names, collapse = ", ")
      ))
    }
    invisible(structure(
      list(
        installed = list(), unchanged = list(),
        failed = stats::setNames(
          rep(list(conditionMessage(error)), length(failure_names)),
          failure_names
        ),
        skipped = list()
      ),
      class = "bigbang_install_result"
    ))
  }
  candidate <- tryCatch({
    path <- .resolve_archive_input(
      package, pkg_dir, ext, build_context = source_build_context
    )
    actual_ext <- .archive_extension(path)
    list(
      path = path,
      ext = actual_ext,
      stem = .archive_stem(path, actual_ext)
    )
  }, error = identity)
  if (inherits(candidate, "error")) {
    return(resolution_failure(candidate))
  }

  candidate_name <- sub("_.*", "", candidate$stem)
  installed_candidate <- .installed_package_info(
    candidate_name, component_libraries
  )$version
  unchanged_result <- function(reason) {
    if (isTRUE(verbose)) message(.bb_trf(
      "Package %s is already installed; %s", candidate_name, reason
    ))
    invisible(structure(
      list(
        installed = list(),
        unchanged = stats::setNames(list(reason), candidate$stem),
        failed = list(),
        skipped = list()
      ),
      class = "bigbang_install_result"
    ))
  }

  if (!is.null(installed_candidate) && identical(upgrade, "never")) {
    return(unchanged_result(.unchanged_without_archive(
      installed_candidate,
      .bb_tr("the archive was not read because upgrade = 'never'")
    )))
  }

  if (!is.null(installed_candidate) && identical(upgrade, "newer")) {
    candidate_metadata <- tryCatch(
      .read_archive_version(candidate$path, candidate$ext),
      error = identity
    )
    if (inherits(candidate_metadata, "error")) {
      reason <- .unchanged_without_archive(
        installed_candidate,
        .bb_trf(
          "archive metadata could not be verified: %s",
          conditionMessage(candidate_metadata)
        )
      )
      return(unchanged_result(reason))
    }
    if (identical(candidate_metadata$package, candidate_name) &&
      identical(
        .component_upgrade_action(
          installed_candidate, candidate_metadata$version, upgrade
        )$action,
        "keep"
      )) {
      reason <- .unchanged_reason(
        installed_candidate, candidate_metadata$version, upgrade,
        installed_candidate > base::package_version(candidate_metadata$version)
      )
      return(unchanged_result(reason))
    }
  }

  resolved <- tryCatch(
    .resolve_components(
      package, pkg_dir, ext, build_context = source_build_context
    ),
    error = function(error) error
  )
  if (inherits(resolved, "error")) {
    return(resolution_failure(resolved))
  }
  components <- resolved$components
  inventory <- resolved$inventory
  component <- components[[1L]]
  .validate_archive_metadata(component)
  install_components <- .collect_install_components(component, inventory)
  state <- new.env(parent = emptyenv())
  state$installed <- list()
  state$unchanged <- list()
  state$failed <- list()
  state$skipped <- list()
  state$visiting <- character()
  state$final <- new.env(parent = emptyenv())

  install_one <- function(item) {
    stem <- item$stem
    base_name <- item$package
    version_text <- item$version
    archive <- item$path

    if (exists(stem, envir = state$final, inherits = FALSE)) {
      return(isTRUE(state$final[[stem]]))
    }

    if (base_name %in% state$visiting) {
      state$failed[[stem]] <- .bb_tr("Circular local dependency")
      state$final[[stem]] <- FALSE
      return(FALSE)
    }
    local_dependencies <- intersect(item$dependencies, inventory$packages)
    blocked_dependencies <- local_dependencies[vapply(
      local_dependencies,
      function(dependency) {
        match <- which(inventory$packages == dependency)
        length(match) == 1L &&
          (length(state$skipped[[inventory$entries[[match]]$stem]]) > 0L ||
             length(state$failed[[inventory$entries[[match]]$stem]]) > 0L)
      },
      logical(1L)
    )]
    if (length(blocked_dependencies) > 0L) {
      dependency <- blocked_dependencies[[1L]]
      dependency_match <- which(inventory$packages == dependency)
      dependency_stem <- inventory$entries[[dependency_match[[1L]]]]$stem
      dependency_reason <- if (length(state$failed[[dependency_stem]]) > 0L) {
        state$failed[[dependency_stem]]
      } else {
        state$skipped[[dependency_stem]]
      }
      state$skipped[[stem]] <- .bb_trf(
        "Skipped because local dependency %s was not installed: %s",
        dependency,
        dependency_reason
      )
      state$final[[stem]] <- FALSE
      return(FALSE)
    }
    # Metadata was read before installation, so the DESCRIPTION version remains
    # authoritative even when the filename has no version or disagrees with it.
    installed_version <- .installed_package_info(
      base_name, component_libraries
    )$version
    keep_installed <- identical(
      .component_upgrade_action(installed_version, version_text, upgrade)$action,
      "keep"
    )
    if (keep_installed) {
      newer <- installed_version > base::package_version(version_text)
      if (isTRUE(verbose)) {
        message(if (newer) {
          .bb_trf(
            "Package %s has installed version %s, newer than archive version %s; keeping the installed version.",
            base_name, as.character(installed_version), version_text
          )
        } else {
          .bb_trf(
            "Package %s has installed version %s and archive version %s; keeping the installed version.",
            base_name, as.character(installed_version), version_text
          )
        })
      }
      state$unchanged[[stem]] <- .unchanged_reason(
        installed_version, version_text, upgrade, newer
      )
      state$final[[stem]] <- TRUE
      return(TRUE)
    }
    state$visiting <- c(state$visiting, base_name)
    on.exit(state$visiting <- setdiff(state$visiting, base_name), add = TRUE)

    extracted <- tempfile("bigbang-archive-")
    dir.create(extracted)
    on.exit(safe_unlink(extracted, recursive = TRUE), add = TRUE)
    extraction_error <- tryCatch({
      .extract_archive_checked(archive, item$ext, extracted)
      NULL
    }, error = identity)
    if (inherits(extraction_error, "error")) {
      state$failed[[stem]] <- conditionMessage(extraction_error)
      state$final[[stem]] <- FALSE
      return(FALSE)
    }

    archive_kind <- tryCatch(.classify_local_archive(archive, item$ext), error = identity)
    if (inherits(archive_kind, "error")) {
      state$failed[[stem]] <- conditionMessage(archive_kind)
      state$final[[stem]] <- FALSE
      return(FALSE)
    }
    binary_zip <- identical(archive_kind, "win.binary")
    if (binary_zip && .Platform$OS.type != "windows") {
      state$failed[[stem]] <- .bb_tr(
        "Windows binary ZIP packages can only be installed on Windows"
      )
      state$final[[stem]] <- FALSE
      return(FALSE)
    }
    package_root <- tryCatch(
      .find_archive_root(extracted, archive, allow_flat = binary_zip),
      error = identity
    )
    if (inherits(package_root, "error")) {
      state$failed[[stem]] <- conditionMessage(package_root)
      state$final[[stem]] <- FALSE
      return(FALSE)
    }
    dependencies <- item$dependencies
    local_dependencies <- intersect(dependencies, inventory$packages)
    local_constraints <- item$constraints[vapply(
      item$constraints,
      function(constraint) constraint$package %in% inventory$packages,
      logical(1L)
    )]
    for (constraint in local_constraints) {
      match <- which(inventory$packages == constraint$package)
      if (length(match) == 1L) {
        actual <- inventory$entries[[match[[1L]]]]$version
        if (!.version_satisfies(actual, constraint$op, constraint$version)) {
          state$failed[[stem]] <- .bb_trf(
            "Component %s requires %s %s %s, but the included archive provides version %s.",
            base_name, constraint$package, constraint$op,
            constraint$version, actual
          )
          state$final[[stem]] <- FALSE
          return(FALSE)
        }
      }
    }
    dependency_blockers <- list()
    for (dependency in local_dependencies) {
      matches <- which(inventory$packages == dependency)
      if (length(matches) > 1L) {
        dependency_blockers[[length(dependency_blockers) + 1L]] <- list(
          dependency = dependency,
          reason = .bb_trf(
            "More than one local archive is available for dependency %s.", dependency
          ),
          circular = FALSE,
          failed = TRUE
        )
        next
      }
      if (!isTRUE(install_one(inventory$entries[[matches[[1L]]]]))) {
        dependency_stem <- inventory$entries[[matches[[1L]]]]$stem
        dependency_reason <- if (length(state$failed[[dependency_stem]]) > 0L) {
          state$failed[[dependency_stem]]
        } else {
          state$skipped[[dependency_stem]]
        }
        if (length(dependency_reason) == 0L) {
          dependency_reason <- .bb_tr(
            "The local dependency did not reach a final installation state."
          )
        }
        dependency_blockers[[length(dependency_blockers) + 1L]] <- list(
          dependency = dependency,
          reason = dependency_reason,
          circular = identical(dependency_reason, .bb_tr("Circular local dependency")),
          failed = FALSE
        )
      }
    }
    if (length(dependency_blockers) > 0L) {
      circular <- vapply(dependency_blockers, `[[`, logical(1L), "circular")
      failed <- vapply(dependency_blockers, `[[`, logical(1L), "failed")
      first <- dependency_blockers[[1L]]
      if (any(circular) || any(failed)) {
        first <- dependency_blockers[[which(circular | failed)[[1L]]]]
        state$failed[[stem]] <- first$reason
      } else {
        state$skipped[[stem]] <- .bb_trf(
          "Skipped because local dependency %s was not installed: %s",
          first$dependency, first$reason
        )
      }
      state$final[[stem]] <- FALSE
      return(FALSE)
    }

    external_requirements <- .external_requirements(
      list(item), inventory$packages
    )
    external_preparation <- .prepare_external_dependencies(
      external_requirements, dependency_libraries, lib, cran_deps, repos
    )
    external_status <- external_preparation$status
    external <- setdiff(dependencies, inventory$packages)
    unsatisfied_external <- external[!vapply(
      external,
      function(dependency) isTRUE(external_status[[dependency]]$satisfied),
      logical(1L)
    )]
    if (length(unsatisfied_external) > 0L) {
      detail <- paste(vapply(
        unsatisfied_external,
        function(dependency) external_status[[dependency]]$detail,
        character(1L)
      ), collapse = "; ")
      if (isTRUE(external_preparation$no_repository)) {
        state$failed[[stem]] <- .bb_tr(
          "Cannot install dependencies without a configured repository"
        )
        state$final[[stem]] <- FALSE
        return(FALSE)
      }
      if (cran_deps == "skip") {
        state$skipped[[stem]] <- .bb_trf(
          "Missing non-local dependencies: %s", detail
        )
      } else {
        state$failed[[stem]] <- .bb_trf(
          "Missing non-local dependencies: %s", detail
        )
      }
      state$final[[stem]] <- FALSE
      return(FALSE)
    }

    install_target <- if (identical(archive_kind, "source.zip")) {
      package_root
    } else {
      archive
    }
    install_error <- tryCatch({
      if (binary_zip) {
        .with_install_library_path(
          dependency_libraries,
          utils::install.packages(
            install_target,
            repos = NULL,
            type = "win.binary",
            dependencies = FALSE,
            lib = lib
          )
        )
      } else {
        .with_install_library_path(
          dependency_libraries,
          .install_source_component(install_target, lib, verbose = verbose)
        )
      }
      NULL
    }, error = identity)
    verified <- !inherits(install_error, "error") && tryCatch(
      utils::packageVersion(base_name, lib.loc = lib) ==
        base::package_version(version_text),
      error = function(e) FALSE
    )
    if (!verified) {
      state$failed[[stem]] <- if (inherits(install_error, "error")) {
        conditionMessage(install_error)
      } else {
        .bb_tr("Installation could not be verified")
      }
      state$final[[stem]] <- FALSE
      return(FALSE)
    }

    state$installed[[stem]] <- .bb_tr("Installed successfully")
    state$final[[stem]] <- TRUE
    if (isTRUE(verbose)) {
      if (identical(base_name, stem)) {
        message(.bb_trf("Installed local package: %s", stem))
      } else {
        message(.bb_trf(
          "Installed local package: %s from %s", base_name, stem
        ))
      }
    }
    TRUE
  }

  install_one(component)
  if (isTRUE(verbose) && length(state$failed) > 0L) {
    message(.bb_trf(
      "Packages that failed: %s", paste(names(state$failed), collapse = ", ")
    ))
  }
  if (isTRUE(verbose) && length(state$skipped) > 0L) {
    message(.bb_trf(
      "Packages skipped by the offline policy: %s",
      paste(names(state$skipped), collapse = ", ")
    ))
  }
  if (isTRUE(verbose) && interactive() && length(state$unchanged) > 0L) {
    message(.bb_trf(
      "Use force = TRUE or upgrade = 'always' to reinstall unchanged packages: %s",
      paste(names(state$unchanged), collapse = ", ")
    ))
  }
  invisible(structure(
    list(
      installed = state$installed,
      unchanged = state$unchanged,
      failed = state$failed,
      skipped = state$skipped
    ),
    class = "bigbang_install_result"
  ))
}
