.classify_local_archive <- function(archive, ext) {
  if (!identical(tolower(ext), ".zip")) return("source")
  members <- gsub(
    "\\\\", "/", utils::unzip(archive, list = TRUE)$Name, fixed = TRUE
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

.install_source_component <- function(target, lib, verbose = TRUE) {
  # utils::install.packages() discards the child installer's output for local
  # source packages, so a failure only reports a non-zero exit status. Run the
  # same R CMD INSTALL directly and keep the output: the child's own ERROR
  # lines become the failure message instead of a generic verification one.
  log_file <- tempfile("bigbang-install-log-")
  on.exit(unlink(log_file, force = TRUE), add = TRUE)
  target <- normalizePath(path.expand(target), winslash = "/", mustWork = FALSE)
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  status <- system2(
    r_binary, c("CMD", "INSTALL", "-l", shQuote(lib), shQuote(target)),
    stdout = log_file, stderr = log_file
  )
  output <- if (file.exists(log_file)) {
    readLines(log_file, warn = FALSE)
  } else {
    character()
  }
  if (isTRUE(verbose) && length(output) > 0L) cat(output, sep = "\n")
  if (!identical(status, 0L)) {
    detail <- grep("ERROR", output, value = TRUE, fixed = TRUE)
    if (length(detail) == 0L) detail <- utils::tail(output, 5L)
    detail <- paste(detail, collapse = " | ")
    if (!nzchar(detail)) {
      detail <- sprintf("R CMD INSTALL exited with status %d", status)
    }
    stop(detail, call. = FALSE)
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
#' @seealso [create_metapackage()] for generating a meta-package with an explicit
#'   component installer.
#' @export
install_local_pkg <- function(
  package,
  pkg_dir = NULL,
  ext = ".tar.gz",
  repos = getOption("repos"),
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive()),
  # Added after 0.1.0, so they go last: a positional call written against
  # 0.1.0 passed verbose sixth, and must keep meaning verbose.
  force = FALSE,
  upgrade = c("newer", "always", "never"),
  lib = .libPaths()[[1L]]
) {
  lib_was_missing <- missing(lib)
  cran_deps <- match.arg(cran_deps)
  upgrade <- .resolve_upgrade_policy(force, upgrade, missing(upgrade))
  if (!is.character(lib) || length(lib) != 1L || is.na(lib) || !nzchar(lib)) {
    stop(.bb_tr("The installation library must be one non-empty path."),
         call. = FALSE)
  }
  if (!dir.exists(lib) && !dir.create(lib, recursive = TRUE)) {
    stop(.bb_trf("Could not create installation library: %s", lib),
         call. = FALSE)
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
    path <- .resolve_archive_input(package, pkg_dir, ext)
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
  installed_candidate <- tryCatch(
    utils::packageVersion(candidate_name, lib.loc = component_libraries),
    error = function(e) NULL
  )
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
      tryCatch(
        installed_candidate >= base::package_version(candidate_metadata$version),
        error = function(e) FALSE
      )) {
      reason <- .unchanged_reason(
        installed_candidate, candidate_metadata$version, upgrade,
        installed_candidate > base::package_version(candidate_metadata$version)
      )
      return(unchanged_result(reason))
    }
  }

  resolved <- tryCatch(
    .resolve_components(package, pkg_dir, ext),
    error = function(error) error
  )
  if (inherits(resolved, "error")) {
    return(resolution_failure(resolved))
  }
  components <- resolved$components
  inventory <- resolved$inventory
  component <- components[[1L]]
  .validate_archive_metadata(component)
  state <- new.env(parent = emptyenv())
  state$installed <- list()
  state$unchanged <- list()
  state$failed <- list()
  state$skipped <- list()
  state$visiting <- character()

  install_one <- function(item) {
    stem <- item$stem
    base_name <- item$package
    version_text <- item$version
    archive <- item$path

    if (base_name %in% state$visiting) {
      state$failed[[stem]] <- "Circular local dependency"
      return(FALSE)
    }
    # Metadata was read before installation, so the DESCRIPTION version remains
    # authoritative even when the filename has no version or disagrees with it.
    installed_version <- tryCatch(
      utils::packageVersion(base_name, lib.loc = component_libraries),
      error = function(e) NULL
    )
    keep_installed <- !is.null(installed_version) && (
      identical(upgrade, "never") ||
        (identical(upgrade, "newer") &&
           installed_version >= base::package_version(version_text))
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
      return(FALSE)
    }

    archive_kind <- tryCatch(.classify_local_archive(archive, item$ext), error = identity)
    if (inherits(archive_kind, "error")) {
      state$failed[[stem]] <- conditionMessage(archive_kind)
      return(FALSE)
    }
    binary_zip <- identical(archive_kind, "win.binary")
    if (binary_zip && .Platform$OS.type != "windows") {
      state$failed[[stem]] <- "Windows binary ZIP packages can only be installed on Windows"
      return(FALSE)
    }
    package_root <- tryCatch(
      .find_archive_root(extracted, archive, allow_flat = binary_zip),
      error = identity
    )
    if (inherits(package_root, "error")) {
      state$failed[[stem]] <- conditionMessage(package_root)
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
          return(FALSE)
        }
      }
    }
    for (dependency in local_dependencies) {
      matches <- which(inventory$packages == dependency)
      if (length(matches) > 1L) {
        state$failed[[stem]] <- .bb_trf(
          "More than one local archive is available for dependency %s.", dependency
        )
        return(FALSE)
      }
      if (!isTRUE(install_one(inventory$entries[[matches[[1L]]]]))) return(FALSE)
    }

    external <- setdiff(dependencies, inventory$packages)
    missing_external <- external[!vapply(
      external, requireNamespace, logical(1), quietly = TRUE,
      lib.loc = dependency_libraries
    )]
    if (length(missing_external) > 0L && cran_deps != "install") {
      detail <- paste(missing_external, collapse = ", ")
      if (cran_deps == "skip") {
        state$skipped[[stem]] <- paste("Missing non-local dependencies:", detail)
      } else {
        state$failed[[stem]] <- paste("Missing non-local dependencies:", detail)
      }
      return(FALSE)
    }
    if (length(missing_external) > 0L) {
      invalid_repos <- is.null(repos) || length(repos) == 0L ||
        all(is.na(repos) | !nzchar(repos) | repos == "@CRAN@")
      if (invalid_repos) {
        state$failed[[stem]] <- "Cannot install dependencies without a configured repository"
        return(FALSE)
      }
      for (dependency in missing_external) {
        install_error <- tryCatch({
          # NA installs Depends, Imports and LinkingTo, not Suggests.
          .with_install_library_path(
            dependency_libraries,
            utils::install.packages(
              dependency, dependencies = NA, repos = repos, lib = lib
            )
          )
          NULL
        }, error = identity)
        if (inherits(install_error, "error")) {
          state$failed[[dependency]] <- conditionMessage(install_error)
        }
      }
      still_missing <- missing_external[!vapply(
        missing_external, requireNamespace, logical(1), quietly = TRUE,
        lib.loc = dependency_libraries
      )]
      if (length(still_missing) > 0L) {
        state$failed[[stem]] <- paste(
          "Could not install non-local dependencies:",
          paste(still_missing, collapse = ", ")
        )
        return(FALSE)
      }
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
        "Installation could not be verified"
      }
      return(FALSE)
    }

    state$installed[[stem]] <- "Installed successfully"
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
