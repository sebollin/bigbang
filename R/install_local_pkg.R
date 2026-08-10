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

#' Install a local package together with its dependencies
#'
#' Installs a package from a local archive. Dependencies available as local
#' archives are installed recursively; missing non-local dependencies follow the
#' explicit `cran_deps` policy. ZIP archives containing `Meta/package.rds` are
#' treated as Windows binaries, while other ZIP archives are unpacked and
#' installed as source packages.
#'
#' @param package Character. Package file name without extension
#'   (for example, `"uspr_0.8.5"`).
#' @param pkg_dir Character. Directory containing local archives.
#' @param ext Character. Archive extension.
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
#'
#' @section Installation:
#' This function installs packages into the user's active R library. Installation
#' occurs only when the user calls the function; loading `bigbang` never installs
#' packages. With the default `cran_deps = "skip"`, it does not access the network.
#'
#' @return Invisibly, a list describing installed, unchanged, failed, and
#'   skipped packages. Components that an upgrade policy left in place are
#'   reported in `unchanged`, not in `installed`.
#' @seealso [create_metapackage()] for generating a meta-package with an explicit
#'   component installer.
#' @export
install_local_pkg <- function(
  package,
  pkg_dir,
  ext = ".tar.gz",
  repos = getOption("repos"),
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive()),
  # Added after 0.1.0, so they go last: a positional call written against
  # 0.1.0 passed verbose sixth, and must keep meaning verbose.
  force = FALSE,
  upgrade = c("newer", "always", "never")
) {
  cran_deps <- match.arg(cran_deps)
  upgrade <- .resolve_upgrade_policy(force, upgrade, missing(upgrade))
  state <- new.env(parent = emptyenv())
  state$installed <- list()
  state$unchanged <- list()
  state$failed <- list()
  state$skipped <- list()
  state$visiting <- character()

  archive_names <- list.files(pkg_dir)
  archive_names <- archive_names[endsWith(archive_names, ext)]
  local_stems <- substr(archive_names, 1L, nchar(archive_names) - nchar(ext))
  local_base_names <- sub("_.*", "", local_stems)

  install_one <- function(stem) {
    base_name <- sub("_.*", "", stem)
    version_text <- sub("^[^_]+_", "", stem)
    archive <- file.path(pkg_dir, paste0(stem, ext))

    if (base_name %in% state$visiting) {
      state$failed[[stem]] <- "Circular local dependency"
      return(FALSE)
    }
    if (!file.exists(archive)) {
      state$failed[[stem]] <- paste("Package archive does not exist:", archive)
      return(FALSE)
    }
    # Preserve the inexpensive early-exit behavior for an already installed
    # package. Full archive metadata validation still happens before any
    # installation when the policy decides that work is needed.
    installed_version <- tryCatch(
      utils::packageVersion(base_name), error = function(e) NULL
    )
    keep_installed <- !is.null(installed_version) && (
      identical(upgrade, "never") ||
        (identical(upgrade, "newer") &&
           installed_version >= base::package_version(version_text))
    )
    if (keep_installed) {
      if (isTRUE(verbose)) {
        newer <- installed_version > base::package_version(version_text)
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
      state$unchanged[[stem]] <- if (identical(upgrade, "never")) {
        .bb_tr("Kept installed version because upgrade = 'never'")
      } else {
        .bb_tr("Already installed")
      }
      return(TRUE)
    }
    state$visiting <- c(state$visiting, base_name)
    on.exit(state$visiting <- setdiff(state$visiting, base_name), add = TRUE)

    extracted <- tempfile("bigbang-archive-")
    dir.create(extracted)
    on.exit(safe_unlink(extracted, recursive = TRUE), add = TRUE)
    extraction_error <- tryCatch({
      .extract_archive_checked(archive, ext, extracted)
      NULL
    }, error = identity)
    if (inherits(extraction_error, "error")) {
      state$failed[[stem]] <- conditionMessage(extraction_error)
      return(FALSE)
    }

    archive_kind <- tryCatch(.classify_local_archive(archive, ext), error = identity)
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
    descriptions <- file.path(package_root, "DESCRIPTION")
    desc <- read.dcf(
      descriptions, fields = c("Package", "Version", "Depends", "Imports", "LinkingTo")
    )
    if (nrow(desc) == 0L) {
      state$failed[[stem]] <- .bb_trf(
        "Archive %s must declare non-empty Package and Version fields.", archive
      )
      return(FALSE)
    }
    declared_package <- if ("Package" %in% colnames(desc)) {
      trimws(unname(desc[1L, "Package"]))
    } else {
      NA_character_
    }
    declared_version <- if ("Version" %in% colnames(desc)) {
      trimws(unname(desc[1L, "Version"]))
    } else {
      NA_character_
    }
    if (is.na(declared_package) || is.na(declared_version) ||
          !identical(declared_package, base_name)) {
      state$failed[[stem]] <- .bb_trf(
        "Archive %s declares package %s, but its filename names %s.",
        archive, declared_package, base_name
      )
      return(FALSE)
    }
    if (!.version_matches(declared_version, version_text)) {
      state$failed[[stem]] <- .bb_trf(
        "Archive %s declares version %s, but its filename names version %s.",
        archive, declared_version, version_text
      )
      return(FALSE)
    }
    version_text <- declared_version

    installed_version <- tryCatch(
      utils::packageVersion(base_name), error = function(e) NULL
    )
    keep_installed <- !is.null(installed_version) && (
      identical(upgrade, "never") ||
        (identical(upgrade, "newer") &&
           installed_version >= base::package_version(version_text))
    )
    if (keep_installed) {
      if (isTRUE(verbose)) {
        newer <- installed_version > base::package_version(version_text)
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
      state$unchanged[[stem]] <- if (identical(upgrade, "never")) {
        .bb_tr("Kept installed version because upgrade = 'never'")
      } else {
        .bb_tr("Already installed")
      }
      return(TRUE)
    }
    dependency_fields <- intersect(
      c("Depends", "Imports", "LinkingTo"), colnames(desc)
    )
    dependencies <- if (length(dependency_fields) == 0L) {
      character()
    } else {
      dependency_values <- unname(desc[1L, dependency_fields])
      dependency_values <- dependency_values[
        !is.na(dependency_values) & nzchar(dependency_values)
      ]
      if (length(dependency_values) == 0L) {
        character()
      } else {
        unlist(
          strsplit(paste(dependency_values, collapse = ","), ","),
          use.names = FALSE
        )
      }
    }
    dependencies <- trimws(gsub("\\s*\\([^)]*\\)", "", dependencies))
    dependencies <- unique(dependencies[nzchar(dependencies) & dependencies != "R"])

    local_dependencies <- intersect(dependencies, local_base_names)
    for (dependency in local_dependencies) {
      dependency_stem <- local_stems[match(dependency, local_base_names)]
      if (!isTRUE(install_one(dependency_stem))) return(FALSE)
    }

    external <- setdiff(dependencies, local_base_names)
    missing_external <- external[!vapply(
      external, requireNamespace, logical(1), quietly = TRUE
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
          utils::install.packages(dependency, dependencies = NA, repos = repos)
          NULL
        }, error = identity)
        if (inherits(install_error, "error")) {
          state$failed[[dependency]] <- conditionMessage(install_error)
        }
      }
      still_missing <- missing_external[!vapply(
        missing_external, requireNamespace, logical(1), quietly = TRUE
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
    install_type <- if (binary_zip) "win.binary" else "source"
    install_error <- tryCatch({
      utils::install.packages(
        install_target,
        repos = NULL,
        type = install_type,
        dependencies = FALSE
      )
      NULL
    }, error = identity)
    verified <- !inherits(install_error, "error") && tryCatch(
      utils::packageVersion(base_name) == base::package_version(version_text),
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
      message(.bb_trf("Installed local package: %s", stem))
    }
    TRUE
  }

  install_one(package)
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
