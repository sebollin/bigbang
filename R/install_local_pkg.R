.classify_local_archive <- function(archive, ext) {
  if (!identical(tolower(ext), ".zip")) return("source")
  members <- gsub(
    "\\\\", "/", utils::unzip(archive, list = TRUE)$Name, fixed = TRUE
  )
  if (!any(grepl("(^|/)DESCRIPTION$", members))) {
    stop("The ZIP archive does not contain a DESCRIPTION file: ", archive,
         domain = "R-bigbang")
  }
  if (any(grepl("(^|/)Meta/package\\.rds$", members))) {
    "win.binary"
  } else {
    "source.zip"
  }
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
#' @param verbose Logical. Whether to emit progress and summary messages. The
#'   default follows `getOption("bigbang.verbose", interactive())`.
#'
#' @return Invisibly, a list describing installed, failed, and skipped packages.
#' @export
install_local_pkg <- function(
  package,
  pkg_dir,
  ext = ".tar.gz",
  repos = getOption("repos"),
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive())
) {
  cran_deps <- match.arg(cran_deps)
  state <- new.env(parent = emptyenv())
  state$installed <- list()
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
    already_installed <- tryCatch(
      utils::packageVersion(base_name) >= base::package_version(version_text),
      error = function(e) FALSE
    )
    if (already_installed) {
      state$installed[[stem]] <- "Already installed"
      return(TRUE)
    }

    state$visiting <- c(state$visiting, base_name)
    on.exit(state$visiting <- setdiff(state$visiting, base_name), add = TRUE)

    extracted <- tempfile("bigbang-archive-")
    dir.create(extracted)
    on.exit(safe_unlink(extracted, recursive = TRUE), add = TRUE)
    extraction_error <- tryCatch({
      if (ext %in% c(".tar.gz", ".tar")) {
        utils::untar(archive, exdir = extracted)
      } else if (identical(tolower(ext), ".zip")) {
        utils::unzip(archive, exdir = extracted)
      } else {
        stop("Unsupported archive format: ", ext, domain = "R-bigbang")
      }
      NULL
    }, error = identity)
    if (inherits(extraction_error, "error")) {
      state$failed[[stem]] <- conditionMessage(extraction_error)
      return(FALSE)
    }

    descriptions <- list.files(
      extracted, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE
    )
    if (length(descriptions) != 1L) {
      state$failed[[stem]] <- paste(
        "Expected one DESCRIPTION in archive; found", length(descriptions)
      )
      return(FALSE)
    }
    desc <- read.dcf(descriptions, fields = c("Depends", "Imports", "LinkingTo"))
    dependencies <- unlist(
      strsplit(paste(desc[!is.na(desc)], collapse = ","), ","),
      use.names = FALSE
    )
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
          utils::install.packages(dependency, dependencies = TRUE, repos = repos)
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
    install_target <- if (identical(archive_kind, "source.zip")) {
      dirname(descriptions[[1L]])
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
      utils::packageVersion(base_name) >= base::package_version(version_text),
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
      message("Installed local package: ", stem, domain = "R-bigbang")
    }
    TRUE
  }

  install_one(package)
  if (isTRUE(verbose) && length(state$failed) > 0L) {
    message("Packages that failed: ", paste(names(state$failed), collapse = ", "),
            domain = "R-bigbang")
  }
  if (isTRUE(verbose) && length(state$skipped) > 0L) {
    message("Packages skipped by the offline policy: ",
            paste(names(state$skipped), collapse = ", "), domain = "R-bigbang")
  }
  invisible(structure(
    list(
      installed = state$installed,
      failed = state$failed,
      skipped = state$skipped
    ),
    class = "bigbang_install_result"
  ))
}

#' Deprecated Spanish alias for `install_local_pkg()`
#'
#' @param nombre_paquete Character package archive stem.
#' @param ruta_instalables Directory containing local archives.
#' @inheritParams install_local_pkg
#' @return The result of [install_local_pkg()].
#' @keywords internal
#' @export
install_loc_pkg_w_dep <- function(
  nombre_paquete,
  ruta_instalables,
  ext = ".tar.gz",
  repos = getOption("repos"),
  cran_deps = c("skip", "error", "install"),
  verbose = getOption("bigbang.verbose", interactive())
) {
  if (isTRUE(getOption("bigbang.deprecation_warnings", interactive()))) {
    .Deprecated("install_local_pkg", package = "bigbang")
  }
  result <- install_local_pkg(
    package = nombre_paquete,
    pkg_dir = ruta_instalables,
    ext = ext,
    repos = repos,
    cran_deps = cran_deps,
    verbose = verbose
  )
  # Preserve the historical field names for existing MIDES callers.
  invisible(list(
    instalados = result$installed,
    fallidos = result$failed,
    omitidos = result$skipped
  ))
}
