.render_install_engine <- function(name, packages, pkg_dir, ext) {
  glue::glue('

.bigbang_abort <- function(class, message, ...) {{
  condition <- structure(
    c(list(message = message, call = NULL), list(...)),
    class = c(class, "bigbang_error", "error", "condition")
  )
  stop(condition)
}}

#\' Read dependencies from a local package archive
#\'
#\' Extracts into an owned temporary directory and reads Depends, Imports, and
#\' LinkingTo from DESCRIPTION. It does not install or load the package.
#\'
#\' @param package Character archive stem including the version.
#\' @param pkg_dir Character directory containing local archives.
#\' @param ext Character archive extension.
#\'
#\' @return A character vector of dependency names.
#\' @keywords internal

read_archive_dependencies <- function(package, pkg_dir, ext = ".tar.gz") {{
  archive <- file.path(pkg_dir, paste0(package, ext))
  if (!file.exists(archive)) {{
    stop(.meta_trf("Package archive does not exist: %s", archive),
         call. = FALSE)
  }}

  temp_dir <- tempfile("bigbang-deps-")
  dir.create(temp_dir)
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  switch(
    ext,
    ".tar.gz" = utils::untar(archive, exdir = temp_dir),
    ".tar" = utils::untar(archive, exdir = temp_dir),
    ".zip" = utils::unzip(archive, exdir = temp_dir),
    stop(.meta_trf("Unsupported archive format: %s", ext), call. = FALSE)
  )

  description_file <- list.files(
    temp_dir, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE
  )
  if (length(description_file) != 1L) {{
    stop(.meta_trf(
      "Expected one DESCRIPTION in %s; found %d.",
      archive, length(description_file)
    ), call. = FALSE)
  }}

  desc <- read.dcf(description_file, fields = c("Depends", "Imports", "LinkingTo"))
  dependencies <- unlist(strsplit(paste(desc[!is.na(desc)], collapse = ","), ","),
                         use.names = FALSE)
  dependencies <- trimws(gsub("\\\\s*\\\\([^)]*\\\\)", "", dependencies))
  unique(dependencies[nzchar(dependencies) & dependencies != "R"])
}}


#\' Classify a local package archive
#\'
#\' A ZIP containing `Meta/package.rds` is a Windows binary package. Other ZIP
#\' archives are treated as source archives and are unpacked before installation.
#\'
#\' @return One of `"source"`, `"source.zip"`, or `"win.binary"`.
#\' @keywords internal
classify_package_archive <- function(archive, ext) {{
  if (!identical(tolower(ext), ".zip")) return("source")

  members <- utils::unzip(archive, list = TRUE)$Name
  members <- gsub("\\\\", "/", members, fixed = TRUE)
  has_description <- any(grepl("(^|/)DESCRIPTION$", members))
  if (!has_description) {{
    stop(.meta_trf(
      "The ZIP archive does not contain a DESCRIPTION file: %s", archive
    ), call. = FALSE)
  }}
  if (any(grepl("(^|/)Meta/package\\\\.rds$", members))) {{
    return("win.binary")
  }}
  "source.zip"
}}


#\' Install a local package with its dependencies
#\'
#\' Checks the installed version, resolves non-local dependencies according to
#\' policy, and installs the local archive. Local dependencies are installed by
#\' the outer topological loop, so this helper is not recursive.
#\'
#\' @param package Character archive stem including the version.
#\' @param pkg_dir Character directory containing local archives.
#\' @param ext Character archive extension.
#\' @param repos Character repositories for non-local dependencies.
#\' @param cran_deps Character missing-dependency policy: `"skip"` and `"error"`
#\'   never access the network; `"install"` uses `repos`.
#\'
#\' @return A list with installation status and detected dependencies.
#\' @keywords internal
install_local_archive <- function(package, pkg_dir, ext = ".tar.gz",
                                   repos = getOption("repos"),
                                   cran_deps = c("skip", "error", "install")) {{
  cran_deps <- match.arg(cran_deps)
  archive <- file.path(pkg_dir, paste0(package, ext))
  if (!file.exists(archive)) {{
    return(list(
      success = FALSE,
      message = .meta_trf("Package archive does not exist: %s", archive)
    ))
  }}

  base_name <- sub("_.*", "", package)
  version <- sub("^[^_]+_", "", package)
  dependencies <- tryCatch(
    read_archive_dependencies(package, pkg_dir, ext),
    error = function(e) e
  )
  if (inherits(dependencies, "error")) {{
    return(list(success = FALSE, message = conditionMessage(dependencies)))
  }}

  already_installed <- tryCatch(
    utils::packageVersion(base_name) >= base::package_version(version),
    error = function(e) FALSE
  )
  if (already_installed) {{
    message(.meta_trf(
      "Package %s (version %s) is already installed.", base_name, version
    ))
    return(list(
      success = TRUE,
      message = .meta_tr("Already installed"),
      installed = stats::setNames(list(.meta_tr("Already installed")), package),
      dependencies = dependencies
    ))
  }}

  local_files <- list.files(pkg_dir)
  local_files <- local_files[endsWith(local_files, ext)]
  local_names <- sub("_.*", "", substr(
    local_files, 1L, nchar(local_files) - nchar(ext)
  ))

  # Local dependencies are installed once by the outer topological loop. This
  # branch resolves only dependencies not provided by local archives.
  missing_nonlocal <- setdiff(dependencies, local_names)
  missing_nonlocal <- missing_nonlocal[!vapply(
    missing_nonlocal, requireNamespace, logical(1), quietly = TRUE
  )]
  if (length(missing_nonlocal) > 0L && cran_deps != "install") {{
    detail <- paste(missing_nonlocal, collapse = ", ")
    if (cran_deps == "skip") {{
      return(list(
        success = FALSE,
        skipped = TRUE,
        message = .meta_trf("Skipped because non-local dependencies are missing: %s", detail),
        missing_dependencies = missing_nonlocal,
        dependencies = dependencies
      ))
    }}
    return(list(
      success = FALSE,
      message = .meta_trf("Missing non-local dependencies: %s", detail),
      missing_dependencies = missing_nonlocal,
      dependencies = dependencies
    ))
  }}

  if (length(missing_nonlocal) > 0L && cran_deps == "install") {{
    invalid_repos <- is.null(repos) || length(repos) == 0L ||
      all(is.na(repos) | !nzchar(repos) | repos == "@CRAN@")
    if (invalid_repos) {{
      detail <- paste(missing_nonlocal, collapse = ", ")
      return(list(
        success = FALSE,
        message = .meta_trf(
          "Cannot install non-local dependencies without a configured repository: %s",
          detail
        ),
        missing_dependencies = missing_nonlocal,
        dependencies = dependencies
      ))
    }}
    for (dep in missing_nonlocal) {{
      message(.meta_trf("Installing non-local dependency: %s", dep))
      tryCatch(
        utils::install.packages(dep, dependencies = TRUE, repos = repos),
        error = function(e) warning(conditionMessage(e), call. = FALSE)
      )
    }}
  }}

  missing <- dependencies[!vapply(
    dependencies, requireNamespace, logical(1), quietly = TRUE
  )]
  if (length(missing) > 0L) {{
    return(list(
      success = FALSE,
      message = .meta_trf(
        "Dependencies are not installed: %s", paste(missing, collapse = ", ")
      ),
      missing_dependencies = missing,
      dependencies = dependencies
    ))
  }}

  archive_type <- tryCatch(
    classify_package_archive(archive, ext),
    error = function(e) e
  )
  if (inherits(archive_type, "error")) {{
    return(list(success = FALSE, message = conditionMessage(archive_type)))
  }}
  if (identical(archive_type, "win.binary") && .Platform$OS.type != "windows") {{
    return(list(
      success = FALSE,
      message = .meta_tr("Windows binary ZIP packages can only be installed on Windows.")
    ))
  }}

  install_target <- archive
  install_type <- if (identical(archive_type, "win.binary")) "win.binary" else "source"
  if (identical(archive_type, "source.zip")) {{
    source_dir <- tempfile("bigbang-source-zip-")
    dir.create(source_dir)
    on.exit(safe_unlink(source_dir, recursive = TRUE), add = TRUE)
    utils::unzip(archive, exdir = source_dir)
    descriptions <- list.files(
      source_dir, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE
    )
    if (length(descriptions) != 1L) {{
      return(list(
        success = FALSE,
        message = .meta_trf(
          "Expected one DESCRIPTION in source ZIP; found %d", length(descriptions)
        )
      ))
    }}
    install_target <- dirname(descriptions[[1L]])
  }}

  install_error <- NULL
  tryCatch(
    utils::install.packages(
      install_target, repos = NULL, type = install_type, dependencies = FALSE
    ),
    error = function(e) install_error <<- conditionMessage(e)
  )

  installed <- is.null(install_error) && tryCatch(
    utils::packageVersion(base_name) >= base::package_version(version),
    error = function(e) FALSE
  )
  if (!installed) {{
    detail <- if (is.null(install_error)) {{
      .meta_tr("Installation could not be verified")
    }} else {{
      install_error
    }}
    return(list(
      success = FALSE,
      message = detail,
      failed = stats::setNames(list(detail), package),
      dependencies = dependencies
    ))
  }}

  message(.meta_trf("Installed package %s successfully.", package))
  list(
    success = TRUE,
    message = .meta_tr("Installed successfully"),
    installed = stats::setNames(list(.meta_tr("Installed successfully")), package),
    dependencies = dependencies
  )
}}


#\' Detect cycles in a dependency graph
#\'
#\' Analyzes an adjacency matrix and returns circular package dependencies.
#\'
#\' @param adjacency Matrix. A value of 1 means the row package depends on the
#\'   column package.
#\'
#\' @return A list of integer vectors, one per cycle.
#\'
#\' @details
#\' Uses depth-first search (DFS).
#\'
#\' @examples
#\' \\dontrun{{
#\'   # Create an adjacency matrix containing a cycle
#\'   mat <- matrix(c(0,1,0, 0,0,1, 1,0,0), nrow=3, byrow=TRUE)
#\'   rownames(mat) <- colnames(mat) <- c("pkg1", "pkg2", "pkg3")
#\'
#\'   # Detect cycles
#\'   cycles <- detect_cycles(mat)
#\'   print(cycles)
#\' }}
#\'
#\' @keywords internal
detect_cycles <- function(adjacency) {{
  package_count <- nrow(adjacency)
  visited <- rep(FALSE, package_count)
  rec_stack <- rep(FALSE, package_count)
  cycles <- list()

  dfs <- function(v, path = integer(0)) {{
    if (rec_stack[v]) {{
      # Cycle found
      cycle_start <- match(v, path)
      if (!is.na(cycle_start)) {{
        cycles <<- c(cycles, list(path[cycle_start:length(path)]))
      }}
      return(TRUE)
    }}

    if (visited[v]) return(FALSE)

    visited[v] <<- TRUE
    rec_stack[v] <<- TRUE
    path <- c(path, v)

    for (u in which(adjacency[v, ] == 1)) {{
      if (dfs(u, path)) return(TRUE)
    }}

    rec_stack[v] <<- FALSE
    return(FALSE)
  }}

  for (i in 1:package_count) {{
    if (!visited[i]) dfs(i)
  }}

  return(cycles)
}}



#\' Build a dependency graph from local packages
#\'
#\' Reads each archive DESCRIPTION without installing or loading packages and
#\' builds the adjacency matrix used for installation ordering.
#\'
#\' @param packages Character archive stems including versions.
#\' @param pkg_dir Character archive directory.
#\' @param ext Character archive extension.
#\'
#\' @return An adjacency matrix.
#\'
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   adj <- build_dependency_graph(
#\'     packages = c("uspr_0.8.6", "conexiones_0.8.3"),
#\'     pkg_dir = "X:/path",
#\'     ext = ".tar.gz"
#\'   )
#\'   print(adj)
#\' }}

build_dependency_graph <- function(packages, pkg_dir, ext) {{
  package_count <- length(packages)
  adjacency <- base::matrix(0, nrow = package_count, ncol = package_count)
  rownames(adjacency) <- colnames(adjacency) <- packages

  for (package in packages) {{
    deps <- read_archive_dependencies(package, pkg_dir, ext)
    local_deps <- intersect(deps, sub("_.*", "", packages))
    for (dep in local_deps) {{
      dependency_index <- which(sub("_.*", "", packages) == dep)
      if (length(dependency_index) > 0) {{
        package_index <- which(packages == package)
        adjacency[package_index, dependency_index[1]] <- 1
      }}
    }}
  }}


  # Check cycles
  cycles <- detect_cycles(adjacency)
  if (length(cycles) > 0) {{
    # Convert indices to package names
    named_cycles <- lapply(cycles, function(cycle) {{
      packages[cycle]
    }})

    cycle_text <- paste(
      vapply(named_cycles, paste, character(1), collapse = " -> "),
      collapse = "; "
    )
    .bigbang_abort(
      "bigbang_error_cycle",
      .meta_trf(
        "Circular dependencies detected: %s. A clean installation has no valid topological order.",
        cycle_text
      ),
      cycles = named_cycles
    )
  }}

  return(adjacency)
}}

#\' Topologically sort a dependency graph
#\'
#\' Uses DFS on the adjacency matrix to find an installation order.
#\'
#\' @param adjacency Matrix where 1 means the row depends on the column.
#\'
#\' @return An integer vector containing the topological order.
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   mat <- base::matrix(c(0,1,0,0), nrow=2, byrow=TRUE)
#\'   rownames(mat) <- colnames(mat) <- c("conexiones_0.8.3", "uspr_0.8.6")
#\'   ord <- topological_order(mat)
#\'   print(ord)
#\' }}

topological_order <- function(adjacency) {{
  package_count <- nrow(adjacency)
  visited <- rep(FALSE, package_count)
  order <- integer(0)

  dfs <- function(v) {{
    visited[v] <<- TRUE
    for (u in which(adjacency[v, ] == 1)) {{
      if (!visited[u]) {{
        dfs(u)
      }}
    }}
    order <<- c(order, v)
  }}

  for (i in seq_len(package_count)) {{
    if (!visited[i]) {{
      dfs(i)
    }}
  }}

  return(order)

}}

#\' Install local packages in dependency order
#\'
#\' Builds the graph, computes its topological order, and installs each package
#\' exactly once.
#\'
#\' @param packages Character archive stems including versions.
#\' @param pkg_dir Character archive directory.
#\' @param ext Character archive extension.
#\' @param verbose Logical progress toggle.
#\' @return Invisibly, installation, failure, skip, and order information.
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   install_packages_in_order(
#\'     packages = c("uspr_1.0.0", "conexiones_0.8.3"),
#\'     pkg_dir = "X:/path"
#\'   )
#\' }}


install_packages_in_order <- function(packages, pkg_dir, ext,
                                      verbose = TRUE,
                                      repos = getOption("repos"),
                                      cran_deps = c("skip", "error", "install")) {{
  cran_deps <- match.arg(cran_deps)
  adjacency <- build_dependency_graph(packages, pkg_dir, ext)
  install_order <- topological_order(adjacency)

  installed_packages <- list()
  failed_packages <- list()
  skipped_packages <- list()
  pb <- NULL

  total_pkgs <- length(packages)
  if (verbose && interactive() && total_pkgs > 1) {{
    message(.meta_trf("Starting installation of %d packages", total_pkgs))
    pb <- utils::txtProgressBar(min = 0, max = total_pkgs, style = 3)
    on.exit(close(pb), add = TRUE)
  }}

  for (i in seq_along(install_order)) {{
    idx <- install_order[i]
    package <- packages[idx]

    result <- tryCatch(
      install_local_archive(
        package, pkg_dir, ext, repos = repos, cran_deps = cran_deps
      ),
      error = function(e) list(success = FALSE, message = conditionMessage(e))
    )

    if (isTRUE(result$success)) {{
      installed_packages[[package]] <- result$message
    }} else if (isTRUE(result$skipped)) {{
      skipped_packages[[package]] <- result$message
      warning(.meta_trf("Skipped %s: %s", package, result$message), call. = FALSE)
    }} else {{
      failed_packages[[package]] <- result$message
      warning(.meta_trf("Installation failed for %s: %s", package, result$message),
              call. = FALSE, immediate. = TRUE)
    }}

    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  }}

  if (!is.null(pb)) message(.meta_tr("Installation complete."))

  # SAFETY (2026-07): no cleanup is performed relative to the current directory.

  invisible(list(
    installed = installed_packages,
    failed = failed_packages,
    skipped = skipped_packages,
    order = packages[install_order]
  ))
}}

# The historical duplicate load-all definition was removed.

#\' List all metapackage dependencies
#\'
#\' Returns component names and dependencies read from their archives.
#\'
#\' @param pkg_dir Character archive directory.
#\' @param ext Character archive extension.
#\' @return A sorted character vector of dependency names.
#\' @export
{name}_deps <- function(
    pkg_dir = {paste(deparse(pkg_dir), collapse = "")},
    ext = {paste(deparse(ext), collapse = "")}) {{
  packages <- {.r_literal(packages)}
  deps <- unlist(lapply(
    packages, read_archive_dependencies,
    pkg_dir = pkg_dir, ext = ext
  ), use.names = FALSE)
  sort(unique(c(sub("_.*", "", packages), deps)))
}}
')
}
#' Render generated metapackage R files
#'
#' @param name Character metapackage name.
#' @param packages Character component names without versions.
#' @param pkg_dir Character archive directory.
#' @param archive_stems Character archive stems including versions.
#' @param ext Character archive extension.
#' @param dest_dir Character R output directory.
#' @param implicit_deps Character implicit dependencies.
#' @param reexport Logical re-export toggle.
#' @param authors Character Authors@R expression.
#' @param description Character metapackage description.
#' @param license Character license declaration.
#' @param verbose Logical debug toggle.
#'
#' @return Invisible character vector of created paths.
#' @noRd
write_metapackage_files <- function(
    name,
    packages,
    pkg_dir,
    archive_stems,
    ext = ".tar.gz",
    dest_dir = "R",
    implicit_deps = NULL,
    reexport = FALSE,
    authors = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
    description = "Local Package Metapackage",
    license = "MIT + file LICENSE",
    verbose = FALSE
) {

  log_debug <- function(debug_message) {
    if (verbose) message(paste0("DEBUG: ", debug_message))
  }

  log_debug("Preparing template data")

  # Prepare shared template data once.
  template_data <- list(
    name = name,
    package_list = .r_literal(packages),
    local_packages = .r_literal(archive_stems),
    install_path = paste(deparse(pkg_dir), collapse = ""),
    extension = paste(deparse(ext), collapse = ""),
    implicit_deps = if (!is.null(implicit_deps)) paste(implicit_deps, collapse = ", ") else ""
  )

  if (verbose) {
    log_debug("Template values:")
    log_debug(paste("name:", template_data$name))
    log_debug(paste("package_list:", template_data$package_list))
    log_debug(paste("local_packages:", template_data$local_packages))
    log_debug(paste("install_path:", template_data$install_path))
    log_debug(paste("extension:", template_data$extension))
  }


  # Templates for the generated runtime files.
  templates <- list(
    attach = '
utils::globalVariables(".pkgs")
.pkgs <- {{{ package_list }}}

attach_installed_packages <- function(pkgs, warn_missing = TRUE) {
  already_attached <- gsub("^package:", "", search())
  to_load <- setdiff(pkgs, already_attached)
  missing <- to_load[!vapply(to_load, requireNamespace, logical(1), quietly = TRUE)]
  if (warn_missing && length(missing) > 0) {
    warning(gettextf(
      "Not installed: %s. Run {{ name }}_install() to install them.",
      paste(missing, collapse = ", "), domain = "R-{{ name }}"
    ), call. = FALSE)
  }
  to_load <- setdiff(to_load, missing)
  if (length(to_load) > 0) {
    suppressPackageStartupMessages(
      lapply(to_load, library, character.only = TRUE)
    )
  }
  invisible(list(attached = to_load, missing = missing))
}

#\' Attach installed local packages
#\'
#\' Attaches installed components with `library()`. It never installs packages;
#\' use `{{ name }}_install()` explicitly for installation.
#\'
#\' @param pkgs Character vector. Packages to attach; defaults to `.pkgs`.
#\'
#\' @return Invisibly, attachment information.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   {{ name }}_attach()
#\' }
{{ name }}_attach <- function(pkgs = .pkgs) {
  attach_installed_packages(pkgs, warn_missing = TRUE)
}

#\' Install local metapackage components
#\'
#\' Installs local archives in topological order and then attaches them. Installation
#\' is explicit and never occurs from a startup hook.
#\'
#\' @param pkg_dir Character archive directory.
#\' @param ext Character archive extension.
#\' @param cran_deps Character missing non-local dependency policy.
#\' @param repos Character repositories used only by `cran_deps = "install"`.
#\' @param verbose Logical progress toggle.
#\'
#\' @return Invisibly, structured installation results.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   {{ name }}_install()
#\' }
{{ name }}_install <- function(pkg_dir = {{{ install_path }}},
                               ext = {{{ extension }}},
                               cran_deps = c("skip", "error", "install"),
                               repos = getOption("repos"),
                               verbose = getOption("bigbang.verbose", interactive())) {
  cran_deps <- match.arg(cran_deps)
  packages <- {{{ local_packages }}}
  result <- install_packages_in_order(
    packages, pkg_dir, ext, verbose = verbose,
    repos = repos, cran_deps = cran_deps
  )
  if (length(result$failed) > 0) {
    details <- paste0(
      names(result$failed), ": ", unlist(result$failed, use.names = FALSE)
    )
    condition <- structure(
      list(
        message = gettextf(
          "Could not install all components: %s",
          paste(details, collapse = "; "), domain = "R-{{ name }}"
        ),
        call = NULL,
        failures = result$failed
      ),
      class = c("bigbang_error_install", "bigbang_error", "error", "condition")
    )
    stop(condition)
  }
  if (length(result$skipped) > 0L) {
    warning(gettextf(
      "Some components were skipped because non-local dependencies are missing: %s",
      paste(names(result$skipped), collapse = ", "), domain = "R-{{ name }}"
    ), call. = FALSE)
  }
  {{ name }}_attach(sub("_.*", "", packages))
  invisible(result)
}

#\' Deprecated alias for `{{ name }}_attach()`
#\'
#\' @return The result of `{{ name }}_attach()`.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   DADverse_load_all()
#\' }
{{ name }}_load_all <- function() {
  .Deprecated("{{ name }}_attach", package = "{{ name }}")
  {{ name }}_attach()
}

#\' Detach all metapackage components
#\'
#\' Detaches packages declared in `.pkgs` when present on the search path.
#\'
#\' @return Invisibly, `NULL`.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   DADverse_detach()
#\' }

{{ name }}_detach <- function() {
  search_entries <- paste0("package:", .pkgs)
  lapply(search_entries[search_entries %in% search()], detach, character.only = TRUE)
  invisible()
}

#\' List metapackage components
#\'
#\' @return A character vector of package names.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   DADverse_packages()
#\' }

{{ name }}_packages <- function() {
  .pkgs
}

#\' Attach all components without a preflight check
#\'
#\' Calls `library()` for every package in `.pkgs` and errors if one is missing.
#\'
#\' @return Invisibly, `NULL`.
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\'   DADverse_attach_all()
#\' }
{{ name }}_attach_all <- function() {
  lapply(.pkgs, library, character.only = TRUE)
  invisible()
}

',
utils = '
# SAFETY NOTE (2026-07): this metapackage does not define `clean_pkg_dirs`.
# That historical helper deleted cwd-relative directories and was removed.

.meta_tr <- function(message) {
  gettext(message, domain = "R-{{ name }}")
}

.meta_trf <- function(format, ...) {
  gettextf(format, ..., domain = "R-{{ name }}")
}


#\' Utilities for {{{ name }}}
#\'
#\' This script contains utility functions for the {{{ name }}} metapackage.
#\'
#\' @keywords internal
style_startup_text <- function(x) {
  # If not in RStudio, return x as is
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return(x)
  }
  if (!rstudioapi::isAvailable() || !rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  theme <- rstudioapi::getThemeInfo()
  if (isTRUE(theme$dark) && requireNamespace("crayon", quietly = TRUE)) crayon::white(x) else x
}

package_version <- function(x) {
  version <- base::unclass(utils::packageVersion(x))[[1]]
  if (length(version) > 3 && requireNamespace("crayon", quietly = TRUE)) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

startup_message <- function(...) {
  packageStartupMessage(style_startup_text(...))
}

#\' Generate an ASCII package banner
#\'
#\' @param name Character metapackage name.
#\' @param packages Character component names.
#\' @return The generated banner.
#\' @keywords internal
generate_ascii_banner <- function(name, packages = NULL) {
  width <- 60
  border <- paste0(rep("=", width), collapse = "")

  # Centered title
  title <- paste0(" ", name, " ")
  padding_length <- floor((width - nchar(title)) / 2)
  left_padding <- paste0(rep("-", padding_length), collapse = "")
  right_padding <- paste0(rep("-", width - padding_length - nchar(title)), collapse = "")
  title_line <- paste0(left_padding, title, right_padding)

  # Build the banner.
  banner <- c(
    border,
    title_line,
    border
  )

  # Add component information
  if (!is.null(packages) && length(packages) > 0) {
    banner <- c(banner, "")
    banner <- c(banner, .meta_tr("Included packages:"))

    for (pkg in packages) {
      # Read the version when available
      version_text <- ""
      if (requireNamespace(pkg, quietly = TRUE)) {
        tryCatch({
          version <- utils::packageVersion(pkg)
          version_text <- paste0(" (v", version, ")")
        }, error = function(e) {})
      }

      banner <- c(banner, paste0("  * ", pkg, version_text))
    }

    banner <- c(banner, "", border)
  }

  paste(banner, collapse = "\\n")
}

#\' Remove an owned temporary path safely
#\'
#\' @description
#\' Wraps `unlink()` with conservative checks. Generated code uses it only for
#\' temporary paths created by the same operation.
#\'
#\' @param path Character path vector.
#\' @param recursive Logical recursive-removal flag.
#\' @param force Logical force flag.
#\' @param verify Logical safety-check flag.
#\'
#\' @return The `unlink()` status or invisible `FALSE` when blocked.
#\'
#\' @details
#\' Checks short paths, roots, UNC paths, protected directories, and non-temporary
#\' R package sources before permitting removal.
#\' \\itemize{
#\'   \\item Rejects suspiciously short paths and filesystem roots.
#\'   \\item Rejects system and development directories.
#\'   \\item Rejects non-temporary R package source directories.
#\' }
#\'
#\' @examples
#\' \\dontrun{
#\' # Remove an owned temporary file
#\' safe_unlink("temporary-file.txt")
#\' # Attempt safe directory removal
#\' safe_unlink("temporary-directory", recursive = TRUE, force = TRUE)
#\' }
#\'
#\' @keywords internal

safe_unlink <- function(path, recursive = FALSE, force = FALSE, verify = TRUE) {

  # Safety configuration
  MIN_PATH_LENGTH <- 3  # Very short paths are suspicious

  # System and development directories that must never be removed.
  PROTECTED_DIRS <- c(
    # Operating-system directories
    "bin", "boot", "dev", "etc", "home", "lib", "mnt", "opt", "proc", "root",
    "run", "sbin", "srv", "sys", "tmp", "usr", "var", "Program Files",
    "Windows", "Users", "System32", "AppData", "ProgramData",

    # R and development directories
    "library", "include", "share", "R", "Rtools", "Git", "src",

    # Version-control and configuration directories
    ".git", ".svn", ".hg", "node_modules"
  )

  # Potentially dangerous path patterns
  DANGEROUS_PATTERNS <- c(
    "^[A-Za-z]:\\\\\\\\$",  # C:\\, D:\\, etc.
    "^/$",             # Unix filesystem root
    "^\\\\\\\\\\\\\\\\",       # UNC paths such as \\\\server\\
    "^~$",             # Home directory
    "^\\\\.$",           # Current directory
    "^\\\\.\\\\.$"         # Parent directory
  )

  # Run conservative validation unless explicitly disabled.
  if (verify) {
    if (is.character(path) && length(path) > 0) {
      for (p in path) {
        # Reject suspiciously short paths such as roots.
        if (nchar(p) < MIN_PATH_LENGTH) {
          message(.meta_trf("SAFETY: Path is too short and may be dangerous: %s", p))
          return(invisible(FALSE))
        }

        # Reject known dangerous patterns.
        if (any(sapply(DANGEROUS_PATTERNS, function(pattern) grepl(pattern, p)))) {
          message(.meta_trf("SAFETY: Potentially dangerous path pattern: %s", p))
          return(invisible(FALSE))
        }

        # Apply directory-specific checks.
        if (dir.exists(p)) {
          # Never remove protected directories.
          if (basename(p) %in% PROTECTED_DIRS) {
            message(.meta_trf("SAFETY: Potentially important directory: %s", p))
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
              is_temp_pkg <- grepl("^00LOCK-|^\\\\.Rcheck$|^tmp|^temp", basename(p))

              if (!is_temp_pkg) {
                message(.meta_trf("SAFETY: Possible non-temporary R package directory: %s", p))
                return(invisible(FALSE))
              }
            }
          }
        }
      }
    }
  }

  # Delegate only after every check passes
  result <- unlink(path, recursive = recursive, force = force)

  # Report incomplete removal
  if (result != 0) {
    warning(.meta_trf("Could not remove completely: %s", paste(path, collapse = ", ")),
            call. = FALSE)
  }

  result
}

#\' Check whether one path is inside another
#\'
#\' @description
#\' Normalizes and compares paths without relying on partial prefix matches.
#\'
#\' @param inner_path Character candidate child path.
#\' @param outer_path Character candidate parent path.
#\'
#\' @return `TRUE` when `inner_path` is contained by `outer_path`.
#\'
#\' @examples
#\' \\dontrun{
#\' # Check a project file
#\' is_path_inside("R/file.R", getwd())
#\' # Check an owned temporary directory
#\' is_path_inside(file.path(tempdir(), "subdir"), tempdir())
#\' }
#\'
#\' @keywords internal

is_path_inside <- function(inner_path, outer_path) {
  # Normalize paths before comparing components.
  inner <- normalizePath(inner_path, mustWork = FALSE)
  outer <- normalizePath(outer_path, mustWork = FALSE)

  # Use one separator representation on Windows.
  if (.Platform$OS.type == "windows") {
    inner <- gsub("\\\\\\\\", "/", inner)
    outer <- gsub("\\\\\\\\", "/", outer)
  }

  # Add a separator to prevent partial-prefix matches.
  if (!endsWith(outer, "/")) {
    outer <- paste0(outer, "/")
  }

  startsWith(inner, outer)
}

',
zzz = '
#\' Package namespace initialization
#\'
#\' Side-effect-free namespace load hook.
#\'
#\' @details
#\' Component installation is exclusively explicit through `{{ name }}_install()`.
#\'
#\' @param libname Character library path.
#\' @param pkgname Character package name.
#\'
#\' @return Invisibly, `NULL`.
#\' @noRd

.onLoad <- function(libname, pkgname) {
  # Safety fix 2026-07: .onLoad never installs packages or deletes files.
  invisible()
}


#\' Package attachment hook
#\'
#\' Attaches components that are already installed and reports missing ones.
#\'
#\' @param libname Character library path.
#\' @param pkgname Character package name.
#\'
#\' @return Invisibly, `NULL`.
#\' @noRd

.onAttach <- function(libname, pkgname) {
  # Safety fix 2026-07: no deletion and no installation from startup.
  pkg_base_names <- sub("_.*", "", {{{ local_packages }}})

  banner <- generate_ascii_banner("{{{ name }}}", pkg_base_names)
  startup_message(paste0("\\n", banner, "\\n"))

  # Tidyverse-style startup hook: delegate search-path changes to a helper.
  result <- attach_installed_packages(pkg_base_names, warn_missing = FALSE)
  missing <- result$missing
  installed <- setdiff(pkg_base_names, missing)

  if (length(installed) > 0) {
    startup_message(.meta_trf("Attached packages: %s", paste(installed, collapse = ", ")))
  }
  if (length(missing) > 0) {
    packageStartupMessage(.meta_trf(
      "Components still need installation: %s\\nRun {{ name }}_install() to install them from local archives.",
      paste(missing, collapse = ", ")
    ))
  }
  invisible()
}


#\' Safe package unload hook
#\'
#\' Detaches attached components without deleting any files or directories.
#\'
#\' @param libpath Character library path.
#\'
#\' @return Invisibly, `NULL`.
#\' @noRd
.onUnload <- function(libpath) {
  # Detach only; never delete.
  tryCatch({
    if (exists(".pkgs")) {
      for (pkg in .pkgs) {
        # Detach without unloading component namespaces.
        try(detach(paste0("package:", pkg), character.only = TRUE, unload = FALSE),
            silent = TRUE)
      }
    }
  }, error = function(e) {
    # Report unload errors without turning them into destructive recovery.
    message(.meta_trf("Note: Error during safe unload: %s", e$message))
  })

  # No cleanup or deletion operation is allowed here.
  invisible()
}
'
  )

  # Ensure the destination directory exists.
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Render and write every runtime template.
  created_files <- character(0)
  for (file_name in names(templates)) {
    file_path <- file.path(dest_dir, paste0(file_name, ".R"))

    if (!file.exists(file_path)) {
      tryCatch({
        content <- whisker::whisker.render(
          template = templates[[file_name]],
          data = template_data,
          partials = list()
        )
        content <- .drop_regular_comment_lines(content)

        # Reject empty rendered output
        if (nchar(content) == 0) {
          stop(.bb_tr("The template rendered empty content."), call. = FALSE)
        }

        .write_utf8(content, file_path)
        if (verbose) {
          message(.bb_trf("Created %s.R successfully.", file_name))
        }
        created_files <- c(created_files, file_path)
      }, error = function(e) {
        warning(.bb_trf("Error creating %s.R: %s", file_name, e$message),
                call. = FALSE)
        # Emit diagnostic context in verbose mode
        if (verbose) {
          message(.bb_tr("\nOriginal template:"))
          message(templates[[file_name]])
          message(.bb_tr("\nTemplate data:"))
          utils::str(template_data)
        }
      })
    } else {
      if (verbose) {
        message(.bb_trf("%s.R already exists and will not be overwritten.", file_name))
      }
    }
  }

  # Generate re-exports when requested.
  if (reexport) {
    write_reexports_file(
      name = name,
      packages = packages,
      dest_dir = dest_dir,
      verbose = verbose
    )
  }

  invisible(created_files)
}
