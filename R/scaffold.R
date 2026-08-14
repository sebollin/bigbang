#' Write a generated metapackage DESCRIPTION
#'
#' @param name Character metapackage name.
#' @param version Character package version.
#' @param implicit_deps Character detected implicit dependencies.
#' @param import_deps Character dependencies to place in Imports.
#' @param authors Character Authors@R expression.
#' @param description Character title and description seed.
#' @param license Character license specification.
#' @param component_packages Character component package names.
#' @param description_path Character output path.
#' @param verbose Logical debug toggle.
#' @param r_requirement Optional R version requirement propagated from
#'   component DESCRIPTION files.
#'
#' @return Invisible `NULL`; writes DESCRIPTION to `description_path`.
#' @noRd
write_description_file <- function(
  name,
  version,
  implicit_deps = NULL,
  import_deps = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts", "zoo"),
  authors = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
  description = "Local Package Metapackage",
  license = "MIT + file LICENSE",
  component_packages = character(),
  description_path = "DESCRIPTION",
  verbose = FALSE,
  r_requirement = list(op = ">=", version = "3.5.0")
) {
  # Use a minimal default when no implicit dependencies were supplied.
  if (is.null(implicit_deps)) {
    implicit_deps <- c("Matrix", "class")
  }

  # Separate attached dependencies from namespace-only imports.
  deps_for_depends <- setdiff(implicit_deps, import_deps)


  # Package titles must not end with a period.
  title <- description
  if (endsWith(title, ".")) {
    title <- substr(title, 1, nchar(title) - 1)
  }

  # DESCRIPTION prose should be a complete sentence.
  if (!endsWith(description, ".")) {
    description <- paste0(description, ".")
  }

  # Add a stable explanation of the generated package behavior.
  description <- paste0(
    description,
    " This package manages, installs, and attaches locally archived R packages through a metapackage interface. ",
    "It provides explicit installation, dependency detection, topological ordering, and safe attachment helpers."
  )


  # Build Depends.
  deps_section <- paste0(
    "    R (", r_requirement$op, " ", r_requirement$version, ")"
  )
  if (length(deps_for_depends) > 0) {
    deps_section <- paste0(
      deps_section, ",\n    ",
      paste(deps_for_depends, collapse = ",\n    ")
    )
  }

  # Build Imports.
  imports_section <- "    utils"
  if (length(import_deps) > 0) {
    # Include only dependencies actually detected or requested.
    deps_imports_to_use <- intersect(import_deps, implicit_deps)
    if (length(deps_imports_to_use) > 0) {
      imports_section <- paste0(
        imports_section, ",\n    ",
        paste(deps_imports_to_use, collapse = ",\n    ")
      )
    }
  }



  # Write DESCRIPTION as UTF-8.
  desc_content <- glue::glue(
    'Package: {name}
Title: {title}
Version: {version}
Authors@R: {authors}
Description: {description}
License: {license}
Encoding: UTF-8
Language: en
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Depends:
{deps_section}
Imports:
{imports_section}
Suggests:
    cli
Config/Needs/website: {paste(implicit_deps, collapse = ", ")}
Config/bigbang/generator-version: {.bb_generator_version()}
Config/bigbang/template-safety-schema: {.template_safety_schema}
Config/bigbang/packages: {paste(component_packages, collapse = ", ")}
'
  )

  .write_utf8(desc_content, description_path)

  if (verbose) {
    message(.bb_tr("DEBUG: DESCRIPTION file created"))
  }
}



#' Names reserved by generated metapackage code.
#'
#' @param name Character metapackage name.
#' @return Character vector of symbols that cannot be replaced by active
#'   component bindings.
#' @noRd
.generated_metapackage_symbols <- function(name) {
  public <- paste0(name, c("_attach", "_detach", "_packages", "_attach_all",
                           "_install", "_load_all", "_deps", "_conflicts"))
  c(public, paste0("print.", name, "_conflicts"),
    ".pkgs", ".component_names", ".component_specs",
    ".component_reexport_specs", ".reexport_state", ".reexport_library_paths",
    ".set_reexport_library", ".reexport_component_value",
    ".make_reexport_binding", ".install_reexport_bindings",
    "attach_installed_packages", ".bigbang_abort",
    "install_packages_in_order", "resolve_upgrade_policy",
    "with_install_library_path", "install_source_component",
    "resolve_component_spec", "resolve_component_archive", "read_archive_metadata",
    "read_archive_dependencies", "version_satisfies",
    "validate_local_constraints", "classify_package_archive",
    "install_local_archive", "detect_cycles", "build_dependency_graph",
    "topological_order",
    "style_startup_text", "package_version", "startup_message",
    "generate_ascii_banner", "format_cli_startup", "safe_unlink",
    "is_path_inside", ".meta_tr", ".meta_trf", ".onLoad", ".onAttach", ".onUnload")
}

.namespace_export_directive <- function(symbol) {
  quoted <- if (any(utf8ToInt(enc2utf8(symbol)) > 0x7fL)) {
    .r_ascii_literal(symbol)
  } else {
    paste(deparse(as.name(symbol), backtick = TRUE), collapse = "")
  }
  paste0("export(", quoted, ")")
}

#' Write a generated metapackage NAMESPACE
#'
#' @param name Character metapackage name.
#' @param namespace_path Character output path.
#' @param implicit_deps Character implicit dependencies.
#' @param import_deps Character dependencies placed in Imports.
#' @return Invisible `NULL`; writes NAMESPACE.
#' @noRd

write_namespace_file <- function(name, namespace_path,
                                 implicit_deps = NULL, import_deps = NULL,
                                 verbose = FALSE,
                                 reexport_symbols = character()) {
  # Export the complete generated API without requiring roxygen at generation time.
  export <- paste0(
    "export(", name, "_attach)\n",
    "export(", name, "_detach)\n",
    "export(", name, "_packages)\n",
    "export(", name, "_attach_all)\n",
    "export(", name, "_install)\n",
    "export(", name, "_load_all)\n",
    "export(", name, "_deps)\n",
    "export(", name, "_conflicts)\n",
    "S3method(print,", name, "_conflicts)\n"
  )
  reexports <- if (length(reexport_symbols) > 0L) {
    paste(vapply(
      reexport_symbols, .namespace_export_directive, character(1L)
    ), collapse = "\n")
  } else {
    character()
  }

  # Import every implicit dependency for compatibility with existing metapackages.
  imports <- character(0)
  if (!is.null(implicit_deps) && length(implicit_deps) > 0) {
    imports <- paste0("import(", implicit_deps, ")")
  }

  # Combine all NAMESPACE directives.
  namespace_content <- c(
    "# Generated by roxygen2: do not edit by hand",
    "",
    export,
    reexports,
    paste(imports, collapse = "\n")
  )

  # Write NAMESPACE as UTF-8.
  .write_utf8(namespace_content, namespace_path)

  if (verbose) {
    message(.bb_tr("DEBUG: NAMESPACE file created"))
  }
}

.ensure_namespace_exports <- function(namespace_path, symbols = character()) {
  symbols <- unique(symbols[nzchar(symbols)])
  if (!file.exists(namespace_path) || length(symbols) == 0L) {
    return(invisible(NULL))
  }
  lines <- readLines(namespace_path, warn = FALSE, encoding = "UTF-8")
  directives <- vapply(
    symbols, .namespace_export_directive, character(1L)
  )
  missing <- setdiff(directives, trimws(lines))
  if (length(missing) > 0L) .write_utf8(c(lines, missing), namespace_path)
  invisible(NULL)
}

.write_reexport_documentation <- function(project_dir, symbols) {
  symbols <- unique(symbols[nzchar(symbols)])
  if (length(symbols) == 0L) return(invisible(NULL))
  content <- c(
    "\\name{reexports}",
    "\\alias{reexports}",
    paste0("\\alias{", symbols, "}"),
    "\\title{Runtime component re-exports}",
    paste0(
      "\\description{Explicit exports from component packages are resolved ",
      "through read-only active bindings when the component is installed.}"
    ),
    "\\details{The component package is loaded lazily when a binding is read.}",
    "\\keyword{internal}"
  )
  .write_utf8(content, file.path(project_dir, "man", "reexports.Rd"))
  invisible(NULL)
}

.deduplicate_namespace_imports <- function(namespace_path) {
  if (!file.exists(namespace_path)) return(invisible(NULL))
  lines <- readLines(namespace_path, warn = FALSE, encoding = "UTF-8")
  seen <- character()
  keep <- vapply(lines, function(line) {
    if (!grepl("^importFrom\\(", line)) return(TRUE)
    key <- gsub("[[:space:]]", "", line)
    if (key %in% seen) return(FALSE)
    seen <<- c(seen, key)
    TRUE
  }, logical(1L))
  if (any(!keep)) .write_utf8(lines[keep], namespace_path)
  invisible(NULL)
}

#' Write a generated metapackage README
#'
#' @param name Character metapackage name.
#' @param project_dir Character project directory.
#' @param include_archives Logical, whether the component archives ship inside
#'   the meta-package. It decides whether the documented installation call needs
#'   an archive directory at all.
#' @return Invisible path to the generated README.
#' @noRd
write_metapackage_readme <- function(name, project_dir,
                                     include_archives = FALSE,
                                     reexport = FALSE) {
  readme_path <- file.path(project_dir, "README.md")
  content <- c(
    paste0("# ", name),
    "",
    "This metapackage was generated by bigbang from local package archives.",
    "Loading it attaches components that are already installed; it never installs",
    "packages during startup.",
    if (isTRUE(reexport)) {
      c(
        "Explicit exports are available through read-only runtime bindings in",
        "this metapackage. Component packages are not installation dependencies,",
        "so the package can be installed and loaded before its components exist.",
        "Before installation, reading a binding returns a placeholder function;",
        "its clear missing-component error appears only when that function is called.",
        "For non-function exports, access returns the placeholder instead of the",
        "object until installation. The same binding then works without reload.",
        "Only explicit export()",
        "directives become bindings; S4 classes and methods remain available by",
        "loading their component package.",
        "An object restored with readRDS() does not load a component by itself,",
        "so base R cannot dispatch that component's S3 method until it is loaded."
      )
    } else {
      c(
        "Attached exports are available directly or through each component namespace;",
        "they are not copied into this metapackage namespace."
      )
    },
    "",
    "## Install components",
    "",
    if (isTRUE(include_archives)) {
      c(
        "The component archives ship inside this package, so installing the",
        "components needs nothing else and no path has to be known:",
        "",
        "```r",
        paste0(name, "_install()"),
        "```"
      )
    } else {
      c(
        "The component archives live outside this package, so the directory",
        "or directories holding them are required:",
        "",
        "```r",
        paste0(name, "_install(pkg_dir = c(\"/path/to/local/archives\"))"),
        "```"
      )
    },
    "",
    "Components are installed in dependency order. No repository is contacted",
    "unless a component depends on a package that only exists in one, which",
    "requires `cran_deps = \"install\"`.",
    "",
    "## Startup messages",
    "",
    "Set the package-specific option to silence attachment messages without changing",
    "which installed components are attached:",
    "",
    "```r",
    paste0("options(", name, ".quiet = TRUE)"),
    paste0("library(", name, ")"),
    "```",
    "",
    "## Helpers",
    "",
    paste0("- `", name, "_packages()` lists components."),
    paste0("- `", name, "_conflicts()` reports masking conflicts."),
    paste0("- `", name, "_detach()` detaches components."),
    "",
    "The install helper also accepts an 'only' component subset and an explicit 'lib' installation library.",
    "",
    "## Adding a component",
    "",
    "To request that a package be included in this meta-package, contact the",
    "maintainer named in DESCRIPTION."
  )
  .write_utf8(content, readme_path)
  invisible(readme_path)
}

#' Write an ordered workflow vignette skeleton
#'
#' @param name Character metapackage name.
#' @param workflow Named character vector mapping stages to packages.
#' @param project_dir Character project directory.
#' @return Invisible path to the generated vignette.
#' @noRd
write_workflow_vignette <- function(name, workflow, project_dir) {
  vignette_path <- file.path(
    project_dir, "vignettes", paste0("workflow-", name, ".Rmd")
  )
  sections <- unlist(Map(function(stage, package, index) {
    c(
      paste0("## ", stage),
      "",
      paste0("Pipeline component: `", package, "`."),
      "",
      paste0("```{r stage-", index, ", eval=FALSE}"),
      paste0("# Add the ", stage, " step using ", package, "."),
      "```",
      ""
    )
  }, names(workflow), unname(workflow), seq_along(workflow)), use.names = FALSE)
  content <- c(
    "---",
    paste0("title: \"Workflow for ", name, "\""),
    "output: rmarkdown::html_vignette",
    "vignette: >",
    paste0("  %\\VignetteIndexEntry{Workflow for ", name, "}"),
    "  %\\VignetteEngine{knitr::rmarkdown}",
    "  %\\VignetteEncoding{UTF-8}",
    "---",
    "",
    "This skeleton follows the configured component order. Replace each placeholder",
    "with the project-specific analysis step.",
    "",
    sections,
    "## Citations",
    "",
    "Use each component's preferred citation:",
    "",
    "```{r component-citations, eval=FALSE}",
    paste0("citations <- lapply(", name, "_packages(), citation)"),
    "citations",
    "```"
  )
  .write_utf8(content, vignette_path)
  invisible(vignette_path)
}

#' Write the generated component-consistency test
#'
#' @param name Character metapackage name.
#' @param project_dir Character project directory.
#' @return Invisible path to the generated test.
#' @noRd
write_consistency_test <- function(name, project_dir) {
  test_dir <- file.path(project_dir, "tests")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  test_path <- file.path(test_dir, "component-consistency.R")
  content <- c(
    paste0("stopifnot(requireNamespace(\"", name, "\", quietly = TRUE))"),
    paste0("description <- utils::packageDescription(\"", name, "\")"),
    "declared <- strsplit(description[[\"Config/bigbang/packages\"]], \",\", fixed = TRUE)[[1L]]",
    "declared <- trimws(declared[nzchar(declared)])",
    paste0(
      "component_packages <- get(\"", name, "_packages\", envir = ",
      "asNamespace(\"", name, "\"))()"
    ),
    "stopifnot(setequal(component_packages, declared))",
    "imports <- description[[\"Imports\"]]",
    "imports <- if (is.null(imports)) character() else strsplit(imports, \",\", fixed = TRUE)[[1L]]",
    "imports <- trimws(gsub(\"\\\\s*\\\\([^)]*\\\\)\", \"\", imports))",
    "stopifnot(length(intersect(component_packages, imports)) == 0L)"
  )
  .write_utf8(content, test_path)
  invisible(test_path)
}
#' Create a basic generated-metapackage vignette
#'
#' Writes an English R Markdown introduction and ensures DESCRIPTION declares
#' its vignette builder and suggested packages.
#'
#' @param name Character metapackage name.
#' @param packages Character archive stems including versions.
#' @param project_dir Character project directory.
#' @param verbose Logical progress toggle.
#' @return Invisible `NULL`; called for side effects.
#' @noRd

write_basic_vignette <- function(name, packages, project_dir,
                                 include_archives = FALSE, verbose = FALSE) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  desc_file <- file.path(project_dir, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    warning(.bb_tr("The DESCRIPTION file does not exist in the project directory."),
            call. = FALSE)
    return(invisible(FALSE))
  }

  # Ensure the vignette directory exists.
  vignette_dir <- file.path(project_dir, "vignettes")
  if (!dir.exists(vignette_dir)) {
    dir.create(vignette_dir, recursive = TRUE, showWarnings = TRUE)
  }

  tryCatch({
    base_packages <- unique(packages)

    # Build the generated English introduction.
    vignette_content <- paste0(
      "---\n",
      "title: \"Introduction to ", name, "\"\n",
      "output: rmarkdown::html_vignette\n",
      "vignette: >\n",
      "  %\\VignetteIndexEntry{Introduction to ", name, "}\n",
      "  %\\VignetteEngine{knitr::rmarkdown}\n",
      "  %\\VignetteEncoding{UTF-8}\n",
      "---\n\n",
      "```{r, include = FALSE}\n",
      "knitr::opts_chunk$set(\n",
      "  collapse = TRUE,\n",
      "  comment = \"#>\"\n",
      ")\n",
      "```\n\n",
      "## Introduction\n\n",
      "`", name, "` is a metapackage for installing and attaching local packages.\n\n",
      "## Included packages\n\n",
      "This metapackage includes:\n\n",
      paste(vapply(base_packages, function(pkg) paste0("* `", pkg, "`\n"), character(1)), collapse = ""),
      "\n\n## Basic use\n\n",
      "To attach all installed components:\n\n",
      "```{r eval=FALSE}\n",
      "library(", name, ")\n",
      "```\n\n",
      "Attached component exports are available directly or through their own",
      "package namespace; they are not copied into this metapackage namespace.\n\n",
      "## Available functions\n\n",
      if (isTRUE(include_archives)) {
        paste0("* `", name, "_install()`: installs the components shipped inside this package.\n")
      } else {
        paste0("* `", name, "_install(pkg_dir = ...)`: installs components from local archives.\n")
      },
      "* `", name, "_attach()`: attaches installed components.\n",
      "* `", name, "_detach()`: detaches all components.\n",
      "* `", name, "_packages()`: lists included packages.\n",
      "* `", name, "_conflicts()`: reports masking conflicts.\n\n",
      "Set `options(", name, ".quiet = TRUE)` to silence startup messages.\n"
    )

    vignette_file <- file.path(vignette_dir, paste0("introduction-", name, ".Rmd"))
    .write_utf8(vignette_content, vignette_file)

    # Ignore rendered vignette by-products.
    gitignore_content <- "# Automatically created files\n*.html\n*.R\n"
    .write_utf8(gitignore_content, file.path(vignette_dir, ".gitignore"))

    # Ensure DESCRIPTION declares the vignette toolchain.
    tryCatch({
      desc_content <- readLines(desc_file)

      # Add VignetteBuilder when absent.
      if (!any(grepl("^VignetteBuilder:", desc_content))) {
        desc_content <- c(desc_content, "VignetteBuilder: knitr")
      }

      # Update the Suggests field while preserving DCF continuation syntax.
      suggests_line <- grep("^Suggests:", desc_content)

      if (length(suggests_line) > 0) {
        # Infer indentation from Depends or Imports.
        indent_pattern <- grep("^(Depends|Imports):", desc_content, value = TRUE)
        if (length(indent_pattern) > 0) {
          # Inspect the first continuation line.
          dep_line <- which(grepl("^(Depends|Imports):", desc_content))[1]
          if (dep_line < length(desc_content)) {
            indent <- gsub("^(\\s*).*", "\\1", desc_content[dep_line + 1])
            if (nchar(indent) == 0) indent <- "    "
          } else {
            indent <- "    "
          }
        } else {
          indent <- "    "
        }

        suggests <- desc_content[suggests_line]

        # Collect all Suggests continuation lines.
        next_line <- suggests_line + 1
        while (next_line <= length(desc_content) &&
                 (grepl("^\\s+", desc_content[next_line]) || desc_content[next_line] == "")) {
          suggests <- c(suggests, desc_content[next_line])
          next_line <- next_line + 1
        }

        # Parse existing package names.
        suggests_text <- paste(suggests, collapse = " ")
        suggests_text <- sub("^Suggests:\\s*", "", suggests_text)
        suggests_pkgs <- trimws(unlist(strsplit(suggests_text, ",")))

        suggests_pkgs <- suggests_pkgs[nzchar(suggests_pkgs)]

        # Add the vignette dependencies when missing.
        if (!any(grepl("rmarkdown", suggests_pkgs))) suggests_pkgs <- c(suggests_pkgs, "rmarkdown")
        if (!any(grepl("knitr", suggests_pkgs))) suggests_pkgs <- c(suggests_pkgs, "knitr")

        suggests_pkgs <- sort(suggests_pkgs)

        # Rebuild a valid Suggests field.
        new_suggests <- "Suggests:"

        if (length(suggests_pkgs) > 0) {
          pkgs_formatted <- paste0(indent, paste(suggests_pkgs, collapse = paste0(",\n", indent)))
          new_suggests <- paste0(new_suggests, "\n", pkgs_formatted)
        }

        # Replace the complete original field.
        desc_content <- desc_content[-(suggests_line:(next_line - 1))]
        desc_content <- append(desc_content, new_suggests, after = suggests_line - 1)
      } else {
        # Add Suggests if the field was absent.
        new_suggests <- "Suggests:\n    rmarkdown,\n    knitr"
        desc_content <- c(desc_content, new_suggests)
      }

      .write_utf8(desc_content, desc_file)

      if (verbose) {
        message(.bb_tr("DESCRIPTION updated with a valid Suggests field"))
      }
    }, error = function(e) {
      warning(.bb_trf("Error updating DESCRIPTION: %s", e$message), call. = FALSE)
    })

    if (verbose) {
      message(.bb_trf("Basic vignette created at %s", vignette_file))
    }
  }, error = function(e) {
    warning(.bb_trf("Error creating basic vignette: %s", e$message), call. = FALSE)
    invisible(FALSE)
  })

  invisible(TRUE)

}
