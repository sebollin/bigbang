# Local metapackage generator
#
# `create_metapackage()` creates a package project whose installation remains
# side-effect free. Component installation is an explicit `<name>_install()` call.
# Generated code reads archive DESCRIPTION files, builds the local dependency
# graph, rejects cycles, and installs each component once in topological order.
# Startup hooks may attach installed components but never install or remove files.

.BIGBANG_GENERATOR_VERSION <- "0.1.0"
.BIGBANG_TEMPLATE_SAFETY_SCHEMA <- "2"

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

.lv_tr <- function(message) {
  gettext(message, domain = "R-bigbang")
}

.lv_trf <- function(format, ...) {
  gettextf(format, ..., domain = "R-bigbang")
}

.po_quote <- function(x) {
  escaped <- gsub("\\\\", "\\\\\\\\", enc2utf8(x))
  escaped <- gsub("\"", "\\\\\"", escaped, fixed = TRUE)
  escaped <- gsub("\n", "\\\\n", escaped, fixed = TRUE)
  paste0("\"", escaped, "\"")
}

.write_po_catalog <- function(messages, translations = NULL, path,
                              project = "bigbang") {
  if (is.null(translations)) translations <- stats::setNames(rep("", length(messages)), messages)
  translations <- translations[messages]
  header <- c(
    'msgid ""',
    'msgstr ""',
    paste0('"Project-Id-Version: ', project, '\\n"'),
    '"MIME-Version: 1.0\\n"',
    '"Content-Type: text/plain; charset=UTF-8\\n"',
    '"Content-Transfer-Encoding: 8bit\\n"',
    '"Language: es\\n"',
    '"Plural-Forms: nplurals=2; plural=(n != 1);\\n"',
    ""
  )
  entries <- unlist(Map(
    function(id, translation) {
      c(paste("msgid", .po_quote(id)), paste("msgstr", .po_quote(translation)), "")
    }, messages, unname(translations)
  ), use.names = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  .write_utf8(c(header, entries), path)
}

.write_mo_catalog <- function(messages, translations, path) {
  header <- paste0(
    "Project-Id-Version: bigbang\n",
    "MIME-Version: 1.0\n",
    "Content-Type: text/plain; charset=UTF-8\n",
    "Content-Transfer-Encoding: 8bit\n",
    "Language: es\n",
    "Plural-Forms: nplurals=2; plural=(n != 1);\n"
  )
  ids <- c("", enc2utf8(messages))
  values <- c(header, enc2utf8(unname(translations[messages])))
  ordering <- order(ids)
  ids <- ids[ordering]
  values <- values[ordering]
  id_raw <- lapply(ids, charToRaw)
  value_raw <- lapply(values, charToRaw)
  n <- length(ids)
  original_table <- 28L
  translation_table <- original_table + 8L * n
  original_strings <- translation_table + 8L * n
  original_offsets <- original_strings + c(0L, cumsum(lengths(id_raw) + 1L)[-n])
  translation_strings <- original_strings + sum(lengths(id_raw) + 1L)
  translation_offsets <- translation_strings + c(0L, cumsum(lengths(value_raw) + 1L)[-n])

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(as.raw(c(0xde, 0x12, 0x04, 0x95)), connection)
  writeBin(
    as.integer(c(0L, n, original_table, translation_table, 0L, 0L)),
    connection, size = 4L, endian = "little"
  )
  writeBin(as.integer(as.vector(rbind(lengths(id_raw), original_offsets))),
           connection, size = 4L, endian = "little")
  writeBin(as.integer(as.vector(rbind(lengths(value_raw), translation_offsets))),
           connection, size = 4L, endian = "little")
  for (value in id_raw) writeBin(c(value, as.raw(0)), connection)
  for (value in value_raw) writeBin(c(value, as.raw(0)), connection)
  invisible(path)
}

.drop_regular_comment_lines <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
  regular_comment <- grepl("^[[:space:]]*#(?!')", lines, perl = TRUE)
  paste(lines[!regular_comment], collapse = "\n")
}

.bigbang_condition <- function(class, message, ..., call = NULL) {
  structure(
    c(list(message = message, call = call), list(...)),
    class = c(class, "bigbang_condition", "condition")
  )
}

.bigbang_abort <- function(class, message, ..., call = NULL) {
  condition <- .bigbang_condition(class, message, ..., call = call)
  class(condition) <- c(class, "bigbang_error", "error", "condition")
  stop(condition)
}

.bigbang_warn <- function(class, message, ..., call = NULL, immediate. = FALSE) {
  condition <- .bigbang_condition(class, message, ..., call = call)
  class(condition) <- c(class, "bigbang_warning", "warning", "condition")
  warning(condition, immediate. = immediate.)
}


#' Extract dependencies from a local package archive
#'
#' Reads Depends, Imports, and LinkingTo from DESCRIPTION and detects a small set
#' of implicit recommended-package uses.
#'
#' @param paquete Character archive stem.
#' @param ruta_instalables Character archive directory.
#' @param ext Character archive extension.
#'
#' @return A character vector of dependency names.
#' @noRd

extraer_dependencias <- function(paquete, ruta_instalables, ext = ".tar.gz") {
  # Ruta al archivo del paquete
  archivo_paquete <- file.path(ruta_instalables, paste0(paquete, ext))

  # Extraer el contenido del archivo DESCRIPTION
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Verificar extensi\u00f3n y extraer archivo
  if (ext == ".tar.gz" || ext == ".tar") {
    utils::untar(archivo_paquete, exdir = temp_dir)
  } else if (ext == ".zip") {
    utils::unzip(archivo_paquete, exdir = temp_dir)
  } else {
    stop(.lv_trf("Unsupported archive extension: %s", ext), call. = FALSE)
  }

  desc_file <- list.files(temp_dir, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE)

  if (length(desc_file) == 0) {
    stop(.lv_trf("No DESCRIPTION file found in package %s", paquete), call. = FALSE)
  }

  desc <- read.dcf(desc_file)

  # Verificar la existencia de los campos Depends, Imports, LinkingTo
  campos <- c("Depends", "Imports", "LinkingTo")
  dependencias <- unlist(lapply(campos, function(campo) {
    if (campo %in% colnames(desc)) {
      return(unlist(strsplit(desc[, campo], split = ",")))
    } else {
      return(NULL)
    }
  }))

  # Limpiar las dependencias (eliminar espacios y versiones)
  dependencias <- gsub("\\s*\\(.*\\)", "", dependencias)  # Eliminar versiones y espacios
  dependencias <- gsub("\\s+", "", dependencias)  # Eliminar espacios

#   # Eliminar el directorio temporal
#   safe_unlink(temp_dir, recursive = TRUE)
#
#   return(dependencias[dependencias != "R"])  # Excluir 'R'
# }

  # A\u00f1adir paquetes recomendados si se detectan objetos S4 o matrices dispersas
  r_files <- list.files(file.path(temp_dir, sub("_.*", "", paquete), "R"),
                        pattern = "\\.[Rr]$", full.names = TRUE)

  # Buscar indicios de uso de S4 o matrices dispersas
  for (file in r_files) {
    content <- readLines(file, warn = FALSE)
    if (any(grepl("setClass|setGeneric|setMethod|setValidity|representation|prototype", content)) ||
        any(grepl("sparseMatrix|dgCMatrix|dsCMatrix|Matrix\\(", content))) {
      dependencias <- c(dependencias, "Matrix")
    }
    if (any(grepl("knn|LDA|QDA|class::", content))) {
      dependencias <- c(dependencias, "class")
    }
  }

  return(unique(dependencias[dependencias != "R"]))  # Excluir 'R' y eliminar duplicados
}

#' Classify local and repository dependencies
#'
#' @param dependencias Character dependency names.
#' @param ruta_instalables Character archive directory.
#' @param ext Character archive extension.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{locales}{Dependencies available as local archives.}
#'   \item{cran}{Dependencies expected from a configured repository.}
#' }
#' @noRd
clasificar_dependencias <- function(dependencias, ruta_instalables, ext = ".tar.gz") {
  archives <- list.files(ruta_instalables)
  archives <- archives[endsWith(tolower(archives), tolower(ext))]
  stems <- substr(archives, 1L, nchar(archives) - nchar(ext))
  local_names <- unique(c(stems, sub("_.*", "", stems)))
  is_local <- dependencias %in% local_names

  list(
    locales = unique(dependencias[is_local]),
    cran = unique(dependencias[!is_local])
  )
}



#' Write a generated metapackage DESCRIPTION
#'
#' @param nombre Character metapackage name.
#' @param version Character package version.
#' @param deps_implicitas Character detected implicit dependencies.
#' @param deps_imports Character dependencies to place in Imports.
#' @param autores Character Authors@R expression.
#' @param descripcion Character title and description seed.
#' @param licencia Character license specification.
#' @param verbose Logical debug toggle.
#'
#' @return Invisible `NULL`; writes DESCRIPTION in the current directory.
#' @noRd
write_description_file <- function(
    nombre,
    version,
    deps_implicitas = NULL,
    deps_imports = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts", "zoo"),
    autores = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
    descripcion = "Local Package Metapackage",
    licencia = "MIT + file LICENSE",
    verbose = FALSE
) {
  # Si no se especifican dependencias impl\u00edcitas, usar un conjunto m\u00ednimo
  if (is.null(deps_implicitas)) {
    deps_implicitas <- c("Matrix", "class")
  }

  # Separar las dependencias entre Depends e Imports
  deps_for_depends <- setdiff(deps_implicitas, deps_imports)


  # Asegurar que el t\u00edtulo no termina con punto
  title <- descripcion
  if (endsWith(title, ".")) {
    title <- substr(title, 1, nchar(title) - 1)
  }

  # Asegurar que la descripci\u00f3n es una oraci\u00f3n completa con punto final
  description <- descripcion
  if (!endsWith(description, ".")) {
    description <- paste0(description, ".")
  }

  # Expandir la descripci\u00f3n para que sea m\u00e1s informativa
  description <- paste0(
    description,
    " This package manages, installs, and attaches locally archived R packages through a metapackage interface. ",
    "It provides explicit installation, dependency detection, topological ordering, and safe attachment helpers."
  )


  # Crear la secci\u00f3n de dependencias (Depends)
  deps_section <- "    R (>= 3.5.0)"
  if (length(deps_for_depends) > 0) {
    deps_section <- paste0(
      deps_section, ",\n    ",
      paste(deps_for_depends, collapse = ",\n    ")
    )
  }

  # Crear secci\u00f3n de imports
  imports_section <- "    utils"
  if (length(deps_imports) > 0) {
    # Filtrar para incluir solo las dependencias que est\u00e1n en deps_implicitas
    deps_imports_to_use <- intersect(deps_imports, deps_implicitas)
    if (length(deps_imports_to_use) > 0) {
      imports_section <- paste0(
        imports_section, ",\n    ",
        paste(deps_imports_to_use, collapse = ",\n    ")
      )
    }
  }



  # Crear DESCRIPTION file
  desc_content <- glue::glue(
    'Package: {nombre}
Title: {title}
Version: {version}
Authors@R: {autores}
Description: {description}
License: {licencia}
Encoding: UTF-8
Language: en
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Depends:
{deps_section}
Imports:
{imports_section}
Suggests:
    crayon,
    rstudioapi
Config/Needs/website: {paste(deps_implicitas, collapse = ", ")}
Config/bigbang/generator-version: {.BIGBANG_GENERATOR_VERSION}
Config/bigbang/template-safety-schema: {.BIGBANG_TEMPLATE_SAFETY_SCHEMA}
'
  )

  .write_utf8(desc_content, "DESCRIPTION")

  if (verbose) {
    message(.lv_tr("DEBUG: DESCRIPTION file created"))
  }
}



#' Write a generated metapackage NAMESPACE
#'
#' @param nombre Character metapackage name.
#' @param paquetes_cran Character non-local dependencies.
#' @param namespace_path Character output path.
#' @param deps_implicitas Character implicit dependencies.
#' @param deps_imports Character dependencies placed in Imports.
#' @return Invisible `NULL`; writes NAMESPACE.
#' @noRd

write_namespace_file <- function(nombre, paquetes_cran, namespace_path,
                                 deps_implicitas = NULL, deps_imports = NULL,
                                 verbose = FALSE) {
  # Generar las exportaciones para las funciones del metapaquete
  export <- paste0(
    "export(", nombre, "_attach)\n",
    "export(", nombre, "_detach)\n",
    "export(", nombre, "_packages)\n",
    "export(", nombre, "_attach_all)\n",
    "export(", nombre, "_install)\n",
    "export(", nombre, "_load_all)\n",
    "export(", nombre, "_deps)\n"
  )

  # A\u00f1adir importaci\u00f3n expl\u00edcita para todas las dependencias impl\u00edcitas
  # incluso las que est\u00e1n en Imports, para mantener compatibilidad
  imports <- character(0)
  if (!is.null(deps_implicitas) && length(deps_implicitas) > 0) {
    imports <- paste0("import(", deps_implicitas, ")")
  }

  # Combinar todas las directivas del NAMESPACE
  namespace_content <- c(
    "# Generated by roxygen2: do not edit by hand",
    "",
    export,
    paste(imports, collapse = "\n")
  )

  # Escribir al archivo
  .write_utf8(namespace_content, namespace_path)

  if (verbose) {
    message(.lv_tr("DEBUG: NAMESPACE file created"))
  }
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
#'     \item{matriz_refs}{Character vector of references to 'Matrix', with file and line.}
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
#' lapply(res, function(x) x$matriz_refs)
#' }
#' @export
diagnose_dependencies <- function(packages, pkg_dir, ext = ".tar.gz") {
  paquetes_locales <- packages
  ruta_instalables <- pkg_dir
  resultados <- list()

  for (paq in paquetes_locales) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

    # Extraer el paquete
    archivo_paquete <- file.path(ruta_instalables, paste0(paq, ext))
    if (!file.exists(archivo_paquete)) {
      message(.lv_trf("Package archive not found: %s", archivo_paquete))
      next
    }

    if (ext == ".tar.gz" || ext == ".tar") {
      utils::untar(archivo_paquete, exdir = temp_dir)
    } else if (ext == ".zip") {
      utils::unzip(archivo_paquete, exdir = temp_dir)
    }

    # Buscar usos de Matrix o class
    paq_base <- sub("_.*", "", paq)
    r_dir <- file.path(temp_dir, paq_base, "R")

    if (!dir.exists(r_dir)) {
      message(.lv_trf("No R directory found for package: %s", paq))
      next
    }

    r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

    matriz_refs <- character(0)
    class_refs <- character(0)

    for (file in r_files) {
      content <- readLines(file, warn = FALSE)

      # Buscar referencias a Matrix
      matriz_lineas <- grep("Matrix|sparseMatrix|[dstz][gsd]Matrix|Sparse", content)
      if (length(matriz_lineas) > 0) {
        for (linea_num in matriz_lineas) {
          matriz_refs <- c(matriz_refs,
                           paste0(basename(file), ":", linea_num, " - ",
                                  trimws(content[linea_num])))
        }
      }

      # Buscar referencias a class
      class_lineas <- grep("\\bclass\\b|\\bknn\\b|\\bLDA\\b|\\bQDA\\b", content)
      if (length(class_lineas) > 0) {
        for (linea_num in class_lineas) {
          class_refs <- c(class_refs,
                          paste0(basename(file), ":", linea_num, " - ",
                                 trimws(content[linea_num])))
        }
      }
    }

    resultados[[paq]] <- list(
      matriz_refs = matriz_refs,
      class_refs = class_refs
    )
  }

  return(resultados)
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
#' @param paquetes_locales Character archive stems including versions.
#' @param ruta_instalables Character archive directory.
#' @param ext Character archive extension.
#' @return A sorted character vector of possible dependency names.
#' @noRd
detectar_dependencias_implicitas <- function(paquetes_locales, ruta_instalables, ext = ".tar.gz") {
  posibles_deps <- character(0)

  # Patrones para buscar dependencias impl\u00edcitas comunes
  patrones_por_paquete <- list(
    # Paquetes de manejo de matrices especiales
    "Matrix" = "sparse|[dstz][gsd]Matrix|Matrix\\.|setClass|setGeneric|setMethod|setValidity|representation|prototype|new\\(",

    # Paquetes de an\u00e1lisis estad\u00edstico
    "class" = "\\bknn\\b|\\bLDA\\b|\\bQDA\\b|\\bnaiveBayes\\b",
    "MASS" = "\\blda\\b|\\bqda\\b|\\bridgeReg\\b|\\blogistic\\b|\\bboxcox\\b|\\bVIF\\b",
    "cluster" = "\\bkmeans\\b|\\bpam\\b|\\bclara\\b|\\bfanny\\b|\\bsilhouette\\b",

    # Paquetes gr\u00e1ficos
    "lattice" = "\\bxyplot\\b|\\bbwplot\\b|\\bcontourplot\\b|\\blevelplot\\b|\\bwireframe\\b",
    "grid" = "\\bgrid\\.arrange\\b|\\bgpar\\b|\\bgrobTree\\b|\\bviewport\\b|\\bgrid\\.layout\\b",

    # Manipulaci\u00f3n de datos
    "data.table" = "\\bdata\\.table\\b|\\bdt\\[|\\bsetkey\\b|\\bfread\\b|\\bfwrite\\b",
    "dplyr" = "\\bfilter\\b|\\barrange\\b|\\bselect\\b|\\bmutate\\b|\\bgroup_by\\b|\\bsummarise\\b",
    "tidyr" = "\\bgather\\b|\\bspread\\b|\\bseparate\\b|\\bunite\\b|\\bpivot_longer\\b|\\bpivot_wider\\b",

    # Series temporales
    "zoo" = "\\bzoo\\b|\\bindex\\b|\\bcoredata\\b|\\brollapply\\b",
    "xts" = "\\bxts\\b|\\bindexClass\\b|\\bperiodicity\\b",

    # Estad\u00edstica espacial
    "sp" = "\\bSpatialPoints\\b|\\bSpatialPolygons\\b|\\bover\\b|\\bspplot\\b",
    "sf" = "\\bst_\\b|\\bsf::st_\\b|\\bsf_\\b",

    # A\u00f1adir detecci\u00f3n para paquetes populares:
    "tibble" = "\\btibble\\b|as_tibble|\\btbl_|tibble::",
    "readr" = "\\bread_csv\\b|\\bwrite_csv\\b|\\bread_delim\\b|readr::",
    "jsonlite" = "\\bfromJSON\\b|\\btoJSON\\b|jsonlite::",
    "data.table" = "\\bdata\\.table\\b|\\bsetkey\\b|\\bfread\\b|:=",
    "ggplot2" = "\\bggplot\\b|\\baes\\b|\\bgeom_\\w+\\b|\\bfacet_\\w+\\b",
    "shiny" = "\\bshinyApp\\b|\\brenderUI\\b|\\bobserveEvent\\b|\\breactiveVal\\b"

  )

  # Examinar cada paquete local
  for (paq in paquetes_locales) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

    # Extraer el paquete
    archivo_paquete <- file.path(ruta_instalables, paste0(paq, ext))
    if (!file.exists(archivo_paquete)) {
      warning(.lv_trf("Package archive not found: %s", archivo_paquete), call. = FALSE)
      next
    }

    tryCatch({
      if (ext == ".tar.gz" || ext == ".tar") {
        utils::untar(archivo_paquete, exdir = temp_dir)
      } else if (ext == ".zip") {
        utils::unzip(archivo_paquete, exdir = temp_dir)
      }

      # Buscar en los archivos R
      paq_base <- sub("_.*", "", paq)
      r_dir <- file.path(temp_dir, paq_base, "R")

      if (!dir.exists(r_dir)) {
        warning(.lv_trf("No R directory found for package: %s", paq), call. = FALSE)
        next
      }

      r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

      # Leer todo el contenido como un \u00fanico texto
      contenido <- paste(unlist(lapply(r_files, readLines, warn = FALSE)), collapse = " ")

      # Buscar patrones para cada posible dependencia
      for (pkg_name in names(patrones_por_paquete)) {
        patron <- patrones_por_paquete[[pkg_name]]
        if (grepl(patron, contenido)) {
          posibles_deps <- c(posibles_deps, pkg_name)
        }
      }
    }, error = function(e) {
      warning(.lv_trf("Error processing package %s: %s", paq, e$message), call. = FALSE)
    })
  }

  # Eliminar duplicados y ordenar
  return(sort(unique(posibles_deps)))
}



#' Generate component re-exports
#'
#' Examines installed component namespaces and writes roxygen re-export
#' directives, resolving duplicate names deterministically.
#'
#' @param nombre Character metapackage name.
#' @param paquetes_locales Character component names without versions.
#' @param ruta_destino Character output directory.
#' @param exclude_funs Character names to exclude.
#' @param exclude_patterns Character exclusion regular expressions.
#' @param verbose Logical debug toggle.
#' @return A list describing re-exports, conflicts, and the generated file.
#' @noRd
generar_archivo_reexports <- function(
    nombre,
    paquetes_locales,
    ruta_destino,
    exclude_funs = NULL,
    exclude_patterns = c("^\\.", "^_"),  # Por defecto excluir funciones que comienzan con . o _
    verbose = FALSE
) {
  # Funci\u00f3n auxiliar para registrar mensajes de depuraci\u00f3n
  log_debug <- function(msg) {
    if (verbose) message(paste0("DEBUG: ", msg))
  }

  log_debug("Starting re-export file generation")

  # Verificar si los paquetes est\u00e1n instalados
  paquetes_disponibles <- paquetes_locales[sapply(paquetes_locales, requireNamespace, quietly = TRUE)]

  if (length(paquetes_disponibles) == 0) {
    warning(.lv_tr("No component packages are installed; re-exports were not generated."),
            call. = FALSE)
    return(NULL)
  }

  log_debug(paste("Available packages:", paste(paquetes_disponibles, collapse = ", ")))

  # Funci\u00f3n auxiliar: verificar si una funci\u00f3n debe ser excluida
  should_exclude <- function(fun_name) {
    # Excluir por nombre espec\u00edfico
    if (!is.null(exclude_funs) && fun_name %in% exclude_funs) {
      return(TRUE)
    }

    # Excluir por patr\u00f3n
    if (!is.null(exclude_patterns)) {
      for (pattern in exclude_patterns) {
        if (grepl(pattern, fun_name)) {
          return(TRUE)
        }
      }
    }

    return(FALSE)
  }

  # Obtener funciones exportadas de cada paquete
  get_exported_functions <- function(pkg) {
    log_debug(paste("Reading exports from", pkg))

    ns <- asNamespace(pkg)
    exports <- getNamespaceExports(ns)

    # Excluir funciones espec\u00edficas y patrones
    exports <- exports[!sapply(exports, should_exclude)]

    # Filtrar para obtener solo funciones (no datos, clases, etc.)
    is_function <- vapply(exports, function(x) {
      tryCatch(
        is.function(get(x, envir = ns)),
        error = function(e) FALSE
      )
    }, logical(1))

    # Intentar identificar funciones S3 gen\u00e9ricas
    is_s3_generic <- vapply(exports[is_function], function(x) {
      tryCatch({
        fun <- get(x, envir = ns)
        is.primitive(fun) || grepl("UseMethod", deparse(body(fun)))
      }, error = function(e) FALSE)
    }, logical(1))

    # Combinar resultados
    result <- list(
      functions = exports[is_function],
      s3_generics = names(is_s3_generic)[is_s3_generic]
    )

    return(result)
  }

  # Recolectar funciones exportadas por paquete
  all_exports <- lapply(paquetes_disponibles, get_exported_functions)
  names(all_exports) <- paquetes_disponibles

  # Extraer solo las funciones
  reexports <- lapply(all_exports, function(x) x$functions)

  # Extraer S3 gen\u00e9ricos para tratamiento especial
  s3_generics <- lapply(all_exports, function(x) x$s3_generics)

  # Filtrar funciones vac\u00edas y paquetes sin exportaciones
  reexports <- reexports[vapply(reexports, length, integer(1)) > 0]

  if (length(reexports) == 0) {
    warning(.lv_tr("No functions were found to re-export."), call. = FALSE)
    return(NULL)
  }

  log_debug(paste("Packages with functions:", length(reexports)))

  # Generar contenido del archivo
  content <- c(
    paste0("#\' Funciones re-exportadas de los paquetes componentes de ", nombre),
    "#\'",
    "#\' Este archivo reexporta las funciones de los paquetes componentes para permitir",
    paste0("#\' acceso directo a trav\u00E9s del metapaquete ", nombre, " (estilo tidyverse)."),
    "#\' Las funciones reexportadas mantienen su comportamiento original.",
    "#\'",
    "#\' @keywords internal",
    ""
  )

  # Detectar conflictos potenciales
  todas_funciones <- unlist(reexports)
  tabla_funciones <- table(todas_funciones)
  nombres_duplicados <- names(tabla_funciones)[tabla_funciones > 1]

  # Mapear funci\u00f3n -> paquetes que la exportan
  duplicates_map <- lapply(nombres_duplicados, function(fun) {
    pkgs <- names(reexports)[sapply(reexports, function(x) fun %in% x)]
    return(pkgs)
  })
  names(duplicates_map) <- nombres_duplicados

  # Si hay duplicados, reportar conflictos
  if (length(nombres_duplicados) > 0) {
    content <- c(
      content,
      "# NOTA: Se detectaron posibles conflictos en los siguientes nombres de funci\u00F3n:",
      "",
      "# Resoluci\u00F3n de conflictos:",
      "# Al cargar varios paquetes, pueden existir funciones con el mismo nombre.",
      "# A continuaci\u00F3n se muestra qu\u00E9 paquete \'gana\' para cada conflicto:"
    )

    for (fun in nombres_duplicados) {
      pkgs <- duplicates_map[[fun]]
      # Por defecto, el \u00faltimo paquete "gana" - se podr\u00eda implementar una pol\u00edtica diferente
      winner <- pkgs[length(pkgs)]
      content <- c(
        content,
        paste0("# - ", fun, ": ", paste(pkgs, collapse = " vs "), " -> ", winner, " (ganador)")
      )
    }

    content <- c(content, "")
    log_debug(paste("Conflicts detected:", paste(nombres_duplicados, collapse = ", ")))
  }

  # Generar secciones para cada paquete
  for (pkg in names(reexports)) {
    content <- c(
      content,
      paste0("# Re-exportaciones de ", pkg),
      ""
    )

    # Obtener lista de funciones para este paquete
    pkg_functions <- reexports[[pkg]]

    # Filtrar duplicados donde este paquete no es el "ganador"
    pkg_functions <- setdiff(
      pkg_functions,
      unlist(lapply(nombres_duplicados, function(fun) {
        pkgs <- duplicates_map[[fun]]
        winner <- pkgs[length(pkgs)]
        if (fun %in% pkg_functions && pkg != winner) {
          return(fun)
        } else {
          return(NULL)
        }
      }))
    )

    # Para cada funci\u00f3n del paquete
    for (fun in pkg_functions) {
      # Verificar si es una funci\u00f3n S3 gen\u00e9rica
      is_s3 <- fun %in% s3_generics[[pkg]]

      if (is_s3) {
        content <- c(
          content,
          paste0("#' @export ", fun),
          paste0("#' @importFrom ", pkg, " ", fun),
          paste0(fun, " <- ", pkg, "::", fun),
          ""
        )
      } else {
        content <- c(
          content,
          paste0("#' @importFrom ", pkg, " ", fun),
          "#' @export",
          fun,
          ""
        )
      }
    }
  }


  # Crear directorio si no existe
  if (!dir.exists(ruta_destino)) {
    dir.create(ruta_destino, recursive = TRUE)
  }

  # Escribir archivo
  reexports_file <- file.path(ruta_destino, "reexports.R")
  .write_utf8(content, reexports_file)

  if (verbose) {
    message(.lv_trf(
      "Re-export file created with %d functions from %d packages.",
      sum(vapply(reexports, length, integer(1))), length(reexports)
    ))
  }

  # Devolver informaci\u00f3n de las funciones reexportadas
  result <- list(
    reexports = reexports,
    conflicts = duplicates_map,
    file = reexports_file
  )

  invisible(result)
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
  MIN_PATH_LENGTH <- 3  # Very short paths are suspicious

  # Lista de directorios del sistema o importantes que nunca deben eliminarse
  PROTECTED_DIRS <- c(
    # Directorios del sistema en Windows
    "bin", "boot", "dev", "etc", "home", "lib", "mnt", "opt", "proc", "root",
    "run", "sbin", "srv", "sys", "tmp", "usr", "var", "Program Files",
    "Windows", "Users", "System32", "AppData", "ProgramData",

    # Directorios espec\u00edficos de R y desarrollo
    "library", "include", "share", "R", "Rtools", "Git", "src",

    # Directorios de control de versiones y configuraci\u00f3n
    ".git", ".svn", ".hg", "node_modules"
  )

  # Patrones de rutas que podr\u00edan ser peligrosas
  DANGEROUS_PATTERNS <- c(
    "^[A-Za-z]:\\\\$",  # C:\, D:\, etc.
    "^/$",             # Ra\u00edz del sistema en Unix
    "^\\\\\\\\",       # Rutas UNC \\servidor\
    "^~$",             # Directorio home
    "^\\.$",           # Directorio actual
    "^\\.\\.$"         # Directorio padre
  )

  # Solo realizar verificaciones detalladas si se solicita
  if (verify) {
    if (is.character(path) && length(path) > 0) {
      for (p in path) {
        # 1. Verificar longitud de la ruta (evita borrar "/", "C:\", etc.)
        if (nchar(p) < MIN_PATH_LENGTH) {
          message(.lv_trf("SAFETY: Path is too short and may be dangerous: %s", p))
          return(invisible(FALSE))
        }

        # 2. Verificar patrones peligrosos
        if (any(sapply(DANGEROUS_PATTERNS, function(pattern) grepl(pattern, p)))) {
          message(.lv_trf("SAFETY: Potentially dangerous path pattern: %s", p))
          return(invisible(FALSE))
        }

        # 3. Verificar si es un directorio existente
        if (dir.exists(p)) {
          # 3.1 Verificar si es un directorio protegido
          if (basename(p) %in% PROTECTED_DIRS) {
            message(.lv_trf("SAFETY: Potentially important directory: %s", p))
            return(invisible(FALSE))
          }

          # 3.2 Si recursive=TRUE y force=TRUE, realizar verificaciones adicionales
          if (recursive && force) {
            # Verificar si parece un paquete R
            has_desc <- file.exists(file.path(p, "DESCRIPTION"))
            has_r_dir <- dir.exists(file.path(p, "R"))
            has_man_dir <- dir.exists(file.path(p, "man"))

            if (has_desc && (has_r_dir || has_man_dir)) {
              # Verificar si es un directorio temporal de paquete R
              is_temp_pkg <- grepl("^00LOCK-|^\\.Rcheck$|^tmp|^temp", basename(p))

              if (!is_temp_pkg) {
                message(.lv_trf("SAFETY: Possible non-temporary R package directory: %s", p))
                return(invisible(FALSE))
              }
            }
          }
        }
      }
    }
  }

  # Si pas\u00f3 todas las verificaciones, proceder con unlink normal
  result <- unlink(path, recursive = recursive, force = force)

  # Verificar si la eliminaci\u00f3n fue exitosa
  if (result != 0) {
    warning(.lv_trf("Could not remove completely: %s", paste(path, collapse = ", ")),
            call. = FALSE)
  }

  return(result)
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
  # Normalizar rutas para manejar diferencias entre sistemas operativos
  inner <- normalizePath(inner_path, mustWork = FALSE)
  outer <- normalizePath(outer_path, mustWork = FALSE)

  # En Windows, convertir barras invertidas a barras normales
  if (.Platform$OS.type == "windows") {
    inner <- gsub("\\\\", "/", inner)
    outer <- gsub("\\\\", "/", outer)
  }

  # Asegurar que outer termina con barra para evitar coincidencias parciales
  # (e.g., evitar que "/usr/local" coincida con "/usr/localhost")
  if (!endsWith(outer, "/")) {
    outer <- paste0(outer, "/")
  }

  # Verificar si inner_path comienza con outer_path
  return(startsWith(inner, outer))
}









#' Create a basic generated-metapackage vignette
#'
#' Writes an English R Markdown introduction and ensures DESCRIPTION declares
#' its vignette builder and suggested packages.
#'
#' @param nombre Character metapackage name.
#' @param paquetes_locales Character archive stems including versions.
#' @param ruta_proyecto Character project directory.
#' @param verbose Logical progress toggle.
#' @return Invisible `NULL`; called for side effects.
#' @noRd

crear_vignette_basica <- function(nombre, paquetes_locales, ruta_proyecto, verbose = FALSE) {
  # Verificar que estamos en el directorio correcto
  if (basename(getwd()) != basename(ruta_proyecto)) {
    warning(.lv_trf(
      "Current directory (%s) does not match project directory (%s).",
      getwd(), ruta_proyecto
    ), call. = FALSE)
    # Intentar cambiar al directorio correcto si es necesario
    if (dir.exists(ruta_proyecto)) {
      old_dir <- getwd()
      on.exit(setwd(old_dir), add = TRUE)
      setwd(ruta_proyecto)
      message(.lv_trf("Temporarily changed directory to: %s", ruta_proyecto))
    }
  }

  # Verificar que el archivo DESCRIPTION existe
  desc_file <- "DESCRIPTION"
  if (!file.exists(desc_file)) {
    warning(.lv_tr("The DESCRIPTION file does not exist in the current directory."),
            call. = FALSE)
    return(invisible(FALSE))
  }

  # Crear directorio de vi\u00f1etas si no existe
  vignette_dir <- "vignettes"
  if (!dir.exists(vignette_dir)) {
    dir.create(vignette_dir, recursive = TRUE, showWarnings = TRUE)
  }

  # Resto de la funci\u00f3n igual, pero con manejo de errores
  tryCatch({
    # Nombre de paquetes base (sin versi\u00f3n)
    paquetes_base <- unique(sub("_.*", "", paquetes_locales))

    # Crear contenido de la vi\u00f1eta
    vignette_content <- paste0(
      "---\n",
      "title: \"Introduction to ", nombre, "\"\n",
      "output: rmarkdown::html_vignette\n",
      "vignette: >\n",
      "  %\\VignetteIndexEntry{Introduction to ", nombre, "}\n",
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
      "`", nombre, "` is a metapackage for installing and attaching local packages.\n\n",
      "## Included packages\n\n",
      "This metapackage includes:\n\n",
      paste(sapply(paquetes_base, function(pkg) paste0("* `", pkg, "`\n")), collapse = ""),
      "\n\n## Basic use\n\n",
      "To attach all installed components:\n\n",
      "```{r eval=FALSE}\n",
      "library(", nombre, ")\n",
      "```\n\n",
      "## Available functions\n\n",
      "* `", nombre, "_install()`: installs components from local archives.\n",
      "* `", nombre, "_attach()`: attaches installed components.\n",
      "* `", nombre, "_detach()`: detaches all components.\n",
      "* `", nombre, "_packages()`: lists included packages.\n"
    )

    # Escribir a archivo
    vignette_file <- file.path(vignette_dir, paste0("introduccion-", nombre, ".Rmd"))
    .write_utf8(vignette_content, vignette_file)

    # Gitignore para vignettes
    gitignore_content <- "# Automatically created files\n*.html\n*.R\n"
    .write_utf8(gitignore_content, file.path(vignette_dir, ".gitignore"))

    # Leer y actualizar DESCRIPTION de forma segura
    tryCatch({
      desc_content <- readLines(desc_file)

      # Buscar o a\u00f1adir VignetteBuilder
      if (!any(grepl("^VignetteBuilder:", desc_content))) {
        desc_content <- c(desc_content, "VignetteBuilder: knitr")
      }

      # Buscar o actualizar la secci\u00f3n Suggests
      suggests_line <- grep("^Suggests:", desc_content)

      if (length(suggests_line) > 0) {
        # Determinar nivel de indentaci\u00f3n en base a otras secciones
        # Buscar indentaci\u00f3n en Depends o Imports como referencia
        indent_pattern <- grep("^(Depends|Imports):", desc_content, value = TRUE)
        if (length(indent_pattern) > 0) {
          # Encontrar la siguiente l\u00ednea despu\u00e9s de Depends/Imports para ver indentaci\u00f3n
          dep_line <- which(grepl("^(Depends|Imports):", desc_content))[1]
          if (dep_line < length(desc_content)) {
            indent <- gsub("^(\\s*).*", "\\1", desc_content[dep_line + 1])
            if (nchar(indent) == 0) indent <- "    " # Usar 4 espacios por defecto
          } else {
            indent <- "    " # Usar 4 espacios por defecto
          }
        } else {
          indent <- "    " # Usar 4 espacios por defecto
        }

        # Extraer paquetes ya sugeridos
        suggests <- desc_content[suggests_line]

        # Buscar l\u00edneas adicionales con indentaci\u00f3n que contin\u00faan el campo Suggests
        next_line <- suggests_line + 1
        while (next_line <= length(desc_content) &&
               (grepl("^\\s+", desc_content[next_line]) || desc_content[next_line] == "")) {
          suggests <- c(suggests, desc_content[next_line])
          next_line <- next_line + 1
        }

        # Extraer nombres de paquetes (ignorando el prefijo "Suggests:" y cualquier indentaci\u00f3n)
        suggests_text <- paste(suggests, collapse = " ")
        suggests_text <- sub("^Suggests:\\s*", "", suggests_text)
        suggests_pkgs <- trimws(unlist(strsplit(suggests_text, ",")))

        # Eliminar cualquier entrada vac\u00eda
        suggests_pkgs <- suggests_pkgs[nzchar(suggests_pkgs)]

        # A\u00f1adir rmarkdown y knitr si no est\u00e1n ya
        if (!any(grepl("rmarkdown", suggests_pkgs))) suggests_pkgs <- c(suggests_pkgs, "rmarkdown")
        if (!any(grepl("knitr", suggests_pkgs))) suggests_pkgs <- c(suggests_pkgs, "knitr")

        # Ordenar los paquetes alfab\u00e9ticamente para mayor claridad
        suggests_pkgs <- sort(suggests_pkgs)

        # Construir el nuevo campo Suggests con el formato correcto
        # Primera l\u00ednea con "Suggests:"
        new_suggests <- "Suggests:"

        # A\u00f1adir paquetes con indentaci\u00f3n correcta
        if (length(suggests_pkgs) > 0) {
          pkgs_formatted <- paste0(indent, paste(suggests_pkgs, collapse = paste0(",\n", indent)))
          new_suggests <- paste0(new_suggests, "\n", pkgs_formatted)
        }

        # Reemplazar todas las l\u00edneas de Suggests con la nueva versi\u00f3n formateada
        desc_content <- desc_content[-(suggests_line:(next_line-1))]
        desc_content <- append(desc_content, new_suggests, after = suggests_line - 1)
      } else {
        # A\u00f1adir nueva secci\u00f3n Suggests con formato adecuado
        new_suggests <- "Suggests:\n    rmarkdown,\n    knitr"
        desc_content <- c(desc_content, new_suggests)
      }

      # Guardar archivo DESCRIPTION actualizado
      .write_utf8(desc_content, desc_file)

      if (verbose) {
        message(.lv_tr("DESCRIPTION updated with a valid Suggests field"))
      }
    }, error = function(e) {
      warning(.lv_trf("Error updating DESCRIPTION: %s", e$message), call. = FALSE)
    })

    if (verbose) {
      message(.lv_trf("Basic vignette created at %s", vignette_file))
    }
  }, error = function(e) {
    warning(.lv_trf("Error creating basic vignette: %s", e$message), call. = FALSE)
    return(invisible(FALSE))
  })

  invisible(TRUE)

}



#' Build a local meta-package
#'
#' @description
#' Creates the full structure and files of a meta-package that installs, manages
#' and loads a set of locally stored R packages, resolving the dependencies between
#' them with a graph-based (topologically ordered) approach.
#'
#' @param name Character. Name of the meta-package to create (must not contain
#'   underscores `_`).
#' @param packages Character vector. Names (with version) of the local
#'   packages to include, e.g. `"myPackage_1.0.0"`.
#' @param pkg_dir Character. Directory containing the local archive files
#'   (`.tar.gz`, `.zip`, etc.).
#' @param ext Character. Archive extension. Defaults to `".tar.gz"`.
#' @param version Character. Version of the meta-package. Defaults to `"0.1.0"`.
#' @param dest_dir Character. Directory in which to create the meta-package.
#'   If `NULL`, it is created in the current directory under the meta-package name.
#' @param reexport Logical. If `TRUE`, re-exports the component packages'
#'   functions so they are reachable directly through the meta-package (tidyverse
#'   style). Defaults to `FALSE`.
#' @param document Logical. If `TRUE`, runs `devtools::document()`
#'   automatically. Defaults to `TRUE`.
#' @param verbose Logical. If `TRUE`, shows progress messages. The default follows
#'   `getOption("bigbang.verbose", interactive())`.
#' @param authors Character. Content for the `Authors@R` field of DESCRIPTION.
#' @param description Character. Description of the meta-package.
#' @param license Character. License of the meta-package.
#' @param additional_deps Character vector. Extra dependencies to add on top of the
#'   ones detected automatically.
#' @param ignore_deps Character vector. Dependencies to ignore even if detected.
#' @param import_deps Character vector. Packages that should go in the `Imports`
#'   field of DESCRIPTION rather than `Depends`. Imports are not attached when the
#'   user calls `library()` on the meta-package, but remain available via `::`
#'   (e.g. `dplyr::filter()`), reducing name clashes in the user's workspace.
#' @param force_deps Character vector. Exact package names to use as dependencies,
#'   bypassing automatic detection. If supplied, only these are used as the
#'   meta-package's implicit dependencies.
#' @param debug Logical. If `TRUE`, emits detailed debugging messages. Defaults
#'   to `FALSE`.
#'
#' @return Invisibly, a `bigbang_result` containing the generated path,
#'   component archives, dependency classification, and documentation status.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Creates the basic R package structure (`R`, `man`, `vignettes`, etc.).
#' 2. Detects dependencies between packages, both explicit (from DESCRIPTION) and
#'    implicit (found by scanning the source code).
#' 3. Generates DESCRIPTION and NAMESPACE with the appropriate dependencies.
#' 4. Creates a basic vignette documenting the meta-package.
#' 5. Generates R files with functions to install and load the component packages:
#'    - `<name>_install()`: installs the component packages from the local archives.
#'    - `<name>_attach()`: attaches the components that are already installed.
#'    - `<name>_detach()`: detaches all the meta-package's components.
#'    - `<name>_packages()`: lists the included packages.
#'
#' Installation is **explicit**: calling `library(<meta>)` attaches the components
#' that are already installed and reports which ones are missing, but does not
#' install anything or delete any files. To install the components from the local
#' archives, the user calls `<meta>_install()`. Installation resolves dependencies
#' with a graph-based topological ordering that also detects circular dependencies.
#'
#' If `reexport = TRUE`, a `reexports.R` file is generated so users can
#' reach the component functions directly through the meta-package
#' (`meta::fun()` instead of `component::fun()`), tidyverse style.
#'
#' @section Requirements:
#' - The local packages must exist in `pkg_dir` with the given extension.
#' - Automatic documentation (`document = TRUE`) requires the
#'   `devtools` package.
#'
#' @examples
#' \dontrun{
#' # Basic: a meta-package with two local packages
#' create_metapackage(
#'   name = "MyMeta",
#'   packages = c("pkg1_1.0.0", "pkg2_0.8.3"),
#'   pkg_dir = "path/to/archives"
#' )
#'
#' # Advanced: with re-exports and custom dependencies
#' create_metapackage(
#'   name = "AnalyticsMeta",
#'   packages = c("myStats_1.2.0", "myPlots_0.9.1"),
#'   pkg_dir = "path/to/archives",
#'   reexport = TRUE,
#'   additional_deps = c("ggplot2", "dplyr"),
#'   import_deps = c("data.table", "purrr", "tibble")
#' )
#' }
#' @export

create_metapackage <- function(
    name,
    packages,
    pkg_dir,
    ext = ".tar.gz",
    version = "0.1.0",
    dest_dir = NULL,
    reexport = FALSE,
    document = TRUE,
    verbose = getOption("bigbang.verbose", interactive()),
    authors = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
    description = "Local Package Metapackage",
    license = "MIT + file LICENSE",
    additional_deps = NULL,
    ignore_deps = NULL,
    import_deps = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts", "zoo"),
    force_deps = NULL,
    debug = FALSE
) {

  nombre <- name
  paquetes_locales <- packages
  ruta_instalables <- pkg_dir
  ruta_destino <- dest_dir
  reexportar_funciones <- reexport
  generar_documentacion <- document
  mostrar_progreso <- isTRUE(verbose)
  autores <- authors
  descripcion <- description
  licencia <- license
  deps_adicionales <- additional_deps
  deps_ignorar <- ignore_deps
  deps_imports <- import_deps
  deps_forzar <- force_deps
  verbose <- isTRUE(debug)

  # Verificaciones iniciales
  if (!is.character(nombre) || length(nombre) != 1) {
    stop(.lv_tr("'name' must be one character string"), call. = FALSE)
  }
  if (!is.character(paquetes_locales) || length(paquetes_locales) < 1) {
    stop(.lv_tr("'packages' must be a non-empty character vector"), call. = FALSE)
  }
  if (!is.character(ruta_instalables) || length(ruta_instalables) != 1) {
    stop(.lv_tr("'pkg_dir' must be one character string"), call. = FALSE)
  }
  if (!dir.exists(ruta_instalables)) {
    stop(.lv_tr("The directory specified by 'pkg_dir' does not exist"), call. = FALSE)
  }

  # Verificar que el nombre del paquete es v\u00e1lido
  if (grepl("_", nombre)) {
    nombre_sugerido <- gsub("_", ".", nombre)
    stop(.lv_trf(
      "Package name '%s' contains underscores, which R package names do not allow. Use '%s' instead.",
      nombre, nombre_sugerido
    ), call. = FALSE)
  }
  # Funci\u00f3n de registro para depuraci\u00f3n
  log_debug <- function(msg) {
    if (verbose) message(paste0("DEBUG: ", msg))
  }

  log_debug("Starting create_metapackage()")


  # Determinar la ruta del proyecto
  dir_original <- getwd()
  on.exit(setwd(dir_original), add = TRUE)
  ruta_proyecto <- if (is.null(ruta_destino)) file.path(dir_original, nombre) else file.path(ruta_destino, nombre)

  log_debug(glue::glue("New project path: {ruta_proyecto}"))

  # Una regeneraci\u00f3n in-place no es segura: podr\u00eda conservar c\u00f3digo emitido por
  # versiones antiguas (incluidos hooks y scripts cleanup) o sobrescribir trabajo
  # del usuario. Solo se admite una ruta nueva o un directorio completamente vac\u00edo.
  if (dir.exists(ruta_proyecto)) {
    contenido_existente <- list.files(
      ruta_proyecto, all.files = TRUE, no.. = TRUE
    )
    if (length(contenido_existente) > 0L) {
      .bigbang_abort(
        "bigbang_error_nonempty_dest",
        .lv_trf(
          "For safety, the destination must be new or empty: %s. Generate into a new empty path; never regenerate an existing source in place.",
          ruta_proyecto
        ),
        path = ruta_proyecto
      )
    }
  } else {
    if (mostrar_progreso) {
      message(.lv_trf("Creating package structure at: %s", ruta_proyecto))
    }
    if (!dir.create(ruta_proyecto, showWarnings = TRUE, recursive = TRUE)) {
      stop(.lv_trf("Could not create project directory: %s", ruta_proyecto),
           call. = FALSE)
    }
  }

  for (subdir in c("R", "man", "vignettes")) {
    ruta_subdir <- file.path(ruta_proyecto, subdir)
    if (!dir.create(ruta_subdir, showWarnings = FALSE) && !dir.exists(ruta_subdir)) {
      stop(.lv_trf("Could not create directory: %s", ruta_subdir), call. = FALSE)
    }
  }
  log_debug("Basic directory structure created")

  setwd(ruta_proyecto)
  log_debug(glue::glue("Changed to project directory: {getwd()}"))

  # Verificar que todos los paquetes locales existen
  paquetes_faltantes <- paquetes_locales[!file.exists(file.path(ruta_instalables, paste0(paquetes_locales, ext)))]
  if (length(paquetes_faltantes) > 0) {
    stop(.lv_trf(
      "The following package archives were not found: %s",
      paste(paquetes_faltantes, collapse = ", ")
    ), call. = FALSE)
  }

  # Mostrar progreso si se solicita
  if (mostrar_progreso) {
    message(.lv_trf(
      "Creating metapackage '%s' for %d local packages...",
      nombre, length(paquetes_locales)
    ))
    if (length(paquetes_locales) > 5) {
      message(.lv_trf(
        "Packages: %s... and %d more",
        paste(utils::head(paquetes_locales, 5), collapse = ", "),
        length(paquetes_locales) - 5
      ))
    } else {
      message(.lv_trf("Packages: %s", paste(paquetes_locales, collapse = ", ")))
    }
  }

  # Si se especifican deps_forzar, usar esas en lugar de detectar autom\u00e1ticamente
  if (!is.null(deps_forzar) && length(deps_forzar) > 0) {
    deps_implicitas <- deps_forzar

    if (mostrar_progreso) {
      message(.lv_trf(
        "Using explicitly supplied dependencies: %s",
        paste(deps_implicitas, collapse = ", ")
      ))
    }
  } else {
    # PASO 1: Detectar dependencias impl\u00edcitas (c\u00f3digo existente)
    if (mostrar_progreso) {
      message(.lv_tr("Scanning local packages for implicit dependencies..."))
    }

    deps_implicitas <- detectar_dependencias_implicitas(paquetes_locales, ruta_instalables, ext)

    # A\u00f1adir dependencias adicionales especificadas por el usuario
    if (!is.null(deps_adicionales) && length(deps_adicionales) > 0) {
      deps_implicitas <- unique(c(deps_implicitas, deps_adicionales))
    }

    # Eliminar dependencias expl\u00edcitamente ignoradas
    if (!is.null(deps_ignorar) && length(deps_ignorar) > 0) {
      deps_implicitas <- setdiff(deps_implicitas, deps_ignorar)
    }

    if (mostrar_progreso) {
      message(.lv_trf(
        "Detected implicit dependencies: %s",
        paste(deps_implicitas, collapse = ", ")
      ))
    }

  }

  # PASO 2: Extraer dependencias expl\u00edcitas
  # Extraer dependencias de los paquetes locales
  dependencias <- unlist(lapply(paquetes_locales, extraer_dependencias, ruta_instalables, ext))

  # PASO 3: Clasificar dependencias
  # Clasificar dependencias en locales y de CRAN
  deps_clasificados <- clasificar_dependencias(dependencias, ruta_instalables, ext)
  deps_cran <- deps_clasificados$cran
  deps_locales <- deps_clasificados$locales

  # Eliminar utils de deps_cran si est\u00e1 presente
  deps_cran <- setdiff(deps_cran, "utils")

  # Eliminar duplicados antes de escribir el archivo DESCRIPTION
  deps_cran <- unique(deps_cran)

  # PASO 4: Crear DESCRIPTION con dependencias impl\u00edcitas y datos personalizados
  if (mostrar_progreso) {
    message(.lv_tr("Generating DESCRIPTION and NAMESPACE..."))
  }

  write_description_file(
    nombre = nombre,
    version = version,
    deps_implicitas = deps_implicitas,
    deps_imports = deps_imports,
    autores = autores,
    descripcion = descripcion,
    licencia = licencia,
    verbose = verbose
  )


  # Crear una vi\u00f1eta b\u00e1sica para pasar R CMD check. Ahora que el DESCRIPTION existe, crear la vi\u00f1eta
  crear_vignette_basica(nombre, paquetes_locales, ruta_proyecto, verbose = verbose)
  if (verbose) {
    log_debug("Basic vignette created for R CMD check")
  }

  # PASO 5: Crear NAMESPACE con dependencias impl\u00edcitas
  write_namespace_file(
    nombre = nombre,
    paquetes_cran = deps_cran,
    namespace_path = "NAMESPACE",
    deps_implicitas = deps_implicitas,
    deps_imports = deps_imports,
    verbose = verbose
  )

  if(reexportar_funciones){
    namespaceAdditions <- c()
    for (pkg in paquetes_locales) {
      exports <- getNamespaceExports(asNamespace(pkg))
      for (func in exports) {
        namespaceAdditions <- c(namespaceAdditions,
                                paste0("S3method(", func, ", default)"))
      }
    }
  }

  # Si hay S3methods para a\u00f1adir al NAMESPACE
  if (exists("namespaceAdditions") && length(namespaceAdditions) > 0) {
    # Leer el NAMESPACE actual
    namespace_content <- readLines("NAMESPACE")

    # A\u00f1adir los S3methods al final
    namespace_content <- c(namespace_content, "", "# S3 methods from reexports", namespaceAdditions)

    # Escribir el NAMESPACE actualizado
    .write_utf8(namespace_content, "NAMESPACE")

    if (verbose) {
      log_debug(paste("Added", length(namespaceAdditions), "S3 methods to NAMESPACE"))
    }
  }

  log_debug("NAMESPACE file created")

  # Generar contenido del archivo de instalaci\u00f3n
  install_packages_content <- glue::glue('

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
#\' @param nombre_paquete Character archive stem including the version.
#\' @param ruta_instalables Character directory containing local archives.
#\' @param ext Character archive extension.
#\'
#\' @return A character vector of dependency names.
#\' @keywords internal

read_archive_dependencies <- function(nombre_paquete, ruta_instalables, ext = ".tar.gz") {{
  archivo_paquete <- file.path(ruta_instalables, paste0(nombre_paquete, ext))
  if (!file.exists(archivo_paquete)) {{
    stop(.meta_trf("Package archive does not exist: %s", archivo_paquete),
         call. = FALSE)
  }}

  temp_dir <- tempfile("bigbang-deps-")
  dir.create(temp_dir)
  on.exit(safe_unlink(temp_dir, recursive = TRUE), add = TRUE)

  switch(
    ext,
    ".tar.gz" = utils::untar(archivo_paquete, exdir = temp_dir),
    ".tar" = utils::untar(archivo_paquete, exdir = temp_dir),
    ".zip" = utils::unzip(archivo_paquete, exdir = temp_dir),
    stop(.meta_trf("Unsupported archive format: %s", ext), call. = FALSE)
  )

  descripcion_file <- list.files(
    temp_dir, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE
  )
  if (length(descripcion_file) != 1L) {{
    stop(.meta_trf(
      "Expected one DESCRIPTION in %s; found %d.",
      archivo_paquete, length(descripcion_file)
    ), call. = FALSE)
  }}

  desc <- read.dcf(descripcion_file, fields = c("Depends", "Imports", "LinkingTo"))
  dependencias <- unlist(strsplit(paste(desc[!is.na(desc)], collapse = ","), ","),
                         use.names = FALSE)
  dependencias <- trimws(gsub("\\\\s*\\\\([^)]*\\\\)", "", dependencias))
  unique(dependencias[nzchar(dependencias) & dependencias != "R"])
}}


#\' Classify a local package archive
#\'
#\' A ZIP containing `Meta/package.rds` is a Windows binary package. Other ZIP
#\' archives are treated as source archives and are unpacked before installation.
#\'
#\' @return One of `"source"`, `"source.zip"`, or `"win.binary"`.
#\' @keywords internal
classify_package_archive <- function(archivo_paquete, ext) {{
  if (!identical(tolower(ext), ".zip")) return("source")

  members <- utils::unzip(archivo_paquete, list = TRUE)$Name
  members <- gsub("\\\\", "/", members, fixed = TRUE)
  has_description <- any(grepl("(^|/)DESCRIPTION$", members))
  if (!has_description) {{
    stop(.meta_trf(
      "The ZIP archive does not contain a DESCRIPTION file: %s", archivo_paquete
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
#\' @param nombre_paquete Character archive stem including the version.
#\' @param ruta_instalables Character directory containing local archives.
#\' @param ext Character archive extension.
#\' @param repos Character repositories for non-local dependencies.
#\' @param cran_deps Character missing-dependency policy: `"skip"` and `"error"`
#\'   never access the network; `"install"` uses `repos`.
#\'
#\' @return A list with installation status and detected dependencies.
#\' @keywords internal
install_loc_pkg_w_dep <- function(nombre_paquete, ruta_instalables, ext = ".tar.gz",
                                   repos = getOption("repos"),
                                   cran_deps = c("skip", "error", "install")) {{
  cran_deps <- match.arg(cran_deps)
  archivo_paquete <- file.path(ruta_instalables, paste0(nombre_paquete, ext))
  if (!file.exists(archivo_paquete)) {{
    return(list(
      success = FALSE,
      message = .meta_trf("Package archive does not exist: %s", archivo_paquete)
    ))
  }}

  nombre_base <- sub("_.*", "", nombre_paquete)
  version <- sub("^[^_]+_", "", nombre_paquete)
  dependencias <- tryCatch(
    read_archive_dependencies(nombre_paquete, ruta_instalables, ext),
    error = function(e) e
  )
  if (inherits(dependencias, "error")) {{
    return(list(success = FALSE, message = conditionMessage(dependencias)))
  }}

  paq_instalado <- tryCatch(
    utils::packageVersion(nombre_base) >= base::package_version(version),
    error = function(e) FALSE
  )
  if (paq_instalado) {{
    message(.meta_trf(
      "Package %s (version %s) is already installed.", nombre_base, version
    ))
    return(list(
      success = TRUE,
      message = .meta_tr("Already installed"),
      instalados = stats::setNames(list(.meta_tr("Already installed")), nombre_paquete),
      dependencias = dependencias
    ))
  }}

  archivos_locales <- list.files(ruta_instalables)
  archivos_locales <- archivos_locales[endsWith(archivos_locales, ext)]
  locales <- sub("_.*", "", substr(
    archivos_locales, 1L, nchar(archivos_locales) - nchar(ext)
  ))

  # Las dependencias locales se instalan una sola vez, por el bucle exterior y en
  # graph. This branch resolves only dependencies not provided by local archives.
  no_locales_faltantes <- setdiff(dependencias, locales)
  no_locales_faltantes <- no_locales_faltantes[!vapply(
    no_locales_faltantes, requireNamespace, logical(1), quietly = TRUE
  )]
  if (length(no_locales_faltantes) > 0L && cran_deps != "install") {{
    detalle <- paste(no_locales_faltantes, collapse = ", ")
    if (cran_deps == "skip") {{
      return(list(
        success = FALSE,
        skipped = TRUE,
        message = .meta_trf("Skipped because non-local dependencies are missing: %s", detalle),
        omitidos = stats::setNames(as.list(rep(.meta_tr("Missing"), length(no_locales_faltantes))),
                                    no_locales_faltantes),
        dependencias = dependencias
      ))
    }}
    return(list(
      success = FALSE,
      message = .meta_trf("Missing non-local dependencies: %s", detalle),
      fallidos = stats::setNames(as.list(rep(.meta_tr("Missing"), length(no_locales_faltantes))),
                                  no_locales_faltantes),
      dependencias = dependencias
    ))
  }}

  if (length(no_locales_faltantes) > 0L && cran_deps == "install") {{
    repos_invalidos <- is.null(repos) || length(repos) == 0L ||
      all(is.na(repos) | !nzchar(repos) | repos == "@CRAN@")
    if (repos_invalidos) {{
      detalle <- paste(no_locales_faltantes, collapse = ", ")
      return(list(
        success = FALSE,
        message = .meta_trf(
          "Cannot install non-local dependencies without a configured repository: %s",
          detalle
        ),
        fallidos = stats::setNames(as.list(rep(.meta_tr("Repository not configured"),
                                                length(no_locales_faltantes))),
                                    no_locales_faltantes),
        dependencias = dependencias
      ))
    }}
    for (dep in no_locales_faltantes) {{
      message(.meta_trf("Installing non-local dependency: %s", dep))
      tryCatch(
        utils::install.packages(dep, dependencies = TRUE, repos = repos),
        error = function(e) warning(conditionMessage(e), call. = FALSE)
      )
    }}
  }}

  faltantes <- dependencias[!vapply(
    dependencias, requireNamespace, logical(1), quietly = TRUE
  )]
  if (length(faltantes) > 0L) {{
    return(list(
      success = FALSE,
      message = .meta_trf(
        "Dependencies are not installed: %s", paste(faltantes, collapse = ", ")
      ),
      fallidos = stats::setNames(as.list(rep(.meta_tr("Not installed"), length(faltantes))), faltantes),
      dependencias = dependencias
    ))
  }}

  tipo_archivo <- tryCatch(
    classify_package_archive(archivo_paquete, ext),
    error = function(e) e
  )
  if (inherits(tipo_archivo, "error")) {{
    return(list(success = FALSE, message = conditionMessage(tipo_archivo)))
  }}
  if (identical(tipo_archivo, "win.binary") && .Platform$OS.type != "windows") {{
    return(list(
      success = FALSE,
      message = .meta_tr("Windows binary ZIP packages can only be installed on Windows.")
    ))
  }}

  objetivo_instalacion <- archivo_paquete
  tipo_instalacion <- if (identical(tipo_archivo, "win.binary")) "win.binary" else "source"
  if (identical(tipo_archivo, "source.zip")) {{
    temp_fuente <- tempfile("bigbang-source-zip-")
    dir.create(temp_fuente)
    on.exit(safe_unlink(temp_fuente, recursive = TRUE), add = TRUE)
    utils::unzip(archivo_paquete, exdir = temp_fuente)
    descripciones <- list.files(
      temp_fuente, pattern = "^DESCRIPTION$", full.names = TRUE, recursive = TRUE
    )
    if (length(descripciones) != 1L) {{
      return(list(
        success = FALSE,
        message = .meta_trf(
          "Expected one DESCRIPTION in source ZIP; found %d", length(descripciones)
        )
      ))
    }}
    objetivo_instalacion <- dirname(descripciones[[1L]])
  }}

  error_instalacion <- NULL
  tryCatch(
    utils::install.packages(
      objetivo_instalacion, repos = NULL, type = tipo_instalacion, dependencies = FALSE
    ),
    error = function(e) error_instalacion <<- conditionMessage(e)
  )

  instalado <- is.null(error_instalacion) && tryCatch(
    utils::packageVersion(nombre_base) >= base::package_version(version),
    error = function(e) FALSE
  )
  if (!instalado) {{
    detalle <- if (is.null(error_instalacion)) {{
      .meta_tr("Installation could not be verified")
    }} else {{
      error_instalacion
    }}
    return(list(
      success = FALSE,
      message = detalle,
      fallidos = stats::setNames(list(detalle), nombre_paquete),
      dependencias = dependencias
    ))
  }}

  message(.meta_trf("Installed package %s successfully.", nombre_paquete))
  list(
    success = TRUE,
    message = .meta_tr("Installed successfully"),
    instalados = stats::setNames(list(.meta_tr("Installed successfully")), nombre_paquete),
    dependencias = dependencias
  )
}}


#\' Detect cycles in a dependency graph
#\'
#\' Analyzes an adjacency matrix and returns circular package dependencies.
#\'
#\' @param matriz_adj Matrix. A value of 1 means the row package depends on the
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
#\'   ciclos <- detect_cycles(mat)
#\'   print(ciclos)
#\' }}
#\'
#\' @keywords internal
detect_cycles <- function(matriz_adj) {{
  n <- nrow(matriz_adj)
  visited <- rep(FALSE, n)
  rec_stack <- rep(FALSE, n)
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

    for (u in which(matriz_adj[v, ] == 1)) {{
      if (dfs(u, path)) return(TRUE)
    }}

    rec_stack[v] <<- FALSE
    return(FALSE)
  }}

  for (i in 1:n) {{
    if (!visited[i]) dfs(i)
  }}

  return(cycles)
}}



#\' Build a dependency graph from local packages
#\'
#\' Reads each archive DESCRIPTION without installing or loading packages and
#\' builds the adjacency matrix used for installation ordering.
#\'
#\' @param paquetes_locales Character archive stems including versions.
#\' @param ruta_instalables Character archive directory.
#\' @param ext Character archive extension.
#\'
#\' @return An adjacency matrix.
#\'
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   adj <- crear_grafo_dependencias(
#\'     paquetes_locales = c("uspr_0.8.6", "conexiones_0.8.3"),
#\'     ruta_instalables = "X:/ruta",
#\'     ext = ".tar.gz"
#\'   )
#\'   print(adj)
#\' }}

crear_grafo_dependencias <- function(paquetes_locales, ruta_instalables, ext) {{
  n <- length(paquetes_locales)
  matriz_adj <- base::matrix(0, nrow = n, ncol = n)
  rownames(matriz_adj) <- colnames(matriz_adj) <- paquetes_locales

  for (paq in paquetes_locales) {{
    deps <- read_archive_dependencies(paq, ruta_instalables, ext)
    deps_locales <- intersect(deps, sub("_.*", "", paquetes_locales))
    for (dep in deps_locales) {{
      idx_dep <- which(sub("_.*", "", paquetes_locales) == dep)
      if (length(idx_dep) > 0) {{
        idx_paq <- which(paquetes_locales == paq)
        matriz_adj[idx_paq, idx_dep[1]] <- 1
      }}
    }}
  }}


  # Check cycles
  cycles <- detect_cycles(matriz_adj)
  if (length(cycles) > 0) {{
    # Convert indices to package names
    named_cycles <- lapply(cycles, function(cycle) {{
      paquetes_locales[cycle]
    }})

    cycle_text <- paste(
      vapply(named_cycles, paste, character(1), collapse = " -> "),
      collapse = "; "
    )
    .bigbang_abort(
      "bigbang_error_cycle",
      .meta_trf(
        "Circular dependencies detected (Dependencias circulares): %s. A clean installation has no valid topological order.",
        cycle_text
      ),
      cycles = named_cycles
    )
  }}

  return(matriz_adj)
}}

#\' Topologically sort a dependency graph
#\'
#\' Uses DFS on the adjacency matrix to find an installation order.
#\'
#\' @param matriz_adj Matrix where 1 means the row depends on the column.
#\'
#\' @return An integer vector containing the topological order.
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   mat <- base::matrix(c(0,1,0,0), nrow=2, byrow=TRUE)
#\'   rownames(mat) <- colnames(mat) <- c("conexiones_0.8.3", "uspr_0.8.6")
#\'   ord <- ordenamiento_topologico(mat)
#\'   print(ord)
#\' }}

ordenamiento_topologico <- function(matriz_adj) {{
  n <- nrow(matriz_adj)
  visitados <- rep(FALSE, n)
  orden <- integer(0)

  dfs <- function(v) {{
    visitados[v] <<- TRUE
    for (u in which(matriz_adj[v, ] == 1)) {{
      if (!visitados[u]) {{
        dfs(u)
      }}
    }}
    orden <<- c(orden, v)
  }}

  for (i in seq_len(n)) {{
    if (!visitados[i]) {{
      dfs(i)
    }}
  }}

  return(orden)

}}

#\' Install local packages in dependency order
#\'
#\' Builds the graph, computes its topological order, and installs each package
#\' exactly once.
#\'
#\' @param paquetes_locales Character archive stems including versions.
#\' @param ruta_instalables Character archive directory.
#\' @param ext Character archive extension.
#\' @param mostrar_progreso Logical progress toggle.
#\' @return Invisibly, installation, failure, skip, and order information.
#\' @keywords internal
#\'
#\' @examples
#\' \\dontrun{{
#\'   install_packages_in_order(
#\'     paquetes_locales = c("uspr_1.0.0", "conexiones_0.8.3"),
#\'     ruta_instalables = "X:/ruta"
#\'   )
#\' }}


install_packages_in_order <- function(paquetes_locales, ruta_instalables, ext,
                                      mostrar_progreso = TRUE,
                                      repos = getOption("repos"),
                                      cran_deps = c("skip", "error", "install")) {{
  cran_deps <- match.arg(cran_deps)
  matriz_adj <- crear_grafo_dependencias(paquetes_locales, ruta_instalables, ext)
  orden_instalacion <- ordenamiento_topologico(matriz_adj)

  paquetes_instalados <- list()
  paquetes_fallidos <- list()
  paquetes_omitidos <- list()
  pb <- NULL

  total_pkgs <- length(paquetes_locales)
  if (mostrar_progreso && interactive() && total_pkgs > 1) {{
    message(.meta_trf("Starting installation of %d packages", total_pkgs))
    pb <- utils::txtProgressBar(min = 0, max = total_pkgs, style = 3)
    on.exit(close(pb), add = TRUE)
  }}

  for (i in seq_along(orden_instalacion)) {{
    idx <- orden_instalacion[i]
    paq <- paquetes_locales[idx]

    resultado <- tryCatch(
      install_loc_pkg_w_dep(
        paq, ruta_instalables, ext, repos = repos, cran_deps = cran_deps
      ),
      error = function(e) list(success = FALSE, message = conditionMessage(e))
    )

    if (isTRUE(resultado$success)) {{
      paquetes_instalados[[paq]] <- resultado$message
    }} else if (isTRUE(resultado$skipped)) {{
      paquetes_omitidos[[paq]] <- resultado$message
      warning(.meta_trf("Skipped %s: %s", paq, resultado$message), call. = FALSE)
    }} else {{
      paquetes_fallidos[[paq]] <- resultado$message
      warning(.meta_trf("Installation failed for %s: %s", paq, resultado$message),
              call. = FALSE, immediate. = TRUE)
    }}

    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  }}

  if (!is.null(pb)) message(.meta_tr("Installation complete."))

  # SAFETY (2026-07): no cleanup is performed relative to the current directory.

  invisible(list(
    instalados = paquetes_instalados,
    fallidos = paquetes_fallidos,
    omitidos = paquetes_omitidos,
    orden = paquetes_locales[orden_instalacion]
  ))
}}

# fix 2026-07: {nombre}_load_all se define una sola vez, en attach.R.
# The historical duplicate load-all definition was removed.

#\' List all metapackage dependencies
#\'
#\' Returns component names and dependencies read from their archives.
#\'
#\' @param ruta_instalables Character archive directory.
#\' @param ext Character archive extension.
#\' @return A sorted character vector of dependency names.
#\' @export
{nombre}_deps <- function(
    ruta_instalables = {paste(deparse(ruta_instalables), collapse = "")},
    ext = {paste(deparse(ext), collapse = "")}) {{
  paquetes_locales <- {.r_literal(paquetes_locales)}
  deps <- unlist(lapply(
    paquetes_locales, read_archive_dependencies,
    ruta_instalables = ruta_instalables, ext = ext
  ), use.names = FALSE)
  sort(unique(c(sub("_.*", "", paquetes_locales), deps)))
}}
')

  install_packages_content <- .drop_regular_comment_lines(install_packages_content)
  .write_utf8(install_packages_content, file.path(ruta_proyecto, "R", "install_packages.R"))
  log_debug("install_packages.R created")

  # Crear archivo LICENSE
  if (grepl("file[[:space:]]+LICENSE", licencia, ignore.case = TRUE)) {
    license_content <- c(
      paste0("YEAR: ", format(Sys.Date(), "%Y")),
      paste0("COPYRIGHT HOLDER: ", .copyright_holders(autores))
    )
    .write_utf8(license_content, "LICENSE")
    log_debug("LICENSE file created")
  }

  # Runtime translations belong to the generated metapackage, so it receives
  # its own source catalog and a precompiled catalog for environments without
  # gettext build tools. The conditional keeps standalone sourcing of this file
  # useful in the security regression script.
  if (exists(".metapackage_spanish_catalog", mode = "function")) {
    meta_es <- .metapackage_spanish_catalog(nombre)
    .write_po_catalog(
      names(meta_es), NULL, file.path("po", paste0("R-", nombre, ".pot")),
      project = paste(nombre, version)
    )
    .write_po_catalog(
      names(meta_es), meta_es, file.path("po", "R-es.po"),
      project = paste(nombre, version)
    )
    .write_mo_catalog(
      names(meta_es), meta_es,
      file.path("inst", "po", "es", "LC_MESSAGES", paste0("R-", nombre, ".mo"))
    )
  }

  # Crear .Rbuildignore con los patrones b\u00e1sicos y a\u00f1adir los paquetes locales
  rbuildignore_content <- c(
    # Patrones b\u00e1sicos
    "^.*\\.Rproj$",        # Cualquier archivo .Rproj
    "^\\.Rproj\\.user$",   # Carpeta oculta de RStudio
    paste0("^", nombre, "\\.Rproj$"),

    # Archivos y directorios de instalaci\u00f3n/check
    "^00LOCK-.*$",
    "^00_pkg_src$",
    "^libs$",
    "^doc$",
    "^Meta$",
    "^tmp$",
    "^temp$",
    "^check$",
    "\\.Rcheck$",

    # Directorios de CI/CD, repositorios, pkgdown, etc.
    "^\\.github$",
    "^_pkgdown\\.yml$",
    "^pkgdown$",
    "^\\.travis\\.yml$",
    "^codecov\\.yml$",
    "^\\.gitignore$",
    "^\\.git$",

    # Ignorar archivos de paquete comprimidos gen\u00e9ricos
    "^.*\\.tar\\.gz$",
    "^.*\\.zip$",
    "^.*\\.tar$",

    # Patrones para paquetes locales
    vapply(sub("_.*", "", paquetes_locales), function(pkg) {
      pkg_pattern <- .escape_regex_literal(pkg)
      c(sprintf("^%s$", pkg_pattern),         # El directorio exacto
        sprintf("^%s(/.*)?$", pkg_pattern),  # Directorio y todo su contenido
        sprintf("^%s[._-].*$", pkg_pattern)  # Cualquier archivo que empiece con el nombre del paquete
      )
    }, character(3))

  )

  # Eliminamos duplicados, por si acaso
  rbuildignore_content <- unique(unlist(rbuildignore_content))

  # Crear .gitignore
  .write_utf8(".Rproj.user", ".gitignore")
  log_debug(".Rbuildignore and .gitignore created")

  # Crear archivo .BBSoptions para aceptar directorios no est\u00e1ndar
  bbsoptions_content <- "UnsupportedPlatforms: \nAcceptNonstandardNonTestDirectories: TRUE"
  .write_utf8(bbsoptions_content, file.path(ruta_proyecto, ".BBSoptions"))
  log_debug(".BBSoptions created")

  # Asegurarse de incluir .BBSoptions en .Rbuildignore
  rbuildignore_content <- c(rbuildignore_content, "^\\.BBSoptions$")

  # Guardar .Rbuildignore en el proyecto
  .write_utf8(rbuildignore_content, file.path(ruta_proyecto, ".Rbuildignore"))

  # Rproj: Crear archivo de proyecto R
  rproj_content <-
    'Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX

AutoAppendNewline: Yes
StripTrailingWhitespace: Yes'

  .write_utf8(rproj_content, paste0(nombre, ".Rproj"))
  log_debug(glue::glue("{nombre}.Rproj created"))

  # PASO 6: Crear archivos del meta-paquete
  if (mostrar_progreso) {
    message(.lv_tr("Generating metapackage R files..."))
  }

  crear_archivos_meta_paquete(
    nombre = nombre,
    paquetes_locales = sub("_.*", "", paquetes_locales),
    ruta_instalables = ruta_instalables,
    paquetes_completos = paquetes_locales,
    ext = ext,
    ruta_destino = "R",
    deps_implicitas = deps_implicitas,
    reexportar_funciones = reexportar_funciones,
    verbose = verbose
  )
  log_debug("Additional metapackage files created")


  if (mostrar_progreso) {
    message(.lv_trf("Metapackage %s created successfully at %s", nombre, ruta_proyecto))
  }


  # NOTA DE SEGURIDAD (fix 2026-07): aqu\u00ed hab\u00eda tres rutas de borrado que se
  # eliminaron por causar p\u00e9rdida de datos (ver NEWS.md):
  #   - V8: borrado incondicional de subdirectorios "no est\u00e1ndar" del proyecto al
  #         regenerar sobre un destino existente (borraba trabajo preexistente).
  #   - Limpieza de regeneraci\u00f3n v\u00eda clean_pkg_dirs(dir_base = ".").
  #   - V7: emisi\u00f3n de scripts ejecutables cleanup/cleanup.win con "rm -rf <componente>".
  # Un generador nunca debe borrar contenido preexistente que no cre\u00f3 y registr\u00f3 en
  # esta misma invocaci\u00f3n. Si el destino ya existe, se conserva lo desconocido.


  # PASO 8: Generar documentaci\u00f3n autom\u00e1ticamente solo si fue solicitada.
  if (isTRUE(generar_documentacion) && requireNamespace("devtools", quietly = TRUE)) {
    if (mostrar_progreso) {
      message(.lv_trf("Generating documentation for %s...", nombre))
    }

    # Guardar directorio actual
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)

    # Cambiar al directorio del paquete
    setwd(ruta_proyecto)

    # Ejecutar document()
    tryCatch({
      if (mostrar_progreso) {
        devtools::document(quiet = TRUE)
      } else {
        suppressPackageStartupMessages(devtools::document(quiet = TRUE))
      }
      if (mostrar_progreso) {
        message(.lv_tr("Documentation generated successfully."))
      }
    }, error = function(e) {
      warning(.lv_trf("Error generating documentation: %s", e$message),
              call. = FALSE)
    })
  } else if (isTRUE(generar_documentacion) && mostrar_progreso) {
    message(.lv_tr("Install package 'devtools' to generate documentation automatically."))
  }


  invisible(structure(
    list(
      path = normalizePath(ruta_proyecto, mustWork = TRUE),
      name = nombre,
      packages = sub("_.*", "", paquetes_locales),
      archives = paquetes_locales,
      local_dependencies = deps_locales,
      cran_dependencies = deps_cran,
      implicit_dependencies = deps_implicitas,
      documented = isTRUE(generar_documentacion) &&
        requireNamespace("devtools", quietly = TRUE)
    ),
    class = "bigbang_result"
  ))
}


#' Deprecated Spanish alias for `create_metapackage()`
#'
#' @inheritParams create_metapackage
#' @param nombre,paquetes_locales,ruta_instalables,ruta_destino,reexportar_funciones,generar_documentacion,mostrar_progreso,autores,descripcion,licencia,deps_adicionales,deps_ignorar,deps_imports,deps_forzar,verbose Deprecated Spanish argument names.
#' @return A `bigbang_result`, invisibly.
#' @keywords internal
#' @export
crear_meta_paquete_local <- function(
    nombre,
    paquetes_locales,
    ruta_instalables,
    ext = ".tar.gz",
    version = "0.1.0",
    ruta_destino = NULL,
    reexportar_funciones = FALSE,
    generar_documentacion = TRUE,
    mostrar_progreso = TRUE,
    autores = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
    descripcion = "Local Package Metapackage",
    licencia = "MIT + file LICENSE",
    deps_adicionales = NULL,
    deps_ignorar = NULL,
    deps_imports = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts", "zoo"),
    deps_forzar = NULL,
    verbose = FALSE) {
  if (isTRUE(getOption("bigbang.deprecation_warnings", interactive()))) {
    .Deprecated("create_metapackage", package = "bigbang")
  }
  tryCatch(
    create_metapackage(
      name = nombre,
      packages = paquetes_locales,
      pkg_dir = ruta_instalables,
      ext = ext,
      version = version,
      dest_dir = ruta_destino,
      reexport = reexportar_funciones,
      document = generar_documentacion,
      verbose = mostrar_progreso,
      authors = autores,
      description = descripcion,
      license = licencia,
      additional_deps = deps_adicionales,
      ignore_deps = deps_ignorar,
      import_deps = deps_imports,
      force_deps = deps_forzar,
      debug = verbose
    ),
    bigbang_error_nonempty_dest = function(error) {
      error$message <- paste0(
        "Por seguridad, el destino debe ser nuevo o estar vac\u00edo: ",
        error$path, ". No regenere in-place una fuente existente."
      )
      stop(error)
    }
  )
}



# NOTA DE SEGURIDAD (fix 2026-07): se elimin\u00f3 la funci\u00f3n interna clean_pkg_dirs
# del propio generador. Estaba muerta (sin llamadas tras quitar V8 y la limpieza
# de regeneraci\u00f3n) y borraba directorios por nombre relativo al cwd.



#' Render generated metapackage R files
#'
#' @param nombre Character metapackage name.
#' @param paquetes_locales Character component names without versions.
#' @param ruta_instalables Character archive directory.
#' @param paquetes_completos Character archive stems including versions.
#' @param ext Character archive extension.
#' @param ruta_destino Character R output directory.
#' @param deps_implicitas Character implicit dependencies.
#' @param reexportar_funciones Logical re-export toggle.
#' @param autores Character Authors@R expression.
#' @param descripcion Character metapackage description.
#' @param licencia Character license declaration.
#' @param verbose Logical debug toggle.
#'
#' @return Invisible character vector of created paths.
#' @noRd

crear_archivos_meta_paquete <- function(nombre,
                                        paquetes_locales,
                                        ruta_instalables,
                                        paquetes_completos,
                                        ext = ".tar.gz",
                                        ruta_destino = "R",
                                        deps_implicitas = NULL,
                                        reexportar_funciones = FALSE,
                                        autores = "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
                                        descripcion = "Local Package Metapackage",
                                        licencia = "MIT + file LICENSE",
                                        verbose = FALSE) {

  # Funci\u00f3n auxiliar para mensajes de depuraci\u00f3n
  log_debug <- function(msg) {
    if (verbose) message(paste0("DEBUG: ", msg))
  }

  # Asegurarnos de que tenemos todos los datos necesarios
  log_debug("Preparing template data")

  # Preparar datos para las plantillas (solo una vez)
  datos_plantilla <- list(
    name = nombre,
    package_list = .r_literal(paquetes_locales),
    local_packages = .r_literal(paquetes_completos),
    install_path = paste(deparse(ruta_instalables), collapse = ""),
    extension = paste(deparse(ext), collapse = ""),
    deps_implicitas = if (!is.null(deps_implicitas)) paste(deps_implicitas, collapse = ", ") else ""
  )

  if (verbose) {
    log_debug("Template values:")
    log_debug(paste("name:", datos_plantilla$name))
    log_debug(paste("package_list:", datos_plantilla$package_list))
    log_debug(paste("local_packages:", datos_plantilla$local_packages))
    log_debug(paste("install_path:", datos_plantilla$install_path))
    log_debug(paste("extension:", datos_plantilla$extension))
  }


  # Plantillas para los archivos necesarios
  plantillas <- list(
    attach = '
utils::globalVariables(".pkgs")
.pkgs <- {{{ package_list }}}

attach_installed_packages <- function(pkgs, warn_missing = TRUE) {
  ya_adjuntos <- gsub("^package:", "", search())
  to_load <- setdiff(pkgs, ya_adjuntos)
  faltan <- to_load[!vapply(to_load, requireNamespace, logical(1), quietly = TRUE)]
  if (warn_missing && length(faltan) > 0) {
    warning(gettextf(
      "Not installed: %s. Run {{ name }}_install() to install them.",
      paste(faltan, collapse = ", "), domain = "R-{{ name }}"
    ), call. = FALSE)
  }
  to_load <- setdiff(to_load, faltan)
  if (length(to_load) > 0) {
    suppressPackageStartupMessages(
      lapply(to_load, library, character.only = TRUE)
    )
  }
  invisible(list(adjuntados = to_load, faltantes = faltan))
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
#\' @param ruta_instalables Character archive directory.
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
{{ name }}_install <- function(ruta_instalables = {{{ install_path }}},
                               ext = {{{ extension }}},
                               cran_deps = c("skip", "error", "install"),
                               repos = getOption("repos"),
                               verbose = getOption("bigbang.verbose", interactive())) {
  cran_deps <- match.arg(cran_deps)
  paquetes_locales <- {{{ local_packages }}}
  resultado <- install_packages_in_order(
    paquetes_locales, ruta_instalables, ext, mostrar_progreso = verbose,
    repos = repos, cran_deps = cran_deps
  )
  if (length(resultado$fallidos) > 0) {
    detalles <- paste0(
      names(resultado$fallidos), ": ", unlist(resultado$fallidos, use.names = FALSE)
    )
    condition <- structure(
      list(
        message = gettextf(
          "Could not install all components: %s",
          paste(detalles, collapse = "; "), domain = "R-{{ name }}"
        ),
        call = NULL,
        failures = resultado$fallidos
      ),
      class = c("bigbang_error_install", "bigbang_error", "error", "condition")
    )
    stop(condition)
  }
  if (length(resultado$omitidos) > 0L) {
    warning(gettextf(
      "Some components were skipped because non-local dependencies are missing: %s",
      paste(names(resultado$omitidos), collapse = ", "), domain = "R-{{ name }}"
    ), call. = FALSE)
  }
  {{ name }}_attach(sub("_.*", "", paquetes_locales))
  invisible(resultado)
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
  pak <- paste0("package:", .pkgs)
  lapply(pak[pak %in% search()], detach, character.only = TRUE)
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
text_col <- function(x) {
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

msg <- function(...) {
  packageStartupMessage(text_col(...))
}

#\' Generate an ASCII package banner
#\'
#\' @param nombre Character metapackage name.
#\' @param paquetes Character component names.
#\' @return The generated banner.
#\' @keywords internal
generate_ascii_banner <- function(nombre, paquetes = NULL) {
  width <- 60
  border <- paste0(rep("=", width), collapse = "")

  # Centered title
  title <- paste0(" ", nombre, " ")
  padding_length <- floor((width - nchar(title)) / 2)
  left_padding <- paste0(rep("-", padding_length), collapse = "")
  right_padding <- paste0(rep("-", width - padding_length - nchar(title)), collapse = "")
  title_line <- paste0(left_padding, title, right_padding)

  # Construir cartel
  banner <- c(
    border,
    title_line,
    border
  )

  # Add component information
  if (!is.null(paquetes) && length(paquetes) > 0) {
    banner <- c(banner, "")
    banner <- c(banner, .meta_tr("Included packages:"))

    for (pkg in paquetes) {
      # Read the version when available
      ver_txt <- ""
      if (requireNamespace(pkg, quietly = TRUE)) {
        tryCatch({
          ver <- utils::packageVersion(pkg)
          ver_txt <- paste0(" (v", ver, ")")
        }, error = function(e) {})
      }

      banner <- c(banner, paste0("  * ", pkg, ver_txt))
    }

    banner <- c(banner, "", border)
  }

  return(paste(banner, collapse = "\\n"))
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
#\' safe_unlink("archivo_temporal.txt")
#\' # Attempt safe directory removal
#\' safe_unlink("directorio_temp", recursive = TRUE, force = TRUE)
#\' }
#\'
#\' @keywords internal

safe_unlink <- function(path, recursive = FALSE, force = FALSE, verify = TRUE) {

  # Configuraciones de seguridad
  MIN_PATH_LENGTH <- 3  # Rutas muy cortas son sospechosas

  # Lista de directorios del sistema o importantes que nunca deben eliminarse
  PROTECTED_DIRS <- c(
    # Directorios del sistema en Windows
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

  # Solo realizar verificaciones detalladas si se solicita
  if (verify) {
    if (is.character(path) && length(path) > 0) {
      for (p in path) {
        # 1. Verificar longitud de la ruta (evita borrar "/", "C:\\", etc.)
        if (nchar(p) < MIN_PATH_LENGTH) {
          message(.meta_trf("SAFETY: Path is too short and may be dangerous: %s", p))
          return(invisible(FALSE))
        }

        # 2. Verificar patrones peligrosos
        if (any(sapply(DANGEROUS_PATTERNS, function(pattern) grepl(pattern, p)))) {
          message(.meta_trf("SAFETY: Potentially dangerous path pattern: %s", p))
          return(invisible(FALSE))
        }

        # 3. Verificar si es un directorio existente
        if (dir.exists(p)) {
          # 3.1 Verificar si es un directorio protegido
          if (basename(p) %in% PROTECTED_DIRS) {
            message(.meta_trf("SAFETY: Potentially important directory: %s", p))
            return(invisible(FALSE))
          }

          # 3.2 Si recursive=TRUE y force=TRUE, realizar verificaciones adicionales
          if (recursive && force) {
            # Verificar si parece un paquete R
            has_desc <- file.exists(file.path(p, "DESCRIPTION"))
            has_r_dir <- dir.exists(file.path(p, "R"))
            has_man_dir <- dir.exists(file.path(p, "man"))

            if (has_desc && (has_r_dir || has_man_dir)) {
              # Verificar si es un directorio temporal de paquete R
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

  return(result)
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
#\' is_path_inside("R/archivo.R", getwd())
#\' # Check an owned temporary directory
#\' is_path_inside(file.path(tempdir(), "subdir"), tempdir())
#\' }
#\'
#\' @keywords internal

is_path_inside <- function(inner_path, outer_path) {
  # Normalizar rutas para manejar diferencias entre sistemas operativos
  inner <- normalizePath(inner_path, mustWork = FALSE)
  outer <- normalizePath(outer_path, mustWork = FALSE)

  # En Windows, convertir barras invertidas a barras normales
  if (.Platform$OS.type == "windows") {
    inner <- gsub("\\\\\\\\", "/", inner)
    outer <- gsub("\\\\\\\\", "/", outer)
  }

  # Asegurar que outer termina con barra para evitar coincidencias parciales
  if (!endsWith(outer, "/")) {
    outer <- paste0(outer, "/")
  }

  # Verificar si inner_path comienza con outer_path
  return(startsWith(inner, outer))
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
  msg(paste0("\\n", banner, "\\n"))

  # Tidyverse-style startup hook: delegate search-path changes to a helper.
  resultado <- attach_installed_packages(pkg_base_names, warn_missing = FALSE)
  faltantes <- resultado$faltantes
  instalados <- setdiff(pkg_base_names, faltantes)

  if (length(instalados) > 0) {
    msg(.meta_trf("Attached packages: %s", paste(instalados, collapse = ", ")))
  }
  if (length(faltantes) > 0) {
    packageStartupMessage(.meta_trf(
      "Components still need installation: %s\\nRun {{ name }}_install() to install them from local archives.",
      paste(faltantes, collapse = ", ")
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

  # Asegurarse de que la ruta de destino existe
  if (!dir.exists(ruta_destino)) {
    dir.create(ruta_destino, recursive = TRUE)
  }

  # Renderizar y escribir cada plantilla
  archivos_creados <- character(0)
  for (nombre_archivo in names(plantillas)) {
    ruta_archivo <- file.path(ruta_destino, paste0(nombre_archivo, ".R"))

    if (!file.exists(ruta_archivo)) {
      tryCatch({
        contenido <- whisker::whisker.render(
          template = plantillas[[nombre_archivo]],
          data = datos_plantilla,
          partials = list()  # Asegurarse de que no hay parciales no definidos
        )
        contenido <- .drop_regular_comment_lines(contenido)

        # Reject empty rendered output
        if (nchar(contenido) == 0) {
          stop(.lv_tr("The template rendered empty content."), call. = FALSE)
        }

        .write_utf8(contenido, ruta_archivo)
        if (verbose) {
          message(.lv_trf("Created %s.R successfully.", nombre_archivo))
        }
        archivos_creados <- c(archivos_creados, ruta_archivo)
      }, error = function(e) {
        warning(.lv_trf("Error creating %s.R: %s", nombre_archivo, e$message),
                call. = FALSE)
        # Emit diagnostic context in verbose mode
        if (verbose) {
          message(.lv_tr("\nOriginal template:"))
          message(plantillas[[nombre_archivo]])
          message(.lv_tr("\nTemplate data:"))
          utils::str(datos_plantilla)
        }
      })
    } else {
      if (verbose) {
        message(.lv_trf("%s.R already exists and will not be overwritten.", nombre_archivo))
      }
    }
  }

  # Generar archivo de re-exportaciones si se solicita
  if (reexportar_funciones) {
    generar_archivo_reexports(
      nombre = nombre,
      paquetes_locales = paquetes_locales,
      ruta_destino = ruta_destino,
      verbose = verbose
    )
  }

  invisible(archivos_creados)
}
