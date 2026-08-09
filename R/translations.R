# Translation keys and values must remain exact, so wrapping them would reduce
# catalog auditability. # nolint start: line_length_linter
.bigbang_spanish_catalog <- function() {
  c(
    "'name' must be one character string" = "'name' debe ser una cadena de caracteres",
    "'packages' must be a non-empty character vector" = "'packages' debe ser un vector de caracteres no vac\u00edo",
    "'pkg_dir' must be one character string" = "'pkg_dir' debe ser una cadena de caracteres",
    "The directory specified by 'pkg_dir' does not exist" = "El directorio indicado por 'pkg_dir' no existe",
    "'dest_dir' must be supplied as one non-empty path: the meta-package is written inside it. Use tempdir() for disposable output." = "'dest_dir' debe proporcionarse como una ruta no vac\u00eda: el metapaquete se escribe dentro de ella. Use tempdir() para una salida descartable.",
    "'workflow' must map unique non-empty stage names to every component package exactly once" = "'workflow' debe asociar nombres de etapa \u00fanicos y no vac\u00edos con cada paquete componente exactamente una vez",
    "Unsupported archive extension: %s" = "Extensi\u00f3n de archivo no compatible: %s",
    "No DESCRIPTION file found in package %s" = "No se encontr\u00f3 DESCRIPTION en el paquete %s",
    "DEBUG: DESCRIPTION file created" = "DEBUG: archivo DESCRIPTION creado",
    "DEBUG: NAMESPACE file created" = "DEBUG: archivo NAMESPACE creado",
    "Package archive not found: %s" = "No se encontr\u00f3 el archivo del paquete: %s",
    "No R directory found for package: %s" = "No se encontr\u00f3 el directorio R del paquete: %s",
    "Error processing package %s: %s" = "Error al procesar el paquete %s: %s",
    "No component packages are installed; re-exports were not generated." = "No hay paquetes componentes instalados; no se generaron reexportaciones.",
    "No functions were found to re-export." = "No se encontraron funciones para reexportar.",
    "Re-export file created with %d functions from %d packages." = "Archivo de reexportaciones creado con %d funciones de %d paquetes.",
    "SAFETY: Path is too short and may be dangerous: %s" = "SEGURIDAD: La ruta es demasiado corta y puede ser peligrosa: %s",
    "SAFETY: Potentially dangerous path pattern: %s" = "SEGURIDAD: Patr\u00f3n de ruta potencialmente peligroso: %s",
    "SAFETY: Potentially important directory: %s" = "SEGURIDAD: Directorio potencialmente importante: %s",
    "SAFETY: Possible non-temporary R package directory: %s" = "SEGURIDAD: Posible directorio no temporal de un paquete R: %s",
    "Could not remove completely: %s" = "No se pudo eliminar por completo: %s",
    "The DESCRIPTION file does not exist in the project directory." = "El archivo DESCRIPTION no existe en el directorio del proyecto.",
    "DESCRIPTION updated with a valid Suggests field" = "DESCRIPTION actualizado con un campo Suggests v\u00e1lido",
    "Error updating DESCRIPTION: %s" = "Error al actualizar DESCRIPTION: %s",
    "Basic vignette created at %s" = "Vi\u00f1eta b\u00e1sica creada en %s",
    "Error creating basic vignette: %s" = "Error al crear la vi\u00f1eta b\u00e1sica: %s",
    "Package name '%s' contains underscores, which R package names do not allow. Use '%s' instead." = "El nombre de paquete '%s' contiene guiones bajos, que R no admite. Use '%s' en su lugar.",
    "Package name '%s' is not a valid R package name: use at least two characters, start with a letter, continue with letters, digits or dots, and do not end with a dot." = "El nombre de paquete '%s' no es un nombre de paquete de R v\u00e1lido: use al menos dos caracteres, comience con una letra, contin\u00fae con letras, d\u00edgitos o puntos, y no termine con un punto.",
    "For safety, the destination must be new or empty: %s. Generate into a new empty path; never regenerate an existing source in place." = "Por seguridad, el destino debe ser nuevo o estar vac\u00edo: %s. Genere en una ruta nueva y vac\u00eda; nunca regenere una fuente existente in-place.",
    "Creating package structure at: %s" = "Creando la estructura del paquete en: %s",
    "Could not create project directory: %s" = "No se pudo crear el directorio del proyecto: %s",
    "Could not create directory: %s" = "No se pudo crear el directorio: %s",
    "The following package archives were not found: %s" = "No se encontraron los siguientes archivos de paquete: %s",
    "Creating metapackage '%s' for %d local packages..." = "Creando el metapaquete '%s' para %d paquetes locales...",
    "Packages: %s... and %d more" = "Paquetes: %s... y %d m\u00e1s",
    "Packages: %s" = "Paquetes: %s",
    "Using explicitly supplied dependencies: %s" = "Usando las dependencias indicadas expl\u00edcitamente: %s",
    "Scanning local packages for implicit dependencies..." = "Analizando dependencias impl\u00edcitas en los paquetes locales...",
    "Detected implicit dependencies: %s" = "Dependencias impl\u00edcitas detectadas: %s",
    "Generating DESCRIPTION and NAMESPACE..." = "Generando DESCRIPTION y NAMESPACE...",
    "Generating metapackage R files..." = "Generando los archivos R del metapaquete...",
    "Metapackage %s created successfully at %s" = "Metapaquete %s creado correctamente en %s",
    "Generating documentation for %s..." = "Generando documentaci\u00f3n para %s...",
    "Documentation generated successfully." = "Documentaci\u00f3n generada correctamente.",
    "Error generating documentation: %s" = "Error al generar documentaci\u00f3n: %s",
    "Install package 'devtools' to generate documentation automatically." = "Instale el paquete 'devtools' para generar documentaci\u00f3n autom\u00e1ticamente.",
    "Created %s.R successfully." = "Archivo %s.R creado correctamente.",
    "Error creating %s.R: %s" = "Error al crear %s.R: %s",
    "The template rendered empty content." = "La plantilla gener\u00f3 contenido vac\u00edo.",
    "Original template:" = "Plantilla original:",
    "Template data:" = "Datos de la plantilla:",
    "%s.R already exists and will not be overwritten." = "%s.R ya existe y no se sobrescribir\u00e1.",
    "The ZIP archive does not contain a DESCRIPTION file: %s" = "El archivo ZIP no contiene DESCRIPTION: %s",
    "Unsupported archive format: %s" = "Formato de archivo no compatible: %s",
    "Installed local package: %s" = "Paquete local instalado: %s",
    "Packages that failed: %s" = "Paquetes que fallaron: %s",
    "Packages skipped by the offline policy: %s" = "Paquetes omitidos por la pol\u00edtica offline: %s",
    "'force' must be TRUE or FALSE" = "'force' debe ser TRUE o FALSE",
    "'include_archives' must be TRUE or FALSE" = "'include_archives' debe ser TRUE o FALSE",
    "Component archives copied into the meta-package: %s (%.1f MB)." = "Archivos de los componentes copiados dentro del metapaquete: %s (%.1f MB).",
    "Package name '%s' belongs to R itself and cannot be reused." = "El nombre de paquete '%s' pertenece a R y no puede reutilizarse.",
    "Could not copy the component archives into the meta-package: %s" = "No se pudieron copiar los archivos de los componentes dentro del metapaquete: %s",
    "'force = TRUE' conflicts with an explicit upgrade policy other than 'always'" = "'force = TRUE' entra en conflicto con una pol\u00edtica 'upgrade' expl\u00edcita distinta de 'always'",
    "Kept installed version because upgrade = 'never'" = "Se conserv\u00f3 la versi\u00f3n instalada porque upgrade = 'never'",
    "Use force = TRUE or upgrade = 'always' to reinstall unchanged packages: %s" = "Use force = TRUE o upgrade = 'always' para reinstalar paquetes sin cambios: %s",
    "'path' must be one non-empty character string." = "'path' debe ser una cadena de caracteres no vac\u00eda.",
    "Only read-only scanning is supported; 'dry_run' must remain TRUE." = "Solo se admite el escaneo de lectura; 'dry_run' debe permanecer en TRUE.",
    "Artifact does not exist: %s" = "El artefacto no existe: %s",
    "Unsupported artifact type: %s" = "Tipo de artefacto no compatible: %s",
    "No DESCRIPTION found in source directory: %s" = "No se encontr\u00f3 DESCRIPTION en el directorio fuente: %s",
    "Refusing to scan symbolic links in the source R directory." = "Se rechaza escanear enlaces simb\u00f3licos en el directorio R de la fuente.",
    "Archive contains unsafe absolute or parent-traversal paths: %s" = "El archivo contiene rutas absolutas o recorridos al directorio padre inseguros: %s",
    "Refusing to scan an archive containing symbolic links." = "Se rechaza escanear un archivo que contiene enlaces simb\u00f3licos.",
    "No DESCRIPTION found in source archive." = "No se encontr\u00f3 DESCRIPTION en el archivo fuente.",
    "Source archive has multiple candidate package roots." = "El archivo fuente tiene varias ra\u00edces de paquete candidatas.",
    "Installed package has no valid Package field." = "El paquete instalado no tiene un campo Package v\u00e1lido.",
    "Could not identify the installed lazy-load database." = "No se pudo identificar la base lazy-load instalada."
  )
}

.metapackage_spanish_catalog <- function(name, include_archives = FALSE) {
  # The hint has to name the call the reader can actually make: without shipped
  # archives the installer needs a directory, and suggesting a bare call would
  # send them into an error.
  call_es <- if (isTRUE(include_archives)) {
    paste0(name, "_install()")
  } else {
    paste0(name, "_install(pkg_dir = RUTA)")
  }
  call_en <- if (isTRUE(include_archives)) {
    paste0(name, "_install()")
  } else {
    paste0(name, "_install(pkg_dir = PATH)")
  }
  repo_es <- if (isTRUE(include_archives)) {
    paste0(name, "_install(cran_deps = 'install')")
  } else {
    paste0(name, "_install(pkg_dir = RUTA, cran_deps = 'install')")
  }
  repo_en <- if (isTRUE(include_archives)) {
    paste0(name, "_install(cran_deps = 'install')")
  } else {
    paste0(name, "_install(pkg_dir = PATH, cran_deps = 'install')")
  }
  stats::setNames(
    c(
      "Los archivos de los componentes que acompa\u00f1an a este paquete no est\u00e1n disponibles. Reinst\u00e1lelo o pase pkg_dir apuntando a un directorio con los archivos de los componentes.",
      "El directorio de archivos no existe: %s",
      "El archivo del paquete no existe: %s",
      "Formato de archivo no compatible: %s",
      "Se esperaba un DESCRIPTION en %s; se encontraron %d.",
      "El ZIP no contiene DESCRIPTION: %s",
      "El paquete %s (versi\u00f3n %s) ya est\u00e1 instalado.",
      "Ya instalado",
      "Omitido porque faltan dependencias no locales: %s",
      "Falta",
      "Faltan dependencias no locales: %s",
      "No se pueden instalar dependencias no locales sin un repositorio configurado: %s",
      "Repositorio no configurado",
      "Instalando dependencia no local: %s",
      "Las dependencias no est\u00e1n instaladas: %s",
      "No instalado",
      "Los ZIP binarios de Windows solo se pueden instalar en Windows.",
      "Se esperaba un DESCRIPTION en el ZIP fuente; se encontraron %d",
      "No se pudo verificar la instalaci\u00f3n",
      "Paquete %s instalado correctamente.",
      "Instalado correctamente",
      "Iniciando la instalaci\u00f3n de %d paquetes",
      "Omitido %s: %s",
      "Fall\u00f3 la instalaci\u00f3n de %s: %s",
      "Instalaci\u00f3n completa.",
      "Paquetes incluidos:",
      "Adjuntando paquetes",
      "Dependencias circulares detectadas: %s. Una instalaci\u00f3n limpia no tiene un orden topol\u00f3gico v\u00e1lido.",
      sprintf("No instalado: %%s. Ejecute %s para instalarlo.", call_es),
      "No se pudieron instalar todos los componentes: %s",
      sprintf("Se omitieron componentes porque faltan dependencias no locales: %%s. Instale esas dependencias o llame a %s para obtenerlas desde un repositorio.", repo_es),
      "SEGURIDAD: La ruta es demasiado corta y puede ser peligrosa: %s",
      "SEGURIDAD: Patr\u00f3n de ruta potencialmente peligroso: %s",
      "SEGURIDAD: Directorio potencialmente importante: %s",
      "SEGURIDAD: Posible directorio no temporal de paquete R: %s",
      "No se pudo eliminar por completo: %s",
      "Paquetes adjuntados: %s",
      "No se encontraron conflictos.",
      "Conflictos:",
      "'force' debe ser TRUE o FALSE",
      "'force = TRUE' entra en conflicto con una pol\u00edtica 'upgrade' expl\u00edcita distinta de 'always'",
      "Se conserv\u00f3 la versi\u00f3n instalada porque upgrade = 'never'",
      "Use force = TRUE o upgrade = 'always' para reinstalar paquetes sin cambios: %s",
      sprintf(
        "Faltan componentes por instalar: %%s\nEjecute %s para instalarlos desde los archivos locales.",
        call_es
      ),
      "Nota: error durante la descarga segura: %s"
    ),
    c(
      "The component archives that ship with this package are not available. Reinstall it, or pass pkg_dir pointing at a directory holding the component archives.",
      "The archive directory does not exist: %s",
      "Package archive does not exist: %s",
      "Unsupported archive format: %s",
      "Expected one DESCRIPTION in %s; found %d.",
      "The ZIP archive does not contain a DESCRIPTION file: %s",
      "Package %s (version %s) is already installed.",
      "Already installed",
      "Skipped because non-local dependencies are missing: %s",
      "Missing",
      "Missing non-local dependencies: %s",
      "Cannot install non-local dependencies without a configured repository: %s",
      "Repository not configured",
      "Installing non-local dependency: %s",
      "Dependencies are not installed: %s",
      "Not installed",
      "Windows binary ZIP packages can only be installed on Windows.",
      "Expected one DESCRIPTION in source ZIP; found %d",
      "Installation could not be verified",
      "Installed package %s successfully.",
      "Installed successfully",
      "Starting installation of %d packages",
      "Skipped %s: %s",
      "Installation failed for %s: %s",
      "Installation complete.",
      "Included packages:",
      "Attaching packages",
      "Circular dependencies detected: %s. A clean installation has no valid topological order.",
      sprintf("Not installed: %%s. Run %s to install them.", call_en),
      "Could not install all components: %s",
      sprintf("Some components were skipped because non-local dependencies are missing: %%s. Install those dependencies, or call %s to obtain them from a repository.", repo_en),
      "SAFETY: Path is too short and may be dangerous: %s",
      "SAFETY: Potentially dangerous path pattern: %s",
      "SAFETY: Potentially important directory: %s",
      "SAFETY: Possible non-temporary R package directory: %s",
      "Could not remove completely: %s",
      "Attached packages: %s",
      "No conflicts found.",
      "Conflicts:",
      "'force' must be TRUE or FALSE",
      "'force = TRUE' conflicts with an explicit upgrade policy other than 'always'",
      "Kept installed version because upgrade = 'never'",
      "Use force = TRUE or upgrade = 'always' to reinstall unchanged packages: %s",
      sprintf(
        "Components still need installation: %%s\nRun %s to install them from local archives.",
        call_en
      ),
      "Note: Error during safe unload: %s"
    )
  )
}
# nolint end
