# Verificación empírica del fix: generar un metapaquete real y probar que ni la
# generación, ni library(), ni la instalación explícita borran directorios del cwd.
#
# Correr desde la raíz del proyecto bigbang:
#   Rscript tests/regresion/verificar_fix_borrado.R
# Requiere: whisker, withr, brio instalados.
options(warn = 1)
gen_src <- if (dir.exists("R")) "R" else "../../R"
sandbox <- tempfile("verifica-fix-")
dir.create(sandbox)
cat("SANDBOX:", sandbox, "\n")

# Cargar las funciones del generador (source directo, sin instalar bigbang)
for (f in c("crear_meta_paquete_local.R", "install_loc_pkg_w_dep.R"))
  sys.source(file.path(gen_src, f), envir = globalenv())

# --- 1. Crear paquetes componentes dummy y empaquetarlos ---
fuentes <- file.path(sandbox, "fuentes"); dir.create(fuentes)
instalables <- file.path(sandbox, "instalables"); dir.create(instalables)

crear_dummy <- function(nombre, dir_fuentes, con_R = TRUE, imports = NULL) {
  ruta <- file.path(dir_fuentes, nombre)
  dir.create(ruta)
  desc <- c(paste0("Package: ", nombre), "Type: Package",
            paste0("Title: Dummy ", nombre), "Version: 0.1.0",
            "Authors@R: person('T','D', email='t@e.com', role=c('aut','cre'))",
            "Description: Paquete de prueba.", "License: GPL (>= 3)", "Encoding: UTF-8",
            if (!is.null(imports)) paste0("Imports: ", imports))
  writeLines(desc, file.path(ruta, "DESCRIPTION"))
  writeLines(if (is.null(imports)) character() else paste0("import(", imports, ")"),
             file.path(ruta, "NAMESPACE"))
  if (con_R) {
    dir.create(file.path(ruta, "R"))
    writeLines(c("#' f", "#' @export", paste0("f_", nombre, " <- function() '", nombre, "'")),
               file.path(ruta, "R", "f.R"))
    writeLines(c(readLines(file.path(ruta, "NAMESPACE"), warn = FALSE),
                 paste0("export(f_", nombre, ")")), file.path(ruta, "NAMESPACE"))
  } else {
    dir.create(file.path(ruta, "data"))
    writeLines("dato del componente", file.path(ruta, "data", "dato.txt"))
  }
  ruta
}
crear_dummy("bbb", fuentes, con_R = FALSE)
crear_dummy("aaa", fuentes, imports = "bbb")
crear_dummy("tmpcomponente", fuentes)
for (pkg in c("aaa", "bbb", "tmpcomponente")) {
  withr::with_dir(fuentes, utils::tar(file.path(instalables, paste0(pkg, "_0.1.0.tar.gz")),
                  files = pkg, compression = "gzip"))
}

# --- 2. Preparar el cwd del usuario con directorios señuelo ---
cwd_usuario <- file.path(sandbox, "cwd_usuario"); dir.create(cwd_usuario)
senuelos <- c("aaa", "bbb", "tmpcomponente", "aaa_extra", "tmp_analisis",
             "doc", "file2024", "Meta", "libs")
# aaa: proyecto fuente homónimo (DESCRIPTION + R/)
crear_dummy("aaa", cwd_usuario)
# bbb: paquete solo-datos (el hueco de safe_unlink)
dir.create(file.path(cwd_usuario, "bbb", "data"), recursive = TRUE)
writeLines(c("Package: bbb", "Version: 0.1.0"), file.path(cwd_usuario, "bbb", "DESCRIPTION"))
writeLines("dato valioso", file.path(cwd_usuario, "bbb", "data", "importante.txt"))
# tmpcomponente: homónimo de un componente real cuyo nombre empieza con tmp
crear_dummy("tmpcomponente", cwd_usuario)
# resto: carpetas de trabajo comunes
for (d in c("aaa_extra", "tmp_analisis", "doc", "file2024", "Meta", "libs")) {
  dir.create(file.path(cwd_usuario, d))
  writeLines("contenido valioso", file.path(cwd_usuario, d, "importante.txt"))
}
for (d in senuelos) {
  writeLines(paste("hash valioso", d), file.path(cwd_usuario, d, "sentinel-hash.txt"))
}
archivos_sentinel <- file.path(cwd_usuario, senuelos, "sentinel-hash.txt")
snapshot <- function() vapply(senuelos, function(d) dir.exists(file.path(cwd_usuario, d)), logical(1))
snapshot_hash <- function() unname(tools::md5sum(archivos_sentinel))
antes <- snapshot()
hash_antes <- snapshot_hash()

# --- 3. Generar el metapaquete PARADOS en el cwd del usuario ---
lib <- file.path(sandbox, "lib"); dir.create(lib)
.libPaths(c(lib, .libPaths()))
withr::with_dir(cwd_usuario, {
  crear_meta_paquete_local(
    nombre = "miverso",
    paquetes_locales = c("aaa_0.1.0", "bbb_0.1.0", "tmpcomponente_0.1.0"),
    ruta_instalables = instalables,
    ruta_destino = file.path(sandbox, "proyecto"),
    generar_documentacion = FALSE,
    mostrar_progreso = FALSE
  )
})
tras_generar <- snapshot()

# --- 4. Inspeccionar el metapaquete emitido: ¿emite vectores de borrado? ---
proj <- file.path(sandbox, "proyecto", "miverso")
emitidos <- list.files(file.path(proj, "R"), full.names = TRUE)
cat("\n--- Archivos R emitidos:", paste(basename(emitidos), collapse=", "), "---\n")
cat("cleanup existe:", file.exists(file.path(proj, "cleanup")),
    "| cleanup.win existe:", file.exists(file.path(proj, "cleanup.win")), "\n")

codigo <- unlist(lapply(emitidos, readLines, warn = FALSE))
zzz_codigo <- readLines(file.path(proj, "R", "zzz.R"), warn = FALSE)
startup_codigo <- sub("#.*$", "", zzz_codigo)
patrones_peligrosos <- c("clean_pkg_dirs", "rm -rf", "rmdir", "list.files\\(tempdir",
                         "install.packages", "safe_unlink\\(pkg", "unlink\\(")
cat("\n--- Búsqueda de patrones destructivos en hooks de startup ---\n")
startup_limpio <- TRUE
for (p in patrones_peligrosos) {
  hits <- grep(p, startup_codigo, value = TRUE)
  cat(sprintf("  %-28s: %d ocurrencia(s)\n", p, length(hits)))
  startup_limpio <- startup_limpio && length(hits) == 0L
}

# --- 5. Cargar el metapaquete con library() parados en el cwd ---
# (instalar el metapaquete desde el directorio fuente, el caso que reproducía el bug)
withr::with_dir(cwd_usuario, {
  utils::install.packages(proj, repos = NULL, type = "source", lib = lib, quiet = TRUE)
})
tras_instalar_meta <- snapshot()

sentinel <- file.path(tempdir(), "sentinel-v3.txt"); writeLines("no me borres", sentinel)
withr::with_dir(cwd_usuario, {
  suppressWarnings(suppressMessages(library("miverso", character.only = TRUE, lib.loc = lib)))
})
tras_library <- snapshot()
sentinel_vive <- file.exists(sentinel)

# --- 6. Instalar explícitamente los componentes y verificar la adjunción ---
contador <- 0L
trace("install_loc_pkg_w_dep",
      tracer = quote(.GlobalEnv$contador <- .GlobalEnv$contador + 1L),
      where = asNamespace("miverso"), print = FALSE)
resultado_install <- withr::with_dir(cwd_usuario, {
  getExportedValue("miverso", "miverso_install")(instalables, ".tar.gz")
})
untrace("install_loc_pkg_w_dep", where = asNamespace("miverso"))
tras_install_explicito <- snapshot()
hash_despues <- snapshot_hash()
componentes <- c("aaa", "bbb", "tmpcomponente")
componentes_instalados <- vapply(componentes, requireNamespace, logical(1), quietly = TRUE)
componentes_adjuntos <- paste0("package:", componentes) %in% search()

# --- 7. Reporte ---
tabla <- data.frame(
  senuelo = senuelos,
  antes = antes,
  tras_generar = tras_generar,
  tras_instalar_meta = tras_instalar_meta,
  tras_library = tras_library,
  tras_install_explicito = tras_install_explicito,
  row.names = NULL
)
cat("\n===== SUPERVIVENCIA DE SEÑUELOS (TRUE = intacto) =====\n")
print(tabla)
cat("\nCentinela en tempdir() sobrevive a library():", sentinel_vive, "\n")
cat("Llamadas al instalador:", contador, "\n")
cat("Orden:", paste(resultado_install$orden, collapse = " -> "), "\n")
cat("Componentes instalados:", paste(componentes_instalados, collapse = ", "), "\n")
cat("Componentes adjuntos:", paste(componentes_adjuntos, collapse = ", "), "\n")
cat("Contenido/hash de señuelos intacto:", identical(hash_antes, hash_despues), "\n")

todo_ok <- all(tras_install_explicito) && sentinel_vive &&
  identical(hash_antes, hash_despues) &&
  startup_limpio &&
  !file.exists(file.path(proj, "cleanup")) &&
  !file.exists(file.path(proj, "cleanup.win")) &&
  contador == length(componentes) &&
  all(componentes_instalados) && all(componentes_adjuntos) &&
  identical(resultado_install$orden,
            c("bbb_0.1.0", "aaa_0.1.0", "tmpcomponente_0.1.0"))
cat("\n>>> RESULTADO:",
    if (todo_ok) "OK - SEGURO Y FUNCIONAL" else "FALLO - REGRESIÓN DETECTADA",
    "<<<\n")
if (!todo_ok) stop("La verificación de integración falló.", call. = FALSE)
