# Test de regresión del bug crítico de pérdida de datos (ver NEWS.md).
#
# Cubre dos niveles:
#  A) ESTÁTICO (rápido, determinista): el metapaquete emitido no trae scripts
#     cleanup, y sus hooks de startup (.onLoad/.onAttach) no contienen ninguna
#     operación destructiva ni de instalación.
#  B) EMPÍRICO: generar parados en un cwd lleno de directorios señuelo no borra
#     ninguno (cubre V7, V8 y la limpieza de generación).
#
# La reproducción completa de V1 vía library()/instalación desde fuente vive en
# tests/regresion/verificar_fix_borrado.R (integración, requiere construir el
# metapaquete). Aquí se cubre lo determinista.

# Localizar el generador: preferir la función del paquete (instalado); si no,
# sourcearla desde R/ para poder correr el test standalone.
if (!exists("crear_meta_paquete_local", mode = "function")) {
  candidatos <- c(
    file.path(testthat::test_path(), "..", "..", "R", "crear_meta_paquete_local.R"),
    "R/crear_meta_paquete_local.R"
  )
  for (p in candidatos) {
    if (file.exists(p)) { sys.source(normalizePath(p), envir = globalenv()); break }
  }
}

crear_componente_dummy <- function(nombre, dir_fuentes, con_R = TRUE, imports = NULL) {
  ruta <- file.path(dir_fuentes, nombre)
  dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    paste0("Package: ", nombre), "Type: Package",
    paste0("Title: Dummy ", nombre), "Version: 0.1.0",
    "Authors@R: person('T', 'D', email = 't@e.com', role = c('aut', 'cre'))",
    "Description: Paquete de prueba.", "License: GPL (>= 3)", "Encoding: UTF-8",
    if (!is.null(imports)) paste0("Imports: ", paste(imports, collapse = ", "))
  ), file.path(ruta, "DESCRIPTION"))
  writeLines(
    if (is.null(imports)) character() else paste0("import(", imports, ")"),
    file.path(ruta, "NAMESPACE")
  )
  if (con_R) {
    dir.create(file.path(ruta, "R"), showWarnings = FALSE)
    writeLines(paste0("f_", nombre, " <- function() '", nombre, "'"),
               file.path(ruta, "R", "f.R"))
    writeLines(c(
      readLines(file.path(ruta, "NAMESPACE"), warn = FALSE),
      paste0("export(f_", nombre, ")")
    ), file.path(ruta, "NAMESPACE"))
  } else {
    dir.create(file.path(ruta, "data"), showWarnings = FALSE)
    writeLines("dato", file.path(ruta, "data", "dato.txt"))
  }
  ruta
}

generar_en_sandbox <- function() {
  skip_if_not(exists("crear_meta_paquete_local", mode = "function"),
              "generador no disponible")
  skip_if_not_installed("whisker")
  skip_if_not_installed("withr")

  # No usar withr::local_tempdir(): borraría el sandbox al retornar el helper,
  # antes de que corran las aserciones del test_that. Se crea a mano y R limpia
  # tempdir() al cerrar la sesión.
  sandbox <- tempfile("regresion-borrado-")
  dir.create(sandbox)
  fuentes <- file.path(sandbox, "fuentes")
  instalables <- file.path(sandbox, "instalables")
  cwd <- file.path(sandbox, "cwd")
  dir.create(fuentes); dir.create(instalables); dir.create(cwd)

  for (pkg in c("aaa", "bbb")) {
    crear_componente_dummy(pkg, fuentes)
    withr::with_dir(fuentes, utils::tar(
      file.path(instalables, paste0(pkg, "_0.1.0.tar.gz")),
      files = pkg, compression = "gzip"))
  }

  # Señuelos en el cwd: incluye los que reproducían el bug (bbb solo-datos, tmp*, doc)
  senuelos <- c("aaa", "bbb", "aaa_extra", "tmp_analisis", "doc", "file2024", "Meta", "libs")
  crear_componente_dummy("aaa", cwd)                       # fuente homónimo
  dir.create(file.path(cwd, "bbb", "data"), recursive = TRUE)
  writeLines(c("Package: bbb", "Version: 0.1.0"), file.path(cwd, "bbb", "DESCRIPTION"))
  writeLines("valioso", file.path(cwd, "bbb", "data", "x.txt"))
  for (d in c("aaa_extra", "tmp_analisis", "doc", "file2024", "Meta", "libs")) {
    dir.create(file.path(cwd, d)); writeLines("valioso", file.path(cwd, d, "x.txt"))
  }

  withr::with_dir(cwd, {
    suppressMessages(crear_meta_paquete_local(
      nombre = "miverso",
      paquetes_locales = c("aaa_0.1.0", "bbb_0.1.0"),
      ruta_instalables = instalables,
      ruta_destino = file.path(sandbox, "proyecto"),
      generar_documentacion = FALSE,
      mostrar_progreso = FALSE
    ))
  })

  list(cwd = cwd, senuelos = senuelos,
       proyecto = file.path(sandbox, "proyecto", "miverso"))
}

test_that("generar el metapaquete no borra ningún directorio del cwd (V7, V8, gen)", {
  s <- generar_en_sandbox()
  archivos <- file.path(s$cwd, s$senuelos, "x.txt")
  archivos[match(c("aaa", "bbb"), s$senuelos)] <- c(
    file.path(s$cwd, "aaa", "R", "f.R"),
    file.path(s$cwd, "bbb", "data", "x.txt")
  )
  antes <- unname(tools::md5sum(archivos))
  for (d in s$senuelos) {
    expect_true(dir.exists(file.path(s$cwd, d)),
                label = paste0("señuelo '", d, "' sobrevive a la generación"))
  }
  expect_true(file.exists(file.path(s$cwd, "bbb", "data", "x.txt")))
  expect_true(file.exists(file.path(s$cwd, "doc", "x.txt")))
  expect_identical(unname(tools::md5sum(archivos)), antes)
})

crear_sandbox_funcional <- function() {
  skip_if_not_installed("whisker")
  skip_if_not_installed("withr")
  sandbox <- tempfile("regresion-funcional-")
  dir.create(sandbox)
  fuentes <- file.path(sandbox, "fuentes")
  instalables <- file.path(sandbox, "instalables")
  destino <- file.path(sandbox, "destino")
  cwd <- file.path(sandbox, "cwd")
  lib <- file.path(sandbox, "lib")
  for (d in c(fuentes, instalables, destino, cwd, lib)) dir.create(d)

  crear_componente_dummy("bbblv", fuentes, con_R = FALSE)
  crear_componente_dummy("aaalv", fuentes, imports = "bbblv")
  crear_componente_dummy("tmpcomponente", fuentes)
  completos <- c("aaalv_0.1.0", "bbblv_0.1.0", "tmpcomponente_0.1.0")
  for (pkg in sub("_.*", "", completos)) {
    withr::with_dir(fuentes, utils::tar(
      file.path(instalables, paste0(pkg, "_0.1.0.tar.gz")),
      files = pkg, compression = "gzip"
    ))
  }

  senuelos <- c("aaalv", "bbblv", "tmpcomponente", "doc", "Meta", "libs")
  for (d in senuelos) {
    dir.create(file.path(cwd, d))
    writeLines(paste("contenido valioso", d), file.path(cwd, d, "sentinel.txt"))
  }
  archivos <- file.path(cwd, senuelos, "sentinel.txt")
  hashes <- unname(tools::md5sum(archivos))

  withr::with_dir(cwd, suppressWarnings(suppressMessages(
    crear_meta_paquete_local(
      nombre = "miversofunc",
      paquetes_locales = completos,
      ruta_instalables = instalables,
      ruta_destino = destino,
      generar_documentacion = FALSE,
      mostrar_progreso = FALSE,
      deps_forzar = character()
    )
  )))

  list(
    sandbox = sandbox, instalables = instalables, cwd = cwd, lib = lib,
    proyecto = file.path(destino, "miversofunc"), paquetes = sub("_.*", "", completos),
    archivos = archivos, hashes = hashes
  )
}

test_that("_install instala una vez, respeta dependencias y adjunta componentes", {
  s <- crear_sandbox_funcional()
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  .libPaths(c(s$lib, old_libs))

  suppressWarnings(utils::install.packages(
    s$proyecto, repos = NULL, type = "source", lib = s$lib, quiet = TRUE
  ))
  suppressWarnings(suppressMessages(
    library("miversofunc", character.only = TRUE, lib.loc = s$lib)
  ))
  expect_false(any(paste0("package:", s$paquetes) %in% search()))

  ns <- asNamespace("miversofunc")
  assign(".contador_bigbang_test", 0L, envir = .GlobalEnv)
  on.exit(rm(".contador_bigbang_test", envir = .GlobalEnv), add = TRUE)
  trace(
    "install_loc_pkg_w_dep",
    tracer = quote(
      .GlobalEnv$.contador_bigbang_test <-
        .GlobalEnv$.contador_bigbang_test + 1L
    ),
    where = ns,
    print = FALSE
  )
  on.exit(untrace("install_loc_pkg_w_dep", where = ns), add = TRUE)

  resultado <- getExportedValue("miversofunc", "miversofunc_install")(
    s$instalables, ".tar.gz"
  )
  expect_equal(.GlobalEnv$.contador_bigbang_test, 3L)
  expect_identical(
    resultado$orden,
    c("bbblv_0.1.0", "aaalv_0.1.0", "tmpcomponente_0.1.0")
  )
  expect_true(all(vapply(s$paquetes, requireNamespace, logical(1), quietly = TRUE)))
  expect_true(all(paste0("package:", s$paquetes) %in% search()))
  expect_identical(unname(tools::md5sum(s$archivos)), s$hashes)
  expect_true(dir.exists(tempdir()))

  detach("package:aaalv", character.only = TRUE, unload = FALSE)
  expect_true("aaalv" %in% loadedNamespaces())
  expect_false("package:aaalv" %in% search())
  getExportedValue("miversofunc", "miversofunc_attach")("aaalv")
  expect_true("package:aaalv" %in% search())
})

test_that("R CMD INSTALL desde fuente conserva contenido dentro del árbol fuente", {
  s <- generar_en_sandbox()
  internos <- c("aaa", "bbb", "tmpcomponente", "doc", "Meta", "libs")
  for (d in internos) {
    dir.create(file.path(s$proyecto, d), showWarnings = FALSE)
    writeLines(paste("sentinel interno", d), file.path(s$proyecto, d, "sentinel.txt"))
  }
  archivos <- file.path(s$proyecto, internos, "sentinel.txt")
  antes <- unname(tools::md5sum(archivos))
  lib <- tempfile("lib-r-cmd-install-")
  dir.create(lib)
  r_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  salida <- system2(
    r_bin,
    c("CMD", "INSTALL", "--no-multiarch", paste0("--library=", shQuote(lib)),
      shQuote(s$proyecto)),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(salida, "status")
  if (is.null(status)) status <- 0L
  expect_equal(status, 0L, info = paste(salida, collapse = "\n"))
  expect_true(all(file.exists(archivos)))
  expect_identical(unname(tools::md5sum(archivos)), antes)
})

test_that("una fuente heredada tóxica se rechaza sin tocar ningún archivo", {
  s <- generar_en_sandbox()
  heredada <- file.path(dirname(s$proyecto), "heredado")
  dir.create(file.path(heredada, "miverso", "R"), recursive = TRUE)
  raiz <- file.path(heredada, "miverso")
  writeLines(
    ".onLoad <- function(...) unlink('aaa', recursive = TRUE, force = TRUE)",
    file.path(raiz, "R", "zzz.R")
  )
  writeLines("rm -rf aaa", file.path(raiz, "cleanup"))
  writeLines("contenido valioso", file.path(raiz, "sentinel.txt"))
  archivos <- list.files(raiz, recursive = TRUE, full.names = TRUE)
  antes <- unname(tools::md5sum(archivos))
  temp_sentinel <- tempfile("tempdir-vive-")
  writeLines("vive", temp_sentinel)

  expect_error(
    crear_meta_paquete_local(
      "miverso", c("aaa_0.1.0", "bbb_0.1.0"),
      file.path(dirname(dirname(s$proyecto)), "instalables"),
      ruta_destino = heredada,
      generar_documentacion = TRUE,
      mostrar_progreso = FALSE
    ),
    "destino debe ser nuevo o estar vac"
  )
  expect_identical(unname(tools::md5sum(archivos)), antes)
  expect_true(file.exists(temp_sentinel))
})

test_that("el grafo detecta ciclos sin instalar durante el análisis", {
  s <- crear_sandbox_funcional()
  fuentes <- file.path(s$sandbox, "fuentes-ciclo")
  archivos <- file.path(s$sandbox, "archivos-ciclo")
  dir.create(fuentes)
  dir.create(archivos)
  crear_componente_dummy("cicloaaa", fuentes, imports = "ciclobbb")
  crear_componente_dummy("ciclobbb", fuentes, imports = "cicloaaa")
  for (pkg in c("cicloaaa", "ciclobbb")) {
    withr::with_dir(fuentes, utils::tar(
      file.path(archivos, paste0(pkg, "_0.1.0.tar.gz")),
      files = pkg, compression = "gzip"
    ))
  }

  env <- new.env(parent = baseenv())
  sys.source(file.path(s$proyecto, "R", "utils.R"), envir = env)
  sys.source(file.path(s$proyecto, "R", "install_packages.R"), envir = env)
  llamadas_instalador <- 0L
  env$install_loc_pkg_w_dep <- function(...) {
    llamadas_instalador <<- llamadas_instalador + 1L
    stop("el análisis no debe instalar")
  }
  expect_error(
    env$crear_grafo_dependencias(
      c("cicloaaa_0.1.0", "ciclobbb_0.1.0"), archivos, ".tar.gz"
    ),
    "Dependencias circulares"
  )
  expect_equal(llamadas_instalador, 0L)
})

test_that("_install convierte fallos del motor en un error visible", {
  s <- crear_sandbox_funcional()
  env <- new.env(parent = baseenv())
  sys.source(file.path(s$proyecto, "R", "attach.R"), envir = env)
  env$install_packages_in_order <- function(...) {
    list(instalados = list(), fallidos = list(
      "aaalv_0.1.0" = "fallo deliberado"
    ))
  }
  expect_error(
    env$miversofunc_install(s$instalables),
    "aaalv_0.1.0: fallo deliberado"
  )
})

test_that("el metapaquete emitido no trae scripts cleanup (V7)", {
  s <- generar_en_sandbox()
  expect_false(file.exists(file.path(s$proyecto, "cleanup")))
  expect_false(file.exists(file.path(s$proyecto, "cleanup.win")))
})

test_that("los hooks de startup emitidos no instalan ni borran nada", {
  s <- generar_en_sandbox()
  archivos <- list.files(file.path(s$proyecto, "R"), full.names = TRUE)
  codigo <- unlist(lapply(archivos, readLines, warn = FALSE))

  # Aislar el cuerpo de .onLoad + .onAttach (desde la 1ª aparición hasta .onUnload)
  ini <- grep("\\.onLoad <- function|\\.onAttach <- function", codigo)
  fin <- grep("\\.onUnload <- function", codigo)
  expect_true(length(ini) > 0)
  hasta <- if (length(fin) > 0) max(fin) else length(codigo)
  startup <- codigo[min(ini):hasta]
  # quitar comentarios para no matchear las notas del fix
  startup_cod <- sub("#.*$", "", startup)

  prohibidos <- c("install\\.packages", "clean_pkg_dirs", "safe_unlink",
                  "unlink\\(", "\\brm -rf\\b", "rmdir", "list\\.files\\(tempdir",
                  "\\bsystem\\(")
  for (pat in prohibidos) {
    expect_false(any(grepl(pat, startup_cod)),
                 label = paste0("startup libre de '", pat, "'"))
  }
})
