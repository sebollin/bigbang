# Deprecated Spanish alias for `create_metapackage()`

Deprecated Spanish alias for
[`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)

## Usage

``` r
crear_meta_paquete_local(
  nombre,
  paquetes_locales,
  ruta_instalables,
  ext = ".tar.gz",
  version = "0.1.0",
  ruta_destino,
  reexportar_funciones = FALSE,
  generar_documentacion = TRUE,
  mostrar_progreso = TRUE,
  autores =
    "person('First', 'Last', email = 'first.last@example.com', role = c('aut', 'cre'))",
  descripcion = "Local Package Metapackage",
  licencia = "MIT + file LICENSE",
  deps_adicionales = NULL,
  deps_ignorar = NULL,
  deps_imports = c("data.table", "dplyr", "ggplot2", "readr", "tibble", "tidyr", "xts",
    "zoo"),
  deps_forzar = NULL,
  verbose = FALSE
)
```

## Arguments

- nombre, paquetes_locales, ruta_instalables:

  Deprecated Spanish arguments.

- ext:

  Character. Archive extension. Defaults to `".tar.gz"`.

- version:

  Character. Version of the meta-package. Defaults to `"0.1.0"`.

- ruta_destino, reexportar_funciones, generar_documentacion:

  Deprecated Spanish arguments.

- mostrar_progreso, autores, descripcion, licencia:

  Deprecated Spanish arguments.

- deps_adicionales, deps_ignorar, deps_imports, deps_forzar, verbose:

  Deprecated Spanish arguments.

## Value

A `bigbang_result`, invisibly.
