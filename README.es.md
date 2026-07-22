# bigbang — create custom R metapackages from local packages

<!-- Logo en diseño:
<img src="man/figures/logo.png" align="right" height="139" alt="logo de bigbang" />
-->

**bigbang** construye metapaquetes estilo tidyverse a partir de archivos locales.
Todo metapaquete termina en *-verse*—`tidyverse`, `tuequipoverse`, el tuyo. Este
paquete es lo que los crea: una llamada, y un nuevo *-verse* existe.

Está pensado para equipos que intercambian archivos locales de paquetes R
(`.tar.gz` o `.zip`) detrás
de un firewall institucional y no mantienen un repositorio de paquetes.

La arquitectura separa dos acciones:

- `library(<meta>)` adjunta los componentes ya instalados e informa los faltantes.
- `<meta>_install()` instala explícitamente los archivos locales una sola vez, en
  orden topológico, y luego los adjunta.

Los hooks de inicio nunca instalan paquetes ni eliminan archivos.

## Uso rápido

Si `archivos/` contiene `datos_1.2.0.tar.gz` y `reportes_0.9.1.tar.gz`:

```r
library(bigbang)

resultado <- create_metapackage(
  name = "equipoverse",
  packages = c("datos_1.2.0", "reportes_0.9.1"),
  pkg_dir = "archivos",
  dest_dir = tempdir(),
  document = TRUE
)
resultado
```

Luego de construir e instalar `equipoverse` como cualquier paquete R:

```r
library(equipoverse)
equipoverse_install(cran_deps = "skip")
```

`"skip"` es el modo predeterminado y nunca usa la red. `"error"` falla si falta
una dependencia no local; `"install"` permite instalar desde un `repos`
configurado explícitamente.

## API

- `create_metapackage()` crea la fuente completa del metapaquete.
- `install_local_pkg()` instala un archivo local y sus dependencias.
- `diagnose_dependencies()` busca dependencias implícitas.
- `scan_bigbang_artifact()` examina artefactos antiguos sin cargarlos.

Los nombres españoles anteriores siguen disponibles como aliases deprecados de
transición. La API canónica usa inglés snake_case.

## ZIP y portabilidad

Un ZIP con `Meta/package.rds` es un binario de Windows y solo se instala en
Windows con `type = "win.binary"`. Los demás ZIP con DESCRIPTION se extraen a un
temporal propio y se instalan como fuente. Todo texto generado se escribe en
UTF-8 explícito y el CI incluye Linux, Windows y macOS.

## Idioma

El inglés es el idioma fuente del código, la ayuda y los mensajes. Los mensajes
tienen traducción completa al español mediante gettext. En R 4.2 o posterior:

```r
Sys.setLanguage("es")
```

En versiones anteriores, defina `LANGUAGE=es` antes de iniciar R. La guía completa
está en `vignette("bigbang-es", package = "bigbang")`. Cuando `rhelpi18n`
madure y llegue a CRAN se podrá evaluar un módulo separado `bigbang.es` para la
ayuda interactiva.

## Diferencias con otras herramientas

`bigbang` distribuye una selección fija de archivos mediante un metapaquete.
`miniCRAN` y `drat` son preferibles cuando se necesita un repositorio convencional
con índices, varias versiones y semántica de repositorio. `pkgverse` cubre el caso
más pequeño de agrupar paquetes disponibles desde repositorios, sin el instalador
offline de archivos locales de `bigbang`.

## Seguridad y artefactos antiguos

Un antecesor no publicado emitía limpieza relativa al directorio de trabajo y
podía eliminar carpetas con nombres de componentes. Esas rutas fueron retiradas y
están cubiertas por tests destructivos que solo usan árboles temporales.

No cargue ni documente una fuente antigua antes de escanearla:

```r
scan_bigbang_artifact("ruta/al/artefacto", dry_run = TRUE)
```

Si resulta vulnerable, póngala en cuarentena y genere una versión nueva en una
ruta nueva y vacía. Nunca regenere in-place una fuente no clasificada.
