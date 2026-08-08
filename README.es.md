# bigbang

> **Creá tus propios metapaquetes de R a partir de paquetes locales.**

[![R-CMD-check](https://github.com/sebollin/bigbang/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebollin/bigbang/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/bigbang)](https://CRAN.R-project.org/package=bigbang)
[![Licencia: GPL
v3](https://img.shields.io/badge/licencia-GPL%20(%3E%3D%203)-142839.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![docs:
English](https://img.shields.io/badge/docs-English-0D9786.svg)](https://github.com/sebollin/bigbang/blob/main/README.md)

**bigbang** construye metapaquetes estilo tidyverse a partir de archivos
locales. Todo metapaquete termina en *-verse*—`tidyverse`,
`tuequipoverse`, el tuyo. Este paquete es lo que los crea: una llamada,
y un nuevo *-verse* existe.

Su razón de ser es **distribuir un conjunto de paquetes como una sola
unidad**. Cuando le pasás a alguien una carpeta con archivos, lo
convertís en tu instalador: tiene que averiguar cuál depende de cuál, en
qué orden instalarlos y qué versiones eran las que realmente funcionaban
juntas. bigbang convierte esa carpeta en un metapaquete que ya sabe las
tres cosas. El conjunto que curaste es el conjunto que instalan.

Eso incluye a los equipos que trabajan detrás de un firewall
institucional y no mantienen un repositorio de paquetes, pero no se
limita a ellos: sirve igual para entregar un conjunto con versiones
fijas a cualquier destinatario.

La arquitectura separa dos acciones:

- `library(<meta>)` adjunta los componentes ya instalados e informa los
  faltantes.
- `<meta>_install()` instala explícitamente los archivos locales una
  sola vez, en orden topológico, y luego los adjunta.

Los hooks de inicio nunca instalan paquetes ni eliminan archivos.

## 🚀 Instalación

La versión estable está en CRAN:

``` r

install.packages("bigbang")
```

La versión de desarrollo está en GitHub:

``` r

# install.packages("pak")
pak::pak("sebollin/bigbang")

# o bien
remotes::install_github("sebollin/bigbang")
```

Y fiel al espíritu offline del paquete, una copia local de la fuente se
instala sin red:

``` r

install.packages("ruta/a/bigbang", repos = NULL, type = "source")
```

## ⚡ Uso rápido

Si `archivos/` contiene `datos_1.2.0.tar.gz` y `reportes_0.9.1.tar.gz`:

``` r

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

``` r

library(equipoverse)
equipoverse_install(cran_deps = "skip")
```

`"skip"` es el modo predeterminado y nunca usa la red. `"error"` falla
si falta una dependencia no local; `"install"` permite instalar desde un
`repos` configurado explícitamente.

## 🧰 API

- [`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)
  crea la fuente completa del metapaquete.
- [`install_local_pkg()`](https://sebollin.github.io/bigbang/reference/install_local_pkg.md)
  instala un archivo local y sus dependencias.
- [`diagnose_dependencies()`](https://sebollin.github.io/bigbang/reference/diagnose_dependencies.md)
  busca dependencias implícitas.
- [`scan_bigbang_artifact()`](https://sebollin.github.io/bigbang/reference/scan_bigbang_artifact.md)
  examina artefactos antiguos sin cargarlos.

Los nombres españoles anteriores siguen disponibles como aliases
deprecados de transición. La API canónica usa inglés snake_case.

## 🗜️ ZIP y portabilidad

Un ZIP con `Meta/package.rds` es un binario de Windows y solo se instala
en Windows con `type = "win.binary"`. Los demás ZIP con DESCRIPTION se
extraen a un temporal propio y se instalan como fuente. Todo texto
generado se escribe en UTF-8 explícito y el CI incluye Linux, Windows y
macOS.

## 🌎 Idioma

El inglés es el idioma fuente del código, la ayuda y los mensajes. Los
mensajes tienen traducción completa al español mediante gettext. En R
4.2 o posterior:

``` r

Sys.setLanguage("es")
```

En versiones anteriores, defina `LANGUAGE=es` antes de iniciar R. La
guía completa está en
[`vignette("bigbang-es", package = "bigbang")`](https://sebollin.github.io/bigbang/articles/bigbang-es.md).
Cuando `rhelpi18n` madure y llegue a CRAN se podrá evaluar un módulo
separado `bigbang.es` para la ayuda interactiva.

## 🧭 Diferencias con otras herramientas

`bigbang` distribuye un conjunto curado de archivos, con versiones
fijas, como una sola unidad instalable. `miniCRAN` y `drat` son
preferibles cuando se necesita un repositorio convencional con índices,
varias versiones y semántica de repositorio. `pkgverse` cubre el caso
más pequeño de agrupar paquetes disponibles desde repositorios, sin el
instalador de archivos locales de `bigbang`.

## 🛡️ Seguridad y artefactos antiguos

Un antecesor no publicado emitía limpieza relativa al directorio de
trabajo y podía eliminar carpetas con nombres de componentes. Esas rutas
fueron retiradas y están cubiertas por tests destructivos que solo usan
árboles temporales.

No cargue ni documente una fuente antigua antes de escanearla:

``` r

scan_bigbang_artifact("ruta/al/artefacto", dry_run = TRUE)
```

Si resulta vulnerable, póngala en cuarentena y genere una versión nueva
en una ruta nueva y vacía. Nunca regenere in-place una fuente no
clasificada.

## 🙏 Agradecimientos

bigbang nació de una sugerencia de [Richard
Detomasi](https://github.com/RichDeto), quien propuso construir una
herramienta de metapaquetes y señaló
[pegeler/metapackage](https://github.com/pegeler/metapackage) como
antecedente. El diseño y la implementación —incluida la resolución de
dependencias mediante grafos— son de Sebastián Lucas. El logo hexagonal
fue creado con
[hexSticker](https://github.com/GuangchuangYu/hexSticker).

## 🤝 Aportes de la comunidad

Los aportes son bienvenidos: reportes de errores e ideas en
[issues](https://github.com/sebollin/bigbang/issues), y pull requests
siguiendo
[CONTRIBUTING.md](https://github.com/sebollin/bigbang/blob/main/CONTRIBUTING.md).
El paquete busca mantenerse chico y enfocado — ver *Diferencias con
otras herramientas* para lo que queda deliberadamente fuera de alcance.

## 📖 Citar el paquete

``` r

citation("bigbang")
```

``` bibtex
@Manual{bigbang2026,
  title  = {bigbang: Build 'Tidyverse'-Style Meta-Packages from Local Package Files},
  author = {Sebastian Lucas},
  note   = {R package version 0.1.0},
  year   = {2026},
  url    = {https://github.com/sebollin/bigbang},
}
```
