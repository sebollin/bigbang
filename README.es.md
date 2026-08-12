# bigbang <img src="man/figures/logo.png" align="right" height="200" alt="logo de bigbang" />

> **Creá tus propios metapaquetes de R a partir de paquetes locales.**

[![R-CMD-check](https://github.com/sebollin/bigbang/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebollin/bigbang/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/bigbang)](https://CRAN.R-project.org/package=bigbang)
[![r-universe](https://sebollin.r-universe.dev/bigbang/badges/version)](https://sebollin.r-universe.dev/bigbang)
[![Licencia: GPL v3](https://img.shields.io/badge/licencia-GPL%20(%3E%3D%203)-142839.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![docs: English](https://img.shields.io/badge/docs-English-0D9786.svg)](https://github.com/sebollin/bigbang/blob/main/README.md)

**bigbang** construye metapaquetes estilo tidyverse a partir de archivos locales.
Todo metapaquete termina en *-verse*—`tidyverse`, `tuequipoverse`, el tuyo. Este
paquete es lo que los crea: una llamada, y un nuevo *-verse* existe.

Su razón de ser es **distribuir un conjunto de paquetes como una sola unidad**.

Supongamos que mantenés cuatro paquetes propios que se usan juntos y dependen
entre sí. Entra alguien nuevo al equipo, o te los pide otra oficina. Como no
están en CRAN, la forma de entregarlos es mandar una carpeta con los `.tar.gz`.

En el mejor de los casos le sumás instrucciones: instalá este primero, después
este otro, esta versión va con aquella. Pero esas instrucciones son trabajo
manual para quien recibe, y un documento más que mantener al día cada vez que
cambia una versión o entra un paquete nuevo.

bigbang pone ese conocimiento adentro del paquete, y también los archivos: el
metapaquete generado lleva los `.tar.gz` de sus componentes. Le entregás un solo
archivo y nada más —ni la carpeta al lado, ni una ruta que acordar— y del otro
lado alcanza una línea. El orden sale del grafo real de dependencias y las
versiones quedan registradas, así que no hay nada que seguir a mano ni nada que
pueda quedar desfasado.

Eso incluye a los equipos que trabajan detrás de un firewall institucional y no
mantienen un repositorio de paquetes, pero no se limita a ellos: sirve igual para
entregar un conjunto con versiones fijas a cualquier destinatario.

La arquitectura separa dos acciones:

- `library(<meta>)` adjunta los componentes ya instalados e informa los faltantes.
- `<meta>_install()` instala explícitamente los archivos locales una sola vez, en
  orden topológico, y luego los adjunta.

Los hooks de inicio nunca instalan paquetes ni eliminan archivos.

## 🚀 Instalación

La versión estable está en CRAN:

```r
install.packages("bigbang")
```

La versión de desarrollo se publica como binario en r-universe, así que no
requiere compilar nada:

```r
install.packages("bigbang", repos = c("https://sebollin.r-universe.dev",
                                      "https://cloud.r-project.org"))
```

O desde las fuentes en GitHub:

```r
# install.packages("pak")
pak::pak("sebollin/bigbang")

# o bien
remotes::install_github("sebollin/bigbang")
```

Y fiel al espíritu offline del paquete, una copia local de la fuente se
instala sin red:

```r
install.packages("ruta/a/bigbang", repos = NULL, type = "source")
```

## ⚡ Uso rápido

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
equipoverse_install()
```

Las funciones exportadas por los componentes adjuntos quedan disponibles en
forma directa (por ejemplo, `informe()`) o mediante su propio espacio de nombres
(`reportes::informe()`). No se copian al espacio de nombres del metapaquete, por
lo que `equipoverse::informe()` no está disponible.

`equipoverse` lleva sus componentes adentro, así que la llamada no necesita
argumentos y eso es todo lo que tiene que hacer quien lo recibe: le pasás el
`equipoverse_0.1.0.tar.gz` construido y nada más. Si preferís que los archivos
queden en una ubicación compartida, generá con `include_archives = FALSE` y
entonces `equipoverse_install()` va a pedir un `pkg_dir` explícito.

`"skip"` es el modo predeterminado y nunca usa la red. `"error"` falla si falta
una dependencia no local; `"install"` permite instalar desde un `repos`
configurado explícitamente.

Los instaladores generados también aceptan `upgrade = "newer"` (predeterminado),
`"always"` o `"never"`; `force = TRUE` equivale a `upgrade = "always"`. Los
metapaquetes usan un mensaje opcional de dos columnas mediante `cli` y vuelven
al banner ASCII cuando `cli` no está disponible. Use
`options(equipoverse.quiet = TRUE)` para silenciar el arranque y
`equipoverse_conflicts()` para revisar conflictos de enmascaramiento.

Para generar una guía de flujo ordenada, indique cada componente una vez:

```r
workflow = c("Importación" = "datos", "Informe" = "reportes")
```

## Validación y tolerancias explícitas

bigbang mantiene como errores duros todas las validaciones que protegen a quien
recibe el metapaquete: archivos inseguros o malformados, metadatos inválidos,
componentes duplicados, restricciones locales insatisfechas y ciclos. Esas
validaciones no se pueden desactivar.

Las comprobaciones de prolijidad se relajan de forma individual y explícita:

```r
create_metapackage(
  # ...,
  tolerate = c("filename_mismatch", "unincluded_local_dep")
)
```

`"filename_mismatch"` silencia los avisos cuando el nombre del archivo difiere
de la identidad declarada en DESCRIPTION. `"unincluded_local_dep"` convierte en
aviso el error por una dependencia local disponible en las fuentes pero omitida
de `packages`. El metapaquete generado no incluirá esa dependencia, por lo que
el receptor debe proporcionarla mediante `pkg_dir` o un repositorio con
`cran_deps = "install"`. Cada relajación aplicada queda en
`result$tolerated`; los nombres desconocidos son un error. No existe un
interruptor que desactive toda la validación.

Durante la generación, bigbang valida todo lo que protege al receptor del
metapaquete generado. Los archivos inseguros o malformados, la metadata inválida,
los componentes duplicados, las restricciones de versión locales insatisfechas y
los ciclos de dependencias siempre son errores duros. La instalación es más
tolerante: puede conservar un componente ya instalado si no puede leer un archivo
que no va a usar, e informa el motivo.

bigbang **no** ejecuta `R CMD check` sobre los paquetes componentes. Un
componente con warnings o notes puede incluirse: las validaciones se limitan a
que el metapaquete distribuido pueda identificar e instalar sus componentes de
forma segura.

## 🎛️ De dónde salen los componentes

Cualquier elemento de `packages` que sea un archivo existente se usa como ruta;
el resto se resuelve como stem en `pkg_dir`, que acepta más de un directorio. Así
que todo esto funciona, incluso mezclado en una sola llamada:

```r
create_metapackage(
  "equipoverse",
  packages = c(
    "/srv/archivos/primero_1.2.0.tar.gz",  # una ruta, cualquier directorio
    "~/builds/segundo.zip",                # otro directorio, otro formato
    "tercero_0.4.0",                       # un stem resuelto en pkg_dir
    "~/fuentes/cuarto"                     # un directorio fuente, se empaqueta
  ),
  pkg_dir = c("/srv/archivos", "~/builds"),
  dest_dir = "~/proyectos"
)
```

Un nombre de archivo sin versión es válido: `Package` y `Version` salen del
`DESCRIPTION` del archivo. Si el nombre discrepa, bigbang avisa y le cree al
`DESCRIPTION`.

Los directorios fuente se construyen con el paquete opcional `pkgbuild`, en un
temporal, y requieren `include_archives = TRUE`, porque ese archivo temporal no
sobrevive a la llamada.

`packages` también puede ser la ruta a un **manifiesto**: un componente por
línea, `#` para comentarios. Las rutas relativas se resuelven contra el
directorio del manifiesto; las rutas absolutas y las que empiezan con `~` se
usan tal cual; y los nombres de archivo se buscan además en `pkg_dir`, así que
la lista puede vivir bajo control de versiones y los archivos no.

## 🎚️ Opciones de generación

```r
plan <- create_metapackage(..., dry_run = TRUE)  # resuelve, valida, no escribe
plan$order                                       # orden de instalación
plan$files                                       # qué se escribiría
plan$findings                                    # todos los hallazgos
```

`dry_run = TRUE` no crea `dest_dir` ni toca el destino, así que es una forma
segura de ver qué haría una llamada antes de que la haga.

- `on_component_error = "skip"` genera con los componentes válidos en lugar de
  abortar, e informa los que dejó afuera. El descarte es transitivo: un
  componente que depende de uno excluido también queda excluido, y se informa la
  cadena. Si un archivo inválido todavía tiene un `DESCRIPTION` legible, se usa
  el nombre declarado del paquete; de lo contrario bigbang recurre al nombre del
  archivo e informa esa limitación. Excluir todo es un error. Durante un update,
  una entrada fallida nunca autoriza borrar un archivo ya embarcado por el
  proyecto. Si el componente anterior no se puede identificar sin ambigüedad, la
  reconciliación de archivos espera a un update limpio.
- `update = TRUE` regenera en el mismo lugar. La generación registra un
  manifiesto de los archivos que escribió con sus hashes de contenido; `update`
  reescribe solo esos, y se niega a correr si falta el manifiesto o si algún
  archivo generado fue modificado o borrado a mano. Lo que bigbang no escribió no
  se toca nunca. Antes de cambiar un proyecto existente, bigbang respalda cada
  archivo generado y su manifiesto. Si el update falla, restaura ese estado para
  poder reintentarlo. Tanto el dry run como el resultado real enumeran las rutas
  eliminadas en `removed_files`.
  Quitar un componente elimina su archivo embarcado, que puede ser la última copia.
  También se niega a escribir a través de una raíz de proyecto simbólica o de
  enlaces simbólicos dentro del proyecto generado, incluidos los enlaces en
  directorios padre de los archivos generados.
- `install_upgrade` fija la política de actualización por defecto del instalador
  emitido, así que decidís al generar si los destinatarios quedan clavados en las
  versiones que distribuís (`"always"`) o conservan lo más nuevo que ya tengan
  (`"newer"`, el default).

El instalador generado también acepta `only` para instalar un subconjunto —las
dependencias locales de lo elegido se agregan solas— y `lib` para elegir la
biblioteca donde instala.

## 🧰 API

- `create_metapackage()` crea la fuente completa del metapaquete.
- `install_local_pkg()` instala un archivo local y sus dependencias.
- `diagnose_dependencies()` busca dependencias implícitas.
- `scan_bigbang_artifact()` examina artefactos antiguos sin cargarlos.

La API usa inglés snake_case.

## 🗜️ ZIP y portabilidad

Un ZIP con `Meta/package.rds` es un binario de Windows y solo se instala en
Windows con `type = "win.binary"`. Los demás ZIP con DESCRIPTION se extraen a un
temporal propio y se instalan como fuente. Todo texto generado se escribe en
UTF-8 explícito y el CI incluye Linux, Windows y macOS.

## 🌎 Idioma

El inglés es el idioma fuente del código, la ayuda y los mensajes. Los mensajes
tienen traducción completa al español mediante gettext. En R 4.2 o posterior:

```r
Sys.setLanguage("es")
```

En versiones anteriores, defina `LANGUAGE=es` antes de iniciar R. La guía completa
está en `vignette("bigbang-es", package = "bigbang")`. Cuando `rhelpi18n`
madure y llegue a CRAN se podrá evaluar un módulo separado `bigbang.es` para la
ayuda interactiva.

## 🔭 Proyectos relacionados

- [pegeler/metapackage](https://github.com/pegeler/metapackage), de Paul
  Egeler, es un metapaquete personal declarativo basado en paquetes disponibles
  en repositorios en línea. bigbang, en cambio, genera metapaquetes a partir de
  archivos locales.
- [metaverse](https://rmetaverse.github.io/metaverse/) es un metapaquete
  comunitario para síntesis de evidencia, modelado sobre tidyverse. El mensaje
  de adjunción y el diseño de `<meta>_packages()` en los metapaquetes generados
  por bigbang se inspiran en tidyverse y en metaverse (Westgate y colaboradores).

## 🧭 Diferencias con otras herramientas

`bigbang` distribuye un conjunto curado de archivos, con versiones fijas, como
una sola unidad instalable. `miniCRAN` y `drat` son preferibles cuando se necesita
un repositorio convencional con índices, varias versiones y semántica de
repositorio. `pkgverse` cubre el caso más pequeño de agrupar paquetes disponibles
desde repositorios, sin el instalador de archivos locales de `bigbang`.

## 🛡️ Seguridad y artefactos antiguos

Un antecesor no publicado emitía limpieza relativa al directorio de trabajo y
podía eliminar carpetas con nombres de componentes. Esas rutas fueron retiradas y
están cubiertas por tests destructivos que solo usan árboles temporales.

No cargue ni documente una fuente antigua antes de escanearla:

```r
scan_bigbang_artifact("ruta/al/artefacto", dry_run = TRUE)
```

Si resulta vulnerable, póngala en cuarentena y genere una versión nueva en una
ruta nueva y vacía. Nunca regenere in-place una fuente no clasificada.

## 🙏 Agradecimientos

bigbang nació de una sugerencia de
[Richard Detomasi](https://github.com/RichDeto), quien propuso construir una
herramienta de metapaquetes y señaló
[pegeler/metapackage](https://github.com/pegeler/metapackage) como antecedente.
El diseño y la implementación —incluida la resolución de dependencias mediante
grafos— son de Sebastián Lucas. El logo hexagonal fue creado con
[hexSticker](https://github.com/GuangchuangYu/hexSticker).

## 🤝 Aportes de la comunidad

Los aportes son bienvenidos: reportes de errores e ideas en
[issues](https://github.com/sebollin/bigbang/issues), y pull requests siguiendo
[CONTRIBUTING.md](https://github.com/sebollin/bigbang/blob/main/CONTRIBUTING.md). El paquete busca mantenerse chico y
enfocado — ver *Diferencias con otras herramientas* para lo que queda
deliberadamente fuera de alcance.

## 📖 Citar el paquete

```r
citation("bigbang")
```

```bibtex
@Manual{bigbang2026,
  title  = {bigbang: Build 'Tidyverse'-Style Meta-Packages from Local Package Files},
  author = {Sebastián Lucas},
  note   = {R package version 0.3.0},
  year   = {2026},
  url    = {https://sebollin.github.io/bigbang/},
}
```
