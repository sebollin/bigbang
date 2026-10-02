# bigbang

> **Creá tus propios metapaquetes de R a partir de paquetes locales.**

[![R-CMD-check](https://github.com/sebollin/bigbang/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebollin/bigbang/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/bigbang)](https://CRAN.R-project.org/package=bigbang)
[![r-universe](https://sebollin.r-universe.dev/bigbang/badges/version)](https://sebollin.r-universe.dev/bigbang)
[![Licencia: GPL
v3](https://img.shields.io/badge/licencia-GPL%20(%3E%3D%203)-142839.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Ciclo de vida:
estable](https://img.shields.io/badge/ciclo%20de%20vida-estable-0D9786.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![docs:
English](https://img.shields.io/badge/docs-English-0D9786.svg)](https://github.com/sebollin/bigbang/blob/main/README.md)

**bigbang** construye metapaquetes estilo tidyverse a partir de archivos
locales. Todo metapaquete termina en *-verse*—`tidyverse`,
`tuequipoverse`, el tuyo. Este paquete es lo que los crea: una llamada,
y un nuevo *-verse* existe.

Su razón de ser es **distribuir un conjunto de paquetes como una sola
unidad**.

Supongamos que mantenés cuatro paquetes propios que se usan juntos y
dependen entre sí. Entra alguien nuevo al equipo, o te los pide otra
oficina. Como no están en CRAN, la forma de entregarlos es mandar una
carpeta con los `.tar.gz`.

En el mejor de los casos le sumás instrucciones: instalá este primero,
después este otro, esta versión va con aquella. Pero esas instrucciones
son trabajo manual para quien recibe, y un documento más que mantener al
día cada vez que cambia una versión o entra un paquete nuevo.

bigbang pone ese conocimiento adentro del paquete, y también los
archivos: el metapaquete generado lleva los `.tar.gz` de sus
componentes. Le entregás un solo archivo y nada más —ni la carpeta al
lado, ni una ruta que acordar— y del otro lado alcanza una línea. El
orden sale del grafo real de dependencias y las versiones quedan
registradas, así que no hay nada que seguir a mano ni nada que pueda
quedar desfasado.

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

La versión de desarrollo se publica como binario en r-universe, así que
no requiere compilar nada:

``` r

install.packages("bigbang", repos = c("https://sebollin.r-universe.dev",
                                      "https://cloud.r-project.org"))
```

O desde las fuentes en GitHub:

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
equipoverse_install()
```

Las funciones exportadas por los componentes adjuntos quedan disponibles
en forma directa (por ejemplo, `informe()`) o mediante su propio espacio
de nombres (`reportes::informe()`). No se copian al espacio de nombres
del metapaquete, por lo que `equipoverse::informe()` no está disponible.

`equipoverse` lleva sus componentes adentro, así que la llamada no
necesita argumentos y eso es todo lo que tiene que hacer quien lo
recibe: le pasás el `equipoverse_0.1.0.tar.gz` construido y nada más. Si
preferís que los archivos queden en una ubicación compartida, generá con
`include_archives = FALSE` y entonces `equipoverse_install()` va a pedir
un `pkg_dir` explícito.

### Actualizaciones interrumpidas y recuperación

Un `update = TRUE` interrumpido deja un diario durable junto al
proyecto. El update siguiente lo revisa antes de escribir; usá
`dry_run = TRUE` para ver la acción prevista. Después de confirmar que
no sigue corriendo otro update, pasá `recover = TRUE` para preservar
bytes de usuario desconocidos y completar el rollback o la recuperación.

`"skip"` es el modo predeterminado y nunca usa la red. `"error"` falla
si falta una dependencia no local; `"install"` permite instalar desde un
`repos` configurado explícitamente.

Los instaladores generados también aceptan `upgrade = "newer"`
(predeterminado), `"always"` o `"never"`; `force = TRUE` equivale a
`upgrade = "always"`. Los metapaquetes usan un mensaje opcional de dos
columnas mediante `cli` y vuelven al banner ASCII cuando `cli` no está
disponible. Use `options(equipoverse.quiet = TRUE)` para silenciar el
arranque y `equipoverse_conflicts()` para revisar conflictos de
enmascaramiento.

Para generar una guía de flujo ordenada, indique cada componente una
vez:

``` r

workflow = c("Importación" = "datos", "Informe" = "reportes")
```

## Validación y tolerancias explícitas

Para exponer las exportaciones explícitas mediante bindings activos de
solo lectura, usá `reexport = TRUE` al generar. Los componentes quedan
fuera de `Imports` y `Depends`, así que el metapaquete se puede instalar
y cargar sin conexión antes de que existan. Evaluar un binding nunca
lanza un error: si falta el componente, no puede cargarse o es una
instalación vieja que ya no exporta el símbolo, devuelve una función
provisoria. Al llamarla informa el componente, la versión instalada, la
exportación faltante y la llamada a `<meta>_install()` que repara la
instalación. Así también son seguros la inspección del namespace
([`as.list()`](https://rdrr.io/r/base/list.html),
[`mget()`](https://rdrr.io/r/base/get.html)) y los paneles de entorno
del IDE. Para exports que no son funciones, el acceso devuelve esa
función en lugar del objeto hasta la instalación. Después el binding
resuelve la función u objeto real sin recargar el metapaquete. Solo las
directivas `export()` explícitas se convierten en bindings, incluidos
los nombres no sintácticos, que se citan de forma segura en NAMESPACE.
Las clases y métodos S4 quedan disponibles cargando el componente. Un
objeto restaurado con [`readRDS()`](https://rdrr.io/r/base/readRDS.html)
no carga un componente por sí mismo, así que R no puede despachar su
método S3 hasta que el componente se haya cargado.

### Colisiones de reexportación

Cuando varios componentes exportan el mismo símbolo, usá
`reexport_prefer = c(símbolo = "componente")` para elegir su proveedor o
`reexport_exclude = "símbolo"` para excluirlo. Toda colisión requiere
una de esas decisiones porque el análisis estático no puede probar que
dos objetos exportados sean el mismo en tiempo de ejecución. El
diagnóstico etiqueta cada colisión como `probable_same_object`,
`distinct_definitions` o `undetermined`, y ordena las razones de sus
fuentes. Para un `probable_same_object` elegido, `<meta>_install()`
verifica los dueños instalados en un subproceso limpio de R, con el
mismo orden de bibliotecas que usa el runtime: primero la biblioteca
destino y después [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).
Así, un dueño instalado solo en una biblioteca posterior se resuelve y
se compara con el objeto que va a recibir el usuario. El resultado
distingue entre no instalado, no exportado y cargado desde otra
biblioteca; si un dueño instalado ya no exporta el símbolo, lo informa
con el mismo aviso de verificación. Si el subproceso no se puede
ejecutar, el resultado queda explícitamente sin verificar. Un espacio de
nombres ya cargado desde otra biblioteca se informa antes de verificar.
`<meta>_conflicts()` repite la comprobación y emite de nuevo el aviso,
por lo que es la forma de verificar otra vez después de instalar. Un
aviso de identidad `FALSE` distingue copias equivalentes de funciones
(mismo cuerpo y formales) de objetos distintos. Si
`on_component_error = "skip"` omite un dueño necesario, la generación
falla en vez de crear un binding hacia un componente que no viaja. El
objeto que devuelve conserva los conflictos de enmascaramiento y guarda
el data frame de verificación como atributo. Leelo con
`<meta>_reexport_verification(conflicts)`, de modo que un componente que
exporte ese nombre siga visible en la lista de conflictos.

En un update interrumpido, el lock se decide primero por el dueño y
después por el reclamante. Un dueño vivo siempre bloquea. Un dueño
incierto bloquea salvo con `recover = TRUE`; uno muerto se puede
reclamar. Un lock descartado cuyo `owner.rds` está vivo se restaura o
bloquea informando el PID y nunca se borra. Solo cuando ese dueño está
probado como muerto se decide por la vida del reclamante si hay que
bloquear, exigir `recover = TRUE` o descartar la entrada. Antes de
continuar, el update vuelve a validar que el lock publicado siga siendo
suyo; si cambió, aborta antes de mutar. Un enlace simbólico en el nombre
del lock se informa como enlace y `recover = TRUE` aparta el enlace sin
seguir su destino. Al descartar, el diario primero se renombra a un
hermano privado impredecible después de verificar su inventario, y cada
borrado vuelve a verificar ancestros y MD5 justo antes de
[`unlink()`](https://rdrr.io/r/base/unlink.html). R no ofrece
`unlinkat()`/`O_NOFOLLOW`, así que un proceso del mismo usuario que
reemplace activamente carpetas del diario durante el descarte sigue
siendo una frontera de integridad, igual que los registros falsificados;
se mide la ventana restante entre la última verificación y
[`unlink()`](https://rdrr.io/r/base/unlink.html).

En Linux, la vida usa `/proc/<pid>` y considera muertos los estados
zombie (`Z`) y terminado (`X`). Sin `/proc`, un fallo de `kill(pid, 0)`
es incierto salvo que `ps -p` pruebe que el PID no existe; un error de
permisos y un proceso de otro usuario nunca se consideran muertos. Un
proceso del mismo usuario con permiso de escritura puede falsificar
estos registros: eso queda fuera del modelo de integridad. La
recuperación también exige que el dueño de `state.rds` coincida con el
de `marker.rds`; si no coincide, el diario se aparta y no se usa para
revertir.

La prueba de vida y el token de inicio se eligen según la plataforma:

| Plataforma | Prueba de existencia | Token de inicio | Política |
|----|----|----|----|
| Linux con `/proc` | `/proc/<pid>/stat` | campo 20, fuente `proc` | Un PID no terminal cuyo token coincide está probado como vivo. |
| macOS, BSD o Unix sin `/proc` | `kill(pid, 0)` o `LC_ALL=C ps -p <pid>` | `LC_ALL=C ps -o lstart= -p <pid>`, fuente `ps` | Un PID vivo cuyo `lstart` coincide está probado como vivo. |
| Windows | Nunca se sondea con [`tools::pskill()`](https://rdrr.io/r/tools/pskill.html) | Ninguno | La propiedad es incierta; la recuperación nunca vence a un dueño probado vivo. |

La fuente se guarda junto con el token, por lo que nunca se compara un
token de `/proc` con uno de `ps`. `LANGUAGE` y `LC_TIME` no pueden
cambiar el token portable de `ps`.

El análisis de colisiones es una ayuda de diagnóstico. La garantía es la
decisión explícita `reexport_prefer` o `reexport_exclude` más la
verificación de `<meta>_install()`; `library(<meta>)` por sí sola no
verifica los dueños instalados. El escáner es conservador y puede contar
un `delayedAssign` que nunca se fuerza, una rama `if (FALSE)` o el
cuerpo de un
[`reg.finalizer()`](https://rdrr.io/r/base/reg.finalizer.html).

bigbang mantiene como errores duros todas las validaciones que protegen
a quien recibe el metapaquete: archivos inseguros o malformados,
metadatos inválidos, componentes duplicados, restricciones locales
insatisfechas y ciclos. Esas validaciones no se pueden desactivar.

Las comprobaciones de prolijidad se relajan de forma individual y
explícita:

``` r

create_metapackage(
  # ...,
  tolerate = c("filename_mismatch", "unincluded_local_dep")
)
```

`"filename_mismatch"` silencia los avisos cuando el nombre del archivo
difiere de la identidad declarada en DESCRIPTION.
`"unincluded_local_dep"` convierte en aviso el error por una dependencia
local disponible en las fuentes pero omitida de `packages`. El
metapaquete generado no incluirá esa dependencia, por lo que el receptor
debe proporcionarla mediante `pkg_dir` o un repositorio con
`cran_deps = "install"`. Cada relajación aplicada queda en
`result$tolerated`; los nombres desconocidos son un error. No existe un
interruptor que desactive toda la validación.

Durante la generación, bigbang valida todo lo que protege al receptor
del metapaquete generado. Los archivos inseguros o malformados, la
metadata inválida, los componentes duplicados, las restricciones de
versión locales insatisfechas y los ciclos de dependencias siempre son
errores duros. La instalación es más tolerante: puede conservar un
componente ya instalado si no puede leer un archivo que no va a usar, e
informa el motivo.

bigbang **no** ejecuta `R CMD check` sobre los paquetes componentes. Un
componente con warnings o notes puede incluirse: las validaciones se
limitan a que el metapaquete distribuido pueda identificar e instalar
sus componentes de forma segura.

## 🎛️ De dónde salen los componentes

Cualquier elemento de `packages` que sea un archivo existente se usa
como ruta; el resto se resuelve como stem en `pkg_dir`, que acepta más
de un directorio. Así que todo esto funciona, incluso mezclado en una
sola llamada:

``` r

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

Un nombre de archivo sin versión es válido: `Package` y `Version` salen
del `DESCRIPTION` del archivo. Si el nombre discrepa, bigbang avisa y le
cree al `DESCRIPTION`.

También se puede usar un nombre de paquete pelado como `"geomides"`
cuando un único archivo de `pkg_dir` declara `Package: geomides`. La
comparación usa la identidad declarada, por lo que `"geo"` nunca
selecciona `geomides`; si coinciden varias versiones o fuentes, bigbang
lista los candidatos y pide un stem o una ruta explícitos. Los archivos
ilegibles encontrados durante esa búsqueda se excluyen con un aviso que
nombra cada archivo afectado.

Los directorios fuente se construyen con el paquete opcional `pkgbuild`,
en un temporal, y requieren `include_archives = TRUE`, porque ese
archivo temporal no sobrevive a la llamada.

`packages` también puede ser la ruta a un **manifiesto**: un componente
por línea, `#` para comentarios. Las rutas relativas se resuelven contra
el directorio del manifiesto; las rutas absolutas y las que empiezan con
`~` se usan tal cual; y los nombres de archivo se buscan además en
`pkg_dir`, así que la lista puede vivir bajo control de versiones y los
archivos no.

## 🎚️ Opciones de generación

``` r

plan <- create_metapackage(..., dry_run = TRUE)  # resuelve, valida, no escribe
plan$order                                       # orden de instalación
plan$files                                       # qué se escribiría
plan$findings                                    # todos los hallazgos
```

`dry_run = TRUE` no crea `dest_dir` ni toca el destino, así que es una
forma segura de ver qué haría una llamada antes de que la haga. Durante
un update también planifica la reconciliación de cada diario hermano e
informa su ruta y acción sin modificar esas carpetas.

- `on_component_error = "skip"` genera con los componentes válidos en
  lugar de abortar, e informa los que dejó afuera. El descarte es
  transitivo: un componente que depende de uno excluido también queda
  excluido, y se informa la cadena. Si un archivo inválido todavía tiene
  un `DESCRIPTION` legible, se usa el nombre declarado del paquete; de
  lo contrario bigbang recurre al nombre del archivo e informa esa
  limitación. Excluir todo es un error. Durante un update, una entrada
  fallida nunca autoriza borrar un archivo ya embarcado por el proyecto.
  Si el componente anterior no se puede identificar sin ambigüedad, la
  reconciliación de archivos espera a un update limpio.
- `update = TRUE` regenera en el mismo lugar. La generación registra un
  manifiesto de los archivos que escribió con sus hashes de contenido;
  `update` reescribe solo esos, y se niega a correr si falta el
  manifiesto o si algún archivo generado fue modificado o borrado a
  mano. Lo que bigbang no escribió no se toca nunca. Antes de cambiar un
  proyecto existente, bigbang respalda cada archivo generado y su
  manifiesto. Si el update falla, restaura ese estado para poder
  reintentarlo. Tanto el dry run como el resultado real enumeran las
  rutas eliminadas en `removed_files`. Quitar un componente elimina su
  archivo embarcado, que puede ser la última copia. Cuando el plan
  crece, los archivos que no están ni en el manifiesto ni en el proyecto
  son nuevos y se escriben; los archivos existentes fuera del manifiesto
  se consideran del usuario y el update aborta sin sobrescribirlos. El
  resultado informa las rutas nuevas en `added_files`: incluye agregar o
  volver a agregar un componente, subir su versión o agregar una viñeta
  de workflow. Los updates mantienen una exclusión mutua desde el armado
  hasta el rollback y la publicación del diario. El lock solo se publica
  renombrando una carpeta temporal hermana que ya contiene `owner.rds`,
  por lo que todo lock publicado tiene dueño. Un huérfano se renombra
  primero a un descarte único; el ganador vuelve a verificar ese dueño
  antes de publicar el reemplazo. Un archivo regular u otra entrada del
  usuario en el nombre del lock se aparta como
  `.<nombre>.bigbang-apartado-*`, sin borrar sus bytes. `recover = TRUE`
  resuelve la incertidumbre (Windows, falta de `/proc`, otro host o
  dueño ilegible), pero nunca fuerza a pasar por encima de un dueño
  probado vivo: mismo host, PID vivo y el mismo token de inicio del
  proceso. En ese caso da error e informa el PID. Los nombres hermanos
  `.<nombre>.bigbang-update`, `.<nombre>.bigbang-update.armando-*`,
  `.<nombre>.bigbang-update.lock`,
  `.<nombre>.bigbang-update.lock.armando-*`,
  `.<nombre>.bigbang-update.lock.descartado-*`,
  `.<nombre>.bigbang-update.descartado-*` y
  `.<nombre>.bigbang-apartado-*` están reservados para estas
  operaciones. También se niega a escribir a través de una raíz de
  proyecto simbólica o de enlaces simbólicos dentro del proyecto
  generado, incluidos los enlaces en directorios padre de los archivos
  generados.
- Los updates interrumpidos se arman en una carpeta hermana durable
  `.<nombre>.bigbang-update.armando-*` y se renombran a
  `.<nombre>.bigbang-update` solo después de verificar el marcador y el
  respaldo. Una preparación sin marcador vacía se elimina; cualquier
  preparación sin marcador que no esté vacía se aparta atómicamente como
  `.<nombre>.bigbang-apartado-*`, sin copiar ni borrar bytes. Para
  descartar un diario se escribe primero una lápida atómica con el
  inventario recursivo exacto de rutas relativas y md5 de lo que
  escribió bigbang, se registra un digest junto a la lápida y se lo
  renombra a `.<nombre>.bigbang-update.descartado-*`, de modo que la
  limpieza se reanuda después de otra interrupción. Solo se eliminan
  archivos cuya ruta y md5 coinciden con el inventario, y directorios
  del inventario solo cuando están vacíos. Cualquier otro archivo,
  directorio o enlace simbólico aparta toda la carpeta y el update
  continúa. Una lápida sin digest o con digest cambiado tiene el mismo
  tratamiento. La llamada siguiente con `update = TRUE` puede recuperar
  un proyecto movido junto con su diario. Renombrar un proyecto no está
  soportado porque los nombres de los archivos generados contienen el
  nombre del metapaquete: renombre el proyecto y su diario de vuelta a
  `<nombre>`. Una copia byte a byte puesta en el mismo lugar y con el
  mismo nombre que el original movido es indistinguible del original,
  así que el diario la trata como el proyecto. Si el original todavía
  existe junto al diario copiado, ese diario no se adopta ni se cambia.
  Un descartado de otra generación o proyecto se aparta con un mensaje
  accionable. Un archivo del usuario con la misma ruta y md5 que una
  entrada del inventario es un límite inevitable: los bytes son
  idénticos, de modo que borrarlo no pierde contenido, pero no se puede
  probar la autoría. Registra cada escritura y borrado pretendidos y
  está diseñado para sobrevivir interrupciones del proceso como SIGKILL,
  un error de R o Ctrl-C; no promete durabilidad fsync ante un apagado
  del sistema operativo o de la energía. Si una ruta no contiene ni su
  valor original ni uno pretendido, la recuperación se detiene en vez de
  pisarla. En Windows, una ruta ausente solo se conoce durante la
  ventana en que el temporal pretendido, con el mismo hash, sigue en el
  área de preparación del diario. Cada archivo original restaurado
  estando ausente queda en el resultado y en el mensaje de recuperación.
  Después de confirmar que no sigue corriendo otro update,
  `recover = TRUE` preserva esos bytes desconocidos en un directorio
  hermano informado y recién entonces recupera. Un dry run informa la
  acción pendiente sin cambiar el proyecto, el lock ni ninguna carpeta
  de diario hermana. El lock solo se evalúa e informa como libre, vivo,
  huérfano o incierto. Los fallos al generar documentación en el área de
  preparación son warnings; un fallo al promover una documentación
  aborta y revierte el update completo. En Windows nunca se prueba la
  vida con [`tools::pskill()`](https://rdrr.io/r/tools/pskill.html),
  porque esa llamada terminaría el proceso sondeado.
- `install_upgrade` fija la política de actualización por defecto del
  instalador emitido, así que decidís al generar si los destinatarios
  quedan clavados en las versiones que distribuís (`"always"`) o
  conservan lo más nuevo que ya tengan (`"newer"`, el default).

El instalador generado también acepta `only` para instalar un
subconjunto —las dependencias locales de lo elegido se agregan solas— y
`lib` para elegir la biblioteca donde instala.

## 🧰 API

- [`create_metapackage()`](https://sebollin.github.io/bigbang/reference/create_metapackage.md)
  crea la fuente completa del metapaquete.
- [`install_local_pkg()`](https://sebollin.github.io/bigbang/reference/install_local_pkg.md)
  instala un archivo local y sus dependencias.
- [`diagnose_dependencies()`](https://sebollin.github.io/bigbang/reference/diagnose_dependencies.md)
  busca dependencias implícitas.
- [`scan_bigbang_artifact()`](https://sebollin.github.io/bigbang/reference/scan_bigbang_artifact.md)
  examina artefactos antiguos sin cargarlos.

La API usa inglés snake_case.

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

## 🔭 Proyectos relacionados

- [pegeler/metapackage](https://github.com/pegeler/metapackage), de Paul
  Egeler, es un metapaquete personal declarativo basado en paquetes
  disponibles en repositorios en línea. bigbang, en cambio, genera
  metapaquetes a partir de archivos locales.
- [metaverse](https://rmetaverse.github.io/metaverse/) es un metapaquete
  comunitario para síntesis de evidencia, modelado sobre tidyverse. El
  mensaje de adjunción y el diseño de `<meta>_packages()` en los
  metapaquetes generados por bigbang se inspiran en tidyverse y en
  metaverse (Westgate y colaboradores).

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
Detomasi](https://github.com/Richard-Detomasi), quien propuso construir
una herramienta de metapaquetes y señaló
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
  author = {Sebastián Lucas},
  note   = {R package version 0.3.0},
  year   = {2026},
  url    = {https://sebollin.github.io/bigbang/},
}
```
