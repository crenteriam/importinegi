# importinegi 2.0.0 (en desarrollo)

## Infraestructura

* El paquete requiere ahora R 4.1 o posterior.
* Dependencias: se eliminaron `data.table`, `dplyr` y `haven`, que no se usaban, y `rio`, que `enoe()` usaba solo para leer archivos DBF (ahora los lee con `foreign`, con el mismo resultado).
* `sf` pasa a ser un paquete opcional. Solo lo necesitan `sig_marcogeo()` y `sig_caminos_extrae()`, que avisan si no esta instalado.
* Pruebas automatizadas con `testthat` (tercera edicion).
* Integracion continua con GitHub Actions en Windows, macOS y Linux, en sustitucion de Travis CI.

## Funcionamiento interno

* Nuevas utilidades internas compartidas, todavia sin efecto en las funciones exportadas: descarga con `httr2` (reintentos, limites de tiempo, copia local opcional y verificacion de que el archivo recibido sea un zip valido), descompresion en una carpeta propia por llamada, lectura de DBF y CSV, catalogo unico de entidades federativas y validacion de argumentos.
* Nuevas dependencias: `httr2` y `jsonlite`.
* Registro de direcciones de descarga generado con el script `data-raw/registro.R` a partir del servicio de descargas del INEGI, y tarea de GitHub Actions que lo ejecuta una vez al ano y avisa cuando el INEGI publica o cambia archivos.
* Las descargas piden el contenido sin comprimir y no usan la decodificacion automatica de curl: el catalogo de la Red Nacional de Metadatos responde con una cabecera `Content-Encoding` que no es valida y curl rechazaba la respuesta.

## Funciones

* `catalogo_inegi()`:
    * Devuelve el catalogo completo de la Red Nacional de Metadatos. Antes solo devolvia los 500 proyectos mas recientes (de 825 en noviembre de 2026).
    * Nuevos argumentos `buscar` (texto en el nombre o la clave del proyecto, sin distinguir mayusculas ni acentos) y `year` (proyectos cuyo periodo incluye ese año).
    * El catalogo incluye la columna `url`, con la direccion de la ficha de cada proyecto.
    * Las columnas `created` y `changed` son ahora fechas (antes eran texto, como `Aug-20-25`).
    * Con `id` funciona en cualquier sistema operativo (antes solo en Windows) y devuelve la direccion de la ficha.
    * El catalogo se descarga una sola vez por sesion de R.
    * Avisa con un mensaje claro cuando el INEGI no devuelve el catalogo, en lugar de devolver una tabla vacia.

## Documentacion

* Ayuda de `catalogo_inegi()`: describe las columnas del catalogo y agrega ejemplos de busqueda.
* La pagina de ayuda del paquete (`?importinegi`) incluye el indice de funciones por proyecto estadistico.
* La ayuda de `sig_marcogeo()` y `sig_caminos_extrae()` indica que requieren el paquete `sf`.
* Viñeta: nueva seccion sobre el catalogo de proyectos estadisticos y requisitos de la version; se corrigieron referencias que ya no eran vigentes (el paquete `rgdal` y dos nombres de funciones).
* README: instalacion de la version estable y de la version en desarrollo, y ejemplo de uso. Las notas de las versiones anteriores pasan a este archivo.

# importinegi 1.2.1

* Se actualizó la nueva URL del Repositorio Nacional de Metadatos. Se reemplazo el paquete rgdal por sf.

# importinegi 1.2.0

* Se arreglan problemas con las funciones catalogo_inegi() y enoe(). Cortesía de Villiem (https://twitter.com/villicanaem).

# importinegi 1.1.3

* INEGI terminó de trasladar algunos repositorios de su versión beta a su versión estable. Esto afectó los URL de las funciones para el Censo y la ENOE. La versión beta `1.1.3` arregla este problema.

# Version 1.0.0

Primera version publicada. Sin noticias.
