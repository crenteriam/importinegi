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

## Funciones

* `catalogo_inegi()`:
    * Devuelve el catalogo completo de la Red Nacional de Metadatos. Antes solo devolvia los 500 proyectos mas recientes (de 825 en noviembre de 2026).
    * Nuevos argumentos `buscar` (texto en el nombre o la clave del proyecto, sin distinguir mayusculas ni acentos) y `year` (proyectos cuyo periodo incluye ese año).
    * El catalogo incluye la columna `url`, con la direccion de la ficha de cada proyecto.
    * Con `id` funciona en cualquier sistema operativo (antes solo en Windows) y devuelve la direccion de la ficha.
    * El catalogo se descarga una sola vez por sesion de R.
    * Avisa con un mensaje claro cuando el INEGI no devuelve el catalogo, en lugar de devolver una tabla vacia.

# Version 1.0.0

Primera version publicada. Sin noticias.
