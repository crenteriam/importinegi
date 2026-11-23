# importinegi 2.0.0 (en desarrollo)

## Infraestructura

* El paquete requiere ahora R 4.1 o posterior.
* Dependencias: se eliminaron `data.table`, `dplyr` y `haven`, que no se usaban, y `rio`, que `enoe()` usaba solo para leer archivos DBF (ahora los lee con `foreign`, con el mismo resultado).
* `sf` pasa a ser un paquete opcional. Solo lo necesitan `sig_marcogeo()` y `sig_caminos_extrae()`, que avisan si no esta instalado.
* `readxl` es un paquete opcional nuevo: solo lo necesita la base `"resultados"` de `intercensal()`.
* Pruebas automatizadas con `testthat` (tercera edicion).
* Integracion continua con GitHub Actions en Windows, macOS y Linux, en sustitucion de Travis CI.

## Funcionamiento interno

* Nuevas utilidades internas compartidas, todavia sin efecto en las funciones exportadas: descarga con `httr2` (reintentos, limites de tiempo, copia local opcional y verificacion de que el archivo recibido sea un zip valido), descompresion en una carpeta propia por llamada, lectura de DBF y CSV, catalogo unico de entidades federativas y validacion de argumentos.
* Nuevas dependencias: `httr2` y `jsonlite`.
* Registro de direcciones de descarga generado con el script `data-raw/registro.R` a partir del servicio de descargas del INEGI, y tarea de GitHub Actions que lo ejecuta una vez al ano y avisa cuando el INEGI publica o cambia archivos.
* El paquete incluye ya el registro de direcciones de descarga (3,550 archivos de 36 proyectos, actualizado el 22 de noviembre de 2026 con las Encuestas Intercensales 2015 y 2025, con las ocho ediciones del Censo Nacional de Gobiernos Municipales y con las 15 ediciones de la serie tradicional de la ENIGH). Las funciones nuevas buscan ahi la direccion de cada archivo en lugar de armarla a mano.
* La lectura de archivos CSV reconoce la codificacion de cada valor (UTF-8 o Latin-1) en todo el archivo, no solo en sus primeras lineas.
* En las descargas de varios archivos, si uno falla, los que ya se descargaron se conservan durante la sesion de R, para que al repetir la llamada solo se descarguen los que faltan.
* Al combinar varias entidades, cada una se convierte a numero antes de unirlas, lo que reduce la memoria que se necesita.
* Al descomprimir un archivo, se extrae solo la tabla que se pidio.
* El registro puede incluir los archivos de datos abiertos del INEGI, ademas de los microdatos: `data-raw/proyectos.csv` indica, por proyecto, los tipos de documento del servicio de descargas que se consultan.
* Nuevo motor compartido para los censos nacionales de gobierno, que reconoce las distintas formas de sus archivos: carpetas con otros nombres o dentro de otra carpeta, y zip que guardan sus rutas con `\` como separador (45 archivos del censo municipal), que en Linux y macOS se extraian sin carpetas.
* La lectura de archivos DBF entrega el texto en UTF-8 (los DBF del INEGI usan paginas de codigos anteriores), para que los nombres con acentos se lean bien en cualquier sistema operativo.
* Las descargas piden el contenido sin comprimir y no usan la decodificacion automatica de curl: el catalogo de la Red Nacional de Metadatos responde con una cabecera `Content-Encoding` que no es valida y curl rechazaba la respuesta.
* La lectura de archivos DBF omite los registros marcados como borrados, que `foreign::read.dbf()` devuelve como si fueran datos. Los hay en archivos de la ENIGH (por ejemplo, dos en los ingresos no monetarios de 1989 y dos en las erogaciones de 2006), y la documentacion del INEGI no los cuenta. El cambio aplica a todas las funciones que leen DBF con las utilidades nuevas.
* Nuevo motor compartido para las series de la ENIGH, que ubica cada tabla en el archivo del INEGI que la contiene: en la serie tradicional, 126 tablas en 106 archivos, con nombres de DBF que cambian de una edicion a otra. Cada archivo se descarga una sola vez aunque se pidan varias de sus tablas.

## Funciones

* Nueva funcion `censo()`, que reune en una sola funcion todas las bases de datos de los Censos de Poblacion y Vivienda.
* **Cambio incompatible:** se eliminaron `censo_poblacion_iter()`, `censo_poblacion_ageb()`, `censo_poblacion_rural()`, `censo_poblacion_urbano()` y `censo_poblacion_muestra()`. `censo()` las sustituye:
    * `censo_poblacion_iter(2010, "Jalisco")` pasa a ser `censo(2010, estado = "Jalisco")`.
    * `censo_poblacion_iter(2010, "Jalisco", totalestado = TRUE, totalmunicipio = TRUE, localidades = FALSE)` pasa a ser `censo(2010, estado = "Jalisco", nivel = c("entidad", "municipio"))`.
    * `censo_poblacion_ageb(2010, "Jalisco")` pasa a ser `censo(2010, "ageb", estado = "Jalisco")`.
    * `censo_poblacion_rural(2010, "Jalisco")` pasa a ser `censo(2010, "localidades", estado = "Jalisco")`.
    * `censo_poblacion_urbano(2010, "Jalisco")` pasa a ser `censo(2010, "entorno", estado = "Jalisco")`.
    * `censo_poblacion_muestra(2010, "Jalisco", "Personas")` pasa a ser `censo(2010, "muestra", estado = "Jalisco", tabla = "personas")`.
    * Los Conteos de Poblacion y Vivienda de 1995 y 2005, que se descargaban con `censo_poblacion_iter()` y `censo_poblacion_muestra()`, se descargan ahora con `conteo()`: `censo_poblacion_iter(2005, "Jalisco")` pasa a ser `conteo(2005, estado = "Jalisco")` y `censo_poblacion_muestra(2005, "Jalisco", "Hogar")` pasa a ser `conteo(2005, "muestra", estado = "Jalisco", tabla = "hogares")`.
* `censo()`:
    * `censo(year, base, estado, tabla, nivel)`: `year` es obligatorio; `base` elige entre `"iter"`, `"ageb"`, `"muestra"`, `"localidades"`, `"entorno"` y `"asistencia"` (alojamientos de asistencia social, Censo 2020).
    * El argumento `tabla` elige la tabla en las bases que se publican en varias: `"muestra"` (personas, viviendas, migrantes), `"entorno"` de 2020 (manzanas, vialidades) y `"asistencia"` (alojamientos, usuarios, trabajadores).
    * Censos de 1990, 2000, 2010 y 2020. El Censo 2020 es nuevo en el paquete; se lee de los archivos CSV del INEGI.
    * `estado` acepta nombre, abreviatura o clave, sin distinguir mayusculas ni acentos, y varias entidades a la vez, que se combinan en una sola tabla. `"nacional"` devuelve todo el pais: el archivo nacional o, si el INEGI publica la base solo por entidad, las 32 entidades combinadas. Si no se indica `estado`, la muestra y las bases que se publican solo por entidad descargan Aguascalientes, con un aviso.
    * `nivel` sustituye a los argumentos `totalestado`, `totalmunicipio`, `localidades`, `totallocalidad`, `totalageb` y `manzana`.
    * Las direcciones salen del registro de archivos del INEGI. Con las funciones anteriores, la direccion del ITER 2010 se armaba mal para 6 entidades.
    * Las claves geograficas se devuelven como texto con ceros a la izquierda y los indicadores, como numero (antes eran texto). En la muestra, las respuestas codificadas, la edad y el factor de expansion tambien son numericos.
    * Los agregados de localidades de una y dos viviendas del ITER (claves 9998 y 9999) ya no se devuelven mezclados con las localidades.
    * La muestra funciona en cualquier sistema operativo (antes solo en Windows).
    * Mensajes de error que indican que años, bases y tablas existen.
* Nueva funcion `conteo()`, con las bases de datos de los Conteos de Poblacion y Vivienda de 1995 y 2005: ITER, muestra y, en 2005, viviendas habitadas y deshabitadas por localidad. Tiene los mismos argumentos que `censo()`; en la muestra, `tabla` elige entre personas y migrantes (1995) o entre personas, viviendas y hogares (2005).
* Nueva funcion `intercensal()`, que descarga las Encuestas Intercensales de 2015 y 2025 con los mismos argumentos que `censo()` y `conteo()`: la muestra (personas, viviendas y, en 2025, migrantes) y, en 2025, los principales resultados por localidad de 50 000 y mas habitantes.
* Nueva funcion `bases_importinegi()`, que muestra las bases de datos que se pueden descargar con el paquete, por proyecto y año: tablas, cobertura (nacional o por entidad), formato y tamaño de la descarga. No descarga nada ni requiere conexion a internet. Usa el mismo registro de archivos que las funciones de descarga, de modo que no puede anunciar bases o años que no existan. Por ahora incluye el Censo y el Conteo de Poblacion y Vivienda, la Encuesta Intercensal, el Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico y la serie tradicional de la ENIGH; los demas proyectos se agregaran conforme se actualicen sus funciones.
* Nueva funcion `cngmd()`, que descarga el Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico (CNGMD) de 2011 a 2025, por modulo y tema. Es la primera de las tres funciones de los censos nacionales de gobierno (municipal, estatal y federal).
* **Cambio incompatible:** se elimino `censo_municipal()`. `cngmd()` la sustituye: el argumento `fuente` pasa a ser `modulo` y el argumento `datos` pasa a ser `tema`.
    * `censo_municipal(2011, "ayuntamiento", "estructura")` pasa a ser `cngmd(2011, "ayuntamiento", "estructura")`.
    * `censo_municipal(year = 2011, fuente = "ayuntamiento", datos = "comision")` pasa a ser `cngmd(year = 2011, modulo = "ayuntamiento", tema = "comision")`.
    * Los valores de `datos` siguen sirviendo como `tema` en las ediciones donde el tema existe, salvo `"recursos"` en el modulo `"administracion"` de 2017, que coincide con varios temas.
    * `censo_municipal()` sin argumentos abria la pagina del censo en el navegador. Los temas de cada edicion se consultan ahora con `bases_importinegi("cngmd")`.
* `cngmd()`:
    * `cngmd(year, modulo, tema, tabla, estado)`: `year` es obligatorio; `modulo` elige entre `"ayuntamiento"`, `"administracion"`, `"catastro"`, `"territorio"`, `"seguridad"`, `"proteccion_civil"`, `"justicia"`, `"agua"` y `"residuos"`; `tema` acepta una parte del titulo que el INEGI le da al tema o del nombre de su archivo.
    * Ediciones de 2011 a 2025. Las de 2019, 2021, 2023 y 2025 son nuevas en el paquete, igual que los modulos de catastro, planeacion y gestion territorial, proteccion civil, agua potable y saneamiento, y residuos solidos urbanos.
    * Devuelve una lista con todas las tablas del tema, como antes, o una sola tabla si se indica `tabla`. `tabla` tambien da acceso al indice de las tablas (`"indice"`), a los catalogos de las variables codificadas (`"catalogos"`) y al diccionario de datos (`"diccionario"`).
    * El argumento `estado` conserva las filas de una o varias entidades.
    * Lee los archivos de datos abiertos del INEGI, en formato CSV (antes, los archivos DBF). Las tablas de la lista se llaman como su archivo, en minusculas (`act_ayun`), y ya no llevan el prefijo `dt.`; las columnas conservan los nombres de esos archivos.
    * La clave del municipio conserva el nombre que le da el INEGI en cada edicion y se devuelve como texto de 5 digitos.
    * La sigla `"NA"` (no aplica) se convierte en `NA`. Las siglas `"NSS"`, `"ND"` y `"NP"` se conservan como las publica el INEGI.
    * Los DBF de agua potable y de residuos solidos de 2011 declaran mas registros de los que contienen; la funcion devuelve solo los registros reales, sin filas vacias.
    * Las direcciones salen del registro de archivos del INEGI. Las que armaba `censo_municipal()` habian dejado de funcionar.
    * Funciona en cualquier sistema operativo (sin argumentos, `censo_municipal()` solo funcionaba en Windows).
* Nueva funcion `enigh_tradicional()`, que descarga la serie tradicional de la Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH), de 1984 a 2014, por tabla. Es la primera de las tres funciones de la ENIGH (serie tradicional, Nueva construccion y Nueva serie).
* `enigh_tradicional()`:
    * `enigh_tradicional(year, tabla)`: `year` es obligatorio; `tabla` elige una o varias tablas de la edicion y, por omision, descarga `"concentrado"`, la tabla con una fila por hogar, su factor de expansion y los agregados de ingreso y de gasto.
    * Las 15 ediciones de la serie: 1984, 1989, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2005, 2006, 2008, 2010, 2012 y 2014. Ninguna estaba en el paquete.
    * Tablas: `"concentrado"`, `"hogares"`, `"poblacion"` e `"ingresos"` en todas las ediciones; `"gastos"` y `"nomonetario"` hasta 2010; `"erogaciones"` desde 1994; `"trabajos"`, `"gastotarjetas"`, `"agro"` y `"noagro"` desde 2008; `"gastodiario"` y `"gastoeducacion"` en 2008 y 2010; `"gastorecibo"` y `"gastocosto"` en 2010; `"viviendas"`, `"gastohogar"` y `"gastopersona"` en 2012 y 2014. `bases_importinegi("enigh_tradicional")` lista las de cada edicion.
    * Con una tabla devuelve un data.frame; con varias, una lista con un data.frame por tabla.
    * Los nombres de las columnas se devuelven en mayusculas en todas las tablas. El INEGI los publica en minusculas en 36 de las 126 tablas, a veces dentro de la misma edicion, y asi las tablas no se podian unir por sus folios.
    * Las columnas conservan el tipo que les da el INEGI: los folios, las claves y los codigos, como texto con sus ceros a la izquierda; las cantidades, como numero. Los datos en blanco o llenos de asteriscos se devuelven como `NA`.
    * Lee los archivos DBF, el unico formato que el INEGI publica en las 15 ediciones. Todos los archivos son nacionales, por lo que la funcion no tiene el argumento `estado`.
    * El atributo `"descriptor"` del resultado contiene la direccion del descriptor de archivos de la edicion, el documento del INEGI que describe las tablas y sus variables.
    * Mensajes de error que indican que años y que tablas existen, y en que ediciones existe una tabla.
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
* Ayuda de `censo()` y seccion del Censo de Poblacion y Vivienda de la viñeta, reescrita para `censo()`: bases disponibles por año, entidades, niveles de agregacion y tablas. La documentacion ya no menciona las funciones `censo_poblacion_*()`.
* Ayuda de `bases_importinegi()` y nueva seccion de la viñeta, "Bases de datos disponibles".
* Ayuda de `conteo()` y nueva seccion de la viñeta, "Conteo de Poblacion y Vivienda", que explica como se levantaron los conteos de 1995 y 2005.
* Ayuda de `intercensal()` y nueva seccion de la viñeta, "Encuesta Intercensal", que explica como se levantaron las encuestas de 2015 y 2025.
* Ayuda de `cngmd()` y seccion de la viñeta sobre el Censo Nacional de Gobiernos Municipales, reescrita para `cngmd()`: modulos por edicion, temas, tablas, catalogos y siglas del INEGI. La documentacion ya no menciona `censo_municipal()`.
* Ayuda de `enigh_tradicional()` y nueva subseccion de la viñeta, "Serie tradicional": tablas por edicion, como unirlas con el identificador del hogar, donde esta el factor de expansion y como se devuelven las columnas. La seccion de la ENIGH de la viñeta presenta ahora las tres series de la encuesta.
* La pagina de ayuda del paquete (`?importinegi`) incluye el indice de funciones por proyecto estadistico.
* La ayuda de `sig_marcogeo()` y `sig_caminos_extrae()` indica que requieren el paquete `sf`.
* Viñeta: nueva seccion sobre el catalogo de proyectos estadisticos y requisitos de la version; se corrigieron referencias que ya no eran vigentes (el paquete `rgdal` y dos nombres de funciones).
* README: instalacion de la version estable y de la version en desarrollo, y ejemplo de uso. Las notas de las versiones anteriores pasan a este archivo.
* README: lista de los proyectos estadisticos cuyas funciones ya se actualizaron en la version 2.0.0, con sus años.

# importinegi 1.2.1

* Se actualizó la nueva URL del Repositorio Nacional de Metadatos. Se reemplazo el paquete rgdal por sf.

# importinegi 1.2.0

* Se arreglan problemas con las funciones catalogo_inegi() y enoe(). Cortesía de Villiem (https://twitter.com/villicanaem).

# importinegi 1.1.3

* INEGI terminó de trasladar algunos repositorios de su versión beta a su versión estable. Esto afectó los URL de las funciones para el Censo y la ENOE. La versión beta `1.1.3` arregla este problema.

# Version 1.0.0

Primera version publicada. Sin noticias.
