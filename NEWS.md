# importinegi 2.0.0 (en desarrollo)

## Infraestructura

* El paquete requiere ahora R 4.1 o posterior.
* Dependencias: se eliminaron `data.table`, `dplyr` y `haven`, que no se usaban, y `rio`, que `enoe()` usaba solo para leer archivos DBF (ahora los lee con `foreign`, con el mismo resultado).
* `sf` pasa a ser un paquete opcional. Solo lo necesitan `sig_marcogeoestadistico()` y `sig_caminos_extrae()`, que avisan si no esta instalado.
* `readxl` es un paquete opcional nuevo: solo lo necesita la base `"resultados"` de `cpv_intercensal()`.
* Pruebas automatizadas con `testthat` (tercera edicion).
* Integracion continua con GitHub Actions en Windows, macOS y Linux, en sustitucion de Travis CI.

## Funcionamiento interno

* Nuevas utilidades internas compartidas, todavia sin efecto en las funciones exportadas: descarga con `httr2` (reintentos, limites de tiempo, copia local opcional y verificacion de que el archivo recibido sea un zip valido), descompresion en una carpeta propia por llamada, lectura de DBF y CSV, catalogo unico de entidades federativas y validacion de argumentos.
* Nuevas dependencias: `httr2` y `jsonlite`.
* Registro de direcciones de descarga generado con el script `data-raw/registro.R` a partir del servicio de descargas del INEGI, y tarea de GitHub Actions que lo ejecuta una vez al ano y avisa cuando el INEGI publica o cambia archivos.
* El paquete incluye ya el registro de direcciones de descarga (3,738 archivos de 46 proyectos, actualizado el 27 de noviembre de 2026 con las Encuestas Intercensales 2015 y 2025, con las ocho ediciones del Censo Nacional de Gobiernos Municipales, con las 15 ediciones de la serie tradicional de la ENIGH, con las nueve ediciones del Censo Nacional de Derechos Humanos Federal, con la Encuesta Telefonica de Ocupacion y Empleo y con el tercer trimestre de 2026 de la ENOE). Las funciones nuevas buscan ahi la direccion de cada archivo en lugar de armarla a mano.
* La lectura de archivos CSV reconoce la codificacion de cada valor (UTF-8 o Latin-1) en todo el archivo, no solo en sus primeras lineas.
* En las descargas de varios archivos, si uno falla, los que ya se descargaron se conservan durante la sesion de R, para que al repetir la llamada solo se descarguen los que faltan.
* Al combinar varias entidades, cada una se convierte a numero antes de unirlas, lo que reduce la memoria que se necesita.
* Al descomprimir un archivo, se extrae solo la tabla que se pidio.
* El registro puede incluir los archivos de datos abiertos del INEGI, ademas de los microdatos: `data-raw/proyectos.csv` indica, por proyecto, los tipos de documento del servicio de descargas que se consultan.
* Nuevo motor compartido para los censos nacionales de gobierno, que reconoce las distintas formas de sus archivos: carpetas con otros nombres o dentro de otra carpeta, y zip que guardan sus rutas con `\` como separador (45 archivos del censo municipal), que en Linux y macOS se extraian sin carpetas.
* La lectura de archivos DBF entrega el texto en UTF-8 (los DBF del INEGI usan paginas de codigos anteriores), para que los nombres con acentos se lean bien en cualquier sistema operativo.
* Las descargas piden el contenido sin comprimir y no usan la decodificacion automatica de curl: el catalogo de la Red Nacional de Metadatos responde con una cabecera `Content-Encoding` que no es valida y curl rechazaba la respuesta.
* La lectura de archivos DBF omite los registros marcados como borrados, que `foreign::read.dbf()` devuelve como si fueran datos. Los hay en archivos de la ENIGH (por ejemplo, dos en los ingresos no monetarios de 1989 y dos en las erogaciones de 2006), y la documentacion del INEGI no los cuenta. El cambio aplica a todas las funciones que leen DBF con las utilidades nuevas.
* La lectura de archivos CSV ya no muestra una advertencia cuando el archivo no termina con un salto de linea, como ocurre en tablas de una sola fila del Censo Nacional de Derechos Humanos Federal.
* Los censos nacionales de gobierno leen cada tabla directamente del zip y la guardan con un nombre propio, en lugar de descomprimirla con el nombre que trae el zip. Asi se leen tambien los zip cuyos nombres no estan en UTF-8 (tres temas del Censo Nacional de Derechos Humanos Federal de 2017 los guardan en la pagina de codigos de DOS), que macOS no admite como nombres de archivo, y la lectura deja de depender de la configuracion regional de R. Si la tabla misma tiene un nombre asi, cada caracter que no es ASCII queda como `_`.
* En todas las funciones del paquete, el texto `"NULL"` que un archivo del INEGI traiga en lugar de un dato se devuelve como `NA`, este escrito con mayusculas o con minusculas (`"NULL"`, `"Null"`, `"null"`). Los diccionarios de datos de los censos de gobierno lo listan entre los valores validos de las columnas, sin definirlo.
* Nuevo motor compartido para las series de la ENIGH, que ubica cada tabla en el archivo del INEGI que la contiene: en la serie tradicional, 126 tablas en 106 archivos, con nombres de DBF que cambian de una edicion a otra. Cada archivo se descarga una sola vez aunque se pidan varias de sus tablas.
* El motor de la ENIGH atiende tambien la Nueva construccion (52 tablas, cada una en su archivo) y puede leer una tabla del archivo CSV del INEGI, con los tipos de sus columnas fijados en el paquete, cuando el archivo DBF no contiene la tabla que anuncia. Por ahora es un solo caso: los negocios no agropecuarios de 2010 de la Nueva construccion.
* El registro admite proyectos cuya pagina no esta en la seccion de programas del sitio del INEGI: en `data-raw/proyectos.csv`, esa pagina se escribe con su ruta completa, empezando con `/`. Es el caso de la Encuesta Telefonica de Ocupacion y Empleo (`/investigacion/etoe`).
* La lectura y la integracion de las tablas de `enoe()` pasan a una funcion interna que tambien usa `etoe()`. El resultado de `enoe()` no cambia.
* Nuevas utilidades internas para leer el indice de un archivo zip, local o remoto, sin depender de `utils::unzip()`. Con el indice se conoce el contenido de un zip aunque los nombres de sus archivos no esten en UTF-8, se avisa con claridad cuando un archivo usa un metodo de compresion que R no abre (Deflate64) y se puede descargar una sola parte de un zip remoto con solicitudes parciales (cabecera `Range`).
* El paquete incluye la lista de productos del Marco Geoestadistico (26 productos) y el inventario de su descarga por capa (160 capas de 2017 a 2026, con las entidades que no tienen archivo). Se generan con el script `data-raw/marcogeo.R` a partir del servicio de productos del INEGI y del sondeo de las 5,280 direcciones de la descarga por capa. La tarea de GitHub Actions que regenera el registro ejecuta tambien este script.

## Funciones

* Nueva funcion `cpv_censo()`, que reune en una sola funcion todas las bases de datos de los Censos de Poblacion y Vivienda.
* **Cambio incompatible:** se eliminaron `censo_poblacion_iter()`, `censo_poblacion_ageb()`, `censo_poblacion_rural()`, `censo_poblacion_urbano()` y `censo_poblacion_muestra()`. `cpv_censo()` las sustituye:
    * `censo_poblacion_iter(2010, "Jalisco")` pasa a ser `cpv_censo(2010, estado = "Jalisco")`.
    * `censo_poblacion_iter(2010, "Jalisco", totalestado = TRUE, totalmunicipio = TRUE, localidades = FALSE)` pasa a ser `cpv_censo(2010, estado = "Jalisco", nivel = c("entidad", "municipio"))`.
    * `censo_poblacion_ageb(2010, "Jalisco")` pasa a ser `cpv_censo(2010, "ageb", estado = "Jalisco")`.
    * `censo_poblacion_rural(2010, "Jalisco")` pasa a ser `cpv_censo(2010, "localidades", estado = "Jalisco")`.
    * `censo_poblacion_urbano(2010, "Jalisco")` pasa a ser `cpv_censo(2010, "entorno", estado = "Jalisco")`.
    * `censo_poblacion_muestra(2010, "Jalisco", "Personas")` pasa a ser `cpv_censo(2010, "muestra", estado = "Jalisco", tabla = "personas")`.
    * Los Conteos de Poblacion y Vivienda de 1995 y 2005, que se descargaban con `censo_poblacion_iter()` y `censo_poblacion_muestra()`, se descargan ahora con `cpv_conteo()`: `censo_poblacion_iter(2005, "Jalisco")` pasa a ser `cpv_conteo(2005, estado = "Jalisco")` y `censo_poblacion_muestra(2005, "Jalisco", "Hogar")` pasa a ser `cpv_conteo(2005, "muestra", estado = "Jalisco", tabla = "hogares")`.
* `cpv_censo()`:
    * `cpv_censo(year, base, estado, tabla, nivel)`: `year` es obligatorio; `base` elige entre `"iter"`, `"ageb"`, `"muestra"`, `"localidades"`, `"entorno"` y `"asistencia"` (alojamientos de asistencia social, Censo 2020).
    * El argumento `tabla` elige la tabla en las bases que se publican en varias: `"muestra"` (personas, viviendas, migrantes), `"entorno"` de 2020 (manzanas, vialidades) y `"asistencia"` (alojamientos, usuarios, trabajadores).
    * Censos de 1990, 2000, 2010 y 2020. El Censo 2020 es nuevo en el paquete; se lee de los archivos CSV del INEGI.
    * `estado` acepta nombre, abreviatura o clave, sin distinguir mayusculas ni acentos, y varias entidades a la vez, que se combinan en una sola tabla. `"nacional"` devuelve todo el pais: el archivo nacional o, si el INEGI publica la base solo por entidad, las 32 entidades combinadas. Si no se indica `estado`, la muestra y las bases que se publican solo por entidad descargan Aguascalientes, con un aviso.
    * `nivel` sustituye a los argumentos `totalestado`, `totalmunicipio`, `localidades`, `totallocalidad`, `totalageb` y `manzana`.
    * Las direcciones salen del registro de archivos del INEGI. Con las funciones anteriores, la direccion del ITER 2010 se armaba mal para 6 entidades.
    * Las claves geograficas se devuelven como texto con ceros a la izquierda y los indicadores, como numero (antes eran texto). En la muestra, las respuestas codificadas, la edad y el factor de expansion tambien son numericos.
    * Los agregados de localidades de una y dos viviendas del ITER (claves 9998 y 9999) ya no se devuelven mezclados con las localidades.
    * La muestra funciona en cualquier sistema operativo (antes solo en Windows).
    * Mensajes de error que indican que años, bases y tablas existen.
* Nueva funcion `cpv_conteo()`, con las bases de datos de los Conteos de Poblacion y Vivienda de 1995 y 2005: ITER, muestra y, en 2005, viviendas habitadas y deshabitadas por localidad. Tiene los mismos argumentos que `cpv_censo()`; en la muestra, `tabla` elige entre personas y migrantes (1995) o entre personas, viviendas y hogares (2005).
* Nueva funcion `cpv_intercensal()`, que descarga las Encuestas Intercensales de 2015 y 2025 con los mismos argumentos que `cpv_censo()` y `cpv_conteo()`: la muestra (personas, viviendas y, en 2025, migrantes) y, en 2025, los principales resultados por localidad de 50 000 y mas habitantes.
* Nueva funcion `bases_importinegi()`, que muestra las bases de datos que se pueden descargar con el paquete, por proyecto y año: tablas, cobertura (nacional o por entidad), formato y tamaño de la descarga. No descarga nada ni requiere conexion a internet. Usa el mismo registro de archivos que las funciones de descarga, de modo que no puede anunciar bases o años que no existan. Por ahora incluye el Censo y el Conteo de Poblacion y Vivienda, la Encuesta Intercensal, el Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico, el Censo Nacional de Derechos Humanos Federal, las series tradicional y Nueva construccion de la ENIGH la Encuesta Nacional de Ocupacion y Empleo (una fila por trimestre, con su edicion y su cuestionario) y la Encuesta Telefonica de Ocupacion y Empleo (una fila por mes, con las versiones de sus ponderadores); los demas proyectos se agregaran conforme se actualicen sus funciones.
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
* Nueva funcion `cndhf()`, que descarga el Censo Nacional de Derechos Humanos Federal (CNDHF) de 2017 a 2025, por modulo y tema. Se usa igual que `cngmd()` y comparte su programacion.
    * `cndhf(year, modulo, tema, tabla)`: `year` y `modulo` son obligatorios; `modulo` elige entre `"estructura"` (estructura organizacional y recursos de la Comision Nacional de los Derechos Humanos) y `"proteccion"` (capacitacion, difusion, defensa y proteccion de los derechos humanos); `tema` y `tabla` funcionan como en `cngmd()`.
    * No tiene el argumento `estado`: el censo tiene un solo informante, la Comision Nacional de los Derechos Humanos.
    * Lee los archivos de datos abiertos del INEGI, en formato CSV. La sigla `"NA"` (no aplica) se convierte en `NA`; las demas siglas se conservan.
    * En las tablas que desglosan por municipio, la clave del municipio en una sola columna (`ubicageo_b`, `ubicageo_c`, `ubicageo_e`; ediciones de 2020 a 2022) se devuelve como texto de 5 digitos. La clave de la entidad (`enticve1`, desde la edicion 2022) y la del municipio (`municve1`, desde la edicion 2023) se devuelven como texto de 2 y de 3 digitos. No se crean columnas.
    * El indice de las tablas y el diccionario de datos estan disponibles como tablas desde la edicion 2020.
* Las bitacoras de cambios que algunos temas de los censos de gobierno traen junto a sus tablas (`..._bitacora_de_cambios.csv`) ya no se leen como tablas.
* Nueva funcion `enigh_tradicional()`, que descarga la serie tradicional de la Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH), de 1984 a 2014, por tabla. Es la primera de las tres funciones de la ENIGH (serie tradicional, Nueva construccion y Nueva serie).
* `enigh_tradicional()`:
    * `enigh_tradicional(year, tabla)`: `year` es obligatorio; `tabla` elige una o varias tablas de la edicion y, por omision, descarga `"concentrado"`, la tabla con una fila por hogar, su factor de expansion y los agregados de ingreso y de gasto. Si no se indica `tabla`, un mensaje avisa que tabla se descarga y lista las demas tablas de la edicion.
    * Las 15 ediciones de la serie: 1984, 1989, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2005, 2006, 2008, 2010, 2012 y 2014. Ninguna estaba en el paquete.
    * Tablas: `"concentrado"`, `"hogares"`, `"poblacion"` e `"ingresos"` en todas las ediciones; `"gastos"` y `"nomonetario"` hasta 2010; `"erogaciones"` desde 1994; `"trabajos"`, `"gastotarjetas"`, `"agro"` y `"noagro"` desde 2008; `"gastodiario"` y `"gastoeducacion"` en 2008 y 2010; `"gastorecibo"` y `"gastocosto"` en 2010; `"viviendas"`, `"gastohogar"` y `"gastopersona"` en 2012 y 2014. `bases_importinegi("enigh_tradicional")` lista las de cada edicion.
    * Con una tabla devuelve un data.frame; con varias, una lista con un data.frame por tabla.
    * Los nombres de las columnas se devuelven en mayusculas en todas las tablas. El INEGI los publica en minusculas en 36 de las 126 tablas, a veces dentro de la misma edicion, y asi las tablas no se podian unir por sus folios.
    * Las columnas conservan el tipo que les da el INEGI: los folios, las claves y los codigos, como texto con sus ceros a la izquierda; las cantidades, como numero. Los datos en blanco o llenos de asteriscos se devuelven como `NA`.
    * Lee los archivos DBF, el unico formato que el INEGI publica en las 15 ediciones. Todos los archivos son nacionales, por lo que la funcion no tiene el argumento `estado`.
    * El atributo `"descriptor"` del resultado contiene la direccion del descriptor de archivos de la edicion, el documento del INEGI que describe las tablas y sus variables.
    * Mensajes de error que indican que años y que tablas existen, y en que ediciones existe una tabla. Si la informacion esta en otra tabla de la edicion, el mensaje lo dice: las caracteristicas de la vivienda estan en `"hogares"` hasta 2010, y los gastos, en `"gastos"` hasta 2010 y en `"gastohogar"` y `"gastopersona"` desde 2012.
* Nueva funcion `enigh_nc()`, que descarga la Nueva construccion de la ENIGH, de 2008 a 2014, por tabla. Se usa igual que `enigh_tradicional()` y comparte su programacion.
* **Cambio incompatible:** se elimino `enigh_nuevaconstruccion()`. `enigh_nc()` la sustituye: el argumento `datos` pasa a ser `tabla`.
    * `enigh_nuevaconstruccion(2014, "hogares")` pasa a ser `enigh_nc(2014, "hogares")`.
    * `enigh_nuevaconstruccion(year = 2014, datos = "agro")` pasa a ser `enigh_nc(year = 2014, tabla = "agro")`.
    * Los nombres de las columnas se devuelven ahora en mayusculas (antes, en minusculas): `folioviv` pasa a ser `FOLIOVIV`.
    * `datos = "viviendas"` en 2008 y 2010 devolvia la tabla de hogares, porque esas ediciones no tienen tabla de viviendas. Ahora es un error que remite a `"hogares"`.
    * `datos = "gastohogar"` en 2008 y 2010 devolvia la tabla de gastos. Ahora se pide con `tabla = "gastos"`.
* `enigh_nc()`:
    * `enigh_nc(year, tabla)`: `year` es obligatorio; `tabla` elige una o varias tablas de la edicion y, por omision, descarga `"concentrado"`. Si no se indica `tabla`, un mensaje avisa que tabla se descarga y lista las demas tablas de la edicion.
    * Las cuatro ediciones de la serie: 2008, 2010, 2012 y 2014.
    * Tablas: las doce que ofrecia `enigh_nuevaconstruccion()` y seis que no estaban en el paquete: `"gastos"`, `"gastodiario"`, `"gastoeducacion"` y `"nomonetario"` en 2008 y 2010, y `"gastorecibo"` y `"gastocosto"` en 2010. Se llaman igual que en `enigh_tradicional()`. `bases_importinegi("enigh_nc")` lista las de cada edicion.
    * Con una tabla devuelve un data.frame; con varias, una lista con un data.frame por tabla.
    * La tabla `"noagro"` de 2010 contiene ahora los negocios no agropecuarios (7,600 filas y 97 columnas). `enigh_nuevaconstruccion(2010, "noagro")` devolvia, sin aviso, los negocios agropecuarios: el archivo DBF que el INEGI publica con el nombre de esa tabla contiene una copia de la tabla `"agro"`. La funcion lee esa tabla del archivo CSV del INEGI y le da a cada columna el tipo que el INEGI declara para ella.
    * Las columnas conservan el tipo que les da el INEGI en cada edicion: texto con sus ceros a la izquierda o numero. Los datos en blanco o llenos de asteriscos se devuelven como `NA`.
    * Cuatro nombres de columna llegan recortados a 10 caracteres, el limite del formato DBF, igual que en `enigh_tradicional()`: `ANIO_PICKU` y `ANIO_CARRE` en hogares, y `COMBUSTIBL` y `MEDIDOR_LU` en viviendas, en 2012 y 2014.
    * El atributo `"descriptor"` del resultado contiene la direccion del descriptor de archivos de la edicion.
    * Las direcciones salen del registro de archivos del INEGI, y la funcion trabaja en cualquier sistema operativo (`enigh_nuevaconstruccion()` solo funcionaba en Windows).
    * Mensajes de error que indican que años y que tablas existen. Con un año que solo esta en la serie tradicional, el mensaje remite a `enigh_tradicional()`.
* `enoe()`:
    * `enoe(year, trimestre, integrar, tabla, estado)`: los tres primeros argumentos son los de la serie 1.x, en el mismo orden; `tabla` y `estado` son nuevos. `trimestre` acepta tambien el numero del trimestre (de 1 a 4).
    * Trimestres de 2005-T1 a 2026-T3 (86), de la serie de la poblacion de 15 años y mas. La funcion anterior no podia descargar los trimestres de 2023 en adelante: armaba la direccion con el nombre que tuvieron los archivos de 2020-T3 a 2022-T4.
    * Las direcciones salen del registro de archivos del INEGI. Un trimestre posterior al registro se busca, con un aviso, en la direccion que usa el INEGI desde 2023.
    * El segundo trimestre de 2020 no existe: el INEGI levanto en su lugar la Encuesta Telefonica de Ocupacion y Empleo (ETOE). La funcion lo explica y remite a `etoe()`, en lugar de fallar en la descarga.
    * **Cambio incompatible:** las tablas de la lista se llaman `coe1`, `coe2`, `hog`, `sdem` y `viv` en todos los trimestres. Antes llevaban el nombre de su archivo, que cambia con el trimestre (`SDEMT105`, `ENOEN_SDEMT320`, `ENOE_SDEMT124`). El orden de la lista no cambia: la tabla de hogares sigue siendo la tercera.
    * Nuevo argumento `tabla`: devuelve solo las tablas indicadas; con una sola tabla, un data.frame. Solo se descomprimen y se leen las tablas pedidas.
    * Nuevo argumento `estado`: conserva las filas de una o varias entidades.
    * **Cambio incompatible:** `integrar = TRUE` une las tablas por la llave de cada nivel (vivienda, hogar y persona), que cambia con el trimestre, y devuelve una fila por residente. Los hogares sin residentes, como los que no se entrevistaron, se conservan con `NA` en las columnas de las tablas de residentes. Si la llave de una tabla tiene valores repetidos, la funcion se detiene en lugar de devolver filas duplicadas. Las columnas que se repiten entre tablas quedan una sola vez, con el valor de la tabla de mas detalle, salvo los ponderadores: la tabla integrada mantiene el del residente, con su nombre (`FAC`, o `FAC_TRI` y `FAC_MEN`), y los del hogar y de la vivienda, con el sufijo de su tabla (`FAC_TRI_HOG`, `FAC_TRI_VIV`), porque no siempre coinciden. Las preguntas de vivienda y de hogar que se llaman igual que otras del cuestionario de ocupacion y empleo llevan el sufijo de su tabla (`P1_VIV`, `P4_1_HOG`). Antes, las tablas se unian por todas las columnas con el mismo nombre, lo que no relacionaba a cada persona con su hogar y su vivienda.
    * Con `integrar = TRUE` y `tabla`, se integran solo las tablas indicadas.
    * Las columnas conservan los nombres y los tipos de los archivos DBF del INEGI, como en la serie 1.x: las respuestas de los cuestionarios y las columnas de identificacion son texto, con sus ceros a la izquierda, y los ponderadores y las variables precodificadas, numeros.
    * Nuevos atributos del resultado: `"fuente"` (direccion del archivo), `"llave"` (columnas que identifican cada fila de cada tabla) y `"nota"` (nota del INEGI sobre el reemplazo de los archivos del trimestre, cuando el archivo la incluye; tambien se muestra como mensaje).
    * Cada llamada descomprime el archivo en una carpeta propia. Antes, una segunda llamada en la misma sesion de R devolvia tambien las tablas de la primera.
    * **Cambio incompatible:** `enoe()` sin argumentos ya no abre la pagina de la encuesta en el navegador (solo funcionaba en Windows). Los trimestres disponibles se consultan con `bases_importinegi("enoe")`.
    * Mensajes de error que indican que años, trimestres y tablas existen.
* Nueva funcion `etoe()`, que descarga la Encuesta Telefonica de Ocupacion y Empleo (ETOE), que el INEGI levanto en abril, mayo y junio de 2020, cuando suspendio las entrevistas de la ENOE. Se usa igual que `enoe()` y comparte su programacion.
    * `etoe(year, mes, integrar, tabla, estado, poblacion)`: `year` y `mes` son obligatorios; `mes` acepta `"abril"`, `"mayo"` y `"junio"`, o el numero del mes (de 4 a 6); `integrar`, `tabla` y `estado` funcionan como en `enoe()`.
    * `poblacion` elige la version de los ponderadores: `"cpv2020"` (por omision), con la estimacion de poblacion con base en el Censo de Poblacion y Vivienda 2020 (`FAC_NP`, `FAC32_NP` y `FAC39_NP`), o `"conapo"`, con las proyecciones de poblacion del CONAPO (`FAC`, `FAC32` y `FAC39`), que reproduce las cifras que el INEGI publico en 2020.
    * Devuelve las mismas cinco tablas que `enoe()` (`coe1`, `coe2`, `hog`, `sdem` y `viv`), con los nombres y los tipos de los archivos DBF del INEGI, y los atributos `"fuente"` y `"llave"`.
    * `integrar = TRUE` sigue las reglas de `enoe()`. La tabla integrada mantiene los tres ponderadores de cada tabla (el nacional, el de 32 ciudades y el de 39 ciudades) para el residente, el hogar y la vivienda: nueve columnas.
    * La llave es la misma en los tres meses y en las dos versiones: `CD_A`, `ENT`, `CON` y `V_SEL`; el hogar agrega `N_HOG` y `H_MUD`, y la persona, `N_REN`. La columna `TIPO`, que solo existe en junio con `poblacion = "cpv2020"`, no forma parte de la llave.
    * Las tablas se devuelven como las publica el INEGI: ninguna columna indica el mes (`PER` vale `"220"` en los tres meses).
    * Con `estado`, la funcion avisa que la encuesta tiene alcance nacional y que su muestra es pequeña para hacer estimaciones por entidad.
    * Las direcciones salen del registro de archivos del INEGI. Los meses disponibles se consultan con `bases_importinegi("etoe")`.
    * Mensajes de error que indican que meses existen. Con otro año o con otro mes, el mensaje remite a `enoe()`.
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
* Ayuda de `cpv_censo()` y seccion del Censo de Poblacion y Vivienda de la viñeta, reescrita para `cpv_censo()`: bases disponibles por año, entidades, niveles de agregacion y tablas. La documentacion ya no menciona las funciones `censo_poblacion_*()`.
* Ayuda de `bases_importinegi()` y nueva seccion de la viñeta, "Bases de datos disponibles".
* Nueva funcion `sig_marcogeoestadistico()`, que descarga los mapas del Marco Geoestadistico del INEGI de 1995 a 2026: las 26 ediciones publicadas, por edicion, mapa y entidad.
    * `sig_marcogeoestadistico(year, mapa, estado, version)`: `year` y `mapa` son obligatorios. `year` es el año de edicion que el INEGI asigna a cada producto.
    * `version` distingue las ediciones de un mismo año como las distingue el INEGI: `"4.3"`, `"5.0"`, `"5.0.A"` y `"5.0.A 1:20 000"` en 2010; `"junio"` y `"diciembre"` en 2017; `"febrero"`, `"junio"` y `"diciembre"` en 2018; `"censo agropecuario 2022"` y `"diciembre"` en 2023. Si el año tiene varias ediciones y no se indica `version`, la funcion se detiene y las lista.
    * 18 mapas: entidades, municipios, AGEB rurales y urbanas, localidades (urbanas, amanzanadas y rurales), integracion territorial, territorio insular y, desde diciembre de 2017, manzanas, frentes de manzana, vialidades, servicios (areas, lineas y puntos), caserio disperso y poligonos externos. Tambien las zonas en disputa entre entidades de dos ediciones de 2010. Desde diciembre de 2017, `mapa` acepta ademas el codigo del INEGI (`"MUN"`, `"FM"`).
    * Devuelve siempre un solo mapa: un objeto de clase `sf`. Varias entidades, o todo el pais cuando el INEGI publica el mapa solo por entidad, se combinan en un solo objeto.
    * `estado` acepta nombre, abreviatura o clave, y varias entidades a la vez. `"nacional"` devuelve todo el pais. Si no se indica, se descarga todo el pais, salvo en los mapas que el INEGI publica solo por entidad y en las manzanas, los frentes de manzana y las vialidades, cuyo archivo nacional pesa de 400 MB a 1 GB: en esos se descarga Aguascalientes, con un aviso.
    * Devuelve cada mapa como lo publica el INEGI: no cambia los nombres de las columnas, la proyeccion ni el contenido. Solo convierte en `NA` el texto `"NULL"`. La ayuda documenta la proyeccion de cada edicion y sus excepciones.
    * El texto se lee con la codificacion que declara cada archivo. Los archivos que no declaran ninguna (la mayoria de 1995 a 2013) se leen como Latin-1; antes, sus nombres con acentos quedaban como texto no valido.
    * Descarga solo lo necesario. De diciembre de 2017 en adelante, el archivo del mapa y de la entidad que se piden, en lugar del producto completo de 2.4 a 3.1 GB. En las tres ediciones que el INEGI publica solo como producto completo (febrero y junio de 2018, y Censo Agropecuario 2022), el tramo del producto que corresponde a cada entidad.
    * `bases_importinegi("sig_marcogeoestadistico")` lista las ediciones y los mapas de cada una.
* **Cambio incompatible:** se elimino `sig_marcogeo()`. `sig_marcogeoestadistico()` la sustituye. Conserva los argumentos `year`, `mapa` y `version`, y agrega `estado` en la tercera posicion, por lo que `version` se debe escribir con su nombre.
    * `sig_marcogeo(2009, "municipios")` pasa a ser `sig_marcogeoestadistico(2009, "municipios")`.
    * `sig_marcogeo(2010, "entidades", version = "5.0.A")` pasa a ser `sig_marcogeoestadistico(2010, "entidades", version = "5.0.A")`.
    * `mapa = "ageb"` pasa a ser `"ageb_urbana"` (2000, 2005, 2009, 2010 y 2013) o `"ageb_rural"` (2007); `"urbano"` pasa a ser `"localidades_urbanas"`, y `"rural"`, `"localidades_rurales"`. Con los nombres anteriores, la funcion se detiene e indica el nombre nuevo.
    * `sig_marcogeo()` sin argumentos abria la pagina del Marco Geoestadistico en el navegador (solo en Windows). Las ediciones y sus mapas se consultan ahora con `bases_importinegi("sig_marcogeoestadistico")`.
    * `sig_marcogeo()` cubria 8 ediciones (1995, 2000, 2005, 2007, 2009, dos de 2010 y 2013). Sus direcciones usaban `http` y un dominio anterior del INEGI.
* Ayuda de `cpv_conteo()` y nueva seccion de la viñeta, "Conteo de Poblacion y Vivienda", que explica como se levantaron los conteos de 1995 y 2005.
* Ayuda de `cpv_intercensal()` y nueva seccion de la viñeta, "Encuesta Intercensal", que explica como se levantaron las encuestas de 2015 y 2025.
* Ayuda de `cngmd()` y seccion de la viñeta sobre el Censo Nacional de Gobiernos Municipales, reescrita para `cngmd()`: modulos por edicion, temas, tablas, catalogos y siglas del INEGI. La documentacion ya no menciona `censo_municipal()`.
* Ayuda de `enigh_tradicional()` y nueva subseccion de la viñeta, "Serie tradicional": tablas por edicion, como unirlas con el identificador del hogar, donde esta el factor de expansion y como se devuelven las columnas. La seccion de la ENIGH de la viñeta presenta ahora las tres series de la encuesta.
* Ayuda de `enigh_nc()` y subseccion de la viñeta "Nueva Construccion", reescrita para `enigh_nc()`: que cambia respecto de la serie tradicional, tablas por edicion, como unirlas con el identificador del hogar, donde estan el factor de expansion y las variables del diseño muestral, y como se devuelven las columnas. La documentacion ya no menciona `enigh_nuevaconstruccion()`.
* Ayuda de `cndhf()` y nueva seccion de la viñeta, "Censo Nacional de Derechos Humanos Federal": modulos, temas, tablas, catalogos, siglas del INEGI y claves de la entidad y del municipio.
* Ayuda de `enoe()` y seccion de la ENOE de la viñeta, actualizadas: trimestres disponibles, tablas, argumentos `tabla` y `estado`, valor que devuelve la funcion, como se integran las tablas y ponderadores. La ayuda de `bases_importinegi()` y la seccion "Bases de datos disponibles" de la viñeta describen las filas de la ENOE.
* Ayuda de `etoe()` y nueva seccion de la viñeta, "Encuesta Telefonica de Ocupacion y Empleo": meses disponibles, versiones de los ponderadores, tablas, como se integran y como unir varios meses. La ayuda de `enoe()` y la seccion de la ENOE de la viñeta remiten a `etoe()`. La ayuda de `bases_importinegi()` y la seccion "Bases de datos disponibles" de la viñeta describen las filas de la ETOE.
* La pagina de ayuda del paquete (`?importinegi`) incluye el indice de funciones por proyecto estadistico.
* La ayuda de `sig_caminos_extrae()` indica que requiere el paquete `sf`.
* Ayuda de `sig_marcogeoestadistico()`: ediciones, mapas, proyeccion de cada edicion y tamaño de las descargas. La ayuda de `bases_importinegi()` describe las filas del Marco Geoestadistico.
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
