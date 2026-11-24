#' ENOE
#'
#' Encuesta Nacional de Ocupacion y Empleo (ENOE)
#'
#' La ENOE es un proyecto estadistico de encuestas en hogares especializado en informacion sobre el mercado laboral. La ENOE provee informacion trimestral sobre la fuerza laboral, la ocupacion, subocupacion y desocupacion de los miembros del hogar encuestado.
#'
#' La funcion descarga la serie vigente de la encuesta (poblacion de 15 años y mas), del primer trimestre de 2005 al segundo trimestre de 2026. El INEGI no publico la ENOE del segundo trimestre de 2020: en ese trimestre levanto la Encuesta Telefonica de Ocupacion y Empleo (ETOE), que esta funcion no descarga. \code{\link{bases_importinegi}} lista los trimestres disponibles sin descargar nada: \code{bases_importinegi("enoe")}.
#'
#' Cada trimestre se publica en cinco bases de datos (tablas), que se eligen con el argumento \code{tabla}:
#' \describe{
#'   \item{\code{"viv"}}{Vivienda: una fila por vivienda seleccionada.}
#'   \item{\code{"hog"}}{Hogar: una fila por hogar, incluidos los hogares que no se entrevistaron.}
#'   \item{\code{"sdem"}}{Sociodemografico: una fila por residente del hogar, de cualquier edad.}
#'   \item{\code{"coe1"} y \code{"coe2"}}{Cuestionario de ocupacion y empleo, primera y segunda parte: una fila por residente de 12 años o mas. El cuestionario tiene una version basica y una ampliada, con mas preguntas; \code{bases_importinegi("enoe")} indica cual se aplico en cada trimestre.}
#' }
#'
#' La funcion toma la direccion del archivo del registro de archivos del INEGI que se distribuye con el paquete, descarga el archivo nacional del trimestre (de 18 a 40 MB) en una carpeta temporal y lee las bases de datos en formato DBF. El INEGI no publica la ENOE por entidad federativa: con el argumento \code{estado}, la funcion descarga el archivo nacional y conserva las filas de las entidades indicadas. Requiere conexion a internet.
#'
#' @section Como unir las bases de datos:
#' Las bases de datos de un trimestre se unen con las columnas que identifican a la vivienda (\code{CD_A}, \code{ENT}, \code{CON} y \code{V_SEL}), al hogar (las de la vivienda, \code{N_HOG} y \code{H_MUD}) y al residente (las del hogar y \code{N_REN}). Desde el tercer trimestre de 2020, la vivienda se identifica ademas con \code{TIPO} y \code{MES_CAL} (y con \code{CA} hasta el segundo trimestre de 2021); desde el tercer trimestre de 2025, la columna \code{ENT} se llama \code{CVE_ENT}. El atributo \code{"llave"} del resultado indica las columnas que identifican cada fila en el trimestre descargado.
#'
#' Con \code{integrar = TRUE}, la funcion hace esa union y devuelve una sola base de datos:
#' \itemize{
#'   \item Tiene una fila por residente. Los residentes sin cuestionario de ocupacion y empleo (los menores de 12 años, principalmente) quedan con \code{NA} en las columnas del cuestionario.
#'   \item No se pierde ninguna fila. Los hogares sin residentes, que son hogares que no se entrevistaron, se agregan al final, con \code{NA} en las columnas de los residentes. Se reconocen porque su \code{N_REN} es \code{NA}; su \code{R_DEF} indica el resultado de la entrevista. Los hogares con entrevista suspendida (\code{R_DEF} igual a \code{"15"}) si tienen residentes y aparecen en las filas de sus residentes.
#'   \item Las columnas que se repiten en varias bases de datos quedan una sola vez, con el valor de la base de datos de mas detalle.
#'   \item Las preguntas de vivienda y de hogar llevan el sufijo de su base de datos (\code{P1_VIV}, \code{P2_VIV}, \code{P3_VIV}, \code{P4_1_HOG} y \code{P4_2_HOG}), porque se llaman igual que otras preguntas del cuestionario de ocupacion y empleo.
#'   \item Se mantienen los ponderadores del residente, del hogar y de la vivienda (ver la seccion Ponderadores).
#'   \item Con el argumento \code{tabla}, se integran solamente las bases de datos indicadas. Por ejemplo, \code{tabla = c("hog", "viv")} devuelve una fila por hogar.
#' }
#'
#' @section Ponderadores:
#' El ponderador indica cuantas personas, hogares o viviendas representa cada fila de la muestra, y las estimaciones deben usarlo. Hasta el primer trimestre de 2020 se llama \code{FAC}. Desde el tercer trimestre de 2020 hay dos: \code{FAC_TRI}, para las estimaciones trimestrales, y \code{FAC_MEN}, para las mensuales. Las cifras que publica el INEGI se reproducen con el ponderador de la base de datos \code{"sdem"} y los residentes con entrevista completa (\code{R_DEF} igual a \code{"00"}), sin los ausentes definitivos (\code{C_RES} igual a \code{"1"} o \code{"3"}) y de 15 a 98 años de edad (\code{EDA}).
#'
#' Las bases de datos de vivienda, de hogar y sociodemografica tienen cada una su ponderador, y los tres no siempre coinciden: en cada trimestre, el ponderador de entre 1.7 y 4 de cada 100 residentes es distinto del de su hogar. Por eso, la base de datos integrada mantiene los tres. El ponderador del residente conserva su nombre (\code{FAC}, o \code{FAC_TRI} y \code{FAC_MEN}); el del hogar y el de la vivienda llevan el sufijo de su base de datos: \code{FAC_HOG} y \code{FAC_VIV}, o \code{FAC_TRI_HOG}, \code{FAC_MEN_HOG}, \code{FAC_TRI_VIV} y \code{FAC_MEN_VIV}. Las estimaciones de personas usan el ponderador del residente; las de hogares, el del hogar, y las de viviendas, el de la vivienda. Como la base de datos integrada tiene una fila por residente, para estimar hogares o viviendas hay que conservar antes una sola fila por hogar o por vivienda, o descargar esas bases de datos con el argumento \code{tabla}. Los hogares sin residentes tienen \code{NA} en el ponderador del residente.
#'
#' @section Contenido de las bases de datos:
#' Los nombres de las columnas son los del INEGI, en mayusculas. Las columnas conservan el tipo que les da el INEGI: las que declara como texto se devuelven como texto, con sus ceros a la izquierda (las columnas de identificacion y las respuestas de los cuestionarios, incluida la edad, \code{EDA}), y las que declara como numero, como numero (los ponderadores y las variables precodificadas de la base de datos \code{"sdem"}). Los datos que el INEGI deja en blanco se devuelven como \code{NA}. Los codigos no se sustituyen por sus descripciones.
#'
#' Algunas columnas cambian de nombre, aparecen o desaparecen a lo largo de la serie (los cambios mayores ocurren en el tercer trimestre de 2020, de 2021 y de 2025), y el cuestionario ampliado tiene mas columnas que el basico, asi que las bases de datos de dos trimestres no se pueden apilar sin revisarlas.
#'
#' El INEGI reemplaza los archivos de trimestres ya publicados cuando actualiza sus estimaciones de poblacion o corrige datos, y lo explica en una nota que acompaña a los archivos. Cuando el trimestre descargado trae esa nota, la funcion la muestra como mensaje y la guarda en el atributo \code{"nota"} del resultado. La direccion del archivo descargado queda en el atributo \code{"fuente"}.
#'
#' @param year Año de levantamiento de la encuesta en formato numerico, de 2005 en adelante.
#' @param trimestre Trimestre de levantamiento de la encuesta en formato alfanumerico. Las opciones son: "trim1", "trim2", "trim3" y "trim4". Tambien se acepta el numero del trimestre, de 1 a 4.
#' @param integrar \code{FALSE}: descarga por separado y en una lista las cinco bases de datos que componen la ENOE. \code{TRUE}: integra las cinco bases de datos en una sola, utilizando el identificador unico del entrevistado. La base de datos integrada tiene una fila por residente, conserva los hogares sin residentes y mantiene los ponderadores del residente, del hogar y de la vivienda (ver las secciones Como unir las bases de datos y Ponderadores).
#' @param tabla Tabla o tablas que se devuelven: \code{"viv"} (vivienda), \code{"hog"} (hogar), \code{"sdem"} (sociodemografico), \code{"coe1"} y \code{"coe2"} (cuestionario de ocupacion y empleo, primera y segunda parte). Por omision, las cinco.
#' @param estado Entidad federativa. Acepta el nombre (\code{"Jalisco"}, \code{"Ciudad de Mexico"}), la abreviatura (\code{"jal"}, \code{"cdmx"}) o la clave del INEGI (\code{14}, \code{"09"}), sin distinguir mayusculas ni acentos, y varias entidades a la vez. La funcion descarga el archivo nacional y conserva las filas de las entidades indicadas. Por omision, todo el pais.
#'
#' @examples
#'
#' # Descargar las bases de datos de la ENOE 2009, Trimestre 1, sin integrar.
#' \dontrun{enoe(year = 2009, trimestre = "trim1")}
#'
#' # Descargar las bases de datos de la ENOE 2009, Trimestre 1, integradas
#' \dontrun{enoe(year = 2009, trimestre = "trim1", integrar = TRUE)}
#'
#' # Descargar solamente la base de datos sociodemografica de la ENOE 2024, Trimestre 1
#' \dontrun{enoe(year = 2024, trimestre = "trim1", tabla = "sdem")}
#'
#' # Descargar las bases de datos de la ENOE 2024, Trimestre 1, de Jalisco, integradas
#' \dontrun{enoe(year = 2024, trimestre = "trim1", integrar = TRUE, estado = "Jalisco")}
#' @return Con \code{integrar = FALSE}, una lista con las cinco bases de datos en formato data.frame, con los nombres \code{coe1}, \code{coe2}, \code{hog}, \code{sdem} y \code{viv}. Si en \code{tabla} se indica una sola base de datos, un data.frame; si se indican varias, una lista con esas bases de datos, en el orden indicado. Con \code{integrar = TRUE}, un data.frame. El resultado tiene los atributos \code{"fuente"}, \code{"llave"} y, cuando el INEGI publico una nota sobre los archivos del trimestre, \code{"nota"}.
#' @seealso \code{\link{bases_importinegi}} para consultar los trimestres disponibles.
#' @export

enoe <- function(year, trimestre, integrar = FALSE, tabla = NULL, estado = NULL) {
  if (missing(year)) {
    year <- NULL
  }
  if (missing(trimestre)) {
    trimestre <- NULL
  }
  enoe_obtener(year, trimestre, integrar, tabla, estado)
}

# Encuesta Nacional de Ocupacion y Empleo, poblacion de 15 años y mas ----------
#
# El INEGI publica un zip por trimestre con cinco tablas en DBF: vivienda
# (VIV), hogar (HOG), sociodemografico (SDEM) y el cuestionario de ocupacion y
# empleo en dos partes (COE1 y COE2). La forma de los archivos se reviso el 22
# de noviembre de 2026 en los encabezados de las 425 tablas de los 85
# trimestres publicados (2005-T1 a 2026-T2), sin descargarlos completos. Las
# columnas que identifican a la vivienda cambian tres veces:
#
#   2005-T1 a 2020-T1    CD_A, ENT, CON y V_SEL.
#   2020-T3 a 2021-T2    Se agregan TIPO, MES_CAL y CA.
#   2021-T3 a 2025-T2    Se agregan TIPO y MES_CAL.
#   2025-T3 en adelante  CVE_ENT en lugar de ENT.
#
# El hogar agrega N_HOG y H_MUD, y la persona, N_REN. El segundo trimestre de
# 2020 no existe: el INEGI levanto en su lugar la ETOE, que es otro proyecto.
# Los nombres de los archivos tambien cambian (SDEMT105.dbf, sdemt212.dbf,
# ENOEN_SDEMT320.DBF, ENOE_SDEMT124.dbf); los de las columnas estan en
# mayusculas en todos los trimestres.

# Tablas de un trimestre, en el orden en que las devolvia la serie 1.x (el de
# los nombres de sus archivos): la tercera de la lista sigue siendo la de hogares.
enoe_tablas <- function() {
  c("coe1", "coe2", "hog", "sdem", "viv")
}

# Nivel de cada tabla: 1, vivienda; 2, hogar; 3, persona.
enoe_nivel <- function() {
  c(viv = 1L, hog = 2L, sdem = 3L, coe1 = 3L, coe2 = 3L)
}

# Orden en que se integran las tablas: de la de mas detalle a la de menos.
enoe_orden <- function(nombres) {
  intersect(c("sdem", "coe1", "coe2", "hog", "viv"), nombres)
}

# Trimestres del registro ------------------------------------------------------

# Trimestres de la ENOE en el registro de archivos del paquete, del primero al
# ultimo. El año y el trimestre se leen de la ruta del archivo ("2005trim1",
# "enoe_n_2020_trim3", "enoe_2023_trim1") y la edicion, del titulo que le da el
# INEGI: de 2020-T3 a 2022-T4 es la ENOE Nueva Edicion (ENOE_N).
enoe_trimestres <- function() {
  registro <- registro_inegi()
  patron <- "^/programas/enoe/15ymas/microdatos/[a-z_]*([0-9]{4})_?trim([1-4])$"
  propias <- registro$proyecto %in% "enoe" & registro$formato %in% "dbf" & grepl(patron, registro$ruta)
  filas <- registro[propias, , drop = FALSE]
  trimestres <- data.frame(
    year = as.integer(sub(patron, "\\1", filas$ruta)),
    trimestre = as.integer(sub(patron, "\\2", filas$ruta)),
    edicion = c("ENOE", "ENOE_N")[grepl("ENOE_N", filas$titulo, fixed = TRUE) + 1L],
    url = registro_url(filas),
    tamano = filas$tamano,
    bytes = filas$bytes,
    stringsAsFactors = FALSE
  )
  trimestres <- trimestres[order(trimestres$year, trimestres$trimestre), , drop = FALSE]
  trimestres <- trimestres[!duplicated(trimestres[c("year", "trimestre")]), , drop = FALSE]
  rownames(trimestres) <- NULL
  trimestres
}

# Cuestionario de ocupacion y empleo de cada trimestre. El ampliado se aplico
# de 2005-T1 a 2006-T2, en el segundo trimestre de 2007 y de 2008 y, desde
# 2009, en el primer trimestre de cada año (26 de los 85 trimestres, segun el
# numero de columnas de COE1 y COE2); en los demas se aplico el basico.
enoe_cuestionario <- function(year, trimestre) {
  ampliado <- year == 2005 | (year == 2006 & trimestre <= 2) |
    (year %in% c(2007, 2008) & trimestre == 2) | (year >= 2009 & trimestre == 1)
  c("b\u00e1sico", "ampliado")[ampliado + 1L]
}

# Nombre de un trimestre para los mensajes ("primer trimestre de 2024").
enoe_etiqueta <- function(year, trimestre) {
  paste(c("primer", "segundo", "tercer", "cuarto")[trimestre], "trimestre de", year)
}

# Trimestres de la ENOE con la forma de bases_importinegi(): una fila por
# trimestre. `base` es el trimestre como se escribe en enoe().
enoe_bases <- function() {
  trimestres <- enoe_trimestres()
  edicion <- c(ENOE = "ENOE", ENOE_N = "ENOE Nueva Edici\u00f3n (ENOE_N)")[trimestres$edicion]
  data.frame(
    proyecto = rep("enoe", nrow(trimestres)),
    year = trimestres$year,
    modulo = rep(NA_character_, nrow(trimestres)),
    base = paste0(rep("trim", nrow(trimestres)), trimestres$trimestre),
    descripcion = paste0(unname(edicion), rep(", cuestionario ", nrow(trimestres)),
                         enoe_cuestionario(trimestres$year, trimestres$trimestre)),
    tablas = rep(paste(enoe_tablas(), collapse = ", "), nrow(trimestres)),
    cobertura = rep("nacional", nrow(trimestres)),
    formato = rep("dbf", nrow(trimestres)),
    tamano_mb = round(round(trimestres$bytes) / 1024^2, 1),
    stringsAsFactors = FALSE
  )
}

# Validacion de los argumentos -------------------------------------------------

enoe_validar_year <- function(year, trimestres) {
  disponibles <- paste0(
    "A\u00f1os disponibles en enoe(): de ", min(trimestres$year), " a ", max(trimestres$year),
    ". Los trimestres se consultan con bases_importinegi(\"enoe\")."
  )
  if (is.null(year) || length(year) != 1 || is.na(year)) {
    detener("Falta indicar el a\u00f1o en `year` (un solo valor). ", disponibles)
  }
  numero <- suppressWarnings(as.integer(as.character(year)))
  if (is.na(numero) || !grepl("^[0-9]{4}$", trimws(as.character(year)))) {
    detener("`year = ", comillas(year), "` no es un a\u00f1o. ", disponibles)
  }
  numero
}

# El trimestre se acepta como en la serie 1.x ("trim1" a "trim4") o como numero.
enoe_validar_trimestre <- function(trimestre) {
  opciones <- "Opciones: \"trim1\", \"trim2\", \"trim3\" y \"trim4\", o el n\u00famero del trimestre (1 a 4)."
  if (is.null(trimestre) || length(trimestre) != 1 || is.na(trimestre)) {
    detener("Falta indicar el trimestre en `trimestre` (un solo valor). ", opciones)
  }
  texto <- tolower(trimws(as.character(trimestre)))
  if (!grepl("^(trim(estre)?|t)? *[1-4]$", texto)) {
    detener("`trimestre = ", comillas(trimestre), "` no es un trimestre. ", opciones)
  }
  as.integer(sub("^.*([1-4])$", "\\1", texto))
}

# Las tablas se piden con su abreviatura, sin distinguir mayusculas. Tambien se
# acepta el nombre del archivo del INEGI ("SDEMT124", "ENOE_SDEMT124.dbf"), que
# es como se llamaban los elementos de la lista en la serie 1.x.
enoe_validar_tabla <- function(tabla) {
  if (is.null(tabla)) {
    return(enoe_tablas())
  }
  opciones <- paste0(
    "Tablas de la ENOE: \"viv\" (vivienda), \"hog\" (hogar), \"sdem\" (sociodemogr\u00e1fico), ",
    "\"coe1\" y \"coe2\" (cuestionario de ocupaci\u00f3n y empleo, primera y segunda parte)."
  )
  if (!is.character(tabla) || length(tabla) == 0 || anyNA(tabla)) {
    detener("`tabla` debe ser el nombre de una o varias tablas. ", opciones)
  }
  patron <- "^(enoen?_)?(viv|hog|sdem|coe1|coe2)(t[0-9]+)?([.]dbf)?$"
  nombre <- tolower(trimws(tabla))
  ajenas <- tabla[!grepl(patron, nombre)]
  if (length(ajenas) > 0) {
    detener("`tabla = ", enumerar(ajenas), "` no es una tabla de la ENOE. ", opciones)
  }
  unique(sub(patron, "\\2", nombre))
}

# Archivo de un trimestre: el del registro o, si el trimestre es posterior al
# registro, el de la direccion que usa el INEGI desde 2023. La ENOE se publica
# cuatro veces al año y el registro se regenera una, asi que los trimestres mas
# recientes pueden no estar en el.
enoe_elegir <- function(year, trimestre) {
  trimestres <- enoe_trimestres()
  if (nrow(trimestres) == 0) {
    detener("El registro de archivos del paquete no tiene los trimestres de la ENOE.")
  }
  year <- enoe_validar_year(year, trimestres)
  trimestre <- enoe_validar_trimestre(trimestre)
  etiqueta <- enoe_etiqueta(year, trimestre)
  fila <- trimestres[trimestres$year == year & trimestres$trimestre == trimestre, , drop = FALSE]
  if (nrow(fila) == 1) {
    return(list(year = year, trimestre = trimestre, etiqueta = etiqueta, url = fila$url,
                tamano = fila$tamano, registrado = TRUE))
  }

  primero <- trimestres[1, ]
  ultimo <- trimestres[nrow(trimestres), ]
  serie <- paste0(
    "El registro de archivos del paquete (", registro_generado(), ") va del ",
    enoe_etiqueta(primero$year, primero$trimestre), " al ", enoe_etiqueta(ultimo$year, ultimo$trimestre), "."
  )
  posicion <- function(year, trimestre) year * 4L + trimestre
  if (posicion(year, trimestre) < posicion(primero$year, primero$trimestre)) {
    detener("La ENOE del ", etiqueta, " no existe. ", serie)
  }
  if (posicion(year, trimestre) < posicion(ultimo$year, ultimo$trimestre)) {
    detener(
      "El INEGI no public\u00f3 la ENOE del ", etiqueta, ".",
      if (year == 2020 && trimestre == 2) {
        paste0(
          " Suspendi\u00f3 las entrevistas en las viviendas por la emergencia sanitaria de COVID-19 y, ",
          "para abril, mayo y junio, public\u00f3 la Encuesta Telef\u00f3nica de Ocupaci\u00f3n y Empleo ",
          "(ETOE), que es otro proyecto y no se descarga con enoe()."
        )
      }
    )
  }
  hoy <- as.POSIXlt(Sys.Date())
  if (posicion(year, trimestre) > posicion(hoy$year + 1900L, hoy$mon %/% 3L + 1L)) {
    detener("El ", etiqueta, " todav\u00eda no ocurre. ", serie)
  }
  list(
    year = year, trimestre = trimestre, etiqueta = etiqueta,
    url = inegi_url(paste0("/contenidos/programas/enoe/15ymas/microdatos/enoe_", year, "_trim", trimestre, "_dbf.zip")),
    tamano = NULL, registrado = FALSE
  )
}

# Descarga y contenido del zip -------------------------------------------------

enoe_descargar <- function(elegido) {
  if (elegido$registrado) {
    return(inegi_descargar(elegido$url, tamano = elegido$tamano))
  }
  message(
    "El ", elegido$etiqueta, " es posterior al registro de archivos del paquete (", registro_generado(),
    "): se busca en la direcci\u00f3n que usa el INEGI desde 2023."
  )
  tryCatch(
    inegi_descargar(elegido$url),
    error = function(e) {
      detener(
        "No se pudo descargar la ENOE del ", elegido$etiqueta, ". Es posible que el INEGI todav\u00eda no la ",
        "publique o que haya cambiado la direcci\u00f3n de sus archivos. Detalle: ", conditionMessage(e)
      )
    }
  )
}

# Archivos del zip: tabla a la que corresponde cada DBF y notas del INEGI. El
# nombre de cada tabla se reconoce sin distinguir mayusculas, con o sin el
# prefijo de la edicion y con o sin el trimestre.
enoe_contenido <- function(zip) {
  lista <- tryCatch(utils::unzip(zip, list = TRUE), error = function(e) NULL)
  if (is.null(lista) || nrow(lista) == 0) {
    detener("No se pudo leer el contenido de ", basename(zip), ".")
  }
  archivo <- basename(lista$Name)
  patron <- "^([a-z]+_)?(viv|hog|sdem|coe1|coe2)t?[0-9]*[.]dbf$"
  es_tabla <- grepl(patron, archivo, ignore.case = TRUE)
  tabla <- tolower(sub(patron, "\\2", archivo[es_tabla], ignore.case = TRUE))
  if (anyDuplicated(tabla) > 0) {
    detener(
      "El archivo de la ENOE tiene m\u00e1s de un archivo para la tabla ", enumerar(unique(tabla[duplicated(tabla)])),
      ". Contiene: ", enumerar(archivo, maximo = 15), "."
    )
  }
  list(
    tablas = stats::setNames(lista$Name[es_tabla], tabla),
    notas = lista$Name[grepl("[.]txt$", archivo, ignore.case = TRUE)],
    archivos = archivo
  )
}

# Texto de las notas que el INEGI incluye en el zip cuando reemplaza los
# archivos de un trimestre, o NULL si no hay. Un problema al leer la nota no
# debe impedir la lectura de las tablas.
enoe_nota <- function(zip, rutas) {
  if (length(rutas) == 0) {
    return(NULL)
  }
  carpeta <- character()
  on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
  texto <- tryCatch({
    carpeta <- inegi_descomprimir(zip, anidados = FALSE, archivos = rutas)
    partes <- vapply(buscar_archivos(carpeta, extension = "txt"), enoe_leer_texto, character(1))
    paste(partes[nzchar(partes)], collapse = "\n")
  }, error = function(e) "")
  if (nzchar(texto)) texto else NULL
}

# Lee un archivo de texto en UTF-8, en UTF-16 (con marca de orden de bytes) o
# en la pagina de codigos de Windows, y lo devuelve en UTF-8, sin lineas vacias.
enoe_leer_texto <- function(archivo) {
  bytes <- readBin(archivo, what = "raw", n = file.size(archivo))
  marca <- if (length(bytes) >= 2) paste(sprintf("%02x", as.integer(bytes[1:2])), collapse = "") else ""
  if (marca %in% c("fffe", "feff")) {
    texto <- iconv(list(bytes[-(1:2)]), from = if (marca == "fffe") "UTF-16LE" else "UTF-16BE", to = "UTF-8")
  } else {
    texto <- rawToChar(bytes[bytes != as.raw(0)])
    if (!validUTF8(texto)) {
      texto <- iconv(texto, from = "CP1252", to = "UTF-8", sub = "?")
    }
    Encoding(texto) <- "UTF-8"
  }
  lineas <- trimws(strsplit(sub("^\ufeff", "", texto), "\r\n|\r|\n")[[1]])
  paste(lineas[nzchar(lineas)], collapse = "\n")
}

# Lectura de las tablas --------------------------------------------------------

# Lee una tabla del zip; solo descomprime ese archivo. Los tipos de las
# columnas son los que declara el INEGI en el DBF: las respuestas de los
# cuestionarios y las columnas de identificacion son texto, con sus ceros a la
# izquierda (R_DEF = "00", EDA = "05"), y los ponderadores y las variables
# precodificadas, numeros. Las celdas en blanco se leen como NA. Como en todo
# el paquete, leer_dbf() omite los registros borrados y devuelve como NA el
# texto "NULL". Con `entidades` conserva solo las filas de esas entidades.
enoe_leer_tabla <- function(zip, ruta, entidades = NULL) {
  carpeta <- inegi_descomprimir(zip, anidados = FALSE, archivos = ruta)
  on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
  datos <- leer_dbf(buscar_archivos(carpeta, extension = "dbf", exigir = TRUE)[1])
  names(datos) <- toupper(names(datos))
  # foreign::read.dbf() numera las filas con texto y agrega el atributo
  # "data_types", que se pierde al elegir filas: se quitan para que las tablas
  # tengan la misma forma con y sin `estado`
  rownames(datos) <- NULL
  attr(datos, "data_types") <- NULL
  if (!is.null(entidades)) {
    datos <- enoe_filtrar_entidades(datos, entidades)
  }
  datos
}

# Conserva las filas de las entidades pedidas (claves de dos digitos). La clave
# de la entidad es ENT hasta 2025-T2 y CVE_ENT desde 2025-T3.
enoe_filtrar_entidades <- function(datos, entidades) {
  columna <- intersect(c("ENT", "CVE_ENT"), names(datos))
  if (length(columna) == 0) {
    detener(
      "La tabla no tiene la clave de la entidad (ENT o CVE_ENT), as\u00ed que `estado` no puede elegir ",
      "sus filas. Columnas: ", enumerar(names(datos), maximo = 12), "."
    )
  }
  clave <- datos[[columna[1]]]
  clave <- if (is.numeric(clave)) sprintf("%02d", as.integer(clave)) else sub("^([0-9])$", "0\\1", clave)
  datos <- datos[!is.na(clave) & clave %in% entidades, , drop = FALSE]
  rownames(datos) <- NULL
  datos
}

# Llave ------------------------------------------------------------------------

# Columnas que identifican cada fila de una tabla, segun las columnas que tiene:
# la llave cambia con el trimestre (ver el comentario inicial).
enoe_llave <- function(columnas, nivel) {
  llave <- c(
    "CD_A", utils::head(intersect(c("ENT", "CVE_ENT"), columnas), 1), "CON", "V_SEL",
    intersect(c("TIPO", "MES_CAL", "CA"), columnas)
  )
  if (nivel >= 2L) {
    llave <- c(llave, "N_HOG", "H_MUD")
  }
  if (nivel >= 3L) {
    llave <- c(llave, "N_REN")
  }
  llave
}

# Llave de una tabla, despues de comprobar que tiene todas sus columnas.
enoe_llave_tabla <- function(datos, nombre) {
  llave <- enoe_llave(names(datos), enoe_nivel()[[nombre]])
  faltan <- setdiff(llave, names(datos))
  if (length(faltan) > 0) {
    detener(
      "La tabla \"", nombre, "\" no tiene las columnas de identificaci\u00f3n ", enumerar(faltan),
      ", as\u00ed que no se puede integrar. Columnas: ", enumerar(names(datos), maximo = 12), "."
    )
  }
  llave
}

# Valores de la llave de cada fila, en un solo texto.
enoe_clave <- function(datos, llave) {
  do.call(paste, c(unname(as.list(datos[llave])), sep = "\r"))
}

# Integracion ------------------------------------------------------------------

# Valores de una tabla con el tipo de la columna que van a completar.
enoe_como <- function(valores, modelo) {
  if (is.character(modelo) && !is.character(valores)) {
    as.character(valores)
  } else if (is.numeric(modelo) && is.character(valores)) {
    suppressWarnings(as.numeric(valores))
  } else {
    valores
  }
}

# Columnas de ponderadores: FAC hasta 2020-T1; FAC_TRI y FAC_MEN despues.
enoe_es_ponderador <- function(columnas) {
  grepl("^FAC(_[A-Z0-9]+)?$", columnas)
}

# Integra las tablas de un trimestre en una sola. Reglas:
#
# 1. La tabla base es la de mas detalle entre las pedidas (sdem; si no esta,
#    coe1, coe2, hog o viv) y cada una de sus filas es una fila del resultado:
#    con las cinco tablas, una fila por residente.
# 2. Las demas tablas se unen por la llave del nivel que les corresponde
#    (persona, hogar o vivienda). Si la llave de una tabla tiene valores
#    repetidos, la funcion se detiene: no devuelve filas duplicadas.
# 3. No se pierde ninguna fila: las de una tabla que no tienen fila en las
#    tablas de mas detalle se agregan al final. Asi se conservan los hogares
#    sin residentes, como los que no se entrevistaron, con NA en las columnas
#    de las tablas de residentes.
# 4. Las columnas que se repiten entre tablas (identificacion, geografia,
#    R_DEF, UR) se toman de la tabla de mas detalle en la que existe la fila:
#    la de residentes para un residente y la de hogares para un hogar sin
#    residentes.
# 5. Las preguntas de vivienda y de hogar (P1, P2 y P3; P4_1 y P4_2) llevan el
#    sufijo de su tabla (P1_VIV, P4_1_HOG), porque se llaman igual que otras
#    preguntas del cuestionario de ocupacion y empleo.
# 6. Los ponderadores (FAC; desde 2020-T3, FAC_TRI y FAC_MEN) no se funden
#    entre niveles, por decision del autor (24 de noviembre de 2026): el de la
#    tabla de hogares y el de la de viviendas llevan tambien el sufijo de su
#    tabla (FAC_TRI_HOG, FAC_TRI_VIV) y el que queda sin sufijo es el del
#    residente. En los 85 trimestres, el ponderador del residente es distinto
#    del de su hogar en 1.7 a 4.0 % de los residentes, y el del hogar es
#    distinto del de su vivienda en decenas o miles de hogares. Un hogar sin
#    residentes queda con NA en el ponderador del residente.
#
# `tablas` es una lista con nombre o un entorno. Si es un entorno, cada tabla
# se quita de el en cuanto se usa, para liberar su memoria.

enoe_integrar <- function(tablas) {
  if (!is.environment(tablas)) {
    tablas <- list2env(as.list(tablas), envir = new.env(parent = emptyenv()))
  }
  nivel <- enoe_nivel()
  orden <- enoe_orden(names(tablas))
  base <- orden[1]
  numero <- function(n) format(n, big.mark = ",", trim = TRUE)
  exigir_llave_unica <- function(nombre, clave, llave) {
    if (anyDuplicated(clave) > 0) {
      detener(
        "La tabla \"", nombre, "\" tiene ", numero(sum(duplicated(clave))), " filas con la llave repetida (",
        paste(llave, collapse = ", "), "), as\u00ed que no se puede integrar. Descarga las tablas por separado ",
        "con integrar = FALSE."
      )
    }
  }

  # Filas del resultado: para cada tabla, que fila suya corresponde a cada fila
  # del resultado (NA si no tiene). `identificacion` guarda la llave de las
  # filas del resultado.
  llave <- enoe_llave_tabla(tablas[[base]], base)
  identificacion <- as.list(tablas[[base]][llave])
  exigir_llave_unica(base, enoe_clave(identificacion, llave), llave)
  filas <- stats::setNames(list(seq_along(identificacion[[1]])), base)
  agregadas <- character()
  otra <- NULL
  for (nombre in orden[-1]) {
    otra <- tablas[[nombre]]
    llave <- intersect(enoe_llave_tabla(otra, nombre), names(identificacion))
    clave <- enoe_clave(otra, llave)
    exigir_llave_unica(nombre, clave, llave)
    hallada <- match(enoe_clave(identificacion, llave), clave)
    sobrantes <- which(!seq_along(clave) %in% hallada)
    if (nivel[[nombre]] < nivel[[base]] && anyNA(hallada)) {
      warning(
        numero(sum(is.na(hallada))), " filas no tienen su fila en la tabla \"", nombre,
        "\": quedan con NA en las columnas de esa tabla.",
        call. = FALSE
      )
    }
    if (base == "sdem" && nivel[[nombre]] == nivel[[base]] && length(sobrantes) > 0) {
      warning(
        numero(length(sobrantes)), " filas de la tabla \"", nombre, "\" no tienen su residente en la tabla ",
        "\"sdem\": se agregan al final de la tabla integrada.",
        call. = FALSE
      )
    }
    filas <- lapply(filas, function(fila) c(fila, rep(NA_integer_, length(sobrantes))))
    filas[[nombre]] <- c(hallada, sobrantes)
    if (length(sobrantes) > 0) {
      for (columna in names(identificacion)) {
        nuevos <- if (columna %in% names(otra)) otra[[columna]][sobrantes] else rep(NA, length(sobrantes))
        identificacion[[columna]] <- c(identificacion[[columna]], nuevos)
      }
      anteriores <- paste0("\"", orden[seq_len(match(nombre, orden) - 1L)], "\"")
      agregadas <- c(agregadas, paste0(
        numero(length(sobrantes)), " de \"", nombre, "\" sin fila en ",
        if (length(anteriores) > 1) paste0(paste(utils::head(anteriores, -1), collapse = ", "), " ni "),
        utils::tail(anteriores, 1)
      ))
    }
  }
  if (length(agregadas) > 0) {
    message(
      "La tabla integrada tiene ", numero(length(identificacion[[1]])), " filas: ", numero(nrow(tablas[[base]])),
      " de \"", base, "\" y ", paste(agregadas, collapse = "; "),
      ". Esas filas quedan con NA en las columnas de las tablas en las que no est\u00e1n."
    )
  }
  rm(otra, identificacion)

  # Columnas del resultado, tabla por tabla. Cada tabla se quita del entorno y
  # cada columna se suelta en cuanto se copia: en la simulacion con tablas del
  # tamaño real, la memoria maxima baja de 2.9 a 2.2 GB.
  presentes <- lapply(filas, function(fila) !is.na(fila))
  resultado <- list()
  fuentes <- list()
  for (nombre in orden) {
    actual <- unclass(tablas[[nombre]])
    rm(list = nombre, envir = tablas)
    fila <- filas[[nombre]]
    directa <- identical(fila, seq_along(actual[[1]]))
    for (columna in names(actual)) {
      valores <- actual[[columna]]
      actual[[columna]] <- NULL
      pregunta <- grepl("^P[0-9]", columna)
      repetida <- columna %in% names(resultado)
      # Reglas 5 y 6: las preguntas y los ponderadores de las tablas de hogares
      # y de viviendas llevan siempre el sufijo de su tabla.
      propia <- nombre %in% c("hog", "viv") && (pregunta || enoe_es_ponderador(columna))
      if (repetida && !pregunta && !propia) {
        # Regla 4: solo se llenan las filas que no existen en las tablas de las
        # que ya se tomo la columna.
        falta <- presentes[[nombre]] & !Reduce(`|`, presentes[fuentes[[columna]]])
        if (any(falta)) {
          resultado[[columna]][falta] <- enoe_como(valores[fila[falta]], resultado[[columna]])
        }
        fuentes[[columna]] <- c(fuentes[[columna]], nombre)
        next
      }
      destino <- columna
      if (repetida || propia) {
        destino <- paste0(columna, "_", toupper(nombre))
      }
      destino <- make.unique(c(names(resultado), destino), sep = "_")[length(resultado) + 1L]
      resultado[[destino]] <- if (directa) valores else valores[fila]
      fuentes[[destino]] <- nombre
    }
  }
  list2DF(resultado)
}

# Flujo de la funcion ----------------------------------------------------------

enoe_obtener <- function(year, trimestre, integrar, tabla, estado) {
  elegido <- enoe_elegir(year, trimestre)
  integrar <- validar_logico(integrar, "integrar")
  pedidas <- enoe_validar_tabla(tabla)
  entidades <- gobierno_entidades(estado)

  zip <- enoe_descargar(elegido)
  if (!isTRUE(getOption("importinegi.cache", FALSE))) {
    on.exit(unlink(zip), add = TRUE)
  }
  contenido <- enoe_contenido(zip)
  faltan <- setdiff(pedidas, names(contenido$tablas))
  if (length(faltan) > 0) {
    detener(
      "El archivo de la ENOE del ", elegido$etiqueta, " no contiene ",
      if (length(faltan) == 1) "la tabla " else "las tablas ", enumerar(faltan),
      ". Contiene: ", enumerar(contenido$archivos, maximo = 15), "."
    )
  }
  nota <- enoe_nota(zip, contenido$notas)
  if (!is.null(nota)) {
    message("Nota del INEGI sobre los archivos del ", elegido$etiqueta, ":\n", nota)
  }

  # Las tablas se guardan en un entorno para que la integracion pueda liberar
  # cada una en cuanto la usa.
  nivel <- enoe_nivel()
  almacen <- new.env(parent = emptyenv())
  llaves <- list()
  for (nombre in pedidas) {
    almacen[[nombre]] <- enoe_leer_tabla(zip, contenido$tablas[[nombre]], entidades)
    columnas <- names(almacen[[nombre]])
    llaves[[nombre]] <- intersect(enoe_llave(columnas, nivel[[nombre]]), columnas)
  }
  con_atributos <- function(resultado, llave) {
    attr(resultado, "fuente") <- elegido$url
    attr(resultado, "llave") <- llave
    resultado
  }

  if (length(pedidas) == 1) {
    resultado <- con_atributos(almacen[[pedidas]], llaves[[pedidas]])
  } else if (integrar) {
    resultado <- con_atributos(enoe_integrar(almacen), llaves[[enoe_orden(pedidas)[1]]])
  } else {
    resultado <- lapply(pedidas, function(nombre) con_atributos(almacen[[nombre]], llaves[[nombre]]))
    names(resultado) <- pedidas
    resultado <- con_atributos(resultado, llaves)
  }
  if (!is.null(nota)) {
    attr(resultado, "nota") <- nota
  }
  resultado
}
