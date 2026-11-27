#' ETOE
#'
#' Encuesta Telefonica de Ocupacion y Empleo (ETOE)
#'
#' La ETOE es un proyecto estadistico de encuestas en hogares especializado en informacion sobre el mercado laboral. El INEGI la levanto en abril, mayo y junio de 2020, cuando suspendio las entrevistas en las viviendas de la Encuesta Nacional de Ocupacion y Empleo (ENOE) por la emergencia sanitaria de COVID-19. La ETOE provee informacion mensual sobre la fuerza laboral, la ocupacion, subocupacion y desocupacion de los miembros del hogar encuestado, y cubre el segundo trimestre de 2020, que la ENOE no tiene (ver \code{\link{enoe}}).
#'
#' La ETOE usa el mismo diseño conceptual y el mismo cuestionario basico de ocupacion y empleo que la ENOE. En abril y mayo, su muestra es una parte de las viviendas de la ENOE del primer trimestre de 2020 que tenian telefono; en junio se agregaron entrevistas en las viviendas. El INEGI presenta la ETOE con alcance nacional y advierte que sus cifras no son estrictamente comparables con las de la ENOE. \code{\link{bases_importinegi}} lista los archivos disponibles sin descargar nada: \code{bases_importinegi("etoe")}.
#'
#' Cada mes se publica en cinco bases de datos (tablas), las mismas de la ENOE, que se eligen con el argumento \code{tabla}:
#' \describe{
#'   \item{\code{"viv"}}{Vivienda: una fila por vivienda seleccionada.}
#'   \item{\code{"hog"}}{Hogar: una fila por hogar, incluidos los hogares que no se entrevistaron.}
#'   \item{\code{"sdem"}}{Sociodemografico: una fila por residente del hogar, de cualquier edad.}
#'   \item{\code{"coe1"} y \code{"coe2"}}{Cuestionario de ocupacion y empleo (version basica), primera y segunda parte: una fila por residente de 12 años o mas.}
#' }
#'
#' La funcion toma la direccion del archivo del registro de archivos del INEGI que se distribuye con el paquete, descarga el archivo nacional del mes (de 2 a 3 MB) en una carpeta temporal y lee las bases de datos en formato DBF. El INEGI no publica la ETOE por entidad federativa: con el argumento \code{estado}, la funcion descarga el archivo nacional y conserva las filas de las entidades indicadas. La muestra es pequeña para hacer estimaciones por entidad: en algunas entidades tiene unas decenas de viviendas por mes. Requiere conexion a internet.
#'
#' @section Ponderadores:
#' El INEGI publica cada mes en dos versiones, que tienen las mismas filas y difieren en los ponderadores. La version se elige con el argumento \code{poblacion}:
#' \describe{
#'   \item{\code{"cpv2020"}}{Ponderadores calculados con la estimacion de poblacion del INEGI con base en el Censo de Poblacion y Vivienda 2020. Se llaman \code{FAC_NP}, \code{FAC32_NP} y \code{FAC39_NP}. Es la version mas reciente y la que se descarga por omision.}
#'   \item{\code{"conapo"}}{Ponderadores calculados con las proyecciones de poblacion del CONAPO. Se llaman \code{FAC}, \code{FAC32} y \code{FAC39}. Es la version que el INEGI publico en 2020: con ella se reproducen las cifras de los boletines de ese año.}
#' }
#'
#' El ponderador indica cuantas personas, hogares o viviendas representa cada fila de la muestra, y las estimaciones deben usarlo. Cada base de datos tiene tres. \code{FAC_NP} (o \code{FAC}) sirve para las estimaciones nacionales. \code{FAC32_NP} y \code{FAC39_NP} (o \code{FAC32} y \code{FAC39}) sirven para las estimaciones del agregado de 32 y de 39 ciudades, y valen cero en las filas que no son de esas ciudades. Las cifras nacionales que publica el INEGI se reproducen con el ponderador de la base de datos \code{"sdem"} y los residentes con entrevista completa (\code{R_DEF} igual a \code{"00"}), sin los ausentes definitivos (\code{C_RES} igual a \code{"1"} o \code{"3"}) y de 15 a 98 años de edad (\code{EDA}).
#'
#' Las bases de datos de vivienda, de hogar y sociodemografica tienen cada una sus ponderadores. Como en \code{\link{enoe}}, la base de datos integrada mantiene los de los tres niveles. Los ponderadores del residente conservan su nombre (\code{FAC_NP}, \code{FAC32_NP} y \code{FAC39_NP}); los del hogar y los de la vivienda llevan el sufijo de su base de datos (\code{FAC_NP_HOG}, \code{FAC32_NP_HOG}, \code{FAC39_NP_HOG}, \code{FAC_NP_VIV}, \code{FAC32_NP_VIV} y \code{FAC39_NP_VIV}). Las estimaciones de personas usan los ponderadores del residente; las de hogares, los del hogar, y las de viviendas, los de la vivienda. En la ETOE, los ponderadores de cada residente son iguales a los de su hogar; los del hogar y los de la vivienda solo difieren en hogares que no se entrevistaron. Los hogares sin residentes tienen \code{NA} en los ponderadores del residente.
#'
#' @section Como unir las bases de datos:
#' Las bases de datos de un mes se unen como las de la ENOE hasta el primer trimestre de 2020: con las columnas que identifican a la vivienda (\code{CD_A}, \code{ENT}, \code{CON} y \code{V_SEL}), al hogar (las de la vivienda, \code{N_HOG} y \code{H_MUD}) y al residente (las del hogar y \code{N_REN}). Son las mismas en los tres meses y en las dos versiones de los ponderadores. El atributo \code{"llave"} del resultado indica las columnas que identifican cada fila.
#'
#' Con \code{integrar = TRUE}, la funcion hace esa union con las reglas de \code{\link{enoe}} y devuelve una sola base de datos:
#' \itemize{
#'   \item Tiene una fila por residente. Los residentes sin cuestionario de ocupacion y empleo (los menores de 12 años, principalmente) quedan con \code{NA} en las columnas del cuestionario.
#'   \item No se pierde ninguna fila. Los hogares sin residentes, que son hogares que no se entrevistaron (entre 5 mil y 6.3 mil en cada mes), se agregan al final, con \code{NA} en las columnas de los residentes. Se reconocen porque su \code{N_REN} es \code{NA}; su \code{R_DEF} indica el resultado de la entrevista. Los hogares con entrevista suspendida (\code{R_DEF} igual a \code{"15"}) si tienen residentes y aparecen en las filas de sus residentes.
#'   \item Las columnas que se repiten en varias bases de datos quedan una sola vez, con el valor de la base de datos de mas detalle.
#'   \item Las preguntas de vivienda y de hogar llevan el sufijo de su base de datos (\code{P1_VIV}, \code{P2_VIV}, \code{P3_VIV}, \code{P4_1_HOG} y \code{P4_2_HOG}), porque se llaman igual que otras preguntas del cuestionario de ocupacion y empleo.
#'   \item Se mantienen los ponderadores del residente, del hogar y de la vivienda (ver la seccion Ponderadores).
#'   \item Con el argumento \code{tabla}, se integran solamente las bases de datos indicadas. Por ejemplo, \code{tabla = c("hog", "viv")} devuelve una fila por hogar.
#' }
#'
#' @section Contenido de las bases de datos:
#' Salvo por los ponderadores, las bases de datos de abril y mayo tienen las mismas columnas que las de la ENOE del cuarto trimestre de 2019, con los mismos nombres y tipos. Las de junio tienen ademas \code{NUPM} (numero consecutivo de la unidad primaria de muestreo) y, en la version \code{"cpv2020"}, \code{TIPO}, que distingue las entrevistas en la vivienda (\code{"1"}) de las telefonicas (\code{"2"}). La columna \code{EST_D} es numero en abril y mayo, y texto en junio. La columna \code{T_LOC} (tamaño de localidad) esta en blanco en los tres meses.
#'
#' Como en \code{\link{enoe}}, los nombres de las columnas son los del INEGI, en mayusculas, y las columnas conservan el tipo que les da el INEGI: las que declara como texto se devuelven como texto, con sus ceros a la izquierda (las columnas de identificacion y las respuestas de los cuestionarios, incluida la edad, \code{EDA}), y las que declara como numero, como numero (los ponderadores y las variables precodificadas de la base de datos \code{"sdem"}). Los datos que el INEGI deja en blanco se devuelven como \code{NA}. Los codigos no se sustituyen por sus descripciones.
#'
#' Ninguna columna indica el mes: \code{PER} vale \code{"220"} (segundo trimestre de 2020) en los tres meses. Para apilar varios meses hay que agregar antes una columna con el mes (ver los ejemplos); para apilar junio con abril o mayo hay que igualar ademas sus columnas, porque las de junio no son las mismas. La muestra es un panel: cuatro de cada cinco viviendas estan en los tres meses, asi que al apilar los meses, las mismas viviendas, hogares y residentes aparecen varias veces.
#'
#' La direccion del archivo descargado queda en el atributo \code{"fuente"}. Si el archivo trae una nota del INEGI, como las de algunos trimestres de la ENOE, la funcion la muestra como mensaje y la guarda en el atributo \code{"nota"}; en noviembre de 2026, ningun archivo de la ETOE la trae.
#'
#' @param year Año de levantamiento de la encuesta en formato numerico. La ETOE solo se levanto en 2020.
#' @param mes Mes de levantamiento de la encuesta en formato alfanumerico. Las opciones son: "abril", "mayo" y "junio". Tambien se acepta el numero del mes, de 4 a 6.
#' @param integrar \code{FALSE}: descarga por separado y en una lista las cinco bases de datos que componen la ETOE. \code{TRUE}: integra las cinco bases de datos en una sola, utilizando el identificador unico del entrevistado. La base de datos integrada tiene una fila por residente, conserva los hogares sin residentes y mantiene los ponderadores del residente, del hogar y de la vivienda (ver las secciones Como unir las bases de datos y Ponderadores).
#' @param tabla Tabla o tablas que se devuelven: \code{"viv"} (vivienda), \code{"hog"} (hogar), \code{"sdem"} (sociodemografico), \code{"coe1"} y \code{"coe2"} (cuestionario de ocupacion y empleo, primera y segunda parte). Por omision, las cinco.
#' @param estado Entidad federativa. Acepta el nombre (\code{"Jalisco"}, \code{"Ciudad de Mexico"}), la abreviatura (\code{"jal"}, \code{"cdmx"}) o la clave del INEGI (\code{14}, \code{"09"}), sin distinguir mayusculas ni acentos, y varias entidades a la vez. La funcion descarga el archivo nacional, conserva las filas de las entidades indicadas y avisa que la encuesta tiene alcance nacional. Por omision, todo el pais.
#' @param poblacion Version de los ponderadores: \code{"cpv2020"} (estimacion de poblacion con base en el Censo de Poblacion y Vivienda 2020) o \code{"conapo"} (proyecciones de poblacion del CONAPO). Por omision, \code{"cpv2020"} (ver la seccion Ponderadores).
#'
#' @examples
#'
#' # Descargar las bases de datos de la ETOE de abril de 2020, sin integrar.
#' \dontrun{etoe(year = 2020, mes = "abril")}
#'
#' # Descargar las bases de datos de la ETOE de junio de 2020, integradas
#' \dontrun{etoe(year = 2020, mes = "junio", integrar = TRUE)}
#'
#' # Descargar solamente la base de datos sociodemografica de mayo de 2020,
#' # con los ponderadores de las proyecciones de poblacion del CONAPO
#' \dontrun{etoe(year = 2020, mes = "mayo", tabla = "sdem", poblacion = "conapo")}
#'
#' # Apilar la base de datos sociodemografica de abril y mayo de 2020,
#' # con una columna que indica el mes
#' \dontrun{
#' abril <- etoe(year = 2020, mes = "abril", tabla = "sdem")
#' mayo <- etoe(year = 2020, mes = "mayo", tabla = "sdem")
#' abril$MES <- "abril"
#' mayo$MES <- "mayo"
#' sdem <- rbind(abril, mayo)
#' }
#' @return Con \code{integrar = FALSE}, una lista con las cinco bases de datos en formato data.frame, con los nombres \code{coe1}, \code{coe2}, \code{hog}, \code{sdem} y \code{viv}. Si en \code{tabla} se indica una sola base de datos, un data.frame; si se indican varias, una lista con esas bases de datos, en el orden indicado. Con \code{integrar = TRUE}, un data.frame. El resultado tiene los atributos \code{"fuente"} y \code{"llave"}.
#' @seealso \code{\link{enoe}} para descargar la Encuesta Nacional de Ocupacion y Empleo.
#' @seealso \code{\link{bases_importinegi}} para consultar los archivos disponibles.
#' @export

etoe <- function(year, mes, integrar = FALSE, tabla = NULL, estado = NULL, poblacion = "cpv2020") {
  if (missing(year)) {
    year <- NULL
  }
  if (missing(mes)) {
    mes <- NULL
  }
  etoe_obtener(year, mes, integrar, tabla, estado, poblacion)
}

# Encuesta Telefonica de Ocupacion y Empleo -------------------------------------
#
# El INEGI publica un zip por mes (abril, mayo y junio de 2020) y por version
# de los ponderadores: seis zip de 2.2 a 2.9 MB, con las mismas cinco tablas en
# DBF que la ENOE. La forma de los archivos se reviso el 27 de noviembre de
# 2026 en las 30 tablas, leidas completas:
#
#   - Columnas: las de la ENOE de 2019-T4 (estructura de 2005-T1 a 2020-T1,
#     cuestionario basico), mas dos ponderadores de ciudades. En junio, ademas,
#     NUPM y, en la version del Censo 2020, TIPO; EST_D es numero en abril y
#     mayo, y texto en junio. T_LOC esta en blanco y PER vale "220" en los tres
#     meses: ninguna columna indica el mes.
#   - Ponderadores: FAC, FAC32 y FAC39 en la version de las proyecciones del
#     CONAPO; FAC_NP, FAC32_NP y FAC39_NP en la de la estimacion de poblacion
#     con base en el Censo 2020. Las dos versiones de un mes tienen las mismas
#     filas, en el mismo orden.
#   - Llave: la de la ENOE hasta 2020-T1 (CD_A, ENT, CON y V_SEL; el hogar
#     agrega N_HOG y H_MUD, y la persona, N_REN) identifica cada fila de las 30
#     tablas. En junio no hace falta TIPO: la llave es la misma en los seis
#     archivos y TIPO se trata como cualquier otra columna.
#   - Nombres de los archivos: COE1T0420.dbf, con la extension en mayusculas o
#     en minusculas; en mayo de la version del Censo 2020, dentro de una
#     carpeta; en junio de esa version, coe1n0620.dbf. Ningun zip trae notas.
#
# La pagina de la encuesta esta en la seccion de investigacion del sitio del
# INEGI (/investigacion/etoe/), no en la de programas.
#
# La lectura y la integracion de las tablas son las de enoe() (R/enoe.R). Aqui
# esta lo propio de la ETOE: los meses del registro, la validacion de los
# argumentos y las filas de bases_importinegi().

# Nombres de los meses, como los escribe el INEGI en las rutas de sus archivos.
etoe_nombres_mes <- function() {
  c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre",
    "noviembre", "diciembre")
}

# Versiones de los ponderadores, como se escriben en `poblacion`, con su
# descripcion. La primera es la que se descarga por omision.
etoe_versiones <- function() {
  c(
    cpv2020 = "estimaci\u00f3n de poblaci\u00f3n con base en el Censo de Poblaci\u00f3n y Vivienda 2020",
    conapo = "proyecciones de poblaci\u00f3n del CONAPO"
  )
}

# Varios textos en uno, para los mensajes: "abril, mayo y junio".
etoe_lista <- function(textos) {
  if (length(textos) < 2) {
    return(paste(textos))
  }
  paste(paste(utils::head(textos, -1), collapse = ", "), "y", utils::tail(textos, 1))
}

# Meses del registro -----------------------------------------------------------

# Archivos de la ETOE en el registro de archivos del paquete: una fila por mes
# y version de los ponderadores. El año, el mes y la version se leen de la ruta
# del archivo ("etoe_2020_abril" es la version de las proyecciones del CONAPO y
# "etoe_2020_abril_cpv2020", la de la estimacion con base en el Censo 2020).
etoe_meses <- function() {
  registro <- registro_inegi()
  patron <- "^/investigacion/etoe/microdatos/etoe_([0-9]{4})_([a-z]+)(_cpv2020)?$"
  propias <- registro$proyecto %in% "etoe" & registro$formato %in% "dbf" & grepl(patron, registro$ruta)
  filas <- registro[propias, , drop = FALSE]
  nombre <- sub(patron, "\\2", filas$ruta)
  meses <- data.frame(
    year = as.integer(sub(patron, "\\1", filas$ruta)),
    mes = match(nombre, etoe_nombres_mes()),
    nombre = nombre,
    poblacion = c("conapo", "cpv2020")[grepl("_cpv2020$", filas$ruta) + 1L],
    url = registro_url(filas),
    tamano = filas$tamano,
    bytes = filas$bytes,
    stringsAsFactors = FALSE
  )
  meses <- meses[!is.na(meses$mes), , drop = FALSE]
  orden <- order(meses$year, meses$mes, match(meses$poblacion, names(etoe_versiones())))
  meses <- meses[orden, , drop = FALSE]
  meses <- meses[!duplicated(meses[c("year", "mes", "poblacion")]), , drop = FALSE]
  rownames(meses) <- NULL
  meses
}

# Periodo de la encuesta para los mensajes: "abril, mayo y junio de 2020".
etoe_periodo <- function(meses) {
  paste(etoe_lista(unique(meses$nombre)), "de", etoe_lista(unique(meses$year)))
}

# Meses de la ETOE con la forma de bases_importinegi(): una fila por mes, para
# que cada fila de esa tabla siga siendo una combinacion distinta de proyecto,
# año y base. `base` es el mes como se escribe en etoe(); la descripcion indica
# las versiones de los ponderadores que tiene el registro, como se escriben en
# `poblacion`, y el tamaño es el del archivo de la primera, que es la que se
# descarga por omision.
etoe_bases <- function() {
  meses <- etoe_meses()
  grupo <- paste(meses$year, meses$mes)
  primera <- !duplicated(grupo)
  versiones <- vapply(grupo[primera], function(g) {
    etoe_lista(paste0("\"", meses$poblacion[grupo == g], "\""))
  }, character(1), USE.NAMES = FALSE)
  veces <- sum(primera)
  data.frame(
    proyecto = rep("etoe", veces),
    year = meses$year[primera],
    modulo = rep(NA_character_, veces),
    base = meses$nombre[primera],
    descripcion = paste0(rep("Versiones de los ponderadores (argumento poblacion): ", veces), versiones),
    tablas = rep(paste(enoe_tablas(), collapse = ", "), veces),
    cobertura = rep("nacional", veces),
    formato = rep("dbf", veces),
    tamano_mb = round(round(meses$bytes[primera]) / 1024^2, 1),
    stringsAsFactors = FALSE
  )
}

# Validacion de los argumentos -------------------------------------------------

etoe_validar_year <- function(year, meses) {
  disponibles <- paste0(
    "La ETOE se levant\u00f3 en ", etoe_periodo(meses), ". Sus archivos se consultan con bases_importinegi(\"etoe\")."
  )
  if (is.null(year) || length(year) != 1 || is.na(year)) {
    detener("Falta indicar el a\u00f1o en `year` (un solo valor). ", disponibles)
  }
  numero <- suppressWarnings(as.integer(as.character(year)))
  if (is.na(numero) || !grepl("^[0-9]{4}$", trimws(as.character(year)))) {
    detener("`year = ", comillas(year), "` no es un a\u00f1o. ", disponibles)
  }
  if (!numero %in% meses$year) {
    detener(
      "La ETOE de ", numero, " no existe: el INEGI solo la levant\u00f3 en ", etoe_periodo(meses),
      ". Los dem\u00e1s periodos se descargan con enoe(), la Encuesta Nacional de Ocupaci\u00f3n y Empleo."
    )
  }
  numero
}

# El mes se acepta con su nombre ("abril"), con sus tres primeras letras
# ("abr") o con su numero (4), sin distinguir mayusculas. Devuelve el numero.
etoe_validar_mes <- function(mes, year, meses) {
  propios <- meses[meses$year == year, , drop = FALSE]
  opciones <- paste0(
    "Opciones: ", etoe_lista(paste0("\"", unique(propios$nombre), "\"")), ", o el n\u00famero del mes (",
    min(propios$mes), " a ", max(propios$mes), ")."
  )
  if (is.null(mes) || length(mes) != 1 || is.na(mes)) {
    detener("Falta indicar el mes en `mes` (un solo valor). ", opciones)
  }
  todos <- etoe_nombres_mes()
  texto <- tolower(trimws(as.character(mes)))
  numero <- if (grepl("^(0?[1-9]|1[0-2])$", texto)) {
    as.integer(texto)
  } else if (texto %in% todos) {
    match(texto, todos)
  } else {
    match(texto, substr(todos, 1, 3))
  }
  if (is.na(numero)) {
    detener("`mes = ", comillas(mes), "` no es un mes. ", opciones)
  }
  if (!numero %in% propios$mes) {
    detener(
      "La ETOE de ", todos[numero], " de ", year, " no existe: el INEGI solo la levant\u00f3 en ", etoe_periodo(meses),
      ". Los dem\u00e1s periodos se descargan con enoe(), la Encuesta Nacional de Ocupaci\u00f3n y Empleo."
    )
  }
  numero
}

etoe_validar_poblacion <- function(poblacion) {
  versiones <- etoe_versiones()
  opciones <- paste0(
    "Opciones: ", etoe_lista(paste0("\"", names(versiones), "\" (", versiones, ")")), "."
  )
  if (!is.character(poblacion) || length(poblacion) != 1 || is.na(poblacion)) {
    detener("`poblacion` debe ser la versi\u00f3n de los ponderadores (un solo valor). ", opciones)
  }
  texto <- tolower(trimws(poblacion))
  if (!texto %in% names(versiones)) {
    detener("`poblacion = ", comillas(poblacion), "` no es una versi\u00f3n de los ponderadores de la ETOE. ", opciones)
  }
  texto
}

# Archivo de un mes y una version de los ponderadores, segun el registro.
etoe_elegir <- function(year, mes, poblacion) {
  meses <- etoe_meses()
  if (nrow(meses) == 0) {
    detener("El registro de archivos del paquete no tiene los archivos de la ETOE.")
  }
  year <- etoe_validar_year(year, meses)
  mes <- etoe_validar_mes(mes, year, meses)
  poblacion <- etoe_validar_poblacion(poblacion)
  etiqueta <- paste(etoe_nombres_mes()[mes], "de", year)
  fila <- meses[meses$year == year & meses$mes == mes & meses$poblacion == poblacion, , drop = FALSE]
  if (nrow(fila) != 1) {
    detener(
      "El registro de archivos del paquete (", registro_generado(), ") no tiene la ETOE de ", etiqueta,
      " con los ponderadores \"", poblacion, "\". Los archivos disponibles se consultan con ",
      "bases_importinegi(\"etoe\")."
    )
  }
  list(year = year, mes = mes, etiqueta = etiqueta, poblacion = poblacion, url = fila$url, tamano = fila$tamano)
}

# Flujo de la funcion ----------------------------------------------------------

etoe_obtener <- function(year, mes, integrar, tabla, estado, poblacion) {
  elegido <- etoe_elegir(year, mes, poblacion)
  integrar <- validar_logico(integrar, "integrar")
  pedidas <- enoe_validar_tabla(tabla, encuesta = "ETOE")
  entidades <- gobierno_entidades(estado)
  if (!is.null(entidades)) {
    message(
      "La ETOE tiene alcance nacional. Su muestra es peque\u00f1a para hacer estimaciones por entidad federativa: ",
      "en algunas entidades tiene unas decenas de viviendas por mes."
    )
  }

  zip <- inegi_descargar(elegido$url, tamano = elegido$tamano)
  if (!isTRUE(getOption("importinegi.cache", FALSE))) {
    on.exit(unlink(zip), add = TRUE)
  }
  # La llave de la ETOE no usa TIPO ni las demas columnas que la ENOE agrega
  # desde 2020-T3 (ver el comentario inicial)
  enoe_leer(zip, elegido$url, paste("de", elegido$etiqueta), pedidas, integrar, entidades,
            encuesta = "ETOE", adicionales = character())
}
