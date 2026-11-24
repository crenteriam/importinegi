#' Conteo de Poblacion y Vivienda
#'
#' Descarga las bases de datos de los Conteos de Poblacion y Vivienda del INEGI (1995 y 2005) y las devuelve como data.frame.
#'
#' Los Conteos de Poblacion y Vivienda se levantaron a la mitad del periodo entre dos censos. Una sola funcion da acceso a todas sus bases de datos. El argumento \code{base} elige cual:
#' \describe{
#'   \item{\code{"iter"}}{Principales resultados por localidad (ITER). Conteos de 1995 y 2005. Tiene archivo nacional.}
#'   \item{\code{"muestra"}}{Microdatos de una muestra de viviendas y de sus ocupantes. Conteos de 1995 y 2005. Se publica por entidad. Las muestras de 1995 y de 2005 se obtuvieron de forma distinta (ver la seccion "Como se levantaron los conteos").}
#'   \item{\code{"viviendas"}}{Viviendas habitadas y deshabitadas por localidad. Conteo de 2005. Tiene archivo nacional.}
#' }
#'
#' Los Censos de Poblacion y Vivienda se descargan con \code{\link{cpv_censo}} y las Encuestas Intercensales, con \code{\link{cpv_intercensal}}.
#'
#' La funcion toma la direccion de cada archivo del registro de archivos del INEGI que se distribuye con el paquete, descarga los archivos en una carpeta temporal y los lee en formato DBF. Requiere conexion a internet.
#'
#' @section Como se levantaron los conteos:
#' El Conteo de 1995 tuvo dos operativos: una enumeracion de toda la poblacion, con un cuestionario basico, y una encuesta por muestreo, con un cuestionario ampliado que se aplico en una muestra de viviendas para obtener informacion mas detallada. Se levanto del 23 de octubre al 18 de noviembre de 1995. La base \code{"muestra"} de 1995 contiene los microdatos de esa encuesta.
#'
#' El Conteo de 2005 se levanto del 4 al 29 de octubre de 2005, con dos semanas adicionales para recuperar las viviendas pendientes. Su muestra no proviene de una encuesta aparte: es un extracto del archivo completo del Conteo, del que el INEGI tomo de forma sistematica el 10 por ciento de las viviendas particulares de cada entidad, con todos sus ocupantes (las viviendas colectivas quedaron fuera). Por eso tiene las mismas preguntas que el resto del Conteo de 2005. Se obtuvo de la misma forma que la muestra del Censo de 1990, y no como la muestra de 1995 ni las muestras censales de 2000 en adelante, que provienen de un cuestionario ampliado.
#'
#' La muestra de 1995 trae los factores de expansion de las personas (\code{FAC_POB}) y de las viviendas (\code{FAC_VIV}). La de 2005 no trae factor de expansion: como se tomo 1 de cada 10 viviendas, cada vivienda de la muestra representa a 10 viviendas y cada persona, a 10 personas.
#'
#' El INEGI publica las muestras por entidad. Si no se indica \code{estado}, la funcion descarga la de Aguascalientes y avisa cuanto pesaria la de todo el pais; con \code{estado = "nacional"} descarga las 32 entidades y las combina. La muestra nacional de 2005 pesa cerca de 120 MB y tiene unos 10 millones de registros de personas: la descarga puede tardar varios minutos y combinarla requiere varios GB de memoria.
#'
#' @section Niveles de agregacion:
#' Las bases \code{"iter"} y \code{"viviendas"} mezclan filas de detalle con filas de totales. El INEGI identifica los totales con claves en ceros: entidad \code{"00"} para el total nacional, municipio \code{"000"} para el total de la entidad y localidad \code{"0000"} para el total del municipio. El argumento \code{nivel} elige que filas se conservan; por omision, solo las localidades.
#'
#' En el ITER de 1995, las localidades de una y de dos viviendas no aparecen una por una: solo estan sumadas en las filas con clave de localidad \code{"9998"} y \code{"9999"} de cada municipio, que se repiten en las de la entidad. Por eso, la suma de las localidades de 1995 es un poco menor que el total de la entidad. Con los agregados de los municipios, la suma coincide con el total en 29 entidades; en Chihuahua, Oaxaca y Queretaro difiere en menos de 0.3 por ciento. En el ITER de 2005, como en los censos, esas localidades si aparecen una por una y las filas \code{"9998"} y \code{"9999"} repiten su poblacion. En los dos conteos, esas filas solo se incluyen con \code{nivel = "todos"}.
#'
#' @section Contenido de la tabla:
#' Las columnas conservan los nombres que les da el INEGI. Las claves geograficas (entidad, municipio y localidad) son texto y conservan los ceros a la izquierda (\code{"09"}, \code{"001"}). Las columnas que solo contienen numeros se convierten a numero, salvo las que tienen valores con ceros a la izquierda, que se conservan como texto porque suelen ser codigos. En la muestra, en cambio, esas columnas tambien se convierten a numero (la edad \code{"008"} pasa a ser \code{8}), salvo las variables del diseño muestral de 1995 (\code{ESTRATO} y \code{UPM}). En las bases \code{"iter"} y \code{"viviendas"}, los datos que el INEGI reserva por confidencialidad (\code{"*"}) o marca como no disponibles (\code{"N/D"}) se convierten en \code{NA}.
#'
#' @param year Año del conteo, en formato numerico. Es obligatorio: 1995 o 2005.
#' @param base Base de datos que se descarga: \code{"iter"} (por omision), \code{"muestra"} o \code{"viviendas"} (solo 2005).
#' @param estado Entidad federativa. Acepta el nombre (\code{"Jalisco"}, \code{"Distrito Federal"}), la abreviatura (\code{"jal"}, \code{"df"}) o la clave del INEGI (\code{14}, \code{"09"}), sin distinguir mayusculas ni acentos. Acepta varias entidades a la vez, que se combinan en una sola tabla. \code{"nacional"} devuelve la base de todo el pais: en \code{"iter"} y \code{"viviendas"} descarga el archivo nacional; en \code{"muestra"}, que el INEGI publica solo por entidad, descarga las 32 entidades y las combina. Por omision, \code{"nacional"} en \code{"iter"} y \code{"viviendas"}, y \code{"Aguascalientes"} en \code{"muestra"}, con un aviso de cuanto pesaria la descarga de todo el pais.
#' @param tabla Solo para \code{base = "muestra"}. En 1995: \code{"personas"} (por omision) o \code{"migrantes"}. En 2005: \code{"personas"} (por omision), \code{"viviendas"} o \code{"hogares"}.
#' @param nivel Solo para las bases \code{"iter"} y \code{"viviendas"}: \code{"nacional"}, \code{"entidad"}, \code{"municipio"} y \code{"localidad"}. Acepta varios niveles, o \code{"todos"} para conservar todas las filas. Por omision, el nivel de mayor detalle.
#'
#' @examples
#'
#' # ITER del Conteo 2005, todo el pais.
#' \dontrun{iter <- cpv_conteo(2005)}
#'
#' # Personas de la muestra del Conteo 1995 en Aguascalientes (entidad por omision).
#' \dontrun{personas <- cpv_conteo(1995, base = "muestra")}
#'
#' # Hogares de la muestra del Conteo 2005 en Yucatan.
#' \dontrun{hogares <- cpv_conteo(2005, base = "muestra", estado = "Yucatan", tabla = "hogares")}
#'
#' # Muestra del Conteo 2005 de todo el pais: descarga y combina las 32 entidades.
#' \dontrun{nacional <- cpv_conteo(2005, base = "muestra", estado = "nacional")}
#'
#' @return Data.frame con la base de datos solicitada. El atributo \code{"fuente"} contiene la direccion de los archivos descargados.
#' @seealso \code{\link{bases_importinegi}} para consultar, sin descargar nada, que bases existen para cada conteo y cuanto pesan. \code{\link{cpv_censo}} e \code{\link{cpv_intercensal}} para los Censos de Poblacion y Vivienda y las Encuestas Intercensales. La viñeta del paquete, \code{vignette("importinegi")}, explica la funcion y como se levantaron los conteos.
#' @export

cpv_conteo <- function(year, base = "iter", estado = NULL, tabla = NULL, nivel = NULL) {
  if (missing(year)) {
    year <- NULL
  }
  poblacion_obtener("conteo", year, base, estado, tabla, nivel)
}
