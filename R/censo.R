#' Censo de Poblacion y Vivienda
#'
#' Descarga las bases de datos de los Censos de Poblacion y Vivienda del INEGI y las devuelve como data.frame.
#'
#' Una sola funcion da acceso a todas las bases de datos de cada censo. El argumento \code{base} elige cual:
#' \describe{
#'   \item{\code{"iter"}}{Principales resultados por localidad (ITER). Censos de 1990, 2000 y 2010. Tiene archivo nacional.}
#'   \item{\code{"ageb"}}{Principales resultados por AGEB y manzana urbana. Censo de 2010. Se publica por entidad.}
#'   \item{\code{"muestra"}}{Microdatos de la muestra (cuestionario ampliado). Censos de 1990, 2000 y 2010. Se publica por entidad.}
#'   \item{\code{"localidades"}}{Resultados sobre localidades con menos de 5 mil habitantes. Censo de 2010. Tiene archivo nacional.}
#'   \item{\code{"entorno"}}{Resultados sobre infraestructura y caracteristicas del entorno urbano, por manzana. Censo de 2010. Se publica por entidad.}
#' }
#'
#' Los Conteos de Poblacion y Vivienda (1995 y 2005) y las Encuestas Intercensales (2015 y 2025) no son censos y no se descargan con esta funcion.
#'
#' La funcion toma la direccion de cada archivo del registro de archivos del INEGI que se distribuye con el paquete, descarga los archivos en una carpeta temporal y los lee. Requiere conexion a internet.
#'
#' @section Censo 2020:
#' El registro de archivos de esta version todavia no incluye el Censo 2020, por lo que \code{censo(2020)} se detiene con un mensaje que lo indica. Las bases de 2020 se habilitaran al actualizar el registro.
#'
#' @section Niveles de agregacion:
#' Las bases \code{"iter"} y \code{"ageb"} mezclan filas de detalle con filas de totales. El INEGI identifica los totales con claves en ceros: municipio \code{"000"} para el total de la entidad, localidad \code{"0000"} para el total del municipio, AGEB \code{"0000"} para el total de la localidad urbana y manzana \code{"000"} para el total del AGEB. El argumento \code{nivel} elige que filas se conservan; por omision, solo las de mayor detalle (localidades en \code{"iter"}, manzanas en \code{"ageb"}).
#'
#' En el ITER, las filas con clave de localidad \code{"9998"} y \code{"9999"} son agregados de las localidades de una y de dos viviendas. No son localidades, asi que solo se incluyen con \code{nivel = "todos"}.
#'
#' @section Contenido de la tabla:
#' Las columnas conservan los nombres que les da el INEGI. Las claves geograficas (entidad, municipio, localidad, AGEB y manzana) son texto y conservan los ceros a la izquierda (\code{"09"}, \code{"001"}). Las columnas que solo contienen numeros se convierten a numero, salvo las que tienen valores con ceros a la izquierda (\code{"0020"}), que se conservan como texto porque suelen ser codigos. En las bases \code{"iter"}, \code{"ageb"} y \code{"localidades"}, los datos que el INEGI reserva por confidencialidad (\code{"*"}) o marca como no disponibles (\code{"N/D"}) se convierten en \code{NA}.
#'
#' @param year Año del censo, en formato numerico. Es obligatorio. Los censos disponibles son 1990, 2000, 2010 y 2020 (ver la seccion sobre el Censo 2020).
#' @param base Base de datos que se descarga: \code{"iter"} (por omision), \code{"ageb"}, \code{"muestra"}, \code{"localidades"} o \code{"entorno"}. No todas las bases existen para todos los censos (ver Detalles).
#' @param estado Entidad federativa. Acepta el nombre (\code{"Jalisco"}, \code{"Ciudad de Mexico"}), la abreviatura (\code{"jal"}, \code{"cdmx"}) o la clave del INEGI (\code{14}, \code{"09"}), sin distinguir mayusculas ni acentos. Acepta varias entidades a la vez, que se combinan en una sola tabla. Por omision, \code{"nacional"} descarga el archivo nacional, en las bases que lo tienen. \code{"todas"} descarga las 32 entidades y las combina.
#' @param tabla Solo para \code{base = "muestra"}: tabla de la muestra que se descarga, \code{"personas"} (por omision), \code{"viviendas"} o \code{"migrantes"}. La muestra de 1990 se publica en una sola tabla y no usa este argumento.
#' @param nivel Solo para las bases \code{"iter"} y \code{"ageb"}: niveles de agregacion que se conservan. En \code{"iter"}: \code{"nacional"}, \code{"entidad"}, \code{"municipio"} y \code{"localidad"}. En \code{"ageb"}: \code{"entidad"}, \code{"municipio"}, \code{"localidad"}, \code{"ageb"} y \code{"manzana"}. Acepta varios niveles, o \code{"todos"} para conservar todas las filas. Por omision, el nivel de mayor detalle.
#'
#' @examples
#'
#' # Principales resultados por localidad (ITER) del Censo 2010, todo el pais.
#' \dontrun{iter <- censo(2010)}
#'
#' # ITER de Colima: solo los totales de la entidad y de sus municipios.
#' \dontrun{colima <- censo(2010, estado = "Colima", nivel = c("entidad", "municipio"))}
#'
#' # Resultados por manzana urbana de Jalisco.
#' \dontrun{manzanas <- censo(2010, base = "ageb", estado = "Jalisco")}
#'
#' # Viviendas de la muestra del Censo 2010 en dos entidades.
#' \dontrun{
#' viviendas <- censo(2010, base = "muestra", estado = c("Colima", "Nayarit"),
#'                    tabla = "viviendas")
#' }
#'
#' # Direccion de los archivos que se descargaron.
#' \dontrun{attr(iter, "fuente")}
#'
#' @return Data.frame con la base de datos solicitada. El atributo \code{"fuente"} contiene la direccion de los archivos descargados.
#' @seealso \code{\link{catalogo_inegi}} para consultar la documentacion de cada censo en la Red Nacional de Metadatos.
#' @export

censo <- function(year, base = "iter", estado = "nacional", tabla = NULL, nivel = NULL) {
  if (missing(year)) {
    year <- NULL
  }
  poblacion_obtener("censo", year, base, estado, tabla, nivel)
}
