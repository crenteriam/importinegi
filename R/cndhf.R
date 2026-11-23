#' Censo Nacional de Derechos Humanos Federal
#'
#' Descarga las bases de datos del Censo Nacional de Derechos Humanos Federal (CNDHF), de 2017 a 2025.
#'
#' El CNDHF es un proyecto estadistico sobre la gestion y desempeño de la Comision Nacional de los Derechos Humanos (CNDH). Se levanta cada año y pregunta por el año anterior: la edicion 2025 informa de 2024.
#'
#' El censo se organiza en modulos y cada modulo, en temas. El INEGI publica un archivo por tema, con varias tablas. Los argumentos \code{modulo} y \code{tema} eligen el archivo, y \code{tabla}, las tablas que se devuelven. El CNDHF tiene dos modulos:
#' \describe{
#'   \item{\code{"estructura"}}{Estructura organizacional y recursos de la CNDH: consejo consultivo, estructura organizacional, recursos humanos y presupuestales, transparencia, control interno y anticorrupcion, entre otros temas.}
#'   \item{\code{"proteccion"}}{Capacitacion, difusion, defensa y proteccion de los derechos humanos: solicitudes de queja, expedientes atendidos, calificados, concluidos y pendientes, seguimiento y hechos violatorios, entre otros temas. De 2017 a 2019 el INEGI lo titula "Ejercicio de la funcion en materia de proteccion de derechos humanos".}
#' }
#'
#' Los temas cambian de una edicion a otra: cada edicion tiene entre 17 y 22, y el INEGI los reorganizo en 2020 y en 2024. \code{\link{bases_importinegi}} los lista sin descargar nada, por ejemplo con \code{bases_importinegi("cndhf", 2025)}. El argumento \code{tema} acepta una parte del titulo que el INEGI le da al tema o una parte del nombre de su archivo, sin distinguir mayusculas ni acentos. Si el texto es igual al titulo o al nombre del archivo de un tema, la funcion elige ese tema; si no, el texto debe coincidir con un solo tema del modulo. Cuando coincide con varios temas o con ninguno, la funcion se detiene y muestra los temas del modulo.
#'
#' La funcion toma la direccion de cada archivo del registro de archivos del INEGI que se distribuye con el paquete, descarga el archivo en una carpeta temporal y lo lee. Usa los archivos de datos abiertos del INEGI, en formato CSV. Requiere conexion a internet.
#'
#' El censo tiene un solo informante, la CNDH. Por eso las tablas no tienen una clave que identifique al informante y la funcion no tiene el argumento \code{estado}.
#'
#' @section Tablas de cada tema:
#' Cada tema tiene entre 1 y 35 tablas. Si no se indica \code{tabla}, la funcion devuelve una lista con todas las tablas del tema; cada tabla se extrae de la lista con su nombre o con su posicion. Si se indica una tabla, la funcion devuelve solo esa tabla, como data.frame.
#'
#' Hasta la edicion 2019, las tablas tienen nombres cortos (\code{"partciud"}). Desde la edicion 2020 hay una tabla por cada pregunta del cuestionario: en 2020 su nombre indica el modulo y la pregunta (\code{"m1p2"}) y, desde 2021, el modulo, la seccion y la pregunta (\code{"m1s1p4"} es la pregunta 4 de la seccion 1 del modulo 1). Desde la edicion 2020, cada archivo incluye un indice que describe el contenido de cada tabla; se obtiene con \code{tabla = "indice"}.
#'
#' La mayoria de las tablas son pequeñas: un total, las categorias de una clasificacion (por ejemplo, el tipo de hecho violatorio) con sus cantidades en columnas, o una lista de personas u organos.
#'
#' @section Contenido de las tablas:
#' Las columnas conservan los nombres que les da el INEGI. La mayoria de las variables de clasificacion estan codificadas. La funcion devuelve los codigos y no los sustituye por su descripcion. Cada archivo incluye un catalogo por cada variable codificada, con el mismo nombre que la columna; \code{tabla = "catalogos"} los devuelve.
#'
#' El INEGI escribe siglas en lugar de un dato: \code{"NA"} (no aplica), \code{"NSS"} (el informante no cuenta con elementos para responder), \code{"ND"} (informacion no disponible) y \code{"NP"} (no publicable por confidencialidad o por reserva de informacion). La funcion convierte \code{"NA"} en \code{NA} y conserva las otras siglas. Tambien convierte en \code{NA} el texto \code{"NULL"}, escrito con mayusculas o con minusculas, que los diccionarios de datos del INEGI listan entre los valores validos sin definirlo. Por eso, las columnas de cantidades que contienen \code{"NSS"}, \code{"ND"} o \code{"NP"} se devuelven como texto; las columnas que solo contienen numeros se devuelven como numero.
#'
#' Desde la edicion 2020, algunas tablas desglosan la informacion por entidad federativa o por municipio. En las ediciones de 2020 a 2022, la clave del municipio viene en una sola columna (\code{ubicageo_b} en 2020, \code{ubicageo_c} en 2021 y \code{ubicageo_e} en 2022), que se devuelve como texto de 5 digitos, con ceros a la izquierda (\code{"01001"}). Desde la edicion 2022, la clave de la entidad viene en la columna \code{enticve1}, que se devuelve como texto de 2 digitos (\code{"01"}), y desde la edicion 2023, la clave del municipio viene en la columna \code{municve1}, que se devuelve como texto de 3 digitos (\code{"001"}); al unir estas dos columnas se obtiene la clave de 5 digitos del municipio. Las columnas de entidad que tienen catalogo (por ejemplo, \code{entfede_c}) se devuelven como numero, igual que en su catalogo.
#'
#' @param year Año del levantamiento del censo en formato numerico. Es obligatorio. Los años disponibles van de 2017 a 2025.
#' @param modulo Modulo del censo en formato alfanumerico. Es obligatorio. Las opciones son: \code{"estructura"} y \code{"proteccion"}.
#' @param tema Tema del modulo en formato alfanumerico: una parte del titulo que le da el INEGI (\code{"consejo consultivo"}, \code{"expedientes calificados"}) o del nombre de su archivo (\code{"exped_calificado"}), sin distinguir mayusculas ni acentos. Si se omite, la funcion se detiene y muestra los temas del modulo.
#' @param tabla Tabla o tablas del tema que se devuelven, con el nombre que les da el INEGI (\code{"partciud"}, \code{"m1s1p4"}). Por omision, todas. Tres valores devuelven la documentacion del tema en lugar de sus datos: \code{"indice"}, \code{"catalogos"} y \code{"diccionario"}.
#'
#' @examples
#'
#' # Consulta los temas del CNDHF 2025, con su modulo y el tamaño de cada descarga
#' temas <- bases_importinegi("cndhf", 2025)
#'
#' # Descarga las tablas del Consejo Consultivo de la CNDH en 2025 y extrae una tabla
#' \dontrun{consejo <- cndhf(year = 2025, modulo = "estructura", tema = "consejo consultivo")}
#' \dontrun{consejeras <- consejo$m1s1p2}
#'
#' # Descarga una sola tabla: hechos presuntamente violatorios, por tipo de hecho
#' \dontrun{hechos <- cndhf(2025, "proteccion", "expedientes calificados", tabla = "m2s5p24")}
#'
#' # Indice de las tablas del tema y catalogos para interpretar los codigos
#' \dontrun{indice <- cndhf(2025, "proteccion", "expedientes calificados", tabla = "indice")}
#' \dontrun{catalogos <- cndhf(2025, "proteccion", "expedientes calificados", tabla = "catalogos")}
#'
#' # El tema tambien se puede indicar con el nombre de su archivo
#' \dontrun{participacion <- cndhf(2017, "estructura", "participacion_ciudada")}
#'
#' @return Si no se indica \code{tabla}, o si se indican varias tablas, una lista con un data.frame por tabla, cada uno con el nombre de su tabla. Si se indica una sola tabla, un data.frame. Con \code{tabla = "indice"}, un data.frame con el nombre de cada tabla y, desde la edicion 2020, su contenido. Con \code{tabla = "catalogos"}, una lista con los catalogos del tema. Con \code{tabla = "diccionario"}, un data.frame con la descripcion de las variables de cada tabla (ediciones de 2020 en adelante; en las anteriores, el diccionario es un documento que viene dentro del archivo del tema). El atributo \code{"fuente"} contiene la direccion del archivo descargado y, en las listas de tablas, el atributo \code{"indice"} contiene el indice del tema.
#' @seealso \code{\link{cngmd}} para el censo de los gobiernos municipales, que se usa de la misma manera. \code{\link{bases_importinegi}} para consultar, sin descargar nada, los temas de cada modulo y edicion y cuanto pesan. \code{\link{catalogo_inegi}} para la documentacion del censo en la Red Nacional de Metadatos.
#' @export
cndhf <- function(year, modulo, tema = NULL, tabla = NULL) {
  if (missing(year)) {
    year <- NULL
  }
  if (missing(modulo)) {
    modulo <- NULL
  }
  gobierno_obtener("cndhf", year, modulo, tema, tabla)
}
