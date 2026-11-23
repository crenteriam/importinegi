#' Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico
#'
#' Descarga las bases de datos del Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico (CNGMD), de 2011 a 2025.
#'
#' El CNGMD es un proyecto estadistico sobre la gestion y desempeño de las entidades gubernamentales mexicanas a nivel municipal. Antes se llamaba Censo Nacional de Gobiernos Municipales y Delegacionales. Se levanta cada dos años y pregunta, sobre todo, por el año anterior: la edicion 2025 informa de 2024.
#'
#' El censo se organiza en modulos y cada modulo, en temas. El INEGI publica un archivo por tema, con varias tablas. Los argumentos \code{modulo} y \code{tema} eligen el archivo, y \code{tabla}, las tablas que se devuelven. El CNGMD tiene nueve modulos:
#' \describe{
#'   \item{\code{"ayuntamiento"}}{Ayuntamientos y, desde 2021, alcaldias de la Ciudad de Mexico. Todas las ediciones.}
#'   \item{\code{"administracion"}}{Administracion publica municipal. Todas las ediciones.}
#'   \item{\code{"catastro"}}{Catastro municipal. Desde 2015.}
#'   \item{\code{"territorio"}}{Planeacion y gestion territorial. Desde 2017.}
#'   \item{\code{"seguridad"}}{Seguridad publica. Todas las ediciones.}
#'   \item{\code{"proteccion_civil"}}{Proteccion civil. Ediciones de 2023 y 2025; de 2015 a 2021 es un tema del modulo \code{"administracion"}.}
#'   \item{\code{"justicia"}}{Justicia municipal y, desde 2021, justicia civica. Todas las ediciones.}
#'   \item{\code{"agua"}}{Agua potable y saneamiento. Todas las ediciones.}
#'   \item{\code{"residuos"}}{Residuos solidos urbanos. Todas las ediciones.}
#' }
#'
#' Los temas cambian de una edicion a otra: el censo paso de 17 temas en 2011 a 130 en 2025. \code{\link{bases_importinegi}} los lista sin descargar nada, por ejemplo con \code{bases_importinegi("cngmd", 2025)}. El argumento \code{tema} acepta una parte del titulo que el INEGI le da al tema o una parte del nombre de su archivo, sin distinguir mayusculas ni acentos. Si el texto es igual al titulo o al nombre del archivo de un tema, la funcion elige ese tema; si no, el texto debe coincidir con un solo tema del modulo. Cuando coincide con varios temas o con ninguno, la funcion se detiene y muestra los temas del modulo.
#'
#' La funcion toma la direccion de cada archivo del registro de archivos del INEGI que se distribuye con el paquete, descarga el archivo en una carpeta temporal y lo lee. Usa los archivos de datos abiertos del INEGI, en formato CSV; los modulos \code{"agua"} y \code{"residuos"} de 2011, que el INEGI publica solo en formato DBF, se leen de ese formato. Todos los archivos son nacionales. Requiere conexion a internet.
#'
#' @section Tablas de cada tema:
#' Cada tema tiene entre 1 y 38 tablas. Si no se indica \code{tabla}, la funcion devuelve una lista con todas las tablas del tema; cada tabla se extrae de la lista con su nombre o con su posicion. Si se indica una tabla, la funcion devuelve solo esa tabla, como data.frame.
#'
#' Hasta la edicion 2019, las tablas tienen nombres cortos (\code{"act_ayun"}, \code{"com_ayun"}). Desde la edicion 2021 hay una tabla por cada pregunta del cuestionario, y su nombre indica el modulo, la seccion y la pregunta: \code{"m1s1p3"} es la pregunta 3 de la seccion 1 del modulo 1. Desde esa edicion, cada archivo incluye un indice que describe el contenido de cada tabla; se obtiene con \code{tabla = "indice"}.
#'
#' @section Contenido de las tablas:
#' Las columnas conservan los nombres que les da el INEGI. La clave del municipio conserva tambien su nombre, que cambia con la edicion y con la tabla (por ejemplo, \code{ubic_geo} en 2011 y \code{cvegeo} en 2025), y se devuelve siempre como texto de 5 digitos, con ceros a la izquierda (\code{"01001"}).
#'
#' La mayoria de las variables estan codificadas. La funcion devuelve los codigos y no los sustituye por su descripcion. Cada archivo incluye un catalogo por cada variable codificada, con el mismo nombre que la columna; \code{tabla = "catalogos"} los devuelve. Los nombres de los municipios no estan en las tablas, sino en uno de los catalogos.
#'
#' En las ediciones de 2021 en adelante, el INEGI escribe siglas en lugar de un dato: \code{"NA"} (no aplica), \code{"NSS"} (el informante no cuenta con elementos para responder), \code{"ND"} (informacion no disponible) y \code{"NP"} (no publicable por confidencialidad o por reserva de informacion). La funcion convierte \code{"NA"} en \code{NA} y conserva las otras tres siglas. Tambien convierte en \code{NA} el texto \code{"NULL"}, escrito con mayusculas o con minusculas, que los diccionarios de datos del INEGI listan entre los valores validos sin definirlo. Por eso, las columnas de cantidades que contienen \code{"NSS"}, \code{"ND"} o \code{"NP"} se devuelven como texto; las columnas que solo contienen numeros se devuelven como numero.
#'
#' @param year Año del levantamiento del censo en formato numerico. Es obligatorio. Los años disponibles son 2011, 2013, 2015, 2017, 2019, 2021, 2023 y 2025.
#' @param modulo Modulo del censo en formato alfanumerico. Las opciones son: \code{"ayuntamiento"}, \code{"administracion"}, \code{"catastro"}, \code{"territorio"}, \code{"seguridad"}, \code{"proteccion_civil"}, \code{"justicia"}, \code{"agua"} y \code{"residuos"}. No todos los modulos existen en todas las ediciones.
#' @param tema Tema del modulo en formato alfanumerico: una parte del titulo que le da el INEGI (\code{"comisiones"}, \code{"recursos humanos"}) o del nombre de su archivo (\code{"ayu_comisiones"}), sin distinguir mayusculas ni acentos. Solo se puede omitir cuando el modulo tiene un solo tema.
#' @param tabla Tabla o tablas del tema que se devuelven, con el nombre que les da el INEGI (\code{"act_ayun"}, \code{"m1s1p3"}). Por omision, todas. Tres valores devuelven la documentacion del tema en lugar de sus datos: \code{"indice"}, \code{"catalogos"} y \code{"diccionario"}.
#' @param estado Entidad federativa. Acepta el nombre (\code{"Jalisco"}, \code{"Ciudad de Mexico"}), la abreviatura (\code{"jal"}, \code{"cdmx"}) o la clave del INEGI (\code{14}, \code{"09"}), sin distinguir mayusculas ni acentos, y varias entidades a la vez. La funcion descarga el archivo nacional y conserva las filas de las entidades indicadas; las tablas sin clave geografica se devuelven completas, con un aviso. Por omision, todo el pais.
#'
#' @examples
#'
#' # Consulta los temas del CNGMD 2025, con su modulo y el tamaño de cada descarga
#' temas <- bases_importinegi("cngmd", 2025)
#'
#' # Descarga los microdatos de la estructura de los ayuntamientos en 2011
#' \dontrun{estruct <- cngmd(year = 2011, modulo = "ayuntamiento", tema = "estructura")}
#'
#' # Descarga las comisiones e iniciativas de los ayuntamientos en 2011 y extrae una tabla
#' \dontrun{comisiones <- cngmd(year = 2011, modulo = "ayuntamiento", tema = "comisiones")}
#' \dontrun{actas <- comisiones$act_ayun}
#'
#' # Descarga una sola tabla: personas sindicas de los ayuntamientos en 2025
#' \dontrun{sindicas <- cngmd(2025, "ayuntamiento", "ayuntamientos", tabla = "m1s1p3")}
#'
#' # Descarga todas las tablas del tema, solo con los municipios de Jalisco
#' \dontrun{jalisco <- cngmd(2025, "ayuntamiento", "ayuntamientos", estado = "Jalisco")}
#'
#' # Indice de las tablas del tema y catalogos para interpretar los codigos
#' \dontrun{indice <- cngmd(2025, "ayuntamiento", "ayuntamientos", tabla = "indice")}
#' \dontrun{catalogos <- cngmd(2025, "ayuntamiento", "ayuntamientos", tabla = "catalogos")}
#'
#' # El tema tambien se puede indicar con el nombre de su archivo
#' \dontrun{integrantes <- cngmd(2017, "ayuntamiento", "integrantes_ayuntami")}
#'
#' @return Si no se indica \code{tabla}, o si se indican varias tablas, una lista con un data.frame por tabla, cada uno con el nombre de su tabla. Si se indica una sola tabla, un data.frame. Con \code{tabla = "indice"}, un data.frame con el nombre de cada tabla y, desde la edicion 2021, su contenido. Con \code{tabla = "catalogos"}, una lista con los catalogos del tema. Con \code{tabla = "diccionario"}, un data.frame con la descripcion de las variables de cada tabla (ediciones de 2021 en adelante). El atributo \code{"fuente"} contiene la direccion del archivo descargado y, en las listas de tablas, el atributo \code{"indice"} contiene el indice del tema.
#' @seealso \code{\link{bases_importinegi}} para consultar, sin descargar nada, los temas de cada modulo y edicion y cuanto pesan. \code{\link{catalogo_inegi}} para la documentacion del censo en la Red Nacional de Metadatos. La viñeta del paquete, \code{vignette("importinegi")}, explica la funcion con ejemplos.
#' @export
cngmd <- function(year, modulo, tema = NULL, tabla = NULL, estado = NULL) {
  if (missing(year)) {
    year <- NULL
  }
  if (missing(modulo)) {
    modulo <- NULL
  }
  gobierno_obtener("cngmd", year, modulo, tema, tabla, estado)
}
