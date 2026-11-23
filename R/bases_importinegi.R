#' Bases de datos que descarga importinegi
#'
#' Devuelve una tabla con las bases de datos del INEGI que se pueden descargar con las funciones de este paquete, por proyecto estadistico y año. No descarga ningun archivo ni requiere conexion a internet.
#'
#' La tabla sale del registro de archivos del INEGI que se distribuye con el paquete, el mismo que usan las funciones de descarga para validar sus argumentos y encontrar la direccion de cada archivo. Por eso, lo que muestra esta funcion es lo que esas funciones pueden descargar. El atributo \code{"registro"} de la tabla indica la fecha en que se genero el registro: los archivos que el INEGI publique despues de esa fecha apareceran al actualizar el paquete.
#'
#' Por ahora la tabla incluye los proyectos cuyas funciones ya usan el registro: el Censo de Poblacion y Vivienda (\code{\link{censo}}), el Conteo de Poblacion y Vivienda (\code{\link{conteo}}), la Encuesta Intercensal (\code{\link{intercensal}}) y el Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico (\code{\link{cngmd}}). Los demas proyectos del paquete se agregaran conforme sus funciones se actualicen; mientras tanto, sus años y bases disponibles se consultan en la ayuda de cada funcion.
#'
#' Tambien incluye la serie tradicional de la Encuesta Nacional de Ingresos y Gastos de los Hogares (\code{\link{enigh_tradicional}}), con una fila por edicion.
#'
#' A diferencia de \code{\link{catalogo_inegi}}, que consulta en linea todos los proyectos estadisticos que documenta el INEGI, esta funcion muestra solo lo que \code{importinegi} puede descargar.
#'
#' @param proyecto Proyecto estadistico que se quiere consultar, en formato alfanumerico (por ejemplo, \code{"censo"}). Por omision, todos los proyectos.
#' @param year Año o años que se quieren consultar, en formato numerico. Por omision, todos los años.
#'
#' @examples
#'
#' # Todas las bases de datos que descarga el paquete.
#' bases_importinegi()
#'
#' # Bases de datos del Censo de Poblacion y Vivienda 2020.
#' bases_importinegi(proyecto = "censo", year = 2020)
#'
#' # Años en los que existe la base de resultados por AGEB y manzana urbana.
#' bases <- bases_importinegi("censo")
#' bases$year[bases$base == "ageb"]
#'
#' # Temas de cada modulo del Censo Nacional de Gobiernos Municipales 2025.
#' temas <- bases_importinegi(proyecto = "cngmd", year = 2025)
#' table(temas$modulo)
#'
#' # Tablas de cada edicion de la serie tradicional de la ENIGH.
#' ediciones <- bases_importinegi(proyecto = "enigh_tradicional")
#' ediciones$tablas[ediciones$year == 2010]
#'
#' @return Data.frame con una fila por proyecto, año y base de datos (en \code{"cngmd"}, una fila por edicion y tema), y las siguientes columnas:
#' \describe{
#'   \item{\code{proyecto}}{Proyecto estadistico. Coincide con el nombre de la funcion que lo descarga (por ejemplo, \code{"censo"} se descarga con \code{censo()}).}
#'   \item{\code{year}}{Año del levantamiento.}
#'   \item{\code{modulo}}{Solo en \code{"cngmd"}: modulo al que pertenece el tema, como se escribe en el argumento \code{modulo} de la funcion. En los demas proyectos, \code{NA}.}
#'   \item{\code{base}}{Nombre de la base de datos, como se escribe en el argumento \code{base} de la funcion. En \code{"cngmd"}, nombre del archivo del tema, que se puede escribir en el argumento \code{tema}.}
#'   \item{\code{descripcion}}{Descripcion de la base de datos. En \code{"cngmd"}, titulo que el INEGI le da al tema, que tambien se puede escribir en el argumento \code{tema}.}
#'   \item{\code{tablas}}{Tablas de la base de datos, separadas por comas, cuando se publica en varias tablas. La primera es la que se descarga por omision. Se eligen con el argumento \code{tabla} de la funcion de descarga. En \code{"cngmd"}, \code{NA}: las tablas de un tema se conocen al descargarlo.}
#'   \item{\code{cobertura}}{\code{"nacional y por entidad"} si el INEGI publica un archivo nacional ademas de los archivos por entidad federativa, \code{"por entidad"} si solo publica archivos por entidad, o \code{"nacional"} si solo publica el archivo nacional.}
#'   \item{\code{formato}}{Formato de los archivos que lee el paquete (\code{"dbf"}, \code{"csv"} o \code{"xlsx"}).}
#'   \item{\code{tamano_mb}}{Tamaño de la descarga de todo el pais, en megabytes: el archivo nacional si existe o, si no, la suma de los archivos de todas las entidades.}
#' }
#'
#' En \code{"enigh_tradicional"} hay una fila por edicion: \code{base} vale \code{"microdatos"}, \code{tablas} lista las tablas que se pueden pedir con el argumento \code{tabla} de la funcion (la primera es la que se descarga por omision) y \code{tamano_mb} es el peso de todos los archivos de la edicion.
#' @seealso \code{\link{enigh_tradicional}} para descargar la serie tradicional de la Encuesta Nacional de Ingresos y Gastos de los Hogares.
#' @seealso \code{\link{censo}}, \code{\link{conteo}}, \code{\link{intercensal}} y \code{\link{cngmd}} para descargar las bases de datos y \code{\link{catalogo_inegi}} para consultar el catalogo de proyectos estadisticos del INEGI.
#' @export

bases_importinegi <- function(proyecto = NULL, year = NULL) {
  bases <- poblacion_bases()
  gobierno <- gobierno_bases()
  enigh <- enigh_bases()
  proyectos <- c(unique(bases$programa), gobierno_programas()$programa, names(enigh_programas()))

  if (!is.null(proyecto)) {
    if (!is.character(proyecto) || length(proyecto) == 0 || anyNA(proyecto)) {
      detener("`proyecto` debe ser el nombre de un proyecto. Opciones: ", enumerar(proyectos), ".")
    }
    proyecto <- unique(tolower(trimws(proyecto)))
    ajenos <- setdiff(proyecto, proyectos)
    if (length(ajenos) > 0) {
      detener(
        "`proyecto = ", enumerar(ajenos), "` no es una opci\u00f3n v\u00e1lida. Proyectos disponibles: ",
        enumerar(proyectos), "."
      )
    }
    bases <- bases[bases$programa %in% proyecto, ]
    gobierno <- gobierno[gobierno$proyecto %in% proyecto, ]
    enigh <- enigh[enigh$proyecto %in% proyecto, ]
  }
  if (!is.null(year)) {
    numeros <- suppressWarnings(as.integer(as.character(year)))
    if (length(year) == 0 || anyNA(numeros)) {
      detener("`year` debe ser uno o varios a\u00f1os en formato num\u00e9rico (por ejemplo, 2020).")
    }
    bases <- bases[bases$year %in% numeros, ]
    gobierno <- gobierno[gobierno$year %in% numeros, ]
    enigh <- enigh[enigh$year %in% numeros, ]
  }

  filas <- lapply(seq_len(nrow(bases)), function(i) {
    archivos <- poblacion_archivos(bases$programa[i], bases$year[i], bases$base[i])
    if (nrow(archivos) == 0) {
      return(NULL)
    }
    nacional <- archivos$clave == "00"
    bytes <- sumar_bytes(if (any(nacional)) archivos$bytes[nacional] else archivos$bytes)
    tablas <- names(poblacion_tablas(bases$programa[i], bases$year[i], bases$base[i]))
    data.frame(
      proyecto = bases$programa[i],
      year = bases$year[i],
      modulo = NA_character_,
      base = bases$base[i],
      descripcion = poblacion_titulo_base(bases$programa[i], bases$year[i], bases$base[i]),
      tablas = paste(tablas, collapse = ", "),
      cobertura = if (all(nacional)) "nacional" else if (any(nacional)) "nacional y por entidad" else "por entidad",
      formato = archivos$formato[1],
      tamano_mb = round(bytes / 1024^2, 1),
      stringsAsFactors = FALSE
    )
  })
  tabla <- do.call(rbind, c(filas, if (nrow(gobierno) > 0) list(gobierno), if (nrow(enigh) > 0) list(enigh)))
  if (is.null(tabla)) {
    tabla <- data.frame(
      proyecto = character(), year = integer(), modulo = character(), base = character(),
      descripcion = character(),
      tablas = character(), cobertura = character(), formato = character(), tamano_mb = numeric(),
      stringsAsFactors = FALSE
    )
    message("importinegi no tiene bases de datos para esa consulta. Usa bases_importinegi() para ver todas.")
  }
  rownames(tabla) <- NULL
  attr(tabla, "registro") <- registro_generado()
  tabla
}
