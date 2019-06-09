#' Catalogo de proyectos estadisticos del INEGI
#'
#' Consulta el catalogo la Red Nacional de Metadatos del INEGI o los metadatos de un proyecto estadistico en particular.
#'
#' La funcion \code{catalogo_inegi} provee una lista de bases de datos con un identificador unico (\code{id}). Si conoces el \code{id} de la base de datos, utilizalo en el parametro para acceder al libro de codigos y los metadatos de la base de datos. Si no conoces el \code{id} de la base de datos a consultar, teclea la funcion \code{catalogo_inegi} sin parametros para descargar la lista de bases de datos (Ver ejemplo).
#'
#' @param id Para acceder al diccionario de datos de una base de datos de la Red Nacional de Metadatos del INEGI, utiliza el numero de identificacion unico (valor numerico). El identificador unico se puede consultar en el catalogo de proyectos estadisticos del INEGI (ver ejemplo).
#'
#' @examples
#'
#' # Accede al repositorio de la Red Nacional de Metadatos
#' # > de INEGI y almacenalo como una base de datos.
#' \dontrun{dt.catalogo <- catalogo_inegi()}
#'
#' # Consulta metadatos de una base de datos.
#' \dontrun{catalogo_inegi(id = 489)}
#'
#' @seealso Consulta el \href{http://www3.inegi.org.mx/rnm/index.php/catalog/}{repositorio la Red Nacional de Metadatos del INEGI.}
#' @export

catalogo_inegi = function(id = NA){
  open.id = paste0("http://www3.inegi.org.mx/rnm/index.php/catalog/", id)
  if (!is.na(id)) {shell.exec(open.id)}
### Catálgo General ---------------------------------------------------------
  else {
  Catalogo = tempfile()
  utils::download.file("http://www3.inegi.org.mx/rnm/index.php/catalog/export/csv?ps=500&", Catalogo)
  Catalogo = utils::read.csv(Catalogo, encoding = "UTF-8")
  names(Catalogo)[names(Catalogo) == "X.U.FEFF.id"] <- "id"
  Catalogo = base::subset(Catalogo, select = c("id", "surveyid", "titl", "data_coll_start", "data_coll_end", "created", "changed"))
  return(Catalogo)
  } # End of Catálogo General
} # End of Function
