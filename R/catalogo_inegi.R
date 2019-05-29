#' Catalogo INEGI
#'
#' Consulta el catalogo la Red Nacional de Metadatos del INEGI o los metadatos de una base de datos en particular.
#'
#' La funcion \code{catalogo_inegi} provee una lista de bases de datos con un identificador unico (\code{id}). Si conoces el \code{id} de la base de datos a consultar, utilizalo en el parametro para obtener el libro de codigos y los metadatos de la base de datos. Si no conoces el \code{id} de la base de datos a consultar, teclea la funcion \code{catalogo_inegi} sin parametros para descargar la lista de bases de datos (Ver ejemplo abajo).
#'
#' @param id Numero de identificacion unico de las bases de datos disponibles en el repositorio la Red Nacional de Metadatos del INEGI
#'
#' @examples
#'
#' # Almacena el catalogo como base de datos.
#' dt.catalogo <- catalogo_inegi()
#'
#' # Consulta metadatos de una base de datos.
#' catalogo_inegi(id = 489)
#'
#' @seealso Consulta el \href{http://www3.inegi.org.mx/rnm/index.php/catalog/}{repositorio la Red Nacional de Metadatos del INEGI.}

catalogo_inegi = function(id = NA){
  open.id = paste0("http://www3.inegi.org.mx/rnm/index.php/catalog/", id, "/data_dictionary")
  if (!is.na(id)) {base::shell.exec(open.id)}
### Catálgo General ---------------------------------------------------------
  else {
  Catalogo = tempfile()
  utils::download.file("http://www3.inegi.org.mx/rnm/index.php/catalog/export/csv?ps=500&", Catalogo)
  Catalogo = utils::read.csv(Catalogo, encoding = "UTF-8")
  names(Catalogo)[names(Catalogo) == "X.U.FEFF.id"] <- "id"
  Catalogo = base::subset(Catalogo, select = c(id, surveyid, titl, data_coll_start, data_coll_end, created, changed))
  return(Catalogo)
  } # End of Catálogo General
} # End of Function
