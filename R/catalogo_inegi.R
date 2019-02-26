#' Cataologo INEGI
#'
#' @param id Describe.

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
