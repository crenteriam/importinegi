#' ENOE
#'
#' Encuesta Nacional de Ocupacion y Empleo (ENOE)
#'
#' La ENOE es un proyecto estadistico de encuestas en hogares especializado en informacion sobre el mercado laboral. La ENOE provee informacion trimestral sobre la fuerza laboral, la ocupacion, subocupacion y desocupacion de los miembros del hogar encuestado.
#'
#'
#' @param year Año de levantamiento de la encuesta en formato numerico.
#' @param trimestre Trimestre de levantamiento de la encuesta en formato alfanumerico. Las opciones son: "trim1", "trim2", "trim3" y "trim4".
#' @param integrar \code{FALSE}: descarga por separado y en una lista las cinco bases de datos que componen la ENOE. \code{TRUE}: integra las cinco bases de datos en una sola, utilizando el identificador unico del entrevistado.
#' @param formato La extensión del formato. Las opciones son: "dta","dbf", "csv" y "sav".
#'
#' @examples
#'
#' # Descargar las bases de datos de la ENOE 2009, Trimestre 1, sin integrar.
#' \dontrun{enoe(year = 2009, trimestre = "trim1")}
#'
#' # Descargar las bases de datos de la ENOE 2009, Trimestre 1, integradas
#' \dontrun{enoe(year = 2009, trimestre = "trim1", integrar = TRUE)}
#' @return Data.frame
#' @export

enoe = function(year = NA, trimestre = NA, integrar = FALSE, formato = "dbf"){
  if (is.na(year) & is.na(trimestre)) {shell.exec("https://www.inegi.org.mx/programas/enoe/15ymas/")}
  # Temp files
  temp.enoe = tempfile()
  zipdir     = tempdir()

  # Descargar
  if (year >= 2020 & !(year == 2020 & trimestre == 'trim1')) {
    url.base = paste("https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_n",year,trimestre,paste0(formato,".zip"), sep = "_")
  } else {
    url.base = paste0("http://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/", year, trimestre,  "_", formato, ".zip")
  }
  utils::download.file(url.base, temp.enoe)
  utils::unzip(temp.enoe, exdir = zipdir)
  list_dataraw = list.files(zipdir, pattern = paste0(formato,'$'), full.names = T)
  list_names = basename(tools::file_path_sans_ext(list_dataraw))
  # Read all files in the folder
  output = lapply(list_dataraw, rio::import)
  names(output) = list_names

  # INTEGRAR - TRUE  FALSE
  if (integrar == TRUE) {
    output = Reduce(function(x, y) merge(x,y, all= TRUE), output)
  }

  # Return Object
  return(output)
}
# Fin de la funcion

