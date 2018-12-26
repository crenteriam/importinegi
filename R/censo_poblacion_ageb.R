#' Censos y Conteos de Población y Vivienda.
#' ITER
#'
#' @param year Año requerido del censo o conteo. En esta versión están disponibles 2000, 2005, 2010 y 2015.
#' @param estado Descarga datos por estado. Utilizar el nombre del estado, con espacios. Ejemplos: "Aguascalientes", "CDMX", "San Luis Potosi", "Nacional.
#' @param totalestado TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel estado.
#' @param totalmunici TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel municipio.
#' @param nolocalidad TRUE para no obtener información observaciones a nivel localidad.
#' @param totalageb   TRUE para obtener, además de la información a nivel manzana, resumen estadístico a nivel ageb.

censo_poblacion_ageb <- function(year = "2010", estado = "Nacional", totalestado = FALSE, totalmunici = FALSE, nolocalidad = FALSE){
  library(foreign) # Importar archivos dbf.

  # Informacion de la version
  message("censo_poblacion_iter() Versión 1.0. Años disponibles: 1990, 2000 y 2010. \nPara los an~os 2015, 2005 y 1995 ver conteo_poblacion_iter(). \n \n")

  # Objetos generales
  formato_archivo = "dbf"
  inegi.base      = "http://www.beta.inegi.org.mx/contenidos/programas/ccpv/"


#2 AGEB -----------------------------------------------------------------------------------
else if (type == "ageb") {
  #1 GET URL
  censo.url.ageb =  paste0(inegi.base, year,
                           "/microdatos/ageb_y_manzana/resageburb_", censo.state, "_", year, "_", fformat, ".zip")
  #2 DOWNLOAD TEMPORARY FILE
  censo.temp.ageb = base::tempfile()
  download.file(censo.url.ageb, censo.temp.ageb)
  #3 Unzip, read file and fix missing. Unlink temporary file
  data.output.ageb = foreign::read.dbf((utils::unzip(censo.temp.ageb)))
  data.output.ageb[data.output.ageb=="*"]<-NA
  #4 Eliminate totals
  if (totalent == FALSE) { data.output.ageb <- base::subset(data.output.ageb, MUN!="000")} else {}
  if (totalmun == FALSE) {data.output.ageb <- base::subset(data.output.ageb, LOC!="0000")} else {}
  if (totalageb == FALSE) {
    data.output.ageb <- base::subset(data.output.ageb, MZA!="000")
    data.output.ageb <- base::subset(data.output.ageb, AGEB!="0000")} else {}

  return(data.output.ageb)
} # End of type->ageb 2-nest
