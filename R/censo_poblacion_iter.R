#' Censos y Conteos de Población y Vivienda.
#' ITER
#'
#' @param year Año requerido del censo o conteo. En esta versión están disponibles 2000, 2005, 2010 y 2015.
#' @param estado Descarga datos por estado. Utilizar el nombre del estado, con espacios. Ejemplos: "Aguascalientes", "CDMX", "San Luis Potosi", "Nacional".
#' @param totalestado TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel estado.
#' @param totalmunici TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel municipio.
#' @param nolocalidad TRUE para no obtener información observaciones a nivel localidad.

censo_poblacion_iter <- function(year = "2010", estado = "Nacional", totalestado = FALSE, totalmunici = FALSE, nolocalidad = FALSE){
  library(foreign) # Importar archivos dbf.

# Informacion de la version
message("censo_poblacion_iter() Versión 1.0. Años disponibles: 1990, 2000 y 2010.
        \nPara los an~os 2015, 2005 y 1995 ver conteo_poblacion_iter(). \n \n")

# Objetos generales
formato_archivo = "dbf"
inegi.base      = "http://www.beta.inegi.org.mx/contenidos/programas/ccpv/"

# Estado  -------------------------------------------------------------
  if      (estado == "Aguascalientes"){   censo.state = "01" }
  else if (estado == "Baja California"){ censo.state = "02" }
  else if (estado == "Baja California Sur"){ censo.state = "03"}
  else if (estado == "Campeche"){ censo.state = "04"}
  else if (estado == "Coahuila"){ censo.state = "05"}
  else if (estado == "Colima")  { censo.state = "06"}
  else if (estado == "Chiapas") { censo.state = "07"}
  else if (estado == "Chihuahua"){censo.state = "08"}
  else if (estado == "CDMX")    { censo.state = "09"}
  else if (estado == "Durango") { censo.state = "10"}
  else if (estado == "Guanajuato"){censo.state = "11"}
  else if (estado == "Guerrero"){ censo.state = "12"}
  else if (estado == "Hidalgo") { censo.state = "13"}
  else if (estado == "Jalisco") { censo.state = "14"}
  else if (estado == "Mexico")  { censo.state = "15"}
  else if (estado == "Michoacan"){censo.state = "16"}
  else if (estado == "Morelos") { censo.state = "17"}
  else if (estado == "Nayarit") { censo.state = "18"}
  else if (estado == "Nuevo Leon"){censo.state = "19"}
  else if (estado == "Oaxaca")  { censo.state = "20"}
  else if (estado == "Puebla")  { censo.state = "21"}
  else if (estado == "Queretaro"){censo.state = "22"}
  else if (estado == "Quintana Roo"){censo.state = "23"}
  else if (estado == "San Luis Potosi"){censo.state = "24"}
  else if (estado == "Sinaloa") { censo.state = "25"}
  else if (estado == "Sonora")  { censo.state = "26"}
  else if (estado == "Tabasco") { censo.state = "27"}
  else if (estado == "Tamaulipas"){censo.state = "28"}
  else if (estado == "Tlaxcala"){ censo.state = "29"}
  else if (estado == "Veracruz"){ censo.state = "30"}
  else if (estado == "Yucatan") { censo.state = "31"}
  else if (estado == "Zacatecas"){censo.state = "32"}
  else if (estado == "Nacional") {censo.state = "00"}
  else {stop("Argumento requerido: nombra un estado o 'Nacional'")}


# ITER  -------------------------------------------------------------

#1 Obtener URL
censo.url.iter  = paste0(inegi.base, year, "/microdatos/iter/", censo.state, "_",
                         tolower(estado), "_", year, "_", "iter_dbf.zip")

#2 Descarga archivo temporal
censo.temp.iter = base::tempfile()
download.file(censo.url.iter, censo.temp.iter)

#3 Unzip, read file and fix missing. Unlink temporary file
data.output.iter = foreign::read.dbf((utils::unzip(censo.temp.iter)), as.is = TRUE)
data.output.iter[data.output.iter=="*"]<-NA  # Crea datos perdidos

#4 TOTALENT - Total Entidad Federativa
if (totalestado == FALSE) { data.output.iter <- base::subset(data.output.iter, MUN!="000")} else {}

#5 TOTALMUN - Total Municipio
if (totalmunici == FALSE) {data.output.iter <- base::subset(data.output.iter, LOC!="0000")} else {}

#NOLOC -> No extraer valores para localidades
if (nolocalidad == TRUE) {data.output.iter <- base::subset(data.output.iter, MUN=="000" | LOC=="0000")} else {}

base::unlink(censo.temp.iter)
return(data.output.iter)
}
