#' Censo de Poblacion - ITER
#'
#' Censo de Poblacion y Vivienda. Principales resultados por localidad (ITER)
#'
#' Esta base de datos tiene dos niveles de agregacion: entidades federativas y municipios.
#'
#' @param year Año del levantamiento del censo en formato numerico. Los años disponibles (incluyendo los conteos) son: 1990, 2000, 2005, 2010 y 2015.
#' @param estado Define el nombre de la entidad federativa para descargar los datos, en formato alfanumerico. La funcion, por defecto utiliza la palabra "Nacional" para descargar los datos de todos los estados. Los nombres de los estados deben ir capitalizados (y en su caso, con espacios), por ejemplo: "Aguascalientes", "CDMX", "San Luis Potosi".
#' @param totalestado Resultados agregados a nivel entidad federativa. \code{FALSE} omite los resultados a nivel entidad federativa.
#' @param totalmunicipiopio Resultados agregados a nivel municipio. \code{FALSE} omite los resultados a nivel municipio.
#' @param totallocalidad Resultados agregados a nivel localidad. \code{FALSE} omite los resultados a nivel localidad.
#'
#' @examples
#'
#' # Consulta los datos ITER del Censo de Poblacion y Vivienda
#' censo_poblacion_iter()
#'
#' # Descarga los datos de San Luis Potosi de 2010.
#' dt.iter.sanluis2010 = censo_poblacion_iter(year = 2010, estado = "San Luis Potosi")
#'
#' @family conteo_poblacion_iter()

censo_poblacion_iter <- function(year = "2010", estado = "Nacional", totalestado = FALSE, totalmunicipio = FALSE, totallocalidad = FALSE){
  library(foreign) # Importar archivos dbf.

# Informacion de la version
message("censo_poblacion_iter() Versión 1.0.0
        \rPrincipales resultados por localidad (ITER)
        \rAños disponibles: 1990, 2000 y 2010.
        \nPara los an~os 2015, 2005 y 1995 usar conteo_poblacion_iter().
        \r\n")

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
if (totalmunicipio == FALSE) {data.output.iter <- base::subset(data.output.iter, LOC!="0000")} else {}

#NOLOC -> No extraer valores para localidades
if (totallocalidad == TRUE) {data.output.iter <- base::subset(data.output.iter, MUN=="000" | LOC=="0000")} else {}

base::unlink(censo.temp.iter)
return(data.output.iter)
}
