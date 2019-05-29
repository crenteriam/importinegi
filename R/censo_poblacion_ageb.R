#' Censo de Poblacion - AGEB
#'
#' Descarga los datos del Censo de Poblacion y Vivienda al nivel de desagregacion AGEB y manzana urbana.
#'
#' Esta base de datos tiene tres niveles de agregacion: entidades federativas, municipios, agebs y manzanas (en zonas urbanas).
#'
#' @param year Año del levantamiento del censo en formato numerico. Los años disponibles (incluyendo los conteos) son: 2000, 2005, 2010 y 2015.
#' @param estado Define el nombre de la entidad federativa para descargar los datos, en formato alfanumerico. Utiliza "Nacional" para descargarlos a nivel nacional. Los nombres de los estados deben ir capitalizados (y en su caso, con espacios), por ejemplo: "Aguascalientes", "CDMX", "San Luis Potosi".
#' @param totalestado Resultados agregados a nivel entidad federativa. \code{FALSE} omite los resultados a nivel entidad federativa.
#' @param totalmunicipio Resultados agregados a nivel municipio. \code{FALSE} omite los resultados a nivel municipio.
#' @param totalageb Resultados agregados a nivel AGEB. \code{FALSE} omite los resultados a nivel AGEB.
#'
#' @examples
#'
#' # Consulta los datos del Censo de Poblacion y Vivienda a nivel AGEB y manzana urbana.
#' censo_poblacion_ageb()
#'
#' # Descarga los datos de San Luis Potosi de 2010.
#' dt.ageb.sanluis2010 = censo_poblacion_ageb(year = 2010, estado = "San Luis Potosi")
#'
#' @family conteo_poblacion_ageb()


censo_poblacion_ageb <- function(year = "2010", estado = NA , totalestado = FALSE, totalmunicipio = FALSE, totalageb = FALSE){
library(foreign) # Importar archivos dbf.

# Informacion de la version
message("censo_poblacion_ageb() Versión 1.0.0
        \rPrincipales resultados por AGEB y manzana urbana
        \rAños disponibles: 2010.
        \r\n")

# Objetos generales
formato_archivo = "dbf"
inegi.base      = "http://www.inegi.org.mx/contenidos/programas/ccpv/"

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

#1 Obtener URL  --------------------------------------------------
censo.url.ageb =  paste0(inegi.base, year, "/microdatos/iter/ageb_manzana/",
                         censo.state, "_", tolower(estado), "_", year,
                         "_ageb_manzana_urbana_", formato_archivo, ".zip")
#2 DOWNLOAD TEMPORARY FILE
censo.temp.ageb = base::tempfile()
download.file(censo.url.ageb, censo.temp.ageb)

#3 Unzip, read file and fix missing. Unlink temporary file
data.output.ageb = foreign::read.dbf((utils::unzip(censo.temp.ageb)), as.is = TRUE)
data.output.ageb[data.output.ageb=="*"]<-NA

#4 Eliminate totals
if (totalestado == FALSE) {data.output.ageb <- base::subset(data.output.ageb, MUN!="000")} else {}
if (totalmunicipio == FALSE) {data.output.ageb <- base::subset(data.output.ageb, LOC!="0000")} else {}
if (totalageb == FALSE) {
  data.output.ageb <- base::subset(data.output.ageb, MZA!="000")
  data.output.ageb <- base::subset(data.output.ageb, AGEB!="0000")} else {}

return(data.output.ageb)
}
