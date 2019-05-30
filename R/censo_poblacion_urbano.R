#' Censo de Poblacion - Entorno urbano
#'
#' Censo de Poblacion y Vivienda. Resultados sobre infraestructura y características del entorno urbano.
#'
#' Esta base de datos tiene dos niveles de agregacion: entidades federativas y municipios.
#'
#' @param year Año del levantamiento del censo en formato numerico. Los años disponibles (incluyendo los conteos) son: 2000, 2005, 2010 y 2015.
#' @param estado Define el nombre de la entidad federativa para descargar los datos, en formato alfanumerico. Utiliza "Nacional" para descargarlos a nivel nacional. Los nombres de los estados deben ir capitalizados (y en su caso, con espacios), por ejemplo: "Aguascalientes", "CDMX", "San Luis Potosi".
#' @param totalestado Resultados agregados a nivel entidad federativa. \code{FALSE} omite los resultados a nivel entidad federativa.
#' @param totalmunicipiopio Resultados agregados a nivel municipio. \code{FALSE} omite los resultados a nivel municipio.
#'
#' @examples
#'
#' # Consulta los datos del entorno urbano del Censo de Poblacion y Vivienda
#' censo_poblacion_entorno()
#'
#' # Descarga los datos de San Luis Potosi de 2010.
#' dt.entorno.sanluis2010 = censo_poblacion_entorno(year = 2010, estado = "San Luis Potosi")
#'
#' @family conteo_poblacion_entorno()

censo_poblacion_urbano <- function(year = "2010", estado = NA, totalestado = FALSE, totalmunicipio = FALSE){

  # Informacion de la version
  message("censo_poblacion_urbano() Version 1.0.0
          \rResultados sobre infraestructura y caracteristicas del entorno urbano.
          \rA\u00f1o disponible: 2010.
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
  else {stop("Argumento requerido: nombra un estado o Nacional")}






#1 Obtener URL  ----------------------------------------------
  censo.url.eu =  paste0(inegi.base, year, "/microdatos/urbano/MANZANA_EU_", censo.state, "_", formato_archivo, ".zip")

#2 Descarga archivo temporal
  censo.temp.eu = base::tempfile()
  utils::download.file(censo.url.eu, censo.temp.eu)

#3 Unzip, read file and fix missing. Unlink temporary file
  data.output.eu = foreign::read.dbf((utils::unzip(censo.temp.eu)), as.is = TRUE)
  return(data.output.eu)
}
