#' Censo de Poblacion - Localidades rurales
#'
#' Censo de Poblacion y Vivienda. Resultados sobre localidades con menos de 5 mil habitantes
#'
#' Esta base de datos tiene dos niveles de agregacion: entidades federativas y municipios.
#'
#' @param year Año del levantamiento del censo en formato numerico. El unico año disponible en INEGI (incluyendo los conteos) es 2010.
#' @param estado Define el nombre de la entidad federativa para descargar los datos en formato alfanumerico. La funcion, por defecto utiliza la palabra "Nacional" para descargar los datos de todos los estados. Los nombres de los estados deben ir capitalizados (y en su caso, con espacios), por ejemplo: "Aguascalientes", "CDMX", "San Luis Potosi".
#'
#' @examples
#'
#' # Descargar los datos de CDMX de 2010.
#' \dontrun{rural = censo_poblacion_rural(year = 2010, estado = "CDMX")}
#' @return Data.frame
#' @export


censo_poblacion_rural <- function(year = NA, estado = "Nacional"){
  if (is.na(year)) {stop(message("Definir a\u00f1o"))} else {}

  # Objetos generales
  formato_archivo = "dbf"
  inegi.base      = "http://www.inegi.org.mx/contenidos/programas/ccpv/"

  # Estado  -------------------------------------------------------------
  if      (estado == "Aguascalientes"){  censo.state = "01" }
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
  else if (estado == "Nacional") {censo.state = "nal"}
  else {stop("Nombre del estado no encontrado.")}

#1 Obtener URL  ----------------------------------------------
censo.url.rural =  paste0(inegi.base, year, "/microdatos/cinco_mil_menos/resloc_", censo.state, "_", year, "_", formato_archivo, ".zip")

#2 Descarga base de datos
  censo.temp.rural = base::tempfile()
  utils::download.file(censo.url.rural, censo.temp.rural)

#3 Unzip, read file and fix missing. Unlink temporary file
  data.output.rural = foreign::read.dbf((utils::unzip(censo.temp.rural)), as.is = TRUE)
  data.output.rural[data.output.rural=="*"]<-NA

# 4 (no hay totales que eliminar)
  return(data.output.rural)
}
