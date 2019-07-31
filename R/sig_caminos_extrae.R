#' Red Nacional de Caminos - Extrae mapas
#'
#' Extrae cada uno de los mapas que componen de la Red Nacional de Caminos, previamente descargados con la funcion \code{sig_caminos_descarga()}.
#'
#' La Red Nacional de Caminos (RNC) provee informacion georreferenciada sobre las vias de comunicacion inter-urbana e intra-urbana. Adicionalmente, contiene informacion sobre la infraestructura publica urbana (p. ej. tuneles, puentes, plazas de cobro, marcas de kilometraje, etc.), y la infraestructura de otros medios de transporte (p. ej. transbordadores, aeropuertos, puertos y estaciones de ferrocarril).
#'
#' @param . Inserta el nombre del objecto previamente creado con la funcion \code{sig_caminos_descarga()}.
#' @param year Año de referencia del mapa en formato numerico (2014-2018).
#' @param mapa Mapa en formato alfanumerico. Las opciones son: estructura, localidad, maniobra_prohibida, plaza_cobro, poste_de_referencia, puente, red_vial, sitio_de_interes, tarifas, transbordador, tred_localidad, tred_sitio_de_interes, runion.
#'
#' @examples
#'
#' # Descargar mapas de la RNC
#' \donttest{mapas.rnc = sig_caminos_descarga(year = 2014)}
#'
#' # Extraer el mapa de las plazas de cobro
#' \donttest{mapa.pzacobro = sig_caminos_extrae(mapas.rnc, year = 2014, mapa = "plaza_cobro")}
#' @return Data.frame
#' @export

sig_caminos_extrae <- function(. = NA, year = NA, mapa = NA){

if      (year == 2018) {rutaarchivo = ""}
else if (year == 2017) {rutaarchivo = ""}
else if (year == 2016) {rutaarchivo = ""}
else if (year == 2015) {rutaarchivo = "./red_nacional_de_caminos_2015/conjunto_de_datos"}
else if (year == 2014) {rutaarchivo = ""}

  # Unzip and open
  message("Descomprimiendo archivo...")
  temp_sigdir = utils::unzip(.)
  #list_sigfiles = as.list(temp_sigdir)
  #path_conjunto  = paste0("./conjunto_de_datos/", "localidad")
  #lista_conjunto = Filter(function (x) str_detect(x, path_conjunto), data)
  #temp = str_replace_all(temp, "^./(.+)/", "./folder/")

  message("Obteniendo Shapefile...")
  map_output = rgdal::readOGR(dsn = rutaarchivo, layer = mapa)

  return(map_output)

} # End of Function
