#' Red Nacional de Caminos - Extrae mapas
#'
#' Extrae cada uno de los mapas que componen de la Red Nacional de Caminos, previamente descargados con la funcion \code{sig_caminos_descarga()}.
#'
#' La Red Nacional de Caminos (RNC) provee informacion georreferenciada sobre las vias de comunicacion inter-urbana e intra-urbana. Adicionalmente, contiene informacion sobre la infraestructura publica urbana (p. ej. tuneles, puentes, plazas de cobro, marcas de kilometraje, etc.), y la infraestructura de otros medios de transporte (p. ej. transbordadores, aeropuertos, puertos y estaciones de ferrocarril).
#'
#' @param . Inserta el nombre del objecto previamente creado con la funcion \code{sig_caminos_descarga()}.
#' @param mapa Mapa en formato alfanumerico. Las opciones son: estructura, localidad, maniobra_prohibida, plaza_cobro, poste_de_referencia, puente, red_vial, sitio_de_interes, tarifas, transbordador, tred_localidad, tred_sitio_de_interes, runion.
#'
#' @examples
#'
#' # Descargar mapas de la RNC
#' \dontrun{mapas.rnc = sig_caminos_descarga(year = 2019)}
#'
#' # Extraer el mapa de las plazas de cobro
#' \dontrun{mapa.pzacobro = sig_caminos_extrae(mapas.rnc, mapa = "puente")}
#' @return Data.frame
#' @export

sig_caminos_extrae <- function(. = NA, mapa = NA){

  # Extraer Shpaefile
  message("Obteniendo Shapefile...")
  map_output = sf::st_read(dsn = ., layer = mapa)

  return(map_output)

} # End of Function
