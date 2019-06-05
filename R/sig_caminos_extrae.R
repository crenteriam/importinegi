#' Red Nacional de Caminos - Extrae Shapefiles
#'
#' Esta funcion extrae cada uno de los Shapefiles de la Red Nacional de Caminos descargados con la funcion \code{sig_caminos_extrae()}.
#'
#' @param . Inserta el nombre del objecto creado con la funcion \code{sig_caminos_extrae()}.
#' @param year Año de referencia del mapa, en formato numerico (2014-2018).
#' @param capa Capa (Shpafile) en formato alfanumerico. Las opciones son: estructura, localidad, maniobra_prohibida, plaza_cobro, poste_de_referencia, puente, red_vial, sitio_de_interes, tarifas, transbordador, tred_localidad, tred_sitio_de_interes, runion.
#'
#' @example
#'
#' # Extrae el Shapefile plaza_cobro del objeto mapamx
#' \dontrun{mapamx = sig_caminos_descarga(year = 2014)}
#' \dontrun{mapamx.pzacobro = sig_caminos_extrae(mapamx, year = 2014, capa = "plaza_cobro")}
#' @export

sig_caminos_extrae <- function(. = NA, year = NA, capa = NA){

if      (year == 2018) {rutaarchivo = ""}
else if (year == 2017) {rutaarchivo = ""}
else if (year == 2016) {rutaarchivo = ""}
else if (year == 2015) {rutaarchivo = "./red_nacional_de_caminos_2015/conjunto_de_datos"}
else if (year == 2014) {rutaarchivo = ""}

  # Unzip and open
  print("Descomprimiendo archivo...")
  temp_sigdir = utils::unzip(.)
  #list_sigfiles = as.list(temp_sigdir)
  #path_conjunto  = paste0("./conjunto_de_datos/", "localidad")
  #lista_conjunto = Filter(function (x) str_detect(x, path_conjunto), data)
  #temp = str_replace_all(temp, "^./(.+)/", "./folder/")

  print("Obteniendo Shapefile...")
  map_output = rgdal::readOGR(dsn = rutaarchivo, layer = capa)
  return(map_output)

} # End of Function
