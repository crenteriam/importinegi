#' Red Nacional de Caminos - Extrae Shapefiles
#'
#' @param . Objeto (inserta el objeto creado con sig_caminos())
#' @param year An~o de la version de la Red Nacional de Caminos (disponibles 2014-2018)
#' @param conjunto Capa de Shapefile a descargar. Disponibles: estructura, localidad, maniobra_prohibida, plaza_cobro, poste_de_referencia, puente, red_vial, sitio_de_interes, tarifas, transbordador, tred_localidad, tred_sitio_de_interes, runion.
#'

sig_caminos_extrae <- function(. = NA, year = NA, conjunto = NA){
  # Generales -----------------------------------------------------------------
  #if (conjunto == ){shell.exec("https://www.inegi.org.mx/temas/mapas/viascomunicacion/")}

# Informacion de la funcion
message("sig_caminos_extrae() Versiu00f3n 1.0.
         \rConjuntos de datos (shapeflies) disponibles:
         \n
         \r2018: estructura, localidad, maniobra_prohibida, plaza_cobro,
         \rposte_de_referencia, puente, red_vial, sitio_de_interes,
         \rtarifas, transbordador, tred_localidad, tred_sitio_de_interes,
         \runion.
         \r\n")

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
  map_output = rgdal::readOGR(dsn = rutaarchivo, layer = conjunto)
  return(map_output)

} # End of Function
