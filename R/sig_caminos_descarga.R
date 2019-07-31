#' Red Nacional de Caminos - Descarga datos
#'
#' Descarga un una lista con todos los mapas de la Red Nacional de Caminos para un año especifico. El objeto resultante de esta funcion es necesario para extraer, por separado, cada mapa con la funcion \code{sig_caminos_extrae()}.
#'
#' La Red Nacional de Caminos (RNC) provee informacion georreferenciada sobre las vias de comunicacion inter-urbana e intra-urbana. Adicionalmente, contiene informacion sobre la infraestructura publica urbana (p. ej. tuneles, puentes, plazas de cobro, marcas de kilometraje, etc.), y la infraestructura de otros medios de transporte (p. ej. transbordadores, aeropuertos, puertos y estaciones de ferrocarril).
#'
#' @param year Año de referencia del mapa, en formato numerico (2014-2018).
#'
#' @examples
#'
#' # Descargar mapas de la RNC
#' \donttest{mapas.rnc = sig_caminos_descarga(year = 2014)}
#' @return Data.frame
#' @export


sig_caminos_descarga <- function(year = NA){

# Objetos generales
base.caminos = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/"

# Definir a\u00f1o
#if (year==2018){codigo.year="889463674641_s"}
#else if (year==2017) {codigo.year="889463171836_s"}
#else if (year==2016) {codigo.year="702825219000_s"}
#else {stop(message("Solo a\u00f1os 2918, 2017 y 2016 disponibles."))}

# URL Completa (de datos.gob.mx)
if (year==2018){codigo.year= "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2018/889463674641_s.zip"}
else if (year==2017) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2016/702825219000_s.zip"}
else if (year==2016) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2016/702825219000_s.zip"}
else if (year==2015) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2015/702825209575_s.zip"}
else if (year==2014) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/702825278724.zip"}
else {stop(message("Solo a\u00f1os 2018, 2017, 2016, 2015 y 2014 estan disponibles."))}

# Inicio --------------------------------------------------------------------
# Download the file
#url.caminos = paste0("http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2018/", codigo.year, ".zip")
tempcaminos = tempfile()
utils::download.file(url.caminos, tempcaminos)

return(tempcaminos)
#output_conjunto = list_sigfiles[c(1, 2, 3, 4, 5)]
#map_output = readOGR(dsn = "./conjunto_de_datos", layer = "red_vial")

# Informacion de la funcion
message("La base de datos con shapefiles ha sido descargada. Utiliza
        la funcion sig_caminos_extrae() para extraer cada conjunto de
        datos (shapefile). Los conjuntos de datos disponibles son:
        \n
        \r2018: estructura, localidad, maniobra_prohibida, plaza_cobro,
        \rposte_de_referencia, puente, red_vial, sitip_de_interes,
        \rtarifas, transbordador, tred_localidad, tred_sitio_de_interes,
        \runion.
        \n
        \rPara consultar el diccionario de datos, escribe:
        \rsig_caminos_extrae(conjunto = \"diccionario\")
        \r\n")

} # End of Function


