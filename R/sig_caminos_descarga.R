#' Red Nacional de Caminos - Descarga Datos
#'
#' #' Esta funcion descarga un una lista todos los Shapefiles de la Red Nacional de Caminos para un año especifico. El objeto resultante de esta funcion es necesario para extraer cada Shapefile con la funcion \code{sig_caminos_extrae()}.
#'
#' @param year Año de referencia del mapa, en formato numerico (2014-2018).
#'
#' @example
#'
#' # Extrae el Shapefile plaza_cobro del objeto mapamx
#' \dontrun{mapamx = sig_caminos_descarga(year = 2014)}
#' @export


sig_caminos_descarga <- function(year = NA){

# Objetos generales
base.caminos = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/"

# Definir a\u00f1o
#if (year==2018){codigo.year="889463674641_s"}
#else if (year==2017) {codigo.year="889463171836_s"}
#else if (year==2016) {codigo.year="702825219000_s"}
#else {stop(print("Solo a\u00f1os 2918, 2017 y 2016 disponibles."))}

# URL Completa (de datos.gob.mx)
if (year==2018){codigo.year= "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2018/889463674641_s.zip"}
else if (year==2017) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2016/702825219000_s.zip"}
else if (year==2016) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2016/702825219000_s.zip"}
else if (year==2015) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2015/702825209575_s.zip"}
else if (year==2014) {url.caminos="http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/702825278724.zip"}
else {stop(print("Solo a\u00f1os 2018, 2017, 2016, 2015 y 2014 estan disponibles."))}

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


