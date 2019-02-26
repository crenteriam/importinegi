#' Red Nacional de Caminos
#'
#' @param year XXXXX.

sig_caminos <- function(year = NA){
# Generales -----------------------------------------------------------------
# Informacion de la version
message("sig_caminos() Versión 1.0.
        \rRed Nacional de Caminos
        \rAños disponible: 2018, 2017 y 2016.
        \r\n")

# Objetos generales
formato_archivo = "dbf"
base.caminos = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/"

# Definir an~o
if (year==2018){codigo.year="889463674641_s"}
else if (year==2017) {codigo.year="889463171836_s"}
else if (year==2016) {codigo.year="702825219000_s"}
else {stop(print("Solo an~os 2918, 2017 y 2016 disponibles."))}

# Inicio --------------------------------------------------------------------
# Download the file
url.caminos = paste0("http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2018/", codigo.year, ".zip")
tempcaminos = tempfile()
download.file(url.caminos, tempcaminos)

# Unzip and open
temp_sigdir = unzip(tempcaminos)
list_sigfiles = as.list(temp_sigdir)

return(temp_sigdir)
#output_conjunto = list_sigfiles[c(1, 2, 3, 4, 5)]
#map_output = readOGR(dsn = "./conjunto_de_datos", layer = "red_vial")

# Informacion de la funcion
message("La base de datos con shapefiles ha sido descargada. Utiliza
        la funcion sig_caminos_extrae() para extraer cada conjunto de
        datos (shapefile). Los conjuntos de datos disponibles son:
        \n
        \n2018: estructura, localidad, maniobra_prohibida, plaza_cobro,
        \nposte_de_referencia, puente, red_vial, sitip_de_interes,
        \ntarifas, transbordador, tred_localidad, tred_sitio_de_interes,
        \nnion.
        \n
        \nPara consultar el diccionario de datos, escribe:
        \nsig_caminos_extrae(conjunto = \"diccionario\")
        \r\n")

} # End of Function

# A second function for managing the downloaded data
sig_caminos_extrae <- function(. = NA, conjunto = NA){
library(rgdal)

# Informacion de la funcion
message("Conjuntos de datos (shapeflies) disponibles:
        \n
        \n2018: estructura, localidad, maniobra_prohibida, plaza_cobro,
        \nposte_de_referencia, puente, red_vial, sitip_de_interes,
        \ntarifas, transbordador, tred_localidad, tred_sitio_de_interes,
        \nnion.
        \n
        \nPara consultar el diccionario de datos, escribe:
        \nsig_caminos_extrae(conjunto = \"diccionario\")
        \r\n")

      #path_conjunto  = paste0("./conjunto_de_datos/", conjunto)
      #lista_conjunto = Filter(function (x) str_detect(x, path_conjunto), list_sigfiles)
      map_output = readOGR(., dsn = "./conjunto_de_datos", layer = conjunto)
      return(map_output)
} # End of Function
