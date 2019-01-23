#' Red Nacional de Caminos
#'
#' @param year XXXXX.

#caminos <- function(year = 2018, conjunto = TRUE, diccionario = FALSE, metadatos = FALSE){
library(rgdal)
# Informacion de la version
message("caminos() Versión 1.0.
        \rRed Nacional de Caminos
        \rAños disponible: 2018, 2017 y 2016.
        \r\n")

# Objetos generales
formato_archivo = "dbf"
base.caminos = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/"

# Year
if (year==2018){codigo.year="889463674641_s"}
else if (year==2017) {codigo.year="889463171836_s"}
else if (year==2016) {codigo.year="702825219000_s"}
else {stop(print("Solo an~os 2918, 2017 y 2016 disponibles."))}

# Download the file
url.caminos = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2018/889463674641_s.zip"
tempcaminos = tempfile()
download.file(url.caminos, tempcaminos)

# Unzip and open
temp_sigdir = unzip(tempcaminos)
list_sigfiles = as.list(temp_sigdir)
output_conjunto = list_sigfiles[c(1, 2, 3, 4, 5)]
map_output = readOGR(dsn = "./conjunto_de_datos", layer = "red_vial")
