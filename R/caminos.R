#' Red Nacional de Caminos
#'
#' @param year Año requerido del censo o conteo. En esta versión están disponibles 2000, 2005, 2010 y 2015.
#' @param estado Descarga datos por estado. Utilizar el nombre del estado, con espacios. Ejemplos: "Aguascalientes", "CDMX", "San Luis Potosi", "Nacional".
#' @param totalestado TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel estado.
#' @param totalmunici TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel municipio.
#' @param nolocalidad TRUE para no obtener información observaciones a nivel localidad.

censo_poblacion_rural <- function(year = "2010", conjunto = TRUE, diccionario = FALSE, metadatos = FALSE){
library(rgdal)
# Informacion de la version
message("censo_poblacion_rural() Versión 1.0.
        \rResultados sobre localidades con menos de 5 mil habitantes
        \rAño disponible: 2010.
        \r\n")

# Objetos generales
formato_archivo = "dbf"
base.caminos = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/"

if (year=="2018"){codigo.year="889463674641_s"}
else if (year==2017){codigo.year="889463171836_s"}
else if (year==2016){codigo.year="702825219000_s"}
else{}

#1 Obtener URL  ----------------------------------------------
url.caminos =  paste0(base.caminos, year, "/", codigo.year, ".zip")

#2 Descarga carpeta
temp.caminos = base::tempfile()
download.file(url.caminos, temp.caminos)

#3 Unzip Conjunto de Datos
if (conjunto == TRUE){
carpeta.conjunto = paste0(temp.caminos, "\\conjunto_de_datos")
list_files  = dir(carpeta.conjunto)

} else {}

#3 Unzip, read file and fix missing. Unlink temporary file
zipdir = tempfile()
utils::unzip(censo.temp.muestra, exdir = zipdir)
zipped_file = dir(zipdir)
data.output.rural = foreign::read.dbf((utils::unzip(censo.temp.rural)), as.is = TRUE)
data.output.rural[data.output.rural=="*"]<-NA
return(data.output.rural)
}

# Pruebas
library(rgdal)
url.caminos = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2018/889463674641_s.zip"
tempcaminos = tempfile()
download.file(url.caminos, tempcaminos)
temp2 = tempdir()
setwd(temp2)
temp2 = unzip(tempcaminos)
thefiles  = dir(temp2)
temp3 = as.list(temp2)
file1 = temp3[c(1, 2, 3, 4, 5)]
mymap = readOGR(dsn = "./conjunto_de_datos", layer = "estructura")
x = as.data.frame(temp2)
