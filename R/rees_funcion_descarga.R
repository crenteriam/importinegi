#' Red Nacional de Caminos - Descarga datos
#'
#' Descarga un una lista con todos los mapas de la Red Nacional de Caminos para un año especifico. El objeto resultante de esta funcion es necesario para extraer, por separado, cada mapa con la funcion \code{sig_caminos_extrae()}.
#'
#' La Red Nacional de Caminos (RNC) provee informacion georreferenciada sobre las vias de comunicacion inter-urbana e intra-urbana. Adicionalmente, contiene informacion sobre la infraestructura publica urbana (p. ej. tuneles, puentes, plazas de cobro, marcas de kilometraje, etc.), y la infraestructura de otros medios de transporte (p. ej. transbordadores, aeropuertos, puertos y estaciones de ferrocarril).
#'
#' @param year Año de referencia del mapa, en formato numerico (2016-2019).
#'
#' @examples
#'
#' # Descargar mapas de la RNC
#' \dontrun{mapas.rnc = sig_caminos_descarga(year = 2019)}
#' @return Data.frame
#' @export


sig_caminos_descarga <- function(year = NA){

  # URL Completa (de datos.gob.mx)
  url.base = "http://internet.contenidos.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/"

  data.frame(
    year = c(2014:2019,2022),
    url = c("https://www.inegi.org.mx/contenido/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/702825278724_s.zip",
            paste0(url.base,year,)), #2014 es diferente

  )


  if  (year==2014) {url.caminos = "https://www.inegi.org.mx/contenido/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/702825278724_s.zip"}
  else {url.caminos = paste0(url.base, code.year, ".zip")}

  # Inicio --------------------------------------------------------------------
  # Download the file
  tempcaminos = tempfile()
  utils::download.file(url.caminos, tempcaminos)

  # Unzip and open
  message("Descomprimiendo archivo...")
  zipdir = tempdir()
  utils::unzip(tempcaminos, exdir = zipdir)

  # Segunda etapa  -------------------------------------------------------------
  if      (year==2014) {subfolder = file.path(paste0(zipdir, "\\\\producto\\\\informaci\U00A2n espacial/"))}
  else if (year==2015 | year == 2017 | year==2018 | year ==2019) {subfolder = file.path(paste0(zipdir, "\\\\conjunto_de_datos\\\\"))}
  else if (year==2016) {second_zip = file.path(paste0(zipdir, "\\\\conjunto_de_datos\\\\red_nacional_de_caminos_2016.zip"))
  subfolder = tempdir()
  utils::unzip(second_zip, exdir = subfolder)
  }

  # Informacion de la funcion
  message("La base de datos con shapefiles ha sido descargada. Utiliza
        la funcion sig_caminos_extrae() para extraer cada conjunto de
        datos (shapefile). A continuacion, se listan los shapefiles contenidos en el objeto \r\n")

  if (year >= 2017) {message("AVISO. El archivo .ZIP a partir del anio 2017 puede ser demasiado grande para su descarga directa en R")}

  print(dir(subfolder))
  return(subfolder)
} # End of Function


