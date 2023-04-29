#' Red Nacional de Caminos - Descarga datos
#'
#' Descarga un una lista con todos los mapas de la Red Nacional de Caminos para un año especifico. El objeto resultante de esta funcion es necesario para extraer, por separado, cada mapa con la funcion \code{sig_caminos_extrae()}.
#'
#' La Red Nacional de Caminos (RNC) provee informacion georreferenciada sobre las vias de comunicacion inter-urbana e intra-urbana. Adicionalmente, contiene informacion sobre la infraestructura publica urbana (p. ej. tuneles, puentes, plazas de cobro, marcas de kilometraje, etc.), y la infraestructura de otros medios de transporte (p. ej. transbordadores, aeropuertos, puertos y estaciones de ferrocarril).
#'
#' @param year Año de referencia del mapa, en formato numerico (2014-2019,2022).
#'
#' @examples
#'
#' # Descargar mapas de la RNC
#' \dontrun{mapas.rnc = sig_caminos_descarga(year = 2019)}
#' @return Data.frame
#' @export


sig_caminos_descarga <- function(year = NA, method = "auto"){
  # URL Completa (de datos.gob.mx)
  url.base = "http://internet.contenidos.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/"
  url.extension = c("2015/702825209575_s",
                    "2016/702825219000_s",
                    "2017/889463171836_s",
                    "2018/889463674641_s",
                    "2019/889463776086_s",
                    "2022/889463770558_s")
  info = data.frame(
    year = c(2014:2019,2022),
    url = c(
      "https://www.inegi.org.mx/contenido/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/702825278724_s.zip", # 2014
      paste0(url.base,url.extension,".zip")
    ),
    size_numeric = c(159496815,161828921,196655012,251510799,399162604,493889534,591775731),
    size_string = c("152.1 Mb", "154.3 Mb", "187.5 Mb", "239.9 Mb", "380.7 Mb","471 Mb","564.4 Mb")
  )
  # define_tamanio_descarga <- function(url){
  #   response = httr::HEAD(url)
  #   as.numeric(httr::headers(response)[["Content-Length"]])
  # }
  #
  # file_size <- sapply(info[["url"]], define_tamanio_descarga)
  # sizes = unname(sapply(file_size, function(x) utils:::format.object_size(x, "auto")))
  #

  if (!year %in% info[["year"]]) {
    stop(paste0("El dato: ",
                year,
                " no es válido. Elige uno de los siguientes:",
                paste0(info[["year"]], collapse=",")
    )
    )
  }
  # Info:

  info_renglon = info[year == info["year"],]
  es_grande = info_renglon[3] > 2.5e8
  timeout = getOption("timeout")

  message(
    paste0("Descargando año ", year, " con un peso de ",
           info_renglon[4])
  )
  if (es_grande){
    message("Esto puede tardar varios minutos")
  }
  if(timeout <= 60){
    message("El tiempo de respuesta es algo bajo, se recomienda utilizar options(timeout = max(300, getOption(\"timeout\")))")
  }

  url.caminos = unlist(info_renglon[2])



  # Inicio --------------------------------------------------------------------
  # Download the file
  tempcaminos = tempfile()
  utils::download.file(url.caminos, tempcaminos, method = method)
  # Maybe a TryCatch
  # Unzip and open
  message("Descomprimiendo archivo...")
  zipdir = tempdir()
  utils::unzip(tempcaminos, exdir = zipdir)

  # Segunda etapa  -------------------------------------------------------------
  if      (year==2014) {subfolder = file.path(paste0(zipdir, "\\\\producto\\\\informaci\U00A2n espacial/"))}
  else if (year %in% c(2015,2017:2019,2022)) {subfolder = file.path(paste0(zipdir, "/conjunto_de_datos"))}
  else if (year==2016) {second_zip = file.path(paste0(zipdir, "\\\\conjunto_de_datos\\\\red_nacional_de_caminos_2016.zip"))
  subfolder = tempdir()
  utils::unzip(second_zip, exdir = subfolder)
  }

  # Informacion de la funcion
  message("La base de datos con shapefiles ha sido descargada. Utiliza
        la funcion sig_caminos_extrae() para extraer cada conjunto de
        datos (shapefile). A continuacion, se listan los shapefiles contenidos en el objeto \r\n")

  #if (year >= 2017) {message("AVISO. El archivo .ZIP a partir del anio 2017 puede ser demasiado grande para su descarga directa en R")}

  print(dir(subfolder))
  return(subfolder)
} # End of Function

