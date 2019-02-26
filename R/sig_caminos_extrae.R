#' Red Nacional de Caminos II
#'
#' @param year An`o de la version de la Red Nacional de Caminos (disponibles 2016, 2017, 2018)

sig_caminos_extrae <- function(. = NA, conjunto = NA){
  library(rgdal)
  # Generales -----------------------------------------------------------------
  if (year == NA){shell.exec("https://www.inegi.org.mx/temas/mapas/viascomunicacion/")}

  # Informacion de la version
  message("
          \rRed Nacional de Caminos
          \rAños disponible: 2018, 2017 y 2016.
          \r\n")

# Informacion de la funcion
message(" sig_caminos_extrae() Versión 1.0.
          \rConjuntos de datos (shapeflies) disponibles:
          \n
          \r2018: estructura, localidad, maniobra_prohibida, plaza_cobro,
          \rposte_de_referencia, puente, red_vial, sitip_de_interes,
          \rtarifas, transbordador, tred_localidad, tred_sitio_de_interes,
          \runion.
          \n
          \rPara consultar el diccionario de datos, escribe:
          \rsig_caminos_extrae(conjunto = \"diccionario\")
          \r\n")

  # Unzip and open
  temp_sigdir = unzip(.)
  list_sigfiles = as.list(temp_sigdir)

  #
  #path_conjunto  = paste0("./conjunto_de_datos/", "localidad")
  #lista_conjunto = Filter(function (x) str_detect(x, path_conjunto), .)
  #lista_conjunto = str_replace_all(lista_conjunto, "^./(.+)/", "")
  map_output = readOGR(dsn = "./red_nacional_de_caminos_2015/conjunto_de_datos/", layer = conjunto)
  return(map_output)

} # End of Function
