#' Encuesta Nacional de Ingreso y Gasto de los Hogares (ENIGH).
#' @param year Año de levantamiento de la encuesta
#' @param hogares En esta tabla se encuentran contenidas las características de los hogares que habitan los integrantes de los mismos.
#' @param concentrado En esta tabla se encuentran las variables construidas a partir de las otras tablas de la base de datos. Registra el resumen concentrado por hogar, de ingresos y gastos en toda modalidad posible. Además todos los ingresos y gastos registrados en esta tabla son trimestrales.
#' @param erogaciones Esta tabla permite identificar las erogaciones financieras y de capital realizadas por hogar.
#' @param gastohogar 	Esta tabla permite identificar los gastos monetarios y no monetarios del hogar.
#' @param gastotarjetas Esta tabla permite identificar en qué medida los hogares se auxilian del financiamiento que les ofrece la tarjeta de crédito bancaria o comercial para cubrir algunas necesidades básicas.
#' @param poblacion Esta tabla proviene de la tabla HOGARES. Identifica las características sociodemográficas de los integrantes del hogar; y el acceso a las instituciones de salud que tienen los integrantes del hogar.
#' @param ingresos Esta tabla permite identificar los ingresos y percepciones financieras y de capital de cada uno de los integrantes del hogar, por diversos conceptos.
#' @param gastopersona Esta tabla permite identificar los gastos realizados por cada integrante del hogar en educación, transporte público y remuneraciones en especie.
#' @param trabajos Esta tabla muestra la condición de actividad de los integrantes del hogar de 12 o más años y algunas características ocupacionales durante el periodo de referencia.
#' @param agro Esta tabla muestra la información de los trabajadores independientes, mayores de 12 años que tienen en el hogar negocios dedicados a las actividades agrícolas, forestales y de tala, además de actividades de cría, explotación y productos derivados de la pesca y caza.
#' @param noagro Esta tabla muestra a los trabajadores independientes mayores de 12 años, que tienen negocios en el hogar dedicados a las actividades industriales, comerciales y de servicios.
#' @param construccion Modelo de cuestionario del INEGI. Las opciones son: "Nueva" o "Tradicional". El modelo "Nueva Serie" para la encuesta de 2016 no está actualmente disponible.

# Tengo que arreglar hard-coding (construcción) em Viviendas y Hogares.
enigh = function(year = NA, hogares = FALSE, concentrado = FALSE, erogaciones = FALSE, gastohogar = FALSE, gastotarjetas = FALSE, poblacion = FALSE, ingresos = FALSE, gastopersona = FALSE, trabajos = FALSE, agro = FALSE, noagro = FALSE, construccion = "Nueva"){

  ### PRELIMINARES ##########################################################
  ### CONSTRUCCION
  if (construccion == "Nueva") { const = "nc"}   else if (construccion == "Tradicional") {const = "tradicional"} else {}
  if (construccion == "Nueva") { p.cons= "NCV"}  else if (construccion == "Tradicional") {p.cons= "Tra"}
  ### FILES
  url.base = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/regulares/enigh/", const, "/", year, "/microdatos/")
  zipdir = tempfile()
  temp.enigh = base::tempfile()
  fformat = "dbf"
  ### NEST_1 - VIVIENDAS ##########################################################
  viv = "NCV_Vivi_"
  if (year == 2012) { viv = "ncv_viviendas_"} else if (year == 2010 | year == 2008) {viv = "NCV_Hogares_"} else {}
  url.viv = paste0(url.base, viv, year, "_concil_2010_", fformat, ".zip")
  utils::download.file(url.viv, temp.enigh)
  utils::unzip(temp.enigh, exdir = zipdir)
  unzipviv = "ncv_vivi_"
  if (year == 2012) { unzipviv = "NCV_Vivi_"} else if (year == 2010 | year == 2008) {unzipviv = "NCV_Hogares_"} else {}
  if (year == 2010 | year == 2008) {fformat = "DBF"} else {}
  data.enigh = foreign::read.dbf(paste0(zipdir, "\\\\", unzipviv, year, "_concil_2010_", fformat, ".", fformat))

  ### NEST_2 - HOGARES ##########################################################
  if (hogares == TRUE) {
    library(dplyr)
    if (year ==2010 | year == 2008){} else {
      hog = "NCV_Hogares_"
      url.hog = paste0(url.base, hog, year, "_concil_2010_", fformat, ".zip")
      utils::download.file(url.hog, temp.enigh)
      utils::unzip(temp.enigh, exdir = zipdir)
      unziphog = "ncv_hogares_"
      data.enigh.N2 = foreign::read.dbf(paste0(zipdir, "\\\\", unziphog, year, "_concil_2010_", fformat, ".", fformat))
      data.enigh.N2$id = paste0(as.character(data.enigh.N2$folioviv), as.character(data.enigh.N2$foliohog))
      data.enigh    = dplyr::full_join(data.enigh, data.enigh.N2, by="folioviv", type="left", match="all")

      ### NEST_3 ##########################################################
      ### CONCENTRADO HOGAR
      if (concentrado == TRUE & erogaciones == FALSE & gastohogar == FALSE & gastotarjetas == FALSE & poblacion == FALSE){
        if (year == 2014 & construccion == "Tradicional") {p.cons ="tra"}
        unzipconcentrado = "_Concentrado_"
        url.concentrado = paste0(url.base, p.cons, unzipconcentrado, year, "_concil_2010_", fformat, ".zip")
        utils::download.file(url.concentrado, temp.enigh)
        utils::unzip(temp.enigh, exdir = zipdir)
        if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
        if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
        if (year == 2014 | year == 2012) { unzipconcentrado = "_concentrado_"} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipconcentrado, year, "_concil_2010_", fformat, ".", fformat))
        data.enigh.N3$id = paste0(as.character(data.enigh.N3$folioviv), as.character(data.enigh.N3$foliohog))
        data.enigh$folioviv = NULL
        data.enigh$foliohog = NULL
        data.enigh    = dplyr::full_join(data.enigh, data.enigh.N3, by="id", type="left", match="all")
      } # End of Nest_3 Concentrado Hogar
      ### EROGACIONES
      else if (erogaciones == TRUE & concentrado == FALSE & gastohogar == FALSE & gastotarjetas == FALSE & poblacion == FALSE){
        if (year == 2014 & construccion == "Tradicional") {p.cons ="tra"}
        unziperogaciones = "_Erogaciones_"
        url.erogaciones = paste0(url.base, p.cons, unziperogaciones, year, "_concil_2010_", fformat, ".zip")
        utils::download.file(url.erogaciones, temp.enigh)
        utils::unzip(temp.enigh, exdir = zipdir)
        if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
        if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
        if (year == 2014 | year == 2012) { unziperogaciones = "_erogaciones_"} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unziperogaciones, year, "_concil_2010_", fformat, ".", fformat))
        data.enigh.N3$id = paste0(as.character(data.enigh.N3$folioviv), as.character(data.enigh.N3$foliohog))
        data.enigh$folioviv = NULL
        data.enigh$foliohog = NULL
        data.enigh    = dplyr::full_join(data.enigh, data.enigh.N3, by="id", type="left", match = "all")
      } # Endo of Nest_3 Erogaciones
      ### GASTO HOGAR
      else if (gastohogar  == TRUE & concentrado == FALSE & erogaciones == FALSE & gastotarjetas == FALSE & poblacion == FALSE){
        if (year == 2014 & construccion == "Tradicional") {p.cons ="tra"}
        unzipgastohogar = "_Gastohogar_"
        url.gastohogar = paste0(url.base, p.cons, unzipgastohogar, year, "_concil_2010_", fformat, ".zip")
        utils::download.file(url.gastohogar, temp.enigh)
        utils::unzip(temp.enigh, exdir = zipdir)
        if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
        if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
        if (year == 2014 | year == 2012) { unzipgastohogar = "_gastohogar_"} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipgastohogar, year, "_concil_2010_", fformat, ".", fformat))
        data.enigh.N3$id = paste0(as.character(data.enigh.N3$folioviv), as.character(data.enigh.N3$foliohog))
        data.enigh$folioviv = NULL
        data.enigh$foliohog = NULL
        data.enigh    = dplyr::full_join(data.enigh, data.enigh.N3, by="id", type="left", match = "all")
      } # End of Nest_3 Gasto Hogar
      ### GASTO TARJETA
      else if (gastotarjetas == TRUE & concentrado == FALSE & erogaciones == FALSE & gastohogar == FALSE & poblacion == FALSE){
        if (year == 2014 & construccion == "Tradicional") {p.cons ="tra"}
        unzipgasto.tarjetas = "_Gastotarjetas_"
        url.gasto.tarjetas = paste0(url.base, p.cons, unzipgasto.tarjetas, year, "_concil_2010_", fformat, ".zip")
        utils::download.file(url.gasto.tarjetas, temp.enigh)
        utils::unzip(temp.enigh, exdir = zipdir)
        if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
        if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
        if (year == 2014 | year == 2012) { unzipgasto.tarjetas = "_gastotarjetas_"} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipgasto.tarjetas, year, "_concil_2010_", fformat, ".", fformat))
        data.enigh.N3$id = paste0(as.character(data.enigh.N3$folioviv), as.character(data.enigh.N3$foliohog))
        data.enigh$folioviv = NULL
        data.enigh$foliohog = NULL
        data.enigh    = dplyr::full_join(data.enigh, data.enigh.N3, by="id", type="left", match = "all")
      } # End of Nest_3 Gasto Tarjetas
      ### POBLACION
      else if (poblacion == TRUE & concentrado == FALSE & erogaciones == FALSE & gastohogar == FALSE & gastotarjetas == FALSE){
        if (year == 2014 & construccion == "Tradicional") {p.cons ="tra"}
        unzipoblacion = "_Poblacion_"
        url.poblacion = paste0(url.base, p.cons, unzipoblacion, year, "_concil_2010_", fformat, ".zip")
        utils::download.file(url.poblacion, temp.enigh)
        utils::unzip(temp.enigh, exdir = zipdir)
        if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
        if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
        if (year == 2014 | year == 2012) { unzipoblacion = "_poblacion_"} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipoblacion, year, "_concil_2010_", fformat, ".", fformat))
        data.enigh.N3$id = paste0(as.character(data.enigh.N3$folioviv), as.character(data.enigh.N3$foliohog))
        data.enigh$folioviv = NULL
        data.enigh$foliohog = NULL
        data.enigh       = dplyr::full_join(data.enigh, data.enigh.N3, by="id", type="left", match = "all")
        data.enigh$id_renglon = paste0(as.character(data.enigh.N3$folioviv), as.character(data.enigh.N3$foliohog), as.character(data.enigh.N3$numren))
        ### NEST_4 ##########################################################
        if (ingresos == TRUE & gastopersona == FALSE & trabajos == FALSE){
          unzipingresos = "_Ingresos_"
          url.ingresos = paste0(url.base, p.cons, unzipingresos, year, "_concil_2010_", fformat, ".zip")
          utils::download.file(url.ingresos, temp.enigh)
          utils::unzip(temp.enigh, exdir = zipdir)
          if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
          if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
          if (year == 2014 | year == 2012) { unzipingresos = "_ingresos_"} else {}
          data.enigh.N4 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipingresos, year, "_concil_2010_", fformat, ".", fformat))
          data.enigh.N4$id_renglon = paste0(as.character(data.enigh.N4$folioviv), as.character(data.enigh.N4$foliohog), as.character(data.enigh.N4$numren))
          data.enigh$numren = NULL
          data.enigh = dplyr::full_join(data.enigh, data.enigh.N4, by="id_renglon", type="left", match = "all")
        } # End of Nest_4 Ingresos
        if (gastopersona == TRUE & ingresos == FALSE & trabajos == FALSE){
          unzipgasto.persona = "_Gastopersona_"
          url.gasto.persona = paste0(url.base, p.cons, unzipgasto.persona, year, "_concil_2010_", fformat, ".zip")
          utils::download.file(url.gasto.persona, temp.enigh)
          utils::unzip(temp.enigh, exdir = zipdir)
          if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
          if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
          if (year == 2014 | year == 2012) { unzipgasto.persona = "_gastopersona_"} else {}
          data.enigh.N4 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipgasto.persona, year, "_concil_2010_", fformat, ".", fformat))
          data.enigh.N4$id_renglon = paste0(as.character(data.enigh.N4$folioviv), as.character(data.enigh.N4$foliohog), as.character(data.enigh.N4$numren))
          data.enigh$numren = NULL
          data.enigh = dplyr::full_join(data.enigh, data.enigh.N4, by="id_renglon", type="left", match = "all")
        } # End of Nest_4 Gasto Persona
        if (trabajos == TRUE & ingresos == FALSE & gastopersona == FALSE){
          unziptrabajos = "_Trabajos_"
          url.trabajos = paste0(url.base, p.cons, unziptrabajos, year, "_concil_2010_", fformat, ".zip")
          utils::download.file(url.trabajos, temp.enigh)
          utils::unzip(temp.enigh, exdir = zipdir)
          if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
          if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
          if (year == 2014 | year == 2012) { unziptrabajos = "_trabajos_"} else {}
          data.enigh.N4 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unziptrabajos, year, "_concil_2010_", fformat, ".", fformat))
          data.enigh.N4$id_renglon = paste0(as.character(data.enigh.N4$folioviv), as.character(data.enigh.N4$foliohog), as.character(data.enigh.N4$numren))
          data.enigh$numren = NULL
          data.enigh$folioviv = NULL
          data.enigh$foliohog = NULL
          data.enigh = dplyr::full_join(data.enigh, data.enigh.N4, by="id_renglon", type="left", match = "all")
          data.enigh$fk_trabajo = paste0(as.character(data.enigh$id_renglon), as.character(data.enigh$id_trabajo))
          ### NEST_5 ###########################################################
          if (agro == TRUE & noagro == FALSE){
            unzipagro = "_agropecuario_"
            url.agro = paste0(url.base, p.cons, unzipagro, year, "_concil_2010_", fformat, ".zip")
            utils::download.file(url.agro, temp.enigh)
            utils::unzip(temp.enigh, exdir = zipdir)
            if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
            if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
            #if (year == 2014 | year == 2012) { unzipagro = "_agropecuario_"} else {}
            data.enigh.N5 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipagro, year, "_concil_2010_", fformat, ".", fformat))
            data.enigh.N5$fk_trabajo = paste0(as.character(data.enigh.N5$folioviv), as.character(data.enigh.N5$foliohog), as.character(data.enigh.N5$numren), as.character(data.enigh.N5$id_trabajo))
            data.enigh$id_trabajo = NULL
            data.enigh$numren = NULL
            data.enigh$folioviv = NULL
            data.enigh$foliohog = NULL
            data.enigh = dplyr::full_join(data.enigh, data.enigh.N5, by="fk_trabajo", type="left", match = "all")
          } # End of Nest_5 Agro
          if (noagro == TRUE & agro == FALSE){
            unzipnoagro = "_Noagropecuario_"
            url.noagro = paste0(url.base, p.cons, unzipnoagro, year, "_concil_2010_", fformat, ".zip")
            utils::download.file(url.noagro, temp.enigh)
            utils::unzip(temp.enigh, exdir = zipdir)
            if ((year == 2014 | year == 2012) & construccion == "Nueva" ) { p.cons = "ncv"} else {}
            if ((year == 2014 | year == 2012) & construccion == "Tradicional" ) { p.cons = "tra"} else {}
            if (year == 2014 | year == 2012) { unzipagro = "_noagropecuario_"} else {}
            data.enigh.N5 = foreign::read.dbf(paste0(zipdir, "\\\\", p.cons, unzipnoagro, year, "_concil_2010_", fformat, ".", fformat))
            data.enigh.N5$fk_trabajo = paste0(as.character(data.enigh.N5$folioviv), as.character(data.enigh.N5$foliohog), as.character(data.enigh.N5$numren), as.character(data.enigh.N5$id_trabajo))
            data.enigh$id_trabajo = NULL
            data.enigh$numren = NULL
            data.enigh$folioviv = NULL
            data.enigh$foliohog = NULL
            data.enigh = dplyr::full_join(data.enigh, data.enigh.N5, by="fk_trabajo", type="left", match = "all")
          } # End of Nest_5 NoAgro
          data.enigh$fk_trabajo = NULL
        } # End Nest_4 Trabajos
        data.enigh$id_renglon = NULL
      } #End of Nest_3 Población
      else {print("Advertencia: Concentrado, erogaciones, gastohogar, gastotarjeta y poblacion no pueden ser TRUE al mismo tiempo")} # End Nest_3 Poblacion
    }  # Nest_2.1 Hogares. Get rid off years 08 and 10.
  } # End if Nest_2 Hogares
  data.enigh$id = NULL
  return(data.enigh)
} # End of Function
