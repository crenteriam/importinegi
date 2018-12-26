#' Censos y Conteos de Población y Vivienda.
#'
#' @param type Proyecto INEGI. Opciones: "iter", "ageb", "EntornoUrbano", "Rural", y "Muestra". La opción "Muestra" tiene argumentos adicionales.
#' @param year Año requerido del censo o conteo. En esta versión están disponibles 2000, 2005, 2010 y 2015.
#' @param state Descarga datos por estado. Utilizar el nombre del estado, sin espacios. Ejemplos: "Aguascalientes", "CDMX", "SanLuisPotosi". NA/Default = Nacional.
#' @param totalent Para el formato "iter", TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel estado.
#' @param totalmun Para el formato "iter", TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel municipio.
#' @param noloc Para el formato "iter", TRUE para no obtener información observaciones a nivel localidad.
#' @param totalageb Para el formato "ageb", TRUE para obtener, además de la información a nivel manzana, resumen estadístico a nivel ageb.
#' @param muestra Para el formato "Muestra", utilizar "Migrantes", "Personas", o "Viviendas" según el tipo de base de datos requerida.

#TODO: Change all datasets factors -> numeric
#https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-structure
censo_poblacion <- function(type = "iter", year = "2010", state = "Nacional", totalent = FALSE, totalmun = FALSE, noloc = FALSE, totalageb = FALSE, muestra = NA){
  library(foreign)

  print("censo_poblacion() Versión 1.0. Años disponibles: 2000, 2005, 2010 y 2015")

  ####### STATE ########
  if      (state == "Aguascalientes"){   ccpv.state = "01" }
  else if (state == "BajaCalifornia"){ ccpv.state = "02" }
  else if (state == "BajaCaliforniaSur"){ ccpv.state = "03"}
  else if (state == "Campeche"){ ccpv.state = "04"}
  else if (state == "Coahuila"){ ccpv.state = "05"}
  else if (state == "Colima")  { ccpv.state = "06"}
  else if (state == "Chiapas") { ccpv.state = "07"}
  else if (state == "Chihuahua"){ccpv.state = "08"}
  else if (state == "CDMX")    { ccpv.state = "09"}
  else if (state == "Durango") { ccpv.state = "10"}
  else if (state == "Guanajuato"){ccpv.state = "11"}
  else if (state == "Guerrero"){ ccpv.state = "12"}
  else if (state == "Hidalgo") { ccpv.state = "13"}
  else if (state == "Jalisco") { ccpv.state = "14"}
  else if (state == "Mexico")  { ccpv.state = "15"}
  else if (state == "Michoacan"){ccpv.state = "16"}
  else if (state == "Morelos") { ccpv.state = "17"}
  else if (state == "Nayarit") { ccpv.state = "18"}
  else if (state == "NuevoLeon"){ccpv.state = "19"}
  else if (state == "Oaxaca")  { ccpv.state = "20"}
  else if (state == "Puebla")  { ccpv.state = "21"}
  else if (state == "Queretaro"){ccpv.state = "22"}
  else if (state == "QuintanaRoo"){ccpv.state = "23"}
  else if (state == "SanLuisPotosi"){ccpv.state = "24"}
  else if (state == "Sinaloa") { ccpv.state = "25"}
  else if (state == "Sonora")  { ccpv.state = "26"}
  else if (state == "Tabasco") { ccpv.state = "27"}
  else if (state == "Tamaulipas"){ccpv.state = "28"}
  else if (state == "Tlaxcala"){ ccpv.state = "29"}
  else if (state == "Veracruz"){ ccpv.state = "30"}
  else if (state == "Yucatan") { ccpv.state = "31"}
  else if (state == "Zacatecas"){ccpv.state = "32"}
  else if (state == "Nacional") {ccpv.state = "00"}
  else {stop("Argumento requerido: nombra un estado o 'Nacional'")}

  #############################################################################################
  ### YEAR 2010
  #############################################################################################
  if (year =="2010"){
    ccpv.year = "2010"
    fformat    = "dbf"

    ###############
    ### FIRST NEST (Type Nest): 1) ITER, 2) AGEB, 3) EntornoUrbano; 4) Rural, 5) Muestra ###
    #1 ITER -----------------------------------------------------------------------------------
    if (type=="iter"){
      #1 GET URL
      ccpv.url.iter  = paste0("http://www.beta.inegi.org.mx/contenidos/programas/ccpv/", year, "/microdatos/", type, "/", ccpv.state, "_",
                              tolower(state), "_", year, "_", type, "_dbf.zip")
      #2 DOWNLOAD TEMPORARY FILE
      ccpv.temp.iter = base::tempfile()
      download.file(ccpv.url.iter, ccpv.temp.iter)

      #3 Unzip, read file and fix missing. Unlink temporary file
      data.output.iter = foreign::read.dbf((utils::unzip(ccpv.temp.iter)))
      data.output.iter[data.output.iter=="*"]<-NA

      #4 TOTALENT - Total Entidad Federativa
      if (totalent == FALSE) { data.output.iter <- base::subset(data.output.iter, MUN!="000")} else {}

      #5 TOTALMUN - Total Municipio
      if (totalmun == FALSE) {data.output.iter <- base::subset(data.output.iter, LOC!="0000")} else {}

      #NOLOC -> No extraer valores para localidades
      if (noloc == TRUE) {data.output.iter <- base::subset(data.output.iter, MUN=="000" | LOC=="0000")} else {}

      base::unlink(ccpv.temp.iter)
      return(data.output.iter)

    } #End of type->iter 2-nest

    #2 AGEB -----------------------------------------------------------------------------------
    else if (type == "ageb") {
      #1 GET URL
      ccpv.url.ageb =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/", year,
                              "/microdatos/ageb_y_manzana/resageburb_", ccpv.state, "_", year, "_", fformat, ".zip")
      #2 DOWNLOAD TEMPORARY FILE
      ccpv.temp.ageb = base::tempfile()
      download.file(ccpv.url.ageb, ccpv.temp.ageb)
      #3 Unzip, read file and fix missing. Unlink temporary file
      data.output.ageb = foreign::read.dbf((utils::unzip(ccpv.temp.ageb)))
      data.output.ageb[data.output.ageb=="*"]<-NA
      #4 Eliminate totals
      if (totalent == FALSE) { data.output.ageb <- base::subset(data.output.ageb, MUN!="000")} else {}
      if (totalmun == FALSE) {data.output.ageb <- base::subset(data.output.ageb, LOC!="0000")} else {}
      if (totalageb == FALSE) {
        data.output.ageb <- base::subset(data.output.ageb, MZA!="000")
        data.output.ageb <- base::subset(data.output.ageb, AGEB!="0000")} else {}

      return(data.output.ageb)
    } # End of type->ageb 2-nest

    #3 EntornoUrbano -----------------------------------------------------------------------------------
    else if (type == "EntornoUrbano"){
      #1 GET URL
      ccpv.url.eu =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/",
                            year, "/microdatos/urbano/MANZANA_EU_", ccpv.state, "_", fformat, ".zip")
      #2 DOWNLOAD TEMPORARY FILE
      ccpv.temp.eu = base::tempfile()
      download.file(ccpv.url.eu, ccpv.temp.eu)
      #3 Unzip, read file and fix missing. Unlink temporary file
      data.output.eu = foreign::read.dbf((utils::unzip(ccpv.temp.eu)))
      return(data.output.eu)
    } # End of type->EntornoUrbano nest

    #4 Rural --------------------------------------------------------------------------------------------
    else if (type == "Rural"){
      #1 GET URL
      ccpv.url.rural =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/", year,
                               "/microdatos/cinco_mil_menos/resloc_", ccpv.state, "_", year, "_", fformat, ".zip")
      #2 DOWNLOAD TEMPORARY FILE
      ccpv.temp.rural = base::tempfile()
      download.file(ccpv.url.rural, ccpv.temp.rural)
      #3 Unzip, read file and fix missing. Unlink temporary file
      data.output.rural = foreign::read.dbf((utils::unzip(ccpv.temp.rural)))
      data.output.rural[data.output.rural=="*"]<-NA
      return(data.output.rural)

    } # End of type->Rural 2-nest

    #5 Muestra --------------------------------------------------------------------------------------------
    else if (type == "Muestra"){

      if (muestra == "Migrantes" | muestra == "Personas" | muestra == "Viviendas"){
        #1 GET URL
        ccpv.url.muestra =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/", year,
                                   "/microdatos/mpv/MC", year, "_", ccpv.state, "_", fformat, ".zip")
        #2 DOWNLOAD TEMPORARY FILE
        ccpv.temp.muestra = base::tempfile()
        download.file(ccpv.url.muestra, ccpv.temp.muestra)
        #3 Unzip, read file and fix missing. Unlink temporary file
        zipdir = tempfile()
        utils::unzip(ccpv.temp.muestra, exdir = zipdir)

        if (muestra == "Migrantes") {
          raw.migrantes = paste0(zipdir, "\\Migrantes_", ccpv.state, ".", fformat)
          data.output.muestra = foreign::read.dbf(raw.migrantes)
        } # End Migrantes 5-Level-Nest
        ##
        else if (muestra == "Personas"){
          raw.personas = paste0(zipdir, "\\Personas_", ccpv.state, ".", fformat)
          data.output.muestra = foreign::read.dbf(raw.personas)
        } # End Personas 5-Level-Nest
        ##
        else if (muestra == "Viviendas"){
          raw.viviendas = paste0(zipdir, "\\Viviendas_", ccpv.state, ".", fformat)
          data.output.muestra = foreign::read.dbf(raw.viviendas)
        }  # End Viviendas 5-Level-Nest
      } # End if to proceed 4-Level-Nest
      ##
      else {stop("ERROR: Debes especificar una base de datos del cuestionario ampliado ('Migrantes', 'Personas' o 'Viviendas')")}
      data.output.muestra[data.output.muestra=="*"]<-NA
      return(data.output.muestra)
      ##

    } # End of Type-Migrantes Nest 3-Level-Nest

    # ---------------------------------------------------------------------------------------------------
    else {} # End of Type Nest: Muestra 2-Level-Nest

  } # End of Year 2010 1-Level-Nest

  #############################################################################################
  ### YEAR 2000
  #############################################################################################
  if (year =="2000"){
    ccpv.year = "2000"
    fformat    = "dbf"
    ###############
    ### FIRST NEST (Type Nest): 1) ITER, 2) Muestra ###
    #1 ITER -----------------------------------------------------------------------------------
    if (type=="iter"){
      #1 GET URL
      ccpv.url.iter  = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/", year, "/microdatos/", type, "/", type, "_",
                              ccpv.state, "_", year, "_", fformat, ".zip")
      #2 DOWNLOAD TEMPORARY FILE
      ccpv.temp.iter = base::tempfile()
      download.file(ccpv.url.iter, ccpv.temp.iter)

      #3 Unzip, read file and fix missing. Unlink temporary file
      data.output.iter = foreign::read.dbf((utils::unzip(ccpv.temp.iter)))
      data.output.iter[data.output.iter=="*"]<-NA

      #4 TOTALENT - Total Entidad Federativa
      if (totalent == FALSE) { data.output.iter <- base::subset(data.output.iter, MUN!="000")} else {}

      #5 TOTALMUN - Total Municipio
      if (totalmun == FALSE) {data.output.iter <- base::subset(data.output.iter, LOC!="0000")} else {}

      #NOLOC -> No extraer valores para localidades
      if (noloc == TRUE) {data.output.iter <- base::subset(data.output.iter, MUN=="000" | LOC=="0000")} else {}

      base::unlink(ccpv.temp.iter)
      return(data.output.iter)

    } #End of type->iter 2-Level-Nest

    #2 Muestra --------------------------------------------------------------------------------------------
    else if (type == "Muestra"){

      if (muestra == "Migrantes" | muestra == "Personas" | muestra == "Viviendas"){
        #1 GET URL
        ccpv.url.muestra =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/", year,
                                   "/microdatos/muestra/cgpv", year, "_", ccpv.state, "_", fformat, ".zip")
        #2 DOWNLOAD TEMPORARY FILE
        ccpv.temp.muestra = base::tempfile()
        download.file(ccpv.url.muestra, ccpv.temp.muestra)
        #3 Unzip, read file and fix missing. Unlink temporary file
        zipdir = tempfile()
        unzip(ccpv.temp.muestra, exdir = zipdir)

        if (muestra == "Migrantes") {
          raw.migrantes = paste0(zipdir, "\\MIN_F", ccpv.state, ".", fformat)
          data.output.muestra = foreign::read.dbf(raw.migrantes)
        } # End Migrantes 5-Level-Nest
        ##
        else if (muestra == "Personas"){
          raw.personas = paste0(zipdir, "\\PER_F", ccpv.state, ".", fformat)
          data.output.muestra = foreign::read.dbf(raw.personas)
        } # End Personas 5-Level-Nest
        ##
        else if (muestra == "Viviendas"){
          raw.viviendas = paste0(zipdir, "\\VHO_F", ccpv.state, ".", fformat)
          data.output.muestra = foreign::read.dbf(raw.viviendas)
        }  # End Viviendas 5-Level-Nest
      } # End if to proceed 4-Level-Nest
      ##
      else {stop("ERROR: Debes especificar una base de datos del cuestionario ampliado ('Migrantes', 'Personas' o 'Viviendas')")}
      data.output.muestra[data.output.muestra=="*"]<-NA
      return(data.output.muestra)
      ##

    } # End of Type-Migrantes Nest 3-Level-Nest

    # ---------------------------------------------------------------------------------------------------
    else {} # End of Type Nest: Muestra 2-Level-Nest

  } # End of Year 2000 1-Level-Nest

  #############################################################################################
  ### YEAR 1990
  #############################################################################################
  if (year =="1990"){
    ccpv.year = "1990"
    fformat    = "dbf"
    ###############
    ### FIRST NEST (Type Nest): 1) ITER, 2) Muestra ###
    #1 ITER -----------------------------------------------------------------------------------
    if (type=="iter"){
      #1 GET URL
      ccpv.url.iter  = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/", year, "/microdatos/", type, "_",
                              ccpv.state, "_", year, "_", fformat, ".zip")
      #2 DOWNLOAD TEMPORARY FILE
      ccpv.temp.iter = base::tempfile()
      download.file(ccpv.url.iter, ccpv.temp.iter)

      #3 Unzip, read file and fix missing. Unlink temporary file
      data.output.iter = foreign::read.dbf((utils::unzip(ccpv.temp.iter)))
      data.output.iter[data.output.iter=="*"]<-NA

      #4 TOTALENT - Total Entidad Federativa
      if (totalent == FALSE) { data.output.iter <- base::subset(data.output.iter, MUN!="000")} else {}

      #5 TOTALMUN - Total Municipio
      if (totalmun == FALSE) {data.output.iter <- base::subset(data.output.iter, LOC!="0000")} else {}

      #NOLOC -> No extraer valores para localidades
      if (noloc == TRUE) {data.output.iter <- base::subset(data.output.iter, MUN=="000" | LOC=="0000")} else {}

      base::unlink(ccpv.temp.iter)
      return(data.output.iter)

    } #End of type->iter 2-Level-Nest

    #2 Muestra --------------------------------------------------------------------------------------------
    else if (type == "Muestra"){

      #1 GET URL
      ccpv.url.muestra =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/", year,
                                 "/microdatos/cgpv90p_", ccpv.state, "_", fformat, ".zip")
      #2 DOWNLOAD TEMPORARY FILE
      ccpv.temp.muestra = base::tempfile()
      download.file(ccpv.url.muestra, ccpv.temp.muestra)

      #3 Unzip, read file and fix missing. Unlink temporary file
      data.output.muestra = foreign::read.dbf((utils::unzip(ccpv.temp.muestra)))
      data.output.muestra[data.output.muestra=="*"]<-NA
      return(data.output.muestra)
      ##

    } # End of Type-Migrantes Nest 3-Level-Nest

    # ---------------------------------------------------------------------------------------------------
    else {} # End of Type Nest: Muestra 2-Level-Nest

  } # End of Year 1990 1-Level-Nest

  #############################################################################################
  #############################################################################################
  else {stop("ERROR: Año no válido o no soportado en esta versión. La función ccpv no regresa datos del Conteo de Población y Vivienda")}
  ###### TODO: ADD MORE YEARS. TH PROBLEM IS THAT THE STRUCTURE CHANGES.

} # End of Function
