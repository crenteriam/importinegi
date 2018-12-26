#' Censos y Conteos de Población y Vivienda.
#' ITER
#'
#' @param year Año requerido del censo o conteo. En esta versión están disponibles 2000, 2005, 2010 y 2015.
#' @param estado Descarga datos por estado. Utilizar el nombre del estado, con espacios. Ejemplos: "Aguascalientes", "CDMX", "San Luis Potosi", "Nacional.
#' @param totalestado TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel estado.
#' @param totalmunici TRUE para obtener, además de la información a nivel localidad, resumen estadístico a nivel municipio.
#' @param nolocalidad TRUE para no obtener información observaciones a nivel localidad.
#' @param muestra Para el formato "Muestra", utilizar "Migrantes", "Personas", o "Viviendas" según el tipo de base de datos requerida.



#5 Muestra --------------------------------------------------------------------------------------------
else if (type == "Muestra"){

  if (muestra == "Migrantes" | muestra == "Personas" | muestra == "Viviendas"){
    #1 GET URL
    censo.url.muestra =  paste0(inegi.base, year, "/microdatos/mpv/MC", year, "_", censo.state, "_", fformat, ".zip")
    #2 DOWNLOAD TEMPORARY FILE
    censo.temp.muestra = base::tempfile()
    download.file(censo.url.muestra, censo.temp.muestra)
    #3 Unzip, read file and fix missing. Unlink temporary file
    zipdir = tempfile()
    utils::unzip(censo.temp.muestra, exdir = zipdir)

    if (muestra == "Migrantes") {
      raw.migrantes = paste0(zipdir, "\\Migrantes_", censo.state, ".", fformat)
      data.output.muestra = foreign::read.dbf(raw.migrantes)
    } # End Migrantes 5-Level-Nest
    ##
    else if (muestra == "Personas"){
      raw.personas = paste0(zipdir, "\\Personas_", censo.state, ".", fformat)
      data.output.muestra = foreign::read.dbf(raw.personas)
    } # End Personas 5-Level-Nest
    ##
    else if (muestra == "Viviendas"){
      raw.viviendas = paste0(zipdir, "\\Viviendas_", censo.state, ".", fformat)
      data.output.muestra = foreign::read.dbf(raw.viviendas)
    }  # End Viviendas 5-Level-Nest
  } # End if to proceed 4-Level-Nest
  ##
  else {stop("ERROR: Debes especificar una base de datos del cuestionario ampliado ('Migrantes', 'Personas' o 'Viviendas')")}
  data.output.muestra[data.output.muestra=="*"]<-NA
  return(data.output.muestra)
  ##

} # End of Type-Migrantes Nest 3-Level-Nest


if (year =="2000"){
  censo.year = "2000"
  fformat    = "dbf"
  ###############
  ### FIRST NEST (Type Nest): 1) ITER, 2) Muestra ###
  #2 Muestra --------------------------------------------------------------------------------------------
  else if (type == "Muestra"){

    if (muestra == "Migrantes" | muestra == "Personas" | muestra == "Viviendas"){
      #1 GET URL
      censo.url.muestra =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censo/", year,
                                  "/microdatos/muestra/cgpv", year, "_", censo.state, "_", fformat, ".zip")
      #2 DOWNLOAD TEMPORARY FILE
      censo.temp.muestra = base::tempfile()
      download.file(censo.url.muestra, censo.temp.muestra)
      #3 Unzip, read file and fix missing. Unlink temporary file
      zipdir = tempfile()
      unzip(censo.temp.muestra, exdir = zipdir)

      if (muestra == "Migrantes") {
        raw.migrantes = paste0(zipdir, "\\MIN_F", censo.state, ".", fformat)
        data.output.muestra = foreign::read.dbf(raw.migrantes)
      } # End Migrantes 5-Level-Nest
      ##
      else if (muestra == "Personas"){
        raw.personas = paste0(zipdir, "\\PER_F", censo.state, ".", fformat)
        data.output.muestra = foreign::read.dbf(raw.personas)
      } # End Personas 5-Level-Nest
      ##
      else if (muestra == "Viviendas"){
        raw.viviendas = paste0(zipdir, "\\VHO_F", censo.state, ".", fformat)
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
  censo.year = "1990"
  fformat    = "dbf"
  ###############
  ### FIRST NEST (Type Nest): 1) ITER, 2) Muestra ###

  #2 Muestra --------------------------------------------------------------------------------------------
  else if (type == "Muestra"){

    #1 GET URL
    censo.url.muestra =  paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censo/", year,
                                "/microdatos/cgpv90p_", censo.state, "_", fformat, ".zip")
    #2 DOWNLOAD TEMPORARY FILE
    censo.temp.muestra = base::tempfile()
    download.file(censo.url.muestra, censo.temp.muestra)

    #3 Unzip, read file and fix missing. Unlink temporary file
    data.output.muestra = foreign::read.dbf((utils::unzip(censo.temp.muestra)))
    data.output.muestra[data.output.muestra=="*"]<-NA
    return(data.output.muestra)
    ##

  } # End of Type-Migrantes Nest 3-Level-Nest

  # ---------------------------------------------------------------------------------------------------
  else {} # End of Type Nest: Muestra 2-Level-Nest

} # End of Year 1990 1-Level-Nest
