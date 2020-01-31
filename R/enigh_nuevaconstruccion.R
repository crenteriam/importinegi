#' ENIGH Nueva Construccion (2008-2014)
#'
#' Descarga datos de la Encuesta Nacional de Ingreso y Gasto de los Hogares, Nueva Construccion (2008-2014).
#'
#' La ENIGH provee informacion estadisticas sobre los ingresos y gastos de los hogares en cuanto a su monto, procedencia y distribucion. Adicionalmente, la ENIGH provee informacion sobre las caracteristicas socio-demograficas de los integrantes del hogar.
#'
#' @param year Año de levantamiento de la encuesta en formato numerico. Los años disponibles son 2008, 2010, 2012 y 2014
#' @param datos Base de datos a descargar "viviendas" "hogares" "concentrado" "erogaciones" "gastohogar" "gastotarjetas" "poblacion" "ingresos" "gastopersona" "trabajos" "agro" "noagro"
#' @examples
#'
#' # Descargar datos de hogares
#' \donttest{hogares14 = enigh_nuevaconstruccion(year = 2014,  datos = "hogares")}
#' @return Data.frame
#' @export


enigh_nuevaconstruccion = function(year = NA, datos = NA){

# General
enigh.nueva.base = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/"
nuevaconstruccion = "NCV"

# 1 Obtener URL   ==================================================================
url.base = paste0(enigh.nueva.base, year, "/microdatos/")
zipdir = tempfile()
temp.enigh = base::tempfile()
formato_archivo = "dbf"

# 2 Descargar base de datos  =======================================================
# Loops for years
if (year == 2012) {nuevaconstruccion = tolower(nuevaconstruccion)} else{}

### 2.1 Viviendas  -----------------------------------------------------------------
    if (datos == "viviendas" | (datos == "hogares" & (year ==2010 | year == 2008))) { # Las bases de datos de vivienda y hogares estan unidas para los an~os 2010 y 2008.
        # Descargar archivo
        vivienda = "NCV_Vivi_"
        if (year == 2012) {vivienda = "ncv_viviendas_"} else if (year == 2010 | year == 2008) {vivienda = "NCV_Hogares_"} else {}
        url.vivienda = paste0(url.base, vivienda, year, "_concil_2010_", formato_archivo, ".zip")
        utils::download.file(url.vivienda, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2012) {vivienda = "NCV_viviendas_"} else if (year == 2010 | year == 2008) {vivienda = "NCV_Hogares_"} else {}
        ruta_vivienda = paste0(zipdir, "\\\\", vivienda, year, "_concil_2010")
        if (year == 2010 | year == 2008) {ruta_vivienda =  paste0(ruta_vivienda, "_DBF")} else {}
        data.enigh = foreign::read.dbf(paste0(ruta_vivienda, ".", formato_archivo), as.is = TRUE)
        names(data.enigh) = tolower(names(data.enigh))
    }

### 2.1.1 Hogares  ----------------------------------------------------------------
   if (datos == "hogares") {
      hogares = "NCV_Hogares_"
      # Descargar Datos
      #Sys.sleep(sample(1:3, 1)) # Some sleeping time
      url.hogares = paste0(url.base, hogares, year, "_concil_2010_", formato_archivo, ".zip")
      utils::download.file(url.hogares, temp.enigh)

      # Unzip y Abrir
      utils::unzip(temp.enigh, exdir = zipdir)
      data.enigh.N2 = foreign::read.dbf(paste0(zipdir, "\\\\", hogares, year, "_concil_2010_", formato_archivo, ".", formato_archivo),as.is = TRUE)
      names(data.enigh.N2) = tolower(names(data.enigh.N2))
      #data.enigh    = merge(data.enigh, data.enigh.N2)
      data.enigh     = data.enigh.N2
   } # End Hogares
### 2.1.1.1 Hogares -> Concentrado Hogar  ----------------------------------------------------
      else if (datos == "concentrado") {

        # Descargar Datos
        #Sys.sleep(sample(1:2, 1)) # Some sleeping time
        Concentrado = "_Concentrado_"
        url.concentrado = paste0(url.base, toupper(nuevaconstruccion), Concentrado, year, "_concil_2010_", formato_archivo, ".zip")
        utils::download.file(url.concentrado, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2014 | year == 2012) {Concentrado = tolower(Concentrado)} else {}
        ruta_nest3 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), Concentrado, year, "_concil_2010")
        if (year == 2010 | year == 2008) {ruta_nest3 =  paste0(ruta_nest3, "_DBF")} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(ruta_nest3, ".", formato_archivo), as.is = TRUE)
        names(data.enigh.N3) = tolower(names(data.enigh.N3))
        #if (hogares == TRUE) {data.enigh = merge(data.enigh.N2, data.enigh.N3)}
        data.enigh = data.enigh.N3
        #else {}
      } # End of Nest 3: Concentrado Hogar

### 2.1.1.2 Hogares -> Erogaciones  -----------------------------------------------------------
      else if (datos == "erogaciones"){

        # Descargar archivos
        #Sys.sleep(sample(1:3, 1))
        Erogaciones = "_Erogaciones_"
        url.erogaciones = paste0(url.base, toupper(nuevaconstruccion), Erogaciones, year, "_concil_2010_", formato_archivo, ".zip")
        utils::download.file(url.erogaciones, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2014 | year == 2012) {ruta_nest3 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(Erogaciones), year, "_concil_2010_", formato_archivo)} else {}
        if (year == 2010 | year == 2008) {ruta_nest3 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), Erogaciones, year, "_concil_2010_", toupper(formato_archivo))} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(ruta_nest3, ".", formato_archivo), as.is = TRUE)
        names(data.enigh.N3) = tolower(names(data.enigh.N3))
        #if (hogares == TRUE) {data.enigh = dplyr::full_join(data.enigh, data.enigh.N3, by=c("folioviv", "foliohog"), type="left", match="all")}
        data.enigh = data.enigh.N3
      } # Endo of Erogaciones

### 2.1.1.3 Hogares -> Gasto en el Hogar  ------------------------------------------------------------------
      else if (datos == "gastohogar") {

        # Descargar datos
        #Sys.sleep(sample(1:3, 1))
        gasto_hogar = "_Gastohogar_"
        url.gasto_hogar = paste0(url.base, toupper(nuevaconstruccion), gasto_hogar, year, "_concil_2010_", formato_archivo, ".zip")
        if (year == 2010 | year ==2008) {url.gasto_hogar = paste0(url.base, toupper(nuevaconstruccion), "_Gastos_", year, "_concil_2010_", formato_archivo, ".zip")}
        utils::download.file(url.gasto_hogar, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2014 | year == 2012) {ruta_nest3 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(gasto_hogar), year, "_concil_2010_", formato_archivo)}
        else if (year == 2010 | year == 2008) {ruta_nest3 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), "_Gastos_", year, "_concil_2010_", toupper(formato_archivo))} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(ruta_nest3, ".", formato_archivo), as.is = TRUE)
        names(data.enigh.N3) = tolower(names(data.enigh.N3))
        #if (hogares == TRUE) {data.enigh = dplyr::full_join(data.enigh, data.enigh.N3, by=c("folioviv", "foliohog"), type="left", match="all")}
        data.enigh = data.enigh.N3
      } # End of Gasto Hogar

### 2.1.1.4 Hogares -> Gasto Tarjetas  -----------------------------------------------------------------
      else if (datos == "gastotarjetas") {

        # Descargar datos
        #Sys.sleep(sample(1:3, 1))
        gasto_tarjetas = "_Gastotarjetas_"
        url.gasto_tarjetas = paste0(url.base, toupper(nuevaconstruccion), gasto_tarjetas, year, "_concil_2010_", formato_archivo, ".zip")
        utils::download.file(url.gasto_tarjetas, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2014 | year == 2012) {ruta_nest3 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(gasto_tarjetas), year, "_concil_2010_", formato_archivo)}
        else if (year == 2010 | year == 2008) {ruta_nest3 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), gasto_tarjetas, year, "_concil_2010_", toupper(formato_archivo))} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(ruta_nest3, ".", formato_archivo), as.is = TRUE)
        names(data.enigh.N3) = tolower(names(data.enigh.N3))
        #if (hogares == TRUE) {data.enigh = dplyr::full_join(data.enigh, data.enigh.N3, by=c("folioviv", "foliohog"), type="left", match="all")}
        data.enigh = data.enigh.N3
      } # End Gasto Tarjetas

### 2.1.1.5 Hogares -> Poblacion  ----------------------------------------------------------------------
      else if (datos == "poblacion"){

        # Descargar datos
        #Sys.sleep(sample(1:3, 1))
        Poblacion = "_Poblacion_"
        url.poblacion = paste0(url.base, toupper(nuevaconstruccion), Poblacion, year, "_concil_2010_", formato_archivo, ".zip")
        utils::download.file(url.poblacion, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2014 | year == 2012) {ruta_nest3 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(Poblacion), year, "_concil_2010_", formato_archivo)}
        else if (year == 2010 | year == 2008) {ruta_nest3 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), Poblacion, year, "_concil_2010_", toupper(formato_archivo))} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(ruta_nest3, ".", formato_archivo), as.is = TRUE)
        names(data.enigh.N3) = tolower(names(data.enigh.N3))
        #if (hogares == TRUE) {data.enigh = merge(data.enigh, data.enigh.N3)}
        #if (hogares == TRUE) {data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog"))[data.table::data.table(data.enigh.N3,key=c("folioviv", "foliohog")), allow.cartesian=TRUE]}
        #if (hogares == TRUE) {data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog"))[data.table::data.table(data.enigh.N3,key=c("folioviv", "foliohog")), allow.cartesian=TRUE]}
        data.enigh = data.enigh.N3
        } # End Poblacion

        ### Nest 4: Poblacion -> Ingresos  ---------------------------------------------------
        #if (ingresos == TRUE & poblacion == FALSE) {stop(message("Poblacion debe ser TRUE"))} else {}
        if (datos == "ingresos") {

          # Descargar datos
          #Sys.sleep(sample(1:3, 1))
          n4_ingresos = "_Ingresos_"
          url.ingresos = paste0(url.base, toupper(nuevaconstruccion), n4_ingresos, year, "_concil_2010_", formato_archivo, ".zip")
          utils::download.file(url.ingresos, temp.enigh)

          # Unzip y abrir
          utils::unzip(temp.enigh, exdir = zipdir)
          if (year == 2014 | year == 2012) {ruta_nest4 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(n4_ingresos), year, "_concil_2010_", formato_archivo, ".", formato_archivo)}
          else if (year == 2010 | year == 2008) {ruta_nest4 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), n4_ingresos, year, "_concil_2010_", toupper(formato_archivo), ".", formato_archivo)} else {}
          data.enigh.N4 = foreign::read.dbf(ruta_nest4, as.is = TRUE)
          names(data.enigh.N4) = tolower(names(data.enigh.N4))
          #if (hogares == FALSE | poblacion == FALSE) {stop(message("Hogares y Poblacion deben ser TRUE"))}
          #data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren"))[data.table::data.table(data.enigh.N4,key=c("folioviv", "foliohog", "numren")), allow.cartesian=TRUE]
          data.enigh = data.enigh.N4
        } # End of Nest_4 Ingresos

        ### Nest 4: Poblacion -> Gastopersona  ----------------------------------------------
        else if (datos == "gastopersona"){

          # Descargar datos
          #Sys.sleep(sample(1:3, 1))
          n4_gastopersona = "_Gastopersona_"
          url.ingresos = paste0(url.base, toupper(nuevaconstruccion), n4_gastopersona, year, "_concil_2010_", formato_archivo, ".zip")
          utils::download.file(url.ingresos, temp.enigh)

          # Unzip y abrir
          utils::unzip(temp.enigh, exdir = zipdir)
          if (year == 2014 | year == 2012) {ruta_nest4 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(n4_gastopersona), year, "_concil_2010_", formato_archivo, ".", formato_archivo)}
          else if (year == 2010 | year == 2008) {ruta_nest4 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), n4_gastopersona, year, "_concil_2010_", toupper(formato_archivo), ".", formato_archivo)} else {}
          data.enigh.N4 = foreign::read.dbf(ruta_nest4, as.is = TRUE)
          names(data.enigh.N4) = tolower(names(data.enigh.N4))
          #if (hogares == FALSE | poblacion == FALSE) {stop(message("Hogares y Poblacion deben ser TRUE"))}
          #data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren"))[data.table::data.table(data.enigh.N4,key=c("folioviv", "foliohog", "numren")), allow.cartesian=TRUE]
          data.enigh = data.enigh.N4
        } # End of Nest_4 Gasto Persona

        ### Nest 4: Poblacion - > Trabajos  -----------------------------------------------
        else if (datos == "trabajos"){

          # Descargar
          #Sys.sleep(sample(1:3, 1))
          n4_trabajos = "_Trabajos_"
          url.trabajos = paste0(url.base, toupper(nuevaconstruccion), n4_trabajos, year, "_concil_2010_", formato_archivo, ".zip")
          utils::download.file(url.trabajos, temp.enigh)

          # Unzip y abrir
          utils::unzip(temp.enigh, exdir = zipdir)
          if (year == 2014 | year == 2012) {ruta_nest4 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(n4_trabajos), year, "_concil_2010_", formato_archivo, ".", formato_archivo)}
          else if (year == 2010 | year == 2008) {ruta_nest4 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), n4_trabajos, year, "_concil_2010_", toupper(formato_archivo), ".", formato_archivo)} else {}
          data.enigh.N4 = foreign::read.dbf(ruta_nest4, as.is = TRUE)
          names(data.enigh.N4) = tolower(names(data.enigh.N4))
          #data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren"))[data.table::data.table(data.enigh.N4,key=c("folioviv", "foliohog", "numren")), allow.cartesian=TRUE]
          #if (hogares == FALSE | poblacion == FALSE) {stop(message("Hogares o Poblacion deben ser TRUE"))}
          data.enigh = data.enigh.N4
          #data.enigh$fk_trabajo = paste0(as.character(data.enigh$id_renglon), as.character(data.enigh$id_trabajo))
          } # End Trabajos

            ### ==========================================================================
            ### NEST 5: Poblacion -> Trabajos -> Agro  -----------------------------------
            else if (datos == "agro"){

              # Descargar
              #Sys.sleep(sample(1:3, 1))
              n5_agro = "_agropecuario_"
              url.agro = paste0(url.base, toupper(nuevaconstruccion), n5_agro, year, "_concil_2010_", formato_archivo, ".zip")
              utils::download.file(url.agro, temp.enigh)

              # Unzip y abrir
              utils::unzip(temp.enigh, exdir = zipdir)
              if (year == 2014 | year == 2012) {ruta_nest5 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(n5_agro), year, "_concil_2010_", formato_archivo, ".", formato_archivo)}
              else if (year == 2010 | year == 2008) {ruta_nest5 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), n5_agro, year, "_concil_2010_", toupper(formato_archivo), ".", formato_archivo)} else {}
              data.enigh.N5 = foreign::read.dbf(ruta_nest5, as.is = TRUE)
              names(data.enigh.N5) = tolower(names(data.enigh.N5))
              #data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren", "id_trabajo"))[data.table::data.table(data.enigh.N5,key=c("folioviv", "foliohog", "numren", "id_trabajo")), allow.cartesian=TRUE]
              #if (trabajos == FALSE) {stop(message("Trabajos debe ser TRUE"))}
              data.enigh = data.enigh.N5
            } # End of Agro

           ### Nest 5: Poblacion -> Trabajos -> NoAgro  --------------------------------
            else if (datos == "noagro"){

              # Descargar
              #Sys.sleep(sample(1:3, 1))
              n5_noagro = "_Noagropecuario_"
              url.noagro = paste0(url.base, toupper(nuevaconstruccion), n5_noagro, year, "_concil_2010_", formato_archivo, ".zip")
              utils::download.file(url.noagro, temp.enigh)

              # Unzip y abrir
              utils::unzip(temp.enigh, exdir = zipdir)
              if (year == 2014 | year == 2012) {ruta_nest5 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(n5_noagro), year, "_concil_2010_", formato_archivo, ".", formato_archivo)}
              else if (year == 2010 | year == 2008) {ruta_nest5 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), n5_noagro, year, "_concil_2010_", toupper(formato_archivo), ".", formato_archivo)} else {}
              data.enigh.N5 = foreign::read.dbf(ruta_nest5, as.is = TRUE)
              names(data.enigh.N5) = tolower(names(data.enigh.N5))
              #data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren", "id_trabajo"))[data.table::data.table(data.enigh.N5,key=c("folioviv", "foliohog", "numren", "id_trabajo")), allow.cartesian=TRUE]
              #if (trabajos == FALSE) {stop(message("Trabajos debe ser TRUE"))}
              data.enigh = data.enigh.N5
            } # End of NoAgro

if (exists("data.enigh")) {return(data.enigh)}
else {stop(message("\nLa opcion tecleada en el parametro 'datos' es incorrecta."))}
} # End of Function
