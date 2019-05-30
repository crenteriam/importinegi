#' ENIGH Nueva Construccion (2008-2014)
#'
#' @param year Año de levantamiento de la encuesta
#' @param hogares En esta tabla se encuentran contenidas las características de los hogares que habitan los integrantes de los mismos.
#' @param concentrado Principales variables por hogar. En esta tabla se encuentran las variables construidas a partir de las otras tablas de la base de datos. Registra el resumen concentrado por hogar, de ingresos y gastos en toda modalidad posible. Además todos los ingresos y gastos registrados en esta tabla son trimestrales.
#' @param erogaciones Esta tabla permite identificar las erogaciones financieras y de capital realizadas por hogar.
#' @param gastohogar 	Esta tabla permite identificar los gastos monetarios y no monetarios del hogar.
#' @param gastotarjetas Esta tabla permite identificar en qué medida los hogares se auxilian del financiamiento que les ofrece la tarjeta de crédito bancaria o comercial para cubrir algunas necesidades básicas.
#' @param poblacion Esta tabla proviene de la tabla HOGARES. Identifica las características sociodemográficas de los integrantes del hogar; y el acceso a las instituciones de salud que tienen los integrantes del hogar.
#' @param ingresos Esta tabla permite identificar los ingresos y percepciones financieras y de capital de cada uno de los integrantes del hogar, por diversos conceptos.
#' @param gastopersona Esta tabla permite identificar los gastos realizados por cada integrante del hogar en educación, transporte público y remuneraciones en especie.
#' @param trabajos Esta tabla muestra la condición de actividad de los integrantes del hogar de 12 o más años y algunas características ocupacionales durante el periodo de referencia.
#' @param agro Esta tabla muestra la información de los trabajadores independientes, mayores de 12 años que tienen en el hogar negocios dedicados a las actividades agrícolas, forestales y de tala, además de actividades de cría, explotación y productos derivados de la pesca y caza.
#' @param noagro Esta tabla muestra a los trabajadores independientes mayores de 12 años, que tienen negocios en el hogar dedicados a las actividades industriales, comerciales y de servicios.

# Tengo que arreglar hard-coding (construcción) em Viviendas y Hogares.
enigh_nueva = function(year = NA, hogares = FALSE, concentrado = FALSE, erogaciones = FALSE, gastohogar = FALSE, gastotarjetas = FALSE, poblacion = FALSE, ingresos = FALSE, gastopersona = FALSE, trabajos = FALSE, agro = FALSE, noagro = FALSE){
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

### 2.1.1 Hogares  ----------------------------------------------------------------
  if (hogares == TRUE & (year ==2010 | year == 2008)) {} # Las bases de datos de vivienda y hogares estan unidas para los an~os 2010 y 2008.
  else if (hogares == TRUE) {
      hogares = "NCV_Hogares_"
      # Descargar Datos
      #Sys.sleep(sample(1:3, 1)) # Some sleeping time
      url.hogares = paste0(url.base, hogares, year, "_concil_2010_", formato_archivo, ".zip")
      utils::download.file(url.hogares, temp.enigh)

      # Unzip y Abrir
      utils::unzip(temp.enigh, exdir = zipdir)
      data.enigh.N2 = foreign::read.dbf(paste0(zipdir, "\\\\", hogares, year, "_concil_2010_", formato_archivo, ".", formato_archivo),as.is = TRUE)
      names(data.enigh.N2) = tolower(names(data.enigh.N2))
      data.enigh    = dplyr::full_join(data.enigh, data.enigh.N2, by="folioviv", type="left", match="all")
} # End of Nest 2: Hogares
### 2.1.1.1 Hogares -> Concentrado Hogar  ----------------------------------------------------
      if (concentrado == TRUE & erogaciones == FALSE & gastohogar == FALSE & gastotarjetas == FALSE & poblacion == FALSE){

        # Descargar Datos
        Sys.sleep(sample(1:3, 1)) # Some sleeping time
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
        if (hogares == TRUE) {data.enigh = dplyr::full_join(data.enigh, data.enigh.N3, by=c("folioviv", "foliohog"), type="left", match="all")}
        else if (hogares == FALSE) {data.enigh = data.enigh.N3}
      } # End of Nest 3: Concentrado Hogar

### 2.1.1.2 Hogares -> Erogaciones  -----------------------------------------------------------
      else if (erogaciones == TRUE & concentrado == FALSE & gastohogar == FALSE & gastotarjetas == FALSE & poblacion == FALSE){

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
        if (hogares == TRUE) {data.enigh = dplyr::full_join(data.enigh, data.enigh.N3, by=c("folioviv", "foliohog"), type="left", match="all")}
        else if (hogares == FALSE) {data.enigh = data.enigh.N3}
      } # Endo of Nest 3: Erogaciones

### 2.1.1.3 Hogares -> Gasto en el Hogar  ------------------------------------------------------------------
      else if (gastohogar  == TRUE & concentrado == FALSE & erogaciones == FALSE & gastotarjetas == FALSE & poblacion == FALSE){

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
        if (hogares == TRUE) {data.enigh = dplyr::full_join(data.enigh, data.enigh.N3, by=c("folioviv", "foliohog"), type="left", match="all")}
        else if (hogares == FALSE) {data.enigh = data.enigh.N3}
      } # End of Nest 3: Gasto Hogar

### 2.1.1.4 Hogares -> Gasto Tarjetas  -----------------------------------------------------------------
      else if (gastotarjetas == TRUE & concentrado == FALSE & erogaciones == FALSE & gastohogar == FALSE & poblacion == FALSE){

        # Descargar datos
        #Sys.sleep(sample(1:3, 1))
        gasto_tarjetas = "_Gastotarjetas_"
        url.gasto_tarjetas = paste0(url.base, toupper(nuevaconstruccion), gasto_tarjetas, year, "_concil_2010_", formato_archivo, ".zip")
        utils::download.file(url.gasto_tarjetas, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2014 | year == 2012) {ruta_nest3 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(gasto_tarjetas), year, "_concil_2010_", formato_archivo)} else {}
        if (year == 2010 | year == 2008) {ruta_nest3 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), gasto_tarjetas, year, "_concil_2010_", toupper(formato_archivo))} else {}
        data.enigh.N3 = foreign::read.dbf(paste0(ruta_nest3, ".", formato_archivo), as.is = TRUE)
        names(data.enigh.N3) = tolower(names(data.enigh.N3))
        if (hogares == TRUE) {data.enigh = dplyr::full_join(data.enigh, data.enigh.N3, by=c("folioviv", "foliohog"), type="left", match="all")}
        else if (hogares == FALSE) {data.enigh = data.enigh.N3}
      } # End of Nest 3: Gasto Tarjetas

### 2.1.1.5 Hogares -> Poblacion  ----------------------------------------------------------------------
      else if (poblacion == TRUE & concentrado == FALSE & erogaciones == FALSE & gastohogar == FALSE & gastotarjetas == FALSE){

        # Descargar datos
        #Sys.sleep(sample(1:3, 1))
        Poblacion = "_Poblacion_"
        url.poblacion = paste0(url.base, toupper(nuevaconstruccion), Poblacion, year, "_concil_2010_", formato_archivo, ".zip")
        utils::download.file(url.poblacion, temp.enigh)

        # Unzip y abrir
        utils::unzip(temp.enigh, exdir = zipdir)
        if (year == 2014 | year == 2012) {ruta_nest3 = paste0(zipdir, "\\\\", tolower(nuevaconstruccion), tolower(Poblacion), year, "_concil_2010_", formato_archivo)}
        else if (year == 2010 | year == 2008) {ruta_nest3 = paste0(zipdir, "\\\\", toupper(nuevaconstruccion), Poblacion, year, "_concil_2010_", toupper(formato_archivo))}
        data.enigh.N3 = foreign::read.dbf(paste0(ruta_nest3, ".", formato_archivo), as.is = TRUE)
        names(data.enigh.N3) = tolower(names(data.enigh.N3))
        if (hogares == FALSE) {stop(print("Hogares debe ser TRUE"))}
        data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog"))[data.table::data.table(data.enigh.N3,key=c("folioviv", "foliohog")), allow.cartesian=TRUE]

        ### Nest 4: Poblacion -> Ingresos  ---------------------------------------------------
        if (ingresos == TRUE & poblacion == FALSE) {stop(message("Poblacion debe ser TRUE"))} else {}
        if (ingresos == TRUE & gastopersona == FALSE & trabajos == FALSE){

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
          if (hogares == FALSE | poblacion == FALSE) {stop(print("Hogares y Poblacion deben ser TRUE"))}
          data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren"))[data.table::data.table(data.enigh.N4,key=c("folioviv", "foliohog", "numren")), allow.cartesian=TRUE]
          if (poblacion == FALSE) {data.enigh = data.enigh.N4}
        } # End of Nest_4 Ingresos

        ### Nest 4: Poblacion -> Gastopersona  ----------------------------------------------
        if (gastopersona == TRUE & ingresos == FALSE & trabajos == FALSE){

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
          if (hogares == FALSE | poblacion == FALSE) {stop(print("Hogares y Poblacion deben ser TRUE"))}
          data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren"))[data.table::data.table(data.enigh.N4,key=c("folioviv", "foliohog", "numren")), allow.cartesian=TRUE]
        } # End of Nest_4 Gasto Persona

        ### Nest 4: Poblacion - > Trabajos  -----------------------------------------------
        if (trabajos == TRUE & ingresos == FALSE & gastopersona == FALSE){

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
          data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren"))[data.table::data.table(data.enigh.N4,key=c("folioviv", "foliohog", "numren")), allow.cartesian=TRUE]
          if (hogares == FALSE | poblacion == FALSE) {stop(print("Hogares o Poblacion deben ser TRUE"))}

          #data.enigh$fk_trabajo = paste0(as.character(data.enigh$id_renglon), as.character(data.enigh$id_trabajo))

            ### ==========================================================================
            ### NEST 5: Poblacion -> Trabajos -> Agro  -----------------------------------
            if (agro == TRUE & noagro == FALSE){

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
              data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren", "id_trabajo"))[data.table::data.table(data.enigh.N5,key=c("folioviv", "foliohog", "numren", "id_trabajo")), allow.cartesian=TRUE]
              if (trabajos == FALSE) {stop(print("Trabajos debe ser TRUE"))}
            } # End of Nest 5: Agro

           ### Nest 5: Poblacion -> Trabajos -> NoAgro  --------------------------------
            if (noagro == TRUE & agro == FALSE){

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
              data.enigh = data.table::data.table(data.enigh, key=c("folioviv", "foliohog", "numren", "id_trabajo"))[data.table::data.table(data.enigh.N5,key=c("folioviv", "foliohog", "numren", "id_trabajo")), allow.cartesian=TRUE]
              if (trabajos == FALSE) {stop(print("Trabajos debe ser TRUE"))}
            } # End of Nest 5: NoAgro
        } # End Nest 4: Poblacion - > Trabajos
      } #End of Nest 3: Poblacion
      else if (concentrado == FALSE & erogaciones == FALSE & gastohogar == FALSE & gastotarjetas == FALSE & poblacion == FALSE) {}
      else {stop(message("Dos o mas de los siguentes parametros no pueden ser TRUE al mismo tiempo: concentrado, erogaciones, gastohogar, gastotarjetas y poblacion."))}
  return(data.enigh)
} # End of Function
