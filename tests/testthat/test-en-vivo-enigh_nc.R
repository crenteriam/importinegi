# Pruebas en vivo de enigh_nc(): descargan del INEGI las cuatro ediciones de la
# Nueva construccion de la ENIGH (52 archivos, unos 76 MB) y leen sus 52
# tablas. Solo corren con IMPORTINEGI_EN_VIVO=true.

# Filas y columnas de cada tabla, comprobadas el 23 de noviembre de 2026 en los
# archivos completos del INEGI: los registros de los 52 DBF (ninguno borrado) y
# las lineas de los 52 CSV, que coinciden en todas las tablas salvo en los
# negocios no agropecuarios de 2010 (ver la segunda prueba).
dimensiones_nc <- function() {
  texto <- c(
    "2008 concentrado 29468 129", "2008 hogares 29468 161", "2008 poblacion 118927 115",
    "2008 trabajos 54710 58", "2008 ingresos 132672 17", "2008 gastos 836231 14",
    "2008 gastodiario 1038586 10", "2008 gastoeducacion 17560 11", "2008 gastotarjetas 6280 6",
    "2008 erogaciones 16199 16", "2008 nomonetario 154977 16", "2008 agro 4073 43", "2008 noagro 8218 86",
    "2010 concentrado 27655 130", "2010 hogares 27655 168", "2010 poblacion 107781 155",
    "2010 trabajos 47619 52", "2010 ingresos 126953 17", "2010 gastos 692547 11",
    "2010 gastodiario 930666 11", "2010 gastoeducacion 15205 10", "2010 gastorecibo 77438 11",
    "2010 gastocosto 10128 14", "2010 gastotarjetas 3319 6", "2010 erogaciones 17508 16",
    "2010 nomonetario 127183 18", "2010 agro 4067 47", "2010 noagro 7600 97",
    "2012 concentrado 9002 127", "2012 viviendas 8861 58", "2012 hogares 9002 109",
    "2012 poblacion 33726 154", "2012 trabajos 16884 52", "2012 ingresos 46616 17",
    "2012 gastohogar 545766 24", "2012 gastopersona 45336 18", "2012 gastotarjetas 1233 6",
    "2012 erogaciones 6107 16", "2012 agro 2581 47", "2012 noagro 2726 96",
    "2014 concentrado 19479 127", "2014 viviendas 19124 64", "2014 hogares 19479 121",
    "2014 poblacion 73592 177", "2014 trabajos 35411 52", "2014 ingresos 89147 17",
    "2014 gastohogar 1191694 25", "2014 gastopersona 111786 18", "2014 gastotarjetas 3036 6",
    "2014 erogaciones 13077 16", "2014 agro 3443 47", "2014 noagro 5294 96"
  )
  partes <- strsplit(texto, " ", fixed = TRUE)
  data.frame(
    year = as.integer(vapply(partes, `[`, character(1), 1)),
    tabla = vapply(partes, `[`, character(1), 2),
    filas = as.integer(vapply(partes, `[`, character(1), 3)),
    columnas = as.integer(vapply(partes, `[`, character(1), 4)),
    stringsAsFactors = FALSE
  )
}

test_that("EN VIVO: enigh_nc() lee las tablas de las cuatro ediciones", {
  en_vivo()
  esperadas <- dimensiones_nc()
  ediciones <- bases_importinegi("enigh_nc")
  expect_identical(ediciones$year, c(2008L, 2010L, 2012L, 2014L))
  borrados <- character()
  leidas <- 0

  for (i in seq_len(nrow(ediciones))) {
    year <- ediciones$year[i]
    nombres <- strsplit(ediciones$tablas[i], ", ", fixed = TRUE)[[1]]
    expect_identical(nombres, esperadas$tabla[esperadas$year == year], info = year)
    inicio <- Sys.time()
    tablas <- suppressMessages(enigh_nc(year, nombres))
    segundos <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")))
    expect_named(tablas, nombres)
    archivos <- enigh_archivos("enigh_nc", year)

    for (nombre in nombres) {
      tabla <- tablas[[nombre]]
      etiqueta <- paste("ENIGH Nueva construccion", year, nombre)
      describir_tabla(etiqueta, tabla)
      leidas <- leidas + 1
      esperada <- esperadas[esperadas$year == year & esperadas$tabla == nombre, ]
      expect_s3_class(tabla, "data.frame")
      expect_identical(dim(tabla), c(esperada$filas, esperada$columnas), info = etiqueta)
      expect_identical(names(tabla), toupper(names(tabla)), info = etiqueta)
      expect_identical(anyDuplicated(names(tabla)), 0L, info = etiqueta)
      # Identificador del hogar: texto y sin faltantes
      llave <- c("FOLIOVIV", if (nombre != "viviendas") "FOLIOHOG")
      expect_true(all(llave %in% names(tabla)), info = etiqueta)
      if (all(llave %in% names(tabla))) {
        expect_true(all(vapply(tabla[llave], is.character, logical(1))), info = etiqueta)
        expect_false(anyNA(tabla[llave]), info = etiqueta)
        expect_identical(unique(nchar(tabla$FOLIOVIV)), if (year == 2014) 10L else 6L, info = etiqueta)
      }

      # El DBF tal como lo publica el INEGI, para comparar con lo leido
      fila <- archivos[archivos$tabla == nombre, ]
      if (fila$formato == "dbf") {
        zip <- suppressMessages(inegi_descargar(fila$url))
        contenido <- enigh_contenido(zip)
        # Cada zip trae una sola tabla
        expect_identical(nrow(contenido), 1L, info = etiqueta)
        extraido <- gobierno_extraer(zip, contenido[grep(fila$patron, contenido$nombre), , drop = FALSE])
        revision <- revisar_dbf(extraido$archivos[1])
        unlink(extraido$carpeta, recursive = TRUE)
        expect_identical(revision$leidos, revision$declarados, info = etiqueta)
        expect_identical(revision$ancho_registro[["campos"]], revision$ancho_registro[["declarado"]], info = etiqueta)
        expect_identical(as.numeric(nrow(tabla)), revision$declarados - revision$borrados, info = etiqueta)
        # Ningun campo que foreign lee como entero tiene decimales
        expect_identical(revision$con_decimales, character(), info = etiqueta)
        expect_true(all(names(revision$tipos) %in% c("C", "N", "F")), info = etiqueta)
        if (revision$borrados > 0) {
          borrados <- c(borrados, paste0(year, " ", nombre, " (", revision$borrados, ")"))
        }
      }
    }

    # Una fila por hogar en el concentrado y en hogares, con el mismo identificador
    concentrado <- tablas$concentrado
    hogar <- function(tabla) paste(tabla$FOLIOVIV, tabla$FOLIOHOG, sep = "-")
    expect_identical(anyDuplicated(hogar(concentrado)), 0L, info = year)
    expect_true(setequal(hogar(tablas$hogares), hogar(concentrado)), info = year)
    # Las personas y los negocios pertenecen a hogares del concentrado
    for (nombre in c("poblacion", "trabajos", "ingresos", "agro", "noagro")) {
      expect_true(all(hogar(tablas[[nombre]]) %in% hogar(concentrado)), info = paste(year, nombre))
    }
    if (year >= 2012) {
      expect_true(all(concentrado$FOLIOVIV %in% tablas$viviendas$FOLIOVIV), info = year)
    }

    # Factor de expansion y diseño muestral
    factor <- if (year <= 2010) "FACTOR" else "FACTOR_HOG"
    for (nombre in c("concentrado", "hogares")) {
      valores <- tablas[[nombre]][[factor]]
      expect_true(is.numeric(valores) && !anyNA(valores) && all(valores > 0), info = paste(year, nombre, factor))
    }
    expect_identical(sum(tablas$hogares[[factor]]), sum(concentrado[[factor]]), info = year)
    con_diseno <- if (year <= 2010) c("concentrado", "hogares") else c("concentrado", "viviendas")
    for (nombre in con_diseno) {
      expect_true(all(c("EST_DIS", "UPM") %in% names(tablas[[nombre]])), info = paste(year, nombre))
    }
    if (year >= 2012) {
      expect_true("FACTOR_VIV" %in% names(tablas$viviendas), info = year)
    }
    # Numero del trabajo en las tablas de trabajos y de negocios
    trabajo <- if (year <= 2010) "NUMTRAB" else "ID_TRABAJO"
    for (nombre in c("trabajos", "agro", "noagro")) {
      expect_true(trabajo %in% names(tablas[[nombre]]), info = paste(year, nombre))
    }
    cat("[en vivo] ENIGH Nueva construccion ", year, ": ", length(nombres), " tablas en ", segundos, " s; ",
        nrow(concentrado), " hogares en la muestra; hogares expandidos: ",
        format(round(sum(concentrado[[factor]])), big.mark = ","),
        "; descriptor: ", attr(tablas, "descriptor"), "\n", sep = "")
    rm(tablas, concentrado)
    gc(verbose = FALSE)
  }

  expect_identical(leidas, 52)
  cat("[en vivo] ENIGH Nueva construccion, tablas con registros borrados: ",
      if (length(borrados) > 0) paste(borrados, collapse = "; ") else "ninguna", "\n", sep = "")
})

test_that("EN VIVO: los negocios no agropecuarios de 2010 se leen del CSV porque el DBF es otra tabla", {
  en_vivo()
  noagro <- suppressMessages(enigh_nc(2010, "noagro"))
  agro <- suppressMessages(enigh_nc(2010, "agro"))
  expect_match(attr(noagro, "fuente"), "NCV_Noagropecuario_2010_concil_2010_csv[.]zip$")
  expect_identical(dim(noagro), c(7600L, 97L))
  expect_identical(dim(agro), c(4067L, 47L))

  # Los tipos son los que fija el paquete: 32 columnas de texto, 23 enteras y
  # 42 con decimales. Ninguna columna numerica se quedo como texto
  tipos <- enigh_tipos_csv("enigh_nc", 2010L, "noagro")
  expect_length(tipos$texto, 32L)
  expect_length(tipos$entero, 23L)
  expect_true(all(c(tipos$texto, tipos$entero) %in% names(noagro)))
  de_texto <- names(noagro)[vapply(noagro, is.character, logical(1))]
  expect_identical(sort(de_texto), sort(tipos$texto))
  expect_true(all(vapply(noagro[intersect(tipos$entero, names(noagro))], is.integer, logical(1))))
  decimales <- setdiff(names(noagro), c(tipos$texto, tipos$entero))
  expect_length(decimales, 42L)
  expect_true(all(vapply(noagro[decimales], is.double, logical(1))))
  # Las llaves conservan sus ceros y sus anchos
  expect_identical(unique(nchar(noagro$FOLIOVIV)), 6L)
  expect_identical(unique(nchar(noagro$NUMREN)), 2L)
  expect_true(any(startsWith(noagro$FOLIOVIV, "0")))
  expect_true(all(noagro$MESACT_10 %in% c(NA, sprintf("%02d", 1:12))))
  describir_tabla("ENIGH Nueva construccion 2010 noagro", noagro)

  # Las columnas son las de la edicion 2012, salvo el numero del trabajo y MES
  noagro12 <- suppressMessages(enigh_nc(2012, "noagro"))
  expect_identical(setdiff(names(noagro), names(noagro12)), c("NUMTRAB", "MES"))
  expect_identical(setdiff(names(noagro12), names(noagro)), "ID_TRABAJO")
  comunes <- intersect(names(noagro), names(noagro12))
  distintos <- comunes[vapply(comunes, function(columna) typeof(noagro[[columna]]) != typeof(noagro12[[columna]]), logical(1))]
  cat("[en vivo] ENIGH Nueva construccion, noagro 2010 (CSV) contra 2012 (DBF): ", length(comunes),
      " columnas comunes; con otro tipo: ", if (length(distintos) > 0) paste(distintos, collapse = ", ") else "ninguna",
      "\n", sep = "")
  expect_identical(distintos, character())

  # El DBF que publica el INEGI con el nombre de esta tabla sigue siendo una
  # copia de la tabla agropecuaria. Si esta comprobacion falla, el INEGI
  # corrigio el archivo y la tabla puede volver a leerse en DBF (enigh_tablas())
  registro <- registro_inegi()
  dbf <- registro$proyecto == "enigh_nc" & registro$edicion == 2010 & registro$formato == "dbf" &
    grepl("Noagropecuario", registro$ruta)
  expect_identical(sum(dbf), 1L)
  zip <- suppressMessages(inegi_descargar(registro_url(registro[dbf, , drop = FALSE])))
  contenido <- enigh_contenido(zip)
  expect_identical(contenido$nombre, "ncv_noagropecuario_2010_concil_2010_dbf")
  extraido <- gobierno_extraer(zip, contenido)
  copia <- enigh_leer_tabla(extraido$archivos[1])
  unlink(extraido$carpeta, recursive = TRUE)
  expect_identical(dim(copia), dim(agro), info = "El DBF de noagro 2010 ya no tiene la forma de la tabla agro")
  expect_equal(copia, agro, ignore_attr = TRUE, info = "El DBF de noagro 2010 ya no es una copia de la tabla agro")
})

test_that("EN VIVO: los casos revisados en el INEGI siguen iguales", {
  en_vivo()
  # Viviendas de 2012: campos numericos con asteriscos, que se leen como NA, y
  # nombres recortados a 10 caracteres
  viviendas <- suppressMessages(enigh_nc(2012, "viviendas"))
  expect_true(is.numeric(viviendas$RENTA) && anyNA(viviendas$RENTA))
  expect_true(all(c("COMBUSTIBL", "MEDIDOR_LU", "FACTOR_VIV") %in% names(viviendas)))
  hogares <- suppressMessages(enigh_nc(2014, "hogares"))
  expect_true(all(c("ANIO_PICKU", "ANIO_CARRE") %in% names(hogares)))
  expect_true(all(nchar(c(names(viviendas), names(hogares))) <= 10))

  # Ingresos de 2008: campos de tipo F con decimales
  ingresos <- suppressMessages(enigh_nc(2008, "ingresos"))
  expect_type(ingresos$ING_TRI, "double")
  expect_true(any(ingresos$ING_TRI != round(ingresos$ING_TRI), na.rm = TRUE))

  # Las tablas que no cambian con la construccion son las de la serie
  # tradicional: misma forma y mismas columnas
  for (nombre in c("poblacion", "hogares", "gastotarjetas")) {
    nueva <- suppressMessages(enigh_nc(2014, nombre))
    tradicional <- suppressMessages(enigh_tradicional(2014, nombre))
    expect_identical(dim(nueva), dim(tradicional), info = nombre)
    expect_identical(names(nueva), names(tradicional), info = nombre)
    cat("[en vivo] ENIGH 2014 ", nombre, ": la tabla de la Nueva construccion ",
        if (isTRUE(all.equal(nueva, tradicional, check.attributes = FALSE))) "es igual a" else "difiere de",
        " la de la serie tradicional\n", sep = "")
  }
  # El concentrado si cambia: mismos hogares, otras columnas
  nuevo <- suppressMessages(enigh_nc(2014, "concentrado"))
  tradicional <- suppressMessages(enigh_tradicional(2014, "concentrado"))
  expect_identical(nrow(nuevo), nrow(tradicional))
  expect_true("INGTRAB" %in% names(nuevo) && !"INGTRAB" %in% names(tradicional))
  expect_true("ING_TOTAL" %in% names(tradicional) && !"ING_TOTAL" %in% names(nuevo))
})
