# enigh_nc(): Nueva construccion de la ENIGH, 2008 a 2014.
# Las pruebas usan las muestras de fixtures/enigh (data-raw/muestras_enigh.R),
# que imitan la forma de los zip del INEGI, y el servidor local.

ediciones_nc <- c(2008L, 2010L, 2012L, 2014L)

# Nombre del archivo que trae cada zip del INEGI, por edicion y nombre corto del
# zip, como estaban el 23 de noviembre de 2026 (indice de los 52 zip en DBF y
# del zip en CSV de los negocios no agropecuarios de 2010). Cada zip trae un
# solo archivo.
contenido_nc <- function() {
  con_mayusculas <- function(year, tablas) {
    stats::setNames(as.list(paste0("NCV_", tablas, "_", year, "_concil_2010_DBF")), tolower(tablas))
  }
  con_minusculas <- function(year, vivienda) {
    tablas <- c("agropecuario", "erogaciones", "gastohogar", "gastopersona", "gastotarjetas", "hogares",
                "ingresos", "noagropecuario", "poblacion", "trabajos")
    propias <- stats::setNames(as.list(paste0("ncv_", tablas, "_", year, "_concil_2010_dbf")), tablas)
    c(
      propias,
      list(concentrado = paste0("NCV_concentrado_", year, "_concil_2010")),
      stats::setNames(list(paste0("NCV_", vivienda, "_", year, "_concil_2010")), vivienda)
    )
  }
  trece <- c("Agropecuario", "Concentrado", "Erogaciones", "Gastodiario", "Gastoeducacion", "Gastos",
             "Gastotarjetas", "Hogares", "Ingresos", "Noagropecuario", "Nomonetario", "Poblacion", "Trabajos")
  list(
    "2008" = con_mayusculas(2008, trece),
    "2010" = con_mayusculas(2010, c(trece, "Gastocosto", "Gastorecibo")),
    "2012" = con_minusculas(2012, "viviendas"),
    "2014" = con_minusculas(2014, "vivi")
  )
}

test_that("el registro tiene un archivo para cada tabla de cada edicion", {
  withr::local_options(importinegi.registro = NULL)
  curadas <- enigh_tablas("enigh_nc")
  archivos <- enigh_archivos("enigh_nc")
  expect_identical(nrow(curadas), 52L)
  expect_identical(nrow(archivos), nrow(curadas))
  expect_identical(anyDuplicated(curadas[, c("tabla", "year")]), 0L)
  expect_identical(sort(unique(archivos$year)), ediciones_nc)
  expect_identical(as.integer(table(archivos$year)), c(13L, 15L, 12L, 12L))
  expect_false(anyNA(archivos$url))
  # Una tabla por archivo
  expect_identical(anyDuplicated(archivos$url), 0L)
  expect_true(all(archivos$bytes > 0))
  # Todas se leen en DBF, salvo los negocios no agropecuarios de 2010
  en_csv <- archivos$formato == "csv"
  expect_identical(archivos[en_csv, c("tabla", "year")], data.frame(tabla = "noagro", year = 2010L), ignore_attr = TRUE)
  expect_true(all(archivos$formato[!en_csv] == "dbf"))
  expect_true(all(grepl("/programas/enigh/nc/[0-9]{4}/microdatos/[^/]+_dbf[.]zip$", archivos$url[!en_csv])))
  expect_match(archivos$url[en_csv], "/programas/enigh/nc/2010/microdatos/NCV_Noagropecuario_2010_concil_2010_csv[.]zip$")
  # El concentrado existe en las cuatro ediciones y va primero
  expect_true(all(tapply(archivos$tabla, archivos$year, function(tablas) tablas[1]) == "concentrado"))
  # Las tablas se llaman igual que en la serie tradicional
  tradicional <- enigh_archivos("enigh_tradicional")
  for (year in ediciones_nc) {
    expect_identical(archivos$tabla[archivos$year == year], tradicional$tabla[tradicional$year == year], info = year)
  }
})

test_that("cada tabla se reconoce en el zip del INEGI por el nombre de su archivo", {
  withr::local_options(importinegi.registro = NULL)
  archivos <- enigh_archivos("enigh_nc")
  contenido <- contenido_nc()
  expect_identical(names(contenido), as.character(ediciones_nc))
  expect_identical(lengths(contenido, use.names = FALSE), c(13L, 15L, 12L, 12L))
  en_dbf <- archivos[archivos$formato == "dbf", ]
  for (i in seq_len(nrow(en_dbf))) {
    de_la_edicion <- contenido[[as.character(en_dbf$year[i])]]
    etiqueta <- paste(en_dbf$year[i], en_dbf$tabla[i])
    # El zip de la tabla esta en el indice y su archivo se reconoce
    expect_true(en_dbf$nombre[i] %in% names(de_la_edicion), info = etiqueta)
    expect_true(grepl(en_dbf$patron[i], tolower(de_la_edicion[[en_dbf$nombre[i]]])), info = etiqueta)
    # La expresion no reconoce el archivo de ninguna otra tabla de la edicion
    expect_identical(sum(grepl(en_dbf$patron[i], tolower(unlist(de_la_edicion)))), 1L, info = etiqueta)
  }
  # El CSV de los negocios no agropecuarios de 2010 trae un solo archivo, "noagro.csv"
  en_csv <- archivos[archivos$formato == "csv", ]
  expect_true(grepl(en_csv$patron, "noagro"))
  expect_false(grepl(en_csv$patron, "agro"))
})

test_that("enigh_nombre_archivo() quita el prefijo de la Nueva construccion", {
  expect_identical(
    enigh_nombre_archivo(c(
      "/programas/enigh/nc/2008/microdatos/NCV_Agropecuario_2008_concil_2010",
      "/programas/enigh/nc/2012/microdatos/ncv_viviendas_2012_concil_2010",
      "/programas/enigh/nc/2014/microdatos/NCV_Vivi_2014_concil_2010"
    )),
    c("agropecuario", "viviendas", "vivi")
  )
})

test_that("enigh_nc() valida el a\u00f1o", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(enigh_nc(), "Falta indicar el a\u00f1o en `year`")
  expect_error(enigh_nc(c(2008, 2010)), "Falta indicar el a\u00f1o en `year`")
  expect_error(enigh_nc("dosmil"), "no es un a\u00f1o")
  expect_error(enigh_nc(2009), "`year = 2009` no est\u00e1 disponible. A\u00f1os disponibles en enigh_nc\\(\\): 2008, 2010, 2012, 2014\\.$")
  # Un año de la serie tradicional
  expect_error(
    enigh_nc(2006),
    "La Nueva construcci\u00f3n empieza en 2008: la edici\u00f3n 2006 est\u00e1 en la serie tradicional, que se descarga con enigh_tradicional\\(\\)"
  )
  expect_error(enigh_nc(1984), "la edici\u00f3n 1984 est\u00e1 en la serie tradicional")
  # Un año de la Nueva serie
  expect_error(enigh_nc(2018), "Desde 2016 el INEGI publica la ENIGH como Nueva serie")
  # Con un año que no se acepta no hay aviso de la tabla por omision
  mensajes <- testthat::capture_messages(try(enigh_nc(2009), silent = TRUE))
  expect_identical(mensajes, character())
})

test_that("enigh_nc() valida la tabla y orienta a quien usaba enigh_nuevaconstruccion()", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(enigh_nc(2010, NULL), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(enigh_nc(2010, 3), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(enigh_nc(2014, "personas"), "`tabla = \"personas\"` no es una tabla de la Nueva construcci\u00f3n de la ENIGH")
  expect_error(
    enigh_nc(2014, "personas"),
    "Tablas de enigh_nc\\(2014\\): \"concentrado\", \"viviendas\", \"hogares\", \"poblacion\", \"trabajos\", \"ingresos\", \"gastohogar\", \"gastopersona\", \"gastotarjetas\", \"erogaciones\", \"agro\", \"noagro\""
  )
  # La funcion anterior devolvia los hogares cuando se pedian las viviendas de 2008 y 2010
  expect_error(
    enigh_nc(2008, "viviendas"),
    "La tabla \"viviendas\" no existe en la edici\u00f3n 2008: existe en 2012 y 2014\\. Hasta 2010 las caracter\u00edsticas de la vivienda est\u00e1n en la tabla \"hogares\"\\."
  )
  # ... y la tabla de gastos cuando se pedia "gastohogar"
  expect_error(
    enigh_nc(2010, "gastohogar"),
    "La tabla \"gastohogar\" no existe en la edici\u00f3n 2010: existe en 2012 y 2014\\. Hasta 2010 los gastos est\u00e1n en la tabla \"gastos\"\\."
  )
  expect_error(enigh_nc(2008, "gastopersona"), "Hasta 2010 los gastos est\u00e1n en la tabla \"gastos\"")
  expect_error(
    enigh_nc(2012, "gastos"),
    "existe en 2008 y 2010\\. Desde 2012 los gastos est\u00e1n en las tablas \"gastohogar\" y \"gastopersona\"\\."
  )
  # Una tabla sin pista
  mensaje <- tryCatch(enigh_nc(2012, "nomonetario"), error = conditionMessage)
  expect_match(mensaje, "La tabla \"nomonetario\" no existe en la edici\u00f3n 2012: existe en 2008 y 2010\\. Tablas de")
})

test_that("enigh_nc() descarga el concentrado por omision y avisa", {
  servidor <- censo_local()
  expect_message(
    concentrado <- enigh_nc(2014),
    "No indicaste `tabla`: se descarga \"concentrado\", la tabla con una fila por hogar\\. Otras tablas de enigh_nc\\(2014\\): \"viviendas\", \"hogares\", \"poblacion\", \"trabajos\", \"ingresos\", \"gastohogar\", \"gastopersona\", \"gastotarjetas\", \"erogaciones\", \"agro\", \"noagro\"\\."
  )
  expect_s3_class(concentrado, "data.frame")
  expect_identical(
    names(concentrado),
    c("FOLIOVIV", "FOLIOHOG", "UBICA_GEO", "TAM_LOC", "EST_DIS", "UPM", "FACTOR_HOG", "ING_COR", "INGTRAB")
  )
  expect_identical(nrow(concentrado), 3L)
  expect_identical(descargas_enigh(servidor), 1L)
  expect_identical(concentrado$FOLIOVIV, c("0100008302", "0100008304", "3160511012"))
  expect_identical(concentrado$FACTOR_HOG, c(694L, 694L, 1100L))
  expect_identical(concentrado$INGTRAB, c(41250.8, 20000, 3000))
  expect_match(attr(concentrado, "fuente"), "/programas/enigh/nc/2014/microdatos/NCV_Concentrado_2014_concil_2010_dbf[.]zip$")
  expect_match(attr(concentrado, "descriptor"), "ficha[.]html[?]upc=702825070373$")
  expect_null(attr(concentrado, "data_types"))

  # Si se escribe la tabla, no hay aviso; el resultado es el mismo
  mensajes <- testthat::capture_messages(escrita <- enigh_nc("2014", "Concentrado"))
  expect_false(any(grepl("No indicaste", mensajes)))
  expect_identical(escrita, concentrado)
  mensajes <- testthat::capture_messages(enigh_nc(2014, tabla = c("hogares", "poblacion")))
  expect_false(any(grepl("No indicaste", mensajes)))
})

test_that("enigh_tradicional() tambien avisa de la tabla por omision", {
  censo_local()
  expect_message(
    enigh_tradicional(1984),
    "No indicaste `tabla`: se descarga \"concentrado\", la tabla con una fila por hogar\\. Otras tablas de enigh_tradicional\\(1984\\): \"hogares\", \"poblacion\", \"ingresos\", \"gastos\", \"nomonetario\"\\."
  )
  mensajes <- testthat::capture_messages(enigh_tradicional(1984, "concentrado"))
  expect_false(any(grepl("No indicaste", mensajes)))
})

test_that("los nombres quedan en mayusculas y las columnas conservan el tipo del INEGI", {
  censo_local()
  # 2008: nombres en mayusculas, codigos numericos y un codigo negativo
  tablas <- enigh_nc(2008, c("concentrado", "hogares", "ingresos"))
  expect_named(tablas, c("concentrado", "hogares", "ingresos"))
  expect_identical(
    vapply(tablas$concentrado, typeof, character(1)),
    c(FOLIOVIV = "character", FOLIOHOG = "character", FACTOR = "integer", ESTRATO = "integer",
      UBICA_GEO = "character", CONAPO = "integer", TENENCIA = "integer", INGCOR = "double", INGTRAB = "double",
      EST_DIS = "character", UPM = "character")
  )
  expect_identical(tablas$concentrado$TENENCIA, c(4L, 1L, NA))
  expect_identical(tablas$hogares$PARED, c(8L, 8L, -1L))
  expect_identical(tablas$hogares$EST_DIS, c("0010", "0010", "3014"))
  # Un campo de tipo F conserva sus decimales
  expect_identical(tablas$ingresos$ING_TRI, c(23934.43, 150.5, 4487.7))
  expect_identical(tablas$ingresos$NUMREN, c("01", "02", "01"))

  # 2010: el INEGI publica los nombres en minusculas
  concentrado <- enigh_nc(2010, "concentrado")
  expect_identical(names(concentrado), c("FOLIOVIV", "FOLIOHOG", "FACTOR", "TAM_LOC", "UBICA_GEO", "INGCOR", "EST_DIS", "UPM"))
  expect_identical(concentrado$FOLIOHOG, c("0", "0", "1"))

  # Las tablas de una edicion se unen por el identificador del hogar
  unidas <- merge(tablas$ingresos, tablas$hogares[, c("FOLIOVIV", "FOLIOHOG", "FACTOR")], by = c("FOLIOVIV", "FOLIOHOG"))
  expect_identical(nrow(unidas), nrow(tablas$ingresos))
  expect_identical(sum(unidas$FACTOR), 710L + 710L + 1302L)
})

test_that("los negocios no agropecuarios de 2010 se leen del CSV, con los tipos del INEGI", {
  servidor <- censo_local()
  noagro <- enigh_nc(2010, "noagro")
  # Una sola descarga: el DBF, que es una copia de la tabla agropecuaria, no se pide
  expect_identical(descargas_enigh(servidor), 1L)
  expect_match(attr(noagro, "fuente"), "/2010/microdatos/NCV_Noagropecuario_2010_concil_2010_csv[.]zip$")
  expect_match(attr(noagro, "descriptor"), "ficha[.]html[?]upc=702825002423$")
  expect_identical(
    names(noagro),
    c("FOLIOVIV", "FOLIOHOG", "NUMREN", "NUMTRAB", "TIPOACT", "NUMESTA", "TOTESTA", "LUGACT", "MES_2", "AUTOCONS",
      "MES", "PERACT", "MESACT_1", "MESACT_10", "T_EMP", "H_EMP", "REG_NOT", "VENTAS1", "OTROSNOM1", "ING_TRI",
      "AUTO_TRI")
  )
  expect_identical(nrow(noagro), 3L)
  texto <- c("FOLIOVIV", "FOLIOHOG", "NUMREN", "NUMTRAB", "TIPOACT", "NUMESTA", "LUGACT", "MES_2", "AUTOCONS",
             "MES", "PERACT", "MESACT_1", "MESACT_10", "REG_NOT")
  entero <- c("TOTESTA", "T_EMP", "H_EMP")
  tipos <- vapply(noagro, typeof, character(1))
  expect_true(all(tipos[texto] == "character"))
  expect_true(all(tipos[entero] == "integer"))
  expect_true(all(tipos[setdiff(names(noagro), c(texto, entero))] == "double"))
  # Las llaves son texto, con sus ceros, como en las demas tablas
  expect_identical(noagro$FOLIOVIV, c("011001", "046001", "200933"))
  expect_identical(noagro$FOLIOHOG, c("0", "1", "0"))
  expect_identical(noagro$NUMREN, c("02", "01", "03"))
  # Los codigos de una misma familia quedan con el mismo tipo, tengan o no ceros
  expect_identical(noagro$MESACT_1, c("01", NA, "09"))
  expect_identical(noagro$MESACT_10, c("10", NA, "12"))
  # El espacio con que el INEGI marca el dato faltante es NA
  expect_identical(noagro$LUGACT, c("1", NA, "5"))
  expect_identical(noagro$TOTESTA, c(NA, 3L, NA))
  expect_identical(noagro$T_EMP, c(2L, -1L, NA))
  # Cantidades: con decimales, sin ellos y sin el cero inicial
  expect_identical(noagro$VENTAS1, c(497.08, 0, 12000))
  expect_identical(noagro$ING_TRI, c(1490.5, 0, 36000.25))
  expect_identical(noagro$AUTO_TRI, c(0.831765, 0, 0))

  # La tabla agropecuaria si se lee del DBF, y es otra tabla
  agro <- enigh_nc(2010, "agro")
  expect_match(attr(agro, "fuente"), "/2010/microdatos/NCV_Agropecuario_2010_concil_2010_dbf[.]zip$")
  expect_identical(names(agro), c("FOLIOVIV", "FOLIOHOG", "NUMREN", "NUMTRAB", "TIPOACT", "T_EMP", "REG_NOT", "VENTAS", "ING_TRI"))
  expect_false(identical(names(agro), names(noagro)))
  # Las dos se unen con los hogares por las mismas llaves
  expect_identical(typeof(agro$FOLIOHOG), typeof(noagro$FOLIOHOG))
  expect_identical(typeof(agro$NUMTRAB), typeof(noagro$NUMTRAB))

  # En las demas ediciones los negocios no agropecuarios se leen del DBF
  archivos <- enigh_archivos("enigh_nc")
  expect_identical(archivos$formato[archivos$tabla == "noagro"], c("dbf", "csv", "dbf", "dbf"))
})

test_that("enigh_leer_csv() no pierde datos cuando una columna no es como se esperaba", {
  archivo <- withr::local_tempfile(fileext = ".csv")
  lineas <- c("folioviv,clave,monto,raro,vacia,nulo", "011001,A001,10.5,12a, ,NULL", "011002, ,-3,7, ,4", "011003,null,0,8, ,5")
  writeBin(charToRaw(paste0(paste(lineas, collapse = "\r\n"), "\r\n")), archivo)
  datos <- enigh_leer_csv(archivo, list(texto = c("FOLIOVIV", "CLAVE"), entero = c("MONTO", "VACIA", "NULO")))
  expect_identical(names(datos), c("FOLIOVIV", "CLAVE", "MONTO", "RARO", "VACIA", "NULO"))
  expect_identical(datos$FOLIOVIV, c("011001", "011002", "011003"))
  # El espacio y el texto "NULL" son datos faltantes, como en todo el paquete
  expect_identical(datos$CLAVE, c("A001", NA, NA))
  expect_identical(datos$NULO, c(NA, 4L, 5L))
  # Una columna declarada como entera que trae decimales queda como numero
  expect_identical(datos$MONTO, c(10.5, -3, 0))
  # Una columna con un valor que no es un numero se deja como texto
  expect_identical(datos$RARO, c("12a", "7", "8"))
  # Una columna sin datos queda como NA del tipo declarado
  expect_identical(datos$VACIA, rep(NA_integer_, 3))
  # Sin tipos declarados, las columnas numericas se convierten
  sin_tipos <- enigh_leer_csv(archivo)
  expect_identical(sin_tipos$FOLIOVIV, c(11001, 11002, 11003))
  expect_identical(sin_tipos$CLAVE, c("A001", NA, NA))
})

test_that("los asteriscos son NA y los nombres recortados se conservan como en el DBF", {
  censo_local()
  viviendas <- enigh_nc(2012, "viviendas")
  expect_identical(
    names(viviendas),
    c("FOLIOVIV", "TIPO_VIV", "ANTIGUEDAD", "RENTA", "ESTIM_PAGO", "COMBUSTIBL", "MEDIDOR_LU", "UBICA_GEO",
      "EST_DIS", "UPM", "FACTOR_VIV")
  )
  expect_identical(viviendas$ANTIGUEDAD, c(35L, NA, 20L))
  expect_identical(viviendas$RENTA, c(1800L, 1100L, NA))
  expect_identical(viviendas$ESTIM_PAGO, c(NA, NA, 1500L))
  expect_identical(viviendas$UPM, c("0000110", "0000160", "0320450"))
  # La tabla de hogares de la misma edicion viene en minusculas
  hogares <- enigh_nc(2012, "hogares")
  expect_identical(names(hogares), c("FOLIOVIV", "FOLIOHOG", "HUESPEDES", "NUM_PICKUP", "ANIO_PICKU", "FACTOR_HOG"))
  expect_identical(hogares$ANIO_PICKU, c(NA, "98", NA))
  expect_identical(nrow(merge(hogares, viviendas[, c("FOLIOVIV", "FACTOR_VIV")], by = "FOLIOVIV")), 3L)
})

test_that("varias tablas se devuelven en una lista, con una descarga por tabla", {
  servidor <- censo_local()
  tablas <- enigh_nc(2014, c("poblacion", "hogares", "poblacion"))
  expect_type(tablas, "list")
  expect_false(is.data.frame(tablas))
  expect_named(tablas, c("poblacion", "hogares"))
  expect_identical(descargas_enigh(servidor), 2L)
  expect_length(attr(tablas, "fuente"), 2L)
  expect_match(attr(tablas, "descriptor"), "ficha[.]html[?]upc=702825070373$")
  expect_identical(attr(tablas$hogares, "descriptor"), attr(tablas, "descriptor"))
  personas <- merge(tablas$poblacion, tablas$hogares[, c("FOLIOVIV", "FOLIOHOG", "FACTOR_HOG")], by = c("FOLIOVIV", "FOLIOHOG"))
  expect_identical(nrow(personas), 4L)
  expect_identical(sum(personas$FACTOR_HOG), 694L + 694L + 694L + 1100L)
})

test_that("enigh_nc() explica cuando el archivo no trae la tabla o no existe", {
  censo_local()
  expect_error(
    enigh_nc(2014, "erogaciones"),
    "El archivo NCV_Erogaciones_2014_concil_2010_dbf.zip no contiene la tabla \"erogaciones\" con el nombre que se esperaba\\. Tablas que contiene: \"otra_tabla.dbf\""
  )
  # El servidor local no tiene los ingresos de 2014
  expect_error(enigh_nc(2014, "ingresos"), "El INEGI no tiene publicado el archivo")
  # Si falla una tabla de varias, las ya descargadas se conservan para repetir la llamada
  expect_error(
    suppressMessages(enigh_nc(2014, c("hogares", "ingresos"))),
    "El INEGI no tiene publicado el archivo .*El archivo que ya se descarg\u00f3 se conserva"
  )
  mensajes <- testthat::capture_messages(dos <- enigh_nc(2014, c("hogares", "poblacion")))
  expect_true(any(grepl("Usando la copia local de NCV_Hogares_2014_concil_2010_dbf.zip", mensajes, fixed = TRUE)))
  expect_named(dos, c("hogares", "poblacion"))
})

test_that("una tabla cuyo archivo falta en el registro deja de ofrecerse", {
  registro <- registro_archivos
  es_csv <- registro$proyecto == "enigh_nc" & registro$edicion == 2010 & registro$formato == "csv" &
    grepl("Noagropecuario", registro$ruta)
  expect_identical(sum(es_csv), 1L)
  recortado <- registro[!es_csv, ]
  attr(recortado, "generado") <- registro_generado()
  withr::local_options(importinegi.registro = recortado, importinegi.base = "http://127.0.0.1:9")
  # Sin el CSV, la tabla no se toma del DBF del INEGI, que es otra tabla
  archivos <- enigh_archivos("enigh_nc", 2010)
  expect_false("noagro" %in% archivos$tabla)
  expect_identical(nrow(archivos), 14L)
  expect_error(enigh_nc(2010, "noagro"), "La tabla \"noagro\" no existe en la edici\u00f3n 2010: existe en 2008, 2012 y 2014")
})

test_that("bases_importinegi() lista las ediciones de la Nueva construccion de la ENIGH", {
  withr::local_options(importinegi.registro = NULL)
  ediciones <- bases_importinegi("enigh_nc")
  expect_identical(nrow(ediciones), 4L)
  expect_identical(ediciones$year, ediciones_nc)
  expect_identical(unique(ediciones$proyecto), "enigh_nc")
  expect_identical(unique(ediciones$base), "microdatos")
  expect_identical(unique(ediciones$cobertura), "nacional")
  # El formato de la mayoria de las tablas: en 2010 una se lee en CSV
  expect_identical(ediciones$formato, rep("dbf", 4))
  expect_identical(
    ediciones$descripcion[4],
    "Encuesta Nacional de Ingresos y Gastos de los Hogares 2014, Nueva construcci\u00f3n"
  )
  expect_identical(
    ediciones$tablas[1],
    "concentrado, hogares, poblacion, trabajos, ingresos, gastos, gastodiario, gastoeducacion, gastotarjetas, erogaciones, nomonetario, agro, noagro"
  )
  expect_identical(lengths(strsplit(ediciones$tablas, ", ", fixed = TRUE)), c(13L, 15L, 12L, 12L))
  # Cada edicion pesa lo que suman sus archivos
  expect_true(all(ediciones$tamano_mb > 8 & ediciones$tamano_mb < 26))
  expect_identical(nrow(bases_importinegi(c("enigh_tradicional", "enigh_nc"), 2010)), 2L)
})
