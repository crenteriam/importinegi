# enigh_tradicional(): serie tradicional de la ENIGH, 1984 a 2014.
# Las pruebas usan las muestras de fixtures/enigh (data-raw/muestras_enigh.R),
# que imitan la forma de los zip del INEGI, y el servidor local.

ediciones_tradicional <- c(1984L, 1989L, 1992L, 1994L, 1996L, 1998L, 2000L, 2002L, 2004L, 2005L,
                           2006L, 2008L, 2010L, 2012L, 2014L)

# Nombre de los DBF que trae cada zip del INEGI, por edicion y nombre corto del
# archivo, como estaban el 22 de noviembre de 2026 (indice de los 106 zip).
contenido_inegi <- function() {
  basico <- function(poblacion, concentrado, gastos = "gastos") {
    list(hogares = "hogares", poblacion = poblacion, gastos = gastos, nomon = "nomon",
         ingresos = "ingresos", concen = concentrado)
  }
  con_sufijo <- function(year, tablas, sin_dbf = character()) {
    stats::setNames(
      lapply(tablas, function(tabla) {
        paste0("tra_", tabla, "_", year, "_concil_2010", if (tabla %in% sin_dbf) "" else "_dbf")
      }),
      tablas
    )
  }
  doce <- c("hogares", "poblacion", "trabajos", "erogaciones", "gastohogar", "gastopersona",
            "gastotarjetas", "ingresos", "agropecuario", "noagropecuario", "concentrado")
  list(
    "1984" = basico("POBLA84", "CONCE84"),
    "1989" = basico("POBLA89", "CONCE89"),
    "1992" = basico("POBLA92", "concen"),
    "1994" = basico("POBLA94", "concen", c("gastos94", "eroga94")),
    "1996" = basico("POBLA96", "concen", c("gastos96", "eroga96")),
    "1998" = basico("POBLA98", "concen", c("gastos98", "eroga98")),
    "2000" = basico("pobla", "concen", c("eroga", "gastos")),
    "2002" = basico("POBLA02", "concen", c("gastos", "eroga")),
    "2004" = basico("POBLA04", "concen", c("gastos", "eroga")),
    "2005" = basico("pobla", "concen", c("gastos", "eroga")),
    "2006" = list(hogares = "Hogares", poblacion = "Pobla06", gastos = c("Eroga", "Gastos"),
                  nomonetario = "Nomon", ingresos = "Ingresos", concentrado = "concen"),
    "2008" = list(
      hogares = "Tra_Hogares_2008_concil_2010_DBF", poblacion = c("Pobla08", "Trabajos"),
      tarjetas = "G_tarje", gastos = c("G_educa", "Gastos", "Eroga", "G_diario"),
      nomonetario = "Tra_Nomonetario_2008_concil_2010_DBF", ingresos = "Tra_Ingresos_2008_concil_2010_DBF",
      negocios = c("Noagro", "Agro"), concentrado = "Tra_Concentrado_2008_concil_2010_DBF"
    ),
    "2010" = list(
      hogares = "Tra_Hogares_2010_concil_2010_DBF", poblacion = c("Trabajos", "Poblacion"),
      tarjetas = "Tra_Tarjetas_2010_concil_2010_DBF",
      gastos = c("G_educa", "G_recibo", "Gastos", "Eroga", "G_costo", "G_diario"),
      nomonetario = "Tra_Nomonetario_2010_concil_2010_DBF", ingresos = "Tra_Ingresos_2010_concil_2010_DBF",
      negocios = c("Noagro", "Agro"), concentrado = "Tra_Concentrado_2010_concil_2010_DBF"
    ),
    "2012" = c(list(viviendas = "tra_viviendas_2012_concil_2010"), con_sufijo(2012, doce, "concentrado")),
    "2014" = c(list(vivi = "tra_vivi_2014_concil_2010"), con_sufijo(2014, doce, "concentrado"))
  )
}

test_that("el registro tiene un archivo para cada tabla de cada edicion", {
  withr::local_options(importinegi.registro = NULL)
  curadas <- enigh_tablas("enigh_tradicional")
  archivos <- enigh_archivos("enigh_tradicional")
  expect_identical(nrow(curadas), 126L)
  expect_identical(nrow(archivos), nrow(curadas))
  expect_identical(anyDuplicated(curadas[, c("tabla", "year")]), 0L)
  expect_identical(sort(unique(archivos$year)), ediciones_tradicional)
  expect_identical(
    as.integer(table(archivos$year)),
    c(6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 13L, 15L, 12L, 12L)
  )
  expect_false(anyNA(archivos$url))
  expect_true(all(grepl("/programas/enigh/tradicional/[0-9]{4}/microdatos/[^/]+_dbf[.]zip$", archivos$url)))
  # 126 tablas en 106 archivos: varias tablas vienen juntas en un zip
  expect_identical(length(unique(archivos$url)), 106L)
  expect_true(all(archivos$bytes > 0))
  # El concentrado existe en todas las ediciones y va primero
  expect_true(all(tapply(archivos$tabla, archivos$year, function(tablas) tablas[1]) == "concentrado"))
})

test_that("cada tabla se reconoce en el zip del INEGI por el nombre de su DBF", {
  withr::local_options(importinegi.registro = NULL)
  archivos <- enigh_archivos("enigh_tradicional")
  contenido <- contenido_inegi()
  expect_identical(names(contenido), as.character(ediciones_tradicional))
  elegidos <- vapply(seq_len(nrow(archivos)), function(i) {
    dentro <- tolower(contenido[[as.character(archivos$year[i])]][[archivos$nombre[i]]])
    hallados <- grep(archivos$dbf[i], dentro, value = TRUE)
    if (length(hallados) == 1) hallados else NA_character_
  }, character(1))
  expect_false(anyNA(elegidos), info = paste(archivos$year[is.na(elegidos)], archivos$tabla[is.na(elegidos)], collapse = "; "))
  # Dos tablas de una edicion nunca eligen el mismo DBF
  expect_identical(anyDuplicated(data.frame(archivos$year, archivos$nombre, elegidos)), 0L)
  # Todos los DBF del INEGI corresponden a una tabla
  expect_identical(length(elegidos), length(unlist(contenido)))
  elegido <- function(year, tabla) elegidos[archivos$year == year & archivos$tabla == tabla]
  expect_identical(elegido(1984, "concentrado"), "conce84")
  expect_identical(elegido(1994, "gastos"), "gastos94")
  expect_identical(elegido(1994, "erogaciones"), "eroga94")
  expect_identical(elegido(2006, "gastos"), "gastos")
  expect_identical(elegido(2008, "poblacion"), "pobla08")
  expect_identical(elegido(2008, "gastotarjetas"), "g_tarje")
  expect_identical(elegido(2010, "gastos"), "gastos")
  expect_identical(elegido(2010, "agro"), "agro")
  expect_identical(elegido(2010, "noagro"), "noagro")
  expect_identical(elegido(2014, "agro"), "tra_agropecuario_2014_concil_2010_dbf")
  expect_identical(elegido(2014, "viviendas"), "tra_vivi_2014_concil_2010")
})

test_that("enigh_nombre_archivo() quita el prefijo de la serie y el sufijo de la edicion", {
  expect_identical(
    enigh_nombre_archivo(c(
      "/programas/enigh/tradicional/1984/microdatos/concen",
      "/programas/enigh/tradicional/2006/microdatos/Tra_Gastos_2006_concil_2010",
      "/programas/enigh/tradicional/2014/microdatos/tra_vivi_2014_concil_2010"
    )),
    c("concen", "gastos", "vivi")
  )
})

test_that("enigh_tradicional() valida el a\u00f1o", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(enigh_tradicional(), "Falta indicar el a\u00f1o en `year`")
  expect_error(enigh_tradicional(c(1984, 1989)), "Falta indicar el a\u00f1o en `year`")
  expect_error(enigh_tradicional(NA), "Falta indicar el a\u00f1o en `year`")
  expect_error(enigh_tradicional("dosmil"), "no es un a\u00f1o")
  expect_error(enigh_tradicional(1995), "`year = 1995` no est\u00e1 disponible. A\u00f1os disponibles en enigh_tradicional\\(\\): 1984, 1989")
  # Un año de la Nueva serie
  expect_error(enigh_tradicional(2018), "Desde 2016 el INEGI publica la ENIGH como Nueva serie")
  mensaje <- tryCatch(enigh_tradicional(1995), error = conditionMessage)
  expect_false(grepl("Nueva serie", mensaje))
})

test_that("enigh_tradicional() valida la tabla", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(enigh_tradicional(1994, NULL), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(enigh_tradicional(1994, 3), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(enigh_tradicional(1994, c("hogares", NA)), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(enigh_tradicional(1994, character()), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(
    enigh_tradicional(1994, "salarios"),
    "`tabla = \"salarios\"` no es una tabla de la serie tradicional de la ENIGH"
  )
  # El error lista las tablas de la edicion
  expect_error(
    enigh_tradicional(1994, "salarios"),
    "Tablas de enigh_tradicional\\(1994\\): \"concentrado\", \"hogares\", \"poblacion\", \"ingresos\", \"gastos\", \"erogaciones\", \"nomonetario\""
  )
  # Una tabla que existe en otras ediciones
  expect_error(
    enigh_tradicional(1994, "viviendas"),
    "La tabla \"viviendas\" no existe en la edici\u00f3n 1994: existe en 2012 y 2014"
  )
  expect_error(
    enigh_tradicional(1984, c("concentrado", "erogaciones")),
    "La tabla \"erogaciones\" no existe en la edici\u00f3n 1984"
  )
  expect_error(enigh_tradicional(2014, "gastos"), "La tabla \"gastos\" no existe en la edici\u00f3n 2014")
})

test_that("enigh_tradicional() descarga el concentrado por omision", {
  servidor <- censo_local()
  concentrado <- enigh_tradicional(1984)
  expect_s3_class(concentrado, "data.frame")
  expect_identical(names(concentrado), c("FOLIO", "UBICA_GEO", "ZONA", "HOG", "INGCOR", "TAM_HOG"))
  expect_identical(nrow(concentrado), 3L)
  expect_identical(rownames(concentrado), c("1", "2", "3"))
  expect_identical(descargas_enigh(servidor), 1L)
  # El texto queda como texto, con sus ceros; los numeros, como numero
  expect_identical(concentrado$FOLIO, c("19840119001", "19840119002", "19840910540"))
  expect_identical(concentrado$UBICA_GEO, c("01001", "01001", "09015"))
  expect_identical(concentrado$HOG, c(1653, 1653, 2210.5))
  expect_identical(concentrado$TAM_HOG, c(5L, 3L, 7L))
  # Un numero en blanco es NA
  expect_identical(concentrado$INGCOR, c(97204.3, 45051.98, NA))
  expect_match(attr(concentrado, "fuente"), "/programas/enigh/tradicional/1984/microdatos/concen_dbf[.]zip$")
  expect_match(attr(concentrado, "descriptor"), "/programas/enigh/tradicional/1984/doc/describe84[.]pdf$")
  expect_null(attr(concentrado, "data_types"))
  # El año se acepta como texto y la tabla, con mayusculas
  expect_identical(enigh_tradicional("1984", "CONCENTRADO"), concentrado)
})

test_that("las columnas conservan el tipo que declara el INEGI", {
  censo_local()
  hogares <- enigh_tradicional(1984, "hogares")
  expect_identical(vapply(hogares, typeof, character(1), USE.NAMES = FALSE),
                   c("character", "character", "integer", "character", "double", "character"))
  # Un campo con varios codigos pegados no se convierte a numero
  expect_identical(hogares$APARATOS, c("1000001010101011011001", "0000000000000000000000", "1010000000000000000001"))
  # Un texto en blanco es NA
  expect_identical(hogares$CARAC_VIV, c("1", "2", NA))
  expect_identical(hogares$FACTOR, c(1653, 1653, 2210.5))

  poblacion <- enigh_tradicional(1994, "Poblaci\u00f3n")
  expect_identical(poblacion$NUM_REN, c("01", "02", "01", "01"))
  expect_identical(poblacion$EDAD, c(30L, 28L, 61L, 8L))
  expect_identical(poblacion$PRESTACIO1, c("010203", NA, "000000", NA))
})

test_that("varias tablas de un mismo archivo se obtienen con una sola descarga", {
  servidor <- censo_local()
  tablas <- enigh_tradicional(1994, c("erogaciones", "gastos"))
  expect_type(tablas, "list")
  expect_false(is.data.frame(tablas))
  # En el orden en que se pidieron
  expect_named(tablas, c("erogaciones", "gastos"))
  expect_identical(descargas_enigh(servidor), 1L)
  expect_identical(names(tablas$gastos), c("FOLIO", "CLAVE", "NUM_REN", "GASTO", "GAS_TRI"))
  expect_identical(nrow(tablas$gastos), 4L)
  expect_identical(names(tablas$erogaciones), c("FOLIO", "CLAVE", "ERO_TRI"))
  expect_identical(tablas$erogaciones$ERO_TRI, c(540, 1800))
  # La direccion queda en cada tabla y en la lista
  expect_match(attr(tablas, "fuente"), "/1994/microdatos/gastos_dbf[.]zip$")
  expect_identical(attr(tablas$gastos, "fuente"), attr(tablas, "fuente"))
  expect_identical(attr(tablas$erogaciones, "fuente"), attr(tablas, "fuente"))
  expect_match(attr(tablas, "descriptor"), "/1994/doc/describe94[.]pdf$")
  expect_identical(attr(tablas$gastos, "descriptor"), attr(tablas, "descriptor"))

  # Tablas de archivos distintos: una descarga por archivo. Una tabla repetida
  # se devuelve una vez
  varias <- enigh_tradicional(1994, c("concentrado", "gastos", "poblacion", "gastos"))
  expect_named(varias, c("concentrado", "gastos", "poblacion"))
  expect_identical(descargas_enigh(servidor), 4L)
  expect_length(attr(varias, "fuente"), 3L)
  expect_identical(varias$gastos, tablas$gastos)
  # El zip del concentrado trae ademas una nota, que no estorba
  expect_identical(varias$concentrado$UPM, c("00012", "00012", "03377"))
  expect_identical(varias$concentrado$GASMON, c(5982.72, 4100, NA))
})

test_that("los nombres de las columnas quedan en mayusculas y las tablas se pueden unir", {
  censo_local()
  # En 2006 el INEGI publica el concentrado en minusculas y los hogares en mayusculas
  tablas <- enigh_tradicional(2006, c("concentrado", "hogares", "poblacion"))
  expect_identical(names(tablas$concentrado), c("FOLIO", "HOG", "ESTRATO", "UBICA_GEO", "INGCOR", "EST_DIS", "UPM"))
  expect_identical(names(tablas$hogares), c("FOLIO", "ESTRATO", "CONAPO", "UBICA_GEO", "RESIDENTES", "FACTOR"))
  expect_identical(tablas$concentrado$FOLIO, tablas$hogares$FOLIO)
  unidas <- merge(tablas$poblacion, tablas$hogares[, c("FOLIO", "FACTOR")], by = "FOLIO")
  expect_identical(nrow(unidas), nrow(tablas$poblacion))
  expect_identical(sum(unidas$FACTOR), 861L + 861L + 861L + 1204L)

  # En 2010, dos tablas del mismo zip: una en mayusculas y otra en minusculas
  personas <- enigh_tradicional(2010, c("poblacion", "trabajos"))
  expect_identical(names(personas$poblacion), c("FOLIOVIV", "FOLIOHOG", "NUMREN", "PARENTESCO", "SEXO", "EDAD"))
  expect_identical(names(personas$trabajos), c("FOLIOVIV", "FOLIOHOG", "NUMREN", "NUMTRAB", "SUBOR", "HTRAB"))
  expect_identical(nrow(merge(personas$trabajos, personas$poblacion, by = c("FOLIOVIV", "FOLIOHOG", "NUMREN"))), 2L)
  expect_identical(names(enigh_tradicional(2010)), c("FOLIOVIV", "FOLIOHOG", "FACTOR", "TAM_LOC", "UBICA_GEO", "INGCOR", "EST_DIS", "UPM"))
})

test_that("se leen las tablas que vienen dentro de una carpeta y se omiten los registros borrados", {
  servidor <- censo_local()
  tablas <- enigh_tradicional(2006, c("gastos", "erogaciones"))
  expect_identical(descargas_enigh(servidor), 1L)
  expect_identical(tablas$gastos$CLAVE, c("A004", "A244", "C001"))
  # El DBF de erogaciones declara cuatro registros; el tercero esta borrado
  expect_identical(nrow(tablas$erogaciones), 3L)
  expect_identical(tablas$erogaciones$CLAVE, c("Q001", "Q005", "Q014"))
  expect_identical(tablas$erogaciones$ERO_TRI, c(498.63, 2.99, 30))
  expect_identical(rownames(tablas$erogaciones), c("1", "2", "3"))
  expect_identical(tablas$erogaciones$MESES, rep("070605040302", 3))
})

test_that("se distinguen las tablas que comparten archivo en 2008 y 2010", {
  servidor <- censo_local()
  gastos <- enigh_tradicional(2008, c("gastos", "gastodiario", "gastoeducacion", "erogaciones"))
  expect_identical(descargas_enigh(servidor), 1L)
  expect_identical(gastos$gastos$CLAVE, c("C001", "K012"))
  expect_identical(gastos$gastodiario$CLAVE, c("A009", "A075", "A009"))
  expect_identical(gastos$gastoeducacion$CLAVE, "E005")
  expect_identical(gastos$erogaciones$CLAVE, "Q014")
  # Campos de tipo F: conservan sus decimales
  expect_identical(gastos$gastodiario$CANTIDAD, c(0.2, 1.5, 12))
  expect_identical(gastos$gastodiario$GAS_TRI, c(51.86, 239.85, 1244.57))
  expect_identical(gastos$gastos$GAS_TRI, c(41.88, 1047.12))
  # Los codigos negativos del INEGI se conservan; un numero en blanco es NA
  expect_identical(gastos$gastos$INMUJER, c(-1L, -1L))
  expect_identical(gastos$gastos$LUG_COM, c(4L, NA))

  negocios <- enigh_tradicional(2008, c("agro", "noagro"))
  expect_identical(descargas_enigh(servidor), 2L)
  expect_identical(negocios$agro$VENTAS, 15000L)
  expect_identical(negocios$noagro$VENTAS, c(5600L, 0L))
  expect_identical(negocios$noagro$T_EMP, c(3L, -1L))
  tarjetas <- enigh_tradicional(2008, "gastotarjetas")
  expect_identical(tarjetas$CLAVE, c("TB01", "TB04"))
  expect_identical(tarjetas$GAS_TRI, c(3590.14, 2468.22))

  # En 2010 el zip de gastos trae seis tablas
  seis <- c("gastos", "gastodiario", "gastoeducacion", "gastorecibo", "gastocosto", "erogaciones")
  gastos10 <- enigh_tradicional(2010, seis)
  expect_named(gastos10, seis)
  expect_identical(
    vapply(gastos10, function(tabla) tabla$CLAVE, character(1), USE.NAMES = FALSE),
    c("C001", "A215", "E003", "R001", "H056", "Q001")
  )
})

test_that("en 2012 y 2014 cada tabla viene en su archivo", {
  servidor <- censo_local()
  tablas <- enigh_tradicional(2014, c("viviendas", "hogares", "agro", "noagro"))
  expect_identical(descargas_enigh(servidor), 4L)
  expect_length(attr(tablas, "fuente"), 4L)
  expect_match(attr(tablas$viviendas, "fuente"), "/2014/microdatos/tra_vivi_2014_concil_2010_dbf[.]zip$")
  expect_match(attr(tablas$agro, "fuente"), "/tra_agropecuario_2014_concil_2010_dbf[.]zip$")
  expect_match(attr(tablas$noagro, "fuente"), "/tra_noagropecuario_2014_concil_2010_dbf[.]zip$")
  expect_identical(tablas$agro$VENTAS, 144000L)
  expect_identical(tablas$noagro$VENTAS, 5200L)
  # El folio de la vivienda tiene 10 caracteres desde 2014
  expect_identical(tablas$viviendas$FOLIOVIV, c("0100008302", "0100008304", "3160511012"))
  # Los campos numericos llenos de asteriscos son NA
  expect_identical(tablas$viviendas$RENTA, c(NA, 4000L, NA))
  expect_identical(tablas$viviendas$ESTIM_PAGO, c(3500L, NA, 1200L))
  expect_identical(tablas$viviendas$ANTIGUEDAD, c(12L, 30L, NA))
  # Hogares se publica en minusculas y viviendas en mayusculas
  expect_identical(names(tablas$hogares), c("FOLIOVIV", "FOLIOHOG", "HUESPEDES", "ACC_ALIM1", "FACTOR_HOG"))
  expect_identical(tablas$hogares$ACC_ALIM1, c("2", "1", NA))
  expect_identical(nrow(merge(tablas$hogares, tablas$viviendas, by = "FOLIOVIV")), 3L)
  # El descriptor de 2014 es una ficha de la biblioteca del INEGI
  expect_match(attr(tablas, "descriptor"), "/app/biblioteca/ficha[.]html[?]upc=702825070366$")
})

test_that("enigh_tradicional() explica cuando el archivo no trae la tabla o no existe", {
  censo_local()
  # El zip de gastos de la muestra de 1984 trae otra tabla
  expect_error(
    enigh_tradicional(1984, "gastos"),
    "El archivo gastos_dbf.zip no contiene la tabla \"gastos\" con el nombre que se esperaba. Tablas que contiene: \"otra_tabla.dbf\""
  )
  # El servidor local no tiene los ingresos de 1994: responde como el INEGI
  # cuando un archivo no existe
  expect_error(enigh_tradicional(1994, "ingresos"), "El INEGI no tiene publicado el archivo")
})

test_that("si falla una descarga de varios archivos, los ya descargados se conservan para repetirla", {
  servidor <- censo_local()
  # El servidor local no tiene los ingresos de 1994. Si falla una tabla, no se
  # devuelve ninguna
  expect_error(
    suppressMessages(enigh_tradicional(1994, c("concentrado", "ingresos"))),
    "El INEGI no tiene publicado el archivo .*ingresos_dbf[.]zip.* El archivo que ya se descarg\u00f3 se conserva durante esta sesi\u00f3n de R: si repites la llamada, solo se descargar\u00e1n los que faltan\\."
  )
  expect_identical(descargas_enigh(servidor), 2L)
  archivos <- enigh_archivos("enigh_tradicional", 1994L)
  conservado <- ruta_reintento(archivos$url[archivos$tabla == "concentrado"])
  expect_true(file.exists(conservado))

  # La siguiente llamada usa el archivo que ya estaba y, si termina bien, borra
  # los archivos de la carpeta temporal
  mensajes <- testthat::capture_messages(dos <- enigh_tradicional(1994, c("concentrado", "gastos")))
  expect_true(any(grepl("Usando la copia local de concen_dbf.zip", mensajes, fixed = TRUE)))
  expect_identical(descargas_enigh(servidor), 3L)
  expect_named(dos, c("concentrado", "gastos"))
  expect_identical(nrow(dos$concentrado), 3L)
  expect_false(file.exists(conservado))
  expect_false(any(file.exists(ruta_reintento(attr(dos, "fuente")))))

  # Con un solo archivo no se usa la carpeta temporal
  error <- tryCatch(suppressMessages(enigh_tradicional(1994, "ingresos")), error = conditionMessage)
  expect_match(error, "El INEGI no tiene publicado el archivo")
  expect_false(grepl("se conserva", error))
  expect_false(dir.exists(dirname(conservado)) && length(list.files(dirname(conservado))) > 0)
})

test_that("enigh_tradicional() usa la copia local de las descargas si esta activada", {
  servidor <- censo_local()
  withr::local_options(importinegi.cache = TRUE, importinegi.cache_dir = withr::local_tempdir())
  primera <- enigh_tradicional(2006, "hogares")
  expect_identical(descargas_enigh(servidor), 1L)
  expect_message(segunda <- enigh_tradicional(2006, "hogares"), "Usando la copia local")
  expect_identical(descargas_enigh(servidor), 1L)
  expect_identical(segunda, primera)
})

test_that("una tabla cuyo archivo falta en el registro deja de ofrecerse", {
  registro <- registro_archivos
  quitar <- registro$proyecto == "enigh_trad" & registro$edicion == 2010 &
    grepl("Tra_Negocios_2010", registro$ruta, fixed = TRUE)
  expect_identical(sum(quitar), 3L)
  attr(registro, "generado") <- registro_generado()
  withr::local_options(importinegi.registro = registro[!quitar, ], importinegi.base = "http://127.0.0.1:9")
  tablas <- enigh_archivos("enigh_tradicional", 2010)$tabla
  expect_false(any(c("agro", "noagro") %in% tablas))
  expect_length(tablas, 13L)
  expect_error(enigh_tradicional(2010, "agro"), "La tabla \"agro\" no existe en la edici\u00f3n 2010: existe en 2008, 2012 y 2014")
})

test_that("bases_importinegi() lista las ediciones de la serie tradicional de la ENIGH", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  ediciones <- bases_importinegi("enigh_tradicional")
  expect_identical(nrow(ediciones), 15L)
  expect_identical(ediciones$year, ediciones_tradicional)
  expect_identical(unique(ediciones$proyecto), "enigh_tradicional")
  expect_true(all(is.na(ediciones$modulo)))
  expect_identical(unique(ediciones$base), "microdatos")
  expect_identical(unique(ediciones$cobertura), "nacional")
  expect_identical(unique(ediciones$formato), "dbf")
  expect_identical(ediciones$descripcion[1], "Encuesta Nacional de Ingresos y Gastos de los Hogares 1984, serie tradicional")
  expect_identical(ediciones$tablas[ediciones$year == 1984], "concentrado, hogares, poblacion, ingresos, gastos, nomonetario")
  expect_identical(
    ediciones$tablas[ediciones$year == 2014],
    "concentrado, viviendas, hogares, poblacion, trabajos, ingresos, gastohogar, gastopersona, gastotarjetas, erogaciones, agro, noagro"
  )
  # Las tablas que lista son las que acepta la funcion
  archivos <- enigh_archivos("enigh_tradicional")
  expect_identical(
    strsplit(ediciones$tablas, ", ", fixed = TRUE),
    unname(split(archivos$tabla, archivos$year))
  )
  # El peso de una edicion cuenta una vez cada archivo, aunque traiga varias tablas
  de_2010 <- archivos[archivos$year == 2010, ]
  expect_equal(ediciones$tamano_mb[ediciones$year == 2010],
               round(sumar_bytes(de_2010$bytes[!duplicated(de_2010$url)]) / 1024^2, 1))
  expect_true(all(ediciones$tamano_mb > 3 & ediciones$tamano_mb < 26))
  expect_identical(bases_importinegi("enigh_tradicional", year = c(1994, 2014))$year, c(1994L, 2014L))
  expect_true("enigh_tradicional" %in% bases_importinegi()$proyecto)
  expect_error(bases_importinegi("enigh"), "Proyectos disponibles: .*\"enigh_tradicional\"")
})
