# Pruebas de etoe(). Los archivos de muestra (fixtures/etoe) son inventados y
# tienen la forma de los zip mensuales del INEGI; los genera
# data-raw/muestras_etoe.R. El servidor local los entrega en las rutas reales
# del registro del paquete: abril en las dos versiones de los ponderadores, y
# mayo y junio en la version del Censo 2020. Todas las muestras tienen cinco
# viviendas, seis hogares (dos sin entrevista) y ocho residentes (seis con
# cuestionario de ocupacion y empleo), como las de la ENOE.

etoe_silencio <- function(...) {
  suppressMessages(etoe(...))
}

ruta_etoe <- function(servidor, archivo) {
  servidor$url(paste0("/contenidos/investigacion/etoe/microdatos/", archivo))
}

# Columnas de una tabla, sin sus atributos (la direccion del archivo cambia con
# el mes y con la version).
valores <- function(datos) {
  attributes(datos) <- list(names = names(datos))
  datos
}

# Meses del registro -----------------------------------------------------------------

test_that("el registro tiene los seis archivos de la ETOE", {
  withr::local_options(importinegi.registro = NULL)
  meses <- etoe_meses()
  expect_named(meses, c("year", "mes", "nombre", "poblacion", "url", "tamano", "bytes"))
  expect_false(anyNA(meses))
  # Un archivo por mes y version de los ponderadores; primero la version por omision
  expect_identical(meses$year, rep(2020L, 6))
  expect_identical(meses$mes, rep(4:6, each = 2))
  expect_identical(meses$nombre, rep(c("abril", "mayo", "junio"), each = 2))
  expect_identical(meses$poblacion, rep(c("cpv2020", "conapo"), 3))
  expect_identical(
    meses$url,
    paste0("https://www.inegi.org.mx/contenidos/investigacion/etoe/microdatos/etoe_2020_",
           c("abril_cpv2020", "abril", "mayo_cpv2020", "mayo", "junio_cpv2020", "junio"), "_dbf.zip")
  )
  expect_identical(meses$tamano, c("2.4 MB", "2.4 MB", "2.2 MB", "2.2 MB", "2.6 MB", "2.9 MB"))
  expect_identical(etoe_periodo(meses), "abril, mayo y junio de 2020")
  expect_identical(names(etoe_versiones()), c("cpv2020", "conapo"))
  # El registro tiene ademas los dos descriptores de archivos y el documento
  # del calculo de los indicadores
  propias <- registro_archivos[registro_archivos$proyecto == "etoe", ]
  expect_identical(nrow(propias), 9L)
  expect_identical(as.vector(table(propias$formato)[c("dbf", "pdf")]), c(6L, 3L))
  expect_identical(unique(propias$id_api), 3093L)
  expect_true(all(is.na(propias$edicion)))
  expect_identical(unique(propias$anio), 2020L)
  expect_true(all(startsWith(propias$ruta, "/investigacion/etoe/microdatos/")))
})

test_that("etoe_lista() une los textos de un mensaje", {
  expect_identical(etoe_lista(c("abril", "mayo", "junio")), "abril, mayo y junio")
  expect_identical(etoe_lista(c("abril", "mayo")), "abril y mayo")
  expect_identical(etoe_lista("abril"), "abril")
  expect_identical(etoe_lista(2020L), "2020")
})

# Argumentos -------------------------------------------------------------------------

test_that("el a\u00f1o, el mes y la version de los ponderadores se validan sin descargar", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(
    etoe(),
    "Falta indicar el a\u00f1o en `year` \\(un solo valor\\). La ETOE se levant\u00f3 en abril, mayo y junio de 2020.*bases_importinegi\\(\"etoe\"\\)"
  )
  expect_error(etoe(NA, "abril"), "Falta indicar el a\u00f1o en `year`")
  expect_error(etoe(c(2020, 2021), "abril"), "Falta indicar el a\u00f1o en `year`")
  expect_error(etoe("veinte", 4), "`year = \"veinte\"` no es un a\u00f1o")
  expect_error(etoe(20, 4), "`year = 20` no es un a\u00f1o")
  # Años sin ETOE: el mensaje remite a enoe()
  expect_error(
    etoe(2019, "abril"),
    "La ETOE de 2019 no existe: el INEGI solo la levant\u00f3 en abril, mayo y junio de 2020. Los dem\u00e1s periodos se descargan con enoe\\(\\)"
  )
  expect_error(etoe(2021, 4), "La ETOE de 2021 no existe")
  expect_error(
    etoe(2020),
    "Falta indicar el mes en `mes` \\(un solo valor\\). Opciones: \"abril\", \"mayo\" y \"junio\", o el n\u00famero del mes \\(4 a 6\\)"
  )
  expect_error(etoe(2020, NA), "Falta indicar el mes en `mes`")
  expect_error(etoe(2020, c("abril", "mayo")), "Falta indicar el mes en `mes`")
  expect_error(etoe(2020, "primavera"), "`mes = \"primavera\"` no es un mes. Opciones: \"abril\"")
  expect_error(etoe(2020, 13), "`mes = 13` no es un mes")
  expect_error(etoe(2020, "trim2"), "`mes = \"trim2\"` no es un mes")
  # Meses sin ETOE
  expect_error(
    etoe(2020, "julio"),
    "La ETOE de julio de 2020 no existe: el INEGI solo la levant\u00f3 en abril, mayo y junio de 2020.*enoe\\(\\)"
  )
  expect_error(etoe(2020, 3), "La ETOE de marzo de 2020 no existe")
  expect_error(
    etoe(2020, "abril", poblacion = "censo"),
    "`poblacion = \"censo\"` no es una versi\u00f3n de los ponderadores de la ETOE. Opciones: \"cpv2020\" \\(estimaci\u00f3n de poblaci\u00f3n con base en el Censo de Poblaci\u00f3n y Vivienda 2020\\) y \"conapo\" \\(proyecciones de poblaci\u00f3n del CONAPO\\)"
  )
  expect_error(etoe(2020, "abril", poblacion = c("cpv2020", "conapo")), "`poblacion` debe ser la versi\u00f3n de los ponderadores \\(un solo valor\\)")
  expect_error(etoe(2020, "abril", poblacion = NA), "`poblacion` debe ser la versi\u00f3n de los ponderadores")
  expect_error(etoe(2020, "abril", poblacion = NULL), "`poblacion` debe ser la versi\u00f3n de los ponderadores")
  expect_error(etoe(2020, "abril", poblacion = 2020), "`poblacion` debe ser la versi\u00f3n de los ponderadores")

  # El mes se escribe con su nombre, con sus tres primeras letras o con su numero
  meses <- etoe_meses()
  for (mes in list("junio", "JUNIO", " Junio ", "jun", "6", "06", 6, 6L)) {
    expect_identical(etoe_validar_mes(mes, 2020L, meses), 6L)
  }
  expect_identical(etoe_validar_mes("abril", 2020L, meses), 4L)
  expect_identical(etoe_validar_mes("May", 2020L, meses), 5L)
  # El año tambien se acepta como texto, y la version, sin distinguir mayusculas
  abril <- etoe_elegir("2020", "abril", "cpv2020")
  expect_identical(abril[c("year", "mes", "etiqueta", "poblacion", "tamano")],
                   list(year = 2020L, mes = 4L, etiqueta = "abril de 2020", poblacion = "cpv2020", tamano = "2.4 MB"))
  expect_match(abril$url, "/contenidos/investigacion/etoe/microdatos/etoe_2020_abril_cpv2020_dbf[.]zip$")
  expect_match(etoe_elegir(2020, 4, "CONAPO")$url, "/microdatos/etoe_2020_abril_dbf[.]zip$")
  expect_identical(etoe_elegir(2020, 5, " Cpv2020 ")$poblacion, "cpv2020")
  expect_match(etoe_elegir(2020, "junio", "conapo")$url, "/microdatos/etoe_2020_junio_dbf[.]zip$")
})

test_that("`integrar`, `tabla` y `estado` se validan antes de descargar", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(etoe(2020, 4, integrar = "si"), "`integrar` debe ser TRUE o FALSE")
  expect_error(etoe(2020, 4, integrar = NA), "`integrar` debe ser TRUE o FALSE")
  expect_error(
    etoe(2020, 4, tabla = "personas"),
    "`tabla = \"personas\"` no es una tabla de la ETOE. Tablas de la ETOE: \"viv\" \\(vivienda\\)"
  )
  expect_error(etoe(2020, 4, tabla = 3), "`tabla` debe ser el nombre de una o varias tablas. Tablas de la ETOE")
  expect_error(etoe(2020, 4, estado = "Atlantida"), "No se reconoce la entidad")
  # Nombres de los archivos del INEGI
  expect_identical(enoe_validar_tabla(c("SDEMT0420", "coe1n0620.dbf", "VIVT0520.DBF", "hog"), "ETOE"),
                   c("sdem", "coe1", "viv", "hog"))
})

test_that("los ponderadores de ciudades se reconocen y la llave puede prescindir de TIPO", {
  expect_identical(
    enoe_es_ponderador(c("FAC", "FAC32", "FAC39", "FAC_NP", "FAC32_NP", "FAC39_NP", "FAC_TRI", "FACTOR", "FAC32X", "F32", "EST_D")),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  columnas <- c("CD_A", "ENT", "CON", "V_SEL", "TIPO", "N_HOG", "H_MUD", "N_REN")
  expect_identical(enoe_llave(columnas, 1L), c("CD_A", "ENT", "CON", "V_SEL", "TIPO"))
  expect_identical(enoe_llave(columnas, 1L, adicionales = character()), c("CD_A", "ENT", "CON", "V_SEL"))
  expect_identical(enoe_llave(columnas, 3L, adicionales = character()),
                   c("CD_A", "ENT", "CON", "V_SEL", "N_HOG", "H_MUD", "N_REN"))
})

# Contenido de los zip ---------------------------------------------------------------

test_that("se reconocen las tablas de las tres formas de zip", {
  contenido <- function(nombre) enoe_contenido(muestra(file.path("etoe", nombre)), "ETOE")
  abril <- contenido("etoe_2020_abril.zip")
  expect_identical(abril$tablas[enoe_tablas()], c(coe1 = "COE1T0420.dbf", coe2 = "COE2T0420.dbf", hog = "HOGT0420.dbf",
                                                  sdem = "SDEMT0420.dbf", viv = "VIVT0420.dbf"))
  expect_identical(abril$notas, character())
  expect_length(abril$archivos, 5)
  # Extension en mayusculas
  expect_identical(contenido("etoe_2020_abril_cpv2020.zip")$tablas[["sdem"]], "SDEMT0420.DBF")
  # Tablas dentro de una carpeta
  mayo <- contenido("etoe_2020_mayo_cpv2020.zip")
  expect_setequal(names(mayo$tablas), enoe_tablas())
  expect_identical(mayo$tablas[["hog"]], "etoe_2020_mayo_cpv2020_dbf/HOGT0520.DBF")
  # "n" en lugar de "T", en minusculas
  junio <- contenido("etoe_2020_junio_cpv2020.zip")
  expect_identical(junio$tablas[enoe_tablas()], c(coe1 = "coe1n0620.dbf", coe2 = "coe2n0620.dbf", hog = "hogn0620.dbf",
                                                  sdem = "sdemn0620.dbf", viv = "vivn0620.dbf"))
})

# Tablas sin integrar ----------------------------------------------------------------

test_that("etoe() devuelve las cinco tablas del mes en una lista", {
  servidor <- censo_local()
  antes <- descargas_etoe(servidor)
  suppressMessages(
    expect_message(abril <- etoe(year = 2020, mes = "abril"), "Descargando etoe_2020_abril_cpv2020_dbf.zip \\(2.4 MB\\)")
  )
  expect_identical(descargas_etoe(servidor) - antes, 1L)
  expect_type(abril, "list")
  expect_named(abril, c("coe1", "coe2", "hog", "sdem", "viv"))
  expect_identical(vapply(abril, nrow, integer(1)), c(coe1 = 6L, coe2 = 6L, hog = 6L, sdem = 8L, viv = 5L))
  expect_identical(vapply(abril, ncol, integer(1)), c(coe1 = 21L, coe2 = 18L, hog = 19L, sdem = 23L, viv = 17L))
  for (nombre in names(abril)) {
    expect_s3_class(abril[[nombre]], "data.frame")
    expect_identical(names(abril[[nombre]]), toupper(names(abril[[nombre]])), info = nombre)
    # Ninguna columna indica el mes: PER es el trimestre
    expect_identical(unique(abril[[nombre]]$PER), "220", info = nombre)
  }
  # Como en enoe(), la tabla de hogares es la tercera de la lista
  expect_identical(abril[[3]], abril$hog)

  # Los tipos son los que declara el INEGI: texto con ceros a la izquierda en la
  # identificacion y en las respuestas; numeros en ponderadores y precodificadas
  sdem <- abril$sdem
  expect_identical(sdem$R_DEF, rep("00", 8))
  expect_identical(sdem$ENT, c("09", "09", "09", "09", "09", "09", "14", "14"))
  expect_identical(sdem$EDA, c("40", "38", "08", "25", "13", "70", "33", "05"))
  expect_identical(sdem$N_REN, c("01", "02", "03", "01", "02", "01", "01", "02"))
  expect_identical(sdem$CLASE1, c(1L, 1L, 0L, 1L, 2L, 2L, 1L, 0L))
  expect_identical(abril$coe2$P6B2, c("009000", "012000", "007500", NA, NA, "015000"))
  expect_identical(abril$hog$R_DEF, c("00", "03", "00", "00", "00", "15"))
  expect_identical(abril$viv$P3, c("02", NA, NA, NA, NA))
  # Tres ponderadores por tabla: el nacional y los de 32 y 39 ciudades, que
  # valen cero fuera de esas ciudades
  expect_identical(grep("^FAC", names(sdem), value = TRUE), c("FAC_NP", "FAC32_NP", "FAC39_NP"))
  expect_identical(sdem$FAC_NP, c(100L, 100L, 100L, 202L, 202L, 300L, 400L, 400L))
  expect_identical(sdem$FAC32_NP, c(1100L, 1100L, 1100L, 1202L, 1202L, 1300L, 0L, 0L))
  expect_identical(sdem$FAC39_NP, sdem$FAC_NP + 2000L)
  expect_identical(abril$viv$FAC_NP, c(100L, 200L, 300L, 400L, 500L))
  expect_identical(abril$hog$FAC_NP, c(100L, 0L, 201L, 300L, 400L, 0L))
  # En abril y mayo, EST_D es un numero; T_LOC esta en blanco
  expect_identical(abril$viv$EST_D, rep(1L, 5))
  expect_identical(abril$hog$EST_D, c(1L, 0L, 1L, 1L, 1L, 0L))
  expect_identical(abril$viv$T_LOC, rep(NA_character_, 5))

  # Atributos: direccion del archivo y llave de cada tabla, que es la de la
  # ENOE hasta 2020-T1. Los zip de la ETOE no traen notas.
  fuente <- ruta_etoe(servidor, "etoe_2020_abril_cpv2020_dbf.zip")
  vivienda <- c("CD_A", "ENT", "CON", "V_SEL")
  hogar <- c(vivienda, "N_HOG", "H_MUD")
  persona <- c(hogar, "N_REN")
  expect_identical(attr(abril, "fuente"), fuente)
  expect_identical(attr(abril, "llave"), list(coe1 = persona, coe2 = persona, hog = hogar, sdem = persona, viv = vivienda))
  expect_null(attr(abril, "nota"))
  for (nombre in names(abril)) {
    expect_identical(attr(abril[[nombre]], "fuente"), fuente, info = nombre)
    expect_identical(attr(abril[[nombre]], "llave"), attr(abril, "llave")[[nombre]], info = nombre)
    expect_identical(anyDuplicated(abril[[nombre]][attr(abril, "llave")[[nombre]]]), 0L, info = nombre)
    expect_null(attr(abril[[nombre]], "data_types"))
    expect_identical(.row_names_info(abril[[nombre]]), -nrow(abril[[nombre]]), info = nombre)
  }
})

test_that("`poblacion` elige la version de los ponderadores", {
  servidor <- censo_local()
  cpv <- etoe_silencio(2020, "abril")
  antes <- descargas_etoe(servidor)
  suppressMessages(expect_message(
    conapo <- etoe(2020, "abril", poblacion = "conapo"),
    "Descargando etoe_2020_abril_dbf.zip \\(2.4 MB\\)"
  ))
  expect_identical(descargas_etoe(servidor) - antes, 1L)
  expect_identical(attr(conapo, "fuente"), ruta_etoe(servidor, "etoe_2020_abril_dbf.zip"))
  expect_identical(etoe_silencio(2020, 4, poblacion = "CPV2020"), cpv)
  expect_identical(etoe_silencio(2020, "abril", poblacion = "cpv2020"), cpv)
  # Las dos versiones tienen las mismas filas; cambian los ponderadores, que en
  # la version del Censo 2020 llevan el sufijo _NP, y EST_D
  for (nombre in names(cpv)) {
    expect_identical(sub("_NP$", "", names(cpv[[nombre]])), names(conapo[[nombre]]), info = nombre)
    iguales <- setdiff(names(conapo[[nombre]]), c("FAC", "FAC32", "FAC39", "EST_D"))
    expect_identical(valores(cpv[[nombre]][iguales]), valores(conapo[[nombre]][iguales]), info = nombre)
    expect_identical(attr(cpv, "llave")[[nombre]], attr(conapo, "llave")[[nombre]], info = nombre)
  }
  expect_identical(grep("^FAC", names(conapo$sdem), value = TRUE), c("FAC", "FAC32", "FAC39"))
  expect_identical(conapo$sdem$FAC, cpv$sdem$FAC_NP)
  expect_identical(conapo$hog$FAC, c(100L, 100L, 201L, 300L, 400L, 500L))
  expect_identical(cpv$hog$FAC_NP, c(100L, 0L, 201L, 300L, 400L, 0L))
  expect_identical(conapo$hog$EST_D, rep(1L, 6))
})

test_that("mayo y junio se leen con las formas de sus archivos", {
  censo_local()
  abril <- etoe_silencio(2020, "abril")
  # Mayo: las tablas estan dentro de una carpeta
  mayo <- etoe_silencio(2020, "mayo")
  expect_named(mayo, enoe_tablas())
  for (nombre in names(mayo)) {
    expect_identical(valores(mayo[[nombre]]), valores(abril[[nombre]]), info = nombre)
  }
  expect_identical(etoe_silencio(2020, 5), mayo)
  expect_identical(etoe_silencio("2020", "May"), mayo)
  # El ejemplo de la ayuda: apilar dos meses con una columna que indica el mes
  sdem_abril <- abril$sdem
  sdem_mayo <- mayo$sdem
  sdem_abril$MES <- "abril"
  sdem_mayo$MES <- "mayo"
  apilada <- rbind(sdem_abril, sdem_mayo)
  expect_identical(dim(apilada), c(16L, 24L))
  expect_identical(apilada$MES, rep(c("abril", "mayo"), each = 8))
  expect_identical(apilada$FAC_NP, rep(abril$sdem$FAC_NP, 2))

  # Junio: archivos con "n", dos columnas mas (TIPO y NUPM) y EST_D como texto
  junio <- etoe_silencio(2020, "junio")
  expect_named(junio, enoe_tablas())
  expect_identical(vapply(junio, nrow, integer(1)), vapply(mayo, nrow, integer(1)))
  for (nombre in names(junio)) {
    expect_identical(setdiff(names(junio[[nombre]]), names(mayo[[nombre]])), c("TIPO", "NUPM"), info = nombre)
    expect_identical(unique(junio[[nombre]]$PER), "220", info = nombre)
  }
  expect_identical(junio$viv$TIPO, c("2", "2", "2", "1", "1"))
  expect_identical(junio$viv$NUPM, c("0001", "0002", "0002", "0001", "0001"))
  expect_identical(junio$viv$EST_D, rep("0001", 5))
  expect_identical(junio$hog$EST_D, c("0001", "0000", "0001", "0001", "0001", "0000"))
  expect_identical(mayo$viv$EST_D, rep(1L, 5))
  expect_identical(junio$sdem$FAC_NP, mayo$sdem$FAC_NP)
  # La llave es la misma en los tres meses: no incluye TIPO, que en la ENOE
  # forma parte de la llave desde 2020-T3
  expect_identical(attr(junio, "llave"), attr(mayo, "llave"))
  expect_identical(attr(junio, "llave")$viv, c("CD_A", "ENT", "CON", "V_SEL"))
  for (nombre in names(junio)) {
    expect_identical(anyDuplicated(junio[[nombre]][attr(junio, "llave")[[nombre]]]), 0L, info = nombre)
  }
})

test_that("`tabla` elige las tablas que se devuelven", {
  servidor <- censo_local()
  todas <- etoe_silencio(2020, "junio")
  # Una tabla: un data.frame
  antes <- descargas_etoe(servidor)
  sdem <- etoe_silencio(2020, "junio", tabla = "sdem")
  expect_identical(descargas_etoe(servidor) - antes, 1L)
  expect_s3_class(sdem, "data.frame")
  expect_identical(sdem, todas$sdem)
  # Varias: una lista, en el orden en que se pidieron
  varias <- etoe_silencio(2020, "junio", tabla = c("viv", "COE1"))
  expect_named(varias, c("viv", "coe1"))
  expect_identical(varias$coe1, todas$coe1)
  expect_identical(attr(varias, "llave"), attr(todas, "llave")[c("viv", "coe1")])
  # Con el nombre del archivo del INEGI
  expect_identical(etoe_silencio(2020, "junio", tabla = "hogn0620.dbf"), todas$hog)
  expect_identical(etoe_silencio(2020, "abril", tabla = "SDEMT0420"), etoe_silencio(2020, "abril")$sdem)
})

# Integracion ------------------------------------------------------------------------

test_that("integrar = TRUE mantiene los nueve ponderadores y los hogares sin entrevista", {
  censo_local()
  antes <- list.files(tempdir(), pattern = "^importinegi_")
  todas <- etoe_silencio(2020, "abril")
  suppressMessages(expect_message(
    integrada <- etoe(2020, "abril", integrar = TRUE),
    "La tabla integrada tiene 10 filas: 8 de \"sdem\" y 2 de \"hog\" sin fila en \"sdem\", \"coe1\" ni \"coe2\""
  ))
  expect_s3_class(integrada, "data.frame")
  expect_identical(dim(integrada), c(10L, 41L))
  expect_identical(anyDuplicated(names(integrada)), 0L)
  # Los residentes van primero, con los valores de la tabla sociodemografica
  expect_identical(names(integrada)[seq_along(todas$sdem)], names(todas$sdem))
  for (columna in names(todas$sdem)) {
    expect_identical(integrada[[columna]][1:8], todas$sdem[[columna]], info = columna)
  }
  # Despues, las columnas que agrega cada tabla: coe1, coe2, hog y viv
  expect_identical(
    setdiff(names(integrada), names(todas$sdem)),
    c("N_INF", "P1", "P3", "P4_1", "P4_2", "P6B1", "P6B2", "P4_1_HOG", "P4_2_HOG", "FAC_NP_HOG", "FAC32_NP_HOG", "FAC39_NP_HOG",
      "P1_VIV", "P2_VIV", "P3_VIV", "FAC_NP_VIV", "FAC32_NP_VIV", "FAC39_NP_VIV")
  )
  expect_identical(integrada$R_DEF, c(rep("00", 8), "03", "15"))
  expect_identical(integrada$N_REN, c("01", "02", "03", "01", "02", "01", "01", "02", NA, NA))
  expect_identical(integrada$P1, c("1", "1", NA, "1", "2", "2", "1", NA, NA, NA))
  expect_identical(integrada$P1_VIV, c("03", "03", "03", "02", "02", "01", "02", "02", "03", NA))
  expect_identical(integrada$P4_1_HOG, c("2", "2", "2", "1", "1", "2", "2", "2", NA, NA))

  # Tres ponderadores por nivel. Los del residente conservan su nombre y son NA
  # en los hogares sin residentes; los del hogar y los de la vivienda llevan el
  # sufijo de su tabla y llegan a todas las filas.
  expect_identical(grep("^FAC", names(integrada), value = TRUE),
                   c("FAC_NP", "FAC32_NP", "FAC39_NP", "FAC_NP_HOG", "FAC32_NP_HOG", "FAC39_NP_HOG",
                     "FAC_NP_VIV", "FAC32_NP_VIV", "FAC39_NP_VIV"))
  expect_identical(integrada$FAC_NP, c(100L, 100L, 100L, 202L, 202L, 300L, 400L, 400L, NA, NA))
  expect_identical(integrada$FAC_NP_HOG, c(100L, 100L, 100L, 201L, 201L, 300L, 400L, 400L, 0L, 0L))
  expect_identical(integrada$FAC_NP_VIV, c(100L, 100L, 100L, 200L, 200L, 300L, 400L, 400L, 100L, 500L))
  expect_identical(integrada$FAC32_NP, c(1100L, 1100L, 1100L, 1202L, 1202L, 1300L, 0L, 0L, NA, NA))
  expect_identical(integrada$FAC32_NP_HOG, c(1100L, 1100L, 1100L, 1201L, 1201L, 1300L, 0L, 0L, 1100L, 0L))
  expect_identical(integrada$FAC32_NP_VIV, c(1100L, 1100L, 1100L, 1200L, 1200L, 1300L, 0L, 0L, 1100L, 0L))
  expect_identical(integrada$FAC39_NP, c(2100L, 2100L, 2100L, 2202L, 2202L, 2300L, 2400L, 2400L, NA, NA))
  expect_identical(integrada$FAC39_NP_HOG, c(2100L, 2100L, 2100L, 2201L, 2201L, 2300L, 2400L, 2400L, 2100L, 2500L))
  expect_identical(integrada$FAC39_NP_VIV, c(2100L, 2100L, 2100L, 2200L, 2200L, 2300L, 2400L, 2400L, 2100L, 2500L))
  # Las demas columnas repetidas quedan una vez, con el valor de la tabla de mas
  # detalle en la que existe la fila: EST_D es el de la tabla de hogares en los
  # hogares sin residentes
  expect_identical(integrada$EST_D, c(rep(1L, 8), 0L, 0L))
  expect_identical(integrada$PER, rep("220", 10))

  # Atributos: la llave es la de la tabla de mas detalle y sigue siendo unica
  expect_identical(attr(integrada, "fuente"), attr(todas, "fuente"))
  expect_identical(attr(integrada, "llave"), attr(todas, "llave")$sdem)
  expect_identical(anyDuplicated(integrada[attr(integrada, "llave")]), 0L)
  expect_null(attr(integrada, "nota"))

  # Con las proyecciones del CONAPO, los ponderadores no llevan _NP
  conapo <- etoe_silencio(2020, "abril", integrar = TRUE, poblacion = "conapo")
  expect_identical(grep("^FAC", names(conapo), value = TRUE),
                   c("FAC", "FAC32", "FAC39", "FAC_HOG", "FAC32_HOG", "FAC39_HOG", "FAC_VIV", "FAC32_VIV", "FAC39_VIV"))
  expect_identical(conapo$FAC, integrada$FAC_NP)
  expect_identical(conapo$FAC_HOG, c(100L, 100L, 100L, 201L, 201L, 300L, 400L, 400L, 100L, 500L))
  expect_identical(conapo$EST_D, rep(1L, 10))
  # No quedan archivos temporales
  expect_identical(list.files(tempdir(), pattern = "^importinegi_"), antes)
})

test_that("la integracion funciona en los tres meses y con las tablas pedidas", {
  censo_local()
  # Junio: TIPO y NUPM son columnas comunes y quedan una sola vez
  junio <- etoe_silencio(2020, "junio", integrar = TRUE)
  expect_identical(dim(junio), c(10L, 43L))
  expect_identical(sum(names(junio) == "TIPO"), 1L)
  expect_identical(sum(names(junio) == "NUPM"), 1L)
  expect_identical(junio$TIPO, c("2", "2", "2", "2", "2", "2", "1", "1", "2", "1"))
  expect_identical(junio$NUPM, c("0001", "0001", "0001", "0002", "0002", "0002", "0001", "0001", "0001", "0001"))
  expect_identical(junio$EST_D, c(rep("0001", 8), "0000", "0000"))
  expect_identical(attr(junio, "llave"), c("CD_A", "ENT", "CON", "V_SEL", "N_HOG", "H_MUD", "N_REN"))
  expect_identical(anyDuplicated(junio[attr(junio, "llave")]), 0L)
  mayo <- etoe_silencio(2020, "mayo", integrar = TRUE)
  expect_identical(dim(mayo), c(10L, 41L))
  expect_identical(mayo$FAC_NP_VIV, junio$FAC_NP_VIV)
  iguales <- setdiff(names(mayo), "EST_D")
  expect_identical(valores(junio[iguales]), valores(mayo[iguales]))

  # Hogares y viviendas: una fila por hogar
  hogares <- etoe_silencio(2020, "mayo", integrar = TRUE, tabla = c("viv", "hog"))
  expect_identical(nrow(hogares), 6L)
  expect_identical(grep("^FAC", names(hogares), value = TRUE),
                   c("FAC_NP_HOG", "FAC32_NP_HOG", "FAC39_NP_HOG", "FAC_NP_VIV", "FAC32_NP_VIV", "FAC39_NP_VIV"))
  expect_identical(hogares$FAC_NP_VIV, c(100L, 100L, 200L, 300L, 400L, 500L))
  expect_identical(hogares$P1_VIV, c("03", "03", "02", "01", "02", NA))
  # Tablas de personas: no hay hogares que agregar ni mensaje
  mensajes <- testthat::capture_messages(
    personas <- etoe(2020, "mayo", integrar = TRUE, tabla = c("sdem", "coe1"))
  )
  expect_false(any(grepl("La tabla integrada", mensajes)))
  expect_identical(nrow(personas), 8L)
  expect_identical(grep("^FAC", names(personas), value = TRUE), c("FAC_NP", "FAC32_NP", "FAC39_NP"))
})

# Entidades --------------------------------------------------------------------------

test_that("`estado` conserva las filas de las entidades y avisa del alcance de la encuesta", {
  censo_local()
  todas <- etoe_silencio(2020, "abril")
  mensajes <- testthat::capture_messages(jalisco <- etoe(2020, "abril", estado = "Jalisco"))
  expect_true(any(grepl(
    "La ETOE tiene alcance nacional. Su muestra es peque\u00f1a para hacer estimaciones por entidad federativa",
    mensajes
  )))
  expect_named(jalisco, names(todas))
  expect_identical(vapply(jalisco, nrow, integer(1)), c(coe1 = 1L, coe2 = 1L, hog = 2L, sdem = 2L, viv = 2L))
  for (nombre in names(jalisco)) {
    expect_identical(unique(jalisco[[nombre]]$ENT), "14", info = nombre)
    expect_identical(names(jalisco[[nombre]]), names(todas[[nombre]]), info = nombre)
  }
  expect_identical(jalisco$sdem$FAC32_NP, c(0L, 0L))
  expect_identical(etoe_silencio(2020, "abril", estado = 14), jalisco)
  expect_identical(etoe_silencio(2020, "abril", estado = c("cdmx", "jal")), todas)
  # Sin `estado` o con todo el pais no hay aviso
  expect_false(any(grepl("alcance nacional", testthat::capture_messages(etoe(2020, "abril", tabla = "viv")))))
  mensajes <- testthat::capture_messages(nacional <- etoe(2020, "abril", estado = "nacional"))
  expect_false(any(grepl("alcance nacional", mensajes)))
  expect_identical(nacional, todas)

  # Con integrar = TRUE: residentes y hogares sin entrevista de la entidad
  integrada <- etoe_silencio(2020, "abril", integrar = TRUE, estado = "Jalisco")
  expect_identical(integrada$R_DEF, c("00", "00", "15"))
  expect_identical(integrada$N_REN, c("01", "02", NA))
  expect_identical(integrada$FAC_NP_VIV, c(400L, 400L, 500L))
  expect_identical(names(integrada), names(etoe_silencio(2020, "abril", integrar = TRUE)))
  # Una entidad sin filas en la muestra
  colima <- etoe_silencio(2020, "abril", tabla = "sdem", estado = "Colima")
  expect_identical(nrow(colima), 0L)
  expect_named(colima, names(todas$sdem))
})

# Archivos que no estan en el registro o en el sitio --------------------------------

test_that("etoe() avisa cuando el INEGI o el registro no tienen el archivo", {
  censo_local()
  # El servidor local no tiene mayo ni junio con las proyecciones del CONAPO
  expect_error(
    etoe_silencio(2020, "mayo", poblacion = "conapo"),
    "El INEGI no tiene publicado el archivo .*etoe_2020_mayo_dbf.zip"
  )
  # Registro sin la version de las proyecciones del CONAPO
  registro <- registro_archivos[!(registro_archivos$proyecto == "etoe" & !grepl("cpv2020", registro_archivos$ruta)), ]
  attr(registro, "generado") <- "2026-11-23"
  withr::local_options(importinegi.registro = registro)
  expect_identical(nrow(etoe_meses()), 3L)
  expect_identical(nrow(etoe_silencio(2020, "abril", tabla = "viv")), 5L)
  expect_error(
    etoe(2020, "abril", poblacion = "conapo"),
    "El registro de archivos del paquete \\(2026-11-23\\) no tiene la ETOE de abril de 2020 con los ponderadores \"conapo\""
  )
  # Sin los archivos de la ETOE en el registro
  withr::local_options(importinegi.registro = registro_archivos[registro_archivos$proyecto != "etoe", ])
  expect_error(etoe(2020, "abril"), "El registro de archivos del paquete no tiene los archivos de la ETOE")
  expect_identical(nrow(etoe_bases()), 0L)
  expect_named(etoe_bases(), names(enoe_bases()))
})

# bases_importinegi() ----------------------------------------------------------------

test_that("bases_importinegi() lista los meses de la ETOE", {
  withr::local_options(importinegi.registro = NULL)
  bases <- bases_importinegi("etoe")
  # Una fila por mes: cada fila de la tabla es una combinacion distinta de
  # proyecto, a\u00f1o y base
  expect_identical(nrow(bases), 3L)
  expect_identical(unique(bases$proyecto), "etoe")
  expect_identical(bases$year, rep(2020L, 3))
  expect_true(all(is.na(bases$modulo)))
  # `base` es el mes, como se escribe en etoe(), y `descripcion` indica las
  # versiones de los ponderadores, como se escriben en `poblacion`
  expect_identical(bases$base, c("abril", "mayo", "junio"))
  expect_identical(bases$descripcion,
                   rep("Versiones de los ponderadores (argumento poblacion): \"cpv2020\" y \"conapo\"", 3))
  expect_identical(unique(bases$tablas), "coe1, coe2, hog, sdem, viv")
  expect_identical(unique(bases$cobertura), "nacional")
  expect_identical(unique(bases$formato), "dbf")
  # El tama\u00f1o es el del archivo que se descarga por omision
  expect_identical(bases$tamano_mb, c(2.4, 2.2, 2.6))
  expect_identical(bases_importinegi("etoe", 2020), bases)
  expect_identical(bases_importinegi("ETOE"), bases)
  expect_message(vacia <- bases_importinegi("etoe", 2021), "no tiene bases de datos para esa consulta")
  expect_identical(nrow(vacia), 0L)
  # Cada fila corresponde a un mes que etoe() puede elegir, en sus dos versiones
  for (i in seq_len(nrow(bases))) {
    elegido <- etoe_elegir(bases$year[i], bases$base[i], "cpv2020")
    expect_identical(round(tamano_a_bytes(elegido$tamano) / 1024^2, 1), bases$tamano_mb[i], info = i)
    expect_identical(etoe_elegir(bases$year[i], bases$base[i], "conapo")$mes, elegido$mes, info = i)
  }
  # La ETOE va despues de la ENOE en la tabla completa
  todas <- bases_importinegi()
  proyectos <- unique(todas$proyecto)
  expect_identical(match("etoe", proyectos), match("enoe", proyectos) + 1L)
  expect_identical(valores(todas[todas$proyecto == "etoe", ]), valores(bases))

  # Con una sola version en el registro, la descripcion lo dice
  registro <- registro_archivos[!(registro_archivos$proyecto == "etoe" & !grepl("cpv2020", registro_archivos$ruta)), ]
  withr::local_options(importinegi.registro = registro)
  expect_identical(etoe_bases()$descripcion, rep("Versiones de los ponderadores (argumento poblacion): \"cpv2020\"", 3))
})
