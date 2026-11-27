# Pruebas de enoe(). Los archivos de muestra (fixtures/enoe) son inventados y
# tienen la forma de los zip trimestrales del INEGI; los genera
# data-raw/muestras_enoe.R. El servidor local los entrega en las rutas reales
# del registro del paquete. Todas las muestras tienen cinco viviendas, seis
# hogares (dos sin entrevista) y ocho residentes (seis con cuestionario de
# ocupacion y empleo).

enoe_silencio <- function(...) {
  suppressMessages(enoe(...))
}

ruta_enoe <- function(servidor, archivo) {
  servidor$url(paste0("/contenidos/programas/enoe/15ymas/microdatos/", archivo))
}

tabla_prueba <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

# Trimestres del registro ----------------------------------------------------------

test_that("el registro tiene los 86 trimestres de la ENOE", {
  withr::local_options(importinegi.registro = NULL)
  trimestres <- enoe_trimestres()
  expect_named(trimestres, c("year", "trimestre", "edicion", "url", "tamano", "bytes"))
  expect_false(anyNA(trimestres))
  # Todos los trimestres de 2005-T1 a 2026-T3, salvo 2020-T2 (lo sustituyo la ETOE)
  todos <- expand.grid(trimestre = 1:4, year = 2005:2026)
  todos <- todos[!(todos$year == 2026 & todos$trimestre > 3) & !(todos$year == 2020 & todos$trimestre == 2), ]
  expect_identical(paste(trimestres$year, trimestres$trimestre), paste(todos$year, todos$trimestre))
  expect_identical(nrow(trimestres), 86L)
  # Tres formas de nombre: 2005trim1, enoe_n_2020_trim3 y enoe_2023_trim1
  forma <- sub("^.*/microdatos/([a-z_]*)[0-9]{4}_?trim[1-4]_dbf[.]zip$", "\\1", trimestres$url)
  expect_identical(vapply(c("", "enoe_n_", "enoe_"), function(f) sum(forma == f), integer(1), USE.NAMES = FALSE),
                   c(61L, 10L, 15L))
  expect_identical(forma[trimestres$year == 2020], c("", "enoe_n_", "enoe_n_"))
  # La ENOE Nueva Edicion va de 2020-T3 a 2022-T4
  nueva <- trimestres[trimestres$edicion == "ENOE_N", ]
  expect_identical(nrow(nueva), 10L)
  expect_identical(paste(nueva$year, nueva$trimestre)[c(1, 10)], c("2020 3", "2022 4"))
  expect_identical(unique(trimestres$edicion), c("ENOE", "ENOE_N"))
  expect_true(all(trimestres$bytes > 15e6 & trimestres$bytes < 45e6))
})

test_that("el cuestionario ampliado se asigna a los 26 trimestres en que se aplico", {
  withr::local_options(importinegi.registro = NULL)
  trimestres <- enoe_trimestres()
  cuestionario <- enoe_cuestionario(trimestres$year, trimestres$trimestre)
  expect_identical(sum(cuestionario == "ampliado"), 26L)
  expect_identical(sum(cuestionario == "b\u00e1sico"), 60L)
  ampliado <- paste(trimestres$year, trimestres$trimestre)[cuestionario == "ampliado"]
  expect_identical(ampliado[1:8], c("2005 1", "2005 2", "2005 3", "2005 4", "2006 1", "2006 2", "2007 2", "2008 2"))
  expect_identical(ampliado[9:26], paste(2009:2026, 1))
  expect_identical(enoe_cuestionario(c(2006, 2007, 2008, 2020), c(3, 1, 1, 3)), rep("b\u00e1sico", 4))
  expect_identical(enoe_etiqueta(c(2005, 2026), c(1, 4)), c("primer trimestre de 2005", "cuarto trimestre de 2026"))
})

# Argumentos -----------------------------------------------------------------------

test_that("el a\u00f1o y el trimestre se validan sin descargar", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(enoe(), "Falta indicar el a\u00f1o en `year` \\(un solo valor\\).*de 2005 a 2026.*bases_importinegi\\(\"enoe\"\\)")
  expect_error(enoe(NA, "trim1"), "Falta indicar el a\u00f1o en `year`")
  expect_error(enoe(c(2024, 2025), 1), "Falta indicar el a\u00f1o en `year`")
  expect_error(enoe("dos mil", 1), "`year = \"dos mil\"` no es un a\u00f1o")
  expect_error(enoe(24, 1), "`year = 24` no es un a\u00f1o")
  expect_error(enoe(2024), "Falta indicar el trimestre en `trimestre` \\(un solo valor\\).*\"trim1\"")
  expect_error(enoe(2024, NA), "Falta indicar el trimestre en `trimestre`")
  expect_error(enoe(2024, c(1, 2)), "Falta indicar el trimestre en `trimestre`")
  expect_error(enoe(2024, 5), "`trimestre = 5` no es un trimestre")
  expect_error(enoe(2024, "primero"), "`trimestre = \"primero\"` no es un trimestre.*o el n\u00famero del trimestre")
  # Trimestres que no existen
  expect_error(
    enoe(2004, 4),
    "La ENOE del cuarto trimestre de 2004 no existe.*del primer trimestre de 2005 al tercer trimestre de 2026"
  )
  # 2020-T2: el mensaje remite a etoe()
  expect_error(
    enoe(2020, "trim2"),
    "El INEGI no public\u00f3 la ENOE del segundo trimestre de 2020.*\\(ETOE\\), que es otro proyecto y se descarga con etoe\\(\\): por ejemplo, etoe\\(2020, \"abril\"\\)"
  )
  expect_error(enoe(2999, 1), "El primer trimestre de 2999 todav\u00eda no ocurre")
  # El trimestre se escribe como en la serie 1.x o como numero
  for (trimestre in list("trim3", "TRIM3", " trim3 ", "trim 3", "Trimestre 3", "t3", "3", 3, 3L)) {
    expect_identical(enoe_validar_trimestre(trimestre), 3L)
  }
  # El a\u00f1o tambien se acepta como texto
  expect_identical(enoe_elegir("2024", "trim1")$etiqueta, "primer trimestre de 2024")
  expect_true(enoe_elegir(2024, 1)$registrado)
  expect_match(enoe_elegir(2005, 1)$url, "/contenidos/programas/enoe/15ymas/microdatos/2005trim1_dbf[.]zip$")
  expect_match(enoe_elegir(2021, 4)$url, "/microdatos/enoe_n_2021_trim4_dbf[.]zip$")
  expect_match(enoe_elegir(2025, 3)$url, "/microdatos/enoe_2025_trim3_dbf[.]zip$")
  # El ultimo trimestre del registro
  ultimo <- enoe_elegir(2026, 3)
  expect_true(ultimo$registrado)
  expect_match(ultimo$url, "/microdatos/enoe_2026_trim3_dbf[.]zip$")
  expect_identical(ultimo$tamano, "32.7 MB")
  expect_identical(enoe_cuestionario(2026, 3), "b\u00e1sico")
  # El trimestre en curso todavia no esta en el registro: se busca con el nombre vigente
  expect_false(enoe_elegir(2026, 4)$registrado)
})

test_that("`integrar`, `tabla` y `estado` se validan antes de descargar", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(enoe(2024, 1, integrar = "si"), "`integrar` debe ser TRUE o FALSE")
  expect_error(enoe(2024, 1, integrar = NA), "`integrar` debe ser TRUE o FALSE")
  expect_error(enoe(2024, 1, tabla = "personas"), "`tabla = \"personas\"` no es una tabla de la ENOE.*\"sdem\" \\(sociodemogr\u00e1fico\\)")
  expect_error(enoe(2024, 1, tabla = c("sdem", "coe3")), "`tabla = \"coe3\"` no es una tabla de la ENOE")
  expect_error(enoe(2024, 1, tabla = 3), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(enoe(2024, 1, tabla = character()), "`tabla` debe ser el nombre de una o varias tablas")
  expect_error(enoe(2024, 1, estado = "Atlantida"), "No se reconoce la entidad")
  expect_identical(enoe_validar_tabla(NULL), c("coe1", "coe2", "hog", "sdem", "viv"))
  expect_identical(enoe_validar_tabla(c("SDEM", " coe1 ", "sdem")), c("sdem", "coe1"))
  # Nombres de los archivos del INEGI, como se llamaban las tablas en la serie 1.x
  expect_identical(enoe_validar_tabla(c("SDEMT124", "ENOE_COE1T124.dbf", "enoen_vivt320", "hogt105.DBF")),
                   c("sdem", "coe1", "viv", "hog"))
})

# Contenido de los zip -------------------------------------------------------------

test_that("se reconocen las tablas y la nota de cada forma de zip", {
  contenido <- function(nombre) enoe_contenido(muestra(file.path("enoe", nombre)))
  y2005 <- contenido("enoe_2005_1.zip")
  expect_identical(y2005$tablas[enoe_tablas()], c(coe1 = "COE1T105.dbf", coe2 = "COE2T105.dbf", hog = "HOGT105.dbf",
                                                  sdem = "SDEMT105.dbf", viv = "VIVT105.dbf"))
  expect_identical(y2005$notas, "nota_bases_datos_enoe_2005_1t.txt")
  expect_length(y2005$archivos, 6)
  # Nombres en minusculas (2012-T1 a 2014-T4)
  y2012 <- contenido("enoe_2012_2.zip")
  expect_identical(y2012$tablas[["sdem"]], "sdemt212.dbf")
  expect_setequal(names(y2012$tablas), enoe_tablas())
  # Prefijo de la edicion y extension en mayusculas
  y2020 <- contenido("enoe_2020_3.zip")
  expect_identical(y2020$tablas[["coe1"]], "ENOEN_COE1T320.DBF")
  expect_setequal(names(y2020$tablas), enoe_tablas())
  y2024 <- contenido("enoe_2024_1.zip")
  expect_identical(y2024$tablas[["hog"]], "ENOE_HOGT124.dbf")
  expect_identical(y2024$notas, character())
  # Zip incompleto
  expect_setequal(names(contenido("enoe_2013_1.zip")$tablas), c("sdem", "viv"))
  # Un zip sin tablas de la ENOE
  expect_identical(unname(enoe_contenido(muestra("gobierno/cngmd2011_agua.zip"))$tablas), character())
})

test_that("la nota del INEGI se lee en cualquier codificacion", {
  nota <- function(nombre) {
    zip <- muestra(file.path("enoe", nombre))
    enoe_nota(zip, enoe_contenido(zip)$notas)
  }
  # Pagina de codigos de Windows
  expect_identical(
    nota("enoe_2005_1.zip"),
    "El 30 de noviembre de 2021 se reemplazaron los archivos de la base de datos por la actualizaci\u00f3n de los ponderadores."
  )
  # UTF-8 con marca de orden de bytes
  expect_identical(
    nota("enoe_2012_2.zip"),
    "El 28 de agosto de 2023 se reemplazaron los archivos de la base de datos por la actualizaci\u00f3n de los ponderadores."
  )
  # Varias lineas: se quitan las vacias
  expect_identical(
    nota("enoe_2020_3.zip"),
    paste0("El 17 de mayo de 2021 se reemplazaron los archivos de la base de datos.\n",
           "El 23 de agosto de 2021 se reemplazaron de nuevo, por un ajuste en los ponderadores.")
  )
  expect_null(nota("enoe_2024_1.zip"))
  # UTF-16, como lo guarda el Bloc de notas de Windows
  archivo <- withr::local_tempfile(fileext = ".txt")
  writeBin(c(as.raw(c(0xff, 0xfe)), iconv("Se actualiz\u00f3 el archivo.\r\n", "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), archivo)
  expect_identical(enoe_leer_texto(archivo), "Se actualiz\u00f3 el archivo.")
  # Un problema con la nota no impide leer las tablas
  expect_null(enoe_nota(muestra("enoe/enoe_2024_1.zip"), "no_existe.txt"))
})

# Tablas sin integrar ----------------------------------------------------------------

test_that("enoe() devuelve las cinco tablas del trimestre en una lista", {
  servidor <- censo_local()
  antes <- descargas_enoe(servidor)
  suppressMessages(
    expect_message(enoe05 <- enoe(year = 2005, trimestre = "trim1"), "Descargando 2005trim1_dbf.zip \\(27.2 MB\\)")
  )
  expect_identical(descargas_enoe(servidor) - antes, 1L)
  expect_type(enoe05, "list")
  expect_named(enoe05, c("coe1", "coe2", "hog", "sdem", "viv"))
  expect_identical(vapply(enoe05, nrow, integer(1)), c(coe1 = 6L, coe2 = 6L, hog = 6L, sdem = 8L, viv = 5L))
  expect_identical(vapply(enoe05, ncol, integer(1)), c(coe1 = 19L, coe2 = 16L, hog = 15L, sdem = 19L, viv = 13L))
  for (nombre in names(enoe05)) {
    expect_s3_class(enoe05[[nombre]], "data.frame")
    expect_identical(names(enoe05[[nombre]]), toupper(names(enoe05[[nombre]])), info = nombre)
  }
  # Como en la serie 1.x, la tabla de hogares es la tercera de la lista
  expect_identical(enoe05[[3]], enoe05$hog)

  # Los tipos son los que declara el INEGI: texto con ceros a la izquierda en la
  # identificacion y en las respuestas; numeros en ponderadores y precodificadas
  sdem <- enoe05$sdem
  expect_identical(sdem$R_DEF, rep("00", 8))
  expect_identical(sdem$ENT, c("09", "09", "09", "09", "09", "09", "14", "14"))
  expect_identical(sdem$EDA, c("40", "38", "08", "25", "13", "70", "33", "05"))
  expect_identical(sdem$N_REN, c("01", "02", "03", "01", "02", "01", "01", "02"))
  expect_identical(sdem$FAC, c(100L, 100L, 100L, 202L, 202L, 300L, 400L, 400L))
  expect_identical(sdem$CLASE1, c(1L, 1L, 0L, 1L, 2L, 2L, 1L, 0L))
  expect_identical(enoe05$coe2$P6B2, c("009000", "012000", "007500", NA, NA, "015000"))
  expect_identical(enoe05$hog$R_DEF, c("00", "03", "00", "00", "00", "15"))
  # Las celdas en blanco son NA
  expect_identical(enoe05$viv$P3, c("02", NA, NA, NA, NA))
  expect_identical(enoe05$hog$P4_1, c("2", NA, "1", "2", "2", NA))

  # Atributos: direccion del archivo, llave de cada tabla y nota del INEGI
  fuente <- ruta_enoe(servidor, "2005trim1_dbf.zip")
  vivienda <- c("CD_A", "ENT", "CON", "V_SEL")
  hogar <- c(vivienda, "N_HOG", "H_MUD")
  persona <- c(hogar, "N_REN")
  expect_identical(attr(enoe05, "fuente"), fuente)
  expect_identical(attr(enoe05, "llave"), list(coe1 = persona, coe2 = persona, hog = hogar, sdem = persona, viv = vivienda))
  expect_match(attr(enoe05, "nota"), "^El 30 de noviembre de 2021 se reemplazaron")
  for (nombre in names(enoe05)) {
    expect_identical(attr(enoe05[[nombre]], "fuente"), fuente, info = nombre)
    expect_identical(attr(enoe05[[nombre]], "llave"), attr(enoe05, "llave")[[nombre]], info = nombre)
    expect_null(attr(enoe05[[nombre]], "nota"))
    expect_null(attr(enoe05[[nombre]], "data_types"))
    expect_identical(.row_names_info(enoe05[[nombre]]), -nrow(enoe05[[nombre]]), info = nombre)
  }
})

test_that("la nota del INEGI se muestra y se guarda en el atributo \"nota\"", {
  censo_local()
  suppressMessages(expect_message(
    viv <- enoe(2020, 3, tabla = "viv"),
    "Nota del INEGI sobre los archivos del tercer trimestre de 2020:\nEl 17 de mayo de 2021"
  ))
  expect_match(attr(viv, "nota"), "ajuste en los ponderadores[.]$")
  # Sin nota en el zip, no hay mensaje ni atributo
  mensajes <- testthat::capture_messages(viv24 <- enoe(2024, 1, tabla = "viv"))
  expect_false(any(grepl("Nota del INEGI", mensajes)))
  expect_null(attr(viv24, "nota"))
})

test_that("la llave cambia con la estructura del trimestre", {
  censo_local()
  llave <- function(year, trimestre) attr(enoe_silencio(year, trimestre), "llave")
  # 2005-T1 a 2020-T1
  expect_identical(llave(2012, 2)$viv, c("CD_A", "ENT", "CON", "V_SEL"))
  # 2020-T3 a 2021-T2: TIPO, MES_CAL y CA
  y2020 <- llave(2020, 3)
  expect_identical(y2020$viv, c("CD_A", "ENT", "CON", "V_SEL", "TIPO", "MES_CAL", "CA"))
  expect_identical(y2020$hog, c(y2020$viv, "N_HOG", "H_MUD"))
  expect_identical(y2020$sdem, c(y2020$hog, "N_REN"))
  expect_identical(y2020$coe1, y2020$sdem)
  # 2021-T3 a 2025-T2: TIPO y MES_CAL
  expect_identical(llave(2024, 1)$viv, c("CD_A", "ENT", "CON", "V_SEL", "TIPO", "MES_CAL"))
  # Desde 2025-T3: CVE_ENT en lugar de ENT
  expect_identical(llave(2026, 2)$coe2, c("CD_A", "CVE_ENT", "CON", "V_SEL", "TIPO", "MES_CAL", "N_HOG", "H_MUD", "N_REN"))

  # La llave identifica cada fila de las cinco tablas en las cinco muestras
  for (trimestre in list(c(2005, 1), c(2012, 2), c(2020, 3), c(2024, 1), c(2026, 2))) {
    tablas <- enoe_silencio(trimestre[1], trimestre[2])
    for (nombre in names(tablas)) {
      etiqueta <- paste(paste(trimestre, collapse = "-T"), nombre)
      expect_identical(anyDuplicated(tablas[[nombre]][attr(tablas, "llave")[[nombre]]]), 0L, info = etiqueta)
    }
  }
  # Desde 2020-T3, la llave de 2005 ya no distingue a las viviendas
  viv <- enoe_silencio(2024, 1, tabla = "viv")
  expect_identical(nrow(unique(viv[c("CD_A", "ENT", "CON", "V_SEL")])), 3L)
  expect_identical(nrow(unique(viv[c("CD_A", "ENT", "CON", "V_SEL", "TIPO")])), 4L)
  expect_identical(nrow(unique(viv[c("CD_A", "ENT", "CON", "V_SEL", "TIPO", "MES_CAL")])), 5L)
  expect_identical(enoe_llave(c("CD_A", "ENT", "CON", "V_SEL", "X"), 1L), c("CD_A", "ENT", "CON", "V_SEL"))
})

test_that("`tabla` elige las tablas que se devuelven", {
  censo_local()
  todas <- enoe_silencio(2024, 1)
  # Una tabla: un data.frame
  sdem <- enoe_silencio(2024, 1, tabla = "sdem")
  expect_s3_class(sdem, "data.frame")
  expect_identical(sdem, todas$sdem)
  expect_identical(sdem$CS_P21_DES, c("CIUDAD DE M\u00c9XICO", NA, NA, "NUEVO LE\u00d3N", NA, "MICHOAC\u00c1N", NA, NA))
  # Como en todo el paquete, el texto "NULL" se devuelve como NA: el DBF de la
  # muestra lo trae en la ultima fila de esa columna
  zip <- muestra("enoe/enoe_2024_1.zip")
  carpeta <- inegi_descomprimir(zip, anidados = FALSE, archivos = enoe_contenido(zip)$tablas[["sdem"]])
  withr::defer(unlink(carpeta, recursive = TRUE))
  crudo <- foreign::read.dbf(buscar_archivos(carpeta, extension = "dbf")[1], as.is = TRUE)
  expect_identical(crudo$CS_P21_DES[8], "NULL")
  expect_true(is.na(sdem$CS_P21_DES[8]))
  # Varias: una lista, en el orden en que se pidieron
  varias <- enoe_silencio(2024, 1, tabla = c("sdem", "COE1"))
  expect_named(varias, c("sdem", "coe1"))
  expect_identical(varias$coe1, todas$coe1)
  expect_identical(attr(varias, "llave"), attr(todas, "llave")[c("sdem", "coe1")])
  # Con el nombre del archivo del INEGI
  expect_identical(enoe_silencio(2024, 1, tabla = "ENOE_HOGT124"), todas$hog)

  # Solo se leen las tablas pedidas: el zip de 2013-T1 de la muestra tiene dos
  expect_identical(nrow(enoe_silencio(2013, 1, tabla = "sdem")), 8L)
  expect_named(enoe_silencio(2013, 1, tabla = c("viv", "sdem")), c("viv", "sdem"))
  expect_error(
    enoe_silencio(2013, 1),
    "El archivo de la ENOE del primer trimestre de 2013 no contiene las tablas \"coe1\", \"coe2\", \"hog\". Contiene: .*\"sdemt113.dbf\""
  )
  expect_error(enoe_silencio(2013, 1, tabla = "hog"), "no contiene la tabla \"hog\"")
})

# Integracion ------------------------------------------------------------------------

test_that("integrar = TRUE devuelve una fila por residente y conserva los hogares sin entrevista", {
  censo_local()
  todas <- enoe_silencio(2024, 1)
  suppressMessages(expect_message(
    integrada <- enoe(2024, 1, integrar = TRUE),
    "La tabla integrada tiene 10 filas: 8 de \"sdem\" y 2 de \"hog\" sin fila en \"sdem\", \"coe1\" ni \"coe2\""
  ))
  expect_s3_class(integrada, "data.frame")
  expect_identical(dim(integrada), c(10L, 40L))
  expect_identical(rownames(integrada), as.character(1:10))
  expect_identical(anyDuplicated(names(integrada)), 0L)
  residentes <- integrada[1:8, ]
  sin_entrevista <- integrada[9:10, ]

  # Los residentes van primero, en el orden y con los valores de la tabla sociodemografica
  expect_identical(names(integrada)[seq_along(todas$sdem)], names(todas$sdem))
  for (columna in names(todas$sdem)) {
    expect_identical(residentes[[columna]], todas$sdem[[columna]], info = columna)
  }
  # Despues, las columnas que agrega cada tabla: coe1, coe2, hog y viv
  expect_identical(
    setdiff(names(integrada), names(todas$sdem)),
    c("N_INF", "P1", "P3", "P4_1", "P4_2", "P6B1", "P6B2", "P4_1_HOG", "P4_2_HOG", "TIPOLEV", "FAC_TRI_HOG", "FAC_MEN_HOG",
      "P1_VIV", "P2_VIV", "P3_VIV", "FAC_TRI_VIV", "FAC_MEN_VIV")
  )
  # Los residentes sin cuestionario de ocupacion (menores de 12 a\u00f1os) quedan con NA
  expect_identical(integrada$P1, c("1", "1", NA, "1", "2", "2", "1", NA, NA, NA))
  expect_identical(integrada$P3, c("8343", "2332", NA, "2512", NA, NA, "1321", NA, NA, NA))
  expect_identical(integrada$P6B2, c("009000", "012000", NA, "007500", NA, NA, "015000", NA, NA, NA))
  # Las preguntas de hogar y de vivienda llevan el sufijo de su tabla y no se
  # confunden con las del cuestionario de ocupacion que se llaman igual
  expect_identical(integrada$P4_1, c("1", "1", NA, "2", NA, NA, "1", NA, NA, NA))
  expect_identical(integrada$P4_1_HOG, c("2", "2", "2", "1", "1", "2", "2", "2", NA, NA))
  expect_identical(integrada$P4_2_HOG, c(NA, NA, NA, "02", "02", NA, NA, NA, NA, NA))
  # Las viviendas 2 y 3 solo se distinguen por TIPO, y la 4 y la 5, por MES_CAL
  expect_identical(integrada$P1_VIV, c("03", "03", "03", "02", "02", "01", "02", "02", "03", NA))
  expect_identical(integrada$P3_VIV, c("02", "02", "02", NA, NA, NA, NA, NA, "02", NA))
  expect_identical(integrada$TIPOLEV, c("1", "1", "1", "1", "1", "2", "1", "1", "1", "1"))

  # Hogares sin entrevista: sin residentes, con NA en las columnas de las tablas
  # de residentes (tambien en el ponderador del residente) y con los valores de
  # la tabla de hogares en las demas
  expect_identical(sin_entrevista$R_DEF, c("03", "15"))
  solo_residentes <- c("N_REN", "C_RES", "SEX", "EDA", "CS_P21_DES", "CLASE1", "INGOCUP", "FAC_TRI", "FAC_MEN",
                       "N_INF", "P1", "P3", "P4_1", "P4_2", "P6B1", "P6B2")
  expect_true(all(is.na(sin_entrevista[solo_residentes])))
  hogares <- todas$hog[todas$hog$R_DEF != "00", ]
  con_sufijo <- c("P4_1", "P4_2", "FAC_TRI", "FAC_MEN")
  for (columna in setdiff(names(hogares), con_sufijo)) {
    expect_identical(sin_entrevista[[columna]], hogares[[columna]], info = columna)
  }
  for (columna in con_sufijo) {
    expect_identical(sin_entrevista[[paste0(columna, "_HOG")]], hogares[[columna]], info = columna)
  }
  expect_identical(which(is.na(integrada$N_REN)), 9:10)

  # Tres ponderadores, uno por nivel. El del residente conserva su nombre: es el
  # de la tabla sociodemografica (202 en el hogar de la vivienda 2) y es NA en
  # los hogares sin residentes. El del hogar (201) y el de la vivienda (200)
  # llevan el sufijo de su tabla y llegan a todas las filas.
  expect_identical(integrada$FAC_TRI, c(100L, 100L, 100L, 202L, 202L, 300L, 400L, 400L, NA, NA))
  expect_identical(integrada$FAC_TRI_HOG, c(100L, 100L, 100L, 201L, 201L, 300L, 400L, 400L, 100L, 500L))
  expect_identical(integrada$FAC_TRI_VIV, c(100L, 100L, 100L, 200L, 200L, 300L, 400L, 400L, 100L, 500L))
  expect_identical(integrada$FAC_MEN, integrada$FAC_TRI * 3L)
  expect_identical(integrada$FAC_MEN_HOG, integrada$FAC_TRI_HOG * 3L)
  expect_identical(integrada$FAC_MEN_VIV, integrada$FAC_TRI_VIV * 3L)
  # Las demas columnas repetidas quedan una vez, con el valor de la tabla de
  # mas detalle en la que existe la fila
  expect_identical(integrada$UR, c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L))
  expect_identical(integrada$CS_P21_DES[c(1, 4, 6)], c("CIUDAD DE M\u00c9XICO", "NUEVO LE\u00d3N", "MICHOAC\u00c1N"))

  # Atributos: la llave es la de la tabla de mas detalle y sigue siendo unica
  expect_identical(attr(integrada, "fuente"), attr(todas, "fuente"))
  expect_identical(attr(integrada, "llave"), attr(todas, "llave")$sdem)
  expect_identical(anyDuplicated(integrada[attr(integrada, "llave")]), 0L)
  expect_null(attr(integrada, "nota"))
})

test_that("la integracion funciona con las cuatro estructuras de la serie", {
  censo_local()
  antes <- list.files(tempdir(), pattern = "^importinegi_")
  for (trimestre in list(c(2005, 1), c(2012, 2), c(2020, 3), c(2024, 1), c(2026, 2))) {
    etiqueta <- paste(trimestre, collapse = "-T")
    integrada <- enoe_silencio(trimestre[1], trimestre[2], integrar = TRUE)
    ponderador <- if (trimestre[1] < 2020) "FAC" else "FAC_TRI"
    expect_identical(nrow(integrada), 10L, info = etiqueta)
    expect_identical(integrada$R_DEF, c(rep("00", 8), "03", "15"), info = etiqueta)
    expect_identical(integrada$EDA, c("40", "38", "08", "25", "13", "70", "33", "05", NA, NA), info = etiqueta)
    expect_identical(integrada$P1_VIV, c("03", "03", "03", "02", "02", "01", "02", "02", "03", NA), info = etiqueta)
    expect_identical(integrada$P6B1, c("1", "1", NA, "1", NA, NA, "1", NA, NA, NA), info = etiqueta)
    expect_identical(integrada[[ponderador]], c(100L, 100L, 100L, 202L, 202L, 300L, 400L, 400L, NA, NA), info = etiqueta)
    expect_identical(integrada[[paste0(ponderador, "_HOG")]], c(100L, 100L, 100L, 201L, 201L, 300L, 400L, 400L, 100L, 500L),
                     info = etiqueta)
    expect_identical(integrada[[paste0(ponderador, "_VIV")]], c(100L, 100L, 100L, 200L, 200L, 300L, 400L, 400L, 100L, 500L),
                     info = etiqueta)
    # FAC hasta 2020-T1; despues, FAC_TRI y FAC_MEN
    expect_identical(grep("^FAC", names(integrada), value = TRUE),
                     if (trimestre[1] < 2020) c("FAC", "FAC_HOG", "FAC_VIV")
                     else c("FAC_TRI", "FAC_MEN", "FAC_TRI_HOG", "FAC_MEN_HOG", "FAC_TRI_VIV", "FAC_MEN_VIV"),
                     info = etiqueta)
    expect_identical(anyDuplicated(integrada[attr(integrada, "llave")]), 0L, info = etiqueta)
    expect_identical(attr(integrada, "nota"), attr(enoe_silencio(trimestre[1], trimestre[2], tabla = "viv"), "nota"),
                     info = etiqueta)
  }
  # No quedan archivos temporales
  expect_identical(list.files(tempdir(), pattern = "^importinegi_"), antes)
})

test_that("la integracion usa solo las tablas pedidas", {
  censo_local()
  todas <- enoe_silencio(2024, 1)
  # Tablas de personas: no hay hogares ni viviendas que agregar
  mensajes <- testthat::capture_messages(
    personas <- enoe(2024, 1, integrar = TRUE, tabla = c("coe2", "sdem", "coe1"))
  )
  expect_false(any(grepl("La tabla integrada", mensajes)))
  expect_identical(nrow(personas), 8L)
  expect_identical(names(personas), c(names(todas$sdem), "N_INF", "P1", "P3", "P4_1", "P4_2", "P6B1", "P6B2"))
  expect_identical(attr(personas, "llave"), attr(todas, "llave")$sdem)

  # Hogares y viviendas: una fila por hogar, con las preguntas y los
  # ponderadores con su sufijo
  hogares <- enoe_silencio(2024, 1, integrar = TRUE, tabla = c("viv", "hog"))
  expect_identical(nrow(hogares), 6L)
  expect_identical(names(hogares), c(sub("^(P4_[12]|FAC_TRI|FAC_MEN)$", "\\1_HOG", names(todas$hog)),
                                     "P1_VIV", "P2_VIV", "P3_VIV", "FAC_TRI_VIV", "FAC_MEN_VIV"))
  expect_identical(hogares$FAC_TRI_HOG, todas$hog$FAC_TRI)
  expect_identical(hogares$FAC_TRI_VIV, c(100L, 100L, 200L, 300L, 400L, 500L))
  expect_identical(hogares$P1_VIV, c("03", "03", "02", "01", "02", NA))
  expect_identical(attr(hogares, "llave"), attr(todas, "llave")$hog)

  # Cuestionario de ocupacion y viviendas: se agrega la vivienda sin personas
  # en el cuestionario, y las columnas que solo tiene la vivienda llegan a todos
  suppressMessages(expect_message(
    ocupacion <- enoe(2024, 1, integrar = TRUE, tabla = c("coe1", "viv")),
    "La tabla integrada tiene 7 filas: 6 de \"coe1\" y 1 de \"viv\" sin fila en \"coe1\"[.]"
  ))
  expect_identical(ocupacion$P1, c("1", "1", "1", "2", "2", "1", NA))
  expect_identical(ocupacion$P1_VIV, c("03", "03", "02", "02", "01", "02", NA))
  expect_identical(ocupacion$MUN, c("002", "002", "015", "015", "015", "039", "039"))
  expect_identical(ocupacion$MES_CAL, c("01", "01", "02", "02", "02", "03", "01"))
  # El ponderador sin sufijo es el de la persona; la vivienda sin personas no lo tiene
  expect_identical(ocupacion$FAC_TRI, c(100L, 100L, 202L, 202L, 300L, 400L, NA))
  expect_identical(ocupacion$FAC_TRI_VIV, c(100L, 100L, 200L, 200L, 300L, 400L, 500L))
  expect_identical(attr(ocupacion, "llave"), attr(todas, "llave")$coe1)

  # Con una sola tabla no hay nada que integrar
  expect_identical(enoe_silencio(2024, 1, integrar = TRUE, tabla = "hog"), todas$hog)
})

test_that("enoe_integrar() no pierde filas y avisa cuando las tablas no corresponden", {
  sdem <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "1", "2"), V_SEL = "01", N_HOG = "1", H_MUD = "0",
                       N_REN = c("01", "02", "01"), EDA = c("30", "31", "32"), LOC = c("0001", "0001", "0002"),
                       UR = c(1L, 1L, 2L))
  hog <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "3"), V_SEL = "01", N_HOG = "1", H_MUD = "0",
                      R_DEF = c("00", "04"), UR = c("1", "2"))
  viv <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "2", "3", "4"), V_SEL = "01",
                      LOC = c("0001", "0002", "0003", "0004"), P1 = c("02", "01", NA, NA))
  # El residente de la vivienda 2 no tiene hogar en la tabla de hogares
  expect_warning(
    integrada <- suppressMessages(enoe_integrar(list(viv = viv, hog = hog, sdem = sdem))),
    "1 filas no tienen su fila en la tabla \"hog\": quedan con NA en las columnas de esa tabla"
  )
  # Tres residentes, un hogar sin residentes y una vivienda sin hogar
  expect_identical(integrada$CON, c("1", "1", "2", "3", "4"))
  expect_identical(integrada$N_REN, c("01", "02", "01", NA, NA))
  expect_identical(integrada$N_HOG, c("1", "1", "1", "1", NA))
  expect_identical(integrada$R_DEF, c("00", "00", NA, "04", NA))
  expect_identical(integrada$P1_VIV, c("02", "02", "01", NA, NA))
  # Una columna repetida que la tabla de hogares no tiene se toma de la de viviendas
  expect_identical(integrada$LOC, c("0001", "0001", "0002", "0003", "0004"))
  # y conserva el tipo que tiene en la tabla de mas detalle
  expect_identical(integrada$UR, c(1, 1, 2, 2, NA))
  expect_message(
    suppressWarnings(enoe_integrar(list(viv = viv, hog = hog, sdem = sdem))),
    "5 filas: 3 de \"sdem\" y 1 de \"hog\" sin fila en \"sdem\"; 1 de \"viv\" sin fila en \"sdem\" ni \"hog\""
  )

  # Una fila del cuestionario de ocupacion sin residente: se agrega y se avisa
  coe1 <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "9"), V_SEL = "01", N_HOG = "1", H_MUD = "0",
                       N_REN = "01", EDA = c("30", "45"), P1 = c("1", "2"))
  expect_warning(
    con_coe <- suppressMessages(enoe_integrar(list(sdem = sdem, coe1 = coe1))),
    "1 filas de la tabla \"coe1\" no tienen su residente en la tabla \"sdem\""
  )
  expect_identical(con_coe$CON, c("1", "1", "2", "9"))
  expect_identical(con_coe$P1, c("1", NA, NA, "2"))
  expect_identical(con_coe$EDA, c("30", "31", "32", "45"))

  # Llave repetida: la funcion se detiene
  expect_error(
    enoe_integrar(list(sdem = sdem, hog = rbind(hog, hog))),
    "La tabla \"hog\" tiene 2 filas con la llave repetida \\(CD_A, ENT, CON, V_SEL, N_HOG, H_MUD\\)"
  )
  expect_error(
    enoe_integrar(list(sdem = sdem[c(1, 1, 2, 3), ], hog = hog)),
    "La tabla \"sdem\" tiene 1 filas con la llave repetida \\(CD_A, ENT, CON, V_SEL, N_HOG, H_MUD, N_REN\\)"
  )
  # Tabla sin las columnas de la llave
  expect_error(
    enoe_integrar(list(sdem = sdem, hog = hog[setdiff(names(hog), "N_HOG")])),
    "La tabla \"hog\" no tiene las columnas de identificaci\u00f3n \"N_HOG\""
  )

  # Con un entorno, cada tabla se libera en cuanto se usa
  almacen <- list2env(list(sdem = sdem, viv = viv), envir = new.env(parent = emptyenv()))
  integrada <- suppressMessages(enoe_integrar(almacen))
  expect_identical(nrow(integrada), 5L)
  expect_identical(ls(almacen), character())
  # Tablas sin filas
  vacia <- enoe_integrar(list(sdem = sdem[0, ], hog = hog[0, ]))
  expect_identical(dim(vacia), c(0L, 11L))
})

test_that("enoe_integrar() mantiene los ponderadores del residente, del hogar y de la vivienda", {
  expect_identical(enoe_es_ponderador(c("FAC", "FAC_TRI", "FAC_MEN", "FACTOR", "FAC_", "P1", "R_DEF")),
                   c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  # El segundo residente de la vivienda 1 tiene un ponderador distinto del de su
  # hogar, y el hogar de la vivienda 2, uno distinto del de su vivienda. La
  # vivienda 3 tiene un hogar sin residentes.
  sdem <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "1", "2"), V_SEL = "01", N_HOG = "1", H_MUD = "0",
                       N_REN = c("01", "02", "01"), FAC = c(10L, 0L, 30L))
  hog <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "2", "3"), V_SEL = "01", N_HOG = "1", H_MUD = "0",
                      R_DEF = c("00", "00", "03"), FAC = c(10L, 31L, 50L))
  viv <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "2", "3"), V_SEL = "01", FAC = c(10.5, 30, 50))
  integrada <- suppressMessages(enoe_integrar(list(viv = viv, hog = hog, sdem = sdem)))
  expect_identical(grep("^FAC", names(integrada), value = TRUE), c("FAC", "FAC_HOG", "FAC_VIV"))
  expect_identical(integrada$N_REN, c("01", "02", "01", NA))
  # El del residente: el de la tabla sociodemografica, y NA en el hogar sin residentes
  expect_identical(integrada$FAC, c(10L, 0L, 30L, NA))
  # El del hogar y el de la vivienda llegan a todas las filas, con su tipo
  expect_identical(integrada$FAC_HOG, c(10L, 10L, 31L, 50L))
  expect_identical(integrada$FAC_VIV, c(10.5, 10.5, 30, 50))

  # Entre tablas de personas el ponderador queda una sola vez
  coe1 <- tabla_prueba(CD_A = "01", ENT = "09", CON = c("1", "2"), V_SEL = "01", N_HOG = "1", H_MUD = "0",
                       N_REN = "01", FAC = c(10L, 30L), P1 = c("1", "2"))
  personas <- enoe_integrar(list(sdem = sdem, coe1 = coe1))
  expect_identical(grep("^FAC", names(personas), value = TRUE), "FAC")
  expect_identical(personas$FAC, c(10L, 0L, 30L))
  # Sin tablas de personas no hay ponderador sin sufijo
  hogares <- enoe_integrar(list(hog = hog, viv = viv))
  expect_identical(grep("^FAC", names(hogares), value = TRUE), c("FAC_HOG", "FAC_VIV"))
  expect_identical(hogares$FAC_HOG, c(10L, 31L, 50L))
  # Con el cuestionario y la vivienda, el de la persona y el de la vivienda
  ocupacion <- suppressMessages(enoe_integrar(list(coe1 = coe1, viv = viv)))
  expect_identical(grep("^FAC", names(ocupacion), value = TRUE), c("FAC", "FAC_VIV"))
  expect_identical(ocupacion$FAC, c(10L, 30L, NA))
  expect_identical(ocupacion$FAC_VIV, c(10.5, 30, 50))

  # Desde 2020-T3 son dos ponderadores por tabla
  names(sdem)[names(sdem) == "FAC"] <- "FAC_TRI"
  names(hog)[names(hog) == "FAC"] <- "FAC_TRI"
  sdem$FAC_MEN <- sdem$FAC_TRI * 3L
  hog$FAC_MEN <- hog$FAC_TRI * 3L
  nueva <- suppressMessages(enoe_integrar(list(sdem = sdem, hog = hog)))
  expect_identical(grep("^FAC", names(nueva), value = TRUE), c("FAC_TRI", "FAC_MEN", "FAC_TRI_HOG", "FAC_MEN_HOG"))
  expect_identical(nueva$FAC_MEN, c(30L, 0L, 90L, NA))
  expect_identical(nueva$FAC_MEN_HOG, c(30L, 30L, 93L, 150L))
})

# Entidades --------------------------------------------------------------------------

test_that("`estado` conserva las filas de las entidades indicadas", {
  censo_local()
  todas <- enoe_silencio(2024, 1)
  jalisco <- enoe_silencio(2024, 1, estado = "Jalisco")
  expect_named(jalisco, names(todas))
  expect_identical(vapply(jalisco, nrow, integer(1)), c(coe1 = 1L, coe2 = 1L, hog = 2L, sdem = 2L, viv = 2L))
  for (nombre in names(jalisco)) {
    expect_identical(unique(jalisco[[nombre]]$ENT), "14", info = nombre)
    expect_identical(names(jalisco[[nombre]]), names(todas[[nombre]]), info = nombre)
    expect_identical(rownames(jalisco[[nombre]]), as.character(seq_len(nrow(jalisco[[nombre]]))), info = nombre)
  }
  expect_identical(jalisco$sdem$EDA, c("33", "05"))
  expect_identical(attr(jalisco, "llave"), attr(todas, "llave"))
  # Clave, abreviatura, varias entidades y todo el pais
  expect_identical(enoe_silencio(2024, 1, estado = 14), jalisco)
  expect_identical(enoe_silencio(2024, 1, estado = "jal"), jalisco)
  expect_identical(enoe_silencio(2024, 1, estado = c("cdmx", "Jalisco")), todas)
  expect_identical(enoe_silencio(2024, 1, estado = "nacional"), todas)
  expect_identical(nrow(enoe_silencio(2024, 1, tabla = "sdem", estado = "Ciudad de M\u00e9xico")), 6L)
  # Desde 2025-T3 la clave de la entidad es CVE_ENT
  y2026 <- enoe_silencio(2026, 2, tabla = "hog", estado = "Jalisco")
  expect_identical(y2026$CVE_ENT, c("14", "14"))
  expect_identical(y2026$CVEGEO, c("140390001", "140390001"))

  # Con integrar = TRUE: residentes y hogares sin entrevista de la entidad
  integrada <- enoe_silencio(2024, 1, integrar = TRUE, estado = "Jalisco")
  expect_identical(integrada$R_DEF, c("00", "00", "15"))
  expect_identical(integrada$N_REN, c("01", "02", NA))
  expect_identical(integrada$P1_VIV, c("02", "02", NA))
  expect_identical(names(integrada), names(enoe_silencio(2024, 1, integrar = TRUE)))

  # Una entidad sin filas en la muestra
  colima <- enoe_silencio(2024, 1, tabla = "sdem", estado = "Colima")
  expect_identical(nrow(colima), 0L)
  expect_named(colima, names(todas$sdem))
  expect_identical(dim(enoe_silencio(2024, 1, integrar = TRUE, estado = "Colima")), c(0L, 40L))
})

test_that("la clave de la entidad se reconoce como texto o como numero", {
  datos <- tabla_prueba(ENT = c(9, 14, 9), X = c("a", "b", "c"))
  expect_identical(enoe_filtrar_entidades(datos, "09")$X, c("a", "c"))
  datos$ENT <- c("9", "14", NA)
  expect_identical(enoe_filtrar_entidades(datos, c("09", "14"))$X, c("a", "b"))
  names(datos)[1] <- "CVE_ENT"
  expect_identical(enoe_filtrar_entidades(datos, "14")$X, "b")
  expect_error(enoe_filtrar_entidades(datos["X"], "14"), "La tabla no tiene la clave de la entidad \\(ENT o CVE_ENT\\)")
})

# Trimestres que no estan en el registro o en el sitio ------------------------------

test_that("un trimestre posterior al registro se busca en la direccion que usa el INEGI", {
  servidor <- censo_local()
  registro <- registro_archivos[!(registro_archivos$proyecto == "enoe" & registro_archivos$anio %in% 2026), ]
  attr(registro, "generado") <- "2026-02-15"
  withr::local_options(importinegi.registro = registro)
  expect_identical(nrow(enoe_trimestres()), 83L)
  elegido <- enoe_elegir(2026, 2)
  expect_false(elegido$registrado)
  expect_identical(elegido$url, ruta_enoe(servidor, "enoe_2026_trim2_dbf.zip"))
  suppressMessages(expect_message(
    viv <- enoe(2026, 2, tabla = "viv"),
    "El segundo trimestre de 2026 es posterior al registro de archivos del paquete \\(2026-02-15\\)"
  ))
  expect_identical(nrow(viv), 5L)
  expect_identical(attr(viv, "fuente"), ruta_enoe(servidor, "enoe_2026_trim2_dbf.zip"))
  # El servidor local no tiene 2026-T1: como cuando el INEGI todavia no lo publica
  expect_error(
    suppressMessages(enoe(2026, 1)),
    "No se pudo descargar la ENOE del primer trimestre de 2026. Es posible que el INEGI todav\u00eda no la publique.*no tiene publicado el archivo"
  )
  # 2020-T2 sigue sin existir aunque el registro termine antes
  expect_error(enoe(2020, 2), "El INEGI no public\u00f3 la ENOE del segundo trimestre de 2020")
})

test_that("enoe() avisa cuando el INEGI no tiene el archivo del registro", {
  censo_local()
  expect_error(suppressMessages(enoe(2019, 4)), "El INEGI no tiene publicado el archivo .*2019trim4_dbf.zip")
  # Sin los trimestres de la ENOE en el registro
  withr::local_options(importinegi.registro = registro_archivos[registro_archivos$proyecto != "enoe", ])
  expect_error(enoe(2024, 1), "El registro de archivos del paquete no tiene los trimestres de la ENOE")
})
