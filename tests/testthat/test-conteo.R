# Las direcciones salen del registro real del paquete. Los archivos son las
# muestras de fixtures/conteo (datos inventados con la forma de los archivos del
# INEGI; ver data-raw/muestras_conteo.R), que el servidor local entrega para
# Aguascalientes (01), Colima (06) y el archivo nacional.

# Registro del paquete sin las entidades de una base que no esten en `claves`:
# sirve para probar estado = "nacional" con las entidades del servidor local.
registro_reducido <- function(programa, year, base, claves) {
  registro <- registro_archivos
  archivos <- poblacion_archivos(programa, year, base)
  quitar <- archivos$url[!archivos$clave %in% claves]
  registro <- registro[!registro_url(registro) %in% quitar, ]
  attr(registro, "generado") <- registro_generado()
  registro
}

# ITER ---------------------------------------------------------------------------

test_that("conteo() descarga el ITER nacional por omision y devuelve solo localidades", {
  servidor <- censo_local()
  expect_message(iter <- conteo(1995), "Descargando 00_nacional_1995_iter_dbf.zip \\(")
  expect_identical(descargas_censo(servidor), 1L)
  expect_identical(iter$ENTIDAD, c("01", "06", "06", "06"))
  expect_identical(iter$LOC, c("0001", "0001", "0010", "0001"))
  expect_identical(iter$POBTOT, c(500L, 296L, 4L, 700L))
  # Los agregados 9998 no se cuentan dos veces: las localidades suman el total
  expect_identical(sum(iter$POBTOT), 1500L)
  expect_match(attr(iter, "fuente"), "/contenidos/programas/ccpv/1995/microdatos/iter/00_nacional_1995_iter_dbf\\.zip$")

  nacional <- suppressMessages(conteo(1995, nivel = "nacional"))
  expect_identical(nacional[, c("ENTIDAD", "MUN", "LOC", "POBTOT")],
                   data.frame(ENTIDAD = "00", MUN = "000", LOC = "0000", POBTOT = 1500L))
})

test_that("conteo() descarga el ITER de una entidad y filtra sus niveles", {
  censo_local()
  colima <- suppressMessages(conteo(2005, estado = "Colima"))
  expect_match(attr(colima, "fuente"), "/ccpv/2005/microdatos/iter/06_colima_2005_iter_dbf\\.zip$")
  expect_identical(colima$MUN, c("001", "001", "002"))
  expect_identical(colima$POBMAS, c(148L, NA, 340L))
  expect_identical(colima$ALTITUD, c(20L, 50L, 490L))
  expect_identical(colima$LONGITUD, c("1035752", "1035910", "1034330"))
  expect_identical(colima$NOM_LOC[2], "Cofradía de Juárez")

  totales <- suppressMessages(conteo(2005, estado = "col", nivel = c("entidad", "municipio")))
  expect_identical(totales$POBTOT, c(1000L, 300L, 700L))
  todos <- suppressMessages(conteo(2005, estado = 6, nivel = "todos"))
  expect_identical(nrow(todos), 8L)
  expect_identical(sum(todos$LOC == "9998"), 2L)

  aguascalientes <- suppressMessages(conteo("1995", estado = "Aguascalientes"))
  expect_match(attr(aguascalientes, "fuente"), "/ccpv/1995/microdatos/iter/01_aguascalientes_1995_iter_dbf\\.zip$")
  expect_identical(aguascalientes$POBTOT, 500L)
})

# Muestra ------------------------------------------------------------------------

test_that("conteo() descarga las dos tablas de la muestra de 1995", {
  censo_local()
  personas <- suppressMessages(conteo(1995, "muestra", estado = "Colima"))
  expect_match(attr(personas, "fuente"), "/ccpv/1995/microdatos/cpv95_06_dbf\\.zip$")
  expect_named(personas, c("ENT", "MUN", "LOC", "NUMVIV", "NUMPER", "SEXO", "EDAD", "OCUPA", "FACTOR"))
  expect_identical(personas$ENT, rep("06", 3))
  # En la muestra, las respuestas con ceros a la izquierda pasan a numero; el
  # numero de vivienda se conserva como texto
  expect_identical(personas$EDAD, c(34L, 8L, 61L))
  expect_identical(personas$OCUPA, c(123L, 201L, NA))
  expect_identical(personas$NUMVIV, c("00001", "00002", "00003"))
  expect_identical(personas$FACTOR, c(25L, 25L, 31L))
  expect_identical(suppressMessages(conteo(1995, "muestra", estado = "Colima", tabla = "personas")), personas)

  migrantes <- suppressMessages(conteo(1995, "muestra", estado = "Colima", tabla = "migrantes"))
  expect_identical(migrantes$PAIS, 221L)
  expect_identical(nrow(migrantes), 1L)
})

test_that("conteo() descarga las tres tablas de la muestra de 2005, dentro de su carpeta", {
  censo_local()
  personas <- suppressMessages(conteo(2005, "muestra", estado = "Colima"))
  expect_match(attr(personas, "fuente"), "/ccpv/2005/microdatos/muestra/cpv2005_06_dbf\\.zip$")
  expect_identical(nrow(personas), 3L)
  expect_identical(personas$EDAD, c(27L, 25L, 3L))
  expect_identical(personas$FACTOR, c(10L, 10L, 11L))
  viviendas <- suppressMessages(conteo(2005, "muestra", estado = "Colima", tabla = "viviendas"))
  expect_named(viviendas, c("ENT", "MUN", "LOC", "NUMVIV", "PISOS", "TOTHOG", "FACTOR"))
  hogares <- suppressMessages(conteo(2005, "muestra", estado = "Colima", tabla = "Hogares"))
  expect_named(hogares, c("ENT", "MUN", "LOC", "NUMVIV", "NUMHOG", "TOTPER", "FACTOR"))
  expect_identical(hogares$TOTPER, 3L)
})

test_that("conteo() descarga Aguascalientes por omision en la muestra y lo avisa", {
  servidor <- censo_local()
  mensajes <- testthat::capture_messages(personas <- conteo(1995, "muestra"))
  expect_match(
    mensajes[1],
    "No indicaste `estado`: se descarga la muestra de Aguascalientes. Para todo el país usa estado = \"nacional\" \\(32 archivos, 8.1 MB en total\\)."
  )
  expect_identical(descargas_censo(servidor), 1L)
  expect_identical(personas$ENT, c("01", "01"))
  expect_match(attr(personas, "fuente"), "/cpv95_01_dbf\\.zip$")
  # OCUPA trae un codigo con letra, asi que se conserva como texto
  expect_identical(personas$OCUPA, c("A10", "0410"))

  mensajes <- testthat::capture_messages(conteo(2005, "muestra", tabla = "hogares"))
  expect_match(mensajes[1], "32 archivos, 121 MB en total")
  # En las bases con archivo nacional no hay aviso
  mensajes <- testthat::capture_messages(conteo(2005, "viviendas"))
  expect_false(any(grepl("No indicaste", mensajes)))
})

test_that("conteo() combina las entidades de la muestra con estado = \"nacional\"", {
  servidor <- censo_local()
  withr::local_options(importinegi.registro = registro_reducido("conteo", 1995L, "muestra", c("01", "06")))
  mensajes <- testthat::capture_messages(nacional <- conteo(1995, "muestra", estado = "nacional"))
  expect_match(mensajes[1], "Se descargarán 2 archivos del INEGI")
  expect_identical(descargas_censo(servidor), 2L)
  expect_identical(nacional$ENT, c("01", "01", "06", "06", "06"))
  expect_identical(nacional$EDAD, c(45L, 12L, 34L, 8L, 61L))
  # Que columnas pasan a numero se decide con todas las entidades: OCUPA es
  # texto en Aguascalientes, asi que en Colima conserva sus ceros
  expect_identical(nacional$OCUPA, c("A10", "0410", "0123", "0201", NA))
  expect_length(attr(nacional, "fuente"), 2)
  expect_identical(suppressMessages(conteo(1995, "muestra", estado = c("ags", "Colima"))), nacional)
})

# Viviendas ----------------------------------------------------------------------

test_that("conteo() descarga las viviendas habitadas y deshabitadas de 2005", {
  censo_local()
  nacional <- suppressMessages(conteo(2005, "viviendas"))
  expect_match(attr(nacional, "fuente"), "/ccpv/2005/microdatos/viviendas/vivloc2005_nal_dbf\\.zip$")
  expect_identical(nacional$ENTIDAD, c("01", "06", "06", "06"))
  expect_identical(nacional$VIVHAB, c(120L, 74L, 1L, 175L))
  expect_identical(nacional$VIVDES, c(10L, 6L, NA, 15L))
  colima <- suppressMessages(conteo(2005, "viviendas", estado = "Colima"))
  expect_identical(colima$NOM_LOC, c("Ciudad de Armería", "Cofradía de Juárez", "Colima"))
})

# Validacion ---------------------------------------------------------------------

test_that("conteo() valida el año, la base, la tabla y el nivel", {
  servidor <- censo_local()
  expect_error(conteo(), "Falta indicar el año en `year`.*1995, 2005")
  expect_error(conteo(2010), "`year = 2010` corresponde al Censo de Población y Vivienda, no al Conteo de Población y Vivienda: usa censo\\(2010\\)")
  expect_error(conteo(2015), "corresponde a la Encuesta Intercensal, no al Conteo")
  expect_error(conteo(2000), "usa censo\\(2000\\)")
  expect_error(conteo(1995, "viviendas"), "La base \"viviendas\" no existe para 1995: el INEGI la publica para 2005")
  expect_error(conteo(1995, "ageb"), "`base = \"ageb\"` no es una opción válida. Bases de 1995: \"iter\", \"muestra\"")
  expect_error(conteo(1995, "muestra", estado = "Colima", tabla = "hogares"),
               "`tabla = \"hogares\"` no es una opción válida.*\"personas\", \"migrantes\"")
  expect_error(conteo(2005, "muestra", estado = "Colima", tabla = "migrantes"),
               "\"personas\", \"viviendas\", \"hogares\"")
  expect_error(conteo(2005, "viviendas", tabla = "personas"),
               "`tabla` solo se usa con la base \"muestra\"; la base \"viviendas\" tiene una sola tabla")
  expect_error(conteo(2005, "muestra", estado = "Colima", nivel = "municipio"),
               "`nivel` solo se usa con la base \"iter\"; la base \"muestra\" no mezcla totales")
  expect_error(conteo(1995, estado = "todas"), "No se reconoce la entidad")
  expect_error(conteo(1995, "muestra", estado = c("nacional", "Colima")), "no se combina con otras entidades")
  expect_identical(descargas_censo(servidor), 0L)
})

test_that("censo() remite a conteo() en los años de los conteos", {
  expect_error(censo(1995), "`year = 1995` corresponde al Conteo de Población y Vivienda, no al Censo de Población y Vivienda: usa conteo\\(1995\\)")
  expect_error(censo(2005), "usa conteo\\(2005\\)")
})

# Registro -----------------------------------------------------------------------

test_that("las reglas del conteo encuentran sus archivos en el registro del paquete", {
  esperado <- list(
    c(1995, "iter", 33), c(1995, "muestra", 32),
    c(2005, "iter", 33), c(2005, "muestra", 32), c(2005, "viviendas", 33)
  )
  for (caso in esperado) {
    archivos <- poblacion_archivos("conteo", as.integer(caso[1]), caso[2])
    etiqueta <- paste(caso[1], caso[2])
    expect_identical(nrow(archivos), as.integer(caso[3]), info = etiqueta)
    expect_identical(setdiff(sprintf("%02d", 1:32), archivos$clave), character(), info = etiqueta)
    expect_identical("00" %in% archivos$clave, caso[3] == "33", info = etiqueta)
    expect_identical(anyDuplicated(archivos$url), 0L, info = etiqueta)
    expect_identical(unique(archivos$formato), "dbf", info = etiqueta)
    expect_true(all(grepl(paste0("/contenidos/programas/ccpv/", caso[1], "/microdatos/.+_dbf\\.zip$"), archivos$url)),
                info = etiqueta)
  }
  # Cada base encuentra solo sus archivos
  expect_true(all(grepl("/microdatos/cpv95_", poblacion_archivos("conteo", 1995L, "muestra")$url)))
  expect_true(all(grepl("/microdatos/muestra/cpv2005_", poblacion_archivos("conteo", 2005L, "muestra")$url)))
  expect_true(all(grepl("/microdatos/viviendas/vivloc2005_", poblacion_archivos("conteo", 2005L, "viviendas")$url)))
  expect_true(all(grepl("/microdatos/iter/", poblacion_archivos("conteo", 2005L, "iter")$url)))
})
