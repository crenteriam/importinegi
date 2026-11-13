# Las direcciones salen del registro real del paquete. Los archivos son las
# muestras de fixtures/censo (datos inventados con la forma de los archivos del
# INEGI; ver data-raw/muestras_censo.R), que el servidor local entrega para
# Colima (06), Nayarit (18) y el archivo nacional.

# ITER ---------------------------------------------------------------------------

test_that("censo() descarga el ITER de una entidad y devuelve solo localidades", {
  censo_local()
  expect_message(iter <- censo(2010, estado = "Colima"), "Descargando 06_colima_2010_iter_dbf.zip")
  expect_s3_class(iter, "data.frame")
  expect_identical(iter$LOC, c("0001", "0015", "0020", "0001"))
  expect_identical(iter$MUN, c("001", "001", "001", "002"))
  expect_identical(unique(iter$ENTIDAD), "06")
  expect_identical(rownames(iter), as.character(1:4))
  expect_match(attr(iter, "fuente"), "/contenidos/programas/ccpv/2010/microdatos/iter/06_colima_2010_iter_dbf\\.zip$")
})

test_that("censo() convierte los indicadores a numero y los datos reservados a NA", {
  censo_local()
  iter <- suppressMessages(censo(2010, estado = "Colima"))
  expect_type(iter$POBTOT, "integer")
  expect_identical(iter$POBTOT, c(240L, 3L, 7L, 400L))
  expect_identical(iter$POBMAS, c(119L, NA, NA, 195L))
  expect_type(iter$PROM_OCUP, "double")
  expect_identical(iter$PROM_OCUP, c(3.48, NA, NA, 3.70))
  # Las claves y los valores con ceros a la izquierda quedan como texto
  expect_type(iter$ENTIDAD, "character")
  expect_identical(iter$ALTITUD, c("0020", "0015", "0040", "0490"))
  # El texto de los DBF se entrega en UTF-8
  expect_identical(iter$NOM_LOC[1:3], c("Ciudad de Armer\u00eda", "El Para\u00edso", "Rinc\u00f3n de L\u00f3pez"))
  # La suma de las localidades es el total de la entidad: los agregados de
  # localidades de una y dos viviendas (9998 y 9999) no se cuentan dos veces
  expect_identical(sum(iter$POBTOT), 650L)
})

test_that("censo() filtra los niveles de agregacion del ITER", {
  censo_local()
  todos <- suppressMessages(censo(2010, estado = "Colima", nivel = "todos"))
  expect_identical(nrow(todos), 11L)
  expect_true(all(c("9998", "9999") %in% todos$LOC))

  entidad <- suppressMessages(censo(2010, estado = "Colima", nivel = "entidad"))
  expect_identical(entidad[, c("MUN", "LOC", "POBTOT")],
                   data.frame(MUN = "000", LOC = "0000", POBTOT = 650L))

  totales <- suppressMessages(censo(2010, estado = "Colima", nivel = c("municipio", "Entidad")))
  expect_identical(totales$MUN, c("000", "001", "002"))
  expect_identical(totales$POBTOT, c(650L, 250L, 400L))
})

test_that("censo() usa el archivo nacional por omision", {
  servidor <- censo_local()
  expect_message(iter <- censo(2010), "Descargando 00_nacional_2010_iter_dbf.zip \\(")
  expect_identical(descargas_censo(servidor), 1L)
  expect_identical(iter$ENTIDAD, c("06", "06", "06", "06", "18"))
  expect_identical(sum(iter$POBTOT), 750L)

  nacional <- suppressMessages(censo(2010, nivel = "nacional"))
  expect_identical(nacional[, c("ENTIDAD", "MUN", "LOC", "POBTOT")],
                   data.frame(ENTIDAD = "00", MUN = "000", LOC = "0000", POBTOT = 750L))
  entidades <- suppressMessages(censo(2010, estado = "nacional", nivel = "entidad"))
  expect_identical(entidades$ENTIDAD, c("06", "18"))
})

test_that("censo() combina varias entidades en una sola tabla", {
  servidor <- censo_local()
  mensajes <- testthat::capture_messages(iter <- censo(2010, estado = c("Nayarit", "col")))
  expect_match(mensajes[1], "Se descargar\u00e1n 2 archivos del INEGI \\([0-9.]+ [KM]B en total\\)")
  expect_length(mensajes, 3)
  expect_identical(descargas_censo(servidor), 2L)
  expect_identical(iter$ENTIDAD, c("18", "06", "06", "06", "06"))
  expect_length(attr(iter, "fuente"), 2)
  # Una entidad repetida se descarga una sola vez
  repetida <- suppressMessages(censo(2010, estado = c("Colima", "06", 6)))
  expect_identical(nrow(repetida), 4L)
  expect_identical(descargas_censo(servidor), 3L)
})

test_that("censo() acepta el a\u00f1o como texto y otros censos", {
  censo_local()
  iter <- suppressMessages(censo("2000", estado = 18))
  expect_identical(iter$ENTIDAD, "18")
  expect_match(attr(iter, "fuente"), "/ccpv/2000/microdatos/iter/18_nayarit_2000_iter_dbf\\.zip$")
  iter <- suppressMessages(censo(1990, base = "ITER", estado = "NAY"))
  expect_match(attr(iter, "fuente"), "/ccpv/1990/microdatos/iter/18_nayarit_1990_iter_dbf\\.zip$")
})

# AGEB y manzana -----------------------------------------------------------------

test_that("censo() descarga los resultados por AGEB y manzana", {
  censo_local()
  manzanas <- suppressMessages(censo(2010, "ageb", estado = "Colima"))
  expect_identical(manzanas$AGEB, c("0012", "0012", "003A"))
  expect_identical(manzanas$MZA, c("001", "002", "001"))
  expect_identical(manzanas$POBTOT, c(150L, 100L, 150L))
  expect_identical(manzanas$POBMAS, c(72L, NA, 75L))
  # La ruta de Colima en el registro del INEGI trae una diagonal doble
  expect_match(attr(manzanas, "fuente"), "ageb_manzana//06_colima_2010_ageb_manzana_urbana_dbf\\.zip$")

  agebs <- suppressMessages(censo(2010, "ageb", estado = "Colima", nivel = "ageb"))
  expect_identical(agebs$AGEB, c("0012", "003A"))
  expect_identical(agebs$POBTOT, c(250L, 150L))
  niveles <- suppressMessages(censo(2010, "ageb", estado = "Colima", nivel = c("entidad", "municipio", "localidad")))
  expect_identical(niveles$POBTOT, c(640L, 400L, 400L))
  expect_identical(nrow(suppressMessages(censo(2010, "ageb", estado = "Colima", nivel = "todos"))), 8L)

  dos <- suppressMessages(censo(2010, "ageb", estado = c("Colima", "Nayarit")))
  expect_identical(dos$ENTIDAD, c("06", "06", "06", "18"))
  expect_identical(dos$AGEB[4], "0105")
})

test_that("censo() explica que el AGEB no tiene archivo nacional", {
  censo_local()
  expect_error(
    censo(2010, "ageb"),
    "no publica un archivo nacional de los resultados por AGEB y manzana urbana de 2010.*estado = \"todas\".*32 entidades \\([0-9]+ MB de descarga\\)"
  )
})

# Muestra ------------------------------------------------------------------------

test_that("censo() descarga cada tabla de la muestra de 2010", {
  censo_local()
  personas <- suppressMessages(censo(2010, "muestra", estado = "Colima"))
  expect_named(personas, c("ENT", "MUN", "ID_VIV", "NUMPER", "SEXO", "EDAD", "ESCOLARI", "FACTOR"))
  expect_identical(nrow(personas), 3L)
  expect_identical(personas$ENT, rep("06", 3))
  expect_identical(personas$MUN, c("001", "002", "003"))
  # Codigos con ceros a la izquierda quedan como texto; el resto, como numero
  expect_identical(personas$ESCOLARI, c("09", "02", "00"))
  expect_identical(personas$ID_VIV[1], "06000000001")
  expect_identical(personas$SEXO, c(1L, 3L, 1L))
  expect_identical(personas$FACTOR, c(12L, 12L, 9L))
  expect_identical(suppressMessages(censo(2010, "muestra", estado = "Colima", tabla = "Personas")), personas)

  viviendas <- suppressMessages(censo(2010, "muestra", estado = "Colima", tabla = "viviendas"))
  expect_named(viviendas, c("ENT", "MUN", "ID_VIV", "PISOS", "NUMPERS", "FACTOR"))
  expect_identical(nrow(viviendas), 2L)
  migrantes <- suppressMessages(censo(2010, "muestra", estado = "Colima", tabla = "migrantes"))
  expect_identical(migrantes$MPAIS, 221L)

  dos <- suppressMessages(censo(2010, "muestra", estado = c("Colima", "Nayarit"), tabla = "viviendas"))
  expect_identical(dos$ENT, c("06", "06", "18"))
})

test_that("censo() reconoce las tablas de la muestra de 2000 y la tabla unica de 1990", {
  censo_local()
  personas <- suppressMessages(censo(2000, "muestra", estado = "Colima"))
  expect_identical(nrow(personas), 3L)
  expect_true("EDAD" %in% names(personas))
  viviendas <- suppressMessages(censo(2000, "muestra", estado = "Colima", tabla = "viviendas"))
  expect_true("PISOS" %in% names(viviendas))
  migrantes <- suppressMessages(censo(2000, "muestra", estado = "Colima", tabla = "migrantes"))
  expect_true("MPAIS" %in% names(migrantes))

  muestra <- suppressMessages(censo(1990, "muestra", estado = "Colima"))
  expect_identical(nrow(muestra), 3L)
  # Las claves que llegan como numero se completan con ceros
  expect_identical(muestra$ENT, rep("06", 3))
  expect_identical(muestra$MUN, c("001", "001", "010"))
  expect_error(censo(1990, "muestra", estado = "Colima", tabla = "personas"),
               "La muestra de 1990 se publica en una sola tabla")
})

test_that("censo() valida los argumentos de la muestra", {
  censo_local()
  expect_error(censo(2010, "muestra"), "no publica un archivo nacional de la muestra")
  expect_error(censo(2010, "muestra", estado = "Colima", tabla = "hogares"),
               "`tabla = \"hogares\"` no es una opci\u00f3n v\u00e1lida.*\"personas\", \"viviendas\", \"migrantes\"")
  expect_error(censo(2010, "muestra", estado = "Colima", tabla = c("personas", "viviendas")),
               "`tabla` debe ser una sola")
  expect_error(censo(2010, "muestra", estado = "Colima", nivel = "municipio"),
               "`nivel` solo se usa con las bases \"iter\" y \"ageb\"")
  expect_error(censo(2010, "iter", estado = "Colima", tabla = "personas"),
               "`tabla` solo se usa con base = \"muestra\"")
})

# Localidades y entorno urbano ---------------------------------------------------

test_that("censo() descarga los resultados sobre localidades", {
  censo_local()
  nacional <- suppressMessages(censo(2010, "localidades"))
  expect_identical(nacional$ENTIDAD, c("06", "06", "18"))
  expect_identical(nacional$NOM_LOC[3], "El Resbal\u00f3n")
  expect_identical(nacional$DIST_CAB, c(12L, NA, 8L))
  expect_identical(nacional$TIENDA, c(2L, NA, 1L))
  expect_match(attr(nacional, "fuente"), "cinco_mil_menos/resloc_nal_2010_dbf\\.zip$")

  # El archivo de Colima declara la pagina de codigos 850
  colima <- suppressMessages(censo(2010, "localidades", estado = "Colima"))
  expect_identical(colima$NOM_LOC, c("El Para\u00edso", "Rinc\u00f3n de L\u00f3pez"))
  expect_identical(colima$LOC, c("0015", "0020"))
})

test_that("censo() descarga los resultados sobre el entorno urbano", {
  censo_local()
  entorno <- suppressMessages(censo(2010, "entorno", estado = "Colima"))
  expect_named(entorno, c("ENT", "MUN", "LOC", "AGEB", "MZA", "CONJHAB", "RECUCALL_", "ALUMPUB_"))
  expect_identical(entorno$AGEB, c("0012", "0012", "003A"))
  expect_identical(entorno$MZA, c("001", "002", "001"))
  expect_identical(entorno$CONJHAB, c(3L, 3L, 1L))
  expect_error(censo(2010, "entorno"), "no publica un archivo nacional")
})

# Validacion y errores -----------------------------------------------------------

test_that("censo() exige el a\u00f1o y distingue censos, conteos y encuestas", {
  expect_error(censo(), "Falta indicar el a\u00f1o en `year`.*1990, 2000, 2010, 2020")
  expect_error(censo(NULL), "Falta indicar el a\u00f1o")
  expect_error(censo(c(2000, 2010)), "Falta indicar el a\u00f1o en `year` \\(un solo valor\\)")
  expect_error(censo(1995), "`year = 1995` corresponde al Conteo de Poblaci\u00f3n y Vivienda, no al Censo")
  expect_error(censo(2005), "corresponde al Conteo")
  expect_error(censo(2015), "corresponde a la Encuesta Intercensal")
  expect_error(censo(1980), "`year = 1980` no est\u00e1 disponible")
  expect_error(censo("dos mil"), "no es un a\u00f1o")
  expect_error(censo(2010.5), "no es un a\u00f1o")
})

test_that("censo() valida la base y dice para que a\u00f1os existe", {
  expect_error(censo(2000, "ageb"), "La base \"ageb\" no existe para 2000.*2010.*Bases de 2000: \"iter\", \"muestra\"")
  expect_error(censo(1990, "entorno"), "La base \"entorno\" no existe para 1990")
  expect_error(censo(2010, "enoe"), "`base = \"enoe\"` no es una opci\u00f3n v\u00e1lida")
  expect_error(censo(2010, c("iter", "ageb")), "`base` debe ser el nombre de una base")
  expect_error(censo(2010, NA), "`base` debe ser el nombre de una base")
})

test_that("censo() valida la entidad y el nivel antes de descargar", {
  servidor <- censo_local()
  expect_error(censo(2010, estado = "Colimaa"), "No se reconoce la entidad.*Colima")
  expect_error(censo(2010, estado = 40), "no es una clave de entidad v\u00e1lida")
  expect_error(censo(2010, estado = c("nacional", "Colima")), "no se combina con otras entidades")
  expect_error(censo(2010, estado = NULL), "Falta indicar `estado`")
  expect_error(censo(2010, estado = "Colima", nivel = "nacional"), "solo est\u00e1 en el archivo nacional")
  expect_error(censo(2010, estado = "Colima", nivel = "manzana"), "no es una opci\u00f3n v\u00e1lida para la base \"iter\"")
  expect_error(censo(2010, estado = "Colima", nivel = c("todos", "entidad")), "no es una opci\u00f3n v\u00e1lida")
  expect_error(censo(2010, estado = "Colima", nivel = 1), "`nivel` debe ser")
  expect_identical(descargas_censo(servidor), 0L)
})

test_that("censo() avisa cuando el INEGI no tiene el archivo o cambio su contenido", {
  censo_local()
  # Zacatecas: el servidor responde con la pagina de "no encontrado"
  expect_error(suppressMessages(censo(2010, estado = "Zacatecas")),
               "El INEGI no tiene publicado el archivo .*32_zacatecas_2010_iter_dbf\\.zip")
  # Aguascalientes: el zip trae dos tablas donde se espera una
  expect_error(suppressMessages(censo(2010, estado = "Aguascalientes")),
               "m\u00e1s de un archivo para la tabla de datos.*ITER_01DBF10")
  # Si una entidad falla, no se devuelve una tabla incompleta
  expect_error(suppressMessages(censo(2010, estado = c("Colima", "Zacatecas"))),
               "El INEGI no tiene publicado el archivo")
})

test_that("censo() no deja archivos temporales y respeta la copia local", {
  censo_local()
  antes <- list.files(tempdir(), pattern = "^importinegi_")
  suppressMessages(censo(2010, estado = "Colima"))
  expect_identical(list.files(tempdir(), pattern = "^importinegi_"), antes)

  servidor <- censo_local()
  copia <- withr::local_tempdir()
  withr::local_options(importinegi.cache = TRUE, importinegi.cache_dir = copia)
  primera <- suppressMessages(censo(2010, estado = "Colima"))
  expect_message(segunda <- censo(2010, estado = "Colima"), "Usando la copia local")
  expect_identical(primera, segunda)
  expect_identical(descargas_censo(servidor), 1L)
  expect_true(file.exists(file.path(copia, "contenidos/programas/ccpv/2010/microdatos/iter/06_colima_2010_iter_dbf.zip")))
})

# Registro -----------------------------------------------------------------------

test_that("las reglas de cada base encuentran sus archivos en el registro del paquete", {
  esperado <- list(
    c(1990, "iter", 33), c(1990, "muestra", 32),
    c(2000, "iter", 33), c(2000, "muestra", 32),
    c(2010, "iter", 33), c(2010, "ageb", 32), c(2010, "muestra", 32),
    c(2010, "localidades", 33), c(2010, "entorno", 32)
  )
  for (caso in esperado) {
    archivos <- poblacion_archivos("censo", as.integer(caso[1]), caso[2])
    etiqueta <- paste(caso[1], caso[2])
    expect_identical(nrow(archivos), as.integer(caso[3]), info = etiqueta)
    expect_identical(setdiff(sprintf("%02d", 1:32), archivos$clave), character(), info = etiqueta)
    expect_identical("00" %in% archivos$clave, caso[3] == "33", info = etiqueta)
    expect_identical(anyDuplicated(archivos$url), 0L, info = etiqueta)
    expect_identical(unique(archivos$formato), "dbf", info = etiqueta)
    expect_true(all(grepl(paste0("/contenidos/programas/ccpv/", caso[1], "/microdatos/.+_dbf\\.zip$"), archivos$url)),
                info = etiqueta)
  }
  # Las bases de ejemplo del cuestionario basico (datos ficticios) no entran
  todas <- unlist(lapply(esperado, function(caso) poblacion_archivos("censo", as.integer(caso[1]), caso[2])$url))
  expect_false(any(grepl("ejemplobd|dummy", todas)))
})

test_that("censo() explica que el registro todavia no incluye el Censo 2020", {
  expect_error(censo(2020), "El registro de archivos de esta versi\u00f3n del paquete \\(generado el 2026-11-11\\) no incluye los principales resultados por localidad \\(ITER\\) de 2020")
  expect_error(censo(2020, "muestra", estado = "Colima"), "no incluye la muestra \\(cuestionario ampliado\\) de 2020")
})

test_that("censo() lee archivos CSV de datos abiertos cuando el registro los incluye", {
  # Registro de prueba con una fila para el ITER 2020 de Colima. La forma del
  # archivo (carpetas conjunto_de_datos, diccionario_de_datos, catalogos y
  # metadatos) es la que describen otros usuarios; se confirmara con el INEGI.
  censo_local()
  registro <- registro_archivos[0, ]
  registro[1:2, ] <- NA
  registro$proyecto <- "ccpv"
  registro$edicion <- 2020L
  registro$titulo <- "Bases de datos|Colima"
  registro$descripcion <- "Principales resultados por localidad (ITER)"
  registro$formato <- c("csv", "xlsx")
  registro$sufijo <- c("_csv.zip", ".xlsx")
  registro$tamano <- "1 KB"
  registro$bytes <- 1024
  registro$ruta <- "/programas/ccpv/2020/datosabiertos/iter/iter_06_cpv2020"
  attr(registro, "generado") <- "2099-01-01"
  withr::local_options(importinegi.registro = registro)

  iter <- suppressMessages(censo(2020, estado = "Colima"))
  expect_match(attr(iter, "fuente"), "/ccpv/2020/datosabiertos/iter/iter_06_cpv2020_csv\\.zip$")
  expect_named(iter, c("ENTIDAD", "NOM_ENT", "MUN", "NOM_MUN", "LOC", "NOM_LOC", "LONGITUD",
                       "ALTITUD", "POBTOT", "POBMAS", "PROM_OCUP", "VIVTOT"))
  expect_identical(iter$LOC, c("0001", "0015", "0020", "0001"))
  expect_identical(iter$POBTOT, c(240L, 3L, 7L, 400L))
  expect_identical(iter$POBMAS, c(119L, NA, NA, 195L))
  expect_identical(iter$NOM_LOC[1], "Ciudad de Armer\u00eda")
  expect_error(censo(2020, estado = "Nayarit"), "El registro del paquete no tiene el archivo de .* para: \"Nayarit\"")
  expect_error(censo(2020), "no publica un archivo nacional")
  # "todas" descarga las entidades que tiene el registro
  expect_identical(suppressMessages(censo(2020, estado = "todas")), iter)
})
