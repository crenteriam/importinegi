# Las direcciones salen del registro real del paquete. Los archivos son las
# muestras de fixtures/intercensal (datos inventados con la forma de los
# archivos del INEGI; ver data-raw/muestras_intercensal.R), que el servidor
# local entrega para Aguascalientes (01), Colima (06), el archivo nacional de la
# muestra de 2025 y el XLSX de los resultados de 2025.

# Registro -----------------------------------------------------------------------

test_that("el registro tiene la muestra y los resultados de la Encuesta Intercensal", {
  muestra15 <- poblacion_archivos("intercensal", 2015L, "muestra")
  expect_identical(muestra15$clave, sprintf("%02d", 1:32))
  expect_identical(unique(muestra15$formato), "csv")
  # En 2015 el titulo es solo el nombre de la entidad; el del Estado de Mexico
  # trae espacios de no separacion
  expect_match(muestra15$url[muestra15$clave == "15"], "/intercensal/2015/microdatos/eic2015_15_csv\\.zip$")
  expect_equal(sum(muestra15$bytes) / 1024^2, 814, tolerance = 0.001)

  muestra25 <- poblacion_archivos("intercensal", 2025L, "muestra")
  expect_identical(muestra25$clave, sprintf("%02d", 0:32))
  expect_match(muestra25$url[1], "/eic/2025/microdatos/eic2025_micro_00_csv\\.zip$")
  expect_equal(muestra25$bytes[1] / 1024^2, 727)

  resultados <- poblacion_archivos("intercensal", 2025L, "resultados")
  expect_identical(resultados$clave, "00")
  expect_identical(resultados$formato, "xlsx")
  expect_match(resultados$url, "/eic/2025/microdatos/eic2025_micro_105\\.xlsx$")
  # La documentacion (clasificaciones en CSV, descriptores) no se toma por datos
  expect_false(any(grepl("889463931966|_fd", c(muestra15$url, muestra25$url, resultados$url))))
})

# Muestra de 2015 -----------------------------------------------------------------

test_that("intercensal() descarga Aguascalientes por omision en 2015 y lo avisa", {
  servidor <- censo_local()
  mensajes <- testthat::capture_messages(personas <- intercensal(2015))
  expect_match(
    mensajes[1],
    "No indicaste `estado`: se descarga la muestra de Aguascalientes. Para todo el país usa estado = \"nacional\" \\(32 archivos, 814 MB en total\\)."
  )
  expect_identical(descargas_censo(servidor), 1L)
  expect_match(attr(personas, "fuente"), "/intercensal/2015/microdatos/eic2015_01_csv\\.zip$")
  expect_named(personas, c("ID_VIV", "ID_PERSONA", "ENT", "NOM_ENT", "MUN", "NOM_MUN", "LOC50K", "NOM_LOC",
                           "COBERTURA", "ESTRATO", "UPM", "NUMPER", "SEXO", "EDAD", "INGTRMEN", "TAMLOC", "FACTOR"))
  # Claves, identificadores y variables del diseño muestral como texto; las
  # respuestas, incluso con ceros a la izquierda, como numero
  expect_identical(personas$ENT, rep("01", 3))
  expect_identical(personas$MUN, rep("001", 3))
  expect_identical(personas$LOC50K, c("0001", "0001", NA))
  expect_identical(personas$ID_VIV, c("010010000101", "010010000101", "010010000102"))
  expect_identical(personas$ID_PERSONA[3], "01001000010203")
  expect_identical(personas$UPM, c("0000120", "0000120", "0000121"))
  expect_identical(personas$ESTRATO, rep("01-1-03", 3))
  expect_identical(personas$EDAD, c(34L, 8L, 0L))
  expect_identical(personas$INGTRMEN, c(8500L, 0L, NA))
  expect_identical(personas$NUMPER, 1:3)
  expect_identical(personas$COBERTURA, rep(2L, 3))
  expect_identical(personas$FACTOR, c(15L, 15L, 22L))
  expect_identical(suppressMessages(intercensal(2015, tabla = "personas")), personas)

  viviendas <- suppressMessages(intercensal(2015, tabla = "Viviendas"))
  expect_identical(viviendas$ID_VIV, c("010010000101", "010010000102"))
  expect_identical(viviendas$NUMPERS, c(2L, 1L))
  expect_identical(viviendas$INGTRHOG, c(12000L, 24000L))
  expect_true(all(personas$ID_VIV %in% viviendas$ID_VIV))
})

test_that("intercensal() lee la muestra de 2015 de otra entidad, con acentos", {
  censo_local()
  mensajes <- testthat::capture_messages(colima <- intercensal(2015, estado = "Colima"))
  expect_false(any(grepl("No indicaste", mensajes)))
  expect_identical(colima$NOM_MUN, rep("Villa de Álvarez", 2))
  expect_identical(colima$EDAD, c(41L, 39L))
  expect_identical(suppressMessages(intercensal(2015, estado = 6)), colima)
})

test_that("intercensal() combina las entidades de la muestra de 2015 con estado = 'nacional'", {
  censo_local()
  withr::local_options(importinegi.registro = registro_reducido("intercensal", 2015L, "muestra", c("01", "06")))
  mensajes <- testthat::capture_messages(nacional <- intercensal(2015, estado = "nacional"))
  expect_true(any(grepl("Se descargarán 2 archivos del INEGI", mensajes)))
  expect_false(any(grepl("memoria", mensajes)))
  expect_identical(nacional$ENT, c("01", "01", "01", "06", "06"))
  expect_identical(nacional$EDAD, c(34L, 8L, 0L, 41L, 39L))
  expect_length(attr(nacional, "fuente"), 2L)
  varias <- suppressMessages(intercensal(2015, estado = c("Colima", "Aguascalientes")))
  expect_identical(varias$ENT, c("06", "06", "01", "01", "01"))
})

test_that("intercensal() avisa de la memoria en las muestras de mas de 500 MB", {
  censo_local()
  registro <- registro_reducido("intercensal", 2015L, "muestra", c("01", "06"))
  grandes <- registro$proyecto == "eic" & registro$edicion %in% 2015L & registro$formato == "csv"
  registro$bytes[grandes] <- 400 * 1024^2
  withr::local_options(importinegi.registro = registro)
  mensajes <- testthat::capture_messages(intercensal(2015, estado = "nacional"))
  expect_true(any(grepl(
    "Leer esta muestra \\(800 MB comprimidos\\) puede requerir más memoria de la que tienen muchas computadoras. Si solo necesitas algunas entidades, indícalas en `estado`.",
    mensajes
  )))
  # Una sola entidad no llega al umbral
  mensajes <- testthat::capture_messages(intercensal(2015, estado = "Colima"))
  expect_false(any(grepl("memoria", mensajes)))
})

# Muestra de 2025 -----------------------------------------------------------------

test_that("intercensal() descarga las tres tablas de la muestra de 2025", {
  servidor <- censo_local()
  mensajes <- testthat::capture_messages(personas <- intercensal(2025))
  expect_match(
    mensajes[1],
    "se descarga la muestra de Aguascalientes. Para todo el país usa estado = \"nacional\" \\(archivo nacional de 727 MB\\)."
  )
  expect_identical(descargas_censo(servidor), 1L)
  expect_match(attr(personas, "fuente"), "/eic/2025/microdatos/eic2025_micro_01_csv\\.zip$")
  # El zip trae ademas un diccionario de personas, que se ignora
  expect_false("NOMBRE_CAMPO" %in% names(personas))
  expect_identical(personas$FACTOR, c(12L, 12L, 17L))
  expect_identical(personas$EDAD, c(34L, 8L, 0L))

  viviendas <- suppressMessages(intercensal(2025, tabla = "viviendas"))
  expect_identical(viviendas$ID_VIV, c("010010000101", "010010000102"))
  migrantes <- suppressMessages(intercensal(2025, estado = "Colima", tabla = "migrantes"))
  expect_identical(migrantes$ENT, "06")
  expect_identical(migrantes$MEDAD, 27L)
  expect_identical(migrantes$MPAIS, 221L)
  expect_identical(migrantes$UPM, "0000120")
})

test_that("intercensal() descarga el archivo nacional de la muestra de 2025 con estado = 'nacional'", {
  servidor <- censo_local()
  mensajes <- testthat::capture_messages(nacional <- intercensal(2025, estado = "nacional", tabla = "viviendas"))
  expect_identical(descargas_censo(servidor), 1L)
  expect_match(attr(nacional, "fuente"), "/eic2025_micro_00_csv\\.zip$")
  expect_identical(nacional$ENT, c("01", "01", "06"))
  # Un solo archivo de 727 MB: no hay aviso de varios archivos, si de memoria
  expect_false(any(grepl("Se descargar", mensajes)))
  expect_true(any(grepl("Leer esta muestra \\(727 MB comprimidos\\)", mensajes)))
})

# Resultados por localidad de 50 000 y mas habitantes (2025) -----------------------

test_that("intercensal() lee los resultados de 2025 y devuelve las localidades por omision", {
  testthat::skip_if_not_installed("readxl")
  servidor <- censo_local()
  mensajes <- testthat::capture_messages(localidades <- intercensal(2025, "resultados"))
  expect_false(any(grepl("No indicaste", mensajes)))
  expect_identical(descargas_censo(servidor), 1L)
  expect_match(attr(localidades, "fuente"), "/eic/2025/microdatos/eic2025_micro_105\\.xlsx$")
  expect_named(localidades, c("CVEGEO", "CVE_ENT", "NOM_ENT", "CVE_MUN", "NOM_MUN", "CVE_LOC", "NOM_LOC",
                              "ESTIMADOR", "POBTOT", "PROM_HNV", "PCN_POB_AFRO"))
  # Tres localidades, cinco estimaciones de cada una. Las claves, que el XLSX
  # guarda como numeros, vuelven como texto con ceros a la izquierda
  expect_identical(nrow(localidades), 15L)
  expect_identical(unique(localidades$CVEGEO), c("010010001", "060020001", "060100001"))
  expect_identical(unique(localidades$CVE_ENT), c("01", "06"))
  expect_identical(unique(localidades$CVE_LOC), "0001")
  expect_identical(localidades$ESTIMADOR[1:5], c("Valor puntual", "Error estándar", "Límite inferior",
                                                 "Límite superior", "Coeficiente de variación"))
  puntual <- localidades[localidades$ESTIMADOR == "Valor puntual", ]
  expect_type(localidades$POBTOT, "double")
  expect_equal(puntual$POBTOT, c(900000, 250000, 150000))
  expect_equal(puntual$PROM_HNV, c(1.89, 1.94, 1.96))
  expect_identical(puntual$NOM_LOC[3], "Ciudad de Villa de Álvarez")
  # El dato reservado ("*") es NA; la columna es numerica
  expect_type(localidades$PCN_POB_AFRO, "double")
  expect_identical(is.na(puntual$PCN_POB_AFRO), c(FALSE, TRUE, FALSE))
})

test_that("intercensal() filtra los niveles de los resultados", {
  testthat::skip_if_not_installed("readxl")
  censo_local()
  municipios <- suppressMessages(intercensal(2025, "resultados", nivel = "municipio"))
  expect_identical(unique(paste(municipios$CVE_ENT, municipios$CVE_MUN)), c("01 001", "01 005", "06 002", "06 010"))
  expect_equal(municipios$POBTOT[municipios$ESTIMADOR == "Valor puntual"], c(1000000, 500000, 300000, 200000))

  todos <- suppressMessages(intercensal(2025, "resultados", nivel = "todos"))
  expect_identical(nrow(todos), 60L)
  expect_identical(todos$CVEGEO[1], "000000000")
  # El agregado de las localidades menores de cada entidad (997/9997) solo
  # aparece con nivel = "todos"; el dato no disponible ("N/D") es NA
  agregados <- todos[todos$CVE_MUN == "997", ]
  expect_identical(unique(agregados$CVE_LOC), "9997")
  expect_identical(unique(agregados$CVEGEO), c("019979997", "069979997"))
  expect_true(all(is.na(agregados$PCN_POB_AFRO[agregados$CVE_ENT == "01"])))
  puntual <- todos[todos$ESTIMADOR == "Valor puntual", ]
  # Las localidades mas el agregado suman la entidad
  entidades <- puntual[puntual$CVE_MUN == "000" & puntual$CVE_ENT != "00", ]
  detalle <- puntual[puntual$CVE_LOC != "0000", ]
  expect_equal(as.vector(tapply(detalle$POBTOT, detalle$CVE_ENT, sum)), entidades$POBTOT)

  nacional <- suppressMessages(intercensal(2025, "resultados", nivel = "nacional"))
  expect_identical(nrow(nacional), 5L)
  expect_equal(nacional$POBTOT[nacional$ESTIMADOR == "Valor puntual"], 2000000)
  dos <- suppressMessages(intercensal(2025, "resultados", nivel = c("entidad", "localidad")))
  expect_identical(nrow(dos), 25L)
})

test_that("intercensal() conserva las filas de las entidades pedidas en los resultados", {
  testthat::skip_if_not_installed("readxl")
  servidor <- censo_local()
  mensajes <- testthat::capture_messages(colima <- intercensal(2025, "resultados", estado = "Colima", nivel = "todos"))
  expect_true(any(grepl(
    "El INEGI publica los principales resultados por localidad de 50 000 y más habitantes de 2025 en un solo archivo nacional: se descarga ese archivo \\(28 MB\\) y se conservan las filas de Colima.",
    mensajes
  )))
  expect_identical(descargas_censo(servidor), 1L)
  expect_identical(unique(colima$CVE_ENT), "06")
  expect_identical(nrow(colima), 30L)
  expect_identical(attr(colima, "fuente"), poblacion_archivos("intercensal", 2025L, "resultados")$url)

  mensajes <- testthat::capture_messages(varias <- intercensal(2025, "resultados", estado = c("ags", "col")))
  expect_true(any(grepl("se conservan las filas de Aguascalientes, Colima", mensajes)))
  expect_identical(unique(varias$CVE_ENT), c("01", "06"))
  expect_identical(nrow(varias), 15L)
  expect_error(
    intercensal(2025, "resultados", estado = "Colima", nivel = "nacional"),
    "`nivel = \"nacional\"` solo está en el archivo nacional"
  )
})

test_that("intercensal() pide readxl antes de descargar los resultados", {
  servidor <- censo_local()
  local_mocked_bindings(requiere_paquete = function(paquete) detener("falta el paquete ", paquete))
  expect_error(intercensal(2025, "resultados"), "falta el paquete readxl")
  expect_identical(descargas_censo(servidor), 0L)
  # La muestra no lo necesita
  expect_no_error(suppressMessages(intercensal(2025)))
})

test_that("la lectura de XLSX encuentra la tabla o avisa que no la hay", {
  testthat::skip_if_not_installed("readxl")
  datos <- leer_tabla(muestra("intercensal/resultados2025_00.xlsx"))
  # Se descartan el titulo, la fila vacia, la nota al pie y la hoja de notas
  expect_identical(nrow(datos), 60L)
  expect_identical(names(datos)[1:3], c("CVEGEO", "CVE_ENT", "NOM_ENT"))
  expect_type(datos$POBTOT, "character")
  expect_error(leer_tabla(muestra("intercensal/sin_claves.xlsx")),
               "ninguna hoja tiene una columna con la clave de la entidad")
})

test_that("intercensal() avisa si el INEGI no tiene el archivo XLSX", {
  servidor <- censo_local()
  expect_error(
    suppressMessages(inegi_descargar("/contenidos/programas/eic/2025/microdatos/eic2025_micro_999.xlsx")),
    "El INEGI no tiene publicado el archivo"
  )
})

# Argumentos y mensajes cruzados ---------------------------------------------------

test_that("intercensal() valida sus argumentos y remite a censo() y conteo()", {
  expect_error(intercensal(), "Falta indicar el año en `year`")
  expect_error(intercensal(2020), "corresponde al Censo de Población y Vivienda, no a la Encuesta Intercensal: usa censo\\(2020\\)")
  expect_error(intercensal(2005), "corresponde al Conteo de Población y Vivienda, no a la Encuesta Intercensal: usa conteo\\(2005\\)")
  expect_error(censo(2015), "corresponde a la Encuesta Intercensal, no al Censo de Población y Vivienda: usa intercensal\\(2015\\)")
  expect_error(conteo(2025), "corresponde a la Encuesta Intercensal, no al Conteo de Población y Vivienda: usa intercensal\\(2025\\)")
  expect_error(intercensal(2010), "Años disponibles en intercensal\\(\\): 2015, 2025")
  expect_error(intercensal(2015, "resultados"), "La base \"resultados\" no existe para 2015: el INEGI la publica para 2025")
  expect_error(intercensal(2015, "iter"), "`base = \"iter\"` no es una opción válida. Bases de 2015: \"muestra\"")
  expect_error(intercensal(2015, tabla = "migrantes"), "migrantes")
  expect_error(intercensal(2025, "resultados", tabla = "personas"), "`tabla` solo se usa con la base \"muestra\"")
  expect_error(intercensal(2015, nivel = "municipio"), "`nivel` solo se usa con la base \"resultados\"")
  expect_error(intercensal(2025, "resultados", nivel = "manzana"), "no es una opción válida para la base \"resultados\"")
})
