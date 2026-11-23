# Pruebas de cng_municipal() y del motor de los censos de gobierno (R/gobierno.R).
# Los archivos de muestra (fixtures/gobierno) son inventados y tienen la forma
# de los zip del INEGI; los genera data-raw/muestras_gobierno.R. El servidor
# local los entrega en las rutas reales del registro del paquete.

ayuntamientos_2025 <- function(...) {
  suppressMessages(cng_municipal(2025, "ayuntamiento", "ayuntamientos", ...))
}

# Temas del registro ---------------------------------------------------------------

test_that("el registro tiene los temas de las ocho ediciones del censo municipal", {
  withr::local_options(importinegi.registro = NULL)
  temas <- gobierno_temas("cng_municipal")
  expect_identical(sort(unique(temas$year)), as.integer(seq(2011, 2025, by = 2)))
  expect_identical(as.integer(table(temas$year)), c(17L, 28L, 46L, 72L, 56L, 72L, 87L, 130L))
  expect_false(anyNA(temas))
  expect_true(all(temas$modulo %in% gobierno_modulos("cng_municipal")$modulo))
  # Un tema se elige dentro de un modulo: ahi no se repiten titulos ni archivos
  expect_identical(anyDuplicated(temas[c("year", "modulo", "base")]), 0L)
  expect_identical(anyDuplicated(paste(temas$year, temas$modulo, normalizar_texto(temas$tema))), 0L)
  # Se leen los datos abiertos (CSV); el DBF solo donde el INEGI no publica CSV
  dbf <- temas[temas$formato == "dbf", ]
  expect_identical(paste(dbf$year, dbf$modulo, dbf$base), c("2011 agua agua", "2011 residuos residuos"))
  expect_true(all(grepl("_csv[.]zip$", temas$url[temas$formato == "csv"])))
  # Todos los archivos del proyecto en el registro pertenecen a un modulo
  registro <- registro_inegi()
  titulos <- registro$titulo[registro$proyecto == "cngmd"]
  claves <- sub("^[0-9]+ ", "", normalizar_texto(sub("[|].*$", "", titulos)))
  patron <- paste(gobierno_modulos("cng_municipal")$patron, collapse = "|")
  expect_identical(length(titulos), 725L)
  expect_true(all(grepl(patron, claves)))
})

test_that("los modulos de cada edicion son los documentados", {
  withr::local_options(importinegi.registro = NULL)
  temas <- gobierno_temas("cng_municipal")
  primera <- tapply(temas$year, temas$modulo, min)
  expect_identical(
    as.list(primera[gobierno_modulos("cng_municipal")$modulo]),
    list(ayuntamiento = 2011L, administracion = 2011L, catastro = 2015L, territorio = 2017L,
         seguridad = 2011L, proteccion_civil = 2023L, justicia = 2011L, agua = 2011L, residuos = 2011L)
  )
  # Una vez que aparece, el modulo sigue en todas las ediciones
  for (modulo in names(primera)) {
    ediciones <- sort(unique(temas$year[temas$modulo == modulo]))
    expect_identical(ediciones, as.integer(seq(primera[[modulo]], 2025, by = 2)), info = modulo)
  }
})

test_that("los nombres de los archivos y de las tablas se abrevian", {
  expect_identical(
    gobierno_identificador(c("ayu_comisiones_cngmd2011", "conjunto_de_datos_ayuntamientos_cngmd2025",
                             "Integrantes_ayuntami_cngmd2017", "Prog_moderni_catastral_cngmd207")),
    c("ayu_comisiones", "ayuntamientos", "integrantes_ayuntami", "prog_moderni_catastral")
  )
  expect_identical(
    gobierno_nombre_tabla(c("m1s1p3_cngmd2025.csv", "ACT_AYUN.DBF", "cabildos_cngmd2019.csv", "m1s1p12a")),
    c("m1s1p3", "act_ayun", "cabildos", "m1s1p12a")
  )
  expect_identical(c("m1s1p12a", "m1s1p3", "m1s1p1", "0_x")[gobierno_orden_natural(c("m1s1p12a", "m1s1p3", "m1s1p1", "0_x"))],
                   c("0_x", "m1s1p1", "m1s1p3", "m1s1p12a"))
  expect_identical(c("ini_ayun", "in_teasu")[gobierno_orden_natural(c("ini_ayun", "in_teasu"))],
                   c("in_teasu", "ini_ayun"))
})

# Argumentos -----------------------------------------------------------------------

test_that("el tema se elige con el titulo o con el nombre del archivo", {
  withr::local_options(importinegi.registro = NULL)
  elegir <- function(year, modulo, tema) gobierno_elegir_tema("cng_municipal", year, modulo, tema)$base
  # Parte del titulo, sin distinguir mayusculas ni acentos
  expect_identical(elegir(2011, "ayuntamiento", "estructura"), "ayu_estructura")
  expect_identical(elegir(2011, "ayuntamiento", "comision"), "ayu_comisiones")
  expect_identical(elegir(2011, "ayuntamiento", "COMISIONES E INICIATIVAS"), "ayu_comisiones")
  expect_identical(elegir(2011, "administracion", "tr\u00e1mites"), "apmd_tramites")
  # Nombre del archivo, completo o en parte
  expect_identical(elegir(2011, "ayuntamiento", "ayu_comisiones"), "ayu_comisiones")
  expect_identical(elegir(2011, "ayuntamiento", "ayu_comisiones_cngmd2011"), "ayu_comisiones")
  expect_identical(elegir(2017, "ayuntamiento", "integrantes_ayuntami"), "integrantes_ayuntami")
  expect_identical(elegir(2017, "ayuntamiento", "integrantes"), "integrantes_ayuntami")
  # El texto igual al titulo gana a las coincidencias parciales
  expect_identical(elegir(2025, "ayuntamiento", "ayuntamientos"), "ayuntamientos")
  expect_identical(elegir(2017, "administracion", "Transparencia"), "transparencia")
  # Un modulo con un solo tema no necesita `tema`
  expect_identical(elegir(2011, "agua", NULL), "agua")
  expect_identical(elegir(2011, "residuos", NULL), "residuos")
})

test_that("un tema ambiguo o inexistente se explica con los temas del modulo", {
  withr::local_options(importinegi.registro = NULL)
  expect_error(
    cng_municipal(2017, "administracion", "recursos"),
    "coincide con 5 temas del m\u00f3dulo \"administracion\" de 2017.*rec_materiales: Recursos materiales"
  )
  expect_error(
    cng_municipal(2011, "justicia", "catastro"),
    "no coincide con ning\u00fan tema del m\u00f3dulo \"justicia\" de 2011.*jm_recursos: Recursos humanos"
  )
  expect_error(cng_municipal(2011, "justicia"), "Falta indicar el tema en `tema`.*jm_ejercicio: Ejercicio")
  expect_error(cng_municipal(2011, "justicia", c("a", "b")), "`tema` debe ser un texto")
  expect_error(cng_municipal(2011, "justicia", ""), "`tema` debe ser un texto")
})

test_that("el a\u00f1o y el modulo se validan", {
  withr::local_options(importinegi.registro = NULL)
  expect_error(cng_municipal(), "Falta indicar el a\u00f1o en `year`.*2011, 2013, 2015, 2017, 2019, 2021, 2023, 2025")
  expect_error(cng_municipal(2012, "agua"), "`year = 2012` no est\u00e1 disponible")
  expect_error(cng_municipal("dos mil", "agua"), "no es un a\u00f1o")
  expect_error(cng_municipal(2025), "Falta indicar el m\u00f3dulo en `modulo`.*\"proteccion_civil\"")
  expect_error(cng_municipal(2025, "salud", "x"), "`modulo = \"salud\"` no es una opci\u00f3n v\u00e1lida")
  expect_error(
    cng_municipal(2015, "proteccion_civil", "x"),
    "El m\u00f3dulo \"proteccion_civil\" no existe en la edici\u00f3n 2015.*bases_importinegi"
  )
  # El modulo acepta mayusculas, acentos y espacios
  expect_identical(gobierno_validar_modulo("cng_municipal", 2025, "Protecci\u00f3n Civil"), "proteccion_civil")
  expect_identical(gobierno_validar_modulo("cng_municipal", 2011, " AGUA "), "agua")
})

test_that("`tabla` y `estado` se validan antes de descargar", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  expect_error(cng_municipal(2025, "ayuntamiento", "ayuntamientos", tabla = 3), "`tabla` debe ser el nombre")
  expect_error(
    cng_municipal(2025, "ayuntamiento", "ayuntamientos", tabla = c("indice", "m1s1p3")),
    "`tabla = \"indice\"` se pide sola"
  )
  expect_error(cng_municipal(2025, "ayuntamiento", "ayuntamientos", estado = "Atlantida"), "No se reconoce la entidad")
  expect_identical(gobierno_validar_tabla(c("M1S1P3", "m1s1p3_cngmd2025")), "m1s1p3")
  expect_identical(gobierno_validar_tabla("Cat\u00e1logos"), "catalogos")
  expect_identical(gobierno_validar_tabla("\u00cdndice"), "indice")
  expect_null(gobierno_entidades(NULL))
  expect_null(gobierno_entidades(c("Jalisco", "nacional")))
  expect_identical(gobierno_entidades(c("jal", 6, "Jalisco")), c("14", "06"))
})

# Contenido de los zip ---------------------------------------------------------------

test_that("se reconocen las tablas, los catalogos y el diccionario de cada forma de zip", {
  tipos <- function(nombre, formato = "csv") {
    contenido <- gobierno_contenido(muestra(file.path("gobierno", nombre)), formato)
    split(contenido$tabla, contenido$tipo)
  }
  y2025 <- tipos("cngmd2025_ayuntamientos.zip")
  expect_setequal(y2025$datos, c("m1s1p1", "m1s1p3", "m1s1p12a"))
  expect_identical(y2025$indice, "0_indice")
  expect_setequal(y2025$catalogo, c("cvegeo", "recono_i", "sexo_i"))
  expect_length(y2025$diccionario, 2)
  # Rutas con "\" como separador
  y2021 <- gobierno_contenido(muestra("gobierno/cngmd2021_alcaldias.zip"), "csv")
  expect_true(all(grepl("\\", y2021$original, fixed = TRUE)))
  expect_false(any(grepl("\\", y2021$ruta, fixed = TRUE)))
  expect_setequal(y2021$tabla[y2021$tipo == "datos"], c("m1s2p1", "m1s2p9"))
  expect_identical(y2021$tabla[y2021$tipo == "catalogo"], "sexo_i")
  # Carpeta "conjunto_de datos" y carpeta de datos dentro de otra
  expect_setequal(tipos("cngmd2019_actividad.zip")$datos, c("cabildos", "iniciati"))
  y2015 <- tipos("cngmd2015_estructura.zip")
  expect_identical(y2015$datos, "ayuntami")
  expect_identical(y2015$catalogo, "entidad")
  # DBF
  dbf <- tipos("cngmd2011_agua.zip", "dbf")
  expect_setequal(dbf$datos, c("agua_red", "plantas"))
  expect_identical(dbf$catalogo, "cat_prest")
})

test_that("las claves se completan con ceros y solo la sigla NA pasa a ser NA", {
  cruda <- data.frame(
    ubicageo_c = c("1001", "14039", "NA"), cve_ent = c("1", "14", "9"), cve_mun = c("1", "39", "15"),
    cantidad = c("5", "NA", "12"), con_sigla = c("5", "NSS", "NA"), texto = c("a", "ND", "NP"),
    stringsAsFactors = FALSE
  )
  lista <- gobierno_preparar(cruda)
  expect_identical(lista$ubicageo_c, c("01001", "14039", NA))
  expect_identical(lista$cve_ent, c("01", "14", "09"))
  expect_identical(lista$cve_mun, c("001", "039", "015"))
  expect_identical(lista$cantidad, c(5L, NA, 12L))
  expect_identical(lista$con_sigla, c("5", "NSS", NA))
  expect_identical(lista$texto, c("a", "ND", "NP"))
  # Las claves numericas de los DBF tambien quedan como texto
  expect_identical(gobierno_preparar(data.frame(UBIC_GEO = c(1001, 14039)))$UBIC_GEO, c("01001", "14039"))
  expect_identical(gobierno_rellenar(c("Jalisco", "7", NA), 2), c("Jalisco", "07", NA))
  expect_identical(gobierno_entidad_fila(lista), c("01", "14", NA))
  expect_null(gobierno_entidad_fila(data.frame(tema = 1)))
  # La clave del municipio tiene varios nombres segun la edicion y la tabla
  nombres <- c("ubic_geo", "UBI_GEO", "ub_geo", "ubicageo_c", "ubicageo_f", "cvegeo", "UBIC_GEO", "geologia", "ubicacion")
  cruda <- as.data.frame(stats::setNames(rep(list("1001"), length(nombres)), nombres), stringsAsFactors = FALSE)
  expect_identical(gobierno_columnas_clave(cruda)$municipio, nombres[1:7])
})

test_that("el diccionario se reconoce aunque sus encabezados lleven acentos", {
  carpeta <- withr::local_tempdir()
  archivo <- file.path(carpeta, "diccionario_de_datos_m1s1p3_cngmd2025.csv")
  writeLines(enc2utf8(c("COLUMNA,DESCRIPCI\u00d3N,ORIGEN,TIPO_DATO,LONGITUD,C\u00d3DIGO V\u00c1LIDO",
                        "cvegeo,Clave geoestad\u00edstica concatenada,Presentaci\u00f3n,Car\u00e1cter,5,01001 al 32058")),
             archivo, useBytes = TRUE)
  diccionario <- gobierno_diccionario(archivo, c("m1s1p1", "m1s1p3"))
  expect_named(diccionario, c("tabla", "COLUMNA", "DESCRIPCION", "ORIGEN", "TIPO_DATO", "LONGITUD", "CODIGO_VALIDO"))
  expect_identical(diccionario$tabla, "m1s1p3")
  expect_identical(diccionario$TIPO_DATO, "Car\u00e1cter")
})

test_that("un DBF que declara mas registros de los que contiene se lee sin filas vacias", {
  carpeta <- withr::local_tempdir()
  utils::unzip(muestra("gobierno/cngmd2011_agua.zip"), exdir = carpeta)
  original <- list.files(carpeta, pattern = "AGUA_RED[.]DBF$", recursive = TRUE, full.names = TRUE)
  expect_identical(gobierno_reparar_dbf(original), original)
  # Como los DBF de agua potable de 2011: el encabezado dice 50,122 registros
  inflado <- file.path(carpeta, "inflado.dbf")
  file.copy(original, inflado)
  conexion <- file(inflado, open = "r+b")
  seek(conexion, where = 4, rw = "write")
  writeBin(50122L, conexion, size = 4, endian = "little")
  close(conexion)
  reparado <- gobierno_reparar_dbf(inflado)
  expect_false(identical(reparado, inflado))
  tabla <- gobierno_leer_tabla(inflado, "dbf")
  expect_identical(tabla, gobierno_leer_tabla(original, "dbf"))
  expect_identical(tabla$UBIC_GEO, c("01001", "06002", "14039"))
})

# Descargas contra el servidor local ---------------------------------------------------

test_that("sin `tabla`, cng_municipal() devuelve una lista con todas las tablas del tema", {
  censo_local()
  tablas <- ayuntamientos_2025()
  expect_type(tablas, "list")
  expect_named(tablas, c("m1s1p1", "m1s1p3", "m1s1p12a"))
  expect_true(all(vapply(tablas, is.data.frame, logical(1))))
  expect_match(attr(tablas, "fuente"), "/cngmd/2025/datosabiertos/conjunto_de_datos_ayuntamientos_cngmd2025_csv[.]zip$")
  indice <- attr(tablas, "indice")
  expect_identical(indice$tabla, names(tablas))
  expect_match(indice$contenido[2], "^Personas s\u00edndicas que integraban el Ayuntamiento")

  sindicas <- tablas$m1s1p3
  expect_identical(names(sindicas), c("cvegeo", "cve_ent", "cve_mun", "numeral", "nombre1", "sexo_i",
                                      "edades", "totalca1", "ingreso"))
  # La clave del municipio conserva el nombre del INEGI y es texto de 5 digitos
  expect_identical(sindicas$cvegeo, c("01001", "06002", "06002", "14039", "14120"))
  expect_identical(sindicas$cve_ent, c("01", "06", "06", "14", "14"))
  expect_identical(sindicas$cve_mun, c("001", "002", "002", "039", "120"))
  # Los codigos y las cantidades son numeros
  expect_identical(sindicas$numeral, c(1L, 1L, 2L, 1L, 1L))
  expect_identical(sindicas$sexo_i, c(2L, 1L, 2L, 9L, 1L))
  expect_equal(sindicas$ingreso, c(35000.5, NA, 28000, 0, 41250.75))
  # Solo "NA" pasa a ser NA; NSS, ND y NP se conservan y la columna queda como texto
  expect_identical(sindicas$edades, c("45", "NSS", "38", NA, "52"))
  expect_identical(sindicas$totalca1, c("2", "NP", "ND", NA, "1"))
  expect_identical(sindicas$nombre1, c("Mar\u00eda", "Jos\u00e9", "Luc\u00eda", "NP", "\u00c1ngel"))
  expect_equal(tablas$m1s1p12a$presupue, c(1234567.5, NA, 0, 250000))
})

test_that("`tabla` elige una o varias tablas", {
  censo_local()
  una <- ayuntamientos_2025(tabla = "m1s1p3")
  expect_s3_class(una, "data.frame")
  expect_identical(nrow(una), 5L)
  expect_match(attr(una, "fuente"), "ayuntamientos_cngmd2025_csv[.]zip$")
  expect_identical(ayuntamientos_2025(tabla = "M1S1P3_cngmd2025"), una)
  varias <- ayuntamientos_2025(tabla = c("m1s1p12a", "m1s1p1"))
  expect_named(varias, c("m1s1p12a", "m1s1p1"))
  expect_identical(attr(varias, "indice")$tabla, c("m1s1p12a", "m1s1p1"))
  expect_error(
    ayuntamientos_2025(tabla = "m1s1p99"),
    "`tabla = \"m1s1p99\"` no es una tabla del tema \"Ayuntamientos\" \\(2025\\). Tablas: \"m1s1p1\", \"m1s1p3\", \"m1s1p12a\""
  )
})

test_that("`tabla` da acceso al indice, a los catalogos y al diccionario", {
  censo_local()
  indice <- ayuntamientos_2025(tabla = "indice")
  expect_named(indice, c("tabla", "contenido"))
  expect_identical(indice$tabla, c("m1s1p1", "m1s1p3", "m1s1p12a"))
  expect_match(indice$contenido[3], "^Presupuesto ejercido")
  expect_match(attr(indice, "fuente"), "ayuntamientos_cngmd2025_csv[.]zip$")

  catalogos <- ayuntamientos_2025(tabla = "catalogos")
  expect_named(catalogos, c("cvegeo", "recono_i", "sexo_i"))
  expect_identical(catalogos$sexo_i, data.frame(sexo_i = c(1L, 2L, 9L),
                                                descrip = c("Hombre", "Mujer", "No identificado")))
  # El catalogo usa la misma clave que las tablas, asi que se pueden unir
  expect_identical(catalogos$cvegeo$cvegeo, c("01001", "06002", "14039", "14120"))
  unida <- merge(ayuntamientos_2025(tabla = "m1s1p1"), catalogos$cvegeo, by = "cvegeo")
  expect_identical(unida$descrip, c("Aguascalientes", "Colima", "Guadalajara", "Zapopan"))

  diccionario <- ayuntamientos_2025(tabla = "Diccionario")
  expect_identical(names(diccionario)[1:3], c("tabla", "COLUMNA", "DESCRIPCION"))
  expect_identical(diccionario$tabla, rep(c("m1s1p1", "m1s1p3"), c(2, 3)))
  expect_identical(diccionario$COLUMNA[3:5], c("cvegeo", "sexo_i", "edades"))
})

test_that("`estado` conserva las filas de las entidades pedidas", {
  censo_local()
  jalisco <- ayuntamientos_2025(tabla = "m1s1p3", estado = "Jalisco")
  expect_identical(jalisco$cvegeo, c("14039", "14120"))
  expect_identical(rownames(jalisco), c("1", "2"))
  expect_identical(ayuntamientos_2025(tabla = "m1s1p3", estado = c("col", 1))$cvegeo, c("01001", "06002", "06002"))
  expect_identical(nrow(ayuntamientos_2025(tabla = "m1s1p3", estado = "nacional")), 5L)
  expect_identical(nrow(ayuntamientos_2025(tabla = "m1s1p3", estado = "Yucat\u00e1n")), 0L)
  todas <- ayuntamientos_2025(estado = "ags")
  expect_identical(vapply(todas, nrow, integer(1)), c(m1s1p1 = 1L, m1s1p3 = 1L, m1s1p12a = 1L))
})

test_that("se leen los zip con \"\\\" como separador y la clave numerica de 2021", {
  censo_local()
  alcaldias <- suppressMessages(cng_municipal(2021, "ayuntamiento", "alcaldias"))
  expect_named(alcaldias, c("m1s2p1", "m1s2p9"))
  expect_identical(alcaldias$m1s2p1$ubicageo_c, c("09002", "09003", "09017"))
  expect_identical(alcaldias$m1s2p1$total, c(1L, NA, 3L))
  expect_match(attr(alcaldias, "indice")$contenido[1], "^Personas titulares de las alcald\u00edas")
  expect_named(suppressMessages(cng_municipal(2021, "ayuntamiento", "alcaldias", tabla = "catalogos")), "sexo_i")
  # Sin diccionario en forma de tabla
  expect_error(
    suppressMessages(cng_municipal(2021, "ayuntamiento", "alcaldias", tabla = "diccionario")),
    "El diccionario de datos del tema .* no es una tabla"
  )
  # Una tabla sin clave geografica se devuelve completa, con un aviso
  expect_message(
    cdmx <- cng_municipal(2021, "ayuntamiento", "alcaldias", estado = "cdmx"),
    "Sin clave geogr\u00e1fica, se devuelve completa: \"m1s2p9\""
  )
  expect_identical(nrow(cdmx$m1s2p1), 3L)
  expect_identical(nrow(cdmx$m1s2p9), 2L)
  jalisco <- suppressMessages(cng_municipal(2021, "ayuntamiento", "alcaldias", estado = "Jalisco"))
  expect_identical(vapply(jalisco, nrow, integer(1)), c(m1s2p1 = 0L, m1s2p9 = 2L))
})

test_that("se leen las carpetas con otros nombres (2015 y 2019)", {
  censo_local()
  actividad <- suppressMessages(cng_municipal(2019, "ayuntamiento", "actividades"))
  expect_named(actividad, c("cabildos", "iniciati"))
  expect_identical(actividad$cabildos$sesiones, c(24L, 18L))
  # Antes de 2021 no hay indice: el contenido queda vacio
  expect_identical(attr(actividad, "indice"), data.frame(tabla = c("cabildos", "iniciati"),
                                                         contenido = NA_character_))
  expect_error(
    suppressMessages(cng_municipal(2019, "ayuntamiento", "actividades", tabla = "catalogos")),
    "no incluye cat\u00e1logos"
  )
  estructura <- suppressMessages(cng_municipal(2015, "ayuntamiento", "estructura"))
  expect_named(estructura, "ayuntami")
  expect_identical(estructura$ayuntami$ubic_geo, c("01001", "06002", "14039"))
  expect_identical(estructura$ayuntami$entidad, c("01", "06", "14"))
  expect_identical(estructura$ayuntami$regidor, c(14L, 11L, 19L))
  expect_identical(
    suppressMessages(cng_municipal(2015, "ayuntamiento", "estructura", tabla = "catalogos"))$entidad$entidad,
    c("01", "06", "14")
  )
  expect_error(
    suppressMessages(cng_municipal(2015, "ayuntamiento", "estructura", tabla = "diccionario")),
    "no es una tabla: el INEGI lo publica como tabla desde la edici\u00f3n 2021"
  )
})

test_that("se leen los temas de 2011 en CSV y en DBF", {
  censo_local()
  comision <- suppressMessages(cng_municipal(year = 2011, modulo = "ayuntamiento", tema = "comision"))
  expect_named(comision, c("act_ayun", "com_ayun", "com_tasu", "in_teasu", "ini_ayun"))
  expect_identical(comision[[1]], comision$act_ayun)
  expect_identical(comision$act_ayun$ubic_geo, c("01001", "06002", "14039"))
  expect_identical(comision$act_ayun$ses_cab, c(3L, 5L, 8L))
  catalogos <- suppressMessages(cng_municipal(2011, "ayuntamiento", "comision", tabla = "catalogos"))
  expect_identical(catalogos$ubi_geo$ubic_geo, c("01001", "06002", "14039"))

  # Agua potable de 2011 solo se publica en DBF; es el unico tema de su modulo
  agua <- suppressMessages(cng_municipal(2011, "agua"))
  expect_named(agua, c("agua_red", "plantas"))
  expect_match(attr(agua, "fuente"), "/cngmd/2011/microdatos/agua_cngmd2011_dbf[.]zip$")
  expect_identical(names(agua$agua_red), c("UBIC_GEO", "TOMAS", "PRESTADOR"))
  expect_identical(agua$agua_red$UBIC_GEO, c("01001", "06002", "14039"))
  expect_equal(agua$agua_red$TOMAS, c(250000, 48000, 510000))
  expect_identical(suppressMessages(cng_municipal(2011, "agua", tabla = "PLANTAS", estado = 14))$UBIC_GEO, "14039")
  expect_named(suppressMessages(cng_municipal(2011, "agua", tabla = "catalogos")), "cat_prest")
})

test_that("un tema que el servidor no tiene se reporta como archivo no publicado", {
  censo_local()
  expect_error(
    suppressMessages(cng_municipal(2025, "ayuntamiento", "alcaldias")),
    "El INEGI no tiene publicado el archivo"
  )
})

# bases_importinegi() ---------------------------------------------------------------------

test_that("bases_importinegi() lista los temas del censo municipal", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  temas <- bases_importinegi("cng_municipal")
  expect_identical(nrow(temas), 508L)
  expect_identical(unique(temas$proyecto), "cng_municipal")
  expect_identical(unique(temas$cobertura), "nacional")
  y2025 <- bases_importinegi(proyecto = "cng_municipal", year = 2025)
  expect_identical(nrow(y2025), 130L)
  fila <- y2025[y2025$modulo == "ayuntamiento" & y2025$base == "ayuntamientos", ]
  expect_identical(fila$descripcion, "Ayuntamientos")
  expect_identical(fila$formato, "csv")
  expect_equal(fila$tamano_mb, 4.8)
  # `modulo`, `base` y `descripcion` son los valores que acepta cng_municipal()
  expect_identical(
    gobierno_elegir_tema("cng_municipal", 2025, fila$modulo, fila$base)$url,
    gobierno_elegir_tema("cng_municipal", 2025, fila$modulo, fila$descripcion)$url
  )
  # Los demas proyectos no cambian y no tienen modulo
  expect_identical(nrow(bases_importinegi(year = 2020)), 6L)
  expect_identical(unique(bases_importinegi()$proyecto), c("censo", "conteo", "intercensal", "cng_municipal"))
  expect_error(bases_importinegi("censo_municipal"), "Proyectos disponibles: .*\"cng_municipal\"")
})
