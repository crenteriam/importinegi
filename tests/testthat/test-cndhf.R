# Pruebas de cndhf(). Usa el motor de los censos de gobierno (R/gobierno.R),
# que se prueba tambien en test-cngmd.R. Los archivos de muestra
# (fixtures/cndhf) son inventados y tienen la forma de los zip del INEGI; los
# genera data-raw/muestras_cndhf.R. El servidor de pruebas las entrega en las
# rutas del registro del paquete (helper-muestras.R).

consejo_2025 <- function(...) {
  suppressMessages(cndhf(2025, "estructura", "consejo consultivo", ...))
}

sin_conexion <- function(entorno = parent.frame()) {
  withr::local_options(importinegi.base = "http://127.0.0.1:9", .local_envir = entorno)
}

# Interfaz ------------------------------------------------------------------------

test_that("cndhf() tiene los argumentos de cngmd(), menos `estado`", {
  expect_true("cndhf" %in% getNamespaceExports("importinegi"))
  argumentos <- formals(cndhf)
  expect_identical(names(argumentos), c("year", "modulo", "tema", "tabla"))
  expect_identical(names(argumentos), setdiff(names(formals(cngmd)), "estado"))
  expect_null(argumentos$tema)
  expect_null(argumentos$tabla)
})

# Temas y modulos -----------------------------------------------------------------

test_that("los dos modulos se reconocen en los titulos de todas las ediciones", {
  sin_conexion()
  temas <- gobierno_temas("cndhf")
  expect_identical(nrow(temas), 175L)
  expect_false(anyNA(temas))
  expect_identical(sort(unique(temas$year)), 2017:2025)
  expect_identical(as.integer(table(temas$year)), c(20L, 21L, 20L, 18L, 18L, 18L, 17L, 21L, 22L))
  # Todos los titulos de modulo se reconocen, aunque cambian con la edicion:
  # hasta 2019, "Ejercicio de la funcion en materia de proteccion de derechos
  # humanos"; en 2020, "Proteccion de derechos humanos"; desde 2021,
  # "Capacitacion, difusion, defensa y proteccion de los derechos humanos"
  por_modulo <- table(temas$year, temas$modulo)
  expect_identical(colnames(por_modulo), c("estructura", "proteccion"))
  expect_identical(as.integer(por_modulo[, "estructura"]), c(13L, 13L, 12L, 9L, 9L, 7L, 7L, 11L, 12L))
  expect_identical(as.integer(por_modulo[, "proteccion"]), c(7L, 8L, 8L, 9L, 9L, 11L, 10L, 10L, 10L))
  # Un tema no se repite dentro de una edicion
  expect_identical(anyDuplicated(temas[c("year", "modulo", "base")]), 0L)
  expect_identical(anyDuplicated(paste(temas$year, normalizar_texto(temas$tema))), 0L)
  expect_true(all(temas$formato == "csv"))
  expect_true(all(grepl("/programas/cndhf/[0-9]{4}/datosabiertos/.*_csv[.]zip$", temas$url)))
  # Los temas del censo municipal no cambian
  expect_identical(nrow(gobierno_temas("cngmd")), 508L)
})

test_that("los nombres de los archivos y de las tablas se abrevian con las siglas del censo", {
  expect_identical(
    gobierno_identificador(c("Participacion_ciudada_cndhf2017", "conjunto_de_datos_consejo_consultivo_cndhf2025",
                             "exped_calificado_cndhf2021"), "cndhf"),
    c("participacion_ciudada", "consejo_consultivo", "exped_calificado")
  )
  expect_identical(
    gobierno_nombre_tabla(c("m1s1p4_cndhf2025.csv", "partciud.csv", "M2S9P2CP4_CNDHF2025.CSV", "m1p2"), "cndhf"),
    c("m1s1p4", "partciud", "m2s9p2cp4", "m1p2")
  )
  # Cada censo quita solo sus siglas
  expect_identical(gobierno_nombre_tabla("m1s1p4_cndhf2025.csv"), "m1s1p4_cndhf2025")
  expect_identical(gobierno_nombre_tabla("m1s1p3_cngmd2025.csv", "cndhf"), "m1s1p3_cngmd2025")
  expect_identical(gobierno_validar_tabla(c("M1S1P4", "m1s1p4_cndhf2025"), "cndhf"), "m1s1p4")
  expect_error(gobierno_dato("cnxyz", "sufijo"), "No se reconoce el censo de gobierno")
  expect_error(gobierno_claves("cnxyz"), "No se reconoce el censo de gobierno")
})

test_that("el tema se elige con el titulo o con el nombre del archivo", {
  sin_conexion()
  elegir <- function(year, modulo, tema) gobierno_elegir_tema("cndhf", year, modulo, tema)$base
  expect_identical(elegir(2025, "estructura", "consejo consultivo"), "consejo_consultivo")
  expect_identical(elegir(2025, "estructura", "CONSEJO"), "consejo_consultivo")
  expect_identical(elegir(2025, "proteccion", "Expedientes calificados"), "exped_calificado")
  expect_identical(elegir(2025, "proteccion", "exped_concluido"), "exped_concluido")
  expect_identical(elegir(2025, "proteccion", "hechos violatorios"), "exploespec_hechoviola_dh_selec")
  expect_identical(elegir(2017, "estructura", "participacion_ciudada"), "participacion_ciudada")
  expect_identical(elegir(2017, "estructura", "Participaci\u00f3n ciudadana"), "participacion_ciudada")
  # En 2020 un tema se llama igual que su modulo
  expect_identical(elegir(2020, "estructura", "estructura organizacional y recursos"),
                   "estructura_organizacional_recursos")
  expect_error(
    cndhf(2025, "proteccion", "expedientes"),
    "`tema = \"expedientes\"` coincide con 4 temas del m\u00f3dulo \"proteccion\" de 2025"
  )
  expect_error(
    cndhf(2025, "proteccion", "ayuntamientos"),
    "no coincide con ning\u00fan tema del m\u00f3dulo \"proteccion\" de 2025"
  )
  expect_error(
    cndhf(2025, "estructura"),
    "Falta indicar el tema en `tema`.*consejo_consultivo: Consejo Consultivo.*rec_huma: Recursos humanos"
  )
})

test_that("el a\u00f1o y el modulo se validan", {
  sin_conexion()
  expect_error(cndhf(), "Falta indicar el a\u00f1o en `year`.*cndhf\\(\\): 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025")
  expect_error(cndhf(2016, "estructura", "consejo"), "`year = 2016` no est\u00e1 disponible")
  expect_error(cndhf("dos mil", "estructura"), "no es un a\u00f1o")
  expect_error(cndhf(2025), "Falta indicar el m\u00f3dulo en `modulo`.*\"estructura\", \"proteccion\"")
  expect_error(cndhf(2025, "ayuntamiento", "consejo"), "`modulo = \"ayuntamiento\"` no es una opci\u00f3n v\u00e1lida")
  # El modulo acepta mayusculas y acentos
  expect_identical(gobierno_validar_modulo("cndhf", 2025, "Protecci\u00f3n"), "proteccion")
  expect_error(cndhf(2025, "estructura", "consejo", tabla = 3), "`tabla` debe ser el nombre")
  expect_error(cndhf(2025, "estructura", "consejo", tabla = c("indice", "m1s1p2")), "se pide sola")
})

# Contenido de los zip ---------------------------------------------------------------

test_that("se reconocen las tablas, los catalogos y el diccionario de cada forma de zip", {
  tipos <- function(nombre) {
    contenido <- gobierno_contenido(muestra(file.path("cndhf", nombre)), "csv", "cndhf")
    split(contenido$tabla, contenido$tipo)
  }
  y2025 <- tipos("cndhf2025_consejo.zip")
  expect_setequal(y2025$datos, c("m1s1p1", "m1s1p2", "m1s1p4"))
  expect_identical(y2025$indice, "0_indice")
  # Los catalogos llevan las siglas del censo; sin ellas, se llaman como su columna
  expect_setequal(y2025$catalogo, c("rnsnni1", "sexo_k"))
  expect_length(y2025$diccionario, 3)
  # Las bitacoras de cambios no son tablas del censo
  y2021 <- gobierno_contenido(muestra("cndhf/cndhf2021_seguimiento.zip"), "csv", "cndhf")
  expect_setequal(y2021$tabla[y2021$tipo == "datos"], c("m2s8p1", "m2s8p2", "m2s8p3"))
  bitacoras <- grepl("bitacora_de_cambios", y2021$ruta)
  expect_identical(sum(bitacoras), 2L)
  expect_identical(unique(y2021$tipo[bitacoras]), "otro")
  # 2017: tabla sin sufijo y una entrada "metadatos\" que es una carpeta
  y2017 <- gobierno_contenido(muestra("cndhf/cndhf2017_participacion.zip"), "csv", "cndhf")
  expect_identical(y2017$tabla[y2017$tipo == "datos"], "partciud")
  expect_setequal(y2017$tabla[y2017$tipo == "catalogo"], c("re_snn1", "re_snn2"))
  expect_identical(y2017$tipo[y2017$original == "metadatos\\"], "carpeta")
  expect_false("indice" %in% y2017$tipo)
})

test_that("las bitacoras de cambios tampoco se leen como tablas en el censo municipal", {
  carpeta <- withr::local_tempdir()
  dir.create(file.path(carpeta, "conjunto_de_datos"))
  for (nombre in c("m1s1p1_cngmd2021.csv", "m1s1p1_cngmd2021_bitacora_de_cambios.csv")) {
    writeLines(c("cvegeo,dato", "01001,1"), file.path(carpeta, "conjunto_de_datos", nombre))
  }
  zip <- file.path(carpeta, "tema.zip")
  withr::with_dir(carpeta, utils::zip(zip, list.files(".", recursive = TRUE), flags = "-r9Xq"))
  contenido <- gobierno_contenido(zip, "csv")
  expect_identical(contenido$tabla[contenido$tipo == "datos"], "m1s1p1")
})

test_that("gobierno_ruta() deja en ASCII los nombres que no son UTF-8 valido", {
  # 0xa2 y 0xa0 son la o y la a con acento en la pagina de codigos de DOS
  nombres <- c(
    "catalogos/sexo_i.csv", "conjunto_de_datos\\m1s2p1.csv", "Difusi\xa2n\\cat\xa0logos\\re_snn1.csv",
    "cat\u00e1logos/sexo.csv"
  )
  rutas <- gobierno_ruta(nombres)
  expect_identical(rutas[1:3], c("catalogos/sexo_i.csv", "conjunto_de_datos/m1s2p1.csv", "Difusi_n/cat_logos/re_snn1.csv"))
  # Los nombres en UTF-8 valido no cambian
  expect_identical(rutas[4], nombres[4])
  expect_true(all(validUTF8(rutas)))
  expect_identical(gobierno_ruta(character()), character())
  expect_identical(nombre_seguro(c("m1s1p3_cngmd2025.csv", "difusi\xa2n (2).csv", "a b/c.dbf")),
                   c("m1s1p3_cngmd2025.csv", "difusi_n__2_.csv", "a_b_c.dbf"))
})

test_that("se clasifica un zip con nombres que no son UTF-8 y con barra invertida", {
  zip <- muestra("cndhf/cndhf2017_capacitacion.zip")
  # La muestra tiene lo que se quiere probar
  nombres <- utils::unzip(zip, list = TRUE)$Name
  expect_identical(sum(!validUTF8(nombres)), 5L)
  expect_true(any(grepl("\\", nombres, fixed = TRUE, useBytes = TRUE)))
  contenido <- gobierno_contenido(zip, "csv", "cndhf")
  expect_identical(contenido$original, nombres)
  expect_identical(contenido$tipo, c("datos", "datos", "catalogo", "diccionario", "otro", "carpeta"))
  expect_identical(contenido$tabla[1:3], c("capacdif", "difusi_n", "re_snn1"))
  expect_false(any(grepl("[^ -~]", contenido$ruta)))
})

test_that("la clasificacion no depende de la configuracion regional de R", {
  zip <- muestra("cndhf/cndhf2017_capacitacion.zip")
  esperado <- gobierno_contenido(zip, "csv", "cndhf")
  # Sin UTF-8, R no puede convertir a la codificacion del equipo un nombre con
  # acentos: la clasificacion fallaba al listar el zip
  cambiada <- tryCatch(
    {
      withr::local_locale(c(LC_CTYPE = "C"))
      TRUE
    },
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  skip_if_not(cambiada, "No se pudo cambiar la configuracion regional")
  expect_identical(gobierno_contenido(zip, "csv", "cndhf"), esperado)
  extraido <- gobierno_extraer(zip, esperado[esperado$tipo == "datos", ])
  withr::defer(unlink(extraido$carpeta, recursive = TRUE))
  expect_true(all(file.exists(extraido$archivos)))
})

test_that("los archivos se extraen con nombres propios, no con los del zip", {
  for (nombre in c("cndhf/cndhf2017_capacitacion.zip", "gobierno/cngmd2021_alcaldias.zip", "cndhf/cndhf2025_consejo.zip")) {
    zip <- muestra(nombre)
    lista <- utils::unzip(zip, list = TRUE)
    contenido <- gobierno_contenido(zip, "csv", if (grepl("cngmd", nombre)) "cngmd" else "cndhf")
    filas <- contenido[contenido$tipo %in% c("datos", "indice", "catalogo", "diccionario"), ]
    expect_gt(nrow(filas), 0)
    extraido <- gobierno_extraer(zip, filas)
    expect_identical(length(extraido$archivos), nrow(filas))
    expect_true(all(file.exists(extraido$archivos)), info = nombre)
    # Cada archivo llega completo
    expect_identical(as.numeric(file.size(extraido$archivos)), as.numeric(lista$Length[match(filas$original, lista$Name)]),
                     info = nombre)
    # Dentro de la carpeta temporal, las rutas solo tienen letras, numeros, punto y guiones
    relativas <- substring(extraido$archivos, nchar(extraido$carpeta) + 2)
    expect_true(all(grepl("^[0-9]{4}/[A-Za-z0-9._-]+$", relativas)), info = paste(nombre, paste(relativas, collapse = " ")))
    # No queda en disco ningun otro archivo ni carpeta
    en_disco <- list.files(extraido$carpeta, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    expect_setequal(en_disco, relativas)
    # El nombre del archivo se conserva cuando ya es seguro
    seguros <- grepl("^[A-Za-z0-9._-]+$", basename(filas$ruta))
    expect_identical(basename(extraido$archivos)[seguros], basename(filas$ruta)[seguros])
    unlink(extraido$carpeta, recursive = TRUE)
  }
})

test_that("la extraccion avisa si un archivo no esta en el zip", {
  zip <- muestra("cndhf/cndhf2025_consejo.zip")
  filas <- gobierno_contenido(zip, "csv", "cndhf")
  filas <- filas[filas$tipo == "datos", ][1, ]
  filas$original <- "conjunto_de_datos/no_existe.csv"
  filas$ruta <- filas$original
  antes <- list.files(tempdir(), pattern = "^importinegi_")
  expect_error(gobierno_extraer(zip, filas), "No se pudo extraer de cndhf2025_consejo.zip: no_existe.csv")
  # No deja carpetas temporales
  expect_setequal(list.files(tempdir(), pattern = "^importinegi_"), antes)
})

# Claves geograficas y siglas ------------------------------------------------------------

test_that("las claves geograficas se completan segun la forma en que se publican", {
  # 2020 a 2022: clave del municipio en una columna, texto de 5 digitos
  una <- gobierno_preparar(data.frame(
    entidad_a = c("1", "14", "9"), ubicageo_b = c("1001", "14039", "9015"), totalca1 = c("5", "NA", "12"),
    stringsAsFactors = FALSE
  ), "cndhf")
  expect_identical(una$ubicageo_b, c("01001", "14039", "09015"))
  expect_identical(una$entidad_a, c(1L, 14L, 9L))
  expect_identical(una$totalca1, c(5L, NA, 12L))
  # 2022 a 2025: entidad y municipio en columnas separadas, 2 y 3 digitos
  separadas <- gobierno_preparar(data.frame(
    numeral = c("1", "2", "3"), enticve1 = c("1", "14", "NSS"), entifed1 = c("Aguascalientes", "Jalisco", "NSS"),
    municve1 = c("1", "39", "NSS"), municip1 = c("Aguascalientes", "Guadalajara", "NSS"),
    entfede_c = c("1", "14", "99"),
    stringsAsFactors = FALSE
  ), "cndhf")
  expect_identical(separadas$enticve1, c("01", "14", "NSS"))
  expect_identical(separadas$municve1, c("001", "039", "NSS"))
  # No se crean columnas y los nombres son los del INEGI
  expect_named(separadas, c("numeral", "enticve1", "entifed1", "municve1", "municip1", "entfede_c"))
  # La entidad con catalogo no es una clave: queda como numero, igual que su catalogo
  expect_identical(separadas$entfede_c, c(1L, 14L, 99L))
  # 2022: las dos formas en la misma tabla; cada columna sigue su regla
  ambas <- gobierno_preparar(data.frame(enticve1 = c(1, 14), ubicageo_e = c(1001, 14039)), "cndhf")
  expect_identical(ambas$enticve1, c("01", "14"))
  expect_identical(ambas$ubicageo_e, c("01001", "14039"))
  # Las claves que ya vienen completas no cambian
  expect_identical(gobierno_preparar(data.frame(enticve1 = "09", municve1 = "015", stringsAsFactors = FALSE),
                                     "cndhf"), data.frame(enticve1 = "09", municve1 = "015"))
  columnas <- gobierno_columnas_clave(
    data.frame(ubicageo_b = 1, UBICAGEO_C = 1, enticve1 = 1, municve1 = 1, entifed1 = 1, municip1 = 1,
               entidad_c = 1, entfede_c = 1, cvegeo = 1, cve_ent = 1), "cndhf"
  )
  expect_identical(columnas$municipio, c("ubicageo_b", "UBICAGEO_C"))
  expect_identical(columnas$entidad, "enticve1")
  expect_identical(columnas$mun, "municve1")
})

test_that("las claves de cada censo no se aplican al otro", {
  cruda <- data.frame(enticve1 = c("1", "14"), municve1 = c("1", "39"), cve_ent = c("1", "14"),
                      stringsAsFactors = FALSE)
  municipal <- gobierno_preparar(cruda)
  expect_identical(municipal$enticve1, c(1L, 14L))
  expect_identical(municipal$cve_ent, c("01", "14"))
  federal <- gobierno_preparar(cruda, "cndhf")
  expect_identical(federal$enticve1, c("01", "14"))
  expect_identical(federal$cve_ent, c(1L, 14L))
})

test_that("la sigla NA y el texto NULL pasan a ser NA; NSS, ND y NP se conservan", {
  archivo <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "limpia,con_sigla,con_null,texto,enticve1",
    "5,5,NULL,ND,1",
    "NA,NSS,3,NP,null",
    "12,NA,Null,a,14"
  ), archivo)
  lista <- gobierno_leer_tabla(archivo, "csv", "cndhf")
  expect_identical(lista$limpia, c(5L, NA, 12L))
  expect_identical(lista$con_sigla, c("5", "NSS", NA))
  # Sin el texto "NULL", la columna queda con numeros y se convierte
  # No distingue mayusculas de minusculas
  expect_identical(lista$con_null, c(NA, 3L, NA))
  expect_identical(lista$texto, c("ND", "NP", "a"))
  # En una clave geografica tambien pasa a ser NA; las demas se rellenan
  expect_identical(lista$enticve1, c("01", NA, "14"))
})

# Descarga y lectura ---------------------------------------------------------------------

test_that("sin `tabla`, cndhf() devuelve una lista con todas las tablas del tema", {
  censo_local()
  tablas <- consejo_2025()
  expect_type(tablas, "list")
  expect_named(tablas, c("m1s1p1", "m1s1p2", "m1s1p4"))
  expect_true(all(vapply(tablas, is.data.frame, logical(1))))
  expect_match(attr(tablas, "fuente"), "/cndhf/2025/datosabiertos/conjunto_de_datos_consejo_consultivo_cndhf2025_csv[.]zip$")
  indice <- attr(tablas, "indice")
  expect_identical(indice$tabla, names(tablas))
  expect_match(indice$contenido[2], "^Personas consejeras del Consejo Consultivo .* seg\u00fan caracter\u00edsticas")

  # Un total: una sola celda
  expect_identical(tablas$m1s1p1, structure(data.frame(totalca1 = 4L), fuente = attr(tablas, "fuente")))
  # Una lista: una fila por persona
  consejeras <- tablas$m1s1p2
  expect_named(consejeras, c("numeral", "nombre1", "sexo_k", "edades", "totalca1", "anio_a"))
  expect_identical(consejeras$numeral, 1:4)
  expect_identical(consejeras$sexo_k, c(2L, 1L, 9L, 2L))
  expect_identical(consejeras$nombre1, c("Mar\u00eda", "Jos\u00e9", "NP", "\u00c1ngela"))
  # Solo "NA" pasa a ser NA; las columnas con NSS, ND o NP quedan como texto
  expect_identical(consejeras$edades, c("52", "NSS", NA, "47"))
  expect_identical(consejeras$totalca1, c("85000", "NP", "ND", "90500"))
  expect_identical(consejeras$anio_a, c(2019L, 2021L, NA, 2024L))
  # Una condicion y su desglose
  expect_identical(tablas$m1s1p4$rnsnni1, 1L)
  expect_identical(tablas$m1s1p4$seoesntt, 12L)
  expect_identical(tablas$m1s1p4$seoesn4, "NSS")
})

test_that("`tabla` elige una o varias tablas", {
  censo_local()
  una <- consejo_2025(tabla = "m1s1p2")
  expect_s3_class(una, "data.frame")
  expect_identical(nrow(una), 4L)
  expect_match(attr(una, "fuente"), "consejo_consultivo_cndhf2025_csv[.]zip$")
  expect_identical(consejo_2025(tabla = "M1S1P2_cndhf2025"), una)
  varias <- consejo_2025(tabla = c("m1s1p4", "m1s1p1"))
  expect_named(varias, c("m1s1p4", "m1s1p1"))
  expect_error(
    consejo_2025(tabla = "m1s1p99"),
    "`tabla = \"m1s1p99\"` no es una tabla del tema \"Consejo Consultivo\" \\(2025\\). Tablas: \"m1s1p1\", \"m1s1p2\", \"m1s1p4\""
  )
})

test_that("`tabla` da acceso al indice, a los catalogos y al diccionario", {
  censo_local()
  indice <- consejo_2025(tabla = "indice")
  expect_named(indice, c("tabla", "contenido"))
  expect_identical(indice$tabla, c("m1s1p1", "m1s1p2", "m1s1p4"))
  expect_match(indice$contenido[3], "^Sesiones celebradas")

  catalogos <- consejo_2025(tabla = "catalogos")
  expect_named(catalogos, c("rnsnni1", "sexo_k"))
  expect_identical(catalogos$sexo_k, structure(
    data.frame(sexo_k = c(1L, 2L, 9L), descrip = c("Hombre", "Mujer", "No identificado")),
    fuente = attr(catalogos, "fuente")
  ), ignore_attr = TRUE)
  # El catalogo se llama como la columna y usa los mismos codigos: se pueden unir
  unida <- merge(consejo_2025(tabla = "m1s1p2"), catalogos$sexo_k, by = "sexo_k")
  expect_identical(sort(unida$descrip), c("Hombre", "Mujer", "Mujer", "No identificado"))

  diccionario <- consejo_2025(tabla = "Diccionario")
  expect_named(diccionario, c("tabla", "COLUMNA", "DESCRIPCION", "ORIGEN", "TIPO_DATO", "LONGITUD", "CODIGO_VALIDO"))
  expect_identical(diccionario$tabla, rep(c("m1s1p1", "m1s1p2", "m1s1p4"), c(1, 6, 6)))
  expect_identical(diccionario$COLUMNA[2:4], c("numeral", "nombre1", "sexo_k"))
  expect_identical(diccionario$ORIGEN[1], "SECC I. 1")
})

test_that("las tablas con geografia se devuelven completas y con las claves rellenadas", {
  censo_local()
  hechos <- suppressMessages(cndhf(2025, "proteccion", "hechos violatorios"))
  expect_named(hechos, c("m2s9p1", "m2s9p2cp4"))
  municipios <- hechos$m2s9p2cp4
  expect_identical(nrow(municipios), 4L)
  expect_named(municipios, c("numeral", "enticve1", "entifed1", "municve1", "municip1", "instresp_y",
                             "hevidh1", "hevidh2"))
  expect_identical(municipios$enticve1, c("01", "14", "14", "NSS"))
  expect_identical(municipios$municve1, c("001", "039", "120", "NSS"))
  expect_identical(municipios$municip1, c("Aguascalientes", "Guadalajara", "Zapopan", "NSS"))
  expect_identical(municipios$hevidh1, c(1L, 0L, NA, 2L))
  expect_identical(municipios$hevidh2, c("0", "NP", "1", "0"))
  # La entidad con catalogo queda como numero y coincide con su catalogo
  expect_identical(hechos$m2s9p1$entfede_c, c(1L, 14L, 99L))
  catalogos <- suppressMessages(cndhf(2025, "proteccion", "hechos violatorios", tabla = "catalogos"))
  expect_identical(catalogos$entfede_c$entfede_c, c(1L, 14L, 99L))
  expect_identical(merge(hechos$m2s9p1, catalogos$entfede_c)$descrip,
                   c("Aguascalientes", "Jalisco", "No especificado"))
})

test_that("se lee la edicion 2021: sin bitacoras y con la clave del municipio en 5 digitos", {
  censo_local()
  seguimiento <- suppressMessages(cndhf(2021, "proteccion", "seguimiento"))
  expect_named(seguimiento, c("m2s8p1", "m2s8p2", "m2s8p3"))
  expect_identical(seguimiento$m2s8p3$ubicageo_c, c("01001", "09015", "14039"))
  expect_identical(seguimiento$m2s8p2$recosett, c("60", "25", "NSS"))
  expect_identical(attr(seguimiento, "indice")$tabla, names(seguimiento))
  # El catalogo de municipios usa la misma clave que la tabla
  catalogos <- suppressMessages(cndhf(2021, "proteccion", "seguimiento", tabla = "catalogos"))
  expect_identical(catalogos$ubicageo_c$ubicageo_c, c("01001", "09015", "14039"))
  expect_identical(merge(seguimiento$m2s8p3, catalogos$ubicageo_c)$descrip,
                   c("Aguascalientes", "Cuauht\u00e9moc", "Guadalajara"))
  diccionario <- suppressMessages(cndhf(2021, "proteccion", "seguimiento", tabla = "diccionario"))
  expect_identical(unique(diccionario$tabla), c("m2s8p1", "m2s8p2", "m2s8p3"))
  expect_error(
    suppressMessages(cndhf(2021, "proteccion", "seguimiento", tabla = "m2s8p1_cndhf2021_bitacora_de_cambios")),
    "no es una tabla del tema \"Seguimiento\" \\(2021\\)"
  )
})

test_that("se lee la edicion 2017: sin indice y con el diccionario como documento", {
  censo_local()
  participacion <- suppressMessages(cndhf(2017, "estructura", "participacion_ciudada"))
  expect_named(participacion, "partciud")
  expect_identical(participacion$partciud, structure(
    data.frame(re_snn1 = 1L, re_snn2 = 2L), fuente = attr(participacion, "fuente")
  ))
  expect_match(attr(participacion, "fuente"), "/cndhf/2017/datosabiertos/m1/Participacion_ciudada_cndhf2017_csv[.]zip$")
  # Con el titulo se elige el mismo tema
  expect_identical(suppressMessages(cndhf(2017, "Estructura", "Participaci\u00f3n ciudadana")), participacion)
  indice <- suppressMessages(cndhf(2017, "estructura", "participacion", tabla = "indice"))
  expect_identical(indice$tabla, "partciud")
  expect_true(is.na(indice$contenido))
  catalogos <- suppressMessages(cndhf(2017, "estructura", "participacion", tabla = "catalogos"))
  expect_named(catalogos, c("re_snn1", "re_snn2"))
  expect_error(
    suppressWarnings(suppressMessages(cndhf(2017, "estructura", "participacion", tabla = "diccionario"))),
    "no es una tabla: el INEGI lo publica como tabla desde la edici\u00f3n 2020.*Participacion_ciudada_cndhf2017_csv[.]zip"
  )
})

test_that("el aviso del diccionario del censo municipal sigue citando la edicion 2021", {
  expect_identical(gobierno_dato("cngmd", "documentado"), 2021L)
  expect_identical(gobierno_dato("cndhf", "documentado"), 2020L)
})

test_that("un tema que el servidor no tiene se reporta como archivo no publicado", {
  censo_local()
  expect_error(
    suppressMessages(cndhf(2025, "estructura", "recursos humanos")),
    "El INEGI no tiene publicado el archivo"
  )
})

test_that("se lee un tema cuyo zip tiene nombres que no son UTF-8 y barra invertida", {
  censo_local()
  tablas <- suppressMessages(cndhf(2017, "estructura", "capacitacion"))
  # La tabla con acento en su nombre queda con "_" en su lugar
  expect_named(tablas, c("capacdif", "difusi_n"))
  expect_identical(tablas$capacdif$re_snn1, 1:2)
  # El texto "NULL" llega como NA y la columna queda como numero
  expect_identical(tablas$capacdif$totalca1, c(5L, NA))
  expect_identical(tablas$capacdif$nombre1, c("Capacitaci\u00f3n", "Difusi\u00f3n"))
  expect_identical(tablas$difusi_n$totalca1, c("NSS", "7"))
  expect_match(attr(tablas, "fuente"), "/2017/datosabiertos/m1/Capacitac_difusion_DH_cndhf2017_csv\\.zip$")
  una <- suppressMessages(cndhf(2017, "estructura", "capacitacion", tabla = "difusi_n"))
  expect_identical(una$totalca1, c("NSS", "7"))
  catalogos <- suppressMessages(cndhf(2017, "estructura", "capacitacion", tabla = "catalogos"))
  expect_named(catalogos, "re_snn1")
  expect_identical(catalogos$re_snn1$descrip, c("S\u00ed", "No", "No se sabe"))
  indice <- suppressMessages(cndhf(2017, "estructura", "capacitacion", tabla = "indice"))
  expect_identical(indice$tabla, c("capacdif", "difusi_n"))
  # En 2017 el diccionario es un documento, tambien cuando su nombre no es UTF-8
  expect_error(suppressMessages(cndhf(2017, "estructura", "capacitacion", tabla = "diccionario")),
               "no es una tabla")
})

# bases_importinegi() ---------------------------------------------------------------------

test_that("bases_importinegi() lista los temas del censo de derechos humanos federal", {
  sin_conexion()
  temas <- bases_importinegi("cndhf")
  expect_identical(nrow(temas), 175L)
  expect_named(temas, names(bases_importinegi("cngmd", 2025)))
  expect_identical(unique(temas$proyecto), "cndhf")
  expect_identical(unique(temas$cobertura), "nacional")
  expect_identical(unique(temas$formato), "csv")
  y2025 <- bases_importinegi(proyecto = "cndhf", year = 2025)
  expect_identical(y2025$modulo, rep(c("estructura", "proteccion"), c(12L, 10L)))
  fila <- y2025[y2025$base == "consejo_consultivo", ]
  expect_identical(fila$descripcion, "Consejo Consultivo")
  expect_equal(fila$tamano_mb, 1.2)
  # `modulo`, `base` y `descripcion` son los valores que acepta cndhf()
  expect_identical(
    gobierno_elegir_tema("cndhf", 2025, fila$modulo, fila$base)$url,
    gobierno_elegir_tema("cndhf", 2025, fila$modulo, fila$descripcion)$url
  )
  expect_identical(unique(bases_importinegi()$proyecto),
                   c("cpv_censo", "cpv_conteo", "cpv_intercensal", "cngmd", "cndhf", "enigh_tradicional", "enigh_nc", "enoe", "etoe",
                     "sig_marcogeoestadistico"))
  expect_error(bases_importinegi("cndh"), "Proyectos disponibles: .*\"cngmd\", \"cndhf\"")
})
