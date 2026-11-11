# Las muestras api_1780.json y api_mixto.json son respuestas reales del servicio
# de descargas del INEGI (10 de noviembre de 2026).

registros_muestra <- function(nombre) {
  jsonlite::fromJSON(leer_muestra(nombre), simplifyVector = FALSE)
}

# Sustituye las consultas al INEGI por las muestras.
texto_simulado <- function(url) {
  if (grepl("idBiinegi=1780&", url, fixed = TRUE)) {
    leer_muestra("api_1780.json")
  } else if (grepl("idBiinegi=", url, fixed = TRUE)) {
    "[]"
  } else if (grepl("enigh/nc/2014", url, fixed = TRUE)) {
    leer_muestra("pagina_programa.html")
  } else if (grepl("sin/identificador", url, fixed = TRUE)) {
    "<html><body>Sin componente de descargas</body></html>"
  } else {
    stop("El INEGI respondi\u00f3 con el c\u00f3digo 500 al solicitar ", url, call. = FALSE)
  }
}

test_that("api_url_archivos() arma la consulta de microdatos de un proyecto", {
  url <- api_url_archivos(1780)
  expect_match(url, "^https://www.inegi.org.mx/app/api/descarga/componente/descargamasiva/lista/archivoscompaginacion\\?")
  expect_match(url, "&idBiinegi=1780&", fixed = TRUE)
  expect_match(url, "&tipodocto=4&", fixed = TRUE)
})

test_that("api_a_tabla() genera una fila por archivo y formato", {
  tabla <- api_a_tabla(registros_muestra("api_1780.json"))
  expect_named(tabla, c("id_archivo", "titulo", "descripcion", "anio", "formato",
                        "sufijo", "tamano", "bytes", "ruta", "url"))
  # 12 tablas en 4 formatos mas el descriptor de archivos
  expect_identical(nrow(tabla), 49L)
  expect_identical(as.vector(table(tabla$formato)[c("csv", "dbf", "dta", "sav", "cuadro")]),
                   c(12L, 12L, 12L, 12L, 1L))
  expect_identical(unique(tabla$anio), 2014L)
  expect_false(anyNA(tabla$url))
})

test_that("las direcciones generadas coinciden con las verificadas en el sitio del INEGI", {
  tabla <- api_a_tabla(registros_muestra("api_1780.json"))
  dbf <- tabla[tabla$formato == "dbf", ]
  verificadas <- paste0(
    "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2014/microdatos/",
    c("NCV_Vivi", "NCV_Hogares", "NCV_Concentrado", "NCV_Erogaciones", "NCV_Gastohogar",
      "NCV_Gastotarjetas", "NCV_Poblacion", "NCV_Ingresos", "NCV_Gastopersona",
      "NCV_Trabajos", "NCV_agropecuario", "NCV_Noagropecuario"),
    "_2014_concil_2010_dbf.zip"
  )
  expect_setequal(tolower(dbf$url), tolower(verificadas))

  mixto <- api_a_tabla(registros_muestra("api_mixto.json"))
  expect_true(
    "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_2026_trim2_csv.zip" %in% mixto$url
  )
  expect_true(
    "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/2005trim1_dbf.zip" %in% mixto$url
  )
})

test_that("api_a_tabla() interpreta tamanos, enlaces y documentos", {
  tabla <- api_a_tabla(registros_muestra("api_mixto.json"))
  expect_identical(nrow(tabla), 10L)

  enoe <- tabla[tabla$ruta == "/programas/enoe/15ymas/microdatos/enoe_2026_trim2", ]
  expect_identical(enoe$formato, c("csv", "dbf", "dta", "sav"))
  expect_identical(enoe$tamano, c("33.6 MB", "32.5 MB", "53.5 MB", "43.8 MB"))
  expect_equal(enoe$bytes[2], 32.5 * 1024^2)
  expect_identical(enoe$anio, rep(2026L, 4))

  # Un enlace a la biblioteca digital no tiene ruta de descarga
  ficha <- tabla[tabla$formato == "html", ]
  expect_identical(ficha$url, "https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=702825002067")
  expect_identical(ficha$ruta, "")

  # Un documento que no esta comprimido
  diccionario <- tabla[tabla$formato == "xls", ]
  expect_identical(
    diccionario$url,
    "https://www.inegi.org.mx/contenidos/programas/ccpv/2010/doc/diccionario_cuestionario_basico.xls"
  )
  expect_equal(diccionario$bytes, 80.5 * 1024)
})

test_that("tamano_a_bytes() entiende los formatos de tamano del INEGI", {
  expect_equal(
    tamano_a_bytes(c("384 KB", "1.00 MB", "1,021 KB", "2.6 GB", "1", NA)),
    c(384 * 1024, 1024^2, 1021 * 1024, 2.6 * 1024^3, NA, NA)
  )
})

test_that("api_a_tabla() devuelve una tabla vacia con las mismas columnas", {
  vacia <- api_a_tabla(list())
  expect_identical(nrow(vacia), 0L)
  expect_named(vacia, names(api_a_tabla(registros_muestra("api_mixto.json"))))
})

test_that("inegi_api_id() lee el identificador del proyecto en la pagina del programa", {
  local_mocked_bindings(inegi_texto = texto_simulado)
  expect_identical(inegi_api_id("enigh/nc/2014"), 1780L)
  expect_identical(inegi_api_id("/enigh/nc/2014/"), 1780L)
  expect_error(inegi_api_id("sin/identificador"), "No se encontr\u00f3 el identificador del proyecto")
})

test_that("inegi_api_archivos() rechaza una respuesta que no es un listado", {
  local_mocked_bindings(inegi_texto = function(url) "<html>Error</html>")
  expect_error(inegi_api_archivos(1780), "no devolvi\u00f3 un listado de archivos v\u00e1lido para el proyecto 1780")
})

test_that("registro_construir() arma el registro y reporta los proyectos que fallan", {
  local_mocked_bindings(inegi_texto = texto_simulado)
  withr::local_options(importinegi.pausa = 0)
  proyectos <- data.frame(
    proyecto = c("enigh_nc", "enoe", "ccpv", "cngmd"),
    edicion = c("2014", "", "1890", "2099"),
    pagina = c("enigh/nc/2014", "sin/identificador", "no/existe", "enigh/nc/2014"),
    stringsAsFactors = FALSE
  )
  suppressMessages(expect_message(
    registro <- registro_construir(proyectos),
    "enigh_nc 2014: 49 archivos"
  ))
  expect_identical(nrow(registro), 98L)
  expect_identical(names(registro)[1:3], c("proyecto", "edicion", "id_api"))
  expect_identical(unique(registro$proyecto), c("enigh_nc", "cngmd"))
  expect_identical(unique(registro$edicion), c(2014L, 2099L))
  expect_identical(unique(registro$id_api), 1780L)

  fallas <- attr(registro, "fallas")
  expect_named(fallas, c("enoe", "ccpv 1890"))
  expect_match(fallas[["enoe"]], "No se encontr\u00f3 el identificador")
  expect_match(fallas[["ccpv 1890"]], "c\u00f3digo 500")
})

test_that("registro_construir() trata como falla un proyecto sin archivos", {
  local_mocked_bindings(inegi_texto = function(url) {
    if (grepl("idBiinegi=", url, fixed = TRUE)) "[]" else leer_muestra("pagina_programa.html")
  })
  registro <- registro_construir(
    data.frame(proyecto = "enoe", edicion = NA, pagina = "enoe/15ymas"),
    silencioso = TRUE
  )
  expect_identical(nrow(registro), 0L)
  expect_true(all(c("proyecto", "edicion", "id_api", "url") %in% names(registro)))
  expect_match(attr(registro, "fallas")[["enoe"]], "no lista archivos de microdatos")
})

test_that("el registro se construye de extremo a extremo contra un servidor HTTP", {
  servidor <- servidor_prueba()
  withr::local_options(
    importinegi.base = sub("/$", "", servidor$url("/")),
    importinegi.pausa = 0, importinegi.espera = 0
  )
  expect_identical(inegi_api_id("enigh/nc/2014"), 1780L)
  archivos <- inegi_api_archivos(1780)
  expect_identical(nrow(archivos), 49L)
  expect_identical(archivos$titulo[1],
                   "Caracter\u00edsticas de las viviendas que habitan los integrantes del hogar")

  registro <- registro_construir(
    data.frame(proyecto = "enigh_nc", edicion = "2014", pagina = "enigh/nc/2014"),
    silencioso = TRUE
  )
  expect_identical(nrow(registro), 49L)
  expect_length(attr(registro, "fallas"), 0)
  expect_true(all(startsWith(registro$url, getOption("importinegi.base"))))
})

test_that("registro_construir() valida la tabla de proyectos", {
  expect_error(registro_construir(data.frame(proyecto = "enoe")), "debe ser una tabla con las columnas")
})

test_that("la lista de proyectos del paquete tiene el formato esperado", {
  archivo <- test_path("..", "..", "data-raw", "proyectos.csv")
  skip_if_not(file.exists(archivo), "data-raw no se incluye en el paquete construido")
  proyectos <- utils::read.csv(archivo, colClasses = "character")
  expect_named(proyectos, c("proyecto", "edicion", "pagina"))
  expect_false(anyDuplicated(proyectos$pagina) > 0)
  expect_true(all(grepl("^[a-z0-9_/]+$", proyectos$pagina)))
})
