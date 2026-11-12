# catalogo_rnm.csv es una muestra real de la exportacion del catalogo de la Red
# Nacional de Metadatos: 8 de sus 825 proyectos, tal como los entrego el INEGI
# el 11 de noviembre de 2026 (UTF-8 con BOM, los mas recientes primero).

# Servidor local con el catalogo de muestra y memoria de la sesion limpia.
catalogo_local <- function(entorno = parent.frame()) {
  servidor <- servidor_prueba(entorno)
  withr::local_options(
    importinegi.base = sub("/$", "", servidor$url("/")),
    importinegi.espera = 0,
    .local_envir = entorno
  )
  memoria_borrar("catalogo_rnm")
  withr::defer(memoria_borrar("catalogo_rnm"), envir = entorno)
  servidor
}

test_that("catalogo_inegi() devuelve el catalogo completo como tabla", {
  servidor <- catalogo_local()
  expect_message(
    catalogo <- catalogo_inegi(),
    "Descargando el cat\u00e1logo de la Red Nacional de Metadatos"
  )
  expect_s3_class(catalogo, "data.frame")
  expect_identical(nrow(catalogo), 8L)
  expect_named(catalogo, c("id", "idno", "title", "nation", "authoring_entity",
                           "year_start", "year_end", "created", "changed", "url"))
  expect_type(catalogo$id, "integer")
  expect_type(catalogo$year_start, "integer")
  expect_type(catalogo$idno, "character")
  expect_type(catalogo$year_end, "integer")
  expect_identical(catalogo$id, c(1121L, 983L, 489L, 632L, 334L, 901L, 1110L, 296L))
  expect_identical(catalogo$title[4], "CENSO DE POBLACI\u00d3N Y VIVIENDA 2020")
  expect_identical(
    catalogo$title[3],
    "Encuesta Nacional de Empresas Constructoras 2019, Datos al mes de diciembre (2013=100)"
  )
  expect_identical(catalogo$idno[3], "MEX-INEGI.EEC3.01-ENEC-2019")
  expect_identical(c(catalogo$year_start[8], catalogo$year_end[8]), c(1985L, 2001L))
  # Las fechas llegan como texto ("Aug-20-25") y se convierten a fecha
  expect_s3_class(catalogo$created, "Date")
  expect_s3_class(catalogo$changed, "Date")
  expect_identical(catalogo$created[1], as.Date("2025-08-20"))
  expect_identical(catalogo$changed[8], as.Date("2022-10-05"))
  expect_identical(catalogo$id[order(catalogo$created)][1], 296L)
  expect_false(anyNA(catalogo))
  expect_identical(
    catalogo$url[3],
    paste0(getOption("importinegi.base"), "/rnm/index.php/catalog/489")
  )
  # Pide el catalogo completo y no solo 500 proyectos
  expect_identical(inegi_texto(servidor$url("/rnm/consultas")), "1 5000")
})

test_that("id = NA e id = NULL equivalen a pedir el catalogo", {
  catalogo_local()
  expect_identical(nrow(suppressMessages(catalogo_inegi(id = NA))), 8L)
  expect_identical(nrow(catalogo_inegi(id = NULL)), 8L)
})

test_that("el catalogo se descarga una sola vez por sesion", {
  servidor <- catalogo_local()
  expect_message(primera <- catalogo_inegi(), "Descargando")
  expect_no_message(segunda <- catalogo_inegi())
  expect_identical(primera, segunda)
  suppressMessages(catalogo_inegi(buscar = "censo"))
  expect_identical(inegi_texto(servidor$url("/rnm/consultas")), "1 5000")

  # Al borrar la memoria se vuelve a descargar
  memoria_borrar("catalogo_rnm")
  expect_message(catalogo_inegi(), "Descargando")
  expect_identical(inegi_texto(servidor$url("/rnm/consultas")), "2 5000")
})

test_that("buscar filtra por nombre sin distinguir mayusculas ni acentos", {
  catalogo_local()
  suppressMessages(catalogo_inegi())
  expect_identical(catalogo_inegi(buscar = "ocupacion")$id, c(1121L, 983L))
  expect_identical(catalogo_inegi(buscar = "OCUPACI\u00d3N")$id, c(1121L, 983L))
  expect_identical(catalogo_inegi(buscar = "m\u00f3dulo")$id, 1110L)
  # El INEGI escribe algunos nombres en mayusculas
  expect_identical(catalogo_inegi(buscar = "censo")$id, c(632L, 334L))
})

test_that("buscar con varias palabras exige todas, en cualquier orden", {
  catalogo_local()
  suppressMessages(catalogo_inegi())
  expect_identical(catalogo_inegi(buscar = "empleo ocupacion 2024")$id, 983L)
  expect_identical(catalogo_inegi(buscar = c("censo", "vivienda"))$id, 632L)
  expect_identical(catalogo_inegi(buscar = "ocupaci\u00f3n y empleo")$id, c(1121L, 983L))
})

test_that("buscar tambien encuentra las siglas de la clave del proyecto", {
  catalogo_local()
  suppressMessages(catalogo_inegi())
  expect_identical(catalogo_inegi(buscar = "enoe")$id, c(1121L, 983L))
  expect_identical(catalogo_inegi(buscar = "MOCIBA")$id, 1110L)
  expect_identical(catalogo_inegi(buscar = "enigh")$id, 901L)
  expect_identical(catalogo_inegi(buscar = "cngmd 2017")$id, 334L)
})

test_that("una busqueda sin resultados devuelve una tabla vacia y lo avisa", {
  catalogo_local()
  suppressMessages(catalogo_inegi())
  expect_message(
    vacio <- catalogo_inegi(buscar = "astronomia"),
    "Ning\u00fan proyecto del cat\u00e1logo coincide con la b\u00fasqueda"
  )
  expect_identical(nrow(vacio), 0L)
  expect_named(vacio, names(catalogo_inegi()))
})

test_that("year conserva los proyectos cuyo periodo incluye el a\u00f1o", {
  catalogo_local()
  suppressMessages(catalogo_inegi())
  expect_identical(catalogo_inegi(year = 2019)$id, c(489L, 632L))
  expect_identical(catalogo_inegi(year = 2025)$id, c(1121L, 1110L))
  expect_identical(catalogo_inegi(year = 1990)$id, 296L)
  expect_identical(catalogo_inegi(year = c(2022, 2024))$id, c(983L, 901L, 1110L))
  expect_identical(nrow(suppressMessages(catalogo_inegi(year = 1950))), 0L)
})

test_that("buscar y year se pueden combinar", {
  catalogo_local()
  suppressMessages(catalogo_inegi())
  resultado <- catalogo_inegi(buscar = "encuesta", year = 2024)
  expect_identical(resultado$id, 983L)
  expect_identical(rownames(resultado), "1")
})

test_that("buscar y year invalidos se rechazan con un mensaje claro", {
  catalogo_local()
  suppressMessages(catalogo_inegi())
  expect_error(catalogo_inegi(buscar = 2020), "`buscar` debe ser un texto")
  expect_error(catalogo_inegi(buscar = ""), "`buscar` debe ser un texto")
  expect_error(catalogo_inegi(buscar = NA_character_), "`buscar` debe ser un texto")
  expect_error(catalogo_inegi(year = "2020"), "`year` debe ser un a\u00f1o")
  expect_error(catalogo_inegi(year = 2020.5), "`year` debe ser un a\u00f1o")
  expect_error(catalogo_inegi(year = NA_real_), "`year` debe ser un a\u00f1o")
})

test_that("con id se obtiene la direccion de la ficha, en cualquier sistema operativo", {
  skip_if(interactive(), "Solo aplica a sesiones no interactivas")
  expect_message(
    url <- catalogo_inegi(id = 489),
    "Consulta la informaci\u00f3n en: https://www.inegi.org.mx/rnm/index.php/catalog/489"
  )
  expect_identical(url, "https://www.inegi.org.mx/rnm/index.php/catalog/489")
  expect_invisible(suppressMessages(catalogo_inegi(id = 489)))
  expect_identical(
    suppressMessages(catalogo_inegi(id = "443")),
    "https://www.inegi.org.mx/rnm/index.php/catalog/443"
  )
  expect_identical(
    suppressMessages(catalogo_inegi(id = 864L)),
    "https://www.inegi.org.mx/rnm/index.php/catalog/864"
  )
})

test_that("un id invalido se rechaza con un mensaje claro", {
  mensaje <- "`id` debe ser el n\u00famero de identificaci\u00f3n de un proyecto"
  expect_error(catalogo_inegi(id = c(489, 864)), mensaje)
  expect_error(catalogo_inegi(id = "enoe"), mensaje)
  expect_error(catalogo_inegi(id = 48.9), mensaje)
  expect_error(catalogo_inegi(id = -3), mensaje)
  expect_error(catalogo_inegi(id = 0), mensaje)
  expect_error(catalogo_inegi(id = TRUE), mensaje)
  expect_error(catalogo_inegi(id = c(NA, NA)), mensaje)
})

test_that("id no se combina con buscar ni con year", {
  expect_error(catalogo_inegi(id = 489, buscar = "empleo"), "no se combina con `buscar` ni con `year`")
  expect_error(catalogo_inegi(id = 489, year = 2019), "no se combina con `buscar` ni con `year`")
})

test_that("se avisa si el catalogo llega al maximo de proyectos que se pide", {
  servidor <- catalogo_local()
  local_mocked_bindings(catalogo_tamano = function() 8L)
  expect_warning(
    catalogo <- suppressMessages(catalogo_inegi()),
    "tiene 8 proyectos, el m\u00e1ximo que se pide al INEGI, y podr\u00eda estar incompleto"
  )
  expect_identical(nrow(catalogo), 8L)
  expect_identical(inegi_texto(servidor$url("/rnm/consultas")), "1 8")
})

test_that("si el INEGI responde con una pagina en lugar del catalogo, se avisa", {
  memoria_borrar("catalogo_rnm")
  withr::defer(memoria_borrar("catalogo_rnm"))
  local_mocked_bindings(inegi_descargar = function(url, destino, ...) {
    file.copy(muestra("no_encontrada.html"), destino)
    invisible(destino)
  })
  expect_error(
    suppressMessages(catalogo_inegi()),
    "El INEGI no devolvi\u00f3 el cat\u00e1logo de la Red Nacional de Metadatos"
  )
  # Una respuesta invalida no se guarda en la memoria de la sesion
  expect_null(memoria_obtener("catalogo_rnm"))
})

test_that("un catalogo vacio o sin las columnas esperadas tambien se rechaza", {
  memoria_borrar("catalogo_rnm")
  withr::defer(memoria_borrar("catalogo_rnm"))
  local_mocked_bindings(inegi_descargar = function(url, destino, ...) {
    writeLines("id,idno,title", destino)
    invisible(destino)
  })
  expect_error(suppressMessages(catalogo_inegi()), "no devolvi\u00f3 el cat\u00e1logo")

  local_mocked_bindings(inegi_descargar = function(url, destino, ...) {
    writeLines(c("clave,nombre", "1,Proyecto"), destino)
    invisible(destino)
  })
  expect_error(suppressMessages(catalogo_inegi()), "no devolvi\u00f3 el cat\u00e1logo")
})

test_that("una falla de conexion se reporta en espa\u00f1ol", {
  memoria_borrar("catalogo_rnm")
  withr::defer(memoria_borrar("catalogo_rnm"))
  withr::local_options(
    importinegi.base = "http://127.0.0.1:9",
    importinegi.espera = 0, importinegi.intentos = 2
  )
  expect_error(
    suppressMessages(catalogo_inegi()),
    "No se pudo completar la solicitud.*Revisa tu conexi\u00f3n a internet"
  )
})

test_that("catalogo_fecha() convierte el formato del INEGI sin depender del idioma del sistema", {
  expect_identical(
    catalogo_fecha(c("Aug-20-25", "Jan-23-21", "Dec-31-13", "apr-01-19")),
    as.Date(c("2025-08-20", "2021-01-23", "2013-12-31", "2019-04-01"))
  )
  expect_identical(catalogo_fecha(c("Nov-01-17", NA)), as.Date(c("2017-11-01", NA)))
  expect_identical(catalogo_fecha(c("Jan-01-00", "Jan-01-99")), as.Date(c("2000-01-01", "1999-01-01")))
  expect_identical(catalogo_fecha(character()), as.Date(character()))
})

test_that("catalogo_fecha() deja el texto original si algun valor no es una fecha valida", {
  otro_formato <- c("Aug-20-25", "2025-08-20")
  expect_identical(catalogo_fecha(otro_formato), otro_formato)
  mes_en_espanol <- c("Ago-20-25", "Ene-05-24")
  expect_identical(catalogo_fecha(mes_en_espanol), mes_en_espanol)
  dia_invalido <- c("Feb-30-25", "Mar-07-25")
  expect_identical(catalogo_fecha(dia_invalido), dia_invalido)
  expect_identical(catalogo_fecha(c("", "Mar-07-25")), c("", "Mar-07-25"))
  # Lo que no es texto se devuelve sin cambios
  expect_identical(catalogo_fecha(20250820L), 20250820L)
})

test_that("si el INEGI cambia el formato de las fechas, el catalogo las conserva como texto", {
  memoria_borrar("catalogo_rnm")
  withr::defer(memoria_borrar("catalogo_rnm"))
  local_mocked_bindings(inegi_descargar = function(url, destino, ...) {
    writeLines(c("id,idno,title,year_start,year_end,created,changed",
                 "1,MEX-A,Proyecto A,2020,2021,2021-03-07,Mar-07-22"), destino)
    invisible(destino)
  })
  catalogo <- suppressMessages(catalogo_inegi())
  expect_identical(catalogo$created, "2021-03-07")
  expect_identical(catalogo$changed, as.Date("2022-03-07"))
})
