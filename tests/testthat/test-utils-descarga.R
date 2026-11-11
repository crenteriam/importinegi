test_that("inegi_url() completa las rutas y pasa a https las direcciones del INEGI", {
  expect_identical(
    inegi_url("/contenidos/programas/enoe/15ymas/microdatos/2019trim1_dbf.zip"),
    "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/2019trim1_dbf.zip"
  )
  expect_identical(inegi_url("programas/enoe/15ymas/"), "https://www.inegi.org.mx/programas/enoe/15ymas/")
  expect_identical(
    inegi_url("http://www.inegi.org.mx/contenidos/x.zip"),
    "https://www.inegi.org.mx/contenidos/x.zip"
  )
  expect_identical(
    inegi_url("http://internet.contenidos.inegi.org.mx/contenidos/x.zip"),
    "https://internet.contenidos.inegi.org.mx/contenidos/x.zip"
  )
  expect_identical(inegi_url("https://www.inegi.org.mx/a"), "https://www.inegi.org.mx/a")
  expect_identical(inegi_url("http://127.0.0.1:8080/a.zip"), "http://127.0.0.1:8080/a.zip")
  expect_identical(inegi_url(c("/a", "http://www.inegi.org.mx/b")),
                   c("https://www.inegi.org.mx/a", "https://www.inegi.org.mx/b"))
})

test_that("es_zip() distingue un zip real de una pagina HTML", {
  expect_true(es_zip(muestra("muestra_dbf.zip")))
  expect_false(es_zip(muestra("no_encontrada.html")))
  expect_false(es_zip(file.path(tempdir(), "archivo_que_no_existe.zip")))
  vacio <- tempfile()
  file.create(vacio)
  expect_false(es_zip(vacio))
})

test_that("ruta_cache() arma una ruta segura dentro de la carpeta de copia local", {
  withr::local_options(importinegi.cache_dir = file.path(tempdir(), "copia"))
  ruta <- ruta_cache("https://www.inegi.org.mx/contenidos/programas/enoe/2019trim1_dbf.zip?a=1&b=2")
  expect_identical(
    ruta,
    file.path(tempdir(), "copia", "contenidos/programas/enoe/2019trim1_dbf.zip_a_1_b_2")
  )
})

test_that("inegi_descargar() descarga un zip valido y devuelve su ruta", {
  servidor <- servidor_prueba()
  expect_message(
    ruta <- inegi_descargar(servidor$url("/datos/muestra_dbf.zip"), tamano = "1 KB"),
    "Descargando muestra_dbf.zip \\(1 KB\\)"
  )
  expect_true(file.exists(ruta))
  expect_true(es_zip(ruta))
  expect_identical(unname(tools::md5sum(ruta)), unname(tools::md5sum(muestra("muestra_dbf.zip"))))
  expect_match(ruta, "\\.zip$")
  expect_false(file.exists(paste0(ruta, ".parcial")))
})

test_that("inegi_descargar() respeta el destino y el modo silencioso", {
  servidor <- servidor_prueba()
  destino <- file.path(tempfile("carpeta_nueva_"), "mi_archivo.zip")
  expect_silent(ruta <- inegi_descargar(servidor$url("/datos/muestra_dbf.zip"),
                                        destino = destino, silencioso = TRUE))
  expect_identical(ruta, destino)
  expect_true(es_zip(destino))
})

test_that("inegi_descargar() detecta la pagina 'no encontrada' que el INEGI envia con codigo 200", {
  servidor <- servidor_prueba()
  destino <- tempfile(fileext = ".zip")
  expect_error(
    inegi_descargar(servidor$url("/datos/no_existe.zip"), destino = destino, silencioso = TRUE),
    "El INEGI no tiene publicado el archivo .*no_existe.zip"
  )
  expect_false(file.exists(destino))
  expect_false(file.exists(paste0(destino, ".parcial")))
})

test_that("inegi_descargar() explica los codigos de error del servidor", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.espera = 0)
  expect_error(
    inegi_descargar(servidor$url("/datos/404.zip"), silencioso = TRUE),
    "El INEGI no tiene publicado el archivo"
  )
  expect_error(
    inegi_descargar(servidor$url("/datos/403.zip"), silencioso = TRUE),
    "El INEGI respondi\u00f3 con el c\u00f3digo 403"
  )
  expect_error(
    inegi_descargar(servidor$url("/datos/500.zip"), silencioso = TRUE),
    "despu\u00e9s de 3 intentos: el servidor respondi\u00f3 con el c\u00f3digo 500"
  )
})

test_that("inegi_descargar() reintenta cuando el servidor falla temporalmente", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.espera = 0)
  ruta <- inegi_descargar(servidor$url("/datos/inestable.zip"), silencioso = TRUE)
  expect_true(es_zip(ruta))
  expect_identical(inegi_texto(servidor$url("/solicitudes")), "3")
})

test_that("inegi_descargar() se detiene despues del numero de intentos configurado", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.espera = 0, importinegi.intentos = 2)
  expect_error(
    inegi_descargar(servidor$url("/datos/inestable.zip"), silencioso = TRUE),
    "despu\u00e9s de 2 intentos"
  )
  expect_identical(inegi_texto(servidor$url("/solicitudes")), "2")
})

test_that("inegi_descargar() da un mensaje claro cuando no hay conexion", {
  testthat::skip_on_cran()
  withr::local_options(importinegi.espera = 0, importinegi.intentos = 2)
  expect_error(
    inegi_descargar("http://127.0.0.1:9/datos/archivo.zip", silencioso = TRUE),
    "No se pudo completar la solicitud a http://127.0.0.1:9/datos/archivo.zip despu\u00e9s de 2 intentos.*Revisa tu conexi\u00f3n"
  )
})

test_that("la copia local evita descargar dos veces el mismo archivo", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.cache_dir = tempfile("copia_"), importinegi.espera = 0)
  url <- servidor$url("/datos/muestra_dbf.zip")
  expect_message(primera <- inegi_descargar(url, cache = TRUE), "Descargando")
  expect_true(startsWith(primera, getOption("importinegi.cache_dir")))
  expect_message(segunda <- inegi_descargar(url, cache = TRUE), "Usando la copia local")
  expect_identical(primera, segunda)

  # Una copia local danada no se reutiliza: se descarga de nuevo
  writeLines("<html>", primera)
  expect_message(tercera <- inegi_descargar(url, cache = TRUE), "Descargando")
  expect_true(es_zip(tercera))
})

test_that("sin pedirlo, no se escribe nada en la carpeta de copia local", {
  servidor <- servidor_prueba()
  carpeta <- tempfile("copia_")
  withr::local_options(importinegi.cache_dir = carpeta)
  inegi_descargar(servidor$url("/datos/muestra_dbf.zip"), silencioso = TRUE)
  expect_false(dir.exists(carpeta))
})

test_that("inegi_texto() devuelve el contenido y se identifica como importinegi", {
  servidor <- servidor_prueba()
  expect_match(inegi_texto(servidor$url("/pagina")), "<menu-gen idm=\"1780\"", fixed = TRUE)
  expect_match(inegi_texto(servidor$url("/pagina")), "Nueva construcci\u00f3n", fixed = TRUE)
  expect_match(inegi_texto(servidor$url("/agente")), "^importinegi/[0-9.]+ ")
  expect_error(inegi_texto(servidor$url("/datos/403.zip")), "c\u00f3digo 403")
})
