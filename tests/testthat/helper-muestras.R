# Archivos de muestra para las pruebas (tests/testthat/fixtures).
muestra <- function(nombre) {
  testthat::test_path("fixtures", nombre)
}

leer_muestra <- function(nombre) {
  paste(readLines(muestra(nombre), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Servidor HTTP local que imita las respuestas del sitio del INEGI.
servidor_prueba <- function(entorno = parent.frame()) {
  testthat::skip_if_not_installed("webfakes")
  testthat::skip_on_cran()
  app <- webfakes::new_app()
  app$locals$carpeta <- normalizePath(testthat::test_path("fixtures"))
  app$locals$solicitudes <- 0

  # Archivo que si existe
  app$get("/datos/muestra_dbf.zip", function(req, res) {
    res$send_file("muestra_dbf.zip", root = req$app$locals$carpeta)
  })
  # Archivo inexistente: el INEGI responde 200 con una pagina HTML
  app$get("/datos/no_existe.zip", function(req, res) {
    res$set_status(200L)
    res$set_type("text/html")
    res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
  })
  app$get("/datos/404.zip", function(req, res) res$send_status(404L))
  app$get("/datos/403.zip", function(req, res) res$send_status(403L))
  app$get("/datos/500.zip", function(req, res) res$send_status(500L))
  # Falla dos veces con 503 y funciona a la tercera
  app$get("/datos/inestable.zip", function(req, res) {
    locales <- req$app$locals
    locales$solicitudes <- locales$solicitudes + 1
    if (locales$solicitudes < 3) {
      res$send_status(503L)
    } else {
      res$send_file("muestra_dbf.zip", root = locales$carpeta)
    }
  })
  app$get("/solicitudes", function(req, res) {
    res$send(as.character(req$app$locals$solicitudes))
  })
  app$get("/agente", function(req, res) {
    res$send(req$get_header("User-Agent"))
  })
  app$get("/pagina", function(req, res) {
    res$send_file("pagina_programa.html", root = req$app$locals$carpeta)
  })
  # Paginas de programa y servicio de descargas, como en el sitio del INEGI
  app$get(webfakes::new_regexp("^/programas/.+/$"), function(req, res) {
    res$send_file("pagina_programa.html", root = req$app$locals$carpeta)
  })
  app$get(
    "/app/api/descarga/componente/descargamasiva/lista/archivoscompaginacion",
    function(req, res) {
      if (identical(req$query$idBiinegi, "1780") && identical(req$query$tipodocto, "4")) {
        res$send_file("api_1780.json", root = req$app$locals$carpeta)
      } else {
        res$set_type("application/json")
        res$send("[]")
      }
    }
  )
  webfakes::local_app_process(app, .local_envir = entorno)
}
