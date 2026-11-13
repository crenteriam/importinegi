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
  # Exportacion del catalogo de la Red Nacional de Metadatos
  app$locals$consultas_catalogo <- 0
  app$locals$ultimo_ps <- ""
  app$get("/rnm/index.php/catalog/export/csv", function(req, res) {
    locales <- req$app$locals
    locales$consultas_catalogo <- locales$consultas_catalogo + 1
    locales$ultimo_ps <- req$query$ps
    # El servidor real envia esta cabecera, que no es valida (Content-Encoding
    # es para compresion): los navegadores la ignoran, pero curl la rechaza si
    # no se le indica que no decodifique el contenido.
    res$set_header("Content-Encoding", "UTF-8")
    res$set_header("Content-Disposition", "attachment;filename=search.csv")
    res$send_file("catalogo_rnm.csv", root = locales$carpeta)
  })
  app$get("/datos/codificacion_invalida.csv", function(req, res) {
    res$set_header("Content-Encoding", "UTF-8")
    res$send_file("utf8_bom.csv", root = req$app$locals$carpeta)
  })
  app$get("/cabeceras", function(req, res) {
    res$send(paste0("Accept-Encoding: ", req$get_header("Accept-Encoding")))
  })
  app$get("/rnm/consultas", function(req, res) {
    res$send(paste(req$app$locals$consultas_catalogo, req$app$locals$ultimo_ps))
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
  # Archivos del Censo de Poblacion y Vivienda, en las rutas del registro del
  # paquete. Responde con las muestras de fixtures/censo para Colima (06),
  # Nayarit (18) y el archivo nacional; para el resto, como el INEGI cuando un
  # archivo no existe: codigo 200 y una pagina HTML.
  app$locals$descargas_censo <- 0
  app$get(webfakes::new_regexp("^/contenidos/programas/ccpv/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_censo <- locales$descargas_censo + 1
    reglas <- c(
      ageb = "/iter/+ageb_manzana/+([0-9]{2})_",
      iter = "/microdatos/iter/([0-9]{2})_",
      muestra2010 = "/mpv/MC2010_([0-9]{2})_",
      muestra2000 = "/muestra/cgpv2000_([0-9]{2})_",
      muestra1990 = "/cgpv90p_([0-9]{2})_",
      localidades = "/cinco_mil_menos/resloc_([0-9]{2}|nal)_",
      entorno = "/urbano/MANZANA_EU_([0-9]{2})_",
      iter2020 = "/datosabiertos/iter/iter_([0-9]{2})_cpv2020_|/microdatos/iter/ITER_([0-9]{2}|NAL)_2020_",
      entorno2020 = "/microdatos/ceu/Censo2020_CEU_([a-z]+)_",
      asistencia2020 = "/microdatos/caas/Censo2020_CAAS_([a-z]+)_"
    )
    # En 2020 algunos archivos se nombran con la abreviatura de la entidad
    abreviaturas <- c(col = "06", nay = "18", eum = "00")
    archivo <- NA_character_
    for (nombre in names(reglas)) {
      hallado <- regmatches(req$path, regexec(reglas[[nombre]], req$path))[[1]]
      clave <- hallado[-1][nzchar(hallado[-1])]
      if (length(clave) == 1) {
        clave <- sub("^nal$", "00", tolower(clave))
        if (clave %in% names(abreviaturas)) {
          clave <- abreviaturas[[clave]]
        }
        archivo <- paste0(nombre, "_", clave, ".zip")
        break
      }
    }
    carpeta <- file.path(locales$carpeta, "censo")
    if (!is.na(archivo) && file.exists(file.path(carpeta, archivo))) {
      res$send_file(archivo, root = carpeta)
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  app$get("/censo/descargas", function(req, res) {
    res$send(as.character(req$app$locals$descargas_censo))
  })
  webfakes::local_app_process(app, .local_envir = entorno)
}

# Servidor local con las muestras del censo y sin esperas entre reintentos.
censo_local <- function(entorno = parent.frame()) {
  servidor <- servidor_prueba(entorno)
  withr::local_options(
    importinegi.base = sub("/$", "", servidor$url("/")),
    importinegi.espera = 0,
    importinegi.cache = FALSE,
    importinegi.registro = NULL,
    .local_envir = entorno
  )
  servidor
}

# Numero de archivos del censo que se han pedido al servidor local.
descargas_censo <- function(servidor) {
  as.integer(httr2::resp_body_string(httr2::req_perform(httr2::request(servidor$url("/censo/descargas")))))
}

# Las pruebas en vivo consultan el sitio real del INEGI. Solo corren cuando la
# variable de entorno IMPORTINEGI_EN_VIVO vale "true" (ver
# .github/workflows/pruebas-en-vivo.yaml).
omitir_si_no_en_vivo <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("IMPORTINEGI_EN_VIVO"), "true"),
    "Las pruebas en vivo solo corren con IMPORTINEGI_EN_VIVO=true"
  )
}
