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
  # Archivos de las Encuestas Intercensales (2015 y 2025), en las rutas del
  # registro del paquete. Responde con las muestras de fixtures/intercensal para
  # Aguascalientes (01), Colima (06), el archivo nacional de la muestra de 2025 y
  # el XLSX de los resultados de 2025; para el resto, como el INEGI cuando un
  # archivo no existe.
  app$get(webfakes::new_regexp("^/contenidos/programas/(intercensal/2015|eic/2025)/microdatos/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_censo <- locales$descargas_censo + 1
    reglas <- c(
      muestra2015 = "/intercensal/2015/microdatos/eic2015_([0-9]{2})_csv[.]zip$",
      muestra2025 = "/eic/2025/microdatos/eic2025_micro_([0-9]{2})_csv[.]zip$",
      resultados2025 = "/eic/2025/microdatos/eic2025_micro_(105)[.]xlsx$"
    )
    archivo <- NA_character_
    for (nombre in names(reglas)) {
      hallado <- regmatches(req$path, regexec(reglas[[nombre]], req$path))[[1]]
      if (length(hallado) == 2) {
        archivo <- if (nombre == "resultados2025") "resultados2025_00.xlsx" else paste0(nombre, "_", hallado[2], ".zip")
        break
      }
    }
    carpeta <- file.path(locales$carpeta, "intercensal")
    if (!is.na(archivo) && file.exists(file.path(carpeta, archivo))) {
      res$send_file(archivo, root = carpeta)
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  # Archivos de los Conteos de Poblacion y Vivienda (1995 y 2005), en las rutas
  # del registro del paquete. Responde con las muestras de fixtures/conteo para
  # Aguascalientes (01), Colima (06) y el archivo nacional; para el resto, como
  # el INEGI cuando un archivo no existe. Va antes que la ruta del censo, que
  # atiende las demas direcciones de ccpv.
  app$get(webfakes::new_regexp("^/contenidos/programas/ccpv/(1995|2005)/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_censo <- locales$descargas_censo + 1
    year <- sub("^/contenidos/programas/ccpv/([0-9]{4})/.*$", "\\1", req$path)
    reglas <- c(
      iter = "/microdatos/iter/([0-9]{2})_",
      muestra = "/microdatos/cpv95_([0-9]{2})_|/microdatos/muestra/cpv2005_([0-9]{2})_",
      viviendas = "/microdatos/viviendas/vivloc2005_([0-9]{2}|nal)_"
    )
    archivo <- NA_character_
    for (nombre in names(reglas)) {
      hallado <- regmatches(req$path, regexec(reglas[[nombre]], req$path))[[1]]
      clave <- hallado[-1][nzchar(hallado[-1])]
      if (length(clave) == 1) {
        archivo <- paste0(nombre, year, "_", sub("^nal$", "00", clave), ".zip")
        break
      }
    }
    carpeta <- file.path(locales$carpeta, "conteo")
    if (!is.na(archivo) && file.exists(file.path(carpeta, archivo))) {
      res$send_file(archivo, root = carpeta)
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
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
  # Archivos del Censo Nacional de Gobiernos Municipales, en las rutas del
  # registro del paquete. Responde con las muestras de fixtures/gobierno (un tema
  # de 2011 en CSV y otro en DBF, y uno de 2015, 2019, 2021 y 2025); para el
  # resto, como el INEGI cuando un archivo no existe.
  app$get(webfakes::new_regexp("^/contenidos/programas/cngmd/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_censo <- locales$descargas_censo + 1
    reglas <- c(
      cngmd2025_ayuntamientos = "/2025/datosabiertos/conjunto_de_datos_ayuntamientos_cngmd2025_csv[.]zip$",
      cngmd2021_alcaldias = "/2021/datosabiertos/m1/alcaldias_cdmx_cngmd2021_csv[.]zip$",
      cngmd2019_actividad = "/2019/datosabiertos/m1/actividad_ayuntami_cngmd2019_csv[.]zip$",
      cngmd2015_estructura = "/2015/datosabiertos/m1/conjunto_de_datos_estruct_aytto_cngmd2015_csv[.]zip$",
      cngmd2011_comisiones = "/2011/datosabiertos/ayu_comisiones_cngmd2011_csv[.]zip$",
      cngmd2011_agua = "/2011/microdatos/agua_cngmd2011_dbf[.]zip$"
    )
    archivo <- names(reglas)[vapply(reglas, function(regla) grepl(regla, req$path), logical(1))]
    if (length(archivo) == 1) {
      res$send_file(paste0(archivo, ".zip"), root = file.path(locales$carpeta, "gobierno"))
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  # Archivos del Censo Nacional de Derechos Humanos Federal, en las rutas del
  # registro del paquete. Responde con las muestras de
  # fixtures/cndhf (dos temas de 2025, uno de 2021 y uno de 2017); para el
  # resto, como el INEGI cuando un archivo no existe.
  app$get(webfakes::new_regexp("^/contenidos/programas/cndhf/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_censo <- locales$descargas_censo + 1
    reglas <- c(
      cndhf2025_consejo = "/2025/datosabiertos/conjunto_de_datos_consejo_consultivo_cndhf2025_csv[.]zip$",
      cndhf2025_hechos = "/2025/datosabiertos/m2/conjunto_de_datos_exploespec_hechoviola_dh_selec_cndhf2025_csv[.]zip$",
      cndhf2021_seguimiento = "/2021/datosabiertos/m2/seguimiento_cndhf2021_csv[.]zip$",
      cndhf2017_participacion = "/2017/datosabiertos/m1/Participacion_ciudada_cndhf2017_csv[.]zip$"
    )
    archivo <- names(reglas)[vapply(reglas, function(regla) grepl(regla, req$path), logical(1))]
    if (length(archivo) == 1) {
      res$send_file(paste0(archivo, ".zip"), root = file.path(locales$carpeta, "cndhf"))
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  # Archivos de la serie tradicional de la ENIGH, en las rutas del registro del
  # paquete. Responde con las muestras de fixtures/enigh (algunas tablas de las
  # ediciones 1984, 1994, 2006, 2008, 2010 y 2014); para el resto, como el INEGI
  # cuando un archivo no existe. La muestra se elige por la edicion y por el
  # nombre corto del archivo: la de
  # ".../2006/microdatos/Tra_Gastos_2006_concil_2010_dbf.zip" es
  # "trad2006_gastos.zip".
  app$locals$descargas_enigh <- 0
  app$get(webfakes::new_regexp("^/contenidos/programas/enigh/tradicional/[0-9]{4}/microdatos/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_enigh <- locales$descargas_enigh + 1
    year <- sub("^/contenidos/programas/enigh/tradicional/([0-9]{4})/.*$", "\\1", req$path)
    nombre <- tolower(sub("_dbf[.]zip$", "", basename(req$path), ignore.case = TRUE))
    nombre <- sub("_[0-9]{4}_concil_[0-9]{4}$", "", sub("^tra_", "", nombre))
    archivo <- paste0("trad", year, "_", nombre, ".zip")
    carpeta <- file.path(locales$carpeta, "enigh")
    if (file.exists(file.path(carpeta, archivo))) {
      res$send_file(archivo, root = carpeta)
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  app$get("/enigh/descargas", function(req, res) {
    res$send(as.character(req$app$locals$descargas_enigh))
  })
  app$get("/censo/descargas", function(req, res) {
    res$send(as.character(req$app$locals$descargas_censo))
  })
  webfakes::local_app_process(app, .local_envir = entorno)
}

# Servidor local con las muestras del censo, del conteo y de la Encuesta
# Intercensal, sin esperas entre reintentos. Empieza sin las descargas que una
# llamada fallida haya dejado en la carpeta temporal de la sesion (ver
# poblacion_leer_entidades()).
censo_local <- function(entorno = parent.frame()) {
  pendientes <- file.path(tempdir(), "importinegi", "descargas")
  unlink(pendientes, recursive = TRUE)
  withr::defer(unlink(pendientes, recursive = TRUE), envir = entorno)
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

# Numero de archivos del censo, del conteo y de la Encuesta Intercensal que se
# han pedido al servidor local.
descargas_censo <- function(servidor) {
  as.integer(httr2::resp_body_string(httr2::req_perform(httr2::request(servidor$url("/censo/descargas")))))
}

# Numero de archivos de la ENIGH que se han pedido al servidor local.
descargas_enigh <- function(servidor) {
  as.integer(httr2::resp_body_string(httr2::req_perform(httr2::request(servidor$url("/enigh/descargas")))))
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
