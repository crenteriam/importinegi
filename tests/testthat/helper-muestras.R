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
  # Para leer el cuerpo de las consultas en JSON (servicio de productos)
  app$use(webfakes::mw_json())
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
  # Paginas de programa y servicio de descargas, como en el sitio del INEGI.
  # La pagina de la ETOE esta en la seccion de investigacion.
  app$get(webfakes::new_regexp("^/(programas|investigacion)/.+/$"), function(req, res) {
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
  # fixtures/cndhf (dos temas de 2025, uno de 2021 y dos de 2017); para el
  # resto, como el INEGI cuando un archivo no existe.
  app$get(webfakes::new_regexp("^/contenidos/programas/cndhf/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_censo <- locales$descargas_censo + 1
    reglas <- c(
      cndhf2025_consejo = "/2025/datosabiertos/conjunto_de_datos_consejo_consultivo_cndhf2025_csv[.]zip$",
      cndhf2025_hechos = "/2025/datosabiertos/m2/conjunto_de_datos_exploespec_hechoviola_dh_selec_cndhf2025_csv[.]zip$",
      cndhf2021_seguimiento = "/2021/datosabiertos/m2/seguimiento_cndhf2021_csv[.]zip$",
      cndhf2017_participacion = "/2017/datosabiertos/m1/Participacion_ciudada_cndhf2017_csv[.]zip$",
      cndhf2017_capacitacion = "/2017/datosabiertos/m1/Capacitac_difusion_DH_cndhf2017_csv[.]zip$"
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
  # Archivos de la Nueva construccion de la ENIGH, con la misma regla: la
  # muestra de ".../2010/microdatos/NCV_Agropecuario_2010_concil_2010_dbf.zip"
  # es "nc2010_agropecuario.zip" y la del mismo archivo en CSV,
  # "nc2010_agropecuario_csv.zip".
  app$get(webfakes::new_regexp("^/contenidos/programas/enigh/nc/[0-9]{4}/microdatos/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_enigh <- locales$descargas_enigh + 1
    year <- sub("^/contenidos/programas/enigh/nc/([0-9]{4})/.*$", "\\1", req$path)
    formato <- tolower(sub("^.*_(dbf|csv)[.]zip$", "\\1", basename(req$path), ignore.case = TRUE))
    nombre <- tolower(sub("_(dbf|csv)[.]zip$", "", basename(req$path), ignore.case = TRUE))
    nombre <- sub("_[0-9]{4}_concil_[0-9]{4}$", "", sub("^ncv_", "", nombre))
    archivo <- paste0("nc", year, "_", nombre, if (identical(formato, "csv")) "_csv" else "", ".zip")
    carpeta <- file.path(locales$carpeta, "enigh")
    if (file.exists(file.path(carpeta, archivo))) {
      res$send_file(archivo, root = carpeta)
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  # Archivos de la Encuesta Nacional de Ocupacion y Empleo, en las rutas del
  # registro del paquete. Responde con las muestras de fixtures/enoe (2005-T1,
  # 2012-T2, 2013-T1, 2020-T3, 2024-T1 y 2026-T2); para el resto, como el INEGI
  # cuando un archivo no existe. La muestra se elige por el año y el trimestre
  # de la ruta: la de ".../microdatos/enoe_n_2020_trim3_dbf.zip" es
  # "enoe_2020_3.zip".
  app$locals$descargas_enoe <- 0
  app$get(webfakes::new_regexp("^/contenidos/programas/enoe/15ymas/microdatos/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_enoe <- locales$descargas_enoe + 1
    hallado <- regmatches(req$path, regexec("/microdatos/[a-z_]*([0-9]{4})_?trim([1-4])_dbf[.]zip$", req$path))[[1]]
    archivo <- if (length(hallado) == 3) paste0("enoe_", hallado[2], "_", hallado[3], ".zip") else NA_character_
    carpeta <- file.path(locales$carpeta, "enoe")
    if (!is.na(archivo) && file.exists(file.path(carpeta, archivo))) {
      res$send_file(archivo, root = carpeta)
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  # Archivos de la Encuesta Telefonica de Ocupacion y Empleo, en las rutas del
  # registro del paquete. Responde con las muestras de fixtures/etoe (abril en
  # las dos versiones de los ponderadores, y mayo y junio en la version del
  # Censo 2020); para el resto, como el INEGI cuando un archivo no existe. La
  # muestra de ".../microdatos/etoe_2020_abril_cpv2020_dbf.zip" es
  # "etoe_2020_abril_cpv2020.zip".
  app$locals$descargas_etoe <- 0
  app$get(webfakes::new_regexp("^/contenidos/investigacion/etoe/microdatos/"), function(req, res) {
    locales <- req$app$locals
    locales$descargas_etoe <- locales$descargas_etoe + 1
    archivo <- sub("_dbf[.]zip$", ".zip", basename(req$path))
    carpeta <- file.path(locales$carpeta, "etoe")
    if (grepl("^etoe_[0-9]{4}_[a-z0-9_]+[.]zip$", archivo) && file.exists(file.path(carpeta, archivo))) {
      res$send_file(archivo, root = carpeta)
    } else {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
    }
  })
  app$get("/etoe/descargas", function(req, res) {
    res$send(as.character(req$app$locals$descargas_etoe))
  })
  # Archivos del Marco Geoestadistico, con las muestras de fixtures/sig: los
  # productos completos ("producto_" mas el nombre del archivo) y la descarga
  # por capa ("capa_" mas el nombre del archivo). Como el servidor del INEGI,
  # atiende las solicitudes parciales (cabecera Range) con el codigo 206 y la
  # cabecera Content-Range, y responde con una pagina HTML y el codigo 200
  # cuando el archivo no existe. Lleva la cuenta de las solicitudes atendidas
  # y de los bytes enviados.
  app$locals$descargas_sig <- 0
  app$locals$bytes_sig <- 0
  app$locals$enviar_sig <- function(req, res, archivo) {
    locales <- req$app$locals
    ruta <- file.path(locales$carpeta, "sig", archivo)
    if (!file.exists(ruta)) {
      res$set_status(200L)
      res$set_type("text/html")
      res$send("<!DOCTYPE html><html><head><title>Pagina no encontrada</title></head></html>")
      return(invisible())
    }
    tamano <- file.size(ruta)
    bytes <- readBin(ruta, what = "raw", n = tamano)
    rango <- req$get_header("Range")
    res$set_header("Accept-Ranges", "bytes")
    res$set_type("application/x-zip-compressed")
    if (!is.null(rango) && grepl("^bytes=[0-9]*-[0-9]*$", rango) && rango != "bytes=-") {
      limites <- strsplit(sub("^bytes=", "", rango), "-", fixed = TRUE)[[1]]
      if (!nzchar(limites[1])) {
        # "-n": los ultimos n bytes
        desde <- max(0, tamano - as.numeric(limites[2]))
        hasta <- tamano - 1
      } else {
        desde <- as.numeric(limites[1])
        hasta <- if (length(limites) < 2) tamano - 1 else min(as.numeric(limites[2]), tamano - 1)
      }
      res$set_status(206L)
      res$set_header("Content-Range", sprintf("bytes %.0f-%.0f/%.0f", desde, hasta, tamano))
      bytes <- bytes[(desde + 1):(hasta + 1)]
    }
    locales$descargas_sig <- locales$descargas_sig + 1
    locales$bytes_sig <- locales$bytes_sig + length(bytes)
    res$send(bytes)
  }
  app$get(
    webfakes::new_regexp("^/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/"),
    function(req, res) req$app$locals$enviar_sig(req, res, paste0("producto_", basename(req$path)))
  )
  app$get(webfakes::new_regexp("^/contenidos/descargadenue/MGdescarga/MGN[0-9]{4}/"), function(req, res) {
    req$app$locals$enviar_sig(req, res, paste0("capa_", basename(req$path)))
  })
  # Servicio de productos del INEGI: responde con la muestra a la consulta de
  # la coleccion del Marco Geoestadistico y, a cualquier otra, como el servicio
  # real cuando faltan parametros.
  app$post("/app/api/productos/interna_v2/componente/mapas/lista/resultados/", function(req, res) {
    consulta <- req$json
    if (identical(consulta$tema, "213") && identical(consulta$titg, "4328")) {
      res$send_file(file.path("sig", "api_productos.json"), root = req$app$locals$carpeta)
    } else {
      res$set_type("application/json")
      res$send("{\"success\":false,\"error\":true,\"message\":\"Par\u00e1metros de consulta nulos.\"}")
    }
  })
  app$get("/sig/descargas", function(req, res) {
    res$send(paste(req$app$locals$descargas_sig, req$app$locals$bytes_sig))
  })
  app$get("/enoe/descargas", function(req, res) {
    res$send(as.character(req$app$locals$descargas_enoe))
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

# Numero de archivos de la ENOE que se han pedido al servidor local.
descargas_enoe <- function(servidor) {
  as.integer(httr2::resp_body_string(httr2::req_perform(httr2::request(servidor$url("/enoe/descargas")))))
}

# Numero de archivos de la ETOE que se han pedido al servidor local.
descargas_etoe <- function(servidor) {
  as.integer(httr2::resp_body_string(httr2::req_perform(httr2::request(servidor$url("/etoe/descargas")))))
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

# Servidor local con las muestras del Marco Geoestadistico. Empieza y termina
# sin las descargas que sig_marcogeoestadistico() conserva durante la sesion y
# sin los indices de zip remotos que guarda en la memoria.
marcogeo_local <- function(entorno = parent.frame()) {
  testthat::skip_if_not_installed("sf")
  limpiar <- function() {
    unlink(file.path(tempdir(), "importinegi", "marcogeo"), recursive = TRUE)
    memoria_borrar()
  }
  limpiar()
  withr::defer(limpiar(), envir = entorno)
  servidor <- servidor_prueba(entorno)
  withr::local_options(
    importinegi.base = sub("/$", "", servidor$url("/")),
    importinegi.espera = 0,
    importinegi.cache = FALSE,
    .local_envir = entorno
  )
  servidor
}

# Solicitudes atendidas y bytes enviados por el servidor local en los archivos
# del Marco Geoestadistico.
descargas_sig <- function(servidor) {
  texto <- httr2::resp_body_string(httr2::req_perform(httr2::request(servidor$url("/sig/descargas"))))
  valores <- as.numeric(strsplit(texto, " ", fixed = TRUE)[[1]])
  list(solicitudes = valores[1], bytes = valores[2])
}

# Comprueba que el resultado sea un solo mapa: un objeto de clase sf (no una
# lista de mapas), con una sola columna de geometria.
esperar_un_mapa <- function(mapa) {
  testthat::expect_s3_class(mapa, "sf")
  testthat::expect_s3_class(mapa, "data.frame")
  testthat::expect_identical(sum(vapply(mapa, inherits, logical(1), what = "sfc")), 1L)
  testthat::expect_length(attr(mapa, "sf_column"), 1L)
  invisible(mapa)
}
