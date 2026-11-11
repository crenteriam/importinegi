# Utilidades internas: descargas del sitio del INEGI ---------------------------
#
# Opciones que el usuario puede ajustar con options():
#   importinegi.timeout   Segundos maximos por descarga (por omision, 7200).
#   importinegi.intentos  Numero de intentos por descarga (por omision, 3).
#   importinegi.espera    Segundos de espera entre intentos (por omision, 2).
#   importinegi.cache     TRUE para conservar las descargas entre sesiones.
#   importinegi.cache_dir Carpeta de la copia local.
#   importinegi.base      Direccion base del sitio (solo para pruebas).

inegi_base <- function() {
  getOption("importinegi.base", "https://www.inegi.org.mx")
}

# Convierte una ruta del sitio en una URL completa y pasa a https las
# direcciones antiguas del INEGI que usaban http.
inegi_url <- function(ruta) {
  ruta <- as.character(ruta)
  es_url <- grepl("^https?://", ruta)
  ruta[es_url] <- sub("^http://([^/]*inegi\\.org\\.mx)", "https://\\1", ruta[es_url])
  ruta[!es_url] <- paste0(inegi_base(), "/", sub("^/+", "", ruta[!es_url]))
  ruta
}

# Solicitud base: agente de usuario del paquete, limites de tiempo y sin
# convertir en error los codigos HTTP (se revisan despues, con mensajes propios).
inegi_solicitud <- function(url) {
  agente <- paste0(
    "importinegi/", utils::packageVersion("importinegi"),
    " (https://github.com/crenteriam/importinegi)"
  )
  solicitud <- httr2::request(url)
  solicitud <- httr2::req_user_agent(solicitud, agente)
  solicitud <- httr2::req_options(
    solicitud,
    connecttimeout = 30,
    low_speed_limit = 1,
    low_speed_time = 60,
    timeout = getOption("importinegi.timeout", 7200)
  )
  httr2::req_error(solicitud, is_error = function(respuesta) FALSE)
}

# Ejecuta la solicitud. Reintenta cuando falla la conexion o cuando el servidor
# responde con un error temporal.
inegi_ejecutar <- function(solicitud, destino = NULL) {
  intentos <- getOption("importinegi.intentos", 3)
  espera <- getOption("importinegi.espera", 2)
  temporales <- c(408, 429, 500, 502, 503, 504)
  motivo <- ""

  for (intento in seq_len(intentos)) {
    respuesta <- tryCatch(
      httr2::req_perform(solicitud, path = destino),
      error = function(e) e
    )
    if (inherits(respuesta, "error")) {
      motivo <- conditionMessage(respuesta)
    } else {
      estado <- httr2::resp_status(respuesta)
      if (!estado %in% temporales) {
        return(respuesta)
      }
      motivo <- paste0("el servidor respondi\u00f3 con el c\u00f3digo ", estado)
    }
    if (intento < intentos) {
      Sys.sleep(espera * intento)
    }
  }

  detener(
    "No se pudo completar la solicitud a ", solicitud$url, " despu\u00e9s de ",
    intentos, " intentos: ", motivo, ". ",
    "Revisa tu conexi\u00f3n a internet o intenta m\u00e1s tarde."
  )
}

# Un zip valido empieza con la firma "PK".
es_zip <- function(archivo) {
  if (!file.exists(archivo) || file.size(archivo) < 4) {
    return(FALSE)
  }
  firma <- readBin(archivo, what = "raw", n = 4)
  identical(firma[1:2], as.raw(c(0x50, 0x4b))) &&
    as.integer(firma[3]) %in% c(3L, 5L, 7L)
}

# Carpeta de la copia local de descargas (solo se usa si el usuario lo pide).
inegi_cache_dir <- function() {
  getOption("importinegi.cache_dir", tools::R_user_dir("importinegi", which = "cache"))
}

ruta_cache <- function(url) {
  relativa <- sub("^https?://[^/]+/", "", url)
  relativa <- gsub("[^A-Za-z0-9._/-]", "_", relativa)
  file.path(inegi_cache_dir(), relativa)
}

# Descarga un archivo del INEGI y devuelve su ruta local.
#
# El sitio del INEGI responde con una pagina "no encontrada" y codigo 200 cuando
# un archivo no existe, asi que para los zip se verifica el contenido recibido.
inegi_descargar <- function(url, destino = NULL, tamano = NULL, silencioso = FALSE,
                            cache = getOption("importinegi.cache", FALSE)) {
  url <- inegi_url(url)
  espera_zip <- grepl("\\.zip$", url, ignore.case = TRUE)

  if (isTRUE(cache)) {
    destino <- destino %||% ruta_cache(url)
    if (file.exists(destino) && (!espera_zip || es_zip(destino))) {
      if (!silencioso) {
        message("Usando la copia local de ", basename(url), ".")
      }
      return(invisible(destino))
    }
  }
  if (is.null(destino)) {
    extension <- tools::file_ext(sub("[?#].*$", "", url))
    destino <- tempfile("importinegi_", fileext = if (nzchar(extension)) paste0(".", extension) else "")
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  parcial <- paste0(destino, ".parcial")
  on.exit(unlink(parcial), add = TRUE)

  if (!silencioso) {
    message(
      "Descargando ", basename(url),
      if (!is.null(tamano) && !is.na(tamano)) paste0(" (", tamano, ")"),
      " ..."
    )
  }
  solicitud <- inegi_solicitud(url)
  if (!silencioso && interactive()) {
    solicitud <- httr2::req_progress(solicitud)
  }
  respuesta <- inegi_ejecutar(solicitud, destino = parcial)
  estado <- httr2::resp_status(respuesta)

  if (estado == 404 || (estado < 400 && espera_zip && !es_zip(parcial))) {
    detener(
      "El INEGI no tiene publicado el archivo ", url, " ",
      "(su sitio respondi\u00f3 con una p\u00e1gina de \"no encontrado\"). ",
      "Es posible que la direcci\u00f3n haya cambiado; puedes reportarlo en ",
      "https://github.com/crenteriam/importinegi/issues"
    )
  }
  if (estado >= 400) {
    detener("El INEGI respondi\u00f3 con el c\u00f3digo ", estado, " al solicitar ", url, ".")
  }

  unlink(destino)
  if (!file.rename(parcial, destino)) {
    file.copy(parcial, destino, overwrite = TRUE)
  }
  invisible(destino)
}

# Descarga el contenido de una pagina o de un servicio del INEGI como texto.
inegi_texto <- function(url) {
  url <- inegi_url(url)
  respuesta <- inegi_ejecutar(inegi_solicitud(url))
  estado <- httr2::resp_status(respuesta)
  if (estado >= 400) {
    detener("El INEGI respondi\u00f3 con el c\u00f3digo ", estado, " al solicitar ", url, ".")
  }
  httr2::resp_body_string(respuesta, encoding = "UTF-8")
}
