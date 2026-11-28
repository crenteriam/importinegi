# Registro del Marco Geoestadistico ------------------------------------------------
#
# Estas funciones arman los dos objetos internos que usa
# sig_marcogeoestadistico():
#
#   mg_productos  Productos completos del Marco Geoestadistico: clave, titulo,
#                 edicion, escala, ruta y bytes. Salen del servicio de productos
#                 del INEGI.
#   mg_capas      Descarga por capa: una fila por año y capa, con el tamaño del
#                 archivo de todo el pais (`bytes`) y las claves de las entidades
#                 que no tienen archivo (`faltan`, separadas por espacios, o
#                 "todas" si la capa no existe ese año). Sale de sondear cada
#                 direccion.
#
# Se generan con el script data-raw/marcogeo.R; las funciones del paquete no
# consultan el servicio para descargar.

# Servicio de productos del INEGI: el que usa la pestaña "Descargas" de
# https://www.inegi.org.mx/temas/mg/. No esta documentado por el INEGI y puede
# cambiar sin aviso.
marcogeo_api_url <- function() {
  inegi_url("/app/api/productos/interna_v2/componente/mapas/lista/resultados/")
}

# Consulta de la coleccion "Marco geoestadistico" (`titg` 4328) del tema Marco
# Geoestadistico (`tema` 213), de la edicion mas reciente a la mas antigua.
marcogeo_api_consulta <- function() {
  list(tema = "213", titg = "4328", orden = 4, desc = TRUE, pag = 0, tam = "100")
}

# Convierte la lista de productos del servicio en una tabla con las ediciones
# del Marco Geoestadistico en formato SHP. La coleccion tiene ademas cartas en
# PDF y la capa de areas de control del Marco Censal Agropecuario, que no son
# ediciones del Marco Geoestadistico.
marcogeo_api_a_tabla <- function(mapas) {
  filas <- lapply(mapas, function(mapa) {
    titulo <- mapa$titulo %||% ""
    if (!startsWith(normalizar_texto(titulo), "marco geoestadistico")) {
      return(NULL)
    }
    for (formato in mapa$formatos) {
      ruta <- formato$url$valor
      if (identical(toupper(formato$extension %||% ""), "SHP") && is.character(ruta) && nzchar(ruta)) {
        return(data.frame(
          clave = as.character(mapa$key %||% NA),
          titulo = titulo,
          edicion = suppressWarnings(as.integer(mapa$edicion %||% NA)),
          escala = mapa$escala %||% "",
          ruta = ruta,
          stringsAsFactors = FALSE
        ))
      }
    }
    NULL
  })
  tabla <- do.call(rbind, filas)
  if (is.null(tabla)) {
    return(data.frame(
      clave = character(), titulo = character(), edicion = integer(), escala = character(),
      ruta = character(), stringsAsFactors = FALSE
    ))
  }
  tabla <- tabla[order(tabla$edicion, tabla$clave), ]
  rownames(tabla) <- NULL
  tabla
}

# Productos del Marco Geoestadistico que lista el servicio del INEGI.
marcogeo_api_productos <- function() {
  consulta <- marcogeo_api_consulta()
  solicitud <- httr2::req_body_json(inegi_solicitud(marcogeo_api_url()), consulta)
  respuesta <- inegi_ejecutar(solicitud)
  estado <- httr2::resp_status(respuesta)
  if (estado >= 400) {
    detener("El INEGI respondi\u00f3 con el c\u00f3digo ", estado, " al consultar ", marcogeo_api_url(), ".")
  }
  contenido <- tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(respuesta, encoding = "UTF-8"), simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(contenido) || !isTRUE(contenido$success) || !is.list(contenido$list$mapas)) {
    detener(
      "El servicio de productos del INEGI no devolvi\u00f3 una lista v\u00e1lida",
      if (is.list(contenido) && is.character(contenido$message)) paste0(": ", contenido$message), "."
    )
  }
  if (length(contenido$list$mapas) >= as.integer(consulta$tam)) {
    detener(
      "El servicio de productos del INEGI devolvi\u00f3 ", length(contenido$list$mapas), " productos, ",
      "el m\u00e1ximo de la consulta: puede haber m\u00e1s. Hay que ampliar la consulta en marcogeo_api_consulta()."
    )
  }
  marcogeo_api_a_tabla(contenido$list$mapas)
}

# Tamaño en bytes de un zip del INEGI, o NA si no existe. Pide solo sus primeros
# cuatro bytes (cabecera Range): si el archivo existe, el servidor responde con
# el codigo 206 y el tamaño total en la cabecera Content-Range; si no existe,
# responde con el codigo 200 y una pagina de "no encontrado".
marcogeo_sondear <- function(url) {
  url <- inegi_url(url)
  solicitud <- httr2::req_headers(inegi_solicitud(url), Range = "bytes=0-3")
  respuesta <- inegi_ejecutar(solicitud)
  estado <- httr2::resp_status(respuesta)
  if (estado == 404) {
    return(NA_real_)
  }
  if (!estado %in% c(200, 206)) {
    detener("El INEGI respondi\u00f3 con el c\u00f3digo ", estado, " al solicitar ", url, ".")
  }
  cuerpo <- httr2::resp_body_raw(respuesta)
  if (length(cuerpo) < 4 || !zip_firma(cuerpo, 1, c(0x50, 0x4b, 0x03, 0x04))) {
    return(NA_real_)
  }
  if (estado == 200) {
    # El servidor no atendio la cabecera Range y envio el archivo completo
    return(as.numeric(length(cuerpo)))
  }
  rango <- httr2::resp_header(respuesta, "Content-Range")
  tamano <- suppressWarnings(as.numeric(sub("^.*/", "", if (is.null(rango)) "" else rango)))
  if (is.na(tamano)) {
    detener("El INEGI no indic\u00f3 el tama\u00f1o de ", url, ".")
  }
  tamano
}

# Tabla `mg_productos`: los productos del servicio con su tamaño exacto (el
# servicio lo da redondeado: "2.74 GB"). `bytes` queda en NA si la direccion de
# un producto no responde con un zip.
marcogeo_registro_productos <- function(silencioso = FALSE) {
  tabla <- marcogeo_api_productos()
  if (!silencioso) {
    message("Productos del Marco Geoestad\u00edstico en el servicio del INEGI: ", nrow(tabla), ".")
  }
  tabla$bytes <- vapply(tabla$ruta, marcogeo_sondear, numeric(1), USE.NAMES = FALSE)
  tabla
}

# Capas de la descarga por capa, en el orden del vocabulario de mapas.
marcogeo_capas_descarga <- function() {
  capas <- marcogeo_mapas()$capa
  capas[!is.na(capas)]
}

# Tabla `mg_capas` de los años indicados. Sondea el archivo de todo el pais y
# el de cada entidad de cada capa: 528 direcciones por año.
marcogeo_registro_capas <- function(years, silencioso = FALSE) {
  claves <- marcogeo_todas()
  filas <- list()
  for (year in years) {
    if (!silencioso) {
      message("Sondeando la descarga por capa de ", year, " ...")
    }
    for (capa in marcogeo_capas_descarga()) {
      nacional <- marcogeo_sondear(marcogeo_url_capa(year, "00", capa))
      existe <- vapply(claves, function(clave) {
        !is.na(marcogeo_sondear(marcogeo_url_capa(year, clave, capa)))
      }, logical(1))
      faltan <- if (is.na(nacional) && !any(existe)) "todas" else paste(claves[!existe], collapse = " ")
      filas[[length(filas) + 1]] <- data.frame(
        year = as.integer(year), capa = capa, bytes = nacional, faltan = faltan, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, filas)
}
