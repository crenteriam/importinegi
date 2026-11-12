#' Catalogo de proyectos estadisticos del INEGI
#'
#' Consulta el catalogo la Red Nacional de Metadatos del INEGI o los metadatos de un proyecto estadistico en particular.
#'
#' La funcion \code{catalogo_inegi} provee una lista de bases de datos con un identificador unico (\code{id}). Si conoces el \code{id} de la base de datos, utilizalo en el parametro para acceder al libro de codigos y los metadatos de la base de datos. Si no conoces el \code{id} de la base de datos a consultar, teclea la funcion \code{catalogo_inegi} sin parametros para descargar la lista de bases de datos (Ver ejemplo).
#'
#' @param id Para acceder al diccionario de datos de una base de datos de la Red Nacional de Metadatos del INEGI, utiliza el numero de identificacion unico (valor numerico). El identificador unico se puede consultar en el catalogo de proyectos estadisticos del INEGI (ver ejemplo).
#' @param buscar Texto para buscar proyectos por su nombre o por su clave, sin distinguir mayusculas ni acentos (por ejemplo, \code{"empleo"}). Si se escriben varias palabras, se devuelven los proyectos que las contienen todas.
#' @param year Año, o vector de años, para conservar solo los proyectos cuyo periodo incluye alguno de ellos (por ejemplo, \code{2020}).
#'
#' @examples
#'
#' # Accede al repositorio de la Red Nacional de Metadatos
#' # > de INEGI y almacenalo como una base de datos.
#' \dontrun{dt.catalogo <- catalogo_inegi()}
#'
#' # Consulta metadatos de una base de datos.
#' \dontrun{catalogo_inegi(id = 489)}
#'
#' # Busca proyectos por nombre y por año.
#' \dontrun{catalogo_inegi(buscar = "ocupacion y empleo", year = 2020)}
#'
#' @return Data.frame
#' @seealso Consulta el \href{https://www.inegi.org.mx/rnm/index.php/catalog/}{repositorio la Red Nacional de Metadatos del INEGI.}
#' @export

catalogo_inegi <- function(id = NA, buscar = NULL, year = NULL) {
  hay_id <- !(is.null(id) || (length(id) == 1 && is.na(id)))

  # Con id: abre la ficha del proyecto en el navegador ------------------------
  if (hay_id) {
    if (!is.null(buscar) || !is.null(year)) {
      detener(
        "`id` abre la ficha de un proyecto y no se combina con `buscar` ni con `year`. ",
        "Usa `id` solo, o busca sin `id`."
      )
    }
    return(abrir_pagina(catalogo_ficha(catalogo_validar_id(id))))
  }

  # Sin id: devuelve el catalogo, completo o filtrado -------------------------
  catalogo <- catalogo_rnm()
  if (!is.null(buscar)) {
    catalogo <- catalogo_buscar(catalogo, buscar)
  }
  if (!is.null(year)) {
    catalogo <- catalogo_filtrar_year(catalogo, year)
  }
  if (nrow(catalogo) == 0) {
    message("Ning\u00fan proyecto del cat\u00e1logo coincide con la b\u00fasqueda.")
  }
  rownames(catalogo) <- NULL
  catalogo
}

# Funciones internas del catalogo ----------------------------------------------

# Numero de proyectos que se pide a la Red Nacional de Metadatos. El sitio
# entrega como maximo los que se le piden en `ps`, empezando por los mas
# recientes, asi que debe ser mayor que el total del catalogo. Verificado el
# 11 de noviembre de 2026: con ps=500 entrega 500 proyectos; con ps=5000, los 825.
catalogo_tamano <- function() {
  5000L
}

# Direccion de la exportacion del catalogo.
catalogo_url_exportacion <- function() {
  paste0("/rnm/index.php/catalog/export/csv?ps=", catalogo_tamano())
}

# Direccion de la ficha de un proyecto.
catalogo_ficha <- function(id) {
  paste0(inegi_base(), "/rnm/index.php/catalog/", id)
}

# El identificador de un proyecto es un solo numero entero positivo.
catalogo_validar_id <- function(id) {
  es_numero <- is.numeric(id) || (is.character(id) && all(grepl("^[0-9]+$", id)))
  valido <- length(id) == 1 && !is.na(id) && es_numero &&
    as.numeric(id) >= 1 && as.numeric(id) == round(as.numeric(id))
  if (!isTRUE(valido)) {
    detener(
      "`id` debe ser el n\u00famero de identificaci\u00f3n de un proyecto (un solo n\u00famero ",
      "entero), por ejemplo `id = 489`. Consulta los identificadores con catalogo_inegi()."
    )
  }
  format(as.numeric(id), scientific = FALSE, trim = TRUE)
}

# Descarga y lee el catalogo. Se conserva en la memoria de la sesion para no
# descargarlo en cada llamada.
catalogo_rnm <- function() {
  guardado <- memoria_obtener("catalogo_rnm")
  if (!is.null(guardado)) {
    return(guardado)
  }
  message("Descargando el cat\u00e1logo de la Red Nacional de Metadatos del INEGI ...")
  archivo <- inegi_descargar(
    catalogo_url_exportacion(),
    destino = tempfile("importinegi_", fileext = ".csv"),
    silencioso = TRUE
  )
  on.exit(unlink(archivo), add = TRUE)

  catalogo <- tryCatch(
    suppressWarnings(leer_tabla(archivo, convertir = TRUE)),
    error = function(e) NULL
  )
  # Si el sitio responde con una pagina en lugar del catalogo, no se devuelve
  # una tabla sin sentido: se avisa.
  es_catalogo <- is.data.frame(catalogo) && nrow(catalogo) > 0 &&
    all(c("id", "idno", "title") %in% names(catalogo))
  if (!es_catalogo) {
    detener(
      "El INEGI no devolvi\u00f3 el cat\u00e1logo de la Red Nacional de Metadatos ",
      "(la respuesta no tiene el formato esperado). Intenta m\u00e1s tarde o ",
      "consulta el cat\u00e1logo en ", inegi_base(), "/rnm/index.php/catalog/"
    )
  }
  # Si llegan tantos proyectos como se pidieron, el catalogo crecio mas alla del
  # limite y podria venir recortado: se avisa en lugar de callarlo.
  if (nrow(catalogo) >= catalogo_tamano()) {
    warning(
      "El cat\u00e1logo recibido tiene ", nrow(catalogo), " proyectos, el m\u00e1ximo que ",
      "se pide al INEGI, y podr\u00eda estar incompleto. Rep\u00f3rtalo en ",
      "https://github.com/crenteriam/importinegi/issues",
      call. = FALSE
    )
  }
  catalogo$url <- catalogo_ficha(catalogo$id)
  memoria_guardar("catalogo_rnm", catalogo)
  catalogo
}

# Conserva los proyectos cuyo nombre o clave contiene todas las palabras
# buscadas, sin distinguir mayusculas ni acentos.
catalogo_buscar <- function(catalogo, buscar) {
  terminos <- if (is.character(buscar) && !anyNA(buscar)) {
    palabras <- unlist(strsplit(normalizar_texto(buscar), " ", fixed = TRUE))
    palabras[nzchar(palabras)]
  } else {
    character()
  }
  if (length(terminos) == 0) {
    detener("`buscar` debe ser un texto con al menos una palabra, por ejemplo `buscar = \"empleo\"`.")
  }
  campo <- function(nombre) {
    valores <- as.character(catalogo[[nombre]])
    valores[is.na(valores)] <- ""
    valores
  }
  donde <- normalizar_texto(paste(campo("title"), campo("idno")))
  coincide <- rep(TRUE, nrow(catalogo))
  for (termino in terminos) {
    coincide <- coincide & grepl(termino, donde, fixed = TRUE)
  }
  catalogo[coincide, , drop = FALSE]
}

# Conserva los proyectos cuyo periodo (year_start a year_end) incluye alguno de
# los anios pedidos.
catalogo_filtrar_year <- function(catalogo, year) {
  valido <- is.numeric(year) && length(year) > 0 && !anyNA(year) && all(year == round(year))
  if (!valido) {
    detener("`year` debe ser un a\u00f1o o un vector de a\u00f1os, por ejemplo `year = 2020`.")
  }
  if (!all(c("year_start", "year_end") %in% names(catalogo))) {
    detener("El cat\u00e1logo no incluye el periodo de los proyectos; no se puede filtrar con `year`.")
  }
  inicio <- suppressWarnings(as.numeric(catalogo$year_start))
  fin <- suppressWarnings(as.numeric(catalogo$year_end))
  fin[is.na(fin)] <- inicio[is.na(fin)]
  inicio[is.na(inicio)] <- fin[is.na(inicio)]
  incluye <- rep(FALSE, nrow(catalogo))
  for (anio in year) {
    incluye <- incluye | (!is.na(inicio) & anio >= inicio & anio <= fin)
  }
  catalogo[incluye, , drop = FALSE]
}
