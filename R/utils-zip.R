# Utilidades internas: archivos comprimidos ------------------------------------

# Descomprime un zip en una carpeta temporal propia de cada llamada, para que
# dos descargas de la misma sesion no se mezclen ni se escriba en la carpeta
# de trabajo del usuario. Devuelve la ruta de esa carpeta.
#
# Con anidados = TRUE tambien descomprime los zip que vengan dentro del zip,
# cada uno en una carpeta con su mismo nombre.
inegi_descomprimir <- function(zip, anidados = TRUE) {
  if (!es_zip(zip)) {
    detener("El archivo ", basename(zip), " no es un zip v\u00e1lido.")
  }
  carpeta <- tempfile("importinegi_")
  dir.create(carpeta)
  extraer_zip(zip, carpeta)

  if (anidados) {
    procesados <- character()
    for (nivel in 1:3) {
      internos <- list.files(carpeta, pattern = "\\.zip$", ignore.case = TRUE,
                             recursive = TRUE, full.names = TRUE)
      internos <- setdiff(internos, procesados)
      if (length(internos) == 0) {
        break
      }
      for (interno in internos) {
        if (es_zip(interno)) {
          extraer_zip(interno, tools::file_path_sans_ext(interno))
        }
      }
      procesados <- c(procesados, internos)
    }
  }
  carpeta
}

extraer_zip <- function(zip, carpeta) {
  resultado <- tryCatch(
    utils::unzip(zip, exdir = carpeta),
    warning = function(w) w,
    error = function(e) e
  )
  if (inherits(resultado, "condition") || length(resultado) == 0) {
    detalle <- if (inherits(resultado, "condition")) conditionMessage(resultado) else "el zip est\u00e1 vac\u00edo"
    detener("No se pudo descomprimir ", basename(zip), ": ", detalle, ".")
  }
  invisible(resultado)
}

# Busca archivos dentro de una carpeta (y sus subcarpetas) por extension y por
# patron en el nombre, sin distinguir mayusculas de minusculas. Con
# exigir = TRUE se detiene si no encuentra ninguno y muestra lo que si hay.
buscar_archivos <- function(carpeta, patron = NULL, extension = NULL, exigir = FALSE) {
  todos <- list.files(carpeta, recursive = TRUE, full.names = TRUE)
  todos <- todos[!dir.exists(todos)]
  archivos <- todos
  if (!is.null(extension)) {
    archivos <- archivos[tolower(tools::file_ext(archivos)) %in% tolower(extension)]
  }
  if (!is.null(patron)) {
    archivos <- archivos[grepl(patron, basename(archivos), ignore.case = TRUE)]
  }
  if (exigir && length(archivos) == 0) {
    detener(
      "El archivo descargado no contiene lo que se esperaba",
      if (!is.null(patron)) paste0(" (patr\u00f3n \"", patron, "\")"),
      if (!is.null(extension)) paste0(" (extensi\u00f3n ", paste(extension, collapse = ", "), ")"),
      ". Contiene: ", enumerar(basename(todos), maximo = 15), "."
    )
  }
  sort(archivos)
}
