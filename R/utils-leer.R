# Utilidades internas: lectura de tablas ---------------------------------------

# Lee una tabla del INEGI segun la extension del archivo (dbf o csv).
#
# faltantes  Codigos que el INEGI usa para datos no disponibles o reservados
#            (por ejemplo "*" y "N/D"); se convierten en NA.
# convertir  TRUE para convertir a numero las columnas de texto que solo
#            contienen numeros.
# texto      Columnas que siempre deben quedar como texto aunque parezcan
#            numeros (por ejemplo las claves geograficas).
leer_tabla <- function(archivo, faltantes = NULL, convertir = FALSE, texto = NULL) {
  extension <- tolower(tools::file_ext(archivo))
  datos <- switch(
    extension,
    dbf = foreign::read.dbf(archivo, as.is = TRUE),
    csv = leer_csv(archivo),
    detener(
      "No se puede leer ", basename(archivo), ": el formato \"", extension,
      "\" no est\u00e1 soportado. Formatos disponibles: \"dbf\", \"csv\"."
    )
  )
  if (!is.null(faltantes)) {
    datos <- limpiar_faltantes(datos, faltantes)
  }
  if (convertir) {
    datos <- convertir_numericas(datos, texto = texto)
  }
  datos
}

# El INEGI publica sus CSV en UTF-8 (a veces con marca BOM) o en Latin-1.
# Todas las columnas se leen como texto para no perder ceros a la izquierda.
leer_csv <- function(archivo) {
  muestra <- readLines(archivo, n = 2000, warn = FALSE, encoding = "bytes")
  codificacion <- if (all(validUTF8(muestra))) "UTF-8-BOM" else "latin1"
  utils::read.csv(
    archivo,
    colClasses = "character",
    fileEncoding = codificacion,
    check.names = FALSE,
    na.strings = "",
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )
}

# Convierte en NA los codigos de dato faltante de las columnas de texto.
limpiar_faltantes <- function(datos, codigos = c("*", "N/D")) {
  for (columna in names(datos)) {
    valores <- datos[[columna]]
    if (is.character(valores)) {
      valores[trimws(valores) %in% codigos] <- NA_character_
      datos[[columna]] <- valores
    }
  }
  datos
}

# Convierte a numero las columnas de texto cuyos valores son todos numericos.
# No convierte las columnas con ceros a la izquierda ("09", "0001"), porque
# son claves, ni las indicadas en `texto`.
convertir_numericas <- function(datos, texto = NULL) {
  protegidas <- tolower(texto %||% character())
  for (columna in names(datos)) {
    valores <- datos[[columna]]
    if (!is.character(valores) || tolower(columna) %in% protegidas) {
      next
    }
    presentes <- trimws(valores[!is.na(valores)])
    if (length(presentes) == 0) {
      next
    }
    es_numero <- grepl("^-?[0-9]+(\\.[0-9]+)?$", presentes)
    es_clave <- grepl("^-?0[0-9]", presentes)
    if (all(es_numero) && !any(es_clave)) {
      numeros <- as.numeric(valores)
      enteros <- !any(grepl(".", presentes, fixed = TRUE)) &&
        all(abs(numeros) <= .Machine$integer.max, na.rm = TRUE)
      datos[[columna]] <- if (enteros) as.integer(numeros) else numeros
    }
  }
  datos
}
