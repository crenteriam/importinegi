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
    dbf = leer_dbf(archivo),
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

# Lee un DBF y deja su texto en UTF-8. Los DBF guardan el texto en la pagina de
# codigos del programa que los creo; si el texto no es UTF-8 valido se convierte
# desde la pagina que declara el encabezado del archivo o, si no declara
# ninguna, desde la de Windows para Europa occidental (CP1252).
leer_dbf <- function(archivo) {
  datos <- foreign::read.dbf(archivo, as.is = TRUE)
  origen <- dbf_codificacion(archivo)
  for (columna in names(datos)) {
    valores <- datos[[columna]]
    if (!is.character(valores) || all(validUTF8(valores))) {
      next
    }
    convertidos <- iconv(valores, from = if (is.na(origen)) "CP1252" else origen, to = "UTF-8")
    fallidos <- is.na(convertidos) & !is.na(valores)
    if (any(fallidos)) {
      convertidos[fallidos] <- iconv(valores[fallidos], from = "latin1", to = "UTF-8", sub = "?")
    }
    datos[[columna]] <- convertidos
  }
  datos
}

# Pagina de codigos que declara un DBF en el byte 29 de su encabezado, o NA.
dbf_codificacion <- function(archivo) {
  encabezado <- readBin(archivo, what = "raw", n = 32)
  if (length(encabezado) < 30) {
    return(NA_character_)
  }
  switch(
    sprintf("%02x", as.integer(encabezado[30])),
    "01" = "CP437",
    "02" = "CP850",
    "03" = "CP1252",
    "57" = "CP1252",
    NA_character_
  )
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
