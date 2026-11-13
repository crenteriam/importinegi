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

# Lee un CSV con todas sus columnas como texto. El INEGI publica sus CSV en
# UTF-8 (a veces con BOM) y algunos en Latin-1. El archivo se lee completo como
# UTF-8 y despues se convierten desde Latin-1 los valores que no son UTF-8
# valido, para no depender de que la primera parte del archivo tenga acentos.
leer_csv <- function(archivo) {
  datos <- utils::read.csv(
    archivo,
    colClasses = "character",
    encoding = "UTF-8",
    check.names = FALSE,
    na.strings = "",
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )
  nombres <- names(datos)
  invalidos <- !validUTF8(nombres)
  nombres[invalidos] <- iconv(nombres[invalidos], from = "latin1", to = "UTF-8")
  names(datos) <- sub("^\ufeff", "", nombres)
  for (columna in seq_along(datos)) {
    valores <- datos[[columna]]
    malos <- !is.na(valores) & !validUTF8(valores)
    if (any(malos)) {
      valores[malos] <- iconv(valores[malos], from = "latin1", to = "UTF-8")
      datos[[columna]] <- valores
    }
  }
  datos
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
# Por omision no convierte las columnas con ceros a la izquierda ("09",
# "0001"), porque suelen ser claves; con `ceros = TRUE` tambien las convierte.
# Nunca convierte las columnas indicadas en `texto`.
convertir_numericas <- function(datos, texto = NULL, ceros = FALSE) {
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
    if (all(es_numero) && (ceros || !any(es_clave))) {
      numeros <- as.numeric(valores)
      enteros <- !any(grepl(".", presentes, fixed = TRUE)) &&
        all(abs(numeros) <= .Machine$integer.max, na.rm = TRUE)
      datos[[columna]] <- if (enteros) as.integer(numeros) else numeros
    }
  }
  datos
}
