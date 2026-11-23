# Utilidades internas: lectura de tablas ---------------------------------------

# Lee una tabla del INEGI segun la extension del archivo (dbf, csv o xlsx).
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
    xlsx = leer_xlsx(archivo),
    detener(
      "No se puede leer ", basename(archivo), ": el formato \"", extension,
      "\" no est\u00e1 soportado. Formatos disponibles: \"dbf\", \"csv\", \"xlsx\"."
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
#
# Omite los registros que el archivo marca como borrados: foreign::read.dbf()
# no revisa esa marca y los devuelve como si fueran vigentes.
leer_dbf <- function(archivo) {
  datos <- foreign::read.dbf(archivo, as.is = TRUE)
  borrados <- dbf_borrados(archivo)
  if (any(borrados)) {
    marcas <- c(borrados, rep(FALSE, max(0, nrow(datos) - length(borrados))))
    datos <- datos[!marcas[seq_len(nrow(datos))], , drop = FALSE]
    rownames(datos) <- NULL
  }
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

# Registros de un DBF marcados como borrados: un valor logico por registro. El
# primer byte de cada registro es un espacio si el registro esta vigente y un
# asterisco si se borro. El INEGI publica algunos archivos con registros
# borrados (por ejemplo, dos en las erogaciones de la ENIGH 2006) y no los
# cuenta en su documentacion. El archivo se recorre por bloques para no cargarlo
# completo en memoria.
dbf_borrados <- function(archivo) {
  conexion <- file(archivo, open = "rb")
  on.exit(close(conexion))
  encabezado <- readBin(conexion, what = "raw", n = 12)
  if (length(encabezado) < 12) {
    return(logical())
  }
  entero <- function(bytes) sum(as.numeric(bytes) * 256^(seq_along(bytes) - 1))
  registros <- entero(encabezado[5:8])
  largo_encabezado <- entero(encabezado[9:10])
  largo_registro <- entero(encabezado[11:12])
  if (registros == 0 || largo_registro == 0 || largo_encabezado < 12) {
    return(logical())
  }
  # El resto del encabezado se lee para avanzar hasta el primer registro
  readBin(conexion, what = "raw", n = largo_encabezado - 12)
  por_bloque <- max(1, floor(8 * 1024^2 / largo_registro))
  marcas <- vector("list", ceiling(registros / por_bloque))
  leidos <- 0
  bloque <- 0
  while (leidos < registros) {
    pedidos <- min(por_bloque, registros - leidos)
    bytes <- readBin(conexion, what = "raw", n = pedidos * largo_registro)
    completos <- length(bytes) %/% largo_registro
    if (completos == 0) {
      break
    }
    bloque <- bloque + 1
    marcas[[bloque]] <- bytes[seq.int(1, by = largo_registro, length.out = completos)] == as.raw(0x2a)
    leidos <- leidos + completos
    if (completos < pedidos) {
      break
    }
  }
  if (bloque == 0) logical() else unlist(marcas[seq_len(bloque)], use.names = FALSE)
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
#
# Algunos CSV de pocas filas no terminan con un salto de linea (por ejemplo, las
# tablas de una sola fila del censo de derechos humanos federal de 2017 a
# 2019). read.csv() los lee completos, pero avisa ("incomplete final line found
# by readTableHeader"); ese aviso se omite porque no hay nada que corregir. Se
# reconoce por el nombre de la funcion, que no cambia con el idioma de R.
leer_csv <- function(archivo) {
  datos <- withCallingHandlers(
    utils::read.csv(
      archivo,
      colClasses = "character",
      encoding = "UTF-8",
      check.names = FALSE,
      na.strings = "",
      strip.white = TRUE,
      stringsAsFactors = FALSE
    ),
    warning = function(aviso) {
      if (grepl("readTableHeader", conditionMessage(aviso), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
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

# Lee un archivo XLSX del INEGI con el paquete readxl (opcional). Los XLSX
# pueden traer filas de titulo antes del encabezado y notas al pie: en cada
# hoja se busca la fila de encabezado (la primera que contiene la clave de la
# entidad, como CVE_ENT, o la clave integrada CVEGEO) y se conservan solo las
# filas con una clave de entidad numerica. Todo se lee como texto, como los CSV
# y los DBF; despues se convierten las columnas numericas. Si varias hojas
# tienen datos, se combinan.
leer_xlsx <- function(archivo) {
  requiere_paquete("readxl")
  claves <- c(claves_geograficas()$ent$nombres, "CVEGEO")
  tablas <- list()
  for (hoja in readxl::excel_sheets(archivo)) {
    vista <- readxl::read_excel(
      archivo, sheet = hoja, col_names = FALSE, col_types = "text", n_max = 30, .name_repair = "minimal"
    )
    if (nrow(vista) == 0) {
      next
    }
    encabezado <- which(apply(as.matrix(vista), 1, function(fila) any(toupper(trimws(fila)) %in% claves)))
    if (length(encabezado) == 0) {
      next
    }
    datos <- readxl::read_excel(
      archivo, sheet = hoja, skip = encabezado[1] - 1, col_types = "text", .name_repair = "minimal"
    )
    datos <- as.data.frame(datos, stringsAsFactors = FALSE)
    names(datos) <- trimws(names(datos))
    vacias <- (is.na(names(datos)) | !nzchar(names(datos))) & vapply(datos, function(x) all(is.na(x)), logical(1))
    datos <- datos[, !vacias, drop = FALSE]
    entidad <- columna_clave(datos, "ent")
    if (!is.na(entidad)) {
      datos <- datos[!is.na(datos[[entidad]]) & grepl("^[0-9]+$", trimws(datos[[entidad]])), , drop = FALSE]
    }
    rownames(datos) <- NULL
    tablas[[length(tablas) + 1]] <- datos
  }
  if (length(tablas) == 0) {
    detener(
      "No se encontr\u00f3 una tabla de datos en ", basename(archivo), ": ninguna hoja tiene una columna ",
      "con la clave de la entidad. Es posible que el INEGI haya cambiado el archivo; puedes reportarlo en ",
      "https://github.com/crenteriam/importinegi/issues"
    )
  }
  combinar_tablas(tablas)
}

# Convierte en NA los codigos de dato faltante de las columnas de texto, salvo
# en las columnas indicadas en `excepto`.
limpiar_faltantes <- function(datos, codigos = c("*", "N/D"), excepto = NULL) {
  for (columna in setdiff(names(datos), excepto)) {
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
  aplicar_conversion(datos, decidir_conversion(list(datos), texto = texto, ceros = ceros))
}

# Perfil de una columna de texto: cuantos valores tiene, si todos son numeros
# y, en ese caso, si alguno tiene ceros a la izquierda o decimales y el mayor
# valor absoluto de los que podrian no caber en un entero (los de 10 o mas
# caracteres; los demas son menores que mil millones).
perfil_numerico <- function(valores) {
  presentes <- trimws(valores[!is.na(valores)])
  numero <- all(grepl("^-?[0-9]+(\\.[0-9]+)?$", presentes))
  if (!numero) {
    return(list(n = length(presentes), numero = FALSE, clave = FALSE, punto = FALSE, maximo = 0))
  }
  punto <- any(grepl(".", presentes, fixed = TRUE))
  largos <- if (punto) character() else presentes[nchar(presentes) >= 10]
  list(
    n = length(presentes),
    numero = TRUE,
    clave = any(grepl("^-?0[0-9]", presentes)),
    punto = punto,
    maximo = if (length(largos) > 0) max(abs(as.numeric(largos))) else 0
  )
}

# Decide que columnas de texto se convierten a numero ("entero" o "real") con
# los valores de una o varias tablas con las mismas columnas. Una columna se
# convierte si es texto en todas las tablas y todos sus valores son numeros;
# las que tienen ceros a la izquierda solo se convierten con `ceros = TRUE` o
# si estan en `medidas`. Nunca se convierten las columnas de `texto`.
decidir_conversion <- function(tablas, texto = NULL, ceros = FALSE, medidas = character()) {
  protegidas <- tolower(texto %||% character())
  medidas <- tolower(medidas)
  decision <- character()
  for (columna in names(tablas[[1]])) {
    if (tolower(columna) %in% protegidas) {
      next
    }
    valores <- lapply(tablas, function(tabla) tabla[[columna]])
    if (!all(vapply(valores, is.character, logical(1)))) {
      next
    }
    perfiles <- lapply(valores, perfil_numerico)
    dato <- function(campo, tipo) vapply(perfiles, function(perfil) perfil[[campo]], tipo)
    if (sum(dato("n", numeric(1))) == 0 || !all(dato("numero", logical(1)))) {
      next
    }
    if (any(dato("clave", logical(1))) && !(ceros || tolower(columna) %in% medidas)) {
      next
    }
    enteros <- !any(dato("punto", logical(1))) && max(dato("maximo", numeric(1))) <= .Machine$integer.max
    decision[[columna]] <- if (enteros) "entero" else "real"
  }
  decision
}

# Convierte las columnas segun lo que decidio decidir_conversion().
aplicar_conversion <- function(datos, decision) {
  for (columna in names(decision)) {
    numeros <- as.numeric(datos[[columna]])
    datos[[columna]] <- if (decision[[columna]] == "entero") as.integer(numeros) else numeros
  }
  datos
}
