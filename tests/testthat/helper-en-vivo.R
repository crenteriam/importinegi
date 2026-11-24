# Utilidades de las pruebas en vivo (tests/testthat/test-en-vivo-*.R), que
# descargan archivos reales del INEGI. Solo corren con IMPORTINEGI_EN_VIVO=true
# (ver omitir_si_no_en_vivo() y .github/workflows/pruebas-en-vivo.yaml).

# Omite la prueba si no es en vivo y conserva las descargas durante la
# ejecucion: varias pruebas leen el mismo zip.
en_vivo <- function(entorno = parent.frame()) {
  omitir_si_no_en_vivo()
  withr::local_options(
    importinegi.cache = TRUE,
    importinegi.cache_dir = file.path(tempdir(), "descargas_en_vivo"),
    .local_envir = entorno
  )
}

# Entidad con el archivo mas pequeño de una base, o NA si el registro no la tiene.
entidad_menor <- function(programa, year, base) {
  archivos <- poblacion_archivos(programa, year, base)
  archivos <- archivos[archivos$clave != "00", ]
  if (nrow(archivos) == 0) NA_character_ else archivos$clave[which.min(archivos$bytes)]
}

# Escribe el contenido del zip de una entidad.
describir_zip <- function(programa, year, base, clave) {
  archivos <- poblacion_archivos(programa, year, base)
  fila <- archivos[archivos$clave == clave, ]
  zip <- suppressMessages(inegi_descargar(fila$url))
  carpeta <- inegi_descomprimir(zip)
  on.exit(unlink(carpeta, recursive = TRUE))
  tablas <- buscar_archivos(carpeta, extension = "dbf")
  paginas <- vapply(tablas, function(tabla) sprintf("%02x", as.integer(readBin(tabla, "raw", n = 32)[30])), character(1))
  cat("[en vivo] ", programa, " ", year, " ", base, " ", clave, ": ", basename(fila$url), " (", fila$tamano,
      ") contiene: ", paste(list.files(carpeta, recursive = TRUE), collapse = " | "),
      if (length(tablas) > 0) paste0("; byte de pagina de codigos: ", paste(unique(paginas), collapse = ", ")),
      "\n", sep = "")
}

# Comprueba que el texto sea UTF-8 valido y muestra algunos valores con acento.
revisar_texto <- function(etiqueta, datos) {
  texto <- names(datos)[vapply(datos, is.character, logical(1))]
  validos <- vapply(datos[texto], function(x) all(validUTF8(x)), logical(1))
  expect_true(all(validos), info = paste(etiqueta, "columnas con texto invalido:", paste(texto[!validos], collapse = ", ")))
  nombres <- grep("^NOM_", texto, value = TRUE)
  ejemplos <- unique(unlist(lapply(datos[nombres], function(x) grep("[^ -~]", x, value = TRUE))))
  cat("[en vivo] ", etiqueta, " texto con acento: ",
      if (length(ejemplos) > 0) paste(utils::head(ejemplos, 5), collapse = " | ") else "(ninguno en columnas NOM_)",
      "\n", sep = "")
}

describir_tabla <- function(etiqueta, datos) {
  tipos <- paste0(names(datos), "=", vapply(datos, function(x) class(x)[1], character(1)))
  cat("[en vivo] ", etiqueta, ": ", nrow(datos), " filas x ", ncol(datos), " columnas. ",
      paste(utils::head(tipos, 16), collapse = ", "), if (length(tipos) > 16) ", ...", "\n", sep = "")
}

# Niveles de agregacion de una tabla, como texto ("entidad=1, municipio=10, ...").
resumir_niveles <- function(niveles) {
  paste(names(table(niveles)), table(niveles), sep = "=", collapse = ", ")
}

# Revisa un DBF sin interpretarlo: registros que declara, registros marcados
# como borrados y campos numericos sin decimales declarados que tienen un punto
# decimal en algun registro (foreign::read.dbf() los leeria como enteros y
# perderia los decimales).
revisar_dbf <- function(archivo) {
  conexion <- file(archivo, open = "rb")
  on.exit(close(conexion))
  entero <- function(x) sum(as.numeric(x) * 256^(seq_along(x) - 1))
  encabezado <- readBin(conexion, "raw", n = 32)
  declarados <- entero(encabezado[5:8])
  inicio <- entero(encabezado[9:10])
  largo <- entero(encabezado[11:12])
  resto <- readBin(conexion, "raw", n = inicio - 32)
  campos <- 0
  while (32 * campos < length(resto) && resto[32 * campos + 1] != as.raw(0x0d)) {
    campos <- campos + 1
  }
  descriptor <- matrix(resto[seq_len(campos * 32)], nrow = 32)
  nombre <- apply(descriptor[1:11, , drop = FALSE], 2, function(x) rawToChar(x[cumsum(x == as.raw(0)) == 0]))
  tipo <- rawToChar(descriptor[12, ], multiple = TRUE)
  ancho <- as.integer(descriptor[17, ])
  decimales <- as.integer(descriptor[18, ])
  # Los registros, uno por columna de la matriz
  registros <- readBin(conexion, "raw", n = declarados * largo)
  completos <- length(registros) %/% largo
  length(registros) <- completos * largo
  dim(registros) <- c(largo, completos)
  desde <- cumsum(c(2, ancho))[seq_len(campos)]
  enteros <- which(tipo %in% c("N", "F") & decimales == 0)
  con_punto <- vapply(enteros, function(i) {
    any(registros[desde[i] + seq_len(ancho[i]) - 1, , drop = FALSE] == as.raw(0x2e))
  }, logical(1))
  list(
    declarados = declarados,
    leidos = as.numeric(completos),
    borrados = sum(registros[1, ] == as.raw(0x2a)),
    con_decimales = nombre[enteros[con_punto]],
    tipos = table(tipo),
    ancho_registro = c(declarado = largo, campos = 1 + sum(ancho))
  )
}
