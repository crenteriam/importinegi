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
