# Utilidades para generar los archivos de muestra de las pruebas
# (data-raw/muestras_censo.R y data-raw/muestras_conteo.R). Cada script define
# `destino`, la carpeta donde se escriben los zip, antes de usar escribir_zip().

tabla <- function(...) data.frame(..., stringsAsFactors = FALSE)

# Escribe un zip con las tablas indicadas. `tablas` es una lista con nombre:
# el nombre es la ruta del archivo dentro del zip (dbf o csv).
escribir_zip <- function(nombre, tablas, otros = list(), codificacion = "latin1") {
  carpeta <- tempfile("muestra_")
  dir.create(carpeta)
  for (ruta in names(tablas)) {
    archivo <- file.path(carpeta, ruta)
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    if (grepl("\\.dbf$", ruta, ignore.case = TRUE)) {
      # Los DBF guardan el texto en una pagina de codigos anterior a UTF-8
      datos <- tablas[[ruta]]
      for (columna in names(datos)) {
        if (is.character(datos[[columna]])) {
          datos[[columna]] <- iconv(datos[[columna]], from = "UTF-8", to = codificacion)
        }
      }
      temporal <- file.path(dirname(archivo), "temporal.dbf")
      foreign::write.dbf(datos, temporal)
      if (codificacion == "CP850") {
        # Byte 29 del encabezado: pagina de codigos 850
        conexion <- file(temporal, open = "r+b")
        seek(conexion, where = 29, rw = "write")
        writeBin(as.raw(0x02), conexion)
        close(conexion)
      }
      file.rename(temporal, archivo)
    } else {
      conexion <- file(archivo, open = "wb")
      writeBin(as.raw(c(0xef, 0xbb, 0xbf)), conexion)
      texto <- utils::capture.output(utils::write.csv(tablas[[ruta]], row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(texto, collapse = "\n"), "\n"))), conexion)
      close(conexion)
    }
  }
  for (ruta in names(otros)) {
    archivo <- file.path(carpeta, ruta)
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    writeLines(otros[[ruta]], archivo)
  }
  anterior <- setwd(carpeta)
  on.exit(setwd(anterior))
  utils::zip(file.path(destino, nombre), list.files(".", recursive = TRUE), flags = "-r9Xq")
  invisible(nombre)
}
