# Utilidades para generar los archivos de muestra de las pruebas
# (data-raw/muestras_censo.R y data-raw/muestras_conteo.R). Cada script define
# `destino`, la carpeta donde se escriben los zip, antes de usar escribir_zip().

tabla <- function(...) data.frame(..., stringsAsFactors = FALSE)

# Escribe un zip con las tablas indicadas. `tablas` es una lista con nombre:
# el nombre es la ruta del archivo dentro del zip (dbf o csv). `otros` agrega
# archivos de texto y `binarios`, copias de archivos que ya existen (por
# ejemplo, otro zip), con la ruta dentro del zip como nombre.
escribir_zip <- function(nombre, tablas, otros = list(), codificacion = "latin1", binarios = character()) {
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
  for (ruta in names(binarios)) {
    archivo <- file.path(carpeta, ruta)
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    file.copy(binarios[[ruta]], archivo)
  }
  anterior <- setwd(carpeta)
  on.exit(setwd(anterior))
  utils::zip(file.path(destino, nombre), list.files(".", recursive = TRUE), flags = "-r9Xq")
  invisible(nombre)
}

# Escribe un zip byte por byte, para decidir como queda guardado el nombre de
# cada entrada. utils::zip() no sirve para eso: no puede guardar nombres que no
# sean UTF-8 valido ni usar "\\" como separador de carpetas, y algunos zip del
# INEGI tienen las dos cosas.
#
# `entradas` es una lista; cada elemento es una lista con `nombre` (vector raw
# con los bytes del nombre, tal como deben quedar en el zip) y `contenido`
# (lineas de texto, que se guardan en UTF-8 con BOM, o un vector raw). Una
# entrada cuyo nombre termina en "/" o en "\\" es una carpeta. Las entradas no
# llevan la marca de UTF-8, como las de los zip que imitan.
escribir_zip_crudo <- function(nombre, entradas) {
  entero <- function(valor, bytes) writeBin(as.integer(valor), raw(), size = bytes, endian = "little")
  # R no tiene una funcion para calcular el CRC-32 ni para comprimir sin
  # encabezado, que es lo que guarda un zip. Un archivo gzip contiene las dos
  # cosas: 10 bytes de encabezado, los datos comprimidos y, al final, el CRC-32
  # y el tamano.
  comprimir <- function(bytes) {
    temporal <- tempfile()
    on.exit(unlink(temporal))
    conexion <- gzfile(temporal, open = "wb", compression = 9)
    writeBin(bytes, conexion)
    close(conexion)
    gz <- readBin(temporal, what = "raw", n = file.size(temporal))
    stopifnot(gz[4] == as.raw(0))
    list(datos = gz[11:(length(gz) - 8)], crc = gz[(length(gz) - 7):(length(gz) - 4)])
  }
  hora <- entero(0, 2)
  fecha <- entero(bitwOr(bitwOr(bitwShiftL(2025 - 1980, 9), bitwShiftL(1, 5)), 1), 2)
  cuerpo <- raw()
  directorio <- raw()
  for (entrada in entradas) {
    bytes_nombre <- entrada$nombre
    ultimo <- bytes_nombre[length(bytes_nombre)]
    carpeta <- ultimo == charToRaw("/") || ultimo == charToRaw("\\")
    contenido <- entrada$contenido
    if (is.character(contenido)) {
      contenido <- c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8(paste0(paste(contenido, collapse = "\n"), "\n"))))
    }
    if (carpeta) {
      comprimido <- list(datos = raw(), crc = entero(0, 4))
      contenido <- raw()
    } else {
      comprimido <- comprimir(contenido)
    }
    metodo <- entero(if (carpeta) 0 else 8, 2)
    comun <- c(
      entero(20, 2), entero(0, 2), metodo, hora, fecha, comprimido$crc,
      entero(length(comprimido$datos), 4), entero(length(contenido), 4), entero(length(bytes_nombre), 2)
    )
    desplazamiento <- length(cuerpo)
    cuerpo <- c(cuerpo, as.raw(c(0x50, 0x4b, 0x03, 0x04)), comun, entero(0, 2), bytes_nombre, comprimido$datos)
    directorio <- c(
      directorio, as.raw(c(0x50, 0x4b, 0x01, 0x02)), entero(20, 2), comun,
      entero(0, 2), entero(0, 2), entero(0, 2), entero(0, 2),
      entero(if (carpeta) 16 else 32, 4), entero(desplazamiento, 4), bytes_nombre
    )
  }
  final <- c(
    as.raw(c(0x50, 0x4b, 0x05, 0x06)), entero(0, 2), entero(0, 2),
    entero(length(entradas), 2), entero(length(entradas), 2),
    entero(length(directorio), 4), entero(length(cuerpo), 4), entero(0, 2)
  )
  writeBin(c(cuerpo, directorio, final), file.path(destino, nombre))
  invisible(nombre)
}

# Bytes de un texto en la pagina de codigos de DOS (CP437), la que usan los zip
# que no marcan sus nombres como UTF-8.
bytes_dos <- function(texto) {
  iconv(enc2utf8(texto), from = "UTF-8", to = "CP437", toRaw = TRUE)[[1]]
}

