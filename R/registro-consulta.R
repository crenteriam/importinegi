# Consulta del registro de archivos del INEGI -----------------------------------
#
# El paquete distribuye una copia del registro (el objeto interno
# `registro_archivos`, que genera data-raw/registro.R). Las funciones de
# descarga buscan ahi la direccion de cada archivo en lugar de armarla a mano.

# Registro que se distribuye con el paquete. La opcion importinegi.registro
# permite sustituirlo por otra tabla (solo para pruebas).
registro_inegi <- function() {
  getOption("importinegi.registro") %||% registro_archivos
}

# Fecha en que se genero el registro, como texto.
registro_generado <- function() {
  attr(registro_inegi(), "generado") %||% "fecha desconocida"
}

# Direccion de descarga de cada fila del registro. Se arma con la ruta y el
# sufijo del formato para que respete la direccion base del sitio.
registro_url <- function(filas) {
  url <- as.character(filas$url)
  con_ruta <- !is.na(filas$ruta) & nzchar(filas$ruta)
  url[con_ruta] <- inegi_url(paste0("/contenidos", filas$ruta[con_ruta], filas$sufijo[con_ruta]))
  url
}

# Suma de los tamanos de varios archivos, en bytes enteros. Los bytes del
# registro se calculan con el texto que publica el INEGI ("6.49 MB") y tienen
# decimales. Al sumarlos tal cual, el ultimo bit del resultado depende de la
# plataforma (de si R suma con mas precision que la de un numero doble), y un
# total que cae justo a la mitad (813.95 MB) se redondeaba a 814.0 en unos
# equipos y a 813.9 en otros. La suma de bytes enteros es exacta en todos.
sumar_bytes <- function(bytes) {
  sum(round(bytes), na.rm = TRUE)
}

# Tamano en bytes como texto para los mensajes ("362 MB", "1.2 GB").
tamano_texto <- function(bytes) {
  bytes <- sumar_bytes(bytes)
  if (bytes >= 1024^3) {
    paste(format(round(bytes / 1024^3, 1), nsmall = 1), "GB")
  } else if (bytes >= 10 * 1024^2) {
    paste(round(bytes / 1024^2), "MB")
  } else if (bytes >= 1024^2) {
    paste(format(round(bytes / 1024^2, 1), nsmall = 1), "MB")
  } else {
    paste(max(1, round(bytes / 1024)), "KB")
  }
}
