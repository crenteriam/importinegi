# Utilidades internas: memoria de la sesion ------------------------------------
#
# Guarda resultados que no cambian durante una sesion de R (por ejemplo, el
# catalogo de la Red Nacional de Metadatos) para no descargarlos en cada
# llamada. No escribe nada en disco: se pierde al cerrar R.

memoria_sesion <- new.env(parent = emptyenv())

memoria_obtener <- function(clave) {
  if (exists(clave, envir = memoria_sesion, inherits = FALSE)) {
    get(clave, envir = memoria_sesion, inherits = FALSE)
  } else {
    NULL
  }
}

memoria_guardar <- function(clave, valor) {
  assign(clave, valor, envir = memoria_sesion)
  invisible(valor)
}

# Sin clave, borra toda la memoria de la sesion.
memoria_borrar <- function(clave = NULL) {
  claves <- if (is.null(clave)) ls(memoria_sesion, all.names = TRUE) else clave
  rm(list = intersect(claves, ls(memoria_sesion, all.names = TRUE)), envir = memoria_sesion)
  invisible(NULL)
}
