# Utilidades internas: errores y validacion de argumentos ---------------------

# Detiene la funcion con un mensaje, sin mostrar la llamada interna.
detener <- function(...) {
  stop(..., call. = FALSE)
}

# Valor por omision cuando el primero es NULL.
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# Texto de un valor para los mensajes: "texto" entre comillas, numeros tal cual.
comillas <- function(valor) {
  if (is.character(valor)) paste0("\"", valor, "\"") else as.character(valor)
}

# Lista de opciones separadas por comas para los mensajes.
enumerar <- function(opciones, maximo = 40) {
  texto <- vapply(as.list(opciones), comillas, character(1))
  if (length(texto) > maximo) {
    texto <- c(texto[seq_len(maximo)], "...")
  }
  paste(texto, collapse = ", ")
}

# Verifica que el argumento sea un solo valor no faltante.
validar_escalar <- function(valor, argumento) {
  if (is.null(valor) || length(valor) != 1 || is.na(valor)) {
    detener("Falta indicar el argumento `", argumento, "` (un solo valor).")
  }
  valor
}

# Verifica que el argumento sea una de las opciones disponibles.
validar_opcion <- function(valor, opciones, argumento) {
  validar_escalar(valor, argumento)
  if (!as.character(valor) %in% as.character(opciones)) {
    detener(
      "`", argumento, " = ", comillas(valor), "` no es una opci\u00f3n v\u00e1lida. ",
      "Opciones disponibles: ", enumerar(opciones), "."
    )
  }
  valor
}

# Verifica que el argumento sea TRUE o FALSE.
validar_logico <- function(valor, argumento) {
  if (!is.logical(valor) || length(valor) != 1 || is.na(valor)) {
    detener("`", argumento, "` debe ser TRUE o FALSE.")
  }
  valor
}
