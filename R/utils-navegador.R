# Utilidades internas: abrir paginas del INEGI ---------------------------------

# Abre una pagina en el navegador del usuario (en cualquier sistema operativo).
# Fuera de una sesion interactiva solo muestra la direccion.
abrir_pagina <- function(url) {
  url <- inegi_url(url)
  if (interactive()) {
    utils::browseURL(url)
  } else {
    message("Consulta la informaci\u00f3n en: ", url)
  }
  invisible(url)
}
