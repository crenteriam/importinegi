# Utilidades internas: paquetes opcionales ------------------------------------

# Verifica que un paquete opcional (listado en Suggests) este instalado.
# Si no lo esta, detiene la funcion con un mensaje que indica como instalarlo.
requiere_paquete <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop(
      "Esta funci\u00f3n requiere el paquete '", paquete, "', que no est\u00e1 instalado. ",
      "Inst\u00e1lalo con install.packages(\"", paquete, "\").",
      ayuda_version_anterior(paquete),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Version de R en uso. Es una funcion para poder sustituirla en las pruebas.
version_r <- function() {
  getRversion()
}

# Indicacion adicional para los paquetes opcionales cuya version actual ya no
# admite la version minima de R del paquete (4.1). readxl requiere R 4.2 desde
# la version 1.6.0; la 1.5.0.1 es la ultima que admite R 4.1, y en algunos
# sistemas install.packages() no la encuentra.
ayuda_version_anterior <- function(paquete) {
  if (paquete != "readxl" || version_r() >= "4.2.0") {
    return(NULL)
  }
  paste0(
    " La versi\u00f3n actual de readxl requiere R 4.2 o posterior. Si install.packages() no lo encuentra en ",
    "tu versi\u00f3n de R (", version_r(), "), instala la versi\u00f3n 1.5.0.1, la \u00faltima que admite R 4.1: ",
    "remotes::install_version(\"readxl\", \"1.5.0.1\")."
  )
}
