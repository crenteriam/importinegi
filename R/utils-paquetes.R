# Utilidades internas: paquetes opcionales ------------------------------------

# Verifica que un paquete opcional (listado en Suggests) este instalado.
# Si no lo esta, detiene la funcion con un mensaje que indica como instalarlo.
requiere_paquete <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop(
      "Esta funci\u00f3n requiere el paquete '", paquete, "', que no est\u00e1 instalado. ",
      "Inst\u00e1lalo con install.packages(\"", paquete, "\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
