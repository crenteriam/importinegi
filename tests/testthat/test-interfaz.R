# Compatibilidad de la interfaz publica.
# La version 2.0.0 conserva los nombres de las funciones y de sus argumentos
# de la serie 1.x. Se pueden agregar funciones y argumentos nuevos (al final),
# pero no quitar ni reordenar los existentes.

interfaz_1x <- list(
  catalogo_inegi          = c("id"),
  censo_municipal         = c("year", "fuente", "datos"),
  censo_poblacion_ageb    = c("year", "estado", "totalestado", "totalmunicipio",
                              "totallocalidad", "totalageb", "manzana"),
  censo_poblacion_iter    = c("year", "estado", "totalestado", "totalmunicipio",
                              "localidades"),
  censo_poblacion_muestra = c("year", "estado", "muestra"),
  censo_poblacion_rural   = c("year", "estado"),
  censo_poblacion_urbano  = c("year", "estado"),
  enigh_nuevaconstruccion = c("year", "datos"),
  enoe                    = c("year", "trimestre", "integrar"),
  sig_caminos_descarga    = c("year"),
  sig_caminos_extrae      = c(".", "mapa"),
  sig_marcogeo            = c("year", "mapa", "version")
)

test_that("se conservan las funciones exportadas de la serie 1.x", {
  exportadas <- getNamespaceExports("importinegi")
  expect_identical(setdiff(names(interfaz_1x), exportadas), character())
})

test_that("se conservan los argumentos de la serie 1.x y su orden", {
  for (nombre in names(interfaz_1x)) {
    esperados <- interfaz_1x[[nombre]]
    actuales  <- names(formals(getExportedValue("importinegi", nombre)))
    expect_identical(actuales[seq_along(esperados)], esperados, info = nombre)
  }
})
