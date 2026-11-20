# Compatibilidad de la interfaz publica.
# La version 2.0.0 conserva los nombres de las funciones y de sus argumentos
# de la serie 1.x. Se pueden agregar funciones y argumentos nuevos (al final),
# pero no quitar ni reordenar los existentes. La unica excepcion son las cinco
# funciones censo_poblacion_*(), que se eliminaron porque censo() las sustituye.

interfaz_1x <- list(
  catalogo_inegi          = c("id"),
  censo_municipal         = c("year", "fuente", "datos"),
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

test_that("las funciones censo_poblacion_*() ya no se exportan", {
  eliminadas <- c("censo_poblacion_ageb", "censo_poblacion_iter", "censo_poblacion_muestra",
                  "censo_poblacion_rural", "censo_poblacion_urbano")
  expect_identical(intersect(eliminadas, getNamespaceExports("importinegi")), character())
  expect_true("censo" %in% getNamespaceExports("importinegi"))
})

test_that("censo() y conteo() tienen los mismos argumentos", {
  expect_true("conteo" %in% getNamespaceExports("importinegi"))
  expect_identical(formals(conteo), formals(censo))
  expect_identical(names(formals(censo)), c("year", "base", "estado", "tabla", "nivel"))
  expect_null(formals(censo)$estado)
})

test_that("se conservan los argumentos de la serie 1.x y su orden", {
  for (nombre in names(interfaz_1x)) {
    esperados <- interfaz_1x[[nombre]]
    actuales  <- names(formals(getExportedValue("importinegi", nombre)))
    expect_identical(actuales[seq_along(esperados)], esperados, info = nombre)
  }
})
