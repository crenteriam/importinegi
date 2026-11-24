# Compatibilidad de la interfaz publica.
# La version 2.0.0 conserva los nombres de las funciones y de sus argumentos
# de la serie 1.x. Se pueden agregar funciones y argumentos nuevos (al final),
# pero no quitar ni reordenar los existentes. Las excepciones son las cinco
# funciones censo_poblacion_*(), que se eliminaron porque cpv_censo() las sustituye;
# censo_municipal(), que se elimino porque cngmd() la sustituye, y
# enigh_nuevaconstruccion(), que se elimino porque enigh_nc() la sustituye.

interfaz_1x <- list(
  catalogo_inegi          = c("id"),
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
  expect_true("cpv_censo" %in% getNamespaceExports("importinegi"))
})

test_that("censo_municipal() ya no se exporta y cngmd() la sustituye", {
  expect_false("censo_municipal" %in% getNamespaceExports("importinegi"))
  expect_true("cngmd" %in% getNamespaceExports("importinegi"))
  argumentos <- formals(cngmd)
  expect_identical(names(argumentos), c("year", "modulo", "tema", "tabla", "estado"))
  expect_null(argumentos$tema)
  expect_null(argumentos$tabla)
  expect_null(argumentos$estado)
})

test_that("cpv_censo(), cpv_conteo() y cpv_intercensal() tienen los mismos argumentos", {
  expect_true(all(c("cpv_conteo", "cpv_intercensal") %in% getNamespaceExports("importinegi")))
  # Los nombres anteriores, sin el prefijo, ya no se exportan
  expect_identical(intersect(c("censo", "conteo", "intercensal"), getNamespaceExports("importinegi")), character())
  expect_identical(formals(cpv_conteo), formals(cpv_censo))
  expect_identical(names(formals(cpv_censo)), c("year", "base", "estado", "tabla", "nivel"))
  expect_null(formals(cpv_censo)$estado)
  # cpv_intercensal() solo cambia la base por omision: la Encuesta Intercensal no
  # tiene ITER
  argumentos <- formals(cpv_intercensal)
  expect_identical(argumentos$base, "muestra")
  argumentos$base <- formals(cpv_censo)$base
  expect_identical(argumentos, formals(cpv_censo))
})

test_that("se conservan los argumentos de la serie 1.x y su orden", {
  for (nombre in names(interfaz_1x)) {
    esperados <- interfaz_1x[[nombre]]
    actuales  <- names(formals(getExportedValue("importinegi", nombre)))
    expect_identical(actuales[seq_along(esperados)], esperados, info = nombre)
  }
})

test_that("enoe() conserva los argumentos de la serie 1.x y agrega `tabla` y `estado`", {
  argumentos <- formals(enoe)
  expect_identical(names(argumentos), c("year", "trimestre", "integrar", "tabla", "estado"))
  expect_false(argumentos$integrar)
  expect_null(argumentos$tabla)
  expect_null(argumentos$estado)
})

test_that("enigh_nuevaconstruccion() ya no se exporta y enigh_nc() la sustituye", {
  expect_false("enigh_nuevaconstruccion" %in% getNamespaceExports("importinegi"))
  expect_true("enigh_nc" %in% getNamespaceExports("importinegi"))
  argumentos <- formals(enigh_nc)
  expect_identical(names(argumentos), c("year", "tabla"))
  expect_identical(argumentos$tabla, "concentrado")
  # Las dos funciones de la ENIGH tienen los mismos argumentos
  expect_identical(formals(enigh_nc), formals(enigh_tradicional))
})

test_that("enigh_tradicional() se exporta con sus dos argumentos", {
  expect_true("enigh_tradicional" %in% getNamespaceExports("importinegi"))
  argumentos <- formals(enigh_tradicional)
  expect_identical(names(argumentos), c("year", "tabla"))
  expect_identical(argumentos$tabla, "concentrado")
})
