test_that("requiere_paquete() acepta un paquete instalado", {
  expect_true(requiere_paquete("foreign"))
  expect_invisible(requiere_paquete("foreign"))
})

test_that("requiere_paquete() explica como instalar un paquete ausente", {
  expect_error(
    requiere_paquete("paqueteQueNoExiste123"),
    "requiere el paquete 'paqueteQueNoExiste123'.*install.packages\\(\"paqueteQueNoExiste123\"\\)"
  )
})

test_that("las funciones geograficas se detienen con un mensaje claro si falta sf", {
  local_mocked_bindings(
    requiere_paquete = function(paquete) stop("falta ", paquete, call. = FALSE)
  )
  expect_error(sig_caminos_extrae(tempdir(), mapa = "red_vial"), "falta sf")
  expect_error(sig_marcogeo(year = 2009, mapa = "municipios"), "falta sf")
})
