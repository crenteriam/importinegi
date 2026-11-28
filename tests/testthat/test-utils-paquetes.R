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
  expect_error(sig_marcogeoestadistico(year = 2009, mapa = "municipios"), "falta sf")
})

test_that("requiere_paquete() indica la version anterior de readxl en R 4.1", {
  local_mocked_bindings(version_r = function() numeric_version("4.1.3"))
  expect_identical(
    ayuda_version_anterior("readxl"),
    paste0(" La versi\u00f3n actual de readxl requiere R 4.2 o posterior. Si install.packages() no lo encuentra en ",
           "tu versi\u00f3n de R (4.1.3), instala la versi\u00f3n 1.5.0.1, la \u00faltima que admite R 4.1: ",
           "remotes::install_version(\"readxl\", \"1.5.0.1\").")
  )
  # Solo para readxl y solo en R anterior a 4.2
  expect_null(ayuda_version_anterior("sf"))
  local_mocked_bindings(version_r = function() numeric_version("4.2.0"))
  expect_null(ayuda_version_anterior("readxl"))
})
