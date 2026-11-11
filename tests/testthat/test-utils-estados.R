test_that("el catalogo tiene las 32 entidades y el total nacional, sin repetidos", {
  tabla <- inegi_estados()
  expect_identical(tabla$clave, sprintf("%02d", 0:32))
  expect_false(anyDuplicated(tabla$nombre) > 0)
  expect_false(anyDuplicated(tabla$abreviatura) > 0)
  expect_identical(tabla$nombre[tabla$clave == "09"], "Ciudad de M\u00e9xico")
})

test_that("estado_clave() reconoce todos los nombres que aceptaba la serie 1.x", {
  nombres_1x <- c(
    Nacional = "00", Aguascalientes = "01", "Baja California" = "02",
    "Baja California Sur" = "03", Campeche = "04", Coahuila = "05", Colima = "06",
    Chiapas = "07", Chihuahua = "08", CDMX = "09", Durango = "10",
    Guanajuato = "11", Guerrero = "12", Hidalgo = "13", Jalisco = "14",
    Mexico = "15", Michoacan = "16", Morelos = "17", Nayarit = "18",
    "Nuevo Leon" = "19", Oaxaca = "20", Puebla = "21", Queretaro = "22",
    "Quintana Roo" = "23", "San Luis Potosi" = "24", Sinaloa = "25", Sonora = "26",
    Tabasco = "27", Tamaulipas = "28", Tlaxcala = "29", Veracruz = "30",
    Yucatan = "31", Zacatecas = "32"
  )
  for (nombre in names(nombres_1x)) {
    expect_identical(estado_clave(nombre), nombres_1x[[nombre]], info = nombre)
  }
})

test_that("estado_clave() acepta acentos, mayusculas, abreviaturas y nombres oficiales", {
  expect_identical(estado_clave("Nuevo Le\u00f3n"), "19")
  expect_identical(estado_clave("SAN LUIS POTOS\u00cd"), "24")
  expect_identical(estado_clave("  michoac\u00e1n de ocampo "), "16")
  expect_identical(estado_clave("Ciudad de M\u00e9xico"), "09")
  expect_identical(estado_clave("Distrito Federal"), "09")
  expect_identical(estado_clave("D.F."), "09")
  expect_identical(estado_clave("Estado de M\u00e9xico"), "15")
  expect_identical(estado_clave("Veracruz de Ignacio de la Llave"), "30")
  expect_identical(estado_clave("Q. Roo"), "23")
  expect_identical(estado_clave("slp"), "24")
  expect_identical(estado_clave("Estados Unidos Mexicanos"), "00")
})

test_that("estado_clave() acepta la clave numerica", {
  expect_identical(estado_clave(9), "09")
  expect_identical(estado_clave("09"), "09")
  expect_identical(estado_clave("9"), "09")
  expect_identical(estado_clave(32L), "32")
  expect_identical(estado_clave(0), "00")
  expect_error(estado_clave(33), "no es una clave de entidad v\u00e1lida")
})

test_that("estado_clave() sugiere el nombre correcto ante un error de dedo", {
  expect_error(estado_clave("Jalisko"), "No se reconoce la entidad.*Jalisco")
  expect_error(estado_clave("Narnia"), "No se reconoce la entidad `estado = \"Narnia\"`")
  expect_error(estado_clave(NA), "Falta indicar el argumento `estado`")
})

test_that("estado_clave() puede rechazar el total nacional", {
  expect_identical(estado_clave("Nacional", nacional = TRUE), "00")
  expect_error(estado_clave("Nacional", nacional = FALSE), "solo se publica por entidad federativa")
  expect_identical(estado_clave("Colima", nacional = FALSE), "06")
})

test_that("normalizar_texto() quita acentos, puntuacion y espacios de sobra", {
  expect_identical(normalizar_texto("  Quer\u00e9taro   de ARTEAGA. "), "queretaro de arteaga")
  expect_identical(normalizar_texto("Yucat\u00e1n"), "yucatan")
  expect_identical(normalizar_texto("\u00d1AND\u00da"), "nandu")
})
