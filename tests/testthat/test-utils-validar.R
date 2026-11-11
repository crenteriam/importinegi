test_that("validar_opcion() acepta opciones validas y devuelve el valor", {
  expect_identical(validar_opcion("trim1", paste0("trim", 1:4), "trimestre"), "trim1")
  expect_identical(validar_opcion(2010, c(2000, 2010), "year"), 2010)
})

test_that("validar_opcion() lista las opciones disponibles cuando el valor no existe", {
  expect_error(
    validar_opcion("trim5", paste0("trim", 1:4), "trimestre"),
    "`trimestre = \"trim5\"` no es una opci\u00f3n v\u00e1lida.*\"trim1\", \"trim2\", \"trim3\", \"trim4\""
  )
  expect_error(validar_opcion(2015, c(2000, 2010), "year"), "`year = 2015`.*2000, 2010")
})

test_that("los argumentos faltantes producen un mensaje claro y no un error interno", {
  expect_error(validar_opcion(NA, c("a", "b"), "datos"), "Falta indicar el argumento `datos`")
  expect_error(validar_opcion(NULL, c("a", "b"), "datos"), "Falta indicar el argumento `datos`")
  expect_error(validar_escalar(c(1, 2), "year"), "Falta indicar el argumento `year`")
})

test_that("validar_logico() solo acepta TRUE o FALSE", {
  expect_true(validar_logico(TRUE, "integrar"))
  expect_error(validar_logico("si", "integrar"), "`integrar` debe ser TRUE o FALSE")
  expect_error(validar_logico(NA, "integrar"), "`integrar` debe ser TRUE o FALSE")
})

test_that("detener() no muestra la llamada interna", {
  error <- tryCatch(detener("mensaje ", "de prueba"), error = function(e) e)
  expect_null(conditionCall(error))
  expect_identical(conditionMessage(error), "mensaje de prueba")
})

test_that("enumerar() abrevia las listas largas", {
  expect_identical(enumerar(c("a", "b")), "\"a\", \"b\"")
  expect_match(enumerar(1:100, maximo = 3), "^1, 2, 3, \\.\\.\\.$")
})
