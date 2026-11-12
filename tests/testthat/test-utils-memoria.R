test_that("la memoria de la sesion guarda, devuelve y borra valores", {
  withr::defer(memoria_borrar("prueba_a"))
  withr::defer(memoria_borrar("prueba_b"))
  expect_null(memoria_obtener("prueba_a"))
  expect_identical(memoria_guardar("prueba_a", 1:3), 1:3)
  expect_invisible(memoria_guardar("prueba_b", "x"))
  expect_identical(memoria_obtener("prueba_a"), 1:3)
  expect_identical(memoria_obtener("prueba_b"), "x")

  memoria_borrar("prueba_a")
  expect_null(memoria_obtener("prueba_a"))
  expect_identical(memoria_obtener("prueba_b"), "x")

  # Borrar una clave que no existe no produce error
  expect_null(memoria_borrar("clave_que_no_existe"))
})

test_that("memoria_borrar() sin clave vacia toda la memoria", {
  memoria_guardar("prueba_a", 1)
  memoria_guardar("prueba_b", 2)
  memoria_borrar()
  expect_null(memoria_obtener("prueba_a"))
  expect_null(memoria_obtener("prueba_b"))
})
