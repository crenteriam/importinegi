test_that("inegi_descomprimir() usa una carpeta nueva en cada llamada", {
  antes <- list.files(getwd(), recursive = TRUE)
  primera <- inegi_descomprimir(muestra("muestra_dbf.zip"))
  segunda <- inegi_descomprimir(muestra("muestra_dbf.zip"))
  expect_false(identical(primera, segunda))
  expect_true(dir.exists(primera))
  expect_setequal(
    list.files(primera, recursive = TRUE),
    c("ITER_01DBF10.dbf", "carpeta/OTRA.DBF")
  )
  # No deja archivos en la carpeta de trabajo
  expect_identical(list.files(getwd(), recursive = TRUE), antes)
})

test_that("inegi_descomprimir() abre los zip que vienen dentro de otro zip", {
  carpeta <- inegi_descomprimir(muestra("anidado.zip"))
  expect_true(file.exists(file.path(carpeta, "interno", "datos.dbf")))
  expect_true(file.exists(file.path(carpeta, "leeme.txt")))

  sin_anidados <- inegi_descomprimir(muestra("anidado.zip"), anidados = FALSE)
  expect_false(dir.exists(file.path(sin_anidados, "interno")))
  expect_true(file.exists(file.path(sin_anidados, "interno.zip")))
})

test_that("inegi_descomprimir() rechaza lo que no es un zip", {
  expect_error(
    inegi_descomprimir(muestra("no_encontrada.html")),
    "no_encontrada.html no es un zip v\u00e1lido"
  )
})

test_that("inegi_descomprimir() avisa si el zip esta danado", {
  danado <- tempfile(fileext = ".zip")
  contenido <- readBin(muestra("muestra_dbf.zip"), "raw", n = 60)
  writeBin(contenido, danado)
  expect_error(inegi_descomprimir(danado), "No se pudo descomprimir")
})

test_that("buscar_archivos() filtra por extension y patron sin distinguir mayusculas", {
  carpeta <- inegi_descomprimir(muestra("muestra_dbf.zip"))
  expect_identical(
    basename(buscar_archivos(carpeta, extension = "dbf")),
    c("ITER_01DBF10.dbf", "OTRA.DBF")
  )
  expect_identical(basename(buscar_archivos(carpeta, patron = "^iter_")), "ITER_01DBF10.dbf")
  expect_identical(basename(buscar_archivos(carpeta, patron = "otra", extension = "DBF")), "OTRA.DBF")
  expect_length(buscar_archivos(carpeta, extension = "csv"), 0)
})

test_that("buscar_archivos() muestra el contenido cuando no encuentra lo esperado", {
  carpeta <- inegi_descomprimir(muestra("muestra_dbf.zip"))
  expect_error(
    buscar_archivos(carpeta, patron = "viviendas", extension = "dbf", exigir = TRUE),
    "no contiene lo que se esperaba \\(patr\u00f3n \"viviendas\"\\).*ITER_01DBF10.dbf"
  )
})
