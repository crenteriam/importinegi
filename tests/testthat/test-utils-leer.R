iter_muestra <- function() {
  carpeta <- inegi_descomprimir(muestra("muestra_dbf.zip"))
  buscar_archivos(carpeta, patron = "^ITER", extension = "dbf", exigir = TRUE)
}

test_that("leer_tabla() lee un DBF igual que foreign::read.dbf(as.is = TRUE)", {
  archivo <- iter_muestra()
  expect_identical(leer_tabla(archivo), foreign::read.dbf(archivo, as.is = TRUE))
  expect_identical(leer_tabla(archivo)$ENTIDAD, c("01", "01", "01"))
})

test_that("leer_tabla() lee DBF con la extension en mayusculas", {
  carpeta <- inegi_descomprimir(muestra("muestra_dbf.zip"))
  otra <- leer_tabla(buscar_archivos(carpeta, patron = "otra"))
  expect_identical(otra$VALOR, c(1.5, 2))
})

test_that("leer_tabla() convierte los codigos de dato faltante en NA", {
  datos <- leer_tabla(iter_muestra(), faltantes = c("*", "N/D"))
  expect_identical(datos$POBTOT, c("1184996", "797010", NA))
  expect_identical(datos$VPH_PISODT, c("270984", NA, NA))
  expect_identical(datos$NOM_LOC[3], "Aguascalientes")
})

test_that("leer_tabla() convierte a numero solo las columnas que no son claves", {
  datos <- leer_tabla(iter_muestra(), faltantes = c("*", "N/D"), convertir = TRUE)
  expect_identical(datos$POBTOT, c(1184996L, 797010L, NA))
  expect_identical(datos$PROM_OCUP, c(4.08, 3.9, NA))
  # Las claves geograficas conservan sus ceros a la izquierda
  expect_identical(datos$ENTIDAD, c("01", "01", "01"))
  expect_identical(datos$MUN, c("000", "001", "001"))
  expect_identical(datos$LOC, c("0000", "0000", "0001"))
  expect_type(datos$NOM_LOC, "character")
})

test_that("sin limpiar los faltantes, una columna con '*' queda como texto", {
  datos <- leer_tabla(iter_muestra(), convertir = TRUE)
  expect_type(datos$POBTOT, "character")
})

test_that("convertir_numericas() respeta las columnas protegidas y los valores especiales", {
  datos <- data.frame(
    ENT = c("10", "32"), n = c("5", "12"), decimal = c("-1.5", "2"),
    mixto = c("1", "a"), vacia = c(NA_character_, NA_character_),
    grande = c("3000000000", "1"), ya_numerica = c(1.5, 2),
    stringsAsFactors = FALSE
  )
  resultado <- convertir_numericas(datos, texto = "ent")
  expect_identical(resultado$ENT, c("10", "32"))
  expect_identical(resultado$n, c(5L, 12L))
  expect_identical(resultado$decimal, c(-1.5, 2))
  expect_identical(resultado$mixto, c("1", "a"))
  expect_identical(resultado$vacia, c(NA_character_, NA_character_))
  expect_identical(resultado$grande, c(3e9, 1))
  expect_identical(resultado$ya_numerica, c(1.5, 2))
  # Sin proteger, una clave sin ceros a la izquierda se convierte
  expect_identical(convertir_numericas(datos)$ENT, c(10L, 32L))
})

test_that("leer_tabla() lee CSV en UTF-8 con BOM y conserva las claves", {
  datos <- leer_tabla(muestra("utf8_bom.csv"))
  expect_named(datos, c("ENTIDAD", "NOM_ENT", "POBTOT", "PROM_OCUP"))
  expect_identical(datos$ENTIDAD, c("01", "09", "19"))
  expect_identical(datos$NOM_ENT, c("Aguascalientes", "Ciudad de M\u00e9xico", "Nuevo Le\u00f3n"))

  limpio <- leer_tabla(muestra("utf8_bom.csv"), faltantes = c("*", "N/D"), convertir = TRUE)
  expect_identical(limpio$POBTOT, c(1425607L, 9209944L, NA))
  expect_identical(limpio$PROM_OCUP, c(3.8, 3.3, NA))
  expect_identical(limpio$ENTIDAD, c("01", "09", "19"))
})

test_that("leer_tabla() lee CSV en Latin-1", {
  datos <- leer_tabla(muestra("latin1.csv"))
  expect_identical(datos$NOM_ENT, c("Michoac\u00e1n de Ocampo", "Quer\u00e9taro"))
  expect_identical(datos$ENTIDAD, c("16", "22"))
})

test_that("leer_tabla() avisa cuando el formato no esta soportado", {
  expect_error(
    leer_tabla(muestra("no_encontrada.html")),
    "el formato \"html\" no est\u00e1 soportado"
  )
})
