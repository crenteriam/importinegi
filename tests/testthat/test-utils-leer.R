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

test_that("leer_dbf() omite los registros marcados como borrados", {
  archivo <- withr::local_tempfile(fileext = ".dbf")
  datos <- data.frame(CLAVE = c("A001", "A002", "A003", "A004"), GASTO = c(10.5, 20, 30.25, 40),
                      stringsAsFactors = FALSE)
  foreign::write.dbf(datos, archivo)
  expect_identical(dbf_borrados(archivo), c(FALSE, FALSE, FALSE, FALSE))
  expect_identical(nrow(leer_dbf(archivo)), 4L)

  # El primer byte de cada registro es un espacio; un asterisco lo marca como borrado
  bytes <- readBin(archivo, "raw", n = file.size(archivo))
  entero <- function(x) sum(as.numeric(x) * 256^(seq_along(x) - 1))
  inicio <- entero(bytes[9:10])
  largo <- entero(bytes[11:12])
  expect_identical(bytes[inicio + 1 + (0:3) * largo], rep(as.raw(0x20), 4))
  bytes[inicio + 1 + c(1, 3) * largo] <- as.raw(0x2a)
  writeBin(bytes, archivo)

  expect_identical(dbf_borrados(archivo), c(FALSE, TRUE, FALSE, TRUE))
  # foreign::read.dbf() no revisa la marca
  expect_identical(nrow(foreign::read.dbf(archivo, as.is = TRUE)), 4L)
  leidos <- leer_dbf(archivo)
  expect_identical(leidos$CLAVE, c("A001", "A003"))
  expect_identical(leidos$GASTO, c(10.5, 30.25))
  expect_identical(rownames(leidos), c("1", "2"))
})

test_that("dbf_borrados() tolera archivos vacios, cortos o con muchos registros", {
  vacio <- withr::local_tempfile(fileext = ".dbf")
  file.create(vacio)
  expect_identical(dbf_borrados(vacio), logical())

  # Un archivo mas corto de lo que declara su encabezado
  archivo <- withr::local_tempfile(fileext = ".dbf")
  foreign::write.dbf(data.frame(CLAVE = sprintf("A%03d", 1:6), stringsAsFactors = FALSE), archivo)
  bytes <- readBin(archivo, "raw", n = file.size(archivo))
  entero <- function(x) sum(as.numeric(x) * 256^(seq_along(x) - 1))
  inicio <- entero(bytes[9:10])
  largo <- entero(bytes[11:12])
  corto <- withr::local_tempfile(fileext = ".dbf")
  writeBin(bytes[seq_len(inicio + 4 * largo)], corto)
  expect_identical(dbf_borrados(corto), rep(FALSE, 4))

  # Mas registros de los que caben en un bloque de lectura
  grande <- withr::local_tempfile(fileext = ".dbf")
  filas <- 60000L
  foreign::write.dbf(data.frame(TEXTO = rep(strrep("x", 200), filas), stringsAsFactors = FALSE), grande)
  bytes <- readBin(grande, "raw", n = file.size(grande))
  inicio <- entero(bytes[9:10])
  largo <- entero(bytes[11:12])
  expect_gt(filas * largo, 8 * 1024^2)
  borrar <- c(1L, 41235L, 41236L, filas)
  bytes[inicio + 1 + (borrar - 1) * largo] <- as.raw(0x2a)
  writeBin(bytes, grande)
  expect_identical(which(dbf_borrados(grande)), borrar)
  expect_identical(nrow(leer_dbf(grande)), filas - 4L)
})

test_that("leer_csv() lee sin avisos un CSV que no termina con salto de linea", {
  archivo <- withr::local_tempfile(fileext = ".csv")
  # Una tabla de una sola fila, como las del censo de derechos humanos federal
  writeBin(charToRaw("re_snn1,re_snn2\r\n1,2"), archivo)
  expect_warning(utils::read.csv(archivo), "readTableHeader")
  expect_no_warning(datos <- leer_csv(archivo))
  expect_identical(datos, data.frame(re_snn1 = "1", re_snn2 = "2"))
  # Con mas filas tampoco se pierde la ultima
  writeBin(charToRaw("a,b\n1,2\n3,4\n5,6"), archivo)
  expect_no_warning(datos <- leer_csv(archivo))
  expect_identical(datos, data.frame(a = c("1", "3", "5"), b = c("2", "4", "6")))
})
