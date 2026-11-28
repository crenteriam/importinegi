# Las muestras de fixtures/sig las genera data-raw/muestras_sig.R.

# Servidor local con las muestras, sin esperas y sin indices en la memoria.
zip_local <- function(entorno = parent.frame()) {
  memoria_borrar()
  withr::defer(memoria_borrar(), envir = entorno)
  servidor <- servidor_prueba(entorno)
  withr::local_options(
    importinegi.base = sub("/$", "", servidor$url("/")), importinegi.espera = 0, .local_envir = entorno
  )
  servidor
}

ruta_producto <- function(archivo) {
  paste0("/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/", archivo)
}

test_that("zip_numero() y zip_campo() leen enteros con el byte menos significativo primero", {
  expect_identical(zip_numero(as.raw(c(0x01, 0x02))), 513)
  expect_identical(zip_numero(as.raw(c(0xff, 0xff, 0xff, 0xff))), 4294967295)
  # Ocho bytes: mas alla de lo que cabe en un entero de R
  expect_identical(zip_numero(as.raw(c(0, 0, 0, 0, 1, 0, 0, 0))), 4294967296)
  bytes <- as.raw(c(0xaa, 0x10, 0x27, 0x00, 0x00))
  expect_identical(zip_campo(bytes, 1, 1, 2), 10000)
})

test_that("zip_indice_local() lista el contenido de un zip igual que utils::unzip()", {
  for (archivo in c("muestra_dbf.zip", "anidado.zip", "sig/capa_2025_1_00_MUN.zip")) {
    indice <- zip_indice_local(muestra(archivo))
    listado <- utils::unzip(muestra(archivo), list = TRUE)
    expect_named(indice, c("nombre", "original", "metodo", "comprimido", "tamano", "posicion"))
    expect_identical(indice$nombre, listado$Name, info = archivo)
    expect_identical(indice$original, listado$Name, info = archivo)
    expect_equal(indice$tamano, listado$Length, info = archivo)
    expect_true(all(indice$metodo %in% c(0, 8)), info = archivo)
  }
})

test_that("zip_indice_local() distingue los archivos guardados sin comprimir", {
  indice <- zip_indice_local(muestra("sig/producto_889463526636_s.zip"))
  expect_identical(indice$nombre, c("01_aguascalientes.zip", "06_colima.zip"))
  expect_identical(indice$metodo, c(0, 0))
  expect_identical(indice$comprimido, indice$tamano)
  # La posicion es la del encabezado de cada archivo dentro del zip
  bytes <- readBin(muestra("sig/producto_889463526636_s.zip"), what = "raw", n = 1e5)
  for (posicion in indice$posicion) {
    expect_true(zip_firma(bytes, posicion + 1, c(0x50, 0x4b, 0x03, 0x04)))
  }
})

test_that("zip_indice_local() lee nombres que no son UTF-8 y separadores con barra invertida", {
  # Nombres en la pagina de codigos de DOS: cada byte que no es ASCII queda
  # como "_" y el nombre original no se puede usar
  indice <- zip_indice_local(muestra("cndhf/cndhf2017_capacitacion.zip"))
  ajenos <- is.na(indice$original)
  expect_true(any(ajenos))
  expect_true(all(grepl("_", indice$nombre[ajenos], fixed = TRUE)))
  expect_true(all(validUTF8(indice$nombre)))
  expect_false(any(grepl("\\", indice$nombre, fixed = TRUE)))
  # La barra invertida se devuelve como "/", y el nombre original se conserva
  indice <- zip_indice_local(muestra("cndhf/cndhf2017_participacion.zip"))
  expect_true("metadatos/" %in% indice$nombre)
  expect_true("metadatos\\" %in% indice$original)

  indice <- zip_indice_local(muestra("sig/producto_702825296520_s.zip"))
  expect_identical(sum(is.na(indice$original)), 1L)
  expect_match(indice$nombre[is.na(indice$original)], "^LEEME MARCO GEOESTAD_STICO 2010")
})

test_that("zip_indice_local() lee el indice en formato zip64", {
  indice <- zip_indice_local(muestra("sig/zip64.zip"))
  expect_identical(indice$nombre, c("carpeta/uno.txt", "dos.txt"))
  expect_identical(indice$tamano, c(360, 18))
  expect_identical(indice$metodo, c(8, 8))
  expect_identical(indice$posicion[1], 0)
  expect_gt(indice$posicion[2], 0)
  # El indice coincide con el contenido
  carpeta <- withr::local_tempdir()
  utils::unzip(muestra("sig/zip64.zip"), exdir = carpeta)
  expect_identical(file.size(file.path(carpeta, indice$nombre)), indice$tamano)
})

test_that("zip_indice_local() encuentra el indice aunque no quepa en el tramo final", {
  completo <- zip_indice_local(muestra("sig/producto_702825296520_s.zip"))
  local_mocked_bindings(zip_tamano_cola = function() 100)
  expect_identical(zip_indice_local(muestra("sig/producto_702825296520_s.zip")), completo)
  expect_identical(zip_indice_local(muestra("sig/zip64.zip"))$nombre, c("carpeta/uno.txt", "dos.txt"))
})

test_that("zip_indice_local() rechaza lo que no es un zip", {
  expect_error(zip_indice_local(muestra("no_encontrada.html")), "no es un zip v\u00e1lido")
  # Un archivo que empieza como zip pero no tiene indice
  truncado <- withr::local_tempfile(fileext = ".zip")
  writeBin(readBin(muestra("muestra_dbf.zip"), what = "raw", n = 60), truncado)
  expect_error(zip_indice_local(truncado), "no se encontr\u00f3 su \u00edndice")
})

test_that("zip_exigir_metodos() explica que R no abre Deflate64", {
  indice <- zip_indice_local(muestra("sig/capa_deflate64.zip"))
  expect_true(9 %in% indice$metodo)
  expect_error(
    zip_exigir_metodos(indice, muestra("sig/capa_deflate64.zip")),
    "R no puede descomprimir .*2025_1_00_FM[.]dbf.* de capa_deflate64[.]zip: el INEGI lo comprimi\u00f3 con Deflate64"
  )
  # utils::unzip() solo avisa que el zip esta dañado
  carpeta <- withr::local_tempdir()
  expect_warning(utils::unzip(muestra("sig/capa_deflate64.zip"), exdir = carpeta))
  expect_true(zip_exigir_metodos(zip_indice_local(muestra("muestra_dbf.zip")), "muestra_dbf.zip"))
  expect_identical(
    unname(zip_metodo(c(0, 8, 9, 12, 14, 99))),
    c("sin comprimir", "Deflate", "Deflate64", "BZIP2", "LZMA", "m\u00e9todo 99")
  )
})

test_that("zip_posicion() escribe posiciones grandes sin notacion cientifica", {
  expect_identical(zip_posicion(2623314163), "2623314163")
  expect_identical(zip_posicion(1e5), "100000")
  expect_identical(zip_posicion(0), "0")
})

test_that("zip_remoto_indice() lee el indice de un zip remoto sin descargarlo completo", {
  servidor <- zip_local()
  archivo <- muestra("sig/producto_794551067284_s.zip")
  local_mocked_bindings(zip_tamano_cola = function() 400)
  antes <- descargas_sig(servidor)
  indice <- zip_remoto_indice(ruta_producto("794551067284_s.zip"))
  despues <- descargas_sig(servidor)
  expect_identical(indice, zip_indice_local(archivo))
  expect_identical(indice$nombre, c("01_aguascalientes.zip", "06_colima.zip", "mgagro2022_integrado.zip"))
  # Una sola solicitud, de los ultimos 400 bytes
  expect_identical(despues$solicitudes - antes$solicitudes, 1)
  expect_identical(despues$bytes - antes$bytes, 400)
  # La segunda vez sale de la memoria de la sesion
  expect_identical(zip_remoto_indice(ruta_producto("794551067284_s.zip")), indice)
  expect_identical(descargas_sig(servidor)$solicitudes, despues$solicitudes)
})

test_that("zip_remoto_indice() pide el indice aparte cuando no cabe en el tramo final", {
  servidor <- zip_local()
  local_mocked_bindings(zip_tamano_cola = function() 60)
  antes <- descargas_sig(servidor)
  indice <- zip_remoto_indice(ruta_producto("794551067284_s.zip"))
  despues <- descargas_sig(servidor)
  expect_identical(indice, zip_indice_local(muestra("sig/producto_794551067284_s.zip")))
  expect_identical(despues$solicitudes - antes$solicitudes, 2)
  expect_lt(despues$bytes - antes$bytes, file.size(muestra("sig/producto_794551067284_s.zip")))
})

test_that("zip_remoto_indice() se detiene si el archivo no existe o el servidor no atiende la solicitud parcial", {
  servidor <- zip_local()
  # El INEGI responde 200 con una pagina HTML cuando el archivo no existe
  expect_error(
    zip_remoto_indice(ruta_producto("no_existe_s.zip")),
    "respondi\u00f3 con el c\u00f3digo 200 a la descarga parcial de .*no_existe_s[.]zip"
  )
  expect_error(zip_remoto_indice("/datos/404.zip"), "c\u00f3digo 404 a la descarga parcial")
  # Servidor que no atiende la cabecera Range: responde con el archivo completo
  expect_error(zip_remoto_indice("/datos/muestra_dbf.zip"), "c\u00f3digo 200 a la descarga parcial")
})

test_that("zip_remoto_entrada() descarga solo una parte del zip remoto", {
  servidor <- zip_local()
  local_mocked_bindings(zip_tamano_cola = function() 400)
  url <- ruta_producto("794551067284_s.zip")
  indice <- zip_remoto_indice(url)
  entrada <- indice[indice$nombre == "06_colima.zip", ]
  destino <- file.path(withr::local_tempdir(), "sub", "colima.zip")
  antes <- descargas_sig(servidor)
  expect_message(
    resultado <- zip_remoto_entrada(url, entrada, destino),
    "Descargando 06_colima[.]zip \\(.*es una parte de 794551067284_s[.]zip\\)"
  )
  despues <- descargas_sig(servidor)
  expect_identical(resultado, destino)
  # Dos solicitudes: el encabezado del archivo (30 bytes) y sus datos
  expect_identical(despues$solicitudes - antes$solicitudes, 2)
  expect_identical(despues$bytes - antes$bytes, 30 + entrada$comprimido)
  # Lo descargado es identico al archivo que contiene el producto
  carpeta <- withr::local_tempdir()
  utils::unzip(muestra("sig/producto_794551067284_s.zip"), files = "06_colima.zip", exdir = carpeta)
  expect_identical(unname(tools::md5sum(destino)), unname(tools::md5sum(file.path(carpeta, "06_colima.zip"))))
  expect_true(es_zip(destino))
  expect_false(file.exists(paste0(destino, ".parcial")))
  expect_silent(zip_remoto_entrada(url, entrada, destino, silencioso = TRUE))
})

test_that("zip_remoto_entrada() no descarga por separado un archivo comprimido", {
  servidor <- zip_local()
  url <- "/contenidos/descargadenue/MGdescarga/MGN2025/2025_1_00_MUN.zip"
  indice <- zip_remoto_indice(url)
  entrada <- indice[indice$nombre == "2025_1_00_MUN.shp", ]
  expect_identical(entrada$metodo, 8)
  expect_error(
    zip_remoto_entrada(url, entrada, withr::local_tempfile()),
    "2025_1_00_MUN[.]shp est\u00e1 comprimido dentro de 2025_1_00_MUN[.]zip"
  )
})

test_that("zip_remoto_entrada() comprueba que el indice corresponda al contenido", {
  servidor <- zip_local()
  url <- ruta_producto("794551067284_s.zip")
  entrada <- zip_remoto_indice(url)[2, ]
  entrada$posicion <- entrada$posicion + 5
  expect_error(zip_remoto_entrada(url, entrada, withr::local_tempfile(), silencioso = TRUE), "no corresponde a su contenido")
})
