test_that("rellenar_clave() completa las claves con ceros a la izquierda", {
  expect_identical(rellenar_clave(c(6L, 18L, NA), 2L), c("06", "18", NA))
  expect_identical(rellenar_clave(c(1, 10, 100), 3L), c("001", "010", "100"))
  expect_identical(rellenar_clave(c("6", "06", " 18 ", NA, ""), 2L), c("06", "06", "18", NA, ""))
  expect_identical(rellenar_clave(c("12", "003A", "1A"), 4L), c("0012", "003A", "001A"))
})

test_that("poblacion_normalizar_claves() reconoce las claves por nombre", {
  datos <- data.frame(ent = c(6, 18), CVE_MUN = c("1", "20"), LOC = c("1", "9998"),
                      POBTOT = c(5L, 6L), stringsAsFactors = FALSE)
  normal <- poblacion_normalizar_claves(datos)
  expect_identical(normal$ent, c("06", "18"))
  expect_identical(normal$CVE_MUN, c("001", "020"))
  expect_identical(normal$LOC, c("0001", "9998"))
  expect_identical(normal$POBTOT, c(5L, 6L))
  expect_identical(columna_clave(datos, "ent"), "ent")
  expect_identical(columna_clave(datos, "mun"), "CVE_MUN")
  expect_identical(columna_clave(datos, "ageb"), NA_character_)
})

test_that("poblacion_clasificar_nivel() identifica los totales del ITER", {
  iter <- data.frame(
    ENTIDAD = c("00", "00", "06", "06", "06", "06", "06", "06"),
    MUN     = c("000", "000", "000", "000", "001", "001", "001", "001"),
    LOC     = c("0000", "9998", "0000", "9999", "0000", "0001", "9998", "0350"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    poblacion_clasificar_nivel(iter, "iter"),
    c("nacional", "agregado", "entidad", "agregado", "municipio", "localidad", "agregado", "localidad")
  )
  expect_null(poblacion_clasificar_nivel(iter[, c("ENTIDAD", "MUN")], "iter"))
})

test_that("poblacion_clasificar_nivel() identifica los totales por AGEB y manzana", {
  ageb <- data.frame(
    ENTIDAD = "06",
    MUN  = c("000", "002", "002", "002", "002"),
    LOC  = c("0000", "0000", "0001", "0001", "0001"),
    AGEB = c("0000", "0000", "0000", "003A", "003A"),
    MZA  = c("000", "000", "000", "000", "010"),
    stringsAsFactors = FALSE
  )
  expect_identical(poblacion_clasificar_nivel(ageb, "ageb"),
                   c("entidad", "municipio", "localidad", "ageb", "manzana"))
  expect_null(poblacion_clasificar_nivel(ageb[, c("ENTIDAD", "MUN", "LOC")], "ageb"))
})

test_that("poblacion_filtrar_nivel() avisa si la tabla no tiene claves geograficas", {
  datos <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  expect_identical(poblacion_filtrar_nivel(datos, "iter", "todos", TRUE), datos)
  expect_error(poblacion_filtrar_nivel(datos, "iter", "municipio", TRUE),
               "No se puede filtrar por `nivel`.*Usa nivel = \"todos\"")
  expect_warning(resultado <- poblacion_filtrar_nivel(datos, "iter", "localidad", FALSE),
                 "se devuelven todas las filas")
  expect_identical(resultado, datos)
})

test_that("combinar_tablas() une entidades con columnas o tipos distintos", {
  a <- data.frame(ENT = "06", X = 1:2, stringsAsFactors = FALSE)
  b <- data.frame(ENT = "18", X = 3L, stringsAsFactors = FALSE)
  expect_identical(combinar_tablas(list(a)), a)
  expect_identical(combinar_tablas(list(a, b)),
                   data.frame(ENT = c("06", "06", "18"), X = 1:3, stringsAsFactors = FALSE))

  # Una columna numerica en una entidad y de texto en otra queda como texto
  texto <- data.frame(ENT = "18", X = "003", stringsAsFactors = FALSE)
  expect_identical(combinar_tablas(list(a, texto))$X, c("1", "2", "003"))

  extra <- data.frame(ENT = "18", Y = "z", stringsAsFactors = FALSE)
  expect_warning(unida <- combinar_tablas(list(a, extra)), "no tienen las mismas columnas")
  expect_named(unida, c("ENT", "X", "Y"))
  expect_identical(unida$X, c(1L, 2L, NA))
  expect_identical(unida$Y, c(NA, NA, "z"))
})

test_that("poblacion_elegir_tabla() encuentra la tabla dentro del archivo descargado", {
  carpeta <- inegi_descomprimir(muestra("censo/muestra2000_06.zip"))
  withr::defer(unlink(carpeta, recursive = TRUE))
  expect_identical(basename(poblacion_elegir_tabla(carpeta, "dbf", "^per", "personas")), "PER_F06.DBF")
  expect_identical(basename(poblacion_elegir_tabla(carpeta, "dbf", "^viv|^vho", "viviendas")), "VHO_F06.DBF")
  expect_error(poblacion_elegir_tabla(carpeta, "dbf", "^hog", "hogares"),
               "No se encontr\u00f3 la tabla \"hogares\".*MIN_F06.DBF.*PER_F06.DBF.*VHO_F06.DBF")
  expect_error(poblacion_elegir_tabla(carpeta, "dbf"), "m\u00e1s de un archivo para la tabla de datos")
  expect_error(poblacion_elegir_tabla(carpeta, "csv"), "no contiene lo que se esperaba")

  # En los zip de datos abiertos se descartan diccionario, catalogos y metadatos
  abiertos <- inegi_descomprimir(muestra("censo/iter2020_06.zip"))
  withr::defer(unlink(abiertos, recursive = TRUE))
  expect_identical(basename(poblacion_elegir_tabla(abiertos, "csv")), "conjunto_de_datos_iter_06CSV20.csv")
})

test_that("las tablas de bases y programas son coherentes", {
  bases <- poblacion_bases()
  programas <- poblacion_programas()
  expect_false(anyNA(bases$reconoce))
  expect_identical(anyDuplicated(bases[, c("programa", "year", "base")]), 0L)
  for (programa in unique(bases$programa)) {
    expect_true(all(bases$year[bases$programa == programa] %in% programas[[programa]]$years), info = programa)
  }
  expect_false(anyNA(poblacion_nombre_base(unique(bases$base))))
  # Ningun año pertenece a dos programas
  expect_identical(anyDuplicated(unlist(lapply(programas, function(p) p$years))), 0L)
})

test_that("poblacion_archivos() devuelve una tabla vacia si la base no existe o no esta en el registro", {
  columnas <- c("clave", "formato", "tamano", "bytes", "url")
  for (caso in list(c(1990, "ageb"), c(2020, "iter"), c(2010, "viviendas"))) {
    archivos <- poblacion_archivos("censo", as.integer(caso[1]), caso[2])
    expect_identical(nrow(archivos), 0L, info = paste(caso, collapse = " "))
    expect_named(archivos, columnas)
  }
  expect_named(poblacion_archivos("censo", 2010L, "iter"), columnas)
})

test_that("registro_url() arma la direccion con la base del sitio", {
  filas <- data.frame(
    ruta = c("/programas/ccpv/2010/microdatos/iter/00_nacional_2010_iter", ""),
    sufijo = c("_dbf.zip", ""),
    url = c("https://www.inegi.org.mx/otra", "http://www.inegi.org.mx/app/cuadro"),
    stringsAsFactors = FALSE
  )
  withr::local_options(importinegi.base = "http://127.0.0.1:9999")
  expect_identical(registro_url(filas), c(
    "http://127.0.0.1:9999/contenidos/programas/ccpv/2010/microdatos/iter/00_nacional_2010_iter_dbf.zip",
    "http://www.inegi.org.mx/app/cuadro"
  ))
})

test_that("el registro del paquete tiene las columnas que usan las funciones", {
  registro <- registro_inegi()
  expect_true(all(c("proyecto", "edicion", "titulo", "descripcion", "formato", "sufijo",
                    "tamano", "bytes", "ruta", "url") %in% names(registro)))
  expect_gt(nrow(registro), 1000)
  expect_match(registro_generado(), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  withr::local_options(importinegi.registro = registro[1:3, ])
  expect_identical(nrow(registro_inegi()), 3L)
})

test_that("tamano_texto() resume los bytes de una descarga", {
  expect_identical(tamano_texto(500), "1 KB")
  expect_identical(tamano_texto(300 * 1024), "300 KB")
  expect_identical(tamano_texto(2.5 * 1024^2), "2.5 MB")
  expect_identical(tamano_texto(c(200, 145.4) * 1024^2), "345 MB")
  expect_identical(tamano_texto(c(1.5 * 1024^3, NA)), "1.5 GB")
})

test_that("leer_tabla() entrega en UTF-8 el texto de los DBF", {
  # iter_06: texto en Latin-1 sin pagina de codigos declarada
  carpeta <- inegi_descomprimir(muestra("censo/iter_06.zip"))
  withr::defer(unlink(carpeta, recursive = TRUE))
  archivo <- buscar_archivos(carpeta, extension = "dbf")
  expect_identical(dbf_codificacion(archivo), NA_character_)
  crudo <- foreign::read.dbf(archivo, as.is = TRUE)
  expect_false(all(validUTF8(crudo$NOM_LOC)))
  leido <- leer_tabla(archivo)
  expect_true(all(validUTF8(leido$NOM_LOC)))
  expect_identical(leido$NOM_LOC[5], "Ciudad de Armer\u00eda")
  expect_identical(leido$POBTOT, crudo$POBTOT)

  # localidades_06: el encabezado declara la pagina de codigos 850
  carpeta850 <- inegi_descomprimir(muestra("censo/localidades_06.zip"))
  withr::defer(unlink(carpeta850, recursive = TRUE))
  archivo850 <- buscar_archivos(carpeta850, extension = "dbf")
  expect_identical(dbf_codificacion(archivo850), "CP850")
  expect_identical(leer_tabla(archivo850)$NOM_LOC, c("El Para\u00edso", "Rinc\u00f3n de L\u00f3pez"))
})
