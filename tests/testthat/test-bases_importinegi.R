test_that("bases_importinegi() lista las bases del paquete sin conexion", {
  # Sin servidor de pruebas: la funcion no debe pedir nada a internet
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  bases <- bases_importinegi()
  expect_s3_class(bases, "data.frame")
  expect_named(bases, c("proyecto", "year", "base", "descripcion", "tablas", "cobertura", "formato", "tamano_mb"))
  expect_type(bases$year, "integer")
  expect_type(bases$tamano_mb, "double")
  expect_identical(attr(bases, "registro"), registro_generado())
  expect_identical(rownames(bases), as.character(seq_len(nrow(bases))))
  expect_false(anyNA(bases))
  expect_identical(anyDuplicated(bases[, c("proyecto", "year", "base")]), 0L)
  expect_true(all(bases$tamano_mb > 0))
  expect_true(all(bases$cobertura %in% c("nacional y por entidad", "por entidad")))
  expect_true(all(bases$formato %in% c("dbf", "csv")))
})

test_that("bases_importinegi() describe las bases del Censo de Poblacion y Vivienda", {
  censo_bases <- bases_importinegi("censo")
  expect_identical(unique(censo_bases$proyecto), "censo")
  resumen <- split(censo_bases$base, censo_bases$year)
  expect_identical(resumen[["1990"]], c("iter", "muestra"))
  expect_identical(resumen[["2000"]], c("iter", "muestra"))
  expect_identical(resumen[["2010"]], c("iter", "ageb", "muestra", "localidades", "entorno"))
  expect_identical(resumen[["2020"]], c("iter", "ageb", "muestra", "localidades", "entorno", "asistencia"))

  fila <- function(year, base) censo_bases[censo_bases$year == year & censo_bases$base == base, ]
  expect_identical(fila(2010, "iter")$cobertura, "nacional y por entidad")
  expect_identical(fila(2010, "iter")$formato, "dbf")
  expect_identical(fila(2010, "iter")$descripcion, "Principales resultados por localidad (ITER)")
  expect_identical(fila(2010, "iter")$tablas, "")
  expect_identical(fila(2010, "ageb")$cobertura, "por entidad")
  expect_identical(fila(2010, "muestra")$tablas, "personas, viviendas, migrantes")
  expect_identical(fila(1990, "muestra")$tablas, "")
  expect_identical(fila(2010, "entorno")$tablas, "")
  expect_identical(fila(2020, "entorno")$tablas, "manzanas, vialidades")
  expect_identical(fila(2020, "asistencia")$tablas, "alojamientos, usuarios, trabajadores")
  expect_identical(fila(2020, "iter")$formato, "csv")
  expect_identical(fila(2020, "muestra")$cobertura, "nacional y por entidad")
  # Tamano: el archivo nacional si existe; si no, la suma de las entidades
  expect_equal(fila(2010, "iter")$tamano_mb, 37.6)
  expect_equal(fila(2010, "muestra")$tamano_mb, 345.3, tolerance = 0.01)
})

test_that("bases_importinegi() coincide con lo que censo() puede descargar", {
  censo_bases <- bases_importinegi("censo")
  curadas <- poblacion_bases()
  curadas <- curadas[curadas$programa == "censo", ]
  expect_identical(paste(censo_bases$year, censo_bases$base), paste(curadas$year, curadas$base))
  for (i in seq_len(nrow(censo_bases))) {
    etiqueta <- paste(censo_bases$year[i], censo_bases$base[i])
    expect_identical(poblacion_validar_base("censo", censo_bases$year[i], censo_bases$base[i]),
                     censo_bases$base[i], info = etiqueta)
    archivos <- poblacion_archivos("censo", censo_bases$year[i], censo_bases$base[i])
    expect_identical("00" %in% archivos$clave, censo_bases$cobertura[i] == "nacional y por entidad", info = etiqueta)
  }
})

test_that("bases_importinegi() filtra por proyecto y por a\u00f1o", {
  expect_identical(bases_importinegi("CENSO "), bases_importinegi("censo"))
  expect_identical(bases_importinegi(proyecto = "censo"), bases_importinegi())
  de_2020 <- bases_importinegi(year = 2020)
  expect_identical(unique(de_2020$year), 2020L)
  expect_identical(nrow(de_2020), 6L)
  expect_identical(bases_importinegi("censo", "2020"), de_2020)
  varios <- bases_importinegi(year = c(1990, 2000))
  expect_identical(unique(varios$year), c(1990L, 2000L))
  expect_identical(nrow(varios), 4L)

  # Un año sin bases (los conteos todavia no estan en el paquete)
  expect_message(vacia <- bases_importinegi(year = 1995), "no tiene bases de datos para esa consulta")
  expect_identical(nrow(vacia), 0L)
  expect_named(vacia, names(bases_importinegi()))
})

test_that("bases_importinegi() valida sus argumentos", {
  expect_error(bases_importinegi("enoe"), "`proyecto = \"enoe\"` no es una opci\u00f3n v\u00e1lida. Proyectos disponibles: \"censo\"")
  expect_error(bases_importinegi(2020), "`proyecto` debe ser el nombre de un proyecto")
  expect_error(bases_importinegi(NA_character_), "`proyecto` debe ser el nombre de un proyecto")
  expect_error(bases_importinegi(year = "dos mil"), "`year` debe ser uno o varios a\u00f1os")
  expect_error(bases_importinegi(year = c(2010, NA)), "`year` debe ser uno o varios a\u00f1os")
})

test_that("bases_importinegi() solo muestra lo que esta en el registro", {
  registro <- registro_archivos[!(registro_archivos$proyecto == "ccpv" & registro_archivos$edicion %in% 2020L), ]
  attr(registro, "generado") <- "2026-11-11"
  withr::local_options(importinegi.registro = registro)
  bases <- bases_importinegi()
  expect_false(2020L %in% bases$year)
  expect_identical(nrow(bases), 9L)
  expect_identical(attr(bases, "registro"), "2026-11-11")
})

test_that("los errores de censo() remiten a bases_importinegi()", {
  expect_error(censo(2000, "ageb"), "Consulta todas con bases_importinegi\\(\"censo\"\\)")
  expect_error(censo(2010, "enoe"), "Consulta todas con bases_importinegi\\(\"censo\"\\)")
})
