test_that("bases_importinegi() lista las bases del paquete sin conexion", {
  # Sin servidor de pruebas: la funcion no debe pedir nada a internet
  withr::local_options(importinegi.base = "http://127.0.0.1:9", importinegi.registro = NULL)
  bases <- bases_importinegi()
  expect_s3_class(bases, "data.frame")
  expect_named(bases, c("proyecto", "year", "modulo", "base", "descripcion", "tablas", "cobertura", "formato", "tamano_mb"))
  expect_type(bases$year, "integer")
  expect_type(bases$tamano_mb, "double")
  expect_identical(attr(bases, "registro"), registro_generado())
  expect_identical(rownames(bases), as.character(seq_len(nrow(bases))))
  expect_false(anyNA(bases[setdiff(names(bases), c("modulo", "tablas"))]))
  expect_identical(anyDuplicated(bases[, c("proyecto", "year", "modulo", "base")]), 0L)
  # Solo los censos de gobierno tienen modulo, y no listan sus tablas
  gobierno <- bases$proyecto %in% c("cngmd", "cndhf")
  expect_true(all(is.na(bases$modulo[!gobierno])) && !anyNA(bases$modulo[gobierno]))
  expect_true(all(is.na(bases$tablas[gobierno])) && !anyNA(bases$tablas[!gobierno]))
  expect_true(all(bases$tamano_mb > 0))
  expect_true(all(bases$cobertura %in% c("nacional y por entidad", "por entidad", "nacional")))
  expect_true(all(bases$formato %in% c("dbf", "csv", "xlsx")))
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
  # La muestra de 1990 es un extracto; desde 2000, el cuestionario ampliado
  expect_identical(fila(1990, "muestra")$descripcion, "Muestra (extracto del 10 % de las viviendas)")
  expect_identical(fila(2010, "muestra")$descripcion, "Muestra (cuestionario ampliado)")
  expect_identical(fila(2010, "entorno")$tablas, "")
  expect_identical(fila(2020, "entorno")$tablas, "manzanas, vialidades")
  expect_identical(fila(2020, "asistencia")$tablas, "alojamientos, usuarios, trabajadores")
  expect_identical(fila(2020, "iter")$formato, "csv")
  expect_identical(fila(2020, "muestra")$cobertura, "nacional y por entidad")
  # Tamano: el archivo nacional si existe; si no, la suma de las entidades
  expect_equal(fila(2010, "iter")$tamano_mb, 37.6)
  expect_equal(fila(2010, "muestra")$tamano_mb, 345.3, tolerance = 0.01)
})

test_that("bases_importinegi() describe las bases del Conteo de Poblacion y Vivienda", {
  conteo_bases <- bases_importinegi("conteo")
  expect_identical(unique(conteo_bases$proyecto), "conteo")
  resumen <- split(conteo_bases$base, conteo_bases$year)
  expect_identical(resumen[["1995"]], c("iter", "muestra"))
  expect_identical(resumen[["2005"]], c("iter", "muestra", "viviendas"))

  fila <- function(year, base) conteo_bases[conteo_bases$year == year & conteo_bases$base == base, ]
  expect_identical(fila(1995, "muestra")$descripcion, "Muestra (encuesta del Conteo, cuestionario ampliado)")
  expect_identical(fila(2005, "muestra")$descripcion, "Muestra (extracto del 10 % de las viviendas)")
  expect_identical(fila(2005, "viviendas")$descripcion, "Viviendas habitadas y deshabitadas por localidad")
  expect_identical(fila(1995, "muestra")$tablas, "personas, migrantes")
  expect_identical(fila(2005, "muestra")$tablas, "personas, viviendas, hogares")
  expect_identical(fila(2005, "muestra")$cobertura, "por entidad")
  expect_identical(fila(2005, "viviendas")$cobertura, "nacional y por entidad")
  expect_identical(unique(conteo_bases$formato), "dbf")
  expect_equal(fila(1995, "iter")$tamano_mb, 11.8)
  expect_equal(fila(2005, "muestra")$tamano_mb, 120.7, tolerance = 0.01)
})

test_that("bases_importinegi() describe las bases de la Encuesta Intercensal", {
  eic <- bases_importinegi("intercensal")
  expect_identical(unique(eic$proyecto), "intercensal")
  expect_identical(paste(eic$year, eic$base), c("2015 muestra", "2025 muestra", "2025 resultados"))
  fila <- function(year, base) eic[eic$year == year & eic$base == base, ]
  expect_identical(fila(2015, "muestra")$descripcion, "Muestra (microdatos de la encuesta)")
  expect_identical(fila(2015, "muestra")$tablas, "personas, viviendas")
  expect_identical(fila(2025, "muestra")$tablas, "personas, viviendas, migrantes")
  expect_identical(fila(2015, "muestra")$cobertura, "por entidad")
  expect_identical(fila(2025, "muestra")$cobertura, "nacional y por entidad")
  expect_identical(fila(2025, "resultados")$cobertura, "nacional")
  expect_identical(fila(2025, "resultados")$descripcion,
                   "Principales resultados por localidad de 50 000 y m\u00e1s habitantes")
  expect_identical(fila(2025, "resultados")$tablas, "")
  expect_identical(fila(2025, "resultados")$formato, "xlsx")
  expect_identical(fila(2015, "muestra")$formato, "csv")
  expect_identical(fila(2015, "muestra")$tamano_mb, 813.9)
  expect_equal(fila(2025, "muestra")$tamano_mb, 727)
  expect_equal(fila(2025, "resultados")$tamano_mb, 28.3)
})

test_that("bases_importinegi() coincide con lo que censo(), conteo() e intercensal() pueden descargar", {
  for (programa in c("censo", "conteo", "intercensal")) {
    tabla <- bases_importinegi(programa)
    curadas <- poblacion_bases()
    curadas <- curadas[curadas$programa == programa, ]
    expect_identical(paste(tabla$year, tabla$base), paste(curadas$year, curadas$base), info = programa)
    for (i in seq_len(nrow(tabla))) {
      etiqueta <- paste(programa, tabla$year[i], tabla$base[i])
      expect_identical(poblacion_validar_base(programa, tabla$year[i], tabla$base[i]), tabla$base[i], info = etiqueta)
      archivos <- poblacion_archivos(programa, tabla$year[i], tabla$base[i])
      cobertura <- if (all(archivos$clave == "00")) "nacional" else if ("00" %in% archivos$clave) "nacional y por entidad" else "por entidad"
      expect_identical(tabla$cobertura[i], cobertura, info = etiqueta)
    }
  }
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
  expect_identical(unique(bases_importinegi()$proyecto),
                   c("censo", "conteo", "intercensal", "cngmd", "cndhf", "enigh_tradicional"))
  expect_identical(bases_importinegi(c("censo", "conteo", "intercensal", "cngmd", "cndhf", "enigh_tradicional")),
                   bases_importinegi())
  de_2020 <- bases_importinegi(year = 2020)
  expect_identical(unique(de_2020$year), 2020L)
  # Seis bases del censo y los 18 temas del censo de derechos humanos federal
  expect_identical(unique(de_2020$proyecto), c("censo", "cndhf"))
  expect_identical(nrow(de_2020), 24L)
  expect_identical(bases_importinegi("censo", "2020"),
                   structure(de_2020[de_2020$proyecto == "censo", ], registro = registro_generado()))
  varios <- bases_importinegi(year = c(1990, 2000))
  expect_identical(unique(varios$year), c(1990L, 2000L))
  # Cuatro bases de los censos y la ENIGH de 2000
  expect_identical(nrow(varios), 5L)

  conteos <- bases_importinegi(year = c(1995, 2005))
  # Cinco bases de los conteos y la ENIGH de 2005
  expect_identical(unique(conteos$proyecto), c("conteo", "enigh_tradicional"))
  expect_identical(nrow(conteos), 6L)

  # En 2015 y 2025 hay Encuesta Intercensal y censo de gobiernos municipales; en
  # 2025, tambien censo de derechos humanos federal
  de_2015_y_2025 <- bases_importinegi(year = c(2015, 2025))
  expect_identical(unique(de_2015_y_2025$proyecto), c("intercensal", "cngmd", "cndhf"))
  intercensales <- bases_importinegi("intercensal", year = c(2015, 2025))
  expect_identical(intercensales, structure(de_2015_y_2025[de_2015_y_2025$proyecto == "intercensal", ],
                                            registro = registro_generado()))
  expect_identical(nrow(intercensales), 3L)

  # Un año sin bases
  expect_message(vacia <- bases_importinegi(year = 2007), "no tiene bases de datos para esa consulta")
  expect_identical(nrow(vacia), 0L)
  expect_named(vacia, names(bases_importinegi()))
})

test_that("bases_importinegi() valida sus argumentos", {
  expect_error(bases_importinegi("enoe"), "`proyecto = \"enoe\"` no es una opci\u00f3n v\u00e1lida. Proyectos disponibles: \"censo\", \"conteo\", \"intercensal\"")
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
  expect_false(2020L %in% bases$year[bases$proyecto == "censo"])
  expect_identical(sum(bases$proyecto %in% c("censo", "conteo", "intercensal")), 17L)
  expect_identical(attr(bases, "registro"), "2026-11-11")
  # Sin los archivos de un proyecto, no se listan sus temas
  sin_gobierno <- registro_archivos[registro_archivos$proyecto != "cngmd", ]
  withr::local_options(importinegi.registro = sin_gobierno)
  expect_false("cngmd" %in% bases_importinegi()$proyecto)
  expect_error(cngmd(2025, "agua", "x"), "`year = 2025` no est\u00e1 disponible")
})

test_that("los errores de censo() remiten a bases_importinegi()", {
  expect_error(censo(2000, "ageb"), "Consulta todas con bases_importinegi\\(\"censo\"\\)")
  expect_error(censo(2010, "enoe"), "Consulta todas con bases_importinegi\\(\"censo\"\\)")
})
