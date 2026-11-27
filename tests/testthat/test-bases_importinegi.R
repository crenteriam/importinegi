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
  censo_bases <- bases_importinegi("cpv_censo")
  expect_identical(unique(censo_bases$proyecto), "cpv_censo")
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
  conteo_bases <- bases_importinegi("cpv_conteo")
  expect_identical(unique(conteo_bases$proyecto), "cpv_conteo")
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
  eic <- bases_importinegi("cpv_intercensal")
  expect_identical(unique(eic$proyecto), "cpv_intercensal")
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

test_that("bases_importinegi() describe los trimestres de la ENOE", {
  ocupacion <- bases_importinegi("enoe")
  expect_identical(unique(ocupacion$proyecto), "enoe")
  expect_identical(nrow(ocupacion), 86L)
  expect_identical(range(ocupacion$year), c(2005L, 2026L))
  # `base` es el trimestre como se escribe en enoe(); 2020-T2 no existe
  expect_identical(ocupacion$base[ocupacion$year == 2019], c("trim1", "trim2", "trim3", "trim4"))
  expect_identical(ocupacion$base[ocupacion$year == 2020], c("trim1", "trim3", "trim4"))
  expect_identical(ocupacion$base[ocupacion$year == 2026], c("trim1", "trim2", "trim3"))
  expect_true(all(is.na(ocupacion$modulo)))
  expect_identical(unique(ocupacion$tablas), "coe1, coe2, hog, sdem, viv")
  expect_identical(unique(ocupacion$cobertura), "nacional")
  expect_identical(unique(ocupacion$formato), "dbf")
  fila <- function(year, base) ocupacion[ocupacion$year == year & ocupacion$base == base, ]
  expect_identical(fila(2005, "trim1")$descripcion, "ENOE, cuestionario ampliado")
  expect_identical(fila(2019, "trim4")$descripcion, "ENOE, cuestionario b\u00e1sico")
  expect_identical(fila(2020, "trim3")$descripcion, "ENOE Nueva Edici\u00f3n (ENOE_N), cuestionario b\u00e1sico")
  expect_identical(fila(2022, "trim1")$descripcion, "ENOE Nueva Edici\u00f3n (ENOE_N), cuestionario ampliado")
  expect_identical(fila(2023, "trim1")$descripcion, "ENOE, cuestionario ampliado")
  cuantos <- function(descripcion) sum(ocupacion$descripcion == descripcion)
  expect_identical(cuantos("ENOE, cuestionario ampliado"), 24L)
  expect_identical(cuantos("ENOE, cuestionario b\u00e1sico"), 52L)
  expect_identical(cuantos("ENOE Nueva Edici\u00f3n (ENOE_N), cuestionario ampliado"), 2L)
  expect_identical(cuantos("ENOE Nueva Edici\u00f3n (ENOE_N), cuestionario b\u00e1sico"), 8L)
  expect_equal(fila(2005, "trim1")$tamano_mb, 27.2)
  expect_equal(fila(2020, "trim3")$tamano_mb, 18.3)
  expect_equal(fila(2024, "trim1")$tamano_mb, 34)
  expect_equal(fila(2026, "trim3")$tamano_mb, 32.7)
  expect_true(all(ocupacion$tamano_mb > 18 & ocupacion$tamano_mb < 40))
  # Coincide con lo que enoe() puede descargar
  for (i in seq_len(nrow(ocupacion))) {
    elegido <- enoe_elegir(ocupacion$year[i], ocupacion$base[i])
    expect_true(elegido$registrado, info = paste(ocupacion$year[i], ocupacion$base[i]))
  }
  expect_identical(bases_importinegi("enoe", 2024)$base, c("trim1", "trim2", "trim3", "trim4"))
})

test_that("bases_importinegi() coincide con lo que cpv_censo(), cpv_conteo() y cpv_intercensal() pueden descargar", {
  for (programa in c("cpv_censo", "cpv_conteo", "cpv_intercensal")) {
    tabla <- bases_importinegi(programa)
    # El proyecto se llama como la funcion; dentro del motor, sin el prefijo
    proyecto <- programa
    programa <- sub("^cpv_", "", proyecto)
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

test_that("bases_importinegi() coincide con lo que cpv_censo() puede descargar", {
  censo_bases <- bases_importinegi("cpv_censo")
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
  expect_identical(bases_importinegi("CPV_CENSO "), bases_importinegi("cpv_censo"))
  expect_identical(unique(bases_importinegi()$proyecto),
                   c("cpv_censo", "cpv_conteo", "cpv_intercensal", "cngmd", "cndhf", "enigh_tradicional", "enigh_nc", "enoe", "etoe"))
  expect_identical(
    bases_importinegi(c("cpv_censo", "cpv_conteo", "cpv_intercensal", "cngmd", "cndhf", "enigh_tradicional", "enigh_nc", "enoe", "etoe")),
    bases_importinegi()
  )
  de_2020 <- bases_importinegi(year = 2020)
  expect_identical(unique(de_2020$year), 2020L)
  # Seis bases del censo, los 18 temas del censo de derechos humanos federal,
  # tres trimestres de la ENOE y los tres meses de la ETOE
  expect_identical(unique(de_2020$proyecto), c("cpv_censo", "cndhf", "enoe", "etoe"))
  expect_identical(nrow(de_2020), 30L)
  expect_identical(bases_importinegi("cpv_censo", "2020"),
                   structure(de_2020[de_2020$proyecto == "cpv_censo", ], registro = registro_generado()))
  varios <- bases_importinegi(year = c(1990, 2000))
  expect_identical(unique(varios$year), c(1990L, 2000L))
  # Cuatro bases de los censos y la ENIGH de 2000
  expect_identical(nrow(varios), 5L)

  conteos <- bases_importinegi(year = c(1995, 2005))
  # Cinco bases de los conteos, la ENIGH de 2005 y los cuatro primeros trimestres de la ENOE
  expect_identical(conteos$proyecto, rep(c("cpv_conteo", "enigh_tradicional", "enoe"), c(5L, 1L, 4L)))

  # En 2015 y 2025 hay Encuesta Intercensal, censo de gobiernos municipales y
  # ENOE; en 2025, tambien censo de derechos humanos federal
  de_2015_y_2025 <- bases_importinegi(year = c(2015, 2025))
  expect_identical(unique(de_2015_y_2025$proyecto), c("cpv_intercensal", "cngmd", "cndhf", "enoe"))
  intercensales <- bases_importinegi("cpv_intercensal", year = c(2015, 2025))
  expect_identical(intercensales, structure(de_2015_y_2025[de_2015_y_2025$proyecto == "cpv_intercensal", ],
                                            registro = registro_generado()))
  expect_identical(nrow(intercensales), 3L)

  # Un año sin bases
  expect_message(vacia <- bases_importinegi(year = 1980), "no tiene bases de datos para esa consulta")
  expect_identical(nrow(vacia), 0L)
  expect_named(vacia, names(bases_importinegi()))
})

test_that("bases_importinegi() valida sus argumentos", {
  expect_error(bases_importinegi("enut"), "`proyecto = \"enut\"` no es una opci\u00f3n v\u00e1lida. Proyectos disponibles: \"cpv_censo\", \"cpv_conteo\", \"cpv_intercensal\"")
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
  expect_false(2020L %in% bases$year[bases$proyecto == "cpv_censo"])
  expect_identical(sum(bases$proyecto %in% c("cpv_censo", "cpv_conteo", "cpv_intercensal")), 17L)
  expect_identical(attr(bases, "registro"), "2026-11-11")
  # Sin los archivos de un proyecto, no se listan sus temas
  sin_gobierno <- registro_archivos[registro_archivos$proyecto != "cngmd", ]
  withr::local_options(importinegi.registro = sin_gobierno)
  expect_false("cngmd" %in% bases_importinegi()$proyecto)
  expect_error(cngmd(2025, "agua", "x"), "`year = 2025` no est\u00e1 disponible")
  sin_ocupacion <- registro_archivos[registro_archivos$proyecto != "enoe", ]
  withr::local_options(importinegi.registro = sin_ocupacion)
  expect_false("enoe" %in% bases_importinegi()$proyecto)
  expect_message(vacia <- bases_importinegi("enoe"), "no tiene bases de datos para esa consulta")
  expect_identical(nrow(vacia), 0L)
})

test_that("los errores de cpv_censo() remiten a bases_importinegi()", {
  expect_error(cpv_censo(2000, "ageb"), "Consulta todas con bases_importinegi\\(\"cpv_censo\"\\)")
  expect_error(cpv_censo(2010, "enoe"), "Consulta todas con bases_importinegi\\(\"cpv_censo\"\\)")
})
