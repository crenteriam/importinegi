# Pruebas en vivo de cndhf(): descargan archivos reales del INEGI. Solo corren
# con IMPORTINEGI_EN_VIVO=true (ver .github/workflows/pruebas-en-vivo.yaml).
#
# La forma de los archivos se reviso el 22 de noviembre de 2026 en el indice de
# los zip y en sus diccionarios de datos, sin descargar las tablas. Estas
# pruebas leen los archivos completos: el tema mas pequeno de cada modulo y
# edicion (18 descargas) y los temas de los ejemplos de la ayuda. Ademas de
# comprobar el resultado, escriben en el registro de la ejecucion lo que no se
# pudo verificar sin descargar: los encabezados reales de las tablas, el
# formato de las claves geograficas y las siglas que aparecen en los datos.

# Columnas geograficas de una tabla: las claves que la funcion rellena y las
# demas columnas de entidad o de municipio.
columnas_geograficas <- function(tabla) {
  names(tabla)[grepl("^(ubic|enticve|entifed|municve|municip|entidad|entfede|cvegeo|cve_ent|cve_mun)", tolower(names(tabla)))]
}

# Valores distintos de una columna, como texto corto para el registro.
ejemplos <- function(valores, n = 6) {
  paste(utils::head(unique(as.character(valores)), n), collapse = " ")
}

# Columnas de texto que contienen alguna de las siglas que se conservan.
con_siglas_cndhf <- function(tabla, siglas = c("NSS", "ND", "NP")) {
  names(tabla)[vapply(tabla, function(x) is.character(x) && any(x %in% siglas), logical(1))]
}

# Comprueba el formato de las claves geograficas de una tabla: 5 digitos en
# la clave del municipio en una columna, 2 en la entidad y 3 en el municipio
# cuando vienen separadas. Los valores que no son numeros (siglas) se aceptan.
revisar_claves <- function(tabla, etiqueta) {
  columnas <- gobierno_columnas_clave(tabla, "cndhf")
  anchos <- c(municipio = 5, entidad = 2, mun = 3)
  for (tipo in names(columnas)) {
    for (columna in columnas[[tipo]]) {
      valores <- tabla[[columna]]
      expect_type(valores, "character")
      numeros <- !is.na(valores) & grepl("^[0-9]+$", valores)
      malos <- unique(valores[numeros & nchar(valores) != anchos[[tipo]]])
      expect_identical(malos, character(), info = paste(etiqueta, columna, ejemplos(malos)))
      cat("[en vivo] ", etiqueta, " clave ", columna, ": ", ejemplos(valores), "; no numericos: ",
          ejemplos(valores[!is.na(valores) & !numeros]), "\n", sep = "")
    }
  }
}

test_that("EN VIVO: el registro tiene los temas de las nueve ediciones del CNDHF", {
  en_vivo()
  temas <- gobierno_temas("cndhf")
  expect_identical(sort(unique(temas$year)), 2017:2025)
  expect_identical(as.integer(table(temas$year)), c(20L, 21L, 20L, 18L, 18L, 18L, 17L, 21L, 22L))
  expect_false(anyNA(temas))
  expect_identical(anyDuplicated(temas[c("year", "modulo", "base")]), 0L)
  expect_identical(anyDuplicated(paste(temas$year, normalizar_texto(temas$tema))), 0L)
  registro <- registro_inegi()
  expect_identical(sum(registro$proyecto == "cndhf"), 175L)
})

test_that("EN VIVO: cndhf() lee el tema mas pequeno de cada modulo y edicion", {
  en_vivo()
  temas <- gobierno_temas("cndhf")
  grupos <- split(temas, paste(temas$year, match(temas$modulo, gobierno_modulos("cndhf")$modulo)))
  expect_identical(length(grupos), 18L)
  for (grupo in grupos) {
    tema <- grupo[which.min(grupo$bytes), ]
    etiqueta <- paste(tema$year, tema$modulo, tema$base)
    tablas <- tryCatch(suppressMessages(cndhf(tema$year, tema$modulo, tema$base)), error = function(e) e)
    expect_false(inherits(tablas, "error"), info = paste(etiqueta, if (inherits(tablas, "error")) conditionMessage(tablas)))
    if (inherits(tablas, "error")) {
      cat("[en vivo] ", etiqueta, ": ERROR ", conditionMessage(tablas), "\n", sep = "")
      next
    }
    contenido <- gobierno_contenido(ruta_cache(tema$url), tema$formato, "cndhf")
    siglas <- unlist(lapply(tablas, con_siglas_cndhf))
    cat("[en vivo] ", etiqueta, " (", tema$tamano, "): ", length(tablas), " tablas (",
        paste(utils::head(names(tablas), 4), collapse = ", "), if (length(tablas) > 4) ", ...", "); filas de ",
        min(vapply(tablas, nrow, integer(1))), " a ", max(vapply(tablas, nrow, integer(1))),
        "; columnas con NSS, ND o NP: ", length(siglas),
        "; archivos: ", paste(names(table(contenido$tipo)), table(contenido$tipo), sep = "=", collapse = ", "),
        if (any(grepl("\\", contenido$original, fixed = TRUE, useBytes = TRUE))) "; rutas con \\", "\n", sep = "")
    # Encabezados reales de la primera tabla: no se pudieron ver sin descargar
    cat("[en vivo] ", etiqueta, " columnas de ", names(tablas)[1], ": ",
        paste(utils::head(names(tablas[[1]]), 10), collapse = ", "), "\n", sep = "")
    otros <- contenido$ruta[contenido$tipo == "otro" & grepl("conjunto_de_datos/", contenido$ruta, ignore.case = TRUE)]
    if (length(otros) > 0) {
      cat("[en vivo] ", etiqueta, " archivos de la carpeta de datos que no son tablas: ", paste(basename(otros), collapse = ", "), "\n", sep = "")
    }

    expect_gt(length(tablas), 0)
    expect_false(any(grepl("_cndhf[0-9]+", names(tablas))), info = etiqueta)
    expect_false(any(grepl("bitacora", names(tablas))), info = etiqueta)
    expect_identical(anyDuplicated(names(tablas)), 0L, info = etiqueta)
    for (nombre in names(tablas)) {
      geograficas <- columnas_geograficas(tablas[[nombre]])
      if (length(geograficas) > 0) {
        cat("[en vivo] ", etiqueta, " ", nombre, " columnas geograficas: ",
            paste0(geograficas, "=", vapply(tablas[[nombre]][geograficas], function(x) class(x)[1], character(1)), collapse = ", "), "\n", sep = "")
        revisar_claves(tablas[[nombre]], paste(etiqueta, nombre))
      }
    }
    texto <- as.character(unlist(lapply(tablas, function(tabla) unlist(tabla[vapply(tabla, is.character, logical(1))], use.names = FALSE))))
    expect_true(all(validUTF8(texto)), info = paste(etiqueta, "texto que no es UTF-8"))
    expect_false(any(texto %in% "NA"), info = paste(etiqueta, "queda la sigla NA"))

    indice <- attr(tablas, "indice")
    expect_identical(indice$tabla, names(tablas), info = etiqueta)
    if (tema$year >= 2020) {
      expect_false(all(is.na(indice$contenido)), info = paste(etiqueta, "indice sin contenido"))
      if (anyNA(indice$contenido)) {
        cat("[en vivo] ", etiqueta, " tablas que el indice no describe: ",
            paste(indice$tabla[is.na(indice$contenido)], collapse = ", "), "\n", sep = "")
      }
      diccionario <- tryCatch(suppressMessages(cndhf(tema$year, tema$modulo, tema$base, tabla = "diccionario")),
                              error = function(e) e)
      expect_false(inherits(diccionario, "error"),
                   info = paste(etiqueta, "diccionario:", if (inherits(diccionario, "error")) substr(conditionMessage(diccionario), 1, 300)))
      if (!inherits(diccionario, "error")) {
        expect_identical(setdiff(unique(diccionario$tabla), names(tablas)), character(), info = paste(etiqueta, "diccionario sin tabla"))
        expect_identical(setdiff(names(tablas), unique(diccionario$tabla)), character(), info = paste(etiqueta, "tabla sin diccionario"))
        cat("[en vivo] ", etiqueta, " diccionario: ", nrow(diccionario), " variables de ",
            length(unique(diccionario$tabla)), " tablas; columnas: ", paste(names(diccionario), collapse = ", "), "\n", sep = "")
      }
    } else {
      expect_true(all(is.na(indice$contenido)), info = paste(etiqueta, "indice con contenido antes de 2020"))
      expect_error(suppressWarnings(suppressMessages(cndhf(tema$year, tema$modulo, tema$base, tabla = "diccionario"))),
                   "no es una tabla", info = etiqueta)
    }
  }
})

test_that("EN VIVO: los temas de los ejemplos tienen la forma documentada", {
  en_vivo()
  # Consejo Consultivo, 2025: una lista de personas consejeras
  consejo <- suppressMessages(cndhf(year = 2025, modulo = "estructura", tema = "consejo consultivo"))
  expect_true(all(c("m1s1p2", "m1s1p4") %in% names(consejo)))
  describir_tabla("2025 consejo consultivo m1s1p2", consejo$m1s1p2)
  expect_true(all(c("numeral", "nombre1") %in% tolower(names(consejo$m1s1p2))))
  indice <- attr(consejo, "indice")
  expect_match(indice$contenido[indice$tabla == "m1s1p2"], "^Personas consejeras")

  # Expedientes calificados, 2025: 29 tablas; hechos por tipo de hecho
  hechos <- suppressMessages(cndhf(2025, "proteccion", "expedientes calificados", tabla = "m2s5p24"))
  describir_tabla("2025 expedientes calificados m2s5p24", hechos)
  expect_true("hechovio_c" %in% tolower(names(hechos)))
  calificados <- suppressMessages(cndhf(2025, "proteccion", "expedientes calificados", tabla = "indice"))
  expect_identical(nrow(calificados), 29L)
  catalogos <- suppressMessages(cndhf(2025, "proteccion", "expedientes calificados", tabla = "catalogos"))
  expect_true("hechovio_c" %in% names(catalogos))
  codificadas <- intersect(names(hechos), names(catalogos))
  cat("[en vivo] 2025 expedientes calificados: ", length(catalogos), " catalogos (", paste(utils::head(names(catalogos), 8), collapse = ", "),
      ", ...); variables de m2s5p24 con catalogo: ", paste(codificadas, collapse = ", "), "\n", sep = "")
  for (variable in codificadas) {
    expect_identical(class(hechos[[variable]]), class(catalogos[[variable]][[1]]), info = variable)
  }

  # El tema tambien se elige con el nombre de su archivo (2017)
  participacion <- suppressMessages(cndhf(2017, "estructura", "participacion_ciudada"))
  expect_named(participacion, "partciud")
  describir_tabla("2017 participacion_ciudada partciud", participacion$partciud)
})

test_that("EN VIVO: las claves geograficas tienen el formato documentado", {
  en_vivo()
  # 2025: entidad y municipio en columnas separadas (2 y 3 digitos) y entidad con catalogo
  seleccionados <- suppressMessages(cndhf(2025, "proteccion", "hechos violatorios"))
  municipal <- seleccionados$m2s9p2cp4
  expect_false(is.null(municipal))
  describir_tabla("2025 hechos violatorios m2s9p2cp4", municipal)
  expect_true(all(c("enticve1", "municve1") %in% names(municipal)))
  revisar_claves(municipal, "2025 hechos violatorios m2s9p2cp4")
  expect_true(any(grepl("^[0-9]{2}$", municipal$enticve1)))
  expect_true(any(grepl("^[0-9]{3}$", municipal$municve1)))
  con_entidad <- names(seleccionados)[vapply(seleccionados, function(tabla) any(grepl("^entfede_", names(tabla))), logical(1))]
  cat("[en vivo] 2025 hechos violatorios, tablas con entfede_*: ", paste(con_entidad, collapse = ", "), "\n", sep = "")

  # Ejemplo de la vineta: al unir las dos columnas se obtiene la clave de 5
  # digitos del municipio en todas las filas de la tabla
  una <- suppressMessages(cndhf(2025, modulo = "proteccion", tema = "hechos violatorios", tabla = "m2s9p2cp4"))
  expect_identical(dim(una), dim(municipal))
  cvegeo <- paste0(una$enticve1, una$municve1)
  validas <- grepl("^[0-9]{5}$", cvegeo)
  cat("[en vivo] 2025 hechos violatorios m2s9p2cp4: ", sum(validas), " de ", length(cvegeo),
      " filas con clave de 5 digitos; ", length(unique(cvegeo[validas])), " municipios distintos; otras: ",
      ejemplos(cvegeo[!validas]), "\n", sep = "")
  expect_true(all(validas))

  # 2020 a 2022: clave del municipio en una columna (5 digitos). La vineta
  # dice que enticve1 aparece desde 2022 y municve1, desde 2023
  for (year in 2020:2022) {
    temas <- gobierno_temas("cndhf", year)
    halladas <- 0L
    n_entidad <- 0L
    n_mun <- 0L
    for (i in seq_len(nrow(temas))) {
      tablas <- tryCatch(suppressMessages(cndhf(year, temas$modulo[i], temas$base[i])), error = function(e) NULL)
      for (nombre in names(tablas)) {
        columnas <- gobierno_columnas_clave(tablas[[nombre]], "cndhf")
        n_entidad <- n_entidad + (length(columnas$entidad) > 0)
        n_mun <- n_mun + (length(columnas$mun) > 0)
        if (length(columnas$municipio) > 0) {
          halladas <- halladas + 1L
          revisar_claves(tablas[[nombre]], paste(year, temas$base[i], nombre))
        }
      }
    }
    cat("[en vivo] ", year, ": tablas con la clave del municipio en una columna = ", halladas,
        "; con enticve* = ", n_entidad, "; con municve* = ", n_mun, "\n", sep = "")
    expect_gt(halladas, 0L)
    expect_identical(n_mun, 0L)
    expect_identical(n_entidad > 0L, year == 2022)
  }

  # 2023 y 2024: entidad y municipio en columnas separadas, como en 2025
  for (year in 2023:2024) {
    separadas <- 0L
    en_una <- 0L
    for (tema in c("capdifu_fortacul_dh", "exploespec_hechoviola_dh_selec")) {
      tablas <- suppressMessages(cndhf(year, "proteccion", tema))
      for (nombre in names(tablas)) {
        columnas <- gobierno_columnas_clave(tablas[[nombre]], "cndhf")
        en_una <- en_una + (length(columnas$municipio) > 0)
        if (length(columnas$entidad) > 0 && length(columnas$mun) > 0) {
          separadas <- separadas + 1L
          revisar_claves(tablas[[nombre]], paste(year, tema, nombre))
        }
      }
    }
    cat("[en vivo] ", year, ": tablas con enticve* y municve* = ", separadas,
        "; con la clave del municipio en una columna = ", en_una, "\n", sep = "")
    expect_gt(separadas, 0L)
    expect_identical(en_una, 0L)
  }
})
