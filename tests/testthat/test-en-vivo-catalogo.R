# Pruebas en vivo de catalogo_inegi() contra el sitio real del INEGI.
# Solo corren con IMPORTINEGI_EN_VIVO=true (tarea pruebas-en-vivo.yaml).
# Las lineas "[en vivo]" quedan en el registro de la tarea como diagnostico.

test_that("EN VIVO: catalogo_inegi() trae el catalogo completo", {
  omitir_si_no_en_vivo()
  memoria_borrar("catalogo_rnm")
  catalogo <- suppressMessages(catalogo_inegi())

  cat("\n[en vivo] catalogo_inegi(): ", nrow(catalogo), " proyectos\n", sep = "")
  cat("[en vivo] columnas: ",
      paste(names(catalogo), vapply(catalogo, function(x) class(x)[1], ""), sep = "=", collapse = ", "),
      "\n", sep = "")
  cat("[en vivo] primera fila: ",
      paste(vapply(catalogo[1, ], function(x) substr(as.character(x), 1, 60), ""), collapse = " | "),
      "\n", sep = "")
  cat("[en vivo] id de ", min(catalogo$id), " a ", max(catalogo$id),
      "; periodo de ", min(catalogo$year_start, na.rm = TRUE), " a ",
      max(catalogo$year_end, na.rm = TRUE), "\n", sep = "")

  expect_s3_class(catalogo, "data.frame")
  # La version 1.2.1 solo devolvia 500 proyectos. El 11 de noviembre de 2026 el
  # catalogo tenia 825 (comprobado en el navegador) y solo crece.
  expect_gte(nrow(catalogo), 800)
  expect_true(all(c("id", "idno", "title", "year_start", "year_end", "url") %in% names(catalogo)))
  expect_type(catalogo$id, "integer")
  expect_false(anyDuplicated(catalogo$id) > 0)
  expect_false(anyNA(catalogo$title))
  # Las fechas del catalogo se convierten a fecha (si el INEGI cambia su
  # formato, quedan como texto y esta prueba lo detecta)
  cat("[en vivo] created: ", class(catalogo$created)[1], " de ", format(min(catalogo$created)),
      " a ", format(max(catalogo$created)), "; changed: ", class(catalogo$changed)[1],
      " hasta ", format(max(catalogo$changed)), "\n", sep = "")
  expect_s3_class(catalogo$created, "Date")
  expect_s3_class(catalogo$changed, "Date")
  expect_false(anyNA(catalogo$created))
})

test_that("EN VIVO: buscar y year funcionan con el catalogo real", {
  omitir_si_no_en_vivo()
  empleo <- suppressMessages(catalogo_inegi(buscar = "ocupacion y empleo"))
  cat("[en vivo] buscar = \"ocupacion y empleo\": ", nrow(empleo), " proyectos; ejemplo: ",
      empleo$title[1], " (", empleo$idno[1], ")\n", sep = "")
  siglas <- suppressMessages(catalogo_inegi(buscar = "enoe"))
  cat("[en vivo] buscar = \"enoe\": ", nrow(siglas), " proyectos\n", sep = "")
  reciente <- suppressMessages(catalogo_inegi(year = 2020))
  cat("[en vivo] year = 2020: ", nrow(reciente), " proyectos\n", sep = "")

  # Valores del 11 de noviembre de 2026: 43, 43 y 163 proyectos. Los umbrales
  # dejan margen por si el INEGI retira alguno.
  expect_gte(nrow(empleo), 40)
  expect_gte(nrow(siglas), 40)
  expect_gte(nrow(reciente), 150)
  expect_true(all(reciente$year_start <= 2020))
})

test_that("EN VIVO: los ejemplos de la documentacion siguen vigentes", {
  omitir_si_no_en_vivo()
  catalogo <- suppressMessages(catalogo_inegi())

  # Proyectos que se citan por su numero en la ayuda (489) y en la vineta
  # (443), con las palabras de su nombre que la documentacion menciona.
  ejemplos <- list(
    list(id = 489L, nombre = "empresas constructoras 2019"),
    list(id = 443L, nombre = "censo nacional de derechos humanos estatal 2018")
  )
  for (ejemplo in ejemplos) {
    fila <- catalogo[catalogo$id == ejemplo$id, ]
    cat("[en vivo] id = ", ejemplo$id, ": ",
        if (nrow(fila) == 1) paste0(fila$title, " (", fila$idno, ")") else "NO ESTA EN EL CATALOGO",
        "\n", sep = "")
    expect_identical(nrow(fila), 1L)
    expect_true(grepl(ejemplo$nombre, normalizar_texto(fila$title), fixed = TRUE))
  }

  # Busquedas de los ejemplos de la ayuda, la vineta y el README
  busquedas <- list(
    list(buscar = "enigh", year = NULL),
    list(buscar = "ciberacoso", year = NULL),
    list(buscar = "ocupacion empleo", year = NULL),
    list(buscar = "ocupacion empleo", year = 2020),
    list(buscar = "ocupacion y empleo", year = 2020),
    list(buscar = "enoe", year = 2020)
  )
  for (busqueda in busquedas) {
    resultado <- suppressMessages(catalogo_inegi(buscar = busqueda$buscar, year = busqueda$year))
    cat("[en vivo] buscar = \"", busqueda$buscar, "\"",
        if (!is.null(busqueda$year)) paste0(", year = ", busqueda$year),
        ": ", nrow(resultado), " proyectos\n", sep = "")
    expect_gt(nrow(resultado), 0)
  }
})

test_that("EN VIVO (informativo): cuantos proyectos entregaba la consulta anterior", {
  omitir_si_no_en_vivo()
  anterior <- tryCatch(
    nrow(utils::read.csv(text = inegi_texto("/rnm/index.php/catalog/export/csv?ps=500"),
                         check.names = FALSE)),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  cat("[en vivo] exportacion con ps=500 (version 1.2.1): ", anterior, " proyectos\n", sep = "")
  succeed()
})

test_that("EN VIVO (informativo): servicio JSON del catalogo", {
  omitir_si_no_en_vivo()
  respuesta <- tryCatch(
    substr(inegi_texto("/rnm/index.php/api/catalog/search?ps=1"), 1, 500),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  cat("[en vivo] api/catalog/search?ps=1: ", respuesta, "\n", sep = "")
  succeed()
})
