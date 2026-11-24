# Pruebas en vivo de cpv_intercensal(): descargan archivos reales del INEGI. Solo
# corren con IMPORTINEGI_EN_VIVO=true (ver .github/workflows/pruebas-en-vivo.yaml).
#
# De la muestra se descarga la entidad con el archivo mas pequeño (Baja
# California Sur: 3.2 MB en 2015 y 2.8 MB en 2025); de los resultados de 2025,
# el unico archivo (28 MB). Ademas de comprobar el resultado, cada prueba
# escribe en el registro de la ejecucion lo que encontro (contenido de cada zip,
# columnas, hojas del XLSX, valores de ESTIMADOR), para confirmar lo que el
# paquete da por supuesto sobre la forma de los archivos.
#
# Las utilidades comunes (en_vivo(), entidad_menor(), describir_zip(), ...) estan
# en helper-en-vivo.R.

# Cifras publicadas por el INEGI para Baja California Sur, con las que se compara
# la suma del factor de expansion. 2025: 955 465 habitantes de viviendas
# particulares y 311 591 viviendas particulares habitadas (comunicado de prensa
# 57/26). 2015: 712 029 habitantes. En la prueba del 22 de noviembre de 2026, la
# suma del factor reprodujo las tres cifras sin diferencia.
publicadas_bcs <- list(
  "2015" = c(personas = 712029, tolerancia = 0.01),
  "2025" = c(personas = 955465, viviendas = 311591, tolerancia = 0.005)
)

columna_factor <- function(datos) {
  factores <- grep("^FACTOR$|^FAC", names(datos), value = TRUE, ignore.case = TRUE)
  if (length(factores) == 0) NA_character_ else factores[1]
}

test_that("EN VIVO: cpv_intercensal() descarga cada tabla de la muestra", {
  en_vivo()
  for (year in poblacion_programas()$intercensal$years) {
    clave <- entidad_menor("intercensal", year, "muestra")
    describir_zip("intercensal", year, "muestra", clave)
    tablas <- list()
    for (tabla in names(poblacion_tablas("intercensal", year, "muestra"))) {
      muestra <- suppressMessages(cpv_intercensal(year, estado = clave, tabla = tabla))
      etiqueta <- paste(year, "muestra", clave, tabla)
      describir_tabla(etiqueta, muestra)
      revisar_texto(etiqueta, muestra)
      expect_gt(nrow(muestra), 0)
      entidad <- columna_clave(muestra, "ent")
      expect_false(is.na(entidad), info = etiqueta)
      if (!is.na(entidad)) {
        expect_identical(unique(muestra[[entidad]]), clave, info = etiqueta)
      }
      factor <- columna_factor(muestra)
      expect_false(is.na(factor), info = paste(etiqueta, ": no hay factor de expansion"))
      if (!is.na(factor)) {
        expect_true(is.numeric(muestra[[factor]]), info = paste(etiqueta, factor))
        cat("[en vivo] ", etiqueta, " factor de expansion ", factor, ": suma ",
            format(sum(muestra[[factor]], na.rm = TRUE), big.mark = ","), "\n", sep = "")
      }
      if ("EDAD" %in% names(muestra)) {
        expect_true(is.numeric(muestra$EDAD), info = paste(etiqueta, "EDAD"))
      }
      if ("COBERTURA" %in% names(muestra)) {
        cat("[en vivo] ", etiqueta, " COBERTURA: ", resumir_niveles(muestra$COBERTURA), "\n", sep = "")
      }
      texto <- names(muestra)[vapply(muestra, is.character, logical(1))]
      cat("[en vivo] ", etiqueta, " columnas de texto: ", paste(texto, collapse = ", "), "\n", sep = "")
      cat("[en vivo] ", etiqueta, " primera fila: ",
          paste(names(muestra), vapply(muestra[1, , drop = FALSE], function(x) as.character(x[1]), character(1)),
                sep = "=", collapse = ", "), "\n", sep = "")
      tablas[[tabla]] <- muestra
    }
    # Las personas pertenecen a viviendas de la tabla de viviendas
    if (all(c("personas", "viviendas") %in% names(tablas)) &&
        "ID_VIV" %in% names(tablas$personas) && "ID_VIV" %in% names(tablas$viviendas)) {
      expect_true(all(tablas$personas$ID_VIV %in% tablas$viviendas$ID_VIV), info = paste(year, "ID_VIV"))
    }
    # La suma del factor reproduce las cifras publicadas por el INEGI
    publicadas <- publicadas_bcs[[as.character(year)]]
    if (clave == "03" && !is.null(publicadas)) {
      for (tabla in intersect(c("personas", "viviendas"), names(publicadas))) {
        factor <- columna_factor(tablas[[tabla]])
        if (is.na(factor) || !is.numeric(tablas[[tabla]][[factor]])) {
          next
        }
        suma <- sum(tablas[[tabla]][[factor]], na.rm = TRUE)
        diferencia <- (suma - publicadas[[tabla]]) / publicadas[[tabla]]
        cat("[en vivo] ", year, " muestra 03 ", tabla, ": suma del factor ", format(suma, big.mark = ","),
            "; cifra publicada ", format(publicadas[[tabla]], big.mark = ","), " (diferencia ",
            round(100 * diferencia, 3), " %)\n", sep = "")
        expect_lt(abs(diferencia), publicadas[["tolerancia"]])
      }
    }
  }
})

test_that("EN VIVO: cpv_intercensal() descarga Aguascalientes por omision en la muestra", {
  en_vivo()
  mensajes <- testthat::capture_messages(muestra <- cpv_intercensal(2025, tabla = "migrantes"))
  expect_match(mensajes[1], "No indicaste `estado`: se descarga la muestra de Aguascalientes")
  expect_identical(unique(muestra[[columna_clave(muestra, "ent")]]), "01")
})

test_that("EN VIVO: cpv_intercensal() lee los principales resultados por localidad de 2025", {
  en_vivo()
  # Sin readxl esta prueba falla en lugar de saltarse: una ejecucion en vivo no
  # debe terminar en verde sin haber leido el XLSX (paso en la primera del PR #21)
  expect_true(requireNamespace("readxl", quietly = TRUE),
              info = "Las pruebas en vivo de la base resultados necesitan el paquete readxl")
  testthat::skip_if_not_installed("readxl")
  archivos <- poblacion_archivos("intercensal", 2025L, "resultados")
  xlsx <- suppressMessages(inegi_descargar(archivos$url))
  hojas <- readxl::excel_sheets(xlsx)
  cat("[en vivo] 2025 resultados: ", basename(archivos$url), " (", archivos$tamano, "), hojas: ",
      paste(hojas, collapse = " | "), "\n", sep = "")
  for (hoja in hojas) {
    vista <- readxl::read_excel(xlsx, sheet = hoja, col_names = FALSE, col_types = "text", n_max = 6,
                                .name_repair = "minimal")
    filas <- apply(as.matrix(vista), 1, function(fila) paste(utils::head(fila[!is.na(fila)], 8), collapse = " ; "))
    cat("[en vivo] 2025 resultados, hoja ", hoja, ", primeras filas: ", paste(filas, collapse = " || "), "\n", sep = "")
  }

  todos <- suppressMessages(cpv_intercensal(2025, "resultados", nivel = "todos"))
  describir_tabla("2025 resultados nivel = todos", todos)
  revisar_texto("2025 resultados", todos)
  niveles <- poblacion_clasificar_nivel(todos, "resultados")
  expect_false(is.null(niveles))
  cat("[en vivo] 2025 resultados niveles (filas): ", resumir_niveles(niveles), "\n", sep = "")
  expect_true("ESTIMADOR" %in% names(todos))
  estimadores <- unique(todos$ESTIMADOR)
  cat("[en vivo] 2025 resultados ESTIMADOR: ", paste(estimadores, collapse = " | "), "\n", sep = "")
  expect_identical(estimadores, c("Valor", "Error estándar", "Límite inferior de confianza",
                                  "Límite superior de confianza", "Coeficiente de variación"))
  # Cada dominio ocupa cinco filas
  dominios <- table(paste(todos$CVE_ENT, todos$CVE_MUN, todos$CVE_LOC))
  expect_true(all(dominios == 5))
  expect_identical(sum(niveles == "nacional"), 5L)
  expect_identical(sum(niveles == "entidad"), 32L * 5L)
  expect_gt(sum(niveles == "municipio") / 5, 2400)
  localidades <- sum(niveles == "localidad") / 5
  expect_true(localidades >= 230 && localidades <= 240, info = paste("localidades:", localidades))
  expect_true(sum(niveles == "agregado") / 5 >= 30)
  expect_identical(nchar(unique(todos$CVEGEO)), rep(9L, length(unique(todos$CVEGEO))))

  # Los 341 indicadores son numeros. Si alguno queda como texto, el INEGI usa
  # una marca de dato faltante distinta de "MI" (muestra insuficiente) y "NA"
  # (no aplica), que son las que describe el descriptor de la base
  claves <- c("CVEGEO", "CVE_ENT", "NOM_ENT", "CVE_MUN", "NOM_MUN", "CVE_LOC", "NOM_LOC", "ESTIMADOR")
  expect_identical(names(todos)[seq_along(claves)], claves)
  indicadores <- setdiff(names(todos), claves)
  expect_length(indicadores, 341)
  texto <- indicadores[vapply(todos[indicadores], is.character, logical(1))]
  otras <- unique(unlist(lapply(todos[texto], function(x) {
    x <- trimws(x[!is.na(x)])
    x[!grepl("^-?[0-9]+(\\.[0-9]+)?$", x)]
  })))
  cat("[en vivo] 2025 resultados indicadores que quedaron como texto: ", length(texto), " (",
      paste(utils::head(texto, 10), collapse = ", "), "); valores no numericos: ",
      paste(utils::head(otras, 10), collapse = " | "), "\n", sep = "")
  expect_length(texto, 0)
  enteros <- sum(vapply(todos[indicadores], is.integer, logical(1)))
  cat("[en vivo] 2025 resultados indicadores: ", enteros, " enteros y ", length(indicadores) - enteros,
      " con decimales. Nombres: ", paste(indicadores, collapse = " "), "\n", sep = "")

  # Donde estan las marcas, en el archivo tal como se lee
  crudo <- poblacion_normalizar_claves(leer_tabla(xlsx), "resultados")
  nivel_crudo <- poblacion_clasificar_nivel(crudo, "resultados")
  marcas <- vapply(crudo[indicadores], function(x) {
    x <- trimws(x)
    ifelse(is.na(x) | grepl("^-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?$", x), NA_character_, x)
  }, character(nrow(crudo)))
  cat("[en vivo] 2025 resultados celdas: ", length(marcas), "; vacias: ", sum(is.na(as.matrix(crudo[indicadores]))),
      "; marcas: ", resumir_niveles(marcas[!is.na(marcas)]), "\n", sep = "")
  for (marca in sort(unique(marcas[!is.na(marcas)]))) {
    celdas <- !is.na(marcas) & marcas == marca
    por_fila <- rowSums(celdas)
    columnas <- colnames(celdas)[colSums(celdas) > 0]
    cat("[en vivo] 2025 resultados marca ", marca, ": ", sum(celdas), " celdas en ", length(columnas),
        " indicadores (", paste(utils::head(columnas, 8), collapse = ", "), "); filas con la marca: ",
        sum(por_fila > 0), "; por estimador: ",
        paste(names(tapply(por_fila, crudo$ESTIMADOR, sum)), tapply(por_fila, crudo$ESTIMADOR, sum),
              sep = "=", collapse = ", "),
        "; por nivel: ",
        paste(names(tapply(por_fila, nivel_crudo, sum)), tapply(por_fila, nivel_crudo, sum), sep = "=", collapse = ", "),
        "\n", sep = "")
  }
  expect_true(all(marcas[!is.na(marcas)] %in% poblacion_faltantes("resultados")))

  # Nombres: el descriptor dice que un asterisco marca los municipios censados
  # y dos, los de muestra insuficiente
  for (columna in c("NOM_ENT", "NOM_MUN", "NOM_LOC")) {
    nombres <- unique(todos[[columna]])
    marcados <- nombres[grepl("[*]", nombres)]
    cat("[en vivo] 2025 resultados ", columna, ": ", length(nombres), " nombres distintos; con asterisco: ",
        length(marcados), " (", paste(utils::head(marcados, 5), collapse = " | "), "); con dos asteriscos: ",
        sum(grepl("[*]{2}", nombres)), " (", paste(utils::head(nombres[grepl("[*]{2}", nombres)], 5), collapse = " | "),
        ")\n", sep = "")
  }
  for (nivel in c("nacional", "entidad", "municipio", "localidad", "agregado")) {
    fila <- todos[which(niveles == nivel)[1], claves]
    cat("[en vivo] 2025 resultados primera fila de nivel ", nivel, ": ",
        paste(names(fila), unlist(fila), sep = "=", collapse = ", "), "\n", sep = "")
  }

  if ("POBTOT" %in% names(todos) && is.numeric(todos$POBTOT)) {
    nacional <- todos$POBTOT[niveles == "nacional"]
    cat("[en vivo] 2025 resultados POBTOT nacional: ", paste(format(nacional, big.mark = ","), collapse = " | "),
        "\n", sep = "")
    # Poblacion en viviendas particulares (130 393 389) o total (130 911 314)
    expect_true(any(abs(nacional - 130393389) < 1 | abs(nacional - 130911314) < 1))
  }

  # Una entidad conserva sus filas del archivo nacional
  bcs <- suppressMessages(cpv_intercensal(2025, "resultados", estado = "03", nivel = "todos"))
  expect_identical(unique(bcs$CVE_ENT), "03")
  expect_identical(nrow(bcs), sum(todos$CVE_ENT == "03"))
})

# Medicion unica, fuera de las pruebas regulares: se activa al lanzar a mano el
# flujo pruebas-en-vivo.yaml con la opcion medir_memoria. Mide la entidad con el
# archivo mas grande, porque la muestra nacional no cabe en la memoria de los
# equipos de GitHub, y extrapola a todo el pais con el tamaño de la descarga.
test_that("EN VIVO (medicion): tiempo y memoria de la muestra de personas de la entidad mas grande", {
  skip_if_not(identical(Sys.getenv("IMPORTINEGI_MEDIR_MEMORIA"), "true"),
              "La medicion solo corre con IMPORTINEGI_MEDIR_MEMORIA=true")
  en_vivo()
  for (year in poblacion_programas()$intercensal$years) {
    archivos <- poblacion_archivos("intercensal", year, "muestra")
    entidades <- archivos[archivos$clave != "00", ]
    clave <- entidades$clave[which.max(entidades$bytes)]
    invisible(gc(reset = TRUE))
    inicio <- Sys.time()
    personas <- suppressMessages(cpv_intercensal(year, estado = clave))
    segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
    memoria <- gc()
    pico <- sum(memoria[, max(which(colnames(memoria) == "(Mb)"))])
    proporcion <- sum(entidades$bytes) / entidades$bytes[entidades$clave == clave]
    tabla_mb <- as.numeric(utils::object.size(personas)) / 1024^2
    cat("[en vivo] medicion ", year, " muestra ", clave, " (", tamano_texto(entidades$bytes[entidades$clave == clave]),
        "): ", nrow(personas), " personas x ", ncol(personas), " columnas; ", round(segundos), " s; tabla de ",
        round(tabla_mb), " MB; pico de memoria de R: ", round(pico), " MB. Extrapolado a todo el pais (x",
        round(proporcion, 1), "): unos ", round(nrow(personas) * proporcion / 1e6, 1), " millones de personas, tabla de ",
        round(tabla_mb * proporcion / 1024, 1), " GB y pico de ", round(pico * proporcion / 1024, 1), " GB\n", sep = "")
    expect_identical(unique(personas[[columna_clave(personas, "ent")]]), clave)
    rm(personas)
    invisible(gc())
  }
})
