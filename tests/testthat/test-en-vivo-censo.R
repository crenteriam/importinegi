# Pruebas en vivo de censo(): descargan archivos reales del INEGI. Solo corren
# con IMPORTINEGI_EN_VIVO=true (ver .github/workflows/pruebas-en-vivo.yaml).
#
# De cada base se descarga el archivo de la entidad mas pequeña, para que la
# prueba sea rapida. Ademas de comprobar el resultado, cada prueba escribe en
# el registro de la ejecucion lo que encontro (contenido de cada zip, columnas
# y numero de filas), para detectar cambios del INEGI.

# Conserva las descargas durante la ejecucion: varias pruebas leen el mismo zip.
censo_en_vivo <- function(entorno = parent.frame()) {
  omitir_si_no_en_vivo()
  withr::local_options(
    importinegi.cache = TRUE,
    importinegi.cache_dir = file.path(tempdir(), "censo_en_vivo"),
    .local_envir = entorno
  )
}

# Entidad con el archivo mas pequeño de una base, o NA si el registro no la tiene.
entidad_menor <- function(year, base) {
  archivos <- poblacion_archivos("censo", year, base)
  archivos <- archivos[archivos$clave != "00", ]
  if (nrow(archivos) == 0) NA_character_ else archivos$clave[which.min(archivos$bytes)]
}

# Escribe el contenido del zip de una entidad.
describir_zip <- function(year, base, clave) {
  archivos <- poblacion_archivos("censo", year, base)
  fila <- archivos[archivos$clave == clave, ]
  zip <- suppressMessages(inegi_descargar(fila$url))
  carpeta <- inegi_descomprimir(zip)
  on.exit(unlink(carpeta, recursive = TRUE))
  tablas <- buscar_archivos(carpeta, extension = "dbf")
  paginas <- vapply(tablas, function(tabla) sprintf("%02x", as.integer(readBin(tabla, "raw", n = 32)[30])), character(1))
  cat("[en vivo] ", year, " ", base, " ", clave, ": ", basename(fila$url), " (", fila$tamano, ") contiene: ",
      paste(list.files(carpeta, recursive = TRUE), collapse = " | "),
      if (length(tablas) > 0) paste0("; byte de pagina de codigos: ", paste(unique(paginas), collapse = ", ")),
      "\n", sep = "")
}

# Comprueba que el texto sea UTF-8 valido y muestra algunos valores con acento.
revisar_texto <- function(etiqueta, datos) {
  texto <- names(datos)[vapply(datos, is.character, logical(1))]
  validos <- vapply(datos[texto], function(x) all(validUTF8(x)), logical(1))
  expect_true(all(validos), info = paste(etiqueta, "columnas con texto invalido:", paste(texto[!validos], collapse = ", ")))
  nombres <- grep("^NOM_", texto, value = TRUE)
  ejemplos <- unique(unlist(lapply(datos[nombres], function(x) grep("[^ -~]", x, value = TRUE))))
  cat("[en vivo] ", etiqueta, " texto con acento: ",
      if (length(ejemplos) > 0) paste(utils::head(ejemplos, 5), collapse = " | ") else "(ninguno en columnas NOM_)",
      "\n", sep = "")
}

describir_tabla <- function(etiqueta, datos) {
  tipos <- paste0(names(datos), "=", vapply(datos, function(x) class(x)[1], character(1)))
  cat("[en vivo] ", etiqueta, ": ", nrow(datos), " filas x ", ncol(datos), " columnas. ",
      paste(utils::head(tipos, 16), collapse = ", "), if (length(tipos) > 16) ", ...", "\n", sep = "")
}

years_censo <- function() {
  poblacion_programas()$censo$years
}

test_that("EN VIVO (informativo): bases del censo que tiene el registro del paquete", {
  censo_en_vivo()
  bases <- poblacion_bases()
  for (i in seq_len(nrow(bases))) {
    archivos <- poblacion_archivos(bases$programa[i], bases$year[i], bases$base[i])
    cat("[en vivo] registro: ", bases$year[i], " ", bases$base[i], ": ", nrow(archivos), " archivos",
        if (nrow(archivos) > 0) paste0(" (", paste(unique(archivos$formato), collapse = "/"), ", ",
                                       if ("00" %in% archivos$clave) "con" else "sin", " archivo nacional, ",
                                       tamano_texto(archivos$bytes), ")"),
        "\n", sep = "")
  }
  expect_gt(nrow(poblacion_archivos("censo", 2010L, "iter")), 0)
})

test_that("EN VIVO: censo() descarga el ITER de una entidad en cada censo", {
  censo_en_vivo()
  for (year in years_censo()) {
    clave <- entidad_menor(year, "iter")
    if (is.na(clave)) {
      cat("[en vivo] ", year, " iter: el registro no lo incluye\n", sep = "")
      next
    }
    describir_zip(year, "iter", clave)
    todos <- suppressMessages(censo(year, estado = clave, nivel = "todos"))
    describir_tabla(paste(year, "iter", clave, "nivel = todos"), todos)
    niveles <- poblacion_clasificar_nivel(todos, "iter")
    expect_false(is.null(niveles), info = paste(year, "claves geograficas"))
    if (is.null(niveles)) {
      cat("[en vivo] ", year, " iter: columnas ", paste(names(todos), collapse = ", "), "\n", sep = "")
      next
    }
    cat("[en vivo] ", year, " iter ", clave, " niveles: ",
        paste(names(table(niveles)), table(niveles), sep = "=", collapse = ", "), "\n", sep = "")
    expect_identical(sum(niveles == "entidad"), 1L, info = year)
    expect_gt(sum(niveles == "municipio"), 0)
    expect_gt(sum(niveles == "localidad"), 0)
    expect_identical(unique(todos[[columna_clave(todos, "ent")]]), clave, info = year)

    revisar_texto(paste(year, "iter", clave), todos)

    # Las localidades suman el total de la entidad; los agregados 9998 y 9999
    # (localidades de una y dos viviendas) repiten poblacion ya contada.
    poblacion <- grep("^POBTOT$|^P_TOTAL$|^POB_TOT$|^POBTOTAL$", names(todos), value = TRUE, ignore.case = TRUE)
    if (length(poblacion) > 0 && is.numeric(todos[[poblacion[1]]])) {
      valores <- todos[[poblacion[1]]]
      cat("[en vivo] ", year, " iter ", poblacion[1], ": entidad = ", sum(valores[niveles == "entidad"]),
          "; suma de municipios = ", sum(valores[niveles == "municipio"], na.rm = TRUE),
          "; suma de localidades = ", sum(valores[niveles == "localidad"], na.rm = TRUE),
          "; suma de agregados 9998/9999 = ", sum(valores[niveles == "agregado"], na.rm = TRUE), "\n", sep = "")
      expect_equal(sum(valores[niveles == "localidad"], na.rm = TRUE), sum(valores[niveles == "entidad"]), info = year)
      expect_equal(sum(valores[niveles == "municipio"], na.rm = TRUE), sum(valores[niveles == "entidad"]), info = year)
    } else {
      cat("[en vivo] ", year, " iter: no se identifico la columna de poblacion total. Columnas: ",
          paste(names(todos), collapse = ", "), "\n", sep = "")
    }

    # Por omision, solo localidades
    localidades <- suppressMessages(censo(year, estado = clave))
    expect_identical(nrow(localidades), sum(niveles == "localidad"), info = year)
  }
})

test_that("EN VIVO: censo() descarga el archivo nacional del ITER", {
  censo_en_vivo()
  # El archivo nacional mas pequeño es el de 1990
  nacional <- suppressMessages(censo(1990, nivel = "todos"))
  describir_tabla("1990 iter nacional", nacional)
  niveles <- poblacion_clasificar_nivel(nacional, "iter")
  expect_false(is.null(niveles))
  cat("[en vivo] 1990 iter nacional niveles: ",
      paste(names(table(niveles)), table(niveles), sep = "=", collapse = ", "), "\n", sep = "")
  expect_identical(sum(niveles == "nacional"), 1L)
  expect_identical(sum(niveles == "entidad"), 32L)
  entidades <- unique(nacional[[columna_clave(nacional, "ent")]][niveles == "localidad"])
  expect_identical(sort(entidades), sprintf("%02d", 1:32))
})

test_that("EN VIVO: censo() descarga los resultados por AGEB y manzana", {
  censo_en_vivo()
  for (year in years_censo()) {
    clave <- entidad_menor(year, "ageb")
    if (is.na(clave)) {
      next
    }
    describir_zip(year, "ageb", clave)
    todos <- suppressMessages(censo(year, "ageb", estado = clave, nivel = "todos"))
    describir_tabla(paste(year, "ageb", clave, "nivel = todos"), todos)
    niveles <- poblacion_clasificar_nivel(todos, "ageb")
    expect_false(is.null(niveles), info = year)
    if (is.null(niveles)) {
      cat("[en vivo] ", year, " ageb: columnas ", paste(names(todos), collapse = ", "), "\n", sep = "")
      next
    }
    cat("[en vivo] ", year, " ageb ", clave, " niveles: ",
        paste(names(table(niveles)), table(niveles), sep = "=", collapse = ", "), "\n", sep = "")
    expect_identical(sum(niveles == "entidad"), 1L, info = year)
    for (nivel in c("municipio", "localidad", "ageb", "manzana")) {
      expect_gt(sum(niveles == nivel), 0)
    }
    revisar_texto(paste(year, "ageb", clave), todos)
    manzanas <- suppressMessages(censo(year, "ageb", estado = clave))
    expect_identical(nrow(manzanas), sum(niveles == "manzana"), info = year)
  }
})

test_that("EN VIVO: censo() descarga cada tabla de la muestra", {
  censo_en_vivo()
  for (year in years_censo()) {
    clave <- entidad_menor(year, "muestra")
    if (is.na(clave)) {
      cat("[en vivo] ", year, " muestra: el registro no la incluye\n", sep = "")
      next
    }
    describir_zip(year, "muestra", clave)
    tablas <- names(poblacion_tablas("censo", year))
    if (is.null(tablas)) {
      muestra <- suppressMessages(censo(year, "muestra", estado = clave))
      describir_tabla(paste(year, "muestra", clave), muestra)
      revisar_texto(paste(year, "muestra", clave), muestra)
      expect_gt(nrow(muestra), 0)
      next
    }
    for (tabla in tablas) {
      muestra <- suppressMessages(censo(year, "muestra", estado = clave, tabla = tabla))
      etiqueta <- paste(year, "muestra", clave, tabla)
      describir_tabla(etiqueta, muestra)
      revisar_texto(etiqueta, muestra)
      expect_gt(nrow(muestra), 0)
      # El factor de expansion y la edad deben ser numericos
      for (columna in grep("^FACTOR$|^EDAD$|^MEDAD$", names(muestra), value = TRUE)) {
        expect_true(is.numeric(muestra[[columna]]), info = paste(etiqueta, columna))
      }
      texto <- names(muestra)[vapply(muestra, is.character, logical(1))]
      cat("[en vivo] ", etiqueta, " columnas de texto: ", paste(texto, collapse = ", "), "\n", sep = "")
    }
  }
})

test_that("EN VIVO: censo() descarga las localidades y el entorno urbano", {
  censo_en_vivo()
  for (base in c("localidades", "entorno")) {
    for (year in years_censo()) {
      clave <- entidad_menor(year, base)
      if (is.na(clave)) {
        next
      }
      describir_zip(year, base, clave)
      datos <- suppressMessages(censo(year, base, estado = clave))
      describir_tabla(paste(year, base, clave), datos)
      revisar_texto(paste(year, base, clave), datos)
      expect_gt(nrow(datos), 0)
      expect_false(is.na(columna_clave(datos, "ent")), info = paste(year, base))
      expect_false(is.na(columna_clave(datos, "mun")), info = paste(year, base))
    }
  }
})
