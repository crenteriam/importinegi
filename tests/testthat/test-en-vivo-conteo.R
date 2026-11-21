# Pruebas en vivo de conteo(): descargan archivos reales del INEGI. Solo corren
# con IMPORTINEGI_EN_VIVO=true (ver .github/workflows/pruebas-en-vivo.yaml).
#
# De cada base se descarga el archivo de la entidad mas pequeña. Ademas de
# comprobar el resultado, cada prueba escribe en el registro de la ejecucion lo
# que encontro (contenido de cada zip, columnas y numero de filas). Las
# utilidades comunes estan en helper-en-vivo.R.

years_conteo <- function() {
  poblacion_programas()$conteo$years
}

test_that("EN VIVO: conteo() descarga el ITER de una entidad en cada conteo", {
  en_vivo()
  for (year in years_conteo()) {
    clave <- entidad_menor("conteo", year, "iter")
    describir_zip("conteo", year, "iter", clave)
    todos <- suppressMessages(conteo(year, estado = clave, nivel = "todos"))
    describir_tabla(paste(year, "iter", clave, "nivel = todos"), todos)
    niveles <- poblacion_clasificar_nivel(todos, "iter")
    expect_false(is.null(niveles), info = paste(year, "claves geograficas"))
    if (is.null(niveles)) {
      cat("[en vivo] ", year, " iter: columnas ", paste(names(todos), collapse = ", "), "\n", sep = "")
      next
    }
    cat("[en vivo] ", year, " iter ", clave, " niveles: ", resumir_niveles(niveles), "\n", sep = "")
    expect_identical(sum(niveles == "entidad"), 1L, info = as.character(year))
    expect_gt(sum(niveles == "municipio"), 0)
    expect_gt(sum(niveles == "localidad"), 0)
    expect_identical(unique(todos[[columna_clave(todos, "ent")]]), clave, info = as.character(year))
    revisar_texto(paste(year, "iter", clave), todos)

    # En los censos, las localidades suman el total de la entidad y los
    # agregados 9998 y 9999 (localidades de una y dos viviendas) repiten
    # poblacion ya contada. En el Conteo de 1995, las localidades pequeñas solo
    # aparecen en los agregados: las localidades mas los agregados de los
    # municipios suman el total. Se informa cual de las dos formas tiene cada
    # conteo.
    poblacion <- grep("^POBTOT$|^P_TOTAL$|^POB_TOT$|^POBTOTAL$", names(todos), value = TRUE, ignore.case = TRUE)
    if (length(poblacion) > 0 && is.numeric(todos[[poblacion[1]]])) {
      valores <- todos[[poblacion[1]]]
      entidad <- sum(valores[niveles == "entidad"])
      localidades <- sum(valores[niveles == "localidad"], na.rm = TRUE)
      agregados_municipales <- sum(valores[niveles == "agregado" & todos$MUN != "000"], na.rm = TRUE)
      cat("[en vivo] ", year, " iter ", poblacion[1], ": entidad = ", entidad,
          "; suma de municipios = ", sum(valores[niveles == "municipio"], na.rm = TRUE),
          "; suma de localidades = ", localidades,
          "; agregados 9998/9999 de los municipios = ", agregados_municipales,
          "; de la entidad = ", sum(valores[niveles == "agregado" & todos$MUN == "000"], na.rm = TRUE),
          if (localidades == entidad) " -> las localidades suman el total (los agregados repiten poblacion)",
          if (localidades != entidad && localidades + agregados_municipales == entidad)
            " -> las localidades mas los agregados de los municipios suman el total",
          "\n", sep = "")
      expect_true(localidades == entidad || localidades + agregados_municipales == entidad, info = as.character(year))
      expect_equal(sum(valores[niveles == "municipio"], na.rm = TRUE), entidad, info = as.character(year))
    } else {
      cat("[en vivo] ", year, " iter: no se identifico la columna de poblacion total. Columnas: ",
          paste(names(todos), collapse = ", "), "\n", sep = "")
    }

    # Por omision, solo localidades
    localidades <- suppressMessages(conteo(year, estado = clave))
    expect_identical(nrow(localidades), sum(niveles == "localidad"), info = as.character(year))
  }
})

test_that("EN VIVO: conteo() descarga el archivo nacional del ITER de 1995", {
  en_vivo()
  nacional <- suppressMessages(conteo(1995, nivel = "todos"))
  describir_tabla("1995 iter nacional", nacional)
  niveles <- poblacion_clasificar_nivel(nacional, "iter")
  expect_false(is.null(niveles))
  cat("[en vivo] 1995 iter nacional niveles: ", resumir_niveles(niveles), "\n", sep = "")
  expect_identical(sum(niveles == "nacional"), 1L)
  expect_identical(sum(niveles == "entidad"), 32L)
  entidades <- unique(nacional[[columna_clave(nacional, "ent")]][niveles == "localidad"])
  expect_identical(sort(entidades), sprintf("%02d", 1:32))

  # En 1995, las localidades de una y dos viviendas solo estan en los
  # agregados 9998 y 9999: en cada entidad, las localidades mas los agregados
  # de los municipios suman el total
  poblacion <- grep("^POBTOT$|^POBTOTAL$|^P_TOTAL$", names(nacional), value = TRUE)[1]
  ent <- nacional[[columna_clave(nacional, "ent")]]
  detalle <- niveles == "localidad" | (niveles == "agregado" & nacional$MUN != "000")
  totales <- tapply(nacional[[poblacion]][niveles == "entidad"], ent[niveles == "entidad"], sum)
  sumas <- tapply(nacional[[poblacion]][detalle], ent[detalle], sum, na.rm = TRUE)[names(totales)]
  solo_localidades <- tapply(nacional[[poblacion]][niveles == "localidad"], ent[niveles == "localidad"], sum, na.rm = TRUE)[names(totales)]
  distintas <- names(totales)[sumas != totales]
  cat("[en vivo] 1995 iter nacional: las localidades mas los agregados de los municipios suman el total en ",
      sum(sumas == totales), " de ", length(totales), " entidades; las localidades solas, en ",
      sum(solo_localidades == totales), ".",
      if (length(distintas) > 0) {
        paste0(" Diferencias: ", paste0(distintas, " (total ", totales[distintas], ", localidades ",
                                        solo_localidades[distintas], ", localidades mas agregados ", sumas[distintas], ")",
                                        collapse = "; "))
      },
      "\n", sep = "")
  # Como dice la ayuda de conteo(): las localidades solas suman menos que el
  # total en todas las entidades, y con los agregados de los municipios lo
  # alcanzan con una diferencia menor de 0.3 por ciento
  expect_true(all(solo_localidades < totales))
  expect_true(all(abs(sumas - totales) / totales < 0.003))
})

test_that("EN VIVO: conteo() descarga cada tabla de la muestra", {
  en_vivo()
  for (year in years_conteo()) {
    clave <- entidad_menor("conteo", year, "muestra")
    describir_zip("conteo", year, "muestra", clave)
    for (tabla in names(poblacion_tablas("conteo", year, "muestra"))) {
      muestra <- suppressMessages(conteo(year, "muestra", estado = clave, tabla = tabla))
      etiqueta <- paste(year, "muestra", clave, tabla)
      describir_tabla(etiqueta, muestra)
      revisar_texto(etiqueta, muestra)
      expect_gt(nrow(muestra), 0)
      expect_false(is.na(columna_clave(muestra, "ent")), info = etiqueta)
      factores <- grep("^FAC", names(muestra), value = TRUE, ignore.case = TRUE)
      cat("[en vivo] ", etiqueta, " factor de expansion: ",
          if (length(factores) > 0) {
            paste0(factores, " (", vapply(muestra[factores], function(x) class(x)[1], character(1)), ", suma ",
                   vapply(muestra[factores], function(x) if (is.numeric(x)) format(sum(x, na.rm = TRUE)) else "-", character(1)),
                   ")", collapse = ", ")
          } else {
            "no hay columnas FAC*"
          },
          "\n", sep = "")
      # El factor de expansion y la edad deben ser numericos
      for (columna in grep("^FACTOR$|^FAC_|^EDAD$", names(muestra), value = TRUE, ignore.case = TRUE)) {
        expect_true(is.numeric(muestra[[columna]]), info = paste(etiqueta, columna))
      }
      texto <- names(muestra)[vapply(muestra, is.character, logical(1))]
      cat("[en vivo] ", etiqueta, " columnas de texto: ", paste(texto, collapse = ", "), "\n", sep = "")
      cat("[en vivo] ", etiqueta, " primera fila: ",
          paste(names(muestra), vapply(muestra[1, , drop = FALSE], function(x) as.character(x[1]), character(1)),
                sep = "=", collapse = ", "), "\n", sep = "")
    }
  }
})

test_that("EN VIVO: conteo() descarga Aguascalientes por omision en la muestra", {
  en_vivo()
  mensajes <- testthat::capture_messages(muestra <- conteo(1995, "muestra"))
  expect_match(mensajes[1], "No indicaste `estado`: se descarga la muestra de Aguascalientes")
  expect_identical(unique(muestra[[columna_clave(muestra, "ent")]]), "01")
})

test_that("EN VIVO: conteo() combina las 32 entidades de la muestra de 1995", {
  en_vivo()
  # Informativo: zip de entidades que traen una tabla repetida
  archivos <- poblacion_archivos("conteo", 1995L, "muestra")
  for (i in seq_len(nrow(archivos))) {
    zip <- suppressMessages(inegi_descargar(archivos$url[i]))
    contenido <- utils::unzip(zip, list = TRUE)$Name
    nombres <- tolower(basename(contenido[!grepl("/$", contenido)]))
    if (anyDuplicated(nombres) > 0 || any(grepl("\\.zip$", nombres))) {
      cat("[en vivo] 1995 muestra ", archivos$clave[i], ": ", basename(archivos$url[i]), " contiene: ",
          paste(contenido, collapse = " | "), "\n", sep = "")
    }
  }
  inicio <- Sys.time()
  nacional <- suppressMessages(conteo(1995, "muestra", estado = "nacional"))
  segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  describir_tabla("1995 muestra nacional", nacional)
  entidad <- columna_clave(nacional, "ent")
  expect_identical(sort(unique(nacional[[entidad]])), sprintf("%02d", 1:32))
  expect_length(attr(nacional, "fuente"), 32)
  cat("[en vivo] 1995 muestra nacional: ", nrow(nacional), " personas, ", round(segundos), " s, ",
      format(utils::object.size(nacional), units = "MB"), "\n", sep = "")
  # Cada entidad aporta las mismas filas que su descarga individual
  clave <- entidad_menor("conteo", 1995L, "muestra")
  sola <- suppressMessages(conteo(1995, "muestra", estado = clave))
  expect_identical(sum(nacional[[entidad]] == clave), nrow(sola))
})

test_that("EN VIVO: conteo() descarga las viviendas habitadas y deshabitadas de 2005", {
  en_vivo()
  clave <- entidad_menor("conteo", 2005L, "viviendas")
  describir_zip("conteo", 2005L, "viviendas", clave)
  viviendas <- suppressMessages(conteo(2005, "viviendas", estado = clave))
  describir_tabla(paste("2005 viviendas", clave), viviendas)
  revisar_texto(paste("2005 viviendas", clave), viviendas)
  expect_gt(nrow(viviendas), 0)
  expect_false(is.na(columna_clave(viviendas, "ent")))
  expect_false(is.na(columna_clave(viviendas, "mun")))
  todas <- suppressMessages(conteo(2005, "viviendas", estado = clave, nivel = "todos"))
  niveles <- poblacion_clasificar_nivel(todas, "viviendas")
  expect_false(is.null(niveles))
  cat("[en vivo] 2005 viviendas ", clave, " niveles: ", resumir_niveles(niveles), "\n", sep = "")
  expect_identical(sum(niveles == "entidad"), 1L)
  expect_gt(sum(niveles == "municipio"), 0)
  expect_identical(nrow(viviendas), sum(niveles == "localidad"))
  total <- grep("^VIV_TOT$|^VIVTOT$", names(todas), value = TRUE)
  if (length(total) > 0 && is.numeric(todas[[total[1]]])) {
    valores <- todas[[total[1]]]
    cat("[en vivo] 2005 viviendas ", total[1], ": entidad = ", sum(valores[niveles == "entidad"]),
        "; suma de municipios = ", sum(valores[niveles == "municipio"], na.rm = TRUE),
        "; suma de localidades = ", sum(valores[niveles == "localidad"], na.rm = TRUE), "\n", sep = "")
    expect_equal(sum(valores[niveles == "municipio"], na.rm = TRUE), sum(valores[niveles == "entidad"]))
  }
  nacional <- suppressMessages(conteo(2005, "viviendas", nivel = "todos"))
  niveles <- poblacion_clasificar_nivel(nacional, "viviendas")
  cat("[en vivo] 2005 viviendas nacional: ", nrow(nacional), " filas; niveles: ", resumir_niveles(niveles), "\n", sep = "")
  expect_identical(sum(niveles == "entidad"), 32L)
})

# Medicion unica, fuera de las pruebas regulares: se activa al lanzar a mano el
# flujo pruebas-en-vivo.yaml con la opcion medir_memoria.
test_that("EN VIVO (medicion): tiempo y memoria de la muestra nacional del Conteo 2005", {
  skip_if_not(identical(Sys.getenv("IMPORTINEGI_MEDIR_MEMORIA"), "true"),
              "La medicion solo corre con IMPORTINEGI_MEDIR_MEMORIA=true")
  en_vivo()
  invisible(gc(reset = TRUE))
  inicio <- Sys.time()
  nacional <- suppressMessages(conteo(2005, "muestra", estado = "nacional"))
  segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  memoria <- gc()
  pico <- sum(memoria[, max(which(colnames(memoria) == "(Mb)"))])
  cat("[en vivo] medicion 2005 muestra nacional: ", nrow(nacional), " filas x ", ncol(nacional), " columnas; ",
      round(segundos), " s (descarga y lectura); tabla de ", format(utils::object.size(nacional), units = "MB"),
      "; pico de memoria de R: ", round(pico), " MB\n", sep = "")
  expect_identical(sort(unique(nacional[[columna_clave(nacional, "ent")]])), sprintf("%02d", 1:32))
})
