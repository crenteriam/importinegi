# Pruebas en vivo de cng_municipal(): descargan archivos reales del INEGI. Solo
# corren con IMPORTINEGI_EN_VIVO=true (ver .github/workflows/pruebas-en-vivo.yaml).
#
# La forma de los archivos se reviso el 22 de noviembre de 2026 en el indice de
# los zip, sin descargarlos. Estas pruebas leen los archivos completos: el tema
# mas pequeño de cada modulo y edicion (61 descargas) y los temas de los
# ejemplos de la ayuda. Ademas de comprobar el resultado, escriben en el
# registro de la ejecucion lo que encontraron.

# Columna con la clave del municipio de una tabla, o NA.
clave_municipio <- function(tabla) {
  columnas <- gobierno_columnas_clave(tabla)$municipio
  if (length(columnas) == 0) NA_character_ else columnas[1]
}

# Columnas de texto que contienen alguna de las siglas que se conservan.
con_siglas <- function(tabla) {
  names(tabla)[vapply(tabla, function(x) is.character(x) && any(x %in% c("NSS", "ND", "NP")), logical(1))]
}

test_that("EN VIVO: cng_municipal() lee el tema mas peque\u00f1o de cada modulo y edicion", {
  en_vivo()
  temas <- gobierno_temas("cng_municipal")
  grupos <- split(temas, paste(temas$year, match(temas$modulo, gobierno_modulos("cng_municipal")$modulo)))
  expect_identical(length(grupos), 61L)
  for (grupo in grupos) {
    tema <- grupo[which.min(grupo$bytes), ]
    etiqueta <- paste(tema$year, tema$modulo, tema$base)
    tablas <- tryCatch(suppressMessages(cng_municipal(tema$year, tema$modulo, tema$base)), error = function(e) e)
    expect_false(inherits(tablas, "error"), info = paste(etiqueta, if (inherits(tablas, "error")) conditionMessage(tablas)))
    if (inherits(tablas, "error")) {
      cat("[en vivo] ", etiqueta, ": ERROR ", conditionMessage(tablas), "\n", sep = "")
      next
    }
    contenido <- gobierno_contenido(ruta_cache(tema$url), tema$formato)
    claves <- vapply(tablas, clave_municipio, character(1))
    siglas <- unlist(lapply(tablas, con_siglas))
    cat("[en vivo] ", etiqueta, " (", tema$tamano, ", ", tema$formato, "): ", length(tablas), " tablas (",
        paste(utils::head(names(tablas), 4), collapse = ", "), if (length(tablas) > 4) ", ...", "); filas de ",
        min(vapply(tablas, nrow, integer(1))), " a ", max(vapply(tablas, nrow, integer(1))),
        "; clave del municipio: ", paste(unique(claves), collapse = ", "),
        "; columnas con NSS, ND o NP: ", length(siglas),
        "; archivos: ", paste(names(table(contenido$tipo)), table(contenido$tipo), sep = "=", collapse = ", "),
        if (any(grepl("\\", contenido$original, fixed = TRUE, useBytes = TRUE))) "; rutas con \\", "\n", sep = "")
    for (nombre in names(tablas)[is.na(claves)]) {
      cat("[en vivo] ", etiqueta, " sin clave del municipio en ", nombre, ": ",
          paste(utils::head(names(tablas[[nombre]]), 8), collapse = ", "), "\n", sep = "")
    }

    expect_gt(length(tablas), 0)
    expect_false(any(grepl("_cng[a-z]+[0-9]+$", names(tablas))), info = etiqueta)
    expect_identical(anyDuplicated(names(tablas)), 0L, info = etiqueta)
    for (nombre in names(tablas)[!is.na(claves)]) {
      valores <- tablas[[nombre]][[claves[[nombre]]]]
      expect_true(all(is.na(valores) | grepl("^[0-9]{5}$", valores)),
                  info = paste(etiqueta, nombre, paste(utils::head(unique(valores[!grepl("^[0-9]{5}$", valores)]), 5), collapse = " ")))
    }
    texto <- as.character(unlist(lapply(tablas, function(tabla) unlist(tabla[vapply(tabla, is.character, logical(1))], use.names = FALSE))))
    expect_true(all(validUTF8(texto)), info = paste(etiqueta, "texto que no es UTF-8"))
    expect_false(any(texto %in% "NA"), info = paste(etiqueta, "queda la sigla NA"))

    indice <- attr(tablas, "indice")
    expect_identical(indice$tabla, names(tablas), info = etiqueta)
    if (tema$year >= 2021) {
      expect_false(all(is.na(indice$contenido)), info = paste(etiqueta, "indice sin contenido"))
      if (anyNA(indice$contenido)) {
        cat("[en vivo] ", etiqueta, " tablas que el indice no describe: ",
            paste(indice$tabla[is.na(indice$contenido)], collapse = ", "), "\n", sep = "")
      }
      diccionario <- tryCatch(suppressMessages(cng_municipal(tema$year, tema$modulo, tema$base, tabla = "diccionario")),
                              error = function(e) e)
      expect_false(inherits(diccionario, "error"),
                   info = paste(etiqueta, "diccionario:", if (inherits(diccionario, "error")) substr(conditionMessage(diccionario), 1, 300)))
      if (!inherits(diccionario, "error")) {
        ajenas <- setdiff(unique(diccionario$tabla), names(tablas))
        expect_identical(ajenas, character(), info = paste(etiqueta, "diccionario sin tabla"))
        cat("[en vivo] ", etiqueta, " diccionario: ", nrow(diccionario), " variables de ",
            length(unique(diccionario$tabla)), " tablas; columnas: ", paste(names(diccionario), collapse = ", "), "\n", sep = "")
      }
    }
  }
})

test_that("EN VIVO: los temas de los ejemplos tienen la forma documentada", {
  en_vivo()
  # Comisiones e iniciativas de los ayuntamientos, 2011: cinco tablas
  comision <- suppressMessages(cng_municipal(year = 2011, modulo = "ayuntamiento", tema = "comision"))
  expect_named(comision, c("act_ayun", "com_ayun", "com_tasu", "in_teasu", "ini_ayun"))
  clave <- clave_municipio(comision$act_ayun)
  expect_false(is.na(clave))
  describir_tabla("2011 comision act_ayun", comision$act_ayun)
  cat("[en vivo] 2011 comision: municipios en act_ayun = ", length(unique(comision$act_ayun[[clave]])), " (", clave, ")", "\n", sep = "")
  estructura <- suppressMessages(cng_municipal(year = 2011, modulo = "ayuntamiento", tema = "estructura"))
  cat("[en vivo] 2011 estructura: ", paste(names(estructura), collapse = ", "), "\n", sep = "")
  expect_gt(length(estructura), 0)

  # Ayuntamientos, 2025: una tabla por pregunta, con indice, catalogos y diccionario
  ayuntamientos <- suppressMessages(cng_municipal(2025, "ayuntamiento", "ayuntamientos"))
  expect_identical(length(ayuntamientos), 24L)
  expect_identical(names(ayuntamientos)[1:3], c("m1s1p1", "m1s1p2", "m1s1p3"))
  expect_identical(names(ayuntamientos)[24], "m1s1p19b")
  sindicas <- ayuntamientos$m1s1p3
  describir_tabla("2025 ayuntamientos m1s1p3", sindicas)
  expect_true(all(c("cvegeo", "cve_ent", "cve_mun") %in% names(sindicas)))
  expect_true(all(grepl("^[0-9]{2}$", sindicas$cve_ent)) && all(grepl("^[0-9]{3}$", sindicas$cve_mun)))
  indice <- attr(ayuntamientos, "indice")
  expect_match(indice$contenido[indice$tabla == "m1s1p3"], "^Personas s\u00edndicas")
  siglas <- lapply(ayuntamientos, con_siglas)
  cat("[en vivo] 2025 ayuntamientos: municipios en m1s1p1 = ", length(unique(ayuntamientos$m1s1p1$cvegeo)),
      "; tablas con columnas de texto por NSS, ND o NP: ", sum(lengths(siglas) > 0), " de 24; en m1s1p3: ",
      paste(siglas$m1s1p3, collapse = ", "), "\n", sep = "")

  jalisco <- suppressMessages(cng_municipal(2025, "ayuntamiento", "ayuntamientos", tabla = "m1s1p3", estado = "Jalisco"))
  expect_identical(unique(substr(jalisco$cvegeo, 1, 2)), "14")
  expect_identical(nrow(jalisco), sum(sindicas$cve_ent == "14"))

  catalogos <- suppressMessages(cng_municipal(2025, "ayuntamiento", "ayuntamientos", tabla = "catalogos"))
  expect_true("sexo_i" %in% names(catalogos))
  codificadas <- intersect(names(sindicas), names(catalogos))
  cat("[en vivo] 2025 ayuntamientos: ", length(catalogos), " catalogos (", paste(utils::head(names(catalogos), 8), collapse = ", "),
      ", ...); variables de m1s1p3 con catalogo: ", paste(codificadas, collapse = ", "), "\n", sep = "")
  for (variable in codificadas) {
    expect_identical(class(sindicas[[variable]]), class(catalogos[[variable]][[1]]), info = variable)
  }

  # El tema tambien se elige con el nombre de su archivo
  integrantes <- suppressMessages(cng_municipal(2017, "ayuntamiento", "integrantes_ayuntami"))
  cat("[en vivo] 2017 integrantes_ayuntami: ", paste(names(integrantes), collapse = ", "), "\n", sep = "")
  expect_gt(length(integrantes), 0)

  # Agua potable de 2011: solo en DBF
  agua <- suppressMessages(cng_municipal(2011, "agua"))
  cat("[en vivo] 2011 agua (DBF): ", length(agua), " tablas (", paste(names(agua), collapse = ", "), "); clave: ",
      paste(unique(vapply(agua, clave_municipio, character(1))), collapse = ", "), "\n", sep = "")
  expect_gt(length(agua), 0)
  for (nombre in names(agua)) {
    vacias <- sum(rowSums(!is.na(agua[[nombre]])) == 0)
    cat("[en vivo] 2011 agua ", nombre, ": ", nrow(agua[[nombre]]), " filas (", vacias, " vacias) x ", ncol(agua[[nombre]]),
        " columnas: ", paste(utils::head(names(agua[[nombre]]), 8), collapse = ", "), "\n", sep = "")
  }
})
