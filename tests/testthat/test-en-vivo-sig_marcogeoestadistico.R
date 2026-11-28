# Pruebas en vivo de sig_marcogeoestadistico(): descargan archivos reales del
# INEGI. Solo corren con IMPORTINEGI_EN_VIVO=true (ver
# .github/workflows/pruebas-en-vivo.yaml).
#
# La forma de los archivos se reviso el 24 y el 27 de noviembre de 2026 en el
# indice de los zip de las 26 ediciones, en los encabezados de sus DBF y en sus
# archivos .prj, sin descargar los productos. Estas pruebas leen los mapas
# completos y comprueban lo que quedo por confirmar:
#
#   - que cada llamada devuelve un solo mapa: un objeto de clase sf;
#   - que el texto llega como UTF-8 valido con la codificacion que elige la
#     funcion (Latin-1 en las capas que no declaran ninguna);
#   - que la proyeccion de cada mapa es la que documenta la ayuda;
#   - que cada capa de los productos completos tiene una columna con la clave
#     de la entidad, con la que se conservan las filas de `estado`;
#   - que el servidor del INEGI atiende la descarga parcial de los productos
#     por entidad;
#   - si los archivos de las entidades de un mismo mapa escriben la proyeccion
#     de distinta manera.
#
# Ademas de comprobar el resultado, escriben en el registro de la ejecucion lo
# que encontraron: filas, columnas, proyeccion y ejemplos de texto con acento.
#
# En los pull requests corre una muestra: todos los mapas de Colima de la
# edicion mas reciente, las entidades de Colima de las diez ediciones de la
# descarga por capa, un producto completo (1995) y un producto por entidad
# (febrero de 2018); cerca de 110 MB de descarga. La opcion marcogeo_completo
# del flujo lee todos los mapas de las 26 ediciones (cerca de 2.2 GB); no corre
# en los pull requests.

marcogeo_completo <- function() {
  skip_if_not(
    identical(Sys.getenv("IMPORTINEGI_MARCOGEO_COMPLETO"), "true"),
    "Las 26 ediciones completas solo corren con IMPORTINEGI_MARCOGEO_COMPLETO=true"
  )
}

# Llama a la funcion y guarda sus mensajes.
mapa_en_vivo <- function(year, mapa, estado = NULL, version = NULL) {
  mensajes <- character()
  resultado <- withCallingHandlers(
    sig_marcogeoestadistico(year, mapa, estado = estado, version = version),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(mapa = resultado, mensajes = mensajes)
}

# Proyeccion que documenta la ayuda para cada mapa: "ccl" (Conica Conforme de
# Lambert), "geograficas" o "ninguna".
proyeccion_documentada <- function(year, mapa) {
  if (year == 2005 && mapa %in% c("entidades", "municipios")) {
    "ninguna"
  } else if (year == 2014 && mapa != "territorio_insular") {
    "geograficas"
  } else if (year == 2024 && mapa == "frentes") {
    "geograficas"
  } else {
    "ccl"
  }
}

# Comprueba un mapa descargado y lo describe en el registro de la ejecucion.
# `clave` es la entidad pedida, si se pidio una en la descarga por capa o por
# entidad: todas las filas deben ser suyas.
revisar_mapa <- function(etiqueta, resultado, proyeccion, clave = NULL) {
  mapa <- resultado$mapa
  # Siempre un solo mapa
  esperar_un_mapa(mapa)
  expect_gt(nrow(mapa), 0, label = paste(etiqueta, "filas"))
  expect_false(is.null(attr(mapa, "fuente")), info = etiqueta)

  datos <- sf::st_drop_geometry(mapa)
  texto <- names(datos)[vapply(datos, is.character, logical(1))]
  validos <- vapply(datos[texto], function(x) all(validUTF8(x)), logical(1))
  expect_true(all(validos), info = paste(etiqueta, "texto no valido en:", paste(texto[!validos], collapse = ", ")))
  # El texto "NULL" llega como NA
  expect_false(any(vapply(datos[texto], function(x) any(x == "NULL", na.rm = TRUE), logical(1))), info = etiqueta)
  con_acento <- unique(unlist(lapply(datos[texto], function(x) utils::head(grep("[^ -~]", x, value = TRUE), 2))))

  crs <- sf::st_crs(mapa)
  caja <- sf::st_bbox(mapa)
  if (proyeccion == "ninguna") {
    expect_true(is.na(crs), info = etiqueta)
    expect_match(resultado$mensajes, "se devuelve sin proyecci\u00f3n", all = FALSE, info = etiqueta)
  } else if (proyeccion == "geograficas") {
    expect_true(isTRUE(sf::st_is_longlat(mapa)), info = etiqueta)
    expect_true(caja[["xmin"]] > -119.5 && caja[["xmax"]] < -86 && caja[["ymin"]] > 14 && caja[["ymax"]] < 33.5, info = etiqueta)
  } else {
    for (parametro in c("+proj=lcc", "+lat_0=12", "+lon_0=-102", "+lat_1=17.5", "+lat_2=29.5", "+x_0=2500000", "+ellps=GRS80", "+units=m")) {
      expect_match(crs$proj4string, parametro, fixed = TRUE, info = etiqueta)
    }
    expect_true(caja[["xmin"]] > 900000 && caja[["xmax"]] < 4200000 && caja[["ymin"]] > 300000 && caja[["ymax"]] < 2500000, info = etiqueta)
  }
  if (!is.null(clave)) {
    propias <- suppressMessages(marcogeo_filtrar(mapa, clave))
    expect_identical(nrow(propias), nrow(mapa), info = paste(etiqueta, "filas de otra entidad"))
  }
  cat(
    "[en vivo] ", etiqueta, ": ", nrow(mapa), " filas; ", paste(names(datos), collapse = ","), "; ",
    paste(unique(as.character(sf::st_geometry_type(mapa))), collapse = "/"), "; proyeccion: ",
    if (is.na(crs)) "(ninguna)" else crs$input, " (epsg ", crs$epsg, "); acentos: ",
    if (length(con_acento) > 0) paste(utils::head(con_acento, 3), collapse = " | ") else "(ninguno)", "\n", sep = ""
  )
  invisible(mapa)
}

# Ediciones de una via de descarga, como lista de filas.
ediciones_via <- function(via) {
  ediciones <- marcogeo_ediciones()
  ediciones <- ediciones[ediciones$via == via, ]
  lapply(seq_len(nrow(ediciones)), function(i) ediciones[i, ])
}
version_de <- function(edicion) {
  if (nzchar(edicion$version)) edicion$version else NULL
}

# Muestra que corre en los pull requests ---------------------------------------------

test_that("EN VIVO: descarga por capa, todos los mapas de Colima en la edicion mas reciente", {
  en_vivo()
  skip_if_not_installed("sf")
  edicion <- ediciones_via("B")
  edicion <- edicion[[length(edicion)]]
  mapas <- marcogeo_mapas_edicion(edicion)
  expect_length(mapas, 16L)
  for (mapa in mapas) {
    etiqueta <- paste(marcogeo_nombre_edicion(edicion), mapa, "Colima")
    resultado <- mapa_en_vivo(edicion$year, mapa, estado = "Colima")
    revisar_mapa(etiqueta, resultado, proyeccion_documentada(edicion$year, mapa), clave = "06")
  }
})

# Entidades de una edicion de la descarga por capa: sin `estado` se descarga el
# archivo de todo el pais, con las mismas columnas que el de una entidad.
revisar_entidades_pais <- function(edicion, colima) {
  etiqueta <- paste(marcogeo_nombre_edicion(edicion), "entidades nacional")
  pais <- mapa_en_vivo(edicion$year, "entidades", version = version_de(edicion))
  revisar_mapa(etiqueta, pais, "ccl")
  expect_identical(nrow(pais$mapa), 32L, info = etiqueta)
  expect_identical(names(pais$mapa), names(colima), info = etiqueta)
}

test_that("EN VIVO: descarga por capa, entidades de Colima en cada edicion y de todo el pais en dos", {
  en_vivo()
  skip_if_not_installed("sf")
  ediciones <- ediciones_via("B")
  for (i in seq_along(ediciones)) {
    edicion <- ediciones[[i]]
    etiqueta <- paste(marcogeo_nombre_edicion(edicion), "entidades Colima")
    colima <- mapa_en_vivo(edicion$year, "entidades", estado = "Colima", version = version_de(edicion))
    revisar_mapa(etiqueta, colima, "ccl", clave = "06")
    expect_identical(nrow(colima$mapa), 1L, info = etiqueta)
    # La primera y la ultima edicion; las demas, con la opcion marcogeo_completo
    if (i %in% c(1L, length(ediciones))) {
      revisar_entidades_pais(edicion, colima$mapa)
    }
  }
})

test_that("EN VIVO: descarga por capa, varias entidades en un solo mapa", {
  en_vivo()
  skip_if_not_installed("sf")
  dos <- mapa_en_vivo(2025, "municipios", estado = c("Colima", "Aguascalientes"))
  revisar_mapa("2025 municipios Aguascalientes y Colima", dos, "ccl")
  # 11 municipios de Aguascalientes y 10 de Colima
  expect_identical(as.vector(table(substr(dos$mapa$CVEGEO, 1, 2))), c(11L, 10L))
  expect_length(attr(dos$mapa, "fuente"), 2L)
  # Entidad sin el mapa: se sabe sin descargar
  expect_error(sig_marcogeoestadistico(2025, "territorio_insular", estado = "Aguascalientes"), "no publica el mapa")
})

test_that("EN VIVO: producto completo de 1995, con el texto en Latin-1 y el filtro por entidad", {
  en_vivo()
  skip_if_not_installed("sf")
  entidades <- mapa_en_vivo(1995, "entidades")
  revisar_mapa("1995 entidades", entidades, "ccl")
  expect_identical(nrow(entidades$mapa), 32L)
  municipios <- mapa_en_vivo(1995, "municipios")
  revisar_mapa("1995 municipios", municipios, "ccl")
  # El segundo mapa sale del producto ya descargado
  expect_match(municipios$mensajes, "Usando la copia local", all = FALSE)
  colima <- mapa_en_vivo(1995, "municipios", estado = "Colima")
  revisar_mapa("1995 municipios Colima", colima, "ccl", clave = "06")
  expect_match(colima$mensajes, "se conservan las filas de Colima", all = FALSE)
  expect_identical(nrow(colima$mapa), 10L)
})

test_that("EN VIVO: producto por entidad de febrero de 2018, con descarga parcial", {
  en_vivo()
  skip_if_not_installed("sf")
  municipios <- mapa_en_vivo(2018, "municipios", estado = "Colima", version = "febrero")
  revisar_mapa("2018 (febrero) municipios Colima", municipios, "ccl", clave = "06")
  expect_identical(nrow(municipios$mapa), 10L)
  # Se descargo la parte de Colima, no el producto de 2.4 GB
  carpeta <- file.path(getOption("importinegi.cache_dir"), "marcogeo")
  parte <- file.path(carpeta, "889463526636_06_colima.zip")
  expect_true(file.exists(parte))
  expect_false(file.exists(file.path(carpeta, "889463526636_s.zip")))
  indice <- zip_remoto_indice(marcogeo_url_producto(marcogeo_edicion(2018, "febrero")))
  expect_identical(as.numeric(file.size(parte)), indice$tamano[indice$nombre == "06_colima.zip"])
  cat("[en vivo] 2018 (febrero): parte de Colima de ", tamano_texto(file.size(parte)), "; el producto contiene: ",
      paste(indice$nombre, zip_metodo(indice$metodo), collapse = " | "), "\n", sep = "")
  expect_true(all(indice$metodo == 0))
  islas <- mapa_en_vivo(2018, "territorio_insular", estado = "Colima", version = "febrero")
  revisar_mapa("2018 (febrero) territorio_insular Colima", islas, "ccl", clave = "06")
  expect_match(islas$mensajes, "Usando la copia local de 06_colima[.]zip", all = FALSE)
})

# Las 26 ediciones completas (opcion marcogeo_completo) -------------------------------

test_that("EN VIVO: todos los mapas de los 13 productos completos, de 1995 a junio de 2017", {
  en_vivo()
  marcogeo_completo()
  skip_if_not_installed("sf")
  for (edicion in ediciones_via("A")) {
    nombre <- marcogeo_nombre_edicion(edicion)
    for (mapa in marcogeo_mapas_edicion(edicion)) {
      etiqueta <- paste(nombre, mapa)
      resultado <- mapa_en_vivo(edicion$year, mapa, version = version_de(edicion))
      revisar_mapa(etiqueta, resultado, proyeccion_documentada(edicion$year, mapa))
      if (startsWith(mapa, "zonas_")) {
        next
      }
      if (mapa == "entidades") {
        expect_identical(nrow(resultado$mapa), 32L, info = etiqueta)
      }
      # La capa tiene una columna con la clave de la entidad, y Colima, filas
      colima <- suppressMessages(marcogeo_filtrar(resultado$mapa, "06"))
      esperar_un_mapa(colima)
      expect_gt(nrow(colima), 0, label = paste(etiqueta, "filas de Colima"))
      expect_lt(nrow(colima), nrow(resultado$mapa), label = paste(etiqueta, "filas de Colima"))
      rm(resultado, colima)
      gc()
    }
    # La funcion completa con `estado`, en el mapa mas ligero
    colima <- mapa_en_vivo(edicion$year, "entidades", estado = "Colima", version = version_de(edicion))
    esperar_un_mapa(colima$mapa)
    expect_identical(nrow(colima$mapa), 1L, info = nombre)
  }
})

test_that("EN VIVO: todos los mapas de Colima en las 10 ediciones de la descarga por capa", {
  en_vivo()
  marcogeo_completo()
  skip_if_not_installed("sf")
  for (edicion in ediciones_via("B")) {
    for (mapa in marcogeo_mapas_edicion(edicion)) {
      etiqueta <- paste(marcogeo_nombre_edicion(edicion), mapa, "Colima")
      resultado <- mapa_en_vivo(edicion$year, mapa, estado = "Colima", version = version_de(edicion))
      revisar_mapa(etiqueta, resultado, proyeccion_documentada(edicion$year, mapa), clave = "06")
      if (mapa == "entidades") {
        revisar_entidades_pais(edicion, resultado$mapa)
      }
    }
  }
})

test_that("EN VIVO: los municipios de las 32 entidades se combinan en un solo mapa", {
  en_vivo()
  marcogeo_completo()
  skip_if_not_installed("sf")
  for (year in c(2017, 2026)) {
    version <- if (year == 2017) "diciembre" else NULL
    pais <- mapa_en_vivo(year, "municipios", estado = "nacional", version = version)
    revisar_mapa(paste(year, "municipios nacional"), pais, "ccl")
    entidades <- mapa_en_vivo(year, "municipios", estado = marcogeo_todas(), version = version)
    revisar_mapa(paste(year, "municipios de las 32 entidades"), entidades, "ccl")
    expect_length(attr(entidades$mapa, "fuente"), 32L)
    # Los archivos por entidad suman lo mismo que el archivo de todo el pais
    expect_identical(nrow(entidades$mapa), nrow(pais$mapa), info = year)
    expect_setequal(entidades$mapa$CVEGEO, pais$mapa$CVEGEO)
    expect_identical(names(entidades$mapa), names(pais$mapa), info = year)
    reescrita <- any(grepl("escrita de distinta manera", entidades$mensajes))
    cat("[en vivo] ", year, " municipios: ", nrow(pais$mapa), " en el archivo nacional y ", nrow(entidades$mapa),
        " en los 32 archivos por entidad; proyeccion escrita de distinta manera entre entidades: ",
        if (reescrita) "si" else "no", "\n", sep = "")
  }
})

test_that("EN VIVO: todos los mapas de Colima en los 3 productos por entidad y el zip nacional de 2022", {
  en_vivo()
  marcogeo_completo()
  skip_if_not_installed("sf")
  for (edicion in ediciones_via("P")) {
    nombre <- marcogeo_nombre_edicion(edicion)
    indice <- zip_remoto_indice(marcogeo_url_producto(edicion))
    por_entidad <- indice[grepl("^[0-9]{2}_[^/]*[.]zip$", indice$nombre), ]
    cat("[en vivo] ", nombre, ": ", nrow(indice), " archivos en el producto; por entidad: ", nrow(por_entidad),
        "; metodos: ", paste(unique(zip_metodo(indice$metodo)), collapse = ", "), "\n", sep = "")
    expect_identical(substr(por_entidad$nombre, 1, 2), marcogeo_todas(), info = nombre)
    expect_true(all(indice$metodo == 0), info = nombre)
    for (mapa in marcogeo_mapas_edicion(edicion)) {
      etiqueta <- paste(nombre, mapa, "Colima")
      resultado <- mapa_en_vivo(edicion$year, mapa, estado = "Colima", version = edicion$version)
      revisar_mapa(etiqueta, resultado, proyeccion_documentada(edicion$year, mapa), clave = "06")
    }
    # Nunca se descarga el producto completo
    carpeta <- file.path(getOption("importinegi.cache_dir"), "marcogeo")
    expect_false(file.exists(file.path(carpeta, basename(marcogeo_url_producto(edicion)))), info = nombre)
  }
  # Censo Agropecuario 2022: cuatro mapas de todo el pais en un solo zip
  for (mapa in marcogeo_mapas_nacional()) {
    etiqueta <- paste("2023 (censo agropecuario 2022)", mapa, "nacional")
    resultado <- mapa_en_vivo(2023, mapa, version = "censo agropecuario 2022")
    revisar_mapa(etiqueta, resultado, "ccl")
    if (mapa == "entidades") {
      expect_identical(nrow(resultado$mapa), 32L)
    }
    rm(resultado)
    gc()
  }
})
