# Pruebas en vivo de enoe(): descargan archivos reales del INEGI. Solo corren
# con IMPORTINEGI_EN_VIVO=true (ver .github/workflows/pruebas-en-vivo.yaml).
#
# La forma de los archivos se reviso el 22 de noviembre de 2026 en los
# encabezados de las tablas de los 85 trimestres, sin descargarlos completos;
# solo las tablas de vivienda y de hogar de cuatro trimestres se leyeron
# enteras. Estas pruebas leen las cinco tablas de un trimestre de cada
# estructura (cuatro descargas, 112 MB) y comprueban lo que quedo por confirmar:
# que la llave identifica cada fila tambien en las tablas de personas, que cada
# fila tiene su fila en la tabla del nivel superior, que los hogares sin
# entrevista no tienen residentes, que los DBF no declaran mas registros de los
# que contienen (y cuantos tienen marcados como borrados, que leer_dbf() omite),
# y que los ponderadores reproducen las cifras publicadas. Ademas de comprobar
# el resultado, escriben en el registro de la ejecucion lo que encontraron.
#
# La opcion enoe_serie del flujo repite las comprobaciones de forma en los 85
# trimestres (2.3 GB de descarga); no corre en los pull requests.

# Un trimestre de cada estructura; dos con cuestionario ampliado y dos con el
# basico; el primero y el ultimo de la serie.
trimestres_en_vivo <- function() {
  list(c(2005L, 1L), c(2020L, 3L), c(2024L, 1L), c(2026L, 2L))
}

# Columnas de cada tabla segun el inventario del 22 de noviembre de 2026.
columnas_inventario <- function(year, trimestre) {
  posicion <- year * 4L + trimestre
  ampliado <- enoe_cuestionario(year, trimestre) == "ampliado"
  cuantas <- if (posicion <= 2020L * 4L + 1L) {
    c(viv = 20, hog = 31, sdem = 104, coe1 = if (ampliado) 186 else 169, coe2 = if (ampliado) 125 else 70)
  } else if (posicion <= 2021L * 4L + 2L) {
    c(viv = 26, hog = 37, sdem = 110, coe1 = if (ampliado) 190 else 173, coe2 = if (ampliado) 129 else 74)
  } else if (posicion <= 2025L * 4L + 2L) {
    c(viv = 25, hog = if (year >= 2023L) 37 else 36, sdem = 114, coe1 = if (ampliado) 189 else 172,
      coe2 = if (ampliado) 137 else 82)
  } else {
    c(viv = 26, hog = 38, sdem = 115, coe1 = if (ampliado) 191 else 174, coe2 = if (ampliado) 139 else 84)
  }
  storage.mode(cuantas) <- "integer"
  cuantas
}

# Registros que declara el encabezado de un DBF, registros que caben en el
# archivo y registros marcados como borrados ("*" en su primer byte).
registros_dbf <- function(archivo) {
  bytes <- readBin(archivo, what = "raw", n = file.size(archivo))
  entero <- function(b) sum(as.numeric(b) * 256^(seq_along(b) - 1))
  declarados <- entero(bytes[5:8])
  encabezado <- entero(bytes[9:10])
  registro <- entero(bytes[11:12])
  reales <- floor((length(bytes) - encabezado) / registro)
  marcas <- bytes[encabezado + 1 + (seq_len(min(declarados, reales)) - 1) * registro]
  c(declarados = declarados, reales = reales, borrados = sum(marcas == as.raw(0x2a)))
}

# Comprueba la forma de las cinco tablas de un trimestre y escribe un resumen.
# Devuelve el numero de hogares sin residentes.
revisar_trimestre <- function(year, trimestre, tablas) {
  etiqueta <- paste0(year, "-T", trimestre)
  llaves <- attr(tablas, "llave")
  expect_named(tablas, enoe_tablas())
  columnas <- vapply(tablas, ncol, integer(1))
  esperadas <- columnas_inventario(year, trimestre)
  expect_identical(columnas[names(esperadas)], esperadas, info = paste(etiqueta, "columnas"))
  for (nombre in names(tablas)) {
    expect_identical(names(tablas[[nombre]]), toupper(names(tablas[[nombre]])), info = paste(etiqueta, nombre))
    expect_identical(anyDuplicated(names(tablas[[nombre]])), 0L, info = paste(etiqueta, nombre))
    expect_true(all(enoe_llave(names(tablas[[nombre]]), enoe_nivel()[[nombre]]) %in% names(tablas[[nombre]])),
                info = paste(etiqueta, nombre, "columnas de la llave"))
  }

  # La llave identifica cada fila de las cinco tablas
  clave <- function(nombre, nivel = nombre) enoe_clave(tablas[[nombre]], llaves[[nivel]])
  repetidas <- vapply(names(tablas), function(nombre) sum(duplicated(clave(nombre))), integer(1))
  expect_identical(unname(repetidas), rep(0L, 5), info = paste(etiqueta, "llaves repetidas:", paste(names(repetidas), repetidas, collapse = " ")))

  # Cada fila tiene su fila en la tabla del nivel superior
  personas <- clave("sdem")
  expect_true(all(clave("coe1") %in% personas), info = paste(etiqueta, "coe1 sin residente"))
  expect_true(all(clave("coe2") %in% personas), info = paste(etiqueta, "coe2 sin residente"))
  expect_true(setequal(clave("coe1"), clave("coe2")), info = paste(etiqueta, "coe1 y coe2 con personas distintas"))
  hogares <- clave("hog")
  hogares_con_residentes <- unique(clave("sdem", "hog"))
  expect_true(all(hogares_con_residentes %in% hogares), info = paste(etiqueta, "residentes sin hogar"))
  viviendas <- clave("viv")
  expect_true(all(clave("hog", "viv") %in% viviendas), info = paste(etiqueta, "hogares sin vivienda"))
  viviendas_sin_hogar <- sum(!viviendas %in% clave("hog", "viv"))

  # Los hogares sin entrevista (R_DEF distinto de "00") no tienen residentes
  entrevistado <- tablas$hog$R_DEF %in% "00"
  con_residentes <- hogares %in% hogares_con_residentes
  expect_identical(sum(!entrevistado & con_residentes), 0L, info = paste(etiqueta, "hogares sin entrevista con residentes"))

  # Tipos que declara el INEGI
  entidad <- intersect(c("ENT", "CVE_ENT"), names(tablas$sdem))
  ponderador <- intersect(c("FAC", "FAC_TRI"), names(tablas$sdem))
  expect_length(entidad, 1)
  expect_length(ponderador, 1)
  expect_true(all(grepl("^[0-9]{2}$", tablas$sdem[[entidad]])), info = paste(etiqueta, "clave de la entidad"))
  expect_identical(length(unique(tablas$sdem[[entidad]])), 32L, info = etiqueta)
  expect_true(all(grepl("^[0-9]{2}$", tablas$sdem$R_DEF)), info = paste(etiqueta, "R_DEF"))
  expect_type(tablas$sdem$EDA, "character")
  expect_true(is.numeric(tablas$sdem[[ponderador]]), info = paste(etiqueta, ponderador))

  cat("[en vivo] ", etiqueta, " (", enoe_cuestionario(year, trimestre), "): filas ",
      paste(names(tablas), vapply(tablas, nrow, integer(1)), sep = "=", collapse = " "), "; columnas ",
      paste(names(columnas), columnas, sep = "=", collapse = " "), "; llave de vivienda: ", paste(llaves$viv, collapse = "+"),
      "; hogares sin entrevista: ", sum(!entrevistado), "; hogares con entrevista y sin residentes: ",
      sum(entrevistado & !con_residentes), "; viviendas sin hogar: ", viviendas_sin_hogar,
      "; R_DEF de hogares: ", paste(names(table(tablas$hog$R_DEF)), table(tablas$hog$R_DEF), sep = "=", collapse = " "),
      "; nota: ", if (is.null(attr(tablas, "nota"))) "no" else substr(gsub("\n", " / ", attr(tablas, "nota")), 1, 200),
      "\n", sep = "")
  sum(!con_residentes)
}

# Integra las tablas ya leidas y comprueba el resultado.
revisar_integracion <- function(year, trimestre, tablas, sin_residentes) {
  etiqueta <- paste0(year, "-T", trimestre)
  avisos <- testthat::capture_warnings(integrada <- suppressMessages(enoe_integrar(tablas)))
  expect_identical(avisos, character(), info = paste(etiqueta, "avisos de la integracion"))
  expect_identical(nrow(integrada), nrow(tablas$sdem) + sin_residentes, info = etiqueta)
  expect_identical(sum(is.na(integrada$N_REN)), sin_residentes, info = etiqueta)
  expect_identical(anyDuplicated(names(integrada)), 0L, info = etiqueta)
  expect_identical(anyDuplicated(enoe_clave(integrada, attr(tablas, "llave")$sdem)), 0L, info = paste(etiqueta, "llave"))
  expect_true(all(c("P1_VIV", "P2_VIV", "P3_VIV", "P4_1_HOG", "P4_2_HOG", "P1", "P3", "P4_1", "P4_2") %in% names(integrada)),
              info = paste(etiqueta, "preguntas con sufijo"))
  residentes <- seq_len(nrow(tablas$sdem))
  ponderador <- intersect(c("FAC", "FAC_TRI"), names(integrada))
  expect_identical(integrada[[ponderador]][residentes], tablas$sdem[[ponderador]], info = paste(etiqueta, ponderador))
  expect_identical(sum(!is.na(integrada$P1)), sum(!is.na(tablas$coe1$P1)), info = paste(etiqueta, "P1 del cuestionario"))
  expect_identical(sum(is.na(integrada$R_DEF)), 0L, info = paste(etiqueta, "R_DEF"))
  sufijos <- setdiff(names(integrada), unlist(lapply(tablas, names), use.names = FALSE))
  cat("[en vivo] ", etiqueta, " integrada: ", nrow(integrada), " filas x ", ncol(integrada), " columnas; ",
      sin_residentes, " hogares sin residentes; columnas con sufijo: ", paste(sufijos, collapse = ", "),
      "; tabla de ", round(as.numeric(utils::object.size(integrada)) / 1024^2), " MB\n", sep = "")
  invisible(integrada)
}

test_that("EN VIVO: enoe() lee las cinco tablas de un trimestre de cada estructura", {
  en_vivo()
  # Filas de las tablas de vivienda y de hogar y hogares sin entrevista, leidos
  # el 22 de noviembre de 2026
  inventario <- list(
    "2020-T3" = c(viv = 160998L, hog = 161806L, sin_entrevista = 76729L),
    "2024-T1" = c(viv = 150460L, hog = 151332L, sin_entrevista = 26147L)
  )
  for (trimestre in trimestres_en_vivo()) {
    etiqueta <- paste0(trimestre[1], "-T", trimestre[2])
    tablas <- tryCatch(suppressMessages(enoe(trimestre[1], trimestre[2])), error = function(e) e)
    expect_false(inherits(tablas, "error"), info = paste(etiqueta, if (inherits(tablas, "error")) conditionMessage(tablas)))
    if (inherits(tablas, "error")) {
      cat("[en vivo] ", etiqueta, ": ERROR ", conditionMessage(tablas), "\n", sep = "")
      next
    }
    sin_residentes <- revisar_trimestre(trimestre[1], trimestre[2], tablas)
    if (!is.null(inventario[[etiqueta]])) {
      esperado <- inventario[[etiqueta]]
      expect_identical(nrow(tablas$viv), esperado[["viv"]], info = etiqueta)
      expect_identical(nrow(tablas$hog), esperado[["hog"]], info = etiqueta)
      expect_identical(sum(!tablas$hog$R_DEF %in% "00"), esperado[["sin_entrevista"]], info = etiqueta)
    }

    # Archivos DBF: registros declarados, reales y borrados
    elegido <- enoe_elegir(trimestre[1], trimestre[2])
    zip <- ruta_cache(elegido$url)
    contenido <- enoe_contenido(zip)
    for (nombre in names(contenido$tablas)) {
      carpeta <- inegi_descomprimir(zip, anidados = FALSE, archivos = contenido$tablas[[nombre]])
      registros <- registros_dbf(buscar_archivos(carpeta, extension = "dbf")[1])
      unlink(carpeta, recursive = TRUE)
      expect_identical(registros[["reales"]], registros[["declarados"]], info = paste(etiqueta, nombre, "registros"))
      # leer_dbf() omite los registros borrados: la tabla tiene solo los vigentes
      vigentes <- registros[["declarados"]] - registros[["borrados"]]
      expect_identical(nrow(tablas[[nombre]]), as.integer(vigentes), info = paste(etiqueta, nombre))
      if (registros[["reales"]] != registros[["declarados"]] || registros[["borrados"]] > 0) {
        cat("[en vivo] ", etiqueta, " ", contenido$tablas[[nombre]], ": declara ", registros[["declarados"]],
            " registros, contiene ", registros[["reales"]], ", borrados ", registros[["borrados"]], "\n", sep = "")
      }
    }
    cat("[en vivo] ", etiqueta, " archivos: ", paste(contenido$archivos, collapse = " | "), "\n", sep = "")
    describir_tabla(paste(etiqueta, "sdem"), tablas$sdem)
    revisar_texto(paste(etiqueta, "sdem"), tablas$sdem)

    revisar_integracion(trimestre[1], trimestre[2], tablas, sin_residentes)
    rm(tablas)
    invisible(gc())
  }
})

test_that("EN VIVO: los ponderadores de 2024-T1 reproducen la poblacion economicamente activa", {
  en_vivo()
  # El INEGI publico para el primer trimestre de 2024 (boletin ENOE2024_05, del
  # 27 de mayo de 2024) una poblacion de 15 años y mas de 101.1 millones de
  # personas, una poblacion economicamente activa de 60.7 millones y una
  # poblacion ocupada de 59.1 millones. Se calculan con los criterios de su
  # documentacion: entrevista completa, residentes habituales y de 15 a 98 años.
  sdem <- suppressMessages(enoe(2024, 1, tabla = "sdem"))
  edad <- suppressWarnings(as.integer(sdem$EDA))
  validos <- sdem$R_DEF %in% "00" & sdem$C_RES %in% c("1", "3") & !is.na(edad) & edad >= 15 & edad <= 98
  activa <- sum(sdem$FAC_TRI[validos & sdem$CLASE1 %in% 1])
  ocupada <- sum(sdem$FAC_TRI[validos & sdem$CLASE2 %in% 1])
  cat("[en vivo] 2024-T1: residentes = ", nrow(sdem), "; poblacion de 15 y mas = ", sum(sdem$FAC_TRI[validos]),
      "; economicamente activa = ", activa, "; ocupada = ", ocupada, "; tasa de desocupacion = ",
      round(100 * (activa - ocupada) / activa, 2), " %; tipos: EDA ", class(sdem$EDA), ", CLASE1 ", class(sdem$CLASE1),
      ", FAC_TRI ", class(sdem$FAC_TRI), "\n", sep = "")
  expect_equal(sum(sdem$FAC_TRI[validos]) / 1e6, 101.1, tolerance = 0.005)
  expect_equal(activa / 1e6, 60.7, tolerance = 0.008)
  expect_equal(ocupada / 1e6, 59.1, tolerance = 0.008)
})

test_that("EN VIVO: enoe() integra y filtra por entidad", {
  en_vivo()
  # 2020-T3 es el trimestre con mas hogares sin entrevista
  tablas <- suppressMessages(enoe(2020, 3, tabla = c("sdem", "hog")))
  avisos <- testthat::capture_warnings(
    mensajes <- testthat::capture_messages(
      integrada <- enoe(2020, 3, integrar = TRUE, estado = c("Colima", "Nayarit"))
    )
  )
  expect_identical(avisos, character())
  expect_true(any(grepl("La tabla integrada tiene", mensajes)))
  cat("[en vivo] 2020-T3 Colima y Nayarit: ", trimws(paste(grep("La tabla integrada", mensajes, value = TRUE), collapse = " ")),
      "\n", sep = "")
  expect_identical(sort(unique(integrada$ENT)), c("06", "18"))
  residentes <- sum(tablas$sdem$ENT %in% c("06", "18"))
  hogares <- tablas$hog[tablas$hog$ENT %in% c("06", "18"), ]
  expect_identical(sum(!is.na(integrada$N_REN)), residentes)
  expect_identical(sum(is.na(integrada$N_REN)), sum(!hogares$R_DEF %in% "00"))
  expect_true(all(is.na(integrada$EDA[is.na(integrada$N_REN)])))
  expect_false(any(integrada$R_DEF[is.na(integrada$N_REN)] %in% "00"))
  expect_identical(attr(integrada, "fuente"), enoe_elegir(2020, 3)$url)
  # Una sola entidad, sin integrar
  jalisco <- suppressMessages(enoe(2020, 3, tabla = "sdem", estado = "Jalisco"))
  expect_identical(unique(jalisco$ENT), "14")
  expect_identical(nrow(jalisco), sum(tablas$sdem$ENT %in% "14"))
})

# Medicion de tiempo y memoria con el trimestre de 2024-T1 (34 MB de descarga).
# Solo corre en la ejecucion manual del flujo pruebas-en-vivo.yaml con la opcion
# medir_memoria.
test_that("EN VIVO (medicion): tiempo y memoria de las cinco tablas y de la tabla integrada", {
  skip_if_not(identical(Sys.getenv("IMPORTINEGI_MEDIR_MEMORIA"), "true"),
              "La medicion solo corre con IMPORTINEGI_MEDIR_MEMORIA=true")
  en_vivo()
  medir <- function(etiqueta, ...) {
    invisible(gc(reset = TRUE))
    inicio <- Sys.time()
    resultado <- suppressMessages(enoe(2024, 1, ...))
    segundos <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
    memoria <- gc()
    pico <- sum(memoria[, max(which(colnames(memoria) == "(Mb)"))])
    filas <- if (is.data.frame(resultado)) nrow(resultado) else sum(vapply(resultado, nrow, integer(1)))
    cat("[en vivo] medicion 2024-T1 ", etiqueta, ": ", filas, " filas; ", round(segundos), " s; resultado de ",
        round(as.numeric(utils::object.size(resultado)) / 1024^2), " MB; pico de memoria de R: ", round(pico), " MB\n", sep = "")
    expect_gt(filas, 0)
    invisible(NULL)
  }
  # De menor a mayor: R no devuelve de inmediato la memoria que deja de usar, y
  # una medicion pequeña despues de una grande saldria inflada
  medir("solo sdem", tabla = "sdem")
  medir("integrada, dos entidades", integrar = TRUE, estado = c("Jalisco", "Nuevo Leon"))
  medir("cinco tablas")
  medir("integrada", integrar = TRUE)
})

# Serie completa: las comprobaciones de forma en los 85 trimestres (2.3 GB de
# descarga). Solo corre en la ejecucion manual del flujo pruebas-en-vivo.yaml
# con la opcion enoe_serie.
test_that("EN VIVO (serie): los 85 trimestres de la ENOE se leen y se integran", {
  skip_if_not(identical(Sys.getenv("IMPORTINEGI_ENOE_SERIE"), "true"),
              "La serie completa solo corre con IMPORTINEGI_ENOE_SERIE=true")
  en_vivo()
  trimestres <- enoe_trimestres()
  expect_identical(nrow(trimestres), 85L)
  for (i in seq_len(nrow(trimestres))) {
    year <- trimestres$year[i]
    trimestre <- trimestres$trimestre[i]
    etiqueta <- paste0(year, "-T", trimestre)
    tablas <- tryCatch(suppressMessages(enoe(year, trimestre)), error = function(e) e)
    unlink(ruta_cache(trimestres$url[i]))
    expect_false(inherits(tablas, "error"), info = paste(etiqueta, if (inherits(tablas, "error")) conditionMessage(tablas)))
    if (inherits(tablas, "error")) {
      cat("[en vivo] ", etiqueta, ": ERROR ", conditionMessage(tablas), "\n", sep = "")
      next
    }
    sin_residentes <- revisar_trimestre(year, trimestre, tablas)
    revisar_integracion(year, trimestre, tablas, sin_residentes)
    rm(tablas)
    invisible(gc())
  }
})
