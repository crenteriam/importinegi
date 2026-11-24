# Pruebas en vivo de enigh_tradicional(): descargan del INEGI las 15 ediciones
# de la serie tradicional de la ENIGH (106 archivos, unos 210 MB) y leen sus
# 126 tablas. Solo corren con IMPORTINEGI_EN_VIVO=true.

# Hogares de cada edicion: registros que declara el encabezado del DBF del
# concentrado (revisado en el INEGI el 22 de noviembre de 2026).
hogares_enigh <- c(
  "1984" = 4735, "1989" = 11531, "1992" = 10530, "1994" = 12815, "1996" = 14042, "1998" = 10952,
  "2000" = 10108, "2002" = 17167, "2004" = 22595, "2005" = 23174, "2006" = 20875, "2008" = 29468,
  "2010" = 27655, "2012" = 9002, "2014" = 19479
)

test_that("EN VIVO: enigh_tradicional() lee las tablas de todas las ediciones", {
  en_vivo()
  ediciones <- bases_importinegi("enigh_tradicional")
  expect_identical(ediciones$year, as.integer(names(hogares_enigh)))
  borrados <- character()
  leidas <- 0

  for (i in seq_len(nrow(ediciones))) {
    year <- ediciones$year[i]
    nombres <- strsplit(ediciones$tablas[i], ", ", fixed = TRUE)[[1]]
    inicio <- Sys.time()
    tablas <- suppressMessages(enigh_tradicional(year, nombres))
    segundos <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")))
    expect_named(tablas, nombres)
    archivos <- enigh_archivos("enigh_tradicional", year)

    for (nombre in nombres) {
      tabla <- tablas[[nombre]]
      etiqueta <- paste("ENIGH tradicional", year, nombre)
      describir_tabla(etiqueta, tabla)
      leidas <- leidas + 1
      expect_s3_class(tabla, "data.frame")
      expect_identical(names(tabla), toupper(names(tabla)), info = etiqueta)
      expect_identical(anyDuplicated(names(tabla)), 0L, info = etiqueta)
      # Identificador del hogar: texto y sin faltantes
      llave <- if (year <= 2006) "FOLIO" else c("FOLIOVIV", if (nombre != "viviendas") "FOLIOHOG")
      expect_true(all(llave %in% names(tabla)), info = etiqueta)
      if (all(llave %in% names(tabla))) {
        expect_true(all(vapply(tabla[llave], is.character, logical(1))), info = etiqueta)
        expect_false(anyNA(tabla[llave]), info = etiqueta)
      }

      # El archivo tal como lo publica el INEGI, para comparar con lo leido
      fila <- archivos[archivos$tabla == nombre, ]
      zip <- suppressMessages(inegi_descargar(fila$url))
      contenido <- enigh_contenido(zip)
      extraido <- gobierno_extraer(zip, contenido[grep(fila$patron, contenido$nombre), , drop = FALSE])
      revision <- revisar_dbf(extraido$archivos[1])
      unlink(extraido$carpeta, recursive = TRUE)
      expect_identical(revision$leidos, revision$declarados, info = etiqueta)
      expect_identical(revision$ancho_registro[["campos"]], revision$ancho_registro[["declarado"]], info = etiqueta)
      # La funcion devuelve todos los registros menos los borrados
      expect_identical(as.numeric(nrow(tabla)), revision$declarados - revision$borrados, info = etiqueta)
      # Ningun campo que foreign lee como entero tiene decimales
      expect_identical(revision$con_decimales, character(), info = etiqueta)
      expect_true(all(names(revision$tipos) %in% c("C", "N", "F")), info = etiqueta)
      if (revision$borrados > 0) {
        borrados <- c(borrados, paste0(year, " ", nombre, " (", revision$borrados, ")"))
      }
      if (nombre == "concentrado") {
        expect_identical(revision$declarados, hogares_enigh[[as.character(year)]], info = etiqueta)
      }
    }

    # Una fila por hogar en el concentrado y en hogares, con el mismo identificador
    concentrado <- tablas$concentrado
    llave <- if (year <= 2006) "FOLIO" else c("FOLIOVIV", "FOLIOHOG")
    hogar <- function(tabla) do.call(paste, c(unname(as.list(tabla[llave])), sep = "-"))
    expect_identical(anyDuplicated(hogar(concentrado)), 0L, info = year)
    expect_identical(nrow(tablas$hogares), nrow(concentrado), info = year)
    expect_true(setequal(hogar(tablas$hogares), hogar(concentrado)), info = year)
    # Las personas pertenecen a hogares del concentrado
    expect_true(all(hogar(tablas$poblacion) %in% hogar(concentrado)), info = year)

    # Factor de expansion del concentrado
    factor <- if (year <= 2006) "HOG" else if (year <= 2010) "FACTOR" else "FACTOR_HOG"
    expect_true(factor %in% names(concentrado), info = year)
    if (factor %in% names(concentrado)) {
      expect_true(is.numeric(concentrado[[factor]]) && !anyNA(concentrado[[factor]]) && all(concentrado[[factor]] > 0), info = year)
    }
    diseno <- c("EST_DIS", "UPM")
    expect_identical(all(diseno %in% names(concentrado)), year >= 1992, info = year)
    cat("[en vivo] ENIGH tradicional ", year, ": ", length(nombres), " tablas en ", segundos, " s; ",
        nrow(concentrado), " hogares en la muestra; hogares expandidos: ",
        if (factor %in% names(concentrado)) format(round(sum(concentrado[[factor]])), big.mark = ",") else "sin factor",
        "; descriptor: ", attr(tablas, "descriptor"), "\n", sep = "")
    rm(tablas, concentrado)
    gc(verbose = FALSE)
  }

  expect_identical(leidas, 126)
  cat("[en vivo] ENIGH tradicional, tablas con registros borrados: ",
      if (length(borrados) > 0) paste(borrados, collapse = "; ") else "ninguna", "\n", sep = "")
})

test_that("EN VIVO: los casos revisados en el INEGI siguen iguales", {
  en_vivo()
  # Las erogaciones de 2006 declaran 18,271 registros; dos estan borrados y el
  # descriptor del INEGI documenta 18,269
  erogaciones <- suppressMessages(enigh_tradicional(2006, "erogaciones"))
  expect_identical(nrow(erogaciones), 18269L)
  # Ingresos no monetarios de 1989: 36,749 registros, dos borrados
  nomonetario <- suppressMessages(enigh_tradicional(1989, "nomonetario"))
  expect_identical(nrow(nomonetario), 36747L)

  # En 2006 el concentrado se publica en minusculas y los hogares en mayusculas:
  # se unen por FOLIO
  tablas <- suppressMessages(enigh_tradicional(2006, c("concentrado", "hogares")))
  unidas <- merge(tablas$hogares[, c("FOLIO", "FACTOR")], tablas$concentrado[, c("FOLIO", "HOG")], by = "FOLIO")
  expect_identical(nrow(unidas), 20875L)
  cat("[en vivo] ENIGH 2006: el factor de hogares y el del concentrado coinciden en ",
      sum(unidas$FACTOR == round(unidas$HOG)), " de ", nrow(unidas), " hogares\n", sep = "")

  # Viviendas de 2014: campos numericos con asteriscos, que se leen como NA
  viviendas <- suppressMessages(enigh_tradicional(2014, "viviendas"))
  expect_gt(nrow(viviendas), 19000)
  expect_true(is.numeric(viviendas$RENTA) && anyNA(viviendas$RENTA))
  expect_identical(unique(nchar(viviendas$FOLIOVIV)), 10L)
  describir_tabla("ENIGH tradicional 2014 viviendas", viviendas)

  # Gastos diarios de 2008: campos de tipo F con decimales
  diario <- suppressMessages(enigh_tradicional(2008, "gastodiario"))
  expect_type(diario$CANTIDAD, "double")
  expect_true(any(diario$CANTIDAD != round(diario$CANTIDAD), na.rm = TRUE))
  expect_true(any(diario$GAS_TRI != round(diario$GAS_TRI), na.rm = TRUE))
})
