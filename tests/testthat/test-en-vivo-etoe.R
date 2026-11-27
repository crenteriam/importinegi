# Pruebas en vivo de etoe(): descargan archivos reales del INEGI. Solo corren
# con IMPORTINEGI_EN_VIVO=true (ver .github/workflows/pruebas-en-vivo.yaml).
#
# Leen los seis zip de la Encuesta Telefonica de Ocupacion y Empleo (abril,
# mayo y junio de 2020, en las dos versiones de los ponderadores; 15 MB en
# total) y comprueban, con la funcion del paquete, lo que se leyo de esos
# archivos el 27 de noviembre de 2026 desde un navegador, sin el paquete: las
# filas y las columnas de las 30 tablas, la llave, la correspondencia entre
# niveles, los hogares sin entrevista, los registros de los DBF, las cifras que
# dan los ponderadores y la tabla integrada. Tambien comprueban lo que afirma
# la ayuda de etoe() sobre los datos.

meses_etoe <- function() {
  c("abril", "mayo", "junio")
}

# Lo que se leyo el 27 de noviembre de 2026. Las dos versiones de un mes
# tienen las mismas filas. Las cifras se calculan con el ponderador nacional de
# la tabla sociodemografica y los criterios del INEGI: entrevista completa,
# residentes habituales y de 15 a 98 años.
inventario_etoe <- function() {
  cifras <- function(...) {
    stats::setNames(c(...), c("poblacion", "activa", "ocupada", "desocupada"))
  }
  list(
    abril = list(
      filas = c(coe1 = 23893L, coe2 = 23893L, hog = 14294L, sdem = 29704L, viv = 14185L),
      sin_entrevista = 5074L, sin_residentes = 5045L, suspendida_con_residentes = 29L,
      conapo = cifras(95628637, 45417952, 43294807, 2123145),
      cpv2020 = cifras(95087510, 44862021, 42759589, 2102432)
    ),
    mayo = list(
      filas = c(coe1 = 22106L, coe2 = 22106L, hog = 13970L, sdem = 27604L, viv = 13884L),
      sin_entrevista = 5416L, sin_residentes = 5392L, suspendida_con_residentes = 24L,
      conapo = cifras(95866594, 45485898, 43578792, 1907106),
      cpv2020 = cifras(95316727, 45225050, 43328874, 1896176)
    ),
    junio = list(
      filas = c(coe1 = 29097L, coe2 = 29097L, hog = 17229L, sdem = 36321L, viv = 17094L),
      sin_entrevista = 6279L, sin_residentes = 6253L, suspendida_con_residentes = 26L,
      conapo = cifras(96282311, 51136700, 48328185, 2808515),
      cpv2020 = cifras(95741519, 50849474, 48056740, 2792734)
    )
  )
}

# Columnas de cada tabla: las de la ENOE de 2019-T4, mas los dos ponderadores
# de ciudades; en junio, mas NUPM y, en la version del Censo 2020, mas TIPO.
columnas_etoe <- function(mes, poblacion) {
  enoe_2019_4 <- c(coe1 = 169L, coe2 = 70L, hog = 31L, sdem = 104L, viv = 20L)
  enoe_2019_4 + 2L + if (mes == "junio") 1L + (poblacion == "cpv2020") else 0L
}

ponderadores_etoe <- function(poblacion) {
  paste0(c("FAC", "FAC32", "FAC39"), if (poblacion == "cpv2020") "_NP" else "")
}

# Poblacion de 15 años y mas, economicamente activa, ocupada y desocupada.
cifras_etoe <- function(sdem, ponderador) {
  edad <- suppressWarnings(as.integer(sdem$EDA))
  validos <- sdem$R_DEF %in% "00" & sdem$C_RES %in% c("1", "3") & !is.na(edad) & edad >= 15 & edad <= 98
  peso <- as.numeric(sdem[[ponderador]])
  c(
    poblacion = sum(peso[validos]),
    activa = sum(peso[validos & sdem$CLASE1 %in% 1]),
    ocupada = sum(peso[validos & sdem$CLASE2 %in% 1]),
    desocupada = sum(peso[validos & sdem$CLASE2 %in% 2])
  )
}

# Las cinco tablas de un mes, o NULL (con un fallo de la prueba) si no se
# pudieron descargar.
leer_etoe <- function(mes, poblacion) {
  etiqueta <- paste(mes, poblacion)
  tablas <- tryCatch(suppressMessages(etoe(2020, mes, poblacion = poblacion)), error = function(e) e)
  expect_false(inherits(tablas, "error"), info = paste(etiqueta, if (inherits(tablas, "error")) conditionMessage(tablas)))
  if (inherits(tablas, "error")) {
    cat("[en vivo] ETOE ", etiqueta, ": ERROR ", conditionMessage(tablas), "\n", sep = "")
    return(NULL)
  }
  tablas
}

# Comprueba la forma de las cinco tablas de un mes y escribe un resumen.
revisar_mes_etoe <- function(mes, poblacion, tablas) {
  etiqueta <- paste("ETOE", mes, poblacion)
  esperado <- inventario_etoe()[[mes]]
  llaves <- attr(tablas, "llave")
  expect_named(tablas, enoe_tablas())
  expect_identical(vapply(tablas, nrow, integer(1)), esperado$filas, info = paste(etiqueta, "filas"))
  expect_identical(vapply(tablas, ncol, integer(1)), columnas_etoe(mes, poblacion), info = paste(etiqueta, "columnas"))
  expect_null(attr(tablas, "nota"))

  # Columnas propias de la ETOE y tipos que declara el INEGI
  ponderadores <- ponderadores_etoe(poblacion)
  for (nombre in names(tablas)) {
    datos <- tablas[[nombre]]
    donde <- paste(etiqueta, nombre)
    expect_identical(names(datos), toupper(names(datos)), info = donde)
    expect_identical(anyDuplicated(names(datos)), 0L, info = donde)
    expect_identical(names(datos)[enoe_es_ponderador(names(datos))], ponderadores, info = paste(donde, "ponderadores"))
    for (ponderador in ponderadores) {
      expect_true(is.numeric(datos[[ponderador]]) && !anyNA(datos[[ponderador]]) && all(datos[[ponderador]] >= 0),
                  info = paste(donde, ponderador))
    }
    expect_identical("NUPM" %in% names(datos), mes == "junio", info = paste(donde, "NUPM"))
    expect_identical("TIPO" %in% names(datos), mes == "junio" && poblacion == "cpv2020", info = paste(donde, "TIPO"))
    # Ninguna columna indica el mes
    expect_identical(unique(datos$PER), "220", info = paste(donde, "PER"))
    expect_false(any(c("MES", "MES_CAL") %in% names(datos)), info = donde)
    # La llave es la de la ENOE hasta 2020-T1 e identifica cada fila
    expect_identical(llaves[[nombre]], enoe_llave(names(datos), enoe_nivel()[[nombre]], adicionales = character()),
                     info = paste(donde, "llave"))
    expect_true(all(llaves[[nombre]] %in% names(datos)), info = paste(donde, "columnas de la llave"))
    expect_identical(anyDuplicated(enoe_clave(datos, llaves[[nombre]])), 0L, info = paste(donde, "llave repetida"))
  }
  expect_identical(llaves$viv, c("CD_A", "ENT", "CON", "V_SEL"), info = etiqueta)
  viv <- tablas$viv
  # EST_D es numero en abril y mayo, y texto en junio; T_LOC esta en blanco
  expect_identical(is.character(viv$EST_D), mes == "junio", info = paste(etiqueta, "EST_D"))
  expect_true(all(is.na(viv$T_LOC)), info = paste(etiqueta, "T_LOC"))
  if ("TIPO" %in% names(viv)) {
    expect_setequal(unique(viv$TIPO), c("1", "2"))
    cat("[en vivo] ", etiqueta, " viviendas por TIPO: ", paste(names(table(viv$TIPO)), table(viv$TIPO), sep = "=", collapse = " "),
        "\n", sep = "")
  }
  sdem <- tablas$sdem
  expect_true(all(grepl("^[0-9]{2}$", sdem$ENT)), info = paste(etiqueta, "clave de la entidad"))
  expect_identical(length(unique(sdem$ENT)), 32L, info = etiqueta)
  expect_true(all(grepl("^[0-9]{2}$", sdem$R_DEF)), info = paste(etiqueta, "R_DEF"))
  expect_type(sdem$EDA, "character")
  # El cuestionario de ocupacion y empleo se aplica desde los 12 años
  expect_identical(min(suppressWarnings(as.integer(tablas$coe1$EDA)), na.rm = TRUE), 12L, info = paste(etiqueta, "edad en coe1"))

  # Cada fila tiene su fila en la tabla del nivel superior
  clave <- function(nombre, nivel = nombre) enoe_clave(tablas[[nombre]], llaves[[nivel]])
  personas <- clave("sdem")
  expect_true(all(clave("coe1") %in% personas), info = paste(etiqueta, "coe1 sin residente"))
  expect_true(setequal(clave("coe1"), clave("coe2")), info = paste(etiqueta, "coe1 y coe2 con personas distintas"))
  hogares <- clave("hog")
  hogar_del_residente <- match(clave("sdem", "hog"), hogares)
  expect_false(anyNA(hogar_del_residente), info = paste(etiqueta, "residentes sin hogar"))
  viviendas <- clave("viv")
  expect_true(all(clave("hog", "viv") %in% viviendas), info = paste(etiqueta, "hogares sin vivienda"))
  expect_true(all(viviendas %in% clave("hog", "viv")), info = paste(etiqueta, "viviendas sin hogar"))

  # Hogares sin entrevista y hogares sin residentes. Como en la ENOE, los
  # hogares con entrevista suspendida (R_DEF "15") si tienen residentes.
  entrevistado <- tablas$hog$R_DEF %in% "00"
  con_residentes <- seq_along(hogares) %in% hogar_del_residente
  expect_identical(sum(!entrevistado), esperado$sin_entrevista, info = paste(etiqueta, "hogares sin entrevista"))
  expect_identical(sum(!con_residentes), esperado$sin_residentes, info = paste(etiqueta, "hogares sin residentes"))
  expect_identical(sum(entrevistado & !con_residentes), 0L, info = paste(etiqueta, "hogares con entrevista y sin residentes"))
  expect_identical(sum(!entrevistado & con_residentes), esperado$suspendida_con_residentes, info = etiqueta)
  expect_identical(unique(tablas$hog$R_DEF[!entrevistado & con_residentes]), "15", info = etiqueta)

  # Ponderadores. El de cada residente es igual al de su hogar; los de
  # ciudades valen cero fuera de las 32 y de las 39 ciudades.
  ciudades32 <- sprintf("%02d", c(1:5, 7:10, 12:19, 21, 24:29, 31:33, 36, 39:41, 43))
  ciudades39 <- c(ciudades32, "06", "20", "30", "42", "44", "46", "52")
  for (ponderador in ponderadores) {
    expect_identical(sdem[[ponderador]], tablas$hog[[ponderador]][hogar_del_residente],
                     info = paste(etiqueta, ponderador, "del residente y de su hogar"))
  }
  expect_setequal(unique(sdem$CD_A[sdem[[ponderadores[2]]] > 0]), ciudades32)
  expect_setequal(unique(sdem$CD_A[sdem[[ponderadores[3]]] > 0]), ciudades39)
  cifras <- cifras_etoe(sdem, ponderadores[1])
  expect_identical(cifras, esperado[[poblacion]], info = paste(etiqueta, "cifras"))

  por_entidad <- table(viv$ENT)
  cat("[en vivo] ", etiqueta, ": filas ", paste(names(tablas), vapply(tablas, nrow, integer(1)), sep = "=", collapse = " "),
      "; columnas ", paste(names(tablas), vapply(tablas, ncol, integer(1)), sep = "=", collapse = " "),
      "; hogares sin entrevista: ", sum(!entrevistado), "; sin residentes: ", sum(!con_residentes),
      "; R_DEF de hogares: ", paste(names(table(tablas$hog$R_DEF)), table(tablas$hog$R_DEF), sep = "=", collapse = " "),
      "; viviendas por entidad: de ", min(por_entidad), " (", names(por_entidad)[which.min(por_entidad)], ") a ",
      max(por_entidad), " (", names(por_entidad)[which.max(por_entidad)], ")",
      "; poblacion de 15 y mas = ", cifras[["poblacion"]], "; economicamente activa = ", cifras[["activa"]],
      "; ocupada = ", cifras[["ocupada"]], "; desocupada = ", cifras[["desocupada"]],
      "; tasa de participacion = ", round(100 * cifras[["activa"]] / cifras[["poblacion"]], 2),
      " %; tasa de desocupacion = ", round(100 * cifras[["desocupada"]] / cifras[["activa"]], 2), " %\n", sep = "")
  # La ayuda dice que en algunas entidades la muestra es de unas decenas de viviendas
  expect_lt(min(por_entidad), 100)
  invisible(cifras)
}

# Registros que declara cada DBF, registros que contiene y registros borrados.
revisar_archivos_etoe <- function(mes, poblacion, tablas) {
  etiqueta <- paste("ETOE", mes, poblacion)
  zip <- ruta_cache(etoe_elegir(2020, mes, poblacion)$url)
  contenido <- enoe_contenido(zip, "ETOE")
  expect_setequal(names(contenido$tablas), enoe_tablas())
  expect_identical(contenido$notas, character(), info = paste(etiqueta, "notas"))
  for (nombre in names(contenido$tablas)) {
    carpeta <- inegi_descomprimir(zip, anidados = FALSE, archivos = contenido$tablas[[nombre]])
    registros <- revisar_dbf(buscar_archivos(carpeta, extension = "dbf")[1])
    unlink(carpeta, recursive = TRUE)
    expect_identical(registros$leidos, registros$declarados, info = paste(etiqueta, nombre, "registros"))
    expect_identical(registros$borrados, 0L, info = paste(etiqueta, nombre, "registros borrados"))
    expect_identical(as.numeric(nrow(tablas[[nombre]])), registros$declarados, info = paste(etiqueta, nombre))
    expect_identical(registros$con_decimales, character(), info = paste(etiqueta, nombre, "decimales sin declarar"))
  }
  cat("[en vivo] ", etiqueta, " archivos: ", paste(contenido$archivos, collapse = " | "), "\n", sep = "")
}

# Integra las tablas ya leidas y comprueba el resultado.
revisar_integracion_etoe <- function(mes, poblacion, tablas) {
  etiqueta <- paste("ETOE", mes, poblacion)
  esperado <- inventario_etoe()[[mes]]
  llaves <- attr(tablas, "llave")
  avisos <- testthat::capture_warnings(
    integrada <- suppressMessages(enoe_integrar(tablas, adicionales = character()))
  )
  expect_identical(avisos, character(), info = paste(etiqueta, "avisos de la integracion"))
  expect_identical(nrow(integrada), nrow(tablas$sdem) + esperado$sin_residentes, info = etiqueta)
  expect_identical(sum(is.na(integrada$N_REN)), esperado$sin_residentes, info = etiqueta)
  expect_identical(anyDuplicated(names(integrada)), 0L, info = etiqueta)
  expect_identical(anyDuplicated(enoe_clave(integrada, llaves$sdem)), 0L, info = paste(etiqueta, "llave"))
  expect_true(all(c("P1_VIV", "P2_VIV", "P3_VIV", "P4_1_HOG", "P4_2_HOG", "P1", "P3", "P4_1", "P4_2") %in% names(integrada)),
              info = paste(etiqueta, "preguntas con sufijo"))
  # Nueve ponderadores: los tres del residente, con su nombre y NA en los
  # hogares sin residentes; los del hogar y los de la vivienda, con el sufijo
  # de su tabla y el valor de la fila que corresponde
  ponderadores <- ponderadores_etoe(poblacion)
  expect_identical(grep("^FAC", names(integrada), value = TRUE),
                   c(ponderadores, paste0(ponderadores, "_HOG"), paste0(ponderadores, "_VIV")), info = etiqueta)
  residentes <- seq_len(nrow(tablas$sdem))
  fila_hogar <- match(enoe_clave(integrada, llaves$hog), enoe_clave(tablas$hog, llaves$hog))
  fila_vivienda <- match(enoe_clave(integrada, llaves$viv), enoe_clave(tablas$viv, llaves$viv))
  for (ponderador in ponderadores) {
    expect_identical(integrada[[ponderador]][residentes], tablas$sdem[[ponderador]], info = paste(etiqueta, ponderador))
    expect_true(all(is.na(integrada[[ponderador]][-residentes])), info = paste(etiqueta, ponderador, "sin residente"))
    expect_identical(integrada[[paste0(ponderador, "_HOG")]], tablas$hog[[ponderador]][fila_hogar],
                     info = paste(etiqueta, ponderador, "del hogar"))
    expect_identical(integrada[[paste0(ponderador, "_VIV")]], tablas$viv[[ponderador]][fila_vivienda],
                     info = paste(etiqueta, ponderador, "de la vivienda"))
  }
  # Las columnas propias de junio quedan una sola vez
  expect_identical(sum(names(integrada) %in% c("NUPM", "TIPO")), sum(c("NUPM", "TIPO") %in% names(tablas$sdem)), info = etiqueta)
  expect_identical(sum(!is.na(integrada$P1)), sum(!is.na(tablas$coe1$P1)), info = paste(etiqueta, "P1 del cuestionario"))
  expect_identical(sum(is.na(integrada$R_DEF)), 0L, info = paste(etiqueta, "R_DEF"))
  # Los ponderadores del hogar y los de su vivienda solo difieren en hogares
  # que no se entrevistaron
  vivienda_del_hogar <- match(enoe_clave(tablas$hog, llaves$viv), enoe_clave(tablas$viv, llaves$viv))
  distintos <- vapply(ponderadores, function(ponderador) {
    distinto <- tablas$hog[[ponderador]] != tablas$viv[[ponderador]][vivienda_del_hogar]
    expect_identical(sum(distinto & tablas$hog$R_DEF %in% "00"), 0L,
                     info = paste(etiqueta, ponderador, "del hogar y de su vivienda, con entrevista"))
    sum(distinto)
  }, integer(1))
  cat("[en vivo] ", etiqueta, " integrada: ", nrow(integrada), " filas x ", ncol(integrada), " columnas; ",
      esperado$sin_residentes, " hogares sin residentes; hogares con ponderador distinto del de su vivienda: ",
      paste(names(distintos), distintos, sep = "=", collapse = " "), "\n", sep = "")
  invisible(integrada)
}

test_that("EN VIVO: etoe() lee e integra los tres meses en las dos versiones de los ponderadores", {
  en_vivo()
  for (mes in meses_etoe()) {
    versiones <- list()
    for (poblacion in names(etoe_versiones())) {
      tablas <- leer_etoe(mes, poblacion)
      if (is.null(tablas)) {
        next
      }
      revisar_mes_etoe(mes, poblacion, tablas)
      revisar_archivos_etoe(mes, poblacion, tablas)
      revisar_integracion_etoe(mes, poblacion, tablas)
      versiones[[poblacion]] <- tablas
    }
    # Las dos versiones de un mes tienen las mismas filas, en el mismo orden, y
    # las mismas columnas, salvo los ponderadores y, en junio, TIPO
    if (length(versiones) == 2) {
      for (nombre in enoe_tablas()) {
        cpv <- versiones$cpv2020[[nombre]]
        conapo <- versiones$conapo[[nombre]]
        llave <- attr(versiones$cpv2020, "llave")[[nombre]]
        expect_identical(enoe_clave(cpv, llave), enoe_clave(conapo, llave), info = paste("ETOE", mes, nombre, "filas de las dos versiones"))
        expect_identical(setdiff(sub("_NP$", "", names(cpv)), "TIPO"), names(conapo), info = paste("ETOE", mes, nombre))
        comunes <- setdiff(intersect(names(cpv), names(conapo)), "EST_D")
        distintas <- comunes[!vapply(comunes, function(columna) identical(cpv[[columna]], conapo[[columna]]), logical(1))]
        cat("[en vivo] ETOE ", mes, " ", nombre, ": columnas comunes que difieren entre las dos versiones: ",
            if (length(distintas) > 0) paste(distintas, collapse = ", ") else "ninguna", "\n", sep = "")
      }
    }
    rm(versiones)
    invisible(gc())
  }
})

test_that("EN VIVO: los ponderadores de la ETOE reproducen las cifras publicadas en 2020", {
  en_vivo()
  # Con las proyecciones del CONAPO, las cifras de los boletines del INEGI de
  # 2020: en abril, una poblacion economicamente activa de 45.4 millones y una
  # tasa de desocupacion de 4.7 %; en mayo, 95.9 millones de personas de 15
  # años y mas, 45.5 millones economicamente activas, 43.6 millones ocupadas y
  # 1.9 millones desocupadas; en junio, 51.1 millones economicamente activas y
  # tasas de participacion de 53.1 % y de desocupacion de 5.5 %.
  publicado <- list(
    abril = c(activa = 45.4, desocupada = 2.1, participacion = 47.5, desocupacion = 4.7),
    mayo = c(poblacion = 95.9, activa = 45.5, ocupada = 43.6, desocupada = 1.9, participacion = 47.4, desocupacion = 4.2),
    junio = c(activa = 51.1, participacion = 53.1, desocupacion = 5.5)
  )
  for (mes in names(publicado)) {
    sdem <- suppressMessages(etoe(2020, mes, tabla = "sdem", poblacion = "conapo"))
    expect_identical(grep("^FAC", names(sdem), value = TRUE), c("FAC", "FAC32", "FAC39"))
    cifras <- cifras_etoe(sdem, "FAC")
    calculado <- c(
      cifras / 1e6,
      participacion = 100 * cifras[["activa"]] / cifras[["poblacion"]],
      desocupacion = 100 * cifras[["desocupada"]] / cifras[["activa"]]
    )
    # Las cifras publicadas tienen un decimal
    for (cifra in names(publicado[[mes]])) {
      expect_lt(abs(calculado[[cifra]] - publicado[[mes]][[cifra]]), 0.051, label = paste("ETOE", mes, cifra))
    }
    cat("[en vivo] ETOE ", mes, " conapo: ", paste(names(calculado), round(calculado, 2), sep = " = ", collapse = "; "), "\n", sep = "")
  }
})

test_that("EN VIVO: la muestra de la ETOE es un panel y etoe() filtra por entidad", {
  en_vivo()
  tablas_viv <- lapply(meses_etoe(), function(mes) suppressMessages(etoe(2020, mes, tabla = "viv")))
  viviendas <- lapply(tablas_viv, function(viv) enoe_clave(viv, attr(viv, "llave")))
  todas <- unique(unlist(viviendas))
  en_los_tres <- Reduce(intersect, viviendas)
  cat("[en vivo] ETOE viviendas: ", paste(meses_etoe(), lengths(viviendas), sep = "=", collapse = " "), "; distintas: ",
      length(todas), "; en los tres meses: ", length(en_los_tres), "; de mayo en abril: ",
      sum(viviendas[[2]] %in% viviendas[[1]]), "\n", sep = "")
  # Leido el 27 de noviembre de 2026: 17,460 viviendas distintas y 13,818 en los
  # tres meses (cuatro de cada cinco, como dice la ayuda)
  expect_identical(length(todas), 17460L)
  expect_identical(length(en_los_tres), 13818L)
  expect_true(all(viviendas[[2]] %in% viviendas[[1]]))
  # En junio, TIPO distingue las entrevistas telefonicas ("2"), que son las de
  # las viviendas que ya estaban en la muestra de mayo, de las entrevistas en la
  # vivienda que se incorporaron ese mes ("1"). Leido el 27 de noviembre de
  # 2026: 13,817 y 3,277 viviendas; una sola de las segundas estaba en mayo.
  tipo <- tablas_viv[[3]]$TIPO
  en_mayo <- viviendas[[3]] %in% viviendas[[2]]
  cat("[en vivo] ETOE junio, viviendas por TIPO: ",
      paste0("TIPO ", sort(unique(tipo)), ": ", vapply(sort(unique(tipo)), function(valor) {
        paste0(sum(tipo == valor), " (", sum(en_mayo[tipo == valor]), " estaban en mayo)")
      }, character(1)), collapse = "; "), "\n", sep = "")
  expect_identical(sum(tipo == "2"), 13817L)
  expect_true(all(en_mayo[tipo == "2"]))
  expect_identical(sum(tipo == "1"), 3277L)
  expect_identical(sum(en_mayo[tipo == "1"]), 1L)

  # `estado` conserva las filas de las entidades y avisa del alcance de la encuesta
  tablas <- suppressMessages(etoe(2020, "junio", tabla = c("sdem", "hog")))
  avisos <- testthat::capture_warnings(
    mensajes <- testthat::capture_messages(
      integrada <- etoe(2020, "junio", integrar = TRUE, estado = c("Colima", "Zacatecas"))
    )
  )
  expect_identical(avisos, character())
  expect_true(any(grepl("La ETOE tiene alcance nacional", mensajes)))
  expect_true(any(grepl("La tabla integrada tiene", mensajes)))
  expect_identical(sort(unique(integrada$ENT)), c("06", "32"))
  expect_identical(sum(!is.na(integrada$N_REN)), sum(tablas$sdem$ENT %in% c("06", "32")))
  llave_hogar <- attr(tablas, "llave")$hog
  hogares <- tablas$hog[tablas$hog$ENT %in% c("06", "32"), ]
  sin_residentes <- !enoe_clave(hogares, llave_hogar) %in% enoe_clave(tablas$sdem, llave_hogar)
  expect_identical(sum(is.na(integrada$N_REN)), sum(sin_residentes))
  expect_identical(attr(integrada, "fuente"), etoe_elegir(2020, "junio", "cpv2020")$url)
  cat("[en vivo] ETOE junio, Colima y Zacatecas: ", trimws(paste(grep("La tabla integrada", mensajes, value = TRUE), collapse = " ")),
      "\n", sep = "")
})
