# Genera los archivos de muestra con los que se prueba enoe() sin conexion
# (tests/testthat/fixtures/enoe).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_enoe.R
#
# Los datos son inventados. Imitan la forma de los zip trimestrales de la
# Encuesta Nacional de Ocupacion y Empleo (poblacion de 15 años y mas), que se
# reviso el 22 de noviembre de 2026 en los encabezados de las tablas de los 85
# trimestres: cinco tablas en DBF (vivienda, hogar, sociodemografico y las dos
# partes del cuestionario de ocupacion y empleo), sin carpetas, y a veces una
# nota del INEGI sobre el reemplazo de los archivos. Cada muestra reproduce una
# de las formas que la funcion debe leer:
#
#   enoe_2005_1  2005-T1. Llave CD_A, ENT, CON y V_SEL; ponderador FAC. Archivos
#                sin prefijo (SDEMT105.dbf). Nota en la pagina de codigos de
#                Windows.
#   enoe_2012_2  2012-T2. Igual, con los nombres de los archivos en minusculas
#                (sdemt212.dbf). Nota en UTF-8 con marca de orden de bytes.
#   enoe_2013_1  2013-T1, incompleto: solo las tablas de vivienda y
#                sociodemografica, para probar el aviso de tablas faltantes.
#   enoe_2020_3  2020-T3 (ENOE Nueva Edicion). La llave agrega TIPO, MES_CAL y
#                CA; ponderadores FAC_TRI y FAC_MEN. Archivos con prefijo y
#                extension en mayusculas (ENOEN_SDEMT320.DBF). Nota de dos lineas.
#   enoe_2024_1  2024-T1. La llave agrega TIPO y MES_CAL. Archivos con el prefijo
#                ENOE_; la tabla de hogares tiene TIPOLEV y la sociodemografica,
#                una columna de texto con acentos y una celda "NULL". Sin nota.
#   enoe_2026_2  2026-T2. CVE_ENT en lugar de ENT, CVE_MUN en lugar de MUN y
#                CVEGEO. Sin nota.
#
# Todas tienen las mismas cinco viviendas, seis hogares y ocho residentes:
#
#   Vivienda 1 (Ciudad de Mexico): hogar 1, con tres residentes, y hogar 2, sin
#     entrevista (R_DEF distinto de "00") y sin residentes.
#   Vivienda 2 (Ciudad de Mexico): un hogar con dos residentes.
#   Vivienda 3 (Ciudad de Mexico): un hogar con un residente. Desde 2020-T3
#     tiene los mismos CD_A, ENT, CON y V_SEL que la vivienda 2 y solo se
#     distingue por TIPO.
#   Vivienda 4 (Jalisco): un hogar con dos residentes.
#   Vivienda 5 (Jalisco): un hogar sin entrevista. Desde 2020-T3 solo se
#     distingue de la vivienda 4 por MES_CAL.
#
# Seis residentes tienen 12 años o mas y estan en el cuestionario de ocupacion
# y empleo. El ponderador del hogar de la vivienda 2 es distinto en las tablas
# de vivienda (200), de hogar (201) y sociodemografica (202), y las tablas de
# vivienda y de hogar tienen preguntas que se llaman igual que otras del
# cuestionario de ocupacion (P1 y P3; P4_1 y P4_2).

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "enoe")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)
source("data-raw/muestras_comun.R")

# Tablas de un trimestre. `estructura` es "A" (2005-T1 a 2020-T1), "B" (2020-T3
# a 2021-T2), "C" (2021-T3 a 2025-T2) o "D" (2025-T3 en adelante); `periodo`,
# el trimestre y el año como los escribe el INEGI ("105" es 2005-T1).
tablas_enoe <- function(estructura, periodo) {
  nueva <- estructura != "A"
  viv <- tabla(
    MUN = c("002", "015", "015", "039", "039"),
    CD_A = c("01", "01", "01", "02", "02"),
    ENT = c("09", "09", "09", "14", "14"),
    CON = if (nueva) c("00001", "00002", "00002", "00001", "00001") else c("00001", "00002", "00003", "00001", "00004"),
    UPM = c("0900001", "0900002", "0900002", "1400001", "1400001"),
    V_SEL = c("01", "02", "02", "01", "01"),
    N_ENT = c("1", "2", "2", "5", "5"),
    PER = periodo,
    P1 = c("03", "02", "01", "02", NA),
    P2 = c("2", "1", "1", "1", NA),
    P3 = c("02", NA, NA, NA, NA),
    UR = c(1L, 1L, 1L, 2L, 2L)
  )
  if (nueva) {
    viv$TIPO <- c("1", "1", "2", "1", "1")
    viv$MES_CAL <- c("01", "02", "02", "03", "01")
  }
  if (estructura == "B") {
    viv$CA <- c("2", "2", "1", "2", "2")
  }
  if (estructura == "D") {
    viv$CVEGEO <- paste0(viv$ENT, viv$MUN, "0001")
  }
  identificacion <- setdiff(names(viv), c("P1", "P2", "P3"))

  # Hogares: los de las filas 2 y 6 no se entrevistaron
  hog <- viv[c(1, 1, 2, 3, 4, 5), identificacion]
  hog$N_HOG <- c("1", "2", "1", "1", "1", "1")
  hog$H_MUD <- "0"
  hog$R_DEF <- c("00", "03", "00", "00", "00", "15")
  hog$P4_1 <- c("2", NA, "1", "2", "2", NA)
  hog$P4_2 <- c(NA, NA, "02", NA, NA, NA)
  if (estructura %in% c("C", "D")) {
    hog$TIPOLEV <- c("1", "1", "1", "2", "1", "1")
  }

  # Residentes de los hogares entrevistados
  hogar <- c(1, 1, 1, 3, 3, 4, 5, 5)
  sdem <- hog[hogar, setdiff(names(hog), c("P4_1", "P4_2", "TIPOLEV"))]
  sdem$N_REN <- c("01", "02", "03", "01", "02", "01", "01", "02")
  sdem$C_RES <- c("1", "1", "1", "1", "3", "1", "1", "1")
  sdem$SEX <- c("1", "2", "1", "2", "2", "1", "2", "1")
  sdem$EDA <- c("40", "38", "08", "25", "13", "70", "33", "05")
  if (estructura %in% c("C", "D")) {
    # La ultima fila trae el texto "NULL", que el paquete devuelve como NA
    sdem$CS_P21_DES <- c("CIUDAD DE M\u00c9XICO", NA, NA, "NUEVO LE\u00d3N", NA, "MICHOAC\u00c1N", NA, "NULL")
  }
  sdem$CLASE1 <- c(1L, 1L, 0L, 1L, 2L, 2L, 1L, 0L)
  sdem$INGOCUP <- c(9000L, 12000L, 0L, 7500L, 0L, 0L, 15000L, 0L)

  # Cuestionario de ocupacion y empleo: residentes de 12 años o mas
  con_coe <- as.integer(sdem$EDA) >= 12
  persona <- c(identificacion, "N_HOG", "H_MUD", "N_REN")
  coe1 <- sdem[con_coe, c("R_DEF", setdiff(persona, c("MUN", "CVEGEO")), "EDA")]
  coe1$N_INF <- c("01", "01", "01", "01", "01", "01")
  coe1$P1 <- c("1", "1", "1", "2", "2", "1")
  coe1$P3 <- c("8343", "2332", "2512", NA, NA, "1321")
  coe1$P4_1 <- c("1", "1", "2", NA, NA, "1")
  coe1$P4_2 <- c("9", "9", "1", NA, NA, "9")
  coe2 <- coe1[, setdiff(names(coe1), c("R_DEF", "P1", "P3", "P4_1", "P4_2"))]
  coe2$P6B1 <- c("1", "1", "1", NA, NA, "1")
  coe2$P6B2 <- c("009000", "012000", "007500", NA, NA, "015000")

  # Ponderadores: FAC hasta 2020-T1; FAC_TRI y FAC_MEN despues
  ponderador <- list(
    viv = c(100L, 200L, 300L, 400L, 500L),
    hog = c(100L, 100L, 201L, 300L, 400L, 500L),
    sdem = c(100L, 100L, 100L, 202L, 202L, 300L, 400L, 400L)
  )
  ponderador$coe1 <- ponderador$coe2 <- ponderador$sdem[con_coe]
  tablas <- list(coe1 = coe1, coe2 = coe2, hog = hog, sdem = sdem, viv = viv)
  for (nombre in names(tablas)) {
    datos <- tablas[[nombre]]
    if (nueva) {
      datos$FAC_TRI <- ponderador[[nombre]]
      datos$FAC_MEN <- ponderador[[nombre]] * 3L
    } else {
      datos$FAC <- ponderador[[nombre]]
    }
    if (estructura == "D") {
      names(datos)[names(datos) == "ENT"] <- "CVE_ENT"
      names(datos)[names(datos) == "MUN"] <- "CVE_MUN"
    }
    rownames(datos) <- NULL
    tablas[[nombre]] <- datos
  }
  tablas
}

# Nota del INEGI, con los bytes indicados (para controlar su codificacion).
escribir_nota <- function(bytes) {
  archivo <- tempfile(fileext = ".txt")
  writeBin(bytes, archivo)
  archivo
}

# Escribe el zip de un trimestre. `archivos` da el nombre del archivo de cada
# tabla, con "%s" en el lugar del nombre de la tabla.
escribir_trimestre <- function(nombre, estructura, periodo, archivos, nota = character(), solo = NULL) {
  tablas <- tablas_enoe(estructura, periodo)
  if (!is.null(solo)) {
    tablas <- tablas[solo]
  }
  names(tablas) <- sprintf(archivos, toupper(names(tablas)))
  if (grepl("^%s", archivos) && archivos == tolower(archivos)) {
    names(tablas) <- tolower(names(tablas))
  }
  escribir_zip(nombre, tablas = tablas, binarios = nota)
}

nota_2005 <- escribir_nota(charToRaw(iconv(
  "El 30 de noviembre de 2021 se reemplazaron los archivos de la base de datos por la actualizaci\u00f3n de los ponderadores.\r\n",
  from = "UTF-8", to = "CP1252"
)))
nota_2012 <- escribir_nota(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8(
  "El 28 de agosto de 2023 se reemplazaron los archivos de la base de datos por la actualizaci\u00f3n de los ponderadores.\n"
))))
nota_2020 <- escribir_nota(charToRaw(paste0(
  "El 17 de mayo de 2021 se reemplazaron los archivos de la base de datos.\r\n\r\n",
  "El 23 de agosto de 2021 se reemplazaron de nuevo, por un ajuste en los ponderadores.\r\n"
)))

escribir_trimestre("enoe_2005_1.zip", "A", "105", "%sT105.dbf",
                   nota = c("nota_bases_datos_enoe_2005_1t.txt" = nota_2005))
escribir_trimestre("enoe_2012_2.zip", "A", "212", "%st212.dbf",
                   nota = c("nota_bases_datos_enoe_2012_2t.txt" = nota_2012))
escribir_trimestre("enoe_2013_1.zip", "A", "113", "%st113.dbf", solo = c("sdem", "viv"))
escribir_trimestre("enoe_2020_3.zip", "B", "320", "ENOEN_%sT320.DBF",
                   nota = c("nota_bases_datos_enoe_2020_3t.txt" = nota_2020))
escribir_trimestre("enoe_2024_1.zip", "C", "124", "ENOE_%sT124.dbf")
escribir_trimestre("enoe_2026_2.zip", "D", "226", "ENOE_%sT226.dbf")

unlink(c(nota_2005, nota_2012, nota_2020))
message("Muestras de la ENOE escritas en ", destino, ": ", paste(list.files(destino), collapse = ", "))
