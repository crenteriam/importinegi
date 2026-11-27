# Tablas inventadas con la forma de las de la Encuesta Nacional de Ocupacion y
# Empleo, para los archivos de muestra de las pruebas. Las usan
# data-raw/muestras_enoe.R y data-raw/muestras_etoe.R (la ETOE publica sus
# tablas con la forma de las de la ENOE hasta 2020-T1), que definen antes la
# funcion tabla() (data-raw/muestras_comun.R). Las viviendas, los hogares y los
# residentes de las tablas se describen en data-raw/muestras_enoe.R.

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
