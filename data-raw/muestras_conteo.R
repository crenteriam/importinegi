# Genera los archivos de muestra con los que se prueba cpv_conteo() sin conexion
# (tests/testthat/fixtures/conteo).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_conteo.R
#
# Los datos son inventados. Imitan la forma de los archivos del INEGI de los
# Conteos de 1995 y 2005: un zip por entidad con una o varias tablas DBF. La
# muestra de 1995 trae datgen95.dbf (personas) y migint95.dbf (migrantes); la de
# 2005 trae trpmueNN.DBF, trvmueNN.DBF y trhmueNN.DBF (personas, viviendas y
# hogares) dentro de la carpeta cpv2005_NN_dbf. Las columnas imitan las que
# encontraron las pruebas en vivo (tests/testthat/test-en-vivo-conteo.R) el
# 20 de noviembre de 2026; esas pruebas comprueban la forma real de cada archivo.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "conteo")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)

source(file.path("data-raw", "muestras_comun.R"), encoding = "UTF-8")

# ITER: principales resultados por localidad ----------------------------------
#
# Totales con claves en ceros y agregados de las localidades de una y de dos
# viviendas (9998 y 9999). En 2005, como en los censos, las localidades
# pequeñas aparecen una por una y los agregados repiten su poblacion; en 1995
# solo aparecen en los agregados. Las columnas de poblacion se llaman como en
# los archivos del INEGI: POBTOTAL y POBTMAS en 1995, P_TOTAL y P_MAS en 2005.

iter_colima <- tabla(
  ENTIDAD = "06", NOM_ENT = "Colima",
  MUN     = c("000", "000", "001", "001", "001", "001", "002", "002"),
  NOM_MUN = c(rep("Total de la entidad Colima", 2), rep("Armería", 4), rep("Colima", 2)),
  LOC     = c("0000", "9998", "0000", "0001", "0010", "9998", "0000", "0001"),
  NOM_LOC = c("Total de la Entidad", "Localidades de una vivienda", "Total del Municipio",
              "Ciudad de Armería", "Cofradía de Juárez", "Localidades de una vivienda",
              "Total del Municipio", "Colima"),
  LONGITUD = c(NA, NA, NA, "1035752", "1035910", NA, NA, "1034330"),
  LATITUD  = c(NA, NA, NA, "185608", "185800", NA, NA, "191444"),
  ALTITUD  = c(NA, NA, NA, "0020", "0050", NA, NA, "0490"),
  P_TOTAL = c("1000", "4", "300", "296", "4", "4", "700", "700"),
  P_MAS   = c("490", "2", "150", "148", "*", "2", "340", "340")
)
iter_aguascalientes <- tabla(
  ENTIDAD = "01", NOM_ENT = "Aguascalientes",
  MUN = c("000", "001", "001"),
  NOM_MUN = c("Total de la entidad Aguascalientes", "Aguascalientes", "Aguascalientes"),
  LOC = c("0000", "0000", "0001"),
  NOM_LOC = c("Total de la Entidad", "Total del Municipio", "Aguascalientes"),
  LONGITUD = c(NA, NA, "1021746"), LATITUD = c(NA, NA, "215247"), ALTITUD = c(NA, NA, "1880"),
  P_TOTAL = c("500", "500", "500"), P_MAS = c("240", "240", "240")
)
iter_nacional <- function(colima, aguascalientes) {
  totales <- tabla(
    ENTIDAD = "00", NOM_ENT = "Total nacional", MUN = "000", NOM_MUN = "Total nacional",
    LOC = c("0000", "9998"), NOM_LOC = c("Total nacional", "Localidades de una vivienda"),
    LONGITUD = NA, LATITUD = NA, ALTITUD = NA, P_TOTAL = c("1500", "4"), P_MAS = c("730", "2")
  )
  names(totales) <- names(colima)
  rbind(totales, aguascalientes, colima)
}
# 1995: sin la localidad de una vivienda (solo esta en los agregados) y con los
# nombres de columna de ese año
a_1995 <- function(datos) {
  names(datos)[names(datos) == "P_TOTAL"] <- "POBTOTAL"
  names(datos)[names(datos) == "P_MAS"] <- "POBTMAS"
  datos
}
colima_1995 <- a_1995(iter_colima[iter_colima$LOC != "0010", ])
aguascalientes_1995 <- a_1995(iter_aguascalientes)
escribir_zip("iter1995_06.zip", list("ITER_06DBF95.dbf" = colima_1995))
escribir_zip("iter1995_01.zip", list("ITER_01DBF95.dbf" = aguascalientes_1995))
escribir_zip("iter1995_00.zip", list("ITER_NALDBF95.dbf" = iter_nacional(colima_1995, aguascalientes_1995)))
escribir_zip("iter2005_06.zip", list("ITER_06DBF05.dbf" = iter_colima))
escribir_zip("iter2005_00.zip", list("ITER_NALDBF05.dbf" = iter_nacional(iter_colima, iter_aguascalientes)))

# Muestra del Conteo 1995 (Encuesta del Conteo) ----------------------------------
#
# Columnas como las del INEGI: diseño muestral (ZONA, ESTRATO, TAM_LOC, UPM),
# vivienda y hogar, factores de expansion (FAC_POB, FAC_VIV) y preguntas
# (P1_5, P7_10C...). P1_5 llega con ceros a la izquierda en Colima y sin ellos
# en Aguascalientes; P7_10C trae un codigo con letra en Aguascalientes, asi
# que queda como texto en las dos entidades. El zip de Colima trae ademas, por
# error, el zip de Aguascalientes, como en el INEGI el de Chiapas trae el de
# Guerrero.

personas1995 <- function(ent, edad, ocupacion) {
  n <- length(edad)
  tabla(ENT = ent, ZONA = "40", ESTRATO = "1", TAM_LOC = "4", MUN = "001", UPM = sprintf("%02d", seq_len(n)),
        VIV = seq_len(n), HOGAR = 1L, FAC_POB = c("025", "025", "031")[seq_len(n)],
        FAC_VIV = c("023", "023", "029")[seq_len(n)], P1_5 = edad, P7_10C = ocupacion)
}
migrantes1995 <- function(ent) {
  tabla(ENT = ent, ZONA = "40", ESTRATO = "1", TAM_LOC = "4", MUN = "001", UPM = "01", VIV = 1L, HOGAR = 1L,
        FAC_VIV = "023", P9_1 = "11", P9_6B = "90")
}
escribir_zip("muestra1995_01.zip", list(
  "datgen95.dbf" = personas1995("01", c("45", "12"), c("A10", "0410")),
  "migint95.dbf" = migrantes1995("01")
))
escribir_zip(
  "muestra1995_06.zip",
  list(
    "datgen95.dbf" = personas1995("06", c("034", "008", "061"), c("0123", "0201", NA)),
    "migint95.dbf" = migrantes1995("06")
  ),
  binarios = c("cpv95_01_dbf.zip" = file.path(destino, "muestra1995_01.zip"))
)

# Muestra del Conteo 2005 (extracto del 10 % de las viviendas) ---------------
#
# Tablas con extension en mayusculas dentro de la carpeta cpv2005_NN_dbf, con
# la pagina de codigos 850 declarada en el encabezado. No traen factor de
# expansion.

muestra2005 <- function(ent, n) {
  carpeta <- paste0("cpv2005_", ent, "_dbf/")
  stats::setNames(
    list(
      tabla(ENT = ent, MUN = "002", CONS_MUN = 1L, CONS_VIV = seq_len(n), CONS_HOG = 1L, CONS_PER = seq_len(n),
            PARENT = c("101", "201", "301")[seq_len(n)], SEXO = c("1", "3", "3")[seq_len(n)],
            EDAD = c("027", "025", "003")[seq_len(n)], TAM_LOC = "3"),
      tabla(ENT = ent, MUN = "002", CONS_MUN = 1L, CONS_VIV = 1L, CLAVIVPA = "1", CLAVIVCO = NA_character_,
            MAT_PISO = "2", TOPERVIV = as.integer(n)),
      tabla(ENT = ent, MUN = "002", CONS_MUN = 1L, CONS_VIV = 1L, CONS_HOG = 1L, TOTPEHOG = as.integer(n),
            TICLAHOG = "12")
    ),
    paste0(carpeta, c("trpmue", "trvmue", "trhmue"), ent, ".DBF")
  )
}
escribir_zip("muestra2005_06.zip", muestra2005("06", 3), codificacion = "CP850")
escribir_zip("muestra2005_01.zip", muestra2005("01", 2), codificacion = "CP850")

# Viviendas habitadas y deshabitadas por localidad (2005) --------------------
#
# Como el ITER: totales con claves en ceros.

viviendas_colima <- tabla(
  ENT = "06", NOM_ENT = "Colima",
  MUN = c("000", "001", "001", "001", "002", "002"),
  NOM_MUN = c("Total de la entidad Colima", rep("Armería", 3), rep("Colima", 2)),
  LOC = c("0000", "0000", "0001", "0010", "0000", "0001"),
  NOM_LOC = c("Total de la Entidad", "Total del Municipio", "Ciudad de Armería", "Cofradía de Juárez",
              "Total del Municipio", "Colima"),
  LONGITUD = c(NA, NA, "1035752", "1035910", NA, "1034330"),
  LATITUD = c(NA, NA, "185608", "185800", NA, "191444"),
  ALTITUD = c(NA, NA, "0020", "0050", NA, "0490"),
  VIV_TOT = c("272", "82", "80", "2", "190", "190"),
  VIV_CENS = c("250", "75", "74", "1", "175", "175"),
  VIV_SININF = c("2", "1", "1", "0", "1", "1"),
  VIV_DESH = c("15", "6", "5", "*", "9", "9"),
  VIV_USOT = c("5", "0", "0", "0", "5", "5")
)
viviendas_aguascalientes <- tabla(
  ENT = "01", NOM_ENT = "Aguascalientes", MUN = c("000", "001", "001"),
  NOM_MUN = c("Total de la entidad Aguascalientes", "Aguascalientes", "Aguascalientes"),
  LOC = c("0000", "0000", "0001"), NOM_LOC = c("Total de la Entidad", "Total del Municipio", "Aguascalientes"),
  LONGITUD = c(NA, NA, "1021746"), LATITUD = c(NA, NA, "215247"), ALTITUD = c(NA, NA, "1880"),
  VIV_TOT = "130", VIV_CENS = "120", VIV_SININF = "0", VIV_DESH = "10", VIV_USOT = "0"
)
viviendas_nacional <- rbind(
  tabla(ENT = "00", NOM_ENT = "Estados Unidos Mexicanos", MUN = "000", NOM_MUN = "Total nacional", LOC = "0000",
        NOM_LOC = "Total nacional", LONGITUD = NA, LATITUD = NA, ALTITUD = NA, VIV_TOT = "402", VIV_CENS = "370",
        VIV_SININF = "2", VIV_DESH = "25", VIV_USOT = "5"),
  viviendas_aguascalientes,
  viviendas_colima
)
escribir_zip("viviendas2005_06.zip", list("VIVLOC2005_06DBF.DBF" = viviendas_colima))
escribir_zip("viviendas2005_00.zip", list("VIVLOC2005_NALDBF.DBF" = viviendas_nacional))

message("Muestras generadas en ", destino, ":")
print(file.info(list.files(destino, full.names = TRUE))[, "size", drop = FALSE])
