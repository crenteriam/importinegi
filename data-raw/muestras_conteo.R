# Genera los archivos de muestra con los que se prueba conteo() sin conexion
# (tests/testthat/fixtures/conteo).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_conteo.R
#
# Los datos son inventados. Imitan la forma de los archivos del INEGI de los
# Conteos de 1995 y 2005: un zip por entidad con una o varias tablas DBF. La
# muestra de 1995 trae datgen95.dbf (personas) y migint95.dbf (migrantes); la de
# 2005 trae trpmueNN.dbf, trvmueNN.dbf y trhmueNN.dbf (personas, viviendas y
# hogares) dentro de la carpeta cpv2005_NN_dbf. La forma real de cada archivo se
# comprueba con las pruebas en vivo (tests/testthat/test-en-vivo-conteo.R).

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "conteo")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)

source(file.path("data-raw", "muestras_comun.R"), encoding = "UTF-8")

# ITER: principales resultados por localidad ----------------------------------
#
# Como en el censo: totales con claves en ceros y agregados de las localidades
# de una y de dos viviendas (9998 y 9999), que repiten poblacion ya contada.

iter_colima <- tabla(
  ENTIDAD = "06", NOM_ENT = "Colima",
  MUN     = c("000", "000", "001", "001", "001", "001", "002", "002"),
  NOM_MUN = c(rep("Total de la entidad Colima", 2), rep("Armería", 4), rep("Colima", 2)),
  LOC     = c("0000", "9998", "0000", "0001", "0010", "9998", "0000", "0001"),
  NOM_LOC = c("Total de la Entidad", "Localidades de una vivienda", "Total del Municipio",
              "Ciudad de Armería", "Cofradía de Juárez", "Localidades de una vivienda",
              "Total del Municipio", "Colima"),
  LONGITUD = c(NA, NA, NA, "1035752", "1035910", NA, NA, "1034330"),
  ALTITUD  = c(NA, NA, NA, "0020", "0050", NA, NA, "0490"),
  POBTOT  = c("1000", "4", "300", "296", "4", "4", "700", "700"),
  POBMAS  = c("490", "2", "150", "148", "*", "2", "340", "340"),
  VIVTOT  = c("250", "1", "75", "74", "1", "1", "175", "175")
)
iter_aguascalientes <- tabla(
  ENTIDAD = "01", NOM_ENT = "Aguascalientes",
  MUN = c("000", "001", "001"),
  NOM_MUN = c("Total de la entidad Aguascalientes", "Aguascalientes", "Aguascalientes"),
  LOC = c("0000", "0000", "0001"),
  NOM_LOC = c("Total de la Entidad", "Total del Municipio", "Aguascalientes"),
  LONGITUD = c(NA, NA, "1021746"), ALTITUD = c(NA, NA, "1880"),
  POBTOT = c("500", "500", "500"), POBMAS = c("240", "240", "240"), VIVTOT = c("120", "120", "120")
)
iter_nacional <- rbind(
  tabla(ENTIDAD = "00", NOM_ENT = "Total nacional", MUN = "000", NOM_MUN = "Total nacional",
        LOC = c("0000", "9998"), NOM_LOC = c("Total nacional", "Localidades de una vivienda"),
        LONGITUD = NA, ALTITUD = NA, POBTOT = c("1500", "4"), POBMAS = c("730", "2"),
        VIVTOT = c("370", "1")),
  iter_aguascalientes,
  iter_colima
)
for (year in c("95", "05")) {
  sufijo <- if (year == "95") "1995" else "2005"
  escribir_zip(paste0("iter", sufijo, "_06.zip"), stats::setNames(list(iter_colima), paste0("ITER_06DBF", year, ".dbf")))
  escribir_zip(paste0("iter", sufijo, "_00.zip"), stats::setNames(list(iter_nacional), paste0("ITER_NALDBF", year, ".dbf")))
}
escribir_zip("iter1995_01.zip", list("ITER_01DBF95.dbf" = iter_aguascalientes))

# Muestra del Conteo 1995 (Encuesta del Conteo) ----------------------------------
#
# La edad llega con ceros a la izquierda en Colima y sin ellos en
# Aguascalientes; OCUPA trae un codigo con letra en Aguascalientes, asi que
# queda como texto en las dos entidades.

personas1995 <- function(ent, edad, ocupa) {
  n <- length(edad)
  tabla(ENT = ent, MUN = "001", LOC = "0001", NUMVIV = sprintf("%05d", seq_len(n)), NUMPER = seq_len(n),
        SEXO = c("1", "2", "1")[seq_len(n)], EDAD = edad, OCUPA = ocupa, FACTOR = c(25L, 25L, 31L)[seq_len(n)])
}
migrantes1995 <- function(ent) {
  tabla(ENT = ent, MUN = "001", LOC = "0001", NUMVIV = "00001", PAIS = "221", FACTOR = 25L)
}
escribir_zip("muestra1995_06.zip", list(
  "datgen95.dbf" = personas1995("06", c("034", "008", "061"), c("0123", "0201", NA)),
  "migint95.dbf" = migrantes1995("06")
))
escribir_zip("muestra1995_01.zip", list(
  "datgen95.dbf" = personas1995("01", c("45", "12"), c("A10", "0410")),
  "migint95.dbf" = migrantes1995("01")
))

# Muestra del Conteo 2005 (extracto del 10 % de las viviendas) ---------------

muestra2005 <- function(ent, n) {
  carpeta <- paste0("cpv2005_", ent, "_dbf/")
  stats::setNames(
    list(
      tabla(ENT = ent, MUN = "002", LOC = "0001", NUMVIV = sprintf("%05d", seq_len(n)), NUMHOG = "1",
            NUMPER = seq_len(n), SEXO = c("1", "2", "2")[seq_len(n)], EDAD = c("027", "025", "003")[seq_len(n)],
            FACTOR = c("10", "10", "11")[seq_len(n)]),
      tabla(ENT = ent, MUN = "002", LOC = "0001", NUMVIV = "00001", PISOS = "2", TOTHOG = 1L, FACTOR = "10"),
      tabla(ENT = ent, MUN = "002", LOC = "0001", NUMVIV = "00001", NUMHOG = "1", TOTPER = as.integer(n), FACTOR = "10")
    ),
    paste0(carpeta, c("trpmue", "trvmue", "trhmue"), ent, ".dbf")
  )
}
escribir_zip("muestra2005_06.zip", muestra2005("06", 3))
escribir_zip("muestra2005_01.zip", muestra2005("01", 2))

# Viviendas habitadas y deshabitadas por localidad (2005) --------------------

viviendas_colima <- tabla(
  ENTIDAD = "06", NOM_ENT = "Colima", MUN = c("001", "001", "002"), LOC = c("0001", "0010", "0001"),
  NOM_LOC = c("Ciudad de Armería", "Cofradía de Juárez", "Colima"),
  VIVTOT = c("80", "2", "190"), VIVHAB = c("74", "1", "175"), VIVDES = c("6", "*", "15")
)
viviendas_aguascalientes <- tabla(
  ENTIDAD = "01", NOM_ENT = "Aguascalientes", MUN = "001", LOC = "0001", NOM_LOC = "Aguascalientes",
  VIVTOT = "130", VIVHAB = "120", VIVDES = "10"
)
escribir_zip("viviendas2005_06.zip", list("VIVLOC2005_06.dbf" = viviendas_colima))
escribir_zip("viviendas2005_00.zip", list("VIVLOC2005_NAL.dbf" = rbind(viviendas_aguascalientes, viviendas_colima)))

message("Muestras generadas en ", destino, ":")
print(file.info(list.files(destino, full.names = TRUE))[, "size", drop = FALSE])
