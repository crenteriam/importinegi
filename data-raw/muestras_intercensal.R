# Genera los archivos de muestra con los que se prueba cpv_intercensal() sin conexion
# (tests/testthat/fixtures/intercensal).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_intercensal.R
#
# Requiere el paquete openxlsx (solo para escribir los XLSX).
#
# Los datos son inventados. Imitan la forma de los archivos del INEGI de las
# Encuestas Intercensales 2015 y 2025, como la registraron las pruebas en vivo
# del 22 de noviembre de 2026 (tests/testthat/test-en-vivo-intercensal.R):
#
# - Muestra 2015: un zip por entidad (eic2015_NN_csv.zip) con TR_PERSONANN.CSV
#   y TR_VIVIENDANN.CSV. Claves ENT y MUN, con sus nombres; LOC50K vale "0000"
#   en las localidades de menos de 50 mil habitantes. Los identificadores no
#   llevan el cero inicial de la entidad.
# - Muestra 2025: un zip por entidad y uno nacional (eic2025_micro_NN_csv.zip,
#   NN = 00 para el nacional) con personasNN.csv, viviendasNN.csv y
#   migrantesNN.csv. Claves CVE_ENT y CVE_MUN, sin nombres, y CVEGEO de cinco
#   digitos (entidad y municipio). Las muestras traen ademas un diccionario,
#   que los zip reales no tienen: sirve para probar que la funcion elige la
#   tabla y no otro archivo con un nombre parecido.
# - Resultados 2025: un XLSX nacional (eic2025_micro_105.xlsx) con una sola hoja
#   (eic2025_micro_105), el encabezado en la primera fila y las claves como
#   texto. En lugar de un dato puede haber "MI" (no disponible por muestra
#   insuficiente) o "NA" (no aplica). En NOM_MUN, un asterisco marca los
#   municipios censados y dos, los de muestra insuficiente.
# - con_titulo.xlsx y sin_claves.xlsx no imitan ningun archivo real: prueban
#   que la lectura de XLSX encuentra la tabla si el INEGI le agrega titulo y
#   notas, y que avisa si no la encuentra.
# - con_nulos.xlsx tampoco imita un archivo real: prueba que el texto "NULL" de
#   una celda se convierte en NA.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "intercensal")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)

source(file.path("data-raw", "muestras_comun.R"), encoding = "UTF-8")

# Las personas se reparten en viviendas de dos: 1, 1, 2, 2, 3...
numero_vivienda <- function(n) seq_len(n) %/% 3 + 1L
personas_por_vivienda <- function(personas, viviendas) {
  as.character(as.vector(table(personas$ID_VIV)[viviendas$ID_VIV]))
}

# Muestra 2015 --------------------------------------------------------------------

personas_2015 <- function(ent, nombre, mun, nom_mun, loc50k, nom_loc, edad, factor) {
  n <- length(edad)
  vivienda <- numero_vivienda(n)
  id_viv <- paste0(as.integer(ent), mun, sprintf("%07d", vivienda))
  tabla(
    ID_VIV = id_viv, ID_PERSONA = paste0(id_viv, sprintf("%02d", seq_len(n))),
    ENT = ent, NOM_ENT = nombre, MUN = mun, NOM_MUN = nom_mun,
    LOC50K = loc50k, NOM_LOC = nom_loc,
    COBERTURA = "2", ESTRATO = paste0(ent, "-", mun, "-03"), UPM = sprintf("%06d", 119L + vivienda),
    FACTOR = factor, NUMPER = sprintf("%02d", seq_len(n)), SEXO = rep(c("1", "3"), length.out = n),
    EDAD = edad, INGTRMEN = c("008500", "000000", rep("", n - 2)), TAMLOC = "4"
  )
}
viviendas_2015 <- function(personas) {
  unicas <- personas[!duplicated(personas$ID_VIV), ]
  tabla(
    ID_VIV = unicas$ID_VIV, ENT = unicas$ENT, NOM_ENT = unicas$NOM_ENT, MUN = unicas$MUN,
    NOM_MUN = unicas$NOM_MUN, LOC50K = unicas$LOC50K, NOM_LOC = unicas$NOM_LOC,
    COBERTURA = unicas$COBERTURA, ESTRATO = unicas$ESTRATO, UPM = unicas$UPM, FACTOR = unicas$FACTOR,
    CLAVIVP = "1", PISOS = "3", NUMPERS = personas_por_vivienda(personas, unicas),
    TAMLOC = unicas$TAMLOC, INGTRHOG = sprintf("%06d", 12000L * seq_len(nrow(unicas)))
  )
}
menor_50k <- "Localidad de menos de 50 mil habitantes"
aguascalientes_2015 <- personas_2015(
  "01", "Aguascalientes", "001", "Aguascalientes", c("0001", "0001", "0000"),
  c("Aguascalientes", "Aguascalientes", menor_50k),
  edad = c("034", "008", "000"), factor = c("15", "15", "22")
)
colima_2015 <- personas_2015(
  "06", "Colima", "010", "Villa de Álvarez", c("0001", "0001"),
  c("Ciudad de Villa de Álvarez", "Ciudad de Villa de Álvarez"),
  edad = c("041", "039"), factor = c("31", "31")
)
escribir_zip("muestra2015_01.zip", list(
  "TR_PERSONA01.CSV" = aguascalientes_2015,
  "TR_VIVIENDA01.CSV" = viviendas_2015(aguascalientes_2015)
))
escribir_zip("muestra2015_06.zip", list(
  "TR_PERSONA06.CSV" = colima_2015,
  "TR_VIVIENDA06.CSV" = viviendas_2015(colima_2015)
))

# Muestra 2025 --------------------------------------------------------------------

personas_2025 <- function(ent, mun, loc50k, edad, factor) {
  n <- length(edad)
  vivienda <- numero_vivienda(n)
  id_viv <- paste0(ent, mun, sprintf("%07d", vivienda))
  tabla(
    CVEGEO = paste0(ent, mun), CVE_ENT = ent, CVE_MUN = mun, LOC50K = loc50k,
    ID_VIV = id_viv, ID_PERSONA = paste0(id_viv, sprintf("%05d", seq_len(n))),
    TIPO_REG = "0", COBERTURA = "2", ESTRATO = paste0(ent, "-", mun, "-0000-030"),
    UPM = sprintf("%05d", 119L + vivienda), FACTOR = factor, CLAVIVP = "1",
    NUMPER = sprintf("%02d", seq_len(n)), SEXO = rep(c("1", "3"), length.out = n),
    EDAD = edad, PARENTESCO = "101", INGTRMEN = c("008500", "000000", rep("", n - 2)), TAMLOC = "4"
  )
}
identificacion_2025 <- c("CVEGEO", "CVE_ENT", "CVE_MUN", "LOC50K", "ID_VIV")
viviendas_2025 <- function(personas) {
  unicas <- personas[!duplicated(personas$ID_VIV), ]
  cbind(
    unicas[c(identificacion_2025, "TIPO_REG", "COBERTURA", "ESTRATO", "UPM", "FACTOR", "CLAVIVP")],
    tabla(PISOS = rep("3", nrow(unicas)), NUMPERS = personas_por_vivienda(personas, unicas),
          INGTRHOG = sprintf("%06d", 12000L * seq_len(nrow(unicas))), TAMLOC = unicas$TAMLOC)
  )
}
# Un migrante internacional de la primera vivienda
migrantes_2025 <- function(personas) {
  primera <- personas[1, ]
  cbind(
    primera[identificacion_2025],
    tabla(ID_MII = paste0(primera$ID_VIV, "01")),
    primera[c("COBERTURA", "ESTRATO", "UPM")],
    tabla(FACTOR = "18", CLAVIVP = "1", MPER = "1", MSEXO = "1", MEDAD = "027", MFECEMIA = "2021",
          MPAIDES_C = "221", TAMLOC = primera$TAMLOC)
  )
}
aguascalientes_2025 <- personas_2025("01", "001", c("0001", "0001", "0000"),
                                     edad = c("034", "008", "000"), factor = c("12", "12", "17"))
colima_2025 <- personas_2025("06", "010", c("0001", "0001"), edad = c("041", "039"), factor = c("26", "26"))
diccionario <- tabla(NOMBRE_CAMPO = c("CVE_ENT", "EDAD"), DESCRIPCION = c("Entidad", "Edad"))
muestra_2025 <- function(nombre, clave, personas, migrantes) {
  tablas <- list(personas, viviendas_2025(personas), migrantes)
  names(tablas) <- paste0(c("personas", "viviendas", "migrantes"), clave, ".csv")
  tablas[[paste0("diccionario_de_datos_personas", clave, ".csv")]] <- diccionario
  escribir_zip(nombre, tablas)
}
migrantes_ags <- migrantes_2025(aguascalientes_2025)
migrantes_col <- migrantes_2025(colima_2025)
muestra_2025("muestra2025_01.zip", "01", aguascalientes_2025, migrantes_ags)
muestra_2025("muestra2025_06.zip", "06", colima_2025, migrantes_col)
muestra_2025("muestra2025_00.zip", "00", rbind(aguascalientes_2025, colima_2025),
             rbind(migrantes_ags, migrantes_col))

# Resultados por localidad de 50 000 y mas habitantes (2025) -----------------------
#
# Una fila por dominio y tipo de estimacion. Dominios: el pais; Aguascalientes
# con dos municipios (uno con una localidad de 50 000 y mas habitantes) y el
# agregado de sus localidades menores (997/9997); Colima con dos municipios,
# cada uno con una localidad, y su agregado. Las poblaciones del valor estimado
# suman: localidades + agregado = entidad; entidades = pais. Los nombres de los
# totales son los del archivo real. Jesus Maria figura como municipio de
# muestra insuficiente (**) y Villa de Alvarez, como municipio censado (*).

total_ent <- "Total de la entidad"
total_mun <- "Total del municipio"
menores <- "Localidades de menos de 50 000 habitantes"
dominios <- tabla(
  CVE_ENT = c("00", rep("01", 5), rep("06", 6)),
  NOM_ENT = c("Estados Unidos Mexicanos", rep("Aguascalientes", 5), rep("Colima", 6)),
  CVE_MUN = c("000", "000", "001", "001", "005", "997", "000", "002", "002", "010", "010", "997"),
  NOM_MUN = c("Total nacional", total_ent, "Aguascalientes", "Aguascalientes", "Jesús María**", menores,
              total_ent, "Colima", "Colima", "Villa de Álvarez*", "Villa de Álvarez*", menores),
  CVE_LOC = c("0000", "0000", "0000", "0001", "0000", "9997", "0000", "0000", "0001", "0000", "0001", "9997"),
  NOM_LOC = c("Total nacional", total_ent, total_mun, "Aguascalientes", total_mun, menores,
              total_ent, total_mun, "Colima", total_mun, "Ciudad de Villa de Álvarez", menores),
  POBTOT = c(2000000, 1500000, 1000000, 900000, 500000, 600000, 500000, 300000, 250000, 200000, 150000, 100000)
)
estimadores <- c("Valor", "Error estándar", "Límite inferior de confianza", "Límite superior de confianza",
                 "Coeficiente de variación")
filas <- lapply(seq_len(nrow(dominios)), function(i) {
  d <- dominios[i, ]
  ee <- round(d$POBTOT * 0.004, 1)
  tabla(
    CVEGEO = paste0(d$CVE_ENT, d$CVE_MUN, d$CVE_LOC), CVE_ENT = d$CVE_ENT, NOM_ENT = d$NOM_ENT,
    CVE_MUN = d$CVE_MUN, NOM_MUN = d$NOM_MUN, CVE_LOC = d$CVE_LOC, NOM_LOC = d$NOM_LOC, ESTIMADOR = estimadores,
    POBTOT = c(d$POBTOT, ee, d$POBTOT - 1.645 * ee, d$POBTOT + 1.645 * ee, round(100 * ee / d$POBTOT, 2)),
    PROM_HNV = c(1.85, 0.012, 1.83, 1.87, 0.65) + i / 100,
    PCN_POB_AFRO = c(2.4, 0.3, 1.9, 2.9, 12.5)
  )
})
resultados <- do.call(rbind, filas)
# Marcas en lugar de numeros, como en el archivo real: en el municipio de
# muestra insuficiente la mayoria de los indicadores (no la poblacion) es "MI";
# algunos indicadores no aplican ("NA") en ciertos municipios y localidades
resultados$PROM_HNV <- as.character(resultados$PROM_HNV)
resultados$PCN_POB_AFRO <- as.character(resultados$PCN_POB_AFRO)
resultados[resultados$CVEGEO == "010050000", c("PROM_HNV", "PCN_POB_AFRO")] <- "MI"
resultados$PCN_POB_AFRO[resultados$CVEGEO == "060100001"] <- "NA"

# Escribe una tabla en una hoja. Las columnas de texto que mezclan numeros con
# marcas ("MI") se escriben celda por celda, cada una con su tipo.
escribir_hoja <- function(libro, hoja, datos, fila = 1) {
  mixtas <- names(datos)[vapply(datos, function(x) {
    is.character(x) && any(!is.na(suppressWarnings(as.numeric(x)))) && any(grepl("^[A-Z/*]+$", x))
  }, logical(1))]
  fijas <- datos
  fijas[mixtas] <- NA_character_
  openxlsx::writeData(libro, hoja, fijas, startRow = fila)
  for (columna in mixtas) {
    numero <- suppressWarnings(as.numeric(datos[[columna]]))
    for (i in seq_len(nrow(datos))) {
      valor <- if (is.na(numero[i])) datos[[columna]][i] else numero[i]
      openxlsx::writeData(libro, hoja, valor, startCol = match(columna, names(datos)), startRow = fila + i,
                          colNames = FALSE)
    }
  }
}

libro <- openxlsx::createWorkbook()
openxlsx::addWorksheet(libro, "eic2025_micro_105")
escribir_hoja(libro, 1, resultados)
openxlsx::saveWorkbook(libro, file.path(destino, "resultados2025_00.xlsx"), overwrite = TRUE)

# XLSX con otra forma: titulo antes del encabezado, claves escritas como
# numeros, nota al pie y una segunda hoja sin datos. La lectura debe encontrar
# la tabla y devolver las claves como texto con ceros a la izquierda.
con_titulo <- openxlsx::createWorkbook()
openxlsx::addWorksheet(con_titulo, "Cuadro")
openxlsx::writeData(con_titulo, 1, "Principales resultados por localidad de 50 000 y más habitantes", startRow = 1)
cuadro <- tabla(
  CVEGEO = c(10010001, 60020001), CVE_ENT = c(1, 6), NOM_ENT = c("Aguascalientes", "Colima"),
  CVE_MUN = c(1, 2), CVE_LOC = c(1, 1), ESTIMADOR = "Valor", POBTOT = c(900000, 250000)
)
openxlsx::writeData(con_titulo, 1, cuadro, startRow = 3)
openxlsx::writeData(con_titulo, 1, "Fuente: datos inventados para las pruebas.", startRow = 3 + nrow(cuadro) + 2)
openxlsx::addWorksheet(con_titulo, "Notas")
openxlsx::writeData(con_titulo, 2, c("Notas", "Los limites del intervalo de confianza son al 90 por ciento."))
openxlsx::saveWorkbook(con_titulo, file.path(destino, "con_titulo.xlsx"), overwrite = TRUE)

# XLSX sin una columna con la clave de la entidad: la funcion debe avisar que no
# encontro la tabla de datos
sin_claves <- openxlsx::createWorkbook()
openxlsx::addWorksheet(sin_claves, "Hoja1")
openxlsx::writeData(sin_claves, 1, tabla(INDICADOR = c("POBTOT", "TGF"), VALOR = c(1, 2)))
openxlsx::saveWorkbook(sin_claves, file.path(destino, "sin_claves.xlsx"), overwrite = TRUE)

# XLSX con el texto "NULL" en dos celdas: la lectura las devuelve como NA
con_nulos <- openxlsx::createWorkbook()
openxlsx::addWorksheet(con_nulos, "Hoja1")
openxlsx::writeData(con_nulos, 1, tabla(
  CVE_ENT = c("01", "06", "14"), NOM_ENT = c("Aguascalientes", "NULL", "Jalisco"),
  POBTOT = c("900000", "250000", "NULL"), NOTA = c("NULL en otro texto", "nulo", "null")
))
openxlsx::saveWorkbook(con_nulos, file.path(destino, "con_nulos.xlsx"), overwrite = TRUE)

message("Muestras escritas en ", destino)
