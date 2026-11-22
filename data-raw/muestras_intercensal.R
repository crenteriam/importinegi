# Genera los archivos de muestra con los que se prueba intercensal() sin conexion
# (tests/testthat/fixtures/intercensal).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_intercensal.R
#
# Requiere el paquete openxlsx (solo para escribir el XLSX de los resultados).
#
# Los datos son inventados. Imitan la forma de los archivos del INEGI de las
# Encuestas Intercensales 2015 y 2025, segun su listado de descargas y su
# documentacion (Red Nacional de Metadatos para 2015; descriptor de los
# principales resultados por localidad para 2025):
#
# - Muestra 2015: un zip por entidad (eic2015_NN_csv.zip) con TR_PERSONA y
#   TR_VIVIENDA en CSV.
# - Muestra 2025: un zip por entidad y uno nacional (eic2025_micro_NN_csv.zip,
#   NN = 00 para el nacional) con personas, viviendas y migrantes. Los nombres
#   de los archivos dentro del zip son una suposicion (Personas01.CSV, ...): las
#   pruebas en vivo (tests/testthat/test-en-vivo-intercensal.R) registran los
#   reales. El zip trae ademas un diccionario, que la funcion debe ignorar.
# - Resultados 2025: un XLSX nacional (eic2025_micro_105.xlsx) con filas de
#   titulo antes del encabezado, una nota al pie y una segunda hoja sin datos.
#   Las claves se escriben como numeros, el caso mas dificil: la funcion debe
#   devolverlas como texto con ceros a la izquierda.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "intercensal")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)

source(file.path("data-raw", "muestras_comun.R"), encoding = "UTF-8")

# Muestra 2015 --------------------------------------------------------------------

personas_2015 <- function(ent, nombre, mun, nom_mun, loc50k, nom_loc, edad, factor) {
  n <- length(edad)
  tabla(
    ID_VIV = paste0(ent, mun, "000010", seq_len(n) %/% 3 + 1L),
    ID_PERSONA = paste0(ent, mun, "000010", seq_len(n) %/% 3 + 1L, sprintf("%02d", seq_len(n))),
    ENT = ent, NOM_ENT = nombre, MUN = mun, NOM_MUN = nom_mun,
    LOC50K = loc50k, NOM_LOC = nom_loc,
    COBERTURA = "2", ESTRATO = paste0(ent, "-1-03"), UPM = sprintf("%07d", 120 + seq_len(n) %/% 3),
    NUMPER = sprintf("%02d", seq_len(n)), SEXO = rep(c("1", "3"), length.out = n),
    EDAD = edad, INGTRMEN = c("008500", "000000", rep("", n - 2)),
    TAMLOC = "4", FACTOR = factor
  )
}
viviendas_2015 <- function(personas) {
  unicas <- personas[!duplicated(personas$ID_VIV), ]
  tabla(
    ID_VIV = unicas$ID_VIV, ENT = unicas$ENT, NOM_ENT = unicas$NOM_ENT, MUN = unicas$MUN,
    NOM_MUN = unicas$NOM_MUN, LOC50K = unicas$LOC50K, NOM_LOC = unicas$NOM_LOC,
    COBERTURA = unicas$COBERTURA, ESTRATO = unicas$ESTRATO, UPM = unicas$UPM, FACTOR = unicas$FACTOR,
    CLAVIVP = "1", PISOS = "3", NUMPERS = as.character(as.vector(table(personas$ID_VIV)[unicas$ID_VIV])),
    INGTRHOG = sprintf("%06d", 12000L * seq_len(nrow(unicas)))
  )
}
aguascalientes_2015 <- personas_2015(
  "01", "Aguascalientes", "001", "Aguascalientes", c("0001", "0001", ""), c("Aguascalientes", "Aguascalientes", ""),
  edad = c("034", "008", "000"), factor = c("15", "15", "22")
)
colima_2015 <- personas_2015(
  "06", "Colima", "010", "Villa de Álvarez", c("0001", "0001"), c("Ciudad de Villa de Álvarez", "Ciudad de Villa de Álvarez"),
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

personas_2025 <- function(datos) {
  datos$COBERTURA <- NULL
  datos$INGTRMEN <- NULL
  datos
}
migrantes_2025 <- function(personas, pais) {
  tabla(
    ID_VIV = personas$ID_VIV[1], ENT = personas$ENT[1], NOM_ENT = personas$NOM_ENT[1],
    MUN = personas$MUN[1], NOM_MUN = personas$NOM_MUN[1], ESTRATO = personas$ESTRATO[1],
    UPM = personas$UPM[1], MNUM = "1", MSEXO = "1", MEDAD = "027", MPAIS = pais, FACTOR = "18"
  )
}
base_aguascalientes_2025 <- aguascalientes_2015
base_aguascalientes_2025$FACTOR <- c("12", "12", "17")
base_colima_2025 <- colima_2015
base_colima_2025$FACTOR <- c("26", "26")
diccionario <- tabla(NOMBRE_CAMPO = c("ENT", "EDAD"), DESCRIPCION = c("Entidad", "Edad"))
muestra_2025 <- function(nombre, clave, base, migrantes) {
  viviendas <- viviendas_2015(base)
  viviendas$COBERTURA <- NULL
  viviendas$INGTRHOG <- NULL
  tablas <- list(personas_2025(base), viviendas, migrantes)
  names(tablas) <- paste0(c("Personas", "Viviendas", "Migrantes"), clave, ".CSV")
  tablas[[paste0("diccionario_de_datos_personas", clave, ".csv")]] <- diccionario
  escribir_zip(nombre, tablas)
}
migrantes_ags <- migrantes_2025(base_aguascalientes_2025, "221")
migrantes_col <- migrantes_2025(base_colima_2025, "221")
muestra_2025("muestra2025_01.zip", "01", base_aguascalientes_2025, migrantes_ags)
muestra_2025("muestra2025_06.zip", "06", base_colima_2025, migrantes_col)
muestra_2025("muestra2025_00.zip", "00", rbind(base_aguascalientes_2025, base_colima_2025),
             rbind(migrantes_ags, migrantes_col))

# Resultados por localidad de 50 000 y mas habitantes (2025) -----------------------
#
# Una fila por dominio y tipo de estimacion. Dominios: el pais; Aguascalientes
# con dos municipios (uno con una localidad de 50 000 y mas habitantes) y el
# agregado de sus localidades menores (997/9997); Colima con dos municipios,
# cada uno con una localidad, y su agregado. Las poblaciones del valor puntual
# suman: localidades + agregado = entidad; entidades = pais.

dominios <- tabla(
  CVE_ENT = c(0, 1, 1, 1, 1, 1, 6, 6, 6, 6, 6, 6),
  NOM_ENT = c("Estados Unidos Mexicanos", rep("Aguascalientes", 5), rep("Colima", 6)),
  CVE_MUN = c(0, 0, 1, 1, 5, 997, 0, 2, 2, 10, 10, 997),
  NOM_MUN = c("Estados Unidos Mexicanos", "Aguascalientes", "Aguascalientes", "Aguascalientes", "Jesús María",
              "Localidades de menos de 50 000 habitantes", "Colima", "Colima", "Colima", "Villa de Álvarez",
              "Villa de Álvarez", "Localidades de menos de 50 000 habitantes"),
  CVE_LOC = c(0, 0, 0, 1, 0, 9997, 0, 0, 1, 0, 1, 9997),
  NOM_LOC = c("Estados Unidos Mexicanos", "Aguascalientes", "Aguascalientes", "Aguascalientes", "Jesús María",
              "Localidades de menos de 50 000 habitantes", "Colima", "Colima", "Colima", "Villa de Álvarez",
              "Ciudad de Villa de Álvarez", "Localidades de menos de 50 000 habitantes"),
  POBTOT = c(2000000, 1500000, 1000000, 900000, 500000, 600000, 500000, 300000, 250000, 200000, 150000, 100000)
)
dominios$CVEGEO <- dominios$CVE_ENT * 1e7 + dominios$CVE_MUN * 1e4 + dominios$CVE_LOC
estimadores <- c("Valor puntual", "Error estándar", "Límite inferior", "Límite superior", "Coeficiente de variación")
filas <- lapply(seq_len(nrow(dominios)), function(i) {
  d <- dominios[i, ]
  ee <- round(d$POBTOT * 0.004, 1)
  tabla(
    CVEGEO = d$CVEGEO, CVE_ENT = d$CVE_ENT, NOM_ENT = d$NOM_ENT, CVE_MUN = d$CVE_MUN, NOM_MUN = d$NOM_MUN,
    CVE_LOC = d$CVE_LOC, NOM_LOC = d$NOM_LOC, ESTIMADOR = estimadores,
    POBTOT = c(d$POBTOT, ee, d$POBTOT - 1.645 * ee, d$POBTOT + 1.645 * ee, round(100 * ee / d$POBTOT, 2)),
    PROM_HNV = c(1.85, 0.012, 1.83, 1.87, 0.65) + i / 100,
    PCN_POB_AFRO = c(2.4, 0.3, 1.9, 2.9, 12.5)
  )
})
resultados <- do.call(rbind, filas)
# Un dato reservado y uno no disponible, como texto en columnas numericas
resultados$PCN_POB_AFRO <- as.character(resultados$PCN_POB_AFRO)
resultados$PCN_POB_AFRO[resultados$CVE_LOC == 1 & resultados$CVE_ENT == 6 & resultados$CVE_MUN == 2] <- "*"
resultados$PCN_POB_AFRO[resultados$CVE_MUN == 997 & resultados$CVE_ENT == 1] <- "N/D"
numero <- suppressWarnings(as.numeric(resultados$PCN_POB_AFRO))

libro <- openxlsx::createWorkbook()
openxlsx::addWorksheet(libro, "Principales resultados")
openxlsx::writeData(libro, 1, "Encuesta Intercensal 2025. Principales resultados por localidad de 50 000 y más habitantes",
                    startRow = 1)
datos <- resultados
datos$PCN_POB_AFRO <- NULL
openxlsx::writeData(libro, 1, cbind(datos, PCN_POB_AFRO = NA_character_), startRow = 3)
# Columna con numeros y textos: cada celda con su tipo
columna <- which(names(resultados) == "PCN_POB_AFRO")
for (i in seq_len(nrow(resultados))) {
  valor <- if (is.na(numero[i])) resultados$PCN_POB_AFRO[i] else numero[i]
  openxlsx::writeData(libro, 1, valor, startCol = ncol(datos) + 1, startRow = 3 + i, colNames = FALSE)
}
openxlsx::writeData(libro, 1, "Fuente: INEGI. Encuesta Intercensal 2025. Datos inventados para las pruebas.",
                    startRow = 3 + nrow(resultados) + 2)
openxlsx::addWorksheet(libro, "Notas")
openxlsx::writeData(libro, 2, c("Notas", "Los limites del intervalo de confianza son al 90 por ciento."))
openxlsx::saveWorkbook(libro, file.path(destino, "resultados2025_00.xlsx"), overwrite = TRUE)

# XLSX sin una columna con la clave de la entidad: la funcion debe avisar que no
# encontro la tabla de datos
sin_claves <- openxlsx::createWorkbook()
openxlsx::addWorksheet(sin_claves, "Hoja1")
openxlsx::writeData(sin_claves, 1, tabla(INDICADOR = c("POBTOT", "TGF"), VALOR = c(1, 2)))
openxlsx::saveWorkbook(sin_claves, file.path(destino, "sin_claves.xlsx"), overwrite = TRUE)

message("Muestras escritas en ", destino)
