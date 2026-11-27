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

# Tablas de un trimestre: tablas_enoe(estructura, periodo)
source("data-raw/muestras_enoe_tablas.R")

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
