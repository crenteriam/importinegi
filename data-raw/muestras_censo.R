# Genera los archivos de muestra con los que se prueba censo() sin conexion
# (tests/testthat/fixtures/censo).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_censo.R
#
# Los datos son inventados. Imitan la forma de los archivos del INEGI: un zip
# por entidad con una o varias tablas DBF, claves geograficas como texto,
# filas de totales con claves en ceros y asteriscos en los datos reservados.
# La forma real de cada archivo se comprueba con las pruebas en vivo
# (tests/testthat/test-en-vivo-censo.R).

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "censo")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)

tabla <- function(...) data.frame(..., stringsAsFactors = FALSE)

# Escribe un zip con las tablas indicadas. `tablas` es una lista con nombre:
# el nombre es la ruta del archivo dentro del zip (dbf o csv).
escribir_zip <- function(nombre, tablas, otros = list(), codificacion = "latin1") {
  carpeta <- tempfile("muestra_")
  dir.create(carpeta)
  for (ruta in names(tablas)) {
    archivo <- file.path(carpeta, ruta)
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    if (grepl("\\.dbf$", ruta, ignore.case = TRUE)) {
      # Los DBF guardan el texto en una pagina de codigos anterior a UTF-8
      datos <- tablas[[ruta]]
      for (columna in names(datos)) {
        if (is.character(datos[[columna]])) {
          datos[[columna]] <- iconv(datos[[columna]], from = "UTF-8", to = codificacion)
        }
      }
      temporal <- file.path(dirname(archivo), "temporal.dbf")
      foreign::write.dbf(datos, temporal)
      if (codificacion == "CP850") {
        # Byte 29 del encabezado: pagina de codigos 850
        conexion <- file(temporal, open = "r+b")
        seek(conexion, where = 29, rw = "write")
        writeBin(as.raw(0x02), conexion)
        close(conexion)
      }
      file.rename(temporal, archivo)
    } else {
      conexion <- file(archivo, open = "wb")
      writeBin(as.raw(c(0xef, 0xbb, 0xbf)), conexion)
      texto <- utils::capture.output(utils::write.csv(tablas[[ruta]], row.names = FALSE, na = ""))
      writeBin(charToRaw(enc2utf8(paste0(paste(texto, collapse = "\n"), "\n"))), conexion)
      close(conexion)
    }
  }
  for (ruta in names(otros)) {
    archivo <- file.path(carpeta, ruta)
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    writeLines(otros[[ruta]], archivo)
  }
  anterior <- setwd(carpeta)
  on.exit(setwd(anterior))
  utils::zip(file.path(destino, nombre), list.files(".", recursive = TRUE), flags = "-r9Xq")
  invisible(nombre)
}

# ITER: principales resultados por localidad ----------------------------------

iter_colima <- tabla(
  ENTIDAD = "06",
  NOM_ENT = "Colima",
  MUN     = c("000", "000", "000", "001", "001", "001", "001", "001", "001", "002", "002"),
  NOM_MUN = c(rep("Total de la entidad Colima", 3), rep("Armer\u00eda", 6), rep("Colima", 2)),
  LOC     = c("0000", "9998", "9999", "0000", "0001", "0015", "0020", "9998", "9999", "0000", "0001"),
  NOM_LOC = c("Total de la Entidad", "Localidades de una vivienda", "Localidades de dos viviendas",
              "Total del Municipio", "Ciudad de Armer\u00eda", "El Para\u00edso", "Rinc\u00f3n de L\u00f3pez",
              "Localidades de una vivienda", "Localidades de dos viviendas",
              "Total del Municipio", "Colima"),
  LONGITUD = c(rep(NA, 4), "1035752", "1035800", "1035910", NA, NA, NA, "1034330"),
  ALTITUD  = c(rep(NA, 4), "0020", "0015", "0040", NA, NA, NA, "0490"),
  POBTOT  = c("650", "3", "7", "250", "240", "3", "7", "3", "7", "400", "400"),
  POBMAS  = c("320", "2", "4", "125", "119", "*", "*", "2", "4", "195", "195"),
  PROM_OCUP = c("3.61", "3.00", "3.50", "3.47", "3.48", "*", "*", "3.00", "3.50", "3.70", "3.70"),
  VIVTOT  = c("180", "1", "2", "72", "69", "1", "2", "1", "2", "108", "108")
)
iter_nayarit <- tabla(
  ENTIDAD = "18", NOM_ENT = "Nayarit",
  MUN = c("000", "001", "001"),
  NOM_MUN = c("Total de la entidad Nayarit", "Acaponeta", "Acaponeta"),
  LOC = c("0000", "0000", "0001"),
  NOM_LOC = c("Total de la Entidad", "Total del Municipio", "Acaponeta"),
  LONGITUD = c(NA, NA, "1052137"), ALTITUD = c(NA, NA, "0030"),
  POBTOT = c("100", "100", "100"), POBMAS = c("49", "49", "49"),
  PROM_OCUP = c("4.00", "4.00", "4.00"), VIVTOT = c("25", "25", "25")
)
iter_nacional_totales <- tabla(
  ENTIDAD = "00", NOM_ENT = "Total nacional", MUN = "000", NOM_MUN = "Total nacional",
  LOC = c("0000", "9998", "9999"),
  NOM_LOC = c("Total nacional", "Localidades de una vivienda", "Localidades de dos viviendas"),
  LONGITUD = NA, ALTITUD = NA,
  POBTOT = c("750", "3", "7"), POBMAS = c("369", "2", "4"),
  PROM_OCUP = c("3.66", "3.00", "3.50"), VIVTOT = c("205", "1", "2")
)
escribir_zip("iter_06.zip", list("ITER_06DBF10.dbf" = iter_colima))
escribir_zip("iter_18.zip", list("ITER_18DBF10.dbf" = iter_nayarit))
escribir_zip("iter_00.zip", list("ITER_NALDBF10.dbf" = rbind(iter_nacional_totales, iter_colima, iter_nayarit)))
# Un zip con dos tablas donde se espera una sola (Aguascalientes)
escribir_zip("iter_01.zip", list("ITER_01DBF10.dbf" = iter_nayarit, "ITER_01DBF10_copia.dbf" = iter_nayarit))

# AGEB y manzana urbana ---------------------------------------------------------

ageb_colima <- tabla(
  ENTIDAD = "06", NOM_ENT = "Colima",
  MUN  = c("000", "002", "002", "002", "002", "002", "002", "002"),
  NOM_MUN = c("Total de la entidad Colima", rep("Colima", 7)),
  LOC  = c("0000", "0000", "0001", "0001", "0001", "0001", "0001", "0001"),
  NOM_LOC = c("Total de la entidad", "Total del municipio", "Total de la localidad urbana",
              "Total AGEB urbana", "Colima", "Colima", "Total AGEB urbana", "Colima"),
  AGEB = c("0000", "0000", "0000", "0012", "0012", "0012", "003A", "003A"),
  MZA  = c("000", "000", "000", "000", "001", "002", "000", "001"),
  POBTOT = c("640", "400", "400", "250", "150", "100", "150", "150"),
  POBMAS = c("315", "195", "195", "120", "72", "*", "75", "75"),
  VIVTOT = c("176", "108", "108", "68", "40", "28", "40", "40")
)
ageb_nayarit <- tabla(
  ENTIDAD = "18", NOM_ENT = "Nayarit",
  MUN = c("000", "001", "001", "001", "001"),
  NOM_MUN = c("Total de la entidad Nayarit", rep("Acaponeta", 4)),
  LOC = c("0000", "0000", "0001", "0001", "0001"),
  NOM_LOC = c("Total de la entidad", "Total del municipio", "Total de la localidad urbana",
              "Total AGEB urbana", "Acaponeta"),
  AGEB = c("0000", "0000", "0000", "0105", "0105"),
  MZA = c("000", "000", "000", "000", "007"),
  POBTOT = c("100", "100", "100", "100", "100"), POBMAS = c("49", "49", "49", "49", "49"),
  VIVTOT = c("25", "25", "25", "25", "25")
)
escribir_zip("ageb_06.zip", list("RESAGEBURB_06DBF10.dbf" = ageb_colima))
escribir_zip("ageb_18.zip", list("RESAGEBURB_18DBF10.dbf" = ageb_nayarit))

# Muestra (cuestionario ampliado) ---------------------------------------------

personas <- function(ent, n) {
  tabla(ENT = ent, MUN = sprintf("%03d", seq_len(n)), ID_VIV = sprintf("%s0000000%02d", ent, seq_len(n)),
        NUMPER = seq_len(n), SEXO = c("1", "3", "1")[seq_len(n)], EDAD = c(34L, 8L, 61L)[seq_len(n)],
        ESCOLARI = c("09", "02", "00")[seq_len(n)], FACTOR = c(12L, 12L, 9L)[seq_len(n)])
}
viviendas <- function(ent, n) {
  tabla(ENT = ent, MUN = sprintf("%03d", seq_len(n)), ID_VIV = sprintf("%s0000000%02d", ent, seq_len(n)),
        PISOS = c("3", "2")[seq_len(n)], NUMPERS = c(4L, 2L)[seq_len(n)], FACTOR = c(12L, 9L)[seq_len(n)])
}
migrantes <- function(ent) {
  tabla(ENT = ent, MUN = "001", ID_VIV = paste0(ent, "000000001"), MPAIS = "221", FACTOR = 12L)
}
escribir_zip("muestra2010_06.zip", list(
  "Personas_06.dbf" = personas("06", 3), "Viviendas_06.dbf" = viviendas("06", 2),
  "Migrantes_06.dbf" = migrantes("06")
))
escribir_zip("muestra2010_18.zip", list(
  "Personas_18.dbf" = personas("18", 2), "Viviendas_18.dbf" = viviendas("18", 1),
  "Migrantes_18.dbf" = migrantes("18")
))
# 2000: otros nombres de archivo y extension en mayusculas
escribir_zip("muestra2000_06.zip", list(
  "PER_F06.DBF" = personas("06", 3), "VHO_F06.DBF" = viviendas("06", 2), "MIN_F06.DBF" = migrantes("06")
))
# 1990: una sola tabla, con las claves como numero
escribir_zip("muestra1990_06.zip", list(
  "CGPV90P_06.DBF" = tabla(ENT = 6L, MUN = c(1L, 1L, 10L), NUMPER = 1:3, SEXO = c("1", "2", "1"),
                           EDAD = c(40L, 12L, 77L))
))

# Localidades con menos de 5 mil habitantes ------------------------------------

localidades_colima <- tabla(
  ENTIDAD = "06", NOM_ENT = "Colima", MUN = c("001", "001"), LOC = c("0015", "0020"),
  NOM_LOC = c("El Para\u00edso", "Rinc\u00f3n de L\u00f3pez"),
  POBTOT = c("3", "7"), CARRETERA = c("1", "2"), DIST_CAB = c("12", "N/D"), TIENDA = c("2", "*")
)
localidades_nayarit <- tabla(
  ENTIDAD = "18", NOM_ENT = "Nayarit", MUN = "001", LOC = "0044", NOM_LOC = "El Resbal\u00f3n",
  POBTOT = "85", CARRETERA = "1", DIST_CAB = "8", TIENDA = "1"
)
# El archivo de Colima declara la pagina de codigos 850 en su encabezado
escribir_zip("localidades_06.zip", list("RESLOC_06_2010.dbf" = localidades_colima), codificacion = "CP850")
escribir_zip("localidades_00.zip", list("RESLOC_NAL_2010.dbf" = rbind(localidades_colima, localidades_nayarit)))

# Entorno urbano (por manzana) --------------------------------------------------

escribir_zip("entorno_06.zip", list("MANZANA_EU_06.dbf" = tabla(
  ENT = "06", MUN = "002", LOC = "0001", AGEB = c("0012", "0012", "003A"), MZA = c("001", "002", "001"),
  CONJHAB = c("3", "3", "1"), RECUCALL_ = c("1", "2", "5"), ALUMPUB_ = c("1", "3", "5")
)))

# Zip de "datos abiertos" en CSV ------------------------------------------------
#
# Forma supuesta de los archivos del Censo 2020 (carpetas conjunto_de_datos,
# diccionario_de_datos, catalogos y metadatos), segun la descripcion de otros
# usuarios. Se confirmara cuando el registro incluya el Censo 2020.
iter_csv <- iter_colima
escribir_zip(
  "iter2020_06.zip",
  list(
    "iter_06_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_06CSV20.csv" = iter_csv,
    "iter_06_cpv2020/diccionario_de_datos/diccionario_datos_iter_06CSV20.csv" =
      tabla(MNEMONICO = c("ENTIDAD", "POBTOT"), DESCRIPCION = c("Clave de entidad", "Poblaci\u00f3n total")),
    "iter_06_cpv2020/catalogos/tam_loc.csv" = tabla(TAM_LOC = c("1", "2"), DESCRIP = c("1-249", "250-499"))
  ),
  otros = list("iter_06_cpv2020/metadatos/metadatos_iter_06_cpv2020.txt" = "Metadatos de muestra")
)

# Entorno urbano 2020: una tabla por manzana y otra por vialidad
escribir_zip("entorno2020_06.zip", list(
  "TI_MANZANA_EU_06.csv" = tabla(ENT = "06", MUN = "002", LOC = "0001", AGEB = "0012",
                                 MZA = c("001", "002"), RECUCALL_ = c("1", "2")),
  "TR_VIALIDAD_EU_06.csv" = tabla(ENT = "06", MUN = "002", LOC = "0001", AGEB = "0012",
                                  MZA = c("001", "001", "002"), CVEVIAL = c("00012", "00013", "00099"),
                                  TIPOVIAL = c("Calle", "Avenida", "Calle"), BANQUETA = c("1", "1", "2"))
))

# Alojamientos de asistencia social 2020: por entidad y nacional
asistencia_colima <- tabla(
  ENT = "06", MUN = c("002", "007"), LOC = c("0001", "0001"),
  NOM_ALOJ = c("Casa Hogar Ni\u00f1os de Colima", "Albergue San Jos\u00e9"),
  TIPO_ALOJ = c("1", "3"), RESIDENTES = c("24", "11")
)
escribir_zip("asistencia2020_06.zip", list("TR_ALOJAMIENTO_06.csv" = asistencia_colima))
escribir_zip("asistencia2020_00.zip", list("TR_ALOJAMIENTO_EUM.csv" = rbind(
  asistencia_colima,
  tabla(ENT = "18", MUN = "017", LOC = "0001", NOM_ALOJ = "Casa de Reposo",
        TIPO_ALOJ = "2", RESIDENTES = "9")
)))

# Archivo nacional: totales del pais y localidades de Colima
escribir_zip(
  "iter2020_00.zip",
  list("iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv" =
         rbind(iter_nacional_totales[1, ], iter_csv))
)

message("Muestras generadas en ", destino, ":")
print(file.info(list.files(destino, full.names = TRUE))[, "size", drop = FALSE])
