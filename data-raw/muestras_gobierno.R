# Genera los archivos de muestra con los que se prueba cngmd() sin
# conexion (tests/testthat/fixtures/gobierno).
#
# Uso, desde la carpeta raiz del paquete, en Linux o macOS:
#
#   Rscript data-raw/muestras_gobierno.R
#
# Los datos son inventados. Imitan la forma de los archivos del Censo Nacional
# de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico
# (CNGMD), que se reviso el 22 de noviembre de 2026 en el indice de los zip del
# INEGI: un zip por tema con las tablas en conjunto_de_datos/, un catalogo por
# variable codificada en catalogos/ y el diccionario en diccionario_de_datos/.
# Cada muestra reproduce una de las formas que la funcion debe leer:
#
#   cngmd2025_ayuntamientos  Edicion 2025: una tabla por pregunta (m1s1p3),
#                            indice de tablas, diccionario por tabla, clave
#                            cvegeo y siglas NA, NSS, ND y NP.
#   cngmd2021_alcaldias      Edicion 2021: rutas con "\" como separador y
#                            clave ubicageo_c como numero. Una tabla sin clave.
#   cngmd2019_actividad      Carpeta "conjunto_de datos" (con espacio), tablas
#                            con el sufijo _cngmd2019 y sin catalogos.
#   cngmd2015_estructura     Carpeta de datos dentro de otra carpeta y
#                            diccionario que no es una tabla.
#   cngmd2011_comisiones     Edicion 2011 en CSV: cinco tablas con nombre corto.
#   cngmd2011_agua           Edicion 2011 en DBF (modulo sin datos abiertos).
#
# Las rutas con "\" se guardan asi solo si el script corre en Linux o macOS: en
# Windows, "\" separa carpetas.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
if (.Platform$OS.type == "windows") {
  stop("Este script debe ejecutarse en Linux o macOS (ver el comentario inicial).")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "gobierno")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)
source("data-raw/muestras_comun.R")

sexo <- tabla(sexo_i = c("1", "2", "9"), descrip = c("Hombre", "Mujer", "No identificado"))

# Edicion 2025 --------------------------------------------------------------------
escribir_zip(
  "cngmd2025_ayuntamientos.zip",
  tablas = list(
    "conjunto_de_datos/0_indice_cngmd2025.csv" = tabla(
      ARCHIVO = c("m1s1p1", "m1s1p3", "m1s1p12a"),
      CONTENIDO = c(
        "Reconocimiento de la figura de persona presidenta municipal en el municipio, al cierre del 2024.",
        "Personas s\u00edndicas que integraban el Ayuntamiento, seg\u00fan caracter\u00edsticas, al cierre del a\u00f1o 2024.",
        "Presupuesto ejercido por las personas integrantes del Ayuntamiento, seg\u00fan tipo de integrante, durante el a\u00f1o 2023."
      )
    ),
    "conjunto_de_datos/m1s1p1_cngmd2025.csv" = tabla(
      cvegeo = c("01001", "06002", "14039", "14120"),
      cve_ent = c("01", "06", "14", "14"),
      cve_mun = c("001", "002", "039", "120"),
      recono_i = c("1", "1", "1", "2")
    ),
    "conjunto_de_datos/m1s1p3_cngmd2025.csv" = tabla(
      cvegeo = c("01001", "06002", "06002", "14039", "14120"),
      cve_ent = c("01", "06", "06", "14", "14"),
      cve_mun = c("001", "002", "002", "039", "120"),
      numeral = c("1", "1", "2", "1", "1"),
      nombre1 = c("Mar\u00eda", "Jos\u00e9", "Luc\u00eda", "NP", "\u00c1ngel"),
      sexo_i = c("2", "1", "2", "9", "1"),
      edades = c("45", "NSS", "38", "NA", "52"),
      totalca1 = c("2", "NP", "ND", "NA", "1"),
      ingreso = c("35000.50", "NA", "28000", "0", "41250.75")
    ),
    "conjunto_de_datos/m1s1p12a_cngmd2025.csv" = tabla(
      cvegeo = c("01001", "06002", "14039", "14120"),
      cve_ent = c("01", "06", "14", "14"),
      cve_mun = c("001", "002", "039", "120"),
      tipo_int = c("1", "1", "2", "3"),
      presupue = c("1234567.50", "NA", "0", "250000")
    ),
    "catalogos/sexo_i.csv" = sexo,
    "catalogos/recono_i.csv" = tabla(recono_i = c("1", "2"), descrip = c("S\u00ed", "No")),
    "catalogos/cvegeo.csv" = tabla(
      cvegeo = c("1001", "6002", "14039", "14120"),
      descrip = c("Aguascalientes", "Colima", "Guadalajara", "Zapopan")
    ),
    "diccionario_de_datos/diccionario_de_datos_m1s1p1_cngmd2025.csv" = tabla(
      COLUMNA = c("cvegeo", "recono_i"),
      DESCRIPCION = c("Clave geoestad\u00edstica del municipio", "Reconocimiento de la figura"),
      ORIGEN = c("", "recono_i"), TIPO_DATO = c("Texto", "Entero"), LONGITUD = c("5", "1"),
      CODIGO_VALIDO = c("", "1, 2")
    ),
    "diccionario_de_datos/diccionario_de_datos_m1s1p3_cngmd2025.csv" = tabla(
      COLUMNA = c("cvegeo", "sexo_i", "edades"),
      DESCRIPCION = c("Clave geoestad\u00edstica del municipio", "Sexo de la persona", "Edad en a\u00f1os"),
      ORIGEN = c("", "sexo_i", ""), TIPO_DATO = c("Texto", "Entero", "Entero"), LONGITUD = c("5", "1", "3"),
      CODIGO_VALIDO = c("", "1, 2, 9", "18-98, NSS, NA")
    )
  ),
  otros = list(
    "diccionario_de_datos/readme.txt" = c("NSS: No cuenta con elementos para responder", "NA: No aplica",
                                          "ND: Informacion no disponible", "NP: No publicable"),
    "metadatos/metadatos_cngmd_2025.txt" = "Censo Nacional de Gobiernos Municipales 2025 (muestra inventada)",
    "readme.txt" = "Descripcion de las carpetas del paquete de datos abiertos."
  )
)

# Edicion 2021: rutas con "\" -----------------------------------------------------------
escribir_zip(
  "cngmd2021_alcaldias.zip",
  tablas = list(
    "conjunto_de_datos\\0_indice_cngmd2021.csv" = tabla(
      ARCHIVO = c("m1s2p1", "m1s2p9"),
      CONTENIDO = c("Personas titulares de las alcald\u00edas, seg\u00fan sexo, al cierre del a\u00f1o 2020.",
                    "Temas de las sesiones de los concejos, durante el a\u00f1o 2020.")
    ),
    "conjunto_de_datos\\m1s2p1_cngmd2021.csv" = tabla(
      ubicageo_c = c("9002", "9003", "9017"),
      sexo_i = c("2", "1", "2"),
      total = c("1", "NA", "3")
    ),
    "conjunto_de_datos\\m1s2p9_cngmd2021.csv" = tabla(
      tema_i = c("1", "2"),
      sesiones = c("12", "7")
    ),
    "catalogos\\sexo_i.csv" = sexo
  ),
  otros = list(
    "diccionario_de_datos\\readme.txt" = "NSS: No cuenta con elementos para responder",
    "metadatos\\metadatos_cngmd_2021.txt" = "Muestra inventada"
  )
)

# Edicion 2019: carpeta con espacio y sin catalogos -------------------------------------
escribir_zip(
  "cngmd2019_actividad.zip",
  tablas = list(
    "conjunto_de datos/cabildos_cngmd2019.csv" = tabla(
      ubic_geo = c("01001", "06002"),
      sesiones = c("24", "18")
    ),
    "conjunto_de datos/iniciati_cngmd2019.csv" = tabla(
      ubic_geo = c("01001", "06002"),
      iniciati = c("5", "0")
    )
  ),
  otros = list("metadatos/metadatos_cngmd_2019.txt" = "Muestra inventada")
)

# Edicion 2015: carpeta de datos dentro de otra --------------------------------------------
escribir_zip(
  "cngmd2015_estructura.zip",
  tablas = list(
    "Estruct_aytto_cngmd2015_dbf/conjunto_de_datos/ayuntami.csv" = tabla(
      ubic_geo = c("01001", "06002", "14039"),
      entidad = c("1", "6", "14"),
      sindicos = c("2", "1", "3"),
      regidor = c("14", "11", "19")
    ),
    "Estruct_aytto_cngmd2015_dbf/catalogos/entidad.csv" = tabla(
      entidad = c("1", "6", "14"),
      descrip = c("Aguascalientes", "Colima", "Jalisco")
    ),
    "Estruct_aytto_cngmd2015_dbf/diccionario_de_datos/diccionario_datos_estructura_cngmd2015.csv" = tabla(
      V1 = c("Diccionario de datos", "Tabla: ayuntami"),
      V2 = c("", "S\u00edndicos y regidores del ayuntamiento")
    )
  )
)

# Edicion 2011 en CSV --------------------------------------------------------------------------
comisiones <- function(columna) {
  datos <- tabla(ubic_geo = c("01001", "06002", "14039"), valor = c("3", "5", "8"))
  names(datos)[2] <- columna
  datos
}
escribir_zip(
  "cngmd2011_comisiones.zip",
  tablas = list(
    "conjunto_de_datos/act_ayun.csv" = comisiones("ses_cab"),
    "conjunto_de_datos/com_ayun.csv" = comisiones("comision"),
    "conjunto_de_datos/com_tasu.csv" = comisiones("tema_asu"),
    "conjunto_de_datos/ini_ayun.csv" = comisiones("iniciati"),
    "conjunto_de_datos/in_teasu.csv" = comisiones("tema_asu"),
    "catalogos/ubi_geo.csv" = tabla(
      ubic_geo = c("1001", "6002", "14039"),
      descrip = c("Aguascalientes", "Colima", "Guadalajara")
    )
  ),
  otros = list("metadatos/metadatos_cngmd_2011.txt" = "Muestra inventada")
)

# Edicion 2011 en DBF --------------------------------------------------------------------------
escribir_zip(
  "cngmd2011_agua.zip",
  tablas = list(
    "Agua potable y saneamiento/Base de datos/AGUA_RED.DBF" = data.frame(
      UBIC_GEO = c(1001L, 6002L, 14039L),
      TOMAS = c(250000L, 48000L, 510000L),
      PRESTADOR = c("Organismo operador", "Municipio", "Organismo operador"),
      stringsAsFactors = FALSE
    ),
    "Agua potable y saneamiento/Base de datos/PLANTAS.DBF" = data.frame(
      UBIC_GEO = c(1001L, 14039L),
      PLANTAS = c(2L, 5L),
      stringsAsFactors = FALSE
    ),
    "Agua potable y saneamiento/Catalogos/CAT_PREST.DBF" = data.frame(
      CLAVE = c(1L, 2L),
      DESCRIP = c("Organismo operador", "Municipio"),
      stringsAsFactors = FALSE
    )
  ),
  otros = list("Agua potable y saneamiento/Descripcion_Archivos/descripcion.txt" = "Muestra inventada")
)

message("Muestras escritas en ", destino, ": ", paste(list.files(destino), collapse = ", "))
