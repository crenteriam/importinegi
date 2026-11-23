# Genera los archivos de muestra con los que se prueba cndhf() sin conexion
# (tests/testthat/fixtures/cndhf).
#
# Uso, desde la carpeta raiz del paquete, en Linux o macOS:
#
#   Rscript data-raw/muestras_cndhf.R
#
# Los datos son inventados. Imitan la forma de los archivos del Censo Nacional
# de Derechos Humanos Federal (CNDHF), que se reviso el 22 de noviembre de 2026
# en el indice de los zip del INEGI y en sus diccionarios de datos, sin
# descargar las tablas. Cada muestra reproduce una de las formas que la funcion
# debe leer:
#
#   cndhf2025_consejo        Edicion 2025: una tabla por pregunta, indice,
#                            diccionario por tabla, catalogos con el sufijo
#                            _cndhf2025 y un readme.txt en la raiz. Tablas de
#                            un total, de una lista y de una condicion.
#   cndhf2025_hechos         Edicion 2025: una tabla por entidad con catalogo
#                            (entfede_c) y otra por municipio, con la entidad y
#                            el municipio en columnas separadas (enticve1 y
#                            municve1).
#   cndhf2021_seguimiento    Edicion 2021: dos bitacoras de cambios en la
#                            carpeta de las tablas y una tabla con la clave del
#                            municipio en una columna (ubicageo_c).
#   cndhf2017_participacion  Edicion 2017: tabla con nombre corto y sin sufijo,
#                            diccionario con forma de reporte y una entrada
#                            "metadatos\" (con barra invertida) en el zip.
#   cndhf2017_capacitacion   Edicion 2017: nombres que no son UTF-8 valido
#                            (estan en la pagina de codigos de DOS, sin la
#                            marca de UTF-8) en la carpeta superior, en una
#                            tabla, en el diccionario y en un archivo que la
#                            funcion no usa; una tabla guardada con "\" como
#                            separador. Tres zip de 2017 tienen nombres asi;
#                            los de esta muestra son inventados, porque no se
#                            anoto en cuales archivos estan.
#
# La entrada con "\" se guarda asi solo si el script corre en Linux o macOS: en
# Windows, "\" separa carpetas.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
if (.Platform$OS.type == "windows") {
  stop("Este script debe ejecutarse en Linux o macOS (ver el comentario inicial).")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "cndhf")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)
source("data-raw/muestras_comun.R")

cantidad <- "0-999999999999999, NA, NSS, ND, NP, NULL"
diccionario <- function(columna, descripcion, origen, tipo, longitud, codigo) {
  tabla(COLUMNA = columna, DESCRIPCION = descripcion, ORIGEN = origen, TIPO_DATO = tipo,
        LONGITUD = longitud, CODIGO_VALIDO = codigo)
}
siglas <- c(
  "NSS: No cuenta con elementos para responder", "NA: No aplica",
  "ND: Informaci\u00f3n no disponible", "NP: No publicable"
)

# Edicion 2025, modulo 1: Consejo Consultivo ---------------------------------------
escribir_zip(
  "cndhf2025_consejo.zip",
  tablas = list(
    "conjunto_de_datos/0_indice_cndhf2025.csv" = tabla(
      ARCHIVO = c("m1s1p1", "m1s1p2", "m1s1p4"),
      CONTENIDO = c(
        "Personas consejeras del Consejo Consultivo de la Comisi\u00f3n Nacional de los Derechos Humanos, al cierre del a\u00f1o 2024.",
        "Personas consejeras del Consejo Consultivo de la Comisi\u00f3n Nacional de los Derechos Humanos, seg\u00fan caracter\u00edsticas, al cierre del a\u00f1o 2024.",
        "Sesiones celebradas por el Consejo Consultivo en las que se realizaron transmisiones en vivo, seg\u00fan tipo, durante el a\u00f1o 2024."
      )
    ),
    # Un total
    "conjunto_de_datos/m1s1p1_cndhf2025.csv" = tabla(totalca1 = "4"),
    # Una lista: una fila por persona
    "conjunto_de_datos/m1s1p2_cndhf2025.csv" = tabla(
      numeral = c("1", "2", "3", "4"),
      nombre1 = c("Mar\u00eda", "Jos\u00e9", "NP", "\u00c1ngela"),
      sexo_k = c("2", "1", "9", "2"),
      edades = c("52", "NSS", "NA", "47"),
      totalca1 = c("85000", "NP", "ND", "90500"),
      anio_a = c("2019", "2021", "NA", "2024")
    ),
    # Una condicion y su desglose
    "conjunto_de_datos/m1s1p4_cndhf2025.csv" = tabla(
      rnsnni1 = "1", seoesntt = "12", seoesn1 = "10", seoesn2 = "2", seoesn3 = "0", seoesn4 = "NSS"
    ),
    "catalogos/sexo_k_cndhf2025.csv" = tabla(
      sexo_k = c("1", "2", "9"), descrip = c("Hombre", "Mujer", "No identificado")
    ),
    "catalogos/rnsnni1_cndhf2025.csv" = tabla(
      rnsnni1 = c("0", "1", "2", "9"), descrip = c("No aplica", "S\u00ed", "No", "No identificado")
    ),
    "diccionario_de_datos/diccionario_de_datos_m1s1p1_cndhf2025.csv" = diccionario(
      "totalca1", "Personas consejeras", "SECC I. 1", "Car\u00e1cter", "20", cantidad
    ),
    "diccionario_de_datos/diccionario_de_datos_m1s1p2_cndhf2025.csv" = diccionario(
      c("numeral", "nombre1", "sexo_k", "edades", "totalca1", "anio_a"),
      c("Identificador o numeral del orden que guarda cada registro", "Descripci\u00f3n", "Sexo",
        "Edad (a\u00f1os)", "Ingresos brutos mensuales", "A\u00f1o de designaci\u00f3n"),
      rep("SECC I. 2", 6),
      c("Car\u00e1cter", "Car\u00e1cter", "Num\u00e9rico", "Car\u00e1cter", "Car\u00e1cter", "Car\u00e1cter"),
      c("4", "2500", "2", "3", "20", "4"),
      c("0-5000", "", "0 al 98", "18-99, NA, NSS, ND, NP, NULL", cantidad, "2000-2090, NA, NSS, ND, NP, NULL")
    ),
    "diccionario_de_datos/diccionario_de_datos_m1s1p4_cndhf2025.csv" = diccionario(
      c("rnsnni1", "seoesntt", "seoesn1", "seoesn2", "seoesn3", "seoesn4"),
      c("Realizaci\u00f3n de transmisiones en vivo de las sesiones", "Total", "Ordinarias",
        "Extraordinarias", "Solemnes", "No identificado"),
      rep("SECC I. 4", 6), c("Num\u00e9rico", rep("Car\u00e1cter", 5)), c("2", rep("20", 5)),
      c("0 al 98", rep(cantidad, 5))
    )
  ),
  otros = list(
    "readme.txt" = "Descripci\u00f3n de las carpetas del paquete de datos abiertos (muestra inventada)",
    "diccionario_de_datos/readme.txt" = siglas,
    "metadatos/metadatos_cndhf_2025.txt" = "Muestra inventada",
    "modelo_entidad_relacion/modelo_er_consejo_consultivo_cndhf2025.png" = "Muestra inventada"
  )
)

# Edicion 2025, modulo 2: tablas con geografia --------------------------------------
escribir_zip(
  "cndhf2025_hechos.zip",
  tablas = list(
    "conjunto_de_datos/0_indice_cndhf2025.csv" = tabla(
      ARCHIVO = c("m2s9p1", "m2s9p2cp4"),
      CONTENIDO = c(
        "Hechos violatorios de derechos humanos seleccionados, seg\u00fan entidad federativa, durante el a\u00f1o 2024.",
        "Hechos violatorios de derechos humanos seleccionados, seg\u00fan municipio e instituci\u00f3n se\u00f1alada como responsable, durante el a\u00f1o 2024."
      )
    ),
    # Por entidad: la columna tiene catalogo y sus codigos no llevan ceros
    "conjunto_de_datos/m2s9p1_cndhf2025.csv" = tabla(
      entfede_c = c("1", "14", "99"), totalca1 = c("3", "NSS", "0")
    ),
    # Por municipio: la entidad y el municipio vienen en columnas separadas
    "conjunto_de_datos/m2s9p2cp4_cndhf2025.csv" = tabla(
      numeral = c("1", "2", "3", "4"),
      enticve1 = c("1", "14", "14", "NSS"),
      entifed1 = c("Aguascalientes", "Jalisco", "Jalisco", "NSS"),
      municve1 = c("1", "39", "120", "NSS"),
      municip1 = c("Aguascalientes", "Guadalajara", "Zapopan", "NSS"),
      instresp_y = c("102", "201", "201", "304"),
      hevidh1 = c("1", "0", "NA", "2"),
      hevidh2 = c("0", "NP", "1", "0")
    ),
    "catalogos/entfede_c_cndhf2025.csv" = tabla(
      entfede_c = c("1", "14", "99"), descrip = c("Aguascalientes", "Jalisco", "No especificado")
    ),
    "catalogos/instresp_y_cndhf2025.csv" = tabla(
      instresp_y = c("102", "201", "304"),
      descrip = c("\u00c1mbito federal", "\u00c1mbito estatal", "\u00c1mbito municipal")
    ),
    "diccionario_de_datos/diccionario_de_datos_m2s9p1_cndhf2025.csv" = diccionario(
      c("entfede_c", "totalca1"), c("Entidad federativa", "Hechos violatorios"), rep("SECC IX. 1", 2),
      c("Num\u00e9rico", "Car\u00e1cter"), c("2", "20"), c("1 al 99", cantidad)
    ),
    "diccionario_de_datos/diccionario_de_datos_m2s9p2cp4_cndhf2025.csv" = diccionario(
      c("numeral", "enticve1", "entifed1", "municve1", "municip1", "instresp_y", "hevidh1", "hevidh2"),
      c("Identificador o numeral del orden que guarda cada registro", "Clave de Entidad federativa",
        "Nombre de Entidad federativa", "Clave de Municipio o demarcaci\u00f3n territorial",
        "Nombre de Municipio o demarcaci\u00f3n territorial", "Instituci\u00f3n se\u00f1alada como responsable",
        "Desaparici\u00f3n forzada", "Ejecuci\u00f3n extrajudicial, arbitraria o sumaria"),
      rep("SECC IX. 2", 8),
      c(rep("Car\u00e1cter", 5), "Num\u00e9rico", "Car\u00e1cter", "Car\u00e1cter"),
      c("4", rep("2500", 4), "3", "20", "20"),
      c("0-5000", rep("", 4), "102 al 304", cantidad, cantidad)
    )
  ),
  otros = list(
    "readme.txt" = "Descripci\u00f3n de las carpetas del paquete de datos abiertos (muestra inventada)",
    "diccionario_de_datos/readme.txt" = siglas,
    "metadatos/metadatos_cndhf_2025.txt" = "Muestra inventada"
  )
)

# Edicion 2021: bitacoras de cambios y clave del municipio en una columna -----------
escribir_zip(
  "cndhf2021_seguimiento.zip",
  tablas = list(
    "conjunto_de_datos/0_indice_cndhf2021.csv" = tabla(
      ARCHIVO = c("m2s8p1", "m2s8p2", "m2s8p3"),
      CONTENIDO = c(
        "Recomendaciones emitidas por la Comisi\u00f3n Nacional de los Derechos Humanos, durante el a\u00f1o 2020.",
        "Recomendaciones emitidas, seg\u00fan instituci\u00f3n se\u00f1alada como responsable, durante el a\u00f1o 2020.",
        "Recomendaciones emitidas, seg\u00fan municipio, durante el a\u00f1o 2020."
      )
    ),
    "conjunto_de_datos/m2s8p1_cndhf2021.csv" = tabla(totalca1 = "90"),
    "conjunto_de_datos/m2s8p2_cndhf2021.csv" = tabla(
      instresp_c = c("101", "102", "103"), recosett = c("60", "25", "NSS"),
      recose1 = c("40", "25", "NSS"), recose2 = c("20", "0", "NSS")
    ),
    "conjunto_de_datos/m2s8p3_cndhf2021.csv" = tabla(
      ubicageo_c = c("1001", "9015", "14039"),
      municip1 = c("Aguascalientes", "Cuauht\u00e9moc", "Guadalajara"),
      totalca1 = c("2", "11", "5")
    ),
    "conjunto_de_datos/m2s8p1_cndhf2021_bitacora_de_cambios.csv" = tabla(
      fecha = "2022-03-01", cambio = "Se corrigi\u00f3 el total de recomendaciones"
    ),
    "conjunto_de_datos/m2s8p2_cndhf2021_bitacora_de_cambios.csv" = tabla(
      fecha = "2022-03-01", cambio = "Se corrigi\u00f3 una instituci\u00f3n"
    ),
    "catalogos/instresp_c_cndhf2021.csv" = tabla(
      instresp_c = c("101", "102", "103"),
      descrip = c("Instituto Mexicano del Seguro Social", "Secretar\u00eda de la Defensa Nacional", "Polic\u00eda Federal")
    ),
    "catalogos/ubicageo_c_cndhf2021.csv" = tabla(
      ubicageo_c = c("1001", "9015", "14039"),
      descrip = c("Aguascalientes", "Cuauht\u00e9moc", "Guadalajara")
    ),
    "diccionario_de_datos/diccionario_de_datos_m2s8p1_cndhf2021.csv" = diccionario(
      "totalca1", "Recomendaciones emitidas", "SECC VIII. 1", "Car\u00e1cter", "20", cantidad
    ),
    "diccionario_de_datos/diccionario_de_datos_m2s8p2_cndhf2021.csv" = diccionario(
      c("instresp_c", "recosett", "recose1", "recose2"),
      c("Instituci\u00f3n se\u00f1alada como responsable", "Total", "Aceptadas", "No aceptadas"),
      rep("SECC VIII. 2", 4), c("Num\u00e9rico", rep("Car\u00e1cter", 3)), c("3", rep("20", 3)),
      c("101 al 304", rep(cantidad, 3))
    ),
    "diccionario_de_datos/diccionario_de_datos_m2s8p3_cndhf2021.csv" = diccionario(
      c("ubicageo_c", "municip1", "totalca1"),
      c("Clave geoestad\u00edstica del municipio", "Nombre del municipio", "Recomendaciones emitidas"),
      rep("SECC VIII. 3", 3), c("Num\u00e9rico", "Car\u00e1cter", "Car\u00e1cter"), c("5", "2500", "20"),
      c("1001 al 32058", "", cantidad)
    )
  ),
  otros = list(
    "diccionario_de_datos/readme.txt" = siglas,
    "metadatos/metadatos_cndhf_2021.txt" = "Muestra inventada"
  )
)

# Edicion 2017: nombre corto, diccionario con forma de reporte y "metadatos\" -------
escribir_zip(
  "cndhf2017_participacion.zip",
  tablas = list(
    "conjunto_de_datos/partciud.csv" = tabla(re_snn1 = "1", re_snn2 = "2"),
    "catalogos/re_snn1.csv" = tabla(re_snn1 = c("1", "2", "9"), descrip = c("S\u00ed", "No", "No se sabe")),
    "catalogos/re_snn2.csv" = tabla(re_snn2 = c("1", "2", "9"), descrip = c("S\u00ed", "No", "No se sabe"))
  ),
  otros = list(
    "metadatos\\" = character(),
    "diccionario_de_datos/diccionario_de_datos_participacion_ciudada_cndhf2017.csv" = c(
      "\"Censo Nacional de Derechos Humanos Federal 2017",
      "M\u00f3dulo 1: Estructura organizacional y recursos de la Comisi\u00f3n Nacional de los Derechos Humanos",
      "Tema: Participaci\u00f3n ciudadana\",,,,,,,,",
      "\"Archivo: PARTCIUD.dbf (Contiene variables que caracterizan a la Comisi\u00f3n Nacional de los Derechos Humanos de acuerdo con la participaci\u00f3n ciudadana, durante el a\u00f1o 2016.)\",,,,,,,,",
      "N\u00fam.de campo,Nombre de la columna,Origen,Descripcion,Tipo de dato,Longitud,C\u00f3digos v\u00e1lidos en la columna,,\"Rango (Los valores NA, NSS y ND equivalen a No aplica, No se sabe y No disponible, respectivamente)\"",
      "1,RE_SNN1,SECC X. 58,Apertura de espacios para la participaci\u00f3n ciudadana.,Num\u00e9rico,1,1,S\u00ed,",
      ",,,,,,2,No,",
      ",,,,,,9,No se sabe,",
      "2,RE_SNN2,SECC X. 59,Atenci\u00f3n de propuestas ciudadanas.,Num\u00e9rico,1,1,S\u00ed,",
      ",,,,,,2,No,",
      ",,,,,,9,No se sabe,"
    ),
    "diccionario_de_datos/diccionario_de_datos_participacion_ciudada_cndhf2017.png" = "Muestra inventada",
    "modelo_entidad_relacion/modelo_er_participacion_ciudada_cndhf2017.png" = "Muestra inventada",
    "metadatos/metadatos_cndhf_2017.txt" = "Muestra inventada"
  )
)

# Edicion 2017: nombres que no son UTF-8 valido y una tabla guardada con "\" ---------
# El zip se escribe byte por byte (escribir_zip_crudo), porque utils::zip() no
# puede guardar estos nombres.
tema_dos <- "Capacitaci\u00f3n y difusi\u00f3n"
escribir_zip_crudo("cndhf2017_capacitacion.zip", list(
  list(
    nombre = bytes_dos(paste0(tema_dos, "/conjunto_de_datos/capacdif.csv")),
    contenido = c("re_snn1,totalca1,nombre1", "1,5,Capacitaci\u00f3n", "2,NULL,Difusi\u00f3n")
  ),
  list(
    nombre = bytes_dos(paste0(tema_dos, "\\conjunto_de_datos\\difusi\u00f3n.csv")),
    contenido = c("re_snn1,totalca1", "1,NSS", "2,7")
  ),
  list(
    nombre = bytes_dos(paste0(tema_dos, "/catalogos/re_snn1.csv")),
    contenido = c("re_snn1,descrip", "1,S\u00ed", "2,No", "9,No se sabe")
  ),
  list(
    nombre = bytes_dos(paste0(tema_dos, "/diccionario_de_datos/diccionario_de_datos_Capacitaci\u00f3n_y_difusi\u00f3n.csv")),
    contenido = c(
      "\"Censo Nacional de Derechos Humanos Federal 2017\",,,,,,,,",
      "\"Archivo: CAPACDIF.dbf (Muestra inventada)\",,,,,,,,",
      "1,RE_SNN1,SECC XI. 1,Muestra inventada,Num\u00e9rico,1,1,S\u00ed,"
    )
  ),
  list(
    nombre = bytes_dos(paste0(tema_dos, "/modelo_entidad_relacion/modelo_er_Capacitaci\u00f3n.png")),
    contenido = charToRaw("Muestra inventada")
  ),
  list(nombre = charToRaw("metadatos\\"), contenido = raw())
))

message("Muestras escritas en ", destino, ": ", paste(list.files(destino), collapse = ", "))
