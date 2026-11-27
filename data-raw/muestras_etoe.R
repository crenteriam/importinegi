# Genera los archivos de muestra con los que se prueba etoe() sin conexion
# (tests/testthat/fixtures/etoe).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_etoe.R
#
# Los datos son inventados. Imitan la forma de los zip mensuales de la Encuesta
# Telefonica de Ocupacion y Empleo, que se reviso el 27 de noviembre de 2026 en
# las 30 tablas de los seis zip: las cinco tablas de la ENOE en DBF, con las
# columnas de la ENOE hasta 2020-T1 y tres ponderadores por tabla. Cada muestra
# reproduce una de las formas que la funcion debe leer:
#
#   etoe_2020_abril          Abril, ponderadores con las proyecciones del
#                            CONAPO: FAC, FAC32 y FAC39. Archivos COE1T0420.dbf.
#   etoe_2020_abril_cpv2020  Abril, ponderadores con la estimacion de poblacion
#                            con base en el Censo 2020: FAC_NP, FAC32_NP y
#                            FAC39_NP. Archivos con la extension en mayusculas
#                            (COE1T0420.DBF). Las mismas filas que la anterior;
#                            en los hogares sin entrevista, el ponderador
#                            nacional y EST_D valen cero.
#   etoe_2020_mayo_cpv2020   Mayo, igual que la anterior, con las tablas dentro
#                            de una carpeta (etoe_2020_mayo_cpv2020_dbf/).
#   etoe_2020_junio_cpv2020  Junio. Archivos con "n" en lugar de "T" y en
#                            minusculas (coe1n0620.dbf). Las tablas tienen
#                            ademas TIPO y NUPM, y EST_D es texto.
#
# No hay muestra de mayo ni de junio con las proyecciones del CONAPO: sirven
# para probar el aviso de que el INEGI no tiene un archivo.
#
# Las viviendas, los hogares y los residentes son los de las muestras de la
# ENOE (ver data-raw/muestras_enoe.R) con la estructura de 2005-T1 a 2020-T1:
# cinco viviendas, seis hogares (dos sin entrevista y sin residentes) y ocho
# residentes (seis con cuestionario de ocupacion y empleo). El ponderador
# nacional del hogar de la vivienda 2 es distinto en las tablas de vivienda
# (200), de hogar (201) y sociodemografica (202). El ponderador de 32 ciudades
# es el nacional mas 1000 en las filas de la Ciudad de Mexico y cero en las de
# Jalisco; el de 39 ciudades, el nacional mas 2000. PER vale "220" en los tres
# meses, T_LOC esta en blanco y ninguna columna indica el mes, como en los
# archivos del INEGI.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "etoe")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)
source("data-raw/muestras_comun.R")
source("data-raw/muestras_enoe_tablas.R")

# Tablas de un mes. `poblacion` es la version de los ponderadores: "conapo" o
# "cpv2020".
tablas_etoe <- function(mes, poblacion) {
  tablas <- tablas_enoe("A", "220")
  for (nombre in names(tablas)) {
    datos <- tablas[[nombre]]
    nacional <- datos$FAC
    datos$FAC <- NULL
    sin_entrevista <- if (nombre == "hog") datos$R_DEF != "00" else rep(FALSE, nrow(datos))

    # Estrato de diseño y tamaño de localidad, despues del municipio. Las dos
    # partes del cuestionario de ocupacion de la muestra no tienen esas columnas.
    if ("MUN" %in% names(datos)) {
      estrato <- rep(1L, nrow(datos))
      if (poblacion == "cpv2020") {
        estrato[sin_entrevista] <- 0L
      }
      diseno <- tabla(EST_D = if (mes == "junio") sprintf("%04d", estrato) else estrato,
                      T_LOC = rep(NA_character_, nrow(datos)))
      despues <- match("MUN", names(datos))
      datos <- cbind(datos[seq_len(despues)], diseno, datos[-seq_len(despues)], stringsAsFactors = FALSE)
    }
    if (mes == "junio") {
      if (poblacion == "cpv2020") {
        # 1: entrevista en la vivienda; 2: entrevista telefonica
        datos$TIPO <- ifelse(datos$ENT == "14", "1", "2")
      }
      datos$NUPM <- sprintf("%04d", as.integer(substr(datos$UPM, 6, 7)))
    }

    ponderadores <- tabla(
      FAC = nacional,
      FAC32 = ifelse(datos$ENT == "09", nacional + 1000L, 0L),
      FAC39 = nacional + 2000L
    )
    if (poblacion == "cpv2020") {
      ponderadores$FAC[sin_entrevista] <- 0L
      names(ponderadores) <- paste0(names(ponderadores), "_NP")
    }
    datos <- cbind(datos, ponderadores, stringsAsFactors = FALSE)
    rownames(datos) <- NULL
    tablas[[nombre]] <- datos
  }
  tablas
}

# Escribe el zip de un mes. `archivos` da el nombre del archivo de cada tabla,
# con "%s" en el lugar del nombre de la tabla; si el resto del nombre esta en
# minusculas, el nombre de la tabla tambien.
escribir_mes <- function(nombre, mes, poblacion, archivos) {
  tablas <- tablas_etoe(mes, poblacion)
  resto <- sub("^.*%s", "", archivos)
  names(tablas) <- sprintf(archivos, if (resto == tolower(resto)) names(tablas) else toupper(names(tablas)))
  # foreign::write.dbf() advierte que T_LOC no tiene ningun dato: es lo que se busca
  suppressWarnings(escribir_zip(nombre, tablas = tablas))
}

escribir_mes("etoe_2020_abril.zip", "abril", "conapo", "%sT0420.dbf")
escribir_mes("etoe_2020_abril_cpv2020.zip", "abril", "cpv2020", "%sT0420.DBF")
escribir_mes("etoe_2020_mayo_cpv2020.zip", "mayo", "cpv2020", "etoe_2020_mayo_cpv2020_dbf/%sT0520.DBF")
escribir_mes("etoe_2020_junio_cpv2020.zip", "junio", "cpv2020", "%sn0620.dbf")

message("Muestras de la ETOE escritas en ", destino, ": ", paste(list.files(destino), collapse = ", "))
