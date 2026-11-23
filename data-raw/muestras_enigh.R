# Genera los archivos de muestra con los que se prueba enigh_tradicional() sin
# conexion (tests/testthat/fixtures/enigh).
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/muestras_enigh.R
#
# Los datos son inventados. Imitan la forma de los archivos de la serie
# tradicional de la Encuesta Nacional de Ingresos y Gastos de los Hogares
# (ENIGH), que se reviso el 22 de noviembre de 2026 en el indice de los 106 zip
# en DBF del INEGI y en el encabezado de sus 126 tablas. Cada muestra reproduce
# una de las formas que la funcion debe leer:
#
#   trad1984_*  Un DBF por zip, con nombres propios de la edicion (CONCE84.dbf).
#               El zip de gastos no trae la tabla que se espera.
#   trad1994_*  Dos tablas en el zip de gastos (gastos94.dbf y eroga94.dbf) y
#               una nota en el del concentrado.
#   trad2006_*  Tablas dentro de una carpeta, concentrado con los nombres de las
#               columnas en minusculas y un registro borrado en las erogaciones.
#   trad2008_*  Cuatro tablas en el zip de gastos, con campos de tipo F, y los
#               negocios del hogar juntos en un zip, con codigos negativos.
#   trad2010_*  Seis tablas en el zip de gastos y, en el de poblacion, una tabla
#               en mayusculas (Trabajos.dbf) y otra en minusculas (Poblacion.dbf).
#   trad2014_*  Una tabla por zip, viviendas con asteriscos en campos numericos
#               y folio de la vivienda de 10 caracteres.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
destino <- file.path(normalizePath("tests/testthat/fixtures"), "enigh")
unlink(destino, recursive = TRUE)
dir.create(destino, recursive = TRUE)
source("data-raw/muestras_comun.R")

# Escribe un DBF con la forma exacta que se indica y devuelve su ruta.
# foreign::write.dbf() decide los anchos y los tipos por su cuenta; aqui se
# necesita controlarlos para imitar los archivos del INEGI.
#   tipos       Vector con nombre: c(FOLIO = "C11", EDAD = "N2", GAS_TRI = "F10.2").
#               Tipo del campo (C, N o F), ancho y, si los tiene, decimales.
#   borrados    Filas que se marcan como registros borrados.
#   asteriscos  Lista con nombre: en esos campos, las filas indicadas se llenan
#               con asteriscos. Los NA se escriben en blanco.
#   pagina      Byte 29 del encabezado: pagina de codigos (0x03 = CP1252).
escribir_dbf <- function(datos, tipos, borrados = integer(), asteriscos = list(), pagina = 0x03) {
  stopifnot(identical(names(datos), names(tipos)))
  partes <- regmatches(tipos, regexec("^([CNF])([0-9]+)(?:[.]([0-9]+))?$", tipos))
  tipo <- vapply(partes, function(p) p[2], character(1))
  ancho <- vapply(partes, function(p) as.integer(p[3]), integer(1))
  decimales <- vapply(partes, function(p) if (nzchar(p[4])) as.integer(p[4]) else 0L, integer(1))

  columnas <- lapply(seq_along(datos), function(i) {
    valores <- datos[[i]]
    texto <- if (tipo[i] == "C") {
      formatC(ifelse(is.na(valores), "", as.character(valores)), width = ancho[i], flag = "-")
    } else {
      numeros <- formatC(as.numeric(valores), width = ancho[i], digits = decimales[i], format = "f")
      ifelse(is.na(valores), strrep(" ", ancho[i]), numeros)
    }
    filas <- asteriscos[[names(datos)[i]]]
    texto[filas] <- strrep("*", ancho[i])
    stopifnot(all(nchar(texto, type = "bytes") == ancho[i]))
    texto
  })
  marca <- rep(" ", nrow(datos))
  marca[borrados] <- "*"
  registros <- do.call(paste0, c(list(marca), columnas))

  entero <- function(valor, bytes) as.raw((valor %/% 256^(seq_len(bytes) - 1)) %% 256)
  largo_encabezado <- 32 + 32 * length(tipos) + 1
  largo_registro <- 1 + sum(ancho)
  encabezado <- c(
    as.raw(c(0x03, 126, 11, 22)), entero(nrow(datos), 4), entero(largo_encabezado, 2),
    entero(largo_registro, 2), raw(17), as.raw(pagina), raw(2)
  )
  campos <- unlist(lapply(seq_along(tipos), function(i) {
    nombre <- charToRaw(names(tipos)[i])
    stopifnot(length(nombre) <= 10)
    c(nombre, raw(11 - length(nombre)), charToRaw(tipo[i]), raw(4), as.raw(ancho[i]), as.raw(decimales[i]), raw(14))
  }))
  archivo <- tempfile(fileext = ".dbf")
  conexion <- file(archivo, open = "wb")
  on.exit(close(conexion))
  writeBin(c(encabezado, campos, as.raw(0x0d)), conexion)
  writeBin(charToRaw(paste(registros, collapse = "")), conexion)
  writeBin(as.raw(0x1a), conexion)
  archivo
}

# Zip de la ENIGH: `dbf` es una lista con nombre (la ruta de cada DBF dentro
# del zip) cuyos elementos son las rutas que devuelve escribir_dbf().
zip_enigh <- function(nombre, dbf, otros = list()) {
  escribir_zip(nombre, tablas = list(), otros = otros, binarios = unlist(dbf))
}

nota_concentrado <- function(year) {
  paste0(
    "El 30 de agosto de 2024, se reemplazo la informacion de esta tabla, debido a que se ",
    "incorporaron las variables \"Estrato de diseno muestral\" (EST_DIS) y \"Unidad Primaria ",
    "de Muestreo\" (UPM), para facilitar el calculo de las precisiones estadisticas. (", year, ")"
  )
}

# Edicion 1984 ------------------------------------------------------------------------
folios84 <- c("19840119001", "19840119002", "19840910540")
zip_enigh("trad1984_concen.zip", list(
  "CONCE84.dbf" = escribir_dbf(
    tabla(FOLIO = folios84, UBICA_GEO = c("01001", "01001", "09015"), ZONA = c("1", "1", "2"),
          HOG = c(1653, 1653, 2210.5), INGCOR = c(97204.3, 45051.98, NA), TAM_HOG = c(5, 3, 7)),
    c(FOLIO = "C11", UBICA_GEO = "C5", ZONA = "C1", HOG = "N10.2", INGCOR = "N11.2", TAM_HOG = "N2")
  )
))
zip_enigh("trad1984_hogares.zip", list(
  "hogares.dbf" = escribir_dbf(
    tabla(FOLIO = folios84, CARAC_VIV = c("1", "2", NA), TOTAL_MIEM = c(5, 3, 7),
          APARATOS = c("1000001010101011011001", "0000000000000000000000", "1010000000000000000001"),
          FACTOR = c(1653, 1653, 2210.5), ZONA = c("1", "1", "2")),
    c(FOLIO = "C11", CARAC_VIV = "C1", TOTAL_MIEM = "N2", APARATOS = "C22", FACTOR = "N8.2", ZONA = "C1")
  )
))
# El zip de gastos de esta muestra no trae la tabla de gastos
zip_enigh("trad1984_gastos.zip", list(
  "otra_tabla.dbf" = escribir_dbf(tabla(FOLIO = folios84[1], CLAVE = "A004"), c(FOLIO = "C11", CLAVE = "C4"))
))

# Edicion 1994 ------------------------------------------------------------------------
folios94 <- c("19940110010", "19940110020", "19943250011")
zip_enigh(
  "trad1994_concen.zip",
  list("concen.dbf" = escribir_dbf(
    tabla(FOLIO = folios94, UBICA_GEO = c("01001", "01001", "32056"), HOG = c(169, 169, 310.25),
          ESTRATO = c("1", "1", "4"), INGCOR = c(7400.31, 5062.41, 1210), GASMON = c(5982.72, 4100, NA),
          TAM_HOG = c(9, 4, 2), EST_DIS = c("001", "001", "412"), UPM = c("00012", "00012", "03377")),
    c(FOLIO = "C11", UBICA_GEO = "C5", HOG = "N11.2", ESTRATO = "C1", INGCOR = "N11.2", GASMON = "N11.2",
      TAM_HOG = "N2", EST_DIS = "C3", UPM = "C5")
  )),
  otros = list("nota_bases_datos_concen_enigh_1994.txt" = nota_concentrado(1994))
)
zip_enigh("trad1994_gastos.zip", list(
  "gastos94.dbf" = escribir_dbf(
    tabla(FOLIO = folios94[c(1, 1, 2, 3)], CLAVE = c("A004", "A078", "A004", "G001"),
          NUM_REN = c("00", "00", "00", "02"), GASTO = c(2, 14.5, 3.75, 600), GAS_TRI = c(26.29, 190.6, 49.3, 1840.02)),
    c(FOLIO = "C11", CLAVE = "C4", NUM_REN = "C2", GASTO = "N8.2", GAS_TRI = "N8.2")
  ),
  "eroga94.dbf" = escribir_dbf(
    tabla(FOLIO = folios94[c(1, 3)], CLAVE = c("Q004", "Q001"), ERO_TRI = c(540, 1800)),
    c(FOLIO = "C11", CLAVE = "C4", ERO_TRI = "N9.2")
  )
))
zip_enigh("trad1994_poblacion.zip", list(
  "POBLA94.dbf" = escribir_dbf(
    tabla(FOLIO = folios94[c(1, 1, 2, 3)], NUM_REN = c("01", "02", "01", "01"), PARENTESCO = c("1", "2", "1", "1"),
          SEXO = c("1", "2", "2", "1"), EDAD = c(30, 28, 61, 8), PRESTACIO1 = c("010203", NA, "000000", NA)),
    c(FOLIO = "C11", NUM_REN = "C2", PARENTESCO = "C1", SEXO = "C1", EDAD = "N2", PRESTACIO1 = "C6")
  )
))

# Edicion 2006 ------------------------------------------------------------------------
folios06 <- c("20060110010", "20060110020", "20063032070")
zip_enigh(
  "trad2006_concentrado.zip",
  list("concen.dbf" = escribir_dbf(
    tabla(folio = folios06, hog = c(861, 861, 1204), estrato = c("1", "1", "4"),
          ubica_geo = c("01001", "01001", "30087"), ingcor = c(130750.76, 99337.07, 18340.5),
          est_dis = c("001", "001", "377"), upm = c("00004", "00004", "02911")),
    c(folio = "C11", hog = "N8.2", estrato = "C1", ubica_geo = "C5", ingcor = "N12.2", est_dis = "C3", upm = "C5"),
    pagina = 0x00
  )),
  otros = list("nota_bases_datos_concen_enigh_2006.txt" = nota_concentrado(2006))
)
zip_enigh("trad2006_hogares.zip", list(
  "Tra_Hogares_2006_concil_2010_DBF/Hogares.dbf" = escribir_dbf(
    tabla(FOLIO = folios06, ESTRATO = c("1", "1", "4"), CONAPO = c("4", "4", "1"),
          UBICA_GEO = c("01001", "01001", "30087"), RESIDENTES = c(5, 2, 6), FACTOR = c(861, 861, 1204)),
    c(FOLIO = "C11", ESTRATO = "C1", CONAPO = "C1", UBICA_GEO = "C5", RESIDENTES = "N2", FACTOR = "N10")
  )
))
zip_enigh("trad2006_poblacion.zip", list(
  "Tra_Poblacion_2006_concil_2010_DBF/Pobla06.dbf" = escribir_dbf(
    tabla(FOLIO = folios06[c(1, 1, 2, 3)], NUM_REN = c("01", "02", "01", "01"),
          PARENTESCO = c("100", "200", "100", "100"), SEXO = c("1", "2", "2", "1"), EDAD = c(53, 49, 71, 34)),
    c(FOLIO = "C11", NUM_REN = "C2", PARENTESCO = "C3", SEXO = "C1", EDAD = "N2")
  )
))
# Las erogaciones tienen un registro marcado como borrado (el tercero)
zip_enigh("trad2006_gastos.zip", list(
  "Tra_Gastos_2006_concil_2010_DBF/Eroga.dbf" = escribir_dbf(
    tabla(FOLIO = folios06[c(1, 2, 2, 3)], MESES = rep("070605040302", 4), CLAVE = c("Q001", "Q005", "Q005", "Q014"),
          ERO_1 = c(1000, 1, 999999, 0), ERO_TRI = c(498.63, 2.99, 99999.99, 30)),
    c(FOLIO = "C11", MESES = "C12", CLAVE = "C4", ERO_1 = "N9", ERO_TRI = "N10.2"),
    borrados = 3
  ),
  "Tra_Gastos_2006_concil_2010_DBF/Gastos.dbf" = escribir_dbf(
    tabla(FOLIO = folios06[c(1, 1, 3)], CLAVE = c("A004", "A244", "C001"), NUM_REN = c("00", "00", "01"),
          GASTO = c(4, 100, 20), GAS_TRI = c(51.86, 1296.44, 58.06)),
    c(FOLIO = "C11", CLAVE = "C4", NUM_REN = "C2", GASTO = "N9.2", GAS_TRI = "N9.2")
  )
))

# Edicion 2008 ------------------------------------------------------------------------
viv08 <- c("011002", "011016", "301477")
zip_enigh("trad2008_gastos.zip", list(
  "G_educa.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv08[2], FOLIOHOG = "0", NUMREN = "03", CLAVE = "E005", GASTO = 750, GAS_TRI = 2243.84),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", NUMREN = "C2", CLAVE = "C4", GASTO = "N9", GAS_TRI = "F10.2")
  ),
  "Gastos.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv08[c(1, 3)], FOLIOHOG = c("0", "0"), CLAVE = c("C001", "K012"), LUG_COM = c(4, NA),
          GASTO = c(14, 350), INMUJER = c(-1, -1), GAS_TRI = c(41.88, 1047.12)),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", CLAVE = "C4", LUG_COM = "N2", GASTO = "N9", INMUJER = "N9", GAS_TRI = "F10.2")
  ),
  "Eroga.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv08[2], FOLIOHOG = "0", CLAVE = "Q014", ERO_1 = 10, ERO_TRI = 4.99),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", CLAVE = "C4", ERO_1 = "N9", ERO_TRI = "F10.2")
  ),
  "G_diario.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv08[c(1, 1, 3)], FOLIOHOG = c("0", "0", "0"), CLAVE = c("A009", "A075", "A009"),
          DIA = c(1, 3, 7), NUMREN = c("00", "00", "00"), CANTIDAD = c(0.2, 1.5, 12),
          GASTO = c(4, 18.5, 96), GAS_TRI = c(51.86, 239.85, 1244.57)),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", CLAVE = "C4", DIA = "N1", NUMREN = "C2", CANTIDAD = "F6.3",
      GASTO = "F10.2", GAS_TRI = "F10.2")
  )
))
zip_enigh("trad2008_tarjetas.zip", list(
  "Tra_Tarjetas_2008_concil_2010_DBF/G_tarje.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv08[c(1, 3)], FOLIOHOG = c("0", "0"), CLAVE = c("TB01", "TB04"),
          GASTO = c(1200, 825), GAS_TRI = c(3590.14, 2468.22)),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", CLAVE = "C4", GASTO = "N9", GAS_TRI = "F10.2")
  )
))
zip_enigh("trad2008_negocios.zip", list(
  "Noagro.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv08[c(1, 3)], FOLIOHOG = c("0", "0"), NUMREN = c("02", "01"), NUMTRAB = c(1, 2),
          T_EMP = c(3, -1), VENTAS = c(5600, 0)),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", NUMREN = "C2", NUMTRAB = "N1", T_EMP = "N2", VENTAS = "N10")
  ),
  "Agro.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv08[3], FOLIOHOG = "0", NUMREN = "01", NUMTRAB = 1, T_EMP = -1, VENTAS = 15000),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", NUMREN = "C2", NUMTRAB = "N1", T_EMP = "N2", VENTAS = "N10")
  )
))

# Edicion 2010 ------------------------------------------------------------------------
viv10 <- c("011001", "046001", "200933")
zip_enigh("trad2010_concentrado.zip", list(
  "Tra_Concentrado_2010_concil_2010_DBF.dbf" = escribir_dbf(
    tabla(folioviv = viv10, foliohog = c("0", "0", "1"), factor = c(915, 402, 1310), tam_loc = c(1, 2, 4),
          ubica_geo = c("01001", "04002", "20067"), ingcor = c(22986.86, 51200, 7310.4),
          est_dis = c("0010", "0461", "2093"), upm = c("00011", "00460", "20093")),
    c(folioviv = "C6", foliohog = "C1", factor = "N5", tam_loc = "N1", ubica_geo = "C5", ingcor = "N15.2",
      est_dis = "C4", upm = "C5"),
    pagina = 0x00
  )
))
# En el mismo zip, una tabla con los nombres en mayusculas y otra en minusculas
zip_enigh("trad2010_poblacion.zip", list(
  "Trabajos.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv10[c(1, 2)], FOLIOHOG = c("0", "0"), NUMREN = c("01", "01"), NUMTRAB = c("1", "1"),
          SUBOR = c("1", "2"), HTRAB = c(48, 20)),
    c(FOLIOVIV = "C6", FOLIOHOG = "C1", NUMREN = "C2", NUMTRAB = "C1", SUBOR = "C1", HTRAB = "N3")
  ),
  "Poblacion.dbf" = escribir_dbf(
    tabla(folioviv = viv10[c(1, 1, 2, 3)], foliohog = c("0", "0", "0", "1"), numren = c("01", "02", "01", "01"),
          parentesco = c("101", "201", "101", "101"), sexo = c("1", "2", "2", "1"), edad = c(48, 45, 33, 67)),
    c(folioviv = "C6", foliohog = "C1", numren = "C2", parentesco = "C3", sexo = "C1", edad = "N2"),
    pagina = 0x00
  )
))
gasto10 <- function(clave, gasto, trimestral) {
  escribir_dbf(
    tabla(folioviv = viv10[1], foliohog = "0", clave = clave, gasto = gasto, gas_tri = trimestral),
    c(folioviv = "C6", foliohog = "C1", clave = "C4", gasto = "N9", gas_tri = "N15.2"),
    pagina = 0x00
  )
}
zip_enigh("trad2010_gastos.zip", list(
  "G_educa.dbf" = gasto10("E003", 300, 897.53),
  "G_recibo.dbf" = gasto10("R001", 130, 194.47),
  "Gastos.dbf" = gasto10("C001", 150, 448.77),
  "Eroga.dbf" = gasto10("Q001", 400, 797.81),
  "G_costo.dbf" = gasto10("H056", 100, 99.73),
  "G_diario.dbf" = gasto10("A215", 3, 38.76)
))

# Edicion 2014 ------------------------------------------------------------------------
viv14 <- c("0100008302", "0100008304", "3160511012")
zip_enigh("trad2014_concentrado.zip", list(
  "tra_concentrado_2014_concil_2010.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv14, FOLIOHOG = c("1", "1", "1"), UBICA_GEO = c("010010000", "010010000", "310500000"),
          TAM_LOC = c("1", "1", "3"), EST_DIS = c("005", "005", "512"), UPM = c("00670", "00670", "09133"),
          FACTOR_HOG = c(694, 694, 1100), ING_COR = c(39786.79, 28173.89, 12050)),
    c(FOLIOVIV = "C10", FOLIOHOG = "C1", UBICA_GEO = "C9", TAM_LOC = "C1", EST_DIS = "C3", UPM = "C5",
      FACTOR_HOG = "N5", ING_COR = "N12.2"),
    pagina = 0x00
  )
))
# Asteriscos en los campos numericos que no aplican: renta de una vivienda propia
zip_enigh("trad2014_vivi.zip", list(
  "tra_vivi_2014_concil_2010.dbf" = escribir_dbf(
    tabla(FOLIOVIV = viv14, TIPO_VIV = c("1", "1", "3"), ANTIGUEDAD = c(12, 30, 4), RENTA = c(NA, 4000, NA),
          ESTIM_PAGO = c(3500, NA, 1200), UBICA_GEO = c("010010000", "010010000", "310500000"),
          EST_DIS = c("005", "005", "512"), UPM = c("00670", "00670", "09133"), FACTOR_VIV = c(694, 694, 1100)),
    c(FOLIOVIV = "C10", TIPO_VIV = "C1", ANTIGUEDAD = "N2", RENTA = "N9", ESTIM_PAGO = "N9", UBICA_GEO = "C9",
      EST_DIS = "C3", UPM = "C5", FACTOR_VIV = "N5"),
    asteriscos = list(RENTA = c(1, 3), ESTIM_PAGO = 2, ANTIGUEDAD = 3),
    pagina = 0x00
  )
))
zip_enigh("trad2014_hogares.zip", list(
  "tra_hogares_2014_concil_2010_dbf.dbf" = escribir_dbf(
    tabla(folioviv = viv14, foliohog = c("1", "1", "1"), huespedes = c(0, 0, 2), acc_alim1 = c("2", "1", NA),
          factor_hog = c(694, 694, 1100)),
    c(folioviv = "C10", foliohog = "C1", huespedes = "N1", acc_alim1 = "C1", factor_hog = "N5"),
    pagina = 0x00
  )
))
negocio14 <- function(folio, ventas) {
  escribir_dbf(
    tabla(folioviv = folio, foliohog = "1", numren = "01", id_trabajo = "1", ventas = ventas),
    c(folioviv = "C10", foliohog = "C1", numren = "C2", id_trabajo = "C1", ventas = "N10"),
    pagina = 0x00
  )
}
zip_enigh("trad2014_agropecuario.zip", list(
  "tra_agropecuario_2014_concil_2010_dbf.dbf" = negocio14(viv14[3], 144000)
))
zip_enigh("trad2014_noagropecuario.zip", list(
  "tra_noagropecuario_2014_concil_2010_dbf.dbf" = negocio14(viv14[1], 5200)
))

message("Muestras de la ENIGH generadas en ", destino, ": ", length(list.files(destino)), " archivos.")
